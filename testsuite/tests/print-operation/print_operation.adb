--  Headless coverage for Gtk.Print_Operation.
--
--  An export run (Print_Operation_Action_Export) needs neither a printer nor
--  a dialog: it renders straight to a PDF through cairo. That drives the
--  whole callback chain -- begin-print, request-page-setup, draw-page,
--  end-print, done -- and, pointed at a directory that does not exist, the
--  error path as well.
--
--  The create-custom-widget signal cannot be reached that way: it is only
--  emitted while the print dialog is up. It is emitted by hand here instead,
--  through g_signal_emitv, because its marshaller is the one place in these
--  bindings where a handler's *result* travels back to C. Returning NULL is
--  how a handler declines to supply a widget, so both outcomes are checked.

with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Cairo;
with Cairo.Image_Surface;
with Cairo.Surface;
with Glib;                use Glib;
with Glib.Main;
with Glib.List_Model;     use Glib.List_Model;
with Gtk.Dialog;
with Gtk.Window;
with Glib.Error;          use Glib.Error;
with Glib.Object;         use Glib.Object;
with Glib.Properties;
with Glib.Test;           use Glib.Test;
with Glib.Values;         use Glib.Values;
with Gtk.Box;             use Gtk.Box;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Label;           use Gtk.Label;
with Gtk.Main;
with Gtk.Page_Setup;      use Gtk.Page_Setup;
with Gtk.Paper_Size;      use Gtk.Paper_Size;
with Gtk.Print_Context;   use Gtk.Print_Context;
with Gtk.Print_Operation; use Gtk.Print_Operation;
with Gtk.Print_Operation_Preview;
with Gtk.Print_Settings;  use Gtk.Print_Settings;
with Gtk.Widget;
with Pango.Context;
with Pango.Font_Map;
with Pango.Layout;
with Gtkada.Bindings;
with Gtkada.Types;        use Gtkada.Types;
with System;

use type Gtk.Widget.Gtk_Widget;
use type Gtk.Print_Operation_Preview.Gtk_Print_Operation_Preview;
use type Cairo.Cairo_Status;

procedure Print_Operation is

   --  Neither of these two is bound by GtkAda: emitting a signal with a
   --  return value is not something an application normally does, but it is
   --  the only way to exercise a marshaller's result without a print dialog.

   function G_Signal_Lookup
     (Name : Gtkada.Types.Chars_Ptr; Itype : Glib.GType) return Glib.Guint;
   pragma Import (C, G_Signal_Lookup, "g_signal_lookup");

   procedure G_Signal_Emitv
     (Instance_And_Params : System.Address;
      Signal_Id           : Glib.Guint;
      Detail              : Glib.GQuark;
      Return_Value        : System.Address);
   pragma Import (C, G_Signal_Emitv, "g_signal_emitv");

   procedure Cb_Request_Page_Setup
     (Self    : access Gtk_Print_Operation_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class;
      Page_Nr : Gint;
      Setup   : not null access Gtk_Page_Setup_Record'Class);

   --  The second connection form passes the emitter as a plain GObject.
   --  Keep separate counts so each adapter is checked when export emits it.
   Slot_Begin_Count       : Natural := 0;
   Slot_Draw_Count        : Natural := 0;
   Slot_Page_Setup_Count  : Natural := 0;
   Slot_End_Count         : Natural := 0;
   Slot_Done_Count        : Natural := 0;
   Slot_Status_Count      : Natural := 0;
   Slot_Paginate_Count    : Natural := 0;
   Slot_Custom_Apply_Count : Natural := 0;
   Slot_Update_Count      : Natural := 0;
   Slot_Create_Count      : Natural := 0;
   Preview_Ready_Count      : Natural := 0;
   Preview_Ready_Slot_Count : Natural := 0;
   Preview_Size_Count       : Natural := 0;
   Preview_Size_Slot_Count  : Natural := 0;
   Exercise_Paginate      : Boolean := False;
   Exercise_Preview_Interface : Boolean := False;
   Raise_Preview_Callbacks : Boolean := False;
   Defer_Drawing_Enabled  : Boolean := False;
   Deferred_Draw_Count    : Natural := 0;

   procedure Cb_Begin_Print_Slot
     (Self    : access GObject_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class);
   procedure Cb_Draw_Page_Slot
     (Self    : access GObject_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class;
      Page_Nr : Gint);
   procedure Cb_Request_Page_Setup_Slot
     (Self    : access GObject_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class;
      Page_Nr : Gint;
      Setup   : not null access Gtk_Page_Setup_Record'Class);
   procedure Cb_End_Print_Slot
     (Self    : access GObject_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class);
   procedure Cb_Done_Slot
     (Self   : access GObject_Record'Class;
      Result : Gtk_Print_Operation_Result);
   procedure Cb_Status_Changed_Slot (Self : access GObject_Record'Class);
   function Cb_Paginate
     (Self    : access Gtk_Print_Operation_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class)
      return Boolean;
   function Cb_Paginate_Slot
     (Self    : access GObject_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class)
      return Boolean;
   procedure Cb_Custom_Widget_Apply
     (Self   : access Gtk_Print_Operation_Record'Class;
      Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Cb_Custom_Widget_Apply_Slot
     (Self   : access GObject_Record'Class;
      Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Cb_Update_Custom_Widget
     (Self     : access Gtk_Print_Operation_Record'Class;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Setup    : not null access Gtk_Page_Setup_Record'Class;
      Settings : not null access Gtk_Print_Settings_Record'Class);
   procedure Cb_Update_Custom_Widget_Slot
     (Self     : access GObject_Record'Class;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Setup    : not null access Gtk_Page_Setup_Record'Class;
      Settings : not null access Gtk_Print_Settings_Record'Class);
   function Cb_Custom_Widget_Slot
     (Self : access GObject_Record'Class) return Glib.Object.GObject;
   function Cb_Preview
     (Self    : access Gtk_Print_Operation_Record'Class;
      Preview : Gtk.Print_Operation_Preview.Gtk_Print_Operation_Preview;
      Context : not null access Gtk_Print_Context_Record'Class;
      Parent  : access Gtk.Window.Gtk_Window_Record'Class) return Boolean;
   function Cb_Preview_Slot
     (Self    : access GObject_Record'Class;
      Preview : Gtk.Print_Operation_Preview.Gtk_Print_Operation_Preview;
      Context : not null access Gtk_Print_Context_Record'Class;
      Parent  : access Gtk.Window.Gtk_Window_Record'Class) return Boolean;
   procedure Cb_Preview_Ready
     (Self    : Gtk.Print_Operation_Preview.Gtk_Print_Operation_Preview;
      Context : not null access Gtk_Print_Context_Record'Class);
   procedure Cb_Preview_Ready_Slot
     (Self    : access GObject_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class);
   procedure Cb_Preview_Got_Page_Size
     (Self       : Gtk.Print_Operation_Preview.Gtk_Print_Operation_Preview;
      Context    : not null access Gtk_Print_Context_Record'Class;
      Page_Setup : not null access Gtk_Page_Setup_Record'Class);
   procedure Cb_Preview_Got_Page_Size_Slot
     (Self       : access GObject_Record'Class;
      Context    : not null access Gtk_Print_Context_Record'Class;
      Page_Setup : not null access Gtk_Page_Setup_Record'Class);

   procedure Emit_Paginate
     (Op      : not null access Gtk_Print_Operation_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class);
   procedure Emit_Custom_Widget_Apply
     (Op     : not null access Gtk_Print_Operation_Record'Class;
      Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Emit_Update_Custom_Widget
     (Op       : not null access Gtk_Print_Operation_Record'Class;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Setup    : not null access Gtk_Page_Setup_Record'Class;
      Settings : not null access Gtk_Print_Settings_Record'Class);
   procedure Emit_Preview_Interface_Signals
     (Op      : not null access Gtk_Print_Operation_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class;
      Setup   : not null access Gtk_Page_Setup_Record'Class);

   Pages : constant Gint := 2;

   Begin_Count  : Natural := 0;
   Draw_Count   : Natural := 0;
   Page_Setup_Count : Natural := 0;
   End_Count    : Natural := 0;
   Done_Count   : Natural := 0;
   Setup_Count  : Natural := 0;
   Last_Result  : Gtk_Print_Operation_Result :=
     Print_Operation_Result_In_Progress;

   --  Exceptions raised inside a signal handler never reach the emitter: the
   --  generated marshallers hand them to Gtkada.Bindings.Process_Exception.
   --  Capturing them here is what makes "the handler returned NULL without
   --  raising" an assertable fact.
   Exception_Count : Natural := 0;

   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Cb_Begin_Print
     (Self    : access Gtk_Print_Operation_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class);

   procedure Cb_Draw_Page
     (Self    : access Gtk_Print_Operation_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class;
      Page_Nr : Gint);

   procedure Cb_End_Print
     (Self    : access Gtk_Print_Operation_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class);

   procedure Cb_Done
     (Self   : access Gtk_Print_Operation_Record'Class;
      Result : Gtk_Print_Operation_Result);

   procedure Cb_Status_Changed
     (Self : access Gtk_Print_Operation_Record'Class);

   function Cb_No_Custom_Widget
     (Self : access Gtk_Print_Operation_Record'Class)
      return Glib.Object.GObject;

   function Cb_Custom_Widget
     (Self : access Gtk_Print_Operation_Record'Class)
      return Glib.Object.GObject;

   procedure Emit_Create_Custom_Widget
     (Op     : not null access Gtk_Print_Operation_Record'Class;
      Result : out Glib.Object.GObject);

   procedure Reset_Counts;

   procedure Test_Page_Setup_Dialogs
   with Convention => C;

   Async_Setup : Gtk_Page_Setup;
   Async_Done : Boolean := False;
   Dialog_Response : Gint := Gint (Gtk.Dialog.Gtk_Response_Cancel);
   Dialog_Dismissed : Boolean := False;

   function Dismiss_Page_Setup return Boolean is
      procedure Respond (Dialog : System.Address; Response : Gint);
      pragma Import (C, Respond, "gtk_dialog_response");
      Windows : constant Glist_Model := Gtk.Window.Get_Toplevels;
      Dialog  : GObject;
   begin
      if Get_N_Items (Windows) = 0 then
         return True;
      end if;
      Dialog := Get_Item (Windows, 0);
      Respond (Get_Object (Dialog), Dialog_Response);
      Dialog.Unref;
      Dialog_Dismissed := True;
      return False;
   end Dismiss_Page_Setup;

   procedure Page_Setup_Done
     (Setup : access Gtk_Page_Setup_Record'Class)
   is
   begin
      --  GTK lends the setup to the callback; keep an independent copy.
      Async_Done := True;
      if Setup /= null then
         Async_Setup := Setup.Copy;
      end if;
   end Page_Setup_Done;

   procedure Test_Page_Setup_Dialogs is
      Setup    : constant Gtk_Page_Setup := Gtk_Page_Setup_New;
      Settings : constant Gtk_Print_Settings := Gtk_Print_Settings_New;
      Returned : Gtk_Page_Setup;
      Source   : Glib.Main.G_Source_Id;
      Dispatched : Boolean;
      pragma Unreferenced (Source, Dispatched);
   begin
      Setup.Set_Orientation (Page_Orientation_Landscape);
      Source := Glib.Main.Idle_Add (Dismiss_Page_Setup'Unrestricted_Access);
      Returned := Run_Page_Setup_Dialog (null, Setup, Settings);
      Assert_True (Returned.Get_Orientation = Page_Orientation_Landscape);
      Returned.Unref;

      Dialog_Response := Gint (Gtk.Dialog.Gtk_Response_Ok);
      Run_Page_Setup_Dialog_Async
        (null, Setup, Settings, Page_Setup_Done'Unrestricted_Access);
      Source := Glib.Main.Idle_Add (Dismiss_Page_Setup'Unrestricted_Access);
      while not Async_Done loop
         Dispatched := Glib.Main.Main_Context_Iteration;
      end loop;
      Assert_True (Async_Setup /= null);
      Assert_Cmpfloat_Gt (Async_Setup.Get_Paper_Width (Mm), 0.0);
      Assert_Cmpfloat_Gt (Async_Setup.Get_Paper_Height (Mm), 0.0);
      Async_Setup.Unref;
      Async_Setup := null;
      Async_Done := False;
      Dialog_Response := Gint (Gtk.Dialog.Gtk_Response_Cancel);
      Run_Page_Setup_Dialog_Async
        (null, null, Settings, Page_Setup_Done'Unrestricted_Access);
      Source := Glib.Main.Idle_Add (Dismiss_Page_Setup'Unrestricted_Access);
      while not Async_Done loop
         Dispatched := Glib.Main.Main_Context_Iteration;
      end loop;
      Assert_True (Async_Setup = null);
      --  A null completion callback is allowed; closing its dialog still
      --  completes the asynchronous helper without calling Ada code.
      Dialog_Dismissed := False;
      Run_Page_Setup_Dialog_Async (null, null, Settings, null);
      Source := Glib.Main.Idle_Add (Dismiss_Page_Setup'Unrestricted_Access);
      while not Dialog_Dismissed loop
         Dispatched := Glib.Main.Main_Context_Iteration;
      end loop;
      Setup.Unref;
      Settings.Unref;
   end Test_Page_Setup_Dialogs;

   procedure Test_Properties
   with Convention => C;

   procedure Test_Additional_Signals
   with Convention => C;

   procedure Test_Cancel
   with Convention => C;

   procedure Test_Export
   with Convention => C;

   procedure Test_Preview_Interface
   with Convention => C;

   procedure Test_Export_Error
   with Convention => C;

   procedure Test_Custom_Widget
   with Convention => C;

   --  The widget a create-custom-widget handler hands back. Created once and
   --  kept alive by the test, since the signal is emitted outside a dialog
   --  and so nothing else holds a reference to it.
   Custom : Gtk_Box;

   ----------------------
   -- Record_Exception --
   ----------------------

   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Exception_Count := Exception_Count + 1;
      Glib.Test.Message
        ("exception in handler: "
         & Ada.Exceptions.Exception_Information (Occurrence));
   end Record_Exception;

   --------------------
   -- Cb_Begin_Print --
   --------------------

   procedure Cb_Begin_Print
     (Self    : access Gtk_Print_Operation_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class)
   is
   begin
      Begin_Count := Begin_Count + 1;
      Self.Set_N_Pages (Pages);
      --  Emit paginate with GTK's live print context to exercise the boolean
      --  return marshaller without opening a print dialog.
      if Exercise_Paginate then
         Emit_Paginate (Self, Context);
      end if;
      if Exercise_Preview_Interface then
         declare
            Setup : constant Gtk_Page_Setup := Gtk_Page_Setup_New;
         begin
            --  A live context lets the interface's signal callbacks validate
            --  the exact objects delivered through their C closures.
            Emit_Preview_Interface_Signals (Self, Context, Setup);
            Setup.Unref;
         end;
      end if;
   end Cb_Begin_Print;

   procedure Cb_Request_Page_Setup
     (Self    : access Gtk_Print_Operation_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class;
      Page_Nr : Gint;
      Setup   : not null access Gtk_Page_Setup_Record'Class)
   is
      pragma Unreferenced (Self, Context);
      Paper : Gtk_Paper_Size;
   begin
      Page_Setup_Count := Page_Setup_Count + 1;
      if Page_Nr = 1 then
         Gtk_New (Paper, Paper_Name_A5);
         Setup.Set_Paper_Size_And_Default_Margins (Paper);
         Setup.Set_Orientation (Page_Orientation_Landscape);
         Paper.Free;
      end if;
   end Cb_Request_Page_Setup;

   ------------------
   -- Cb_Draw_Page --
   ------------------

   procedure Cb_Draw_Page
     (Self    : access Gtk_Print_Operation_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class;
      Page_Nr : Gint)
   is
   begin
      Draw_Count := Draw_Count + 1;

      --  The pages are drawn in order, and the context must describe a
      --  non-degenerate page for cairo to render onto.
      Assert_Cmpint_Eq (Page_Nr, Gint (Draw_Count) - 1);
      Assert_Cmpfloat_Gt (Context.Get_Width, 0.0);
      Assert_Cmpfloat_Gt (Context.Get_Height, 0.0);

      --  A PDF export supplies a real print context without a printer. Query
      --  its drawing and text objects on the first page, then replace the
      --  Cairo context with itself at preview DPI to exercise the setter.
      if Page_Nr = 0 then
         declare
            Cr : constant Cairo.Cairo_Context := Context.Get_Cairo_Context;
            Text_Context : constant Pango.Context.Pango_Context :=
              Context.Create_Pango_Context;
            Layout : constant Pango.Layout.Pango_Layout :=
              Context.Create_Pango_Layout;
            Fontmap : constant Pango.Font_Map.Pango_Font_Map :=
              Context.Get_Pango_Fontmap;
            Preview_Surface : constant Cairo.Cairo_Surface :=
              Cairo.Image_Surface.Create
                (Cairo.Image_Surface.Cairo_Format_ARGB32, 64, 64);
            Preview_Cr : constant Cairo.Cairo_Context :=
              Cairo.Create (Preview_Surface);
            Top, Bottom, Left, Right : Gdouble;
         begin
            --  The print context owns a live Cairo context and positive DPI.
            Assert_True (Cairo.Status (Cr) = Cairo.Cairo_Status_Success);
            Assert_Cmpfloat_Gt (Context.Get_Dpi_X, 0.0);
            Assert_Cmpfloat_Gt (Context.Get_Dpi_Y, 0.0);

            --  The created text objects are owned by this callback; the font
            --  map belongs to the print context and remains borrowed.
            Assert_Nonnull (Get_Object (Text_Context));
            Assert_Nonnull (Get_Object (Layout));
            Assert_Nonnull (Get_Object (Fontmap));
            Pango.Layout.Set_Text (Layout, "GtkAda print context");
            Assert_Cmpstr_Eq
              (Pango.Layout.Get_Text (Layout), "GtkAda print context");

            --  A PDF surface has no physical printer hard margins.
            Assert_False
              (Context.Get_Hard_Margins
                 (Top, Bottom, Left, Right));

            --  The setter replaces the old Cairo reference, so pass a
            --  separately owned context rather than its borrowed old handle.
            Context.Set_Cairo_Context (Preview_Cr, 144.0, 120.0);
            Assert_True
              (Cairo.Status (Context.Get_Cairo_Context)
               = Cairo.Cairo_Status_Success);
            Assert_Cmpfloat_Eq (Context.Get_Dpi_X, 144.0);
            Assert_Cmpfloat_Eq (Context.Get_Dpi_Y, 120.0);

            Text_Context.Unref;
            Layout.Unref;
            Cairo.Destroy (Preview_Cr);
            Cairo.Surface.Destroy (Preview_Surface);
         end;
      end if;

      if Page_Nr = 1 then
         Assert_True
           (Context.Get_Page_Setup.Get_Orientation = Page_Orientation_Landscape);
         Assert_Cmpstr_Eq
           (Context.Get_Page_Setup.Get_Paper_Size.Get_Name, Paper_Name_A5);
         Assert_Cmpfloat_Eq
           (Context.Get_Page_Setup.Get_Paper_Width (Mm), 210.0);
         Assert_Cmpfloat_Eq
           (Context.Get_Page_Setup.Get_Paper_Height (Mm), 148.0);
      end if;
      if Defer_Drawing_Enabled and then Deferred_Draw_Count < 2 then
         --  Deferred drawing is completed by the application, as required by
         --  Set_Defer_Drawing. Both calls belong inside draw-page.
         Self.Set_Defer_Drawing;
         Self.Draw_Page_Finish;
         Deferred_Draw_Count := Deferred_Draw_Count + 1;
      end if;
   end Cb_Draw_Page;

   ------------------
   -- Cb_End_Print --
   ------------------

   procedure Cb_End_Print
     (Self    : access Gtk_Print_Operation_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class)
   is
      pragma Unreferenced (Self, Context);
   begin
      End_Count := End_Count + 1;
   end Cb_End_Print;

   -------------
   -- Cb_Done --
   -------------

   procedure Cb_Done
     (Self   : access Gtk_Print_Operation_Record'Class;
      Result : Gtk_Print_Operation_Result)
   is
      pragma Unreferenced (Self);
   begin
      Done_Count := Done_Count + 1;
      Last_Result := Result;
   end Cb_Done;

   -----------------------
   -- Cb_Status_Changed --
   -----------------------

   procedure Cb_Status_Changed
     (Self : access Gtk_Print_Operation_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Setup_Count := Setup_Count + 1;
   end Cb_Status_Changed;

   ------------------------
   -- Slot signal adapters --
   ------------------------

   procedure Cb_Begin_Print_Slot
     (Self    : access GObject_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class)
   is
      --  The slot adapter must pass a live context and the operation object.
      pragma Unreferenced (Context);
   begin
      Assert_True (Self /= null);
      Slot_Begin_Count := Slot_Begin_Count + 1;
   end Cb_Begin_Print_Slot;

   procedure Cb_Draw_Page_Slot
     (Self    : access GObject_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class;
      Page_Nr : Gint)
   is
      --  Preserve the page number so the integer parameter marshaller runs.
      pragma Unreferenced (Context);
   begin
      Assert_True (Self /= null);
      Assert_True (Page_Nr >= 0);
      Slot_Draw_Count := Slot_Draw_Count + 1;
   end Cb_Draw_Page_Slot;

   procedure Cb_Request_Page_Setup_Slot
     (Self    : access GObject_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class;
      Page_Nr : Gint;
      Setup   : not null access Gtk_Page_Setup_Record'Class)
   is
      --  GTK supplies both the current page and its effective page setup.
      pragma Unreferenced (Context, Setup);
   begin
      Assert_True (Self /= null);
      Assert_True (Page_Nr >= 0);
      Slot_Page_Setup_Count := Slot_Page_Setup_Count + 1;
   end Cb_Request_Page_Setup_Slot;

   procedure Cb_End_Print_Slot
     (Self    : access GObject_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class)
   is
      --  End-print carries the same context type as begin-print.
      pragma Unreferenced (Context);
   begin
      Assert_True (Self /= null);
      Slot_End_Count := Slot_End_Count + 1;
   end Cb_End_Print_Slot;

   procedure Cb_Done_Slot
     (Self   : access GObject_Record'Class;
      Result : Gtk_Print_Operation_Result)
   is
      --  The successful export result must survive enum-value conversion.
   begin
      Assert_True (Self /= null);
      Assert_True (Result = Print_Operation_Result_Apply);
      Slot_Done_Count := Slot_Done_Count + 1;
   end Cb_Done_Slot;

   procedure Cb_Status_Changed_Slot (Self : access GObject_Record'Class) is
      --  Status changes have no signal arguments beyond the emitter.
   begin
      Assert_True (Self /= null);
      Slot_Status_Count := Slot_Status_Count + 1;
   end Cb_Status_Changed_Slot;

   function Cb_Paginate
     (Self    : access Gtk_Print_Operation_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class)
      return Boolean
   is
      --  True says the document has finished pagination.
      pragma Unreferenced (Self, Context);
   begin
      return True;
   end Cb_Paginate;

   function Cb_Paginate_Slot
     (Self    : access GObject_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class)
      return Boolean
   is
      --  The operation's boolean accumulator stops after a successful handler.
      pragma Unreferenced (Context);
   begin
      Assert_True (Self /= null);
      Slot_Paginate_Count := Slot_Paginate_Count + 1;
      return True;
   end Cb_Paginate_Slot;

   procedure Cb_Custom_Widget_Apply
     (Self   : access Gtk_Print_Operation_Record'Class;
      Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      --  The signal must deliver the custom widget supplied to the dialog.
      pragma Unreferenced (Self);
   begin
      Assert_True (Gtk.Widget.Gtk_Widget (Widget) = Gtk.Widget.Gtk_Widget (Custom));
   end Cb_Custom_Widget_Apply;

   procedure Cb_Custom_Widget_Apply_Slot
     (Self   : access GObject_Record'Class;
      Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      --  Check the generic-object adapter receives the same widget.
   begin
      Assert_True (Self /= null);
      Assert_True (Gtk.Widget.Gtk_Widget (Widget) = Gtk.Widget.Gtk_Widget (Custom));
      Slot_Custom_Apply_Count := Slot_Custom_Apply_Count + 1;
   end Cb_Custom_Widget_Apply_Slot;

   procedure Cb_Update_Custom_Widget
     (Self     : access Gtk_Print_Operation_Record'Class;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Setup    : not null access Gtk_Page_Setup_Record'Class;
      Settings : not null access Gtk_Print_Settings_Record'Class)
   is
      --  All three custom-widget update arguments must be unmarshalled.
      pragma Unreferenced (Self);
   begin
      Assert_True (Gtk.Widget.Gtk_Widget (Widget) = Gtk.Widget.Gtk_Widget (Custom));
      Assert_True (Gtk_Page_Setup (Setup) /= null);
      Assert_True (Gtk_Print_Settings (Settings) /= null);
   end Cb_Update_Custom_Widget;

   procedure Cb_Update_Custom_Widget_Slot
     (Self     : access GObject_Record'Class;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Setup    : not null access Gtk_Page_Setup_Record'Class;
      Settings : not null access Gtk_Print_Settings_Record'Class)
   is
      --  The slot form uses the same GTK arguments and a generic emitter.
   begin
      Assert_True (Self /= null);
      Assert_True (Gtk.Widget.Gtk_Widget (Widget) = Gtk.Widget.Gtk_Widget (Custom));
      Assert_True (Gtk_Page_Setup (Setup) /= null);
      Assert_True (Gtk_Print_Settings (Settings) /= null);
      Slot_Update_Count := Slot_Update_Count + 1;
   end Cb_Update_Custom_Widget_Slot;

   function Cb_Custom_Widget_Slot
     (Self : access GObject_Record'Class) return Glib.Object.GObject
   is
      --  Returning a widget also exercises the GObject result marshaller.
   begin
      Assert_True (Self /= null);
      Slot_Create_Count := Slot_Create_Count + 1;
      return Glib.Object.GObject (Custom);
   end Cb_Custom_Widget_Slot;

   function Cb_Preview
     (Self    : access Gtk_Print_Operation_Record'Class;
      Preview : Gtk.Print_Operation_Preview.Gtk_Print_Operation_Preview;
      Context : not null access Gtk_Print_Context_Record'Class;
      Parent  : access Gtk.Window.Gtk_Window_Record'Class) return Boolean
   is
      --  Preview callbacks are connected here but need a native print backend.
      pragma Unreferenced (Self, Preview, Context, Parent);
   begin
      return False;
   end Cb_Preview;

   function Cb_Preview_Slot
     (Self    : access GObject_Record'Class;
      Preview : Gtk.Print_Operation_Preview.Gtk_Print_Operation_Preview;
      Context : not null access Gtk_Print_Context_Record'Class;
      Parent  : access Gtk.Window.Gtk_Window_Record'Class) return Boolean
   is
      --  Keep the GObject-first preview callback type compile-checked as well.
      pragma Unreferenced (Self, Preview, Context, Parent);
   begin
      return False;
   end Cb_Preview_Slot;

   ------------------------------
   -- Preview interface signals --
   ------------------------------

   procedure Cb_Preview_Ready
     (Self    : Gtk.Print_Operation_Preview.Gtk_Print_Operation_Preview;
      Context : not null access Gtk_Print_Context_Record'Class)
   is
   begin
      --  The typed callback must receive the interface and the live context.
      Assert_True (Gtk.Print_Operation."-" (Self) /= null);
      Assert_Cmpfloat_Gt (Context.Get_Width, 0.0);
      Preview_Ready_Count := Preview_Ready_Count + 1;
      if Raise_Preview_Callbacks then
         --  The generated marshaller must catch and report user exceptions.
         raise Program_Error with "preview ready callback test exception";
      end if;
   end Cb_Preview_Ready;

   procedure Cb_Preview_Ready_Slot
     (Self    : access GObject_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class)
   is
   begin
      --  The slot form receives its emitter as a generic GObject.
      Assert_True (Self /= null);
      Assert_Cmpfloat_Gt (Context.Get_Width, 0.0);
      Preview_Ready_Slot_Count := Preview_Ready_Slot_Count + 1;
      if Raise_Preview_Callbacks then
         --  Exercise the GObject-first marshaller's exception handler too.
         raise Program_Error with "preview ready slot test exception";
      end if;
   end Cb_Preview_Ready_Slot;

   procedure Cb_Preview_Got_Page_Size
     (Self       : Gtk.Print_Operation_Preview.Gtk_Print_Operation_Preview;
      Context    : not null access Gtk_Print_Context_Record'Class;
      Page_Setup : not null access Gtk_Page_Setup_Record'Class)
   is
   begin
      --  Both object arguments are borrowed from GTK for the callback.
      Assert_True (Gtk.Print_Operation."-" (Self) /= null);
      Assert_Cmpfloat_Gt (Context.Get_Height, 0.0);
      Assert_Cmpfloat_Gt (Page_Setup.Get_Paper_Width (Mm), 0.0);
      Preview_Size_Count := Preview_Size_Count + 1;
      if Raise_Preview_Callbacks then
         --  A callback exception must not escape through GTK's signal emitter.
         raise Program_Error with "preview page-size callback test exception";
      end if;
   end Cb_Preview_Got_Page_Size;

   procedure Cb_Preview_Got_Page_Size_Slot
     (Self       : access GObject_Record'Class;
      Context    : not null access Gtk_Print_Context_Record'Class;
      Page_Setup : not null access Gtk_Page_Setup_Record'Class)
   is
   begin
      --  Check the slot adapter preserves both non-null signal arguments.
      Assert_True (Self /= null);
      Assert_Cmpfloat_Gt (Context.Get_Height, 0.0);
      Assert_Cmpfloat_Gt (Page_Setup.Get_Paper_Height (Mm), 0.0);
      Preview_Size_Slot_Count := Preview_Size_Slot_Count + 1;
      if Raise_Preview_Callbacks then
         --  Exercise the slot page-size marshaller's catch-all path.
         raise Program_Error with "preview page-size slot test exception";
      end if;
   end Cb_Preview_Got_Page_Size_Slot;

   -----------------------------
   -- Headless signal emission --
   -----------------------------

   procedure Emit_Paginate
     (Op      : not null access Gtk_Print_Operation_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class)
   is
      Name   : Chars_Ptr := New_String ("paginate");
      Id     : constant Guint := G_Signal_Lookup (Name, Gtk.Print_Operation.Get_Type);
      Params : GValue_Array (1 .. 2);
      Ret    : GValue;
   begin
      --  Signals with return values need a GValue initialized to the C type.
      Free (Name);
      Assert_Cmpuint_Gt (Id, 0);
      Init (Params (1), Gtk.Print_Operation.Get_Type);
      Set_Object (Params (1), Op);
      Init (Params (2), Gtk.Print_Context.Get_Type);
      Set_Object (Params (2), Context);
      Init (Ret, GType_Boolean);
      G_Signal_Emitv (Params'Address, Id, 0, Ret'Address);
      Assert_True (Get_Boolean (Ret));
      Unset (Ret);
      Unset (Params (2));
      Unset (Params (1));
   end Emit_Paginate;

   procedure Emit_Custom_Widget_Apply
     (Op     : not null access Gtk_Print_Operation_Record'Class;
      Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Name   : Chars_Ptr := New_String ("custom-widget-apply");
      Id     : constant Guint := G_Signal_Lookup (Name, Gtk.Print_Operation.Get_Type);
      Params : GValue_Array (1 .. 2);
   begin
      --  Match the signal's widget argument type, not the base GObject type.
      Free (Name);
      Assert_Cmpuint_Gt (Id, 0);
      Init (Params (1), Gtk.Print_Operation.Get_Type);
      Set_Object (Params (1), Op);
      Init (Params (2), Gtk.Widget.Get_Type);
      Set_Object (Params (2), Widget);
      G_Signal_Emitv (Params'Address, Id, 0, System.Null_Address);
      Unset (Params (2));
      Unset (Params (1));
   end Emit_Custom_Widget_Apply;

   procedure Emit_Update_Custom_Widget
     (Op       : not null access Gtk_Print_Operation_Record'Class;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Setup    : not null access Gtk_Page_Setup_Record'Class;
      Settings : not null access Gtk_Print_Settings_Record'Class)
   is
      Name   : Chars_Ptr := New_String ("update-custom-widget");
      Id     : constant Guint := G_Signal_Lookup (Name, Gtk.Print_Operation.Get_Type);
      Params : GValue_Array (1 .. 4);
   begin
      --  The update signal carries widget, page setup, and print settings.
      Free (Name);
      Assert_Cmpuint_Gt (Id, 0);
      Init (Params (1), Gtk.Print_Operation.Get_Type);
      Set_Object (Params (1), Op);
      Init (Params (2), Gtk.Widget.Get_Type);
      Set_Object (Params (2), Widget);
      Init (Params (3), Gtk.Page_Setup.Get_Type);
      Set_Object (Params (3), Setup);
      Init (Params (4), Gtk.Print_Settings.Get_Type);
      Set_Object (Params (4), Settings);
      G_Signal_Emitv (Params'Address, Id, 0, System.Null_Address);
      Unset (Params (4));
      Unset (Params (3));
      Unset (Params (2));
      Unset (Params (1));
   end Emit_Update_Custom_Widget;

   procedure Emit_Preview_Interface_Signals
     (Op      : not null access Gtk_Print_Operation_Record'Class;
      Context : not null access Gtk_Print_Context_Record'Class;
      Setup   : not null access Gtk_Page_Setup_Record'Class)
   is
      --  GtkPrintOperation implements this interface, so its live object can
      --  emit the interface signals with the same arguments GTK supplies.
      Ready_Name : Chars_Ptr := New_String ("ready");
      Ready_Id   : constant Guint :=
        G_Signal_Lookup
          (Ready_Name, Gtk.Print_Operation_Preview.Get_Type);
      Size_Name  : Chars_Ptr := New_String ("got-page-size");
      Size_Id    : constant Guint :=
        G_Signal_Lookup
          (Size_Name, Gtk.Print_Operation_Preview.Get_Type);
      Ready_Params : GValue_Array (1 .. 2);
      Size_Params  : GValue_Array (1 .. 3);
      Preview      : constant Gtk.Print_Operation_Preview.Gtk_Print_Operation_Preview :=
        Gtk.Print_Operation."+" (Op);
      Same_Preview : constant Gtk.Print_Operation_Preview.Gtk_Print_Operation_Preview :=
        Gtk.Print_Operation_Preview."+" (Preview);
   begin
      Free (Ready_Name);
      Free (Size_Name);
      Assert_Cmpuint_Gt (Ready_Id, 0);
      Assert_Cmpuint_Gt (Size_Id, 0);

      --  Exercise the interface identity operator and its page-selection API.
      Assert_True (Same_Preview = Preview);
      Assert_True (Gtk.Print_Operation_Preview.Is_Selected (Preview, 0));
      Assert_False (Gtk.Print_Operation_Preview.Is_Selected (Preview, 9));

      --  Both overloaded registration forms use the preview interface itself.
      Gtk.Print_Operation_Preview.On_Ready
        (Preview, Cb_Preview_Ready'Unrestricted_Access);
      Gtk.Print_Operation_Preview.On_Ready
        (Preview, Cb_Preview_Ready_Slot'Unrestricted_Access, Op);
      Gtk.Print_Operation_Preview.On_Got_Page_Size
        (Preview, Cb_Preview_Got_Page_Size'Unrestricted_Access);
      Gtk.Print_Operation_Preview.On_Got_Page_Size
        (Preview, Cb_Preview_Got_Page_Size_Slot'Unrestricted_Access, Op);

      --  GSignal emission keeps this test independent of printer backends and
      --  still enters the generated Ada callback marshallers with real values.
      Init (Ready_Params (1), Gtk.Print_Operation.Get_Type);
      Set_Object (Ready_Params (1), Op);
      Init (Ready_Params (2), Gtk.Print_Context.Get_Type);
      Set_Object (Ready_Params (2), Context);
      G_Signal_Emitv
        (Ready_Params'Address, Ready_Id, 0, System.Null_Address);
      Unset (Ready_Params (2));
      Unset (Ready_Params (1));

      Init (Size_Params (1), Gtk.Print_Operation.Get_Type);
      Set_Object (Size_Params (1), Op);
      Init (Size_Params (2), Gtk.Print_Context.Get_Type);
      Set_Object (Size_Params (2), Context);
      Init (Size_Params (3), Gtk.Page_Setup.Get_Type);
      Set_Object (Size_Params (3), Setup);
      G_Signal_Emitv (Size_Params'Address, Size_Id, 0, System.Null_Address);
      Unset (Size_Params (3));
      Unset (Size_Params (2));
      Unset (Size_Params (1));
   end Emit_Preview_Interface_Signals;

   -------------------------
   -- Cb_No_Custom_Widget --
   -------------------------

   function Cb_No_Custom_Widget
     (Self : access Gtk_Print_Operation_Record'Class)
      return Glib.Object.GObject
   is
      pragma Unreferenced (Self);
   begin
      return null;
   end Cb_No_Custom_Widget;

   ----------------------
   -- Cb_Custom_Widget --
   ----------------------

   function Cb_Custom_Widget
     (Self : access Gtk_Print_Operation_Record'Class)
      return Glib.Object.GObject
   is
      pragma Unreferenced (Self);
   begin
      return Glib.Object.GObject (Custom);
   end Cb_Custom_Widget;

   -------------------------------
   -- Emit_Create_Custom_Widget --
   -------------------------------

   procedure Emit_Create_Custom_Widget
     (Op     : not null access Gtk_Print_Operation_Record'Class;
      Result : out Glib.Object.GObject)
   is
      Name   : Chars_Ptr := New_String ("create-custom-widget");
      Id     : constant Guint :=
        G_Signal_Lookup (Name, Gtk.Print_Operation.Get_Type);
      Params : GValue_Array (1 .. 1);
      Ret    : GValue;
   begin
      Free (Name);
      Assert_Cmpuint_Gt (Id, 0);

      Init (Params (1), Gtk.Print_Operation.Get_Type);
      Set_Object (Params (1), Op);
      Init (Ret, Glib.GType_Object);

      G_Signal_Emitv (Params'Address, Id, 0, Ret'Address);

      Result := Glib.Values.Get_Object (Ret);

      Unset (Ret);
      Unset (Params (1));
   end Emit_Create_Custom_Widget;

   ------------------
   -- Reset_Counts --
   ------------------

   procedure Reset_Counts is
   begin
      Begin_Count := 0;
      Draw_Count := 0;
      Page_Setup_Count := 0;
      End_Count := 0;
      Done_Count := 0;
      Setup_Count := 0;
      Exception_Count := 0;
      Slot_Begin_Count := 0;
      Slot_Draw_Count := 0;
      Slot_Page_Setup_Count := 0;
      Slot_End_Count := 0;
      Slot_Done_Count := 0;
      Slot_Status_Count := 0;
      Slot_Paginate_Count := 0;
      Slot_Custom_Apply_Count := 0;
      Slot_Update_Count := 0;
      Slot_Create_Count := 0;
      Preview_Ready_Count := 0;
      Preview_Ready_Slot_Count := 0;
      Preview_Size_Count := 0;
      Preview_Size_Slot_Count := 0;
      Exercise_Paginate := False;
      Exercise_Preview_Interface := False;
      Raise_Preview_Callbacks := False;
      Defer_Drawing_Enabled := False;
      Deferred_Draw_Count := 0;
      Last_Result := Print_Operation_Result_In_Progress;
   end Reset_Counts;

   ---------------------
   -- Test_Properties --
   ---------------------

   procedure Test_Properties is
      Op       : constant Gtk_Print_Operation := Gtk_Print_Operation_New;
      Settings : constant Gtk_Print_Settings := Gtk_Print_Settings_New;
      Setup    : constant Gtk_Page_Setup := Gtk_Page_Setup_New;
      Via_Proc : Gtk_Print_Operation;
   begin
      --  Gtk_New initializes an allocated Ada wrapper; repeating Initialize
      --  must preserve its already-created GObject.
      Gtk_New (Via_Proc);
      Gtk.Print_Operation.Initialize (Via_Proc);
      Assert_True (Via_Proc.Get_Status = Print_Status_Initial);

      Op.Set_Job_Name ("a job");
      Op.Set_Custom_Tab_Label ("a tab");
      Op.Set_Unit (Points);
      --  Read back the properties to check the setters' public behavior.
      Assert_Cmpstr_Eq
        (Glib.Properties.Get_Property (Op, Job_Name_Property), "a job");
      Assert_Cmpstr_Eq
        (Glib.Properties.Get_Property (Op, Custom_Tab_Label_Property), "a tab");
      Assert_True
        (Gtk.Enums.Gtk_Unit_Properties.Get_Property
           (Op, Gtk.Enums.Gtk_Unit_Properties.Property (Unit_Property)) = Points);

      Op.Set_Allow_Async (True);
      Op.Set_Current_Page (1);
      Op.Set_Show_Progress (True);
      Op.Set_Track_Print_Status (True);
      Op.Set_Use_Full_Page (True);
      --  These settings have no dedicated getter methods in GtkAda.
      Assert_True (Glib.Properties.Get_Property (Op, Allow_Async_Property));
      Assert_Cmpint_Eq
        (Glib.Properties.Int_Properties.Get_Property
           (Op, Glib.Properties.Int_Properties.Property (Current_Page_Property)), 1);
      Assert_True (Glib.Properties.Get_Property (Op, Show_Progress_Property));
      Assert_True (Glib.Properties.Get_Property (Op, Track_Print_Status_Property));
      Assert_True (Glib.Properties.Get_Property (Op, Use_Full_Page_Property));

      Op.Set_Default_Page_Setup (Setup);
      --  The operation returns the configured page setup as a borrowed object.
      Assert_True (Op.Get_Default_Page_Setup = Setup);

      Op.Set_Embed_Page_Setup (True);
      Assert_True (Op.Get_Embed_Page_Setup);
      Op.Set_Embed_Page_Setup (False);
      Assert_False (Op.Get_Embed_Page_Setup);

      Op.Set_Has_Selection (True);
      Assert_True (Op.Get_Has_Selection);

      Op.Set_Support_Selection (True);
      Assert_True (Op.Get_Support_Selection);

      --  Print settings start out unset, and come back as the very object
      --  that was handed over.
      Assert_True (Op.Get_Print_Settings = null);
      Op.Set_Print_Settings (Settings);
      Assert_True (Op.Get_Print_Settings = Settings);

      Assert_True (Op.Get_Status = Print_Status_Initial);
      Assert_False (Op.Is_Finished);
      Assert_True (Print_Error_Quark = Gtk_Print_Error_Domain);
   end Test_Properties;

   ---------------------------------
   -- Test_Additional_Signals --
   ---------------------------------

   procedure Test_Additional_Signals is
      Op       : constant Gtk_Print_Operation := Gtk_Print_Operation_New;
      Setup    : constant Gtk_Page_Setup := Gtk_Page_Setup_New;
      Settings : constant Gtk_Print_Settings := Gtk_Print_Settings_New;
   begin
      Reset_Counts;
      --  These dialog signals are not emitted by the headless export action.
      --  Emit their declared object arguments directly to test both callback
      --  signatures and the GObject-first slot adapters.
      Op.On_Custom_Widget_Apply
        (Cb_Custom_Widget_Apply'Unrestricted_Access);
      Op.On_Custom_Widget_Apply
        (Cb_Custom_Widget_Apply_Slot'Unrestricted_Access, Op);
      Emit_Custom_Widget_Apply (Op, Custom);
      Assert_Cmpint_Eq (Gint (Slot_Custom_Apply_Count), 1);

      Op.On_Update_Custom_Widget
        (Cb_Update_Custom_Widget'Unrestricted_Access);
      Op.On_Update_Custom_Widget
        (Cb_Update_Custom_Widget_Slot'Unrestricted_Access, Op);
      Emit_Update_Custom_Widget (Op, Custom, Setup, Settings);
      Assert_Cmpint_Eq (Gint (Slot_Update_Count), 1);

      --  A preview dialog requires a platform print backend. Connecting both
      --  forms still checks the binding's signal lookup and closure setup.
      Op.On_Preview (Cb_Preview'Unrestricted_Access);
      Op.On_Preview (Cb_Preview_Slot'Unrestricted_Access, Op);
   end Test_Additional_Signals;

   -----------------
   -- Test_Cancel --
   -----------------

   procedure Test_Cancel is
      Op    : constant Gtk_Print_Operation := Gtk_Print_Operation_New;
      Error : GError;
      Res   : Gtk_Print_Operation_Result;

      procedure Cancel_During_Begin
        (Self    : access Gtk_Print_Operation_Record'Class;
         Context : not null access Gtk_Print_Context_Record'Class);

      procedure Cancel_During_Begin
        (Self    : access Gtk_Print_Operation_Record'Class;
         Context : not null access Gtk_Print_Context_Record'Class)
      is
         pragma Unreferenced (Context);
      begin
         Self.Cancel;
      end Cancel_During_Begin;
   begin
      --  Cancellation from begin-print is explicitly supported and avoids
      --  any printer dialog or physical printer.
      Op.Set_Export_Filename ("cancelled.pdf");
      Op.On_Begin_Print (Cancel_During_Begin'Unrestricted_Access);
      Res := Op.Run (Print_Operation_Action_Export, null, Error);
      Assert_True (Res = Print_Operation_Result_Cancel);
      Assert_True (Error = null);
   end Test_Cancel;

   -----------------
   -- Test_Export --
   -----------------

   procedure Test_Export is
      File  : constant String := "exported.pdf";
      Op    : constant Gtk_Print_Operation := Gtk_Print_Operation_New;
      Error : GError;
      Res   : Gtk_Print_Operation_Result;
   begin
      Reset_Counts;

      Op.Set_Export_Filename (File);
      Op.Set_Job_Name ("export");
      Defer_Drawing_Enabled := True;
      Op.On_Begin_Print (Cb_Begin_Print'Unrestricted_Access);
      Op.On_Begin_Print (Cb_Begin_Print_Slot'Unrestricted_Access, Op);
      Op.On_Draw_Page (Cb_Draw_Page'Unrestricted_Access);
      Op.On_Draw_Page (Cb_Draw_Page_Slot'Unrestricted_Access, Op);
      Op.On_Request_Page_Setup
        (Cb_Request_Page_Setup'Unrestricted_Access);
      Op.On_Request_Page_Setup
        (Cb_Request_Page_Setup_Slot'Unrestricted_Access, Op);
      Op.On_End_Print (Cb_End_Print'Unrestricted_Access);
      Op.On_End_Print (Cb_End_Print_Slot'Unrestricted_Access, Op);
      Op.On_Done (Cb_Done'Unrestricted_Access);
      Op.On_Done (Cb_Done_Slot'Unrestricted_Access, Op);
      Op.On_Status_Changed (Cb_Status_Changed'Unrestricted_Access);
      Op.On_Status_Changed (Cb_Status_Changed_Slot'Unrestricted_Access, Op);
      Op.On_Paginate (Cb_Paginate'Unrestricted_Access);
      --  GTK stops after the first successful pagination handler. Keep the
      --  slot connection after the typed handler to cover closure setup too.
      Op.On_Paginate (Cb_Paginate_Slot'Unrestricted_Access, Op);

      Exercise_Paginate := True;
      Res := Op.Run (Print_Operation_Action_Export, null, Error);
      Exercise_Paginate := False;

      Assert_No_Error (Error);
      Assert_True (Res = Print_Operation_Result_Apply);

      Assert_Cmpint_Eq (Gint (Begin_Count), 1);
      Assert_Cmpint_Eq (Gint (Draw_Count), Pages);
      Assert_Cmpint_Eq (Gint (Page_Setup_Count), Pages);
      Assert_Cmpint_Eq (Gint (End_Count), 1);
      Assert_Cmpint_Eq (Gint (Done_Count), 1);
      Assert_Cmpint_Gt (Gint (Setup_Count), 0);
      Assert_Cmpint_Eq (Gint (Slot_Begin_Count), 1);
      Assert_Cmpint_Eq (Gint (Slot_Draw_Count), Pages);
      Assert_Cmpint_Eq (Gint (Slot_Page_Setup_Count), Pages);
      Assert_Cmpint_Eq (Gint (Slot_End_Count), 1);
      Assert_Cmpint_Eq (Gint (Slot_Done_Count), 1);
      Assert_Cmpint_Gt (Gint (Slot_Status_Count), 0);
      Assert_Cmpint_Eq (Gint (Deferred_Draw_Count), Pages);
      Assert_Cmpint_Eq (Gint (Exception_Count), 0);

      --  The result carried by ::done is an enumerated GValue: it must come
      --  through as the value C sent, not as the first enumerator.
      Assert_True (Last_Result = Print_Operation_Result_Apply);

      Assert_Cmpint_Eq (Op.Get_N_Pages_To_Print, Pages);
      Assert_True (Op.Get_Status_String'Length > 0);
      Assert_Cmpint_Eq
        (Glib.Properties.Int_Properties.Get_Property
           (Op, Glib.Properties.Int_Properties.Property (N_Pages_Property)), Pages);

      Assert_True (Ada.Directories.Exists (File));
      Assert_Cmpint_Gt (Gint (Ada.Directories.Size (File)), 0);
   end Test_Export;

   --------------------------------
   -- Test_Preview_Interface --
   --------------------------------

   procedure Test_Preview_Interface is
      --  Export creates a real GtkPrintContext while avoiding a print dialog.
      Op    : constant Gtk_Print_Operation := Gtk_Print_Operation_New;
      Error : GError;
      Res   : Gtk_Print_Operation_Result;
   begin
      Reset_Counts;
      Op.Set_Export_Filename ("preview-interface.pdf");
      Op.On_Begin_Print (Cb_Begin_Print'Unrestricted_Access);

      --  The begin-print callback emits the preview interface signals with
      --  that live context. Each callback raises on purpose to verify that
      --  all four generated marshallers report exceptions instead of letting
      --  Ada exceptions cross GTK's C signal-emission frames.
      Exercise_Preview_Interface := True;
      Raise_Preview_Callbacks := True;
      Res := Op.Run (Print_Operation_Action_Export, null, Error);
      Exercise_Preview_Interface := False;
      Raise_Preview_Callbacks := False;

      Assert_No_Error (Error);
      Assert_True (Res = Print_Operation_Result_Apply);
      Assert_Cmpint_Eq (Gint (Preview_Ready_Count), 1);
      Assert_Cmpint_Eq (Gint (Preview_Ready_Slot_Count), 1);
      Assert_Cmpint_Eq (Gint (Preview_Size_Count), 1);
      Assert_Cmpint_Eq (Gint (Preview_Size_Slot_Count), 1);
      Assert_Cmpint_Eq (Gint (Exception_Count), 4);
   end Test_Preview_Interface;

   -----------------------
   -- Test_Export_Error --
   -----------------------

   procedure Test_Export_Error is
      Op       : constant Gtk_Print_Operation := Gtk_Print_Operation_New;
      Error    : GError;
      Reported : GError;
      Res      : Gtk_Print_Operation_Result;
   begin
      Reset_Counts;

      --  No such directory, so cairo cannot open the output stream.
      Op.Set_Export_Filename ("no-such-directory/exported.pdf");
      Op.On_Begin_Print (Cb_Begin_Print'Unrestricted_Access);
      Op.On_Draw_Page (Cb_Draw_Page'Unrestricted_Access);
      Op.On_Done (Cb_Done'Unrestricted_Access);

      Res := Op.Run (Print_Operation_Action_Export, null, Error);

      Assert_True (Res = Print_Operation_Result_Error);
      Assert_True (Error /= null);

      --  Get_Error reports the same failure, as documented for a result of
      --  Print_Operation_Result_Error.
      Op.Get_Error (Reported);
      Assert_True (Reported /= null);
      Assert_True (Get_Domain (Reported) = Print_Error_Quark);
      Error_Free (Error);
      Error_Free (Reported);

      Assert_Cmpint_Eq (Gint (Done_Count), 1);
      Assert_True (Last_Result = Print_Operation_Result_Error);
      Assert_Cmpint_Eq (Gint (Exception_Count), 0);
   end Test_Export_Error;

   ------------------------
   -- Test_Custom_Widget --
   ------------------------

   procedure Test_Custom_Widget is
      Result : Glib.Object.GObject;
   begin
      Reset_Counts;

      --  A handler that declines by returning NULL must leave the GValue
      --  empty rather than raise Constraint_Error inside the marshaller.
      declare
         Op : constant Gtk_Print_Operation := Gtk_Print_Operation_New;
      begin
         Op.On_Create_Custom_Widget (Cb_No_Custom_Widget'Unrestricted_Access);
         Emit_Create_Custom_Widget (Op, Result);
         Assert_True (Result = null);
         Assert_Cmpint_Eq (Gint (Exception_Count), 0);
      end;

      --  A handler that does supply a widget must have it reach the GValue.
      declare
         Op : constant Gtk_Print_Operation := Gtk_Print_Operation_New;
      begin
         Op.On_Create_Custom_Widget (Cb_Custom_Widget'Unrestricted_Access);
         Emit_Create_Custom_Widget (Op, Result);
         Assert_True (Result = Glib.Object.GObject (Custom));
         Assert_Cmpint_Eq (Gint (Exception_Count), 0);
      end;

      --  The GObject-first callback may also return the supplied widget.
      declare
         Op : constant Gtk_Print_Operation := Gtk_Print_Operation_New;
      begin
         Op.On_Create_Custom_Widget
           (Cb_Custom_Widget_Slot'Unrestricted_Access, Op);
         Emit_Create_Custom_Widget (Op, Result);
         Assert_True (Result = Glib.Object.GObject (Custom));
         Assert_Cmpint_Eq (Gint (Slot_Create_Count), 1);
         Assert_Cmpint_Eq (Gint (Exception_Count), 0);
      end;
   end Test_Custom_Widget;

   Label : Gtk_Label;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Gtkada.Bindings.Set_On_Exception (Record_Exception'Unrestricted_Access);

   Gtk_New (Custom, Orientation_Vertical, 0);
   Gtk_New (Label, "Custom option");
   Custom.Append (Label);
   Custom.Ref_Sink;

   Glib.Test.Add_Func
     ("/print-operation/page-setup-dialogs",
      Test_Page_Setup_Dialogs'Unrestricted_Access);

   Glib.Test.Add_Func
     ("/print-operation/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/print-operation/additional-signals",
      Test_Additional_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/print-operation/cancel", Test_Cancel'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/print-operation/export", Test_Export'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/print-operation/preview-interface",
      Test_Preview_Interface'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/print-operation/export-error", Test_Export_Error'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/print-operation/custom-widget",
      Test_Custom_Widget'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Print_Operation;
