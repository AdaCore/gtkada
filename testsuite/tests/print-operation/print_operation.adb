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
with Glib;                use Glib;
with Glib.Main;
with Glib.List_Model;     use Glib.List_Model;
with Gtk.Dialog;
with Gtk.Window;
with Glib.Error;          use Glib.Error;
with Glib.Object;         use Glib.Object;
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
with Gtk.Print_Settings;  use Gtk.Print_Settings;
with Gtkada.Bindings;
with Gtkada.Types;        use Gtkada.Types;
with System;

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
      Setup.Unref;
      Settings.Unref;
   end Test_Page_Setup_Dialogs;

   procedure Test_Properties
   with Convention => C;

   procedure Test_Export
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
      pragma Unreferenced (Context);
   begin
      Begin_Count := Begin_Count + 1;
      Self.Set_N_Pages (Pages);
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
      pragma Unreferenced (Self);
   begin
      Draw_Count := Draw_Count + 1;

      --  The pages are drawn in order, and the context must describe a
      --  non-degenerate page for cairo to render onto.
      Assert_Cmpint_Eq (Page_Nr, Gint (Draw_Count) - 1);
      Assert_Cmpfloat_Gt (Context.Get_Width, 0.0);
      Assert_Cmpfloat_Gt (Context.Get_Height, 0.0);
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
      Last_Result := Print_Operation_Result_In_Progress;
   end Reset_Counts;

   ---------------------
   -- Test_Properties --
   ---------------------

   procedure Test_Properties is
      Op       : constant Gtk_Print_Operation := Gtk_Print_Operation_New;
      Settings : constant Gtk_Print_Settings := Gtk_Print_Settings_New;
   begin
      Op.Set_Job_Name ("a job");
      Op.Set_Custom_Tab_Label ("a tab");
      Op.Set_Unit (Points);

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
   end Test_Properties;

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
      Op.On_Begin_Print (Cb_Begin_Print'Unrestricted_Access);
      Op.On_Draw_Page (Cb_Draw_Page'Unrestricted_Access);
      Op.On_Request_Page_Setup
        (Cb_Request_Page_Setup'Unrestricted_Access);
      Op.On_End_Print (Cb_End_Print'Unrestricted_Access);
      Op.On_Done (Cb_Done'Unrestricted_Access);
      Op.On_Status_Changed (Cb_Status_Changed'Unrestricted_Access);

      Res := Op.Run (Print_Operation_Action_Export, null, Error);

      Assert_No_Error (Error);
      Assert_True (Res = Print_Operation_Result_Apply);

      Assert_Cmpint_Eq (Gint (Begin_Count), 1);
      Assert_Cmpint_Eq (Gint (Draw_Count), Pages);
      Assert_Cmpint_Eq (Gint (Page_Setup_Count), Pages);
      Assert_Cmpint_Eq (Gint (End_Count), 1);
      Assert_Cmpint_Eq (Gint (Done_Count), 1);
      Assert_Cmpint_Gt (Gint (Setup_Count), 0);
      Assert_Cmpint_Eq (Gint (Exception_Count), 0);

      --  The result carried by ::done is an enumerated GValue: it must come
      --  through as the value C sent, not as the first enumerator.
      Assert_True (Last_Result = Print_Operation_Result_Apply);

      Assert_Cmpint_Eq (Op.Get_N_Pages_To_Print, Pages);

      Assert_True (Ada.Directories.Exists (File));
      Assert_Cmpint_Gt (Gint (Ada.Directories.Size (File)), 0);
   end Test_Export;

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
     ("/print-operation/export", Test_Export'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/print-operation/export-error", Test_Export_Error'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/print-operation/custom-widget",
      Test_Custom_Widget'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Print_Operation;
