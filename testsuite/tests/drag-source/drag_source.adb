--  Tests Gtk.Drag_Source and Gdk.Drag constructors, methods, and signal bindings.
--  GTK has no C test for GtkDragSource, so these focused cases exercise the
--  public GtkAda API directly. A GDK drag is started on the test window so
--  the drag-related signal callbacks receive a valid GdkDrag object. The
--  Gdk.Drag checks also exercise its accessors and signal marshallers.
with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces.C;
with Interfaces.C.Strings;
with System;                use type System.Address;
with Glib;                  use Glib;
with Glib.Error;            use Glib.Error;
with Glib.Object;           use Glib.Object;
with Glib.Test;             use Glib.Test;
with Glib.Values;           use Glib.Values;
with Gdk.Content_Provider;  use Gdk.Content_Provider;
with Gdk.Content_Formats;
with Gdk.Display;
with Gdk.Drag;
with Gdk.Paintable;
with Gdk.Seat;
with Gtk.Drag_Source;       use Gtk.Drag_Source;
with Gtk.Label;             use Gtk.Label;
with Gtk.Main;
with Gtk.Window;
with Gtkada.Bindings;

procedure Drag_Source is

   use type Gdk.Gdk_Device;
   use type Gdk.Gdk_Display;
   use type Gdk.Gdk_Seat;
   use type Gdk.Gdk_Surface;
   use type Gdk.Drag.Drag_Action;
   use type Gdk.Drag.Drag_Cancel_Reason;
   use type Gdk.Drag.Gdk_Drag;

   --  Shared prepare state lets the tests check the values passed through
   --  each generated signal marshaller.
   Prepared : Natural := 0;
   Last_X, Last_Y : Gdouble := 0.0;
   Provider : Gdk_Content_Provider;

   --  The drag-signal callbacks record their arguments for assertions after
   --  g_signal_emit_by_name has returned to Ada.
   Begin_Calls, Slot_Begin_Calls : Natural := 0;
   Cancel_Calls, Slot_Cancel_Calls : Natural := 0;
   End_Calls, Slot_End_Calls : Natural := 0;
   Slot_Prepare_Calls : Natural := 0;
   Last_Drag : System.Address := System.Null_Address;
   Slot_Object : System.Address := System.Null_Address;
   Last_Reason : Gdk.Drag.Drag_Cancel_Reason := Gdk.Drag.No_Target;
   Last_Delete_Data, Slot_Delete_Data : Boolean := False;
   Slot_X, Slot_Y : Gdouble := 0.0;
   Signal_Icon : Gdk.Paintable.Gdk_Paintable := Gdk.Paintable.Null_Gdk_Paintable;
   Signal_Drag : Gdk.Drag.Gdk_Drag;
   Gdk_Cancel_Calls, Gdk_Cancel_Slot_Calls : Natural := 0;
   Gdk_Finished_Calls, Gdk_Finished_Slot_Calls : Natural := 0;
   Gdk_Drop_Calls, Gdk_Drop_Slot_Calls : Natural := 0;
   Gdk_Callback_Exception_Calls : Natural := 0;
   Raise_In_Gdk_Callbacks : Boolean := False;
   Signal_Slot : Gtk_Label;

   --  These imports expose GTK's signal emitter to the test so signal
   --  callbacks can be checked without synthesizing physical mouse input.
   procedure Emit_Prepare
     (Instance : System.Address;
      Name     : Interfaces.C.Strings.chars_ptr;
      X, Y     : Gdouble;
      Result   : access System.Address)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";
   --  The variadic tail of "prepare" is (double, double, GdkContentProvider **).

   procedure Emit_Drag_Begin
     (Instance : System.Address;
      Name     : Interfaces.C.Strings.chars_ptr;
      Drag     : System.Address)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";
   --  The variadic tail is a GdkDrag pointer.

   procedure Emit_Drag_Cancel
     (Instance : System.Address;
      Name     : Interfaces.C.Strings.chars_ptr;
      Drag     : System.Address;
      Reason   : Gdk.Drag.Drag_Cancel_Reason;
      Result   : access Glib.Gboolean)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";
   --  The tail is (GdkDrag pointer, GdkDragCancelReason, gboolean *).

   procedure Emit_Drag_End
     (Instance    : System.Address;
      Name        : Interfaces.C.Strings.chars_ptr;
      Drag        : System.Address;
      Delete_Data : Glib.Gboolean)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";
   --  The variadic tail is (GdkDrag pointer, gboolean).

   procedure Emit_Gdk_Drag_Cancel
     (Instance : System.Address;
      Name     : Interfaces.C.Strings.chars_ptr;
      Reason   : Interfaces.C.int)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";
   --  The variadic tail is a GdkDragCancelReason enum.

   procedure Emit_Gdk_Drag_Void
     (Instance : System.Address;
      Name     : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";
   --  Used for GdkDrag's no-argument completion signals.

   function On_Prepare
     (Self : access Gtk_Drag_Source_Record'Class;
      X, Y : Gdouble) return Gdk_Content_Provider;
   function On_Prepare_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      X, Y : Gdouble) return Gdk_Content_Provider;

   procedure On_Begin
     (Self : access Gtk_Drag_Source_Record'Class;
      Drag : not null access Gdk.Drag.Gdk_Drag_Record'Class);
   procedure On_Begin_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Drag : not null access Gdk.Drag.Gdk_Drag_Record'Class);
   function On_Cancel
     (Self   : access Gtk_Drag_Source_Record'Class;
      Drag   : not null access Gdk.Drag.Gdk_Drag_Record'Class;
      Reason : Gdk.Drag.Drag_Cancel_Reason) return Boolean;
   function On_Cancel_Slot
     (Self   : access Glib.Object.GObject_Record'Class;
      Drag   : not null access Gdk.Drag.Gdk_Drag_Record'Class;
      Reason : Gdk.Drag.Drag_Cancel_Reason) return Boolean;
   procedure On_End
     (Self        : access Gtk_Drag_Source_Record'Class;
      Drag        : not null access Gdk.Drag.Gdk_Drag_Record'Class;
      Delete_Data : Boolean);
   procedure On_End_Slot
     (Self        : access Glib.Object.GObject_Record'Class;
      Drag        : not null access Gdk.Drag.Gdk_Drag_Record'Class;
      Delete_Data : Boolean);
   procedure On_Gdk_Cancel
     (Self : access Gdk.Drag.Gdk_Drag_Record'Class;
      Reason : Gdk.Drag.Drag_Cancel_Reason);
   procedure On_Gdk_Cancel_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Reason : Gdk.Drag.Drag_Cancel_Reason);
   procedure On_Gdk_Finished
     (Self : access Gdk.Drag.Gdk_Drag_Record'Class);
   procedure On_Gdk_Finished_Slot
     (Self : access Glib.Object.GObject_Record'Class);
   procedure On_Gdk_Drop
     (Self : access Gdk.Drag.Gdk_Drag_Record'Class);
   procedure On_Gdk_Drop_Slot
     (Self : access Glib.Object.GObject_Record'Class);
   procedure Record_Gdk_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Test_Constructors with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Content with Convention => C;
   procedure Test_Icon with Convention => C;
   procedure Test_Prepare with Convention => C;
   procedure Test_Signals with Convention => C;

   ----------------
   -- On_Prepare --
   ----------------

   function On_Prepare
     (Self : access Gtk_Drag_Source_Record'Class;
      X, Y : Gdouble) return Gdk_Content_Provider
   is
      pragma Unreferenced (Self);
   begin
      Prepared := Prepared + 1;
      Last_X   := X;
      Last_Y   := Y;
      return Provider;
   end On_Prepare;

   ---------------------
   -- On_Prepare_Slot --
   ---------------------

   function On_Prepare_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      X, Y : Gdouble) return Gdk_Content_Provider
   is
   begin
      Slot_Prepare_Calls := Slot_Prepare_Calls + 1;
      Slot_Object := Get_Object (Self);
      Slot_X := X;
      Slot_Y := Y;
      return Provider;
   end On_Prepare_Slot;

   ----------------
   -- On_Begin --
   ----------------

   procedure On_Begin
     (Self : access Gtk_Drag_Source_Record'Class;
      Drag : not null access Gdk.Drag.Gdk_Drag_Record'Class)
   is
   begin
      Begin_Calls := Begin_Calls + 1;
      Last_Drag := Get_Object (Drag);
      --  The drag-begin signal is the documented point for changing the icon.
      Self.Set_Icon (Signal_Icon, 2, 3);
   end On_Begin;

   ---------------------
   -- On_Begin_Slot --
   ---------------------

   procedure On_Begin_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Drag : not null access Gdk.Drag.Gdk_Drag_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Slot_Begin_Calls := Slot_Begin_Calls + 1;
      Last_Drag := Get_Object (Drag);
   end On_Begin_Slot;

   -----------------
   -- On_Cancel --
   -----------------

   function On_Cancel
     (Self   : access Gtk_Drag_Source_Record'Class;
      Drag   : not null access Gdk.Drag.Gdk_Drag_Record'Class;
      Reason : Gdk.Drag.Drag_Cancel_Reason) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      Cancel_Calls := Cancel_Calls + 1;
      Last_Drag := Get_Object (Drag);
      Last_Reason := Reason;
      return True;
   end On_Cancel;

   ----------------------
   -- On_Cancel_Slot --
   ----------------------

   function On_Cancel_Slot
     (Self   : access Glib.Object.GObject_Record'Class;
      Drag   : not null access Gdk.Drag.Gdk_Drag_Record'Class;
      Reason : Gdk.Drag.Drag_Cancel_Reason) return Boolean
   is
   begin
      Slot_Cancel_Calls := Slot_Cancel_Calls + 1;
      Slot_Object := Get_Object (Self);
      Last_Drag := Get_Object (Drag);
      Last_Reason := Reason;
      return True;
   end On_Cancel_Slot;

   --------------
   -- On_End --
   --------------

   procedure On_End
     (Self        : access Gtk_Drag_Source_Record'Class;
      Drag        : not null access Gdk.Drag.Gdk_Drag_Record'Class;
      Delete_Data : Boolean)
   is
      pragma Unreferenced (Self);
   begin
      End_Calls := End_Calls + 1;
      Last_Drag := Get_Object (Drag);
      Last_Delete_Data := Delete_Data;
   end On_End;

   -------------------
   -- On_End_Slot --
   -------------------

   procedure On_End_Slot
     (Self        : access Glib.Object.GObject_Record'Class;
      Drag        : not null access Gdk.Drag.Gdk_Drag_Record'Class;
      Delete_Data : Boolean)
   is
   begin
      Slot_End_Calls := Slot_End_Calls + 1;
      Slot_Object := Get_Object (Self);
      Last_Drag := Get_Object (Drag);
      Slot_Delete_Data := Delete_Data;
   end On_End_Slot;

   --  The callbacks verify that typed handlers receive the emitting drag,
   --  while slot handlers receive the explicitly connected GtkLabel object.
   --  A flag lets the test also exercise GtkAda's exception containment.
   procedure On_Gdk_Cancel
     (Self : access Gdk.Drag.Gdk_Drag_Record'Class;
      Reason : Gdk.Drag.Drag_Cancel_Reason) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Signal_Drag));
      Last_Reason := Reason;
      if Raise_In_Gdk_Callbacks then
         raise Program_Error with "expected Gdk.Drag cancel callback exception";
      end if;
      Gdk_Cancel_Calls := Gdk_Cancel_Calls + 1;
   end On_Gdk_Cancel;

   procedure On_Gdk_Cancel_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Reason : Gdk.Drag.Drag_Cancel_Reason) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Signal_Slot));
      Last_Reason := Reason;
      if Raise_In_Gdk_Callbacks then
         raise Program_Error with "expected Gdk.Drag cancel slot exception";
      end if;
      Gdk_Cancel_Slot_Calls := Gdk_Cancel_Slot_Calls + 1;
   end On_Gdk_Cancel_Slot;

   procedure On_Gdk_Finished
     (Self : access Gdk.Drag.Gdk_Drag_Record'Class) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Signal_Drag));
      if Raise_In_Gdk_Callbacks then
         raise Program_Error with "expected Gdk.Drag dnd-finished exception";
      end if;
      Gdk_Finished_Calls := Gdk_Finished_Calls + 1;
   end On_Gdk_Finished;

   procedure On_Gdk_Finished_Slot
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Signal_Slot));
      if Raise_In_Gdk_Callbacks then
         raise Program_Error with "expected Gdk.Drag dnd-finished slot exception";
      end if;
      Gdk_Finished_Slot_Calls := Gdk_Finished_Slot_Calls + 1;
   end On_Gdk_Finished_Slot;

   procedure On_Gdk_Drop
     (Self : access Gdk.Drag.Gdk_Drag_Record'Class) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Signal_Drag));
      if Raise_In_Gdk_Callbacks then
         raise Program_Error with "expected Gdk.Drag drop-performed exception";
      end if;
      Gdk_Drop_Calls := Gdk_Drop_Calls + 1;
   end On_Gdk_Drop;

   procedure On_Gdk_Drop_Slot
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Signal_Slot));
      if Raise_In_Gdk_Callbacks then
         raise Program_Error with "expected Gdk.Drag drop-performed slot exception";
      end if;
      Gdk_Drop_Slot_Calls := Gdk_Drop_Slot_Calls + 1;
   end On_Gdk_Drop_Slot;

   --  The marshallers must catch callback exceptions before control returns
   --  through GLib's C signal machinery.
   procedure Record_Gdk_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Gdk_Callback_Exception_Calls := Gdk_Callback_Exception_Calls + 1;
   end Record_Gdk_Callback_Exception;

   ----------------------
   -- Test_Constructors --
   ----------------------

   procedure Test_Constructors is
      By_Initialize : Gtk_Drag_Source;
      By_Function   : constant Gtk_Drag_Source := Gtk_Drag_Source_New;
      Original_Object : System.Address := System.Null_Address;
   begin
      --  Both GtkAda construction forms return initialized GObjects.
      Gtk_New (By_Initialize);
      Original_Object := Get_Object (By_Initialize);
      Assert_True (Original_Object /= System.Null_Address);
      Assert_True (Get_Object (By_Function) /= System.Null_Address);

      --  Initialize is deliberately idempotent for an already-created object.
      Gtk.Drag_Source.Initialize (By_Initialize);
      Assert_True (Get_Object (By_Initialize) = Original_Object);

      Unref (By_Initialize);
      Unref (By_Function);
   end Test_Constructors;

   --------------------
   -- Test_Properties --
   --------------------

   procedure Test_Properties is
      S : constant Gtk_Drag_Source := Gtk_Drag_Source_New;
   begin
      --  A fresh source has no provider and no drag in progress.
      Assert_True (S.Get_Content = null);
      Assert_True (S.Get_Drag = null);

      --  Actions round-trip through the GtkDragSource property.
      S.Set_Actions (Gdk.Drag.Gdk_Action_Copy or Gdk.Drag.Gdk_Action_Move);
      Assert_True
        (S.Get_Actions =
           (Gdk.Drag.Gdk_Action_Copy or Gdk.Drag.Gdk_Action_Move));

      --  Cancelling with no drag in progress is harmless.
      S.Drag_Cancel;
      Unref (S);
   end Test_Properties;

   -----------------
   -- Test_Content --
   -----------------

   procedure Test_Content is
      S     : constant Gtk_Drag_Source := Gtk_Drag_Source_New;
      Value : GValue;
      P     : Gdk_Content_Provider;
      Error : GError;
   begin
      --  Build a provider for a string and transfer it to the source.
      Init_Set_String (Value, "hello");
      P := Gdk_Content_Provider_New_For_Value (Value);
      Unset (Value);
      S.Set_Content (P);
      Assert_True (S.Get_Content = P);

      --  The source keeps its own reference after the caller drops theirs.
      Unref (P);
      Assert_True (S.Get_Content /= null);

      --  The retained provider still returns the value it was made from.
      Init (Value, GType_String);
      Assert_True (S.Get_Content.Get_Value (Value, Error));
      Assert_True (Get_String (Value) = "hello");
      Unset (Value);
      Unref (S);
   end Test_Content;

   ---------------
   -- Test_Icon --
   ---------------

   procedure Test_Icon is
      S : constant Gtk_Drag_Source := Gtk_Drag_Source_New;
      Icon : constant Gdk.Paintable.Gdk_Paintable :=
        Gdk.Paintable.New_Empty (12, 14);
   begin
      --  Empty paintables are valid drag icons and have a stable intrinsic size.
      Assert_Cmpint_Eq
        (Gdk.Paintable.Get_Intrinsic_Width (Icon), 12);
      S.Set_Icon (Icon, 3, 5);

      --  A null paintable selects GTK's default drag icon as documented.
      S.Set_Icon (Gdk.Paintable.Null_Gdk_Paintable, 0, 0);
      Assert_True (Gdk.Paintable.Get_Intrinsic_Height (Icon) = 14);
      Unref (S);
   end Test_Icon;

   ------------------
   -- Test_Prepare --
   ------------------

   procedure Test_Prepare is
      S      : constant Gtk_Drag_Source := Gtk_Drag_Source_New;
      Value  : GValue;
      Result : aliased System.Address := System.Null_Address;
      Name   : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("prepare");
   begin
      --  Connect a typed Ada callback and emit the signal with known pointer
      --  coordinates so both its arguments and return provider can be checked.
      Init_Set_String (Value, "dragged");
      Provider := Gdk_Content_Provider_New_For_Value (Value);
      Unset (Value);
      S.On_Prepare (On_Prepare'Unrestricted_Access);
      Emit_Prepare (Get_Object (S), Name, 3.5, 7.25, Result'Access);
      Interfaces.C.Strings.Free (Name);

      Assert_Cmpint_Eq (Gint (Prepared), 1);
      Assert_True (Last_X = 3.5);
      Assert_True (Last_Y = 7.25);
      Assert_True (Result = Get_Object (Provider));

      Unref (S);
      Unref (Provider);
      Provider := null;
   end Test_Prepare;

   -----------------
   -- Test_Signals --
   -----------------

   procedure Test_Signals is
      Source       : constant Gtk_Drag_Source := Gtk_Drag_Source_New;
      Slot_Source  : constant Gtk_Drag_Source := Gtk_Drag_Source_New;
      Window       : constant Gtk.Window.Gtk_Window := Gtk.Window.Gtk_Window_New;
      Child        : constant Gtk_Label := Gtk_Label_New ("drag surface");
      Slot         : constant Gtk_Label := Gtk_Label_New ("callback slot");
      Display      : constant Gdk.Gdk_Display := Gdk.Display.Get_Default;
      Seat         : constant Gdk.Gdk_Seat := Display.Get_Default_Seat;
      Pointer      : constant Gdk.Gdk_Device := Seat.Get_Pointer;
      Surface      : Gdk.Gdk_Surface;
      Drag         : Gdk.Drag.Gdk_Drag;
      Drag_Source_Surface : Gdk.Gdk_Surface;
      Value        : GValue;
      Cancel_Value : aliased Glib.Gboolean := 0;
      Prepare_Value : aliased System.Address := System.Null_Address;
      Begin_Name   : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("drag-begin");
      Cancel_Name  : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("drag-cancel");
      End_Name     : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("drag-end");
      Prepare_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("prepare");
      Gdk_Cancel_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("cancel");
      Gdk_Finished_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("dnd-finished");
      Gdk_Drop_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("drop-performed");
      Formats : Gdk.Content_Formats.Gdk_Content_Formats;
   begin
      --  Present a small window to obtain a real backend surface and pointer.
      Window.Set_Child (Child);
      Window.Present;
      Surface := Window.Get_Surface;
      Assert_True (Display /= null);
      Assert_True (Seat /= null);
      Assert_True (Pointer /= null);
      Assert_True (Surface /= null);

      --  Begin_Drag supplies a valid GdkDrag for every tested signal signature.
      Init_Set_String (Value, "signal payload");
      Provider := Gdk_Content_Provider_New_For_Value (Value);
      Unset (Value);
      Drag := Gdk.Drag.Begin_Drag
        (Surface, Pointer, Provider, Gdk.Drag.Gdk_Action_Copy, 0.0, 0.0);
      Assert_True (Drag /= null);
      Signal_Drag := Drag;

      --  Check every accessor against the inputs used to begin this drag.
      --  A drag icon surface may not exist on every backend until rendering
      --  starts, so only its documented nullable result is required here.
      Assert_True (Drag.Get_Actions = Gdk.Drag.Gdk_Action_Copy);
      Assert_True (Get_Object (Drag.Get_Content) = Get_Object (Provider));
      Assert_True (Drag.Get_Device = Pointer);
      Assert_True (Drag.Get_Display = Display);
      Assert_True
        (Drag.Get_Drag_Surface = null or else
         Drag.Get_Drag_Surface.Get_Display = Display);
      Formats := Drag.Get_Formats;
      Assert_True
        (Gdk.Content_Formats.Contain_Gtype (Formats, GType_String));
      Assert_True (Drag.Get_Selected_Action = Gdk.Drag.Gdk_Action_None);
      --  The drag reports its originating surface on the same display. The
      --  X11 backend may wrap that surface separately from GtkWindow's wrapper.
      Drag_Source_Surface := Drag.Get_Surface;
      Assert_True (Drag_Source_Surface /= null);
      Assert_True (Drag_Source_Surface.Get_Display = Display);
      Drag.Set_Hotspot (4, 6);

      --  A slot is an ordinary GObject kept alive by this test. The generic
      --  overloads pass that object as the callback's Self parameter.
      --  Labels start with a floating reference, so sink it before unrefing it.
      Glib.Object.Ref_Sink (Glib.Object.GObject (Slot));
      Signal_Slot := Slot;
      Source.Set_Content (Provider);
      Slot_Source.Set_Content (Provider);
      Source.Set_Actions (Gdk.Drag.Gdk_Action_Copy);
      Slot_Source.Set_Actions (Gdk.Drag.Gdk_Action_Copy);
      Signal_Icon := Gdk.Paintable.New_Empty (8, 8);

      --  Each signal is connected once through its widget-specific callback
      --  type and once through the GObject-plus-slot overload.
      Source.On_Drag_Begin (On_Begin'Unrestricted_Access);
      Slot_Source.On_Drag_Begin
        (On_Begin_Slot'Unrestricted_Access, Slot, After => True);
      Source.On_Drag_Cancel (On_Cancel'Unrestricted_Access);
      Slot_Source.On_Drag_Cancel
        (On_Cancel_Slot'Unrestricted_Access, Slot, After => True);
      Source.On_Drag_End (On_End'Unrestricted_Access);
      Slot_Source.On_Drag_End
        (On_End_Slot'Unrestricted_Access, Slot, After => True);
      Slot_Source.On_Prepare
        (On_Prepare_Slot'Unrestricted_Access, Slot, After => False);

      --  Emitting drag-begin checks both callback conversions and the icon
      --  setter used from a normal drag-begin handler.
      Emit_Drag_Begin (Get_Object (Source), Begin_Name, Get_Object (Drag));
      Emit_Drag_Begin
        (Get_Object (Slot_Source), Begin_Name, Get_Object (Drag));
      Assert_Cmpint_Eq (Gint (Begin_Calls), 1);
      Assert_Cmpint_Eq (Gint (Slot_Begin_Calls), 1);
      Assert_True (Last_Drag = Get_Object (Drag));

      --  The boolean-returning cancel signal must preserve both the callback
      --  result and the Drag_Cancel_Reason enum through the GtkAda marshaller.
      Emit_Drag_Cancel
        (Get_Object (Source), Cancel_Name, Get_Object (Drag),
         Gdk.Drag.User_Cancelled, Cancel_Value'Access);
      Assert_True (Cancel_Value /= 0);
      Assert_True (Last_Reason = Gdk.Drag.User_Cancelled);
      Cancel_Value := 0;
      Emit_Drag_Cancel
        (Get_Object (Slot_Source), Cancel_Name, Get_Object (Drag),
         Gdk.Drag.User_Cancelled, Cancel_Value'Access);
      Assert_True (Cancel_Value /= 0);
      Assert_Cmpint_Eq (Gint (Cancel_Calls), 1);
      Assert_Cmpint_Eq (Gint (Slot_Cancel_Calls), 1);
      Assert_True (Slot_Object = Get_Object (Slot));

      --  drag-end carries the delete-data flag, which move actions require
      --  applications to handle after a successful drop.
      Emit_Drag_End
        (Get_Object (Source), End_Name, Get_Object (Drag),
         Glib.Gboolean (1));
      Emit_Drag_End
        (Get_Object (Slot_Source), End_Name, Get_Object (Drag),
         Glib.Gboolean (0));
      Assert_Cmpint_Eq (Gint (End_Calls), 1);
      Assert_Cmpint_Eq (Gint (Slot_End_Calls), 1);
      Assert_True (Last_Delete_Data);
      Assert_True (not Slot_Delete_Data);
      Assert_True (Slot_Object = Get_Object (Slot));

      --  The prepare slot overload returns a provider and receives the exact
      --  coordinates and user-data object selected by GTK's closure machinery.
      Emit_Prepare
        (Get_Object (Slot_Source), Prepare_Name, 11.0, 13.5,
         Prepare_Value'Access);
      Assert_Cmpint_Eq (Gint (Slot_Prepare_Calls), 1);
      Assert_True (Slot_Object = Get_Object (Slot));
      Assert_True (Slot_X = 11.0);
      Assert_True (Slot_Y = 13.5);
      Assert_True (Prepare_Value = Get_Object (Provider));

      --  Exercise both callback overloads for all three Gdk.Drag signals.
      --  Manual emission keeps the test independent of external drag input.
      Drag.On_Cancel (On_Gdk_Cancel'Unrestricted_Access);
      Drag.On_Cancel (On_Gdk_Cancel_Slot'Unrestricted_Access, Slot);
      Drag.On_Dnd_Finished (On_Gdk_Finished'Unrestricted_Access);
      Drag.On_Dnd_Finished (On_Gdk_Finished_Slot'Unrestricted_Access, Slot);
      Drag.On_Drop_Performed (On_Gdk_Drop'Unrestricted_Access);
      Drag.On_Drop_Performed (On_Gdk_Drop_Slot'Unrestricted_Access, Slot);

      Emit_Gdk_Drag_Cancel
        (Get_Object (Drag), Gdk_Cancel_Name,
         Interfaces.C.int
           (Gdk.Drag.Drag_Cancel_Reason'Pos (Gdk.Drag.User_Cancelled)));
      Assert_True (Last_Reason = Gdk.Drag.User_Cancelled);
      Emit_Gdk_Drag_Void (Get_Object (Drag), Gdk_Finished_Name);
      Emit_Gdk_Drag_Void (Get_Object (Drag), Gdk_Drop_Name);
      Assert_Cmpint_Eq (Gint (Gdk_Cancel_Calls), 1);
      Assert_Cmpint_Eq (Gint (Gdk_Cancel_Slot_Calls), 1);
      Assert_Cmpint_Eq (Gint (Gdk_Finished_Calls), 1);
      Assert_Cmpint_Eq (Gint (Gdk_Finished_Slot_Calls), 1);
      Assert_Cmpint_Eq (Gint (Gdk_Drop_Calls), 1);
      Assert_Cmpint_Eq (Gint (Gdk_Drop_Slot_Calls), 1);
      Assert_True (Slot_Object = Get_Object (Slot));

      --  Exercise the exception branch in each of Gdk.Drag's four generated
      --  marshallers: typed and slot callbacks for cancel and no-argument.
      Gtkada.Bindings.Set_On_Exception
        (Record_Gdk_Callback_Exception'Unrestricted_Access);
      Raise_In_Gdk_Callbacks := True;
      Emit_Gdk_Drag_Cancel
        (Get_Object (Drag), Gdk_Cancel_Name,
         Interfaces.C.int
           (Gdk.Drag.Drag_Cancel_Reason'Pos (Gdk.Drag.Error)));
      Emit_Gdk_Drag_Void (Get_Object (Drag), Gdk_Finished_Name);
      Raise_In_Gdk_Callbacks := False;
      Assert_Cmpint_Eq (Gint (Gdk_Callback_Exception_Calls), 4);

      --  Free the strings created for the C variadic signal helper.
      Interfaces.C.Strings.Free (Begin_Name);
      Interfaces.C.Strings.Free (Cancel_Name);
      Interfaces.C.Strings.Free (End_Name);
      Interfaces.C.Strings.Free (Prepare_Name);
      Interfaces.C.Strings.Free (Gdk_Cancel_Name);
      Interfaces.C.Strings.Free (Gdk_Finished_Name);
      Interfaces.C.Strings.Free (Gdk_Drop_Name);

      --  Complete and release the GDK drag before destroying its source window.
      Drag.Drop_Done (False);
      Glib.Object.Unref (Glib.Object.GObject (Drag));
      Window.Destroy;
      Unref (Slot);
      Unref (Source);
      Unref (Slot_Source);
      Unref (Provider);
      Provider := null;
      Signal_Icon := Gdk.Paintable.Null_Gdk_Paintable;
   end Test_Signals;

begin
   --  Glib.Test.Init parses test options; GTK must initialize afterward before
   --  any widget or event controller is constructed.
   Glib.Test.Init;
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/dragsource/constructors", Test_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/dragsource/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/dragsource/content", Test_Content'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/dragsource/icon", Test_Icon'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/dragsource/prepare", Test_Prepare'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/dragsource/signals", Test_Signals'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Drag_Source;
