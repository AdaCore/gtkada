--  Unit test for Gtk.Search_Entry.
--
--  GTK's own testsuite has no searchentry.c to port, so this exercises the
--  widget's constructors, properties, accessibility API, signals and the
--  editable operations it inherits from Gtk.Editable.

with Ada.Command_Line;
with Ada.Exceptions;
with Glib;             use Glib;
with Glib.Main;        use Glib.Main;
with Glib.Object;      use Glib.Object;
with Glib.Test;        use Glib.Test;
with Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Button;       use Gtk.Button;
with Gtk.Editable;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Main;
with Gtk.Search_Entry; use Gtk.Search_Entry;
with Gtk.Widget;       use Gtk.Widget;
with Gtkada.Bindings;
with Gtkada.Types;
with Interfaces.C;
with System;

procedure Search_Entry is

   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Accessible.Gtk_Accessible_Role;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gtk.Editable.Gtk_Editable;
   use type Gtk.Search_Entry.Gtk_Search_Entry;
   use type Gtk.Widget.Gtk_Widget;
   use type Interfaces.C.C_float;
   use type System.Address;

   Changed : Natural := 0;
   pragma Volatile (Changed);
   --  Incremented by the "search-changed" handler, which is dispatched from
   --  Main_Context_Iteration below.

   Done : Boolean := False;
   pragma Volatile (Done);
   --  Set from a timeout callback dispatched by Main_Context_Iteration

   Typed_Signal_Calls : Natural := 0;
   Slot_Signal_Calls  : Natural := 0;
   Exception_Calls    : Natural := 0;
   Last_Slot          : System.Address := System.Null_Address;

   procedure On_Changed (Self : access Gtk_Search_Entry_Record'Class);
   procedure On_Typed_Signal (Self : access Gtk_Search_Entry_Record'Class);
   procedure On_Slot_Signal (Self : access GObject_Record'Class);
   procedure On_Raising_Typed_Signal
     (Self : access Gtk_Search_Entry_Record'Class);
   procedure On_Raising_Slot_Signal (Self : access GObject_Record'Class);
   procedure Capture_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);
   procedure Emit_Void_Signal
     (Object : System.Address;
      Name   : Gtkada.Types.Chars_Ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";
   function Stop return Boolean;

   procedure Test_Constructors with Convention => C;
   procedure Test_Accessors with Convention => C;
   procedure Test_Key_Capture_Widget with Convention => C;
   procedure Test_Editable with Convention => C;
   procedure Test_Accessibility with Convention => C;
   procedure Test_Search_Changed with Convention => C;
   procedure Test_Signals with Convention => C;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed (Self : access Gtk_Search_Entry_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Changed := Changed + 1;
   end On_Changed;

   ----------------------
   -- On_Typed_Signal --
   ----------------------

   procedure On_Typed_Signal (Self : access Gtk_Search_Entry_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Typed_Signal_Calls := Typed_Signal_Calls + 1;
   end On_Typed_Signal;

   ---------------------
   -- On_Slot_Signal --
   ---------------------

   procedure On_Slot_Signal (Self : access GObject_Record'Class) is
   begin
      Slot_Signal_Calls := Slot_Signal_Calls + 1;
      Last_Slot := Get_Object (Self);
   end On_Slot_Signal;

   --------------------------------
   -- On_Raising_Typed_Signal --
   --------------------------------

   procedure On_Raising_Typed_Signal
     (Self : access Gtk_Search_Entry_Record'Class) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "expected typed search-entry callback exception";
   end On_Raising_Typed_Signal;

   -------------------------------
   -- On_Raising_Slot_Signal --
   -------------------------------

   procedure On_Raising_Slot_Signal (Self : access GObject_Record'Class) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "expected slot search-entry callback exception";
   end On_Raising_Slot_Signal;

   -----------------------
   -- Capture_Exception --
   -----------------------

   procedure Capture_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Exception_Calls := Exception_Calls + 1;
   end Capture_Exception;

   ----------
   -- Stop --
   ----------

   function Stop return Boolean is
   begin
      Done := True;
      Wakeup (null);
      return False;
   end Stop;

   -----------------------
   -- Test_Constructors --
   -----------------------

   procedure Test_Constructors is
      By_Procedure : Gtk_Search_Entry;
      By_Initialize : constant Gtk_Search_Entry :=
        new Gtk_Search_Entry_Record;
   begin
      --  Exercise the procedure constructor and the public initializer.
      Gtk_New (By_Procedure);
      Gtk.Search_Entry.Initialize (By_Procedure);
      Gtk.Search_Entry.Initialize (By_Initialize);
      Gtk.Search_Entry.Initialize (By_Initialize);

      --  Reinitializing an existing object must preserve its current text.
      By_Procedure.Set_Text ("created");
      Gtk.Search_Entry.Initialize (By_Procedure);
      Assert_Cmpstr_Eq (By_Procedure.Get_Text, "created");
      Assert_Cmpstr_Eq (By_Initialize.Get_Text, "");
      Assert_True (Gtk.Search_Entry.Get_Type /= 0);
   end Test_Constructors;

   --------------------
   -- Test_Accessors --
   --------------------

   procedure Test_Accessors is
      E : constant Gtk_Search_Entry := Gtk_Search_Entry_New;
   begin
      --  placeholder-text is nullable on both sides: the unset value comes
      --  back as the empty string rather than raising.
      Assert_Cmpstr_Eq (E.Get_Placeholder_Text, "");
      E.Set_Placeholder_Text ("Search");
      Assert_Cmpstr_Eq (E.Get_Placeholder_Text, "Search");
      E.Set_Placeholder_Text ("");
      Assert_Cmpstr_Eq (E.Get_Placeholder_Text, "");

      E.Set_Search_Delay (300);
      Assert_Cmpuint_Eq (E.Get_Search_Delay, 300);

      E.Set_Input_Purpose (Input_Purpose_Alpha);
      Assert_True (E.Get_Input_Purpose = Input_Purpose_Alpha);

      E.Set_Input_Hints (Input_Hint_No_Spellcheck);
      Assert_True (E.Get_Input_Hints = Input_Hint_No_Spellcheck);
   end Test_Accessors;

   ------------------------------
   -- Test_Key_Capture_Widget --
   ------------------------------

   procedure Test_Key_Capture_Widget is
      E      : constant Gtk_Search_Entry := Gtk_Search_Entry_New;
      Button : constant Gtk_Button := Gtk_Button_New_With_Label;
   begin
      Assert_True (E.Get_Key_Capture_Widget = null);

      E.Set_Key_Capture_Widget (Button);
      Assert_True (E.Get_Key_Capture_Widget = Gtk_Widget (Button));

      --  The property is nullable, so the capture widget can be unset again.
      E.Set_Key_Capture_Widget (null);
      Assert_True (E.Get_Key_Capture_Widget = null);
   end Test_Key_Capture_Widget;

   -------------------
   -- Test_Editable --
   -------------------

   procedure Test_Editable is
      E             : constant Gtk_Search_Entry := Gtk_Search_Entry_New;
      Start_Pos     : Gint;
      End_Pos       : Gint;
      Has_Selection : Boolean;
      Insert_Pos    : Gint := 5;
   begin
      E.Set_Text ("helloworld");
      Assert_Cmpstr_Eq (E.Get_Text, "helloworld");
      Assert_Cmpstr_Eq (E.Get_Chars (0, 5), "hello");
      Assert_Cmpstr_Eq (E.Get_Chars (5), "world");

      --  Cursor and alignment accessors round-trip values through GtkEditable.
      E.Set_Position (2);
      Assert_Cmpint_Eq (E.Get_Position, 2);
      E.Set_Alignment (Interfaces.C.C_float (0.25));
      Assert_True (E.Get_Alignment = Interfaces.C.C_float (0.25));

      --  Insertion updates the caller's character position and the text.
      E.Insert_Text ("-", 1, Insert_Pos);
      Assert_Cmpint_Eq (Insert_Pos, 6);
      Assert_Cmpstr_Eq (E.Get_Text, "hello-world");

      E.Select_Region (0, 5);
      E.Get_Selection_Bounds (Start_Pos, End_Pos, Has_Selection);
      Assert_True (Has_Selection);
      Assert_Cmpint_Eq (Glib.Gint'Pos (Start_Pos), 0);
      Assert_Cmpint_Eq (Glib.Gint'Pos (End_Pos), 5);

      --  Deleting a selected range and an explicit range both update text.
      E.Delete_Selection;
      Assert_Cmpstr_Eq (E.Get_Text, "-world");
      E.Delete_Text (1, 4);
      Assert_Cmpstr_Eq (E.Get_Text, "-ld");
      E.Get_Selection_Bounds (Start_Pos, End_Pos, Has_Selection);
      Assert_False (Has_Selection);

      E.Set_Editable (False);
      Assert_False (E.Get_Editable);
      E.Set_Editable (True);
      Assert_True (E.Get_Editable);

      E.Set_Width_Chars (12);
      Assert_Cmpint_Eq (Glib.Gint'Pos (E.Get_Width_Chars), 12);

      --  The maximum width and undo settings are independent editable options.
      E.Set_Max_Width_Chars (24);
      Assert_Cmpint_Eq (E.Get_Max_Width_Chars, 24);
      E.Set_Enable_Undo (True);
      Assert_True (E.Get_Enable_Undo);
      E.Set_Enable_Undo (False);
      Assert_False (E.Get_Enable_Undo);
   end Test_Editable;

   -------------------------
   -- Test_Accessibility --
   -------------------------

   procedure Test_Accessibility is
      E : constant Gtk_Search_Entry := Gtk_Search_Entry_New;
      X, Y, Width, Height : Gint;
      Accessible_Id : constant UTF8_String := E.Get_Accessible_Id;
      Delegate : constant Gtk.Editable.Gtk_Editable := E.Get_Delegate;
   begin
      --  Search entries identify their search-box role and are editable
      --  delegates; an unrooted widget has no context, bounds or sibling.
      Assert_True (E.Get_Accessible_Role = Gtk.Accessible.Accessible_Role_Search_Box);
      Assert_True (E.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (E.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (E.Get_First_Accessible_Child /= Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (E.Get_At_Context = null);
      Assert_False (E.Get_Bounds (X, Y, Width, Height));
      Assert_True (Accessible_Id'Length < 256);
      Assert_True (Delegate /= Gtk.Editable.Null_Gtk_Editable);
      Assert_True (E.Get_Editable);

      --  The delegate and widget accessibility queries should agree for a
      --  platform state before any explicit update has been requested.
      Assert_True
        (E.Delegate_Get_Accessible_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focusable)
         = E.Get_Platform_State
             (Gtk.Accessible.Accessible_Platform_State_Focusable));

      --  Accessibility mutation calls use valid enum values and null parent
      --  links, so they do not depend on a screen reader or realized window.
      E.Announce ("Search ready", Gtk.Accessible.Accessible_Announcement_Priority_Low);
      E.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      E.Reset_Relation (Gtk.Accessible.Accessible_Relation_Labelled_By);
      E.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      E.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (E.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      E.Update_Next_Accessible_Sibling (Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (E.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      E.Update_Platform_State (Gtk.Accessible.Accessible_Platform_State_Focused);
   end Test_Accessibility;

   -------------------------
   -- Test_Search_Changed --
   -------------------------

   procedure Test_Search_Changed is
      E  : constant Gtk_Search_Entry := Gtk_Search_Entry_New;
      Id : G_Source_Id;
      pragma Unreferenced (Id);
   begin
      --  "search-changed" is emitted only after search-delay milliseconds
      --  have elapsed without further input, so the main loop has to be
      --  spun for it to arrive at all.
      E.Set_Search_Delay (50);
      E.On_Search_Changed (On_Changed'Unrestricted_Access);

      Changed := 0;
      E.Set_Text ("needle");

      Done := False;
      Id := Timeout_Add (1000, Stop'Unrestricted_Access);

      while not Done and then Changed = 0 loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => True);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;

      Assert_Cmpuint_Eq (Guint (Changed), 1);
   end Test_Search_Changed;

   -------------------
   -- Test_Signals --
   -------------------

   procedure Test_Signals is
      E             : constant Gtk_Search_Entry := Gtk_Search_Entry_New;
      Exceptional_E : constant Gtk_Search_Entry := Gtk_Search_Entry_New;
      Slot          : constant Gtk_Button := Gtk_Button_New_With_Label ("callback slot");

      --  The helper emits each signal by name, avoiding keyboard input and
      --  covering both GtkAda callback adapters deterministically.
      procedure Emit (Object : Gtk_Search_Entry; Signal : String) is
         Name : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String (Signal);
      begin
         Emit_Void_Signal (Get_Object (Object), Name);
         Gtkada.Types.Free (Name);
      end Emit;
   begin
      E.Ref_Sink;
      Exceptional_E.Ref_Sink;
      Slot.Ref_Sink;
      Typed_Signal_Calls := 0;
      Slot_Signal_Calls := 0;
      Exception_Calls := 0;
      Last_Slot := System.Null_Address;

      --  Each signal supports a typed callback and a callback with a slot
      --  object. The latter must receive the supplied slot in its closure.
      E.On_Activate (On_Typed_Signal'Unrestricted_Access);
      E.On_Activate (On_Slot_Signal'Unrestricted_Access, Slot);
      E.On_Next_Match (On_Typed_Signal'Unrestricted_Access);
      E.On_Next_Match (On_Slot_Signal'Unrestricted_Access, Slot);
      E.On_Previous_Match (On_Typed_Signal'Unrestricted_Access);
      E.On_Previous_Match (On_Slot_Signal'Unrestricted_Access, Slot);
      E.On_Search_Changed (On_Typed_Signal'Unrestricted_Access);
      E.On_Search_Changed (On_Slot_Signal'Unrestricted_Access, Slot);
      E.On_Search_Started (On_Typed_Signal'Unrestricted_Access);
      E.On_Search_Started (On_Slot_Signal'Unrestricted_Access, Slot);
      E.On_Stop_Search (On_Typed_Signal'Unrestricted_Access);
      E.On_Stop_Search (On_Slot_Signal'Unrestricted_Access, Slot);

      Emit (E, "activate");
      Emit (E, "next-match");
      Emit (E, "previous-match");
      Emit (E, "search-changed");
      Emit (E, "search-started");
      Emit (E, "stop-search");

      Assert_Cmpuint_Eq (Guint (Typed_Signal_Calls), 6);
      Assert_Cmpuint_Eq (Guint (Slot_Signal_Calls), 6);
      Assert_True (Last_Slot = Get_Object (Slot));

      --  The generated marshallers must catch exceptions from both callback
      --  styles and pass them to GtkAda's handler instead of unwinding into C.
      Gtkada.Bindings.Set_On_Exception
        (Capture_Exception'Unrestricted_Access);
      Exceptional_E.On_Activate
        (On_Raising_Typed_Signal'Unrestricted_Access);
      Exceptional_E.On_Activate
        (On_Raising_Slot_Signal'Unrestricted_Access, Slot);
      Emit (Exceptional_E, "activate");
      Assert_Cmpuint_Eq (Guint (Exception_Calls), 2);

      Slot.Unref;
      Exceptional_E.Unref;
      E.Unref;
   end Test_Signals;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/searchentry/constructors", Test_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/searchentry/accessors", Test_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/searchentry/key-capture-widget",
      Test_Key_Capture_Widget'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/searchentry/editable", Test_Editable'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/searchentry/accessibility", Test_Accessibility'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/searchentry/search-changed", Test_Search_Changed'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/searchentry/signals", Test_Signals'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Search_Entry;
