--  Unit tests for Gtk.Editable_Label's constructors, editable interface,
--  accessibility methods, and conversions to the interfaces it implements.
--  There is no corresponding C EditableLabel test to port.

with Ada.Command_Line;
with Interfaces.C;
with Glib;                use Glib;
with Glib.Object;         use Glib.Object;
with Glib.Properties;     use Glib.Properties;
with Glib.Properties.Creation;
with Glib.Test;           use Glib.Test;
with Glib.Types;
with Glib.Values;
with Gtk.Accessible;      use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Constraint_Target;
with Gtk.Editable;
with Gtk.Editable_Label;  use Gtk.Editable_Label;
with Gtk.Enums;
with Gtk.Main;

procedure Main is
   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gtk.Buildable.Gtk_Buildable;
   use type Gtk.Constraint_Target.Gtk_Constraint_Target;
   use type Gtk.Editable.Gtk_Editable;
   use type Gtk.Editable_Label.Gtk_Editable_Label;
   use type Interfaces.C.C_float;

   --  Observations for the GObject slot callback overloads of GtkEditable.
   Changed_Slot_Calls : Natural := 0;
   Insert_Slot_Calls  : Natural := 0;
   Delete_Slot_Calls  : Natural := 0;
   Insert_Slot_Text   : UTF8_String (1 .. 8) := (others => ' ');
   Insert_Slot_Length : Gint := -1;
   Insert_Slot_Pos    : Gint := -1;
   Delete_Slot_Start  : Gint := -1;
   Delete_Slot_End    : Gint := -1;

   procedure Test_Constructors
   with Convention => C;

   procedure Test_Editing
   with Convention => C;

   procedure Test_Accessibility_And_Interfaces
   with Convention => C;

   procedure Test_Editable_Interface_Wrappers
   with Convention => C;

   --  The slot overloads receive the slot object instead of the editable.
   procedure On_Changed_Slot
     (Self : access GObject_Record'Class);
   procedure On_Delete_Slot
     (Self      : access GObject_Record'Class;
      Start_Pos : Gint;
      End_Pos   : Gint);
   procedure On_Insert_Slot
     (Self     : access GObject_Record'Class;
      Text     : UTF8_String;
      Length   : Gint;
      Position : access Gint);

   ----------------------
   -- Test_Constructors --
   ----------------------

   procedure Test_Constructors is
      By_Function : constant Gtk_Editable_Label :=
        Gtk_Editable_Label_New ("function constructor");
      By_Procedure : Gtk_Editable_Label;
      By_Initialize : constant Gtk_Editable_Label :=
        new Gtk_Editable_Label_Record;
   begin
      By_Function.Ref_Sink;
      Gtk_New (By_Procedure, "procedure constructor");
      By_Procedure.Ref_Sink;

      --  Initialize is also public for callers that allocate the Ada record
      --  directly. A second call must leave the existing GTK object intact.
      Gtk.Editable_Label.Initialize (By_Initialize, "initialized once");
      By_Initialize.Ref_Sink;
      Gtk.Editable_Label.Initialize (By_Initialize, "must be ignored");

      Assert_Cmpstr_Eq (By_Function.Get_Text, "function constructor");
      Assert_Cmpstr_Eq (By_Procedure.Get_Text, "procedure constructor");
      Assert_Cmpstr_Eq (By_Initialize.Get_Text, "initialized once");
      Assert_True (Gtk.Editable_Label.Get_Type /= 0);

      By_Function.Unref;
      By_Procedure.Unref;
      By_Initialize.Unref;
   end Test_Constructors;

   -----------------
   -- Test_Editing --
   -----------------

   procedure Test_Editing is
      Label : constant Gtk_Editable_Label := Gtk_Editable_Label_New ("start");
      Position : Gint := 0;
      Start_Pos, End_Pos : Gint;
      Has_Selection : Boolean;
   begin
      Label.Ref_Sink;

      --  Text methods use character positions; insertion reports the updated
      --  cursor position, and selection bounds identify the selected slice.
      Assert_Cmpstr_Eq (Label.Get_Text, "start");
      Label.Set_Text ("café");
      Assert_Cmpstr_Eq (Label.Get_Text, "café");
      Assert_Cmpstr_Eq (Label.Get_Chars (1, 3), "af");
      Label.Insert_Text ("!x", 1, Position);
      Assert_Cmpint_Eq (Position, 1);
      Assert_Cmpstr_Eq (Label.Get_Text, "!café");

      Label.Set_Position (3);
      Assert_Cmpint_Eq (Label.Get_Position, 3);
      Label.Select_Region (1, 3);
      Label.Get_Selection_Bounds (Start_Pos, End_Pos, Has_Selection);
      Assert_True (Has_Selection);
      Assert_Cmpint_Eq (Start_Pos, 1);
      Assert_Cmpint_Eq (End_Pos, 3);
      Assert_Cmpstr_Eq (Label.Get_Chars (Start_Pos, End_Pos), "ca");

      --  Deleting the selected characters and a following range checks both
      --  GtkEditable deletion wrappers and the resulting text.
      Label.Delete_Selection;
      Assert_Cmpstr_Eq (Label.Get_Text, "!fé");
      Label.Delete_Text (1, 2);
      Assert_Cmpstr_Eq (Label.Get_Text, "!é");

      --  GtkEditableLabel exposes edit mode through both a method and a
      --  property. Stop_Editing is exercised for both commit choices.
      Assert_False (Label.Get_Editing);
      Assert_False (Get_Property (Label, Editing_Property));
      Label.Start_Editing;
      Assert_True (Label.Get_Editing);
      Assert_True (Get_Property (Label, Editing_Property));
      Assert_True (Label.Get_Delegate /= Gtk.Editable.Null_Gtk_Editable);
      Label.Set_Text ("committed");
      Label.Stop_Editing (True);
      Assert_False (Label.Get_Editing);
      Assert_Cmpstr_Eq (Label.Get_Text, "committed");

      Label.Start_Editing;
      Label.Set_Text ("discarded");
      Label.Stop_Editing (False);
      Assert_False (Label.Get_Editing);
      Assert_Cmpstr_Eq (Label.Get_Text, "committed");

      --  Editable and undo settings are ordinary widget properties and can
      --  be checked without simulating keyboard input.
      Assert_True (Label.Get_Editable);
      Label.Set_Editable (False);
      Assert_False (Label.Get_Editable);
      Label.Set_Editable (True);
      Assert_True (Label.Get_Editable);
      Label.Set_Enable_Undo (False);
      Assert_False (Label.Get_Enable_Undo);
      Label.Set_Enable_Undo (True);
      Assert_True (Label.Get_Enable_Undo);

      --  Width hints and horizontal alignment round-trip their configured
      --  values through the GtkEditable accessors.
      Label.Set_Width_Chars (8);
      Assert_Cmpint_Eq (Label.Get_Width_Chars, 8);
      Label.Set_Max_Width_Chars (20);
      Assert_Cmpint_Eq (Label.Get_Max_Width_Chars, 20);
      Label.Set_Alignment (0.25);
      Assert_True (abs (Label.Get_Alignment - 0.25) < 0.001);

      Label.Unref;
   end Test_Editing;

   -------------------------------------------
   -- Test_Accessibility_And_Interfaces --
   -------------------------------------------

   procedure Test_Accessibility_And_Interfaces is
      Label : constant Gtk_Editable_Label := Gtk_Editable_Label_New ("a label");
      Accessible : Gtk_Accessible;
      Buildable : Gtk.Buildable.Gtk_Buildable;
      Target : Gtk.Constraint_Target.Gtk_Constraint_Target;
      Editable : Gtk.Editable.Gtk_Editable;
      X, Y, Width, Height : Gint;
   begin
      Label.Ref_Sink;

      --  Round trips exercise both directions of every generated interface
      --  conversion implemented by GtkEditableLabel.
      Accessible := Gtk.Editable_Label."+" (Label);
      Buildable := Gtk.Editable_Label."+" (Label);
      Target := Gtk.Editable_Label."+" (Label);
      Editable := Gtk.Editable_Label."+" (Label);
      Assert_True (Gtk.Editable_Label."-" (Accessible) = Label);
      Assert_True (Gtk.Editable_Label."-" (Buildable) = Label);
      Assert_True (Gtk.Editable_Label."-" (Target) = Label);
      Assert_True (Gtk.Editable_Label."-" (Editable) = Label);

      --  The widget keeps GTK's generic role and exposes an accessible child
      --  even before it is rooted.
      Assert_True (Label.Get_Accessible_Role = Accessible_Role_Generic);
      Assert_True (Label.Get_Accessible_Parent = Null_Gtk_Accessible);
      Assert_True (Label.Get_First_Accessible_Child /= Null_Gtk_Accessible);
      Assert_True (Label.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      Assert_Cmpstr_Eq (Label.Get_Accessible_Id, "");
      Assert_True (Label.Get_At_Context = null);
      Assert_False (Label.Get_Bounds (X, Y, Width, Height));
      Assert_True
        (Label.Delegate_Get_Accessible_Platform_State
           (Accessible_Platform_State_Focusable)
         = Label.Get_Platform_State (Accessible_Platform_State_Focusable));

      --  The reset and announcement API has no query for the AT announcement
      --  stream. These reset calls check the wrappers with valid GTK enums.
      Label.Announce ("Editable label ready", Accessible_Announcement_Priority_Low);
      Label.Reset_Property (Accessible_Property_Label);
      Label.Reset_Relation (Accessible_Relation_Described_By);
      Label.Reset_State (Accessible_State_Busy);
      Label.Update_Platform_State (Accessible_Platform_State_Focusable);

      --  Null values clear the optional parent and sibling relationships.
      Label.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Label.Get_Accessible_Parent = Null_Gtk_Accessible);
      Label.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (Label.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      Label.Unref;
   end Test_Accessibility_And_Interfaces;

   --------------------------------------
   -- On_Changed_Slot and signal slots --
   --------------------------------------

   procedure On_Changed_Slot
     (Self : access GObject_Record'Class) is
   begin
      Changed_Slot_Calls := Changed_Slot_Calls + 1;
   end On_Changed_Slot;

   procedure On_Delete_Slot
     (Self      : access GObject_Record'Class;
      Start_Pos : Gint;
      End_Pos   : Gint) is
   begin
      Delete_Slot_Calls := Delete_Slot_Calls + 1;
      Delete_Slot_Start := Start_Pos;
      Delete_Slot_End := End_Pos;
   end On_Delete_Slot;

   procedure On_Insert_Slot
     (Self     : access GObject_Record'Class;
      Text     : UTF8_String;
      Length   : Gint;
      Position : access Gint) is
   begin
      Insert_Slot_Calls := Insert_Slot_Calls + 1;
      Insert_Slot_Text := (others => ' ');
      Insert_Slot_Text (1 .. Text'Length) := Text;
      Insert_Slot_Length := Length;
      Insert_Slot_Pos := Position.all;
   end On_Insert_Slot;

   --------------------------------------
   -- Test_Editable_Interface_Wrappers --
   --------------------------------------

   procedure Test_Editable_Interface_Wrappers is
      Label : constant Gtk_Editable_Label := Gtk_Editable_Label_New ("ab");
      Editable : Gtk.Editable.Gtk_Editable;
      Position : Gint := 1;
      Start_Pos, End_Pos : Gint;
      Has_Selection : Boolean;
      Class : constant GObject_Class :=
        Glib.Types.Class_Ref (Gtk.Editable_Label.Get_Type);
      Props : constant Param_Spec_Array := Class_List_Properties (Class);
      Text_Pspec : Glib.Param_Spec := null;
      Value : Glib.Values.GValue;
   begin
      Label.Ref_Sink;

      --  GtkEditableLabel reserves property IDs 1 for "editing" and 2 as
      --  the first GtkEditable property. Find the installed text spec to
      --  pass the metadata expected by GTK's delegate-property helpers.
      for Prop of Props loop
         if Glib.Properties.Creation.Pspec_Name (Prop) = "text" then
            Text_Pspec := Prop;
         end if;
      end loop;
      Assert_True (Text_Pspec /= null);

      --  The delegate helpers are intended for get_property/set_property
      --  implementations; this editable label uses that same delegation.
      Glib.Values.Init (Value, Glib.GType_String);
      Assert_True
        (Gtk.Editable.Delegate_Get_Property
           (Label, 2, Value, Text_Pspec));
      Assert_Cmpstr_Eq (Glib.Values.Get_String (Value), "ab");
      Glib.Values.Set_String (Value, "from property helper");
      Assert_True
        (Gtk.Editable.Delegate_Set_Property
           (Label, 2, Value, Text_Pspec));
      Assert_Cmpstr_Eq
        (Gtk.Editable.Get_Text (Gtk.Editable_Label."+" (Label)),
         "from property helper");
      Glib.Values.Unset (Value);
      Glib.Types.Class_Unref (Gtk.Editable_Label.Get_Type);

      --  Dispatch through GtkEditable itself so these calls cover its UTF-8
      --  conversions and out-parameter handling, rather than label wrappers.
      Editable := Gtk.Editable_Label."+" (Label);
      Gtk.Editable.Set_Text (Editable, "ab");
      Editable := Gtk.Editable."+" (Editable);
      Assert_Cmpstr_Eq (Gtk.Editable.Get_Text (Editable), "ab");
      Assert_Cmpstr_Eq (Gtk.Editable.Get_Chars (Editable, 1), "b");
      Gtk.Editable.Get_Selection_Bounds
        (Editable, Start_Pos, End_Pos, Has_Selection);
      Assert_False (Has_Selection);

      Gtk.Editable.Set_Text (Editable, "ab");
      Assert_Cmpstr_Eq (Gtk.Editable.Get_Text (Editable), "ab");
      Assert_True (Gtk.Editable.Get_Editable (Editable));
      Gtk.Editable.Set_Editable (Editable, False);
      Assert_False (Gtk.Editable.Get_Editable (Editable));
      Gtk.Editable.Set_Editable (Editable, True);
      Assert_True (Gtk.Editable.Get_Editable (Editable));
      Assert_True (Gtk.Editable.Get_Enable_Undo (Editable));
      Gtk.Editable.Set_Enable_Undo (Editable, False);
      Assert_False (Gtk.Editable.Get_Enable_Undo (Editable));
      Gtk.Editable.Set_Enable_Undo (Editable, True);
      Assert_True (Gtk.Editable.Get_Enable_Undo (Editable));

      --  Each slot signal is emitted by a real text edit and its arguments
      --  are checked after the callback returns to the test.
      Changed_Slot_Calls := 0;
      Insert_Slot_Calls := 0;
      Delete_Slot_Calls := 0;
      Gtk.Editable.On_Changed
        (Editable, On_Changed_Slot'Unrestricted_Access, Label);
      Gtk.Editable.On_Insert_Text
        (Editable, On_Insert_Slot'Unrestricted_Access, Label);
      Gtk.Editable.On_Delete_Text
        (Editable, On_Delete_Slot'Unrestricted_Access, Label);

      Gtk.Editable.Insert_Text (Editable, "X", 1, Position);
      Assert_Cmpstr_Eq (Gtk.Editable.Get_Text (Editable), "aXb");
      Assert_Cmpint_Eq (Position, 2);
      Assert_Cmpuint_Eq (Guint (Insert_Slot_Calls), 1);
      Assert_Cmpstr_Eq (Insert_Slot_Text (1 .. 1), "X");
      Assert_Cmpint_Eq (Insert_Slot_Length, 1);
      Assert_Cmpint_Eq (Insert_Slot_Pos, 1);
      Assert_Cmpuint_Eq (Guint (Changed_Slot_Calls), 1);

      Gtk.Editable.Delete_Text (Editable, 1, 2);
      Assert_Cmpstr_Eq (Gtk.Editable.Get_Text (Editable), "ab");
      Assert_Cmpuint_Eq (Guint (Delete_Slot_Calls), 1);
      Assert_Cmpint_Eq (Delete_Slot_Start, 1);
      Assert_Cmpint_Eq (Delete_Slot_End, 2);
      Assert_Cmpuint_Eq (Guint (Changed_Slot_Calls), 2);

      Gtk.Editable.Select_Region (Editable, 0, 1);
      Gtk.Editable.Get_Selection_Bounds
        (Editable, Start_Pos, End_Pos, Has_Selection);
      Assert_True (Has_Selection);
      Assert_Cmpint_Eq (Start_Pos, 0);
      Assert_Cmpint_Eq (End_Pos, 1);
      Assert_Cmpstr_Eq (Gtk.Editable.Get_Chars (Editable, 0, 1), "a");

      --  The convenience overload appends a UTF-8 NUL and forwards the byte
      --  count to the full Insert_Text wrapper.
      Position := 2;
      Gtk.Editable.Insert_Text (Editable, "!", Position);
      Assert_Cmpstr_Eq (Gtk.Editable.Get_Text (Editable), "ab!");
      Assert_Cmpint_Eq (Position, 3);

      Label.Unref;
   end Test_Editable_Interface_Wrappers;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/gtk-editable-label/constructors", Test_Constructors'Unrestricted_Access);
   Add_Func ("/gtk-editable-label/editing", Test_Editing'Unrestricted_Access);
   Add_Func
     ("/gtk-editable-label/accessibility-and-interfaces",
      Test_Accessibility_And_Interfaces'Unrestricted_Access);
   Add_Func
     ("/gtk-editable-label/editable-interface-wrappers",
      Test_Editable_Interface_Wrappers'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
