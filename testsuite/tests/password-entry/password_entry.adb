--  GTK has no testsuite/gtk/passwordentry.c to port, so these cases are ours:
--  they cover the surface the Gtk.Password_Entry binding adds, plus a sample
--  of what it inherits from Gtk.Editable.

with Ada.Command_Line;
with Interfaces.C.Strings;
with System;
with Glib;                use Glib;
with Glib.Menu;           use Glib.Menu;
with Glib.Menu_Model;     use Glib.Menu_Model;
with Glib.Object;         use Glib.Object;
with Glib.Properties;     use Glib.Properties;
with Glib.Test;           use Glib.Test;
with Gtk.Editable;        use Gtk.Editable;
with Gtk.Accessible;      use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Main;
with Gtk.Entry_Buffer;      use Gtk.Entry_Buffer;
with Gtk.Password_Entry;  use Gtk.Password_Entry;
with Gtk.Password_Entry_Buffer;

procedure Password_Entry is

   use type Gtk.Atcontext.Gtk_Atcontext;

   Changed : Natural := 0;
   --  Incremented by the "changed" handler

   Activated : Natural := 0;
   --  Incremented by the "activate" handlers

   Last_Activated : GObject := null;
   --  The instance handed to the last "activate" handler

   procedure Emit_By_Name
     (Instance : System.Address; Name : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Activate (E : Gtk_Password_Entry);
   --  Emit "activate" on E. Gtk.Widget.Activate is no help here: the entry
   --  sets no activate signal on its widget class, it relays the "activate"
   --  of its inner text, so the call returns False and emits nothing.

   procedure On_Changed (Self : Gtk_Editable);

   procedure On_Activate_Entry (Self : access Gtk_Password_Entry_Record'Class);
   procedure On_Activate_Object (Self : access GObject_Record'Class);

   procedure Test_Peek_Icon with Convention => C;
   procedure Test_Extra_Menu with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Constructor with Convention => C;
   procedure Test_Buffer_Constructors with Convention => C;
   procedure Test_Editable with Convention => C;
   procedure Test_Accessibility with Convention => C;
   procedure Test_Activate with Convention => C;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed (Self : Gtk_Editable) is
      pragma Unreferenced (Self);
   begin
      Changed := Changed + 1;
   end On_Changed;

   --------------
   -- Activate --
   --------------

   procedure Activate (E : Gtk_Password_Entry) is
      Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("activate");
   begin
      Emit_By_Name (Get_Object (E), Name);
      Interfaces.C.Strings.Free (Name);
   end Activate;

   -----------------------
   -- On_Activate_Entry --
   -----------------------

   procedure On_Activate_Entry (Self : access Gtk_Password_Entry_Record'Class)
   is
   begin
      Activated := Activated + 1;
      Last_Activated := GObject (Self);
   end On_Activate_Entry;

   ------------------------
   -- On_Activate_Object --
   ------------------------

   procedure On_Activate_Object (Self : access GObject_Record'Class) is
   begin
      Activated := Activated + 1;
      Last_Activated := GObject (Self);
   end On_Activate_Object;

   ---------------------
   -- Test_Peek_Icon --
   ---------------------

   procedure Test_Peek_Icon is
      E : constant Gtk_Password_Entry := Gtk_Password_Entry_New;
   begin
      Ref_Sink (E);

      Assert_False (E.Get_Show_Peek_Icon);
      E.Set_Show_Peek_Icon (True);
      Assert_True (E.Get_Show_Peek_Icon);
      E.Set_Show_Peek_Icon (False);
      Assert_False (E.Get_Show_Peek_Icon);

      Unref (E);
   end Test_Peek_Icon;

   ---------------------
   -- Test_Extra_Menu --
   ---------------------

   procedure Test_Extra_Menu is
      E    : constant Gtk_Password_Entry := Gtk_Password_Entry_New;
      Menu : constant Gmenu := Gmenu_New;
   begin
      Ref_Sink (E);

      Assert_True (E.Get_Extra_Menu = null);

      --  Round-tripping the menu is not asserted: gtk_password_entry_get_
      --  extra_menu returns NULL here even right after the C setter, which
      --  the binding only forwards to. The setter must at least accept a
      --  model and, the property being nullable, a null.
      E.Set_Extra_Menu (Menu);
      E.Set_Extra_Menu (null);
      Assert_True (E.Get_Extra_Menu = null);

      Unref (E);
      Unref (Menu);
   end Test_Extra_Menu;

   ---------------------
   -- Test_Properties --
   ---------------------

   procedure Test_Properties is
      E : constant Gtk_Password_Entry := Gtk_Password_Entry_New;
   begin
      Ref_Sink (E);

      --  These two have no accessors of their own, only properties.
      Assert_Cmpstr_Eq (Get_Property (E, Placeholder_Text_Property), "");
      Set_Property (E, Placeholder_Text_Property, "Password");
      Assert_Cmpstr_Eq
        (Get_Property (E, Placeholder_Text_Property), "Password");

      Assert_False (Get_Property (E, Activates_Default_Property));
      Set_Property (E, Activates_Default_Property, True);
      Assert_True (Get_Property (E, Activates_Default_Property));

      Unref (E);
   end Test_Properties;

   ----------------------
   -- Test_Constructor --
   ----------------------

   procedure Test_Constructor is
      E : Gtk_Password_Entry;
   begin
      --  Gtk_New is the procedure form of the constructor; initialize it a
      --  second time to check the documented idempotent behavior.
      Gtk_New (E);
      Ref_Sink (E);
      Gtk.Password_Entry.Initialize (E);

      Assert_True (E.Get_Accessible_Role = Accessible_Role_Text_Box);
      Assert_False (E.Get_Show_Peek_Icon);

      Unref (E);
   end Test_Constructor;

   --  Password entries use secure entry buffers; check both buffer
   --  constructors and that a repeated Initialize preserves existing text.
   procedure Test_Buffer_Constructors is
      Via_Function : constant Gtk.Password_Entry_Buffer.Gtk_Password_Entry_Buffer :=
        Gtk.Password_Entry_Buffer.Gtk_Password_Entry_Buffer_New;
      Via_Procedure : Gtk.Password_Entry_Buffer.Gtk_Password_Entry_Buffer;
   begin
      Assert_True
        (Gtk.Password_Entry_Buffer.Get_Type /= Glib.GType_Invalid);
      Gtk.Password_Entry_Buffer.Gtk_New (Via_Procedure);

      --  The new objects start empty and accept the inherited buffer API.
      Assert_Cmpuint_Eq (Via_Function.Get_Length, 0);
      Assert_Cmpuint_Eq (Via_Procedure.Get_Length, 0);
      Via_Function.Set_Text ("function secret", -1);
      Via_Procedure.Set_Text ("procedure secret", -1);
      Assert_Cmpstr_Eq (Via_Function.Get_Text, "function secret");

      --  Gtk_New has already initialized this object, so this call must not
      --  replace the text stored in the secure buffer.
      Gtk.Password_Entry_Buffer.Initialize (Via_Procedure);
      Assert_Cmpstr_Eq (Via_Procedure.Get_Text, "procedure secret");

      --  Convert to the parent buffer access type for inherited cleanup.
      Unref (Gtk_Entry_Buffer (Via_Function));
      Unref (Gtk_Entry_Buffer (Via_Procedure));
   end Test_Buffer_Constructors;

   --------------------
   -- Test_Editable --
   --------------------

   procedure Test_Editable is
      E : constant Gtk_Password_Entry := Gtk_Password_Entry_New;
      Start_Pos, End_Pos : Gint;
      Has_Selection : Boolean;
      Position : Gint;
   begin
      Ref_Sink (E);

      Changed := 0;
      On_Changed (+E, On_Changed'Unrestricted_Access);

      Assert_Cmpstr_Eq (E.Get_Text, "");
      E.Set_Text ("hunter2");
      Assert_Cmpstr_Eq (E.Get_Text, "hunter2");
      Assert_True (Changed > 0);
      Assert_Cmpstr_Eq (E.Get_Chars (0, 6), "hunter");

      --  GtkEditable uses character offsets and an exclusive selection end.
      E.Set_Position (2);
      Assert_Cmpint_Eq (E.Get_Position, 2);
      E.Select_Region (1, 6);
      E.Get_Selection_Bounds (Start_Pos, End_Pos, Has_Selection);
      Assert_True (Has_Selection);
      Assert_Cmpint_Eq (Start_Pos, 1);
      Assert_Cmpint_Eq (End_Pos, 6);
      E.Delete_Selection;
      Assert_Cmpstr_Eq (E.Get_Text, "h2");

      --  Insert_Text reports the position just after the inserted text.
      E.Set_Text ("secret");
      Position := 2;
      E.Insert_Text ("X", 1, Position);
      Assert_Cmpstr_Eq (E.Get_Text, "seXcret");
      Assert_Cmpint_Eq (Position, 3);
      E.Delete_Text (2, 3);
      Assert_Cmpstr_Eq (E.Get_Text, "secret");
      E.Get_Selection_Bounds (Start_Pos, End_Pos, Has_Selection);
      Assert_False (Has_Selection);

      --  Width hints, undo, and alignment are ordinary getter/setter pairs.
      E.Set_Width_Chars (8);
      Assert_Cmpint_Eq (E.Get_Width_Chars, 8);
      E.Set_Max_Width_Chars (12);
      Assert_Cmpint_Eq (E.Get_Max_Width_Chars, 12);
      E.Set_Enable_Undo (True);
      Assert_True (E.Get_Enable_Undo);
      E.Set_Enable_Undo (False);
      Assert_False (E.Get_Enable_Undo);
      E.Set_Alignment (Interfaces.C.C_float (0.25));
      Assert_Cmpfloat_Eq (Gdouble (E.Get_Alignment), 0.25);

      E.Set_Editable (False);
      Assert_False (E.Get_Editable);
      E.Set_Editable (True);
      Assert_True (E.Get_Editable);

      --  GtkPasswordEntry exposes its internal editable delegate.
      Assert_True (E.Get_Delegate /= Gtk.Editable.Null_Gtk_Editable);

      Unref (E);
   end Test_Editable;

   --------------------------
   -- Test_Accessibility --
   --------------------------

   procedure Test_Accessibility is
      E : constant Gtk_Password_Entry := Gtk_Password_Entry_New;
      X, Y, Width, Height : Gint;
   begin
      Ref_Sink (E);

      --  Detached password entries have the text-box role, an internal
      --  accessible child, and no platform context or screen-coordinate bounds.
      Assert_True (E.Get_Accessible_Role = Accessible_Role_Text_Box);
      Assert_Cmpstr_Eq (E.Get_Accessible_Id, "");
      Assert_True (E.Get_First_Accessible_Child /= Null_Gtk_Accessible);
      Assert_True (E.Get_Accessible_Parent = Null_Gtk_Accessible);
      Assert_True (E.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      Assert_True (E.Get_At_Context = null);
      Assert_False (E.Get_Bounds (X, Y, Width, Height));
      Assert_False (E.Get_Platform_State (Accessible_Platform_State_Focused));

      --  The editable delegate reports the same focusability as its owner.
      Assert_True
        (E.Delegate_Get_Accessible_Platform_State
           (Accessible_Platform_State_Focusable)
         = E.Get_Platform_State (Accessible_Platform_State_Focusable));

      --  These notifications and reset calls need no screen reader; using
      --  null parents preserves the detached accessible tree.
      E.Announce ("Password entry ready", Accessible_Announcement_Priority_Low);
      E.Reset_Property (Accessible_Property_Label);
      E.Reset_Relation (Accessible_Relation_Labelled_By);
      E.Reset_State (Accessible_State_Busy);
      E.Update_Platform_State (Accessible_Platform_State_Focused);
      E.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (E.Get_Accessible_Parent = Null_Gtk_Accessible);
      E.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (E.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      Unref (E);
   end Test_Accessibility;

   -------------------
   -- Test_Activate --
   -------------------

   procedure Test_Activate is
      E     : constant Gtk_Password_Entry := Gtk_Password_Entry_New;
      Other : constant Gtk_Password_Entry := Gtk_Password_Entry_New;
   begin
      Ref_Sink (E);
      Ref_Sink (Other);

      Activated := 0;
      Last_Activated := null;

      --  Typing does not activate the entry.
      E.On_Activate (On_Activate_Entry'Unrestricted_Access);
      E.Set_Text ("hunter2");
      Assert_Cmpint_Eq (Gint (Activated), 0);

      --  The signal is the one the Enter key is bound to.
      Activate (E);
      Assert_Cmpint_Eq (Gint (Activated), 1);
      Assert_True (Last_Activated = GObject (E));

      Activate (E);
      Assert_Cmpint_Eq (Gint (Activated), 2);

      --  The variant with a slot: the handler gets the slot, not the
      --  emitter, and is connected on top of the first one.
      E.On_Activate (On_Activate_Object'Unrestricted_Access, Other);
      Last_Activated := null;
      Activate (E);
      Assert_Cmpint_Eq (Gint (Activated), 4);
      Assert_True (Last_Activated = GObject (Other));

      --  Activating another entry does not reach the handlers of E.
      Activate (Other);
      Assert_Cmpint_Eq (Gint (Activated), 4);

      Unref (E);
      Unref (Other);
   end Test_Activate;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/passwordentry/peek-icon", Test_Peek_Icon'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/passwordentry/extra-menu", Test_Extra_Menu'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/passwordentry/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/passwordentry/constructor", Test_Constructor'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/passwordentry/buffer-constructors",
      Test_Buffer_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/passwordentry/editable", Test_Editable'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/passwordentry/accessibility", Test_Accessibility'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/passwordentry/activate", Test_Activate'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Password_Entry;
