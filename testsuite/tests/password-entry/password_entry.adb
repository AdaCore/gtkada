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
with Gtk.Main;
with Gtk.Password_Entry;  use Gtk.Password_Entry;

procedure Password_Entry is

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
   procedure Test_Editable with Convention => C;
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

   --------------------
   -- Test_Editable --
   --------------------

   procedure Test_Editable is
      E : constant Gtk_Password_Entry := Gtk_Password_Entry_New;
   begin
      Ref_Sink (E);

      Changed := 0;
      On_Changed (+E, On_Changed'Unrestricted_Access);

      Assert_Cmpstr_Eq (E.Get_Text, "");
      E.Set_Text ("hunter2");
      Assert_Cmpstr_Eq (E.Get_Text, "hunter2");
      Assert_True (Changed > 0);
      Assert_Cmpstr_Eq (E.Get_Chars (0, 6), "hunter");

      E.Set_Editable (False);
      Assert_False (E.Get_Editable);

      Unref (E);
   end Test_Editable;

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
     ("/passwordentry/editable", Test_Editable'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/passwordentry/activate", Test_Activate'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Password_Entry;
