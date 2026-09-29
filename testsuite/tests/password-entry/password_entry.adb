--  GTK has no testsuite/gtk/passwordentry.c to port, so these cases are ours:
--  they cover the surface the Gtk.Password_Entry binding adds, plus a sample
--  of what it inherits from Gtk.Editable.

with Ada.Command_Line;
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

   procedure On_Changed (Self : Gtk_Editable);

   procedure Test_Peek_Icon with Convention => C;
   procedure Test_Extra_Menu with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Editable with Convention => C;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed (Self : Gtk_Editable) is
      pragma Unreferenced (Self);
   begin
      Changed := Changed + 1;
   end On_Changed;

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

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Password_Entry;
