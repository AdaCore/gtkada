--  Ada port of GTK's testsuite/gtk/popovermenu.c (GTK 4.22.2).
--
--  Exercises swapping the menu model of a live Gtk_Popover_Menu (including
--  unsetting it) and changing the popover position in between.
--
--  The C test builds its two menu models from GtkBuilder XML; GtkBuilder is
--  not bound, so the models are constructed programmatically with Glib.Menu,
--  which preserves the behaviour under test (model swapping on a live
--  popover).

with Ada.Command_Line;
with Glib.Menu;        use Glib.Menu;
with Glib.Object;      use Glib.Object;
with Glib.Test;        use Glib.Test;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Main;
with Gtk.Popover_Menu; use Gtk.Popover_Menu;

procedure Popovermenu is

   procedure Test_Set_Model
   with Convention => C;

   --------------------
   -- Test_Set_Model --
   --------------------

   procedure Test_Set_Model is
      Menu1   : constant Gmenu := Gmenu_New;
      Menu2   : constant Gmenu := Gmenu_New;
      Popover : Gtk_Popover_Menu;
   begin
      Menu1.Append ("Record events", "record.record-events");
      Menu2.Append ("Do not record events", "record.no-record-events");

      Popover := Gtk_Popover_Menu_New_From_Model (Menu1);

      Popover.Set_Menu_Model (null);

      Popover.Set_Position (Pos_Left);

      Popover.Set_Menu_Model (Menu2);

      Popover.Set_Position (Pos_Bottom);

      Ref_Sink (Popover);
      Popover.Unref;

      Menu1.Unref;
      Menu2.Unref;
   end Test_Set_Model;

begin
   Glib.Test.Init;

   --  The C test uses gtk_test_init, which also initializes GTK; widgets
   --  cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/popover/menu/set-model", Test_Set_Model'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Popovermenu;
