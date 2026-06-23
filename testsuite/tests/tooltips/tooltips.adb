--  Ada port of GTK's testsuite/gtk/tooltips.c.
--
--  Exercises the GtkWidget tooltip-text / tooltip-markup accessors and the
--  markup escaping performed when a plain-text tooltip is read back as markup.

with Ada.Command_Line;
with Glib;             use Glib;
with Glib.Test;        use Glib.Test;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Label;        use Gtk.Label;
with Gtk.Main;
with Gtk.Widget;       use Gtk.Widget;

procedure Tooltips is

   procedure Test_Widget_Accessors
   with Convention => C;

   ---------------------------
   -- Test_Widget_Accessors --
   ---------------------------

   procedure Test_Widget_Accessors is
      Check : Gtk_Check_Button;
      Label : Gtk_Label;
   begin
      Message ("A button using tooltip-text");
      Check :=
        Gtk_Check_Button_New_With_Label
          ("This one uses the tooltip-text property");
      Check.Set_Tooltip_Text ("Hello, I am a static tooltip.");

      Assert_Cmpstr_Eq
        (Check.Get_Tooltip_Text, "Hello, I am a static tooltip.");
      Assert_Cmpstr_Eq
        (Check.Get_Tooltip_Markup, "Hello, I am a static tooltip.");

      Message ("A label using tooltip-text");
      Label := Gtk_Label_New ("I am just a label");
      Label.Set_Tooltip_Text ("Label & and tooltip");

      Assert_Cmpstr_Eq (Label.Get_Tooltip_Text, "Label & and tooltip");
      Assert_Cmpstr_Eq (Label.Get_Tooltip_Markup, "Label &amp; and tooltip");

      Message ("A label using tooltip-markup");
      Label := Gtk_Label_New ("I am a selectable label");
      Label.Set_Selectable (True);
      Label.Set_Tooltip_Markup ("<b>Another</b> Label tooltip");

      Assert_Cmpstr_Eq (Label.Get_Tooltip_Text, "Another Label tooltip");
      Assert_Cmpstr_Eq
        (Label.Get_Tooltip_Markup, "<b>Another</b> Label tooltip");
   end Test_Widget_Accessors;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/tooltips/widget-accessors", Test_Widget_Accessors'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Tooltips;
