--  Ada port of GTK's testsuite/gtk/searchbar.c (GTK 4.22.2), extended with
--  a check of the bar's own properties and of Connect_Entry.

with Ada.Command_Line;
with Glib.Object;      use Glib.Object;
with Glib.Test;        use Glib.Test;
with Gtk.Button;       use Gtk.Button;
with Gtk.Main;
with Gtk.Search_Bar;   use Gtk.Search_Bar;
with Gtk.Search_Entry; use Gtk.Search_Entry;
with Gtk.Widget;       use Gtk.Widget;

procedure Search_Bar is

   procedure Test_Capture_Widget_Destroy with Convention => C;
   procedure Test_Capture_Widget_Unset with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Connect_Entry with Convention => C;

   ---------------------------------
   -- Test_Capture_Widget_Destroy --
   ---------------------------------

   procedure Test_Capture_Widget_Destroy is
      Bar    : constant Gtk_Search_Bar := Gtk_Search_Bar_New;
      Button : constant Gtk_Button := Gtk_Button_New_With_Label;
   begin
      Ref_Sink (Bar);
      Ref_Sink (Button);

      Bar.Set_Key_Capture_Widget (Button);
      Assert_True (Bar.Get_Key_Capture_Widget = Gtk_Widget (Button));

      --  Dropping the last reference to the capture widget must clear the
      --  bar's pointer to it rather than leave it dangling.
      Unref (Button);
      Assert_True (Bar.Get_Key_Capture_Widget = null);

      Unref (Bar);
   end Test_Capture_Widget_Destroy;

   -------------------------------
   -- Test_Capture_Widget_Unset --
   -------------------------------

   procedure Test_Capture_Widget_Unset is
      Bar    : constant Gtk_Search_Bar := Gtk_Search_Bar_New;
      Button : constant Gtk_Button := Gtk_Button_New_With_Label;
   begin
      Ref_Sink (Bar);
      Ref_Sink (Button);

      Bar.Set_Key_Capture_Widget (Button);
      Assert_True (Bar.Get_Key_Capture_Widget = Gtk_Widget (Button));

      Bar.Set_Key_Capture_Widget (null);
      Assert_True (Bar.Get_Key_Capture_Widget = null);

      Unref (Bar);
      Unref (Button);
   end Test_Capture_Widget_Unset;

   ---------------------
   -- Test_Properties --
   ---------------------

   procedure Test_Properties is
      Bar : constant Gtk_Search_Bar := Gtk_Search_Bar_New;
   begin
      Assert_False (Bar.Get_Search_Mode);
      Bar.Set_Search_Mode (True);
      Assert_True (Bar.Get_Search_Mode);
      Bar.Set_Search_Mode (False);
      Assert_False (Bar.Get_Search_Mode);

      --  Both properties default to False.
      Assert_False (Bar.Get_Show_Close_Button);
      Bar.Set_Show_Close_Button (True);
      Assert_True (Bar.Get_Show_Close_Button);
      Bar.Set_Show_Close_Button (False);
      Assert_False (Bar.Get_Show_Close_Button);
   end Test_Properties;

   -------------------------
   -- Test_Connect_Entry --
   -------------------------

   procedure Test_Connect_Entry is
      Bar   : constant Gtk_Search_Bar := Gtk_Search_Bar_New;
      Entry_Widget : constant Gtk_Search_Entry := Gtk_Search_Entry_New;
   begin
      Assert_True (Bar.Get_Child = null);

      Bar.Set_Child (Entry_Widget);
      Assert_True (Bar.Get_Child = Gtk_Widget (Entry_Widget));

      --  In Gtk4 Connect_Entry takes a Gtk_Editable, not a Gtk_Entry, so any
      --  editable (here the search entry itself) can be driven by the bar.
      Bar.Connect_Entry (+Entry_Widget);
   end Test_Connect_Entry;

begin
   Glib.Test.Init;

   --  The C test uses gtk_test_init, which also initializes GTK; widgets
   --  cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/searchbar/capture-widget-destroy",
      Test_Capture_Widget_Destroy'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/searchbar/capture-widget-unset",
      Test_Capture_Widget_Unset'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/searchbar/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/searchbar/connect-entry", Test_Connect_Entry'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Search_Bar;
