--  Headless regression guard for the Gtk4 colour-selection API exercised by
--  the gtkada_demo "Color Chooser" page (see gtkada_demo/create_color_chooser).
--
--  The asynchronous Gtk.Color_Dialog.Choose_Rgba flow cannot be driven without
--  user interaction, so this test covers what can be checked headlessly: the
--  dialog's properties and the Gtk_Color_Dialog_Button's RGBA round-trip.

with Glib;                    use Glib;
with Glib.Test;               use Glib.Test;
with Ada.Command_Line;
with Gdk.RGBA;                use Gdk.RGBA;
with Gtk.Color_Dialog;        use Gtk.Color_Dialog;
with Gtk.Color_Dialog_Button; use Gtk.Color_Dialog_Button;
with Gtk.Main;

procedure Color_Dialog is

   procedure Test_Dialog_Properties
   with Convention => C;

   procedure Test_Button_Rgba_Round_Trip
   with Convention => C;

   ------------------------------
   -- Test_Dialog_Properties --
   ------------------------------

   procedure Test_Dialog_Properties is
      Dialog : Gtk_Color_Dialog;
   begin
      Gtk_New (Dialog);
      Assert_Nonnull (Dialog.all'Address);

      Dialog.Set_Title ("Choose a colour");
      Assert_Cmpstr_Eq (Dialog.Get_Title, "Choose a colour");

      Dialog.Set_With_Alpha (True);
      Assert_True (Dialog.Get_With_Alpha);

      Dialog.Set_With_Alpha (False);
      Assert_False (Dialog.Get_With_Alpha);
   end Test_Dialog_Properties;

   ----------------------------------
   -- Test_Button_Rgba_Round_Trip --
   ----------------------------------

   procedure Test_Button_Rgba_Round_Trip is
      Dialog : Gtk_Color_Dialog;
      Button : Gtk_Color_Dialog_Button;
      Color  : constant Gdk_RGBA := (0.25, 0.5, 0.75, 1.0);
      Got    : Gdk_RGBA;
   begin
      Gtk_New (Dialog);
      Gtk_New (Button, Dialog);

      --  The button must report the dialog it was created with.
      Assert_True (Button.Get_Dialog = Dialog);

      Button.Set_Rgba (Color);
      Got := Button.Get_Rgba;

      Assert_Cmpfloat_Eq (Gdouble (Got.Red), Gdouble (Color.Red));
      Assert_Cmpfloat_Eq (Gdouble (Got.Green), Gdouble (Color.Green));
      Assert_Cmpfloat_Eq (Gdouble (Got.Blue), Gdouble (Color.Blue));
      Assert_Cmpfloat_Eq (Gdouble (Got.Alpha), Gdouble (Color.Alpha));
   end Test_Button_Rgba_Round_Trip;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/color-dialog/properties", Test_Dialog_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/color-dialog/button-rgba-round-trip",
      Test_Button_Rgba_Round_Trip'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Color_Dialog;
