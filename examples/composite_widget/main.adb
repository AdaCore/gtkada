with Glib; use Glib;
with Gtk.Box;    use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Label; use Gtk.Label;
with Gtk.Main; use Gtk.Main;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;

with My_Dialog; use My_Dialog;

procedure Main is
   Main_W : Gtk_Window;
   Ok     : Gtk_Button;
   Id     : Guint;

   package Button_Cb is new Void_Callback (Gtk_Button_Record);
   package Dialog_Cb is new Object_Callback (Gtk_Widget_Record);

   procedure Open_Dialog (B : access Gtk_Button_Record) is
      Dialog : My_Dialog.My_Dialog;
      Button : Gtk_Button;
      Label  : Gtk_Label;
      Id     : Guint;

   begin
      Gtk_New (Dialog);
      Set_Border_Width (Dialog, 10);

      Gtk_New (Label, "This dialog widget was completly written in Ada");
      Pack_Start (Dialog.Vbox, Label, True, True, 0);
      Gtk_New (Label, "No C involved!");
      Pack_Start (Dialog.Vbox, Label, True, True, 0);

      Gtk_New (Button, "Quit");
      Pack_Start (Dialog.Action_Area, Button, True, True, 0);
      Id := Dialog_Cb.Connect (Button, "clicked", Destroy'Access, Dialog);

      Show_All (Dialog);
   end Open_Dialog;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   Gtk_New (Main_W, Window_Toplevel);

   Gtk_New (Ok, "Click here to show a dialog");
   Add (Main_W, Ok);
   Id := Button_Cb.Connect (Ok, "clicked", Open_Dialog'Access);
   Show (Ok);

   Show (Main_W);

   Gtk.Main.Main;
end Main;

