
with Glib;
with Gtk; use Gtk;
with Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Button; use Gtk.Button;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk.Pixmap; use Gtk.Pixmap;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Callbacks_Gladeedit; use Callbacks_Gladeedit;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Window; use Gtk.Window;
with Gtk.Container; use Gtk.Container;
with Gtk.Box; use Gtk.Box;
with Gtk.Label; use Gtk.Label;
with Gtk.Misc; use Gtk.Misc;
with Gtk.Button; use Gtk.Button;

function Create_About_Dialog return Gtk_Dialog is
   Cb_Id : Glib.Guint;
   The_Accel_Group : Gtk_Accel_Group;
   About_Dialog : Gtk_Dialog;
   Dialog_Vbox2 : Gtk_Vbox;
   Label2 : Gtk_Label;
   Dialog_Action_Area2 : Gtk_Hbox;
   Button7 : Gtk_Button;

begin
   Dialog.Gtk_New (About_Dialog);
   Window.Set_Title (Gtk_Window (About_Dialog), "About The Editor");
   Window.Set_Policy (Gtk_Window (About_Dialog), True, True, False);
   Window.Set_Position (Gtk_Window (About_Dialog), Win_Pos_Mouse);
   Dialog_Vbox2 := Get_Vbox (About_Dialog);
   Container.Set_Border_Width (Gtk_Container (Dialog_Vbox2), 2);
   Box.Set_Homogeneous (Gtk_Box (Dialog_Vbox2), False);
   Box.Set_Spacing (Gtk_Box (Dialog_Vbox2), 0);

   Label.Gtk_New (Label2, "The Editor - v 1.0");
   Box.Pack_Start (Dialog_Vbox2, Label2, True, True, 10);
   Misc.Set_Alignment (Gtk_Misc (Label2), 0.5, 0.5);
   Misc.Set_Padding (Gtk_Misc (Label2), 12, 0);
   Label.Set_Justify (Gtk_Label (Label2), Justify_Center);

   Dialog_Action_Area2 := Get_Action_Area (About_Dialog);
   Container.Set_Border_Width (Gtk_Container (Dialog_Action_Area2), 10);
   Box.Set_Homogeneous (Gtk_Box (Dialog_Action_Area2), True);
   Box.Set_Spacing (Gtk_Box (Dialog_Action_Area2), 5);

   Button.Gtk_New (Button7, "OK");
   Box.Pack_Start (Dialog_Action_Area2, Button7, False, False, 0);
   Cb_Id := Button_Callback.Connect
     (Button7, "clicked", On_About_Ok_Clicked'Access);

   return About_Dialog;
end Create_About_Dialog;
