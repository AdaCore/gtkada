with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Gladeedit; use Callbacks_Gladeedit;
with About_Dialog_Pkg.Callbacks; use About_Dialog_Pkg.Callbacks;

package body About_Dialog_Pkg is

procedure Gtk_New (About_Dialog : out About_Dialog_Access) is
begin
   About_Dialog := new About_Dialog_Record;
   About_Dialog_Pkg.Initialize (About_Dialog);
end Gtk_New;

procedure Initialize (About_Dialog : access About_Dialog_Record'Class) is
begin
   Gtk.Dialog.Initialize (About_Dialog);
   Set_Title (About_Dialog, "About The Editor");
   Set_Policy (About_Dialog, True, True, False);
   Set_Position (About_Dialog, Win_Pos_Mouse);
   Set_Modal (About_Dialog, False);

   About_Dialog.Dialog_Vbox2 := Get_Vbox (About_Dialog);
   Set_Border_Width (About_Dialog.Dialog_Vbox2, 2);
   Set_Homogeneous (About_Dialog.Dialog_Vbox2, False);
   Set_Spacing (About_Dialog.Dialog_Vbox2, 0);

   Gtk_New (About_Dialog.Label2, "The Editor - v 1.0");
   Pack_Start (About_Dialog.Dialog_Vbox2, About_Dialog.Label2, True, True, 10);
   Set_Alignment (About_Dialog.Label2, 0.5, 0.5);
   Set_Padding (About_Dialog.Label2, 12, 0);
   Set_Justify (About_Dialog.Label2, Justify_Center);
   Set_Line_Wrap (About_Dialog.Label2, False);

   About_Dialog.Dialog_Action_Area2 := Get_Action_Area (About_Dialog);
   Set_Border_Width (About_Dialog.Dialog_Action_Area2, 10);
   Set_Homogeneous (About_Dialog.Dialog_Action_Area2, True);
   Set_Spacing (About_Dialog.Dialog_Action_Area2, 5);

   Gtk_New (About_Dialog.Button7, "OK");
   Set_Flags (About_Dialog.Button7, Can_Default);
   Grab_Default (About_Dialog.Button7);
   Pack_Start (About_Dialog.Dialog_Action_Area2, About_Dialog.Button7, False, False, 0);
   Button_Callback.Connect
     (About_Dialog.Button7, "clicked",
      Button_Callback.To_Marshaller (On_About_Ok_Clicked'Access));

end Initialize;

end About_Dialog_Pkg;
