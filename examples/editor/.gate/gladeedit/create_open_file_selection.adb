
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
with Gtk.File_Selection; use Gtk.File_Selection;
with Gtk.Container; use Gtk.Container;
with Gtk.Window; use Gtk.Window;

function Create_Open_File_Selection return Gtk_File_Selection is
   Cb_Id : Glib.Guint;
   The_Accel_Group : Gtk_Accel_Group;
   Open_File_Selection : Gtk_File_Selection;
   Ok_Button1 : Gtk_Button;
   Cancel_Button1 : Gtk_Button;

begin
   File_Selection.Gtk_New (Open_File_Selection, "Open File");
   Cb_Id := File_Selection_Callback.Connect
     (Open_File_Selection, "delete_event", On_Open_Filesel_Delete_Event'Access);
   Container.Set_Border_Width (Gtk_Container (Open_File_Selection), 10);
   Window.Set_Title (Gtk_Window (Open_File_Selection), "Open File");
   Window.Set_Policy (Gtk_Window (Open_File_Selection), False, True, False);
   Window.Set_Position (Gtk_Window (Open_File_Selection), Win_Pos_Mouse);
   Ok_Button1 := Get_Ok_Button (Open_File_Selection);
   Cb_Id := Button_Callback.Connect
     (Ok_Button1, "clicked", On_Open_Filesel_Ok_Button_Clicked'Access);

   Cancel_Button1 := Get_Cancel_Button (Open_File_Selection);
   Cb_Id := Button_Callback.Connect
     (Cancel_Button1, "clicked", On_Open_Filesel_Cancel_Button_Clicked'Access);

   return Open_File_Selection;
end Create_Open_File_Selection;
