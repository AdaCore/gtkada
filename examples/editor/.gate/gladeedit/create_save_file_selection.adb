
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

function Create_Save_File_Selection return Gtk_File_Selection is
   Cb_Id : Glib.Guint;
   The_Accel_Group : Gtk_Accel_Group;
   Save_File_Selection : Gtk_File_Selection;
   Ok_Button3 : Gtk_Button;
   Cancel_Button3 : Gtk_Button;

begin
   File_Selection.Gtk_New (Save_File_Selection, "Save File");
   Cb_Id := File_Selection_Callback.Connect
     (Save_File_Selection, "delete_event", On_Save_Filesel_Delete_Event'Access);
   Container.Set_Border_Width (Gtk_Container (Save_File_Selection), 10);
   Window.Set_Title (Gtk_Window (Save_File_Selection), "Save File");
   Window.Set_Policy (Gtk_Window (Save_File_Selection), False, True, False);
   Window.Set_Position (Gtk_Window (Save_File_Selection), Win_Pos_Mouse);
   Ok_Button3 := Get_Ok_Button (Save_File_Selection);
   Cb_Id := Button_Callback.Connect
     (Ok_Button3, "clicked", On_Save_Filesel_Ok_Button_Clicked'Access);

   Cancel_Button3 := Get_Cancel_Button (Save_File_Selection);
   Cb_Id := Button_Callback.Connect
     (Cancel_Button3, "clicked", On_Save_Filesel_Cancel_Button_Clicked'Access);

   return Save_File_Selection;
end Create_Save_File_Selection;
