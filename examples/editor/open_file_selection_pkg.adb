with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types; use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Pixmap; use Gtk.Pixmap;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Callbacks_Gladeedit; use Callbacks_Gladeedit;

package body Open_File_Selection_Pkg is

procedure Gtk_New (Open_File_Selection : out Open_File_Selection_Access) is
begin
   Open_File_Selection := new Open_File_Selection_Record;
   Open_File_Selection_Pkg.Initialize (Open_File_Selection);
end Gtk_New;

procedure Initialize (Open_File_Selection : access Open_File_Selection_Record'Class) is
   Cb_Id : Glib.Guint;

begin
   Gtk.File_Selection.Initialize (Open_File_Selection, "Open File");
   Set_Show_File_Op_Buttons (Open_File_Selection, True);
   Cb_Id := File_Selection_Callback.Connect
     (Open_File_Selection, "delete_event", On_Open_Filesel_Delete_Event'Access);
   Set_Border_Width (Open_File_Selection, 10);
   Set_Title (Open_File_Selection, "Open File");
   Set_Policy (Open_File_Selection, False, True, False);
   Set_Position (Open_File_Selection, Win_Pos_Mouse);
   Open_File_Selection.Ok_Button1 := Get_Ok_Button (Open_File_Selection);
   Set_Flags (Open_File_Selection.Ok_Button1, Can_Default);
   Cb_Id := Button_Callback.Connect
     (Open_File_Selection.Ok_Button1, "clicked", On_Open_Filesel_Ok_Button_Clicked'Access);

   Open_File_Selection.Cancel_Button1 := Get_Cancel_Button (Open_File_Selection);
   Set_Flags (Open_File_Selection.Cancel_Button1, Can_Default);
   Cb_Id := Button_Callback.Connect
     (Open_File_Selection.Cancel_Button1, "clicked", On_Open_Filesel_Cancel_Button_Clicked'Access);

end Initialize;

end Open_File_Selection_Pkg;
