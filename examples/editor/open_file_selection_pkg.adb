with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types; use Gdk.Types;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Enums;  use Gtk.Enums;
with Callbacks_Gladeedit; use Callbacks_Gladeedit;
with Open_File_Selection_Pkg.Callbacks; use Open_File_Selection_Pkg.Callbacks;

package body Open_File_Selection_Pkg is

procedure Gtk_New (Open_File_Selection : out Open_File_Selection_Access) is
begin
   Open_File_Selection := new Open_File_Selection_Record;
   Open_File_Selection_Pkg.Initialize (Open_File_Selection);
end Gtk_New;

procedure Initialize (Open_File_Selection : access Open_File_Selection_Record'Class) is
begin
   Gtk.File_Selection.Initialize (Open_File_Selection, "Open File");
   Set_Show_File_Op_Buttons (Open_File_Selection, True);
   File_Selection_Callback.Connect
     (Open_File_Selection, "delete_event", On_Open_Filesel_Delete_Event'Access);
   Set_Border_Width (Open_File_Selection, 10);
   Set_Title (Open_File_Selection, "Open File");
   Set_Policy (Open_File_Selection, False, True, False);
   Set_Position (Open_File_Selection, Win_Pos_Mouse);
   Set_Modal (Open_File_Selection, False);

   Open_File_Selection.Ok_Button1 := Get_Ok_Button (Open_File_Selection);
   Set_Flags (Open_File_Selection.Ok_Button1, Can_Default);
   Button_Callback.Connect
     (Open_File_Selection.Ok_Button1, "clicked",
      Button_Callback.To_Marshaller (On_Open_Filesel_Ok_Button_Clicked'Access));

   Open_File_Selection.Cancel_Button1 := Get_Cancel_Button (Open_File_Selection);
   Set_Flags (Open_File_Selection.Cancel_Button1, Can_Default);
   Button_Callback.Connect
     (Open_File_Selection.Cancel_Button1, "clicked",
      Button_Callback.To_Marshaller (On_Open_Filesel_Cancel_Button_Clicked'Access));

end Initialize;

end Open_File_Selection_Pkg;
