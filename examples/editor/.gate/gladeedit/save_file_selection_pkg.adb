with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Gladeedit; use Callbacks_Gladeedit;
with Save_File_Selection_Pkg.Callbacks; use Save_File_Selection_Pkg.Callbacks;

package body Save_File_Selection_Pkg is

procedure Gtk_New (Save_File_Selection : out Save_File_Selection_Access) is
begin
   Save_File_Selection := new Save_File_Selection_Record;
   Save_File_Selection_Pkg.Initialize (Save_File_Selection);
end Gtk_New;

procedure Initialize (Save_File_Selection : access Save_File_Selection_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.File_Selection.Initialize (Save_File_Selection, "Save File");
   Set_Show_File_Op_Buttons (Save_File_Selection, True);
   Return_Callback.Connect
     (Save_File_Selection, "delete_event", On_Save_Filesel_Delete_Event'Access);
   Set_Border_Width (Save_File_Selection, 10);
   Set_Title (Save_File_Selection, "Save File");
   Set_Policy (Save_File_Selection, False, True, False);
   Set_Position (Save_File_Selection, Win_Pos_Mouse);
   Set_Modal (Save_File_Selection, False);

   Save_File_Selection.Ok_Button3 := Get_Ok_Button (Save_File_Selection);
   Set_Flags (Save_File_Selection.Ok_Button3, Can_Default);
   Button_Callback.Connect
     (Save_File_Selection.Ok_Button3, "clicked",
      Button_Callback.To_Marshaller (On_Save_Filesel_Ok_Button_Clicked'Access));

   Save_File_Selection.Cancel_Button3 := Get_Cancel_Button (Save_File_Selection);
   Set_Flags (Save_File_Selection.Cancel_Button3, Can_Default);
   Button_Callback.Connect
     (Save_File_Selection.Cancel_Button3, "clicked",
      Button_Callback.To_Marshaller (On_Save_Filesel_Cancel_Button_Clicked'Access));

end Initialize;

end Save_File_Selection_Pkg;
