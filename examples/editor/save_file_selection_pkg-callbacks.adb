with Gtk.Widget; use Gtk.Widget;
with File_Utils; use File_Utils;

package body Save_File_Selection_Pkg.Callbacks is

   ------------------------------------
   --  On_Save_Filesel_Delete_Event  --
   ------------------------------------

   procedure On_Save_Filesel_Delete_Event
     (Object : access Gtk_File_Selection_Record) is
   begin
      Gtk.Widget.Hide (Gtk.Widget.Get_Toplevel (Gtk_Widget (Object)));
   end On_Save_Filesel_Delete_Event;

   -----------------------------------------
   --  On_Save_Filesel_Ok_Button_Clicked  --
   -----------------------------------------

   procedure On_Save_Filesel_Ok_Button_Clicked
     (Object : access Gtk_Button_Record) is
   begin
      Gtk.Widget.Hide (Gtk_Widget (Save_File_Selection));
      Free (Current_Filename);
      Current_Filename := new String' (Get_Filename (Save_File_Selection));
      Real_Open_File (Current_Filename.all);
   end On_Save_Filesel_Ok_Button_Clicked;

   ---------------------------------------------
   --  On_Save_Filesel_Cancel_Button_Clicked  --
   ---------------------------------------------

   procedure On_Save_Filesel_Cancel_Button_Clicked
     (Object : access Gtk_Button_Record) is
   begin
      Gtk.Widget.Hide (Gtk.Widget.Get_Toplevel (Gtk_Widget (Object)));
   end On_Save_Filesel_Cancel_Button_Clicked;

end Save_File_Selection_Pkg.Callbacks;
