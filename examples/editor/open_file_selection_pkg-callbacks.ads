with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Open_File_Selection_Pkg.Callbacks is
   function On_Open_Filesel_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Open_Filesel_Ok_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Open_Filesel_Cancel_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

end Open_File_Selection_Pkg.Callbacks;
