with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Save_File_Selection_Pkg.Callbacks is
   function On_Save_Filesel_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Save_Filesel_Ok_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Save_Filesel_Cancel_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

end Save_File_Selection_Pkg.Callbacks;
