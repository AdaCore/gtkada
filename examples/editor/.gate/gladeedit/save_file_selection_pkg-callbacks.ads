with Gtk.Arguments;

package Save_File_Selection_Pkg.Callbacks is
   procedure On_Save_Filesel_Delete_Event
     (Object : access Gtk_File_Selection_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Save_Filesel_Ok_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Save_Filesel_Cancel_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

end Save_File_Selection_Pkg.Callbacks;
