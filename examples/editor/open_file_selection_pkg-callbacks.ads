package Open_File_Selection_Pkg.Callbacks is
   procedure On_Open_Filesel_Delete_Event
     (Object : access Gtk_File_Selection_Record);

   procedure On_Open_Filesel_Ok_Button_Clicked
     (Object : access Gtk_Button_Record);

   procedure On_Open_Filesel_Cancel_Button_Clicked
     (Object : access Gtk_Button_Record);

end Open_File_Selection_Pkg.Callbacks;
