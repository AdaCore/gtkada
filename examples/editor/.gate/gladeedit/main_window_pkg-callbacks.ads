package Main_Window_Pkg.Callbacks is
   procedure On_Main_Window_Delete_Event
     (Object : access Gtk_Window_Record);

   procedure On_New_Activate
     (Object : access Gtk_Menu_Item_Record);

   procedure On_Open_Activate
     (Object : access Gtk_Menu_Item_Record);

   procedure On_Save_Activate
     (Object : access Gtk_Menu_Item_Record);

   procedure On_Save_As_Activate
     (Object : access Gtk_Menu_Item_Record);

   procedure On_Quit_Activate
     (Object : access Gtk_Menu_Item_Record);

   procedure On_Cut_Activate
     (Object : access Gtk_Menu_Item_Record);

   procedure On_Copy_Activate
     (Object : access Gtk_Menu_Item_Record);

   procedure On_Paste_Activate
     (Object : access Gtk_Menu_Item_Record);

   procedure On_Delete_Activate
     (Object : access Gtk_Menu_Item_Record);

   procedure On_About_Activate
     (Object : access Gtk_Menu_Item_Record);

   procedure On_New_Button_Clicked
     (Object : access Gtk_Button_Record);

   procedure On_Open_Button_Clicked
     (Object : access Gtk_Button_Record);

   procedure On_Save_Button_Clicked
     (Object : access Gtk_Button_Record);

   procedure On_Text_Changed
     (Object : access Gtk_Text_Record);

end Main_Window_Pkg.Callbacks;
