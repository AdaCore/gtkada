
with Gtk.Signal;
with Gtk.Window; use Gtk.Window;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Button; use Gtk.Button;
with Gtk.Text; use Gtk.Text;
with Gtk.File_Selection; use Gtk.File_Selection;

package Callbacks_Gladeedit is

   package Window_Callback is new
     Gtk.Signal.Void_Callback (Gtk_Window_Record);

   package Menu_Item_Callback is new
     Gtk.Signal.Void_Callback (Gtk_Menu_Item_Record);

   package Button_Callback is new
     Gtk.Signal.Void_Callback (Gtk_Button_Record);

   package Text_Callback is new
     Gtk.Signal.Void_Callback (Gtk_Text_Record);

   package File_Selection_Callback is new
     Gtk.Signal.Void_Callback (Gtk_File_Selection_Record);

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

   procedure On_Open_Filesel_Delete_Event
     (Object : access Gtk_File_Selection_Record);

   procedure On_Open_Filesel_Ok_Button_Clicked
     (Object : access Gtk_Button_Record);

   procedure On_Open_Filesel_Cancel_Button_Clicked
     (Object : access Gtk_Button_Record);

   procedure On_About_Ok_Clicked
     (Object : access Gtk_Button_Record);

   procedure On_Save_Filesel_Delete_Event
     (Object : access Gtk_File_Selection_Record);

   procedure On_Save_Filesel_Ok_Button_Clicked
     (Object : access Gtk_Button_Record);

   procedure On_Save_Filesel_Cancel_Button_Clicked
     (Object : access Gtk_Button_Record);

end Callbacks_Gladeedit;
