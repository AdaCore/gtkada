
with Gtk.Hbox;
with Gtk.Button;
with Gtk.Widget;
with Gtk.Window;

package Gtk.File_Selection is

   type Gtk_File_Selection is new Gtk.Window.Gtk_Window with private;

   function Get_Action_Area (Widget : in Gtk_File_Selection'Class)
                             return      Gtk.Hbox.Gtk_Hbox;
   function Get_Button_Area (Widget : in Gtk_File_Selection'Class)
                             return      Gtk.Hbox.Gtk_Hbox;
   function Get_Cancel_Button (Widget : in Gtk_File_Selection'Class)
                               return      Gtk.Button.Gtk_Button;
   function Get_Dir_List (Widget : in Gtk_File_Selection'Class)
                          return      Gtk.Widget.Gtk_Widget'Class;
   function Get_File_List (Widget : in Gtk_File_Selection'Class)
                           return      Gtk.Widget.Gtk_Widget'Class;
   function Get_Filename (Filesel : in Gtk_File_Selection'Class)
                          return       String;
   function Get_Help_Button (Widget : in Gtk_File_Selection'Class)
                             return      Gtk.Button.Gtk_Button;
   function Get_History_Pulldown (Widget : in Gtk_File_Selection'Class)
                                  return      Gtk.Widget.Gtk_Widget'Class;
   function Get_Ok_Button (Widget : in Gtk_File_Selection'Class)
                           return      Gtk.Button.Gtk_Button;
   function Get_Selection_Entry (Widget : in Gtk_File_Selection'Class)
                                 return      Gtk.Widget.Gtk_Widget'Class;
   function Get_Selection_Text (Widget : in Gtk_File_Selection'Class)
                                return      Gtk.Widget.Gtk_Widget'Class;
   procedure Gtk_New (Widget : out Gtk_File_Selection;
                      Title  : in String);
   procedure Hide_Fileop_Buttons (Filesel : in out Gtk_File_Selection'Class);
   procedure Set_Filename
      (Filesel  : in Gtk_File_Selection'Class;
       Filename : in String);
   procedure Show_Fileop_Buttons (Filesel : in out Gtk_File_Selection'Class);

private
   type Gtk_File_Selection is new Gtk.Window.Gtk_Window with null record;

   --  mapping: Get_Action_Area gtkfilesel.h GtkFileSelection->action_area
   --  mapping: Get_Button_Area gtkfilesel.h GtkFileSelection->button_area
   --  mapping: Get_Cancel_Button gtkfilesel.h GtkFileSelection->cancel_button
   --  mapping: Get_Dir_List gtkfilesel.h GtkFileSelection->dir_list
   --  mapping: Get_File_List gtkfilesel.h GtkFileSelection->file_list
   --  mapping: Get_Filename gtkfilesel.h gtk_file_selection_get_filename
   --  mapping: Get_Help_Button gtkfilesel.h GtkFileSelection->help_button
   --  mapping: Get_History_Pulldown gtkfilesel.h \
   --  mapping:    GtkFileSelection->history_pulldown
   --  mapping: Get_Ok_Button gtkfilesel.h GtkFileSelection->ok_button
   --  mapping: Get_Selection_Entry gtkfilesel.h \
   --  mapping:    GtkFileSelection->selection_entry
   --  mapping: Get_Selection_Text gtkfilesel.h \
   --  mapping:    GtkFileSelection->selection_text
   --  mapping: NOT_IMPLEMENTED gtkfilesel.h gtk_file_selection_get_type
   --  mapping: Gtk_New gtkfilesel.h gtk_file_selection_new
   --  mapping: Hide_Fileop_Buttons gtkfilesel.h \
   --  mapping:    gtk_file_selection_hide_fileop_buttons
   --  mapping: Set_Filename gtkfilesel.h gtk_file_selection_set_filename
   --  mapping: Show_Fileop_Buttons gtkfilesel.h \
   --  mapping:    gtk_file_selection_show_fileop_buttons
end Gtk.File_Selection;
