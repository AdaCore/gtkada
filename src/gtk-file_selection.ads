with Gtk.Window;

package Gtk.File_Selection is

   type Gtk_File_Selection is new Window.Gtk_Window with private;


   procedure Gtk_New (File_Selection : out Gtk_File_Selection);
   --  mapping: Gtk_New gtkfilesel.h gtk_file_selection_new

   procedure Set_Filename (File_Selection : in out Gtk_File_Selection'Class;
                           Filename       : in     String);
   --  mapping: Set_Filename gtkfilesel.h gtk_file_selection_set_filename

   function Get_Filename (File_Selection : in Gtk_File_Selection'Class)
                          return String;
   --  mapping: Get_Filename gtkfilesel.h gtk_file_selection_get_filename

   procedure Show_Fileop_Buttons
     (File_Selection : in out Gtk_File_Selection'Class);
   --  mapping: Show_Fileop_Buttons gtkfilesel.h \
   --  mapping:                     gtk_file_selection_show_fileop_buttons

   procedure Hide_Fileop_Buttons
     (File_Selection : in out Gtk_File_Selection'Class);
   --  mapping: Hide_Fileop_Buttons gtkfilesel.h \
   --  mapping:                     gtk_file_selection_hide_fileop_buttons

private

   type Gtk_File_Selection is new Window.Gtk_Window with null record;

   --  mapping: USE_OBJECT_ORIENTED gtkfilesel.h gtk_file_selection_get_type

end Gtk.File_Selection;
