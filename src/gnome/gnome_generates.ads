with Ada.Text_IO; use Ada.Text_IO;
with Glib.Glade; use Glib.Glade; use Glib.Glade.Glib_XML;

package Gnome_Generates is

   procedure Color_Picker_Generate (N : Node_Ptr; File : File_Type);

end Gnome_Generates;
