with Gtk_Generates; use Gtk_Generates;

package body Gnome_Generates is

   procedure Color_Picker_Generate (N : Node_Ptr; File : File_Type) is
   begin
      Gen_New (N, "Color_Picker", File => File);
      Widget_Generate (N, File);
   end Color_Picker_Generate;

end Gnome_Generates;
