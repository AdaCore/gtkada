with Gtk.File_Selection; use Gtk.File_Selection;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
with Gtk.Button; use Gtk.Button;
package Save_File_Selection_Pkg is

   type Save_File_Selection_Record is new Gtk_File_Selection_Record with record
      Ok_Button3 : Gtk_Button;
      Cancel_Button3 : Gtk_Button;
   end record;
   type Save_File_Selection_Access is access all Save_File_Selection_Record'Class;

   procedure Gtk_New (Save_File_Selection : out Save_File_Selection_Access);
   procedure Initialize (Save_File_Selection : access Save_File_Selection_Record'Class);

   Save_File_Selection : Save_File_Selection_Access;

end Save_File_Selection_Pkg;
