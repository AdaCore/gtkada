with Gtk.File_Selection; use Gtk.File_Selection;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
with Gtk.Button; use Gtk.Button;
package Open_File_Selection_Pkg is

   type Open_File_Selection_Record is new Gtk_File_Selection_Record with record
      Ok_Button1 : Gtk_Button;
      Cancel_Button1 : Gtk_Button;
   end record;
   type Open_File_Selection_Access is access all Open_File_Selection_Record'Class;

   procedure Gtk_New (Open_File_Selection : out Open_File_Selection_Access);
   procedure Initialize (Open_File_Selection : access Open_File_Selection_Record'Class);

end Open_File_Selection_Pkg;
