with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Box; use Gtk.Box;
with Gtk.Label; use Gtk.Label;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
with Gtk.Window; use Gtk.Window;

package About_Dialog_Pkg is

   type About_Dialog_Record is new Gtk_Dialog_Record with record
      Dialog_Vbox2 : Gtk_Vbox;
      Label2 : Gtk_Label;
      Dialog_Action_Area2 : Gtk_Hbox;
      Button7 : Gtk_Button;
   end record;
   type About_Dialog_Access is access all About_Dialog_Record'Class;

   procedure Gtk_New (About_Dialog : out About_Dialog_Access);
   procedure Initialize (About_Dialog : access About_Dialog_Record'Class);

   About_Dialog : About_Dialog_Access;

end About_Dialog_Pkg;
