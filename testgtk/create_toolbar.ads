with Gtk.Button; use Gtk.Button;
with Gtk.Toolbar; use Gtk.Toolbar;
with Gtk.Window;  use Gtk.Window;

package Create_Toolbar is

   procedure Make_Toolbar (Toolbar    : out Gtk_Toolbar;
                           Toplevel   : in out Gtk_Window;
                           With_Entry : in Boolean := False);
   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class);

end Create_Toolbar;
