with Gtk.Toolbar; use Gtk.Toolbar;
with Gtk.Window;  use Gtk.Window;

package Create_Toolbar is

   procedure Make_Toolbar (Toolbar : out Gtk_Toolbar;
                           Toplevel : in out Gtk_Window);

end Create_Toolbar;
