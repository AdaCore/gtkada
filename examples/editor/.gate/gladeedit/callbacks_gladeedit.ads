with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Text; use Gtk.Text;
with Gtk.Button; use Gtk.Button;

package Callbacks_Gladeedit is

   package Menu_Item_Callback is new
     Gtk.Handlers.Callback (Gtk_Menu_Item_Record);

   package Text_Callback is new
     Gtk.Handlers.Callback (Gtk_Text_Record);

   package Button_Callback is new
     Gtk.Handlers.Callback (Gtk_Button_Record);

end Callbacks_Gladeedit;
