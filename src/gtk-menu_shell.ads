with Gtk.Container;
with Gtk.Widget;

package Gtk.Menu_Shell is

   type Gtk_Menu_Shell is new Gtk.Container.Gtk_Container with private;

   procedure Append
     (Menu_Shell : in Gtk_Menu_Shell'Class;
      Child      : in Gtk.Widget.Gtk_Widget'Class);
   procedure Deactivate (Menu_Shell : in Gtk_Menu_Shell'Class);
   procedure Insert
     (Menu_Shell : in Gtk_Menu_Shell'Class;
      Child      : in Gtk.Widget.Gtk_Widget'Class;
      Position   : in Gint);
   procedure Prepend
     (Menu_Shell : in Gtk_Menu_Shell'Class;
      Child      : in Gtk.Widget.Gtk_Widget'Class);

private

   type Gtk_Menu_Shell is new Gtk.Container.Gtk_Container with null record;

   --  mapping: NOT_IMPLEMENTED gtkmenushell.h gtk_menu_shell_get_type
   --  mapping: Append gtkmenushell.h gtk_menu_shell_append
   --  mapping: Prepend gtkmenushell.h gtk_menu_shell_prepend
   --  mapping: Insert gtkmenushell.h gtk_menu_shell_insert
   --  mapping: Deactivate gtkmenushell.h gtk_menu_shell_deactivate

end Gtk.Menu_Shell;
