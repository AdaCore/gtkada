
with Gtk.Menu_Shell;
with Gtk.Widget;

package Gtk.Menu_Bar is

   type Gtk_Menu_Bar is new Gtk.Menu_Shell.Gtk_Menu_Shell with private;

   procedure Gtk_New (Widget : out Gtk_Menu_Bar);
   procedure Append
     (Menu_Bar : in Gtk_Menu_Bar'Class;
      Child    : in Gtk.Widget.Gtk_Widget'Class);
   procedure Insert
     (Menu_Bar : in Gtk_Menu_Bar'Class;
      Child    : in Gtk.Widget.Gtk_Widget'Class;
      Position : in Gint);
   procedure Prepend
     (Menu_Bar : in Gtk_Menu_Bar'Class;
      Child    : in Gtk.Widget.Gtk_Widget'Class);

private

   type Gtk_Menu_Bar is new Gtk.Menu_Shell.Gtk_Menu_Shell with null record;

   --  mapping: NOT_IMPLEMENTED gtkmenubar.h gtk_menu_bar_get_type
   --  mapping: Gtk_New gtkmenubar.h gtk_menu_bar_new
   --  mapping: Append gtkmenubar.h gtk_menu_bar_append
   --  mapping: Prepend gtkmenubar.h gtk_menu_bar_prepend
   --  mapping: Insert gtkmenubar.h gtk_menu_bar_insert

end Gtk.Menu_Bar;
