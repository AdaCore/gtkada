
with Glib.Glist; use Glib.Glist;
with Gtk.Container;
with Gtk.Widget;

package Gtk.Fixed is

   type Gtk_Fixed is new Gtk.Container.Gtk_Container with private;
 
   package Children_List is new Glib.Glist.Generic_List
     (Gtk.Widget.Gtk_Widget'Class);

   function Get_Children (Widget : in Gtk.Fixed.Gtk_Fixed'Class)
                          return      Children_List.Glist;
   procedure Gtk_New (Widget : out Gtk_Fixed);
   procedure Move
      (Fixed  : in Gtk_Fixed'Class;
       Widget : in Gtk.Widget.Gtk_Widget'Class;
       X      : in Gint16;
       Y      : in Gint16);
   procedure Put
      (Fixed  : in Gtk_Fixed'Class;
       Widget : in Gtk.Widget.Gtk_Widget'Class;
       X      : in Gint16;
       Y      : in Gint16);

private
   type Gtk_Fixed is new Gtk.Container.Gtk_Container with null record;

   --  mapping: Get_Children gtkfixed.h GtkFixed->children
   --  mapping: NOT_IMPLEMENTED gtkfixed.h gtk_fixed_get_type
   --  mapping: Gtk_New gtkfixed.h gtk_fixed_new
   --  mapping: Move gtkfixed.h gtk_fixed_move
   --  mapping: Put gtkfixed.h gtk_fixed_put
end Gtk.Fixed;
