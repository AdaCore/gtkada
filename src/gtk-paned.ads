
with Gtk.Container;
with Gtk.Widget;

package Gtk.Paned is

   type Gtk_Paned is new Gtk.Container.Gtk_Container with private;

   procedure Add1
      (Paned : in Gtk_Paned'Class;
       Child : in Gtk.Widget.Gtk_Widget'Class);
   procedure Add2
      (Paned : in Gtk_Paned'Class;
       Child : in Gtk.Widget.Gtk_Widget'Class);
   procedure Gtk_New_Vpaned (Widget : out Gtk_Paned);
   procedure Gtk_New_Hpaned (Widget : out Gtk_Paned);
   procedure Gutter_Size
      (Paned : in Gtk_Paned'Class;
       Size  : in Guint16);
   procedure Handle_Size
      (Paned : in Gtk_Paned'Class;
       Size  : in Guint16);

private
   type Gtk_Paned is new Gtk.Container.Gtk_Container with null record;

   --  mapping: Add1 gtkpaned.h gtk_paned_add1
   --  mapping: Add2 gtkpaned.h gtk_paned_add2
   --  mapping: NOT_IMPLEMENTED gtkpaned.h gtk_paned_get_type
   --  mapping: Gutter_Size gtkpaned.h gtk_paned_gutter_size
   --  mapping: Handle_Size gtkpaned.h gtk_paned_handle_size
   --  mapping: Gtk_New_Vpaned gtkvpaned.h gtk_vpaned_new
   --  mapping: Gtk_New_Hpaned gtkhpaned.h gtk_hpaned_new
end Gtk.Paned;

