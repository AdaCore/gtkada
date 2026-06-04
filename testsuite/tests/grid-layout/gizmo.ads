--  A minimal custom widget, ported from the GtkGizmo helper in GTK's
--  testsuite/gtk/grid-layout.c. It advertises configurable minimum and
--  natural sizes through "measure" and records the size it is handed through
--  "size_allocate" (without chaining to GtkWidget, exactly as the C gizmo).
--
--  See gtkada_demo/create_custom_widget.adb for the general pattern of
--  overriding GtkWidget virtual methods from Ada.

with Glib;       use Glib;
with Gtk.Widget; use Gtk.Widget;

package Gizmo is

   type Gizmo_Record is new Gtk_Widget_Record with record
      Min_Width, Min_Height : Gint := 0;
      Nat_Width, Nat_Height : Gint := 0;
      Alloc_Width           : Gint := 0;
      Alloc_Height          : Gint := 0;
   end record;
   type Gizmo_Widget is access all Gizmo_Record'Class;

   procedure Gtk_New (Self : out Gizmo_Widget);
   --  Create a new gizmo. All sizes default to zero.

   procedure Set_Sizes
     (Self                  : not null access Gizmo_Record'Class;
      Min_Width, Min_Height : Gint;
      Nat_Width, Nat_Height : Gint);
   --  Set the minimum and natural sizes advertised by "measure".

   function Allocated_Width
     (Self : not null access Gizmo_Record'Class) return Gint;
   function Allocated_Height
     (Self : not null access Gizmo_Record'Class) return Gint;
   --  The width/height the gizmo was last allocated by "size_allocate".

end Gizmo;
