
with Gdk.Bitmap;
with Gtk.Misc;

package Gtk.Pixmap is

   type Gtk_Pixmap is new Gtk.Misc.Gtk_Misc with private;

   procedure Get
      (Pixmap : in Gtk_Pixmap'Class;
       Val    : in Gdk_Pixmap'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class);
   procedure Gtk_New
      (Widget : out Gtk_Pixmap;
       Pixmap : in Gdk_Pixmap'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class);
   procedure Set
      (Pixmap : in Gtk_Pixmap'Class;
       Val    : in Gdk_Pixmap'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class);

private
   type Gtk_Pixmap is new Gtk.Misc.Gtk_Misc with null record;

   --  mapping: Get gtkpixmap.h gtk_pixmap_get
   --  mapping: NOT_IMPLEMENTED gtkpixmap.h gtk_pixmap_get_type
   --  mapping: Gtk_New gtkpixmap.h gtk_pixmap_new
   --  mapping: Set gtkpixmap.h gtk_pixmap_set
end Gtk.Pixmap;
