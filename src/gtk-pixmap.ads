
with Gdk.Bitmap;
with Gdk.Pixmap;
with Gtk.Misc;

package Gtk.Pixmap is

   type Gtk_Pixmap is new Gtk.Misc.Gtk_Misc with private;

   procedure Get
      (Pixmap : in Gtk_Pixmap'Class;
       Val    : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class);
   function Get_Mask (Widget : in Gtk_Pixmap'Class)
                      return      Gdk.Bitmap.Gdk_Bitmap'Class;
   function Get_Pixmap (Widget : in Gtk_Pixmap'Class)
                        return      Gdk.Pixmap.Gdk_Pixmap'Class;
   procedure Gtk_New
      (Widget : out Gtk_Pixmap;
       Pixmap : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class);
   procedure Set
      (Pixmap : in Gtk_Pixmap'Class;
       Val    : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class);

private
   type Gtk_Pixmap is new Gtk.Misc.Gtk_Misc with null record;

   --  mapping: Get gtkpixmap.h gtk_pixmap_get
   --  mapping: Get_Mask gtkpixmap.h GtkPixmap->mask
   --  mapping: Get_Pixmap gtkpixmap.h GtkPixmap->pixmap
   --  mapping: NOT_IMPLEMENTED gtkpixmap.h gtk_pixmap_get_type
   --  mapping: Gtk_New gtkpixmap.h gtk_pixmap_new
   --  mapping: Set gtkpixmap.h gtk_pixmap_set
end Gtk.Pixmap;
