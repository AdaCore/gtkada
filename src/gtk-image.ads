
with Gdk.Bitmap;
with Gdk.Image;
with Gtk.Misc;

package Gtk.Image is

   type Gtk_Image is new Gtk.Misc.Gtk_Misc with private;

   procedure Get
      (Image : in Gtk_Image'Class;
       Val   : in Gdk.Image.Gdk_Image'Class;
       Mask  : in Gdk.Bitmap.Gdk_Bitmap'Class);
   procedure Gtk_New
      (Widget : out Gtk_Image;
       Val    : in Gdk.Image.Gdk_Image'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class);
   procedure Set
      (Image : in Gtk_Image'Class;
       Val   : in Gdk.Image.Gdk_Image'Class;
       Mask  : in Gdk.Bitmap.Gdk_Bitmap'Class);

private
   type Gtk_Image is new Gtk.Misc.Gtk_Misc with null record;

   --  mapping: Get gtkimage.h gtk_image_get
   --  mapping: NOT_IMPLEMENTED gtkimage.h gtk_image_get_type
   --  mapping: Gtk_New gtkimage.h gtk_image_new
   --  mapping: Set gtkimage.h gtk_image_set
end Gtk.Image;
