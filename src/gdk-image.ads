with Glib; use Glib;

with Gdk.Visual;
with Gdk.Window;

package Gdk.Image is


   type Gdk_Image is new Gdk.Root_Type with private;

   type Gdk_Image_Type is (Image_Normal,
                           Image_Shared,
                           Image_Fastest);

   procedure Gdk_New (Image      :    out Gdk_Image;
                      Image_Type : in     Gdk_Image_Type;
                      Visual     : in     Gdk.Visual.Gdk_Visual;
                      Width      : in     Gint;
                      Height     : in     Gint);
   --  mapping: Gdk_New gdk.h gdk_image_new

   procedure Get (Image  :    out Gdk_Image;
                  Window : in     Gdk.Window.Gdk_Window;
                  X      : in     Gint;
                  Y      : in     Gint;
                  Width  : in     Gint;
                  Height : in     Gint);
   --  mapping: Get gdk.h gdk_image_get

   procedure Put_Pixel (Image : in out Gdk_Image;
                        X     : in     Gint;
                        Y     : in     Gint;
                        Pixel : in     Guint32);
   --  mapping: Put_Pixel gdk.h gdk_image_put_pixel

   function Get_Pixel (Image : in Gdk_Image;
                       X     : in Gint;
                       Y     : in Gint) return Guint32;
   --  mapping: Get_Pixel gdk.h gdk_image_get_pixel

   procedure Destroy (Image : in out Gdk_Image);
   --  mapping: Destroy gdk.h gdk_image_destroy



private

   type Gdk_Image is new Gdk.Root_Type with null record;


   --  mapping: NOT_IMPLEMENTED gdk.h gdk_image_new_bitmap

end Gdk.Image;
