with Glib; use Glib;

with Gdk.Window;

package Gdk.Bitmap is

   type Gdk_Bitmap is new Root_Type with private;

   procedure Create_From_Data (Bitmap :    out Gdk_Bitmap;
                               Window : in     Gdk.Window.Gdk_Window;
                               Data   : in     String;
                               Width  : in     Gint;
                               Height : in     Gint);
   --  mapping: Create_From_Data gdk.h gdk_bitmap_create_from_data


private

   type Gdk_Bitmap is new Root_Type with null record;

   --  mapping: NOT_IMPLEMENTED gdk.h gdk_bitmap_ref
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_bitmap_unref

end Gdk.Bitmap;
