with Glib; use Glib;
with Gdk.Bitmap;
with Gdk.Color;
with Gdk.Window;
with Interfaces.C.Strings;

package Gdk.Pixmap is

   type Gdk_Pixmap is new Root_Type with private;

   procedure Gtk_New (Pixmap :    out Gdk_Pixmap;
                      Window : in     Gdk.Window.Gdk_Window;
                      Width  : in     Gint;
                      Height : in     Gint;
                      Depth  : in     Gint);
   --  mapping: Gtk_New gdk.h gdk_pixmap_new

   procedure Create_From_Data (Pixmap :    out Gdk_Pixmap;
                               Window : in     Gdk.Window.Gdk_Window'Class;
                               Data   : in     String;
                               Width  : in     Gint;
                               Height : in     Gint;
                               Depth  : in     Gint;
                               Fg     : in     Color.Gdk_Color'Class;
                               Bg     : in     Color.Gdk_Color'Class);
   --  mapping: Create_From_Data gdk.h gdk_pixmap_create_from_data

   procedure Create_From_Xpm (Pixmap      : out Gdk_Pixmap;
                              Window      : in  Gdk.Window.Gdk_Window'Class;
                              Mask        : out Gdk.Bitmap.Gdk_Bitmap;
                              Transparent : in  Gdk.Color.Gdk_Color'Class;
                              Filename    : in  String);
   --  mapping: Create_From_Xpm gdk.h gdk_pixmap_create_from_xpm

   procedure Create_From_Xpm_D
     (Pixmap      : out Gdk_Pixmap;
      Window      : in  Gdk.Window.Gdk_Window'Class;
      Mask        : out Gdk.Bitmap.Gdk_Bitmap;
      Transparent : in  Gdk.Color.Gdk_Color'Class;
      Data        : in  Interfaces.C.Strings.chars_ptr_array);
   --  mapping: Create_From_Xpm_D gdk.h gdk_pixmap_create_from_xpm_d

   --  mapping: NOT_IMPLEMENTED gdk.h gdk_pixmap_colormap_create_from_xpm_d

private

   type Gdk_Pixmap is new Root_Type with null record;

   --  mapping: NOT_IMPLEMENTED gdk.h gdk_pixmap_ref
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_pixmap_unref

end Gdk.Pixmap;
