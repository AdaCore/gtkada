with Glib; use Glib;
with Gdk.Color;
with Gdk.Window;

package Gdk.Pixmap is

   type Gdk_Pixmap is new Root_Type with private;

   procedure Gtk_New (Pixmap :    out Gdk_Pixmap;
                      Window : in     Gdk.Window.Gdk_Window;
                      Width  : in     Gint;
                      Height : in     Gint;
                      Depth  : in     Gint);
   --  mapping: Gtk_New gdk.h gdk_pixmap_new

   procedure Create_From_Data (Pixmap :    out Gdk_Pixmap;
                               Window : in     Gdk.Window.Gdk_Window;
                               Data   : in     String;
                               Width  : in     Gint;
                               Height : in     Gint;
                               Depth  : in     Gint;
                               Fg     : in     Color.Gdk_Color;
                               Bg     : in     Color.Gdk_Color);
   --  mapping: Create_From_Data gdk.h gdk_pixmap_create_from_data


   --  mapping: NOT_IMPLEMENTED gdk.h gdk_pixmap_create_from_xpm
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_pixmap_create_from_xpm_d
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_pixmap_colormap_create_from_xpm_d
   --
   --  FIXME : Can not implement for the moment due to a parameter
   --  FIXME : beeing GdkBitmap ** mask.
   --  FIXME : How should we do this?


private

   type Gdk_Pixmap is new Root_Type with null record;

   --  mapping: NOT_IMPLEMENTED gdk.h gdk_pixmap_ref
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_pixmap_unref

end Gdk.Pixmap;
