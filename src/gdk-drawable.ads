with Glib; use Glib;
with Gdk.GC;
with Gdk.Window;

package Gdk.Drawable is

   subtype Gdk_Drawable is Window.Gdk_Window;

   procedure Draw_Rectangle (Drawable : in Gdk_Drawable'Class;
                             Gc       : in Gdk.GC.Gdk_GC'Class;
                             Filled   : in Boolean := False;
                             X        : in Gint;
                             Y        : in Gint;
                             Width    : in Gint;
                             Height   : in Gint);
   --  mapping: Draw_Rectangle gdk.h gdk_draw_rectangle

end Gdk.Drawable;
