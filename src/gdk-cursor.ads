with Glib; use Glib;
with Gdk.Color;
with Gdk.Pixmap;
with Gdk.Types;

package Gdk.Cursor is

   type Gdk_Cursor is new Root_Type with private;

   procedure Gdk_New (Widget      : out Gdk_Cursor;
                      Cursor_Type : in  Gdk.Types.Gdk_Cursor_Type);
   --  mapping: Gdk_New gdk.h gdk_cursor_new

   procedure Gdk_New
      (Widget : out Gdk_Cursor;
       Source : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask   : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Fg     : in Gdk.Color.Gdk_Color'Class;
       Bg     : in Gdk.Color.Gdk_Color'Class;
       X      : in Gint;
       Y      : in Gint);
   --  mapping: Gdk_New gdk.h gdk_cursor_new_from_pixmap

   procedure Destroy (Cursor : in out Gdk_Cursor'Class);
   --  mapping: Destroy gdk.h gdk_cursor_destroy

private

   type Gdk_Cursor is new Root_Type with null record;

end Gdk.Cursor;
