with Glib; use Glib;

with Gdk.Visual;

package Gdk.Color is

   type Gdk_Color is new Root_Type with private;
   type Gdk_Colormap is new Root_Type with private;

   Null_Color : constant Gdk_Color;


   procedure Gdk_New (Colormap     :    out Gdk_Colormap;
                      Visual       : in     Gdk.Visual.Gdk_Visual;
                      Private_Cmap : in     Gint);
   --  mapping: Gdk_New gdk.h gdk_colormap_new

   procedure Change (Colormap : in Gdk_Colormap;
                     Ncolors  : in Gint);
   --  mapping: Change gdk.h gdk_colormap_change

   procedure Store (Colormap : in out Gdk_Colormap;
                    Colors   : in     Gdk_Color'Class;
                    Ncolors  : in     Gint);
   --  mapping: Store gdk.h gdk_colors_store

   procedure Alloc (Colormap   : in out Gdk_Colormap;
                    Contiguous : in     Boolean;
                    Planes     : in     Gulong_Array;
                    Pixels     : in     Gulong_Array;
                    Succeeded  :    out Boolean);
   --  mapping: Alloc gdk.h gdk_colors_alloc

   procedure Free (Colormap : in out Gdk_Colormap;
                   Pixels   : in     Gulong_Array;
                   Planes   : in     Gulong);
   --  mapping: Free gdk.h gdk_colors_free

   procedure White (Colormap  : in out Gdk_Colormap;
                    Color     : in out Gdk_Color'Class;
                    Succeeded :    out Boolean);
   --  mapping: White gdk.h gdk_color_white

   procedure Black (Colormap  : in out Gdk_Colormap;
                    Color     : in out Gdk_Color'Class;
                    Succeeded :    out Boolean);
   --  mapping: Black gdk.h gdk_color_black

   procedure Parse (Spec      : in     String;
                    Color     : in out Gdk_Color'Class;
                    Succeeded :    out Boolean);
   --  mapping: Parse gdk.h gdk_color_parse

   procedure Alloc (Colormap  : in out Gdk_Colormap;
                    Color     : in out Gdk_Color'Class;
                    Succeeded :    out Boolean);
   --  mapping: Alloc gdk.h gdk_color_alloc

   procedure Change (Colormap  : in out Gdk_Colormap;
                    Color     : in out Gdk_Color'Class;
                    Succeeded :    out Boolean);
   --  mapping: Change gdk.h gdk_color_change

   function "=" (Colora, Colorb : in Gdk_Color'Class) return Boolean;
   --  mapping: "=" gdk.h gdk_color_equal

private

   type Gdk_Color is new Root_Type with null record;
   type Gdk_Colormap is new Root_Type with null record;

   Null_Color : constant Gdk_Color := (Ptr => System.Null_Address);

   --  mapping: NOT_IMPLEMENTED gdk.h gdk_colormap_ref
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_colormap_unref
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_colormap_get_system
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_colormap_get_system_size

end Gdk.Color;
