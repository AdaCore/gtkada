
with Gdk.Colormap;
with Gdk.GC;
with Gdk.Visual;
with Gdk.Window;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Preview_Info;
with Gtk.Widget;

package Gtk.Preview is

   type Gtk_Preview is new Gtk.Widget.Gtk_Widget with private;

   procedure Draw_Row
      (Preview : in Gtk_Preview'Class;
       Data    : in String;
       X       : in Gint;
       Y       : in Gint;
       W       : in Gint);
   function Get_Cmap return Gdk.Colormap.Gdk_Colormap'Class;
   function Get_Info return Gtk.Preview_Info.Gtk_Preview_Info'Class;
   function Get_Visual return Gdk.Visual.Gdk_Visual'Class;
   procedure Gtk_New (Widget   : out Gtk_Preview;
                      The_Type : in Gtk_Preview_Type);
   procedure Put
      (Preview : in Gtk_Preview'Class;
       Window  : in Gdk.Window.Gdk_Window'Class;
       Gc      : in Gdk.GC.Gdk_GC'Class;
       Srcx    : in Gint;
       Srcy    : in Gint;
       Destx   : in Gint;
       Desty   : in Gint;
       Width   : in Gint;
       Height  : in Gint);
   procedure Put_Row
      (Preview : in Gtk_Preview'Class;
       Src     : in String;
       Dest    : in String;
       X       : in Gint;
       Y       : in Gint;
       W       : in Gint);
   procedure Reset;
   procedure Set_Color_Cube
      (Nred_Shades   : in Guint;
       Ngreen_Shades : in Guint;
       Nblue_Shades  : in Guint;
       Ngray_Shades  : in Guint);
   procedure Set_Expand
      (Preview : in Gtk_Preview'Class;
       Expand  : in Gint);
   procedure Set_Gamma (Gamma : in double);
   procedure Set_Install_Cmap (Install_Cmap : in Gint);
   procedure Set_Reserved (Nreserved : in Gint);
   procedure Size
      (Preview : in Gtk_Preview'Class;
       Width   : in Gint;
       Height  : in Gint);
   procedure Uninit;

private
   type Gtk_Preview is new Gtk.Widget.Gtk_Widget with null record;

   --  mapping: Draw_Row gtkpreview.h gtk_preview_draw_row
   --  mapping: Get_Cmap gtkpreview.h gtk_preview_get_cmap
   --  mapping: Get_Info gtkpreview.h gtk_preview_get_info
   --  mapping: NOT_IMPLEMENTED gtkpreview.h gtk_preview_get_type
   --  mapping: Get_Visual gtkpreview.h gtk_preview_get_visual
   --  mapping: Gtk_New gtkpreview.h gtk_preview_new
   --  mapping: Put gtkpreview.h gtk_preview_put
   --  mapping: Put_Row gtkpreview.h gtk_preview_put_row
   --  mapping: Reset gtkpreview.h gtk_preview_reset
   --  mapping: Set_Color_Cube gtkpreview.h gtk_preview_set_color_cube
   --  mapping: Set_Expand gtkpreview.h gtk_preview_set_expand
   --  mapping: Set_Gamma gtkpreview.h gtk_preview_set_gamma
   --  mapping: Set_Install_Cmap gtkpreview.h gtk_preview_set_install_cmap
   --  mapping: Set_Reserved gtkpreview.h gtk_preview_set_reserved
   --  mapping: Size gtkpreview.h gtk_preview_size
   --  mapping: Uninit gtkpreview.h gtk_preview_uninit
end Gtk.Preview;
