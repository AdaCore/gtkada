-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <c_version>1.3.4</c_version>

with Gdk; use Gdk;
with Gdk.Color;
with Gdk.GC;
with Gdk.Font;
with Gdk.Types;
with Gdk.Pixmap;
with Gdk.Window;
with Gtk.Enums;

package Gtk.Style is

   type Gtk_Style is new Gdk.C_Proxy;
   --  Type used to handle styles.
   --  @pxref{Package_Gtk.Widget} for style handling.

   Null_Style : constant Gtk_Style := null;

   procedure Gtk_New (Style : out Gtk_Style);

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Style.

   function Copy (Source : Gtk_Style) return Gtk_Style;

   function Attach
     (Style  : Gtk_Style;
      Window : Gdk.Window.Gdk_Window) return Gtk_Style;

   procedure Detach (Style : Gtk_Style);

   procedure Ref (Object : Gtk_Style);

   procedure Unref (Object : Gtk_Style);

   procedure Set_Background
     (Style      : Gtk_Style;
      Window     : Gdk.Window.Gdk_Window;
      State_Type : Enums.Gtk_State_Type);

   procedure Draw_Hline
     (Style      : Gtk_Style;
      Window     : Gdk.Window.Gdk_Window;
      State_Type : Enums.Gtk_State_Type;
      X1, X2     : Gint;
      Y          : Gint);

   procedure Draw_Vline
     (Style      : Gtk_Style;
      Window     : Gdk.Window.Gdk_Window;
      State_Type : Enums.Gtk_State_Type;
      Y1, Y2     : Gint;
      X          : Gint);

   procedure Draw_Shadow
     (Style       : Gtk_Style;
      Window      : Gdk.Window.Gdk_Window;
      State_Type  : Enums.Gtk_State_Type;
      Shadow_Type : Enums.Gtk_Shadow_Type;
      X, Y        : Gint;
      Width       : Gint;
      Height      : Gint);

   procedure Draw_Polygon
     (Style       : Gtk_Style;
      Window      : Gdk.Window.Gdk_Window;
      State_Type  : Enums.Gtk_State_Type;
      Shadow_Type : Enums.Gtk_Shadow_Type;
      Points      : Gdk.Types.Gdk_Points_Array;
      Fill        : Boolean);

   procedure Draw_Arrow
     (Style       : Gtk_Style;
      Window      : Gdk.Window.Gdk_Window;
      State_Type  : Enums.Gtk_State_Type;
      Shadow_Type : Enums.Gtk_Shadow_Type;
      Arrow_Type  : Enums.Gtk_Arrow_Type;
      Fill        : Boolean;
      X, Y        : Gint;
      Width       : Gint;
      Height      : Gint);

   procedure Draw_Diamond
     (Style       : Gtk_Style;
      Window      : Gdk.Window.Gdk_Window;
      State_Type  : Enums.Gtk_State_Type;
      Shadow_Type : Enums.Gtk_Shadow_Type;
      X, Y        : Gint;
      Width       : Gint;
      Height      : Gint);

   procedure Draw_String
     (Style       : Gtk_Style;
      Window      : Gdk.Window.Gdk_Window;
      State_Type  : Enums.Gtk_State_Type;
      X, Y        : Gint;
      Str         : String);
   --  pragma Deprecated (Draw_String);

   procedure Set_Foreground
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color);

   procedure Set_Fg
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color) renames Set_Foreground;

   function Get_Foreground
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color;

   function Get_Fg
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color
      renames Get_Foreground;

   procedure Set_Background
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color);

   procedure Set_Bg
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color) renames Set_Background;

   function Get_Background
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color;

   function Get_Bg
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color
      renames Get_Background;

   procedure Set_Light
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color);

   function Get_Light
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color;

   procedure Set_Dark
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color);

   function Get_Dark
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color;

   procedure Set_Middle
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color);

   procedure Set_Mid
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color) renames Set_Middle;

   function Get_Middle
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color;

   function Get_Mid
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color
      renames Get_Middle;

   procedure Set_Text
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color);

   function Get_Text
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color;

   procedure Set_Base
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color);

   function Get_Base
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color;

   procedure Set_Black
     (Style : Gtk_Style;
      Color : Gdk.Color.Gdk_Color);

   function Get_Black (Style : Gtk_Style) return Gdk.Color.Gdk_Color;

   procedure Set_White (Style : Gtk_Style; Color : Gdk.Color.Gdk_Color);

   function Get_White (Style : Gtk_Style) return Gdk.Color.Gdk_Color;

   procedure Set_Font (Style : Gtk_Style; Font : Gdk.Font.Gdk_Font);

   function Get_Font (Style : Gtk_Style) return Gdk.Font.Gdk_Font;

   procedure Set_Black_GC (Style : Gtk_Style; GC : Gdk.GC.Gdk_GC);

   function Get_Black_GC (Style : Gtk_Style) return Gdk.GC.Gdk_GC;

   procedure Set_Black
     (Style : Gtk_Style; GC : Gdk.GC.Gdk_GC) renames Set_Black_GC;

   function Get_Black
     (Style : Gtk_Style) return Gdk.GC.Gdk_GC renames Get_Black_GC;

   procedure Set_White_GC (Style : Gtk_Style; GC : Gdk.GC.Gdk_GC);

   function Get_White_GC (Style : Gtk_Style) return Gdk.GC.Gdk_GC;

   procedure Set_White
     (Style : Gtk_Style; GC : Gdk.GC.Gdk_GC) renames Set_White_GC;

   function Get_White
     (Style : Gtk_Style) return Gdk.GC.Gdk_GC renames Get_White_GC;

   procedure Set_Foreground_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC);

   function Get_Foreground_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC;

   procedure Set_Foreground
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Foreground_GC;

   function Get_Foreground
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
      renames Get_Foreground_GC;

   procedure Set_Fg
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Foreground_GC;

   function Get_Fg
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
      renames Get_Foreground_GC;

   procedure Set_Fg_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Foreground_GC;

   function Get_Fg_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
      renames Get_Foreground_GC;

   procedure Set_Background_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC);

   function Get_Background_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC;

   procedure Set_Background
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Background_GC;

   function Get_Background
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
      renames Get_Background_GC;

   procedure Set_Bg
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Background_GC;

   function Get_Bg
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
      renames Get_Background_GC;

   procedure Set_Bg_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Background_GC;

   function Get_Bg_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
      renames Get_Background_GC;

   procedure Set_Light_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC);

   function Get_Light_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC;

   procedure Set_Light
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Light_GC;

   function Get_Light
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
      renames Get_Light_GC;

   procedure Set_Dark_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC);

   function Get_Dark_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC;

   procedure Set_Dark
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Dark_GC;

   function Get_Dark
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
      renames Get_Dark_GC;

   procedure Set_Middle_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC);

   function Get_Middle_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC;

   procedure Set_Middle
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Middle_GC;

   function Get_Middle
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
      renames Get_Middle_GC;

   procedure Set_Mid_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Middle_GC;

   function Get_Mid_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
      renames Get_Middle_GC;

   procedure Set_Mid
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Middle_GC;

   function Get_Mid
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
      renames Get_Middle_GC;

   procedure Set_Text_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC);

   function Get_Text_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC;

   procedure Set_Text
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Text_GC;

   function Get_Text
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
      renames Get_Text_GC;

   procedure Set_Base_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC);

   function Get_Base_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC;

   procedure Set_Base
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Base_GC;

   function Get_Base
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
      renames Get_Base_GC;

   procedure Set_Bg_Pixmap
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Pixmap     : Gdk.Pixmap.Gdk_Pixmap);

   function Get_Bg_Pixmap
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Pixmap.Gdk_Pixmap;

   function X_Thickness (Style : Gtk_Style) return Gint;
   --  Width of the vertical scrollbars and ranges when Style is applied.
   --  In fact, this thickness is used for a lot of widgets whose width
   --  does not depend on their content, such as rulers,...

   function Y_Thickness (Style : Gtk_Style) return Gint;
   --  Height of the horizontal scrollbars and ranges when Style is applied.

private
   pragma Import (C, Get_Type, "gtk_style_get_type");
   pragma Import (C, Attach, "gtk_style_attach");
   pragma Import (C, Copy, "gtk_style_copy");
   pragma Import (C, Detach, "gtk_style_detach");
   pragma Import (C, Ref, "gtk_style_ref");
   pragma Import (C, Unref, "gtk_style_unref");
   pragma Import (C, Draw_Diamond, "gtk_draw_diamond");
   pragma Import (C, Draw_Hline, "gtk_draw_hline");
   pragma Import (C, Draw_Shadow, "gtk_draw_shadow");
   pragma Import (C, Draw_Vline, "gtk_draw_vline");
   pragma Import (C, Set_Font, "ada_style_set_font");
   pragma Import (C, Get_Font, "ada_style_get_font");
   pragma Import (C, Set_Black_GC, "ada_style_set_black_gc");
   pragma Import (C, Get_Black_GC, "ada_style_get_black_gc");
   pragma Import (C, Set_White_GC, "ada_style_set_white_gc");
   pragma Import (C, Get_White_GC, "ada_style_get_white_gc");
   pragma Import (C, X_Thickness, "ada_style_get_x_thickness");
   pragma Import (C, Y_Thickness, "ada_style_get_y_thickness");
end Gtk.Style;

--  missing:
--  gtk_style_apply_default_background
--  gtk_style_lookup_icon_set
--  gtk_style_render_icon
--  gtk_draw_box
--  gtk_draw_flat_box
--  gtk_draw_check
--  gtk_draw_option
--  gtk_draw_tab
--  gtk_draw_shadow_gap
--  gtk_draw_box_gap
--  gtk_draw_extension
--  gtk_draw_focus
--  gtk_draw_slider
--  gtk_draw_handle
--  gtk_draw_expander
--  gtk_draw_layout
--  gtk_draw_resize_grip
--  gtk_paint_hline
--  gtk_paint_vline
--  gtk_paint_shadow
--  gtk_paint_polygon
--  gtk_paint_arrow
--  gtk_paint_diamond
--  gtk_paint_box
--  gtk_paint_flat_box
--  gtk_paint_check
--  gtk_paint_option
--  gtk_paint_tab
--  gtk_paint_shadow_gap
--  gtk_paint_box_gap
--  gtk_paint_extension
--  gtk_paint_focus
--  gtk_paint_slider
--  gtk_paint_handle
--  gtk_paint_expander
--  gtk_paint_layout
--  gtk_paint_resize_grip
--  gtk_boder_copy
--  gtk_boder_free
