-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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

with Glib.Object;   use Glib.Object;
with Gdk.Rectangle; use Gdk.Rectangle;
with Gtk.Enums;     use Gtk.Enums;
with System;
with Gdk.Color;     use Gdk.Color;
with Ada.Unchecked_Conversion;

package body Gtk.Style is

   type Gdk_Color_Access is access Gdk_Color;
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Gdk_Color_Access);

   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Style : out Gtk_Style) is
      function Internal return Gtk_Style;
      pragma Import (C, Internal, "gtk_style_new");

   begin
      Style := Internal;
   end Gtk_New;

   ----------------
   -- Draw_Arrow --
   ----------------

   procedure Draw_Arrow (Style       : in Gtk_Style;
                         Window      : in Gdk.Window.Gdk_Window;
                         State_Type  : in Enums.Gtk_State_Type;
                         Shadow_Type : in Enums.Gtk_Shadow_Type;
                         Arrow_Type  : in Enums.Gtk_Arrow_Type;
                         Fill        : in Boolean;
                         X, Y        : in Gint;
                         Width       : in Gint;
                         Height      : in Gint)
   is
      procedure Internal (Style         : in Gtk_Style;
                          Window        : in Gdk.Window.Gdk_Window;
                          State_Type  : in Enums.Gtk_State_Type;
                          Shadow_Type : in Enums.Gtk_Shadow_Type;
                          Arrow_Type  : in Enums.Gtk_Arrow_Type;
                          Fill        : in Gint;
                          X, Y        : in Gint;
                          Width       : in Gint;
                          Height      : in Gint);
      pragma Import (C, Internal, "gtk_draw_arrow");
   begin
      Internal (Style, Window, State_Type,
                Shadow_Type, Arrow_Type, Boolean'Pos (Fill), X, Y,
                Width, Height);
   end Draw_Arrow;

   ------------------
   -- Draw_Polygon --
   ------------------

   procedure Draw_Polygon
     (Style       : Gtk_Style;
      Window      : Gdk.Window.Gdk_Window;
      State_Type  : Enums.Gtk_State_Type;
      Shadow_Type : Enums.Gtk_Shadow_Type;
      Points      : Gdk.Types.Gdk_Points_Array;
      Fill        : Boolean) is

      procedure Internal
        (Style         : Gtk_Style;
         Window        : Gdk.Window.Gdk_Window;
         State_Type    : Enums.Gtk_State_Type;
         Shadow_Type   : Enums.Gtk_Shadow_Type;
         Points        : Gdk.Types.Gdk_Points_Array;
         Npoints       : Gint;
         Fill          : Gint);
      pragma Import (C, Internal, "gtk_draw_polygon");

   begin
      Internal
        (Style, Window, State_Type, Shadow_Type,
         Points, Points'Length, Boolean'Pos (Fill));
   end Draw_Polygon;

   -----------------
   -- Draw_String --
   -----------------

   procedure Draw_String
     (Style       : Gtk_Style;
      Window      : Gdk.Window.Gdk_Window;
      State_Type  : Enums.Gtk_State_Type;
      X, Y        : Gint;
      Str         : String)
   is
      procedure Internal
        (Style         : Gtk_Style;
         Window        : Gdk.Window.Gdk_Window;
         State_Type    : Enums.Gtk_State_Type;
         X, Y          : Gint;
         Str           : String);
      pragma Import (C, Internal, "gtk_draw_string");

   begin
      Internal
        (Style, Window, State_Type, X, Y, Str & ASCII.NUL);
   end Draw_String;

   --------------------
   -- Get_Foreground --
   --------------------

   function Get_Foreground
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color
   is
      function Internal
        (Style      : Gtk_Style;
         State_Type : Enums.Gtk_State_Type) return System.Address;
      pragma Import (C, Internal, "ada_style_get_fg");
   begin
      return Convert (Internal (Style, State_Type)).all;
   end Get_Foreground;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Style      : Gtk_Style;
      Window     : Gdk.Window.Gdk_Window;
      State_Type : Enums.Gtk_State_Type)
   is
      procedure Internal
        (Style      : Gtk_Style;
         Window     : Gdk.Window.Gdk_Window;
         State_Type : Enums.Gtk_State_Type);
      pragma Import (C, Internal, "gtk_style_set_background");

   begin
      Internal (Style, Window, State_Type);
   end Set_Background;

   --------------------
   -- Set_Foreground --
   --------------------

   procedure Set_Foreground
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : Gtk_Style;
         State : Enums.Gtk_State_Type;
         Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_fg");

      use type Gdk.Color.Gdk_Color;

      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Style, State_Type, Color_A);
   end Set_Foreground;

   --------------------
   -- Get_Background --
   --------------------

   function Get_Background
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color
   is
      function Internal
        (Style      : Gtk_Style;
         State_Type : Enums.Gtk_State_Type) return System.Address;
      pragma Import (C, Internal, "ada_style_get_bg");
   begin
      return Convert (Internal (Style, State_Type)).all;
   end Get_Background;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : Gtk_Style;
         State : Enums.Gtk_State_Type;
         Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_bg");

      use type Gdk.Color.Gdk_Color;

      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Style, State_Type, Color_A);
   end Set_Background;

   ---------------
   -- Get_Light --
   ---------------

   function Get_Light
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color
   is
      function Internal
        (Style      : Gtk_Style;
         State_Type : Enums.Gtk_State_Type) return System.Address;
      pragma Import (C, Internal, "ada_style_get_light");
   begin
      return Convert (Internal (Style, State_Type)).all;
   end Get_Light;

   ---------------
   -- Set_Light --
   ---------------

   procedure Set_Light
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : Gtk_Style;
         State : Enums.Gtk_State_Type;
         Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_light");

      use type Gdk.Color.Gdk_Color;

      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Style, State_Type, Color_A);
   end Set_Light;

   --------------
   -- Get_Dark --
   --------------

   function Get_Dark
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color
   is
      function Internal
        (Style      : Gtk_Style;
         State_Type : Enums.Gtk_State_Type) return System.Address;
      pragma Import (C, Internal, "ada_style_get_dark");
   begin
      return Convert (Internal (Style, State_Type)).all;
   end Get_Dark;

   --------------
   -- Set_Dark --
   --------------

   procedure Set_Dark
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : Gtk_Style;
         State : Enums.Gtk_State_Type;
         Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_dark");

      use type Gdk.Color.Gdk_Color;

      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Style, State_Type, Color_A);
   end Set_Dark;

   ----------------
   -- Get_Middle --
   ----------------

   function Get_Middle
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color
   is
      function Internal
        (Style      : Gtk_Style;
         State_Type : Enums.Gtk_State_Type) return System.Address;
      pragma Import (C, Internal, "ada_style_get_mid");
   begin
      return Convert (Internal (Style, State_Type)).all;
   end Get_Middle;

   ----------------
   -- Set_Middle --
   ----------------

   procedure Set_Middle
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : Gtk_Style;
         State : Enums.Gtk_State_Type;
         Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_mid");
      use type Gdk.Color.Gdk_Color;

      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Style, State_Type, Color_A);
   end Set_Middle;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color
   is
      function Internal
        (Style      : Gtk_Style;
         State_Type : Enums.Gtk_State_Type) return System.Address;
      pragma Import (C, Internal, "ada_style_get_text");
   begin
      return Convert (Internal (Style, State_Type)).all;
   end Get_Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : Gtk_Style;
         State : Enums.Gtk_State_Type;
         Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_text");

      use type Gdk.Color.Gdk_Color;

      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Style, State_Type, Color_A);
   end Set_Text;

   --------------
   -- Get_Base --
   --------------

   function Get_Base
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color
   is
      function Internal
        (Style      : Gtk_Style;
         State_Type : Enums.Gtk_State_Type) return System.Address;
      pragma Import (C, Internal, "ada_style_get_base");
   begin
      return Convert (Internal (Style, State_Type)).all;
   end Get_Base;

   --------------
   -- Set_Base --
   --------------

   procedure Set_Base
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : Gtk_Style;
         State : Enums.Gtk_State_Type;
         Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_base");

      use type Gdk.Color.Gdk_Color;

      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Style, State_Type, Color_A);
   end Set_Base;

   ---------------
   -- Get_Black --
   ---------------

   function Get_Black (Style : Gtk_Style) return Gdk.Color.Gdk_Color is
      function Internal (Style : Gtk_Style) return System.Address;
      pragma Import (C, Internal, "ada_style_get_black");
   begin
      return Convert (Internal (Style)).all;
   end Get_Black;

   ---------------
   -- Set_Black --
   ---------------

   procedure Set_Black (Style : Gtk_Style; Color : Gdk.Color.Gdk_Color) is
      procedure Internal (Style : Gtk_Style; Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_black");

      use type Gdk.Color.Gdk_Color;

      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Style, Color_A);
   end Set_Black;

   ---------------
   -- Get_White --
   ---------------

   function Get_White (Style : Gtk_Style) return Gdk.Color.Gdk_Color is
      function Internal (Style : Gtk_Style) return System.Address;
      pragma Import (C, Internal, "ada_style_get_white");
   begin
      return Convert (Internal (Style)).all;
   end Get_White;

   ---------------
   -- Set_White --
   ---------------

   procedure Set_White (Style : Gtk_Style; Color : Gdk.Color.Gdk_Color) is
      procedure Internal (Style : Gtk_Style; Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_white");

      use type Gdk.Color.Gdk_Color;

      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Style, Color_A);
   end Set_White;

   -------------------------
   --  Get_Background_GC  --
   -------------------------

   function Get_Background_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
   is
      function Internal
        (Style : Gtk_Style; State : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_bg_gc");

   begin
      return Internal (Style, State_Type);
   end Get_Background_GC;

   -----------------------
   -- Set_Background_GC --
   -----------------------

   procedure Set_Background_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC)
   is
      procedure Internal
        (Style : Gtk_Style;
         State : Enums.Gtk_State_Type;
         GC    : Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_bg_gc");

   begin
      Internal (Style, State_Type, GC);
   end Set_Background_GC;

   -----------------------
   -- Get_Foreground_GC --
   -----------------------

   function Get_Foreground_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
   is
      function Internal
        (Style : Gtk_Style; State : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_fg_gc");

   begin
      return Internal (Style, State_Type);
   end Get_Foreground_GC;

   -----------------------
   -- Set_Foreground_GC --
   -----------------------

   procedure Set_Foreground_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC)
   is
      procedure Internal
        (Style : Gtk_Style;
         State : Enums.Gtk_State_Type;
         GC    : Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_fg_gc");

   begin
      Internal (Style, State_Type, GC);
   end Set_Foreground_GC;

   ------------------
   -- Get_Light_GC --
   ------------------

   function Get_Light_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
   is
      function Internal
        (Style : Gtk_Style; State : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_light_gc");

   begin
      return Internal (Style, State_Type);
   end Get_Light_GC;

   ------------------
   -- Set_Light_GC --
   ------------------

   procedure Set_Light_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC)
   is
      procedure Internal
        (Style : Gtk_Style;
         State : Enums.Gtk_State_Type;
         GC    : Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_light_gc");

   begin
      Internal (Style, State_Type, GC);
   end Set_Light_GC;

   -----------------
   -- Get_Dark_GC --
   -----------------

   function Get_Dark_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
   is
      function Internal
        (Style : Gtk_Style; State : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_dark_gc");

   begin
      return Internal (Style, State_Type);
   end Get_Dark_GC;

   -----------------
   -- Set_Dark_GC --
   -----------------

   procedure Set_Dark_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC)
   is
      procedure Internal
        (Style : Gtk_Style;
         State : Enums.Gtk_State_Type;
         GC    : Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_dark_gc");

   begin
      Internal (Style, State_Type, GC);
   end Set_Dark_GC;

   -------------------
   -- Get_Middle_GC --
   -------------------

   function Get_Middle_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
   is
      function Internal
        (Style : Gtk_Style; State : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_mid_gc");

   begin
      return Internal (Style, State_Type);
   end Get_Middle_GC;

   -------------------
   -- Set_Middle_GC --
   -------------------

   procedure Set_Middle_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC)
   is
      procedure Internal
        (Style : Gtk_Style;
         State : Enums.Gtk_State_Type;
         GC    : Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_mid_gc");

   begin
      Internal (Style, State_Type, GC);
   end Set_Middle_GC;

   -----------------
   -- Get_Text_GC --
   -----------------

   function Get_Text_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
   is
      function Internal
        (Style : Gtk_Style; State : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_text_gc");

   begin
      return Internal (Style, State_Type);
   end Get_Text_GC;

   -----------------
   -- Set_Text_GC --
   -----------------

   procedure Set_Text_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC)
   is
      procedure Internal
        (Style : Gtk_Style;
         State : Enums.Gtk_State_Type;
         GC    : Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_text_gc");

   begin
      Internal (Style, State_Type, GC);
   end Set_Text_GC;

   -----------------
   -- Get_Base_GC --
   -----------------

   function Get_Base_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
   is
      function Internal
        (Style : Gtk_Style; State : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_base_gc");

   begin
      return Internal (Style, State_Type);
   end Get_Base_GC;

   -----------------
   -- Set_Base_GC --
   -----------------

   procedure Set_Base_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC)
   is
      procedure Internal
        (Style : Gtk_Style;
         State : Enums.Gtk_State_Type;
         GC    : Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_base_gc");

   begin
      Internal (Style, State_Type, GC);
   end Set_Base_GC;

   -------------------
   -- Set_Bg_Pixmap --
   -------------------

   procedure Set_Bg_Pixmap
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Pixmap     : Gdk.Pixmap.Gdk_Pixmap)
   is
      procedure Internal
        (Style  : Gtk_Style;
         State  : Enums.Gtk_State_Type;
         Pixmap : Gdk.Pixmap.Gdk_Pixmap);
      pragma Import (C, Internal, "ada_style_set_bg_pixmap");

   begin
      Internal (Style, State_Type, Pixmap);
   end Set_Bg_Pixmap;

   -------------------
   -- Get_Bg_Pixmap --
   -------------------

   function Get_Bg_Pixmap
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Pixmap.Gdk_Pixmap
   is
      function Internal
        (Style : Gtk_Style;
         State : Enums.Gtk_State_Type) return Gdk.Pixmap.Gdk_Pixmap;
      pragma Import (C, Internal, "ada_style_get_bg_pixmap");

   begin
      return Internal (Style, State_Type);
   end Get_Bg_Pixmap;

   ------------------
   -- Paint_Handle --
   ------------------

   procedure Paint_Handle
     (Style               : Gtk_Style;
      Window              : Gdk.Gdk_Window;
      State_Type          : Gtk.Enums.Gtk_State_Type;
      Shadow_Type         : Gtk.Enums.Gtk_Shadow_Type;
      Area                : Gdk_Rectangle;
      Widget              : access Gtk.Object.Gtk_Object_Record'Class;
      Detail              : String := "paned";
      X, Y, Width, Height : Gint;
      Orientation         : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
        (Style               : Gtk_Style;
         Window              : Gdk.Gdk_Window;
         State_Type          : Gtk.Enums.Gtk_State_Type;
         Shadow_Type         : Gtk.Enums.Gtk_Shadow_Type;
         Area                : Gdk_Rectangle;
         Widget              : System.Address;
         Detail              : String;
         X, Y, Width, Height : Gint;
         Orientation         : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_paint_handle");
   begin
      Internal
        (Style, Window, State_Type, Shadow_Type,
         Area, Get_Object (Widget), Detail & ASCII.Nul,
         X, Y, Width, Height, Orientation);
   end Paint_Handle;



end Gtk.Style;
