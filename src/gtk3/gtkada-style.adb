------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2011-2021, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Numerics;       use Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Unchecked_Deallocation;

with Glib;               use Glib;
with Glib.Error;         use Glib.Error;

with Cairo;              use Cairo;
with Cairo.Pattern;      use Cairo.Pattern;
with Pango.Cairo;        use Pango.Cairo;
with Gdk.Cairo;          use Gdk.Cairo;
with Gdk.Device;         use Gdk.Device;
with Gdk.Device_Manager; use Gdk.Device_Manager;
with Gdk.Display;        use Gdk.Display;
with Gdk.RGBA;           use Gdk.RGBA;
with Gdk.Screen;         use Gdk.Screen;
with Gdk.Types;          use Gdk.Types;
with Gdk.Window;         use Gdk;

with Gtk.Enums;          use Gtk.Enums;
with Gtk.Icon_Theme;     use Gtk.Icon_Theme;
with Gtk.Style_Context;  use Gtk.Style_Context;
with Gtk.Style;          use Gtk.Style;
with Gtk.Widget;         use Gtk.Widget;
with Gtk.Css_Provider;   use Gtk.Css_Provider;
with Gtk.Style_Provider; use Gtk.Style_Provider;
with Pango;              use Pango;
with Pango.Attributes;   use Pango.Attributes;
with Pango.Enums;        use Pango.Enums;
with Pango.Font;         use Pango.Font;
with Pango.Layout;       use Pango.Layout;

pragma Warnings (Off, "call to obsolescent function ""List_Devices""");
pragma Warnings
  (Off, "call to obsolescent procedure ""Get_Background_Color""");
--  Deprecated in Gtk+ 3.24

package body Gtkada.Style is

   Dec_To_Hex : constant array (0 .. 15) of Character :=
     (0 => '0', 1 => '1', 2 => '2', 3 => '3', 4 => '4', 5 => '5',
      6 => '6', 7 => '7', 8 => '8', 9 => '9', 10 => 'A', 11 => 'B',
      12 => 'C', 13 => 'D', 14 => 'E', 15 => 'F');

   package Gdouble_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (Gdouble);
   use Gdouble_Functions;

   procedure Setup_Layout
     (Self     : Font_Style;
      Layout   : not null access Pango.Layout.Pango_Layout_Record'Class;
      Max_Width, Max_Height : Glib.Gdouble := -1.0);
   --  Setup the layout for the drawing style

   ------------
   -- To_HSV --
   ------------

   function To_HSV (Color : Cairo_Color) return HSV_Color is
      Max : constant Gdouble :=
        Gdouble'Max (Gdouble'Max (Color.Red, Color.Green), Color.Blue);
      Min : constant Gdouble :=
        Gdouble'Min (Gdouble'Min (Color.Red, Color.Green), Color.Blue);
      Del : constant Gdouble := Max - Min;
      Del_R, Del_G, Del_B : Gdouble;
      Ret                 : HSV_Color;

   begin
      Ret.V := Max;
      Ret.A := Color.Alpha;

      if Del = 0.0 then
         Ret.H := 0.0;
         Ret.S := 0.0;

      else
         Ret.S := Del / Max;

         Del_R := (((Max - Color.Red) / 6.0) + (Del / 2.0)) / Del;
         Del_G := (((Max - Color.Green) / 6.0) + (Del / 2.0)) / Del;
         Del_B := (((Max - Color.Blue) / 6.0) + (Del / 2.0)) / Del;

         if Max = Color.Red then
            Ret.H := Del_B - Del_G;
         elsif Max = Color.Green then
            Ret.H := (1.0 / 3.0) + Del_R - Del_B;
         else
            Ret.H := (2.0 / 3.0) + Del_G - Del_R;
         end if;

         if Ret.H < 0.0 then
            Ret.H := Ret.H + 1.0;
         end if;

         if Ret.H >= 1.0 then
            Ret.H := Ret.H - 1.0;
         end if;
      end if;

      return Ret;
   end To_HSV;

   ------------
   -- To_HSL --
   ------------

   function To_HSLA (Color : Gdk.RGBA.Gdk_RGBA) return HSLA_Color is
      --  Algorithm from www.rapidtables.com/convert/color/rgb-to-hsl.htm
      Min : constant Gdouble :=
        Gdouble'Min (Gdouble'Min (Color.Red, Color.Green), Color.Blue);
      Max : constant Gdouble :=
        Gdouble'Max (Gdouble'Max (Color.Red, Color.Green), Color.Blue);
      Del : constant Gdouble := Max - Min;
      Result : HSLA_Color;
   begin
      Result.Lightness := (Max + Min) / 2.0;
      Result.Alpha     := Color.Alpha;

      --  Do we have a gray ?
      if Del = 0.0 then
         Result.Hue := 0.0;
         Result.Saturation := 0.0;
      else
         if Result.Lightness < 0.5 then
            Result.Saturation := Del / (Max + Min);
         else
            Result.Saturation := Del / (2.0 - Max - Min);
         end if;

         if Color.Red = Max then
            Result.Hue := ((Color.Green - Color.Blue) / Del) / 6.0;
         elsif Color.Green = Max then
            Result.Hue := ((Color.Blue - Color.Red) / Del + 2.0) / 6.0;
         else
            Result.Hue := ((Color.Red - Color.Green) / Del + 4.0) / 6.0;
         end if;

         while Result.Hue < 0.0 loop
            Result.Hue := Result.Hue + 1.0;
         end loop;
         while Result.Hue > 1.0 loop
            Result.Hue := Result.Hue - 1.0;
         end loop;
      end if;

      return Result;
   end To_HSLA;

   -------------
   -- To_RGBA --
   -------------

   function To_RGBA (Color : HSLA_Color) return Gdk.RGBA.Gdk_RGBA is
      --  Algorithm from http://www.easyrgb.com/

      V1, V2 : Gdouble;

      function Hue_To_RGB (Hue : Gdouble) return Gdouble;
      function Hue_To_RGB (Hue : Gdouble) return Gdouble is
         H : Gdouble := Hue;
      begin
         if H < 0.0 then
            H := H + 1.0;
         elsif H > 1.0 then
            H := H - 1.0;
         end if;

         if H * 6.0 < 1.0 then
            return V1 + (V2 - V1) * 6.0 * H;
         elsif H * 2.0 < 1.0 then
            return V2;
         elsif H * 3.0 < 2.0 then
            return V1 + (V2 - V1) * 6.0 * (2.0 / 3.0 - H);
         else
            return V1;
         end if;
      end Hue_To_RGB;

   begin
      if Color.Saturation = 0.0 then
         return (Red   => Color.Lightness,
                 Green => Color.Lightness,
                 Blue  => Color.Lightness,
                 Alpha => Color.Alpha);
      end if;

      if Color.Lightness < 0.5 then
         V2 := Color.Lightness * (1.0 + Color.Saturation);
      else
         V2 := (Color.Lightness + Color.Saturation)
           - (Color.Lightness * Color.Saturation);
      end if;

      V1 := 2.0 * Color.Lightness - V2;

      return (Red   => Hue_To_RGB (Color.Hue + 1.0 / 3.0),
              Green => Hue_To_RGB (Color.Hue),
              Blue  => Hue_To_RGB (Color.Hue - 1.0 / 3.0),
              Alpha => Color.Alpha);
   end To_RGBA;

   --------------
   -- To_Cairo --
   --------------

   function To_Cairo (HSV : HSV_Color) return Cairo_Color
   is
      Var_H, Var_J, Var_1, Var_2, Var_3 : Gdouble;
      Ret  : Cairo_Color;

   begin
      Ret.Alpha := HSV.A;

      if HSV.S = 0.0 then
         Ret.Red   := HSV.V;
         Ret.Green := HSV.V;
         Ret.Blue  := HSV.V;
      else
         if HSV.H = 1.0 then
            Var_H := 0.0;
         else
            Var_H := HSV.H * 6.0;
         end if;

         Var_J := Gdouble'Floor (Var_H);
         Var_1 := HSV.V * (1.0 - HSV.S);
         Var_2 := HSV.V * (1.0 - HSV.S * (Var_H - Var_J));
         Var_3 := HSV.V * (1.0 - HSV.S * (1.0 - (Var_H - Var_J)));

         if Var_J = 0.0 then
            Ret.Red   := HSV.V;
            Ret.Green := Var_3;
            Ret.Blue  := Var_1;
         elsif Var_J = 1.0 then
            Ret.Red   := Var_2;
            Ret.Green := HSV.V;
            Ret.Blue  := Var_1;
         elsif Var_J = 2.0 then
            Ret.Red   := Var_1;
            Ret.Green := HSV.V;
            Ret.Blue  := Var_3;
         elsif Var_J = 3.0 then
            Ret.Red   := Var_1;
            Ret.Green := Var_2;
            Ret.Blue  := HSV.V;
         elsif Var_J = 4.0 then
            Ret.Red   := Var_3;
            Ret.Green := Var_1;
            Ret.Blue  := HSV.V;
         else
            Ret.Red   := HSV.V;
            Ret.Green := Var_1;
            Ret.Blue  := Var_2;
         end if;
      end if;

      return Ret;
   end To_Cairo;

   --------------
   -- To_Cairo --
   --------------

   function To_Cairo (Color : Gdk.Color.Gdk_Color) return Cairo_Color is
   begin
      return (Red   => Gdouble (Gdk.Color.Red (Color)) / 65535.0,
              Green => Gdouble (Gdk.Color.Green (Color)) / 65535.0,
              Blue  => Gdouble (Gdk.Color.Blue (Color)) / 65535.0,
              Alpha => 1.0);
   end To_Cairo;

   --------------
   -- To_Cairo --
   --------------

   function To_Cairo (Color : Cairo_Color) return Gdk.RGBA.Gdk_RGBA is
   begin
      return Color;
   end To_Cairo;

   ------------
   -- To_Hex --
   ------------

   function To_Hex (Color : Gdk.RGBA.Gdk_RGBA) return String is
      function To_Hex (Val : Gdouble) return String;
      function To_Hex (Val : Gdouble) return String is
         V : constant Integer := Integer (Val * 255.0);
      begin
         return Dec_To_Hex (V / 16) & Dec_To_Hex (V mod 16);
      end To_Hex;

   begin
      return '#'
        & To_Hex (Color.Red)
        & To_Hex (Color.Green)
        & To_Hex (Color.Blue);
   end To_Hex;

   -------------------
   -- Complementary --
   -------------------

   function Complementary
     (Color : Gdk.RGBA.Gdk_RGBA) return Gdk.RGBA.Gdk_RGBA
   is
      V : HSLA_Color := To_HSLA (Color);
   begin
      V.Hue := V.Hue + 0.5;
      if V.Hue > 1.0 then
         V.Hue := V.Hue - 1.0;
      end if;

      return To_RGBA (V);
   end Complementary;

   ----------------------
   -- Shade_Or_Lighten --
   ----------------------

   function Shade_Or_Lighten
     (Color  : Gdk.RGBA.Gdk_RGBA;
      Amount : Percent := 0.4) return Gdk.RGBA.Gdk_RGBA
   is
      V : HSLA_Color := To_HSLA (Color);
   begin
      if V.Lightness < 0.5 then
         V.Lightness :=
           Gdouble'Min (1.0, V.Lightness + Amount);   --  lighten
      else
         V.Lightness :=
           Gdouble'Max (0.0, V.Lightness - Amount);   --  darken
      end if;
      return To_RGBA (V);
   end Shade_Or_Lighten;

   -----------
   -- Shade --
   -----------

   function Shade
     (Color : Gdk.Color.Gdk_Color;
      Value : Percent)
      return Cairo_Color
   is
   begin
      return Shade (To_Cairo (Color), Value);
   end Shade;

   -----------
   -- Shade --
   -----------

   function Shade
     (Color : Cairo_Color;
      Value : Percent)
      return Cairo_Color
   is
      V : HSLA_Color := To_HSLA (Color);
   begin
      V.Lightness := Gdouble'Max (0.0, V.Lightness - Value);
      return To_RGBA (V);
   end Shade;

   ----------------------
   -- Set_Source_Color --
   ----------------------

   procedure Set_Source_Color
     (Cr : Cairo.Cairo_Context; Color : Cairo_Color) is
   begin
      Cairo.Set_Source_Rgba
        (Cr, Color.Red, Color.Green, Color.Blue, Color.Alpha);
   end Set_Source_Color;

   -------------------------
   -- Create_Rgba_Pattern --
   -------------------------

   function Create_Rgba_Pattern
      (Color : Cairo_Color) return Cairo.Cairo_Pattern
   is
   begin
      return Cairo.Pattern.Create_Rgba
         (Color.Red, Color.Green, Color.Blue, Color.Alpha);
   end Create_Rgba_Pattern;

   -----------------------
   -- Rounded_Rectangle --
   -----------------------

   procedure Rounded_Rectangle
     (Cr         : Cairo.Cairo_Context;
      X, Y, W, H : Glib.Gdouble;
      Radius     : Glib.Gdouble)
   is
      X1 : constant Gdouble := X + Radius;
      X2 : constant Gdouble := X + W - Radius;
      Y1 : constant Gdouble := Y + Radius;
      Y2 : constant Gdouble := Y + H - Radius;
   begin
      New_Sub_Path (Cr);
      Cairo.Arc (Cr, X2, Y1, Radius, -Pi / 2.0, 0.0);
      Cairo.Arc (Cr, X2, Y2, Radius, 0.0, Pi / 2.0);
      Cairo.Arc (Cr, X1, Y2, Radius, Pi / 2.0, Pi);
      Cairo.Arc (Cr, X1, Y1, Radius, Pi, 3.0 * Pi / 2.0);
      Close_Path (Cr);
   end Rounded_Rectangle;

   -----------------
   -- Draw_Shadow --
   -----------------

   procedure Draw_Shadow
     (Cr                  : Cairo.Cairo_Context;
      Widget              : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Shadow_Type         : Gtk.Enums.Gtk_Shadow_Type;
      X, Y, Width, Height : Glib.Gint;
      Corner_Radius       : Glib.Gdouble := 0.0)
   is
      Hilight : Cairo_Color;
      Shadow  : Cairo_Color;
      HSV     : HSV_Color;

      Ctx : constant Gtk_Style_Context := Get_Style_Context (Widget);
      Border : Gtk_Border;

      X_Thick : Gdouble;
      Radius  : Gdouble;
      dX, dY, dW, dH : Gdouble;
      Color : Gdk_RGBA;

   begin
      if Shadow_Type = Shadow_None then
         return;
      end if;

      Ctx.Get_Border (Gtk_State_Flag_Normal, Border);

      X_Thick := Gdouble (Border.Left + Border.Right);
      Ctx.Get_Background_Color (Gtk_State_Flag_Normal, Color);

      Cairo.Save (Cr);

      HSV := To_HSV (Color);

      HSV.V := 0.3;
      Shadow := To_Cairo (HSV);

      HSV.V := 0.9;
      Hilight := To_Cairo (HSV);

      dX := Gdouble (X) + X_Thick + 0.5;
      dY := Gdouble (Y) + X_Thick + 0.5;
      dW := Gdouble (Width) - 2.0 * X_Thick - 1.0;
      dH := Gdouble (Height) - 2.0 * X_Thick - 1.0;

      if Corner_Radius < X_Thick then
         Radius := X_Thick;
      else
         Radius := Corner_Radius;
      end if;

      if Shadow_Type = Shadow_Out or else Shadow_Type = Shadow_Etched_Out then
         Set_Line_Width (Cr, X_Thick);
         Set_Source_Color (Cr, Hilight);

      else
         if X_Thick < 0.5 then
            Set_Line_Width (Cr, 0.5);
         else
            Set_Line_Width (Cr, X_Thick);
         end if;

         Set_Source_Color (Cr, Shadow);
      end if;

      Rounded_Rectangle
        (Cr, dX, dY, dW, dH, Radius);
      Cairo.Stroke (Cr);

      if Shadow_Type = Shadow_Out or else Shadow_Type = Shadow_Etched_Out then
         if X_Thick < 0.5 then
            Set_Line_Width (Cr, 0.5);
         else
            Set_Line_Width (Cr, X_Thick);
         end if;

         Set_Source_Color (Cr, Shadow);

      else
         Set_Line_Width (Cr, X_Thick);
         Set_Source_Color (Cr, Hilight);
      end if;

      Rounded_Rectangle
        (Cr,
         dX + X_Thick, dY + X_Thick, dW - 2.0 * X_Thick, dH - 2.0 * X_Thick,
         Radius - X_Thick * 1.0);
      Cairo.Stroke (Cr);

      Cairo.Restore (Cr);
   end Draw_Shadow;

   --------------------
   -- Draw_Rectangle --
   --------------------

   procedure Draw_Rectangle
     (Cr                  : Cairo.Cairo_Context;
      Color               : Gdk.Color.Gdk_Color;
      Filled              : Boolean;
      X, Y, Width, Height : Glib.Gint;
      Corner_Radius       : Glib.Gdouble := 0.0) is
   begin
      Draw_Rectangle
        (Cr, To_Cairo (Color), Filled, X, Y, Width, Height, Corner_Radius);
   end Draw_Rectangle;

   --------------------
   -- Draw_Rectangle --
   --------------------

   procedure Draw_Rectangle
     (Cr                  : Cairo.Cairo_Context;
      Color               : Cairo_Color;
      Filled              : Boolean;
      X, Y, Width, Height : Glib.Gint;
      Corner_Radius       : Glib.Gdouble := 0.0)
   is
      dX, dY, dW, dH : Gdouble;
      Line_Width     : Gdouble;

   begin
      dX := Gdouble (X);
      dY := Gdouble (Y);
      dW := Gdouble (Width);
      dH := Gdouble (Height);

      if not Filled then
         Line_Width := Get_Line_Width (Cr) / 2.0;
         dX := dX + Line_Width;
         dY := dY + Line_Width;
         dW := dW - 2.0 * Line_Width;
         dH := dH - 2.0 * Line_Width;
      end if;

      if Corner_Radius = 0.0 then
         Rectangle (Cr, dX, dY, dW, dH);
      else
         Rounded_Rectangle (Cr, dX, dY, dW, dH, Corner_Radius);
      end if;

      Set_Source_Color (Cr, Color);

      if Filled then
         Cairo.Fill (Cr);
      else
         Stroke (Cr);
      end if;
   end Draw_Rectangle;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line
     (Cr             : Cairo.Cairo_Context;
      Color          : Gdk.Color.Gdk_Color;
      X1, Y1, X2, Y2 : Glib.Gint) is
   begin
      Draw_Line (Cr, To_Cairo (Color), X1, Y1, X2, Y2);
   end Draw_Line;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line
     (Cr             : Cairo.Cairo_Context;
      Color          : Cairo_Color;
      X1, Y1, X2, Y2 : Glib.Gint)
   is
      Line_Width     : Gdouble;
   begin
      Set_Source_Color (Cr, Color);
      Line_Width := Get_Line_Width (Cr) / 2.0;

      Move_To (Cr, Gdouble (X1) + Line_Width, Gdouble (Y1) + Line_Width);
      Line_To (Cr, Gdouble (X2) + Line_Width, Gdouble (Y2) + Line_Width);
      Stroke (Cr);
   end Draw_Line;

   -----------------
   -- Draw_Layout --
   -----------------

   procedure Draw_Layout
     (Cr     : Cairo.Cairo_Context;
      Color  : Gdk.Color.Gdk_Color;
      X, Y   : Glib.Gint;
      Layout : Pango.Layout.Pango_Layout)
   is
   begin
      Draw_Layout (Cr, To_Cairo (Color), X, Y, Layout);
   end Draw_Layout;

   -----------------
   -- Draw_Layout --
   -----------------

   procedure Draw_Layout
     (Cr     : Cairo.Cairo_Context;
      Color  : Cairo_Color;
      X, Y   : Glib.Gint;
      Layout : Pango.Layout.Pango_Layout)
   is
   begin
      Set_Source_Color (Cr, Color);
      Move_To (Cr, Gdouble (X), Gdouble (Y));
      Pango.Cairo.Show_Layout (Cr, Layout);
   end Draw_Layout;

   -----------------
   -- Draw_Pixbuf --
   -----------------

   procedure Draw_Pixbuf
     (Cr     : Cairo.Cairo_Context;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
      X, Y   : Glib.Gint)
   is
   begin
      Gdk.Cairo.Set_Source_Pixbuf (Cr, Pixbuf, Gdouble (X), Gdouble (Y));
      Cairo.Paint (Cr);
   end Draw_Pixbuf;

   ----------------------------
   -- Draw_Pixbuf_With_Scale --
   ----------------------------

   procedure Draw_Pixbuf_With_Scale
      (Cr     : Cairo.Cairo_Context;
       Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
       X, Y   : Glib.Gdouble;
       Widget : access Gtk_Widget_Record'Class := null)
   is
      Scale : Gint;
      Surf  : Cairo_Surface;
   begin
      Save (Cr);

      if Widget /= null then
         Scale := Widget.Get_Scale_Factor;
         Surf := Create_From_Pixbuf
            (Pixbuf, Scale => Scale, For_Window => Widget.Get_Window);
         Get_Style_Context (Widget).Render_Icon_Surface (Cr, Surf, X, Y);
      else
         Surf := Create_From_Pixbuf
            (Pixbuf, Scale => 1, For_Window => null);
         Set_Source_Surface (Cr, Surf, X, Y);
         Cairo.Fill (Cr);
      end if;

      Surface_Destroy (Surf);
      Restore (Cr);
   end Draw_Pixbuf_With_Scale;

   ----------------------------
   -- Draw_Pixbuf_With_Scale --
   ----------------------------

   procedure Draw_Pixbuf_With_Scale
      (Cr        : Cairo.Cairo_Context;
       Icon_Name : String;
       X, Y      : Glib.Gdouble;
       Size      : Glib.Gint;
       Widget    : access Gtk_Widget_Record'Class := null)
   is
      use Gdk.Pixbuf;
      P     : Gdk.Pixbuf.Gdk_Pixbuf;
      Scale : Gint;
   begin
      if Widget /= null then
         Scale := Widget.Get_Scale_Factor;
      else
         Scale := 1;
      end if;

      P := Gtk.Icon_Theme.Get_Default.Load_Icon_For_Scale
         (Icon_Name,
          Size  => Size,
          Scale => Scale,
          Flags => 0,
          Error => null);
      if P /= null then
         Draw_Pixbuf_With_Scale (Cr, P, X, Y, Widget);
         Gdk.Pixbuf.Unref (P);
      end if;
   end Draw_Pixbuf_With_Scale;

   --------------
   -- Snapshot --
   --------------

   function Snapshot
     (Widget : not null access Gtk_Widget_Record'Class)
      return Cairo.Cairo_Surface
   is
      Ctx     : Cairo_Context;
      Surface : Cairo_Surface;
      Color   : Gdk_RGBA;

      Window_Provider  : Gtk_Widget := Gtk_Widget (Widget);
   begin
      Surface := Gdk.Window.Create_Similar_Surface
        (Self    => Get_Window (Widget),
         Content => Cairo_Content_Color,
         Width   => Get_Allocated_Width (Widget),
         Height  => Get_Allocated_Height (Widget));

      while Window_Provider /= null
        and then not Window_Provider.Get_Has_Window
      loop
         Window_Provider := Window_Provider.Get_Parent;
      end loop;

      if Window_Provider /= null then
         Get_Style_Context (Window_Provider).Get_Background_Color
           (Gtk_State_Flag_Normal, Color);
      end if;

      Ctx := Create (Surface);
      Set_Source_RGBA (Ctx, Color);
      Paint (Ctx);
      Draw (Widget, Ctx);  --  Capture current rendering
      Destroy (Ctx);
      return Surface;
   end Snapshot;

   -------------------
   -- Load_Css_File --
   -------------------

   procedure Load_Css_File
     (Path : String;
      Error : access procedure (Str : String) := null;
      Priority : Gtk.Style_Provider.Priority)
   is
      Css     : Gtk_Css_Provider;
      Display : Gdk_Display;
      Screen  : Gdk_Screen;
      Err     : aliased GError;
   begin
      Gtk_New (Css);
      if not Css.Load_From_Path (Path, Err'Access) then
         if Error /= null then
            Error (Get_Message (Err));
         end if;
      else
         Display := Get_Default;
         Screen  := Get_Default_Screen (Display);
         Gtk.Style_Context.Add_Provider_For_Screen
           (Screen, +Css, Priority => Priority);
      end if;

      Unref (Css);
   end Load_Css_File;

   ---------------------
   -- Load_Css_String --
   ---------------------

   procedure Load_Css_String
     (Data : String;
      Error : access procedure (Str : String) := null;
      Priority : Gtk.Style_Provider.Priority)
   is
      Css     : Gtk_Css_Provider;
      Display : Gdk_Display;
      Screen  : Gdk_Screen;
      Err     : aliased GError;
   begin
      Gtk_New (Css);
      if not Css.Load_From_Data (Data, Err'Access) then
         if Error /= null then
            Error (Get_Message (Err));
         end if;
      else
         Display := Get_Default;
         Screen  := Get_Default_Screen (Display);
         Gtk.Style_Context.Add_Provider_For_Screen
           (Screen, +Css, Priority => Priority);
      end if;

      Unref (Css);
   end Load_Css_String;

   ----------------
   -- Get_Offset --
   ----------------

   procedure Get_Offset
     (Window : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Parent : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y   : out Gint)
   is
      Parent_Win : constant Gdk_Window := Parent.Get_Window;
      Win      : Gdk_Window := Window.Get_Window;
      Wx, Wy   : Gint;
   begin
      X := 0;
      Y := 0;
      while Win /= null and then Win /= Parent_Win loop
         Gdk.Window.Get_Position (Win, Wx, Wy);
         X := X + Wx;
         Y := Y + Wy;
         Win := Gdk.Window.Get_Parent (Win);
      end loop;
   end Get_Offset;

   ------------------
   -- Draw_Overlay --
   ------------------

   procedure Draw_Overlay
     (Widget  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Overlay : in out Cairo.Cairo_Surface;
      Do_Draw : not null access procedure
        (Context : Cairo.Cairo_Context;
         Draw    : Boolean))
   is
      Toplevel : constant Gtk_Widget := Widget.Get_Toplevel;
      X, Y     : Gint := 0;  --  location of Split relative to Toplevel
      Top_Win  : constant Gdk_Window := Toplevel.Get_Window;
      Hide_Previous : Boolean := True;

   begin
      Get_Offset (Widget, Toplevel, X, Y);

      --  Save the display of the toplevel window, so that we can easily
      --  erase the resize handle later. We cannot capture Split only,
      --  because that would not include a background color if Split does
      --  not have a window.
      --  ??? Apparently, we need to redraw once, we can't simply capture
      --  the screen. There might be a way using:
      --     pixbuf = gtk.gdk.Pixbuf(gtk.gdk.COLORSPACE_RGB, False, 9, w, h)
      --     shot = pixbuf.get_from_drawable(win.window, win.get_colormap(),
      --                  0, 0, 0, 0, w, h)

      if Overlay = Null_Surface then
         declare
            Cr2 : Cairo_Context;
         begin
            Overlay := Gdk.Window.Create_Similar_Surface
              (Self    => Top_Win,
               Content => Cairo_Content_Color,
               Width   => Toplevel.Get_Allocated_Width,
               Height  => Toplevel.Get_Allocated_Height);
            Cr2 := Create (Overlay);
            Reset_Clip (Cr2);

            --  ??? Optimization: we could translate and clip Cr2, so that
            --  only the part of Toplevel corresponding to Split is actually
            --  stored in Split.Overlay.
--              Cairo.Rectangle
--                (Cr2,
--                 Gdouble (X - 5), Gdouble (Y - 5),
--                 Gdouble (Get_Allocated_Width (Split) + 10),
--                 Gdouble (Get_Allocated_Height (Split) + 10));
--              Cairo.Clip (Cr2);

            Draw (Toplevel, Cr2);
            Destroy (Cr2);

            Hide_Previous := False;
         end;
      end if;

      declare
         --  The call to Gdk.Cairo.Create returns a new cairo_context which
         --  is clipped. Drawing on it would in fact show nothing below the
         --  child windows. One way to create a usable context would be to
         --  use:
         --      Surf : constant Cairo_Surface := Cairo.Get_Target (Cr);
         --      Cr3  : constant Cairo_Context := Create (Surf);
         --  But in fact it seems we can safely call Reset_Clip instead on
         --  the context.

         Cr      : constant Cairo_Context := Gdk.Cairo.Create (Top_Win);
      begin
         Reset_Clip (Cr);  --  draw on top of child windows
         Translate (Cr, Gdouble (X), Gdouble (Y));

         if Hide_Previous then
            Cairo.Save (Cr);

            --  Position is x_dest - x_src, y_dest - y_src
            --     Where x_dest is the location at which we are drawing
            --     within the target cairo_context, i.e. X + Handle_X
            --     and x_src is the location within the overlay from which
            --     we are copying, ie X + Handle_X
            --  However, this position does not take into account the
            --  translation of the context, so we have to compensate.
            Set_Source_Surface (Cr, Overlay, -Gdouble (X), -Gdouble (Y));

            Set_Operator (Cr, Cairo_Operator_Source);
            Do_Draw (Cr, Draw => False);
            Cairo.Restore (Cr);
         end if;

         Do_Draw (Cr, Draw => True);
         Cairo.Destroy (Cr);
      end;
   end Draw_Overlay;

   --------------------
   -- Delete_Overlay --
   --------------------

   procedure Delete_Overlay
     (Widget  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Overlay : in out Cairo.Cairo_Surface)
   is
   begin
      if Overlay /= Null_Surface then
         Surface_Destroy (Overlay);
         Overlay := Null_Surface;

         --  Force a redraw
         Gdk.Window.Invalidate_Rect
           (Widget.Get_Window,
            (0, 0, Widget.Get_Allocated_Width,
             Widget.Get_Allocated_Height), True);
      end if;
   end Delete_Overlay;

   -------------
   -- Lighten --
   -------------

   function Lighten
     (Color : Gdk.RGBA.Gdk_RGBA;
      Amount : Percent) return Gdk.RGBA.Gdk_RGBA
   is
      V : HSLA_Color := To_HSLA (Color);
   begin
      V.Lightness := Gdouble'Min (1.0, V.Lightness + Amount);
      return To_RGBA (V);
   end Lighten;

   ----------------------
   -- Get_First_Device --
   ----------------------

   function Get_First_Device
     (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Source : Gdk.Types.Gdk_Input_Source) return Gdk.Device.Gdk_Device
   is
      use Device_List;
      Screen : constant Gdk_Screen := Widget.Get_Screen;
      Mgr : constant Gdk_Device_Manager :=
        Get_Device_Manager (Screen.Get_Display);
      L : Device_List.Glist := Mgr.List_Devices (Gdk_Device_Type_Master);
      L2 : Device_List.Glist := L;
      Device : Gdk_Device;
   begin
      while L2 /= Device_List.Null_List loop
         Device := Device_List.Get_Data (L2);
         if Device.Get_Source = Source then
            Device_List.Free (L);
            return Device;
         end if;
         L2 := Device_List.Next (L2);
      end loop;

      Device_List.Free (L);
      return null;
   end Get_First_Device;

   -------------
   -- Gtk_New --
   -------------

   function Gtk_New
      (Stroke           : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Black_RGBA;
       Fill             : Cairo.Cairo_Pattern := Cairo.Null_Pattern;
       Font             : Font_Style := Default_Font;
       Line_Width       : Glib.Gdouble := 1.0;
       Dashes           : Cairo.Dash_Array := Cairo.No_Dashes;
       Arrow_From       : Arrow_Style := No_Arrow_Style;
       Arrow_To         : Arrow_Style := No_Arrow_Style;
       Symbol_From      : Symbol_Style := No_Symbol;
       Symbol_To        : Symbol_Style := No_Symbol;
       Shadow           : Shadow_Style := No_Shadow;
       Sloppy           : Boolean := False)
     return Drawing_Style
   is
      Data : constant Drawing_Style_Data_Access := new Drawing_Style_Data'
        (Refcount    => 1,
         Stroke      => Stroke,
         Fill        => Null_Pattern,
         Font        => Font,
         Line_Width  => Line_Width,
         Dashes      => null,
         Arrow_From  => Arrow_From,
         Arrow_To    => Arrow_To,
         Symbol_From => Symbol_From,
         Symbol_To   => Symbol_To,
         Shadow      => Shadow,
         Sloppy      => Sloppy);
   begin
      if Fill /= Null_Pattern then
         Data.Fill := Reference (Fill);
      end if;

      if Dashes'Length > 0 then
         Data.Dashes := new Dash_Array'(Dashes);
      end if;

      if Font.Name = null then
         if Sloppy then
            Data.Font.Name := From_String ("Comic Sans MS 14");
         else
            Data.Font.Name := From_String ("sans 14");
         end if;
      end if;

      return (Ada.Finalization.Controlled with Data => Data);
   end Gtk_New;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Drawing_Style) is
   begin
      if Self.Data /= null then
         Self.Data.Refcount := Self.Data.Refcount + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Drawing_Style) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Dash_Array, Dash_Array_Access);
      D : constant Drawing_Style_Data_Access := Self.Data;
   begin
      Self.Data := null;   --  make idempotent
      if D /= null then
         D.Refcount := D.Refcount - 1;
         if D.Refcount = 0 then
            Unchecked_Free (D.Dashes);

            if D.Fill /= Null_Pattern then
               Destroy (D.Fill);
            end if;

            if D.Font.Name /= null then
               Free (D.Font.Name);
            end if;
         end if;
      end if;
   end Finalize;

   ---------------
   -- Path_Rect --
   ---------------

   function Path_Rect
      (Self          : Drawing_Style;
       Cr            : Cairo.Cairo_Context;
       Topleft       : Point;
       Width, Height : Glib.Gdouble;
       Radius        : Glib.Gdouble := 0.0) return Boolean
   is
   begin
      if Self.Data = null
        or else (Width = 0.0 and then Height = 0.0)
      then
         return False;

      elsif Radius <= 0.001 or else Self.Data.Sloppy then
         if Self.Data.Sloppy then
            return Path_Polyline
               (Self,
                Cr,
                ((Topleft.X, Topleft.Y),
                 (Topleft.X + Width, Topleft.Y),
                 (Topleft.X + Width, Topleft.Y + Height),
                 (Topleft.X, Topleft.Y + Height)),
                Close       => True);
         else
            New_Path (Cr);
            Cairo.Rectangle (Cr, Topleft.X, Topleft.Y, Width, Height);
         end if;

      else
         Rounded_Rectangle (Cr, Topleft.X, Topleft.Y, Width, Height, Radius);
      end if;
      return True;
   end Path_Rect;

   ---------------
   -- Draw_Rect --
   ---------------

   procedure Draw_Rect
      (Self          : Drawing_Style;
       Cr            : Cairo.Cairo_Context;
       Topleft       : Point;
       Width, Height : Glib.Gdouble;
       Radius        : Gdouble := 0.0)
   is
   begin
      if Path_Rect (Self, Cr, Topleft, Width, Height, Radius) then
         Finish_Path (Self, Cr);
      end if;
   end Draw_Rect;

   ------------------
   -- Path_Ellipse --
   ------------------

   function Path_Ellipse
     (Self          : Drawing_Style;
      Cr            : Cairo.Cairo_Context;
      Topleft       : Point;
      Width, Height : Glib.Gdouble) return Boolean
   is
   begin
      if Self.Data /= null then
         Save (Cr);
         Translate (Cr, Topleft.X + Width / 2.0, Topleft.Y + Height / 2.0);
         Scale (Cr, Width / 2.0, Height / 2.0);
         Arc (Cr, 0.0, 0.0, 1.0, 0.0, 2.0 * Ada.Numerics.Pi);
         Restore (Cr);   --  before we do the stroke, or line is too large
         return True;
      end if;
      return False;
   end Path_Ellipse;

   ------------------
   -- Draw_Ellipse --
   ------------------

   procedure Draw_Ellipse
     (Self          : Drawing_Style;
      Cr            : Cairo.Cairo_Context;
      Topleft       : Point;
      Width, Height : Glib.Gdouble) is
   begin
      if Path_Ellipse (Self, Cr, Topleft, Width, Height) then
         Self.Finish_Path (Cr);
      end if;
   end Draw_Ellipse;

   -------------------
   -- Path_Polyline --
   -------------------

   function Path_Polyline
      (Self        : Drawing_Style;
       Cr          : Cairo.Cairo_Context;
       Points      : Point_Array;
       Close       : Boolean := False;
       Relative    : Boolean := False) return Boolean
   is
      Sloppy_Deviation : constant Gdouble := 0.3;
      --  This variable controls the amount of deviation from a straight line
      --  that a sloppy line uses. Using a random number instead of a constant
      --  does not change the rendering much, is slower to draw, and makes it
      --  hard to hide the line during a drag-and-drop operation

      procedure Sloppy_Line (From, To : Point);
      --  Draw a sloppy line between the two points

      procedure Sloppy_Line (From, To : Point) is
         Xc1, Xc2 : Gdouble;
         Yc1, Yc2 : Gdouble;
         Deltax, Deltay, Length, Offset : Gdouble;
      begin
         Deltax := To.X - From.X;
         Deltay := To.Y - From.Y;
         Length := Sqrt (Deltax * Deltax + Deltay * Deltay);
         Offset := Gdouble'Min (Length / 40.0, 20.0);

         Deltax := Sloppy_Deviation * Deltax + Offset;
         Deltay := Sloppy_Deviation * Deltay + Offset;

         Xc1 := From.X + Deltax;
         Yc1 := From.Y + Deltay;
         Xc2 := To.X - Deltax;
         Yc2 := To.Y - Deltay;

         Curve_To (Cr, Xc1, Yc1, Xc2, Yc2, To.X, To.Y);
      end Sloppy_Line;

      Current, Next : Point;
   begin
      if Points'Length < 2 then
         return False;
      end if;

      New_Path (Cr);
      Move_To (Cr, Points (Points'First).X, Points (Points'First).Y);

      if Self.Data /= null and then Self.Data.Sloppy then

         if Relative then
            Current := Points (Points'First);

            for P in Points'First + 1 .. Points'Last loop
               Next := (Current.X + Points (P).X, Current.Y + Points (P).Y);
               Sloppy_Line (Current, Next);
               Current := Next;
            end loop;

            if Close or else Self.Data.Fill /= Cairo.Null_Pattern then
               Sloppy_Line (Current, Points (Points'First));
               --  Manually draw line back to the first point

               Close_Path (Cr);
            end if;

         else
            for P in Points'First + 1 .. Points'Last loop
               Sloppy_Line (Points (P - 1), Points (P));
            end loop;

            if Close or else Self.Data.Fill /= Cairo.Null_Pattern then
               Sloppy_Line (Points (Points'Last), Points (Points'First));
               --  Manually draw line back to the first point

               Close_Path (Cr);
            end if;
         end if;

      else
         for P in Points'First + 1 .. Points'Last loop
            if Relative then
               Rel_Line_To (Cr, Points (P).X, Points (P).Y);
            else
               Line_To (Cr, Points (P).X, Points (P).Y);
            end if;
         end loop;

         if Close
           or else (Self.Data /= null
                    and then Self.Data.Fill /= Cairo.Null_Pattern)
         then
            Close_Path (Cr);
         end if;
      end if;

      return True;
   end Path_Polyline;

   -------------------
   -- Draw_Polyline --
   -------------------

   procedure Draw_Polyline
     (Self        : Drawing_Style;
      Cr          : Cairo.Cairo_Context;
      Points      : Point_Array;
      Close       : Boolean := False;
      Show_Arrows : Boolean := True;
      Relative    : Boolean := False)
   is
   begin
      if Path_Polyline (Self, Cr, Points, Close, Relative) then
         Finish_Path (Self, Cr);

         if Self.Data /= null and then Show_Arrows then
            Self.Draw_Arrows_And_Symbols (Cr, Points, Relative => Relative);
         end if;
      end if;
   end Draw_Polyline;

   --------------------
   -- Path_Polycurve --
   --------------------

   function Path_Polycurve
     (Self        : Drawing_Style;
      Cr          : Cairo.Cairo_Context;
      Points      : Point_Array;
      Relative    : Boolean := False) return Boolean
   is
      P : Integer;
   begin
      if Self.Data = null or else Points'Length < 4 then
         return False;
      end if;

      New_Path (Cr);
      Move_To (Cr, Points (Points'First).X, Points (Points'First).Y);

      P := Points'First;
      while P + 3 <= Points'Last loop
         if Relative then
            Rel_Curve_To
              (Cr,
               Points (P + 1).X, Points (P + 1).Y,
               Points (P + 2).X, Points (P + 2).Y,
               Points (P + 3).X, Points (P + 3).Y);
         else
            Curve_To
              (Cr,
               Points (P + 1).X, Points (P + 1).Y,
               Points (P + 2).X, Points (P + 2).Y,
               Points (P + 3).X, Points (P + 3).Y);
         end if;
         P := P + 3;
      end loop;
      return True;
   end Path_Polycurve;

   --------------------
   -- Draw_Polycurve --
   --------------------

   procedure Draw_Polycurve
     (Self        : Drawing_Style;
      Cr          : Cairo.Cairo_Context;
      Points      : Point_Array;
      Show_Arrows : Boolean := True;
      Relative    : Boolean := False) is
   begin
      if Path_Polycurve (Self, Cr, Points) then
         Finish_Path (Self, Cr);

         if Show_Arrows then
            Self.Draw_Arrows_And_Symbols (Cr, Points, Relative => Relative);
         end if;
      end if;
   end Draw_Polycurve;

   -----------------------------
   -- Draw_Arrows_And_Symbols --
   -----------------------------

   procedure Draw_Arrows_And_Symbols
     (Self     : Drawing_Style;
      Cr       : Cairo.Cairo_Context;
      Points   : Point_Array;
      Relative : Boolean := False)
   is
      procedure Draw_Arrow
        (Self    : Arrow_Style;
         Cr      : Cairo.Cairo_Context;
         To      : Point;
         Angle   : Gdouble);
      --  Draw an arrow at the end of the line from Start to To, with the given
      --  style.

      procedure Draw_Symbol (S : Symbol_Style; P : Point);
      --  Draw the symbol at the end of the line

      procedure Draw_Symbol (S : Symbol_Style; P : Point) is
      begin
         case S.Name is
            when None =>
               null;

            when Cross =>
               Save (Cr);
               New_Path (Cr);
               Move_To (Cr, P.X - 4.0, P.Y - 4.0);
               Rel_Line_To (Cr, 8.0, 8.0);
               Move_To (Cr, P.X - 4.0, P.Y + 4.0);
               Rel_Line_To (Cr, 8.0, -8.0);

               Set_Source_Color (Cr, S.Stroke);
               Set_Line_Width (Cr, S.Line_Width);
               Stroke (Cr);
               Restore (Cr);

            when Strike =>
               Save (Cr);
               Set_Line_Width (Cr, S.Line_Width);
               New_Path (Cr);
               Move_To (Cr, P.X - 6.0, P.Y + 6.0);
               Rel_Line_To (Cr, 12.0, -12.0);

               Set_Source_Color (Cr, S.Stroke);
               Set_Line_Width (Cr, S.Line_Width);
               Stroke (Cr);
               Restore (Cr);

            when Double_Strike =>
               Save (Cr);
               Set_Line_Width (Cr, S.Line_Width);
               New_Path (Cr);
               Move_To (Cr, P.X - 6.0, P.Y + 6.0);
               Rel_Line_To (Cr, 12.0, -12.0);
               Move_To (Cr, P.X - 4.0 + Self.Get_Line_Width, P.Y + 6.0);
               Rel_Line_To (Cr, 12.0, -12.0);

               Set_Source_Color (Cr, S.Stroke);
               Set_Line_Width (Cr, S.Line_Width);
               Stroke (Cr);
               Restore (Cr);
         end case;
      end Draw_Symbol;

      ----------------
      -- Draw_Arrow --
      ----------------

      procedure Draw_Arrow
        (Self    : Arrow_Style;
         Cr      : Cairo.Cairo_Context;
         To      : Point;
         Angle   : Gdouble)
      is
         X1, Y1, X2, Y2, X4, Y4, TX, TY : Gdouble;
      begin
         if Self.Head = None then
            return;
         end if;

         Set_Line_Width (Cr, Self.Line_Width);

         TX := To.X + (Get_Line_Width (Cr) - 0.5) * Cos (Angle);
         TY := To.Y + (Get_Line_Width (Cr) - 0.5) * Sin (Angle);
         X1 := To.X + Self.Length * Cos (Angle - Self.Angle);
         Y1 := To.Y + Self.Length * Sin (Angle - Self.Angle);
         X2 := To.X + Self.Length * Cos (Angle + Self.Angle);
         Y2 := To.Y + Self.Length * Sin (Angle + Self.Angle);

         case Self.Head is
         when None =>
            null;
         when Open =>
            Save (Cr);
            New_Path (Cr);
            Move_To (Cr, X1, Y1);
            Line_To (Cr, TX, TY);
            Line_To (Cr, X2, Y2);

            Set_Source_Color (Cr, Self.Stroke);
            Stroke (Cr);
            Restore (Cr);

         when Solid =>
            Save (Cr);
            New_Path (Cr);
            Move_To (Cr, X1, Y1);
            Line_To (Cr, TX, TY);
            Line_To (Cr, X2, Y2);
            Close_Path (Cr);

            if Self.Stroke /= Null_RGBA then
               Set_Source_Color (Cr, Self.Stroke);
               Stroke_Preserve (Cr);
            end if;

            if Self.Fill /= Null_RGBA then
               Set_Source_Color (Cr, Self.Fill);
               Cairo.Fill (Cr);
            end if;

            Restore (Cr);

         when Diamond =>
            X4 := X1 + Self.Length * Cos (Angle + Self.Angle);
            Y4 := Y1 + Self.Length * Sin (Angle + Self.Angle);

            Save (Cr);
            Move_To (Cr, X1, Y1);
            Line_To (Cr, TX, TY);
            Line_To (Cr, X2, Y2);
            Line_To (Cr, X4, Y4);
            Close_Path (Cr);

            if Self.Stroke /= Null_RGBA then
               Set_Source_Color (Cr, Self.Stroke);
               Stroke_Preserve (Cr);
            end if;

            if Self.Fill /= Null_RGBA then
               Set_Source_Color (Cr, Self.Fill);
               Cairo.Fill (Cr);
            end if;

            Restore (Cr);

         when Circle =>
            Save (Cr);
            Arc
              (Cr,
               Xc     => To.X,
               Yc     => To.Y,
               Radius => Self.Length,
               Angle1 => 0.0,
               Angle2 => 2.0 * Pi);

            if Self.Stroke /= Null_RGBA then
               Set_Source_Color (Cr, Self.Stroke);
               Stroke_Preserve (Cr);
            end if;

            if Self.Fill /= Null_RGBA then
               Set_Source_Color (Cr, Self.Fill);
               Cairo.Fill (Cr);
            end if;

            Restore (Cr);
         end case;
      end Draw_Arrow;

      X2, Y2, Angle : Gdouble;
      To : Point;
   begin
      if Self.Data.Symbol_From /= No_Symbol
        or else Self.Data.Arrow_From.Head /= None
      then
         if Relative then
            Angle := Pi + Arctan
              (Y => -Points (Points'First + 1).Y,
               X => -Points (Points'First + 1).X);
         else
            Angle := Pi + Arctan
              (Y => Points (Points'First).Y - Points (Points'First + 1).Y,
               X => Points (Points'First).X - Points (Points'First + 1).X);
         end if;

         if Self.Data.Symbol_From /= No_Symbol then
            X2 := Points (Points'First).X
              + Self.Data.Symbol_From.Distance * Cos (Angle);
            Y2 := Points (Points'First).Y
              + Self.Data.Symbol_From.Distance * Sin (Angle);
            Draw_Symbol (Self.Data.Symbol_From, (X2, Y2));
         end if;

         if Self.Data.Arrow_From.Head /= None then
            Draw_Arrow
              (Self.Data.Arrow_From,
               Cr,
               To     => Points (Points'First),
               Angle  => Angle);
         end if;
      end if;

      if Self.Data.Symbol_To /= No_Symbol
        or else Self.Data.Arrow_To.Head /= None
      then
         if Relative then
            To := Points (Points'First);
            for W in Points'First + 1 .. Points'Last loop
               To := (To.X + Points (W).X, To.Y + Points (W).Y);
            end loop;

            Angle := Pi + Arctan
              (Y => Points (Points'Last).Y,
               X => Points (Points'Last).X);
         else
            To := Points (Points'Last);
            Angle := Pi + Arctan
              (Y => To.Y - Points (Points'Last - 1).Y,
               X => To.X - Points (Points'Last - 1).X);
         end if;

         if Self.Data.Symbol_To /= No_Symbol then
            X2 := To.X + Self.Data.Symbol_To.Distance * Cos (Angle);
            Y2 := To.Y + Self.Data.Symbol_To.Distance * Sin (Angle);
            Draw_Symbol (Self.Data.Symbol_To, (X2, Y2));
         end if;

         if Self.Data.Arrow_To.Head /= None then
            Draw_Arrow
              (Self.Data.Arrow_To,
               Cr,
               To     => To,
               Angle  => Angle);
         end if;
      end if;
   end Draw_Arrows_And_Symbols;

   ------------------
   -- Setup_Layout --
   ------------------

   procedure Setup_Layout
     (Self     : Font_Style;
      Layout   : not null access Pango.Layout.Pango_Layout_Record'Class;
      Max_Width, Max_Height : Glib.Gdouble := -1.0)
   is
      Attr   : Pango_Attr_List;
   begin
      Gdk_New (Attr);
      Attr.Insert (Attr_Underline_New (Self.Underline));
      Attr.Insert
        (Attr_Foreground_New
           (Guint16 (Gdouble (Guint16'Last) * Self.Color.Red),
            Guint16 (Gdouble (Guint16'Last) * Self.Color.Green),
            Guint16 (Gdouble (Guint16'Last) * Self.Color.Blue)));
      Attr.Insert (Attr_Strikethrough_New (Self.Strikethrough));

      Layout.Set_Attributes (Attr);
      Layout.Set_Alignment (Self.Halign);

      if Self.Name /= null then
         Layout.Set_Font_Description (Self.Name);
      end if;

      if Max_Height > 0.0 then
         Layout.Set_Height (Gint (Max_Height + 1.0) * Pango_Scale);
      else
         Layout.Set_Height (-1);
      end if;

      if Max_Width > 0.0 then
         Layout.Set_Width (Gint (Max_Width + 1.0) * Pango_Scale);
      else
         Layout.Set_Width (-1);
      end if;

      Layout.Set_Spacing (Self.Line_Spacing);   --  spacing between lines

      Unref (Attr);

      Layout.Context_Changed;
   end Setup_Layout;

   ------------------
   -- Measure_Text --
   ------------------

   procedure Measure_Text
     (Self     : Drawing_Style;
      Layout   : not null access Pango.Layout.Pango_Layout_Record'Class;
      Text     : String;
      Width    : out Glib.Gdouble;
      Height   : out Glib.Gdouble)
   is
      Ink_Rect, Logical_Rect : Pango_Rectangle;
   begin
      if Self.Data /= null and then Self.Data.Font.Name /= null then
         Setup_Layout (Self.Data.Font, Layout);
         Layout.Set_Ellipsize (Ellipsize_None);
         Layout.Set_Text (Text);
         Layout.Get_Extents (Ink_Rect, Logical_Rect);
         Width  := Gdouble (Logical_Rect.Width) / Gdouble (Pango_Scale);
         Height := Gdouble (Logical_Rect.Height) / Gdouble (Pango_Scale);
      else
         Width := 0.0;
         Height := 0.0;
      end if;
   end Measure_Text;

   ---------------
   -- Draw_Text --
   ---------------

   procedure Draw_Text
     (Self       : Drawing_Style;
      Cr         : Cairo.Cairo_Context;
      Layout     : not null access Pango.Layout.Pango_Layout_Record'Class;
      Topleft    : Point;
      Text       : String;
      Max_Width  : Glib.Gdouble := Glib.Gdouble'First;
      Max_Height : Glib.Gdouble := Glib.Gdouble'First)
   is
   begin
      if Self.Data /= null and then Self.Data.Font.Name /= null then
         Setup_Layout (Self.Data.Font, Layout, Max_Width, Max_Height);
         Layout.Set_Ellipsize (Ellipsize_End);
         Layout.Set_Text (Text);

         Move_To (Cr, Topleft.X, Topleft.Y);
         Show_Layout (Cr, Pango_Layout (Layout));
         New_Path (Cr);
      end if;
   end Draw_Text;

   -----------------
   -- Finish_Path --
   -----------------

   procedure Finish_Path
      (Self    : Drawing_Style;
       Cr      : Cairo.Cairo_Context;
       Clear_Path : Boolean := True)
   is
   begin
      Save (Cr);

      if Self.Data /= null then
         Set_Line_Width (Cr, Self.Data.Line_Width);
      end if;

      if Self.Data /= null
        and then Self.Data.Fill /= Cairo.Null_Pattern
      then
         --  for shadows:

         --  When using Cairo 1.13, we should use cairo_set_shadow_*
         --  instead.
         --  For now, we can't draw shadows, because Translate does not
         --  apply to an existing path, so the shadow would in effect be
         --  displayed below the object, and thus be invisible.

         Set_Source (Cr, Self.Data.Fill);
         Fill_Preserve (Cr);
      end if;

      if Self.Data = null then
         Set_Source_Color (Cr, Default_Style.Stroke);

         if Clear_Path then
            Stroke (Cr);
         else
            Stroke_Preserve (Cr);
         end if;

      elsif Self.Data.Stroke /= Gdk.RGBA.Null_RGBA then
         if Self.Data.Dashes /= null then
            Set_Dash (Cr, Self.Data.Dashes.all, 0.0);
         end if;

         Set_Source_Color (Cr, Self.Data.Stroke);

         if Clear_Path then
            Stroke (Cr);
         else
            Stroke_Preserve (Cr);
         end if;

      elsif Clear_Path then
         New_Path (Cr);  --  clear existing path
      end if;

      Restore (Cr);
   end Finish_Path;

   ----------
   -- Copy --
   ----------

   function Copy (Source : Font_Style) return Font_Style is
   begin
      return Font_Style'
        (Name          => Copy (Source.Name),
         Underline     => Source.Underline,
         Strikethrough => Source.Strikethrough,
         Color         => Source.Color,
         Line_Spacing  => Source.Line_Spacing,
         Halign        => Source.Halign);
   end Copy;

   --------------------
   -- Get_Arrow_From --
   --------------------

   function Get_Arrow_From (Self : Drawing_Style) return Arrow_Style is
   begin
      if Self.Data = null then
         return Default_Style.Arrow_From;
      else
         return Self.Data.Arrow_From;
      end if;
   end Get_Arrow_From;

   ------------------
   -- Get_Arrow_To --
   ------------------

   function Get_Arrow_To (Self : Drawing_Style) return Arrow_Style is
   begin
      if Self.Data = null then
         return Default_Style.Arrow_To;
      else
         return Self.Data.Arrow_To;
      end if;
   end Get_Arrow_To;

   ----------------
   -- Get_Stroke --
   ----------------

   function Get_Stroke (Self : Drawing_Style) return Gdk.RGBA.Gdk_RGBA is
   begin
      if Self.Data = null then
         return Default_Style.Stroke;
      else
         return Self.Data.Stroke;
      end if;
   end Get_Stroke;

   --------------------
   -- Get_Line_Width --
   --------------------

   function Get_Line_Width (Self : Drawing_Style) return Glib.Gdouble is
   begin
      if Self.Data = null then
         return Default_Style.Line_Width;
      else
         return Self.Data.Line_Width;
      end if;
   end Get_Line_Width;

   --------------
   -- Get_Font --
   --------------

   function Get_Font (Self : Drawing_Style) return Font_Style is
   begin
      if Self.Data = null then
         return Default_Style.Font;
      else
         return Self.Data.Font;
      end if;
   end Get_Font;

   --------------
   -- Get_Fill --
   --------------

   function Get_Fill (Self : Drawing_Style) return Cairo.Cairo_Pattern is
   begin
      if Self.Data = null then
         return Default_Style.Fill;
      else
         return Self.Data.Fill;
      end if;
   end Get_Fill;

   ----------------
   -- Get_Shadow --
   ----------------

   function Get_Shadow (Self : Drawing_Style) return Shadow_Style is
   begin
      if Self.Data = null then
         return Default_Style.Shadow;
      else
         return Self.Data.Shadow;
      end if;
   end Get_Shadow;

   --------------
   -- Set_Fill --
   --------------

   procedure Set_Fill
     (Self   : Drawing_Style;
      Fill   : Cairo.Cairo_Pattern := Cairo.Null_Pattern) is
   begin
      if Self.Data /= null then
         Self.Data.Fill := Fill;
      end if;
   end Set_Fill;

   ----------------
   -- Set_Stroke --
   ----------------

   procedure Set_Stroke
     (Self   : Drawing_Style;
      Stroke : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Black_RGBA) is
   begin
      if Self.Data /= null then
         Self.Data.Stroke := Stroke;
      end if;
   end Set_Stroke;

end Gtkada.Style;
