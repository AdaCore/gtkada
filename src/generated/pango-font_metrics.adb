------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");

package body Pango.Font_Metrics is

   function From_Object_Free
     (B : access Pango_Font_Metrics'Class) return Pango_Font_Metrics
   is
      Result : constant Pango_Font_Metrics := Pango_Font_Metrics (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Pango_Font_Metrics is
      S : Pango_Font_Metrics;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   --------------------------------
   -- Get_Approximate_Char_Width --
   --------------------------------

   function Get_Approximate_Char_Width
      (Self : Pango_Font_Metrics) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_font_metrics_get_approximate_char_width");
   begin
      return Internal (Get_Object (Self));
   end Get_Approximate_Char_Width;

   ---------------------------------
   -- Get_Approximate_Digit_Width --
   ---------------------------------

   function Get_Approximate_Digit_Width
      (Self : Pango_Font_Metrics) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_font_metrics_get_approximate_digit_width");
   begin
      return Internal (Get_Object (Self));
   end Get_Approximate_Digit_Width;

   ----------------
   -- Get_Ascent --
   ----------------

   function Get_Ascent (Self : Pango_Font_Metrics) return Glib.Gint is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_font_metrics_get_ascent");
   begin
      return Internal (Get_Object (Self));
   end Get_Ascent;

   -----------------
   -- Get_Descent --
   -----------------

   function Get_Descent (Self : Pango_Font_Metrics) return Glib.Gint is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_font_metrics_get_descent");
   begin
      return Internal (Get_Object (Self));
   end Get_Descent;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height (Self : Pango_Font_Metrics) return Glib.Gint is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_font_metrics_get_height");
   begin
      return Internal (Get_Object (Self));
   end Get_Height;

   --------------------------------
   -- Get_Strikethrough_Position --
   --------------------------------

   function Get_Strikethrough_Position
      (Self : Pango_Font_Metrics) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_font_metrics_get_strikethrough_position");
   begin
      return Internal (Get_Object (Self));
   end Get_Strikethrough_Position;

   ---------------------------------
   -- Get_Strikethrough_Thickness --
   ---------------------------------

   function Get_Strikethrough_Thickness
      (Self : Pango_Font_Metrics) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_font_metrics_get_strikethrough_thickness");
   begin
      return Internal (Get_Object (Self));
   end Get_Strikethrough_Thickness;

   ----------------------------
   -- Get_Underline_Position --
   ----------------------------

   function Get_Underline_Position
      (Self : Pango_Font_Metrics) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_font_metrics_get_underline_position");
   begin
      return Internal (Get_Object (Self));
   end Get_Underline_Position;

   -----------------------------
   -- Get_Underline_Thickness --
   -----------------------------

   function Get_Underline_Thickness
      (Self : Pango_Font_Metrics) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_font_metrics_get_underline_thickness");
   begin
      return Internal (Get_Object (Self));
   end Get_Underline_Thickness;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Pango_Font_Metrics) return Pango_Font_Metrics is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_font_metrics_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Pango_Font_Metrics) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "pango_font_metrics_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Pango.Font_Metrics;
