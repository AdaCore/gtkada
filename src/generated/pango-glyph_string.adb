------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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
with Glib.Object;                          use Glib.Object;
with System.Address_To_Access_Conversions;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;                         use Gtkada.Types;
pragma Warnings(On);

package body Pango.Glyph_String is

   package Conv is new System.Address_To_Access_Conversions
     (Pango_Glyph_String);

   ----------------------------
   -- Pango_Glyph_String_New --
   ----------------------------

   function Pango_Glyph_String_New return access Pango_Glyph_String is
      function Internal return System.Address;
      pragma Import (C, Internal, "pango_glyph_string_new");
   begin
      return Conv.To_Pointer (Internal);
   end Pango_Glyph_String_New;

   -------------
   -- Extents --
   -------------

   procedure Extents
      (Self         : access Pango_Glyph_String;
       Font         : not null access Pango.Font.Pango_Font_Record'Class;
       Ink_Rect     : out Pango_Rectangle;
       Logical_Rect : out Pango_Rectangle)
   is
      procedure Internal
         (Self         : access Pango_Glyph_String;
          Font         : System.Address;
          Ink_Rect     : out Pango_Rectangle;
          Logical_Rect : out Pango_Rectangle);
      pragma Import (C, Internal, "pango_glyph_string_extents");
   begin
      Internal (Self, Get_Object (Font), Ink_Rect, Logical_Rect);
   end Extents;

   -------------------
   -- Extents_Range --
   -------------------

   procedure Extents_Range
      (Self         : access Pango_Glyph_String;
       Start        : Glib.Gint;
       The_End      : Glib.Gint;
       Font         : not null access Pango.Font.Pango_Font_Record'Class;
       Ink_Rect     : out Pango_Rectangle;
       Logical_Rect : out Pango_Rectangle)
   is
      procedure Internal
         (Self         : access Pango_Glyph_String;
          Start        : Glib.Gint;
          The_End      : Glib.Gint;
          Font         : System.Address;
          Ink_Rect     : out Pango_Rectangle;
          Logical_Rect : out Pango_Rectangle);
      pragma Import (C, Internal, "pango_glyph_string_extents_range");
   begin
      Internal (Self, Start, The_End, Get_Object (Font), Ink_Rect, Logical_Rect);
   end Extents_Range;

   ------------------------
   -- Get_Logical_Widths --
   ------------------------

   procedure Get_Logical_Widths
      (Self            : access Pango_Glyph_String;
       Text            : UTF8_String;
       Length          : Glib.Gint;
       Embedding_Level : Glib.Gint;
       Logical_Widths  : Gint_Array)
   is
      procedure Internal
         (Self            : access Pango_Glyph_String;
          Text            : Gtkada.Types.Chars_Ptr;
          Length          : Glib.Gint;
          Embedding_Level : Glib.Gint;
          Logical_Widths  : System.Address);
      pragma Import (C, Internal, "pango_glyph_string_get_logical_widths");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Self, Tmp_Text, Length, Embedding_Level, Logical_Widths'Address);
      Free (Tmp_Text);
   end Get_Logical_Widths;

   ----------------
   -- Index_To_X --
   ----------------

   procedure Index_To_X
      (Self     : access Pango_Glyph_String;
       Text     : UTF8_String;
       Length   : Glib.Gint;
       Analysis : access Pango.Item.Pango_Analysis;
       Index    : Glib.Gint;
       Trailing : Boolean;
       X_Pos    : out Glib.Gint)
   is
      procedure Internal
         (Self     : access Pango_Glyph_String;
          Text     : Gtkada.Types.Chars_Ptr;
          Length   : Glib.Gint;
          Analysis : access Pango.Item.Pango_Analysis;
          Index    : Glib.Gint;
          Trailing : Glib.Gboolean;
          X_Pos    : out Glib.Gint);
      pragma Import (C, Internal, "pango_glyph_string_index_to_x");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Self, Tmp_Text, Length, Analysis, Index, Boolean'Pos (Trailing), X_Pos);
      Free (Tmp_Text);
   end Index_To_X;

   ----------------
   -- X_To_Index --
   ----------------

   procedure X_To_Index
      (Self     : access Pango_Glyph_String;
       Text     : UTF8_String;
       Length   : Glib.Gint;
       Analysis : access Pango.Item.Pango_Analysis;
       X_Pos    : Glib.Gint;
       Index    : out Glib.Gint;
       Trailing : out Glib.Gint)
   is
      procedure Internal
         (Self     : access Pango_Glyph_String;
          Text     : Gtkada.Types.Chars_Ptr;
          Length   : Glib.Gint;
          Analysis : access Pango.Item.Pango_Analysis;
          X_Pos    : Glib.Gint;
          Index    : out Glib.Gint;
          Trailing : out Glib.Gint);
      pragma Import (C, Internal, "pango_glyph_string_x_to_index");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Self, Tmp_Text, Length, Analysis, X_Pos, Index, Trailing);
      Free (Tmp_Text);
   end X_To_Index;

   ----------------------
   -- From_Object_Free --
   ----------------------

   function From_Object_Free
     (B : not null access Pango_Glyph_Vis_Attr) return Pango_Glyph_Vis_Attr
   is
      Result : constant Pango_Glyph_Vis_Attr := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   ----------------------
   -- From_Object_Free --
   ----------------------

   function From_Object_Free
     (B : not null access Pango_Glyph_Geometry) return Pango_Glyph_Geometry
   is
      Result : constant Pango_Glyph_Geometry := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   ----------------------
   -- From_Object_Free --
   ----------------------

   function From_Object_Free
     (B : not null access Pango_Glyph_Info) return Pango_Glyph_Info
   is
      Result : constant Pango_Glyph_Info := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   ----------------------
   -- From_Object_Free --
   ----------------------

   function From_Object_Free
     (B : not null access Pango_Glyph_String) return Pango_Glyph_String
   is
      Result : constant Pango_Glyph_String := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

end Pango.Glyph_String;
