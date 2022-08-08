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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Pango.Font is

   subtype String7 is String (1 .. 7);
   Style_Map : constant array (Enums.Style) of String7 :=
     (Enums.Pango_Style_Normal  => "       ",
      Enums.Pango_Style_Oblique => "Oblique",
      Enums.Pango_Style_Italic  => "Italic ");

   subtype String10 is String (1 .. 10);
   Variant_Map : constant array (Enums.Variant) of String10 :=
     (Enums.Pango_Variant_Normal     => "          ",
      Enums.Pango_Variant_Small_Caps => "Small-Caps");

   subtype String15 is String (1 .. 15);
   Stretch_Map : constant array (Enums.Stretch) of String15 :=
     (Enums.Pango_Stretch_Ultra_Condensed => "Ultra-Condensed",
      Enums.Pango_Stretch_Extra_Condensed => "Extra-Condensed",
      Enums.Pango_Stretch_Condensed       => "Condensed      ",
      Enums.Pango_Stretch_Semi_Condensed  => "Semi-Condensed ",
      Enums.Pango_Stretch_Normal          => "               ",
      Enums.Pango_Stretch_Semi_Expanded   => "Semi-Expanded  ",
      Enums.Pango_Stretch_Expanded        => "Expanded       ",
      Enums.Pango_Stretch_Extra_Expanded  => "Extra-Expanded ",
      Enums.Pango_Stretch_Ultra_Expanded  => "Ultra-Expanded ");

   --  Some of the values are not directly supported by pango.
   --  ??? See fonts.c in pango

   Weight_Map : constant array (Enums.Weight) of String10 :=
     (Enums.Pango_Weight_Ultralight  => "Light     ",
      Enums.Pango_Weight_Thin        => "Thin      ",
      Enums.Pango_Weight_Light       => "Light     ",
      Enums.Pango_Weight_Normal      => "          ",
      Enums.Pango_Weight_Book        => "Book      ",
      Enums.Pango_Weight_Medium      => "Medium    ",
      Enums.Pango_Weight_Semibold    => "Semi-Bold ",
      Enums.Pango_Weight_Bold        => "Bold      ",
      Enums.Pango_Weight_Semilight   => "Semi-Light",
      Enums.Pango_Weight_Ultrabold   => "Bold      ",
      Enums.Pango_Weight_Ultraheavy  => "Ultraheavy",
      Enums.Pango_Weight_Heavy       => "Heavy     ");

   function To_Font_Description
     (Family_Name : String := "";
      Style       : Enums.Style := Enums.Pango_Style_Normal;
      Variant     : Enums.Variant := Enums.Pango_Variant_Normal;
      Weight      : Enums.Weight := Enums.Pango_Weight_Normal;
      Stretch     : Enums.Stretch := Enums.Pango_Stretch_Normal;
      Size        : Gint := 0) return Pango_Font_Description
   is
   begin
      return From_String (Family_Name & " " &
         Style_Map (Style) & " " &
         Variant_Map (Variant) &
         Weight_Map (Weight) & " " &
         Stretch_Map (Stretch) & Gint'Image (Size));
   end To_Font_Description;

   function Get_Style_As_String
     (Self : Pango_Font_Description) return String
   is
      Style : constant Pango.Enums.Style := Get_Style (Self);
   begin
      if Style = Pango.Enums.Pango_Style_Normal then
         return "Normal";
      else
         return Style_Map (Style);
      end if;
   end Get_Style_As_String;

   function Get_Weight_As_String
     (Self : Pango_Font_Description) return String
   is
      Weight : constant Pango.Enums.Weight := Get_Weight (Self);
   begin
      if Weight = Pango.Enums.Pango_Weight_Normal then
         return "Normal";
      else
         return Weight_Map (Weight);
      end if;
   end Get_Weight_As_String;

   function To_Address
     (F : Pango_Font_Description; Add : System.Address)
   return System.Address
   is
      pragma Unreferenced (Add);
   begin
      return F.all'Address;
   end To_Address;

   ----------
   -- Free --
   ----------

   procedure Free (Desc : in out Pango_Font_Description) is
      procedure Internal (Desc : Pango_Font_Description);
      pragma Import (C, Internal, "pango_font_description_free");

   begin
      Internal (Desc);
      Desc := null;
   end Free;

   package Type_Conversion_Pango_Font is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Font_Get_Type'Access, Pango_Font_Record);
   pragma Unreferenced (Type_Conversion_Pango_Font);

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Self : out Pango_Font_Description) is
      function Internal return Pango_Font_Description;
      pragma Import (C, Internal, "pango_font_description_new");
   begin
      Self := Internal;
   end Gdk_New;

   --------------------------------
   -- Pango_Font_Description_New --
   --------------------------------

   function Pango_Font_Description_New return Pango_Font_Description is
      function Internal return Pango_Font_Description;
      pragma Import (C, Internal, "pango_font_description_new");
      Self : Pango_Font_Description;
   begin
      Self := Internal;
      return Self;
   end Pango_Font_Description_New;

   ------------------
   -- Better_Match --
   ------------------

   function Better_Match
      (Self      : Pango_Font_Description;
       Old_Match : Pango_Font_Description;
       New_Match : Pango_Font_Description) return Boolean
   is
      function Internal
         (Self      : Pango_Font_Description;
          Old_Match : Pango_Font_Description;
          New_Match : Pango_Font_Description) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_font_description_better_match");
   begin
      return Internal (Self, Old_Match, New_Match) /= 0;
   end Better_Match;

   --------------
   -- Describe --
   --------------

   function Describe
      (Font : not null access Pango_Font_Record'Class)
       return Pango_Font_Description
   is
      function Internal
         (Font : System.Address) return Pango_Font_Description;
      pragma Import (C, Internal, "pango_font_describe");
   begin
      return Internal (Get_Object (Font));
   end Describe;

   ---------------------------------
   -- Describe_With_Absolute_Size --
   ---------------------------------

   function Describe_With_Absolute_Size
      (Font : not null access Pango_Font_Record'Class)
       return Pango_Font_Description
   is
      function Internal
         (Font : System.Address) return Pango_Font_Description;
      pragma Import (C, Internal, "pango_font_describe_with_absolute_size");
   begin
      return Internal (Get_Object (Font));
   end Describe_With_Absolute_Size;

   -----------
   -- Equal --
   -----------

   function Equal
      (Self  : Pango_Font_Description;
       Desc2 : Pango_Font_Description) return Boolean
   is
      function Internal
         (Self  : Pango_Font_Description;
          Desc2 : Pango_Font_Description) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_font_description_equal");
   begin
      return Internal (Self, Desc2) /= 0;
   end Equal;

   ----------------
   -- Get_Family --
   ----------------

   function Get_Family (Self : Pango_Font_Description) return UTF8_String is
      function Internal
         (Self : Pango_Font_Description) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "pango_font_description_get_family");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Self));
   end Get_Family;

   -----------------------
   -- Get_Glyph_Extents --
   -----------------------

   procedure Get_Glyph_Extents
      (Font         : not null access Pango_Font_Record;
       Glyph        : Pango_Glyph;
       Ink_Rect     : out Pango_Rectangle;
       Logical_Rect : out Pango_Rectangle)
   is
      procedure Internal
         (Font         : System.Address;
          Glyph        : Pango_Glyph;
          Ink_Rect     : out Pango_Rectangle;
          Logical_Rect : out Pango_Rectangle);
      pragma Import (C, Internal, "pango_font_get_glyph_extents");
   begin
      Internal (Get_Object (Font), Glyph, Ink_Rect, Logical_Rect);
   end Get_Glyph_Extents;

   -----------------
   -- Get_Metrics --
   -----------------

   function Get_Metrics
      (Font     : not null access Pango_Font_Record;
       Language : Pango.Language.Pango_Language := Pango.Language.Null_Pango_Language)
       return Pango.Font_Metrics.Pango_Font_Metrics
   is
      function Internal
         (Font     : System.Address;
          Language : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_font_get_metrics");
   begin
      return From_Object (Internal (Get_Object (Font), Get_Object (Language)));
   end Get_Metrics;

   --------------------------
   -- Get_Size_Is_Absolute --
   --------------------------

   function Get_Size_Is_Absolute
      (Self : Pango_Font_Description) return Boolean
   is
      function Internal (Self : Pango_Font_Description) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_font_description_get_size_is_absolute");
   begin
      return Internal (Self) /= 0;
   end Get_Size_Is_Absolute;

   --------------------
   -- Get_Variations --
   --------------------

   function Get_Variations
      (Self : Pango_Font_Description) return UTF8_String
   is
      function Internal
         (Self : Pango_Font_Description) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "pango_font_description_get_variations");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Self));
   end Get_Variations;

   --------------
   -- Has_Char --
   --------------

   function Has_Char
      (Font : not null access Pango_Font_Record;
       Wc   : Gunichar) return Boolean
   is
      function Internal
         (Font : System.Address;
          Wc   : Gunichar) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_font_has_char");
   begin
      return Internal (Get_Object (Font), Wc) /= 0;
   end Has_Char;

   -----------
   -- Merge --
   -----------

   procedure Merge
      (Self             : Pango_Font_Description;
       Desc_To_Merge    : Pango_Font_Description;
       Replace_Existing : Boolean)
   is
      procedure Internal
         (Self             : Pango_Font_Description;
          Desc_To_Merge    : Pango_Font_Description;
          Replace_Existing : Glib.Gboolean);
      pragma Import (C, Internal, "pango_font_description_merge");
   begin
      Internal (Self, Desc_To_Merge, Boolean'Pos (Replace_Existing));
   end Merge;

   ------------------
   -- Merge_Static --
   ------------------

   procedure Merge_Static
      (Self             : Pango_Font_Description;
       Desc_To_Merge    : Pango_Font_Description;
       Replace_Existing : Boolean)
   is
      procedure Internal
         (Self             : Pango_Font_Description;
          Desc_To_Merge    : Pango_Font_Description;
          Replace_Existing : Glib.Gboolean);
      pragma Import (C, Internal, "pango_font_description_merge_static");
   begin
      Internal (Self, Desc_To_Merge, Boolean'Pos (Replace_Existing));
   end Merge_Static;

   ----------------
   -- Set_Family --
   ----------------

   procedure Set_Family
      (Self   : Pango_Font_Description;
       Family : UTF8_String)
   is
      procedure Internal
         (Self   : Pango_Font_Description;
          Family : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "pango_font_description_set_family");
      Tmp_Family : Gtkada.Types.Chars_Ptr := New_String (Family);
   begin
      Internal (Self, Tmp_Family);
      Free (Tmp_Family);
   end Set_Family;

   -----------------------
   -- Set_Family_Static --
   -----------------------

   procedure Set_Family_Static
      (Self   : Pango_Font_Description;
       Family : UTF8_String)
   is
      procedure Internal
         (Self   : Pango_Font_Description;
          Family : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "pango_font_description_set_family_static");
      Tmp_Family : Gtkada.Types.Chars_Ptr := New_String (Family);
   begin
      Internal (Self, Tmp_Family);
      Free (Tmp_Family);
   end Set_Family_Static;

   --------------------
   -- Set_Variations --
   --------------------

   procedure Set_Variations
      (Self       : Pango_Font_Description;
       Variations : UTF8_String)
   is
      procedure Internal
         (Self       : Pango_Font_Description;
          Variations : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "pango_font_description_set_variations");
      Tmp_Variations : Gtkada.Types.Chars_Ptr := New_String (Variations);
   begin
      Internal (Self, Tmp_Variations);
      Free (Tmp_Variations);
   end Set_Variations;

   ---------------------------
   -- Set_Variations_Static --
   ---------------------------

   procedure Set_Variations_Static
      (Self       : Pango_Font_Description;
       Variations : UTF8_String)
   is
      procedure Internal
         (Self       : Pango_Font_Description;
          Variations : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "pango_font_description_set_variations_static");
      Tmp_Variations : Gtkada.Types.Chars_Ptr := New_String (Variations);
   begin
      Internal (Self, Tmp_Variations);
      Free (Tmp_Variations);
   end Set_Variations_Static;

   -----------------
   -- To_Filename --
   -----------------

   function To_Filename (Self : Pango_Font_Description) return UTF8_String is
      function Internal
         (Self : Pango_Font_Description) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "pango_font_description_to_filename");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end To_Filename;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : Pango_Font_Description) return UTF8_String is
      function Internal
         (Self : Pango_Font_Description) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "pango_font_description_to_string");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end To_String;

   -----------------
   -- From_String --
   -----------------

   function From_String (Str : UTF8_String) return Pango_Font_Description is
      function Internal
         (Str : Gtkada.Types.Chars_Ptr) return Pango_Font_Description;
      pragma Import (C, Internal, "pango_font_description_from_string");
      Tmp_Str    : Gtkada.Types.Chars_Ptr := New_String (Str);
      Tmp_Return : Pango_Font_Description;
   begin
      Tmp_Return := Internal (Tmp_Str);
      Free (Tmp_Str);
      return Tmp_Return;
   end From_String;

end Pango.Font;
