-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

with Interfaces.C;

package body Gdk.Font is

   package C renames Interfaces.C;


   -------------
   --  Equal  --
   -------------

   function Equal (Fonta, Fontb : in Gdk_Font) return Boolean is
      function Internal (Fonta, Fontb : in Gdk_Font) return Gint;
      pragma Import (C, Internal, "gdk_font_equal");
   begin
      return Boolean'Val (Internal (Fonta, Fontb));
   end Equal;

   --------------------
   --  Char_Measure  --
   --------------------

   function Char_Measure (Font : in Gdk_Font;
                          Char : in Character) return Gint is
      function Internal (Font : in Gdk_Font;
                         Char : in C.char) return Gint;
      pragma Import (C, Internal, "gdk_char_measure");
   begin
      return Internal (Font, C.To_C (Char));
   end Char_Measure;

   ------------------
   --  Char_Width  --
   ------------------

   function Char_Width (Font : in Gdk_Font;
                        Char : in Character) return Gint is
      function Internal (Font : in Gdk_Font;
                         Char : in C.char) return Gint;
      pragma Import (C, Internal, "gdk_char_width");
   begin
      return Internal (Font, C.To_C (Char));
   end Char_Width;

   ------------------
   --  Char_Width  --
   ------------------

   function Char_Width (Font : in Gdk_Font;
                        Char : in Gdk.Types.Gdk_WChar) return Gint is
      function Internal (Font : in Gdk_Font;
                         Char : in C.wchar_t) return Gint;
      pragma Import (C, Internal, "gdk_char_width_wc");
   begin
      return Internal (Font, C.To_C (Char));
   end Char_Width;

   --------------------
   --  Fontset_Load  --
   --------------------

   procedure Fontset_Load (Font         :   out Gdk_Font;
                           Fontset_Name : in    String) is
      function Internal (Fontset_Name : in String) return Gdk_Font;
      pragma Import (C, Internal, "gdk_fontset_load");
   begin
      Font := Internal (Fontset_Name & ASCII.NUL);
   end Fontset_Load;

   ------------
   --  Load  --
   ------------

   procedure Load (Font      :    out Gdk_Font;
                   Font_Name : in     String) is
      function Internal (Font_Name : in String) return Gdk_Font;
      pragma Import (C, Internal, "gdk_font_load");
   begin
      Font := Internal (Font_Name & ASCII.NUL);
   end Load;

   --------------------
   -- String_Extents --
   --------------------

   procedure String_Extents (Font     : in      Gdk.Font.Gdk_Font;
                             Str      : in      String;
                             Lbearing :     out Gint;
                             Rbearing :     out Gint;
                             Width    :     out Gint;
                             Ascent   :     out Gint;
                             Descent  :     out Gint) is
      procedure Internal (Font     : in     Gdk.Font.Gdk_Font;
                          Str      : in     String;
                          Lbearing :    out Gint;
                          Rbearing :    out Gint;
                          Width    :    out Gint;
                          Ascent   :    out Gint;
                          Descent  :    out Gint);
      pragma Import (C, Internal, "gdk_string_extents");
   begin
      Internal (Font, Str & ASCII.Nul, Lbearing, Rbearing,
                Width, Ascent, Descent);
   end String_Extents;

   ---------------------
   --  String_Height  --
   ---------------------

   function String_Height (Font : in Gdk_Font;
                           Str  : in String) return Gint is
      function Internal (Font : in Gdk_Font;
                         Str  : in String) return Gint;
      pragma Import (C, Internal, "gdk_string_height");
   begin
      return Internal (Font, Str & ASCII.NUL);
   end String_Height;

   -----------------
   -- Text_Height --
   -----------------

   function Text_Height (Font : in Gdk_Font;
                         Str  : in String)
                        return Gint
   is
      function Internal (Font        : in Gdk_Font;
                         Text        : in String;
                         Text_Length : in Gint) return Gint;
      pragma Import (C, Internal, "gdk_text_height");
   begin
      return Internal (Font, Str, Str'Length);
   end Text_Height;

   ----------------------
   --  String_Measure  --
   ----------------------

   function String_Measure (Font : in Gdk_Font;
                            Str  : in String) return Gint is
      function Internal (Font : in Gdk_Font;
                         Str  : in String) return Gint;
      pragma Import (C, Internal, "gdk_string_measure");
   begin
      return Internal (Font, Str & ASCII.NUL);
   end String_Measure;

   --------------------
   --  String_Width  --
   --------------------

   function String_Width (Font : in Gdk_Font;
                          Str  : in String) return Gint is
      function Internal (Font : in Gdk_Font;
                         Str : in String) return Gint;
      pragma Import (C, Internal, "gdk_string_width");
   begin
      return Internal (Font, Str & ASCII.NUL);
   end String_Width;

   ------------------
   -- Text_Extents --
   ------------------

   procedure Text_Extents (Font        : in Gdk_Font;
                           Text        : in String;
                           Lbearing    :    out Gint;
                           Rbearing    :    out Gint;
                           Width       :    out Gint;
                           Ascent      :    out Gint;
                           Descent     :    out Gint) is
      procedure Internal (Font        : in Gdk_Font;
                          Text        : in String;
                          Text_Length : in Gint;
                          Lbearing    :    out Gint;
                          Rbearing    :    out Gint;
                          Width       :    out Gint;
                          Ascent      :    out Gint;
                          Descent     :    out Gint);
      pragma Import (C, Internal, "gdk_text_extents");
   begin
      Internal (Font, Text, Text'Length, Lbearing, Rbearing,
                Width, Ascent, Descent);
   end Text_Extents;

   ------------------
   -- Text_Extents --
   ------------------

   procedure Text_Extents (Font        : in Gdk_Font;
                           Text        : in Gdk.Types.Gdk_WString;
                           Lbearing    :    out Gint;
                           Rbearing    :    out Gint;
                           Width       :    out Gint;
                           Ascent      :    out Gint;
                           Descent     :    out Gint) is
      procedure Internal (Font        : in Gdk_Font;
                          Text        : in Gdk.Types.Gdk_WString;
                          Text_Length : in Gint;
                          Lbearing    :    out Gint;
                          Rbearing    :    out Gint;
                          Width       :    out Gint;
                          Ascent      :    out Gint;
                          Descent     :    out Gint);
      pragma Import (C, Internal, "gdk_text_extents_wc");
   begin
      Internal (Font, Text, Text'Length, Lbearing, Rbearing,
                Width, Ascent, Descent);
   end Text_Extents;

   --------------------
   --  Text_Measure  --
   --------------------

   function Text_Measure (Font : in Gdk_Font;
                          Text : in String) return Gint is
      function Internal (Font        : in Gdk_Font;
                         Text        : in String;
                         Text_Length : in Gint) return Gint;
      pragma Import (C, Internal, "gdk_text_measure");
   begin
      return Internal (Font, Text, Text'Length);
   end Text_Measure;

   ------------------
   --  Text_Width  --
   ------------------

   function Text_Width (Font : in Gdk_Font;
                        Text : in String) return Gint is
      function Internal (Font        : in Gdk_Font;
                         Text        : in String;
                         Text_Length : in Gint) return Gint;
      pragma Import (C, Internal, "gdk_text_width");
   begin
      return Internal (Font, Text, Text'Length);
   end Text_Width;

   ------------------
   --  Text_Width  --
   ------------------

   function Text_Width (Font : in Gdk_Font;
                        Text : in Gdk.Types.Gdk_WString) return Gint is
      function Internal (Font        : in Gdk_Font;
                         Text        : in Gdk.Types.Gdk_WString;
                         Text_Length : in Gint) return Gint;
      pragma Import (C, Internal, "gdk_text_width_wc");
   begin
      return Internal (Font, Text, Text'Length);
   end Text_Width;

end Gdk.Font;
