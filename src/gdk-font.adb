-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
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


   -----------
   --  "="  --
   -----------

   function "=" (Fonta, Fontb : in Gdk_Font) return Boolean is
      function Internal (Fonta, Fontb : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_font_equal");
   begin
      return To_Boolean (Internal (Get_Object (Fonta),
                                   Get_Object (Fontb)));
   end "=";


   -------------------
   --  Char_Height  --
   -------------------

   function Char_Height (Font : in Gdk_Font;
                         Char : in Character) return Gint is
      function Internal (Font : in System.Address;
                         Char : in Character) return Gint;
      pragma Import (C, Internal, "gdk_char_height");
   begin
      return Internal (Get_Object (Font), Char);
   end Char_Height;


   --------------------
   --  Char_Measure  --
   --------------------

   function Char_Measure (Font : in Gdk_Font;
                          Char : in Character) return Gint is
      function Internal (Font : in System.Address;
                         Char : in C.char) return Gint;
      pragma Import (C, Internal, "gdk_char_measure");
   begin
      return Internal (Get_Object (Font), C.To_C (Char));
   end Char_Measure;


   ------------------
   --  Char_Width  --
   ------------------

   function Char_Width (Font : in Gdk_Font;
                        Char : in Character) return Gint is
      function Internal (Font : in System.Address;
                         Char : in C.char) return Gint;
      pragma Import (C, Internal, "gdk_char_width");
   begin
      return Internal (Get_Object (Font), C.To_C (Char));
   end Char_Width;


   ------------------
   --  Char_Width  --
   ------------------

   function Char_Width (Font : in Gdk_Font;
                        Char : in Gdk.Types.Gdk_WChar) return Gint is
      function Internal (Font : in System.Address;
                         Char : in C.wchar_t) return Gint;
      pragma Import (C, Internal, "gdk_char_width_wc");
   begin
      return Internal (Get_Object (Font), C.To_C (Char));
   end Char_Width;


   --------------------
   --  Fontset_Load  --
   --------------------

   procedure Fontset_Load (Font         :   out Gdk_Font;
                           Fontset_Name : in    String) is
      function Internal (Fontset_Name : in String) return System.Address;
      pragma Import (C, Internal, "gdk_fontset_load");
   begin
      Set_Object (Font, Internal (Fontset_Name & ASCII.NUL));
   end Fontset_Load;


   ----------
   --  Id  --
   ----------

   function Id (Font : in Gdk_Font) return Gint is
      function Internal (Font : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_font_id");
   begin
      return Internal (Get_Object (Font));
   end Id;


   ------------
   --  Load  --
   ------------

   procedure Load (Font      :    out Gdk_Font;
                   Font_Name : in     String) is
      function Internal (Font_Name : in String) return System.Address;
      pragma Import (C, Internal, "gdk_font_load");
   begin
      Set_Object (Font, Internal (Font_Name & ASCII.NUL));
   end Load;


   -----------
   --  Ref  --
   -----------

   procedure Ref (Font : in out Gdk_Font) is
      function Internal (Font : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_font_ref");
      S : System.Address;
   begin
      S := Internal (Get_Object (Font));
   end Ref;


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
      procedure Internal (Font     : in     System.Address;
                          Str      : in     String;
                          Lbearing :    out Gint;
                          Rbearing :    out Gint;
                          Width    :    out Gint;
                          Ascent   :    out Gint;
                          Descent  :    out Gint);
      pragma Import (C, Internal, "gdk_string_extents");
   begin
      Internal (Get_Object (Font),
                Str & Ascii.NUL,
                Lbearing, Rbearing,
                Width, Ascent, Descent);
   end String_Extents;


   ---------------------
   --  String_Height  --
   ---------------------

   function String_Height (Font : in Gdk_Font;
                           Str  : in String) return Gint is
      function Internal (Font : in System.Address;
                         Str  : in String) return Gint;
      pragma Import (C, Internal, "gdk_string_height");
   begin
      return Internal (Get_Object (Font), Str & ASCII.NUL);
   end String_Height;


   ----------------------
   --  String_Measure  --
   ----------------------

   function String_Measure (Font : in Gdk_Font;
                            Str  : in String) return Gint is
      function Internal (Font : in System.Address;
                         Str  : in String) return Gint;
      pragma Import (C, Internal, "gdk_string_measure");
   begin
      return Internal (Get_Object (Font), Str & ASCII.NUL);
   end String_Measure;


   --------------------
   --  String_Width  --
   --------------------

   function String_Width (Font : in Gdk_Font;
                          Str  : in String) return Gint is
      function Internal (Font : in System.Address;
                         Str : in String) return Gint;
      pragma Import (C, Internal, "gdk_string_width");
   begin
      return Internal (Get_Object (Font), Str & ASCII.NUL);
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
      procedure Internal (Font        : in System.Address;
                          Text        : in String;
                          Text_Length : in Gint;
                          Lbearing    :    out Gint;
                          Rbearing    :    out Gint;
                          Width       :    out Gint;
                          Ascent      :    out Gint;
                          Descent     :    out Gint);
      pragma Import (C, Internal, "gdk_text_extents");
   begin
      Internal (Get_Object (Font),
                Text, Text'Length,
                Lbearing, Rbearing,
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
      procedure Internal (Font        : in System.Address;
                          Text        : in Gdk.Types.Gdk_WString;
                          Text_Length : in Gint;
                          Lbearing    :    out Gint;
                          Rbearing    :    out Gint;
                          Width       :    out Gint;
                          Ascent      :    out Gint;
                          Descent     :    out Gint);
      pragma Import (C, Internal, "gdk_text_extents_wc");
   begin
      Internal (Get_Object (Font),
                Text, Text'Length,
                Lbearing, Rbearing,
                Width, Ascent, Descent);
   end Text_Extents;


   --------------------
   --  Text_Measure  --
   --------------------

   function Text_Measure (Font : in Gdk_Font;
                          Text : in String) return Gint is
      function Internal (Font        : in System.Address;
                         Text        : in String;
                         Text_Length : in Gint) return Gint;
      pragma Import (C, Internal, "gdk_text_measure");
   begin
      return Internal (Get_Object (Font), Text, Text'Length);
   end Text_Measure;


   ------------------
   --  Text_Width  --
   ------------------

   function Text_Width (Font : in Gdk_Font;
                        Text : in String) return Gint is
      function Internal (Font        : in System.Address;
                         Text        : in String;
                         Text_Length : in Gint) return Gint;
      pragma Import (C, Internal, "gdk_text_width");
   begin
      return Internal (Get_Object (Font), Text, Text'Length);
   end Text_Width;


   ------------------
   --  Text_Width  --
   ------------------

   function Text_Width (Font : in Gdk_Font;
                        Text : in Gdk.Types.Gdk_WString) return Gint is
      function Internal (Font        : in System.Address;
                         Text        : in Gdk.Types.Gdk_WString;
                         Text_Length : in Gint) return Gint;
      pragma Import (C, Internal, "gdk_text_width_wc");
   begin
      return Internal (Get_Object (Font), Text, Text'Length);
   end Text_Width;

   -------------
   --  Unref  --
   -------------

   procedure Unref (Font : in out Gdk_Font) is
      procedure Internal (Font : in System.Address);
      pragma Import (C, Internal, "gdk_font_unref");
   begin
      Internal (Get_Object (Font));
      Set_Object (Font, System.Null_Address);
   end Unref;

end Gdk.Font;
