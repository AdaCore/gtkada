-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2002                          --
--                         ACT-Europe                                --
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

with Interfaces.C.Strings;

package body Pango.Font is

   procedure g_free (c_str : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, g_free, "g_free");

   -----------
   -- Equal --
   -----------

   function Equal
     (Desc1 : Pango_Font_Description;
      Desc2 : Pango_Font_Description) return Boolean
   is
      function Internal
        (Desc1 : Pango_Font_Description;
         Desc2 : Pango_Font_Description) return Gboolean;
      pragma Import (C, Internal, "pango_font_description_equal");

   begin
      return Boolean'Val (Internal (Desc1, Desc2));
   end Equal;

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

   -----------------
   -- From_String --
   -----------------

   function From_String (Str : String) return Pango_Font_Description is
      function Internal (Str : String) return Pango_Font_Description;
      pragma Import (C, Internal, "pango_font_description_from_string");

   begin
      return Internal (Str & ASCII.NUL);
   end From_String;

   ------------------------
   -- To_Font_Decription --
   ------------------------

   function To_Font_Description
     (Family_Name : String := "";
      Style       : Enums.Style := Enums.Pango_Style_Normal;
      Variant     : Enums.Variant := Enums.Pango_Variant_Normal;
      Weight      : Enums.Weight := Enums.Pango_Weight_Normal;
      Stretch     : Enums.Stretch := Enums.Pango_Stretch_Normal;
      Size        : Gint := 0) return Pango_Font_Description
   is
      --  The only function provided by Pango to build a new font descriptiion
      --  is From_String. But it is an overkill to format the given parameters
      --  into a string that From_String will then parse back! The approach
      --  used here is to build a font description using the Family_Name only,
      --  and then to set directly the other different parameters...

      Result : Pango_Font_Description :=
        From_String (Family_Name & Gint'Image (Size) & " " &
                     Enums.Style'Image (Style) & " " &
                     Enums.Variant'Image (Variant) & " " &
                     Enums.Weight'Image (Weight) & " " &
                     Enums.Stretch'Image (Stretch));

   begin
      return Result;
   end To_Font_Description;

   ---------------
   -- To_String --
   ---------------

   function To_String (Desc : Pango_Font_Description) return String is
      function Internal
        (Desc : Pango_Font_Description) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "pango_font_description_to_string");

      C_Result : Interfaces.C.Strings.chars_ptr := Internal (Desc);
      Result   : constant String := Interfaces.C.Strings.Value (C_Result);

   begin
      g_free (C_Result);
      return Result;
   end To_String;

   -----------------
   -- To_Filename --
   -----------------

   function To_Filename (Desc : Pango_Font_Description) return String is
      function Internal
        (Desc : Pango_Font_Description) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "pango_font_description_to_filename");

      C_Result : Interfaces.C.Strings.chars_ptr := Internal (Desc);
      Result   : constant String := Interfaces.C.Strings.Value (C_Result);

   begin
      g_free (C_Result);
      return Result;
   end To_Filename;

   ----------------
   -- Get_Family --
   ----------------

   function Get_Family (Desc : Pango_Font_Description) return String is
      function Internal
        (Desc : Pango_Font_Description) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "pango_font_description_get_family");

   begin
      return Interfaces.C.Strings.Value (Internal (Desc));
   end Get_Family;

   ----------------
   -- Set_Family --
   ----------------

   procedure Set_Family
     (Desc : Pango_Font_Description;
      Name : String)
   is
      procedure Internal
        (Desc : Pango_Font_Description;
         Name : String);
      pragma Import (C, Internal, "pango_font_description_set_family");

   begin
      Internal (Desc, Name & ASCII.NUL);
   end Set_Family;

end Pango.Font;
