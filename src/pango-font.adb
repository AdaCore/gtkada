-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
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

package body Pango.Font is

   --  When managing the Family_Name field of Pango_Font_Description_Record,
   --  we prefer to use the memory allocation routines provided by gtk+ (ie
   --  g_strdup and g_free) rather than manage it using the routines provided
   --  in Interfaces.C.Strings. This is necessary to avoid mixing the use of
   --  both Ada and C memory allocation/deallocation routines, which can
   --  confuse their memory tracking mechanism, and which might even lead to
   --  failures in certains OSes.

   function g_strndup
     (str : String; len : Integer) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, g_strndup, "g_strndup");

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
      Style       : Pango.Enums.Style := Pango.Enums.Pango_Style_Normal;
      Variant     : Pango.Enums.Variant := Pango.Enums.Pango_Variant_Normal;
      Weight      : Pango.Enums.Weight := Pango.Enums.Pango_Weight_Normal;
      Stretch     : Pango.Enums.Stretch := Pango.Enums.Pango_Stretch_Normal;
      Size        : Gint := 0) return Pango_Font_Description
   is
      --  The only function provided by Pango to build a new font descriptiion
      --  is From_String. But it is an overkill to format the given parameters
      --  into a string that From_String will then parse back! The approach
      --  used here is to build a font description using the Family_Name only,
      --  and then to set directly the other different parameters...
      Result : Pango_Font_Description :=
        From_String (Family_Name & Gint'Image (Size));
   begin
      Result.all :=
        (Family_Name => Result.Family_Name,
         Style => Style,
         Variant => Variant,
         Weight => Weight,
         Stretch => Stretch,
         Size => Result.Size);
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

   ---------------------
   -- Get_Family_Name --
   ---------------------

   function Get_Family_Name (Desc : Pango_Font_Description) return String is
   begin
      return Interfaces.C.Strings.Value (Desc.Family_Name);
   end Get_Family_Name;

   ---------------------
   -- Set_Family_Name --
   ---------------------

   procedure Set_Family_Name
     (Desc : Pango_Font_Description;
      Name : String)
   is
      use type Interfaces.C.Strings.chars_ptr;
   begin
      if Desc.Family_Name /= Interfaces.C.Strings.Null_Ptr then
         g_free (Desc.Family_Name);
      end if;

      Desc.Family_Name := g_strndup (Name, Name'Length);
   end Set_Family_Name;

end Pango.Font;
