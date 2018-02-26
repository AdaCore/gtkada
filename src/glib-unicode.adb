------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package body Glib.Unicode is

   -------------------
   -- UTF8_Validate --
   -------------------

   procedure UTF8_Validate
     (Str         : UTF8_String;
      Valid       : out Boolean;
      Invalid_Pos : out Natural)
   is
      function UTF8_Validate
        (Str         : UTF8_String;
         Len         : Gsize;
         Invalid_Pos : access System.Address) return Gboolean;
      pragma Import (C, UTF8_Validate, "g_utf8_validate");

      Pos : aliased System.Address;

   begin
      Valid := UTF8_Validate (Str, Str'Length, Pos'Access) /= 0;

      if Valid then
         Invalid_Pos := 0;
      else
         Invalid_Pos := Natural (Pos - Str (Str'First)'Address) + Str'First;
      end if;
   end UTF8_Validate;

   --------------
   -- Is_Space --
   --------------

   function Is_Space (Char : Gunichar) return Boolean is
      function Internal (Char : Gunichar) return Integer;
      pragma Import (C, Internal, "g_unichar_isspace");
   begin
      return Boolean'Val (Internal (Char));
   end Is_Space;

   --------------
   -- Is_Alnum --
   --------------

   function Is_Alnum (Char : Gunichar) return Boolean is
      function Internal (Char : Gunichar) return Integer;
      pragma Import (C, Internal, "g_unichar_isalnum");
   begin
      return Boolean'Val (Internal (Char));
   end Is_Alnum;

   --------------
   -- Is_Alpha --
   --------------

   function Is_Alpha (Char : Gunichar) return Boolean is
      function Internal (Char : Gunichar) return Integer;
      pragma Import (C, Internal, "g_unichar_isalpha");
   begin
      return Boolean'Val (Internal (Char));
   end Is_Alpha;

   --------------
   -- Is_Digit --
   --------------

   function Is_Digit (Char : Gunichar) return Boolean is
      function Internal (Char : Gunichar) return Integer;
      pragma Import (C, Internal, "g_unichar_isdigit");
   begin
      return Boolean'Val (Internal (Char));
   end Is_Digit;

   --------------
   -- Is_Lower --
   --------------

   function Is_Lower (Char : Gunichar) return Boolean is
      function Internal (Char : Gunichar) return Integer;
      pragma Import (C, Internal, "g_unichar_islower");
   begin
      return Boolean'Val (Internal (Char));
   end Is_Lower;

   --------------
   -- Is_Upper --
   --------------

   function Is_Upper (Char : Gunichar) return Boolean is
      function Internal (Char : Gunichar) return Integer;
      pragma Import (C, Internal, "g_unichar_isupper");
   begin
      return Boolean'Val (Internal (Char));
   end Is_Upper;

   --------------
   -- Is_Punct --
   --------------

   function Is_Punct (Char : Gunichar) return Boolean is
      function Internal (Char : Gunichar) return Integer;
      pragma Import (C, Internal, "g_unichar_ispunct");
   begin
      return Boolean'Val (Internal (Char));
   end Is_Punct;

   ------------------
   -- UTF8_Strdown --
   ------------------

   function UTF8_Strdown (Str : UTF8_String) return UTF8_String is
      function Internal
        (Str : UTF8_String; Len : Natural) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_utf8_strdown");

      S : constant Gtkada.Types.Chars_Ptr := Internal (Str, Str'Length);
      Result : constant String := Gtkada.Types.Value (S);
   begin
      Gtkada.Types.g_free (S);
      return Result;
   end UTF8_Strdown;

   ----------------
   -- UTF8_Strup --
   ----------------

   function UTF8_Strup (Str : UTF8_String) return UTF8_String is
      function Internal
        (Str : UTF8_String; Length : Natural) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_utf8_strup");

      S : constant Gtkada.Types.Chars_Ptr := Internal (Str, Str'Length);
      Result : constant String := Gtkada.Types.Value (S);
   begin
      Gtkada.Types.g_free (S);
      return Result;
   end UTF8_Strup;

   --------------------
   -- UTF8_Next_Char --
   --------------------

   type Byte is range 1 .. 6;
   type Byte_Array is array (Character) of Byte;

   UTF8_Skip_Data : constant Byte_Array :=
     (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 1, 1);

   function UTF8_Next_Char
     (Str : UTF8_String; Index : Natural) return Natural is
   begin
      return Index + Natural (UTF8_Skip_Data (Str (Index)));
   end UTF8_Next_Char;

   -------------------------
   -- UTF8_Find_Next_Char --
   -------------------------

   function UTF8_Find_Next_Char
     (Str : UTF8_String; Index : Natural) return Natural
   is
      function Internal (Str, Str_End : System.Address) return System.Address;
      pragma Import (C, Internal, "g_utf8_find_next_char");

      Result : System.Address;
      First  : constant Natural := Str'First;

   begin
      Result := Internal
        (Str (First)'Address + Storage_Offset (Index - First),
         Str (First)'Address + Storage_Offset (Str'Length));

      if Result = System.Null_Address then
         return Str'Last + 1;
      else
         return Natural (Result - Str'Address + Storage_Offset (First));
      end if;
   end UTF8_Find_Next_Char;

   -------------------------
   -- UTF8_Find_Prev_Char --
   -------------------------

   function UTF8_Find_Prev_Char
     (Str : UTF8_String; Index : Natural) return Natural
   is
      function Internal (Start, Str : System.Address) return System.Address;
      pragma Import (C, Internal, "g_utf8_find_prev_char");

      Result : System.Address;
      First  : constant Natural := Str'First;

   begin
      Result := Internal
        (Str (First)'Address,
         Str (First)'Address + Storage_Offset (Index - First));

      if Result = System.Null_Address then
         return First - 1;
      else
         return Natural (Result - Str'Address + Storage_Offset (First));
      end if;
   end UTF8_Find_Prev_Char;

   ---------------------
   -- Unichar_To_UTF8 --
   ---------------------

   procedure Unichar_To_UTF8
     (C      : Gunichar;
      Buffer : out UTF8_String;
      Last   : out Natural)
   is
      function Internal (C : Gunichar; Buffer : System.Address) return Integer;
      pragma Import (C, Internal, "g_unichar_to_utf8");
   begin
      Last := Internal (C, Buffer (Buffer'First)'Address) + Buffer'First - 1;
   end Unichar_To_UTF8;

   -------------------
   -- UTF8_Get_Char --
   -------------------

   function UTF8_Get_Char (Str : UTF8_String) return Gunichar is
      function Internal (Str : System.Address) return Gunichar;
      pragma Import (C, Internal, "g_utf8_get_char");
   begin
      return Internal (Str'Address);
   end UTF8_Get_Char;

   -----------------------------
   -- UTF8_Get_Char_Validated --
   -----------------------------

   function UTF8_Get_Char_Validated (Str : UTF8_String) return Gunichar is
      function Internal (Str : System.Address) return Gunichar;
      pragma Import (C, Internal, "g_utf8_get_char_validated");
   begin
      return Internal (Str'Address);
   end UTF8_Get_Char_Validated;

   -----------------
   -- UTF8_Strlen --
   -----------------

   function UTF8_Strlen (Str : UTF8_String) return Glong is
      function Internal (Str : System.Address; Max : Integer) return Glong;
      pragma Import (C, Internal, "g_utf8_strlen");
   begin
      return Internal (Str'Address, Str'Length);
   end UTF8_Strlen;

end Glib.Unicode;
