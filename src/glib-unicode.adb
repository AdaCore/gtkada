-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2003 ACT-Europe                   --
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

with System.Storage_Elements; use System.Storage_Elements;

package body Glib.Unicode is

   procedure UTF8_Validate
     (Str         : String;
      Valid       : out Boolean;
      Invalid_Pos : out Natural)
   is
      function UTF8_Validate
        (Str         : String;
         Len         : Gsize;
         Invalid_Pos : access System.Address) return Gboolean;
      pragma Import (C, UTF8_Validate, "g_utf8_validate");

      Pos : aliased System.Address;

   begin
      Valid := To_Boolean (UTF8_Validate (Str, Str'Length, Pos'Access));

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
end Glib.Unicode;
