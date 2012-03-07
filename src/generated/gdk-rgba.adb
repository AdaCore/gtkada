------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

pragma Ada_05;
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Gtkada.Bindings;      use Gtkada.Bindings;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Gdk.RGBA is

   function Gdk_RGBA_Or_Null (Val : System.Address) return System.Address is
      function Internal is new Gtkada.Bindings.Generic_To_Address_Or_Null
        (Gdk_RGBA, Null_RGBA);
   begin
      return Internal (Val);
   end Gdk_RGBA_Or_Null;

   -----------
   -- Equal --
   -----------

   function Equal (Self : Gdk_RGBA; P2 : Gdk_RGBA) return Boolean is
      function Internal (Self : Gdk_RGBA; P2 : Gdk_RGBA) return Integer;
      pragma Import (C, Internal, "gdk_rgba_equal");
   begin
      return Boolean'Val (Internal (Self, P2));
   end Equal;

   -----------
   -- Parse --
   -----------

   function Parse (Self : Gdk_RGBA; Spec : UTF8_String) return Boolean is
      function Internal
         (Self : Gdk_RGBA;
          Spec : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gdk_rgba_parse");
      Tmp_Spec   : Interfaces.C.Strings.chars_ptr := New_String (Spec);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Self, Tmp_Spec);
      Free (Tmp_Spec);
      return Boolean'Val (Tmp_Return);
   end Parse;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : Gdk_RGBA) return UTF8_String is
      function Internal
         (Self : Gdk_RGBA) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gdk_rgba_to_string");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end To_String;

end Gdk.RGBA;
