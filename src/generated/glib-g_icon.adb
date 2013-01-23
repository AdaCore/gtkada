------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2013, AdaCore                     --
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
with Gtkada.Bindings;      use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Interfaces.C.Strings; use Interfaces.C.Strings;
pragma Warnings(On);

package body Glib.G_Icon is

   -----------
   -- Equal --
   -----------

   function Equal (Self : G_Icon; Icon2 : G_Icon) return Boolean is
      function Internal (Self : G_Icon; Icon2 : G_Icon) return Integer;
      pragma Import (C, Internal, "g_icon_equal");
   begin
      return Internal (Self, Icon2) /= 0;
   end Equal;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : G_Icon) return UTF8_String is
      function Internal
         (Self : G_Icon) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_icon_to_string");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end To_String;

   --------------------
   -- New_For_String --
   --------------------

   function New_For_String (Str : UTF8_String) return G_Icon is
      function Internal (Str : Interfaces.C.Strings.chars_ptr) return G_Icon;
      pragma Import (C, Internal, "g_icon_new_for_string");
      Tmp_Str    : Interfaces.C.Strings.chars_ptr := New_String (Str);
      Tmp_Return : G_Icon;
   begin
      Tmp_Return := Internal (Tmp_Str);
      Free (Tmp_Str);
      return Tmp_Return;
   end New_For_String;

   function "+" (W : G_Icon) return G_Icon is
   begin
      return W;
   end "+";

end Glib.G_Icon;
