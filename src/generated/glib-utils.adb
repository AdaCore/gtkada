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
with Gtkada.Types; use Gtkada.Types;

package body Glib.Utils is

   ------------------
   -- Get_Home_Dir --
   ------------------

   function Get_Home_Dir return UTF8_String
   is
      function Internal return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "glib_get_home_dir");

      Tmp    : constant Gtkada.Types.Chars_Ptr := Internal;
      Result : constant String := Gtkada.Types.Value (Tmp);
   begin
      Gtkada.Types.g_free (Tmp);
      return Result;
   end Get_Home_Dir;

end Glib.Utils;
