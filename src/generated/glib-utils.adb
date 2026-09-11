------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings; use Gtkada.Bindings;
with Gtkada.Types;    use Gtkada.Types;
pragma Warnings(On);

package body Glib.Utils is

   -------------------
   -- Check_Version --
   -------------------

   function Check_Version
      (Required_Major : Guint;
       Required_Minor : Guint;
       Required_Micro : Guint) return UTF8_String
   is
      function Internal
         (Required_Major : Guint;
          Required_Minor : Guint;
          Required_Micro : Guint) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "glib_check_version");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Required_Major, Required_Minor, Required_Micro));
   end Check_Version;

   ------------------
   -- Get_Home_Dir --
   ------------------

   function Get_Home_Dir return UTF8_String is
      function Internal return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_get_home_dir");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal);
   end Get_Home_Dir;

end Glib.Utils;
