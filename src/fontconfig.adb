------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2015-2018, AdaCore                     --
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

with System;               use System;
with Gtkada.Types;         use Gtkada.Types;
with Interfaces.C;         use Interfaces.C;

package body Fontconfig is

   -----------------------
   -- App_Font_Add_File --
   -----------------------

   function App_Font_Add_File (Filename : String) return Boolean is
      function Add_Font_File
        (FC_Config : System.Address;
         File : Gtkada.Types.Chars_Ptr) return Interfaces.C.int;
      pragma Import (C, Add_Font_File, "FcConfigAppFontAddFile");

      Path   : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Result : Boolean;
   begin
      Result := Add_Font_File (System.Null_Address, Path) /= 0;
      Free (Path);
      return Result;
   end App_Font_Add_File;

end Fontconfig;
