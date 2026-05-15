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
with Ada.Unchecked_Conversion;
with Gtkada.Types;

package body Gtk.Enums is

   function Convert (S : String) return System.Address is
      function Internal is new
      Ada.Unchecked_Conversion (Gtkada.Types.Chars_Ptr, System.Address);
   begin
      return Internal (Gtkada.Types.New_String (S));
   end Convert;

   function Convert_Chars_Ptr is new
   Ada.Unchecked_Conversion (System.Address, Gtkada.Types.Chars_Ptr);

   function Convert (S : System.Address) return String is
   begin
      return Gtkada.Types.Value (Convert_Chars_Ptr (S));
   end Convert;

   procedure Free_String_List (List : in out String_List.Glist) is
      use type String_List.Glist;
      Tmp   : String_List.Glist := List;
      Chars : Gtkada.Types.Chars_Ptr;
   begin
      while Tmp /= String_List.Null_List loop
         Chars := Convert_Chars_Ptr (String_List.Get_Data_Address (Tmp));
         Gtkada.Types.g_free (Chars);
         Tmp := String_List.Next (Tmp);
      end loop;

      String_List.Free (List);
      List := String_List.Null_List;
   end Free_String_List;

   procedure Free_String_List (List : in out String_SList.GSlist) is
      use type String_SList.GSlist;

      Tmp   : String_SList.GSlist := List;
      Chars : Gtkada.Types.Chars_Ptr;
   begin
      while Tmp /= String_SList.Null_List loop
         Chars := Convert_Chars_Ptr (String_SList.Get_Data_Address (Tmp));
         Gtkada.Types.g_free (Chars);
         Tmp := String_SList.Next (Tmp);
      end loop;

      String_SList.Free (List);
      List := String_SList.Null_List;
   end Free_String_List;

end Gtk.Enums;
