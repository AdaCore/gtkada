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

package body Glib.Action_Map is

   function From_Object_Free (B : access GAction_Entry) return GAction_Entry is
      Result : constant GAction_Entry := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function Convert is new Ada.Unchecked_Conversion
     (Activate_Callback, System.Address);
   function Convert is new Ada.Unchecked_Conversion
     (Change_State_Callback, System.Address);

   function Build
     (Name           : String;
      Activate       : Activate_Callback := null;
      Parameter_Type : String := "";
      State          : String := "";
      Change_State   : Change_State_Callback := null)
   return GAction_Entry
   is
      P, S : chars_ptr := null_ptr;
   begin
      if Parameter_Type /= "" then
         P := New_String (Parameter_Type);
      end if;
      if State /= "" then
         S := New_String (State);
      end if;
      return GAction_Entry'
        (Name           => New_String (Name),
         Activate       => Convert (Activate),
         Parameter_Type => P,
         State          => S,
         Change_State   => Convert (Change_State),
         Padding        => <>);
   end Build;

end Glib.Action_Map;
