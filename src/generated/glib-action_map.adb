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

   ------------------------
   -- Add_Action_Entries --
   ------------------------

   procedure Add_Action_Entries
      (Self      : Gaction_Map;
       Entries   : GAction_Entry_Array;
       User_Data : System.Address := System.Null_Address)
   is
      procedure Internal
         (Self      : Gaction_Map;
          Entries   : GAction_Entry_Array;
          N_Entries : Glib.Gint;
          User_Data : System.Address);
      pragma Import (C, Internal, "g_action_map_add_action_entries");
   begin
      Internal (Self, Entries, Entries'Length, User_Data);
   end Add_Action_Entries;

   -------------------
   -- Lookup_Action --
   -------------------

   function Lookup_Action
      (Self        : Gaction_Map;
       Action_Name : UTF8_String) return Glib.Action.Gaction
   is
      function Internal
         (Self        : Gaction_Map;
          Action_Name : Gtkada.Types.Chars_Ptr) return Glib.Action.Gaction;
      pragma Import (C, Internal, "g_action_map_lookup_action");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
      Tmp_Return      : Glib.Action.Gaction;
   begin
      Tmp_Return := Internal (Self, Tmp_Action_Name);
      Free (Tmp_Action_Name);
      return Tmp_Return;
   end Lookup_Action;

   -------------------
   -- Remove_Action --
   -------------------

   procedure Remove_Action (Self : Gaction_Map; Action_Name : UTF8_String) is
      procedure Internal
         (Self        : Gaction_Map;
          Action_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_action_map_remove_action");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
   begin
      Internal (Self, Tmp_Action_Name);
      Free (Tmp_Action_Name);
   end Remove_Action;

   function "+" (W : Gaction_Map) return Gaction_Map is
   begin
      return W;
   end "+";

end Glib.Action_Map;
