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

package body Glib.Action is

   --------------
   -- Activate --
   --------------

   procedure Activate (Self : Gaction; Parameter : Glib.Variant.Gvariant) is
      procedure Internal (Self : Gaction; Parameter : System.Address);
      pragma Import (C, Internal, "g_action_activate");
   begin
      Internal (Self, Get_Object (Parameter));
   end Activate;

   ------------------
   -- Change_State --
   ------------------

   procedure Change_State (Self : Gaction; Value : Glib.Variant.Gvariant) is
      procedure Internal (Self : Gaction; Value : System.Address);
      pragma Import (C, Internal, "g_action_change_state");
   begin
      Internal (Self, Get_Object (Value));
   end Change_State;

   -----------------
   -- Get_Enabled --
   -----------------

   function Get_Enabled (Self : Gaction) return Boolean is
      function Internal (Self : Gaction) return Integer;
      pragma Import (C, Internal, "g_action_get_enabled");
   begin
      return Internal (Self) /= 0;
   end Get_Enabled;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Self : Gaction) return UTF8_String is
      function Internal
         (Self : Gaction) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_action_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Self));
   end Get_Name;

   ---------------
   -- Get_State --
   ---------------

   function Get_State (Self : Gaction) return Glib.Variant.Gvariant is
      function Internal (Self : Gaction) return System.Address;
      pragma Import (C, Internal, "g_action_get_state");
   begin
      return From_Object (Internal (Self));
   end Get_State;

   --------------------
   -- Get_State_Hint --
   --------------------

   function Get_State_Hint (Self : Gaction) return Glib.Variant.Gvariant is
      function Internal (Self : Gaction) return System.Address;
      pragma Import (C, Internal, "g_action_get_state_hint");
   begin
      return From_Object (Internal (Self));
   end Get_State_Hint;

   function "+" (W : Gaction) return Gaction is
   begin
      return W;
   end "+";

end Glib.Action;
