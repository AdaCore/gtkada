
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


pragma Warnings (Off, "*is already use-visible*");
with Glib.Simple_Action; use Glib.Simple_Action;
with Glib.Variant;       use Glib.Variant;
with Gtkada.Types;       use Gtkada.Types;

package Glib.Action_Map is

   type GAction_Entry is private;
   function From_Object_Free (B : access GAction_Entry) return GAction_Entry;
   pragma Inline (From_Object_Free);
   --  This struct defines a single action. It is for use with
   --  g_action_map_add_action_entries.
   --
   --  The order of the items in the structure are intended to reflect
   --  frequency of use. It is permissible to use an incomplete initialiser in
   --  order to leave some of the later values as null. All values after Name
   --  are optional. Additional optional fields may be added in the future.
   --
   --  See g_action_map_add_action_entries for an example.

   type GAction_Entry_Array is array (Natural range <>) of GAction_Entry;

   ----------------------
   -- GtkAda additions --
   ----------------------

   type Activate_Callback is access procedure
     (Action    : access Glib.Simple_Action.Gsimple_Action;
      Parameter : Glib.Variant.Gvariant;
      Data      : System.Address);
   pragma Convention (C, Activate_Callback);

   type Change_State_Callback is access procedure
     (Action    : access Glib.Simple_Action.Gsimple_Action;
      Parameter : Glib.Variant.Gvariant;
      Data      : System.Address);
   pragma Convention (C, Change_State_Callback);

   function Build
     (Name           : String;
      Activate       : Activate_Callback := null;
      Parameter_Type : String := "";
      State          : String := "";
      Change_State   : Change_State_Callback := null)
   return GAction_Entry;
   --  Return a newly allocation action entry.
   --  This will be freed by the the action_map when needed.
   --
   --  Name is the name of the action.
   --  Activate is the callback to connect to the "activate" signal of
   --  the action.
   --  Parameter_Type is the type of the parameter that must be passed
   --  to the activate function for this action, given as a single
   --  Gvariant (or the empty string for no parameter).
   --  State is the initial state of this action, given in Gvariant
   --  text format. The state is parsed with no extra type information
   --  so type tags must be added to the string if they are necessary.
   --  Change_State is the callback for the "change-state" signal.

private
type GAction_Entry is record
   Name : Gtkada.Types.Chars_Ptr;
   Activate : System.Address;
   Parameter_Type : Gtkada.Types.Chars_Ptr;
   State : Gtkada.Types.Chars_Ptr;
   Change_State : System.Address;
   Padding : array_of_gsize (1 .. 3);
end record;
pragma Convention (C, GAction_Entry);

end Glib.Action_Map;
