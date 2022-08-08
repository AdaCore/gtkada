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

--  <description>
--  The GActionMap interface is implemented by Glib.Action_Group.Gaction_Group
--  implementations that operate by containing a number of named
--  Glib.Action.Gaction instances, such as
--  Glib.Simple_Action_Group.Gsimple_Action_Group.
--
--  One useful application of this interface is to map the names of actions
--  from various action groups to unique, prefixed names (e.g. by prepending
--  "app." or "win."). This is the motivation for the 'Map' part of the
--  interface name.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib.Action;        use Glib.Action;
with Glib.Object;        use Glib.Object;
with Glib.Simple_Action; use Glib.Simple_Action;
with Glib.Types;         use Glib.Types;
with Glib.Variant;       use Glib.Variant;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;       use Gtkada.Types;
pragma Warnings(On);

package Glib.Action_Map is

   type Gaction_Map is new Glib.Types.GType_Interface;
   Null_Gaction_Map : constant Gaction_Map;

   type GAction_Entry is private;
   function From_Object_Free (B : access GAction_Entry) return GAction_Entry;
   pragma Inline (From_Object_Free);
   --  This struct defines a single action. It is for use with
   --  Glib.Action_Map.Add_Action_Entries.
   --
   --  The order of the items in the structure are intended to reflect
   --  frequency of use. It is permissible to use an incomplete initialiser in
   --  order to leave some of the later values as null. All values after Name
   --  are optional. Additional optional fields may be added in the future.
   --
   --  See Glib.Action_Map.Add_Action_Entries for an example.

   type GAction_Entry_Array is array (Natural range <>) of GAction_Entry;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_action_map_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Action (Self : Gaction_Map; Action : Glib.Action.Gaction);
   pragma Import (C, Add_Action, "g_action_map_add_action");
   --  Adds an action to the Action_Map.
   --  If the action map already contains an action with the same name as
   --  Action then the old action is dropped from the action map.
   --  The action map takes its own reference on Action.
   --  Since: gtk+ 2.32
   --  "action": a Glib.Action.Gaction

   procedure Add_Action_Entries
      (Self      : Gaction_Map;
       Entries   : GAction_Entry_Array;
       User_Data : System.Address := System.Null_Address);
   --  A convenience function for creating multiple
   --  Glib.Simple_Action.Gsimple_Action instances and adding them to a
   --  Glib.Action_Map.Gaction_Map.
   --  Each action is constructed as per one Glib.Action_Map.GAction_Entry.
   --  |[<!-- language="C" --> static void activate_quit (GSimpleAction
   --  *simple, GVariant *parameter, gpointer user_data) { exit (0); }
   --  static void activate_print_string (GSimpleAction *simple, GVariant
   --  *parameter, gpointer user_data) { g_print ("%s\n", g_variant_get_string
   --  (parameter, NULL)); }
   --  static GActionGroup * create_action_group (void) { const GActionEntry
   --  entries[] = { { "quit", activate_quit }, { "print-string",
   --  activate_print_string, "s" } }; GSimpleActionGroup *group;
   --  group = g_simple_action_group_new (); g_action_map_add_action_entries
   --  (G_ACTION_MAP (group), entries, G_N_ELEMENTS (entries), NULL);
   --  return G_ACTION_GROUP (group); } ]|
   --  Since: gtk+ 2.32
   --  "entries": a pointer to the first item in an array of
   --  Glib.Action_Map.GAction_Entry structs
   --  "user_data": the user data for signal connections

   function Lookup_Action
      (Self        : Gaction_Map;
       Action_Name : UTF8_String) return Glib.Action.Gaction;
   --  Looks up the action with the name Action_Name in Action_Map.
   --  If no such action exists, returns null.
   --  Since: gtk+ 2.32
   --  "action_name": the name of an action

   procedure Remove_Action (Self : Gaction_Map; Action_Name : UTF8_String);
   --  Removes the named action from the action map.
   --  If no action of this name is in the map then nothing happens.
   --  Since: gtk+ 2.32
   --  "action_name": the name of the action

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

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gaction_Map"

   function "+" (W : Gaction_Map) return Gaction_Map;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Add_Action is access procedure (Self : Gaction_Map; Action : Glib.Action.Gaction);
   pragma Convention (C, Virtual_Add_Action);
   --  Adds an action to the Action_Map.
   --  If the action map already contains an action with the same name as
   --  Action then the old action is dropped from the action map.
   --  The action map takes its own reference on Action.
   --  Since: gtk+ 2.32
   --  "action": a Glib.Action.Gaction

   type Virtual_Lookup_Action is access function
     (Self        : Gaction_Map;
      Action_Name : Gtkada.Types.Chars_Ptr) return Glib.Action.Gaction;
   pragma Convention (C, Virtual_Lookup_Action);
   --  Looks up the action with the name Action_Name in Action_Map.
   --  If no such action exists, returns null.
   --  Since: gtk+ 2.32
   --  "action_name": the name of an action

   type Virtual_Remove_Action is access procedure (Self : Gaction_Map; Action_Name : Gtkada.Types.Chars_Ptr);
   pragma Convention (C, Virtual_Remove_Action);
   --  Removes the named action from the action map.
   --  If no action of this name is in the map then nothing happens.
   --  Since: gtk+ 2.32
   --  "action_name": the name of the action

   subtype Action_Map_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Add_Action
     (Self    : Action_Map_Interface_Descr;
      Handler : Virtual_Add_Action);
   pragma Import (C, Set_Add_Action, "gtkada_Action_Map_set_add_action");

   procedure Set_Lookup_Action
     (Self    : Action_Map_Interface_Descr;
      Handler : Virtual_Lookup_Action);
   pragma Import (C, Set_Lookup_Action, "gtkada_Action_Map_set_lookup_action");

   procedure Set_Remove_Action
     (Self    : Action_Map_Interface_Descr;
      Handler : Virtual_Remove_Action);
   pragma Import (C, Set_Remove_Action, "gtkada_Action_Map_set_remove_action");
   --  See Glib.Object.Add_Interface

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


Null_Gaction_Map : constant Gaction_Map :=
   Gaction_Map (Glib.Types.Null_Interface);
end Glib.Action_Map;
