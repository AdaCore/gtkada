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
--  Glib.Simple_Action_Group.Gsimple_Action_Group is a hash table filled with
--  Glib.Action.Gaction objects, implementing the
--  Glib.Action_Group.Gaction_Group and Glib.Action_Map.Gaction_Map interfaces.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;      use GNAT.Strings;
with Glib.Action;       use Glib.Action;
with Glib.Action_Group; use Glib.Action_Group;
with Glib.Action_Map;   use Glib.Action_Map;
with Glib.Object;       use Glib.Object;
with Glib.Types;        use Glib.Types;
with Glib.Variant;      use Glib.Variant;

package Glib.Simple_Action_Group is

   type Gsimple_Action_Group_Record is new GObject_Record with null record;
   type Gsimple_Action_Group is access all Gsimple_Action_Group_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure G_New (Self : out Gsimple_Action_Group);
   --  Creates a new, empty, Glib.Simple_Action_Group.Gsimple_Action_Group.
   --  Since: gtk+ 2.28

   procedure Initialize
      (Self : not null access Gsimple_Action_Group_Record'Class);
   --  Creates a new, empty, Glib.Simple_Action_Group.Gsimple_Action_Group.
   --  Since: gtk+ 2.28
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gsimple_Action_Group_New return Gsimple_Action_Group;
   --  Creates a new, empty, Glib.Simple_Action_Group.Gsimple_Action_Group.
   --  Since: gtk+ 2.28

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_simple_action_group_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Entries
      (Self      : not null access Gsimple_Action_Group_Record;
       Entries   : Glib.Action_Map.GAction_Entry_Array;
       User_Data : System.Address := System.Null_Address);
   pragma Obsolescent (Add_Entries);
   --  A convenience function for creating multiple
   --  Glib.Simple_Action.Gsimple_Action instances and adding them to the
   --  action group.
   --  Since: gtk+ 2.30
   --  Deprecated since 2.38, 1
   --  "entries": a pointer to the first item in an array of
   --  Glib.Action_Map.GAction_Entry structs
   --  "user_data": the user data for signal connections

   procedure Insert
      (Self   : not null access Gsimple_Action_Group_Record;
       Action : Glib.Action.Gaction);
   pragma Obsolescent (Insert);
   --  Adds an action to the action group.
   --  If the action group already contains an action with the same name as
   --  Action then the old action is dropped from the group.
   --  The action group takes its own reference on Action.
   --  Since: gtk+ 2.28
   --  Deprecated since 2.38, 1
   --  "action": a Glib.Action.Gaction

   function Lookup
      (Self        : not null access Gsimple_Action_Group_Record;
       Action_Name : UTF8_String) return Glib.Action.Gaction;
   pragma Obsolescent (Lookup);
   --  Looks up the action with the name Action_Name in the group.
   --  If no such action exists, returns null.
   --  Since: gtk+ 2.28
   --  Deprecated since 2.38, 1
   --  "action_name": the name of an action

   procedure Remove
      (Self        : not null access Gsimple_Action_Group_Record;
       Action_Name : UTF8_String);
   pragma Obsolescent (Remove);
   --  Removes the named action from the action group.
   --  If no action of this name is in the group then nothing happens.
   --  Since: gtk+ 2.28
   --  Deprecated since 2.38, 1
   --  "action_name": the name of the action

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Action_Added
      (Self        : not null access Gsimple_Action_Group_Record;
       Action_Name : UTF8_String);

   procedure Action_Enabled_Changed
      (Self        : not null access Gsimple_Action_Group_Record;
       Action_Name : UTF8_String;
       Enabled     : Boolean);

   procedure Action_Removed
      (Self        : not null access Gsimple_Action_Group_Record;
       Action_Name : UTF8_String);

   procedure Action_State_Changed
      (Self        : not null access Gsimple_Action_Group_Record;
       Action_Name : UTF8_String;
       State       : Glib.Variant.Gvariant);

   procedure Activate_Action
      (Self        : not null access Gsimple_Action_Group_Record;
       Action_Name : UTF8_String;
       Parameter   : Glib.Variant.Gvariant);

   procedure Change_Action_State
      (Self        : not null access Gsimple_Action_Group_Record;
       Action_Name : UTF8_String;
       Value       : Glib.Variant.Gvariant);

   function Get_Action_Enabled
      (Self        : not null access Gsimple_Action_Group_Record;
       Action_Name : UTF8_String) return Boolean;

   function Get_Action_Parameter_Type
      (Self        : not null access Gsimple_Action_Group_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant_Type;

   function Get_Action_State
      (Self        : not null access Gsimple_Action_Group_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant;

   function Get_Action_State_Hint
      (Self        : not null access Gsimple_Action_Group_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant;

   function Get_Action_State_Type
      (Self        : not null access Gsimple_Action_Group_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant_Type;

   function Has_Action
      (Self        : not null access Gsimple_Action_Group_Record;
       Action_Name : UTF8_String) return Boolean;

   function List_Actions
      (Self : not null access Gsimple_Action_Group_Record)
       return GNAT.Strings.String_List;

   function Query_Action
      (Self           : not null access Gsimple_Action_Group_Record;
       Action_Name    : UTF8_String;
       Enabled        : access Boolean;
       Parameter_Type : access Glib.Variant.Gvariant_Type;
       State_Type     : access Glib.Variant.Gvariant_Type;
       State_Hint     : access Glib.Variant.Gvariant;
       State          : access Glib.Variant.Gvariant) return Boolean;

   procedure Add_Action
      (Self   : not null access Gsimple_Action_Group_Record;
       Action : Glib.Action.Gaction);

   procedure Add_Action_Entries
      (Self      : not null access Gsimple_Action_Group_Record;
       Entries   : GAction_Entry_Array;
       User_Data : System.Address := System.Null_Address);

   function Lookup_Action
      (Self        : not null access Gsimple_Action_Group_Record;
       Action_Name : UTF8_String) return Glib.Action.Gaction;

   procedure Remove_Action
      (Self        : not null access Gsimple_Action_Group_Record;
       Action_Name : UTF8_String);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "ActionGroup"
   --
   --  - "ActionMap"

   package Implements_Gaction_Group is new Glib.Types.Implements
     (Glib.Action_Group.Gaction_Group, Gsimple_Action_Group_Record, Gsimple_Action_Group);
   function "+"
     (Widget : access Gsimple_Action_Group_Record'Class)
   return Glib.Action_Group.Gaction_Group
   renames Implements_Gaction_Group.To_Interface;
   function "-"
     (Interf : Glib.Action_Group.Gaction_Group)
   return Gsimple_Action_Group
   renames Implements_Gaction_Group.To_Object;

   package Implements_Gaction_Map is new Glib.Types.Implements
     (Glib.Action_Map.Gaction_Map, Gsimple_Action_Group_Record, Gsimple_Action_Group);
   function "+"
     (Widget : access Gsimple_Action_Group_Record'Class)
   return Glib.Action_Map.Gaction_Map
   renames Implements_Gaction_Map.To_Interface;
   function "-"
     (Interf : Glib.Action_Map.Gaction_Map)
   return Gsimple_Action_Group
   renames Implements_Gaction_Map.To_Object;

end Glib.Simple_Action_Group;
