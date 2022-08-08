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
--  A Gtk.Radio_Action.Gtk_Radio_Action is similar to
--  Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item. A number of radio actions can be
--  linked together so that only one may be active at any one time.
--
--  </description>
--  <group>Action-based menus</group>
--  <see>Gtk_Action</see>

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Glib.Properties;   use Glib.Properties;
with Glib.Types;        use Glib.Types;
with Gtk.Buildable;     use Gtk.Buildable;
with Gtk.Toggle_Action; use Gtk.Toggle_Action;
with Gtk.Widget;        use Gtk.Widget;

package Gtk.Radio_Action is

   type Gtk_Radio_Action_Record is new Gtk_Toggle_Action_Record with null record;
   type Gtk_Radio_Action is access all Gtk_Radio_Action_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Action   : out Gtk_Radio_Action;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "";
       Value    : Glib.Gint);
   procedure Initialize
      (Action   : not null access Gtk_Radio_Action_Record'Class;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "";
       Value    : Glib.Gint);
   --  Creates a new Gtk.Radio_Action.Gtk_Radio_Action object. To add the
   --  action to a Gtk.Action_Group.Gtk_Action_Group and set the accelerator
   --  for the action, call Gtk.Action_Group.Add_Action_With_Accel.
   --  Since: gtk+ 2.4
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "name": A unique name for the action
   --  "label": The label displayed in menu items and on buttons, or null
   --  "tooltip": A tooltip for this action, or null
   --  "stock_id": The stock icon to display in widgets representing this
   --  action, or null
   --  "value": The value which Gtk.Radio_Action.Get_Current_Value should
   --  return if this action is selected.

   function Gtk_Radio_Action_New
      (Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "";
       Value    : Glib.Gint) return Gtk_Radio_Action;
   --  Creates a new Gtk.Radio_Action.Gtk_Radio_Action object. To add the
   --  action to a Gtk.Action_Group.Gtk_Action_Group and set the accelerator
   --  for the action, call Gtk.Action_Group.Add_Action_With_Accel.
   --  Since: gtk+ 2.4
   --  "name": A unique name for the action
   --  "label": The label displayed in menu items and on buttons, or null
   --  "tooltip": A tooltip for this action, or null
   --  "stock_id": The stock icon to display in widgets representing this
   --  action, or null
   --  "value": The value which Gtk.Radio_Action.Get_Current_Value should
   --  return if this action is selected.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_radio_action_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Current_Value
      (Action : not null access Gtk_Radio_Action_Record) return Glib.Gint;
   pragma Obsolescent (Get_Current_Value);
   --  Obtains the value property of the currently active member of the group
   --  to which Action belongs.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   procedure Set_Current_Value
      (Action        : not null access Gtk_Radio_Action_Record;
       Current_Value : Glib.Gint);
   pragma Obsolescent (Set_Current_Value);
   --  Sets the currently active group member to the member with value
   --  property Current_Value.
   --  Since: gtk+ 2.10
   --  Deprecated since 3.10, 1
   --  "current_value": the new value

   function Get_Group
      (Action : not null access Gtk_Radio_Action_Record)
       return Gtk.Widget.Widget_SList.GSlist;
   pragma Obsolescent (Get_Group);
   --  Returns the list representing the radio group for this object. Note
   --  that the returned list is only valid until the next change to the group.
   --  A common way to set up a group of radio group is the following: |[<!--
   --  language="C" --> GSList *group = NULL; GtkRadioAction *action; while (
   --  ...more actions to add... /) { action = gtk_radio_action_new (...);
   --  gtk_radio_action_set_group (action, group); group =
   --  gtk_radio_action_get_group (action); } ]|
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   procedure Set_Group
      (Action : not null access Gtk_Radio_Action_Record;
       Group  : Gtk.Widget.Widget_SList.GSlist);
   pragma Obsolescent (Set_Group);
   --  Sets the radio group for the radio action object.
   --  A common way to set up a group of radio group is the following:
   --       Group  : GSlist := null;
   --       Action : Gtk_Radio_Action;
   --       while ... loop
   --          Gtk_New (Action, ...);
   --          Set_Group (Action, Group);
   --          Group := Get_Group (Action);
   --       end loop;
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "group": a list representing a radio group, or null

   procedure Join_Group
      (Action       : not null access Gtk_Radio_Action_Record;
       Group_Source : access Gtk_Radio_Action_Record'Class);
   pragma Obsolescent (Join_Group);
   --  Joins a radio action object to the group of another radio action
   --  object.
   --  Use this in language bindings instead of the Gtk.Radio_Action.Get_Group
   --  and Gtk.Radio_Action.Set_Group methods
   --  A common way to set up a group of radio actions is the following:
   --  |[<!-- language="C" --> GtkRadioAction *action; GtkRadioAction
   --  *last_action; while ( ...more actions to add... /) { action =
   --  gtk_radio_action_new (...); gtk_radio_action_join_group (action,
   --  last_action); last_action = action; } ]|
   --  Since: gtk+ 3.0
   --  Deprecated since 3.10, 1
   --  "group_source": a radio action object whos group we are joining, or
   --  null to remove the radio action from its group

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Current_Value_Property : constant Glib.Properties.Property_Int;
   --  The value property of the currently active member of the group to which
   --  this action belongs.

   Group_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk_Radio_Action
   --  Flags: write
   --  Sets a new group for a radio action.

   Value_Property : constant Glib.Properties.Property_Int;
   --  The value is an arbitrary integer which can be used as a convenient way
   --  to determine which action in the group is currently active in an
   --  ::activate or ::changed signal handler. See
   --  Gtk.Radio_Action.Get_Current_Value and Gtk_Radio_Action_Entry for
   --  convenient ways to get and set this property.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Radio_Action_Gtk_Radio_Action_Void is not null access procedure
     (Self    : access Gtk_Radio_Action_Record'Class;
      Current : not null access Gtk_Radio_Action_Record'Class);

   type Cb_GObject_Gtk_Radio_Action_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      Current : not null access Gtk_Radio_Action_Record'Class);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : not null access Gtk_Radio_Action_Record;
       Call  : Cb_Gtk_Radio_Action_Gtk_Radio_Action_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : not null access Gtk_Radio_Action_Record;
       Call  : Cb_GObject_Gtk_Radio_Action_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::changed signal is emitted on every member of a radio group when
   --  the active member is changed. The signal gets emitted after the
   --  ::activate signals for the previous and current active members.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Radio_Action_Record, Gtk_Radio_Action);
   function "+"
     (Widget : access Gtk_Radio_Action_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Radio_Action
   renames Implements_Gtk_Buildable.To_Object;

private
   Value_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("value");
   Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("group");
   Current_Value_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("current-value");
end Gtk.Radio_Action;
