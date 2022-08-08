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
--  Glib.Action_Group.Gaction_Group represents a group of actions. Actions can
--  be used to expose functionality in a structured way, either from one part
--  of a program to another, or to the outside world. Action groups are often
--  used together with a Glib.Menu_Model.Gmenu_Model that provides additional
--  representation data for displaying the actions to the user, e.g. in a menu.
--
--  The main way to interact with the actions in a GActionGroup is to activate
--  them with Glib.Action_Group.Activate_Action. Activating an action may
--  require a Glib.Variant.Gvariant parameter. The required type of the
--  parameter can be inquired with Glib.Action_Group.Get_Action_Parameter_Type.
--  Actions may be disabled, see Glib.Action_Group.Get_Action_Enabled.
--  Activating a disabled action has no effect.
--
--  Actions may optionally have a state in the form of a
--  Glib.Variant.Gvariant. The current state of an action can be inquired with
--  Glib.Action_Group.Get_Action_State. Activating a stateful action may change
--  its state, but it is also possible to set the state by calling
--  Glib.Action_Group.Change_Action_State.
--
--  As typical example, consider a text editing application which has an
--  option to change the current font to 'bold'. A good way to represent this
--  would be a stateful action, with a boolean state. Activating the action
--  would toggle the state.
--
--  Each action in the group has a unique name (which is a string). All method
--  calls, except Glib.Action_Group.List_Actions take the name of an action as
--  an argument.
--
--  The Glib.Action_Group.Gaction_Group API is meant to be the 'public' API to
--  the action group. The calls here are exactly the interaction that 'external
--  forces' (eg: UI, incoming D-Bus messages, etc.) are supposed to have with
--  actions. 'Internal' APIs (ie: ones meant only to be accessed by the action
--  group implementation) are found on subclasses. This is why you will find -
--  for example - Glib.Action_Group.Get_Action_Enabled but not an equivalent
--  set call.
--
--  Signals are emitted on the action group in response to state changes on
--  individual actions.
--
--  Implementations of Glib.Action_Group.Gaction_Group should provide
--  implementations for the virtual functions Glib.Action_Group.List_Actions
--  and Glib.Action_Group.Query_Action. The other virtual functions should not
--  be implemented - their "wrappers" are actually implemented with calls to
--  Glib.Action_Group.Query_Action.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;    use GNAT.Strings;
with Glib.Object;     use Glib.Object;
with Glib.Types;      use Glib.Types;
with Glib.Variant;    use Glib.Variant;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings; use Gtkada.Bindings;
with Gtkada.Types;    use Gtkada.Types;
pragma Warnings(On);

package Glib.Action_Group is

   type Gaction_Group is new Glib.Types.GType_Interface;
   Null_Gaction_Group : constant Gaction_Group;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_action_group_get_type");

   -------------
   -- Methods --
   -------------

   procedure Action_Added (Self : Gaction_Group; Action_Name : UTF8_String);
   --  Emits the Glib.Action_Group.Gaction_Group::action-added signal on
   --  Action_Group.
   --  This function should only be called by Glib.Action_Group.Gaction_Group
   --  implementations.
   --  Since: gtk+ 2.28
   --  "action_name": the name of an action in the group

   procedure Action_Enabled_Changed
      (Self        : Gaction_Group;
       Action_Name : UTF8_String;
       Enabled     : Boolean);
   --  Emits the Glib.Action_Group.Gaction_Group::action-enabled-changed
   --  signal on Action_Group.
   --  This function should only be called by Glib.Action_Group.Gaction_Group
   --  implementations.
   --  Since: gtk+ 2.28
   --  "action_name": the name of an action in the group
   --  "enabled": whether or not the action is now enabled

   procedure Action_Removed
      (Self        : Gaction_Group;
       Action_Name : UTF8_String);
   --  Emits the Glib.Action_Group.Gaction_Group::action-removed signal on
   --  Action_Group.
   --  This function should only be called by Glib.Action_Group.Gaction_Group
   --  implementations.
   --  Since: gtk+ 2.28
   --  "action_name": the name of an action in the group

   procedure Action_State_Changed
      (Self        : Gaction_Group;
       Action_Name : UTF8_String;
       State       : Glib.Variant.Gvariant);
   --  Emits the Glib.Action_Group.Gaction_Group::action-state-changed signal
   --  on Action_Group.
   --  This function should only be called by Glib.Action_Group.Gaction_Group
   --  implementations.
   --  Since: gtk+ 2.28
   --  "action_name": the name of an action in the group
   --  "state": the new state of the named action

   procedure Activate_Action
      (Self        : Gaction_Group;
       Action_Name : UTF8_String;
       Parameter   : Glib.Variant.Gvariant);
   --  Activate the named action within Action_Group.
   --  If the action is expecting a parameter, then the correct type of
   --  parameter must be given as Parameter. If the action is expecting no
   --  parameters then Parameter must be null. See
   --  Glib.Action_Group.Get_Action_Parameter_Type.
   --  Since: gtk+ 2.28
   --  "action_name": the name of the action to activate
   --  "parameter": parameters to the activation

   procedure Change_Action_State
      (Self        : Gaction_Group;
       Action_Name : UTF8_String;
       Value       : Glib.Variant.Gvariant);
   --  Request for the state of the named action within Action_Group to be
   --  changed to Value.
   --  The action must be stateful and Value must be of the correct type. See
   --  Glib.Action_Group.Get_Action_State_Type.
   --  This call merely requests a change. The action may refuse to change its
   --  state or may change its state to something other than Value. See
   --  Glib.Action_Group.Get_Action_State_Hint.
   --  If the Value GVariant is floating, it is consumed.
   --  Since: gtk+ 2.28
   --  "action_name": the name of the action to request the change on
   --  "value": the new state

   function Get_Action_Enabled
      (Self        : Gaction_Group;
       Action_Name : UTF8_String) return Boolean;
   --  Checks if the named action within Action_Group is currently enabled.
   --  An action must be enabled in order to be activated or in order to have
   --  its state changed from outside callers.
   --  Since: gtk+ 2.28
   --  "action_name": the name of the action to query

   function Get_Action_Parameter_Type
      (Self        : Gaction_Group;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant_Type;
   --  Queries the type of the parameter that must be given when activating
   --  the named action within Action_Group.
   --  When activating the action using Glib.Action_Group.Activate_Action, the
   --  Glib.Variant.Gvariant given to that function must be of the type
   --  returned by this function.
   --  In the case that this function returns null, you must not give any
   --  Glib.Variant.Gvariant, but null instead.
   --  The parameter type of a particular action will never change but it is
   --  possible for an action to be removed and for a new action to be added
   --  with the same name but a different parameter type.
   --  Since: gtk+ 2.28
   --  "action_name": the name of the action to query

   function Get_Action_State
      (Self        : Gaction_Group;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant;
   --  Queries the current state of the named action within Action_Group.
   --  If the action is not stateful then null will be returned. If the action
   --  is stateful then the type of the return value is the type given by
   --  Glib.Action_Group.Get_Action_State_Type.
   --  The return value (if non-null) should be freed with Glib.Variant.Unref
   --  when it is no longer required.
   --  Since: gtk+ 2.28
   --  "action_name": the name of the action to query

   function Get_Action_State_Hint
      (Self        : Gaction_Group;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant;
   --  Requests a hint about the valid range of values for the state of the
   --  named action within Action_Group.
   --  If null is returned it either means that the action is not stateful or
   --  that there is no hint about the valid range of values for the state of
   --  the action.
   --  If a Glib.Variant.Gvariant array is returned then each item in the
   --  array is a possible value for the state. If a Glib.Variant.Gvariant pair
   --  (ie: two-tuple) is returned then the tuple specifies the inclusive lower
   --  and upper bound of valid values for the state.
   --  In any case, the information is merely a hint. It may be possible to
   --  have a state value outside of the hinted range and setting a value
   --  within the range may fail.
   --  The return value (if non-null) should be freed with Glib.Variant.Unref
   --  when it is no longer required.
   --  Since: gtk+ 2.28
   --  "action_name": the name of the action to query

   function Get_Action_State_Type
      (Self        : Gaction_Group;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant_Type;
   --  Queries the type of the state of the named action within Action_Group.
   --  If the action is stateful then this function returns the
   --  Glib.Variant.Gvariant_Type of the state. All calls to
   --  Glib.Action_Group.Change_Action_State must give a Glib.Variant.Gvariant
   --  of this type and Glib.Action_Group.Get_Action_State will return a
   --  Glib.Variant.Gvariant of the same type.
   --  If the action is not stateful then this function will return null. In
   --  that case, Glib.Action_Group.Get_Action_State will return null and you
   --  must not call Glib.Action_Group.Change_Action_State.
   --  The state type of a particular action will never change but it is
   --  possible for an action to be removed and for a new action to be added
   --  with the same name but a different state type.
   --  Since: gtk+ 2.28
   --  "action_name": the name of the action to query

   function Has_Action
      (Self        : Gaction_Group;
       Action_Name : UTF8_String) return Boolean;
   --  Checks if the named action exists within Action_Group.
   --  Since: gtk+ 2.28
   --  "action_name": the name of the action to check for

   function List_Actions
      (Self : Gaction_Group) return GNAT.Strings.String_List;
   --  Lists the actions contained within Action_Group.
   --  The caller is responsible for freeing the list with g_strfreev when it
   --  is no longer required.
   --  Since: gtk+ 2.28

   function Query_Action
      (Self           : Gaction_Group;
       Action_Name    : UTF8_String;
       Enabled        : access Boolean;
       Parameter_Type : access Glib.Variant.Gvariant_Type;
       State_Type     : access Glib.Variant.Gvariant_Type;
       State_Hint     : access Glib.Variant.Gvariant;
       State          : access Glib.Variant.Gvariant) return Boolean;
   --  Queries all aspects of the named action within an Action_Group.
   --  This function acquires the information available from
   --  Glib.Action_Group.Has_Action, Glib.Action_Group.Get_Action_Enabled,
   --  Glib.Action_Group.Get_Action_Parameter_Type,
   --  Glib.Action_Group.Get_Action_State_Type,
   --  Glib.Action_Group.Get_Action_State_Hint and
   --  Glib.Action_Group.Get_Action_State with a single function call.
   --  This provides two main benefits.
   --  The first is the improvement in efficiency that comes with not having
   --  to perform repeated lookups of the action in order to discover different
   --  things about it. The second is that implementing
   --  Glib.Action_Group.Gaction_Group can now be done by only overriding this
   --  one virtual function.
   --  The interface provides a default implementation of this function that
   --  calls the individual functions, as required, to fetch the information.
   --  The interface also provides default implementations of those functions
   --  that call this function. All implementations, therefore, must override
   --  either this function or all of the others.
   --  If the action exists, True is returned and any of the requested fields
   --  (as indicated by having a non-null reference passed in) are filled. If
   --  the action doesn't exist, False is returned and the fields may or may
   --  not have been modified.
   --  Since: gtk+ 2.32
   --  "action_name": the name of an action in the group
   --  "enabled": if the action is presently enabled
   --  "parameter_type": the parameter type, or null if none needed
   --  "state_type": the state type, or null if stateless
   --  "state_hint": the state hint, or null if none
   --  "state": the current state, or null if stateless

   -------------
   -- Signals --
   -------------

   type Cb_Gaction_Group_UTF8_String_Void is not null access procedure
     (Self        : Gaction_Group;
      Action_Name : UTF8_String);

   type Cb_GObject_UTF8_String_Void is not null access procedure
     (Self        : access Glib.Object.GObject_Record'Class;
      Action_Name : UTF8_String);

   Signal_Action_Added : constant Glib.Signal_Name := "action-added";
   procedure On_Action_Added
      (Self  : Gaction_Group;
       Call  : Cb_Gaction_Group_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Action_Added
      (Self  : Gaction_Group;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Signals that a new action was just added to the group. This signal is
   --  emitted after the action has been added and is now visible.

   type Cb_Gaction_Group_UTF8_String_Boolean_Void is not null access procedure
     (Self        : Gaction_Group;
      Action_Name : UTF8_String;
      Enabled     : Boolean);

   type Cb_GObject_UTF8_String_Boolean_Void is not null access procedure
     (Self        : access Glib.Object.GObject_Record'Class;
      Action_Name : UTF8_String;
      Enabled     : Boolean);

   Signal_Action_Enabled_Changed : constant Glib.Signal_Name := "action-enabled-changed";
   procedure On_Action_Enabled_Changed
      (Self  : Gaction_Group;
       Call  : Cb_Gaction_Group_UTF8_String_Boolean_Void;
       After : Boolean := False);
   procedure On_Action_Enabled_Changed
      (Self  : Gaction_Group;
       Call  : Cb_GObject_UTF8_String_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Signals that the enabled status of the named action has changed.
   -- 
   --  Callback parameters:
   --    --  "action_name": the name of the action in Action_Group
   --    --  "enabled": whether the action is enabled or not

   Signal_Action_Removed : constant Glib.Signal_Name := "action-removed";
   procedure On_Action_Removed
      (Self  : Gaction_Group;
       Call  : Cb_Gaction_Group_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Action_Removed
      (Self  : Gaction_Group;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Signals that an action is just about to be removed from the group. This
   --  signal is emitted before the action is removed, so the action is still
   --  visible and can be queried from the signal handler.

   type Cb_Gaction_Group_UTF8_String_Gvariant_Void is not null access procedure
     (Self        : Gaction_Group;
      Action_Name : UTF8_String;
      Value       : Glib.Variant.Gvariant);

   type Cb_GObject_UTF8_String_Gvariant_Void is not null access procedure
     (Self        : access Glib.Object.GObject_Record'Class;
      Action_Name : UTF8_String;
      Value       : Glib.Variant.Gvariant);

   Signal_Action_State_Changed : constant Glib.Signal_Name := "action-state-changed";
   procedure On_Action_State_Changed
      (Self  : Gaction_Group;
       Call  : Cb_Gaction_Group_UTF8_String_Gvariant_Void;
       After : Boolean := False);
   procedure On_Action_State_Changed
      (Self  : Gaction_Group;
       Call  : Cb_GObject_UTF8_String_Gvariant_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Signals that the state of the named action has changed.
   -- 
   --  Callback parameters:
   --    --  "action_name": the name of the action in Action_Group
   --    --  "value": the new value of the state

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gaction_Group"

   function "+" (W : Gaction_Group) return Gaction_Group;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Action_Added is access procedure
     (Self        : Gaction_Group;
      Action_Name : Gtkada.Types.Chars_Ptr);
   pragma Convention (C, Virtual_Action_Added);
   --  Emits the Glib.Action_Group.Gaction_Group::action-added signal on
   --  Action_Group.
   --  This function should only be called by Glib.Action_Group.Gaction_Group
   --  implementations.
   --  Since: gtk+ 2.28
   --  "action_name": the name of an action in the group

   type Virtual_Action_Enabled_Changed is access procedure
     (Self        : Gaction_Group;
      Action_Name : Gtkada.Types.Chars_Ptr;
      Enabled     : Glib.Gboolean);
   pragma Convention (C, Virtual_Action_Enabled_Changed);
   --  Emits the Glib.Action_Group.Gaction_Group::action-enabled-changed
   --  signal on Action_Group.
   --  This function should only be called by Glib.Action_Group.Gaction_Group
   --  implementations.
   --  Since: gtk+ 2.28
   --  "action_name": the name of an action in the group
   --  "enabled": whether or not the action is now enabled

   type Virtual_Action_Removed is access procedure
     (Self        : Gaction_Group;
      Action_Name : Gtkada.Types.Chars_Ptr);
   pragma Convention (C, Virtual_Action_Removed);
   --  Emits the Glib.Action_Group.Gaction_Group::action-removed signal on
   --  Action_Group.
   --  This function should only be called by Glib.Action_Group.Gaction_Group
   --  implementations.
   --  Since: gtk+ 2.28
   --  "action_name": the name of an action in the group

   type Virtual_Action_State_Changed is access procedure
     (Self        : Gaction_Group;
      Action_Name : Gtkada.Types.Chars_Ptr;
      State       : System.Address);
   pragma Convention (C, Virtual_Action_State_Changed);
   --  Emits the Glib.Action_Group.Gaction_Group::action-state-changed signal
   --  on Action_Group.
   --  This function should only be called by Glib.Action_Group.Gaction_Group
   --  implementations.
   --  Since: gtk+ 2.28
   --  "action_name": the name of an action in the group
   --  "state": the new state of the named action

   type Virtual_Activate_Action is access procedure
     (Self        : Gaction_Group;
      Action_Name : Gtkada.Types.Chars_Ptr;
      Parameter   : System.Address);
   pragma Convention (C, Virtual_Activate_Action);
   --  Activate the named action within Action_Group.
   --  If the action is expecting a parameter, then the correct type of
   --  parameter must be given as Parameter. If the action is expecting no
   --  parameters then Parameter must be null. See
   --  Glib.Action_Group.Get_Action_Parameter_Type.
   --  Since: gtk+ 2.28
   --  "action_name": the name of the action to activate
   --  "parameter": parameters to the activation

   type Virtual_Change_Action_State is access procedure
     (Self        : Gaction_Group;
      Action_Name : Gtkada.Types.Chars_Ptr;
      Value       : System.Address);
   pragma Convention (C, Virtual_Change_Action_State);
   --  Request for the state of the named action within Action_Group to be
   --  changed to Value.
   --  The action must be stateful and Value must be of the correct type. See
   --  Glib.Action_Group.Get_Action_State_Type.
   --  This call merely requests a change. The action may refuse to change its
   --  state or may change its state to something other than Value. See
   --  Glib.Action_Group.Get_Action_State_Hint.
   --  If the Value GVariant is floating, it is consumed.
   --  Since: gtk+ 2.28
   --  "action_name": the name of the action to request the change on
   --  "value": the new state

   type Virtual_Get_Action_Enabled is access function
     (Self        : Gaction_Group;
      Action_Name : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
   pragma Convention (C, Virtual_Get_Action_Enabled);
   --  Checks if the named action within Action_Group is currently enabled.
   --  An action must be enabled in order to be activated or in order to have
   --  its state changed from outside callers.
   --  Since: gtk+ 2.28
   --  "action_name": the name of the action to query

   type Virtual_Get_Action_Parameter_Type is access function
     (Self        : Gaction_Group;
      Action_Name : Gtkada.Types.Chars_Ptr)
   return Glib.Variant.Gvariant_Type;
   pragma Convention (C, Virtual_Get_Action_Parameter_Type);
   --  Queries the type of the parameter that must be given when activating
   --  the named action within Action_Group.
   --  When activating the action using Glib.Action_Group.Activate_Action, the
   --  Glib.Variant.Gvariant given to that function must be of the type
   --  returned by this function.
   --  In the case that this function returns null, you must not give any
   --  Glib.Variant.Gvariant, but null instead.
   --  The parameter type of a particular action will never change but it is
   --  possible for an action to be removed and for a new action to be added
   --  with the same name but a different parameter type.
   --  Since: gtk+ 2.28
   --  "action_name": the name of the action to query

   type Virtual_Get_Action_State is access function
     (Self        : Gaction_Group;
      Action_Name : Gtkada.Types.Chars_Ptr) return System.Address;
   pragma Convention (C, Virtual_Get_Action_State);
   --  Queries the current state of the named action within Action_Group.
   --  If the action is not stateful then null will be returned. If the action
   --  is stateful then the type of the return value is the type given by
   --  Glib.Action_Group.Get_Action_State_Type.
   --  The return value (if non-null) should be freed with g_variant_unref
   --  when it is no longer required.
   --  Since: gtk+ 2.28
   --  "action_name": the name of the action to query

   type Virtual_Get_Action_State_Hint is access function
     (Self        : Gaction_Group;
      Action_Name : Gtkada.Types.Chars_Ptr) return System.Address;
   pragma Convention (C, Virtual_Get_Action_State_Hint);
   --  Requests a hint about the valid range of values for the state of the
   --  named action within Action_Group.
   --  If null is returned it either means that the action is not stateful or
   --  that there is no hint about the valid range of values for the state of
   --  the action.
   --  If a Glib.Variant.Gvariant array is returned then each item in the
   --  array is a possible value for the state. If a Glib.Variant.Gvariant pair
   --  (ie: two-tuple) is returned then the tuple specifies the inclusive lower
   --  and upper bound of valid values for the state.
   --  In any case, the information is merely a hint. It may be possible to
   --  have a state value outside of the hinted range and setting a value
   --  within the range may fail.
   --  The return value (if non-null) should be freed with g_variant_unref
   --  when it is no longer required.
   --  Since: gtk+ 2.28
   --  "action_name": the name of the action to query

   type Virtual_Get_Action_State_Type is access function
     (Self        : Gaction_Group;
      Action_Name : Gtkada.Types.Chars_Ptr)
   return Glib.Variant.Gvariant_Type;
   pragma Convention (C, Virtual_Get_Action_State_Type);
   --  Queries the type of the state of the named action within Action_Group.
   --  If the action is stateful then this function returns the
   --  Glib.Variant.Gvariant_Type of the state. All calls to
   --  Glib.Action_Group.Change_Action_State must give a Glib.Variant.Gvariant
   --  of this type and Glib.Action_Group.Get_Action_State will return a
   --  Glib.Variant.Gvariant of the same type.
   --  If the action is not stateful then this function will return null. In
   --  that case, Glib.Action_Group.Get_Action_State will return null and you
   --  must not call Glib.Action_Group.Change_Action_State.
   --  The state type of a particular action will never change but it is
   --  possible for an action to be removed and for a new action to be added
   --  with the same name but a different state type.
   --  Since: gtk+ 2.28
   --  "action_name": the name of the action to query

   type Virtual_Has_Action is access function
     (Self        : Gaction_Group;
      Action_Name : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
   pragma Convention (C, Virtual_Has_Action);
   --  Checks if the named action exists within Action_Group.
   --  Since: gtk+ 2.28
   --  "action_name": the name of the action to check for

   type Virtual_List_Actions is access function (Self : Gaction_Group) return chars_ptr_array_access;
   pragma Convention (C, Virtual_List_Actions);
   --  Lists the actions contained within Action_Group.
   --  The caller is responsible for freeing the list with g_strfreev when it
   --  is no longer required.
   --  Since: gtk+ 2.28

   type Virtual_Query_Action is access function
     (Self           : Gaction_Group;
      Action_Name    : Gtkada.Types.Chars_Ptr;
      Enabled        : access Glib.Gboolean;
      Parameter_Type : access Glib.Variant.Gvariant_Type;
      State_Type     : access Glib.Variant.Gvariant_Type;
      State_Hint     : access System.Address;
      State          : access System.Address) return Glib.Gboolean;
   pragma Convention (C, Virtual_Query_Action);
   --  Queries all aspects of the named action within an Action_Group.
   --  This function acquires the information available from
   --  Glib.Action_Group.Has_Action, Glib.Action_Group.Get_Action_Enabled,
   --  Glib.Action_Group.Get_Action_Parameter_Type,
   --  Glib.Action_Group.Get_Action_State_Type,
   --  Glib.Action_Group.Get_Action_State_Hint and
   --  Glib.Action_Group.Get_Action_State with a single function call.
   --  This provides two main benefits.
   --  The first is the improvement in efficiency that comes with not having
   --  to perform repeated lookups of the action in order to discover different
   --  things about it. The second is that implementing
   --  Glib.Action_Group.Gaction_Group can now be done by only overriding this
   --  one virtual function.
   --  The interface provides a default implementation of this function that
   --  calls the individual functions, as required, to fetch the information.
   --  The interface also provides default implementations of those functions
   --  that call this function. All implementations, therefore, must override
   --  either this function or all of the others.
   --  If the action exists, True is returned and any of the requested fields
   --  (as indicated by having a non-null reference passed in) are filled. If
   --  the action doesn't exist, False is returned and the fields may or may
   --  not have been modified.
   --  Since: gtk+ 2.32
   --  "action_name": the name of an action in the group
   --  "enabled": if the action is presently enabled
   --  "parameter_type": the parameter type, or null if none needed
   --  "state_type": the state type, or null if stateless
   --  "state_hint": the state hint, or null if none
   --  "state": the current state, or null if stateless

   subtype Action_Group_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Action_Added
     (Self    : Action_Group_Interface_Descr;
      Handler : Virtual_Action_Added);
   pragma Import (C, Set_Action_Added, "gtkada_Action_Group_set_action_added");

   procedure Set_Action_Enabled_Changed
     (Self    : Action_Group_Interface_Descr;
      Handler : Virtual_Action_Enabled_Changed);
   pragma Import (C, Set_Action_Enabled_Changed, "gtkada_Action_Group_set_action_enabled_changed");

   procedure Set_Action_Removed
     (Self    : Action_Group_Interface_Descr;
      Handler : Virtual_Action_Removed);
   pragma Import (C, Set_Action_Removed, "gtkada_Action_Group_set_action_removed");

   procedure Set_Action_State_Changed
     (Self    : Action_Group_Interface_Descr;
      Handler : Virtual_Action_State_Changed);
   pragma Import (C, Set_Action_State_Changed, "gtkada_Action_Group_set_action_state_changed");

   procedure Set_Activate_Action
     (Self    : Action_Group_Interface_Descr;
      Handler : Virtual_Activate_Action);
   pragma Import (C, Set_Activate_Action, "gtkada_Action_Group_set_activate_action");

   procedure Set_Change_Action_State
     (Self    : Action_Group_Interface_Descr;
      Handler : Virtual_Change_Action_State);
   pragma Import (C, Set_Change_Action_State, "gtkada_Action_Group_set_change_action_state");

   procedure Set_Get_Action_Enabled
     (Self    : Action_Group_Interface_Descr;
      Handler : Virtual_Get_Action_Enabled);
   pragma Import (C, Set_Get_Action_Enabled, "gtkada_Action_Group_set_get_action_enabled");

   procedure Set_Get_Action_Parameter_Type
     (Self    : Action_Group_Interface_Descr;
      Handler : Virtual_Get_Action_Parameter_Type);
   pragma Import (C, Set_Get_Action_Parameter_Type, "gtkada_Action_Group_set_get_action_parameter_type");

   procedure Set_Get_Action_State
     (Self    : Action_Group_Interface_Descr;
      Handler : Virtual_Get_Action_State);
   pragma Import (C, Set_Get_Action_State, "gtkada_Action_Group_set_get_action_state");

   procedure Set_Get_Action_State_Hint
     (Self    : Action_Group_Interface_Descr;
      Handler : Virtual_Get_Action_State_Hint);
   pragma Import (C, Set_Get_Action_State_Hint, "gtkada_Action_Group_set_get_action_state_hint");

   procedure Set_Get_Action_State_Type
     (Self    : Action_Group_Interface_Descr;
      Handler : Virtual_Get_Action_State_Type);
   pragma Import (C, Set_Get_Action_State_Type, "gtkada_Action_Group_set_get_action_state_type");

   procedure Set_Has_Action
     (Self    : Action_Group_Interface_Descr;
      Handler : Virtual_Has_Action);
   pragma Import (C, Set_Has_Action, "gtkada_Action_Group_set_has_action");

   procedure Set_List_Actions
     (Self    : Action_Group_Interface_Descr;
      Handler : Virtual_List_Actions);
   pragma Import (C, Set_List_Actions, "gtkada_Action_Group_set_list_actions");

   procedure Set_Query_Action
     (Self    : Action_Group_Interface_Descr;
      Handler : Virtual_Query_Action);
   pragma Import (C, Set_Query_Action, "gtkada_Action_Group_set_query_action");
   --  See Glib.Object.Add_Interface

private

Null_Gaction_Group : constant Gaction_Group :=
   Gaction_Group (Glib.Types.Null_Interface);
end Glib.Action_Group;
