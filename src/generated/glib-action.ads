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

--  <description>
--  Glib.Action.Gaction represents a single named action.
--
--  The main interface to an action is that it can be activated with
--  Glib.Action.Activate. This results in the 'activate' signal being emitted.
--  An activation has a Glib.Variant.Gvariant parameter (which may be null).
--  The correct type for the parameter is determined by a static parameter type
--  (which is given at construction time).
--
--  An action may optionally have a state, in which case the state may be set
--  with Glib.Action.Change_State. This call takes a Glib.Variant.Gvariant. The
--  correct type for the state is determined by a static state type (which is
--  given at construction time).
--
--  The state may have a hint associated with it, specifying its valid range.
--
--  Glib.Action.Gaction is merely the interface to the concept of an action,
--  as described above. Various implementations of actions exist, including
--  Glib.Simple_Action.Gsimple_Action and Gtk.Action.Gtk_Action.
--
--  In all cases, the implementing class is responsible for storing the name
--  of the action, the parameter type, the enabled state, the optional state
--  type and the state and emitting the appropriate signals when these change.
--  The implementor responsible for filtering calls to Glib.Action.Activate and
--  Glib.Action.Change_State for type safety and for the state being enabled.
--
--  Probably the only useful thing to do with a Glib.Action.Gaction is to put
--  it inside of a Glib.Simple_Action_Group.Gsimple_Action_Group.
--
--  </description>
pragma Ada_2005;

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Glib.Variant;    use Glib.Variant;

package Glib.Action is

   type Gaction is new Glib.Types.GType_Interface;
   Null_Gaction : constant Gaction;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_action_get_type");

   -------------
   -- Methods --
   -------------

   procedure Activate (Self : Gaction; Parameter : Glib.Variant.Gvariant);
   --  Activates the action.
   --  Parameter must be the correct type of parameter for the action (ie: the
   --  parameter type given at construction time). If the parameter type was
   --  null then Parameter must also be null.
   --  Since: gtk+ 2.28
   --  "parameter": the parameter to the activation

   procedure Change_State (Self : Gaction; Value : Glib.Variant.Gvariant);
   --  Request for the state of Action to be changed to Value.
   --  The action must be stateful and Value must be of the correct type. See
   --  Glib.Action.Get_State_Type.
   --  This call merely requests a change. The action may refuse to change its
   --  state or may change its state to something other than Value. See
   --  Glib.Action.Get_State_Hint.
   --  If the Value GVariant is floating, it is consumed.
   --  Since: gtk+ 2.30
   --  "value": the new state

   function Get_Enabled (Self : Gaction) return Boolean;
   --  Checks if Action is currently enabled.
   --  An action must be enabled in order to be activated or in order to have
   --  its state changed from outside callers.
   --  Since: gtk+ 2.28

   function Get_Name (Self : Gaction) return UTF8_String;
   --  Queries the name of Action.
   --  Since: gtk+ 2.28

   function Get_Parameter_Type
      (Self : Gaction) return Glib.Variant.Gvariant_Type;
   pragma Import (C, Get_Parameter_Type, "g_action_get_parameter_type");
   --  Queries the type of the parameter that must be given when activating
   --  Action.
   --  When activating the action using Glib.Action.Activate, the
   --  Glib.Variant.Gvariant given to that function must be of the type
   --  returned by this function.
   --  In the case that this function returns null, you must not give any
   --  Glib.Variant.Gvariant, but null instead.
   --  Since: gtk+ 2.28

   function Get_State (Self : Gaction) return Glib.Variant.Gvariant;
   --  Queries the current state of Action.
   --  If the action is not stateful then null will be returned. If the action
   --  is stateful then the type of the return value is the type given by
   --  Glib.Action.Get_State_Type.
   --  The return value (if non-null) should be freed with Glib.Variant.Unref
   --  when it is no longer required.
   --  Since: gtk+ 2.28

   function Get_State_Hint (Self : Gaction) return Glib.Variant.Gvariant;
   --  Requests a hint about the valid range of values for the state of
   --  Action.
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

   function Get_State_Type
      (Self : Gaction) return Glib.Variant.Gvariant_Type;
   pragma Import (C, Get_State_Type, "g_action_get_state_type");
   --  Queries the type of the state of Action.
   --  If the action is stateful (e.g. created with
   --  Glib.Simple_Action.G_New_Stateful) then this function returns the
   --  Glib.Variant.Gvariant_Type of the state. This is the type of the initial
   --  value given as the state. All calls to Glib.Action.Change_State must
   --  give a Glib.Variant.Gvariant of this type and Glib.Action.Get_State will
   --  return a Glib.Variant.Gvariant of the same type.
   --  If the action is not stateful (e.g. created with
   --  Glib.Simple_Action.G_New) then this function will return null. In that
   --  case, Glib.Action.Get_State will return null and you must not call
   --  Glib.Action.Change_State.
   --  Since: gtk+ 2.28

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Enabled_Property : constant Glib.Properties.Property_Boolean;
   --  If Action is currently enabled.
   --
   --  If the action is disabled then calls to Glib.Action.Activate and
   --  Glib.Action.Change_State have no effect.

   Name_Property : constant Glib.Properties.Property_String;
   --  The name of the action. This is mostly meaningful for identifying the
   --  action once it has been added to a Glib.Action_Group.Gaction_Group.

   Parameter_Type_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GLib.Variant_Type
   --  The type of the parameter that must be given when activating the
   --  action.

   State_Property : constant Glib.Properties.Property_Object;
   --  Type: Glib.Variant.Gvariant
   --  The state of the action, or null if the action is stateless.

   State_Type_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GLib.Variant_Type
   --  The Glib.Variant.Gvariant_Type of the state that the action has, or
   --  null if the action is stateless.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gaction"

   function "+" (W : Gaction) return Gaction;
   pragma Inline ("+");

private
   State_Type_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("state-type");
   State_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("state");
   Parameter_Type_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("parameter-type");
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
   Enabled_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enabled");

Null_Gaction : constant Gaction :=
   Gaction (Glib.Types.Null_Interface);
end Glib.Action;
