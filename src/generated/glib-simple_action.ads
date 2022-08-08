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
--  A Glib.Simple_Action.Gsimple_Action is the obvious simple implementation
--  of the Glib.Action.Gaction interface. This is the easiest way to create an
--  action for purposes of adding it to a
--  Glib.Simple_Action_Group.Gsimple_Action_Group.
--
--  See also Gtk.Action.Gtk_Action.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib.Action;     use Glib.Action;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Glib.Variant;    use Glib.Variant;

package Glib.Simple_Action is

   type Gsimple_Action_Record is new GObject_Record with null record;
   type Gsimple_Action is access all Gsimple_Action_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure G_New
      (Self           : out Gsimple_Action;
       Name           : UTF8_String;
       Parameter_Type : Glib.Variant.Gvariant_Type);
   --  Creates a new action.
   --  The created action is stateless. See Glib.Simple_Action.G_New_Stateful
   --  to create an action that has state.
   --  Since: gtk+ 2.28
   --  "name": the name of the action
   --  "parameter_type": the type of parameter that will be passed to handlers
   --  for the Glib.Simple_Action.Gsimple_Action::activate signal, or null for
   --  no parameter

   procedure Initialize
      (Self           : not null access Gsimple_Action_Record'Class;
       Name           : UTF8_String;
       Parameter_Type : Glib.Variant.Gvariant_Type);
   --  Creates a new action.
   --  The created action is stateless. See Glib.Simple_Action.G_New_Stateful
   --  to create an action that has state.
   --  Since: gtk+ 2.28
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "name": the name of the action
   --  "parameter_type": the type of parameter that will be passed to handlers
   --  for the Glib.Simple_Action.Gsimple_Action::activate signal, or null for
   --  no parameter

   function Gsimple_Action_New
      (Name           : UTF8_String;
       Parameter_Type : Glib.Variant.Gvariant_Type) return Gsimple_Action;
   --  Creates a new action.
   --  The created action is stateless. See Glib.Simple_Action.G_New_Stateful
   --  to create an action that has state.
   --  Since: gtk+ 2.28
   --  "name": the name of the action
   --  "parameter_type": the type of parameter that will be passed to handlers
   --  for the Glib.Simple_Action.Gsimple_Action::activate signal, or null for
   --  no parameter

   procedure G_New_Stateful
      (Self           : out Gsimple_Action;
       Name           : UTF8_String;
       Parameter_Type : Glib.Variant.Gvariant_Type;
       State          : Glib.Variant.Gvariant);
   --  Creates a new stateful action.
   --  All future state values must have the same Glib.Variant.Gvariant_Type
   --  as the initial State.
   --  If the State Glib.Variant.Gvariant is floating, it is consumed.
   --  Since: gtk+ 2.28
   --  "name": the name of the action
   --  "parameter_type": the type of the parameter that will be passed to
   --  handlers for the Glib.Simple_Action.Gsimple_Action::activate signal, or
   --  null for no parameter
   --  "state": the initial state of the action

   procedure Initialize_Stateful
      (Self           : not null access Gsimple_Action_Record'Class;
       Name           : UTF8_String;
       Parameter_Type : Glib.Variant.Gvariant_Type;
       State          : Glib.Variant.Gvariant);
   --  Creates a new stateful action.
   --  All future state values must have the same Glib.Variant.Gvariant_Type
   --  as the initial State.
   --  If the State Glib.Variant.Gvariant is floating, it is consumed.
   --  Since: gtk+ 2.28
   --  Initialize_Stateful does nothing if the object was already created with
   --  another call to Initialize* or G_New.
   --  "name": the name of the action
   --  "parameter_type": the type of the parameter that will be passed to
   --  handlers for the Glib.Simple_Action.Gsimple_Action::activate signal, or
   --  null for no parameter
   --  "state": the initial state of the action

   function Gsimple_Action_New_Stateful
      (Name           : UTF8_String;
       Parameter_Type : Glib.Variant.Gvariant_Type;
       State          : Glib.Variant.Gvariant) return Gsimple_Action;
   --  Creates a new stateful action.
   --  All future state values must have the same Glib.Variant.Gvariant_Type
   --  as the initial State.
   --  If the State Glib.Variant.Gvariant is floating, it is consumed.
   --  Since: gtk+ 2.28
   --  "name": the name of the action
   --  "parameter_type": the type of the parameter that will be passed to
   --  handlers for the Glib.Simple_Action.Gsimple_Action::activate signal, or
   --  null for no parameter
   --  "state": the initial state of the action

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_simple_action_get_type");

   -------------
   -- Methods --
   -------------

   procedure Set_Enabled
      (Self    : not null access Gsimple_Action_Record;
       Enabled : Boolean);
   --  Sets the action as enabled or not.
   --  An action must be enabled in order to be activated or in order to have
   --  its state changed from outside callers.
   --  This should only be called by the implementor of the action. Users of
   --  the action should not attempt to modify its enabled flag.
   --  Since: gtk+ 2.28
   --  "enabled": whether the action is enabled

   procedure Set_State
      (Self  : not null access Gsimple_Action_Record;
       Value : Glib.Variant.Gvariant);
   --  Sets the state of the action.
   --  This directly updates the 'state' property to the given value.
   --  This should only be called by the implementor of the action. Users of
   --  the action should not attempt to directly modify the 'state' property.
   --  Instead, they should call Glib.Action.Change_State to request the
   --  change.
   --  If the Value GVariant is floating, it is consumed.
   --  Since: gtk+ 2.30
   --  "value": the new Glib.Variant.Gvariant for the state

   procedure Set_State_Hint
      (Self       : not null access Gsimple_Action_Record;
       State_Hint : Glib.Variant.Gvariant);
   --  Sets the state hint for the action.
   --  See Glib.Action.Get_State_Hint for more information about action state
   --  hints.
   --  Since: gtk+ 2.44
   --  "state_hint": a Glib.Variant.Gvariant representing the state hint

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Activate
      (Self      : not null access Gsimple_Action_Record;
       Parameter : Glib.Variant.Gvariant);

   procedure Change_State
      (Self  : not null access Gsimple_Action_Record;
       Value : Glib.Variant.Gvariant);

   function Get_Enabled
      (Self : not null access Gsimple_Action_Record) return Boolean;

   function Get_Name
      (Self : not null access Gsimple_Action_Record) return UTF8_String;

   function Get_Parameter_Type
      (Self : not null access Gsimple_Action_Record)
       return Glib.Variant.Gvariant_Type;

   function Get_State
      (Self : not null access Gsimple_Action_Record)
       return Glib.Variant.Gvariant;

   function Get_State_Hint
      (Self : not null access Gsimple_Action_Record)
       return Glib.Variant.Gvariant;

   function Get_State_Type
      (Self : not null access Gsimple_Action_Record)
       return Glib.Variant.Gvariant_Type;

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
   --  action once it has been added to a
   --  Glib.Simple_Action_Group.Gsimple_Action_Group.

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

   -------------
   -- Signals --
   -------------

   type Cb_Gsimple_Action_Gvariant_Void is not null access procedure
     (Self      : access Gsimple_Action_Record'Class;
      Parameter : Glib.Variant.Gvariant);

   type Cb_GObject_Gvariant_Void is not null access procedure
     (Self      : access Glib.Object.GObject_Record'Class;
      Parameter : Glib.Variant.Gvariant);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gsimple_Action_Record;
       Call  : Cb_Gsimple_Action_Gvariant_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gsimple_Action_Record;
       Call  : Cb_GObject_Gvariant_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Indicates that the action was just activated.
   --
   --  Parameter will always be of the expected type, i.e. the parameter type
   --  specified when the action was created. If an incorrect type is given
   --  when activating the action, this signal is not emitted.
   --
   --  Since GLib 2.40, if no handler is connected to this signal then the
   --  default behaviour for boolean-stated actions with a null parameter type
   --  is to toggle them via the
   --  Glib.Simple_Action.Gsimple_Action::change-state signal. For stateful
   --  actions where the state type is equal to the parameter type, the default
   --  is to forward them directly to
   --  Glib.Simple_Action.Gsimple_Action::change-state. This should allow
   --  almost all users of Glib.Simple_Action.Gsimple_Action to connect only
   --  one handler or the other.

   Signal_Change_State : constant Glib.Signal_Name := "change-state";
   procedure On_Change_State
      (Self  : not null access Gsimple_Action_Record;
       Call  : Cb_Gsimple_Action_Gvariant_Void;
       After : Boolean := False);
   procedure On_Change_State
      (Self  : not null access Gsimple_Action_Record;
       Call  : Cb_GObject_Gvariant_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Indicates that the action just received a request to change its state.
   --
   --  Value will always be of the correct state type, i.e. the type of the
   --  initial state passed to Glib.Simple_Action.G_New_Stateful. If an
   --  incorrect type is given when requesting to change the state, this signal
   --  is not emitted.
   --
   --  If no handler is connected to this signal then the default behaviour is
   --  to call Glib.Simple_Action.Set_State to set the state to the requested
   --  value. If you connect a signal handler then no default action is taken.
   --  If the state should change then you must call
   --  Glib.Simple_Action.Set_State from the handler.
   --
   --  An example of a 'change-state' handler: |[<!-- language="C" --> static
   --  void change_volume_state (GSimpleAction *action, GVariant *value,
   --  gpointer user_data) { gint requested;
   --
   --  requested = g_variant_get_int32 (value);
   --
   --  // Volume only goes from 0 to 10 if (0 <= requested && requested <= 10)
   --  g_simple_action_set_state (action, value); } ]|
   --
   --  The handler need not set the state to the requested value. It could set
   --  it to any value at all, or take some other action.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Action"

   package Implements_Gaction is new Glib.Types.Implements
     (Glib.Action.Gaction, Gsimple_Action_Record, Gsimple_Action);
   function "+"
     (Widget : access Gsimple_Action_Record'Class)
   return Glib.Action.Gaction
   renames Implements_Gaction.To_Interface;
   function "-"
     (Interf : Glib.Action.Gaction)
   return Gsimple_Action
   renames Implements_Gaction.To_Object;

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
end Glib.Simple_Action;
