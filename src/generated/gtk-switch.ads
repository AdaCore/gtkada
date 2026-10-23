------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Shows a "light switch" that has two states: on or off.
--
--  <picture> <source srcset="switch-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkSwitch" src="switch.png"> </picture>
--  The user can control which state should be active by clicking the empty
--  area, or by dragging the slider.
--
--  `GtkSwitch` can also express situations where the underlying state changes
--  with a delay. In this case, the slider position indicates the user's recent
--  change (represented by the [propertyGtk.Switch:active] property), while the
--  trough color indicates the present underlying state (represented by the
--  [propertyGtk.Switch:state] property).
--
--  <picture> <source srcset="switch-state-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="GtkSwitch with delayed
--  state change" src="switch-state.png"> </picture>
--  See [signalGtk.Switch::state-set] for details.
--
--  # Shortcuts and Gestures
--
--  `GtkSwitch` supports pan and drag gestures to move the slider.
--
--  # CSS nodes
--
--  ``` switch ├── image ├── image ╰── slider ```
--
--  `GtkSwitch` has four css nodes, the main node with the name switch and
--  subnodes for the slider and the on and off images. Neither of them is using
--  any style classes.
--
--  # Accessibility
--
--  `GtkSwitch` uses the [enumGtk.AccessibleRole.switch] role.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Glib.Variant;          use Glib.Variant;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Actionable;        use Gtk.Actionable;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Switch is

   type Gtk_Switch_Record is new Gtk_Widget_Record with null record;
   type Gtk_Switch is access all Gtk_Switch_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Switch);
   procedure Initialize (Self : not null access Gtk_Switch_Record'Class);
   --  Creates a new `GtkSwitch` widget.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Switch_New return Gtk_Switch;
   --  Creates a new `GtkSwitch` widget.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_switch_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Active
      (Self : not null access Gtk_Switch_Record) return Boolean;
   --  Gets whether the `GtkSwitch` is in its "on" or "off" state.
   --  @return True if the `GtkSwitch` is active, and False otherwise

   procedure Set_Active
      (Self      : not null access Gtk_Switch_Record;
       Is_Active : Boolean);
   --  Changes the state of Self to the desired one.
   --  @param Is_Active True if Self should be active, and False otherwise

   function Get_State
      (Self : not null access Gtk_Switch_Record) return Boolean;
   --  Gets the underlying state of the `GtkSwitch`.
   --  @return the underlying state

   procedure Set_State
      (Self  : not null access Gtk_Switch_Record;
       State : Boolean);
   --  Sets the underlying state of the `GtkSwitch`.
   --  This function is typically called from a [signalGtk.Switch::state-set]
   --  signal handler in order to set up delayed state changes.
   --  See [signalGtk.Switch::state-set] for details.
   --  @param State the new state

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Switch_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Switch_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Switch_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Switch_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Switch_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Switch_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Switch_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Switch_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Switch_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Switch_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Switch_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Switch_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Switch_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Switch_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Switch_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Action_Name
      (Self : not null access Gtk_Switch_Record) return UTF8_String;

   procedure Set_Action_Name
      (Self        : not null access Gtk_Switch_Record;
       Action_Name : UTF8_String := "");

   function Get_Action_Target_Value
      (Self : not null access Gtk_Switch_Record)
       return Glib.Variant.Gvariant;

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_Switch_Record;
       Target_Value : Glib.Variant.Gvariant);

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_Switch_Record;
       Detailed_Action_Name : UTF8_String);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Active_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `GtkSwitch` widget is in its on or off state.

   State_Property : constant Glib.Properties.Property_Boolean;
   --  The backend state that is controlled by the switch.
   --
   --  Applications should usually set the [propertyGtk.Switch:active]
   --  property, except when indicating a change to the backend state which
   --  occurs separately from the user's interaction.
   --
   --  See [signalGtk.Switch::state-set] for details.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Switch_Void is not null access procedure (Self : access Gtk_Switch_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gtk_Switch_Record;
       Call  : Cb_Gtk_Switch_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gtk_Switch_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to animate the switch.
   --
   --  Applications should never connect to this signal, but use the
   --  [propertyGtk.Switch:active] property.

   type Cb_Gtk_Switch_Boolean_Boolean is not null access function
     (Self  : access Gtk_Switch_Record'Class;
      State : Boolean) return Boolean;

   type Cb_GObject_Boolean_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      State : Boolean) return Boolean;

   Signal_State_Set : constant Glib.Signal_Name := "state-set";
   procedure On_State_Set
      (Self  : not null access Gtk_Switch_Record;
       Call  : Cb_Gtk_Switch_Boolean_Boolean;
       After : Boolean := False);
   procedure On_State_Set
      (Self  : not null access Gtk_Switch_Record;
       Call  : Cb_GObject_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to change the underlying state.
   --
   --  The ::state-set signal is emitted when the user changes the switch
   --  position. The default handler calls [methodGtk.Switch.set_state] with
   --  the value of State.
   --
   --  To implement delayed state change, applications can connect to this
   --  signal, initiate the change of the underlying state, and call
   --  [methodGtk.Switch.set_state] when the underlying state change is
   --  complete. The signal handler should return True to prevent the default
   --  handler from running.
   -- 
   --  Callback parameters:
   --    --  @param State the new state of the switch

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Actionable"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Switch_Record, Gtk_Switch);
   function "+"
     (Widget : access Gtk_Switch_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Switch
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Actionable is new Glib.Types.Implements
     (Gtk.Actionable.Gtk_Actionable, Gtk_Switch_Record, Gtk_Switch);
   function "+"
     (Widget : access Gtk_Switch_Record'Class)
   return Gtk.Actionable.Gtk_Actionable
   renames Implements_Gtk_Actionable.To_Interface;
   function "-"
     (Interf : Gtk.Actionable.Gtk_Actionable)
   return Gtk_Switch
   renames Implements_Gtk_Actionable.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Switch_Record, Gtk_Switch);
   function "+"
     (Widget : access Gtk_Switch_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Switch
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Switch_Record, Gtk_Switch);
   function "+"
     (Widget : access Gtk_Switch_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Switch
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   State_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("state");
   Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("active");
end Gtk.Switch;
