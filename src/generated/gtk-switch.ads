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
--  Gtk.Switch.Gtk_Switch is a widget that has two states: on or off. The user
--  can control which state should be active by clicking the empty area, or by
--  dragging the handle.
--
--  GtkSwitch can also handle situations where the underlying state changes
--  with a delay. See Gtk.Switch.Gtk_Switch::state-set for details.
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> switch ╰── slider ]|
--
--  GtkSwitch has two css nodes, the main node with the name switch and a
--  subnode named slider. Neither of them is using any style classes.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Glib.Variant;    use Glib.Variant;
with Gtk.Action;      use Gtk.Action;
with Gtk.Actionable;  use Gtk.Actionable;
with Gtk.Activatable; use Gtk.Activatable;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Switch is

   type Gtk_Switch_Record is new Gtk_Widget_Record with null record;
   type Gtk_Switch is access all Gtk_Switch_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Switch);
   procedure Initialize (Self : not null access Gtk_Switch_Record'Class);
   --  Creates a new Gtk.Switch.Gtk_Switch widget.
   --  Since: gtk+ 3.0
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Switch_New return Gtk_Switch;
   --  Creates a new Gtk.Switch.Gtk_Switch widget.
   --  Since: gtk+ 3.0

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_switch_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Active
      (Self : not null access Gtk_Switch_Record) return Boolean;
   --  Gets whether the Gtk.Switch.Gtk_Switch is in its "on" or "off" state.
   --  Since: gtk+ 3.0

   procedure Set_Active
      (Self      : not null access Gtk_Switch_Record;
       Is_Active : Boolean);
   --  Changes the state of Sw to the desired one.
   --  Since: gtk+ 3.0
   --  "is_active": True if Sw should be active, and False otherwise

   function Get_State
      (Self : not null access Gtk_Switch_Record) return Boolean;
   --  Gets the underlying state of the Gtk.Switch.Gtk_Switch.
   --  Since: gtk+ 3.14

   procedure Set_State
      (Self  : not null access Gtk_Switch_Record;
       State : Boolean);
   --  Sets the underlying state of the Gtk.Switch.Gtk_Switch.
   --  Normally, this is the same as Gtk.Switch.Gtk_Switch:active, unless the
   --  switch is set up for delayed state changes. This function is typically
   --  called from a Gtk.Switch.Gtk_Switch::state-set signal handler.
   --  See Gtk.Switch.Gtk_Switch::state-set for details.
   --  Since: gtk+ 3.14
   --  "state": the new state

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

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

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Switch_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Do_Set_Related_Action);

   function Get_Related_Action
      (Self : not null access Gtk_Switch_Record)
       return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Related_Action);

   procedure Set_Related_Action
      (Self   : not null access Gtk_Switch_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Set_Related_Action);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Switch_Record) return Boolean;
   pragma Obsolescent (Get_Use_Action_Appearance);

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Switch_Record;
       Use_Appearance : Boolean);
   pragma Obsolescent (Set_Use_Action_Appearance);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Switch_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Sync_Action_Properties);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Active_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the Gtk.Switch.Gtk_Switch widget is in its on or off state.

   State_Property : constant Glib.Properties.Property_Boolean;
   --  The backend state that is controlled by the switch. See
   --  Gtk.Switch.Gtk_Switch::state-set for details.

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
   --  The ::activate signal on GtkSwitch is an action signal and emitting it
   --  causes the switch to animate. Applications should never connect to this
   --  signal, but use the notify::active signal.

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
   --  The ::state-set signal on GtkSwitch is emitted to change the underlying
   --  state. It is emitted when the user changes the switch position. The
   --  default handler keeps the state in sync with the
   --  Gtk.Switch.Gtk_Switch:active property.
   --
   --  To implement delayed state change, applications can connect to this
   --  signal, initiate the change of the underlying state, and call
   --  Gtk.Switch.Set_State when the underlying state change is complete. The
   --  signal handler should return True to prevent the default handler from
   --  running.
   --
   --  Visually, the underlying state is represented by the trough color of
   --  the switch, while the Gtk.Switch.Gtk_Switch:active property is
   --  represented by the position of the switch.
   -- 
   --  Callback parameters:
   --    --  "state": the new state of the switch
   --    --  Returns True to stop the signal emission

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Actionable"
   --
   --  - "Activatable"
   --
   --  - "Buildable"

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

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Switch_Record, Gtk_Switch);
   function "+"
     (Widget : access Gtk_Switch_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Switch
   renames Implements_Gtk_Activatable.To_Object;

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

private
   State_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("state");
   Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("active");
end Gtk.Switch;
