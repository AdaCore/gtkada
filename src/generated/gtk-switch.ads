------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Action;      use Gtk.Action;
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

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Switch_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);

   function Get_Related_Action
      (Self : not null access Gtk_Switch_Record)
       return Gtk.Action.Gtk_Action;

   procedure Set_Related_Action
      (Self   : not null access Gtk_Switch_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Switch_Record) return Boolean;

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Switch_Record;
       Use_Appearance : Boolean);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Switch_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Activatable"
   --
   --  - "Buildable"

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

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Active_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Whether the Gtk.Switch.Gtk_Switch widget is in its on or off state.

   Active_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "activate"
   --     procedure Handler (Self : access Gtk_Switch_Record'Class);
   --  The ::activate signal on GtkSwitch is an action signal and emitting it
   --  causes the switch to animate. Applications should never connect to this
   --  signal, but use the notify::active signal.

   Signal_Activate : constant Glib.Signal_Name := "activate";

private
   Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("active");
end Gtk.Switch;
