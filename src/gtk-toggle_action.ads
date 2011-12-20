------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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
--  A Gtk_Toggle_Action corresponds roughly to a Gtk_Check_Menu_Item. It has an
--  "active" state specifying whether the action has been checked or not.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Action-based menus</group>
--  <see>Gtk.Action</see>

with Glib.Properties;
with Gtk.Action;

package Gtk.Toggle_Action is

   type Gtk_Toggle_Action_Record is new Gtk.Action.Gtk_Action_Record with
     null record;
   type Gtk_Toggle_Action is access all Gtk_Toggle_Action_Record'Class;

   procedure Gtk_New
     (Action   : out Gtk_Toggle_Action;
      Name     : String;
      Label    : String := "";
      Tooltip  : String := "";
      Stock_Id : String := "");
   procedure Initialize
     (Action   : access Gtk_Toggle_Action_Record'Class;
      Name     : String;
      Label    : String := "";
      Tooltip  : String := "";
      Stock_Id : String := "");
   --  Creates a new Gtk_Toggle_Action object. To add the action to
   --  a Gtk_Action_Group and set the accelerator for the action,
   --  call Gtk.Action_Group.Add_Action_With_Accel.

   function Get_Type return GType;
   --  Return the internal type associated with Gtk_Toggle_Action.

   procedure Set_Active
     (Action : access Gtk_Toggle_Action_Record; Is_Active : Boolean);
   function Get_Active
     (Action : access Gtk_Toggle_Action_Record) return Boolean;
   --  Returns the checked state of the toggle action.

   procedure Set_Draw_As_Radio
     (Action : access Gtk_Toggle_Action_Record; Draw_As_Radio : Boolean);
   function Get_Draw_As_Radio
     (Action : access Gtk_Toggle_Action_Record) return Boolean;
   --  Returns whether the action should have proxies like a radio action. This
   --  changes the display of widgets associated with that action.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Draw_As_Radio_Property
   --  Type:  Boolean
   --  Descr: Whether the proxies for this action look like radio action
   --         proxies
   --
   --  </properties>

   Draw_As_Radio_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "toggled"
   --    procedure Handler (Toggled : access Gtk_Toggle_Action_Record'Class);
   --    Called when the state of the action is toggled.
   --
   --  </signals>

   Signal_Toggled : constant Glib.Signal_Name := "toggled";

   procedure Toggled (Action : access Gtk_Toggle_Action_Record);
   --  Emits the "toggled" signal on the toggle action.

private
   Draw_As_Radio_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("draw-as-radio");

   pragma Import (C, Get_Type, "gtk_toggle_action_get_type");
end Gtk.Toggle_Action;
