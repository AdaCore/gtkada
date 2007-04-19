-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006-2007 AdaCore                    --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  A Gtk_Radio_Action is similar to Gtk_Radio_Menu_Item. A number of radio
--  actions can be linked together so that only one may be active at any one
--  time.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Action-based menus</group>
--  <see>Gtk_Action</see>

with Glib.Properties;
with Gtk.Toggle_Action;
with Gtk.Widget;

package Gtk.Radio_Action is

   type Gtk_Radio_Action_Record is
     new Gtk.Toggle_Action.Gtk_Toggle_Action_Record with null record;
   type Gtk_Radio_Action is access all Gtk_Radio_Action_Record'Class;

   procedure Gtk_New
     (Action   : out Gtk_Radio_Action;
      Name     : String;
      Label    : String := "";
      Tooltip  : String := "";
      Stock_Id : String := "";
      Value    : Gint);
   procedure Initialize
     (Action   : access Gtk_Radio_Action_Record'Class;
      Name     : String;
      Label    : String := "";
      Tooltip  : String := "";
      Stock_Id : String := "";
      Value    : Gint);
   --  Creates a new Gtk_Radio_Action object. To add the action to
   --  a Gtk_Action_Group and set the accelerator for the action,
   --  call Gtk.Action_Group.Add_Action_With_Accel.

   function Get_Type return GType;
   --  Return the internal type used for a Gtk_Radio_Action

   function Get_Current_Value
     (Action : access Gtk_Radio_Action_Record) return Glib.Gint;
   --  Obtains the value property of the currently active member of
   --  the group to which Action belongs.

   procedure Set_Group
     (Action : access Gtk_Radio_Action_Record;
      Group  : Gtk.Widget.Widget_SList.GSlist);
   function Get_Group
     (Action : access Gtk_Radio_Action_Record)
     return Gtk.Widget.Widget_SList.GSlist;
   --  Returns the list representing the radio group for this object.
   --  Note that the returned list is only valid until the next change
   --  to the group.
   --
   --  A common way to set up a group of radio group is the following:
   --      Group  : GSlist := null;
   --      Action : Gtk_Radio_Action;
   --      while ... loop
   --          Gtk_New (Action, ...);
   --          Set_Group (Action, Group);
   --          Group := Get_Group (Action);
   --      end loop;

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Group_Property
   --  Type:  Object
   --  Descr: The radio action whose group this action belongs to.
   --
   --  Name:  Value_Property
   --  Type:  Int
   --  Descr: The value returned by Gtk.Radio_Action.Get_Current_Value when
   --         this action is the current action of its group.
   --  </properties>

   Group_Property : constant Glib.Properties.Property_Object;
   Value_Property : constant Glib.Properties.Property_Int;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "changed"
   --    procedure Handler
   --       (Action  : access Gtk_Radio_Action_Record'Class;
   --        Current : access Gtk_Radio_Action_Record'Class);
   --    The changed signal is emitted on every member of a radio group when
   --    the active member is changed. The signal gets emitted after the
   --    activate signals for the previous and current active members.
   --    Current is the action that is currently active
   --  </signals>

   Signal_Changed : constant Glib.Signal_Name := "changed";

private
   Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("group");
   Value_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("value");

   pragma Import (C, Get_Type, "gtk_radio_action_get_type");
end Gtk.Radio_Action;


