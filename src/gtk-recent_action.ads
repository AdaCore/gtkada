------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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
--  A Gtk_Recent_Action represents a list of recently used files, which
--  can be shown by widgets such as Gtk_Recent_Chooser_Dialog or
--  Gtk_Recent_Chooser_Menu.
--
--  To construct a submenu showing recently used files, use a Gtk_Recent_Action
--  as the action for a menuitem. To construct a menu toolbutton showing the
--  recently used files in the popup menu, use a Gtk_Recent_Action as the
--  action for a toolitem element.
--  </description>
--  <group>Action-based menus</group>
--  <c_version>2.16.6</c_version>

with Glib.Properties;
with Gtk.Action;
with Gtk.Recent_Manager;

package Gtk.Recent_Action is

   type Gtk_Recent_Action_Record is
     new Gtk.Action.Gtk_Action_Record with private;
   type Gtk_Recent_Action is access all Gtk_Recent_Action_Record'Class;

   procedure Gtk_New
     (Widget   : out Gtk_Recent_Action;
      Name     : String;
      Label    : String := "";
      Tooltip  : String := "";
      Stock_Id : String := "");
   procedure Initialize
     (Widget   : access Gtk_Recent_Action_Record'Class;
      Name     : String;
      Label    : String := "";
      Tooltip  : String := "";
      Stock_Id : String := "");
   --  Name: a unique name for the action
   --  Label: the label displayed in menu items and on buttons
   --  Tooltip: a tooltip for the action
   --  Stock_Id: the stock icon to display in widgets representing the
   --  action
   --
   --  Creates a new Gtk_Recent_Action object. To add the action to
   --  a Gtk_Action_Group and set the accelerator for the action,
   --  call Gtk.Action_Group.Add_Action_With_Accel.

   function Get_Type return GType;
   --  Return the internal value associated with this widget.

   procedure Gtk_New_For_Manager
     (Widget   : out Gtk_Recent_Action;
      Name     : String;
      Label    : String := "";
      Tooltip  : String := "";
      Stock_Id : String := "";
      Manager  : access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class :=
                 Gtk.Recent_Manager.Get_Default);
   procedure Initialize_For_Manager
     (Widget   : access Gtk_Recent_Action_Record'Class;
      Name     : String;
      Label    : String := "";
      Tooltip  : String := "";
      Stock_Id : String := "";
      Manager  : access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class :=
                 Gtk.Recent_Manager.Get_Default);
   --  Name: a unique name for the action
   --  Label: the label displayed in menu items and on buttons
   --  Tooltip: a tooltip for the action
   --  Stock_Id: the stock icon to display in widgets representing the
   --  action
   --  manager: a Gtk_Recent_Manager, or use the default Gtk_Recent_Manager
   --
   --  Creates a new Gtk_Recent_Action object. To add the action to
   --  a Gtk_Action_Group and set the accelerator for the action,
   --  call gtk_action_group_add_action_with_accel().

   function Get_Show_Numbers
     (Action : access Gtk_Recent_Action_Record) return Boolean;
   procedure Set_Show_Numbers
     (Action       : access Gtk_Recent_Action_Record;
      Show_Numbers : Boolean);
   --  Whether a number should be added to the items shown by the
   --  widgets representing Action. The numbers are shown to provide
   --  a unique character for a mnemonic to be used inside the menu item's
   --  label. Only the first ten items get a number to avoid clashes.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  Name:  Show_Numbers_Property
   --  Type:  Boolean
   --  Descr: Whether the items should be displayed with a number
   --
   --  </properties>

   Show_Numbers_Property : constant Glib.Properties.Property_Boolean;

private
   type Gtk_Recent_Action_Record is
     new Gtk.Action.Gtk_Action_Record with null record;

   Show_Numbers_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-numbers");

   pragma Import (C, Get_Type, "gtk_recent_action_get_type");
end Gtk.Recent_Action;
