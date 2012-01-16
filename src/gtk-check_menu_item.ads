------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2012, AdaCore                     --
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
--  A Gtk_Check_Menu_Item is a menu item that maintains the state of a boolean
--  value in addition to a Gtk_Menu_Item's usual role in activating application
--  code.
--
--  A check box indicating the state of the boolean value is displayed at the
--  left side of the Gtk_Menu_Item. Activating the Gtk_Menu_Item toggles the
--  value.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Menus and Toolbars</group>

with Glib.Properties;
with Gtk.Menu_Item;

package Gtk.Check_Menu_Item is

   type Gtk_Check_Menu_Item_Record is new
     Gtk.Menu_Item.Gtk_Menu_Item_Record with private;
   type Gtk_Check_Menu_Item is access all Gtk_Check_Menu_Item_Record'Class;

   procedure Gtk_New
     (Check_Menu_Item : out Gtk_Check_Menu_Item;
      Label           : UTF8_String := "");
   procedure Initialize
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record'Class;
      Label           : UTF8_String := "");
   --  Creates or initializes a new Gtk_Check_Menu_Item with a label, if label
   --  isn't null.

   procedure Gtk_New_With_Mnemonic
     (Check_Menu_Item : out Gtk_Check_Menu_Item;
      Label           : UTF8_String);
   procedure Initialize_With_Mnemonic
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record'Class;
      Label           : UTF8_String);
   --  Creates or initializes a new Gtk_Check_Menu_Item containing a label. The
   --  label will be created will be created using Gtk.Label.New_With_Mnemonic,
   --  so underscores in the label indicate the mnemonic for the menu item.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Calendar.

   procedure Set_Active
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record;
      Is_Active       : Boolean);
   function Get_Active
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record) return Boolean;
   --  Set the active state of the menu item's check box.

   procedure Set_Inconsistent
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record;
      Setting         : Boolean);
   function Get_Inconsistent
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record) return Boolean;
   --  If the user has selected a range of elements (such as some text or
   --  spreadsheet cells) that are affected by a boolean setting, and the
   --  current values in that range are inconsistent, you may want to
   --  display the check in an "in between" state. This function turns on
   --  "in between" display.  Normally you would turn off the inconsistent
   --  state again if the user explicitly selects a setting. This has to be
   --  done manually, Set_Inconsistent only affects visual appearance, it
   --  doesn't affect the semantics of the widget.

   procedure Set_Draw_As_Radio
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record;
      Draw_As_Radio   : Boolean);
   function Get_Draw_As_Radio
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record) return Boolean;
   --  Sets whether Check_Menu_Item is drawn like a Radio_Menu_Item.

   procedure Toggled (Check_Menu_Item : access Gtk_Check_Menu_Item_Record);
   --  Emit the "toggled" signal.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Active_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: Whether the menu item is checked.
   --  See also:  Set_Active and Get_Active
   --
   --  Name:  Inconsistent_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: Whether to display an "inconsistent" state.
   --  See also:  Set_Inconsistent and Get_Inconsistent
   --
   --  Name:  Draw_As_Radio_Property
   --  Type:  Boolean
   --  Descr: Whether the menu item looks like a radio menu item
   --
   --  </properties>

   Active_Property        : constant Glib.Properties.Property_Boolean;
   Inconsistent_Property  : constant Glib.Properties.Property_Boolean;
   Draw_As_Radio_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  - "toggled"
   --    procedure Handler
   --      (Check_Menu_Item : access Gtk_Check_Menu_Item_Record'Class);
   --
   --  Emitted when the state of the check box is changed.
   --  A signal handler can call Get_Active to discover the new state.
   --  </signals>

   Signal_Toggled : constant Glib.Signal_Name := "toggled";

private
   type Gtk_Check_Menu_Item_Record is new Gtk.Menu_Item.Gtk_Menu_Item_Record
     with null record;

   Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("active");
   Inconsistent_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inconsistent");
   Draw_As_Radio_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("draw-as-radio");

   pragma Import (C, Get_Type, "gtk_check_menu_item_get_type");
end Gtk.Check_Menu_Item;

--  The following subprogram never had a binding, and is now obsolescent
--  No binding: gtk_check_menu_item_set_show_toggle
