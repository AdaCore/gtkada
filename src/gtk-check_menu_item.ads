-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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
--  A Gtk_Check_Menu_Item is a menu item that maintains the state of a boolean
--  value in addition to a Gtk_Menu_Item's usual role in activating application
--  code.
--
--  A check box indicating the state of the boolean value is displayed at the
--  left side of the Gtk_Menu_Item. Activating the Gtk_Menu_Item toggles the
--  value.
--  </description>
--  <c_version>1.2.8</c_version>

with Gtk.Menu_Item;

package Gtk.Check_Menu_Item is

   type Gtk_Check_Menu_Item_Record is new Gtk.Menu_Item.Gtk_Menu_Item_Record
     with private;
   type Gtk_Check_Menu_Item is access all Gtk_Check_Menu_Item_Record'Class;

   procedure Gtk_New
     (Check_Menu_Item : out Gtk_Check_Menu_Item;
      Label  : in String := "");
   --  Create a new Gtk_Check_Menu_Item with a label, if label isn't null.

   procedure Initialize
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record'Class;
      Label  : in String := "");
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Calendar.

   procedure Set_Show_Toggle
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record;
      Always          : in Boolean);
   --  Control whether the check box is shown at all times.
   --  Normally the check box is shown only when it is active or while the
   --  menu item is selected.

   procedure Set_Always_Show_Toggle
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record;
      Always          : in Boolean);
   --  Same as Set_Show_Toggle.
   --  Provided to simplify automated tools, such as Gate.

   procedure Set_Active
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record;
      Is_Active       : in Boolean);
   --  Set the active state of the menu item's check box.

   procedure Toggled (Check_Menu_Item : access Gtk_Check_Menu_Item_Record);
   --  Emit the "toggled" signal.

   function Get_Active
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record) return Boolean;
   --  Return True if the Item is active

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

private
   type Gtk_Check_Menu_Item_Record is new Gtk.Menu_Item.Gtk_Menu_Item_Record
     with null record;

   pragma Import (C, Get_Type, "gtk_check_menu_item_get_type");
end Gtk.Check_Menu_Item;
