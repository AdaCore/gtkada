-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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
--  <c_version>1.3.4</c_version>

with Gtk.Menu_Item;

package Gtk.Check_Menu_Item is

   type Gtk_Check_Menu_Item_Record is new
     Gtk.Menu_Item.Gtk_Menu_Item_Record with private;
   type Gtk_Check_Menu_Item is access all Gtk_Check_Menu_Item_Record'Class;

   procedure Gtk_New
     (Check_Menu_Item : out Gtk_Check_Menu_Item;
      Label           : String := "");
   --  Create a new Gtk_Check_Menu_Item with a label, if label isn't null.

   procedure Initialize
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record'Class;
      Label           : String := "");
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Calendar.

   procedure Set_Active
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record;
      Is_Active       : Boolean);
   --  Set the active state of the menu item's check box.

   function Get_Active
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record) return Boolean;
   --  Return True if the Item is active

   procedure Toggled (Check_Menu_Item : access Gtk_Check_Menu_Item_Record);
   --  Emit the "toggled" signal.

   procedure Set_Inconsistent
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record;
      Setting         : Boolean);
   --  If the user has selected a range of elements (such as some text or
   --  spreadsheet cells) that are affected by a boolean setting, and the
   --  current values in that range are inconsistent, you may want to
   --  display the check in an "in between" state. This function turns on
   --  "in between" display.  Normally you would turn off the inconsistent
   --  state again if the user explicitly selects a setting. This has to be
   --  done manually, Set_Inconsistent only affects visual appearance, it
   --  doesn't affect the semantics of the widget.

   function Get_Inconsistent
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record) return Boolean;
   --  Return True if the Item is inconsistent.

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
