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
--
--  This widget is a base class for all menu widgets. It contains a list of
--  items that can be navigated, selected and activated by the user.
--  It can not be instantiated directly.
--
--  A menu is considered "active" when it is displayed on the screen, or, in
--  the case of a menu_bar when one of its menus is active.
--
--  An item is "selected" if it is displayed in a prelight state and its
--  submenu (if any) displayed.
--
--  </description>
--  <c_version>1.3.4</c_version>

with Gtk.Container;
with Gtk.Menu_Item; use Gtk.Menu_Item;

package Gtk.Menu_Shell is

   type Gtk_Menu_Shell_Record is new
     Gtk.Container.Gtk_Container_Record with private;
   type Gtk_Menu_Shell is access all Gtk_Menu_Shell_Record'Class;

   type Gtk_Menu_Direction_Type is
     (Menu_Dir_Parent,
      Menu_Dir_Child,
      Menu_Dir_Next,
      Menu_Dir_Prev);
   --  Direction where to move the selection. See the signal "selection-done"
   --  below.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Menu_Shell.

   procedure Append
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Child      : access Gtk_Menu_Item_Record'Class);
   --  Add a new item at the end of the menu.

   procedure Prepend
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Child      : access Gtk_Menu_Item_Record'Class);
   --  Add a new item at the beginning of the menu

   procedure Insert
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Child      : access Gtk_Menu_Item_Record'Class;
      Position   : Gint);
   --  Add a new item at a specific position in the menu.
   --  The first item is at position 0. To insert as the last item in the menu,
   --  set Position to -1.

   ----------------------
   -- Signals emission --
   ----------------------

   procedure Deactivate (Menu_Shell : access Gtk_Menu_Shell_Record);
   --  Emit the "deactivate" signal.
   --  This deselects the selected item, ungrabs the mouse and keyboard, and
   --  erase the Menu_Shell from the screen.

   procedure Select_Item
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Item       : access Gtk_Menu_Item_Record'Class);
   --  Select a new item in the menu, after deselecting the current item.

   procedure Deselect (Menu_Shell : access Gtk_Menu_Shell_Record);
   --  Deselect the currently selected item.

   procedure Activate_Item
     (Menu_Shell       : access Gtk_Menu_Shell_Record;
      Item             : access Gtk_Menu_Item_Record'Class;
      Force_Deactivate : Boolean);
   --  Activate the item.
   --  If Force_Deactivate is True or the menu_shell sets this property,
   --  Menu_Shell and all its parent menus are deactivated and erased from
   --  the screen.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "deactivate"
   --    procedure Handler (Menu_Shell : access Gtk_Menu_Shell_Record'Class);
   --
   --    Emitted when the menu is deactivated, ie is erased from the screen.
   --
   --  - "selection-done"
   --    procedure Handler (Menu_Shell : access Gtk_Menu_Shell_Record'Class);
   --
   --    Emitted when an item has been selected. The menu shell might not be
   --    activated when the signal is emitted.
   --
   --  - "move_current"
   --    procedure Handler (Menu_Shell : access Gtk_Menu_Shell_Record'Class;
   --                       Direction  : Gtk_Menu_Direction_Type);
   --
   --    An action signal which selects another menu item (given by direction).
   --    In a menu, this is bound by default to the arrow keys to move the
   --    the selection.
   --
   --  - "activate_current"
   --    procedure Handler (Menu_Shell : access Gtk_Menu_Shell_Record'Class;
   --                       Force_Hide : Gboolean);
   --
   --    Activates the current menu item within the Menu_Shell.
   --    if Force_Hide is True, hide the menu afterwards.
   --
   --  - "cancel"
   --    procedure Handler (Menu_Shell : access Gtk_Menu_Shell_Record'Class);
   --
   --    Cancels the selection within the menu_shell. Causes a "selection-done"
   --    signal to be emitted.
   --
   --  </signals>

private
   type Gtk_Menu_Shell_Record is new
     Gtk.Container.Gtk_Container_Record with null record;
   pragma Import (C, Get_Type, "gtk_menu_shell_get_type");
end Gtk.Menu_Shell;
