-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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
--
--  This widget is a base class for all menu widgets. It contains a list of
--  items that can be navigated, selected and activated by the user.
--  It can not be instanciated directly.
--
--  A menu is considered "active" when it is displayed on the screen, or, in
--  the case of a menu_bar when one of its menus is active.
--
--  An item is "selected" if it is displayed in a prelight state and its
--  submenu (if any) displayed.
--
--  </description>
--  <c_version>1.2.6</c_version>

with Gtk.Object;
with Gtk.Container;
with Gtk.Menu_Item;

package Gtk.Menu_Shell is

   type Gtk_Menu_Shell_Record is new Gtk.Container.Gtk_Container_Record
     with private;
   type Gtk_Menu_Shell is access all Gtk_Menu_Shell_Record'Class;

   type Gtk_Menu_Direction_Type is (Menu_Dir_Parent,
                                    Menu_Dir_Child,
                                    Menu_Dir_Next,
                                    Menu_Dir_Prev);
   --  Direction where to move the selection. See the signal "selection-done"
   --  below.

   procedure Append
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Child      : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class);
   --  Adds a new item at the end of the menu.

   procedure Prepend
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Child      : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class);
   --  Adds a new item at the beginning of the menu

   procedure Insert
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Child      : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      Position   : in Gint);
   --  Adds a new item at a specific position in the menu.
   --  The first item is at position 0. To insert as the last item in the menu,
   --  set POSITION to -1.

   procedure Select_Item
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Item       : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class);
   --  Selects a new item in the menu, after deselecting the current item.

   procedure Deselect (Menu_Shell : access Gtk_Menu_Shell_Record);
   --  Deselects the currently selected item.

   procedure Activate_Item
     (Menu_Shell       : access Gtk_Menu_Shell_Record;
      Item             : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      Force_Deactivate : Boolean);
   --  Activates the item.
   --  If FORCE_DEACTIVATE is True or the menu_shell sets this property,
   --  MENU_SHELL and all its parent menus are deactivated and erased from
   --  the screen.

   ----------------------
   -- Signals emission --
   ----------------------

   procedure Deactivate (Menu_Shell : access Gtk_Menu_Shell_Record);
   --  Emits the "deactivate" signal.
   --  This deselects the selected item, ungrabs the mouse and keyboard, and
   --  erase the MENU_SHELL from the screen.

   ----------------------------
   -- Support for GATE/DGATE --
   ----------------------------

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type)
     renames Gtk.Container.Generate;
   --  Gate internal function

   procedure Generate (Menu_Shell : in out Gtk.Object.Gtk_Object;
                       N : in Node_Ptr)
     renames Gtk.Container.Generate;
   --  Dgate internal function

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
   --    if FORCE_HIDE is True, hide the menu afterwards.
   --
   --  - "cancel"
   --    procedure Handler (Menu_Shell : access Gtk_Menu_Shell_Record'Class);
   --
   --    Cancels the selection within the menu_shell. Causes a "selection-done"
   --    signal to be emitted.
   --
   --  </signals>

private
   type Gtk_Menu_Shell_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

end Gtk.Menu_Shell;
