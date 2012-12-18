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
--  A Gtk.Menu_Shell.Gtk_Menu_Shell is the abstract base class used to derive
--  the Gtk.Menu.Gtk_Menu and Gtk.Menu_Bar.Gtk_Menu_Bar subclasses.
--
--  A Gtk.Menu_Shell.Gtk_Menu_Shell is a container of
--  Gtk.Menu_Item.Gtk_Menu_Item objects arranged in a list which can be
--  navigated, selected, and activated by the user to perform application
--  functions. A Gtk.Menu_Item.Gtk_Menu_Item can have a submenu associated with
--  it, allowing for nested hierarchical menus.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Container;   use Gtk.Container;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Menu_Shell is

   type Gtk_Menu_Shell_Record is new Gtk_Container_Record with null record;
   type Gtk_Menu_Shell is access all Gtk_Menu_Shell_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_menu_shell_get_type");

   -------------
   -- Methods --
   -------------

   procedure Activate_Item
      (Menu_Shell       : not null access Gtk_Menu_Shell_Record;
       Menu_Item        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Force_Deactivate : Boolean);
   --  Activates the menu item within the menu shell.
   --  "menu_item": the Gtk.Menu_Item.Gtk_Menu_Item to activate
   --  "force_deactivate": if True, force the deactivation of the menu shell
   --  after the menu item is activated

   procedure Append
      (Menu_Shell : not null access Gtk_Menu_Shell_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds a new Gtk.Menu_Item.Gtk_Menu_Item to the end of the menu shell's
   --  item list.
   --  "child": The Gtk.Menu_Item.Gtk_Menu_Item to add

   procedure Cancel (Menu_Shell : not null access Gtk_Menu_Shell_Record);
   --  Cancels the selection within the menu shell.
   --  Since: gtk+ 2.4

   procedure Deactivate (Menu_Shell : not null access Gtk_Menu_Shell_Record);
   --  Deactivates the menu shell.
   --  Typically this results in the menu shell being erased from the screen.

   procedure Deselect (Menu_Shell : not null access Gtk_Menu_Shell_Record);
   --  Deselects the currently selected item from the menu shell, if any.

   function Get_Parent_Shell
      (Menu_Shell : not null access Gtk_Menu_Shell_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the parent menu shell.
   --  The parent menu shell of a submenu is the Gtk.Menu.Gtk_Menu or
   --  Gtk.Menu_Bar.Gtk_Menu_Bar from which it was opened up.
   --  Since: gtk+ 3.0

   function Get_Selected_Item
      (Menu_Shell : not null access Gtk_Menu_Shell_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the currently selected item.
   --  Since: gtk+ 3.0

   function Get_Take_Focus
      (Menu_Shell : not null access Gtk_Menu_Shell_Record) return Boolean;
   --  Returns True if the menu shell will take the keyboard focus on popup.
   --  Since: gtk+ 2.8

   procedure Set_Take_Focus
      (Menu_Shell : not null access Gtk_Menu_Shell_Record;
       Take_Focus : Boolean);
   --  If Take_Focus is True (the default) the menu shell will take the
   --  keyboard focus so that it will receive all keyboard events which is
   --  needed to enable keyboard navigation in menus.
   --  Setting Take_Focus to False is useful only for special applications
   --  like virtual keyboard implementations which should not take keyboard
   --  focus.
   --  The Take_Focus state of a menu or menu bar is automatically propagated
   --  to submenus whenever a submenu is popped up, so you don't have to worry
   --  about recursively setting it for your entire menu hierarchy. Only when
   --  programmatically picking a submenu and popping it up manually, the
   --  Take_Focus property of the submenu needs to be set explicitely.
   --  Note that setting it to False has side-effects:
   --  If the focus is in some other app, it keeps the focus and keynav in the
   --  menu doesn't work. Consequently, keynav on the menu will only work if
   --  the focus is on some toplevel owned by the onscreen keyboard.
   --  To avoid confusing the user, menus with Take_Focus set to False should
   --  not display mnemonics or accelerators, since it cannot be guaranteed
   --  that they will work.
   --  See also gdk_keyboard_grab
   --  Since: gtk+ 2.8
   --  "take_focus": True if the menu shell should take the keyboard focus on
   --  popup

   procedure Insert
      (Menu_Shell : not null access Gtk_Menu_Shell_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position   : Gint);
   --  Adds a new Gtk.Menu_Item.Gtk_Menu_Item to the menu shell's item list at
   --  the position indicated by Position.
   --  "child": The Gtk.Menu_Item.Gtk_Menu_Item to add
   --  "position": The position in the item list where Child is added.
   --  Positions are numbered from 0 to n-1

   procedure Prepend
      (Menu_Shell : not null access Gtk_Menu_Shell_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds a new Gtk.Menu_Item.Gtk_Menu_Item to the beginning of the menu
   --  shell's item list.
   --  "child": The Gtk.Menu_Item.Gtk_Menu_Item to add

   procedure Select_First
      (Menu_Shell       : not null access Gtk_Menu_Shell_Record;
       Search_Sensitive : Boolean);
   --  Select the first visible or selectable child of the menu shell; don't
   --  select tearoff items unless the only item is a tearoff item.
   --  Since: gtk+ 2.2
   --  "search_sensitive": if True, search for the first selectable menu item,
   --  otherwise select nothing if the first item isn't sensitive. This should
   --  be False if the menu is being popped up initially.

   procedure Select_Item
      (Menu_Shell : not null access Gtk_Menu_Shell_Record;
       Menu_Item  : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Selects the menu item from the menu shell.
   --  "menu_item": The Gtk.Menu_Item.Gtk_Menu_Item to select

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Menu_Shell_Record, Gtk_Menu_Shell);
   function "+"
     (Widget : access Gtk_Menu_Shell_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Menu_Shell
   renames Implements_Gtk_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Take_Focus_Property : constant Glib.Properties.Property_Boolean;
   --  A boolean that determines whether the menu and its submenus grab the
   --  keyboard focus. See Gtk.Menu_Shell.Set_Take_Focus and
   --  Gtk.Menu_Shell.Get_Take_Focus.

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "activate-current"
   --     procedure Handler
   --       (Self       : access Gtk_Menu_Shell_Record'Class;
   --        Force_Hide : Boolean);
   --    --  "force_hide": if True, hide the menu after activating the menu item
   --  An action signal that activates the current menu item within the menu
   --  shell.
   --
   --  "cancel"
   --     procedure Handler (Self : access Gtk_Menu_Shell_Record'Class);
   --  An action signal which cancels the selection within the menu shell.
   --  Causes the Gtk.Menu_Shell.Gtk_Menu_Shell::selection-done signal to be
   --  emitted.
   --
   --  "cycle-focus"
   --     procedure Handler
   --       (Self      : access Gtk_Menu_Shell_Record'Class;
   --        Direction : Gtk.Enums.Gtk_Direction_Type);
   --    --  "direction": the direction to cycle in
   --  A keybinding signal which moves the focus in the given Direction.
   --
   --  "deactivate"
   --     procedure Handler (Self : access Gtk_Menu_Shell_Record'Class);
   --  This signal is emitted when a menu shell is deactivated.
   --
   --  "insert"
   --     procedure Handler
   --       (Self     : access Gtk_Menu_Shell_Record'Class;
   --        Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
   --        Position : Gint);
   --    --  "child": the Gtk.Menu_Item.Gtk_Menu_Item that is being inserted
   --    --  "position": the position at which the insert occurs
   --  The ::insert signal is emitted when a new Gtk.Menu_Item.Gtk_Menu_Item
   --  is added to a Gtk.Menu_Shell.Gtk_Menu_Shell. A separate signal is used
   --  instead of GtkContainer::add because of the need for an additional
   --  position parameter.
   --
   --  The inverse of this signal is the GtkContainer::removed signal.
   --
   --  "move-current"
   --     procedure Handler
   --       (Self      : access Gtk_Menu_Shell_Record'Class;
   --        Direction : Gtk.Enums.Gtk_Menu_Direction_Type);
   --    --  "direction": the direction to move
   --  An keybinding signal which moves the current menu item in the direction
   --  specified by Direction.
   --
   --  "move-selected"
   --     function Handler
   --       (Self     : access Gtk_Menu_Shell_Record'Class;
   --        Distance : Gint) return Boolean;
   --    --  "distance": +1 to move to the next item, -1 to move to the previous
   --  The ::move-selected signal is emitted to move the selection to another
   --  item.
   --
   --  Returns True to stop the signal emission, False to continue
   --
   --  "selection-done"
   --     procedure Handler (Self : access Gtk_Menu_Shell_Record'Class);
   --  This signal is emitted when a selection has been completed within a
   --  menu shell.

   Signal_Activate_Current : constant Glib.Signal_Name := "activate-current";
   Signal_Cancel : constant Glib.Signal_Name := "cancel";
   Signal_Cycle_Focus : constant Glib.Signal_Name := "cycle-focus";
   Signal_Deactivate : constant Glib.Signal_Name := "deactivate";
   Signal_Insert : constant Glib.Signal_Name := "insert";
   Signal_Move_Current : constant Glib.Signal_Name := "move-current";
   Signal_Move_Selected : constant Glib.Signal_Name := "move-selected";
   Signal_Selection_Done : constant Glib.Signal_Name := "selection-done";

private
   Take_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("take-focus");
end Gtk.Menu_Shell;
