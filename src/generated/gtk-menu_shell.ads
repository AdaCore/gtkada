------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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
--  # Terminology
--
--  A menu item can be "selected", this means that it is displayed in the
--  prelight state, and if it has a submenu, that submenu will be popped up.
--
--  A menu is "active" when it is visible onscreen and the user is selecting
--  from it. A menubar is not active until the user clicks on one of its
--  menuitems. When a menu is active, passing the mouse over a submenu will pop
--  it up.
--
--  There is also is a concept of the current menu and a current menu item.
--  The current menu item is the selected menu item that is furthest down in
--  the hierarchy. (Every active menu shell does not necessarily contain a
--  selected menu item, but if it does, then the parent menu shell must also
--  contain a selected menu item.) The current menu is the menu that contains
--  the current menu item. It will always have a GTK grab and receive all key
--  presses.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Menu_Model; use Glib.Menu_Model;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Container;   use Gtk.Container;
with Gtk.Enums;       use Gtk.Enums;
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

   procedure Bind_Model
      (Menu_Shell       : not null access Gtk_Menu_Shell_Record;
       Model            : access Glib.Menu_Model.Gmenu_Model_Record'Class;
       Action_Namespace : UTF8_String := "";
       With_Separators  : Boolean);
   --  Establishes a binding between a Gtk.Menu_Shell.Gtk_Menu_Shell and a
   --  Glib.Menu_Model.Gmenu_Model.
   --  The contents of Shell are removed and then refilled with menu items
   --  according to Model. When Model changes, Shell is updated. Calling this
   --  function twice on Shell with different Model will cause the first
   --  binding to be replaced with a binding to the new model. If Model is null
   --  then any previous binding is undone and all children are removed.
   --  With_Separators determines if toplevel items (eg: sections) have
   --  separators inserted between them. This is typically desired for menus
   --  but doesn't make sense for menubars.
   --  If Action_Namespace is non-null then the effect is as if all actions
   --  mentioned in the Model have their names prefixed with the namespace,
   --  plus a dot. For example, if the action "quit" is mentioned and
   --  Action_Namespace is "app" then the effective action name is "app.quit".
   --  This function uses Gtk.Actionable.Gtk_Actionable to define the action
   --  name and target values on the created menu items. If you want to use an
   --  action group other than "app" and "win", or if you want to use a
   --  Gtk.Menu_Shell.Gtk_Menu_Shell outside of a
   --  Gtk.Application_Window.Gtk_Application_Window, then you will need to
   --  attach your own action group to the widget hierarchy using
   --  gtk_widget_insert_action_group. As an example, if you created a group
   --  with a "quit" action and inserted it with the name "mygroup" then you
   --  would use the action name "mygroup.quit" in your
   --  Glib.Menu_Model.Gmenu_Model.
   --  For most cases you are probably better off using
   --  Gtk.Menu.Gtk_New_From_Model or Gtk.Menu_Bar.Gtk_New_From_Model or just
   --  directly passing the Glib.Menu_Model.Gmenu_Model to
   --  Gtk.Application.Set_App_Menu or Gtk.Application.Set_Menubar.
   --  Since: gtk+ 3.6
   --  "model": the Glib.Menu_Model.Gmenu_Model to bind to or null to remove
   --  binding
   --  "action_namespace": the namespace for actions in Model
   --  "with_separators": True if toplevel items in Shell should have
   --  separators between them

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
   --  Take_Focus property of the submenu needs to be set explicitly.
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
       Position   : Glib.Gint);
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

   type Cb_Gtk_Menu_Shell_Boolean_Void is not null access procedure
     (Self       : access Gtk_Menu_Shell_Record'Class;
      Force_Hide : Boolean);

   type Cb_GObject_Boolean_Void is not null access procedure
     (Self       : access Glib.Object.GObject_Record'Class;
      Force_Hide : Boolean);

   Signal_Activate_Current : constant Glib.Signal_Name := "activate-current";
   procedure On_Activate_Current
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_Gtk_Menu_Shell_Boolean_Void;
       After : Boolean := False);
   procedure On_Activate_Current
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_GObject_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  An action signal that activates the current menu item within the menu
   --  shell.

   type Cb_Gtk_Menu_Shell_Void is not null access procedure (Self : access Gtk_Menu_Shell_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Cancel : constant Glib.Signal_Name := "cancel";
   procedure On_Cancel
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_Gtk_Menu_Shell_Void;
       After : Boolean := False);
   procedure On_Cancel
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  An action signal which cancels the selection within the menu shell.
   --  Causes the Gtk.Menu_Shell.Gtk_Menu_Shell::selection-done signal to be
   --  emitted.

   type Cb_Gtk_Menu_Shell_Gtk_Direction_Type_Void is not null access procedure
     (Self      : access Gtk_Menu_Shell_Record'Class;
      Direction : Gtk.Enums.Gtk_Direction_Type);

   type Cb_GObject_Gtk_Direction_Type_Void is not null access procedure
     (Self      : access Glib.Object.GObject_Record'Class;
      Direction : Gtk.Enums.Gtk_Direction_Type);

   Signal_Cycle_Focus : constant Glib.Signal_Name := "cycle-focus";
   procedure On_Cycle_Focus
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_Gtk_Menu_Shell_Gtk_Direction_Type_Void;
       After : Boolean := False);
   procedure On_Cycle_Focus
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_GObject_Gtk_Direction_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  A keybinding signal which moves the focus in the given Direction.

   Signal_Deactivate : constant Glib.Signal_Name := "deactivate";
   procedure On_Deactivate
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_Gtk_Menu_Shell_Void;
       After : Boolean := False);
   procedure On_Deactivate
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when a menu shell is deactivated.

   type Cb_Gtk_Menu_Shell_Gtk_Widget_Gint_Void is not null access procedure
     (Self     : access Gtk_Menu_Shell_Record'Class;
      Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Position : Glib.Gint);

   type Cb_GObject_Gtk_Widget_Gint_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Position : Glib.Gint);

   Signal_Insert : constant Glib.Signal_Name := "insert";
   procedure On_Insert
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_Gtk_Menu_Shell_Gtk_Widget_Gint_Void;
       After : Boolean := False);
   procedure On_Insert
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_GObject_Gtk_Widget_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::insert signal is emitted when a new Gtk.Menu_Item.Gtk_Menu_Item
   --  is added to a Gtk.Menu_Shell.Gtk_Menu_Shell. A separate signal is used
   --  instead of GtkContainer::add because of the need for an additional
   --  position parameter.
   --
   --  The inverse of this signal is the GtkContainer::removed signal.
   -- 
   --  Callback parameters:
   --    --  "child": the Gtk.Menu_Item.Gtk_Menu_Item that is being inserted
   --    --  "position": the position at which the insert occurs

   type Cb_Gtk_Menu_Shell_Gtk_Menu_Direction_Type_Void is not null access procedure
     (Self      : access Gtk_Menu_Shell_Record'Class;
      Direction : Gtk.Enums.Gtk_Menu_Direction_Type);

   type Cb_GObject_Gtk_Menu_Direction_Type_Void is not null access procedure
     (Self      : access Glib.Object.GObject_Record'Class;
      Direction : Gtk.Enums.Gtk_Menu_Direction_Type);

   Signal_Move_Current : constant Glib.Signal_Name := "move-current";
   procedure On_Move_Current
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_Gtk_Menu_Shell_Gtk_Menu_Direction_Type_Void;
       After : Boolean := False);
   procedure On_Move_Current
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_GObject_Gtk_Menu_Direction_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  An keybinding signal which moves the current menu item in the direction
   --  specified by Direction.

   type Cb_Gtk_Menu_Shell_Gint_Boolean is not null access function
     (Self     : access Gtk_Menu_Shell_Record'Class;
      Distance : Glib.Gint) return Boolean;

   type Cb_GObject_Gint_Boolean is not null access function
     (Self     : access Glib.Object.GObject_Record'Class;
      Distance : Glib.Gint) return Boolean;

   Signal_Move_Selected : constant Glib.Signal_Name := "move-selected";
   procedure On_Move_Selected
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_Gtk_Menu_Shell_Gint_Boolean;
       After : Boolean := False);
   procedure On_Move_Selected
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_GObject_Gint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::move-selected signal is emitted to move the selection to another
   --  item.
   -- 
   --  Callback parameters:
   --    --  "distance": +1 to move to the next item, -1 to move to the previous
   --    --  Returns True to stop the signal emission, False to continue

   Signal_Selection_Done : constant Glib.Signal_Name := "selection-done";
   procedure On_Selection_Done
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_Gtk_Menu_Shell_Void;
       After : Boolean := False);
   procedure On_Selection_Done
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when a selection has been completed within a
   --  menu shell.

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

private
   Take_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("take-focus");
end Gtk.Menu_Shell;
