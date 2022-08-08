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
--  A Gtk.Menu.Gtk_Menu is a Gtk.Menu_Shell.Gtk_Menu_Shell that implements a
--  drop down menu consisting of a list of Gtk.Menu_Item.Gtk_Menu_Item objects
--  which can be navigated and activated by the user to perform application
--  functions.
--
--  A Gtk.Menu.Gtk_Menu is most commonly dropped down by activating a
--  Gtk.Menu_Item.Gtk_Menu_Item in a Gtk.Menu_Bar.Gtk_Menu_Bar or popped up by
--  activating a Gtk.Menu_Item.Gtk_Menu_Item in another Gtk.Menu.Gtk_Menu.
--
--  A Gtk.Menu.Gtk_Menu can also be popped up by activating a
--  Gtk.Combo_Box.Gtk_Combo_Box. Other composite widgets such as the
--  Gtk.Notebook.Gtk_Notebook can pop up a Gtk.Menu.Gtk_Menu as well.
--
--  Applications can display a Gtk.Menu.Gtk_Menu as a popup menu by calling
--  the Gtk.Menu.Popup function. The example below shows how an application can
--  pop up a menu when the 3rd mouse button is pressed.
--
--  ## Connecting the popup signal handler.
--
--  |[<!-- language="C" --> // connect our handler which will popup the menu
--  g_signal_connect_swapped (window, "button_press_event", G_CALLBACK
--  (my_popup_handler), menu); ]|
--
--  ## Signal handler which displays a popup menu.
--
--  |[<!-- language="C" --> static gint my_popup_handler (GtkWidget *widget,
--  GdkEvent *event) { GtkMenu *menu; GdkEventButton *event_button;
--
--  g_return_val_if_fail (widget != NULL, FALSE); g_return_val_if_fail
--  (GTK_IS_MENU (widget), FALSE); g_return_val_if_fail (event != NULL, FALSE);
--
--  // The "widget" is the menu that was supplied when //
--  g_signal_connect_swapped was called. menu = GTK_MENU (widget);
--
--  if (event->type == GDK_BUTTON_PRESS) { event_button = (GdkEventButton *)
--  event; if (event_button->button == GDK_BUTTON_SECONDARY) { gtk_menu_popup
--  (menu, NULL, NULL, NULL, NULL, event_button->button, event_button->time);
--  return TRUE; } }
--
--  return FALSE; } ]|
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> menu ├── arrow.top ├── <child> ┊ ├── <child>
--  ╰── arrow.bottom ]|
--
--  The main CSS node of GtkMenu has name menu, and there are two subnodes
--  with name arrow, for scrolling menu arrows. These subnodes get the .top and
--  .bottom style classes.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk;             use Gdk;
with Gdk.Device;      use Gdk.Device;
with Gdk.Event;       use Gdk.Event;
with Gdk.Monitor;     use Gdk.Monitor;
with Gdk.Rectangle;   use Gdk.Rectangle;
with Gdk.Screen;      use Gdk.Screen;
with Gdk.Window;      use Gdk.Window;
with Glib;            use Glib;
with Glib.Menu_Model; use Glib.Menu_Model;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Menu_Item;   use Gtk.Menu_Item;
with Gtk.Menu_Shell;  use Gtk.Menu_Shell;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Menu is

   type Gtk_Menu_Record is new Gtk_Menu_Shell_Record with null record;
   type Gtk_Menu is access all Gtk_Menu_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Menu_Detach_Func is access procedure (Attach_Widget : System.Address; Menu : System.Address);
   --  A user function supplied when calling Gtk.Menu.Attach_To_Widget which
   --  will be called when the menu is later detached from the widget.
   --  "attach_widget": the Gtk.Widget.Gtk_Widget that the menu is being
   --  detached from.
   --  "menu": the Gtk.Menu.Gtk_Menu being detached.

   pragma Convention (C, Gtk_Menu_Detach_Func);

   type Gtk_Menu_Position_Func is access procedure
     (Menu    : not null access Gtk_Menu_Record'Class;
      X       : out Glib.Gint;
      Y       : out Glib.Gint;
      Push_In : out Boolean);
   --  A user function supplied when calling Gtk.Menu.Popup which controls the
   --  positioning of the menu when it is displayed. The function sets the X
   --  and Y parameters to the coordinates where the menu is to be drawn. To
   --  make the menu appear on a different monitor than the mouse pointer,
   --  gtk_menu_set_monitor must be called.
   --  "menu": a Gtk.Menu.Gtk_Menu.
   --  "x": address of the Glib.Gint representing the horizontal position
   --  where the menu shall be drawn.
   --  "y": address of the Glib.Gint representing the vertical position where
   --  the menu shall be drawn. This is an output parameter.
   --  "push_in": This parameter controls how menus placed outside the monitor
   --  are handled. If this is set to True and part of the menu is outside the
   --  monitor then GTK+ pushes the window into the visible area, effectively
   --  modifying the popup position. Note that moving and possibly resizing the
   --  menu around will alter the scroll position to keep the menu items "in
   --  place", i.e. at the same monitor position they would have been without
   --  resizing. In practice, this behavior is only useful for combobox popups
   --  or option menus and cannot be used to simply confine a menu to monitor
   --  boundaries. In that case, changing the scroll offset is not desirable.

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Menu : out Gtk_Menu);
   procedure Initialize (Menu : not null access Gtk_Menu_Record'Class);
   --  Creates a new Gtk.Menu.Gtk_Menu
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Menu_New return Gtk_Menu;
   --  Creates a new Gtk.Menu.Gtk_Menu

   procedure Gtk_New_From_Model
      (Menu  : out Gtk_Menu;
       Model : not null access Glib.Menu_Model.Gmenu_Model_Record'Class);
   procedure Initialize_From_Model
      (Menu  : not null access Gtk_Menu_Record'Class;
       Model : not null access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Creates a Gtk.Menu.Gtk_Menu and populates it with menu items and
   --  submenus according to Model.
   --  The created menu items are connected to actions found in the
   --  Gtk.Application_Window.Gtk_Application_Window to which the menu belongs
   --  - typically by means of being attached to a widget (see
   --  Gtk.Menu.Attach_To_Widget) that is contained within the
   --  Gtk_Application_Windows widget hierarchy.
   --  Actions can also be added using gtk_widget_insert_action_group on the
   --  menu's attach widget or on any of its parent widgets.
   --  Since: gtk+ 3.4
   --  Initialize_From_Model does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "model": a Glib.Menu_Model.Gmenu_Model

   function Gtk_Menu_New_From_Model
      (Model : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
       return Gtk_Menu;
   --  Creates a Gtk.Menu.Gtk_Menu and populates it with menu items and
   --  submenus according to Model.
   --  The created menu items are connected to actions found in the
   --  Gtk.Application_Window.Gtk_Application_Window to which the menu belongs
   --  - typically by means of being attached to a widget (see
   --  Gtk.Menu.Attach_To_Widget) that is contained within the
   --  Gtk_Application_Windows widget hierarchy.
   --  Actions can also be added using gtk_widget_insert_action_group on the
   --  menu's attach widget or on any of its parent widgets.
   --  Since: gtk+ 3.4
   --  "model": a Glib.Menu_Model.Gmenu_Model

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_menu_get_type");

   -------------
   -- Methods --
   -------------

   procedure Attach
      (Menu          : not null access Gtk_Menu_Record;
       Child         : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Left_Attach   : Guint;
       Right_Attach  : Guint;
       Top_Attach    : Guint;
       Bottom_Attach : Guint);
   --  Adds a new Gtk.Menu_Item.Gtk_Menu_Item to a (table) menu. The number of
   --  "cells" that an item will occupy is specified by Left_Attach,
   --  Right_Attach, Top_Attach and Bottom_Attach. These each represent the
   --  leftmost, rightmost, uppermost and lower column and row numbers of the
   --  table. (Columns and rows are indexed from zero).
   --  Note that this function is not related to Gtk.Menu.Detach.
   --  Since: gtk+ 2.4
   --  "child": a Gtk.Menu_Item.Gtk_Menu_Item
   --  "left_attach": The column number to attach the left side of the item to
   --  "right_attach": The column number to attach the right side of the item
   --  to
   --  "top_attach": The row number to attach the top of the item to
   --  "bottom_attach": The row number to attach the bottom of the item to

   procedure Attach_To_Widget
      (Menu          : not null access Gtk_Menu_Record;
       Attach_Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Detacher      : Gtk_Menu_Detach_Func);
   --  Attaches the menu to the widget and provides a callback function that
   --  will be invoked when the menu calls Gtk.Menu.Detach during its
   --  destruction.
   --  If the menu is attached to the widget then it will be destroyed when
   --  the widget is destroyed, as if it was a child widget. An attached menu
   --  will also move between screens correctly if the widgets moves between
   --  screens.
   --  "attach_widget": the Gtk.Widget.Gtk_Widget that the menu will be
   --  attached to
   --  "detacher": the user supplied callback function that will be called
   --  when the menu calls Gtk.Menu.Detach

   procedure Detach (Menu : not null access Gtk_Menu_Record);
   --  Detaches the menu from the widget to which it had been attached. This
   --  function will call the callback function, Detacher, provided when the
   --  Gtk.Menu.Attach_To_Widget function was called.

   function Get_Accel_Group
      (Menu : not null access Gtk_Menu_Record)
       return Gtk.Accel_Group.Gtk_Accel_Group;
   --  Gets the Gtk.Accel_Group.Gtk_Accel_Group which holds global
   --  accelerators for the menu. See Gtk.Menu.Set_Accel_Group.

   procedure Set_Accel_Group
      (Menu        : not null access Gtk_Menu_Record;
       Accel_Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class);
   --  Set the Gtk.Accel_Group.Gtk_Accel_Group which holds global accelerators
   --  for the menu. This accelerator group needs to also be added to all
   --  windows that this menu is being used in with Gtk.Window.Add_Accel_Group,
   --  in order for those windows to support all the accelerators contained in
   --  this group.
   --  "accel_group": the Gtk.Accel_Group.Gtk_Accel_Group to be associated
   --  with the menu.

   function Get_Accel_Path
      (Menu : not null access Gtk_Menu_Record) return UTF8_String;
   --  Retrieves the accelerator path set on the menu.
   --  Since: gtk+ 2.14

   procedure Set_Accel_Path
      (Menu       : not null access Gtk_Menu_Record;
       Accel_Path : UTF8_String := "");
   --  Sets an accelerator path for this menu from which accelerator paths for
   --  its immediate children, its menu items, can be constructed. The main
   --  purpose of this function is to spare the programmer the inconvenience of
   --  having to call Gtk.Menu_Item.Set_Accel_Path on each menu item that
   --  should support runtime user changable accelerators. Instead, by just
   --  calling Gtk.Menu.Set_Accel_Path on their parent, each menu item of this
   --  menu, that contains a label describing its purpose, automatically gets
   --  an accel path assigned.
   --  For example, a menu containing menu items "New" and "Exit", will, after
   --  `gtk_menu_set_accel_path (menu, "<Gnumeric-Sheet>/File");` has been
   --  called, assign its items the accel paths: `"<Gnumeric-Sheet>/File/New"`
   --  and `"<Gnumeric-Sheet>/File/Exit"`.
   --  Assigning accel paths to menu items then enables the user to change
   --  their accelerators at runtime. More details about accelerator paths and
   --  their default setups can be found at Gtk.Accel_Map.Add_Entry.
   --  Note that Accel_Path string will be stored in a Glib.GQuark. Therefore,
   --  if you pass a static string, you can save some memory by interning it
   --  first with g_intern_static_string.
   --  "accel_path": a valid accelerator path, or null to unset the path

   function Get_Active
      (Menu : not null access Gtk_Menu_Record)
       return Gtk.Menu_Item.Gtk_Menu_Item;
   --  Returns the selected menu item from the menu. This is used by the
   --  Gtk.Combo_Box.Gtk_Combo_Box.

   procedure Set_Active
      (Menu  : not null access Gtk_Menu_Record;
       Index : Guint);
   --  Selects the specified menu item within the menu. This is used by the
   --  Gtk.Combo_Box.Gtk_Combo_Box and should not be used by anyone else.
   --  "index": the index of the menu item to select. Index values are from 0
   --  to n-1

   function Get_Attach_Widget
      (Menu : not null access Gtk_Menu_Record) return Gtk.Widget.Gtk_Widget;
   --  Returns the Gtk.Widget.Gtk_Widget that the menu is attached to.

   function Get_Monitor
      (Menu : not null access Gtk_Menu_Record) return Glib.Gint;
   --  Retrieves the number of the monitor on which to show the menu.
   --  Since: gtk+ 2.14

   procedure Set_Monitor
      (Menu        : not null access Gtk_Menu_Record;
       Monitor_Num : Glib.Gint);
   --  Informs GTK+ on which monitor a menu should be popped up. See
   --  Gdk.Monitor.Get_Geometry.
   --  This function should be called from a Gtk_Menu_Position_Func if the
   --  menu should not appear on the same monitor as the pointer. This
   --  information can't be reliably inferred from the coordinates returned by
   --  a Gtk_Menu_Position_Func, since, for very long menus, these coordinates
   --  may extend beyond the monitor boundaries or even the screen boundaries.
   --  Since: gtk+ 2.4
   --  "monitor_num": the number of the monitor on which the menu should be
   --  popped up

   function Get_Reserve_Toggle_Size
      (Menu : not null access Gtk_Menu_Record) return Boolean;
   --  Returns whether the menu reserves space for toggles and icons,
   --  regardless of their actual presence.
   --  Since: gtk+ 2.18

   procedure Set_Reserve_Toggle_Size
      (Menu                : not null access Gtk_Menu_Record;
       Reserve_Toggle_Size : Boolean);
   --  Sets whether the menu should reserve space for drawing toggles or
   --  icons, regardless of their actual presence.
   --  Since: gtk+ 2.18
   --  "reserve_toggle_size": whether to reserve size for toggles

   function Get_Tearoff_State
      (Menu : not null access Gtk_Menu_Record) return Boolean;
   pragma Obsolescent (Get_Tearoff_State);
   --  Returns whether the menu is torn off. See Gtk.Menu.Set_Tearoff_State.
   --  Deprecated since 3.10, 1

   procedure Set_Tearoff_State
      (Menu     : not null access Gtk_Menu_Record;
       Torn_Off : Boolean);
   pragma Obsolescent (Set_Tearoff_State);
   --  Changes the tearoff state of the menu. A menu is normally displayed as
   --  drop down menu which persists as long as the menu is active. It can also
   --  be displayed as a tearoff menu which persists until it is closed or
   --  reattached.
   --  Deprecated since 3.10, 1
   --  "torn_off": If True, menu is displayed as a tearoff menu.

   function Get_Title
      (Menu : not null access Gtk_Menu_Record) return UTF8_String;
   pragma Obsolescent (Get_Title);
   --  Returns the title of the menu. See Gtk.Menu.Set_Title.
   --  Deprecated since 3.10, 1

   procedure Set_Title
      (Menu  : not null access Gtk_Menu_Record;
       Title : UTF8_String := "");
   pragma Obsolescent (Set_Title);
   --  Sets the title string for the menu.
   --  The title is displayed when the menu is shown as a tearoff menu. If
   --  Title is null, the menu will see if it is attached to a parent menu
   --  item, and if so it will try to use the same text as that menu item's
   --  label.
   --  Deprecated since 3.10, 1
   --  "title": a string containing the title for the menu, or null to inherit
   --  the title of the parent menu item, if any

   procedure Place_On_Monitor
      (Menu    : not null access Gtk_Menu_Record;
       Monitor : not null access Gdk.Monitor.Gdk_Monitor_Record'Class);
   --  Places Menu on the given monitor.
   --  Since: gtk+ 3.22
   --  "monitor": the monitor to place the menu on

   procedure Popdown (Menu : not null access Gtk_Menu_Record);
   --  Removes the menu from the screen.

   procedure Popup
      (Menu              : not null access Gtk_Menu_Record;
       Parent_Menu_Shell : Gtk.Menu_Shell.Gtk_Menu_Shell := null;
       Parent_Menu_Item  : Gtk.Menu_Item.Gtk_Menu_Item := null;
       Func              : Gtk_Menu_Position_Func := null;
       Button            : Guint := 1;
       Activate_Time     : Guint32 := 0);
   pragma Obsolescent (Popup);
   --  Displays a menu and makes it available for selection.
   --  Applications can use this function to display context-sensitive menus,
   --  and will typically supply null for the Parent_Menu_Shell,
   --  Parent_Menu_Item, Func and Data parameters. The default menu positioning
   --  function will position the menu at the current mouse cursor position.
   --  The Button parameter should be the mouse button pressed to initiate the
   --  menu popup. If the menu popup was initiated by something other than a
   --  mouse button press, such as a mouse button release or a keypress, Button
   --  should be 0.
   --  The Activate_Time parameter is used to conflict-resolve initiation of
   --  concurrent requests for mouse/keyboard grab requests. To function
   --  properly, this needs to be the timestamp of the user event (such as a
   --  mouse click or key press) that caused the initiation of the popup. Only
   --  if no such event is available, Gtk.Main.Get_Current_Event_Time can be
   --  used instead.
   --  Note that this function does not work very well on GDK backends that do
   --  not have global coordinates, such as Wayland or Mir. You should probably
   --  use one of the gtk_menu_popup_at_ variants, which do not have this
   --  problem.
   --  Deprecated since 3.22, 1
   --  "parent_menu_shell": the menu shell containing the triggering menu
   --  item, or null
   --  "parent_menu_item": the menu item whose activation triggered the popup,
   --  or null
   --  "func": a user supplied function used to position the menu, or null
   --  "button": the mouse button which was pressed to initiate the event.
   --  "activate_time": the time at which the activation event occurred.

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Popup_User_Data is

      type Gtk_Menu_Position_Func is access procedure
        (Menu      : not null access Gtk.Menu.Gtk_Menu_Record'Class;
         X         : out Glib.Gint;
         Y         : out Glib.Gint;
         Push_In   : out Boolean;
         User_Data : User_Data_Type);
      --  A user function supplied when calling Gtk.Menu.Popup which controls the
      --  positioning of the menu when it is displayed. The function sets the X
      --  and Y parameters to the coordinates where the menu is to be drawn. To
      --  make the menu appear on a different monitor than the mouse pointer,
      --  gtk_menu_set_monitor must be called.
      --  "menu": a Gtk.Menu.Gtk_Menu.
      --  "x": address of the Glib.Gint representing the horizontal position
      --  where the menu shall be drawn.
      --  "y": address of the Glib.Gint representing the vertical position where
      --  the menu shall be drawn. This is an output parameter.
      --  "push_in": This parameter controls how menus placed outside the monitor
      --  are handled. If this is set to True and part of the menu is outside the
      --  monitor then GTK+ pushes the window into the visible area, effectively
      --  modifying the popup position. Note that moving and possibly resizing the
      --  menu around will alter the scroll position to keep the menu items "in
      --  place", i.e. at the same monitor position they would have been without
      --  resizing. In practice, this behavior is only useful for combobox popups
      --  or option menus and cannot be used to simply confine a menu to monitor
      --  boundaries. In that case, changing the scroll offset is not desirable.
      --  "user_data": the data supplied by the user in the Gtk.Menu.Popup Data
      --  parameter.

      procedure Popup
         (Menu              : not null access Gtk.Menu.Gtk_Menu_Record'Class;
          Parent_Menu_Shell : Gtk.Menu_Shell.Gtk_Menu_Shell := null;
          Parent_Menu_Item  : Gtk.Menu_Item.Gtk_Menu_Item := null;
          Func              : Gtk_Menu_Position_Func := null;
          Data              : User_Data_Type;
          Button            : Guint := 1;
          Activate_Time     : Guint32 := 0);
      pragma Obsolescent (Popup);
      --  Displays a menu and makes it available for selection.
      --  Applications can use this function to display context-sensitive
      --  menus, and will typically supply null for the Parent_Menu_Shell,
      --  Parent_Menu_Item, Func and Data parameters. The default menu
      --  positioning function will position the menu at the current mouse
      --  cursor position.
      --  The Button parameter should be the mouse button pressed to initiate
      --  the menu popup. If the menu popup was initiated by something other
      --  than a mouse button press, such as a mouse button release or a
      --  keypress, Button should be 0.
      --  The Activate_Time parameter is used to conflict-resolve initiation
      --  of concurrent requests for mouse/keyboard grab requests. To function
      --  properly, this needs to be the timestamp of the user event (such as a
      --  mouse click or key press) that caused the initiation of the popup.
      --  Only if no such event is available, Gtk.Main.Get_Current_Event_Time
      --  can be used instead.
      --  Note that this function does not work very well on GDK backends that
      --  do not have global coordinates, such as Wayland or Mir. You should
      --  probably use one of the gtk_menu_popup_at_ variants, which do not
      --  have this problem.
      --  Deprecated since 3.22, 1
      --  "parent_menu_shell": the menu shell containing the triggering menu
      --  item, or null
      --  "parent_menu_item": the menu item whose activation triggered the
      --  popup, or null
      --  "func": a user supplied function used to position the menu, or null
      --  "data": user supplied data to be passed to Func.
      --  "button": the mouse button which was pressed to initiate the event.
      --  "activate_time": the time at which the activation event occurred.

   end Popup_User_Data;

   procedure Popup_At_Pointer
      (Menu          : not null access Gtk_Menu_Record;
       Trigger_Event : Gdk.Event.Gdk_Event);
   --  Displays Menu and makes it available for selection.
   --  See gtk_menu_popup_at_widget () to pop up a menu at a widget.
   --  gtk_menu_popup_at_rect () also allows you to position a menu at an
   --  arbitrary rectangle.
   --  Menu will be positioned at the pointer associated with Trigger_Event.
   --  Properties that influence the behaviour of this function are
   --  Gtk.Menu.Gtk_Menu:anchor-hints, Gtk.Menu.Gtk_Menu:rect-anchor-dx,
   --  Gtk.Menu.Gtk_Menu:rect-anchor-dy, and Gtk.Menu.Gtk_Menu:menu-type-hint.
   --  Connect to the Gtk.Menu.Gtk_Menu::popped-up signal to find out how it
   --  was actually positioned.
   --  Since: gtk+ 3.22
   --  "trigger_event": the Gdk.Event.Gdk_Event that initiated this request or
   --  null if it's the current event

   procedure Popup_At_Rect
      (Menu          : not null access Gtk_Menu_Record;
       Rect_Window   : Gdk.Gdk_Window;
       Rect          : Gdk.Rectangle.Gdk_Rectangle;
       Rect_Anchor   : Gdk.Window.Gdk_Gravity;
       Menu_Anchor   : Gdk.Window.Gdk_Gravity;
       Trigger_Event : Gdk.Event.Gdk_Event);
   --  Displays Menu and makes it available for selection.
   --  See gtk_menu_popup_at_widget () and gtk_menu_popup_at_pointer (), which
   --  handle more common cases for popping up menus.
   --  Menu will be positioned at Rect, aligning their anchor points. Rect is
   --  relative to the top-left corner of Rect_Window. Rect_Anchor and
   --  Menu_Anchor determine anchor points on Rect and Menu to pin together.
   --  Menu can optionally be offset by Gtk.Menu.Gtk_Menu:rect-anchor-dx and
   --  Gtk.Menu.Gtk_Menu:rect-anchor-dy.
   --  Anchors should be specified under the assumption that the text
   --  direction is left-to-right; they will be flipped horizontally
   --  automatically if the text direction is right-to-left.
   --  Other properties that influence the behaviour of this function are
   --  Gtk.Menu.Gtk_Menu:anchor-hints and Gtk.Menu.Gtk_Menu:menu-type-hint.
   --  Connect to the Gtk.Menu.Gtk_Menu::popped-up signal to find out how it
   --  was actually positioned.
   --  Since: gtk+ 3.22
   --  "rect_window": the Gdk.Gdk_Window Rect is relative to
   --  "rect": the Gdk.Rectangle.Gdk_Rectangle to align Menu with
   --  "rect_anchor": the point on Rect to align with Menu's anchor point
   --  "menu_anchor": the point on Menu to align with Rect's anchor point
   --  "trigger_event": the Gdk.Event.Gdk_Event that initiated this request or
   --  null if it's the current event

   procedure Popup_At_Widget
      (Menu          : not null access Gtk_Menu_Record;
       Widget        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Widget_Anchor : Gdk.Window.Gdk_Gravity;
       Menu_Anchor   : Gdk.Window.Gdk_Gravity;
       Trigger_Event : Gdk.Event.Gdk_Event);
   --  Displays Menu and makes it available for selection.
   --  See gtk_menu_popup_at_pointer () to pop up a menu at the master
   --  pointer. gtk_menu_popup_at_rect () also allows you to position a menu at
   --  an arbitrary rectangle.
   --  ![](popup-anchors.png)
   --  Menu will be positioned at Widget, aligning their anchor points.
   --  Widget_Anchor and Menu_Anchor determine anchor points on Widget and Menu
   --  to pin together. Menu can optionally be offset by
   --  Gtk.Menu.Gtk_Menu:rect-anchor-dx and Gtk.Menu.Gtk_Menu:rect-anchor-dy.
   --  Anchors should be specified under the assumption that the text
   --  direction is left-to-right; they will be flipped horizontally
   --  automatically if the text direction is right-to-left.
   --  Other properties that influence the behaviour of this function are
   --  Gtk.Menu.Gtk_Menu:anchor-hints and Gtk.Menu.Gtk_Menu:menu-type-hint.
   --  Connect to the Gtk.Menu.Gtk_Menu::popped-up signal to find out how it
   --  was actually positioned.
   --  Since: gtk+ 3.22
   --  "widget": the Gtk.Widget.Gtk_Widget to align Menu with
   --  "widget_anchor": the point on Widget to align with Menu's anchor point
   --  "menu_anchor": the point on Menu to align with Widget's anchor point
   --  "trigger_event": the Gdk.Event.Gdk_Event that initiated this request or
   --  null if it's the current event

   procedure Popup_For_Device
      (Menu              : not null access Gtk_Menu_Record;
       Device            : access Gdk.Device.Gdk_Device_Record'Class;
       Parent_Menu_Shell : access Gtk.Widget.Gtk_Widget_Record'Class;
       Parent_Menu_Item  : access Gtk.Widget.Gtk_Widget_Record'Class;
       Func              : Gtk_Menu_Position_Func;
       Button            : Guint;
       Activate_Time     : Guint32);
   pragma Obsolescent (Popup_For_Device);
   --  Displays a menu and makes it available for selection.
   --  Applications can use this function to display context-sensitive menus,
   --  and will typically supply null for the Parent_Menu_Shell,
   --  Parent_Menu_Item, Func, Data and Destroy parameters. The default menu
   --  positioning function will position the menu at the current position of
   --  Device (or its corresponding pointer).
   --  The Button parameter should be the mouse button pressed to initiate the
   --  menu popup. If the menu popup was initiated by something other than a
   --  mouse button press, such as a mouse button release or a keypress, Button
   --  should be 0.
   --  The Activate_Time parameter is used to conflict-resolve initiation of
   --  concurrent requests for mouse/keyboard grab requests. To function
   --  properly, this needs to be the time stamp of the user event (such as a
   --  mouse click or key press) that caused the initiation of the popup. Only
   --  if no such event is available, Gtk.Main.Get_Current_Event_Time can be
   --  used instead.
   --  Note that this function does not work very well on GDK backends that do
   --  not have global coordinates, such as Wayland or Mir. You should probably
   --  use one of the gtk_menu_popup_at_ variants, which do not have this
   --  problem.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.22, 1
   --  "device": a Gdk.Device.Gdk_Device
   --  "parent_menu_shell": the menu shell containing the triggering menu
   --  item, or null
   --  "parent_menu_item": the menu item whose activation triggered the popup,
   --  or null
   --  "func": a user supplied function used to position the menu, or null
   --  "button": the mouse button which was pressed to initiate the event
   --  "activate_time": the time at which the activation event occurred

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Popup_For_Device_User_Data is

      type Gtk_Menu_Position_Func is access procedure
        (Menu      : not null access Gtk.Menu.Gtk_Menu_Record'Class;
         X         : out Glib.Gint;
         Y         : out Glib.Gint;
         Push_In   : out Boolean;
         User_Data : User_Data_Type);
      --  A user function supplied when calling Gtk.Menu.Popup which controls the
      --  positioning of the menu when it is displayed. The function sets the X
      --  and Y parameters to the coordinates where the menu is to be drawn. To
      --  make the menu appear on a different monitor than the mouse pointer,
      --  gtk_menu_set_monitor must be called.
      --  "menu": a Gtk.Menu.Gtk_Menu.
      --  "x": address of the Glib.Gint representing the horizontal position
      --  where the menu shall be drawn.
      --  "y": address of the Glib.Gint representing the vertical position where
      --  the menu shall be drawn. This is an output parameter.
      --  "push_in": This parameter controls how menus placed outside the monitor
      --  are handled. If this is set to True and part of the menu is outside the
      --  monitor then GTK+ pushes the window into the visible area, effectively
      --  modifying the popup position. Note that moving and possibly resizing the
      --  menu around will alter the scroll position to keep the menu items "in
      --  place", i.e. at the same monitor position they would have been without
      --  resizing. In practice, this behavior is only useful for combobox popups
      --  or option menus and cannot be used to simply confine a menu to monitor
      --  boundaries. In that case, changing the scroll offset is not desirable.
      --  "user_data": the data supplied by the user in the Gtk.Menu.Popup Data
      --  parameter.

      procedure Popup_For_Device
         (Menu              : not null access Gtk.Menu.Gtk_Menu_Record'Class;
          Device            : access Gdk.Device.Gdk_Device_Record'Class;
          Parent_Menu_Shell : access Gtk.Widget.Gtk_Widget_Record'Class;
          Parent_Menu_Item  : access Gtk.Widget.Gtk_Widget_Record'Class;
          Func              : Gtk_Menu_Position_Func;
          Data              : User_Data_Type;
          Button            : Guint;
          Activate_Time     : Guint32);
      pragma Obsolescent (Popup_For_Device);
      --  Displays a menu and makes it available for selection.
      --  Applications can use this function to display context-sensitive
      --  menus, and will typically supply null for the Parent_Menu_Shell,
      --  Parent_Menu_Item, Func, Data and Destroy parameters. The default menu
      --  positioning function will position the menu at the current position
      --  of Device (or its corresponding pointer).
      --  The Button parameter should be the mouse button pressed to initiate
      --  the menu popup. If the menu popup was initiated by something other
      --  than a mouse button press, such as a mouse button release or a
      --  keypress, Button should be 0.
      --  The Activate_Time parameter is used to conflict-resolve initiation
      --  of concurrent requests for mouse/keyboard grab requests. To function
      --  properly, this needs to be the time stamp of the user event (such as
      --  a mouse click or key press) that caused the initiation of the popup.
      --  Only if no such event is available, Gtk.Main.Get_Current_Event_Time
      --  can be used instead.
      --  Note that this function does not work very well on GDK backends that
      --  do not have global coordinates, such as Wayland or Mir. You should
      --  probably use one of the gtk_menu_popup_at_ variants, which do not
      --  have this problem.
      --  Since: gtk+ 3.0
      --  Deprecated since 3.22, 1
      --  "device": a Gdk.Device.Gdk_Device
      --  "parent_menu_shell": the menu shell containing the triggering menu
      --  item, or null
      --  "parent_menu_item": the menu item whose activation triggered the
      --  popup, or null
      --  "func": a user supplied function used to position the menu, or null
      --  "data": user supplied data to be passed to Func
      --  "button": the mouse button which was pressed to initiate the event
      --  "activate_time": the time at which the activation event occurred

   end Popup_For_Device_User_Data;

   procedure Reorder_Child
      (Menu     : not null access Gtk_Menu_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position : Glib.Gint);
   --  Moves Child to a new Position in the list of Menu children.
   --  "child": the Gtk.Menu_Item.Gtk_Menu_Item to move
   --  "position": the new position to place Child. Positions are numbered
   --  from 0 to n - 1

   procedure Reposition (Menu : not null access Gtk_Menu_Record);
   --  Repositions the menu according to its position function.

   procedure Set_Screen
      (Menu   : not null access Gtk_Menu_Record;
       Screen : access Gdk.Screen.Gdk_Screen_Record'Class);
   --  Sets the Gdk.Screen.Gdk_Screen on which the menu will be displayed.
   --  Since: gtk+ 2.2
   --  "screen": a Gdk.Screen.Gdk_Screen, or null if the screen should be
   --  determined by the widget the menu is attached to

   ---------------
   -- Functions --
   ---------------

   function Get_For_Attach_Widget
      (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk.Widget.Widget_List.Glist;
   --  Returns a list of the menus which are attached to this widget. This
   --  list is owned by GTK+ and must not be modified.
   --  Since: gtk+ 2.6
   --  "widget": a Gtk.Widget.Gtk_Widget

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Accel_Group_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Accel_Group.Gtk_Accel_Group
   --  The accel group holding accelerators for the menu.

   Accel_Path_Property : constant Glib.Properties.Property_String;
   --  An accel path used to conveniently construct accel paths of child
   --  items.

   Active_Property : constant Glib.Properties.Property_Int;
   --  The index of the currently selected menu item, or -1 if no menu item is
   --  selected.

   Anchor_Hints_Property : constant Gdk.Window.Property_Gdk_Anchor_Hints;
   --  Type: Gdk.Window.Gdk_Anchor_Hints
   --  Positioning hints for aligning the menu relative to a rectangle.
   --
   --  These hints determine how the menu should be positioned in the case
   --  that the menu would fall off-screen if placed in its ideal position.
   --
   --  ![](popup-flip.png)
   --
   --  For example, Gdk.Anchor_Flip_Y will replace Gdk.Gravity_North_West with
   --  Gdk.Gravity_South_West and vice versa if the menu extends beyond the
   --  bottom edge of the monitor.
   --
   --  See gtk_menu_popup_at_rect (), gtk_menu_popup_at_widget (),
   --  gtk_menu_popup_at_pointer (), Gtk.Menu.Gtk_Menu:rect-anchor-dx,
   --  Gtk.Menu.Gtk_Menu:rect-anchor-dy, Gtk.Menu.Gtk_Menu:menu-type-hint, and
   --  Gtk.Menu.Gtk_Menu::popped-up.

   Attach_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The widget the menu is attached to. Setting this property attaches the
   --  menu without a Gtk_Menu_Detach_Func. If you need to use a detacher, use
   --  Gtk.Menu.Attach_To_Widget directly.

   Menu_Type_Hint_Property : constant Gdk.Window.Property_Gdk_Window_Type_Hint;
   --  Type: Gdk.Window.Gdk_Window_Type_Hint
   --  The Gdk.Window.Gdk_Window_Type_Hint to use for the menu's
   --  Gdk.Gdk_Window.
   --
   --  See gtk_menu_popup_at_rect (), gtk_menu_popup_at_widget (),
   --  gtk_menu_popup_at_pointer (), Gtk.Menu.Gtk_Menu:anchor-hints,
   --  Gtk.Menu.Gtk_Menu:rect-anchor-dx, Gtk.Menu.Gtk_Menu:rect-anchor-dy, and
   --  Gtk.Menu.Gtk_Menu::popped-up.

   Monitor_Property : constant Glib.Properties.Property_Int;
   --  The monitor the menu will be popped up on.

   Rect_Anchor_Dx_Property : constant Glib.Properties.Property_Int;
   --  Horizontal offset to apply to the menu, i.e. the rectangle or widget
   --  anchor.
   --
   --  See gtk_menu_popup_at_rect (), gtk_menu_popup_at_widget (),
   --  gtk_menu_popup_at_pointer (), Gtk.Menu.Gtk_Menu:anchor-hints,
   --  Gtk.Menu.Gtk_Menu:rect-anchor-dy, Gtk.Menu.Gtk_Menu:menu-type-hint, and
   --  Gtk.Menu.Gtk_Menu::popped-up.

   Rect_Anchor_Dy_Property : constant Glib.Properties.Property_Int;
   --  Vertical offset to apply to the menu, i.e. the rectangle or widget
   --  anchor.
   --
   --  See gtk_menu_popup_at_rect (), gtk_menu_popup_at_widget (),
   --  gtk_menu_popup_at_pointer (), Gtk.Menu.Gtk_Menu:anchor-hints,
   --  Gtk.Menu.Gtk_Menu:rect-anchor-dx, Gtk.Menu.Gtk_Menu:menu-type-hint, and
   --  Gtk.Menu.Gtk_Menu::popped-up.

   Reserve_Toggle_Size_Property : constant Glib.Properties.Property_Boolean;
   --  A boolean that indicates whether the menu reserves space for toggles
   --  and icons, regardless of their actual presence.
   --
   --  This property should only be changed from its default value for
   --  special-purposes such as tabular menus. Regular menus that are connected
   --  to a menu bar or context menus should reserve toggle space for
   --  consistency.

   Tearoff_State_Property : constant Glib.Properties.Property_Boolean;
   --  A boolean that indicates whether the menu is torn-off.

   Tearoff_Title_Property : constant Glib.Properties.Property_String;
   --  A title that may be displayed by the window manager when this menu is
   --  torn-off.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Menu_Gtk_Scroll_Type_Void is not null access procedure
     (Self        : access Gtk_Menu_Record'Class;
      Scroll_Type : Gtk.Enums.Gtk_Scroll_Type);

   type Cb_GObject_Gtk_Scroll_Type_Void is not null access procedure
     (Self        : access Glib.Object.GObject_Record'Class;
      Scroll_Type : Gtk.Enums.Gtk_Scroll_Type);

   Signal_Move_Scroll : constant Glib.Signal_Name := "move-scroll";
   procedure On_Move_Scroll
      (Self  : not null access Gtk_Menu_Record;
       Call  : Cb_Gtk_Menu_Gtk_Scroll_Type_Void;
       After : Boolean := False);
   procedure On_Move_Scroll
      (Self  : not null access Gtk_Menu_Record;
       Call  : Cb_GObject_Gtk_Scroll_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   type Cb_Gtk_Menu_Address_Address_Boolean_Boolean_Void is not null access procedure
     (Self         : access Gtk_Menu_Record'Class;
      Flipped_Rect : System.Address;
      Final_Rect   : System.Address;
      Flipped_X    : Boolean;
      Flipped_Y    : Boolean);

   type Cb_GObject_Address_Address_Boolean_Boolean_Void is not null access procedure
     (Self         : access Glib.Object.GObject_Record'Class;
      Flipped_Rect : System.Address;
      Final_Rect   : System.Address;
      Flipped_X    : Boolean;
      Flipped_Y    : Boolean);

   Signal_Popped_Up : constant Glib.Signal_Name := "popped-up";
   procedure On_Popped_Up
      (Self  : not null access Gtk_Menu_Record;
       Call  : Cb_Gtk_Menu_Address_Address_Boolean_Boolean_Void;
       After : Boolean := False);
   procedure On_Popped_Up
      (Self  : not null access Gtk_Menu_Record;
       Call  : Cb_GObject_Address_Address_Boolean_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the position of Menu is finalized after being popped up
   --  using gtk_menu_popup_at_rect (), gtk_menu_popup_at_widget (), or
   --  gtk_menu_popup_at_pointer ().
   --
   --  Menu might be flipped over the anchor rectangle in order to keep it
   --  on-screen, in which case Flipped_X and Flipped_Y will be set to True
   --  accordingly.
   --
   --  Flipped_Rect is the ideal position of Menu after any possible flipping,
   --  but before any possible sliding. Final_Rect is Flipped_Rect, but
   --  possibly translated in the case that flipping is still ineffective in
   --  keeping Menu on-screen.
   --
   --  ![](popup-slide.png)
   --
   --  The blue menu is Menu's ideal position, the green menu is Flipped_Rect,
   --  and the red menu is Final_Rect.
   --
   --  See gtk_menu_popup_at_rect (), gtk_menu_popup_at_widget (),
   --  gtk_menu_popup_at_pointer (), Gtk.Menu.Gtk_Menu:anchor-hints,
   --  Gtk.Menu.Gtk_Menu:rect-anchor-dx, Gtk.Menu.Gtk_Menu:rect-anchor-dy, and
   --  Gtk.Menu.Gtk_Menu:menu-type-hint.
   -- 
   --  Callback parameters:
   --    --  "flipped_rect": the position of Menu after any possible flipping or
   --    --  null if the backend can't obtain it
   --    --  "final_rect": the final position of Menu or null if the backend can't
   --    --  obtain it
   --    --  "flipped_x": True if the anchors were flipped horizontally
   --    --  "flipped_y": True if the anchors were flipped vertically

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Menu_Record, Gtk_Menu);
   function "+"
     (Widget : access Gtk_Menu_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Menu
   renames Implements_Gtk_Buildable.To_Object;

private
   Tearoff_Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("tearoff-title");
   Tearoff_State_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("tearoff-state");
   Reserve_Toggle_Size_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("reserve-toggle-size");
   Rect_Anchor_Dy_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("rect-anchor-dy");
   Rect_Anchor_Dx_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("rect-anchor-dx");
   Monitor_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("monitor");
   Menu_Type_Hint_Property : constant Gdk.Window.Property_Gdk_Window_Type_Hint :=
     Gdk.Window.Build ("menu-type-hint");
   Attach_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("attach-widget");
   Anchor_Hints_Property : constant Gdk.Window.Property_Gdk_Anchor_Hints :=
     Gdk.Window.Build ("anchor-hints");
   Active_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("active");
   Accel_Path_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("accel-path");
   Accel_Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("accel-group");
end Gtk.Menu;
