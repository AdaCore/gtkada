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

pragma Ada_05;
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
--  A Gtk.Menu.Gtk_Menu can also be popped up by activating a Gtk_Option_Menu.
--  Other composite widgets such as the Gtk.Notebook.Gtk_Notebook can pop up a
--  Gtk.Menu.Gtk_Menu as well.
--
--  Applications can display a Gtk.Menu.Gtk_Menu as a popup menu by calling
--  the Gtk.Menu.Popup function. The example below shows how an application can
--  pop up a menu when the 3rd mouse button is pressed.
--
--  == Connecting the popup signal handler. ==
--
--    /<!---->* connect our handler which will popup the menu *<!---->/
--    g_signal_connect_swapped (window, "button_press_event",
--       G_CALLBACK (my_popup_handler), menu);
--
--  == Signal handler which displays a popup menu. ==
--
--    static gint
--    my_popup_handler (GtkWidget *widget, GdkEvent *event)
--    {
--       GtkMenu *menu;
--       GdkEventButton *event_button;
--       g_return_val_if_fail (widget != NULL, FALSE);
--       g_return_val_if_fail (GTK_IS_MENU (widget), FALSE);
--       g_return_val_if_fail (event != NULL, FALSE);
--       /<!---->* The "widget" is the menu that was supplied when
--       * g_signal_connect_swapped was called.
--       *<!---->/
--       menu = GTK_MENU (widget);
--       if (event->type == GDK_BUTTON_PRESS)
--       {
--          event_button = (GdkEventButton *) event;
--          if (event_button->button == 3)
--          {
--             gtk_menu_popup (menu, NULL, NULL, NULL, NULL,
--                event_button->button, event_button->time);
--             return TRUE;
--          }
--       }
--       return FALSE;
--    }
--
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Device;      use Gdk.Device;
with Gdk.Screen;      use Gdk.Screen;
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Menu_Item;   use Gtk.Menu_Item;
with Gtk.Menu_Shell;  use Gtk.Menu_Shell;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Menu is

   type Gtk_Menu_Record is new Gtk_Menu_Shell_Record with null record;
   type Gtk_Menu is access all Gtk_Menu_Record'Class;

   type Gtk_Menu_Position_Func is access procedure
     (Menu    : not null access Gtk_Menu_Record'Class;
      X       : out Gint;
      Y       : out Gint;
      Push_In : out Boolean);
   --  A user function supplied when calling Gtk.Menu.Popup which controls the
   --  positioning of the menu when it is displayed. The function sets the X
   --  and Y parameters to the coordinates where the menu is to be drawn. To
   --  make the menu appear on a different monitor than the mouse pointer,
   --  gtk_menu_set_monitor must be called.
   --  "menu": a Gtk.Menu.Gtk_Menu.
   --  "x": address of the Gint representing the horizontal position where the
   --  menu shall be drawn.
   --  "y": address of the Gint representing the vertical position where the
   --  menu shall be drawn. This is an output parameter.
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
   --  'cells' that an item will occupy is specified by Left_Attach,
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

   procedure Detach (Menu : not null access Gtk_Menu_Record);
   --  Detaches the menu from the widget to which it had been attached. This
   --  function will call the callback function, Detacher, provided when the
   --  Gtk.Menu.Attach_To_Widget function was called.

   function Get_Accel_Group
      (Menu : not null access Gtk_Menu_Record)
       return Gtk.Accel_Group.Gtk_Accel_Group;
   procedure Set_Accel_Group
      (Menu        : not null access Gtk_Menu_Record;
       Accel_Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class);
   --  Set the Gtk.Accel_Group.Gtk_Accel_Group which holds global accelerators
   --  for the menu. This accelerator group needs to also be added to all
   --  windows that this menu is being used in with gtk_window_add_accel_group,
   --  in order for those windows to support all the accelerators contained in
   --  this group.
   --  "accel_group": the Gtk.Accel_Group.Gtk_Accel_Group to be associated
   --  with the menu.

   function Get_Accel_Path
      (Menu : not null access Gtk_Menu_Record) return UTF8_String;
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
   --  'gtk_menu_set_accel_path (menu, "<Gnumeric-Sheet>/File");' has been
   --  called, assign its items the accel paths: '"<Gnumeric-Sheet>/File/New"'
   --  and '"<Gnumeric-Sheet>/File/Exit"'.
   --  Assigning accel paths to menu items then enables the user to change
   --  their accelerators at runtime. More details about accelerator paths and
   --  their default setups can be found at Gtk.Accel_Map.Add_Entry.
   --  Note that Accel_Path string will be stored in a Glib.GQuark. Therefore,
   --  if you pass a static string, you can save some memory by interning it
   --  first with g_intern_static_string.
   --  "accel_path": a valid accelerator path

   function Get_Active
      (Menu : not null access Gtk_Menu_Record)
       return Gtk.Menu_Item.Gtk_Menu_Item;
   procedure Set_Active
      (Menu  : not null access Gtk_Menu_Record;
       Index : Guint);
   --  Selects the specified menu item within the menu. This is used by the
   --  Gtk_Option_Menu and should not be used by anyone else.
   --  "index": the index of the menu item to select. Iindex values are from 0
   --  to n-1

   function Get_Attach_Widget
      (Menu : not null access Gtk_Menu_Record) return Gtk.Widget.Gtk_Widget;
   --  Returns the Gtk.Widget.Gtk_Widget that the menu is attached to.

   function Get_Monitor (Menu : not null access Gtk_Menu_Record) return Gint;
   procedure Set_Monitor
      (Menu        : not null access Gtk_Menu_Record;
       Monitor_Num : Gint);
   --  Informs GTK+ on which monitor a menu should be popped up. See
   --  Gdk.Screen.Get_Monitor_Geometry.
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
   procedure Set_Reserve_Toggle_Size
      (Menu                : not null access Gtk_Menu_Record;
       Reserve_Toggle_Size : Boolean);
   --  Sets whether the menu should reserve space for drawing toggles or
   --  icons, regardless of their actual presence.
   --  Since: gtk+ 2.18
   --  "reserve_toggle_size": whether to reserve size for toggles

   function Get_Tearoff_State
      (Menu : not null access Gtk_Menu_Record) return Boolean;
   procedure Set_Tearoff_State
      (Menu     : not null access Gtk_Menu_Record;
       Torn_Off : Boolean);
   --  Changes the tearoff state of the menu. A menu is normally displayed as
   --  drop down menu which persists as long as the menu is active. It can also
   --  be displayed as a tearoff menu which persists until it is closed or
   --  reattached.
   --  "torn_off": If True, menu is displayed as a tearoff menu.

   function Get_Title
      (Menu : not null access Gtk_Menu_Record) return UTF8_String;
   procedure Set_Title
      (Menu  : not null access Gtk_Menu_Record;
       Title : UTF8_String);
   --  Sets the title string for the menu.
   --  The title is displayed when the menu is shown as a tearoff menu. If
   --  Title is null, the menu will see if it is attached to a parent menu
   --  item, and if so it will try to use the same text as that menu item's
   --  label.
   --  "title": a string containing the title for the menu

   procedure Popdown (Menu : not null access Gtk_Menu_Record);
   --  Removes the menu from the screen.

   procedure Popup
      (Menu              : not null access Gtk_Menu_Record;
       Parent_Menu_Shell : Gtk.Menu_Shell.Gtk_Menu_Shell := null;
       Parent_Menu_Item  : Gtk.Menu_Item.Gtk_Menu_Item := null;
       Func              : Gtk_Menu_Position_Func := null;
       Button            : Guint := 1;
       Activate_Time     : guint32 := 0);
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
   --  if no such event is available, gtk_get_current_event_time can be used
   --  instead.
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
         X         : out Gint;
         Y         : out Gint;
         Push_In   : out Boolean;
         User_Data : User_Data_Type);
      --  A user function supplied when calling Gtk.Menu.Popup which controls the
      --  positioning of the menu when it is displayed. The function sets the X
      --  and Y parameters to the coordinates where the menu is to be drawn. To
      --  make the menu appear on a different monitor than the mouse pointer,
      --  gtk_menu_set_monitor must be called.
      --  "menu": a Gtk.Menu.Gtk_Menu.
      --  "x": address of the Gint representing the horizontal position where the
      --  menu shall be drawn.
      --  "y": address of the Gint representing the vertical position where the
      --  menu shall be drawn. This is an output parameter.
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
          Activate_Time     : guint32 := 0);
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
      --  Only if no such event is available, gtk_get_current_event_time can be
      --  used instead.
      --  "parent_menu_shell": the menu shell containing the triggering menu
      --  item, or null
      --  "parent_menu_item": the menu item whose activation triggered the
      --  popup, or null
      --  "func": a user supplied function used to position the menu, or null
      --  "data": user supplied data to be passed to Func.
      --  "button": the mouse button which was pressed to initiate the event.
      --  "activate_time": the time at which the activation event occurred.

   end Popup_User_Data;

   procedure Popup_For_Device
      (Menu              : not null access Gtk_Menu_Record;
       Device            : access Gdk.Device.Gdk_Device_Record'Class;
       Parent_Menu_Shell : access Gtk.Widget.Gtk_Widget_Record'Class;
       Parent_Menu_Item  : access Gtk.Widget.Gtk_Widget_Record'Class;
       Func              : Gtk_Menu_Position_Func;
       Button            : Guint;
       Activate_Time     : guint32);
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
   --  if no such event is available, gtk_get_current_event_time can be used
   --  instead.
   --  Since: gtk+ 3.0
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
         X         : out Gint;
         Y         : out Gint;
         Push_In   : out Boolean;
         User_Data : User_Data_Type);
      --  A user function supplied when calling Gtk.Menu.Popup which controls the
      --  positioning of the menu when it is displayed. The function sets the X
      --  and Y parameters to the coordinates where the menu is to be drawn. To
      --  make the menu appear on a different monitor than the mouse pointer,
      --  gtk_menu_set_monitor must be called.
      --  "menu": a Gtk.Menu.Gtk_Menu.
      --  "x": address of the Gint representing the horizontal position where the
      --  menu shall be drawn.
      --  "y": address of the Gint representing the vertical position where the
      --  menu shall be drawn. This is an output parameter.
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
          Activate_Time     : guint32);
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
      --  Only if no such event is available, gtk_get_current_event_time can be
      --  used instead.
      --  Since: gtk+ 3.0
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
       Position : Gint);
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
   --  of menus attached to his widget.
   --  Since: gtk+ 2.6
   --  "widget": a Gtk.Widget.Gtk_Widget

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

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Menu_Record, Gtk_Menu);
   function "+"
     (Widget : access Gtk_Menu_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Menu
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Accel_Group_Property
   --  Type: Gtk.Accel_Group.Gtk_Accel_Group
   --  Flags: read-write
   --  The accel group holding accelerators for the menu.
   --
   --  Name: Accel_Path_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  An accel path used to conveniently construct accel paths of child
   --  items.
   --
   --  Name: Active_Property
   --  Type: Gint
   --  Flags: read-write
   --  The index of the currently selected menu item, or -1 if no menu item is
   --  selected.
   --
   --  Name: Attach_Widget_Property
   --  Type: Gtk.Widget.Gtk_Widget
   --  Flags: read-write
   --  The widget the menu is attached to. Setting this property attaches the
   --  menu without a Gtk_Menu_Detach_Func. If you need to use a detacher, use
   --  Gtk.Menu.Attach_To_Widget directly.
   --
   --  Name: Monitor_Property
   --  Type: Gint
   --  Flags: read-write
   --  The monitor the menu will be popped up on.
   --
   --  Name: Reserve_Toggle_Size_Property
   --  Type: Boolean
   --  Flags: read-write
   --  A boolean that indicates whether the menu reserves space for toggles
   --  and icons, regardless of their actual presence.
   --
   --  This property should only be changed from its default value for
   --  special-purposes such as tabular menus. Regular menus that are connected
   --  to a menu bar or context menus should reserve toggle space for
   --  consistency.
   --
   --  Name: Tearoff_State_Property
   --  Type: Boolean
   --  Flags: read-write
   --  A boolean that indicates whether the menu is torn-off.
   --
   --  Name: Tearoff_Title_Property
   --  Type: UTF8_String
   --  Flags: read-write

   Accel_Group_Property : constant Glib.Properties.Property_Object;
   Accel_Path_Property : constant Glib.Properties.Property_String;
   Active_Property : constant Glib.Properties.Property_Int;
   Attach_Widget_Property : constant Glib.Properties.Property_Object;
   Monitor_Property : constant Glib.Properties.Property_Int;
   Reserve_Toggle_Size_Property : constant Glib.Properties.Property_Boolean;
   Tearoff_State_Property : constant Glib.Properties.Property_Boolean;
   Tearoff_Title_Property : constant Glib.Properties.Property_String;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "move-scroll"
   --     procedure Handler
   --       (Self        : access Gtk_Menu_Record'Class;
   --        Scroll_Type : Gtk.Enums.Gtk_Scroll_Type);
   --    --  "scroll_type": a Gtk.Enums.Gtk_Scroll_Type

   Signal_Move_Scroll : constant Glib.Signal_Name := "move-scroll";

private
   Accel_Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("accel-group");
   Accel_Path_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("accel-path");
   Active_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("active");
   Attach_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("attach-widget");
   Monitor_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("monitor");
   Reserve_Toggle_Size_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("reserve-toggle-size");
   Tearoff_State_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("tearoff-state");
   Tearoff_Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("tearoff-title");
end Gtk.Menu;
