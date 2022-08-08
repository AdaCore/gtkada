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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Menu is

   procedure C_Gtk_Menu_Attach_To_Widget
      (Menu          : System.Address;
       Attach_Widget : System.Address;
       Detacher      : System.Address);
   pragma Import (C, C_Gtk_Menu_Attach_To_Widget, "gtk_menu_attach_to_widget");
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

   procedure C_Gtk_Menu_Popup
      (Menu              : System.Address;
       Parent_Menu_Shell : System.Address;
       Parent_Menu_Item  : System.Address;
       Func              : System.Address;
       Data              : System.Address;
       Button            : Guint;
       Activate_Time     : Guint32);
   pragma Import (C, C_Gtk_Menu_Popup, "gtk_menu_popup");
   pragma Obsolescent (C_Gtk_Menu_Popup);
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
   --  "data": user supplied data to be passed to Func.
   --  "button": the mouse button which was pressed to initiate the event.
   --  "activate_time": the time at which the activation event occurred.

   procedure C_Gtk_Menu_Popup_For_Device
      (Menu              : System.Address;
       Device            : System.Address;
       Parent_Menu_Shell : System.Address;
       Parent_Menu_Item  : System.Address;
       Func              : System.Address;
       Data              : System.Address;
       Destroy           : System.Address;
       Button            : Guint;
       Activate_Time     : Guint32);
   pragma Import (C, C_Gtk_Menu_Popup_For_Device, "gtk_menu_popup_for_device");
   pragma Obsolescent (C_Gtk_Menu_Popup_For_Device);
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
   --  "data": user supplied data to be passed to Func
   --  "destroy": destroy notify for Data
   --  "button": the mouse button which was pressed to initiate the event
   --  "activate_time": the time at which the activation event occurred

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Menu_Detach_Func, System.Address);

   function To_Gtk_Menu_Position_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Menu_Position_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Menu_Position_Func, System.Address);

   procedure Internal_Gtk_Menu_Position_Func
      (Menu      : System.Address;
       X         : out Glib.Gint;
       Y         : out Glib.Gint;
       Push_In   : out Glib.Gboolean;
       User_Data : System.Address);
   pragma Convention (C, Internal_Gtk_Menu_Position_Func);
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

   -------------------------------------
   -- Internal_Gtk_Menu_Position_Func --
   -------------------------------------

   procedure Internal_Gtk_Menu_Position_Func
      (Menu      : System.Address;
       X         : out Glib.Gint;
       Y         : out Glib.Gint;
       Push_In   : out Glib.Gboolean;
       User_Data : System.Address)
   is
      Func          : constant Gtk_Menu_Position_Func := To_Gtk_Menu_Position_Func (User_Data);
      Stub_Gtk_Menu : Gtk_Menu_Record;
      Tmp_Push_In   : Boolean;
   begin
      Func (Gtk.Menu.Gtk_Menu (Get_User_Data (Menu, Stub_Gtk_Menu)), X, Y, Tmp_Push_In);
      Push_In := Boolean'Pos (Tmp_Push_In);
   end Internal_Gtk_Menu_Position_Func;

   package Type_Conversion_Gtk_Menu is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Menu_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Menu);

   ------------------
   -- Gtk_Menu_New --
   ------------------

   function Gtk_Menu_New return Gtk_Menu is
      Menu : constant Gtk_Menu := new Gtk_Menu_Record;
   begin
      Gtk.Menu.Initialize (Menu);
      return Menu;
   end Gtk_Menu_New;

   -----------------------------
   -- Gtk_Menu_New_From_Model --
   -----------------------------

   function Gtk_Menu_New_From_Model
      (Model : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
       return Gtk_Menu
   is
      Menu : constant Gtk_Menu := new Gtk_Menu_Record;
   begin
      Gtk.Menu.Initialize_From_Model (Menu, Model);
      return Menu;
   end Gtk_Menu_New_From_Model;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Menu : out Gtk_Menu) is
   begin
      Menu := new Gtk_Menu_Record;
      Gtk.Menu.Initialize (Menu);
   end Gtk_New;

   ------------------------
   -- Gtk_New_From_Model --
   ------------------------

   procedure Gtk_New_From_Model
      (Menu  : out Gtk_Menu;
       Model : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
   begin
      Menu := new Gtk_Menu_Record;
      Gtk.Menu.Initialize_From_Model (Menu, Model);
   end Gtk_New_From_Model;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Menu : not null access Gtk_Menu_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_menu_new");
   begin
      if not Menu.Is_Created then
         Set_Object (Menu, Internal);
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_From_Model --
   ---------------------------

   procedure Initialize_From_Model
      (Menu  : not null access Gtk_Menu_Record'Class;
       Model : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      function Internal (Model : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_new_from_model");
   begin
      if not Menu.Is_Created then
         Set_Object (Menu, Internal (Get_Object (Model)));
      end if;
   end Initialize_From_Model;

   ------------
   -- Attach --
   ------------

   procedure Attach
      (Menu          : not null access Gtk_Menu_Record;
       Child         : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Left_Attach   : Guint;
       Right_Attach  : Guint;
       Top_Attach    : Guint;
       Bottom_Attach : Guint)
   is
      procedure Internal
         (Menu          : System.Address;
          Child         : System.Address;
          Left_Attach   : Guint;
          Right_Attach  : Guint;
          Top_Attach    : Guint;
          Bottom_Attach : Guint);
      pragma Import (C, Internal, "gtk_menu_attach");
   begin
      Internal (Get_Object (Menu), Get_Object (Child), Left_Attach, Right_Attach, Top_Attach, Bottom_Attach);
   end Attach;

   ----------------------
   -- Attach_To_Widget --
   ----------------------

   procedure Attach_To_Widget
      (Menu          : not null access Gtk_Menu_Record;
       Attach_Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Detacher      : Gtk_Menu_Detach_Func)
   is
   begin
      if Detacher = null then
         C_Gtk_Menu_Attach_To_Widget (Get_Object (Menu), Get_Object (Attach_Widget), System.Null_Address);
      else
         C_Gtk_Menu_Attach_To_Widget (Get_Object (Menu), Get_Object (Attach_Widget), To_Address (Detacher));
      end if;
   end Attach_To_Widget;

   ------------
   -- Detach --
   ------------

   procedure Detach (Menu : not null access Gtk_Menu_Record) is
      procedure Internal (Menu : System.Address);
      pragma Import (C, Internal, "gtk_menu_detach");
   begin
      Internal (Get_Object (Menu));
   end Detach;

   ---------------------
   -- Get_Accel_Group --
   ---------------------

   function Get_Accel_Group
      (Menu : not null access Gtk_Menu_Record)
       return Gtk.Accel_Group.Gtk_Accel_Group
   is
      function Internal (Menu : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_get_accel_group");
      Stub_Gtk_Accel_Group : Gtk.Accel_Group.Gtk_Accel_Group_Record;
   begin
      return Gtk.Accel_Group.Gtk_Accel_Group (Get_User_Data (Internal (Get_Object (Menu)), Stub_Gtk_Accel_Group));
   end Get_Accel_Group;

   --------------------
   -- Get_Accel_Path --
   --------------------

   function Get_Accel_Path
      (Menu : not null access Gtk_Menu_Record) return UTF8_String
   is
      function Internal
         (Menu : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_menu_get_accel_path");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Menu)));
   end Get_Accel_Path;

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active
      (Menu : not null access Gtk_Menu_Record)
       return Gtk.Menu_Item.Gtk_Menu_Item
   is
      function Internal (Menu : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_get_active");
      Stub_Gtk_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item_Record;
   begin
      return Gtk.Menu_Item.Gtk_Menu_Item (Get_User_Data (Internal (Get_Object (Menu)), Stub_Gtk_Menu_Item));
   end Get_Active;

   -----------------------
   -- Get_Attach_Widget --
   -----------------------

   function Get_Attach_Widget
      (Menu : not null access Gtk_Menu_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Menu : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_get_attach_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Menu)), Stub_Gtk_Widget));
   end Get_Attach_Widget;

   -----------------
   -- Get_Monitor --
   -----------------

   function Get_Monitor
      (Menu : not null access Gtk_Menu_Record) return Glib.Gint
   is
      function Internal (Menu : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_menu_get_monitor");
   begin
      return Internal (Get_Object (Menu));
   end Get_Monitor;

   -----------------------------
   -- Get_Reserve_Toggle_Size --
   -----------------------------

   function Get_Reserve_Toggle_Size
      (Menu : not null access Gtk_Menu_Record) return Boolean
   is
      function Internal (Menu : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_menu_get_reserve_toggle_size");
   begin
      return Internal (Get_Object (Menu)) /= 0;
   end Get_Reserve_Toggle_Size;

   -----------------------
   -- Get_Tearoff_State --
   -----------------------

   function Get_Tearoff_State
      (Menu : not null access Gtk_Menu_Record) return Boolean
   is
      function Internal (Menu : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_menu_get_tearoff_state");
   begin
      return Internal (Get_Object (Menu)) /= 0;
   end Get_Tearoff_State;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
      (Menu : not null access Gtk_Menu_Record) return UTF8_String
   is
      function Internal
         (Menu : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_menu_get_title");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Menu)));
   end Get_Title;

   ----------------------
   -- Place_On_Monitor --
   ----------------------

   procedure Place_On_Monitor
      (Menu    : not null access Gtk_Menu_Record;
       Monitor : not null access Gdk.Monitor.Gdk_Monitor_Record'Class)
   is
      procedure Internal (Menu : System.Address; Monitor : System.Address);
      pragma Import (C, Internal, "gtk_menu_place_on_monitor");
   begin
      Internal (Get_Object (Menu), Get_Object (Monitor));
   end Place_On_Monitor;

   -------------
   -- Popdown --
   -------------

   procedure Popdown (Menu : not null access Gtk_Menu_Record) is
      procedure Internal (Menu : System.Address);
      pragma Import (C, Internal, "gtk_menu_popdown");
   begin
      Internal (Get_Object (Menu));
   end Popdown;

   -----------
   -- Popup --
   -----------

   procedure Popup
      (Menu              : not null access Gtk_Menu_Record;
       Parent_Menu_Shell : Gtk.Menu_Shell.Gtk_Menu_Shell := null;
       Parent_Menu_Item  : Gtk.Menu_Item.Gtk_Menu_Item := null;
       Func              : Gtk_Menu_Position_Func := null;
       Button            : Guint := 1;
       Activate_Time     : Guint32 := 0)
   is
   begin
      if Func = null then
         C_Gtk_Menu_Popup (Get_Object (Menu), Get_Object_Or_Null (GObject (Parent_Menu_Shell)), Get_Object_Or_Null (GObject (Parent_Menu_Item)), System.Null_Address, System.Null_Address, Button, Activate_Time);
      else
         C_Gtk_Menu_Popup (Get_Object (Menu), Get_Object_Or_Null (GObject (Parent_Menu_Shell)), Get_Object_Or_Null (GObject (Parent_Menu_Item)), Internal_Gtk_Menu_Position_Func'Address, To_Address (Func), Button, Activate_Time);
      end if;
   end Popup;

   ----------------------
   -- Popup_At_Pointer --
   ----------------------

   procedure Popup_At_Pointer
      (Menu          : not null access Gtk_Menu_Record;
       Trigger_Event : Gdk.Event.Gdk_Event)
   is
      procedure Internal
         (Menu          : System.Address;
          Trigger_Event : Gdk.Event.Gdk_Event);
      pragma Import (C, Internal, "gtk_menu_popup_at_pointer");
   begin
      Internal (Get_Object (Menu), Trigger_Event);
   end Popup_At_Pointer;

   -------------------
   -- Popup_At_Rect --
   -------------------

   procedure Popup_At_Rect
      (Menu          : not null access Gtk_Menu_Record;
       Rect_Window   : Gdk.Gdk_Window;
       Rect          : Gdk.Rectangle.Gdk_Rectangle;
       Rect_Anchor   : Gdk.Window.Gdk_Gravity;
       Menu_Anchor   : Gdk.Window.Gdk_Gravity;
       Trigger_Event : Gdk.Event.Gdk_Event)
   is
      procedure Internal
         (Menu          : System.Address;
          Rect_Window   : Gdk.Gdk_Window;
          Rect          : Gdk.Rectangle.Gdk_Rectangle;
          Rect_Anchor   : Gdk.Window.Gdk_Gravity;
          Menu_Anchor   : Gdk.Window.Gdk_Gravity;
          Trigger_Event : Gdk.Event.Gdk_Event);
      pragma Import (C, Internal, "gtk_menu_popup_at_rect");
   begin
      Internal (Get_Object (Menu), Rect_Window, Rect, Rect_Anchor, Menu_Anchor, Trigger_Event);
   end Popup_At_Rect;

   ---------------------
   -- Popup_At_Widget --
   ---------------------

   procedure Popup_At_Widget
      (Menu          : not null access Gtk_Menu_Record;
       Widget        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Widget_Anchor : Gdk.Window.Gdk_Gravity;
       Menu_Anchor   : Gdk.Window.Gdk_Gravity;
       Trigger_Event : Gdk.Event.Gdk_Event)
   is
      procedure Internal
         (Menu          : System.Address;
          Widget        : System.Address;
          Widget_Anchor : Gdk.Window.Gdk_Gravity;
          Menu_Anchor   : Gdk.Window.Gdk_Gravity;
          Trigger_Event : Gdk.Event.Gdk_Event);
      pragma Import (C, Internal, "gtk_menu_popup_at_widget");
   begin
      Internal (Get_Object (Menu), Get_Object (Widget), Widget_Anchor, Menu_Anchor, Trigger_Event);
   end Popup_At_Widget;

   ----------------------
   -- Popup_For_Device --
   ----------------------

   procedure Popup_For_Device
      (Menu              : not null access Gtk_Menu_Record;
       Device            : access Gdk.Device.Gdk_Device_Record'Class;
       Parent_Menu_Shell : access Gtk.Widget.Gtk_Widget_Record'Class;
       Parent_Menu_Item  : access Gtk.Widget.Gtk_Widget_Record'Class;
       Func              : Gtk_Menu_Position_Func;
       Button            : Guint;
       Activate_Time     : Guint32)
   is
   begin
      if Func = null then
         C_Gtk_Menu_Popup_For_Device (Get_Object (Menu), Get_Object_Or_Null (GObject (Device)), Get_Object_Or_Null (GObject (Parent_Menu_Shell)), Get_Object_Or_Null (GObject (Parent_Menu_Item)), System.Null_Address, System.Null_Address, System.Null_Address, Button, Activate_Time);
      else
         C_Gtk_Menu_Popup_For_Device (Get_Object (Menu), Get_Object_Or_Null (GObject (Device)), Get_Object_Or_Null (GObject (Parent_Menu_Shell)), Get_Object_Or_Null (GObject (Parent_Menu_Item)), Internal_Gtk_Menu_Position_Func'Address, To_Address (Func), System.Null_Address, Button, Activate_Time);
      end if;
   end Popup_For_Device;

   package body Popup_For_Device_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Menu_Position_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Menu_Position_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Menu_Position_Func, System.Address);

      procedure Internal_Cb
         (Menu      : System.Address;
          X         : out Glib.Gint;
          Y         : out Glib.Gint;
          Push_In   : out Glib.Gboolean;
          User_Data : System.Address);
      pragma Convention (C, Internal_Cb);
      --  A user function supplied when calling Gtk.Menu.Popup which controls
      --  the positioning of the menu when it is displayed. The function sets
      --  the X and Y parameters to the coordinates where the menu is to be
      --  drawn. To make the menu appear on a different monitor than the mouse
      --  pointer, Gtk.Menu.Set_Monitor must be called.
      --  "menu": a Gtk.Menu.Gtk_Menu.
      --  "x": address of the Glib.Gint representing the horizontal position
      --  where the menu shall be drawn.
      --  "y": address of the Glib.Gint representing the vertical position
      --  where the menu shall be drawn. This is an output parameter.
      --  "push_in": This parameter controls how menus placed outside the
      --  monitor are handled. If this is set to True and part of the menu is
      --  outside the monitor then GTK+ pushes the window into the visible
      --  area, effectively modifying the popup position. Note that moving and
      --  possibly resizing the menu around will alter the scroll position to
      --  keep the menu items "in place", i.e. at the same monitor position
      --  they would have been without resizing. In practice, this behavior is
      --  only useful for combobox popups or option menus and cannot be used to
      --  simply confine a menu to monitor boundaries. In that case, changing
      --  the scroll offset is not desirable.
      --  "user_data": the data supplied by the user in the Gtk.Menu.Popup
      --  Data parameter.

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Menu      : System.Address;
          X         : out Glib.Gint;
          Y         : out Glib.Gint;
          Push_In   : out Glib.Gboolean;
          User_Data : System.Address)
      is
         D             : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Gtk_Menu : Gtk.Menu.Gtk_Menu_Record;
         Tmp_Push_In   : Boolean;
      begin
         To_Gtk_Menu_Position_Func (D.Func) (Gtk.Menu.Gtk_Menu (Get_User_Data (Menu, Stub_Gtk_Menu)), X, Y, Tmp_Push_In, D.Data.all);
         Push_In := Boolean'Pos (Tmp_Push_In);
      end Internal_Cb;

      ----------------------
      -- Popup_For_Device --
      ----------------------

      procedure Popup_For_Device
         (Menu              : not null access Gtk.Menu.Gtk_Menu_Record'Class;
          Device            : access Gdk.Device.Gdk_Device_Record'Class;
          Parent_Menu_Shell : access Gtk.Widget.Gtk_Widget_Record'Class;
          Parent_Menu_Item  : access Gtk.Widget.Gtk_Widget_Record'Class;
          Func              : Gtk_Menu_Position_Func;
          Data              : User_Data_Type;
          Button            : Guint;
          Activate_Time     : Guint32)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Menu_Popup_For_Device (Get_Object (Menu), Get_Object_Or_Null (GObject (Device)), Get_Object_Or_Null (GObject (Parent_Menu_Shell)), Get_Object_Or_Null (GObject (Parent_Menu_Item)), System.Null_Address, System.Null_Address, Users.Free_Data'Address, Button, Activate_Time);
         else
            D := Users.Build (To_Address (Func), Data);
            C_Gtk_Menu_Popup_For_Device (Get_Object (Menu), Get_Object_Or_Null (GObject (Device)), Get_Object_Or_Null (GObject (Parent_Menu_Shell)), Get_Object_Or_Null (GObject (Parent_Menu_Item)), Internal_Cb'Address, D, Users.Free_Data'Address, Button, Activate_Time);
         end if;
      end Popup_For_Device;

   end Popup_For_Device_User_Data;

   package body Popup_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Menu_Position_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Menu_Position_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Menu_Position_Func, System.Address);

      procedure Internal_Cb
         (Menu      : System.Address;
          X         : out Glib.Gint;
          Y         : out Glib.Gint;
          Push_In   : out Glib.Gboolean;
          User_Data : System.Address);
      pragma Convention (C, Internal_Cb);
      --  A user function supplied when calling Gtk.Menu.Popup which controls
      --  the positioning of the menu when it is displayed. The function sets
      --  the X and Y parameters to the coordinates where the menu is to be
      --  drawn. To make the menu appear on a different monitor than the mouse
      --  pointer, Gtk.Menu.Set_Monitor must be called.
      --  "menu": a Gtk.Menu.Gtk_Menu.
      --  "x": address of the Glib.Gint representing the horizontal position
      --  where the menu shall be drawn.
      --  "y": address of the Glib.Gint representing the vertical position
      --  where the menu shall be drawn. This is an output parameter.
      --  "push_in": This parameter controls how menus placed outside the
      --  monitor are handled. If this is set to True and part of the menu is
      --  outside the monitor then GTK+ pushes the window into the visible
      --  area, effectively modifying the popup position. Note that moving and
      --  possibly resizing the menu around will alter the scroll position to
      --  keep the menu items "in place", i.e. at the same monitor position
      --  they would have been without resizing. In practice, this behavior is
      --  only useful for combobox popups or option menus and cannot be used to
      --  simply confine a menu to monitor boundaries. In that case, changing
      --  the scroll offset is not desirable.
      --  "user_data": the data supplied by the user in the Gtk.Menu.Popup
      --  Data parameter.

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Menu      : System.Address;
          X         : out Glib.Gint;
          Y         : out Glib.Gint;
          Push_In   : out Glib.Gboolean;
          User_Data : System.Address)
      is
         D             : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Gtk_Menu : Gtk.Menu.Gtk_Menu_Record;
         Tmp_Push_In   : Boolean;
      begin
         To_Gtk_Menu_Position_Func (D.Func) (Gtk.Menu.Gtk_Menu (Get_User_Data (Menu, Stub_Gtk_Menu)), X, Y, Tmp_Push_In, D.Data.all);
         Push_In := Boolean'Pos (Tmp_Push_In);
      end Internal_Cb;

      -----------
      -- Popup --
      -----------

      procedure Popup
         (Menu              : not null access Gtk.Menu.Gtk_Menu_Record'Class;
          Parent_Menu_Shell : Gtk.Menu_Shell.Gtk_Menu_Shell := null;
          Parent_Menu_Item  : Gtk.Menu_Item.Gtk_Menu_Item := null;
          Func              : Gtk_Menu_Position_Func := null;
          Data              : User_Data_Type;
          Button            : Guint := 1;
          Activate_Time     : Guint32 := 0)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Menu_Popup (Get_Object (Menu), Get_Object_Or_Null (GObject (Parent_Menu_Shell)), Get_Object_Or_Null (GObject (Parent_Menu_Item)), System.Null_Address, System.Null_Address, Button, Activate_Time);
         else
            D := Users.Build (To_Address (Func), Data);
            C_Gtk_Menu_Popup (Get_Object (Menu), Get_Object_Or_Null (GObject (Parent_Menu_Shell)), Get_Object_Or_Null (GObject (Parent_Menu_Item)), Internal_Cb'Address, D, Button, Activate_Time);
            Users.Free_Data (D);
         end if;
      end Popup;

   end Popup_User_Data;

   -------------------
   -- Reorder_Child --
   -------------------

   procedure Reorder_Child
      (Menu     : not null access Gtk_Menu_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position : Glib.Gint)
   is
      procedure Internal
         (Menu     : System.Address;
          Child    : System.Address;
          Position : Glib.Gint);
      pragma Import (C, Internal, "gtk_menu_reorder_child");
   begin
      Internal (Get_Object (Menu), Get_Object (Child), Position);
   end Reorder_Child;

   ----------------
   -- Reposition --
   ----------------

   procedure Reposition (Menu : not null access Gtk_Menu_Record) is
      procedure Internal (Menu : System.Address);
      pragma Import (C, Internal, "gtk_menu_reposition");
   begin
      Internal (Get_Object (Menu));
   end Reposition;

   ---------------------
   -- Set_Accel_Group --
   ---------------------

   procedure Set_Accel_Group
      (Menu        : not null access Gtk_Menu_Record;
       Accel_Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class)
   is
      procedure Internal
         (Menu        : System.Address;
          Accel_Group : System.Address);
      pragma Import (C, Internal, "gtk_menu_set_accel_group");
   begin
      Internal (Get_Object (Menu), Get_Object_Or_Null (GObject (Accel_Group)));
   end Set_Accel_Group;

   --------------------
   -- Set_Accel_Path --
   --------------------

   procedure Set_Accel_Path
      (Menu       : not null access Gtk_Menu_Record;
       Accel_Path : UTF8_String := "")
   is
      procedure Internal
         (Menu       : System.Address;
          Accel_Path : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_menu_set_accel_path");
      Tmp_Accel_Path : Gtkada.Types.Chars_Ptr;
   begin
      if Accel_Path = "" then
         Tmp_Accel_Path := Gtkada.Types.Null_Ptr;
      else
         Tmp_Accel_Path := New_String (Accel_Path);
      end if;
      Internal (Get_Object (Menu), Tmp_Accel_Path);
      Free (Tmp_Accel_Path);
   end Set_Accel_Path;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
      (Menu  : not null access Gtk_Menu_Record;
       Index : Guint)
   is
      procedure Internal (Menu : System.Address; Index : Guint);
      pragma Import (C, Internal, "gtk_menu_set_active");
   begin
      Internal (Get_Object (Menu), Index);
   end Set_Active;

   -----------------
   -- Set_Monitor --
   -----------------

   procedure Set_Monitor
      (Menu        : not null access Gtk_Menu_Record;
       Monitor_Num : Glib.Gint)
   is
      procedure Internal (Menu : System.Address; Monitor_Num : Glib.Gint);
      pragma Import (C, Internal, "gtk_menu_set_monitor");
   begin
      Internal (Get_Object (Menu), Monitor_Num);
   end Set_Monitor;

   -----------------------------
   -- Set_Reserve_Toggle_Size --
   -----------------------------

   procedure Set_Reserve_Toggle_Size
      (Menu                : not null access Gtk_Menu_Record;
       Reserve_Toggle_Size : Boolean)
   is
      procedure Internal
         (Menu                : System.Address;
          Reserve_Toggle_Size : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_menu_set_reserve_toggle_size");
   begin
      Internal (Get_Object (Menu), Boolean'Pos (Reserve_Toggle_Size));
   end Set_Reserve_Toggle_Size;

   ----------------
   -- Set_Screen --
   ----------------

   procedure Set_Screen
      (Menu   : not null access Gtk_Menu_Record;
       Screen : access Gdk.Screen.Gdk_Screen_Record'Class)
   is
      procedure Internal (Menu : System.Address; Screen : System.Address);
      pragma Import (C, Internal, "gtk_menu_set_screen");
   begin
      Internal (Get_Object (Menu), Get_Object_Or_Null (GObject (Screen)));
   end Set_Screen;

   -----------------------
   -- Set_Tearoff_State --
   -----------------------

   procedure Set_Tearoff_State
      (Menu     : not null access Gtk_Menu_Record;
       Torn_Off : Boolean)
   is
      procedure Internal (Menu : System.Address; Torn_Off : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_menu_set_tearoff_state");
   begin
      Internal (Get_Object (Menu), Boolean'Pos (Torn_Off));
   end Set_Tearoff_State;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
      (Menu  : not null access Gtk_Menu_Record;
       Title : UTF8_String := "")
   is
      procedure Internal
         (Menu  : System.Address;
          Title : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_menu_set_title");
      Tmp_Title : Gtkada.Types.Chars_Ptr;
   begin
      if Title = "" then
         Tmp_Title := Gtkada.Types.Null_Ptr;
      else
         Tmp_Title := New_String (Title);
      end if;
      Internal (Get_Object (Menu), Tmp_Title);
      Free (Tmp_Title);
   end Set_Title;

   ---------------------------
   -- Get_For_Attach_Widget --
   ---------------------------

   function Get_For_Attach_Widget
      (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk.Widget.Widget_List.Glist
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_get_for_attach_widget");
      Tmp_Return : Gtk.Widget.Widget_List.Glist;
   begin
      Gtk.Widget.Widget_List.Set_Object (Tmp_Return, Internal (Get_Object (Widget)));
      return Tmp_Return;
   end Get_For_Attach_Widget;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Menu_Gtk_Scroll_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Menu_Gtk_Scroll_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Scroll_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Scroll_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Menu_Address_Address_Boolean_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Menu_Address_Address_Boolean_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Address_Address_Boolean_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Address_Address_Boolean_Boolean_Void);

   procedure Connect
      (Object  : access Gtk_Menu_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Gtk_Scroll_Type_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Menu_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Address_Address_Boolean_Boolean_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Scroll_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Address_Address_Boolean_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Address_Address_Boolean_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Address_Address_Boolean_Boolean_Void);

   procedure Marsh_GObject_Gtk_Scroll_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Scroll_Type_Void);

   procedure Marsh_Gtk_Menu_Address_Address_Boolean_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Menu_Address_Address_Boolean_Boolean_Void);

   procedure Marsh_Gtk_Menu_Gtk_Scroll_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Menu_Gtk_Scroll_Type_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Menu_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Gtk_Scroll_Type_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Menu_Gtk_Scroll_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Menu_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Address_Address_Boolean_Boolean_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Menu_Address_Address_Boolean_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Scroll_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Scroll_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Address_Address_Boolean_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Address_Address_Boolean_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   --------------------------------------------------------
   -- Marsh_GObject_Address_Address_Boolean_Boolean_Void --
   --------------------------------------------------------

   procedure Marsh_GObject_Address_Address_Boolean_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Address_Address_Boolean_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Address (Params, 1), Unchecked_To_Address (Params, 2), Unchecked_To_Boolean (Params, 3), Unchecked_To_Boolean (Params, 4));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Address_Address_Boolean_Boolean_Void;

   ----------------------------------------
   -- Marsh_GObject_Gtk_Scroll_Type_Void --
   ----------------------------------------

   procedure Marsh_GObject_Gtk_Scroll_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Scroll_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Scroll_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Scroll_Type_Void;

   ---------------------------------------------------------
   -- Marsh_Gtk_Menu_Address_Address_Boolean_Boolean_Void --
   ---------------------------------------------------------

   procedure Marsh_Gtk_Menu_Address_Address_Boolean_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Menu_Address_Address_Boolean_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Menu := Gtk_Menu (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Address (Params, 1), Unchecked_To_Address (Params, 2), Unchecked_To_Boolean (Params, 3), Unchecked_To_Boolean (Params, 4));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Menu_Address_Address_Boolean_Boolean_Void;

   -----------------------------------------
   -- Marsh_Gtk_Menu_Gtk_Scroll_Type_Void --
   -----------------------------------------

   procedure Marsh_Gtk_Menu_Gtk_Scroll_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Menu_Gtk_Scroll_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Menu := Gtk_Menu (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Scroll_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Menu_Gtk_Scroll_Type_Void;

   --------------------
   -- On_Move_Scroll --
   --------------------

   procedure On_Move_Scroll
      (Self  : not null access Gtk_Menu_Record;
       Call  : Cb_Gtk_Menu_Gtk_Scroll_Type_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "move-scroll" & ASCII.NUL, Call, After);
   end On_Move_Scroll;

   --------------------
   -- On_Move_Scroll --
   --------------------

   procedure On_Move_Scroll
      (Self  : not null access Gtk_Menu_Record;
       Call  : Cb_GObject_Gtk_Scroll_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "move-scroll" & ASCII.NUL, Call, After, Slot);
   end On_Move_Scroll;

   ------------------
   -- On_Popped_Up --
   ------------------

   procedure On_Popped_Up
      (Self  : not null access Gtk_Menu_Record;
       Call  : Cb_Gtk_Menu_Address_Address_Boolean_Boolean_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "popped-up" & ASCII.NUL, Call, After);
   end On_Popped_Up;

   ------------------
   -- On_Popped_Up --
   ------------------

   procedure On_Popped_Up
      (Self  : not null access Gtk_Menu_Record;
       Call  : Cb_GObject_Address_Address_Boolean_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "popped-up" & ASCII.NUL, Call, After, Slot);
   end On_Popped_Up;

end Gtk.Menu;
