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
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with Glib.Object;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Menu is

   function To_Gtk_Menu_Position_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Menu_Position_Func);

   procedure C_Gtk_Menu_Popup
      (Menu              : System.Address;
       Parent_Menu_Shell : System.Address;
       Parent_Menu_Item  : System.Address;
       Func              : System.Address;
       Data              : System.Address;
       Button            : Guint;
       Activate_Time     : guint32);
   pragma Import (C, C_Gtk_Menu_Popup, "gtk_menu_popup");
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
       Activate_Time     : guint32);
   pragma Import (C, C_Gtk_Menu_Popup_For_Device, "gtk_menu_popup_for_device");
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
   --  "data": user supplied data to be passed to Func
   --  "destroy": destroy notify for Data
   --  "button": the mouse button which was pressed to initiate the event
   --  "activate_time": the time at which the activation event occurred

   procedure Internal_Gtk_Menu_Position_Func
      (Menu      : System.Address;
       X         : out Gint;
       Y         : out Gint;
       Push_In   : out Integer;
       User_Data : System.Address);
   pragma Convention (C, Internal_Gtk_Menu_Position_Func);
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

   -------------------------------------
   -- Internal_Gtk_Menu_Position_Func --
   -------------------------------------

   procedure Internal_Gtk_Menu_Position_Func
      (Menu      : System.Address;
       X         : out Gint;
       Y         : out Gint;
       Push_In   : out Integer;
       User_Data : System.Address)
   is
      Func          : constant Gtk_Menu_Position_Func := To_Gtk_Menu_Position_Func (User_Data);
      Stub_Gtk_Menu : Gtk_Menu_Record;
      Tmp_Push_In   : Boolean;
   begin
      Func (Gtk.Menu.Gtk_Menu (Get_User_Data (Menu, Stub_Gtk_Menu)), X, Y, Tmp_Push_In);
      Push_In := Boolean'Pos (Tmp_Push_In);
   end Internal_Gtk_Menu_Position_Func;

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Menu_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Menu : out Gtk_Menu) is
   begin
      Menu := new Gtk_Menu_Record;
      Gtk.Menu.Initialize (Menu);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Menu : not null access Gtk_Menu_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_menu_new");
   begin
      Set_Object (Menu, Internal);
   end Initialize;

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
         (Menu : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_menu_get_accel_path");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Menu)));
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

   function Get_Monitor (Menu : not null access Gtk_Menu_Record) return Gint is
      function Internal (Menu : System.Address) return Gint;
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
      function Internal (Menu : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_menu_get_reserve_toggle_size");
   begin
      return Boolean'Val (Internal (Get_Object (Menu)));
   end Get_Reserve_Toggle_Size;

   -----------------------
   -- Get_Tearoff_State --
   -----------------------

   function Get_Tearoff_State
      (Menu : not null access Gtk_Menu_Record) return Boolean
   is
      function Internal (Menu : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_menu_get_tearoff_state");
   begin
      return Boolean'Val (Internal (Get_Object (Menu)));
   end Get_Tearoff_State;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
      (Menu : not null access Gtk_Menu_Record) return UTF8_String
   is
      function Internal
         (Menu : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_menu_get_title");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Menu)));
   end Get_Title;

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
       Activate_Time     : guint32 := 0)
   is
   begin
      C_Gtk_Menu_Popup (Get_Object (Menu), Get_Object_Or_Null (GObject (Parent_Menu_Shell)), Get_Object_Or_Null (GObject (Parent_Menu_Item)), Internal_Gtk_Menu_Position_Func'Address, Func'Address, Button, Activate_Time);
   end Popup;

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
       Activate_Time     : guint32)
   is
   begin
      C_Gtk_Menu_Popup_For_Device (Get_Object (Menu), Get_Object_Or_Null (GObject (Device)), Get_Object_Or_Null (GObject (Parent_Menu_Shell)), Get_Object_Or_Null (GObject (Parent_Menu_Item)), Internal_Gtk_Menu_Position_Func'Address, Func'Address, System.Null_Address, Button, Activate_Time);
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
          X         : out Gint;
          Y         : out Gint;
          Push_In   : out Integer;
          User_Data : System.Address);
      pragma Convention (C, Internal_Cb);
      --  A user function supplied when calling Gtk.Menu.Popup which controls
      --  the positioning of the menu when it is displayed. The function sets
      --  the X and Y parameters to the coordinates where the menu is to be
      --  drawn. To make the menu appear on a different monitor than the mouse
      --  pointer, Gtk.Menu.Set_Monitor must be called.
      --  "menu": a Gtk.Menu.Gtk_Menu.
      --  "x": address of the Gint representing the horizontal position where
      --  the menu shall be drawn.
      --  "y": address of the Gint representing the vertical position where
      --  the menu shall be drawn. This is an output parameter.
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
          X         : out Gint;
          Y         : out Gint;
          Push_In   : out Integer;
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
          Activate_Time     : guint32)
      is
      begin
         C_Gtk_Menu_Popup_For_Device (Get_Object (Menu), Get_Object_Or_Null (GObject (Device)), Get_Object_Or_Null (GObject (Parent_Menu_Shell)), Get_Object_Or_Null (GObject (Parent_Menu_Item)), Internal_Cb'Address, Users.Build (To_Address (Func), Data), Users.Free_Data'Address, Button, Activate_Time);
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
          X         : out Gint;
          Y         : out Gint;
          Push_In   : out Integer;
          User_Data : System.Address);
      pragma Convention (C, Internal_Cb);
      --  A user function supplied when calling Gtk.Menu.Popup which controls
      --  the positioning of the menu when it is displayed. The function sets
      --  the X and Y parameters to the coordinates where the menu is to be
      --  drawn. To make the menu appear on a different monitor than the mouse
      --  pointer, Gtk.Menu.Set_Monitor must be called.
      --  "menu": a Gtk.Menu.Gtk_Menu.
      --  "x": address of the Gint representing the horizontal position where
      --  the menu shall be drawn.
      --  "y": address of the Gint representing the vertical position where
      --  the menu shall be drawn. This is an output parameter.
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
          X         : out Gint;
          Y         : out Gint;
          Push_In   : out Integer;
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
          Activate_Time     : guint32 := 0)
      is
      begin
         C_Gtk_Menu_Popup (Get_Object (Menu), Get_Object_Or_Null (GObject (Parent_Menu_Shell)), Get_Object_Or_Null (GObject (Parent_Menu_Item)), Internal_Cb'Address, Users.Build (To_Address (Func), Data), Button, Activate_Time);
      end Popup;

   end Popup_User_Data;

   -------------------
   -- Reorder_Child --
   -------------------

   procedure Reorder_Child
      (Menu     : not null access Gtk_Menu_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position : Gint)
   is
      procedure Internal
         (Menu     : System.Address;
          Child    : System.Address;
          Position : Gint);
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
          Accel_Path : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_menu_set_accel_path");
      Tmp_Accel_Path : Interfaces.C.Strings.chars_ptr;
   begin
      if Accel_Path = "" then
         Tmp_Accel_Path := Interfaces.C.Strings.Null_Ptr;
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
       Monitor_Num : Gint)
   is
      procedure Internal (Menu : System.Address; Monitor_Num : Gint);
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
          Reserve_Toggle_Size : Integer);
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
      procedure Internal (Menu : System.Address; Torn_Off : Integer);
      pragma Import (C, Internal, "gtk_menu_set_tearoff_state");
   begin
      Internal (Get_Object (Menu), Boolean'Pos (Torn_Off));
   end Set_Tearoff_State;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
      (Menu  : not null access Gtk_Menu_Record;
       Title : UTF8_String)
   is
      procedure Internal
         (Menu  : System.Address;
          Title : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_menu_set_title");
      Tmp_Title : Interfaces.C.Strings.chars_ptr := New_String (Title);
   begin
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

end Gtk.Menu;
