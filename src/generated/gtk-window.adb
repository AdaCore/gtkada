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

package body Gtk.Window is

   package Type_Conversion_Gtk_Window is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Window_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Window);

   package Type_Conversion_Gtk_Window_Group is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Group_Get_Type'Access, Gtk_Window_Group_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Window_Group);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Window   : out Gtk_Window;
       The_Type : Gtk.Enums.Gtk_Window_Type := Gtk.Enums.Window_Toplevel)
   is
   begin
      Window := new Gtk_Window_Record;
      Gtk.Window.Initialize (Window, The_Type);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Window_Group : out Gtk_Window_Group) is
   begin
      Window_Group := new Gtk_Window_Group_Record;
      Gtk.Window.Initialize (Window_Group);
   end Gtk_New;

   --------------------------
   -- Gtk_Window_Group_New --
   --------------------------

   function Gtk_Window_Group_New return Gtk_Window_Group is
      Window_Group : constant Gtk_Window_Group := new Gtk_Window_Group_Record;
   begin
      Gtk.Window.Initialize (Window_Group);
      return Window_Group;
   end Gtk_Window_Group_New;

   --------------------
   -- Gtk_Window_New --
   --------------------

   function Gtk_Window_New
      (The_Type : Gtk.Enums.Gtk_Window_Type := Gtk.Enums.Window_Toplevel)
       return Gtk_Window
   is
      Window : constant Gtk_Window := new Gtk_Window_Record;
   begin
      Gtk.Window.Initialize (Window, The_Type);
      return Window;
   end Gtk_Window_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Window   : not null access Gtk_Window_Record'Class;
       The_Type : Gtk.Enums.Gtk_Window_Type := Gtk.Enums.Window_Toplevel)
   is
      function Internal
         (The_Type : Gtk.Enums.Gtk_Window_Type) return System.Address;
      pragma Import (C, Internal, "gtk_window_new");
   begin
      if not Window.Is_Created then
         Set_Object (Window, Internal (The_Type));
      end if;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Window_Group : not null access Gtk_Window_Group_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_window_group_new");
   begin
      if not Window_Group.Is_Created then
         Set_Object (Window_Group, Internal);
      end if;
   end Initialize;

   ----------------------
   -- Activate_Default --
   ----------------------

   function Activate_Default
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_activate_default");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Activate_Default;

   --------------------
   -- Activate_Focus --
   --------------------

   function Activate_Focus
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_activate_focus");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Activate_Focus;

   ------------------
   -- Activate_Key --
   ------------------

   function Activate_Key
      (Window : not null access Gtk_Window_Record;
       Event  : Gdk.Event.Gdk_Event_Key) return Boolean
   is
      function Internal
         (Window : System.Address;
          Event  : Gdk.Event.Gdk_Event_Key) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_activate_key");
   begin
      return Internal (Get_Object (Window), Event) /= 0;
   end Activate_Key;

   ---------------------
   -- Add_Accel_Group --
   ---------------------

   procedure Add_Accel_Group
      (Window      : not null access Gtk_Window_Record;
       Accel_Group : not null access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class)
   is
      procedure Internal
         (Window      : System.Address;
          Accel_Group : System.Address);
      pragma Import (C, Internal, "gtk_window_add_accel_group");
   begin
      Internal (Get_Object (Window), Get_Object (Accel_Group));
   end Add_Accel_Group;

   ------------------
   -- Add_Mnemonic --
   ------------------

   procedure Add_Mnemonic
      (Window : not null access Gtk_Window_Record;
       Keyval : Gdk.Types.Gdk_Key_Type;
       Target : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Window : System.Address;
          Keyval : Gdk.Types.Gdk_Key_Type;
          Target : System.Address);
      pragma Import (C, Internal, "gtk_window_add_mnemonic");
   begin
      Internal (Get_Object (Window), Keyval, Get_Object (Target));
   end Add_Mnemonic;

   ----------------
   -- Add_Window --
   ----------------

   procedure Add_Window
      (Window_Group : not null access Gtk_Window_Group_Record;
       Window       : not null access Gtk_Window_Record'Class)
   is
      procedure Internal
         (Window_Group : System.Address;
          Window       : System.Address);
      pragma Import (C, Internal, "gtk_window_group_add_window");
   begin
      Internal (Get_Object (Window_Group), Get_Object (Window));
   end Add_Window;

   ---------------------
   -- Begin_Move_Drag --
   ---------------------

   procedure Begin_Move_Drag
      (Window    : not null access Gtk_Window_Record;
       Button    : Glib.Gint;
       Root_X    : Glib.Gint;
       Root_Y    : Glib.Gint;
       Timestamp : Guint32)
   is
      procedure Internal
         (Window    : System.Address;
          Button    : Glib.Gint;
          Root_X    : Glib.Gint;
          Root_Y    : Glib.Gint;
          Timestamp : Guint32);
      pragma Import (C, Internal, "gtk_window_begin_move_drag");
   begin
      Internal (Get_Object (Window), Button, Root_X, Root_Y, Timestamp);
   end Begin_Move_Drag;

   -----------------------
   -- Begin_Resize_Drag --
   -----------------------

   procedure Begin_Resize_Drag
      (Window    : not null access Gtk_Window_Record;
       Edge      : Gdk.Window.Gdk_Window_Edge;
       Button    : Glib.Gint;
       Root_X    : Glib.Gint;
       Root_Y    : Glib.Gint;
       Timestamp : Guint32)
   is
      procedure Internal
         (Window    : System.Address;
          Edge      : Gdk.Window.Gdk_Window_Edge;
          Button    : Glib.Gint;
          Root_X    : Glib.Gint;
          Root_Y    : Glib.Gint;
          Timestamp : Guint32);
      pragma Import (C, Internal, "gtk_window_begin_resize_drag");
   begin
      Internal (Get_Object (Window), Edge, Button, Root_X, Root_Y, Timestamp);
   end Begin_Resize_Drag;

   -----------
   -- Close --
   -----------

   procedure Close (Window : not null access Gtk_Window_Record) is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_close");
   begin
      Internal (Get_Object (Window));
   end Close;

   ---------------
   -- Deiconify --
   ---------------

   procedure Deiconify (Window : not null access Gtk_Window_Record) is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_deiconify");
   begin
      Internal (Get_Object (Window));
   end Deiconify;

   ----------------
   -- Fullscreen --
   ----------------

   procedure Fullscreen (Window : not null access Gtk_Window_Record) is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_fullscreen");
   begin
      Internal (Get_Object (Window));
   end Fullscreen;

   ---------------------------
   -- Fullscreen_On_Monitor --
   ---------------------------

   procedure Fullscreen_On_Monitor
      (Window  : not null access Gtk_Window_Record;
       Screen  : not null access Gdk.Screen.Gdk_Screen_Record'Class;
       Monitor : Glib.Gint)
   is
      procedure Internal
         (Window  : System.Address;
          Screen  : System.Address;
          Monitor : Glib.Gint);
      pragma Import (C, Internal, "gtk_window_fullscreen_on_monitor");
   begin
      Internal (Get_Object (Window), Get_Object (Screen), Monitor);
   end Fullscreen_On_Monitor;

   ----------------------
   -- Get_Accept_Focus --
   ----------------------

   function Get_Accept_Focus
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_accept_focus");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Get_Accept_Focus;

   ---------------------
   -- Get_Attached_To --
   ---------------------

   function Get_Attached_To
      (Window : not null access Gtk_Window_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_get_attached_to");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Window)), Stub_Gtk_Widget));
   end Get_Attached_To;

   -----------------------------
   -- Get_Current_Device_Grab --
   -----------------------------

   function Get_Current_Device_Grab
      (Window_Group : not null access Gtk_Window_Group_Record;
       Device       : not null access Gdk.Device.Gdk_Device_Record'Class)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Window_Group : System.Address;
          Device       : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_group_get_current_device_grab");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Window_Group), Get_Object (Device)), Stub_Gtk_Widget));
   end Get_Current_Device_Grab;

   ----------------------
   -- Get_Current_Grab --
   ----------------------

   function Get_Current_Grab
      (Window_Group : not null access Gtk_Window_Group_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Window_Group : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_group_get_current_grab");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Window_Group)), Stub_Gtk_Widget));
   end Get_Current_Grab;

   -------------------
   -- Get_Decorated --
   -------------------

   function Get_Decorated
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_decorated");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Get_Decorated;

   ----------------------
   -- Get_Default_Size --
   ----------------------

   procedure Get_Default_Size
      (Window : not null access Gtk_Window_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint)
   is
      procedure Internal
         (Window : System.Address;
          Width  : out Glib.Gint;
          Height : out Glib.Gint);
      pragma Import (C, Internal, "gtk_window_get_default_size");
   begin
      Internal (Get_Object (Window), Width, Height);
   end Get_Default_Size;

   ------------------------
   -- Get_Default_Widget --
   ------------------------

   function Get_Default_Widget
      (Window : not null access Gtk_Window_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_get_default_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Window)), Stub_Gtk_Widget));
   end Get_Default_Widget;

   -------------------
   -- Get_Deletable --
   -------------------

   function Get_Deletable
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_deletable");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Get_Deletable;

   -----------------------------
   -- Get_Destroy_With_Parent --
   -----------------------------

   function Get_Destroy_With_Parent
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_destroy_with_parent");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Get_Destroy_With_Parent;

   ---------------
   -- Get_Focus --
   ---------------

   function Get_Focus
      (Window : not null access Gtk_Window_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_get_focus");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Window)), Stub_Gtk_Widget));
   end Get_Focus;

   ----------------------
   -- Get_Focus_On_Map --
   ----------------------

   function Get_Focus_On_Map
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_focus_on_map");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Get_Focus_On_Map;

   -----------------------
   -- Get_Focus_Visible --
   -----------------------

   function Get_Focus_Visible
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_focus_visible");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Get_Focus_Visible;

   -----------------
   -- Get_Gravity --
   -----------------

   function Get_Gravity
      (Window : not null access Gtk_Window_Record)
       return Gdk.Window.Gdk_Gravity
   is
      function Internal
         (Window : System.Address) return Gdk.Window.Gdk_Gravity;
      pragma Import (C, Internal, "gtk_window_get_gravity");
   begin
      return Internal (Get_Object (Window));
   end Get_Gravity;

   ---------------
   -- Get_Group --
   ---------------

   function Get_Group
      (Window : not null access Gtk_Window_Record) return Gtk_Window_Group
   is
      function Internal (Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_get_group");
      Stub_Gtk_Window_Group : Gtk_Window_Group_Record;
   begin
      return Gtk.Window.Gtk_Window_Group (Get_User_Data (Internal (Get_Object (Window)), Stub_Gtk_Window_Group));
   end Get_Group;

   -------------------------
   -- Get_Has_Resize_Grip --
   -------------------------

   function Get_Has_Resize_Grip
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_has_resize_grip");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Get_Has_Resize_Grip;

   --------------------------------------
   -- Get_Hide_Titlebar_When_Maximized --
   --------------------------------------

   function Get_Hide_Titlebar_When_Maximized
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_hide_titlebar_when_maximized");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Get_Hide_Titlebar_When_Maximized;

   --------------
   -- Get_Icon --
   --------------

   function Get_Icon
      (Window : not null access Gtk_Window_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal (Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_get_icon");
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Window)), Stub_Gdk_Pixbuf));
   end Get_Icon;

   -------------------
   -- Get_Icon_List --
   -------------------

   function Get_Icon_List
      (Window : not null access Gtk_Window_Record)
       return Glib.Object.Object_Simple_List.Glist
   is
      function Internal (Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_get_icon_list");
      Tmp_Return : Glib.Object.Object_Simple_List.Glist;
   begin
      Glib.Object.Object_Simple_List.Set_Object (Tmp_Return, Internal (Get_Object (Window)));
      return Tmp_Return;
   end Get_Icon_List;

   -------------------
   -- Get_Icon_Name --
   -------------------

   function Get_Icon_Name
      (Window : not null access Gtk_Window_Record) return UTF8_String
   is
      function Internal
         (Window : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_window_get_icon_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Window)));
   end Get_Icon_Name;

   ---------------------------
   -- Get_Mnemonic_Modifier --
   ---------------------------

   function Get_Mnemonic_Modifier
      (Window : not null access Gtk_Window_Record)
       return Gdk.Types.Gdk_Modifier_Type
   is
      function Internal
         (Window : System.Address) return Gdk.Types.Gdk_Modifier_Type;
      pragma Import (C, Internal, "gtk_window_get_mnemonic_modifier");
   begin
      return Internal (Get_Object (Window));
   end Get_Mnemonic_Modifier;

   ---------------------------
   -- Get_Mnemonics_Visible --
   ---------------------------

   function Get_Mnemonics_Visible
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_mnemonics_visible");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Get_Mnemonics_Visible;

   ---------------
   -- Get_Modal --
   ---------------

   function Get_Modal
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_modal");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Get_Modal;

   ------------------
   -- Get_Position --
   ------------------

   procedure Get_Position
      (Window : not null access Gtk_Window_Record;
       Root_X : out Glib.Gint;
       Root_Y : out Glib.Gint)
   is
      procedure Internal
         (Window : System.Address;
          Root_X : out Glib.Gint;
          Root_Y : out Glib.Gint);
      pragma Import (C, Internal, "gtk_window_get_position");
   begin
      Internal (Get_Object (Window), Root_X, Root_Y);
   end Get_Position;

   -------------------
   -- Get_Resizable --
   -------------------

   function Get_Resizable
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_resizable");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Get_Resizable;

   --------------------------
   -- Get_Resize_Grip_Area --
   --------------------------

   procedure Get_Resize_Grip_Area
      (Window    : not null access Gtk_Window_Record;
       Rect      : out Gdk.Rectangle.Gdk_Rectangle;
       retrieved : out Boolean)
   is
      function Internal
         (Window   : System.Address;
          Acc_Rect : access Gdk.Rectangle.Gdk_Rectangle)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_resize_grip_area");
      Acc_Rect   : aliased Gdk.Rectangle.Gdk_Rectangle;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Window), Acc_Rect'Access);
      Rect := Acc_Rect;
      retrieved := Tmp_Return /= 0;
   end Get_Resize_Grip_Area;

   --------------
   -- Get_Role --
   --------------

   function Get_Role
      (Window : not null access Gtk_Window_Record) return UTF8_String
   is
      function Internal
         (Window : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_window_get_role");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Window)));
   end Get_Role;

   ----------------
   -- Get_Screen --
   ----------------

   function Get_Screen
      (Window : not null access Gtk_Window_Record)
       return Gdk.Screen.Gdk_Screen
   is
      function Internal (Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_get_screen");
      Stub_Gdk_Screen : Gdk.Screen.Gdk_Screen_Record;
   begin
      return Gdk.Screen.Gdk_Screen (Get_User_Data (Internal (Get_Object (Window)), Stub_Gdk_Screen));
   end Get_Screen;

   --------------
   -- Get_Size --
   --------------

   procedure Get_Size
      (Window : not null access Gtk_Window_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint)
   is
      procedure Internal
         (Window : System.Address;
          Width  : out Glib.Gint;
          Height : out Glib.Gint);
      pragma Import (C, Internal, "gtk_window_get_size");
   begin
      Internal (Get_Object (Window), Width, Height);
   end Get_Size;

   -------------------------
   -- Get_Skip_Pager_Hint --
   -------------------------

   function Get_Skip_Pager_Hint
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_skip_pager_hint");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Get_Skip_Pager_Hint;

   ---------------------------
   -- Get_Skip_Taskbar_Hint --
   ---------------------------

   function Get_Skip_Taskbar_Hint
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_skip_taskbar_hint");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Get_Skip_Taskbar_Hint;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
      (Window : not null access Gtk_Window_Record) return UTF8_String
   is
      function Internal
         (Window : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_window_get_title");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Window)));
   end Get_Title;

   ------------------
   -- Get_Titlebar --
   ------------------

   function Get_Titlebar
      (Window : not null access Gtk_Window_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_get_titlebar");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Window)), Stub_Gtk_Widget));
   end Get_Titlebar;

   -----------------------
   -- Get_Transient_For --
   -----------------------

   function Get_Transient_For
      (Window : not null access Gtk_Window_Record) return Gtk_Window
   is
      function Internal (Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_get_transient_for");
      Stub_Gtk_Window : Gtk_Window_Record;
   begin
      return Gtk.Window.Gtk_Window (Get_User_Data (Internal (Get_Object (Window)), Stub_Gtk_Window));
   end Get_Transient_For;

   -------------------
   -- Get_Type_Hint --
   -------------------

   function Get_Type_Hint
      (Window : not null access Gtk_Window_Record)
       return Gdk.Window.Gdk_Window_Type_Hint
   is
      function Internal
         (Window : System.Address) return Gdk.Window.Gdk_Window_Type_Hint;
      pragma Import (C, Internal, "gtk_window_get_type_hint");
   begin
      return Internal (Get_Object (Window));
   end Get_Type_Hint;

   ----------------------
   -- Get_Urgency_Hint --
   ----------------------

   function Get_Urgency_Hint
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_urgency_hint");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Get_Urgency_Hint;

   ---------------------
   -- Get_Window_Type --
   ---------------------

   function Get_Window_Type
      (Window : not null access Gtk_Window_Record)
       return Gtk.Enums.Gtk_Window_Type
   is
      function Internal
         (Window : System.Address) return Gtk.Enums.Gtk_Window_Type;
      pragma Import (C, Internal, "gtk_window_get_window_type");
   begin
      return Internal (Get_Object (Window));
   end Get_Window_Type;

   ---------------
   -- Has_Group --
   ---------------

   function Has_Group
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_has_group");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Has_Group;

   ------------------------
   -- Has_Toplevel_Focus --
   ------------------------

   function Has_Toplevel_Focus
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_has_toplevel_focus");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Has_Toplevel_Focus;

   -------------
   -- Iconify --
   -------------

   procedure Iconify (Window : not null access Gtk_Window_Record) is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_iconify");
   begin
      Internal (Get_Object (Window));
   end Iconify;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_is_active");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Is_Active;

   ------------------
   -- Is_Maximized --
   ------------------

   function Is_Maximized
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_is_maximized");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Is_Maximized;

   ------------------
   -- List_Windows --
   ------------------

   function List_Windows
      (Window_Group : not null access Gtk_Window_Group_Record)
       return Gtk.Widget.Widget_List.Glist
   is
      function Internal
         (Window_Group : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_group_list_windows");
      Tmp_Return : Gtk.Widget.Widget_List.Glist;
   begin
      Gtk.Widget.Widget_List.Set_Object (Tmp_Return, Internal (Get_Object (Window_Group)));
      return Tmp_Return;
   end List_Windows;

   --------------
   -- Maximize --
   --------------

   procedure Maximize (Window : not null access Gtk_Window_Record) is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_maximize");
   begin
      Internal (Get_Object (Window));
   end Maximize;

   -----------------------
   -- Mnemonic_Activate --
   -----------------------

   function Mnemonic_Activate
      (Window   : not null access Gtk_Window_Record;
       Keyval   : Gdk.Types.Gdk_Key_Type;
       Modifier : Gdk.Types.Gdk_Modifier_Type) return Boolean
   is
      function Internal
         (Window   : System.Address;
          Keyval   : Gdk.Types.Gdk_Key_Type;
          Modifier : Gdk.Types.Gdk_Modifier_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_mnemonic_activate");
   begin
      return Internal (Get_Object (Window), Keyval, Modifier) /= 0;
   end Mnemonic_Activate;

   ----------
   -- Move --
   ----------

   procedure Move
      (Window : not null access Gtk_Window_Record;
       X      : Glib.Gint;
       Y      : Glib.Gint)
   is
      procedure Internal
         (Window : System.Address;
          X      : Glib.Gint;
          Y      : Glib.Gint);
      pragma Import (C, Internal, "gtk_window_move");
   begin
      Internal (Get_Object (Window), X, Y);
   end Move;

   --------------------
   -- Parse_Geometry --
   --------------------

   function Parse_Geometry
      (Window   : not null access Gtk_Window_Record;
       Geometry : UTF8_String) return Boolean
   is
      function Internal
         (Window   : System.Address;
          Geometry : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_parse_geometry");
      Tmp_Geometry : Gtkada.Types.Chars_Ptr := New_String (Geometry);
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Window), Tmp_Geometry);
      Free (Tmp_Geometry);
      return Tmp_Return /= 0;
   end Parse_Geometry;

   -------------
   -- Present --
   -------------

   procedure Present (Window : not null access Gtk_Window_Record) is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_present");
   begin
      Internal (Get_Object (Window));
   end Present;

   -----------------------
   -- Present_With_Time --
   -----------------------

   procedure Present_With_Time
      (Window    : not null access Gtk_Window_Record;
       Timestamp : Guint32)
   is
      procedure Internal (Window : System.Address; Timestamp : Guint32);
      pragma Import (C, Internal, "gtk_window_present_with_time");
   begin
      Internal (Get_Object (Window), Timestamp);
   end Present_With_Time;

   -------------------------
   -- Propagate_Key_Event --
   -------------------------

   function Propagate_Key_Event
      (Window : not null access Gtk_Window_Record;
       Event  : Gdk.Event.Gdk_Event_Key) return Boolean
   is
      function Internal
         (Window : System.Address;
          Event  : Gdk.Event.Gdk_Event_Key) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_propagate_key_event");
   begin
      return Internal (Get_Object (Window), Event) /= 0;
   end Propagate_Key_Event;

   ------------------------
   -- Remove_Accel_Group --
   ------------------------

   procedure Remove_Accel_Group
      (Window      : not null access Gtk_Window_Record;
       Accel_Group : not null access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class)
   is
      procedure Internal
         (Window      : System.Address;
          Accel_Group : System.Address);
      pragma Import (C, Internal, "gtk_window_remove_accel_group");
   begin
      Internal (Get_Object (Window), Get_Object (Accel_Group));
   end Remove_Accel_Group;

   ---------------------
   -- Remove_Mnemonic --
   ---------------------

   procedure Remove_Mnemonic
      (Window : not null access Gtk_Window_Record;
       Keyval : Gdk.Types.Gdk_Key_Type;
       Target : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Window : System.Address;
          Keyval : Gdk.Types.Gdk_Key_Type;
          Target : System.Address);
      pragma Import (C, Internal, "gtk_window_remove_mnemonic");
   begin
      Internal (Get_Object (Window), Keyval, Get_Object (Target));
   end Remove_Mnemonic;

   -------------------
   -- Remove_Window --
   -------------------

   procedure Remove_Window
      (Window_Group : not null access Gtk_Window_Group_Record;
       Window       : not null access Gtk_Window_Record'Class)
   is
      procedure Internal
         (Window_Group : System.Address;
          Window       : System.Address);
      pragma Import (C, Internal, "gtk_window_group_remove_window");
   begin
      Internal (Get_Object (Window_Group), Get_Object (Window));
   end Remove_Window;

   ------------------------------
   -- Reshow_With_Initial_Size --
   ------------------------------

   procedure Reshow_With_Initial_Size
      (Window : not null access Gtk_Window_Record)
   is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_reshow_with_initial_size");
   begin
      Internal (Get_Object (Window));
   end Reshow_With_Initial_Size;

   ------------
   -- Resize --
   ------------

   procedure Resize
      (Window : not null access Gtk_Window_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint)
   is
      procedure Internal
         (Window : System.Address;
          Width  : Glib.Gint;
          Height : Glib.Gint);
      pragma Import (C, Internal, "gtk_window_resize");
   begin
      Internal (Get_Object (Window), Width, Height);
   end Resize;

   ----------------------------
   -- Resize_Grip_Is_Visible --
   ----------------------------

   function Resize_Grip_Is_Visible
      (Window : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_resize_grip_is_visible");
   begin
      return Internal (Get_Object (Window)) /= 0;
   end Resize_Grip_Is_Visible;

   ------------------------
   -- Resize_To_Geometry --
   ------------------------

   procedure Resize_To_Geometry
      (Window : not null access Gtk_Window_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint)
   is
      procedure Internal
         (Window : System.Address;
          Width  : Glib.Gint;
          Height : Glib.Gint);
      pragma Import (C, Internal, "gtk_window_resize_to_geometry");
   begin
      Internal (Get_Object (Window), Width, Height);
   end Resize_To_Geometry;

   ----------------------
   -- Set_Accept_Focus --
   ----------------------

   procedure Set_Accept_Focus
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Window : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_accept_focus");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Accept_Focus;

   ---------------------
   -- Set_Attached_To --
   ---------------------

   procedure Set_Attached_To
      (Window        : not null access Gtk_Window_Record;
       Attach_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Window        : System.Address;
          Attach_Widget : System.Address);
      pragma Import (C, Internal, "gtk_window_set_attached_to");
   begin
      Internal (Get_Object (Window), Get_Object_Or_Null (GObject (Attach_Widget)));
   end Set_Attached_To;

   -------------------
   -- Set_Decorated --
   -------------------

   procedure Set_Decorated
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Window : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_decorated");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Decorated;

   -----------------
   -- Set_Default --
   -----------------

   procedure Set_Default
      (Window         : not null access Gtk_Window_Record;
       Default_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Window         : System.Address;
          Default_Widget : System.Address);
      pragma Import (C, Internal, "gtk_window_set_default");
   begin
      Internal (Get_Object (Window), Get_Object_Or_Null (GObject (Default_Widget)));
   end Set_Default;

   --------------------------
   -- Set_Default_Geometry --
   --------------------------

   procedure Set_Default_Geometry
      (Window : not null access Gtk_Window_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint)
   is
      procedure Internal
         (Window : System.Address;
          Width  : Glib.Gint;
          Height : Glib.Gint);
      pragma Import (C, Internal, "gtk_window_set_default_geometry");
   begin
      Internal (Get_Object (Window), Width, Height);
   end Set_Default_Geometry;

   ----------------------
   -- Set_Default_Size --
   ----------------------

   procedure Set_Default_Size
      (Window : not null access Gtk_Window_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint)
   is
      procedure Internal
         (Window : System.Address;
          Width  : Glib.Gint;
          Height : Glib.Gint);
      pragma Import (C, Internal, "gtk_window_set_default_size");
   begin
      Internal (Get_Object (Window), Width, Height);
   end Set_Default_Size;

   -------------------
   -- Set_Deletable --
   -------------------

   procedure Set_Deletable
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Window : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_deletable");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Deletable;

   -----------------------------
   -- Set_Destroy_With_Parent --
   -----------------------------

   procedure Set_Destroy_With_Parent
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Window : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_destroy_with_parent");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Destroy_With_Parent;

   ---------------
   -- Set_Focus --
   ---------------

   procedure Set_Focus
      (Window : not null access Gtk_Window_Record;
       Focus  : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Window : System.Address; Focus : System.Address);
      pragma Import (C, Internal, "gtk_window_set_focus");
   begin
      Internal (Get_Object (Window), Get_Object_Or_Null (GObject (Focus)));
   end Set_Focus;

   ----------------------
   -- Set_Focus_On_Map --
   ----------------------

   procedure Set_Focus_On_Map
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Window : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_focus_on_map");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Focus_On_Map;

   -----------------------
   -- Set_Focus_Visible --
   -----------------------

   procedure Set_Focus_Visible
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Window : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_focus_visible");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Focus_Visible;

   ------------------------
   -- Set_Geometry_Hints --
   ------------------------

   procedure Set_Geometry_Hints
      (Window          : not null access Gtk_Window_Record;
       Geometry_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
       Geometry        : Gdk.Window.Gdk_Geometry;
       Geom_Mask       : Gdk.Window.Gdk_Window_Hints)
   is
      procedure Internal
         (Window          : System.Address;
          Geometry_Widget : System.Address;
          Geometry        : Gdk.Window.Gdk_Geometry;
          Geom_Mask       : Gdk.Window.Gdk_Window_Hints);
      pragma Import (C, Internal, "gtk_window_set_geometry_hints");
   begin
      Internal (Get_Object (Window), Get_Object_Or_Null (GObject (Geometry_Widget)), Geometry, Geom_Mask);
   end Set_Geometry_Hints;

   -----------------
   -- Set_Gravity --
   -----------------

   procedure Set_Gravity
      (Window  : not null access Gtk_Window_Record;
       Gravity : Gdk.Window.Gdk_Gravity)
   is
      procedure Internal
         (Window  : System.Address;
          Gravity : Gdk.Window.Gdk_Gravity);
      pragma Import (C, Internal, "gtk_window_set_gravity");
   begin
      Internal (Get_Object (Window), Gravity);
   end Set_Gravity;

   -------------------------
   -- Set_Has_Resize_Grip --
   -------------------------

   procedure Set_Has_Resize_Grip
      (Window : not null access Gtk_Window_Record;
       Value  : Boolean)
   is
      procedure Internal (Window : System.Address; Value : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_has_resize_grip");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Value));
   end Set_Has_Resize_Grip;

   ----------------------------
   -- Set_Has_User_Ref_Count --
   ----------------------------

   procedure Set_Has_User_Ref_Count
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Window : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_has_user_ref_count");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Has_User_Ref_Count;

   --------------------------------------
   -- Set_Hide_Titlebar_When_Maximized --
   --------------------------------------

   procedure Set_Hide_Titlebar_When_Maximized
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Window : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_hide_titlebar_when_maximized");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Hide_Titlebar_When_Maximized;

   --------------
   -- Set_Icon --
   --------------

   procedure Set_Icon
      (Window : not null access Gtk_Window_Record;
       Icon   : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal (Window : System.Address; Icon : System.Address);
      pragma Import (C, Internal, "gtk_window_set_icon");
   begin
      Internal (Get_Object (Window), Get_Object_Or_Null (GObject (Icon)));
   end Set_Icon;

   ------------------------
   -- Set_Icon_From_File --
   ------------------------

   function Set_Icon_From_File
      (Window   : not null access Gtk_Window_Record;
       Filename : UTF8_String) return Boolean
   is
      function Internal
         (Window   : System.Address;
          Filename : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_set_icon_from_file");
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Window), Tmp_Filename);
      Free (Tmp_Filename);
      return Tmp_Return /= 0;
   end Set_Icon_From_File;

   -------------------
   -- Set_Icon_List --
   -------------------

   procedure Set_Icon_List
      (Window : not null access Gtk_Window_Record;
       List   : Glib.Object.Object_Simple_List.Glist)
   is
      procedure Internal (Window : System.Address; List : System.Address);
      pragma Import (C, Internal, "gtk_window_set_icon_list");
   begin
      Internal (Get_Object (Window), Glib.Object.Object_Simple_List.Get_Object (List));
   end Set_Icon_List;

   -------------------
   -- Set_Icon_Name --
   -------------------

   procedure Set_Icon_Name
      (Window : not null access Gtk_Window_Record;
       Name   : UTF8_String := "")
   is
      procedure Internal
         (Window : System.Address;
          Name   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_window_set_icon_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Name = "" then
         Tmp_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Name := New_String (Name);
      end if;
      Internal (Get_Object (Window), Tmp_Name);
      Free (Tmp_Name);
   end Set_Icon_Name;

   --------------------
   -- Set_Keep_Above --
   --------------------

   procedure Set_Keep_Above
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Window : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_keep_above");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Keep_Above;

   --------------------
   -- Set_Keep_Below --
   --------------------

   procedure Set_Keep_Below
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Window : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_keep_below");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Keep_Below;

   ---------------------------
   -- Set_Mnemonic_Modifier --
   ---------------------------

   procedure Set_Mnemonic_Modifier
      (Window   : not null access Gtk_Window_Record;
       Modifier : Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal
         (Window   : System.Address;
          Modifier : Gdk.Types.Gdk_Modifier_Type);
      pragma Import (C, Internal, "gtk_window_set_mnemonic_modifier");
   begin
      Internal (Get_Object (Window), Modifier);
   end Set_Mnemonic_Modifier;

   ---------------------------
   -- Set_Mnemonics_Visible --
   ---------------------------

   procedure Set_Mnemonics_Visible
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Window : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_mnemonics_visible");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Mnemonics_Visible;

   ---------------
   -- Set_Modal --
   ---------------

   procedure Set_Modal
      (Window : not null access Gtk_Window_Record;
       Modal  : Boolean := True)
   is
      procedure Internal (Window : System.Address; Modal : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_modal");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Modal));
   end Set_Modal;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
      (Window   : not null access Gtk_Window_Record;
       Position : Gtk.Enums.Gtk_Window_Position)
   is
      procedure Internal
         (Window   : System.Address;
          Position : Gtk.Enums.Gtk_Window_Position);
      pragma Import (C, Internal, "gtk_window_set_position");
   begin
      Internal (Get_Object (Window), Position);
   end Set_Position;

   -------------------
   -- Set_Resizable --
   -------------------

   procedure Set_Resizable
      (Window    : not null access Gtk_Window_Record;
       Resizable : Boolean)
   is
      procedure Internal
         (Window    : System.Address;
          Resizable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_resizable");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Resizable));
   end Set_Resizable;

   --------------
   -- Set_Role --
   --------------

   procedure Set_Role
      (Window : not null access Gtk_Window_Record;
       Role   : UTF8_String)
   is
      procedure Internal
         (Window : System.Address;
          Role   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_window_set_role");
      Tmp_Role : Gtkada.Types.Chars_Ptr := New_String (Role);
   begin
      Internal (Get_Object (Window), Tmp_Role);
      Free (Tmp_Role);
   end Set_Role;

   ----------------
   -- Set_Screen --
   ----------------

   procedure Set_Screen
      (Window : not null access Gtk_Window_Record;
       Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class)
   is
      procedure Internal (Window : System.Address; Screen : System.Address);
      pragma Import (C, Internal, "gtk_window_set_screen");
   begin
      Internal (Get_Object (Window), Get_Object (Screen));
   end Set_Screen;

   -------------------------
   -- Set_Skip_Pager_Hint --
   -------------------------

   procedure Set_Skip_Pager_Hint
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Window : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_skip_pager_hint");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Skip_Pager_Hint;

   ---------------------------
   -- Set_Skip_Taskbar_Hint --
   ---------------------------

   procedure Set_Skip_Taskbar_Hint
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Window : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_skip_taskbar_hint");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Skip_Taskbar_Hint;

   --------------------
   -- Set_Startup_Id --
   --------------------

   procedure Set_Startup_Id
      (Window     : not null access Gtk_Window_Record;
       Startup_Id : UTF8_String)
   is
      procedure Internal
         (Window     : System.Address;
          Startup_Id : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_window_set_startup_id");
      Tmp_Startup_Id : Gtkada.Types.Chars_Ptr := New_String (Startup_Id);
   begin
      Internal (Get_Object (Window), Tmp_Startup_Id);
      Free (Tmp_Startup_Id);
   end Set_Startup_Id;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
      (Window : not null access Gtk_Window_Record;
       Title  : UTF8_String)
   is
      procedure Internal
         (Window : System.Address;
          Title  : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_window_set_title");
      Tmp_Title : Gtkada.Types.Chars_Ptr := New_String (Title);
   begin
      Internal (Get_Object (Window), Tmp_Title);
      Free (Tmp_Title);
   end Set_Title;

   ------------------
   -- Set_Titlebar --
   ------------------

   procedure Set_Titlebar
      (Window   : not null access Gtk_Window_Record;
       Titlebar : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Window   : System.Address;
          Titlebar : System.Address);
      pragma Import (C, Internal, "gtk_window_set_titlebar");
   begin
      Internal (Get_Object (Window), Get_Object_Or_Null (GObject (Titlebar)));
   end Set_Titlebar;

   -----------------------
   -- Set_Transient_For --
   -----------------------

   procedure Set_Transient_For
      (Window : not null access Gtk_Window_Record;
       Parent : access Gtk_Window_Record'Class)
   is
      procedure Internal (Window : System.Address; Parent : System.Address);
      pragma Import (C, Internal, "gtk_window_set_transient_for");
   begin
      Internal (Get_Object (Window), Get_Object_Or_Null (GObject (Parent)));
   end Set_Transient_For;

   -------------------
   -- Set_Type_Hint --
   -------------------

   procedure Set_Type_Hint
      (Window : not null access Gtk_Window_Record;
       Hint   : Gdk.Window.Gdk_Window_Type_Hint)
   is
      procedure Internal
         (Window : System.Address;
          Hint   : Gdk.Window.Gdk_Window_Type_Hint);
      pragma Import (C, Internal, "gtk_window_set_type_hint");
   begin
      Internal (Get_Object (Window), Hint);
   end Set_Type_Hint;

   ----------------------
   -- Set_Urgency_Hint --
   ----------------------

   procedure Set_Urgency_Hint
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Window : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_urgency_hint");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Urgency_Hint;

   -----------------
   -- Set_Wmclass --
   -----------------

   procedure Set_Wmclass
      (Window        : not null access Gtk_Window_Record;
       Wmclass_Name  : UTF8_String;
       Wmclass_Class : UTF8_String)
   is
      procedure Internal
         (Window        : System.Address;
          Wmclass_Name  : Gtkada.Types.Chars_Ptr;
          Wmclass_Class : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_window_set_wmclass");
      Tmp_Wmclass_Name  : Gtkada.Types.Chars_Ptr := New_String (Wmclass_Name);
      Tmp_Wmclass_Class : Gtkada.Types.Chars_Ptr := New_String (Wmclass_Class);
   begin
      Internal (Get_Object (Window), Tmp_Wmclass_Name, Tmp_Wmclass_Class);
      Free (Tmp_Wmclass_Class);
      Free (Tmp_Wmclass_Name);
   end Set_Wmclass;

   -----------
   -- Stick --
   -----------

   procedure Stick (Window : not null access Gtk_Window_Record) is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_stick");
   begin
      Internal (Get_Object (Window));
   end Stick;

   ------------------
   -- Unfullscreen --
   ------------------

   procedure Unfullscreen (Window : not null access Gtk_Window_Record) is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_unfullscreen");
   begin
      Internal (Get_Object (Window));
   end Unfullscreen;

   ----------------
   -- Unmaximize --
   ----------------

   procedure Unmaximize (Window : not null access Gtk_Window_Record) is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_unmaximize");
   begin
      Internal (Get_Object (Window));
   end Unmaximize;

   -------------
   -- Unstick --
   -------------

   procedure Unstick (Window : not null access Gtk_Window_Record) is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_unstick");
   begin
      Internal (Get_Object (Window));
   end Unstick;

   ---------------------------
   -- Get_Default_Icon_List --
   ---------------------------

   function Get_Default_Icon_List return Glib.Object.Object_Simple_List.Glist is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_window_get_default_icon_list");
      Tmp_Return : Glib.Object.Object_Simple_List.Glist;
   begin
      Glib.Object.Object_Simple_List.Set_Object (Tmp_Return, Internal);
      return Tmp_Return;
   end Get_Default_Icon_List;

   ---------------------------
   -- Get_Default_Icon_Name --
   ---------------------------

   function Get_Default_Icon_Name return UTF8_String is
      function Internal return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_window_get_default_icon_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal);
   end Get_Default_Icon_Name;

   --------------------
   -- List_Toplevels --
   --------------------

   function List_Toplevels return Gtk.Widget.Widget_List.Glist is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_window_list_toplevels");
      Tmp_Return : Gtk.Widget.Widget_List.Glist;
   begin
      Gtk.Widget.Widget_List.Set_Object (Tmp_Return, Internal);
      return Tmp_Return;
   end List_Toplevels;

   -----------------------------------
   -- Set_Auto_Startup_Notification --
   -----------------------------------

   procedure Set_Auto_Startup_Notification (Setting : Boolean) is
      procedure Internal (Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_auto_startup_notification");
   begin
      Internal (Boolean'Pos (Setting));
   end Set_Auto_Startup_Notification;

   ----------------------
   -- Set_Default_Icon --
   ----------------------

   procedure Set_Default_Icon
      (Icon : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal (Icon : System.Address);
      pragma Import (C, Internal, "gtk_window_set_default_icon");
   begin
      Internal (Get_Object (Icon));
   end Set_Default_Icon;

   --------------------------------
   -- Set_Default_Icon_From_File --
   --------------------------------

   function Set_Default_Icon_From_File
      (Filename : UTF8_String) return Boolean
   is
      function Internal
         (Filename : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_set_default_icon_from_file");
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Tmp_Filename);
      Free (Tmp_Filename);
      return Tmp_Return /= 0;
   end Set_Default_Icon_From_File;

   ---------------------------
   -- Set_Default_Icon_List --
   ---------------------------

   procedure Set_Default_Icon_List
      (List : Glib.Object.Object_Simple_List.Glist)
   is
      procedure Internal (List : System.Address);
      pragma Import (C, Internal, "gtk_window_set_default_icon_list");
   begin
      Internal (Glib.Object.Object_Simple_List.Get_Object (List));
   end Set_Default_Icon_List;

   ---------------------------
   -- Set_Default_Icon_Name --
   ---------------------------

   procedure Set_Default_Icon_Name (Name : UTF8_String) is
      procedure Internal (Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_window_set_default_icon_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Tmp_Name);
      Free (Tmp_Name);
   end Set_Default_Icon_Name;

   -------------------------------
   -- Set_Interactive_Debugging --
   -------------------------------

   procedure Set_Interactive_Debugging (Enable : Boolean) is
      procedure Internal (Enable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_interactive_debugging");
   begin
      Internal (Boolean'Pos (Enable));
   end Set_Interactive_Debugging;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Window_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Window_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Window_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Window_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Window_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Window_Gtk_Widget_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Widget_Void);

   procedure Connect
      (Object  : access Gtk_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Window_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Window_Boolean_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Window_Gtk_Widget_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean_Boolean);

   procedure Marsh_GObject_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Widget_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Window_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Window_Boolean_Boolean);

   procedure Marsh_Gtk_Window_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Window_Gtk_Widget_Void);

   procedure Marsh_Gtk_Window_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Window_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Window_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Window_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Window_Boolean_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Window_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Window_Gtk_Widget_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Window_Gtk_Widget_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Widget_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   -----------------------------------
   -- Marsh_GObject_Boolean_Boolean --
   -----------------------------------

   procedure Marsh_GObject_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Boolean (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean_Boolean;

   -----------------------------------
   -- Marsh_GObject_Gtk_Widget_Void --
   -----------------------------------

   procedure Marsh_GObject_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Widget_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Widget_Void;

   ------------------------
   -- Marsh_GObject_Void --
   ------------------------

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   --------------------------------------
   -- Marsh_Gtk_Window_Boolean_Boolean --
   --------------------------------------

   procedure Marsh_Gtk_Window_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Window_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Window := Gtk_Window (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Boolean (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Window_Boolean_Boolean;

   --------------------------------------
   -- Marsh_Gtk_Window_Gtk_Widget_Void --
   --------------------------------------

   procedure Marsh_Gtk_Window_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Window_Gtk_Widget_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Window := Gtk_Window (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Window_Gtk_Widget_Void;

   ---------------------------
   -- Marsh_Gtk_Window_Void --
   ---------------------------

   procedure Marsh_Gtk_Window_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Window_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Window := Gtk_Window (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Window_Void;

   -------------------------
   -- On_Activate_Default --
   -------------------------

   procedure On_Activate_Default
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_Gtk_Window_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate-default" & ASCII.NUL, Call, After);
   end On_Activate_Default;

   -------------------------
   -- On_Activate_Default --
   -------------------------

   procedure On_Activate_Default
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate-default" & ASCII.NUL, Call, After, Slot);
   end On_Activate_Default;

   -----------------------
   -- On_Activate_Focus --
   -----------------------

   procedure On_Activate_Focus
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_Gtk_Window_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate-focus" & ASCII.NUL, Call, After);
   end On_Activate_Focus;

   -----------------------
   -- On_Activate_Focus --
   -----------------------

   procedure On_Activate_Focus
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate-focus" & ASCII.NUL, Call, After, Slot);
   end On_Activate_Focus;

   -------------------------
   -- On_Enable_Debugging --
   -------------------------

   procedure On_Enable_Debugging
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_Gtk_Window_Boolean_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "enable-debugging" & ASCII.NUL, Call, After);
   end On_Enable_Debugging;

   -------------------------
   -- On_Enable_Debugging --
   -------------------------

   procedure On_Enable_Debugging
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_GObject_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "enable-debugging" & ASCII.NUL, Call, After, Slot);
   end On_Enable_Debugging;

   ---------------------
   -- On_Keys_Changed --
   ---------------------

   procedure On_Keys_Changed
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_Gtk_Window_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "keys-changed" & ASCII.NUL, Call, After);
   end On_Keys_Changed;

   ---------------------
   -- On_Keys_Changed --
   ---------------------

   procedure On_Keys_Changed
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "keys-changed" & ASCII.NUL, Call, After, Slot);
   end On_Keys_Changed;

   ------------------
   -- On_Set_Focus --
   ------------------

   procedure On_Set_Focus
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_Gtk_Window_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "set-focus" & ASCII.NUL, Call, After);
   end On_Set_Focus;

   ------------------
   -- On_Set_Focus --
   ------------------

   procedure On_Set_Focus
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "set-focus" & ASCII.NUL, Call, After, Slot);
   end On_Set_Focus;

end Gtk.Window;
