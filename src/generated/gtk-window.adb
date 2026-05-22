------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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
with Gdk.Surface;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;

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

   procedure Gtk_New (Self : out Gtk_Window) is
   begin
      Self := new Gtk_Window_Record;
      Gtk.Window.Initialize (Self);
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

   function Gtk_Window_New return Gtk_Window is
      Self : constant Gtk_Window := new Gtk_Window_Record;
   begin
      Gtk.Window.Initialize (Self);
      return Self;
   end Gtk_Window_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Window_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_window_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
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

   -----------
   -- Close --
   -----------

   procedure Close (Self : not null access Gtk_Window_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_window_close");
   begin
      Internal (Get_Object (Self));
   end Close;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : not null access Gtk_Window_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_window_destroy");
   begin
      Internal (Get_Object (Self));
   end Destroy;

   ----------------
   -- Fullscreen --
   ----------------

   procedure Fullscreen (Self : not null access Gtk_Window_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_window_fullscreen");
   begin
      Internal (Get_Object (Self));
   end Fullscreen;

   ---------------------------
   -- Fullscreen_On_Monitor --
   ---------------------------

   procedure Fullscreen_On_Monitor
      (Self    : not null access Gtk_Window_Record;
       Monitor : not null access Gdk.Monitor.Gdk_Monitor_Record'Class)
   is
      procedure Internal (Self : System.Address; Monitor : System.Address);
      pragma Import (C, Internal, "gtk_window_fullscreen_on_monitor");
   begin
      Internal (Get_Object (Self), Get_Object (Monitor));
   end Fullscreen_On_Monitor;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
      (Self : not null access Gtk_Window_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_get_child");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Child;

   -------------------
   -- Get_Decorated --
   -------------------

   function Get_Decorated
      (Self : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_decorated");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Decorated;

   ----------------------
   -- Get_Default_Size --
   ----------------------

   procedure Get_Default_Size
      (Self   : not null access Gtk_Window_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint)
   is
      procedure Internal
         (Self   : System.Address;
          Width  : out Glib.Gint;
          Height : out Glib.Gint);
      pragma Import (C, Internal, "gtk_window_get_default_size");
   begin
      Internal (Get_Object (Self), Width, Height);
   end Get_Default_Size;

   ------------------------
   -- Get_Default_Widget --
   ------------------------

   function Get_Default_Widget
      (Self : not null access Gtk_Window_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_get_default_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Default_Widget;

   -------------------
   -- Get_Deletable --
   -------------------

   function Get_Deletable
      (Self : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_deletable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Deletable;

   -----------------------------
   -- Get_Destroy_With_Parent --
   -----------------------------

   function Get_Destroy_With_Parent
      (Self : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_destroy_with_parent");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Destroy_With_Parent;

   -----------------------
   -- Get_Focus_Visible --
   -----------------------

   function Get_Focus_Visible
      (Self : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_focus_visible");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Focus_Visible;

   -----------------
   -- Get_Gravity --
   -----------------

   function Get_Gravity
      (Self : not null access Gtk_Window_Record)
       return Gtk.Enums.Gtk_Window_Gravity
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Window_Gravity;
      pragma Import (C, Internal, "gtk_window_get_gravity");
   begin
      return Internal (Get_Object (Self));
   end Get_Gravity;

   ---------------
   -- Get_Group --
   ---------------

   function Get_Group
      (Self : not null access Gtk_Window_Record) return Gtk_Window_Group
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_get_group");
      Stub_Gtk_Window_Group : Gtk_Window_Group_Record;
   begin
      return Gtk.Window.Gtk_Window_Group (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Window_Group));
   end Get_Group;

   ------------------------------
   -- Get_Handle_Menubar_Accel --
   ------------------------------

   function Get_Handle_Menubar_Accel
      (Self : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_handle_menubar_accel");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Handle_Menubar_Accel;

   -----------------------
   -- Get_Hide_On_Close --
   -----------------------

   function Get_Hide_On_Close
      (Self : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_hide_on_close");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Hide_On_Close;

   -------------------
   -- Get_Icon_Name --
   -------------------

   function Get_Icon_Name
      (Self : not null access Gtk_Window_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_window_get_icon_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Icon_Name;

   ---------------------------
   -- Get_Mnemonics_Visible --
   ---------------------------

   function Get_Mnemonics_Visible
      (Self : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_mnemonics_visible");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Mnemonics_Visible;

   ---------------
   -- Get_Modal --
   ---------------

   function Get_Modal
      (Self : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_modal");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Modal;

   -------------------
   -- Get_Resizable --
   -------------------

   function Get_Resizable
      (Self : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_get_resizable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Resizable;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
      (Self : not null access Gtk_Window_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_window_get_title");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Title;

   ------------------
   -- Get_Titlebar --
   ------------------

   function Get_Titlebar
      (Self : not null access Gtk_Window_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_get_titlebar");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Titlebar;

   -----------------------
   -- Get_Transient_For --
   -----------------------

   function Get_Transient_For
      (Self : not null access Gtk_Window_Record) return Gtk_Window
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_get_transient_for");
      Stub_Gtk_Window : Gtk_Window_Record;
   begin
      return Gtk.Window.Gtk_Window (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Window));
   end Get_Transient_For;

   ---------------
   -- Has_Group --
   ---------------

   function Has_Group
      (Self : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_has_group");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Has_Group;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active
      (Self : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_is_active");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Active;

   -------------------
   -- Is_Fullscreen --
   -------------------

   function Is_Fullscreen
      (Self : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_is_fullscreen");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Fullscreen;

   ------------------
   -- Is_Maximized --
   ------------------

   function Is_Maximized
      (Self : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_is_maximized");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Maximized;

   ------------------
   -- Is_Suspended --
   ------------------

   function Is_Suspended
      (Self : not null access Gtk_Window_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_window_is_suspended");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Suspended;

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

   procedure Maximize (Self : not null access Gtk_Window_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_window_maximize");
   begin
      Internal (Get_Object (Self));
   end Maximize;

   --------------
   -- Minimize --
   --------------

   procedure Minimize (Self : not null access Gtk_Window_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_window_minimize");
   begin
      Internal (Get_Object (Self));
   end Minimize;

   -------------
   -- Present --
   -------------

   procedure Present (Self : not null access Gtk_Window_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_window_present");
   begin
      Internal (Get_Object (Self));
   end Present;

   -----------------------
   -- Present_With_Time --
   -----------------------

   procedure Present_With_Time
      (Self      : not null access Gtk_Window_Record;
       Timestamp : Guint32)
   is
      procedure Internal (Self : System.Address; Timestamp : Guint32);
      pragma Import (C, Internal, "gtk_window_present_with_time");
   begin
      Internal (Get_Object (Self), Timestamp);
   end Present_With_Time;

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

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
      (Self  : not null access Gtk_Window_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_window_set_child");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Child)));
   end Set_Child;

   -------------------
   -- Set_Decorated --
   -------------------

   procedure Set_Decorated
      (Self    : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Self : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_decorated");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_Decorated;

   ----------------------
   -- Set_Default_Size --
   ----------------------

   procedure Set_Default_Size
      (Self   : not null access Gtk_Window_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint)
   is
      procedure Internal
         (Self   : System.Address;
          Width  : Glib.Gint;
          Height : Glib.Gint);
      pragma Import (C, Internal, "gtk_window_set_default_size");
   begin
      Internal (Get_Object (Self), Width, Height);
   end Set_Default_Size;

   ------------------------
   -- Set_Default_Widget --
   ------------------------

   procedure Set_Default_Widget
      (Self           : not null access Gtk_Window_Record;
       Default_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Self           : System.Address;
          Default_Widget : System.Address);
      pragma Import (C, Internal, "gtk_window_set_default_widget");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Default_Widget)));
   end Set_Default_Widget;

   -------------------
   -- Set_Deletable --
   -------------------

   procedure Set_Deletable
      (Self    : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Self : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_deletable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_Deletable;

   -----------------------------
   -- Set_Destroy_With_Parent --
   -----------------------------

   procedure Set_Destroy_With_Parent
      (Self    : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Self : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_destroy_with_parent");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_Destroy_With_Parent;

   -----------------
   -- Set_Display --
   -----------------

   procedure Set_Display
      (Self    : not null access Gtk_Window_Record;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class)
   is
      procedure Internal (Self : System.Address; Display : System.Address);
      pragma Import (C, Internal, "gtk_window_set_display");
   begin
      Internal (Get_Object (Self), Get_Object (Display));
   end Set_Display;

   -----------------------
   -- Set_Focus_Visible --
   -----------------------

   procedure Set_Focus_Visible
      (Self    : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Self : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_focus_visible");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_Focus_Visible;

   -----------------
   -- Set_Gravity --
   -----------------

   procedure Set_Gravity
      (Self    : not null access Gtk_Window_Record;
       Gravity : Gtk.Enums.Gtk_Window_Gravity)
   is
      procedure Internal
         (Self    : System.Address;
          Gravity : Gtk.Enums.Gtk_Window_Gravity);
      pragma Import (C, Internal, "gtk_window_set_gravity");
   begin
      Internal (Get_Object (Self), Gravity);
   end Set_Gravity;

   ------------------------------
   -- Set_Handle_Menubar_Accel --
   ------------------------------

   procedure Set_Handle_Menubar_Accel
      (Self                 : not null access Gtk_Window_Record;
       Handle_Menubar_Accel : Boolean)
   is
      procedure Internal
         (Self                 : System.Address;
          Handle_Menubar_Accel : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_handle_menubar_accel");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Handle_Menubar_Accel));
   end Set_Handle_Menubar_Accel;

   -----------------------
   -- Set_Hide_On_Close --
   -----------------------

   procedure Set_Hide_On_Close
      (Self    : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Self : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_hide_on_close");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_Hide_On_Close;

   -------------------
   -- Set_Icon_Name --
   -------------------

   procedure Set_Icon_Name
      (Self : not null access Gtk_Window_Record;
       Name : UTF8_String := "")
   is
      procedure Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_window_set_icon_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Name = "" then
         Tmp_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Name := New_String (Name);
      end if;
      Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
   end Set_Icon_Name;

   ---------------------------
   -- Set_Mnemonics_Visible --
   ---------------------------

   procedure Set_Mnemonics_Visible
      (Self    : not null access Gtk_Window_Record;
       Setting : Boolean)
   is
      procedure Internal (Self : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_mnemonics_visible");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_Mnemonics_Visible;

   ---------------
   -- Set_Modal --
   ---------------

   procedure Set_Modal
      (Self  : not null access Gtk_Window_Record;
       Modal : Boolean)
   is
      procedure Internal (Self : System.Address; Modal : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_modal");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Modal));
   end Set_Modal;

   -------------------
   -- Set_Resizable --
   -------------------

   procedure Set_Resizable
      (Self      : not null access Gtk_Window_Record;
       Resizable : Boolean)
   is
      procedure Internal (Self : System.Address; Resizable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_window_set_resizable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Resizable));
   end Set_Resizable;

   --------------------
   -- Set_Startup_Id --
   --------------------

   procedure Set_Startup_Id
      (Self       : not null access Gtk_Window_Record;
       Startup_Id : UTF8_String)
   is
      procedure Internal
         (Self       : System.Address;
          Startup_Id : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_window_set_startup_id");
      Tmp_Startup_Id : Gtkada.Types.Chars_Ptr := New_String (Startup_Id);
   begin
      Internal (Get_Object (Self), Tmp_Startup_Id);
      Free (Tmp_Startup_Id);
   end Set_Startup_Id;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
      (Self  : not null access Gtk_Window_Record;
       Title : UTF8_String := "")
   is
      procedure Internal
         (Self  : System.Address;
          Title : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_window_set_title");
      Tmp_Title : Gtkada.Types.Chars_Ptr;
   begin
      if Title = "" then
         Tmp_Title := Gtkada.Types.Null_Ptr;
      else
         Tmp_Title := New_String (Title);
      end if;
      Internal (Get_Object (Self), Tmp_Title);
      Free (Tmp_Title);
   end Set_Title;

   ------------------
   -- Set_Titlebar --
   ------------------

   procedure Set_Titlebar
      (Self     : not null access Gtk_Window_Record;
       Titlebar : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Titlebar : System.Address);
      pragma Import (C, Internal, "gtk_window_set_titlebar");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Titlebar)));
   end Set_Titlebar;

   -----------------------
   -- Set_Transient_For --
   -----------------------

   procedure Set_Transient_For
      (Self   : not null access Gtk_Window_Record;
       Parent : access Gtk_Window_Record'Class)
   is
      procedure Internal (Self : System.Address; Parent : System.Address);
      pragma Import (C, Internal, "gtk_window_set_transient_for");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)));
   end Set_Transient_For;

   ------------------
   -- Unfullscreen --
   ------------------

   procedure Unfullscreen (Self : not null access Gtk_Window_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_window_unfullscreen");
   begin
      Internal (Get_Object (Self));
   end Unfullscreen;

   ----------------
   -- Unmaximize --
   ----------------

   procedure Unmaximize (Self : not null access Gtk_Window_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_window_unmaximize");
   begin
      Internal (Get_Object (Self));
   end Unmaximize;

   ----------------
   -- Unminimize --
   ----------------

   procedure Unminimize (Self : not null access Gtk_Window_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_window_unminimize");
   begin
      Internal (Get_Object (Self));
   end Unminimize;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Window_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority)
   is
      procedure Internal
         (Self     : System.Address;
          Message  : Gtkada.Types.Chars_Ptr;
          Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);
      pragma Import (C, Internal, "gtk_accessible_announce");
      Tmp_Message : Gtkada.Types.Chars_Ptr := New_String (Message);
   begin
      Internal (Get_Object (Self), Tmp_Message, Priority);
      Free (Tmp_Message);
   end Announce;

   -----------------------
   -- Get_Accessible_Id --
   -----------------------

   function Get_Accessible_Id
      (Self : not null access Gtk_Window_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_id");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Accessible_Id;

   ---------------------------
   -- Get_Accessible_Parent --
   ---------------------------

   function Get_Accessible_Parent
      (Self : not null access Gtk_Window_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_parent");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Parent;

   -------------------------
   -- Get_Accessible_Role --
   -------------------------

   function Get_Accessible_Role
      (Self : not null access Gtk_Window_Record)
       return Gtk.Accessible.Gtk_Accessible_Role
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible_Role;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_role");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Role;

   --------------------
   -- Get_At_Context --
   --------------------

   function Get_At_Context
      (Self : not null access Gtk_Window_Record)
       return Gtk.Atcontext.Gtk_Atcontext
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_accessible_get_at_context");
      Stub_Gtk_Atcontext : Gtk.Atcontext.Gtk_Atcontext_Record;
   begin
      return Gtk.Atcontext.Gtk_Atcontext (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Atcontext));
   end Get_At_Context;

   ----------------
   -- Get_Bounds --
   ----------------

   function Get_Bounds
      (Self   : not null access Gtk_Window_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Acc_X      : access Glib.Gint;
          Acc_Y      : access Glib.Gint;
          Acc_Width  : access Glib.Gint;
          Acc_Height : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accessible_get_bounds");
      Acc_X      : aliased Glib.Gint;
      Acc_Y      : aliased Glib.Gint;
      Acc_Width  : aliased Glib.Gint;
      Acc_Height : aliased Glib.Gint;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_X'Access, Acc_Y'Access, Acc_Width'Access, Acc_Height'Access);
      X.all := Acc_X;
      Y.all := Acc_Y;
      Width.all := Acc_Width;
      Height.all := Acc_Height;
      return Tmp_Return /= 0;
   end Get_Bounds;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gtk_Window_Record) return Gdk.Gdk_Display
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_root_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Gdk_Display (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Display;

   --------------------------------
   -- Get_First_Accessible_Child --
   --------------------------------

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Window_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_first_accessible_child");
   begin
      return Internal (Get_Object (Self));
   end Get_First_Accessible_Child;

   ---------------
   -- Get_Focus --
   ---------------

   function Get_Focus
      (Self : not null access Gtk_Window_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_root_get_focus");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Focus;

   ---------------------------------
   -- Get_Next_Accessible_Sibling --
   ---------------------------------

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Window_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_next_accessible_sibling");
   begin
      return Internal (Get_Object (Self));
   end Get_Next_Accessible_Sibling;

   ------------------------
   -- Get_Platform_State --
   ------------------------

   function Get_Platform_State
      (Self  : not null access Gtk_Window_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean
   is
      function Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accessible_get_platform_state");
   begin
      return Internal (Get_Object (Self), State) /= 0;
   end Get_Platform_State;

   -----------------
   -- Get_Surface --
   -----------------

   function Get_Surface
      (Self : not null access Gtk_Window_Record) return Gdk.Gdk_Surface
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_native_get_surface");
      Stub_Gdk_Surface : Gdk.Surface.Gdk_Surface_Record;
   begin
      return Gdk.Gdk_Surface (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Surface));
   end Get_Surface;

   ---------------------------
   -- Get_Surface_Transform --
   ---------------------------

   procedure Get_Surface_Transform
      (Self : not null access Gtk_Window_Record;
       X    : out Gdouble;
       Y    : out Gdouble)
   is
      procedure Internal
         (Self : System.Address;
          X    : out Gdouble;
          Y    : out Gdouble);
      pragma Import (C, Internal, "gtk_native_get_surface_transform");
   begin
      Internal (Get_Object (Self), X, Y);
   end Get_Surface_Transform;

   -------------
   -- Realize --
   -------------

   procedure Realize (Self : not null access Gtk_Window_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_native_realize");
   begin
      Internal (Get_Object (Self));
   end Realize;

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self     : not null access Gtk_Window_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property)
   is
      procedure Internal
         (Self     : System.Address;
          Property : Gtk.Accessible.Gtk_Accessible_Property);
      pragma Import (C, Internal, "gtk_accessible_reset_property");
   begin
      Internal (Get_Object (Self), Property);
   end Reset_Property;

   --------------------
   -- Reset_Relation --
   --------------------

   procedure Reset_Relation
      (Self     : not null access Gtk_Window_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation)
   is
      procedure Internal
         (Self     : System.Address;
          Relation : Gtk.Accessible.Gtk_Accessible_Relation);
      pragma Import (C, Internal, "gtk_accessible_reset_relation");
   begin
      Internal (Get_Object (Self), Relation);
   end Reset_Relation;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State
      (Self  : not null access Gtk_Window_Record;
       State : Gtk.Accessible.Gtk_Accessible_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_State);
      pragma Import (C, Internal, "gtk_accessible_reset_state");
   begin
      Internal (Get_Object (Self), State);
   end Reset_State;

   ---------------------------
   -- Set_Accessible_Parent --
   ---------------------------

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Window_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible)
   is
      procedure Internal
         (Self         : System.Address;
          Parent       : Gtk.Accessible.Gtk_Accessible;
          Next_Sibling : Gtk.Accessible.Gtk_Accessible);
      pragma Import (C, Internal, "gtk_accessible_set_accessible_parent");
   begin
      Internal (Get_Object (Self), Parent, Next_Sibling);
   end Set_Accessible_Parent;

   ---------------
   -- Set_Focus --
   ---------------

   procedure Set_Focus
      (Self  : not null access Gtk_Window_Record;
       Focus : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Focus : System.Address);
      pragma Import (C, Internal, "gtk_root_set_focus");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Focus)));
   end Set_Focus;

   ---------------
   -- Unrealize --
   ---------------

   procedure Unrealize (Self : not null access Gtk_Window_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_native_unrealize");
   begin
      Internal (Get_Object (Self));
   end Unrealize;

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Window_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible)
   is
      procedure Internal
         (Self        : System.Address;
          New_Sibling : Gtk.Accessible.Gtk_Accessible);
      pragma Import (C, Internal, "gtk_accessible_update_next_accessible_sibling");
   begin
      Internal (Get_Object (Self), New_Sibling);
   end Update_Next_Accessible_Sibling;

   ---------------------------
   -- Update_Platform_State --
   ---------------------------

   procedure Update_Platform_State
      (Self  : not null access Gtk_Window_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

   ---------------------------
   -- Get_Default_Icon_Name --
   ---------------------------

   function Get_Default_Icon_Name return UTF8_String is
      function Internal return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_window_get_default_icon_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal);
   end Get_Default_Icon_Name;

   -------------------
   -- Get_Toplevels --
   -------------------

   function Get_Toplevels return Glib.List_Model.Glist_Model is
      function Internal return Glib.List_Model.Glist_Model;
      pragma Import (C, Internal, "gtk_window_get_toplevels");
   begin
      return Internal;
   end Get_Toplevels;

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

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Window_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Window_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Window_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Window_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Window_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Window_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean_Boolean);

   procedure Connect
      (Object  : access Gtk_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Window_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Window_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Window_Boolean_Boolean;
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
       Handler : Cb_GObject_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean);

   procedure Marsh_GObject_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean_Boolean);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Window_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Window_Boolean);

   procedure Marsh_Gtk_Window_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Window_Boolean_Boolean);

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
       Handler : Cb_Gtk_Window_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Window_Boolean'Access,
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
       Handler : Cb_GObject_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean'Access,
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

   ---------------------------
   -- Marsh_GObject_Boolean --
   ---------------------------

   procedure Marsh_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean;

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

   ------------------------------
   -- Marsh_Gtk_Window_Boolean --
   ------------------------------

   procedure Marsh_Gtk_Window_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Window_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Window := Gtk_Window (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Window_Boolean;

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

   ----------------------
   -- On_Close_Request --
   ----------------------

   procedure On_Close_Request
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_Gtk_Window_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "close-request" & ASCII.NUL, Call, After);
   end On_Close_Request;

   ----------------------
   -- On_Close_Request --
   ----------------------

   procedure On_Close_Request
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "close-request" & ASCII.NUL, Call, After, Slot);
   end On_Close_Request;

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

end Gtk.Window;
