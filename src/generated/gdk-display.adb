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

package body Gdk.Display is

   procedure Store_Clipboard
     (Display          : not null access Gdk_Display_Record;
      Clipboard_Window : Gdk_Window;
      Time             : Guint32;
      Targets          : Gdk.Types.Gdk_Atom_Array)
   is
      procedure Internal
        (Display          : System.Address;
         Clipboard_Window : Gdk_Window;
         Time             : Guint32;
         Targets          : System.Address;
         N_Targets        : Gint);
      pragma Import (C, Internal, "gdk_display_store_clipboard");
   begin
      Internal (Get_Object (Display), Clipboard_Window, Time,
         Targets (Targets'First)'Address, Targets'Length);
   end Store_Clipboard;

   procedure Get_Window_At_Pointer
     (Display : access Gdk_Display_Record;
      Win_X   : out Gint;
      Win_Y   : out Gint;
      Win     : out Gdk_Window)
   is
      function Internal
        (Display : System.Address;
         Win_X   : access Gint;
         Win_Y   : access Gint) return Gdk_Window;
      pragma Import (C, Internal, "gdk_display_get_window_at_pointer");
      X, Y : aliased Gint;
   begin
      Win   := Internal (Get_Object (Display), X'Access, Y'Access);
      Win_X := X;
      Win_Y := Y;
   end Get_Window_At_Pointer;

   package Type_Conversion_Gdk_Display is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Display_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Display);

   ----------
   -- Beep --
   ----------

   procedure Beep (Self : not null access Gdk_Display_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_display_beep");
   begin
      Internal (Get_Object (Self));
   end Beep;

   -----------
   -- Close --
   -----------

   procedure Close (Self : not null access Gdk_Display_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_display_close");
   begin
      Internal (Get_Object (Self));
   end Close;

   -----------
   -- Flush --
   -----------

   procedure Flush (Self : not null access Gdk_Display_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_display_flush");
   begin
      Internal (Get_Object (Self));
   end Flush;

   -----------------------------
   -- Get_Default_Cursor_Size --
   -----------------------------

   function Get_Default_Cursor_Size
      (Self : not null access Gdk_Display_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gdk_display_get_default_cursor_size");
   begin
      return Internal (Get_Object (Self));
   end Get_Default_Cursor_Size;

   -----------------------
   -- Get_Default_Group --
   -----------------------

   function Get_Default_Group
      (Self : not null access Gdk_Display_Record) return Gdk.Gdk_Window
   is
      function Internal (Self : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gdk_display_get_default_group");
   begin
      return Internal (Get_Object (Self));
   end Get_Default_Group;

   ----------------------
   -- Get_Default_Seat --
   ----------------------

   function Get_Default_Seat
      (Self : not null access Gdk_Display_Record) return Glib.Object.GObject
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_display_get_default_seat");
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      return Get_User_Data (Internal (Get_Object (Self)), Stub_GObject);
   end Get_Default_Seat;

   ---------------
   -- Get_Event --
   ---------------

   function Get_Event
      (Self : not null access Gdk_Display_Record) return Gdk.Event.Gdk_Event
   is
      function Internal (Self : System.Address) return Gdk.Event.Gdk_Event;
      pragma Import (C, Internal, "gdk_display_get_event");
   begin
      return Internal (Get_Object (Self));
   end Get_Event;

   -----------------------------
   -- Get_Maximal_Cursor_Size --
   -----------------------------

   procedure Get_Maximal_Cursor_Size
      (Self   : not null access Gdk_Display_Record;
       Width  : out Guint;
       Height : out Guint)
   is
      procedure Internal
         (Self   : System.Address;
          Width  : out Guint;
          Height : out Guint);
      pragma Import (C, Internal, "gdk_display_get_maximal_cursor_size");
   begin
      Internal (Get_Object (Self), Width, Height);
   end Get_Maximal_Cursor_Size;

   -----------------
   -- Get_Monitor --
   -----------------

   function Get_Monitor
      (Self        : not null access Gdk_Display_Record;
       Monitor_Num : Glib.Gint) return Gdk.Monitor.Gdk_Monitor
   is
      function Internal
         (Self        : System.Address;
          Monitor_Num : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gdk_display_get_monitor");
      Stub_Gdk_Monitor : Gdk.Monitor.Gdk_Monitor_Record;
   begin
      return Gdk.Monitor.Gdk_Monitor (Get_User_Data (Internal (Get_Object (Self), Monitor_Num), Stub_Gdk_Monitor));
   end Get_Monitor;

   --------------------------
   -- Get_Monitor_At_Point --
   --------------------------

   function Get_Monitor_At_Point
      (Self : not null access Gdk_Display_Record;
       X    : Glib.Gint;
       Y    : Glib.Gint) return Gdk.Monitor.Gdk_Monitor
   is
      function Internal
         (Self : System.Address;
          X    : Glib.Gint;
          Y    : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gdk_display_get_monitor_at_point");
      Stub_Gdk_Monitor : Gdk.Monitor.Gdk_Monitor_Record;
   begin
      return Gdk.Monitor.Gdk_Monitor (Get_User_Data (Internal (Get_Object (Self), X, Y), Stub_Gdk_Monitor));
   end Get_Monitor_At_Point;

   ---------------------------
   -- Get_Monitor_At_Window --
   ---------------------------

   function Get_Monitor_At_Window
      (Self   : not null access Gdk_Display_Record;
       Window : Gdk.Gdk_Window) return Gdk.Monitor.Gdk_Monitor
   is
      function Internal
         (Self   : System.Address;
          Window : Gdk.Gdk_Window) return System.Address;
      pragma Import (C, Internal, "gdk_display_get_monitor_at_window");
      Stub_Gdk_Monitor : Gdk.Monitor.Gdk_Monitor_Record;
   begin
      return Gdk.Monitor.Gdk_Monitor (Get_User_Data (Internal (Get_Object (Self), Window), Stub_Gdk_Monitor));
   end Get_Monitor_At_Window;

   --------------------
   -- Get_N_Monitors --
   --------------------

   function Get_N_Monitors
      (Self : not null access Gdk_Display_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_display_get_n_monitors");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Monitors;

   -------------------
   -- Get_N_Screens --
   -------------------

   function Get_N_Screens
      (Self : not null access Gdk_Display_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_display_get_n_screens");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Screens;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Self : not null access Gdk_Display_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gdk_display_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Name;

   -------------------------
   -- Get_Primary_Monitor --
   -------------------------

   function Get_Primary_Monitor
      (Self : not null access Gdk_Display_Record)
       return Gdk.Monitor.Gdk_Monitor
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_display_get_primary_monitor");
      Stub_Gdk_Monitor : Gdk.Monitor.Gdk_Monitor_Record;
   begin
      return Gdk.Monitor.Gdk_Monitor (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Monitor));
   end Get_Primary_Monitor;

   -----------------
   -- Has_Pending --
   -----------------

   function Has_Pending
      (Self : not null access Gdk_Display_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_display_has_pending");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Has_Pending;

   ---------------
   -- Is_Closed --
   ---------------

   function Is_Closed
      (Self : not null access Gdk_Display_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_display_is_closed");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Closed;

   ---------------------
   -- Keyboard_Ungrab --
   ---------------------

   procedure Keyboard_Ungrab
      (Self : not null access Gdk_Display_Record;
       Time : Guint32)
   is
      procedure Internal (Self : System.Address; Time : Guint32);
      pragma Import (C, Internal, "gdk_display_keyboard_ungrab");
   begin
      Internal (Get_Object (Self), Time);
   end Keyboard_Ungrab;

   ----------------
   -- List_Seats --
   ----------------

   function List_Seats
      (Self : not null access Gdk_Display_Record)
       return Glib.Object.Object_Simple_List.Glist
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_display_list_seats");
      Tmp_Return : Glib.Object.Object_Simple_List.Glist;
   begin
      Glib.Object.Object_Simple_List.Set_Object (Tmp_Return, Internal (Get_Object (Self)));
      return Tmp_Return;
   end List_Seats;

   -----------------------------
   -- Notify_Startup_Complete --
   -----------------------------

   procedure Notify_Startup_Complete
      (Self       : not null access Gdk_Display_Record;
       Startup_Id : UTF8_String)
   is
      procedure Internal
         (Self       : System.Address;
          Startup_Id : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gdk_display_notify_startup_complete");
      Tmp_Startup_Id : Gtkada.Types.Chars_Ptr := New_String (Startup_Id);
   begin
      Internal (Get_Object (Self), Tmp_Startup_Id);
      Free (Tmp_Startup_Id);
   end Notify_Startup_Complete;

   ----------------
   -- Peek_Event --
   ----------------

   function Peek_Event
      (Self : not null access Gdk_Display_Record) return Gdk.Event.Gdk_Event
   is
      function Internal (Self : System.Address) return Gdk.Event.Gdk_Event;
      pragma Import (C, Internal, "gdk_display_peek_event");
   begin
      return Internal (Get_Object (Self));
   end Peek_Event;

   ------------------------
   -- Pointer_Is_Grabbed --
   ------------------------

   function Pointer_Is_Grabbed
      (Self : not null access Gdk_Display_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_display_pointer_is_grabbed");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Pointer_Is_Grabbed;

   --------------------
   -- Pointer_Ungrab --
   --------------------

   procedure Pointer_Ungrab
      (Self : not null access Gdk_Display_Record;
       Time : Guint32)
   is
      procedure Internal (Self : System.Address; Time : Guint32);
      pragma Import (C, Internal, "gdk_display_pointer_ungrab");
   begin
      Internal (Get_Object (Self), Time);
   end Pointer_Ungrab;

   ---------------
   -- Put_Event --
   ---------------

   procedure Put_Event
      (Self  : not null access Gdk_Display_Record;
       Event : Gdk.Event.Gdk_Event)
   is
      procedure Internal
         (Self  : System.Address;
          Event : Gdk.Event.Gdk_Event);
      pragma Import (C, Internal, "gdk_display_put_event");
   begin
      Internal (Get_Object (Self), Event);
   end Put_Event;

   ------------------------------------
   -- Request_Selection_Notification --
   ------------------------------------

   function Request_Selection_Notification
      (Self      : not null access Gdk_Display_Record;
       Selection : Gdk.Types.Gdk_Atom) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Selection : Gdk.Types.Gdk_Atom) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_display_request_selection_notification");
   begin
      return Internal (Get_Object (Self), Selection) /= 0;
   end Request_Selection_Notification;

   -------------------------------
   -- Set_Double_Click_Distance --
   -------------------------------

   procedure Set_Double_Click_Distance
      (Self     : not null access Gdk_Display_Record;
       Distance : Guint)
   is
      procedure Internal (Self : System.Address; Distance : Guint);
      pragma Import (C, Internal, "gdk_display_set_double_click_distance");
   begin
      Internal (Get_Object (Self), Distance);
   end Set_Double_Click_Distance;

   ---------------------------
   -- Set_Double_Click_Time --
   ---------------------------

   procedure Set_Double_Click_Time
      (Self : not null access Gdk_Display_Record;
       Msec : Guint)
   is
      procedure Internal (Self : System.Address; Msec : Guint);
      pragma Import (C, Internal, "gdk_display_set_double_click_time");
   begin
      Internal (Get_Object (Self), Msec);
   end Set_Double_Click_Time;

   ------------------------------------
   -- Supports_Clipboard_Persistence --
   ------------------------------------

   function Supports_Clipboard_Persistence
      (Self : not null access Gdk_Display_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_display_supports_clipboard_persistence");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Supports_Clipboard_Persistence;

   ------------------------
   -- Supports_Composite --
   ------------------------

   function Supports_Composite
      (Self : not null access Gdk_Display_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_display_supports_composite");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Supports_Composite;

   ---------------------------
   -- Supports_Cursor_Alpha --
   ---------------------------

   function Supports_Cursor_Alpha
      (Self : not null access Gdk_Display_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_display_supports_cursor_alpha");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Supports_Cursor_Alpha;

   ---------------------------
   -- Supports_Cursor_Color --
   ---------------------------

   function Supports_Cursor_Color
      (Self : not null access Gdk_Display_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_display_supports_cursor_color");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Supports_Cursor_Color;

   ---------------------------
   -- Supports_Input_Shapes --
   ---------------------------

   function Supports_Input_Shapes
      (Self : not null access Gdk_Display_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_display_supports_input_shapes");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Supports_Input_Shapes;

   -------------------------------------
   -- Supports_Selection_Notification --
   -------------------------------------

   function Supports_Selection_Notification
      (Self : not null access Gdk_Display_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_display_supports_selection_notification");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Supports_Selection_Notification;

   ---------------------
   -- Supports_Shapes --
   ---------------------

   function Supports_Shapes
      (Self : not null access Gdk_Display_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_display_supports_shapes");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Supports_Shapes;

   ----------
   -- Sync --
   ----------

   procedure Sync (Self : not null access Gdk_Display_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_display_sync");
   begin
      Internal (Get_Object (Self));
   end Sync;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default return Gdk_Display is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_display_get_default");
      Stub_Gdk_Display : Gdk_Display_Record;
   begin
      return Gdk.Display.Gdk_Display (Get_User_Data (Internal, Stub_Gdk_Display));
   end Get_Default;

   ----------
   -- Open --
   ----------

   function Open (Display_Name : UTF8_String) return Gdk_Display is
      function Internal
         (Display_Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gdk_display_open");
      Tmp_Display_Name : Gtkada.Types.Chars_Ptr := New_String (Display_Name);
      Stub_Gdk_Display : Gdk_Display_Record;
      Tmp_Return       : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Display_Name);
      Free (Tmp_Display_Name);
      return Gdk.Display.Gdk_Display (Get_User_Data (Tmp_Return, Stub_Gdk_Display));
   end Open;

   ------------------------------
   -- Open_Default_Libgtk_Only --
   ------------------------------

   function Open_Default_Libgtk_Only return Gdk_Display is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_display_open_default_libgtk_only");
      Stub_Gdk_Display : Gdk_Display_Record;
   begin
      return Gdk.Display.Gdk_Display (Get_User_Data (Internal, Stub_Gdk_Display));
   end Open_Default_Libgtk_Only;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Display_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Display_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Display_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Display_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Display_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Display_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Display_Boolean_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Display_GObject_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Display_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean_Void);

   procedure Marsh_GObject_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_GObject_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gdk_Display_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Display_Boolean_Void);

   procedure Marsh_Gdk_Display_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Display_GObject_Void);

   procedure Marsh_Gdk_Display_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Display_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Display_Boolean_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Display_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Display_GObject_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Display_GObject_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Display_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Display_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_GObject_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gdk_Display_Record'Class;
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

   --------------------------------
   -- Marsh_GObject_Boolean_Void --
   --------------------------------

   procedure Marsh_GObject_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Boolean (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean_Void;

   --------------------------------
   -- Marsh_GObject_GObject_Void --
   --------------------------------

   procedure Marsh_GObject_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_GObject_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Object (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_GObject_Void;

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

   ------------------------------------
   -- Marsh_Gdk_Display_Boolean_Void --
   ------------------------------------

   procedure Marsh_Gdk_Display_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Display_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Display := Gdk_Display (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Boolean (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gdk_Display_Boolean_Void;

   ------------------------------------
   -- Marsh_Gdk_Display_GObject_Void --
   ------------------------------------

   procedure Marsh_Gdk_Display_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Display_GObject_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Display := Gdk_Display (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Object (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gdk_Display_GObject_Void;

   ----------------------------
   -- Marsh_Gdk_Display_Void --
   ----------------------------

   procedure Marsh_Gdk_Display_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Display_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Display := Gdk_Display (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gdk_Display_Void;

   ---------------
   -- On_Closed --
   ---------------

   procedure On_Closed
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_Gdk_Display_Boolean_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "closed" & ASCII.NUL, Call, After);
   end On_Closed;

   ---------------
   -- On_Closed --
   ---------------

   procedure On_Closed
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_GObject_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "closed" & ASCII.NUL, Call, After, Slot);
   end On_Closed;

   ----------------------
   -- On_Monitor_Added --
   ----------------------

   procedure On_Monitor_Added
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_Gdk_Display_GObject_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "monitor-added" & ASCII.NUL, Call, After);
   end On_Monitor_Added;

   ----------------------
   -- On_Monitor_Added --
   ----------------------

   procedure On_Monitor_Added
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_GObject_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "monitor-added" & ASCII.NUL, Call, After, Slot);
   end On_Monitor_Added;

   ------------------------
   -- On_Monitor_Removed --
   ------------------------

   procedure On_Monitor_Removed
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_Gdk_Display_GObject_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "monitor-removed" & ASCII.NUL, Call, After);
   end On_Monitor_Removed;

   ------------------------
   -- On_Monitor_Removed --
   ------------------------

   procedure On_Monitor_Removed
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_GObject_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "monitor-removed" & ASCII.NUL, Call, After, Slot);
   end On_Monitor_Removed;

   ---------------
   -- On_Opened --
   ---------------

   procedure On_Opened
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_Gdk_Display_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "opened" & ASCII.NUL, Call, After);
   end On_Opened;

   ---------------
   -- On_Opened --
   ---------------

   procedure On_Opened
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "opened" & ASCII.NUL, Call, After, Slot);
   end On_Opened;

   -------------------
   -- On_Seat_Added --
   -------------------

   procedure On_Seat_Added
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_Gdk_Display_GObject_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "seat-added" & ASCII.NUL, Call, After);
   end On_Seat_Added;

   -------------------
   -- On_Seat_Added --
   -------------------

   procedure On_Seat_Added
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_GObject_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "seat-added" & ASCII.NUL, Call, After, Slot);
   end On_Seat_Added;

   ---------------------
   -- On_Seat_Removed --
   ---------------------

   procedure On_Seat_Removed
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_Gdk_Display_GObject_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "seat-removed" & ASCII.NUL, Call, After);
   end On_Seat_Removed;

   ---------------------
   -- On_Seat_Removed --
   ---------------------

   procedure On_Seat_Removed
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_GObject_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "seat-removed" & ASCII.NUL, Call, After, Slot);
   end On_Seat_Removed;

end Gdk.Display;
