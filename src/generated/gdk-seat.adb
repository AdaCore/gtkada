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

package body Gdk.Seat is

   function Convert (R : Gdk.Seat.Gdk_Seat) return System.Address is
   begin
      return Get_Object (R);
   end Convert;

   function Convert (R : System.Address) return Gdk.Seat.Gdk_Seat is
      Stub : Gdk.Seat.Gdk_Seat_Record;begin
         return Gdk.Seat.Gdk_Seat (Glib.Object.Get_User_Data (R, Stub));
      end Convert;

   function C_Gdk_Seat_Grab
      (Self              : System.Address;
       Window            : Gdk.Gdk_Window;
       Capabilities      : Gdk_Seat_Capabilities;
       Owner_Events      : Glib.Gboolean;
       Cursor            : Gdk.Gdk_Cursor;
       Event             : Gdk.Event.Gdk_Event;
       Prepare_Func      : System.Address;
       Prepare_Func_Data : System.Address) return Gdk_Grab_Status;
   pragma Import (C, C_Gdk_Seat_Grab, "gdk_seat_grab");
   --  Grabs the seat so that all events corresponding to the given
   --  Capabilities are passed to this application until the seat is ungrabbed
   --  with Gdk.Seat.Ungrab, or the window becomes hidden. This overrides any
   --  previous grab on the seat by this client.
   --  As a rule of thumb, if a grab is desired over
   --  Gdk.Seat.Gdk_Seat_Capability_Pointer, all other "pointing" capabilities
   --  (eg. Gdk.Seat.Gdk_Seat_Capability_Touch) should be grabbed too, so the
   --  user is able to interact with all of those while the grab holds, you
   --  should thus use Gdk.Seat.Gdk_Seat_Capability_All_Pointing most commonly.
   --  Grabs are used for operations which need complete control over the
   --  events corresponding to the given capabilities. For example in GTK+ this
   --  is used for Drag and Drop operations, popup menus and such.
   --  Note that if the event mask of a Gdk.Gdk_Window has selected both
   --  button press and button release events, or touch begin and touch end,
   --  then a press event will cause an automatic grab until the button is
   --  released, equivalent to a grab on the window with Owner_Events set to
   --  True. This is done because most applications expect to receive paired
   --  press and release events.
   --  If you set up anything at the time you take the grab that needs to be
   --  cleaned up when the grab ends, you should handle the
   --  Gdk.Event.Gdk_Event_Grab_Broken events that are emitted when the grab
   --  ends unvoluntarily.
   --  Since: gtk+ 3.20
   --  "window": the Gdk.Gdk_Window which will own the grab
   --  "capabilities": capabilities that will be grabbed
   --  "owner_events": if False then all device events are reported with
   --  respect to Window and are only reported if selected by Event_Mask. If
   --  True then pointer events for this application are reported as normal,
   --  but pointer events outside this application are reported with respect to
   --  Window and only if selected by Event_Mask. In either mode, unreported
   --  events are discarded.
   --  "cursor": the cursor to display while the grab is active. If this is
   --  null then the normal cursors are used for Window and its descendants,
   --  and the cursor for Window is used elsewhere.
   --  "event": the event that is triggering the grab, or null if none is
   --  available.
   --  "prepare_func": function to prepare the window to be grabbed, it can be
   --  null if Window is visible before this call.
   --  "prepare_func_data": user data to pass to Prepare_Func

   function To_Gdk_Seat_Grab_Prepare_Func is new Ada.Unchecked_Conversion
     (System.Address, Gdk_Seat_Grab_Prepare_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gdk_Seat_Grab_Prepare_Func, System.Address);

   procedure Internal_Gdk_Seat_Grab_Prepare_Func
      (Seat      : System.Address;
       Window    : Gdk.Gdk_Window;
       User_Data : System.Address);
   pragma Convention (C, Internal_Gdk_Seat_Grab_Prepare_Func);
   --  "seat": the Gdk.Seat.Gdk_Seat being grabbed
   --  "window": the Gdk.Gdk_Window being grabbed
   --  "user_data": user data passed in Gdk.Seat.Grab

   -----------------------------------------
   -- Internal_Gdk_Seat_Grab_Prepare_Func --
   -----------------------------------------

   procedure Internal_Gdk_Seat_Grab_Prepare_Func
      (Seat      : System.Address;
       Window    : Gdk.Gdk_Window;
       User_Data : System.Address)
   is
      Func          : constant Gdk_Seat_Grab_Prepare_Func := To_Gdk_Seat_Grab_Prepare_Func (User_Data);
      Stub_Gdk_Seat : Gdk_Seat_Record;
   begin
      Func (Gdk.Seat.Gdk_Seat (Get_User_Data (Seat, Stub_Gdk_Seat)), Window);
   end Internal_Gdk_Seat_Grab_Prepare_Func;

   package Type_Conversion_Gdk_Seat is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Seat_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Seat);

   ----------------------
   -- Get_Capabilities --
   ----------------------

   function Get_Capabilities
      (Self : not null access Gdk_Seat_Record) return Gdk_Seat_Capabilities
   is
      function Internal (Self : System.Address) return Gdk_Seat_Capabilities;
      pragma Import (C, Internal, "gdk_seat_get_capabilities");
   begin
      return Internal (Get_Object (Self));
   end Get_Capabilities;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gdk_Seat_Record) return Glib.Object.GObject
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_seat_get_display");
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      return Get_User_Data (Internal (Get_Object (Self)), Stub_GObject);
   end Get_Display;

   ------------------
   -- Get_Keyboard --
   ------------------

   function Get_Keyboard
      (Self : not null access Gdk_Seat_Record) return Gdk.Device.Gdk_Device
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_seat_get_keyboard");
      Stub_Gdk_Device : Gdk.Device.Gdk_Device_Record;
   begin
      return Gdk.Device.Gdk_Device (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Device));
   end Get_Keyboard;

   -----------------
   -- Get_Pointer --
   -----------------

   function Get_Pointer
      (Self : not null access Gdk_Seat_Record) return Gdk.Device.Gdk_Device
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_seat_get_pointer");
      Stub_Gdk_Device : Gdk.Device.Gdk_Device_Record;
   begin
      return Gdk.Device.Gdk_Device (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Device));
   end Get_Pointer;

   ----------------
   -- Get_Slaves --
   ----------------

   function Get_Slaves
      (Self         : not null access Gdk_Seat_Record;
       Capabilities : Gdk_Seat_Capabilities)
       return Gdk.Device.Device_List.Glist
   is
      function Internal
         (Self         : System.Address;
          Capabilities : Gdk_Seat_Capabilities) return System.Address;
      pragma Import (C, Internal, "gdk_seat_get_slaves");
      Tmp_Return : Gdk.Device.Device_List.Glist;
   begin
      Gdk.Device.Device_List.Set_Object (Tmp_Return, Internal (Get_Object (Self), Capabilities));
      return Tmp_Return;
   end Get_Slaves;

   ----------
   -- Grab --
   ----------

   function Grab
      (Self         : not null access Gdk_Seat_Record;
       Window       : Gdk.Gdk_Window;
       Capabilities : Gdk_Seat_Capabilities;
       Owner_Events : Boolean;
       Cursor       : Gdk.Gdk_Cursor;
       Event        : Gdk.Event.Gdk_Event;
       Prepare_Func : Gdk_Seat_Grab_Prepare_Func) return Gdk_Grab_Status
   is
   begin
      if Prepare_Func = null then
         return C_Gdk_Seat_Grab (Get_Object (Self), Window, Capabilities, Boolean'Pos (Owner_Events), Cursor, Event, System.Null_Address, System.Null_Address);
      else
         return C_Gdk_Seat_Grab (Get_Object (Self), Window, Capabilities, Boolean'Pos (Owner_Events), Cursor, Event, Internal_Gdk_Seat_Grab_Prepare_Func'Address, To_Address (Prepare_Func));
      end if;
   end Grab;

   ------------
   -- Ungrab --
   ------------

   procedure Ungrab (Self : not null access Gdk_Seat_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_seat_ungrab");
   begin
      Internal (Get_Object (Self));
   end Ungrab;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Seat_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Seat_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_GObject_Void);

   procedure Connect
      (Object  : access Gdk_Seat_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Seat_GObject_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gdk_Seat_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_GObject_Void);

   procedure Marsh_Gdk_Seat_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Seat_GObject_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Seat_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Seat_GObject_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Seat_GObject_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gdk_Seat_Record'Class;
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

   ---------------------------------
   -- Marsh_Gdk_Seat_GObject_Void --
   ---------------------------------

   procedure Marsh_Gdk_Seat_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Seat_GObject_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Seat := Gdk_Seat (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Object (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gdk_Seat_GObject_Void;

   ---------------------
   -- On_Device_Added --
   ---------------------

   procedure On_Device_Added
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_Gdk_Seat_GObject_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "device-added" & ASCII.NUL, Call, After);
   end On_Device_Added;

   ---------------------
   -- On_Device_Added --
   ---------------------

   procedure On_Device_Added
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_GObject_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "device-added" & ASCII.NUL, Call, After, Slot);
   end On_Device_Added;

   -----------------------
   -- On_Device_Removed --
   -----------------------

   procedure On_Device_Removed
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_Gdk_Seat_GObject_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "device-removed" & ASCII.NUL, Call, After);
   end On_Device_Removed;

   -----------------------
   -- On_Device_Removed --
   -----------------------

   procedure On_Device_Removed
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_GObject_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "device-removed" & ASCII.NUL, Call, After, Slot);
   end On_Device_Removed;

   -------------------
   -- On_Tool_Added --
   -------------------

   procedure On_Tool_Added
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_Gdk_Seat_GObject_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "tool-added" & ASCII.NUL, Call, After);
   end On_Tool_Added;

   -------------------
   -- On_Tool_Added --
   -------------------

   procedure On_Tool_Added
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_GObject_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "tool-added" & ASCII.NUL, Call, After, Slot);
   end On_Tool_Added;

   ---------------------
   -- On_Tool_Removed --
   ---------------------

   procedure On_Tool_Removed
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_Gdk_Seat_GObject_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "tool-removed" & ASCII.NUL, Call, After);
   end On_Tool_Removed;

   ---------------------
   -- On_Tool_Removed --
   ---------------------

   procedure On_Tool_Removed
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_GObject_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "tool-removed" & ASCII.NUL, Call, After, Slot);
   end On_Tool_Removed;

end Gdk.Seat;
