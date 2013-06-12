------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2013, AdaCore                     --
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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Interfaces.C.Strings;       use Interfaces.C.Strings;
pragma Warnings(On);

package body Gdk.Device is

   function Convert (R : Gdk.Device.Gdk_Device) return System.Address is
   begin
      return Get_Object (R);
   end Convert;

   function Convert (R : System.Address) return Gdk.Device.Gdk_Device is
      Stub : Gdk.Device.Gdk_Device_Record;begin
         return Gdk.Device.Gdk_Device (Glib.Object.Get_User_Data (R, Stub));
      end Convert;

   procedure Set_Source_Device
     (Event  : Gdk.Event.Gdk_Event;
      Device : not null access Gdk_Device_Record)
   is
      procedure Internal (Event : Gdk.Event.Gdk_Event; D : System.Address);
      pragma Import (C, Internal, "gdk_event_set_source_device");
   begin
      Internal (Event, Get_Object (Device));
   end Set_Source_Device;

   procedure Set_Device
     (Event  : Gdk.Event.Gdk_Event;
      Device : not null access Gdk_Device_Record)
   is
      procedure Internal (Event : Gdk.Event.Gdk_Event; D : System.Address);
      pragma Import (C, Internal, "gdk_event_set_device");
   begin
      Internal (Event, Get_Object (Device));
   end Set_Device;

   procedure Get_Window_At_Position
     (Self   : not null access Gdk_Device_Record;
      Win_X  : out Gint;
      Win_Y  : out Gint;
      Window : out Gdk.Gdk_Window)
   is
      function Internal
        (Self      : System.Address;
         Acc_Win_X : access Gint;
         Acc_Win_Y : access Gint) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gdk_device_get_window_at_position");
      Acc_Win_X  : aliased Gint;
      Acc_Win_Y  : aliased Gint;
   begin
      Window := Internal (Get_Object (Self), Acc_Win_X'Access, Acc_Win_Y'Access);
      Win_X := Acc_Win_X;
      Win_Y := Acc_Win_Y;
   end Get_Window_At_Position;

   package Type_Conversion_Gdk_Device is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Device_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Device);

   ---------------------------
   -- Get_Associated_Device --
   ---------------------------

   function Get_Associated_Device
      (Self : not null access Gdk_Device_Record) return Gdk_Device
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_device_get_associated_device");
      Stub_Gdk_Device : Gdk_Device_Record;
   begin
      return Gdk.Device.Gdk_Device (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Device));
   end Get_Associated_Device;

   ------------------
   -- Get_Axis_Use --
   ------------------

   function Get_Axis_Use
      (Self  : not null access Gdk_Device_Record;
       Index : Guint) return Gdk_Axis_Use
   is
      function Internal
         (Self  : System.Address;
          Index : Guint) return Gdk_Axis_Use;
      pragma Import (C, Internal, "gdk_device_get_axis_use");
   begin
      return Internal (Get_Object (Self), Index);
   end Get_Axis_Use;

   ---------------------
   -- Get_Device_Type --
   ---------------------

   function Get_Device_Type
      (Self : not null access Gdk_Device_Record) return Gdk_Device_Type
   is
      function Internal (Self : System.Address) return Gdk_Device_Type;
      pragma Import (C, Internal, "gdk_device_get_device_type");
   begin
      return Internal (Get_Object (Self));
   end Get_Device_Type;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gdk_Device_Record)
       return Gdk.Display.Gdk_Display
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_device_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Display.Gdk_Display (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Display;

   --------------------
   -- Get_Has_Cursor --
   --------------------

   function Get_Has_Cursor
      (Self : not null access Gdk_Device_Record) return Boolean
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gdk_device_get_has_cursor");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Has_Cursor;

   --------------
   -- Get_Mode --
   --------------

   function Get_Mode
      (Self : not null access Gdk_Device_Record) return Gdk_Input_Mode
   is
      function Internal (Self : System.Address) return Gdk_Input_Mode;
      pragma Import (C, Internal, "gdk_device_get_mode");
   begin
      return Internal (Get_Object (Self));
   end Get_Mode;

   ----------------
   -- Get_N_Axes --
   ----------------

   function Get_N_Axes
      (Self : not null access Gdk_Device_Record) return Gint
   is
      function Internal (Self : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_device_get_n_axes");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Axes;

   ----------------
   -- Get_N_Keys --
   ----------------

   function Get_N_Keys
      (Self : not null access Gdk_Device_Record) return Gint
   is
      function Internal (Self : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_device_get_n_keys");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Keys;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Self : not null access Gdk_Device_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gdk_device_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Name;

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source
      (Self : not null access Gdk_Device_Record) return Gdk_Input_Source
   is
      function Internal (Self : System.Address) return Gdk_Input_Source;
      pragma Import (C, Internal, "gdk_device_get_source");
   begin
      return Internal (Get_Object (Self));
   end Get_Source;

   ---------------
   -- Get_State --
   ---------------

   procedure Get_State
      (Self   : not null access Gdk_Device_Record;
       Window : Gdk.Gdk_Window;
       Axes   : in out Gdouble;
       Mask   : in out Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal
         (Self   : System.Address;
          Window : Gdk.Gdk_Window;
          Axes   : in out Gdouble;
          Mask   : in out Gdk.Types.Gdk_Modifier_Type);
      pragma Import (C, Internal, "gdk_device_get_state");
   begin
      Internal (Get_Object (Self), Window, Axes, Mask);
   end Get_State;

   ----------
   -- Grab --
   ----------

   function Grab
      (Self           : not null access Gdk_Device_Record;
       Window         : Gdk.Gdk_Window;
       Grab_Ownership : Gdk_Grab_Ownership;
       Owner_Events   : Boolean;
       Event_Mask     : Gdk.Event.Gdk_Event_Mask;
       Cursor         : Gdk.Gdk_Cursor;
       Time           : Guint32) return Gdk_Grab_Status
   is
      function Internal
         (Self           : System.Address;
          Window         : Gdk.Gdk_Window;
          Grab_Ownership : Gdk_Grab_Ownership;
          Owner_Events   : Integer;
          Event_Mask     : Gdk.Event.Gdk_Event_Mask;
          Cursor         : Gdk.Gdk_Cursor;
          Time           : Guint32) return Gdk_Grab_Status;
      pragma Import (C, Internal, "gdk_device_grab");
   begin
      return Internal (Get_Object (Self), Window, Grab_Ownership, Boolean'Pos (Owner_Events), Event_Mask, Cursor, Time);
   end Grab;

   ------------------
   -- Set_Axis_Use --
   ------------------

   procedure Set_Axis_Use
      (Self  : not null access Gdk_Device_Record;
       Index : Guint;
       GUse  : Gdk_Axis_Use)
   is
      procedure Internal
         (Self  : System.Address;
          Index : Guint;
          GUse  : Gdk_Axis_Use);
      pragma Import (C, Internal, "gdk_device_set_axis_use");
   begin
      Internal (Get_Object (Self), Index, GUse);
   end Set_Axis_Use;

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key
      (Self      : not null access Gdk_Device_Record;
       Index     : Guint;
       Keyval    : Guint;
       Modifiers : Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal
         (Self      : System.Address;
          Index     : Guint;
          Keyval    : Guint;
          Modifiers : Gdk.Types.Gdk_Modifier_Type);
      pragma Import (C, Internal, "gdk_device_set_key");
   begin
      Internal (Get_Object (Self), Index, Keyval, Modifiers);
   end Set_Key;

   --------------
   -- Set_Mode --
   --------------

   function Set_Mode
      (Self : not null access Gdk_Device_Record;
       Mode : Gdk_Input_Mode) return Boolean
   is
      function Internal
         (Self : System.Address;
          Mode : Gdk_Input_Mode) return Integer;
      pragma Import (C, Internal, "gdk_device_set_mode");
   begin
      return Internal (Get_Object (Self), Mode) /= 0;
   end Set_Mode;

   ------------
   -- Ungrab --
   ------------

   procedure Ungrab
      (Self : not null access Gdk_Device_Record;
       Time : Guint32)
   is
      procedure Internal (Self : System.Address; Time : Guint32);
      pragma Import (C, Internal, "gdk_device_ungrab");
   begin
      Internal (Get_Object (Self), Time);
   end Ungrab;

   ----------
   -- Warp --
   ----------

   procedure Warp
      (Self   : not null access Gdk_Device_Record;
       Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class;
       X      : Gint;
       Y      : Gint)
   is
      procedure Internal
         (Self   : System.Address;
          Screen : System.Address;
          X      : Gint;
          Y      : Gint);
      pragma Import (C, Internal, "gdk_device_warp");
   begin
      Internal (Get_Object (Self), Get_Object (Screen), X, Y);
   end Warp;

end Gdk.Device;
