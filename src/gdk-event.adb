------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2026, AdaCore                          --
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

with Gdk.Seat;
with Glib.Object; use Glib.Object;

package body Gdk.Event is

   use type Glib.Gboolean;
   use type System.Address;

   function Event_Ref (Event : System.Address) return System.Address;
   pragma Import (C, Event_Ref, "gdk_event_ref");

   procedure Event_Unref (Event : System.Address);
   pragma Import (C, Event_Unref, "gdk_event_unref");

   procedure Unreference (Self : in out Gdk_Event'Class);
   pragma Inline (Unreference);

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Gdk_Event) is
   begin
      if Self.Data /= System.Null_Address then
         Self.Data := Event_Ref (Self.Data);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Gdk_Event) is
   begin
      Unreference (Self);
   end Finalize;

   -----------------
   -- From_Object --
   -----------------

   function From_Object (Object : System.Address) return Gdk_Event is
   begin
      return Result : Gdk_Event do
         Set_Object (Result, Object);
      end return;
   end From_Object;

   ---------------
   -- Get_Angle --
   ---------------

   function Get_Angle
     (Event_1 : Gdk_Event'Class;
      Event_2 : Gdk_Event'Class;
      Angle   : out Glib.Gdouble) return Boolean
   is
      function Internal
        (Event_1 : System.Address;
         Event_2 : System.Address;
         Angle   : out Glib.Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_events_get_angle");
   begin
      return Internal (Get_Object (Event_1), Get_Object (Event_2), Angle) /= 0;
   end Get_Angle;

   ----------------
   -- Get_Center --
   ----------------

   function Get_Center
     (Event_1 : Gdk_Event'Class;
      Event_2 : Gdk_Event'Class;
      X       : out Glib.Gdouble;
      Y       : out Glib.Gdouble) return Boolean
   is
      function Internal
        (Event_1 : System.Address;
         Event_2 : System.Address;
         X       : out Glib.Gdouble;
         Y       : out Glib.Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_events_get_center");
   begin
      return Internal
        (Get_Object (Event_1), Get_Object (Event_2), X, Y) /= 0;
   end Get_Center;

   ------------------
   -- Get_Distance --
   ------------------

   function Get_Distance
     (Event_1  : Gdk_Event'Class;
      Event_2  : Gdk_Event'Class;
      Distance : out Glib.Gdouble) return Boolean
   is
      function Internal
        (Event_1   : System.Address;
         Event_2   : System.Address;
         Distance  : out Glib.Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_events_get_distance");
   begin
      return Internal
        (Get_Object (Event_1), Get_Object (Event_2), Distance) /= 0;
   end Get_Distance;

   --------------
   -- Get_Axes --
   --------------

   --  function Get_Axes
   --    (Self   : Gdk_Event'Class;
   --     Axes   : out System.Address;
   --     N_Axes : out Glib.Guint) return Boolean
   --  is
   --     function Internal
   --       (Self   : System.Address;
   --        Axes   : access System.Address;
   --        N_Axes : access Glib.Guint) return Glib.Gboolean;
   --     pragma Import (C, Internal, "gdk_event_get_axes");
   --  begin
   --     return Internal (Get_Object (Self), Axes'Access, N_Axes'Access) /= 0;
   --  end Get_Axes;

   --------------
   -- Get_Axis --
   --------------

   function Get_Axis
     (Self     : Gdk_Event'Class;
      Axis_Use : Glib.Gint;
      Value    : out Glib.Gdouble) return Boolean
   is
      function Internal
        (Self     : System.Address;
         Axis_Use : Glib.Gint;
         Value    : out Glib.Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_event_get_axis");
   begin
      return Internal (Get_Object (Self), Axis_Use, Value) /= 0;
   end Get_Axis;

   ----------------
   -- Get_Device --
   ----------------

   function Get_Device (Self : Gdk_Event'Class) return Gdk.Device.Gdk_Device is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_event_get_device");
      Stub_Gdk_Device : Gdk.Device.Gdk_Device_Record;
   begin
      return Gdk.Device.Gdk_Device
        (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Device));
   end Get_Device;

   ---------------------
   -- Get_Device_Tool --
   ---------------------

   function Get_Device_Tool
     (Self : Gdk_Event'Class) return Gdk.Device_Tool.Gdk_Device_Tool
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_event_get_device_tool");
      Stub_Gdk_Device_Tool : Gdk.Device_Tool.Gdk_Device_Tool_Record;
   begin
      return Gdk.Device_Tool.Gdk_Device_Tool
        (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Device_Tool));
   end Get_Device_Tool;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display (Self : Gdk_Event'Class) return Gdk.Display.Gdk_Display is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_event_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Display.Gdk_Display
        (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Display;

   ------------------------
   -- Get_Event_Sequence --
   ------------------------

   --  function Get_Event_Sequence (Self : Gdk_Event'Class) return System.Address is
   --     function Internal (Self : System.Address) return System.Address;
   --     pragma Import (C, Internal, "gdk_event_get_event_sequence");
   --  begin
   --     return Internal (Get_Object (Self));
   --  end Get_Event_Sequence;

   --------------------
   -- Get_Event_Type --
   --------------------

   function Get_Event_Type (Self : Gdk_Event'Class) return Gdk_Event_Type is
      function Internal (Self : System.Address) return Gdk_Event_Type;
      pragma Import (C, Internal, "gdk_event_get_event_type");
   begin
      return Internal (Get_Object (Self));
   end Get_Event_Type;

   -----------------
   -- Get_History --
   -----------------

   --  function Get_History
   --    (Self     : Gdk_Event'Class;
   --     N_Coords : out Glib.Guint) return System.Address
   --  is
   --     function Internal
   --       (Self     : System.Address;
   --        N_Coords : access Glib.Guint) return System.Address;
   --     pragma Import (C, Internal, "gdk_event_get_history");
   --  begin
   --     return Internal (Get_Object (Self), N_Coords'Access);
   --  end Get_History;

   ------------------------
   -- Get_Modifier_State --
   ------------------------

   --  function Get_Modifier_State (Self : Gdk_Event'Class) return Glib.Guint is
   --     function Internal (Self : System.Address) return Glib.Guint;
   --     pragma Import (C, Internal, "gdk_event_get_modifier_state");
   --  begin
   --     return Internal (Get_Object (Self));
   --  end Get_Modifier_State;

   --------------------------
   -- Get_Pointer_Emulated --
   --------------------------

   function Get_Pointer_Emulated (Self : Gdk_Event'Class) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_event_get_pointer_emulated");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Pointer_Emulated;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position
     (Self : Gdk_Event'Class;
      X    : out Glib.Gdouble;
      Y    : out Glib.Gdouble) return Boolean
   is
      function Internal
        (Self : System.Address;
         X    : out Glib.Gdouble;
         Y    : out Glib.Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_event_get_position");
   begin
      return Internal (Get_Object (Self), X, Y) /= 0;
   end Get_Position;

   --------------
   -- Get_Seat --
   --------------

   function Get_Seat (Self : Gdk_Event'Class) return Gdk.Gdk_Seat is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_event_get_seat");
      Stub_Gdk_Seat : Gdk.Seat.Gdk_Seat_Record;
   begin
      return Gdk.Gdk_Seat
        (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Seat));
   end Get_Seat;

   -----------------
   -- Get_Surface --
   -----------------

   function Get_Surface (Self : Gdk_Event'Class) return Gdk.Surface.Gdk_Surface is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_event_get_surface");
      Stub_Gdk_Surface : Gdk.Surface.Gdk_Surface_Record;
   begin
      return Gdk.Surface.Gdk_Surface
        (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Surface));
   end Get_Surface;

   --------------
   -- Get_Time --
   --------------

   function Get_Time (Self : Gdk_Event'Class) return Glib.Guint32 is
      function Internal (Self : System.Address) return Glib.Guint32;
      pragma Import (C, Internal, "gdk_event_get_time");
   begin
      return Internal (Get_Object (Self));
   end Get_Time;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (Self : Gdk_Event'Class) return System.Address is
   begin
      return Self.Data;
   end Get_Object;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : Gdk_Event'Class) return Boolean is
   begin
      return Self.Data = System.Null_Address;
   end Is_Null;

   ---------------------------
   -- Triggers_Context_Menu --
   ---------------------------

   function Triggers_Context_Menu (Self : Gdk_Event'Class) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_event_triggers_context_menu");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Triggers_Context_Menu;

   ----------------
   -- Set_Object --
   ----------------

   procedure Set_Object
     (Self   : in out Gdk_Event'Class;
      Object : System.Address)
   is
   begin
      if Self.Data = Object then
         return;
      end if;

      Unreference (Self);

      if Object /= System.Null_Address then
         Self.Data := Event_Ref (Object);
      end if;
   end Set_Object;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Self : in out Gdk_Event'Class) is
   begin
      if Self.Data /= System.Null_Address then
         Event_Unref (Self.Data);
         Self.Data := System.Null_Address;
      end if;
   end Unreference;

end Gdk.Event;