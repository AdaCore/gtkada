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

package body Gdk.Frame_Clock is

   package Type_Conversion_Gdk_Frame_Clock is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Frame_Clock_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Frame_Clock);

   --------------------
   -- Begin_Updating --
   --------------------

   procedure Begin_Updating (Self : not null access Gdk_Frame_Clock_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_frame_clock_begin_updating");
   begin
      Internal (Get_Object (Self));
   end Begin_Updating;

   ------------------
   -- End_Updating --
   ------------------

   procedure End_Updating (Self : not null access Gdk_Frame_Clock_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_frame_clock_end_updating");
   begin
      Internal (Get_Object (Self));
   end End_Updating;

   -------------------------
   -- Get_Current_Timings --
   -------------------------

   function Get_Current_Timings
      (Self : not null access Gdk_Frame_Clock_Record)
       return Gdk.Frame_Timings.Gdk_Frame_Timings
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_frame_clock_get_current_timings");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Current_Timings;

   -----------------------
   -- Get_Frame_Counter --
   -----------------------

   function Get_Frame_Counter
      (Self : not null access Gdk_Frame_Clock_Record) return Gint64
   is
      function Internal (Self : System.Address) return Gint64;
      pragma Import (C, Internal, "gdk_frame_clock_get_frame_counter");
   begin
      return Internal (Get_Object (Self));
   end Get_Frame_Counter;

   --------------------
   -- Get_Frame_Time --
   --------------------

   function Get_Frame_Time
      (Self : not null access Gdk_Frame_Clock_Record) return Gint64
   is
      function Internal (Self : System.Address) return Gint64;
      pragma Import (C, Internal, "gdk_frame_clock_get_frame_time");
   begin
      return Internal (Get_Object (Self));
   end Get_Frame_Time;

   -----------------------
   -- Get_History_Start --
   -----------------------

   function Get_History_Start
      (Self : not null access Gdk_Frame_Clock_Record) return Gint64
   is
      function Internal (Self : System.Address) return Gint64;
      pragma Import (C, Internal, "gdk_frame_clock_get_history_start");
   begin
      return Internal (Get_Object (Self));
   end Get_History_Start;

   ----------------------
   -- Get_Refresh_Info --
   ----------------------

   procedure Get_Refresh_Info
      (Self                     : not null access Gdk_Frame_Clock_Record;
       Base_Time                : Gint64;
       Refresh_Interval_Return  : out Gint64;
       Presentation_Time_Return : out Gint64)
   is
      procedure Internal
         (Self                     : System.Address;
          Base_Time                : Gint64;
          Refresh_Interval_Return  : out Gint64;
          Presentation_Time_Return : out Gint64);
      pragma Import (C, Internal, "gdk_frame_clock_get_refresh_info");
   begin
      Internal (Get_Object (Self), Base_Time, Refresh_Interval_Return, Presentation_Time_Return);
   end Get_Refresh_Info;

   -----------------
   -- Get_Timings --
   -----------------

   function Get_Timings
      (Self          : not null access Gdk_Frame_Clock_Record;
       Frame_Counter : Gint64) return Gdk.Frame_Timings.Gdk_Frame_Timings
   is
      function Internal
         (Self          : System.Address;
          Frame_Counter : Gint64) return System.Address;
      pragma Import (C, Internal, "gdk_frame_clock_get_timings");
   begin
      return From_Object (Internal (Get_Object (Self), Frame_Counter));
   end Get_Timings;

   -------------------
   -- Request_Phase --
   -------------------

   procedure Request_Phase
      (Self  : not null access Gdk_Frame_Clock_Record;
       Phase : Gdk_Frame_Clock_Phase)
   is
      procedure Internal
         (Self  : System.Address;
          Phase : Gdk_Frame_Clock_Phase);
      pragma Import (C, Internal, "gdk_frame_clock_request_phase");
   begin
      Internal (Get_Object (Self), Phase);
   end Request_Phase;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Frame_Clock_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Frame_Clock_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gdk_Frame_Clock_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Frame_Clock_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gdk_Frame_Clock_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gdk_Frame_Clock_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Frame_Clock_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Frame_Clock_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Frame_Clock_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Frame_Clock_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gdk_Frame_Clock_Record'Class;
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

   --------------------------------
   -- Marsh_Gdk_Frame_Clock_Void --
   --------------------------------

   procedure Marsh_Gdk_Frame_Clock_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Frame_Clock_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Frame_Clock := Gdk_Frame_Clock (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gdk_Frame_Clock_Void;

   --------------------
   -- On_After_Paint --
   --------------------

   procedure On_After_Paint
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_Gdk_Frame_Clock_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "after-paint" & ASCII.NUL, Call, After);
   end On_After_Paint;

   --------------------
   -- On_After_Paint --
   --------------------

   procedure On_After_Paint
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "after-paint" & ASCII.NUL, Call, After, Slot);
   end On_After_Paint;

   ---------------------
   -- On_Before_Paint --
   ---------------------

   procedure On_Before_Paint
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_Gdk_Frame_Clock_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "before-paint" & ASCII.NUL, Call, After);
   end On_Before_Paint;

   ---------------------
   -- On_Before_Paint --
   ---------------------

   procedure On_Before_Paint
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "before-paint" & ASCII.NUL, Call, After, Slot);
   end On_Before_Paint;

   ---------------------
   -- On_Flush_Events --
   ---------------------

   procedure On_Flush_Events
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_Gdk_Frame_Clock_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "flush-events" & ASCII.NUL, Call, After);
   end On_Flush_Events;

   ---------------------
   -- On_Flush_Events --
   ---------------------

   procedure On_Flush_Events
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "flush-events" & ASCII.NUL, Call, After, Slot);
   end On_Flush_Events;

   ---------------
   -- On_Layout --
   ---------------

   procedure On_Layout
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_Gdk_Frame_Clock_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "layout" & ASCII.NUL, Call, After);
   end On_Layout;

   ---------------
   -- On_Layout --
   ---------------

   procedure On_Layout
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "layout" & ASCII.NUL, Call, After, Slot);
   end On_Layout;

   --------------
   -- On_Paint --
   --------------

   procedure On_Paint
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_Gdk_Frame_Clock_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "paint" & ASCII.NUL, Call, After);
   end On_Paint;

   --------------
   -- On_Paint --
   --------------

   procedure On_Paint
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "paint" & ASCII.NUL, Call, After, Slot);
   end On_Paint;

   ----------------------
   -- On_Resume_Events --
   ----------------------

   procedure On_Resume_Events
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_Gdk_Frame_Clock_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "resume-events" & ASCII.NUL, Call, After);
   end On_Resume_Events;

   ----------------------
   -- On_Resume_Events --
   ----------------------

   procedure On_Resume_Events
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "resume-events" & ASCII.NUL, Call, After, Slot);
   end On_Resume_Events;

   ---------------
   -- On_Update --
   ---------------

   procedure On_Update
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_Gdk_Frame_Clock_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "update" & ASCII.NUL, Call, After);
   end On_Update;

   ---------------
   -- On_Update --
   ---------------

   procedure On_Update
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "update" & ASCII.NUL, Call, After, Slot);
   end On_Update;

end Gdk.Frame_Clock;
