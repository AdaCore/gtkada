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

--  <description>
--  A Gdk.Frame_Clock.Gdk_Frame_Clock tells the application when to update and
--  repaint a window. This may be synced to the vertical refresh rate of the
--  monitor, for example. Even when the frame clock uses a simple timer rather
--  than a hardware-based vertical sync, the frame clock helps because it
--  ensures everything paints at the same time (reducing the total number of
--  frames). The frame clock can also automatically stop painting when it knows
--  the frames will not be visible, or scale back animation framerates.
--
--  Gdk.Frame_Clock.Gdk_Frame_Clock is designed to be compatible with an
--  OpenGL-based implementation or with mozRequestAnimationFrame in Firefox,
--  for example.
--
--  A frame clock is idle until someone requests a frame with
--  Gdk.Frame_Clock.Request_Phase. At some later point that makes sense for the
--  synchronization being implemented, the clock will process a frame and emit
--  signals for each phase that has been requested. (See the signals of the
--  Gdk.Frame_Clock.Gdk_Frame_Clock class for documentation of the phases.
--  Gdk.Frame_Clock.Gdk_Frame_Clock_Phase_Update and the
--  Gdk.Frame_Clock.Gdk_Frame_Clock::update signal are most interesting for
--  application writers, and are used to update the animations, using the frame
--  time given by Gdk.Frame_Clock.Get_Frame_Time.
--
--  The frame time is reported in microseconds and generally in the same
--  timescale as g_get_monotonic_time, however, it is not the same as
--  g_get_monotonic_time. The frame time does not advance during the time a
--  frame is being painted, and outside of a frame, an attempt is made so that
--  all calls to Gdk.Frame_Clock.Get_Frame_Time that are called at a "similar"
--  time get the same value. This means that if different animations are timed
--  by looking at the difference in time between an initial value from
--  Gdk.Frame_Clock.Get_Frame_Time and the value inside the
--  Gdk.Frame_Clock.Gdk_Frame_Clock::update signal of the clock, they will stay
--  exactly synchronized.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Frame_Timings;       use Gdk.Frame_Timings;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;

package Gdk.Frame_Clock is

   type Gdk_Frame_Clock_Record is new GObject_Record with null record;
   type Gdk_Frame_Clock is access all Gdk_Frame_Clock_Record'Class;

   type Gdk_Frame_Clock_Phase is mod 2 ** Integer'Size;
   pragma Convention (C, Gdk_Frame_Clock_Phase);
   --  Gdk.Frame_Clock.Gdk_Frame_Clock_Phase is used to represent the
   --  different paint clock phases that can be requested. The elements of the
   --  enumeration correspond to the signals of
   --  Gdk.Frame_Clock.Gdk_Frame_Clock.

   Gdk_Frame_Clock_Phase_None : constant Gdk_Frame_Clock_Phase := 0;
   Gdk_Frame_Clock_Phase_Flush_Events : constant Gdk_Frame_Clock_Phase := 1;
   Gdk_Frame_Clock_Phase_Before_Paint : constant Gdk_Frame_Clock_Phase := 2;
   Gdk_Frame_Clock_Phase_Update : constant Gdk_Frame_Clock_Phase := 4;
   Gdk_Frame_Clock_Phase_Layout : constant Gdk_Frame_Clock_Phase := 8;
   Gdk_Frame_Clock_Phase_Paint : constant Gdk_Frame_Clock_Phase := 16;
   Gdk_Frame_Clock_Phase_Resume_Events : constant Gdk_Frame_Clock_Phase := 32;
   Gdk_Frame_Clock_Phase_After_Paint : constant Gdk_Frame_Clock_Phase := 64;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Frame_Clock_Phase_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Frame_Clock_Phase);
   type Property_Gdk_Frame_Clock_Phase is new Gdk_Frame_Clock_Phase_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_frame_clock_get_type");

   -------------
   -- Methods --
   -------------

   procedure Begin_Updating (Self : not null access Gdk_Frame_Clock_Record);
   --  Starts updates for an animation. Until a matching call to
   --  Gdk.Frame_Clock.End_Updating is made, the frame clock will continually
   --  request a new frame with the
   --  Gdk.Frame_Clock.Gdk_Frame_Clock_Phase_Update phase. This function may be
   --  called multiple times and frames will be requested until
   --  Gdk.Frame_Clock.End_Updating is called the same number of times.
   --  Since: gtk+ 3.8

   procedure End_Updating (Self : not null access Gdk_Frame_Clock_Record);
   --  Stops updates for an animation. See the documentation for
   --  Gdk.Frame_Clock.Begin_Updating.
   --  Since: gtk+ 3.8

   function Get_Current_Timings
      (Self : not null access Gdk_Frame_Clock_Record)
       return Gdk.Frame_Timings.Gdk_Frame_Timings;
   --  Gets the frame timings for the current frame.
   --  Since: gtk+ 3.8

   function Get_Frame_Counter
      (Self : not null access Gdk_Frame_Clock_Record) return Gint64;
   --  A Gdk.Frame_Clock.Gdk_Frame_Clock maintains a 64-bit counter that
   --  increments for each frame drawn.
   --  Since: gtk+ 3.8

   function Get_Frame_Time
      (Self : not null access Gdk_Frame_Clock_Record) return Gint64;
   --  Gets the time that should currently be used for animations. Inside the
   --  processing of a frame, it's the time used to compute the animation
   --  position of everything in a frame. Outside of a frame, it's the time of
   --  the conceptual "previous frame," which may be either the actual previous
   --  frame time, or if that's too old, an updated time.
   --  Since: gtk+ 3.8

   function Get_History_Start
      (Self : not null access Gdk_Frame_Clock_Record) return Gint64;
   --  Gdk.Frame_Clock.Gdk_Frame_Clock internally keeps a history of
   --  Gdk.Frame_Timings.Gdk_Frame_Timings objects for recent frames that can
   --  be retrieved with Gdk.Frame_Clock.Get_Timings. The set of stored frames
   --  is the set from the counter values given by
   --  Gdk.Frame_Clock.Get_History_Start and Gdk.Frame_Clock.Get_Frame_Counter,
   --  inclusive.
   --  Since: gtk+ 3.8

   procedure Get_Refresh_Info
      (Self                     : not null access Gdk_Frame_Clock_Record;
       Base_Time                : Gint64;
       Refresh_Interval_Return  : out Gint64;
       Presentation_Time_Return : out Gint64);
   --  Using the frame history stored in the frame clock, finds the last known
   --  presentation time and refresh interval, and assuming that presentation
   --  times are separated by the refresh interval, predicts a presentation
   --  time that is a multiple of the refresh interval after the last
   --  presentation time, and later than Base_Time.
   --  Since: gtk+ 3.8
   --  "base_time": base time for determining a presentaton time
   --  "refresh_interval_return": a location to store the determined refresh
   --  interval, or null. A default refresh interval of 1/60th of a second will
   --  be stored if no history is present.
   --  "presentation_time_return": a location to store the next candidate
   --  presentation time after the given base time. 0 will be will be stored if
   --  no history is present.

   function Get_Timings
      (Self          : not null access Gdk_Frame_Clock_Record;
       Frame_Counter : Gint64) return Gdk.Frame_Timings.Gdk_Frame_Timings;
   --  Retrieves a Gdk.Frame_Timings.Gdk_Frame_Timings object holding timing
   --  information for the current frame or a recent frame. The
   --  Gdk.Frame_Timings.Gdk_Frame_Timings object may not yet be complete: see
   --  Gdk.Frame_Timings.Get_Complete.
   --  Since: gtk+ 3.8
   --  "frame_counter": the frame counter value identifying the frame to be
   --  received.

   procedure Request_Phase
      (Self  : not null access Gdk_Frame_Clock_Record;
       Phase : Gdk_Frame_Clock_Phase);
   --  Asks the frame clock to run a particular phase. The signal
   --  corresponding the requested phase will be emitted the next time the
   --  frame clock processes. Multiple calls to Gdk.Frame_Clock.Request_Phase
   --  will be combined together and only one frame processed. If you are
   --  displaying animated content and want to continually request the
   --  Gdk.Frame_Clock.Gdk_Frame_Clock_Phase_Update phase for a period of time,
   --  you should use Gdk.Frame_Clock.Begin_Updating instead, since this allows
   --  GTK+ to adjust system parameters to get maximally smooth animations.
   --  Since: gtk+ 3.8
   --  "phase": the phase that is requested

   -------------
   -- Signals --
   -------------

   type Cb_Gdk_Frame_Clock_Void is not null access procedure
     (Self : access Gdk_Frame_Clock_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_After_Paint : constant Glib.Signal_Name := "after-paint";
   procedure On_After_Paint
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_Gdk_Frame_Clock_Void;
       After : Boolean := False);
   procedure On_After_Paint
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal ends processing of the frame. Applications should generally
   --  not handle this signal.

   Signal_Before_Paint : constant Glib.Signal_Name := "before-paint";
   procedure On_Before_Paint
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_Gdk_Frame_Clock_Void;
       After : Boolean := False);
   procedure On_Before_Paint
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal begins processing of the frame. Applications should
   --  generally not handle this signal.

   Signal_Flush_Events : constant Glib.Signal_Name := "flush-events";
   procedure On_Flush_Events
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_Gdk_Frame_Clock_Void;
       After : Boolean := False);
   procedure On_Flush_Events
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is used to flush pending motion events that are being
   --  batched up and compressed together. Applications should not handle this
   --  signal.

   Signal_Layout : constant Glib.Signal_Name := "layout";
   procedure On_Layout
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_Gdk_Frame_Clock_Void;
       After : Boolean := False);
   procedure On_Layout
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted as the second step of toolkit and application
   --  processing of the frame. Any work to update sizes and positions of
   --  application elements should be performed. GTK+ normally handles this
   --  internally.

   Signal_Paint : constant Glib.Signal_Name := "paint";
   procedure On_Paint
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_Gdk_Frame_Clock_Void;
       After : Boolean := False);
   procedure On_Paint
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted as the third step of toolkit and application
   --  processing of the frame. The frame is repainted. GDK normally handles
   --  this internally and produces expose events, which are turned into GTK+
   --  Gtk.Widget.Gtk_Widget::draw signals.

   Signal_Resume_Events : constant Glib.Signal_Name := "resume-events";
   procedure On_Resume_Events
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_Gdk_Frame_Clock_Void;
       After : Boolean := False);
   procedure On_Resume_Events
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted after processing of the frame is finished, and
   --  is handled internally by GTK+ to resume normal event processing.
   --  Applications should not handle this signal.

   Signal_Update : constant Glib.Signal_Name := "update";
   procedure On_Update
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_Gdk_Frame_Clock_Void;
       After : Boolean := False);
   procedure On_Update
      (Self  : not null access Gdk_Frame_Clock_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted as the first step of toolkit and application
   --  processing of the frame. Animations should be updated using
   --  Gdk.Frame_Clock.Get_Frame_Time. Applications can connect directly to
   --  this signal, or use Gtk.Widget.Add_Tick_Callback as a more convenient
   --  interface.

end Gdk.Frame_Clock;
