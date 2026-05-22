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

--  Tells the application when to update and repaint a surface.
--
--  This may be synced to the vertical refresh rate of the monitor, for
--  example. Even when the frame clock uses a simple timer rather than a
--  hardware-based vertical sync, the frame clock helps because it ensures
--  everything paints at the same time (reducing the total number of frames).
--
--  The frame clock can also automatically stop painting when it knows the
--  frames will not be visible, or scale back animation framerates.
--
--  `GdkFrameClock` is designed to be compatible with an OpenGL-based
--  implementation or with mozRequestAnimationFrame in Firefox, for example.
--
--  A frame clock is idle until someone requests a frame with
--  [methodGdk.FrameClock.request_phase]. At some later point that makes sense
--  for the synchronization being implemented, the clock will process a frame
--  and emit signals for each phase that has been requested. (See the signals
--  of the `GdkFrameClock` class for documentation of the phases.
--  Gdk.Frame_Clock.Gdk_Frame_Clock_Phase_Update and the
--  [signalGdk.FrameClock::update] signal are most interesting for application
--  writers, and are used to update the animations, using the frame time given
--  by [methodGdk.FrameClock.get_frame_time].
--
--  The frame time is reported in microseconds and generally in the same
--  timescale as g_get_monotonic_time, however, it is not the same as
--  g_get_monotonic_time. The frame time does not advance during the time a
--  frame is being painted, and outside of a frame, an attempt is made so that
--  all calls to [methodGdk.FrameClock.get_frame_time] that are called at a
--  "similar" time get the same value. This means that if different animations
--  are timed by looking at the difference in time between an initial value
--  from [methodGdk.FrameClock.get_frame_time] and the value inside the
--  [signalGdk.FrameClock::update] signal of the clock, they will stay exactly
--  synchronized.

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
   --  Used to represent the different paint clock phases that can be
   --  requested.
   --
   --  The elements of the enumeration correspond to the signals of
   --  `GdkFrameClock`.

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
   --  Starts updates for an animation.
   --  Until a matching call to [methodGdk.FrameClock.end_updating] is made,
   --  the frame clock will continually request a new frame with the
   --  Gdk.Frame_Clock.Gdk_Frame_Clock_Phase_Update phase. This function may be
   --  called multiple times and frames will be requested until
   --  Gdk.Frame_Clock.End_Updating is called the same number of times.

   procedure End_Updating (Self : not null access Gdk_Frame_Clock_Record);
   --  Stops updates for an animation.
   --  See the documentation for [methodGdk.FrameClock.begin_updating].

   function Get_Current_Timings
      (Self : not null access Gdk_Frame_Clock_Record)
       return Gdk.Frame_Timings.Gdk_Frame_Timings;
   --  Gets the frame timings for the current frame.
   --  @return the `GdkFrameTimings` for the frame currently being processed,
   --  or even no frame is being processed, for the previous frame. Before any
   --  frames have been processed, returns null.

   function Get_Fps
      (Self : not null access Gdk_Frame_Clock_Record) return Gdouble;
   --  Calculates the current frames-per-second, based on the frame timings of
   --  Frame_Clock.
   --  @return the current fps, as a `double`

   function Get_Frame_Counter
      (Self : not null access Gdk_Frame_Clock_Record) return Gint64;
   --  `GdkFrameClock` maintains a 64-bit counter that increments for each
   --  frame drawn.
   --  @return inside frame processing, the value of the frame counter for the
   --  current frame. Outside of frame processing, the frame counter for the
   --  last frame.

   function Get_Frame_Time
      (Self : not null access Gdk_Frame_Clock_Record) return Gint64;
   --  Gets the time that should currently be used for animations.
   --  Inside the processing of a frame, it's the time used to compute the
   --  animation position of everything in a frame. Outside of a frame, it's
   --  the time of the conceptual "previous frame," which may be either the
   --  actual previous frame time, or if that's too old, an updated time.
   --  @return a timestamp in microseconds, in the timescale of of
   --  g_get_monotonic_time.

   function Get_History_Start
      (Self : not null access Gdk_Frame_Clock_Record) return Gint64;
   --  Returns the frame counter for the oldest frame available in history.
   --  `GdkFrameClock` internally keeps a history of `GdkFrameTimings` objects
   --  for recent frames that can be retrieved with
   --  [methodGdk.FrameClock.get_timings]. The set of stored frames is the set
   --  from the counter values given by
   --  [methodGdk.FrameClock.get_history_start] and
   --  [methodGdk.FrameClock.get_frame_counter], inclusive.
   --  @return the frame counter value for the oldest frame that is available
   --  in the internal frame history of the `GdkFrameClock`

   procedure Get_Refresh_Info
      (Self                     : not null access Gdk_Frame_Clock_Record;
       Base_Time                : Gint64;
       Refresh_Interval_Return  : out Gint64;
       Presentation_Time_Return : out Gint64);
   --  Predicts a presentation time, based on history.
   --  Using the frame history stored in the frame clock, finds the last known
   --  presentation time and refresh interval, and assuming that presentation
   --  times are separated by the refresh interval, predicts a presentation
   --  time that is a multiple of the refresh interval after the last
   --  presentation time, and later than Base_Time.
   --  @param Base_Time base time for determining a presentaton time
   --  @param Refresh_Interval_Return a location to store the determined
   --  refresh interval, or null. A default refresh interval of 1/60th of a
   --  second will be stored if no history is present.
   --  @param Presentation_Time_Return a location to store the next candidate
   --  presentation time after the given base time. 0 will be will be stored if
   --  no history is present.

   function Get_Timings
      (Self          : not null access Gdk_Frame_Clock_Record;
       Frame_Counter : Gint64) return Gdk.Frame_Timings.Gdk_Frame_Timings;
   --  Retrieves a `GdkFrameTimings` object holding timing information for the
   --  current frame or a recent frame.
   --  The `GdkFrameTimings` object may not yet be complete: see
   --  [methodGdk.FrameTimings.get_complete] and
   --  [methodGdk.FrameClock.get_history_start].
   --  @param Frame_Counter the frame counter value identifying the frame to
   --  be received
   --  @return the `GdkFrameTimings` object for the specified frame, or null
   --  if it is not available

   procedure Request_Phase
      (Self  : not null access Gdk_Frame_Clock_Record;
       Phase : Gdk_Frame_Clock_Phase);
   --  Asks the frame clock to run a particular phase.
   --  The signal corresponding the requested phase will be emitted the next
   --  time the frame clock processes. Multiple calls to
   --  Gdk.Frame_Clock.Request_Phase will be combined together and only one
   --  frame processed. If you are displaying animated content and want to
   --  continually request the Gdk.Frame_Clock.Gdk_Frame_Clock_Phase_Update
   --  phase for a period of time, you should use
   --  [methodGdk.FrameClock.begin_updating] instead, since this allows GTK to
   --  adjust system parameters to get maximally smooth animations.
   --  @param Phase the phase that is requested

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
   --  This signal ends processing of the frame.
   --
   --  Applications should generally not handle this signal.

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
   --  Begins processing of the frame.
   --
   --  Applications should generally not handle this signal.

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
   --  Used to flush pending motion events that are being batched up and
   --  compressed together.
   --
   --  Applications should not handle this signal.

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
   --  Emitted as the second step of toolkit and application processing of the
   --  frame.
   --
   --  Any work to update sizes and positions of application elements should
   --  be performed. GTK normally handles this internally.

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
   --  Emitted as the third step of toolkit and application processing of the
   --  frame.
   --
   --  The frame is repainted. GDK normally handles this internally and emits
   --  [signalGdk.Surface::render] signals which are turned into
   --  [GtkWidget::snapshot](../gtk4/signal.Widget.snapshot.html) signals by
   --  GTK.

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
   --  Emitted after processing of the frame is finished.
   --
   --  This signal is handled internally by GTK to resume normal event
   --  processing. Applications should not handle this signal.

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
   --  Emitted as the first step of toolkit and application processing of the
   --  frame.
   --
   --  Animations should be updated using
   --  [methodGdk.FrameClock.get_frame_time]. Applications can connect directly
   --  to this signal, or use
   --  [gtk_widget_add_tick_callback](../gtk4/method.Widget.add_tick_callback.html)
   --  as a more convenient interface.

end Gdk.Frame_Clock;
