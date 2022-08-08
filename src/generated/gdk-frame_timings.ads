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
--  A Gdk.Frame_Timings.Gdk_Frame_Timings object holds timing information for
--  a single frame of the application's displays. To retrieve
--  Gdk.Frame_Timings.Gdk_Frame_Timings objects, use
--  Gdk.Frame_Clock.Get_Timings or Gdk.Frame_Clock.Get_Current_Timings. The
--  information in Gdk.Frame_Timings.Gdk_Frame_Timings is useful for precise
--  synchronization of video with the event or audio streams, and for measuring
--  quality metrics for the application's display, such as latency and jitter.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib; use Glib;

package Gdk.Frame_Timings is

   type Gdk_Frame_Timings is new Glib.C_Boxed with null record;
   Null_Gdk_Frame_Timings : constant Gdk_Frame_Timings;

   function From_Object (Object : System.Address) return Gdk_Frame_Timings;
   function From_Object_Free (B : access Gdk_Frame_Timings'Class) return Gdk_Frame_Timings;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_frame_timings_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Complete (Self : Gdk_Frame_Timings) return Boolean;
   --  The timing information in a Gdk.Frame_Timings.Gdk_Frame_Timings is
   --  filled in incrementally as the frame as drawn and passed off to the
   --  window system for processing and display to the user. The accessor
   --  functions for Gdk.Frame_Timings.Gdk_Frame_Timings can return 0 to
   --  indicate an unavailable value for two reasons: either because the
   --  information is not yet available, or because it isn't available at all.
   --  Once Gdk.Frame_Timings.Get_Complete returns True for a frame, you can be
   --  certain that no further values will become available and be stored in
   --  the Gdk.Frame_Timings.Gdk_Frame_Timings.
   --  Since: gtk+ 3.8

   function Get_Frame_Counter (Self : Gdk_Frame_Timings) return Gint64;
   --  Gets the frame counter value of the Gdk.Frame_Clock.Gdk_Frame_Clock
   --  when this this frame was drawn.
   --  Since: gtk+ 3.8

   function Get_Frame_Time (Self : Gdk_Frame_Timings) return Gint64;
   --  Returns the frame time for the frame. This is the time value that is
   --  typically used to time animations for the frame. See
   --  Gdk.Frame_Clock.Get_Frame_Time.

   function Get_Predicted_Presentation_Time
      (Self : Gdk_Frame_Timings) return Gint64;
   --  Gets the predicted time at which this frame will be displayed. Although
   --  no predicted time may be available, if one is available, it will be
   --  available while the frame is being generated, in contrast to
   --  Gdk.Frame_Timings.Get_Presentation_Time, which is only available after
   --  the frame has been presented. In general, if you are simply animating,
   --  you should use Gdk.Frame_Clock.Get_Frame_Time rather than this function,
   --  but this function is useful for applications that want exact control
   --  over latency. For example, a movie player may want this information for
   --  Audio/Video synchronization.
   --  Since: gtk+ 3.8

   function Get_Presentation_Time (Self : Gdk_Frame_Timings) return Gint64;
   --  Reurns the presentation time. This is the time at which the frame
   --  became visible to the user.
   --  Since: gtk+ 3.8

   function Get_Refresh_Interval (Self : Gdk_Frame_Timings) return Gint64;
   --  Gets the natural interval between presentation times for the display
   --  that this frame was displayed on. Frame presentation usually happens
   --  during the "vertical blanking interval".
   --  Since: gtk+ 3.8

   function Ref (Self : Gdk_Frame_Timings) return Gdk_Frame_Timings;
   --  Increases the reference count of Timings.
   --  Since: gtk+ 3.8

   procedure Unref (Self : Gdk_Frame_Timings);
   --  Decreases the reference count of Timings. If Timings is no longer
   --  referenced, it will be freed.
   --  Since: gtk+ 3.8

private

   Null_Gdk_Frame_Timings : constant Gdk_Frame_Timings := (Glib.C_Boxed with null record);

end Gdk.Frame_Timings;
