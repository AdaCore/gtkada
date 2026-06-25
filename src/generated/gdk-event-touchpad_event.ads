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

--  An event related to a gesture on a touchpad device.
--
--  Unlike touchscreens, where the windowing system sends basic sequences of
--  begin, update, end events, and leaves gesture recognition to the clients,
--  touchpad gestures are typically processed by the system, resulting in these
--  events.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;

package Gdk.Event.Touchpad_Event is

   type Gdk_Touchpad_Event is new Gdk_Event with null record;

   type Gdk_Touchpad_Gesture_Phase is (
      Phase_Begin,
      Phase_Update,
      Phase_End,
      Phase_Cancel);
   pragma Convention (C, Gdk_Touchpad_Gesture_Phase);
   --  Specifies the current state of a touchpad gesture.
   --
   --  All gestures are guaranteed to begin with an event with phase
   --  Gdk.Event.Touchpad_Event.Phase_Begin, followed by 0 or several events
   --  with phase Gdk.Event.Touchpad_Event.Phase_Update.
   --
   --  A finished gesture may have 2 possible outcomes, an event with phase
   --  Gdk.Event.Touchpad_Event.Phase_End will be emitted when the gesture is
   --  considered successful, this should be used as the hint to perform any
   --  permanent changes.
   --
   --  Cancelled gestures may be so for a variety of reasons, due to hardware
   --  or the compositor, or due to the gesture recognition layers hinting the
   --  gesture did not finish resolutely (eg. a 3rd finger being added during a
   --  pinch gesture). In these cases, the last event will report the phase
   --  Gdk.Event.Touchpad_Event.Phase_Cancel, this should be used as a hint to
   --  undo any visible/permanent changes that were done throughout the
   --  progress of the gesture.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Touchpad_Gesture_Phase_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Touchpad_Gesture_Phase);
   type Property_Gdk_Touchpad_Gesture_Phase is new Gdk_Touchpad_Gesture_Phase_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_touchpad_event_get_type");

   -------------
   -- Methods --
   -------------

   procedure Get_Deltas
      (Self : Gdk.Event.Gdk_Event;
       Dx   : out Gdouble;
       Dy   : out Gdouble);
   --  Extracts delta information from a touchpad event.
   --  @param Dx return location for x
   --  @param Dy return location for y

   function Get_Gesture_Phase
      (Self : Gdk.Event.Gdk_Event) return Gdk_Touchpad_Gesture_Phase;
   --  Extracts the touchpad gesture phase from a touchpad event.
   --  @return the gesture phase of Event

   function Get_N_Fingers (Self : Gdk.Event.Gdk_Event) return Guint;
   --  Extracts the number of fingers from a touchpad event.
   --  @return the number of fingers for Event

   function Get_Pinch_Angle_Delta
      (Self : Gdk.Event.Gdk_Event) return Gdouble;
   --  Extracts the angle delta from a touchpad pinch event.
   --  @return the angle delta of Event

   function Get_Pinch_Scale (Self : Gdk.Event.Gdk_Event) return Gdouble;
   --  Extracts the scale from a touchpad pinch event.
   --  @return the scale of Event

end Gdk.Event.Touchpad_Event;
