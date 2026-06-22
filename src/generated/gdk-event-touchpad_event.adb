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

package body Gdk.Event.Touchpad_Event is

   ----------------
   -- Get_Deltas --
   ----------------

   procedure Get_Deltas
      (Self : Gdk.Event.Gdk_Event;
       Dx   : out Gdouble;
       Dy   : out Gdouble)
   is
      procedure Internal
         (Self : System.Address;
          Dx   : out Gdouble;
          Dy   : out Gdouble);
      pragma Import (C, Internal, "gdk_touchpad_event_get_deltas");
   begin
      Internal (Get_Object (Self), Dx, Dy);
   end Get_Deltas;

   -----------------------
   -- Get_Gesture_Phase --
   -----------------------

   function Get_Gesture_Phase
      (Self : Gdk.Event.Gdk_Event) return Gdk_Touchpad_Gesture_Phase
   is
      function Internal
         (Self : System.Address) return Gdk_Touchpad_Gesture_Phase;
      pragma Import (C, Internal, "gdk_touchpad_event_get_gesture_phase");
   begin
      return Internal (Get_Object (Self));
   end Get_Gesture_Phase;

   -------------------
   -- Get_N_Fingers --
   -------------------

   function Get_N_Fingers (Self : Gdk.Event.Gdk_Event) return Guint is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gdk_touchpad_event_get_n_fingers");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Fingers;

   ---------------------------
   -- Get_Pinch_Angle_Delta --
   ---------------------------

   function Get_Pinch_Angle_Delta
      (Self : Gdk.Event.Gdk_Event) return Gdouble
   is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "gdk_touchpad_event_get_pinch_angle_delta");
   begin
      return Internal (Get_Object (Self));
   end Get_Pinch_Angle_Delta;

   ---------------------
   -- Get_Pinch_Scale --
   ---------------------

   function Get_Pinch_Scale (Self : Gdk.Event.Gdk_Event) return Gdouble is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "gdk_touchpad_event_get_pinch_scale");
   begin
      return Internal (Get_Object (Self));
   end Get_Pinch_Scale;

end Gdk.Event.Touchpad_Event;
