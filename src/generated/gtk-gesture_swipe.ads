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

--  Recognizes swipe gestures.
--
--  After a press/move/.../move/release sequence happens, the
--  [signalGtk.GestureSwipe::swipe] signal will be emitted, providing the
--  velocity and directionality of the sequence at the time it was lifted.
--
--  If the velocity is desired in intermediate points,
--  [methodGtk.GestureSwipe.get_velocity] can be called in a
--  [signalGtk.Gesture::update] handler.
--
--  All velocities are reported in pixels/sec units.

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Gtk.Gesture_Single; use Gtk.Gesture_Single;

package Gtk.Gesture_Swipe is

   type Gtk_Gesture_Swipe_Record is new Gtk_Gesture_Single_Record with null record;
   type Gtk_Gesture_Swipe is access all Gtk_Gesture_Swipe_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Gesture_Swipe);
   procedure Initialize
      (Self : not null access Gtk_Gesture_Swipe_Record'Class);
   --  Returns a newly created `GtkGesture` that recognizes swipes.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Gesture_Swipe_New return Gtk_Gesture_Swipe;
   --  Returns a newly created `GtkGesture` that recognizes swipes.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_gesture_swipe_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Velocity
      (Self       : not null access Gtk_Gesture_Swipe_Record;
       Velocity_X : out Gdouble;
       Velocity_Y : out Gdouble) return Boolean;
   --  Gets the current velocity.
   --  If the gesture is recognized, this function returns True and fills in
   --  Velocity_X and Velocity_Y with the recorded velocity, as per the last
   --  events processed.
   --  @param Velocity_X return value for the velocity in the X axis, in
   --  pixels/sec
   --  @param Velocity_Y return value for the velocity in the Y axis, in
   --  pixels/sec
   --  @return whether velocity could be calculated

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Gesture_Swipe_Gdouble_Gdouble_Void is not null access procedure
     (Self       : access Gtk_Gesture_Swipe_Record'Class;
      Velocity_X : Gdouble;
      Velocity_Y : Gdouble);

   type Cb_GObject_Gdouble_Gdouble_Void is not null access procedure
     (Self       : access Glib.Object.GObject_Record'Class;
      Velocity_X : Gdouble;
      Velocity_Y : Gdouble);

   Signal_Swipe : constant Glib.Signal_Name := "swipe";
   procedure On_Swipe
      (Self  : not null access Gtk_Gesture_Swipe_Record;
       Call  : Cb_Gtk_Gesture_Swipe_Gdouble_Gdouble_Void;
       After : Boolean := False);
   procedure On_Swipe
      (Self  : not null access Gtk_Gesture_Swipe_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the recognized gesture is finished.
   --
   --  Velocity and direction are a product of previously recorded events.
   -- 
   --  Callback parameters:
   --    --  @param Velocity_X velocity in the X axis, in pixels/sec
   --    --  @param Velocity_Y velocity in the Y axis, in pixels/sec

end Gtk.Gesture_Swipe;
