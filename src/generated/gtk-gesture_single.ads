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
--  Gtk.Gesture_Single.Gtk_Gesture_Single is a subclass of
--  Gtk.Gesture.Gtk_Gesture, optimized (although not restricted) for dealing
--  with mouse and single-touch gestures. Under interaction, these gestures
--  stick to the first interacting sequence, which is accessible through
--  Gtk.Gesture_Single.Get_Current_Sequence while the gesture is being
--  interacted with.
--
--  By default gestures react to both GDK_BUTTON_PRIMARY and touch events,
--  Gtk.Gesture_Single.Set_Touch_Only can be used to change the touch behavior.
--  Callers may also specify a different mouse button number to interact with
--  through Gtk.Gesture_Single.Set_Button, or react to any mouse button by
--  setting 0. While the gesture is active, the button being currently pressed
--  can be known through Gtk.Gesture_Single.Get_Current_Button.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Event;       use Gdk.Event;
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Gtk.Gesture;     use Gtk.Gesture;

package Gtk.Gesture_Single is

   type Gtk_Gesture_Single_Record is new Gtk_Gesture_Record with null record;
   type Gtk_Gesture_Single is access all Gtk_Gesture_Single_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_gesture_single_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Button
      (Self : not null access Gtk_Gesture_Single_Record) return Guint;
   --  Returns the button number Gesture listens for, or 0 if Gesture reacts
   --  to any button press.
   --  Since: gtk+ 3.14

   procedure Set_Button
      (Self   : not null access Gtk_Gesture_Single_Record;
       Button : Guint);
   --  Sets the button number Gesture listens to. If non-0, every button press
   --  from a different button number will be ignored. Touch events implicitly
   --  match with button 1.
   --  Since: gtk+ 3.14
   --  "button": button number to listen to, or 0 for any button

   function Get_Current_Button
      (Self : not null access Gtk_Gesture_Single_Record) return Guint;
   --  Returns the button number currently interacting with Gesture, or 0 if
   --  there is none.
   --  Since: gtk+ 3.14

   function Get_Current_Sequence
      (Self : not null access Gtk_Gesture_Single_Record)
       return Gdk.Event.Gdk_Event_Sequence;
   --  Returns the event sequence currently interacting with Gesture. This is
   --  only meaningful if Gtk.Gesture.Is_Active returns True.
   --  Since: gtk+ 3.14

   function Get_Exclusive
      (Self : not null access Gtk_Gesture_Single_Record) return Boolean;
   --  Gets whether a gesture is exclusive. For more information, see
   --  Gtk.Gesture_Single.Set_Exclusive.
   --  Since: gtk+ 3.14

   procedure Set_Exclusive
      (Self      : not null access Gtk_Gesture_Single_Record;
       Exclusive : Boolean);
   --  Sets whether Gesture is exclusive. An exclusive gesture will only
   --  handle pointer and "pointer emulated" touch events, so at any given
   --  time, there is only one sequence able to interact with those.
   --  Since: gtk+ 3.14
   --  "exclusive": True to make Gesture exclusive

   function Get_Touch_Only
      (Self : not null access Gtk_Gesture_Single_Record) return Boolean;
   --  Returns True if the gesture is only triggered by touch events.
   --  Since: gtk+ 3.14

   procedure Set_Touch_Only
      (Self       : not null access Gtk_Gesture_Single_Record;
       Touch_Only : Boolean);
   --  If Touch_Only is True, Gesture will only handle events of type
   --  GDK_TOUCH_BEGIN, GDK_TOUCH_UPDATE or GDK_TOUCH_END. If False, mouse
   --  events will be handled too.
   --  Since: gtk+ 3.14
   --  "touch_only": whether Gesture handles only touch events

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Button_Property : constant Glib.Properties.Property_Uint;
   --  Mouse button number to listen to, or 0 to listen for any button.

   Exclusive_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the gesture is exclusive. Exclusive gestures only listen to
   --  pointer and pointer emulated events.

   Touch_Only_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the gesture handles only touch events.

private
   Touch_Only_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("touch-only");
   Exclusive_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("exclusive");
   Button_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("button");
end Gtk.Gesture_Single;
