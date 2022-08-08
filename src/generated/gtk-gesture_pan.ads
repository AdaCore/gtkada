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
--  Gtk.Gesture_Pan.Gtk_Gesture_Pan is a Gtk.Gesture.Gtk_Gesture
--  implementation able to recognize pan gestures, those are drags that are
--  locked to happen along one axis. The axis that a
--  Gtk.Gesture_Pan.Gtk_Gesture_Pan handles is defined at construct time, and
--  can be changed through Gtk.Gesture_Pan.Set_Orientation.
--
--  When the gesture starts to be recognized, Gtk.Gesture_Pan.Gtk_Gesture_Pan
--  will attempt to determine as early as possible whether the sequence is
--  moving in the expected direction, and denying the sequence if this does not
--  happen.
--
--  Once a panning gesture along the expected axis is recognized, the
--  Gtk.Gesture_Pan.Gtk_Gesture_Pan::pan signal will be emitted as input events
--  are received, containing the offset in the given axis.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Gesture_Drag; use Gtk.Gesture_Drag;
with Gtk.Widget;       use Gtk.Widget;

package Gtk.Gesture_Pan is

   type Gtk_Gesture_Pan_Record is new Gtk_Gesture_Drag_Record with null record;
   type Gtk_Gesture_Pan is access all Gtk_Gesture_Pan_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self        : out Gtk_Gesture_Pan;
       Widget      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation);
   procedure Initialize
      (Self        : not null access Gtk_Gesture_Pan_Record'Class;
       Widget      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation);
   --  Returns a newly created Gtk.Gesture.Gtk_Gesture that recognizes pan
   --  gestures.
   --  Since: gtk+ 3.14
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "widget": a Gtk.Widget.Gtk_Widget
   --  "orientation": expected orientation

   function Gtk_Gesture_Pan_New
      (Widget      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation) return Gtk_Gesture_Pan;
   --  Returns a newly created Gtk.Gesture.Gtk_Gesture that recognizes pan
   --  gestures.
   --  Since: gtk+ 3.14
   --  "widget": a Gtk.Widget.Gtk_Widget
   --  "orientation": expected orientation

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_gesture_pan_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Orientation
      (Self : not null access Gtk_Gesture_Pan_Record)
       return Gtk.Enums.Gtk_Orientation;
   --  Returns the orientation of the pan gestures that this Gesture expects.
   --  Since: gtk+ 3.14

   procedure Set_Orientation
      (Self        : not null access Gtk_Gesture_Pan_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);
   --  Sets the orientation to be expected on pan gestures.
   --  Since: gtk+ 3.14
   --  "orientation": expected orientation

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Orientation_Property : constant Gtk.Enums.Property_Gtk_Orientation;
   --  The expected orientation of pan gestures.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Gesture_Pan_Gtk_Pan_Direction_Gdouble_Void is not null access procedure
     (Self      : access Gtk_Gesture_Pan_Record'Class;
      Direction : Gtk.Enums.Gtk_Pan_Direction;
      Offset    : Gdouble);

   type Cb_GObject_Gtk_Pan_Direction_Gdouble_Void is not null access procedure
     (Self      : access Glib.Object.GObject_Record'Class;
      Direction : Gtk.Enums.Gtk_Pan_Direction;
      Offset    : Gdouble);

   Signal_Pan : constant Glib.Signal_Name := "pan";
   procedure On_Pan
      (Self  : not null access Gtk_Gesture_Pan_Record;
       Call  : Cb_Gtk_Gesture_Pan_Gtk_Pan_Direction_Gdouble_Void;
       After : Boolean := False);
   procedure On_Pan
      (Self  : not null access Gtk_Gesture_Pan_Record;
       Call  : Cb_GObject_Gtk_Pan_Direction_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted once a panning gesture along the expected axis
   --  is detected.
   -- 
   --  Callback parameters:
   --    --  "direction": current direction of the pan gesture
   --    --  "offset": Offset along the gesture orientation

private
   Orientation_Property : constant Gtk.Enums.Property_Gtk_Orientation :=
     Gtk.Enums.Build ("orientation");
end Gtk.Gesture_Pan;
