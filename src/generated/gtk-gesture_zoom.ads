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
--  Gtk.Gesture_Zoom.Gtk_Gesture_Zoom is a Gtk.Gesture.Gtk_Gesture
--  implementation able to recognize pinch/zoom gestures, whenever the distance
--  between both tracked sequences changes, the
--  Gtk.Gesture_Zoom.Gtk_Gesture_Zoom::scale-changed signal is emitted to
--  report the scale factor.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;        use Glib;
with Glib.Object; use Glib.Object;
with Gtk.Gesture; use Gtk.Gesture;
with Gtk.Widget;  use Gtk.Widget;

package Gtk.Gesture_Zoom is

   type Gtk_Gesture_Zoom_Record is new Gtk_Gesture_Record with null record;
   type Gtk_Gesture_Zoom is access all Gtk_Gesture_Zoom_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gtk_Gesture_Zoom;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Initialize
      (Self   : not null access Gtk_Gesture_Zoom_Record'Class;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Returns a newly created Gtk.Gesture.Gtk_Gesture that recognizes zoom
   --  in/out gestures (usually known as pinch/zoom).
   --  Since: gtk+ 3.14
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "widget": a Gtk.Widget.Gtk_Widget

   function Gtk_Gesture_Zoom_New
      (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk_Gesture_Zoom;
   --  Returns a newly created Gtk.Gesture.Gtk_Gesture that recognizes zoom
   --  in/out gestures (usually known as pinch/zoom).
   --  Since: gtk+ 3.14
   --  "widget": a Gtk.Widget.Gtk_Widget

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_gesture_zoom_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Scale_Delta
      (Self : not null access Gtk_Gesture_Zoom_Record) return Gdouble;
   --  If Gesture is active, this function returns the zooming difference
   --  since the gesture was recognized (hence the starting point is considered
   --  1:1). If Gesture is not active, 1 is returned.
   --  Since: gtk+ 3.14

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Gesture_Zoom_Gdouble_Void is not null access procedure
     (Self  : access Gtk_Gesture_Zoom_Record'Class;
      Scale : Gdouble);

   type Cb_GObject_Gdouble_Void is not null access procedure
     (Self  : access Glib.Object.GObject_Record'Class;
      Scale : Gdouble);

   Signal_Scale_Changed : constant Glib.Signal_Name := "scale-changed";
   procedure On_Scale_Changed
      (Self  : not null access Gtk_Gesture_Zoom_Record;
       Call  : Cb_Gtk_Gesture_Zoom_Gdouble_Void;
       After : Boolean := False);
   procedure On_Scale_Changed
      (Self  : not null access Gtk_Gesture_Zoom_Record;
       Call  : Cb_GObject_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted whenever the distance between both tracked
   --  sequences changes.

end Gtk.Gesture_Zoom;
