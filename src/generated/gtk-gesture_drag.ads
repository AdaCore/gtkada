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
--  Gtk.Gesture_Drag.Gtk_Gesture_Drag is a Gtk.Gesture.Gtk_Gesture
--  implementation that recognizes drag operations. The drag operation itself
--  can be tracked throught the Gtk.Gesture_Drag.Gtk_Gesture_Drag::drag-begin,
--  Gtk.Gesture_Drag.Gtk_Gesture_Drag::drag-update and
--  Gtk.Gesture_Drag.Gtk_Gesture_Drag::drag-end signals, or the relevant
--  coordinates be extracted through Gtk.Gesture_Drag.Get_Offset and
--  Gtk.Gesture_Drag.Get_Start_Point.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Gtk.Gesture_Single; use Gtk.Gesture_Single;
with Gtk.Widget;         use Gtk.Widget;

package Gtk.Gesture_Drag is

   type Gtk_Gesture_Drag_Record is new Gtk_Gesture_Single_Record with null record;
   type Gtk_Gesture_Drag is access all Gtk_Gesture_Drag_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gtk_Gesture_Drag;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Initialize
      (Self   : not null access Gtk_Gesture_Drag_Record'Class;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Returns a newly created Gtk.Gesture.Gtk_Gesture that recognizes drags.
   --  Since: gtk+ 3.14
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "widget": a Gtk.Widget.Gtk_Widget

   function Gtk_Gesture_Drag_New
      (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk_Gesture_Drag;
   --  Returns a newly created Gtk.Gesture.Gtk_Gesture that recognizes drags.
   --  Since: gtk+ 3.14
   --  "widget": a Gtk.Widget.Gtk_Widget

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_gesture_drag_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Offset
      (Self : not null access Gtk_Gesture_Drag_Record;
       X    : access Gdouble;
       Y    : access Gdouble) return Boolean;
   --  If the Gesture is active, this function returns True and fills in X and
   --  Y with the coordinates of the current point, as an offset to the
   --  starting drag point.
   --  Since: gtk+ 3.14
   --  "x": X offset for the current point
   --  "y": Y offset for the current point

   function Get_Start_Point
      (Self : not null access Gtk_Gesture_Drag_Record;
       X    : access Gdouble;
       Y    : access Gdouble) return Boolean;
   --  If the Gesture is active, this function returns True and fills in X and
   --  Y with the drag start coordinates, in window-relative coordinates.
   --  Since: gtk+ 3.14
   --  "x": X coordinate for the drag start point
   --  "y": Y coordinate for the drag start point

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Gesture_Drag_Gdouble_Gdouble_Void is not null access procedure
     (Self    : access Gtk_Gesture_Drag_Record'Class;
      Start_X : Gdouble;
      Start_Y : Gdouble);

   type Cb_GObject_Gdouble_Gdouble_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      Start_X : Gdouble;
      Start_Y : Gdouble);

   Signal_Drag_Begin : constant Glib.Signal_Name := "drag-begin";
   procedure On_Drag_Begin
      (Self  : not null access Gtk_Gesture_Drag_Record;
       Call  : Cb_Gtk_Gesture_Drag_Gdouble_Gdouble_Void;
       After : Boolean := False);
   procedure On_Drag_Begin
      (Self  : not null access Gtk_Gesture_Drag_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted whenever dragging starts.
   -- 
   --  Callback parameters:
   --    --  "start_x": X coordinate, relative to the widget allocation
   --    --  "start_y": Y coordinate, relative to the widget allocation

   Signal_Drag_End : constant Glib.Signal_Name := "drag-end";
   procedure On_Drag_End
      (Self  : not null access Gtk_Gesture_Drag_Record;
       Call  : Cb_Gtk_Gesture_Drag_Gdouble_Gdouble_Void;
       After : Boolean := False);
   procedure On_Drag_End
      (Self  : not null access Gtk_Gesture_Drag_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted whenever the dragging is finished.
   -- 
   --  Callback parameters:
   --    --  "offset_x": X offset, relative to the start point
   --    --  "offset_y": Y offset, relative to the start point

   Signal_Drag_Update : constant Glib.Signal_Name := "drag-update";
   procedure On_Drag_Update
      (Self  : not null access Gtk_Gesture_Drag_Record;
       Call  : Cb_Gtk_Gesture_Drag_Gdouble_Gdouble_Void;
       After : Boolean := False);
   procedure On_Drag_Update
      (Self  : not null access Gtk_Gesture_Drag_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted whenever the dragging point moves.
   -- 
   --  Callback parameters:
   --    --  "offset_x": X offset, relative to the start point
   --    --  "offset_y": Y offset, relative to the start point

end Gtk.Gesture_Drag;
