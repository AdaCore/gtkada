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

--  Recognizes drag gestures.
--
--  The drag operation itself can be tracked throughout the
--  [signalGtk.GestureDrag::drag-begin], [signalGtk.GestureDrag::drag-update]
--  and [signalGtk.GestureDrag::drag-end] signals, and the relevant coordinates
--  can be extracted through [methodGtk.GestureDrag.get_offset] and
--  [methodGtk.GestureDrag.get_start_point].

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Gtk.Gesture_Single; use Gtk.Gesture_Single;

package Gtk.Gesture_Drag is

   type Gtk_Gesture_Drag_Record is new Gtk_Gesture_Single_Record with null record;
   type Gtk_Gesture_Drag is access all Gtk_Gesture_Drag_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Gesture_Drag);
   procedure Initialize
      (Self : not null access Gtk_Gesture_Drag_Record'Class);
   --  Returns a newly created `GtkGesture` that recognizes drags.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Gesture_Drag_New return Gtk_Gesture_Drag;
   --  Returns a newly created `GtkGesture` that recognizes drags.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_gesture_drag_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Offset
      (Self : not null access Gtk_Gesture_Drag_Record;
       X    : access Gdouble := null;
       Y    : access Gdouble := null) return Boolean;
   --  Gets the offset from the start point.
   --  If the Gesture is active, this function returns True and fills in X and
   --  Y with the coordinates of the current point, as an offset to the
   --  starting drag point.
   --  @param X X offset for the current point
   --  @param Y Y offset for the current point
   --  @return True if the gesture is active

   function Get_Start_Point
      (Self : not null access Gtk_Gesture_Drag_Record;
       X    : access Gdouble := null;
       Y    : access Gdouble := null) return Boolean;
   --  Gets the point where the drag started.
   --  If the Gesture is active, this function returns True and fills in X and
   --  Y with the drag start coordinates, in widget-relative coordinates.
   --  @param X X coordinate for the drag start point
   --  @param Y Y coordinate for the drag start point
   --  @return True if the gesture is active

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
   --  Emitted whenever dragging starts.
   -- 
   --  Callback parameters:
   --    --  @param Start_X X coordinate, relative to the widget allocation
   --    --  @param Start_Y Y coordinate, relative to the widget allocation

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
   --  Emitted whenever the dragging is finished.
   -- 
   --  Callback parameters:
   --    --  @param Offset_X X offset, relative to the start point
   --    --  @param Offset_Y Y offset, relative to the start point

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
   --  Emitted whenever the dragging point moves.
   -- 
   --  Callback parameters:
   --    --  @param Offset_X X offset, relative to the start point
   --    --  @param Offset_Y Y offset, relative to the start point

end Gtk.Gesture_Drag;
