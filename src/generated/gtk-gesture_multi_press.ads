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
--  Gtk.Gesture_Multi_Press.Gtk_Gesture_Multi_Press is a
--  Gtk.Gesture.Gtk_Gesture implementation able to recognize multiple clicks on
--  a nearby zone, which can be listened for through the
--  Gtk.Gesture_Multi_Press.Gtk_Gesture_Multi_Press::pressed signal. Whenever
--  time or distance between clicks exceed the GTK+ defaults,
--  Gtk.Gesture_Multi_Press.Gtk_Gesture_Multi_Press::stopped is emitted, and
--  the click counter is reset.
--
--  Callers may also restrict the area that is considered valid for a >1
--  touch/button press through Gtk.Gesture_Multi_Press.Set_Area, so any click
--  happening outside that area is considered to be a first click of its own.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Rectangle;      use Gdk.Rectangle;
with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Gtk.Gesture_Single; use Gtk.Gesture_Single;
with Gtk.Widget;         use Gtk.Widget;

package Gtk.Gesture_Multi_Press is

   type Gtk_Gesture_Multi_Press_Record is new Gtk_Gesture_Single_Record with null record;
   type Gtk_Gesture_Multi_Press is access all Gtk_Gesture_Multi_Press_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gtk_Gesture_Multi_Press;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Initialize
      (Self   : not null access Gtk_Gesture_Multi_Press_Record'Class;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Returns a newly created Gtk.Gesture.Gtk_Gesture that recognizes single
   --  and multiple presses.
   --  Since: gtk+ 3.14
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "widget": a Gtk.Widget.Gtk_Widget

   function Gtk_Gesture_Multi_Press_New
      (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk_Gesture_Multi_Press;
   --  Returns a newly created Gtk.Gesture.Gtk_Gesture that recognizes single
   --  and multiple presses.
   --  Since: gtk+ 3.14
   --  "widget": a Gtk.Widget.Gtk_Widget

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_gesture_multi_press_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Area
      (Self : not null access Gtk_Gesture_Multi_Press_Record;
       Rect : access Gdk.Rectangle.Gdk_Rectangle) return Boolean;
   --  If an area was set through Gtk.Gesture_Multi_Press.Set_Area, this
   --  function will return True and fill in Rect with the press area. See
   --  Gtk.Gesture_Multi_Press.Set_Area for more details on what the press area
   --  represents.
   --  Since: gtk+ 3.14
   --  "rect": return location for the press area

   procedure Set_Area
      (Self : not null access Gtk_Gesture_Multi_Press_Record;
       Rect : Gdk.Rectangle.Gdk_Rectangle);
   --  If Rect is non-null, the press area will be checked to be confined
   --  within the rectangle, otherwise the button count will be reset so the
   --  press is seen as being the first one. If Rect is null, the area will be
   --  reset to an unrestricted state.
   --  Note: The rectangle is only used to determine whether any non-first
   --  click falls within the expected area. This is not akin to an input
   --  shape.
   --  Since: gtk+ 3.14
   --  "rect": rectangle to receive coordinates on

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Gesture_Multi_Press_Gint_Gdouble_Gdouble_Void is not null access procedure
     (Self    : access Gtk_Gesture_Multi_Press_Record'Class;
      N_Press : Glib.Gint;
      X       : Gdouble;
      Y       : Gdouble);

   type Cb_GObject_Gint_Gdouble_Gdouble_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      N_Press : Glib.Gint;
      X       : Gdouble;
      Y       : Gdouble);

   Signal_Pressed : constant Glib.Signal_Name := "pressed";
   procedure On_Pressed
      (Self  : not null access Gtk_Gesture_Multi_Press_Record;
       Call  : Cb_Gtk_Gesture_Multi_Press_Gint_Gdouble_Gdouble_Void;
       After : Boolean := False);
   procedure On_Pressed
      (Self  : not null access Gtk_Gesture_Multi_Press_Record;
       Call  : Cb_GObject_Gint_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted whenever a button or touch press happens.
   -- 
   --  Callback parameters:
   --    --  "n_press": how many touch/button presses happened with this one
   --    --  "x": The X coordinate, in widget allocation coordinates
   --    --  "y": The Y coordinate, in widget allocation coordinates

   Signal_Released : constant Glib.Signal_Name := "released";
   procedure On_Released
      (Self  : not null access Gtk_Gesture_Multi_Press_Record;
       Call  : Cb_Gtk_Gesture_Multi_Press_Gint_Gdouble_Gdouble_Void;
       After : Boolean := False);
   procedure On_Released
      (Self  : not null access Gtk_Gesture_Multi_Press_Record;
       Call  : Cb_GObject_Gint_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when a button or touch is released. N_Press will
   --  report the number of press that is paired to this event, note that
   --  Gtk.Gesture_Multi_Press.Gtk_Gesture_Multi_Press::stopped may have been
   --  emitted between the press and its release, N_Press will only start over
   --  at the next press.
   -- 
   --  Callback parameters:
   --    --  "n_press": number of press that is paired with this release
   --    --  "x": The X coordinate, in widget allocation coordinates
   --    --  "y": The Y coordinate, in widget allocation coordinates

   type Cb_Gtk_Gesture_Multi_Press_Void is not null access procedure
     (Self : access Gtk_Gesture_Multi_Press_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Stopped : constant Glib.Signal_Name := "stopped";
   procedure On_Stopped
      (Self  : not null access Gtk_Gesture_Multi_Press_Record;
       Call  : Cb_Gtk_Gesture_Multi_Press_Void;
       After : Boolean := False);
   procedure On_Stopped
      (Self  : not null access Gtk_Gesture_Multi_Press_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted whenever any time/distance threshold has been
   --  exceeded.

end Gtk.Gesture_Multi_Press;
