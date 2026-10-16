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

--  Recognizes tablet stylus input.
--
--  The provided signals just relay the basic information of the stylus
--  events.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Device;         use Gdk.Device;
with Gdk.Device_Tool;    use Gdk.Device_Tool;
with Gdk.Enums;          use Gdk.Enums;
with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Glib.Properties;    use Glib.Properties;
with Gtk.Gesture_Single; use Gtk.Gesture_Single;

package Gtk.Gesture_Stylus is

   type Gtk_Gesture_Stylus_Record is new Gtk_Gesture_Single_Record with null record;
   type Gtk_Gesture_Stylus is access all Gtk_Gesture_Stylus_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Gesture_Stylus);
   procedure Initialize
      (Self : not null access Gtk_Gesture_Stylus_Record'Class);
   --  Creates a new `GtkGestureStylus`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Gesture_Stylus_New return Gtk_Gesture_Stylus;
   --  Creates a new `GtkGestureStylus`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_gesture_stylus_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Axis
      (Self  : not null access Gtk_Gesture_Stylus_Record;
       Axis  : Gdk.Enums.Gdk_Axis_Use;
       Value : out Gdouble) return Boolean;
   --  Returns the current value for the requested Axis.
   --  This function must be called from the handler of one of the
   --  [signalGtk.GestureStylus::down], [signalGtk.GestureStylus::motion],
   --  [signalGtk.GestureStylus::up] or [signalGtk.GestureStylus::proximity]
   --  signals.
   --  @param Axis requested device axis
   --  @param Value return location for the axis value
   --  @return True if there is a current value for the axis

   function Get_Device_Tool
      (Self : not null access Gtk_Gesture_Stylus_Record)
       return Gdk.Device_Tool.Gdk_Device_Tool;
   --  Returns the `GdkDeviceTool` currently driving input through this
   --  gesture.
   --  This function must be called from the handler of one of the
   --  [signalGtk.GestureStylus::down], [signalGtk.GestureStylus::motion],
   --  [signalGtk.GestureStylus::up] or [signalGtk.GestureStylus::proximity]
   --  signals.
   --  @return The current stylus tool. Has transfer-ownership='none'.

   function Get_Stylus_Only
      (Self : not null access Gtk_Gesture_Stylus_Record) return Boolean;
   --  Checks whether the gesture is for styluses only.
   --  Stylus-only gestures will signal events exclusively from stylus input
   --  devices.
   --  Since: gtk+ 4.10
   --  @return True if the gesture is only for stylus events

   procedure Set_Stylus_Only
      (Self        : not null access Gtk_Gesture_Stylus_Record;
       Stylus_Only : Boolean);
   --  Sets the state of stylus-only
   --  If true, the gesture will exclusively handle events from stylus input
   --  devices, otherwise it'll handle events from any pointing device.
   --  Since: gtk+ 4.10
   --  @param Stylus_Only whether the gesture is used exclusively for stylus
   --  events

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Get_Axes
     (Self   : not null access Gtk_Gesture_Stylus_Record;
      Axes   : Gdk_Axis_Use_Array;
      Values : out Gdouble_Array) return Boolean
   with Pre =>
   Axes'Length > 1
   and then Axes (Axes'Last) = Gdk.Enums.Gdk_Axis_Ignore
   and then (for all I in Axes'Range =>
      I = Axes'Last
      or else Axes (I) /= Gdk.Enums.Gdk_Axis_Ignore)
   and then Values'Length = Axes'Length - 1;
   --  Returns the current values for the requested Axes.
   --  This function must be called from the handler of one of the
   --  [signalGtk.GestureStylus::down], [signalGtk.GestureStylus::motion],
   --  [signalGtk.GestureStylus::up] or [signalGtk.GestureStylus::proximity]
   --  signals.
   --  @param Axes array of requested axes, terminated with
   --  Gdk.Enums.Gdk_Axis_Ignore
   --  @param Values return location for the axis values
   --  @return True if there is a current value for the axes

   function Get_Backlog
     (Self    : not null access Gtk_Gesture_Stylus_Record;
      Backlog : out Gdk.Device.Gdk_Time_Coord_Array_Access)
   return Boolean;
   --  Returns the accumulated backlog of tracking information.
   --  By default, GTK will limit rate of input events. On stylus input where
   --  accuracy of strokes is paramount, this function returns the accumulated
   --  coordinate/timing state before the emission of the current
   --  [Gtk.GestureStylus::motion] signal.
   --  This function may only be called within a
   --  [signalGtk.GestureStylus::motion] signal handler, the state given in
   --  this signal and obtainable through [methodGtk.GestureStylus.get_axis]
   --  express the latest (most up-to-date) state in motion history.
   --  The Backlog is provided in chronological order.
   --  @param Backlog coordinates and times for the backlog events.
   --  Should be freed by caller.
   --  @return True if there is a backlog to unfold in the current state.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Stylus_Only_Property : constant Glib.Properties.Property_Boolean;
   --  If this gesture should exclusively react to stylus input devices.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void is not null access procedure
     (Self : access Gtk_Gesture_Stylus_Record'Class;
      X    : Gdouble;
      Y    : Gdouble);

   type Cb_GObject_Gdouble_Gdouble_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      X    : Gdouble;
      Y    : Gdouble);

   Signal_Down : constant Glib.Signal_Name := "down";
   procedure On_Down
      (Self  : not null access Gtk_Gesture_Stylus_Record;
       Call  : Cb_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void;
       After : Boolean := False);
   procedure On_Down
      (Self  : not null access Gtk_Gesture_Stylus_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the stylus touches the device.
   -- 
   --  Callback parameters:
   --    --  @param X the X coordinate of the stylus event
   --    --  @param Y the Y coordinate of the stylus event

   Signal_Motion : constant Glib.Signal_Name := "motion";
   procedure On_Motion
      (Self  : not null access Gtk_Gesture_Stylus_Record;
       Call  : Cb_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void;
       After : Boolean := False);
   procedure On_Motion
      (Self  : not null access Gtk_Gesture_Stylus_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the stylus moves while touching the device.
   -- 
   --  Callback parameters:
   --    --  @param X the X coordinate of the stylus event
   --    --  @param Y the Y coordinate of the stylus event

   Signal_Proximity : constant Glib.Signal_Name := "proximity";
   procedure On_Proximity
      (Self  : not null access Gtk_Gesture_Stylus_Record;
       Call  : Cb_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void;
       After : Boolean := False);
   procedure On_Proximity
      (Self  : not null access Gtk_Gesture_Stylus_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the stylus is in proximity of the device.
   -- 
   --  Callback parameters:
   --    --  @param X the X coordinate of the stylus event
   --    --  @param Y the Y coordinate of the stylus event

   Signal_Up : constant Glib.Signal_Name := "up";
   procedure On_Up
      (Self  : not null access Gtk_Gesture_Stylus_Record;
       Call  : Cb_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void;
       After : Boolean := False);
   procedure On_Up
      (Self  : not null access Gtk_Gesture_Stylus_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the stylus no longer touches the device.
   -- 
   --  Callback parameters:
   --    --  @param X the X coordinate of the stylus event
   --    --  @param Y the Y coordinate of the stylus event

private
   Stylus_Only_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("stylus-only");
end Gtk.Gesture_Stylus;
