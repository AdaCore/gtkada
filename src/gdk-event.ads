------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2026, AdaCore                          --
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

private with Ada.Finalization;
with System;

with Gdk.Device;
with Gdk.Device_Tool;
with Gdk.Display;
limited with Gdk.Event.Button_Event;
limited with Gdk.Event.Crossing_Event;
limited with Gdk.Event.Delete_Event;
limited with Gdk.Event.DND_Event;
limited with Gdk.Event.Focus_Event;
limited with Gdk.Event.Grab_Broken_Event;
limited with Gdk.Event.Key_Event;
limited with Gdk.Event.Motion_Event;
limited with Gdk.Event.Pad_Event;
limited with Gdk.Event.Proximity_Event;
limited with Gdk.Event.Scroll_Event;
limited with Gdk.Event.Touch_Event;
limited with Gdk.Event.Touchpad_Event;
with Gdk.Surface;
with Glib;

package Gdk.Event is

   type Gdk_Event is tagged private;
   --  Opaque reference-counted wrapper around GdkEvent*.

   type Gdk_Event_Type is
     (Delete,
      Motion_Notify,
      Button_Press,
      Button_Release,
      Key_Press,
      Key_Release,
      Enter_Notify,
      Leave_Notify,
      Focus_Change,
      Proximity_In,
      Proximity_Out,
      Drag_Enter,
      Drag_Leave,
      Drag_Motion,
      Drop_Start,
      Scroll,
      Grab_Broken,
      Touch_Begin,
      Touch_Update,
      Touch_End,
      Touch_Cancel,
      Touchpad_Swipe,
      Touchpad_Pinch,
      Pad_Button_Press,
      Pad_Button_Release,
      Pad_Ring,
      Pad_Strip,
      Pad_Group_Mode,
      Touchpad_Hold,
      Pad_Dial,
      Event_Last);
   pragma Convention (C, Gdk_Event_Type);
   --  Specifies the type of the event.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_event_get_type");
   --  Returns the GType for GdkEvent.
   --
   --  @return the GType for GdkEvent.

   function From_Object (Object : System.Address) return Gdk_Event;
   --  Create an Ada handle from a raw C pointer.
   --  Always acquires a reference with gdk_event_ref.
   --
   --  @param Object raw GdkEvent* pointer.
   --  @return a reference-counted Gdk_Event handle.

   function Get_Object (Self : Gdk_Event'Class) return System.Address;
   pragma Inline (Get_Object);
   --  Returns the underlying GdkEvent* pointer.
   --
   --  @param Self a Gdk_Event wrapper.
   --  @return the wrapped GdkEvent* pointer.

   procedure Set_Object
     (Self   : in out Gdk_Event'Class;
      Object : System.Address);
   --  Assign a raw C pointer to Self.
   --  Releases any previous pointer in Self.
   --  Always acquires a reference on Object.
   --
   --  @param Self destination wrapper that receives the new pointer.
   --  @param Object raw GdkEvent* pointer to assign.

   function Is_Null (Self : Gdk_Event'Class) return Boolean;
   pragma Inline (Is_Null);
   --  Returns True if Self does not wrap a GdkEvent*.
   --
   --  @param Self a Gdk_Event wrapper.
   --  @return True if the wrapped pointer is null.

   function As_Button_Event
     (Self : Gdk_Event'Class) return Gdk.Event.Button_Event.Gdk_Button_Event;
   function As_Crossing_Event
     (Self : Gdk_Event'Class) return Gdk.Event.Crossing_Event.Gdk_Crossing_Event;
   function As_Delete_Event
     (Self : Gdk_Event'Class) return Gdk.Event.Delete_Event.Gdk_Delete_Event;
   function As_DND_Event
     (Self : Gdk_Event'Class) return Gdk.Event.DND_Event.Gdk_DND_Event;
   function As_Focus_Event
     (Self : Gdk_Event'Class) return Gdk.Event.Focus_Event.Gdk_Focus_Event;
   function As_Grab_Broken_Event
     (Self : Gdk_Event'Class)
      return Gdk.Event.Grab_Broken_Event.Gdk_Grab_Broken_Event;
   function As_Key_Event
     (Self : Gdk_Event'Class) return Gdk.Event.Key_Event.Gdk_Key_Event;
   function As_Motion_Event
     (Self : Gdk_Event'Class) return Gdk.Event.Motion_Event.Gdk_Motion_Event;
   function As_Pad_Event
     (Self : Gdk_Event'Class) return Gdk.Event.Pad_Event.Gdk_Pad_Event;
   function As_Proximity_Event
     (Self : Gdk_Event'Class)
      return Gdk.Event.Proximity_Event.Gdk_Proximity_Event;
   function As_Scroll_Event
     (Self : Gdk_Event'Class) return Gdk.Event.Scroll_Event.Gdk_Scroll_Event;
   function As_Touch_Event
     (Self : Gdk_Event'Class) return Gdk.Event.Touch_Event.Gdk_Touch_Event;
   function As_Touchpad_Event
     (Self : Gdk_Event'Class) return Gdk.Event.Touchpad_Event.Gdk_Touchpad_Event;

   function Is_Button_Event (Self : Gdk_Event'Class) return Boolean;
   function Is_Crossing_Event (Self : Gdk_Event'Class) return Boolean;
   function Is_Delete_Event (Self : Gdk_Event'Class) return Boolean;
   function Is_DND_Event (Self : Gdk_Event'Class) return Boolean;
   function Is_Focus_Event (Self : Gdk_Event'Class) return Boolean;
   function Is_Grab_Broken_Event (Self : Gdk_Event'Class) return Boolean;
   function Is_Key_Event (Self : Gdk_Event'Class) return Boolean;
   function Is_Motion_Event (Self : Gdk_Event'Class) return Boolean;
   function Is_Pad_Event (Self : Gdk_Event'Class) return Boolean;
   function Is_Proximity_Event (Self : Gdk_Event'Class) return Boolean;
   function Is_Scroll_Event (Self : Gdk_Event'Class) return Boolean;
   function Is_Touch_Event (Self : Gdk_Event'Class) return Boolean;
   function Is_Touchpad_Event (Self : Gdk_Event'Class) return Boolean;

   function Get_Angle
     (Event_1 : Gdk_Event'Class;
      Event_2 : Gdk_Event'Class;
      Angle   : out Glib.Gdouble) return Boolean;
   --  Returns the relative angle from @event1 to @event2.
   --
   --  The relative angle is the angle between the X axis and the line
   --  through both events' positions. The rotation direction for positive
   --  angles is from the positive X axis towards the positive Y axis.
   --
   --  This assumes that both events have X/Y information.
   --  If not, this function returns %FALSE.
   --
   --  @param Event_1 first `GdkEvent`.
   --  @param Event_2 second `GdkEvent`.
   --  @param Angle
   --    return location for the relative angle between both events.
   --  @return %TRUE if the angle could be calculated.

   function Get_Center
     (Event_1 : Gdk_Event'Class;
      Event_2 : Gdk_Event'Class;
      X       : out Glib.Gdouble;
      Y       : out Glib.Gdouble) return Boolean;
   --  Returns the point halfway between the events' positions.
   --
   --  This assumes that both events have X/Y information.
   --  If not, this function returns %FALSE.
   --
   --  @param Event_1 first `GdkEvent`.
   --  @param Event_2 second `GdkEvent`.
   --  @param X return location for the X coordinate of the center.
   --  @param Y return location for the Y coordinate of the center.
   --  @return %TRUE if the center could be calculated.

   function Get_Distance
     (Event_1   : Gdk_Event'Class;
      Event_2   : Gdk_Event'Class;
      Distance  : out Glib.Gdouble) return Boolean;
   --  Returns the distance between the event locations.
   --
   --  This assumes that both events have X/Y information.
   --  If not, this function returns %FALSE.
   --
   --  @param Event_1 first `GdkEvent`.
   --  @param Event_2 second `GdkEvent`.
   --  @param Distance return location for the distance.
   --  @return %TRUE if the distance could be calculated.

   --  function Get_Axes
   --    (Self   : Gdk_Event'Class;
   --    Axes   : out System.Address;
   --    N_Axes : out Glib.Guint) return Boolean;
   --  Extracts all axis values from an event.
   --
   --  To find out which axes are used, use [method@Gdk.DeviceTool.get_axes]
   --  on the device tool returned by [method@Gdk.Event.get_device_tool].
   --
   --  @param Self a `GdkEvent`.
   --  @param Axes the array of values for all axes.
   --  @param N_Axes the length of array.
   --  @return %TRUE on success, otherwise %FALSE.

   function Get_Axis
     (Self     : Gdk_Event'Class;
      Axis_Use : Glib.Gint;
      Value    : out Glib.Gdouble) return Boolean;
   --  Extract the axis value for a particular axis use from
   --  an event structure.
   --
   --  To find out which axes are used, use [method@Gdk.DeviceTool.get_axes]
   --  on the device tool returned by [method@Gdk.Event.get_device_tool].
   --
   --  @param Self a `GdkEvent`.
   --  @param Axis_Use the axis use to look for.
   --  @param Value location to store the value found.
   --  @return %TRUE if the specified axis was found, otherwise %FALSE.

   function Get_Device (Self : Gdk_Event'Class) return Gdk.Device.Gdk_Device;
   --  Returns the device of an event.
   --
   --  @param Self a `GdkEvent`.
   --  @return a `GdkDevice`.

   function Get_Device_Tool
     (Self : Gdk_Event'Class) return Gdk.Device_Tool.Gdk_Device_Tool;
   --  Returns a `GdkDeviceTool` representing the tool that
   --  caused the event.
   --
   --  If the was not generated by a device that supports
   --  different tools (such as a tablet), this function will
   --  return %NULL.
   --
   --  Note: the `GdkDeviceTool` will be constant during
   --  the application lifetime, if settings must be stored
   --  persistently across runs, see [method@Gdk.DeviceTool.get_serial].
   --
   --  @param Self a `GdkEvent`.
   --  @return The current device tool.

   function Get_Display (Self : Gdk_Event'Class) return Gdk.Display.Gdk_Display;
   --  Retrieves the display associated to the @event.
   --
   --  @param Self a `GdkEvent`.
   --  @return a `GdkDisplay`.

   --  function Get_Event_Sequence (Self : Gdk_Event'Class) return System.Address;
   --  Returns the event sequence to which the event belongs.
   --
   --  Related touch events are connected in a sequence. Other
   --  events typically don't have event sequence information.
   --
   --  @param Self a `GdkEvent`.
   --  @return the event sequence that the event belongs to.

   function Get_Event_Type (Self : Gdk_Event'Class) return Gdk_Event_Type;
   --  Retrieves the type of the event.
   --
   --  @param Self a `GdkEvent`.
   --  @return a `GdkEvent`Type.

   --  function Get_History
   --    (Self     : Gdk_Event'Class;
   --     N_Coords : out Glib.Guint) return System.Address;
   --  Retrieves the history of the device that @event is for, as a list of
   --  time and coordinates.
   --
   --  The history includes positions that are not delivered as separate events
   --  to the application because they occurred in the same frame as @event.
   --
   --  Note that only motion and scroll events record history, and motion
   --  events do it only if one of the mouse buttons is down, or the device
   --  has a tool.
   --
   --  @param Self a motion or scroll event.
   --  @param N_Coords
   --    Return location for the length of the returned array.
   --  @return an array of time and coordinates.

   --  function Get_Modifier_State (Self : Gdk_Event'Class) return Glib.Guint;
   --  Returns the modifier state field of an event.
   --
   --  @param Self a `GdkEvent`.
   --  @return
   --  the modifier state of @event.

   function Get_Pointer_Emulated (Self : Gdk_Event'Class) return Boolean;
   --  Returns whether this event is an 'emulated' pointer event.
   --
   --  Emulated pointer events typically originate from a touch events.
   --
   --  @param Self a `GdkEvent`.
   --  @return %TRUE if this event is emulated.

   function Get_Position
     (Self : Gdk_Event'Class;
      X    : out Glib.Gdouble;
      Y    : out Glib.Gdouble) return Boolean;
   --  Extract the event surface relative x/y coordinates from an event.
   --
   --  This position is in [surface coordinates](coordinates.html).
   --
   --  @param Self a `GdkEvent`.
   --  @param X location to put event surface x coordinate.
   --  @param Y location to put event surface y coordinate.
   --  @return whether the positions were set.

   function Get_Seat (Self : Gdk_Event'Class) return Gdk.Gdk_Seat;
   --  Returns the seat that originated the event.
   --
   --  @param Self a `GdkEvent`.
   --  @return a `GdkSeat`.

   function Get_Surface (Self : Gdk_Event'Class) return Gdk.Surface.Gdk_Surface;
   --  Extracts the surface associated with an event.
   --
   --  @param Self a `GdkEvent`.
   --  @return The `GdkSurface` associated with the event.

   function Get_Time (Self : Gdk_Event'Class) return Glib.Guint32;
   --  Returns the timestamp of @event.
   --
   --  Not all events have timestamps. In that case, this function
   --  returns %GDK_CURRENT_TIME.
   --
   --  @param Self a `GdkEvent`.
   --  @return
   --  timestamp field from @event.

   function Triggers_Context_Menu (Self : Gdk_Event'Class) return Boolean;
   --  Returns whether a `GdkEvent` should trigger a context menu,
   --  according to platform conventions.
   --
   --  The right mouse button typically triggers context menus.
   --  On macOS, Control+left mouse button also triggers.
   --
   --  This function should always be used instead of simply checking for
   --
   --  ```c
   --  event-&gt;button == GDK_BUTTON_SECONDARY
   --  ```
   --
   --  @param Self
   --  a `GdkEvent`, currently only button events are meaningful values.
   --  @return %TRUE if the event should trigger a context menu.

private

   type Gdk_Event is new Ada.Finalization.Controlled with record
      Data : System.Address := System.Null_Address;
   end record;

   overriding procedure Adjust (Self : in out Gdk_Event);
   overriding procedure Finalize (Self : in out Gdk_Event);

end Gdk.Event;
