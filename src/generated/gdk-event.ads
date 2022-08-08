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
--  A Gdk.Event.Gdk_Event contains a union of all of the event types, and
--  allows access to the data fields in a number of ways.
--
--  The event type is always the first field in all of the event types, and
--  can always be accessed with the following code, no matter what type of
--  event it is: |[<!-- language="C" --> GdkEvent *event; GdkEventType type;
--
--  type = event->type; ]|
--
--  To access other fields of the event, the pointer to the event can be cast
--  to the appropriate event type, or the union member name can be used. For
--  example if the event type is Gdk.Event.Button_Press then the x coordinate
--  of the button press can be accessed with: |[<!-- language="C" --> GdkEvent
--  *event; gdouble x;
--
--  x = ((GdkEventButton*)event)->x; ]| or: |[<!-- language="C" --> GdkEvent
--  *event; gdouble x;
--
--  x = event->button.x; ]|
--
--  </description>
--  <group>Gdk, the low-level API</group>

pragma Warnings (Off, "*is already use-visible*");
with Cairo.Region;            use Cairo.Region;
with Gdk.Device_Tool;         use Gdk.Device_Tool;
with Gdk.Rectangle;           use Gdk.Rectangle;
with Gdk.Types;               use Gdk.Types;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Values;             use Glib.Values;
with Gtkada.Types;            use Gtkada.Types;

package Gdk.Event is

   type Gdk_Event_Type is (
      Nothing,
      Delete,
      Destroy,
      Expose,
      Motion_Notify,
      Button_Press,
      Gdk_2button_Press,
      Gdk_3button_Press,
      Button_Release,
      Key_Press,
      Key_Release,
      Enter_Notify,
      Leave_Notify,
      Focus_Change,
      Configure,
      Map,
      Unmap,
      Property_Notify,
      Selection_Clear,
      Selection_Request,
      Selection_Notify,
      Proximity_In,
      Proximity_Out,
      Drag_Enter,
      Drag_Leave,
      Drag_Motion,
      Drag_Status,
      Drop_Start,
      Drop_Finished,
      Client_Event,
      Visibility_Notify,
      Scroll,
      Window_State,
      Setting,
      Owner_Change,
      Grab_Broken,
      Damage,
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
      Pad_Group_Mode);
   pragma Convention (C, Gdk_Event_Type);
   --  Specifies the type of the event.
   --
   --  Do not confuse these events with the signals that GTK+ widgets emit.
   --  Although many of these events result in corresponding signals being
   --  emitted, the events are often transformed or filtered along the way.
   --
   --  In some language bindings, the values Gdk.Event.Gdk_2button_Press and
   --  Gdk.Event.Gdk_3button_Press would translate into something syntactically
   --  invalid (eg `Gdk.EventType.2ButtonPress`, where a symbol is not allowed
   --  to start with a number). In that case, the aliases
   --  GDK_DOUBLE_BUTTON_PRESS and GDK_TRIPLE_BUTTON_PRESS can be used instead.

   for Gdk_Event_Type use (
      Nothing => -1,
      Delete => 0,
      Destroy => 1,
      Expose => 2,
      Motion_Notify => 3,
      Button_Press => 4,
      Gdk_2button_Press => 5,
      Gdk_3button_Press => 6,
      Button_Release => 7,
      Key_Press => 8,
      Key_Release => 9,
      Enter_Notify => 10,
      Leave_Notify => 11,
      Focus_Change => 12,
      Configure => 13,
      Map => 14,
      Unmap => 15,
      Property_Notify => 16,
      Selection_Clear => 17,
      Selection_Request => 18,
      Selection_Notify => 19,
      Proximity_In => 20,
      Proximity_Out => 21,
      Drag_Enter => 22,
      Drag_Leave => 23,
      Drag_Motion => 24,
      Drag_Status => 25,
      Drop_Start => 26,
      Drop_Finished => 27,
      Client_Event => 28,
      Visibility_Notify => 29,
      Scroll => 31,
      Window_State => 32,
      Setting => 33,
      Owner_Change => 34,
      Grab_Broken => 35,
      Damage => 36,
      Touch_Begin => 37,
      Touch_Update => 38,
      Touch_End => 39,
      Touch_Cancel => 40,
      Touchpad_Swipe => 41,
      Touchpad_Pinch => 42,
      Pad_Button_Press => 43,
      Pad_Button_Release => 44,
      Pad_Ring => 45,
      Pad_Strip => 46,
      Pad_Group_Mode => 47);

   type Gdk_Event_Mask is mod 2 ** Integer'Size;
   pragma Convention (C, Gdk_Event_Mask);
   --  A set of bit-flags to indicate which events a window is to receive.
   --  Most of these masks map onto one or more of the Gdk.Event.Gdk_Event_Type
   --  event types above.
   --
   --  See the [input handling overview][chap-input-handling] for details of
   --  [event masks][event-masks] and [event propagation][event-propagation].
   --
   --  Gdk.Event.Pointer_Motion_Hint_Mask is deprecated. It is a special mask
   --  to reduce the number of Gdk.Event.Motion_Notify events received. When
   --  using Gdk.Event.Pointer_Motion_Hint_Mask, fewer Gdk.Event.Motion_Notify
   --  events will be sent, some of which are marked as a hint (the is_hint
   --  member is True). To receive more motion events after a motion hint
   --  event, the application needs to asks for more, by calling
   --  Gdk.Event.Request_Motions.
   --
   --  Since GTK 3.8, motion events are already compressed by default,
   --  independent of this mechanism. This compression can be disabled with
   --  Gdk.Window.Set_Event_Compression. See the documentation of that function
   --  for details.
   --
   --  If Gdk.Event.Touch_Mask is enabled, the window will receive touch
   --  events from touch-enabled devices. Those will come as sequences of
   --  Gdk.Event.Gdk_Event_Touch with type Gdk.Event.Touch_Update, enclosed by
   --  two events with type Gdk.Event.Touch_Begin and Gdk.Event.Touch_End (or
   --  Gdk.Event.Touch_Cancel). Gdk.Event.Get_Event_Sequence returns the event
   --  sequence for these events, so different sequences may be distinguished.

   Exposure_Mask : constant Gdk_Event_Mask := 2;
   Pointer_Motion_Mask : constant Gdk_Event_Mask := 4;
   Pointer_Motion_Hint_Mask : constant Gdk_Event_Mask := 8;
   Button_Motion_Mask : constant Gdk_Event_Mask := 16;
   Button1_Motion_Mask : constant Gdk_Event_Mask := 32;
   Button2_Motion_Mask : constant Gdk_Event_Mask := 64;
   Button3_Motion_Mask : constant Gdk_Event_Mask := 128;
   Button_Press_Mask : constant Gdk_Event_Mask := 256;
   Button_Release_Mask : constant Gdk_Event_Mask := 512;
   Key_Press_Mask : constant Gdk_Event_Mask := 1024;
   Key_Release_Mask : constant Gdk_Event_Mask := 2048;
   Enter_Notify_Mask : constant Gdk_Event_Mask := 4096;
   Leave_Notify_Mask : constant Gdk_Event_Mask := 8192;
   Focus_Change_Mask : constant Gdk_Event_Mask := 16384;
   Structure_Mask : constant Gdk_Event_Mask := 32768;
   Property_Change_Mask : constant Gdk_Event_Mask := 65536;
   Visibility_Notify_Mask : constant Gdk_Event_Mask := 131072;
   Proximity_In_Mask : constant Gdk_Event_Mask := 262144;
   Proximity_Out_Mask : constant Gdk_Event_Mask := 524288;
   Substructure_Mask : constant Gdk_Event_Mask := 1048576;
   Scroll_Mask : constant Gdk_Event_Mask := 2097152;
   Touch_Mask : constant Gdk_Event_Mask := 4194304;
   Smooth_Scroll_Mask : constant Gdk_Event_Mask := 8388608;
   Touchpad_Gesture_Mask : constant Gdk_Event_Mask := 16777216;
   Tablet_Pad_Mask : constant Gdk_Event_Mask := 33554432;
   All_Events_Mask : constant Gdk_Event_Mask := 67108862;

   type Gdk_Visibility_State is (
      Visibility_Unobscured,
      Visibility_Partial,
      Visibility_Fully_Obscured);
   pragma Convention (C, Gdk_Visibility_State);
   --  Specifies the visiblity status of a window for a
   --  Gdk.Event.Gdk_Event_Visibility.

   type Gdk_Scroll_Direction is (
      Scroll_Up,
      Scroll_Down,
      Scroll_Left,
      Scroll_Right,
      Scroll_Smooth);
   pragma Convention (C, Gdk_Scroll_Direction);
   --  Specifies the direction for Gdk.Event.Gdk_Event_Scroll.

   type Gdk_Notify_Type is (
      Notify_Ancestor,
      Notify_Virtual,
      Notify_Inferior,
      Notify_Nonlinear,
      Notify_Nonlinear_Virtual,
      Notify_Unknown);
   pragma Convention (C, Gdk_Notify_Type);
   --  Specifies the kind of crossing for Gdk.Event.Gdk_Event_Crossing.
   --
   --  See the X11 protocol specification of LeaveNotify for full details of
   --  crossing event generation.

   type Gdk_Crossing_Mode is (
      Crossing_Normal,
      Crossing_Grab,
      Crossing_Ungrab,
      Crossing_Gtk_Grab,
      Crossing_Gtk_Ungrab,
      Crossing_State_Changed,
      Crossing_Touch_Begin,
      Crossing_Touch_End,
      Crossing_Device_Switch);
   pragma Convention (C, Gdk_Crossing_Mode);
   --  Specifies the crossing mode for Gdk.Event.Gdk_Event_Crossing.

   type Gdk_Property_State is (
      Property_New_Value,
      Property_Delete);
   pragma Convention (C, Gdk_Property_State);
   --  Specifies the type of a property change for a
   --  Gdk.Event.Gdk_Event_Property.

   type Gdk_Window_State is mod 2 ** Integer'Size;
   pragma Convention (C, Gdk_Window_State);
   --  Specifies the state of a toplevel window.

   Window_State_Withdrawn : constant Gdk_Window_State := 1;
   Window_State_Iconified : constant Gdk_Window_State := 2;
   Window_State_Maximized : constant Gdk_Window_State := 4;
   Window_State_Sticky : constant Gdk_Window_State := 8;
   Window_State_Fullscreen : constant Gdk_Window_State := 16;
   Window_State_Above : constant Gdk_Window_State := 32;
   Window_State_Below : constant Gdk_Window_State := 64;
   Window_State_Focused : constant Gdk_Window_State := 128;
   Window_State_Tiled : constant Gdk_Window_State := 256;
   Window_State_Top_Tiled : constant Gdk_Window_State := 512;
   Window_State_Top_Resizable : constant Gdk_Window_State := 1024;
   Window_State_Right_Tiled : constant Gdk_Window_State := 2048;
   Window_State_Right_Resizable : constant Gdk_Window_State := 4096;
   Window_State_Bottom_Tiled : constant Gdk_Window_State := 8192;
   Window_State_Bottom_Resizable : constant Gdk_Window_State := 16384;
   Window_State_Left_Tiled : constant Gdk_Window_State := 32768;
   Window_State_Left_Resizable : constant Gdk_Window_State := 65536;

   type Gdk_Setting_Action is (
      Setting_Action_New,
      Setting_Action_Changed,
      Setting_Action_Deleted);
   pragma Convention (C, Gdk_Setting_Action);
   --  Specifies the kind of modification applied to a setting in a
   --  Gdk.Event.Gdk_Event_Setting.

   type Gdk_Owner_Change is (
      Owner_Change_New_Owner,
      Owner_Change_Destroy,
      Owner_Change_Close);
   pragma Convention (C, Gdk_Owner_Change);
   --  Specifies why a selection ownership was changed.

   type Gdk_Event_Sequence is new Glib.C_Proxy;
   function From_Object_Free (B : access Gdk_Event_Sequence) return Gdk_Event_Sequence;
   pragma Inline (From_Object_Free);


   type Gdk_Event_Any is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
   end record;
   pragma Convention (C, Gdk_Event_Any);

   function From_Object_Free (B : access Gdk_Event_Any) return Gdk_Event_Any;
   pragma Inline (From_Object_Free);
   --  Contains the fields which are common to all event structs. Any event
   --  pointer can safely be cast to a pointer to a Gdk.Event.Gdk_Event_Any to
   --  access these fields.

   type Gdk_Event_Button is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Time : Guint32;
      X : Gdouble;
      Y : Gdouble;
      Axes : access Gdouble;
      State : Gdk.Types.Gdk_Modifier_Type;
      Button : Guint;
      Device : System.Address;
      X_Root : Gdouble;
      Y_Root : Gdouble;
   end record;
   pragma Convention (C, Gdk_Event_Button);

   function From_Object_Free (B : access Gdk_Event_Button) return Gdk_Event_Button;
   pragma Inline (From_Object_Free);
   --  Used for button press and button release events. The Type field will be
   --  one of Gdk.Event.Button_Press, Gdk.Event.Gdk_2button_Press,
   --  Gdk.Event.Gdk_3button_Press or Gdk.Event.Button_Release,
   --
   --  Double and triple-clicks result in a sequence of events being received.
   --  For double-clicks the order of events will be:
   --
   --  - Gdk.Event.Button_Press - Gdk.Event.Button_Release -
   --  Gdk.Event.Button_Press - Gdk.Event.Gdk_2button_Press -
   --  Gdk.Event.Button_Release
   --
   --  Note that the first click is received just like a normal button press,
   --  while the second click results in a Gdk.Event.Gdk_2button_Press being
   --  received just after the Gdk.Event.Button_Press.
   --
   --  Triple-clicks are very similar to double-clicks, except that
   --  Gdk.Event.Gdk_3button_Press is inserted after the third click. The order
   --  of the events is:
   --
   --  - Gdk.Event.Button_Press - Gdk.Event.Button_Release -
   --  Gdk.Event.Button_Press - Gdk.Event.Gdk_2button_Press -
   --  Gdk.Event.Button_Release - Gdk.Event.Button_Press -
   --  Gdk.Event.Gdk_3button_Press - Gdk.Event.Button_Release
   --
   --  For a double click to occur, the second button press must occur within
   --  1/4 of a second of the first. For a triple click to occur, the third
   --  button press must also occur within 1/2 second of the first button
   --  press.

   type Gdk_Event_Expose is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Area : Gdk.Rectangle.Gdk_Rectangle;
      Region : Cairo.Region.Cairo_Region;
      Count : Glib.Gint := 0;
   end record;
   pragma Convention (C, Gdk_Event_Expose);

   function From_Object_Free (B : access Gdk_Event_Expose) return Gdk_Event_Expose;
   pragma Inline (From_Object_Free);
   --  Generated when all or part of a window becomes visible and needs to be
   --  redrawn.

   type Gdk_Event_Visibility is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      State : Gdk_Visibility_State;
   end record;
   pragma Convention (C, Gdk_Event_Visibility);

   function From_Object_Free (B : access Gdk_Event_Visibility) return Gdk_Event_Visibility;
   pragma Inline (From_Object_Free);
   --  Generated when the window visibility status has changed.

   type Gdk_Event_Motion is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Time : Guint32;
      X : Gdouble;
      Y : Gdouble;
      Axes : access Gdouble;
      State : Gdk.Types.Gdk_Modifier_Type;
      Is_Hint : Gint16;
      Device : System.Address;
      X_Root : Gdouble;
      Y_Root : Gdouble;
   end record;
   pragma Convention (C, Gdk_Event_Motion);

   function From_Object_Free (B : access Gdk_Event_Motion) return Gdk_Event_Motion;
   pragma Inline (From_Object_Free);
   --  Generated when the pointer moves.

   type Gdk_Event_Scroll is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Time : Guint32;
      X : Gdouble;
      Y : Gdouble;
      State : Gdk.Types.Gdk_Modifier_Type;
      Direction : Gdk_Scroll_Direction;
      Device : System.Address;
      X_Root : Gdouble;
      Y_Root : Gdouble;
      Delta_X : Gdouble;
      Delta_Y : Gdouble;
      Is_Stop : Guint;
   end record;
   pragma Convention (C, Gdk_Event_Scroll);

   function From_Object_Free (B : access Gdk_Event_Scroll) return Gdk_Event_Scroll;
   pragma Inline (From_Object_Free);
   --  Generated from button presses for the buttons 4 to 7. Wheel mice are
   --  usually configured to generate button press events for buttons 4 and 5
   --  when the wheel is turned.
   --
   --  Some GDK backends can also generate "smooth" scroll events, which can
   --  be recognized by the Gdk.Event.Scroll_Smooth scroll direction. For
   --  these, the scroll deltas can be obtained with
   --  Gdk.Event.Get_Scroll_Deltas.

   type Gdk_Event_Key is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Time : Guint32;
      State : Gdk.Types.Gdk_Modifier_Type;
      Keyval : Gdk.Types.Gdk_Key_Type;
      Length : Glib.Gint := 0;
      String : Gtkada.Types.Chars_Ptr;
      Hardware_Keycode : Guint16;
      Group : Guint8;
      Is_Modifier : Guint;
   end record;
   pragma Convention (C, Gdk_Event_Key);

   function From_Object_Free (B : access Gdk_Event_Key) return Gdk_Event_Key;
   pragma Inline (From_Object_Free);
   --  Describes a key press or key release event.

   type Gdk_Event_Crossing is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Subwindow : Gdk.Gdk_Window;
      Time : Guint32;
      X : Gdouble;
      Y : Gdouble;
      X_Root : Gdouble;
      Y_Root : Gdouble;
      Mode : Gdk_Crossing_Mode;
      Detail : Gdk_Notify_Type;
      Focus : Boolean;
      State : Gdk.Types.Gdk_Modifier_Type;
   end record;
   pragma Convention (C, Gdk_Event_Crossing);

   function From_Object_Free (B : access Gdk_Event_Crossing) return Gdk_Event_Crossing;
   pragma Inline (From_Object_Free);
   --  Generated when the pointer enters or leaves a window.

   type Gdk_Event_Focus is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Gtk_In : Gint16;
   end record;
   pragma Convention (C, Gdk_Event_Focus);

   function From_Object_Free (B : access Gdk_Event_Focus) return Gdk_Event_Focus;
   pragma Inline (From_Object_Free);
   --  Describes a change of keyboard focus.

   type Gdk_Event_Configure is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      X : Glib.Gint := 0;
      Y : Glib.Gint := 0;
      Width : Glib.Gint := 0;
      Height : Glib.Gint := 0;
   end record;
   pragma Convention (C, Gdk_Event_Configure);

   function From_Object_Free (B : access Gdk_Event_Configure) return Gdk_Event_Configure;
   pragma Inline (From_Object_Free);
   --  Generated when a window size or position has changed.

   type Gdk_Event_Property is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Atom : Gdk.Types.Gdk_Atom;
      Time : Guint32;
      State : Gdk_Property_State;
   end record;
   pragma Convention (C, Gdk_Event_Property);

   function From_Object_Free (B : access Gdk_Event_Property) return Gdk_Event_Property;
   pragma Inline (From_Object_Free);
   --  Describes a property change on a window.

   type Gdk_Event_Selection is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Selection : Gdk.Types.Gdk_Atom;
      Target : Gdk.Types.Gdk_Atom;
      Property : Gdk.Types.Gdk_Atom;
      Time : Guint32;
      Requestor : Gdk.Gdk_Window;
   end record;
   pragma Convention (C, Gdk_Event_Selection);

   function From_Object_Free (B : access Gdk_Event_Selection) return Gdk_Event_Selection;
   pragma Inline (From_Object_Free);
   --  Generated when a selection is requested or ownership of a selection is
   --  taken over by another client application.

   type Gdk_Event_Owner_Change is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Owner : Gdk.Gdk_Window;
      Reason : Gdk_Owner_Change;
      Selection : Gdk.Types.Gdk_Atom;
      Time : Guint32;
      Selection_Time : Guint32;
   end record;
   pragma Convention (C, Gdk_Event_Owner_Change);

   function From_Object_Free (B : access Gdk_Event_Owner_Change) return Gdk_Event_Owner_Change;
   pragma Inline (From_Object_Free);
   --  Generated when the owner of a selection changes. On X11, this
   --  information is only available if the X server supports the XFIXES
   --  extension.

   type Gdk_Event_Proximity is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Time : Guint32;
      Device : System.Address;
   end record;
   pragma Convention (C, Gdk_Event_Proximity);

   function From_Object_Free (B : access Gdk_Event_Proximity) return Gdk_Event_Proximity;
   pragma Inline (From_Object_Free);
   --  Proximity events are generated when using GDK's wrapper for the XInput
   --  extension. The XInput extension is an add-on for standard X that allows
   --  you to use nonstandard devices such as graphics tablets. A proximity
   --  event indicates that the stylus has moved in or out of contact with the
   --  tablet, or perhaps that the user's finger has moved in or out of contact
   --  with a touch screen.
   --
   --  This event type will be used pretty rarely. It only is important for
   --  XInput aware programs that are drawing their own cursor.

   type Gdk_Event_DND is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Context : System.Address;
      Time : Guint32;
      X_Root : Gshort;
      Y_Root : Gshort;
   end record;
   pragma Convention (C, Gdk_Event_DND);

   function From_Object_Free (B : access Gdk_Event_DND) return Gdk_Event_DND;
   pragma Inline (From_Object_Free);
   --  Generated during DND operations.

   type Gdk_Event_Window_State is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Changed_Mask : Gdk_Window_State;
      New_Window_State : Gdk_Window_State;
   end record;
   pragma Convention (C, Gdk_Event_Window_State);

   function From_Object_Free (B : access Gdk_Event_Window_State) return Gdk_Event_Window_State;
   pragma Inline (From_Object_Free);
   --  Generated when the state of a toplevel window changes.

   type Gdk_Event_Setting is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Action : Gdk_Setting_Action;
      Name : Gtkada.Types.Chars_Ptr;
   end record;
   pragma Convention (C, Gdk_Event_Setting);

   function From_Object_Free (B : access Gdk_Event_Setting) return Gdk_Event_Setting;
   pragma Inline (From_Object_Free);
   --  Generated when a setting is modified.

   type Gdk_Event_Touch is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Time : Guint32;
      X : Gdouble;
      Y : Gdouble;
      Axes : access Gdouble;
      State : Gdk.Types.Gdk_Modifier_Type;
      Sequence : Gdk_Event_Sequence;
      Emulating_Pointer : Boolean;
      Device : System.Address;
      X_Root : Gdouble;
      Y_Root : Gdouble;
   end record;
   pragma Convention (C, Gdk_Event_Touch);

   function From_Object_Free (B : access Gdk_Event_Touch) return Gdk_Event_Touch;
   pragma Inline (From_Object_Free);
   --  Used for touch events. Type field will be one of Gdk.Event.Touch_Begin,
   --  Gdk.Event.Touch_Update, Gdk.Event.Touch_End or Gdk.Event.Touch_Cancel.
   --
   --  Touch events are grouped into sequences by means of the Sequence field,
   --  which can also be obtained with Gdk.Event.Get_Event_Sequence. Each
   --  sequence begins with a Gdk.Event.Touch_Begin event, followed by any
   --  number of Gdk.Event.Touch_Update events, and ends with a
   --  Gdk.Event.Touch_End (or Gdk.Event.Touch_Cancel) event. With multitouch
   --  devices, there may be several active sequences at the same time.

   type Gdk_Event_Grab_Broken is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Keyboard : Boolean;
      Implicit : Boolean;
      Grab_Window : Gdk.Gdk_Window;
   end record;
   pragma Convention (C, Gdk_Event_Grab_Broken);

   function From_Object_Free (B : access Gdk_Event_Grab_Broken) return Gdk_Event_Grab_Broken;
   pragma Inline (From_Object_Free);
   --  Generated when a pointer or keyboard grab is broken. On X11, this
   --  happens when the grab window becomes unviewable (i.e. it or one of its
   --  ancestors is unmapped), or if the same application grabs the pointer or
   --  keyboard again. Note that implicit grabs (which are initiated by button
   --  presses) can also cause Gdk.Event.Gdk_Event_Grab_Broken events.
   --  Generated when a pointer or keyboard grab is broken. On X11, this
   --  happens when the grab window becomes unviewable (i.e. it or one of its
   --  ancestors is unmapped), or if the same application grabs the pointer or
   --  keyboard again. Note that implicit grabs (which are initiated by button
   --  presses) can also cause Gdk.Event.Gdk_Event_Grab_Broken events.

   type Gdk_Event_Touchpad_Swipe is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Phase : Gint8;
      N_Fingers : Gint8;
      Time : Guint32;
      X : Gdouble;
      Y : Gdouble;
      Dx : Gdouble;
      Dy : Gdouble;
      X_Root : Gdouble;
      Y_Root : Gdouble;
      State : Gdk.Types.Gdk_Modifier_Type;
   end record;
   pragma Convention (C, Gdk_Event_Touchpad_Swipe);

   function From_Object_Free (B : access Gdk_Event_Touchpad_Swipe) return Gdk_Event_Touchpad_Swipe;
   pragma Inline (From_Object_Free);
   --  Generated during touchpad swipe gestures.

   type Gdk_Event_Touchpad_Pinch is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Phase : Gint8;
      N_Fingers : Gint8;
      Time : Guint32;
      X : Gdouble;
      Y : Gdouble;
      Dx : Gdouble;
      Dy : Gdouble;
      Angle_Delta : Gdouble;
      Scale : Gdouble;
      X_Root : Gdouble;
      Y_Root : Gdouble;
      State : Gdk.Types.Gdk_Modifier_Type;
   end record;
   pragma Convention (C, Gdk_Event_Touchpad_Pinch);

   function From_Object_Free (B : access Gdk_Event_Touchpad_Pinch) return Gdk_Event_Touchpad_Pinch;
   pragma Inline (From_Object_Free);
   --  Generated during touchpad swipe gestures.

   type Gdk_Event_Pad_Button is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Time : Guint32;
      Group : Guint;
      Button : Guint;
      Mode : Guint;
   end record;
   pragma Convention (C, Gdk_Event_Pad_Button);

   function From_Object_Free (B : access Gdk_Event_Pad_Button) return Gdk_Event_Pad_Button;
   pragma Inline (From_Object_Free);
   --  Generated during GDK_SOURCE_TABLET_PAD button presses and releases.

   type Gdk_Event_Pad_Axis is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Time : Guint32;
      Group : Guint;
      Index : Guint;
      Mode : Guint;
      Value : Gdouble;
   end record;
   pragma Convention (C, Gdk_Event_Pad_Axis);

   function From_Object_Free (B : access Gdk_Event_Pad_Axis) return Gdk_Event_Pad_Axis;
   pragma Inline (From_Object_Free);
   --  Generated during GDK_SOURCE_TABLET_PAD interaction with tactile
   --  sensors.

   type Gdk_Event_Pad_Group_Mode is record
      The_Type : Gdk_Event_Type;
      Window : Gdk.Gdk_Window;
      Send_Event : Gint8;
      Time : Guint32;
      Group : Guint;
      Mode : Guint;
   end record;
   pragma Convention (C, Gdk_Event_Pad_Group_Mode);

   function From_Object_Free (B : access Gdk_Event_Pad_Group_Mode) return Gdk_Event_Pad_Group_Mode;
   pragma Inline (From_Object_Free);
   --  Generated during GDK_SOURCE_TABLET_PAD mode switches in a group.

   type Gdk_Event_Record (The_Type : Gdk_Event_Type := Gdk.Event.Nothing) is record
      case The_Type is

         when Gdk.Event.Nothing
         | Gdk.Event.Delete
         | Gdk.Event.Destroy
         | Gdk.Event.Map
         | Gdk.Event.Unmap
         | Gdk.Event.Client_Event =>
         Any : Gdk_Event_Any;

         when Gdk.Event.Expose
         | Gdk.Event.Damage =>
         Expose : Gdk_Event_Expose;

         when Gdk.Event.Visibility_Notify =>
         Visibility : Gdk_Event_Visibility;

         when Gdk.Event.Motion_Notify =>
         Motion : Gdk_Event_Motion;

         when Gdk.Event.Button_Press
         | Gdk.Event.Gdk_2button_Press
         | Gdk.Event.Gdk_3button_Press
         | Gdk.Event.Button_Release =>
         Button : Gdk_Event_Button;

         when Gdk.Event.Touch_Begin
         | Gdk.Event.Touch_Update
         | Gdk.Event.Touch_End
         | Gdk.Event.Touch_Cancel =>
         Touch : Gdk_Event_Touch;

         when Gdk.Event.Scroll =>
         Scroll : Gdk_Event_Scroll;

         when Gdk.Event.Key_Press
         | Gdk.Event.Key_Release =>
         Key : Gdk_Event_Key;

         when Gdk.Event.Enter_Notify
         | Gdk.Event.Leave_Notify =>
         Crossing : Gdk_Event_Crossing;

         when Gdk.Event.Focus_Change =>
         Focus_Change : Gdk_Event_Focus;

         when Gdk.Event.Configure =>
         Configure : Gdk_Event_Configure;

         when Gdk.Event.Property_Notify =>
         Property : Gdk_Event_Property;

         when Gdk.Event.Selection_Clear
         | Gdk.Event.Selection_Request
         | Gdk.Event.Selection_Notify =>
         Selection : Gdk_Event_Selection;

         when Gdk.Event.Owner_Change =>
         Owner_Change : Gdk_Event_Owner_Change;

         when Gdk.Event.Proximity_In
         | Gdk.Event.Proximity_Out =>
         Proximity : Gdk_Event_Proximity;

         when Gdk.Event.Drag_Enter
         | Gdk.Event.Drag_Leave
         | Gdk.Event.Drag_Motion
         | Gdk.Event.Drag_Status
         | Gdk.Event.Drop_Start
         | Gdk.Event.Drop_Finished =>
         Dnd : Gdk_Event_DND;

         when Gdk.Event.Window_State =>
         Window_State : Gdk_Event_Window_State;

         when Gdk.Event.Setting =>
         Setting : Gdk_Event_Setting;

         when Gdk.Event.Grab_Broken =>
         Grab_Broken : Gdk_Event_Grab_Broken;

         when Gdk.Event.Touchpad_Swipe =>
         Touchpad_Swipe : Gdk_Event_Touchpad_Swipe;

         when Gdk.Event.Touchpad_Pinch =>
         Touchpad_Pinch : Gdk_Event_Touchpad_Pinch;

         when Gdk.Event.Pad_Button_Press
         | Gdk.Event.Pad_Button_Release =>
         Pad_Button : Gdk_Event_Pad_Button;

         when Gdk.Event.Pad_Ring
         | Gdk.Event.Pad_Strip =>
         Pad_Axis : Gdk_Event_Pad_Axis;

         when Gdk.Event.Pad_Group_Mode =>
         Pad_Group_Mode : Gdk_Event_Pad_Group_Mode;
      end case;
   end record;
   pragma Convention (C, Gdk_Event_Record);
   pragma Unchecked_Union(Gdk_Event_Record);

   function From_Object_Free (B : access Gdk_Event_Record) return Gdk_Event_Record;
   pragma Inline (From_Object_Free);
   --  A Gdk.Event.Gdk_Event contains a union of all of the event types, and
   --  allows access to the data fields in a number of ways.
   --
   --  The event type is always the first field in all of the event types, and
   --  can always be accessed with the following code, no matter what type of
   --  event it is: |[<!-- language="C" --> GdkEvent *event; GdkEventType type;
   --
   --  type = event->type; ]|
   --
   --  To access other fields of the event, the pointer to the event can be
   --  cast to the appropriate event type, or the union member name can be
   --  used. For example if the event type is Gdk.Event.Button_Press then the x
   --  coordinate of the button press can be accessed with: |[<!-- language="C"
   --  --> GdkEvent *event; gdouble x;
   --
   --  x = ((GdkEventButton*)event)->x; ]| or: |[<!-- language="C" -->
   --  GdkEvent *event; gdouble x;
   --
   --  x = event->button.x; ]|

   type Gdk_Event is access all Gdk_Event_Record;
   pragma No_Strict_Aliasing (Gdk_Event);

   ---------------
   -- Callbacks --
   ---------------

   type Gdk_Event_Func is access procedure (Event : Gdk_Event);
   --  Specifies the type of function passed to Gdk.Event.Handler_Set to
   --  handle all GDK events.
   --  "event": the Gdk.Event.Gdk_Event to process.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Event_Type_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Event_Type);
   type Property_Gdk_Event_Type is new Gdk_Event_Type_Properties.Property;

   package Gdk_Event_Mask_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Event_Mask);
   type Property_Gdk_Event_Mask is new Gdk_Event_Mask_Properties.Property;

   package Gdk_Visibility_State_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Visibility_State);
   type Property_Gdk_Visibility_State is new Gdk_Visibility_State_Properties.Property;

   package Gdk_Scroll_Direction_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Scroll_Direction);
   type Property_Gdk_Scroll_Direction is new Gdk_Scroll_Direction_Properties.Property;

   package Gdk_Notify_Type_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Notify_Type);
   type Property_Gdk_Notify_Type is new Gdk_Notify_Type_Properties.Property;

   package Gdk_Crossing_Mode_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Crossing_Mode);
   type Property_Gdk_Crossing_Mode is new Gdk_Crossing_Mode_Properties.Property;

   package Gdk_Property_State_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Property_State);
   type Property_Gdk_Property_State is new Gdk_Property_State_Properties.Property;

   package Gdk_Window_State_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Window_State);
   type Property_Gdk_Window_State is new Gdk_Window_State_Properties.Property;

   package Gdk_Setting_Action_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Setting_Action);
   type Property_Gdk_Setting_Action is new Gdk_Setting_Action_Properties.Property;

   package Gdk_Owner_Change_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Owner_Change);
   type Property_Gdk_Owner_Change is new Gdk_Owner_Change_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gdk_New (Event : out Gdk_Event; The_Type : Gdk_Event_Type);
   --  Creates a new event of the given type. All fields are set to 0.
   --  Since: gtk+ 2.2
   --  "type": a Gdk.Event.Gdk_Event_Type

   function Gdk_Event_New (The_Type : Gdk_Event_Type) return Gdk_Event;
   --  Creates a new event of the given type. All fields are set to 0.
   --  Since: gtk+ 2.2
   --  "type": a Gdk.Event.Gdk_Event_Type

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_event_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Angle
      (Event  : Gdk_Event;
       Event2 : Gdk_Event;
       Angle  : access Gdouble) return Boolean;
   --  If both events contain X/Y information, this function will return True
   --  and return in Angle the relative angle from Event1 to Event2. The
   --  rotation direction for positive angles is from the positive X axis
   --  towards the positive Y axis.
   --  Since: gtk+ 3.0
   --  "event2": second Gdk.Event.Gdk_Event
   --  "angle": return location for the relative angle between both events

   function Get_Center
      (Event  : Gdk_Event;
       Event2 : Gdk_Event;
       X      : access Gdouble;
       Y      : access Gdouble) return Boolean;
   --  If both events contain X/Y information, the center of both coordinates
   --  will be returned in X and Y.
   --  Since: gtk+ 3.0
   --  "event2": second Gdk.Event.Gdk_Event
   --  "x": return location for the X coordinate of the center
   --  "y": return location for the Y coordinate of the center

   function Get_Distance
      (Event    : Gdk_Event;
       Event2   : Gdk_Event;
       Distance : access Gdouble) return Boolean;
   --  If both events have X/Y information, the distance between both
   --  coordinates (as in a straight line going from Event1 to Event2) will be
   --  returned.
   --  Since: gtk+ 3.0
   --  "event2": second Gdk.Event.Gdk_Event
   --  "distance": return location for the distance

   function Copy (Event : Gdk_Event) return Gdk_Event;
   pragma Import (C, Copy, "gdk_event_copy");
   --  Copies a Gdk.Event.Gdk_Event, copying or incrementing the reference
   --  count of the resources associated with it (e.g. Gdk.Gdk_Window's and
   --  strings).

   procedure Free (Event : Gdk_Event);
   pragma Import (C, Free, "gdk_event_free");
   --  Frees a Gdk.Event.Gdk_Event, freeing or decrementing any resources
   --  associated with it. Note that this function should only be called with
   --  events returned from functions such as Gdk.Event.Peek, Gdk.Event.Get,
   --  Gdk.Event.Copy and gdk_event_new.

   procedure Get_Axis
      (Event    : Gdk_Event;
       Axis_Use : Gdk_Axis_Use;
       Value    : out Gdouble);
   pragma Import (C, Get_Axis, "gdk_event_get_axis");
   --  Extract the axis value for a particular axis use from an event
   --  structure.
   --  "axis_use": the axis use to look for
   --  "value": location to store the value found

   procedure Get_Coords
      (Event : Gdk_Event;
       X_Win : out Gdouble;
       Y_Win : out Gdouble);
   pragma Import (C, Get_Coords, "gdk_event_get_coords");
   --  Extract the event window relative x/y coordinates from an event.
   --  "x_win": location to put event window x coordinate
   --  "y_win": location to put event window y coordinate

   function Get_Device_Tool
      (Event : Gdk_Event) return Gdk.Device_Tool.Gdk_Device_Tool;
   --  If the event was generated by a device that supports different tools
   --  (eg. a tablet), this function will return a
   --  Gdk.Device_Tool.Gdk_Device_Tool representing the tool that caused the
   --  event. Otherwise, null will be returned.
   --  Note: the Gdk.Device_Tool.Gdk_Device_Tool<!-- -->s will be constant
   --  during the application lifetime, if settings must be stored persistently
   --  across runs, see Gdk.Device_Tool.Get_Serial
   --  Since: gtk+ 3.22

   procedure Set_Device_Tool
      (Event : Gdk_Event;
       Tool  : access Gdk.Device_Tool.Gdk_Device_Tool_Record'Class);
   --  Sets the device tool for this event, should be rarely used.
   --  Since: gtk+ 3.22
   --  "tool": tool to set on the event, or null

   function Get_Event_Sequence (Event : Gdk_Event) return Gdk_Event_Sequence;
   pragma Import (C, Get_Event_Sequence, "gdk_event_get_event_sequence");
   --  If Event if of type Gdk.Event.Touch_Begin, Gdk.Event.Touch_Update,
   --  Gdk.Event.Touch_End or Gdk.Event.Touch_Cancel, returns the
   --  Gdk.Event.Gdk_Event_Sequence to which the event belongs. Otherwise,
   --  return null.
   --  Since: gtk+ 3.4

   function Get_Event_Type (Event : Gdk_Event) return Gdk_Event_Type;
   pragma Import (C, Get_Event_Type, "gdk_event_get_event_type");
   --  Retrieves the type of the event.
   --  Since: gtk+ 3.10

   function Get_Pointer_Emulated (Event : Gdk_Event) return Boolean;
   --  event: a Gdk.Event.Gdk_Event Returns whether this event is an
   --  'emulated' pointer event (typically from a touch event), as opposed to a
   --  real one.
   --  Since: gtk+ 3.22

   procedure Get_Root_Coords
      (Event  : Gdk_Event;
       X_Root : out Gdouble;
       Y_Root : out Gdouble);
   pragma Import (C, Get_Root_Coords, "gdk_event_get_root_coords");
   --  Extract the root window relative x/y coordinates from an event.
   --  "x_root": location to put root window x coordinate
   --  "y_root": location to put root window y coordinate

   function Get_Scancode (Event : Gdk_Event) return Glib.Gint;
   pragma Import (C, Get_Scancode, "gdk_event_get_scancode");
   --  Gets the keyboard low-level scancode of a key event.
   --  This is usually hardware_keycode. On Windows this is the high word of
   --  WM_KEY{DOWN,UP} lParam which contains the scancode and some extended
   --  flags.
   --  Since: gtk+ 3.22

   procedure Get_Scroll_Deltas
      (Event   : Gdk_Event;
       Delta_X : out Gdouble;
       Delta_Y : out Gdouble);
   pragma Import (C, Get_Scroll_Deltas, "gdk_event_get_scroll_deltas");
   --  Retrieves the scroll deltas from a Gdk.Event.Gdk_Event
   --  See also: Gdk.Event.Get_Scroll_Direction
   --  Since: gtk+ 3.4
   --  "delta_x": return location for X delta
   --  "delta_y": return location for Y delta

   procedure Get_Scroll_Direction
      (Event     : Gdk_Event;
       Direction : out Gdk_Scroll_Direction);
   pragma Import (C, Get_Scroll_Direction, "gdk_event_get_scroll_direction");
   --  Extracts the scroll direction from an event.
   --  If Event is not of type Gdk.Event.Scroll, the contents of Direction are
   --  undefined.
   --  If you wish to handle both discrete and smooth scrolling, you should
   --  check the return value of this function, or of
   --  Gdk.Event.Get_Scroll_Deltas; for instance:
   --  |[<!-- language="C" --> GdkScrollDirection direction; double
   --  vscroll_factor = 0.0; double x_scroll, y_scroll;
   --  if (gdk_event_get_scroll_direction (event, &direction)) { // Handle
   --  discrete scrolling with a known constant delta; const double delta =
   --  12.0;
   --  switch (direction) { case GDK_SCROLL_UP: vscroll_factor = -delta;
   --  break; case GDK_SCROLL_DOWN: vscroll_factor = delta; break; default: //
   --  no scrolling break; } } else if (gdk_event_get_scroll_deltas (event,
   --  &x_scroll, &y_scroll)) { // Handle smooth scrolling directly
   --  vscroll_factor = y_scroll; } ]|
   --  Since: gtk+ 3.2
   --  "direction": location to store the scroll direction

   function Get_Seat (Event : Gdk_Event) return Glib.Object.GObject;
   --  Returns the Gdk.Seat.Gdk_Seat this event was generated for.
   --  Since: gtk+ 3.20

   function Get_Time (Event : Gdk_Event) return Guint32;
   pragma Import (C, Get_Time, "gdk_event_get_time");
   --  Returns the time stamp from Event, if there is one; otherwise returns
   --  GDK_CURRENT_TIME. If Event is null, returns GDK_CURRENT_TIME.

   function Get_Window (Event : Gdk_Event) return Gdk.Gdk_Window;
   pragma Import (C, Get_Window, "gdk_event_get_window");
   --  Extracts the Gdk.Gdk_Window associated with an event.
   --  Since: gtk+ 3.10

   function Is_Scroll_Stop_Event (Event : Gdk_Event) return Boolean;
   --  Check whether a scroll event is a stop scroll event. Scroll sequences
   --  with smooth scroll information may provide a stop scroll event once the
   --  interaction with the device finishes, e.g. by lifting a finger. This
   --  stop scroll event is the signal that a widget may trigger kinetic
   --  scrolling based on the current velocity.
   --  Stop scroll events always have a a delta of 0/0.
   --  Since: gtk+ 3.20

   procedure Put (Event : Gdk_Event);
   pragma Import (C, Put, "gdk_event_put");
   --  Appends a copy of the given event onto the front of the event queue for
   --  event->any.window's display, or the default event queue if
   --  event->any.window is null. See Gdk.Display.Put_Event.

   function Triggers_Context_Menu (Event : Gdk_Event) return Boolean;
   --  This function returns whether a Gdk.Event.Gdk_Event_Button should
   --  trigger a context menu, according to platform conventions. The right
   --  mouse button always triggers context menus. Additionally, if
   --  gdk_keymap_get_modifier_mask returns a non-0 mask for
   --  GDK_MODIFIER_INTENT_CONTEXT_MENU, then the left mouse button will also
   --  trigger a context menu if this modifier is pressed.
   --  This function should always be used instead of simply checking for
   --  event->button == GDK_BUTTON_SECONDARY.
   --  Since: gtk+ 3.4

   function Get_Button (Event : Gdk_Event) return Guint;
   pragma Import (C, Get_Button, "ada_gdk_event_get_button");
   --  Extract the button number from an event

   function Get_State (Event : Gdk_Event) return Gdk.Types.Gdk_Modifier_Type;
   pragma Import (C, Get_State, "ada_gdk_event_get_state");
   --  State of the mouse buttons and keyboard keys just prior to the event

   function Get_Key_Val (Event : Gdk_Event) return Gdk.Types.Gdk_Key_Type;
   pragma Import (C, Get_Key_Val, "ada_gdk_event_get_keyval");
   --  Code of the key that was pressed (and that generated the event)

   function Get_Keycode (Event : Gdk_Event) return Guint;
   pragma Import (C, Get_Keycode, "ada_gdk_event_get_keycode");
   --  Hardware key code of the key that was pressed

   procedure Handler_Set (Func : Gdk_Event_Func);
   --  Sets the function to call to handle all events from GDK.
   --  Note that GTK+ uses this to install its own event handler, so it is
   --  usually not useful for GTK+ applications. (Although an application can
   --  call this function then call Gtk.Main.Main_Do_Event to pass events to
   --  GTK+.)
   --  "func": the function to call to handle events from GDK.

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Handler_Set_User_Data is

      type Gdk_Event_Func is access procedure (Event : Gdk.Event.Gdk_Event; Data : User_Data_Type);
      --  Specifies the type of function passed to Gdk.Event.Handler_Set to
      --  handle all GDK events.
      --  "event": the Gdk.Event.Gdk_Event to process.
      --  "data": user data set when the event handler was installed with
      --  Gdk.Event.Handler_Set.

      procedure Handler_Set (Func : Gdk_Event_Func; Data : User_Data_Type);
      --  Sets the function to call to handle all events from GDK.
      --  Note that GTK+ uses this to install its own event handler, so it is
      --  usually not useful for GTK+ applications. (Although an application
      --  can call this function then call Gtk.Main.Main_Do_Event to pass
      --  events to GTK+.)
      --  "func": the function to call to handle events from GDK.
      --  "data": user data to pass to the function.

   end Handler_Set_User_Data;

   ----------------------
   -- GtkAda additions --
   ----------------------

   Double_Button_Press : constant Gdk_Event_Type := Gdk_2button_Press;
   Triple_Button_Press : constant Gdk_Event_Type := Gdk_3button_Press;

   Button_Primary   : constant Gint := 1;
   Button_Secondary : constant Gint := 3;
   Button_Middle    : constant Gint := 2;
   --  The primary button is typically the left mouse button, or the
   --  right button in a left-handed setup. The secondary button is the
   --  other one.

   function From_Address (C : System.Address) return Gdk_Event;
   --  Convert a C handler to the matching Event structure.

   function To_Address (C : Gdk_Event) return System.Address;
   --  Convert an event to the underlying C handler.

   function Get_Event (Value : Glib.Values.GValue) return Gdk_Event;
   --  Convert a value into a Gdk_Event.

   function To_Event (Event : access Gdk_Event_Button) return Gdk_Event;
   function To_Event (Event : access Gdk_Event_Key) return Gdk_Event;
   --  Cast Event into a Gdk_Event, which can be used to call some of
   --  subprograms in the API. The return value is a pointer to Event,
   --  which should therefore remain valid as long as the pointer is in
   --  use.

   -------------------------------
   -- Some constants used below --
   -------------------------------
   --  This constants have the '-1' since in some cases gtk itself uses
   --  the extrema to return some meaningful value (for instance, the result
   --  of Get_Area can have the values Guint16'Last to mean the whole area).

   Invalid_Gdouble_Value : constant Gdouble := Gdouble'Last - 1.0;
   Invalid_Gint_Value    : constant Gint    := Gint'Last - 1;
   Invalid_Guint_Value   : constant Guint   := Guint'Last - 1;
   Invalid_Guint32_Value : constant Guint32 := Guint32'Last - 1;
   Invalid_Gulong_Value  : constant Gulong  := Gulong'Last - 1;

   pragma Export (C, Invalid_Gdouble_Value, "ada_gdk_invalid_gdouble_value");
   pragma Export (C, Invalid_Gint_Value, "ada_gdk_invalid_gint_value");
   pragma Export (C, Invalid_Guint_Value, "ada_gdk_invalid_guint_value");
   pragma Export (C, Invalid_Guint32_Value, "ada_gdk_invalid_guint32_value");
   pragma Export (C, Invalid_Gulong_Value, "ada_gdk_invalid_gulong_value");

   ---------------
   -- Functions --
   ---------------

   function Get return Gdk_Event;
   pragma Import (C, Get, "gdk_event_get");
   --  Checks all open displays for a Gdk.Event.Gdk_Event to process,to be
   --  processed on, fetching events from the windowing system if necessary.
   --  See Gdk.Display.Get_Event.

   function Peek return Gdk_Event;
   pragma Import (C, Peek, "gdk_event_peek");
   --  If there is an event waiting in the event queue of some open display,
   --  returns a copy of it. See Gdk.Display.Peek_Event.

   procedure Request_Motions (Event : Gdk_Event_Motion);
   pragma Import (C, Request_Motions, "gdk_event_request_motions");
   --  Request more motion notifies if Event is a motion notify hint event.
   --  This function should be used instead of Gdk.Window.Get_Pointer to
   --  request further motion notifies, because it also works for extension
   --  events where motion notifies are provided for devices other than the
   --  core pointer. Coordinate extraction, processing and requesting more
   --  motion events from a Gdk.Event.Motion_Notify event usually works like
   --  this:
   --  |[<!-- language="C" --> { // motion_event handler x = motion_event->x;
   --  y = motion_event->y; // handle (x,y) motion gdk_event_request_motions
   --  (motion_event); // handles is_hint events } ]|
   --  Since: gtk+ 2.12
   --  "event": a valid Gdk.Event.Gdk_Event

   function Events_Pending return Boolean;
   --  Checks if any events are ready to be processed for any display.

   procedure Set_Show_Events (Show_Events : Boolean);
   --  Sets whether a trace of received events is output. Note that GTK+ must
   --  be compiled with debugging (that is, configured using the
   --  `--enable-debug` option) to use this option.
   --  "show_events": True to output event debugging information.

   function Get_Show_Events return Boolean;
   --  Gets whether event debugging output is enabled.

end Gdk.Event;
