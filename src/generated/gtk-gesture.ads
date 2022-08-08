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
--  Gtk.Gesture.Gtk_Gesture is the base object for gesture recognition,
--  although this object is quite generalized to serve as a base for
--  multi-touch gestures, it is suitable to implement single-touch and
--  pointer-based gestures (using the special null Gdk.Event.Gdk_Event_Sequence
--  value for these).
--
--  The number of touches that a Gtk.Gesture.Gtk_Gesture need to be recognized
--  is controlled by the Gtk.Gesture.Gtk_Gesture:n-points property, if a
--  gesture is keeping track of less or more than that number of sequences, it
--  won't check wether the gesture is recognized.
--
--  As soon as the gesture has the expected number of touches, the gesture
--  will run the Gtk.Gesture.Gtk_Gesture::check signal regularly on input
--  events until the gesture is recognized, the criteria to consider a gesture
--  as "recognized" is left to Gtk.Gesture.Gtk_Gesture subclasses.
--
--  A recognized gesture will then emit the following signals: -
--  Gtk.Gesture.Gtk_Gesture::begin when the gesture is recognized. - A number
--  of Gtk.Gesture.Gtk_Gesture::update, whenever an input event is processed. -
--  Gtk.Gesture.Gtk_Gesture::end when the gesture is no longer recognized.
--
--  ## Event propagation
--
--  In order to receive events, a gesture needs to either set a propagation
--  phase through Gtk.Event_Controller.Set_Propagation_Phase, or feed those
--  manually through Gtk.Event_Controller.Handle_Event.
--
--  In the capture phase, events are propagated from the toplevel down to the
--  target widget, and gestures that are attached to containers above the
--  widget get a chance to interact with the event before it reaches the
--  target.
--
--  After the capture phase, GTK+ emits the traditional
--  Gtk.Widget.Gtk_Widget::button-press-event,
--  Gtk.Widget.Gtk_Widget::button-release-event,
--  Gtk.Widget.Gtk_Widget::touch-event, etc signals. Gestures with the
--  Gtk.Enums.Phase_Target phase are fed events from the default
--  Gtk.Widget.Gtk_Widget::event handlers.
--
--  In the bubble phase, events are propagated up from the target widget to
--  the toplevel, and gestures that are attached to containers above the widget
--  get a chance to interact with events that have not been handled yet.
--
--  ## States of a sequence # {touch-sequence-states}
--
--  Whenever input interaction happens, a single event may trigger a cascade
--  of Gtk_Gestures, both across the parents of the widget receiving the event
--  and in parallel within an individual widget. It is a responsibility of the
--  widgets using those gestures to set the state of touch sequences
--  accordingly in order to enable cooperation of gestures around the
--  Gdk_Event_Sequences triggering those.
--
--  Within a widget, gestures can be grouped through Gtk.Gesture.Group,
--  grouped gestures synchronize the state of sequences, so calling
--  Gtk.Gesture.Set_Sequence_State on one will effectively propagate the state
--  throughout the group.
--
--  By default, all sequences start out in the GTK_EVENT_SEQUENCE_NONE state,
--  sequences in this state trigger the gesture event handler, but event
--  propagation will continue unstopped by gestures.
--
--  If a sequence enters into the GTK_EVENT_SEQUENCE_DENIED state, the gesture
--  group will effectively ignore the sequence, letting events go unstopped
--  through the gesture, but the "slot" will still remain occupied while the
--  touch is active.
--
--  If a sequence enters in the GTK_EVENT_SEQUENCE_CLAIMED state, the gesture
--  group will grab all interaction on the sequence, by: - Setting the same
--  sequence to GTK_EVENT_SEQUENCE_DENIED on every other gesture group within
--  the widget, and every gesture on parent widgets in the propagation chain. -
--  calling Gtk.Gesture.Gtk_Gesture::cancel on every gesture in widgets
--  underneath in the propagation chain. - Stopping event propagation after the
--  gesture group handles the event.
--
--  Note: if a sequence is set early to GTK_EVENT_SEQUENCE_CLAIMED on
--  GDK_TOUCH_BEGIN/GDK_BUTTON_PRESS (so those events are captured before
--  reaching the event widget, this implies GTK_PHASE_CAPTURE), one similar
--  event will emulated if the sequence changes to GTK_EVENT_SEQUENCE_DENIED.
--  This way event coherence is preserved before event propagation is unstopped
--  again.
--
--  Sequence states can't be changed freely, see
--  Gtk.Gesture.Set_Sequence_State to know about the possible lifetimes of a
--  Gdk.Event.Gdk_Event_Sequence.
--
--  ## Touchpad gestures
--
--  On the platforms that support it, Gtk.Gesture.Gtk_Gesture will handle
--  transparently touchpad gesture events. The only precautions users of
--  Gtk.Gesture.Gtk_Gesture should do to enable this support are: - Enabling
--  Gdk.Event.Touchpad_Gesture_Mask on their Gdk_Windows - If the gesture has
--  Gtk.Enums.Phase_None, ensuring events of type Gdk.Event.Touchpad_Swipe and
--  Gdk.Event.Touchpad_Pinch are handled by the Gtk.Gesture.Gtk_Gesture
--
--  </description>
--  <description>
--  GtkAda
--
--  Gesture events are meant to replace low-level handling in most cases. For
--  instance, you should move away from direct use of "button_press" event, and
--  instead use a Gtk.Gesture.Multi_Press gesture. They give you more control
--  (on the area for which a double click is valid for instance), work with
--  touchpad in addition to mouse events, and you can have multiple gestures
--  associated with the same widget, and that won't compete with each other.
--
--  For instance, having drag-and-drop on a widget which also manipulates
--  low-level "button_press" events is hard to get rid, since the latter
--  callback will often consume the events needed by gtk+ to detect that a drag
--  operation is about to take place.
--
--  Gestures are objects in their full rights, and therefore needs to be
--  Unref-ed to free memory. GtkAda provides a convenient function for this, so
--  that the typical workflow is:
--
--  Gesture : Gtk_Gesture_Multi_Press;
--
--  Gtk_New (Gesture, Widget); Gesture.Watch (Widget); -- Destroy Gesture when
--  Widget is destroyed Gesture.On_Pressed (On_Pressed'Access, Slot => Widget);
--
--  Without the call to Watch, Gesture would be kept in memory for ever, ready
--  to be attached to some other widget.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk;                  use Gdk;
with Gdk.Device;           use Gdk.Device;
with Gdk.Event;            use Gdk.Event;
with Gdk.Rectangle;        use Gdk.Rectangle;
with Glib;                 use Glib;
with Glib.Glist;           use Glib.Glist;
with Glib.Object;          use Glib.Object;
with Glib.Properties;      use Glib.Properties;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Event_Controller; use Gtk.Event_Controller;

package Gtk.Gesture is

   type Gtk_Gesture_Record is new Gtk_Event_Controller_Record with null record;
   type Gtk_Gesture is access all Gtk_Gesture_Record'Class;

   function Convert (R : Gtk.Gesture.Gtk_Gesture) return System.Address;
   function Convert (R : System.Address) return Gtk.Gesture.Gtk_Gesture;
   package Gesture_List is new Generic_List (Gtk.Gesture.Gtk_Gesture);

   function Convert (R : Gdk.Event.Gdk_Event_Sequence) return System.Address;
   function Convert (R : System.Address) return Gdk.Event.Gdk_Event_Sequence;
   package Gdk_Event_Sequence_List is new Generic_List (Gdk.Event.Gdk_Event_Sequence);

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_gesture_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Bounding_Box
      (Self : not null access Gtk_Gesture_Record;
       Rect : access Gdk.Rectangle.Gdk_Rectangle) return Boolean;
   --  If there are touch sequences being currently handled by Gesture, this
   --  function returns True and fills in Rect with the bounding box containing
   --  all active touches. Otherwise, False will be returned.
   --  Note: This function will yield unexpected results on touchpad gestures.
   --  Since there is no correlation between physical and pixel distances,
   --  these will look as if constrained in an infinitely small area, Rect
   --  width and height will thus be 0 regardless of the number of touchpoints.
   --  Since: gtk+ 3.14
   --  "rect": bounding box containing all active touches.

   function Get_Bounding_Box_Center
      (Self : not null access Gtk_Gesture_Record;
       X    : access Gdouble;
       Y    : access Gdouble) return Boolean;
   --  If there are touch sequences being currently handled by Gesture, this
   --  function returns True and fills in X and Y with the center of the
   --  bounding box containing all active touches. Otherwise, False will be
   --  returned.
   --  Since: gtk+ 3.14
   --  "x": X coordinate for the bounding box center
   --  "y": Y coordinate for the bounding box center

   function Get_Device
      (Self : not null access Gtk_Gesture_Record)
       return Gdk.Device.Gdk_Device;
   --  Returns the master Gdk.Device.Gdk_Device that is currently operating on
   --  Gesture, or null if the gesture is not being interacted.
   --  Since: gtk+ 3.14

   function Get_Group
      (Self : not null access Gtk_Gesture_Record) return Gesture_List.Glist;
   --  Returns all gestures in the group of Gesture
   --  Since: gtk+ 3.14

   procedure Group
      (Self    : not null access Gtk_Gesture_Record;
       Gesture : not null access Gtk_Gesture_Record'Class);
   --  Adds Gesture to the same group than Group_Gesture. Gestures are by
   --  default isolated in their own groups.
   --  When gestures are grouped, the state of Gdk_Event_Sequences is kept in
   --  sync for all of those, so calling Gtk.Gesture.Set_Sequence_State, on one
   --  will transfer the same value to the others.
   --  Groups also perform an "implicit grabbing" of sequences, if a
   --  Gdk.Event.Gdk_Event_Sequence state is set to GTK_EVENT_SEQUENCE_CLAIMED
   --  on one group, every other gesture group attached to the same
   --  Gtk.Widget.Gtk_Widget will switch the state for that sequence to
   --  GTK_EVENT_SEQUENCE_DENIED.
   --  Since: gtk+ 3.14
   --  "gesture": a Gtk.Gesture.Gtk_Gesture

   function Get_Last_Event
      (Self     : not null access Gtk_Gesture_Record;
       Sequence : Gdk.Event.Gdk_Event_Sequence) return Gdk.Event.Gdk_Event;
   --  Returns the last event that was processed for Sequence.
   --  Note that the returned pointer is only valid as long as the Sequence is
   --  still interpreted by the Gesture. If in doubt, you should make a copy of
   --  the event.
   --  "sequence": a Gdk.Event.Gdk_Event_Sequence

   function Get_Last_Updated_Sequence
      (Self : not null access Gtk_Gesture_Record)
       return Gdk.Event.Gdk_Event_Sequence;
   --  Returns the Gdk.Event.Gdk_Event_Sequence that was last updated on
   --  Gesture.
   --  Since: gtk+ 3.14

   function Get_Point
      (Self     : not null access Gtk_Gesture_Record;
       Sequence : Gdk.Event.Gdk_Event_Sequence;
       X        : access Gdouble;
       Y        : access Gdouble) return Boolean;
   --  If Sequence is currently being interpreted by Gesture, this function
   --  returns True and fills in X and Y with the last coordinates stored for
   --  that event sequence. The coordinates are always relative to the widget
   --  allocation.
   --  Since: gtk+ 3.14
   --  "sequence": a Gdk.Event.Gdk_Event_Sequence, or null for pointer events
   --  "x": return location for X axis of the sequence coordinates
   --  "y": return location for Y axis of the sequence coordinates

   function Get_Sequence_State
      (Self     : not null access Gtk_Gesture_Record;
       Sequence : Gdk.Event.Gdk_Event_Sequence)
       return Gtk.Enums.Gtk_Event_Sequence_State;
   --  Returns the Sequence state, as seen by Gesture.
   --  Since: gtk+ 3.14
   --  "sequence": a Gdk.Event.Gdk_Event_Sequence

   function Set_Sequence_State
      (Self     : not null access Gtk_Gesture_Record;
       Sequence : Gdk.Event.Gdk_Event_Sequence;
       State    : Gtk.Enums.Gtk_Event_Sequence_State) return Boolean;
   --  Sets the state of Sequence in Gesture. Sequences start in state
   --  GTK_EVENT_SEQUENCE_NONE, and whenever they change state, they can never
   --  go back to that state. Likewise, sequences in state
   --  GTK_EVENT_SEQUENCE_DENIED cannot turn back to a not denied state. With
   --  these rules, the lifetime of an event sequence is constrained to the
   --  next four:
   --  * None * None → Denied * None → Claimed * None → Claimed → Denied
   --  Note: Due to event handling ordering, it may be unsafe to set the state
   --  on another gesture within a Gtk.Gesture.Gtk_Gesture::begin signal
   --  handler, as the callback might be executed before the other gesture
   --  knows about the sequence. A safe way to perform this could be:
   --  |[ static void first_gesture_begin_cb (GtkGesture *first_gesture,
   --  GdkEventSequence *sequence, gpointer user_data) {
   --  gtk_gesture_set_sequence_state (first_gesture, sequence,
   --  GTK_EVENT_SEQUENCE_CLAIMED); gtk_gesture_set_sequence_state
   --  (second_gesture, sequence, GTK_EVENT_SEQUENCE_DENIED); }
   --  static void second_gesture_begin_cb (GtkGesture *second_gesture,
   --  GdkEventSequence *sequence, gpointer user_data) { if
   --  (gtk_gesture_get_sequence_state (first_gesture, sequence) ==
   --  GTK_EVENT_SEQUENCE_CLAIMED) gtk_gesture_set_sequence_state
   --  (second_gesture, sequence, GTK_EVENT_SEQUENCE_DENIED); } ]|
   --  If both gestures are in the same group, just set the state on the
   --  gesture emitting the event, the sequence will be already be initialized
   --  to the group's global state when the second gesture processes the event.
   --  Since: gtk+ 3.14
   --  "sequence": a Gdk.Event.Gdk_Event_Sequence
   --  "state": the sequence state

   function Get_Sequences
      (Self : not null access Gtk_Gesture_Record)
       return Gdk_Event_Sequence_List.Glist;
   --  Returns the list of Gdk_Event_Sequences currently being interpreted by
   --  Gesture.
   --  Since: gtk+ 3.14

   function Get_Window
      (Self : not null access Gtk_Gesture_Record) return Gdk.Gdk_Window;
   --  Returns the user-defined window that receives the events handled by
   --  Gesture. See Gtk.Gesture.Set_Window for more information.
   --  Since: gtk+ 3.14

   procedure Set_Window
      (Self   : not null access Gtk_Gesture_Record;
       Window : Gdk.Gdk_Window);
   --  Sets a specific window to receive events about, so Gesture will
   --  effectively handle only events targeting Window, or a child of it.
   --  Window must pertain to Gtk.Event_Controller.Get_Widget.
   --  Since: gtk+ 3.14
   --  "window": a Gdk.Gdk_Window, or null

   function Handles_Sequence
      (Self     : not null access Gtk_Gesture_Record;
       Sequence : Gdk.Event.Gdk_Event_Sequence) return Boolean;
   --  Returns True if Gesture is currently handling events corresponding to
   --  Sequence.
   --  Since: gtk+ 3.14
   --  "sequence": a Gdk.Event.Gdk_Event_Sequence or null

   function Is_Active
      (Self : not null access Gtk_Gesture_Record) return Boolean;
   --  Returns True if the gesture is currently active. A gesture is active
   --  meanwhile there are touch sequences interacting with it.
   --  Since: gtk+ 3.14

   function Is_Grouped_With
      (Self  : not null access Gtk_Gesture_Record;
       Other : not null access Gtk_Gesture_Record'Class) return Boolean;
   --  Returns True if both gestures pertain to the same group.
   --  Since: gtk+ 3.14
   --  "other": another Gtk.Gesture.Gtk_Gesture

   function Is_Recognized
      (Self : not null access Gtk_Gesture_Record) return Boolean;
   --  Returns True if the gesture is currently recognized. A gesture is
   --  recognized if there are as many interacting touch sequences as required
   --  by Gesture, and Gtk.Gesture.Gtk_Gesture::check returned True for the
   --  sequences being currently interpreted.
   --  Since: gtk+ 3.14

   function Set_State
      (Self  : not null access Gtk_Gesture_Record;
       State : Gtk.Enums.Gtk_Event_Sequence_State) return Boolean;
   --  Sets the state of all sequences that Gesture is currently interacting
   --  with. See Gtk.Gesture.Set_Sequence_State for more details on sequence
   --  states.
   --  Since: gtk+ 3.14
   --  "state": the sequence state

   procedure Ungroup (Self : not null access Gtk_Gesture_Record);
   --  Separates Gesture into an isolated group.
   --  Since: gtk+ 3.14

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Set_State
     (Self : not null access Gtk_Gesture_Record'Class;
      State : Gtk.Enums.Gtk_Event_Sequence_State := Event_Sequence_Denied);
   pragma Inline (Set_State);
   --  Same as the function Set_State, but ignore the return value.
   --  This is in general call in a gesture callback to stop monitoring the
   --  current sequence of events. For instance, when you have a multipress
   --  gesture, you could call Set_State when the number of clicks is greater
   --  than 2 and you will never be interested in triple clicks or more.

   procedure Watch
     (Self   : not null access Gtk_Gesture_Record'Class;
      Object : not null access GObject_Record'Class);
   --  Automatically unref Self when Widget is destroyed.
   --  This is a convenience function, since attaching a gesture to a widget
   --  does not automatically free the gesture when the widget is destroyed
   --  (presumably so that you can reuse the gesture and its settings for
   --  another widget).

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   N_Points_Property : constant Glib.Properties.Property_Uint;
   --  The number of touch points that trigger recognition on this gesture,

   Window_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gdk.Window
   --  If non-null, the gesture will only listen for events that happen on
   --  this Gdk.Gdk_Window, or a child of it.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Gesture_Gdk_Event_Sequence_Void is not null access procedure
     (Self     : access Gtk_Gesture_Record'Class;
      Sequence : Gdk.Event.Gdk_Event_Sequence);

   type Cb_GObject_Gdk_Event_Sequence_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Sequence : Gdk.Event.Gdk_Event_Sequence);

   Signal_Begin : constant Glib.Signal_Name := "begin";
   procedure On_Begin
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_Gtk_Gesture_Gdk_Event_Sequence_Void;
       After : Boolean := False);
   procedure On_Begin
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_GObject_Gdk_Event_Sequence_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when the gesture is recognized. This means the
   --  number of touch sequences matches Gtk.Gesture.Gtk_Gesture:n-points, and
   --  the Gtk.Gesture.Gtk_Gesture::check handler(s) returned TRUE.
   --
   --  Note: These conditions may also happen when an extra touch (eg. a third
   --  touch on a 2-touches gesture) is lifted, in that situation Sequence
   --  won't pertain to the current set of active touches, so don't rely on
   --  this being true.

   Signal_Cancel : constant Glib.Signal_Name := "cancel";
   procedure On_Cancel
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_Gtk_Gesture_Gdk_Event_Sequence_Void;
       After : Boolean := False);
   procedure On_Cancel
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_GObject_Gdk_Event_Sequence_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted whenever a sequence is cancelled. This usually
   --  happens on active touches when Gtk.Event_Controller.Reset is called on
   --  Gesture (manually, due to grabs...), or the individual Sequence was
   --  claimed by parent widgets' controllers (see
   --  Gtk.Gesture.Set_Sequence_State).
   --
   --  Gesture must forget everything about Sequence as a reaction to this
   --  signal.

   Signal_End : constant Glib.Signal_Name := "end";
   procedure On_End
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_Gtk_Gesture_Gdk_Event_Sequence_Void;
       After : Boolean := False);
   procedure On_End
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_GObject_Gdk_Event_Sequence_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when Gesture either stopped recognizing the
   --  event sequences as something to be handled (the
   --  Gtk.Gesture.Gtk_Gesture::check handler returned False), or the number of
   --  touch sequences became higher or lower than
   --  Gtk.Gesture.Gtk_Gesture:n-points.
   --
   --  Note: Sequence might not pertain to the group of sequences that were
   --  previously triggering recognition on Gesture (ie. a just pressed touch
   --  sequence that exceeds Gtk.Gesture.Gtk_Gesture:n-points). This situation
   --  may be detected by checking through Gtk.Gesture.Handles_Sequence.

   type Cb_Gtk_Gesture_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void is not null access procedure
     (Self     : access Gtk_Gesture_Record'Class;
      Sequence : Gdk.Event.Gdk_Event_Sequence;
      State    : Gtk.Enums.Gtk_Event_Sequence_State);

   type Cb_GObject_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Sequence : Gdk.Event.Gdk_Event_Sequence;
      State    : Gtk.Enums.Gtk_Event_Sequence_State);

   Signal_Sequence_State_Changed : constant Glib.Signal_Name := "sequence-state-changed";
   procedure On_Sequence_State_Changed
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_Gtk_Gesture_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void;
       After : Boolean := False);
   procedure On_Sequence_State_Changed
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_GObject_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted whenever a sequence state changes. See
   --  Gtk.Gesture.Set_Sequence_State to know more about the expectable
   --  sequence lifetimes.
   -- 
   --  Callback parameters:
   --    --  "sequence": the Gdk.Event.Gdk_Event_Sequence that was cancelled
   --    --  "state": the new sequence state

   Signal_Update : constant Glib.Signal_Name := "update";
   procedure On_Update
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_Gtk_Gesture_Gdk_Event_Sequence_Void;
       After : Boolean := False);
   procedure On_Update
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_GObject_Gdk_Event_Sequence_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted whenever an event is handled while the gesture
   --  is recognized. Sequence is guaranteed to pertain to the set of active
   --  touches.

private
   Window_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("window");
   N_Points_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n-points");
end Gtk.Gesture;
