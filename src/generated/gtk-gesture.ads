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

--  The base class for gesture recognition.
--
--  Although `GtkGesture` is quite generalized to serve as a base for
--  multi-touch gestures, it is suitable to implement single-touch and
--  pointer-based gestures (using the special null `GdkEventSequence` value for
--  these).
--
--  The number of touches that a `GtkGesture` need to be recognized is
--  controlled by the [propertyGtk.Gesture:n-points] property, if a gesture is
--  keeping track of less or more than that number of sequences, it won't check
--  whether the gesture is recognized.
--
--  As soon as the gesture has the expected number of touches, it will check
--  regularly if it is recognized, the criteria to consider a gesture as
--  "recognized" is left to `GtkGesture` subclasses.
--
--  A recognized gesture will then emit the following signals:
--
--  - [signalGtk.Gesture::begin] when the gesture is recognized. -
--  [signalGtk.Gesture::update], whenever an input event is processed. -
--  [signalGtk.Gesture::end] when the gesture is no longer recognized.
--
--  ## Event propagation
--
--  In order to receive events, a gesture needs to set a propagation phase
--  through [methodGtk.EventController.set_propagation_phase].
--
--  In the capture phase, events are propagated from the toplevel down to the
--  target widget, and gestures that are attached to containers above the
--  widget get a chance to interact with the event before it reaches the
--  target.
--
--  In the bubble phase, events are propagated up from the target widget to
--  the toplevel, and gestures that are attached to containers above the widget
--  get a chance to interact with events that have not been handled yet.
--
--  ## States of a sequence
--
--  Whenever input interaction happens, a single event may trigger a cascade
--  of `GtkGesture`s, both across the parents of the widget receiving the event
--  and in parallel within an individual widget. It is a responsibility of the
--  widgets using those gestures to set the state of touch sequences
--  accordingly in order to enable cooperation of gestures around the
--  `GdkEventSequence`s triggering those.
--
--  Within a widget, gestures can be grouped through
--  [methodGtk.Gesture.group]. Grouped gestures synchronize the state of
--  sequences, so calling [methodGtk.Gesture.set_state] on one will effectively
--  propagate the state throughout the group.
--
--  By default, all sequences start out in the Gtk.Enums.Event_Sequence_None
--  state, sequences in this state trigger the gesture event handler, but event
--  propagation will continue unstopped by gestures.
--
--  If a sequence enters into the Gtk.Enums.Event_Sequence_Denied state, the
--  gesture group will effectively ignore the sequence, letting events go
--  unstopped through the gesture, but the "slot" will still remain occupied
--  while the touch is active.
--
--  If a sequence enters in the Gtk.Enums.Event_Sequence_Claimed state, the
--  gesture group will grab all interaction on the sequence, by:
--
--  - Setting the same sequence to Gtk.Enums.Event_Sequence_Denied on every
--  other gesture group within the widget, and every gesture on parent widgets
--  in the propagation chain. - Emitting [signalGtk.Gesture::cancel] on every
--  gesture in widgets underneath in the propagation chain. - Stopping event
--  propagation after the gesture group handles the event.
--
--  Note: if a sequence is set early to Gtk.Enums.Event_Sequence_Claimed on
--  GDK_TOUCH_BEGIN/GDK_BUTTON_PRESS (so those events are captured before
--  reaching the event widget, this implies Gtk.Enums.Phase_Capture), one
--  similar event will be emulated if the sequence changes to
--  Gtk.Enums.Event_Sequence_Denied. This way event coherence is preserved
--  before event propagation is unstopped again.
--
--  Sequence states can't be changed freely. See [methodGtk.Gesture.set_state]
--  to know about the possible lifetimes of a `GdkEventSequence`.
--
--  ## Touchpad gestures
--
--  On the platforms that support it, `GtkGesture` will handle transparently
--  touchpad gesture events. The only precautions users of `GtkGesture` should
--  do to enable this support are:
--
--  - If the gesture has Gtk.Enums.Phase_None, ensuring events of type
--  GDK_TOUCHPAD_SWIPE and GDK_TOUCHPAD_PINCH are handled by the `GtkGesture`

pragma Warnings (Off, "*is already use-visible*");
with Gdk;                  use Gdk;
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
       Rect : out Gdk.Rectangle.Gdk_Rectangle) return Boolean;
   --  If there are touch sequences being currently handled by Gesture,
   --  returns True and fills in Rect with the bounding box containing all
   --  active touches.
   --  Otherwise, False will be returned.
   --  Note: This function will yield unexpected results on touchpad gestures.
   --  Since there is no correlation between physical and pixel distances,
   --  these will look as if constrained in an infinitely small area, Rect
   --  width and height will thus be 0 regardless of the number of touchpoints.
   --  @param Rect bounding box containing all active touches.
   --  @return True if there are active touches, False otherwise

   function Get_Bounding_Box_Center
      (Self : not null access Gtk_Gesture_Record;
       X    : out Gdouble;
       Y    : out Gdouble) return Boolean;
   --  If there are touch sequences being currently handled by Gesture,
   --  returns True and fills in X and Y with the center of the bounding box
   --  containing all active touches.
   --  Otherwise, False will be returned.
   --  @param X X coordinate for the bounding box center
   --  @param Y Y coordinate for the bounding box center
   --  @return False if no active touches are present, True otherwise

   function Get_Device
      (Self : not null access Gtk_Gesture_Record) return Gdk.Gdk_Device;
   --  Returns the logical `GdkDevice` that is currently operating on Gesture.
   --  This returns null if the gesture is not being interacted.
   --  @return a `GdkDevice`
   --  Return has transfer-ownership='none'

   function Get_Group
      (Self : not null access Gtk_Gesture_Record) return Gesture_List.Glist;
   --  Returns all gestures in the group of Gesture

   procedure Group
      (Self    : not null access Gtk_Gesture_Record;
       Gesture : not null access Gtk_Gesture_Record'Class);
   --  Adds Gesture to the same group than Group_Gesture.
   --  Gestures are by default isolated in their own groups.
   --  Both gestures must have been added to the same widget before they can
   --  be grouped.
   --  When gestures are grouped, the state of `GdkEventSequences` is kept in
   --  sync for all of those, so calling
   --  [methodGtk.Gesture.set_sequence_state], on one will transfer the same
   --  value to the others.
   --  Groups also perform an "implicit grabbing" of sequences, if a
   --  `GdkEventSequence` state is set to Gtk.Enums.Event_Sequence_Claimed on
   --  one group, every other gesture group attached to the same `GtkWidget`
   --  will switch the state for that sequence to
   --  Gtk.Enums.Event_Sequence_Denied.
   --  @param Gesture a `GtkGesture`

   function Get_Last_Event
      (Self     : not null access Gtk_Gesture_Record;
       Sequence : Gdk.Event.Gdk_Event_Sequence) return Gdk.Event.Gdk_Event;
   --  Returns the last event that was processed for Sequence.
   --  Note that the returned pointer is only valid as long as the Sequence is
   --  still interpreted by the Gesture. If in doubt, you should make a copy of
   --  the event.
   --  @param Sequence a `GdkEventSequence`
   --  @return The last event from Sequence
   --  Return has transfer-ownership='none'

   function Get_Last_Updated_Sequence
      (Self : not null access Gtk_Gesture_Record)
       return Gdk.Event.Gdk_Event_Sequence;
   --  Returns the `GdkEventSequence` that was last updated on Gesture.
   --  @return The last updated sequence

   function Get_Point
      (Self     : not null access Gtk_Gesture_Record;
       Sequence : Gdk.Event.Gdk_Event_Sequence;
       X        : access Gdouble := null;
       Y        : access Gdouble := null) return Boolean;
   --  If Sequence is currently being interpreted by Gesture, returns True and
   --  fills in X and Y with the last coordinates stored for that event
   --  sequence.
   --  The coordinates are always relative to the widget allocation.
   --  @param Sequence a `GdkEventSequence`, or null for pointer events
   --  @param X return location for X axis of the sequence coordinates
   --  @param Y return location for Y axis of the sequence coordinates
   --  @return True if Sequence is currently interpreted

   function Get_Sequence_State
      (Self     : not null access Gtk_Gesture_Record;
       Sequence : Gdk.Event.Gdk_Event_Sequence)
       return Gtk.Enums.Gtk_Event_Sequence_State;
   --  Returns the Sequence state, as seen by Gesture.
   --  @param Sequence a `GdkEventSequence`
   --  @return The sequence state in Gesture

   function Set_Sequence_State
      (Self     : not null access Gtk_Gesture_Record;
       Sequence : Gdk.Event.Gdk_Event_Sequence;
       State    : Gtk.Enums.Gtk_Event_Sequence_State) return Boolean;
   pragma Obsolescent (Set_Sequence_State);
   --  Sets the state of Sequence in Gesture.
   --  Sequences start in state Gtk.Enums.Event_Sequence_None, and whenever
   --  they change state, they can never go back to that state. Likewise,
   --  sequences in state Gtk.Enums.Event_Sequence_Denied cannot turn back to a
   --  not denied state. With these rules, the lifetime of an event sequence is
   --  constrained to the next four:
   --  * None * None → Denied * None → Claimed * None → Claimed → Denied
   --  Note: Due to event handling ordering, it may be unsafe to set the state
   --  on another gesture within a [signalGtk.Gesture::begin] signal handler,
   --  as the callback might be executed before the other gesture knows about
   --  the sequence. A safe way to perform this could be:
   --  ```c static void first_gesture_begin_cb (GtkGesture *first_gesture,
   --  GdkEventSequence *sequence, gpointer user_data) {
   --  gtk_gesture_set_sequence_state (first_gesture, sequence,
   --  GTK_EVENT_SEQUENCE_CLAIMED); gtk_gesture_set_sequence_state
   --  (second_gesture, sequence, GTK_EVENT_SEQUENCE_DENIED); }
   --  static void second_gesture_begin_cb (GtkGesture *second_gesture,
   --  GdkEventSequence *sequence, gpointer user_data) { if
   --  (gtk_gesture_get_sequence_state (first_gesture, sequence) ==
   --  GTK_EVENT_SEQUENCE_CLAIMED) gtk_gesture_set_sequence_state
   --  (second_gesture, sequence, GTK_EVENT_SEQUENCE_DENIED); } ```
   --  If both gestures are in the same group, just set the state on the
   --  gesture emitting the event, the sequence will be already be initialized
   --  to the group's global state when the second gesture processes the event.
   --  Deprecated since 4.10., 1
   --  @param Sequence a `GdkEventSequence`
   --  @param State the sequence state
   --  @return True if Sequence is handled by Gesture, and the state is
   --  changed successfully

   function Get_Sequences
      (Self : not null access Gtk_Gesture_Record)
       return Gdk_Event_Sequence_List.Glist;
   --  Returns the list of `GdkEventSequences` currently being interpreted by
   --  Gesture.

   function Handles_Sequence
      (Self     : not null access Gtk_Gesture_Record;
       Sequence : Gdk.Event.Gdk_Event_Sequence) return Boolean;
   --  Returns True if Gesture is currently handling events corresponding to
   --  Sequence.
   --  @param Sequence a `GdkEventSequence`
   --  @return True if Gesture is handling Sequence, False otherwise

   function Is_Active
      (Self : not null access Gtk_Gesture_Record) return Boolean;
   --  Returns True if the gesture is currently active.
   --  A gesture is active while there are touch sequences interacting with
   --  it.
   --  @return True if gesture is active

   function Is_Grouped_With
      (Self  : not null access Gtk_Gesture_Record;
       Other : not null access Gtk_Gesture_Record'Class) return Boolean;
   --  Returns True if both gestures pertain to the same group.
   --  @param Other another `GtkGesture`
   --  @return whether the gestures are grouped

   function Is_Recognized
      (Self : not null access Gtk_Gesture_Record) return Boolean;
   --  Returns True if the gesture is currently recognized.
   --  A gesture is recognized if there are as many interacting touch
   --  sequences as required by Gesture.
   --  @return True if gesture is recognized

   function Set_State
      (Self  : not null access Gtk_Gesture_Record;
       State : Gtk.Enums.Gtk_Event_Sequence_State) return Boolean;
   --  Sets the state of all sequences that Gesture is currently interacting
   --  with.
   --  Sequences start in state Gtk.Enums.Event_Sequence_None, and whenever
   --  they change state, they can never go back to that state. Likewise,
   --  sequences in state Gtk.Enums.Event_Sequence_Denied cannot turn back to a
   --  not denied state. With these rules, the lifetime of an event sequence is
   --  constrained to the next four:
   --  * None * None → Denied * None → Claimed * None → Claimed → Denied
   --  Note: Due to event handling ordering, it may be unsafe to set the state
   --  on another gesture within a [signalGtk.Gesture::begin] signal handler,
   --  as the callback might be executed before the other gesture knows about
   --  the sequence. A safe way to perform this could be:
   --  ```c static void first_gesture_begin_cb (GtkGesture *first_gesture,
   --  GdkEventSequence *sequence, gpointer user_data) { gtk_gesture_set_state
   --  (first_gesture, GTK_EVENT_SEQUENCE_CLAIMED); gtk_gesture_set_state
   --  (second_gesture, GTK_EVENT_SEQUENCE_DENIED); }
   --  static void second_gesture_begin_cb (GtkGesture *second_gesture,
   --  GdkEventSequence *sequence, gpointer user_data) { if
   --  (gtk_gesture_get_sequence_state (first_gesture, sequence) ==
   --  GTK_EVENT_SEQUENCE_CLAIMED) gtk_gesture_set_state (second_gesture,
   --  GTK_EVENT_SEQUENCE_DENIED); } ```
   --  If both gestures are in the same group, just set the state on the
   --  gesture emitting the event, the sequence will be already be initialized
   --  to the group's global state when the second gesture processes the event.
   --  @param State the sequence state
   --  @return True if the state of at least one sequence was changed
   --  successfully

   procedure Ungroup (Self : not null access Gtk_Gesture_Record);
   --  Separates Gesture into an isolated group.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   N_Points_Property : constant Glib.Properties.Property_Uint;
   --  The number of touch points that trigger recognition on this gesture.

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
   --  Emitted when the gesture is recognized.
   --
   --  This means the number of touch sequences matches
   --  [propertyGtk.Gesture:n-points].
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
   --  Emitted whenever a sequence is cancelled.
   --
   --  This usually happens on active touches when
   --  [methodGtk.EventController.reset] is called on Gesture (manually, due to
   --  grabs...), or the individual Sequence was claimed by parent widgets'
   --  controllers (see [methodGtk.Gesture.set_sequence_state]).
   --
   --  Gesture must forget everything about Sequence as in response to this
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
   --  Emitted when Gesture either stopped recognizing the event sequences as
   --  something to be handled, or the number of touch sequences became higher
   --  or lower than [propertyGtk.Gesture:n-points].
   --
   --  Note: Sequence might not pertain to the group of sequences that were
   --  previously triggering recognition on Gesture (ie. a just pressed touch
   --  sequence that exceeds [propertyGtk.Gesture:n-points]). This situation
   --  may be detected by checking through
   --  [methodGtk.Gesture.handles_sequence].

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
   --  Emitted whenever a sequence state changes.
   --
   --  See [methodGtk.Gesture.set_sequence_state] to know more about the
   --  expectable sequence lifetimes.
   -- 
   --  Callback parameters:
   --    --  @param Sequence the `GdkEventSequence` that was cancelled
   --    --  @param State the new sequence state

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
   --  Emitted whenever an event is handled while the gesture is recognized.
   --
   --  Sequence is guaranteed to pertain to the set of active touches.

private
   N_Points_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n-points");
end Gtk.Gesture;
