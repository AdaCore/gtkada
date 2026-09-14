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

--  Recognizes click gestures.
--
--  It is able to recognize multiple clicks on a nearby zone, which can be
--  listened for through the [signalGtk.GestureClick::pressed] signal. Whenever
--  time or distance between clicks exceed the GTK defaults,
--  [signalGtk.GestureClick::stopped] is emitted, and the click counter is
--  reset.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Event;          use Gdk.Event;
with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Gtk.Gesture_Single; use Gtk.Gesture_Single;

package Gtk.Gesture_Click is

   type Gtk_Gesture_Click_Record is new Gtk_Gesture_Single_Record with null record;
   type Gtk_Gesture_Click is access all Gtk_Gesture_Click_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Gesture_Click);
   procedure Initialize
      (Self : not null access Gtk_Gesture_Click_Record'Class);
   --  Returns a newly created `GtkGesture` that recognizes single and
   --  multiple presses.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Gesture_Click_New return Gtk_Gesture_Click;
   --  Returns a newly created `GtkGesture` that recognizes single and
   --  multiple presses.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_gesture_click_get_type");

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Gesture_Click_Gint_Gdouble_Gdouble_Void is not null access procedure
     (Self    : access Gtk_Gesture_Click_Record'Class;
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
      (Self  : not null access Gtk_Gesture_Click_Record;
       Call  : Cb_Gtk_Gesture_Click_Gint_Gdouble_Gdouble_Void;
       After : Boolean := False);
   procedure On_Pressed
      (Self  : not null access Gtk_Gesture_Click_Record;
       Call  : Cb_GObject_Gint_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted whenever a button or touch press happens.
   -- 
   --  Callback parameters:
   --    --  @param N_Press how many touch/button presses happened with this one
   --    --  @param X The X coordinate, in widget allocation coordinates
   --    --  @param Y The Y coordinate, in widget allocation coordinates

   Signal_Released : constant Glib.Signal_Name := "released";
   procedure On_Released
      (Self  : not null access Gtk_Gesture_Click_Record;
       Call  : Cb_Gtk_Gesture_Click_Gint_Gdouble_Gdouble_Void;
       After : Boolean := False);
   procedure On_Released
      (Self  : not null access Gtk_Gesture_Click_Record;
       Call  : Cb_GObject_Gint_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a button or touch is released.
   --
   --  N_Press will report the number of press that is paired to this event,
   --  note that [signalGtk.GestureClick::stopped] may have been emitted
   --  between the press and its release, N_Press will only start over at the
   --  next press.
   -- 
   --  Callback parameters:
   --    --  @param N_Press number of press that is paired with this release
   --    --  @param X The X coordinate, in widget allocation coordinates
   --    --  @param Y The Y coordinate, in widget allocation coordinates

   type Cb_Gtk_Gesture_Click_Void is not null access procedure
     (Self : access Gtk_Gesture_Click_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Stopped : constant Glib.Signal_Name := "stopped";
   procedure On_Stopped
      (Self  : not null access Gtk_Gesture_Click_Record;
       Call  : Cb_Gtk_Gesture_Click_Void;
       After : Boolean := False);
   procedure On_Stopped
      (Self  : not null access Gtk_Gesture_Click_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted whenever any time/distance threshold has been exceeded.

   type Cb_Gtk_Gesture_Click_Gdouble_Gdouble_Guint_Gdk_Event_Sequence_Void is not null access procedure
     (Self     : access Gtk_Gesture_Click_Record'Class;
      X        : Gdouble;
      Y        : Gdouble;
      Button   : Guint;
      Sequence : Gdk.Event.Gdk_Event_Sequence);

   type Cb_GObject_Gdouble_Gdouble_Guint_Gdk_Event_Sequence_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      X        : Gdouble;
      Y        : Gdouble;
      Button   : Guint;
      Sequence : Gdk.Event.Gdk_Event_Sequence);

   Signal_Unpaired_Release : constant Glib.Signal_Name := "unpaired-release";
   procedure On_Unpaired_Release
      (Self  : not null access Gtk_Gesture_Click_Record;
       Call  : Cb_Gtk_Gesture_Click_Gdouble_Gdouble_Guint_Gdk_Event_Sequence_Void;
       After : Boolean := False);
   procedure On_Unpaired_Release
      (Self  : not null access Gtk_Gesture_Click_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Guint_Gdk_Event_Sequence_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted whenever the gesture receives a release event that had no
   --  previous corresponding press.
   --
   --  Due to implicit grabs, this can only happen on situations where input
   --  is grabbed elsewhere mid-press or the pressed widget voluntarily
   --  relinquishes its implicit grab.
   -- 
   --  Callback parameters:
   --    --  @param X X coordinate of the event
   --    --  @param Y Y coordinate of the event
   --    --  @param Button Button being released
   --    --  @param Sequence Sequence being released

end Gtk.Gesture_Click;
