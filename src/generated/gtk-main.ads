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


pragma Warnings (Off, "*is already use-visible*");
with Gdk.Device;     use Gdk.Device;
with Gdk.Event;      use Gdk.Event;
with Gdk.Types;      use Gdk.Types;
with Glib;           use Glib;
with Gtk.Widget;     use Gtk.Widget;
with Pango.Language; use Pango.Language;

package Gtk.Main is

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Key_Snoop_Func is access function
     (Grab_Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Event       : Gdk.Event.Gdk_Event_Key) return Glib.Gint;
   --  Key snooper functions are called before normal event delivery. They can
   --  be used to implement custom key event handling.
   --  "grab_widget": the widget to which the event will be delivered
   --  "event": the key event

   -------------
   -- Methods --
   -------------

   function Key_Snooper_Install (Snooper : Gtk_Key_Snoop_Func) return Guint;
   pragma Obsolescent (Key_Snooper_Install);
   --  Installs a key snooper function, which will get called on all key
   --  events before delivering them normally.
   --  Deprecated since 3.4, 1
   --  "snooper": a Gtk_Key_Snoop_Func

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Init;
   --  Initialize GtkAda's internal structures.
   --  This subprogram should be called before any other one in GtkAda.
   --  If GtkAda could not be initialized (no access to the display, etc.), the
   --  application exits with an error

   function Init_Check return Boolean;
   --  Initialize GtkAda's internal structures.
   --  Return False if there was an error (no access to the display, etc.)

   ---------------
   -- Functions --
   ---------------

   function Get_Major_Version return Guint;
   --  Returns the major version number of the GTK+ library. (e.g. in GTK+
   --  version 3.1.5 this is 3.)
   --  This function is in the library, so it represents the GTK+ library your
   --  code is running against. Contrast with the GTK_MAJOR_VERSION macro,
   --  which represents the major version of the GTK+ headers you have included
   --  when compiling your code.
   --  Since: gtk+ 3.0

   function Get_Minor_Version return Guint;
   --  Returns the minor version number of the GTK+ library. (e.g. in GTK+
   --  version 3.1.5 this is 1.)
   --  This function is in the library, so it represents the GTK+ library your
   --  code is are running against. Contrast with the GTK_MINOR_VERSION macro,
   --  which represents the minor version of the GTK+ headers you have included
   --  when compiling your code.
   --  Since: gtk+ 3.0

   function Get_Micro_Version return Guint;
   --  Returns the micro version number of the GTK+ library. (e.g. in GTK+
   --  version 3.1.5 this is 5.)
   --  This function is in the library, so it represents the GTK+ library your
   --  code is are running against. Contrast with the GTK_MICRO_VERSION macro,
   --  which represents the micro version of the GTK+ headers you have included
   --  when compiling your code.
   --  Since: gtk+ 3.0

   function Get_Binary_Age return Guint;
   --  Returns the binary age as passed to `libtool` when building the GTK+
   --  library the process is running against. If `libtool` means nothing to
   --  you, don't worry about it.
   --  Since: gtk+ 3.0

   function Get_Interface_Age return Guint;
   --  Returns the interface age as passed to `libtool` when building the GTK+
   --  library the process is running against. If `libtool` means nothing to
   --  you, don't worry about it.
   --  Since: gtk+ 3.0

   function Check_Version
      (Required_Major : Guint;
       Required_Minor : Guint;
       Required_Micro : Guint) return UTF8_String;
   --  Checks that the GTK+ library in use is compatible with the given
   --  version. Generally you would pass in the constants GTK_MAJOR_VERSION,
   --  GTK_MINOR_VERSION, GTK_MICRO_VERSION as the three arguments to this
   --  function; that produces a check that the library in use is compatible
   --  with the version of GTK+ the application or module was compiled against.
   --  Compatibility is defined by two things: first the version of the
   --  running library is newer than the version
   --  Required_Major.required_minor.Required_Micro. Second the running library
   --  must be binary compatible with the version
   --  Required_Major.required_minor.Required_Micro (same major version.)
   --  This function is primarily for GTK+ modules; the module can call this
   --  function to check that it wasn't loaded into an incompatible version of
   --  GTK+. However, such a check isn't completely reliable, since the module
   --  may be linked against an old version of GTK+ and calling the old version
   --  of Gtk.Main.Check_Version, but still get loaded into an application
   --  using a newer version of GTK+.
   --  "required_major": the required major version
   --  "required_minor": the required minor version
   --  "required_micro": the required micro version

   procedure Disable_Setlocale;
   --  Prevents gtk_init, gtk_init_check, gtk_init_with_args and
   --  gtk_parse_args from automatically calling `setlocale (LC_ALL, "")`. You
   --  would want to use this function if you wanted to set the locale for your
   --  program to something other than the user's locale, or if you wanted to
   --  set different values for different locale categories.
   --  Most programs should not need to call this function.

   function Get_Default_Language return Pango.Language.Pango_Language;
   --  Returns the Pango.Language.Pango_Language for the default language
   --  currently in effect. (Note that this can change over the life of an
   --  application.) The default language is derived from the current locale.
   --  It determines, for example, whether GTK+ uses the right-to-left or
   --  left-to-right text direction.
   --  This function is equivalent to Pango.Language.Get_Default. See that
   --  function for details.

   function Events_Pending return Boolean;
   --  Checks if any events are pending.
   --  This can be used to update the UI and invoke timeouts etc. while doing
   --  some time intensive computation.
   --  ## Updating the UI during a long computation
   --  |[<!-- language="C" --> // computation going on...
   --  while (gtk_events_pending ()) gtk_main_iteration ();
   --  // ...computation continued ]|

   procedure Main_Do_Event (Event : Gdk.Event.Gdk_Event);
   --  Processes a single GDK event.
   --  This is public only to allow filtering of events between GDK and GTK+.
   --  You will not usually need to call this function directly.
   --  While you should not call this function directly, you might want to
   --  know how exactly events are handled. So here is what this function does
   --  with the event:
   --  1. Compress enter/leave notify events. If the event passed build an
   --  enter/leave pair together with the next event (peeked from GDK), both
   --  events are thrown away. This is to avoid a backlog of (de-)highlighting
   --  widgets crossed by the pointer.
   --  2. Find the widget which got the event. If the widget can't be
   --  determined the event is thrown away unless it belongs to a INCR
   --  transaction.
   --  3. Then the event is pushed onto a stack so you can query the currently
   --  handled event with Gtk.Main.Get_Current_Event.
   --  4. The event is sent to a widget. If a grab is active all events for
   --  widgets that are not in the contained in the grab widget are sent to the
   --  latter with a few exceptions: - Deletion and destruction events are
   --  still sent to the event widget for obvious reasons. - Events which
   --  directly relate to the visual representation of the event widget. -
   --  Leave events are delivered to the event widget if there was an enter
   --  event delivered to it before without the paired leave event. - Drag
   --  events are not redirected because it is unclear what the semantics of
   --  that would be. Another point of interest might be that all key events
   --  are first passed through the key snooper functions if there are any.
   --  Read the description of Gtk.Main.Key_Snooper_Install if you need this
   --  feature.
   --  5. After finishing the delivery the event is popped from the event
   --  stack.
   --  "event": An event to process (normally passed by GDK)

   procedure Main;
   --  Runs the main loop until Gtk.Main.Main_Quit is called.
   --  You can nest calls to Gtk.Main.Main. In that case Gtk.Main.Main_Quit
   --  will make the innermost invocation of the main loop return.

   function Main_Level return Guint;
   --  Asks for the current nesting level of the main loop.

   procedure Main_Quit;
   --  Makes the innermost invocation of the main loop return when it regains
   --  control.

   function Main_Iteration return Boolean;
   --  Runs a single iteration of the mainloop.
   --  If no events are waiting to be processed GTK+ will block until the next
   --  event is noticed. If you don't want to block look at
   --  Gtk.Main.Main_Iteration_Do or check if any events are pending with
   --  Gtk.Main.Events_Pending first.

   function Main_Iteration_Do (Blocking : Boolean) return Boolean;
   --  Runs a single iteration of the mainloop. If no events are available
   --  either return or block depending on the value of Blocking.
   --  "blocking": True if you want GTK+ to block if no events are pending

   function True return Boolean;
   --  All this function does it to return True.
   --  This can be useful for example if you want to inhibit the deletion of a
   --  window. Of course you should not do this as the user expects a reaction
   --  from clicking the close icon of the window...
   --  ## A persistent window
   --  |[<!-- language="C" --> include <gtk/gtk.h>
   --  int main (int argc, char **argv) { GtkWidget *win, *but; const char
   --  *text = "Close yourself. I mean it!";
   --  gtk_init (&argc, &argv);
   --  win = gtk_window_new (GTK_WINDOW_TOPLEVEL); g_signal_connect (win,
   --  "delete-event", G_CALLBACK (gtk_true), NULL); g_signal_connect (win,
   --  "destroy", G_CALLBACK (gtk_main_quit), NULL);
   --  but = gtk_button_new_with_label (text); g_signal_connect_swapped (but,
   --  "clicked", G_CALLBACK (gtk_object_destroy), win); gtk_container_add
   --  (GTK_CONTAINER (win), but);
   --  gtk_widget_show_all (win);
   --  gtk_main ();
   --  return 0; } ]|

   function False return Boolean;
   --  Analogical to Gtk.Main.True, this function does nothing but always
   --  returns False.

   function Grab_Get_Current return Gtk.Widget.Gtk_Widget;
   --  Queries the current grab of the default window group.

   procedure Device_Grab_Add
      (Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Device       : not null access Gdk.Device.Gdk_Device_Record'Class;
       Block_Others : Boolean);
   --  Adds a GTK+ grab on Device, so all the events on Device and its
   --  associated pointer or keyboard (if any) are delivered to Widget. If the
   --  Block_Others parameter is True, any other devices will be unable to
   --  interact with Widget during the grab.
   --  Since: gtk+ 3.0
   --  "widget": a Gtk.Widget.Gtk_Widget
   --  "device": a Gdk.Device.Gdk_Device to grab on.
   --  "block_others": True to prevent other devices to interact with Widget.

   procedure Device_Grab_Remove
      (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class);
   --  Removes a device grab from the given widget.
   --  You have to pair calls to Gtk.Main.Device_Grab_Add and
   --  Gtk.Main.Device_Grab_Remove.
   --  Since: gtk+ 3.0
   --  "widget": a Gtk.Widget.Gtk_Widget
   --  "device": a Gdk.Device.Gdk_Device

   procedure Key_Snooper_Remove (Snooper_Handler_Id : Guint);
   pragma Obsolescent (Key_Snooper_Remove);
   --  Removes the key snooper function with the given id.
   --  Deprecated since 3.4, 1
   --  "snooper_handler_id": Identifies the key snooper to remove

   function Get_Current_Event return Gdk.Event.Gdk_Event;
   --  Obtains a copy of the event currently being processed by GTK+.
   --  For example, if you are handling a Gtk.Button.Gtk_Button::clicked
   --  signal, the current event will be the Gdk.Event.Gdk_Event_Button that
   --  triggered the ::clicked signal.

   function Get_Current_Event_Time return Guint32;
   --  If there is a current event and it has a timestamp, return that
   --  timestamp, otherwise return GDK_CURRENT_TIME.

   procedure Get_Current_Event_State
      (State             : out Gdk.Types.Gdk_Modifier_Type;
       Has_Current_Event : out Boolean);
   --  If there is a current event and it has a state field, place that state
   --  field in State and return True, otherwise return False.
   --  "state": a location to store the state of the current event

   function Get_Current_Event_Device return Gdk.Device.Gdk_Device;
   --  If there is a current event and it has a device, return that device,
   --  otherwise return null.

   function Get_Event_Widget
      (Event : Gdk.Event.Gdk_Event) return Gtk.Widget.Gtk_Widget;
   --  If Event is null or the event was not associated with any widget,
   --  returns null, otherwise returns the widget that received the event
   --  originally.
   --  "event": a Gdk.Event.Gdk_Event

   procedure Propagate_Event
      (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Event  : Gdk.Event.Gdk_Event);
   --  Sends an event to a widget, propagating the event to parent widgets if
   --  the event remains unhandled.
   --  Events received by GTK+ from GDK normally begin in
   --  Gtk.Main.Main_Do_Event. Depending on the type of event, existence of
   --  modal dialogs, grabs, etc., the event may be propagated; if so, this
   --  function is used.
   --  Gtk.Main.Propagate_Event calls Gtk.Widget.Event on each widget it
   --  decides to send the event to. So Gtk.Widget.Event is the lowest-level
   --  function; it simply emits the Gtk.Widget.Gtk_Widget::event and possibly
   --  an event-specific signal on a widget. Gtk.Main.Propagate_Event is a bit
   --  higher-level, and Gtk.Main.Main_Do_Event is the highest level.
   --  All that said, you most likely don't want to use any of these
   --  functions; synthesizing events is rarely needed. There are almost
   --  certainly better ways to achieve your goals. For example, use
   --  Gdk.Window.Invalidate_Rect or Gtk.Widget.Queue_Draw instead of making up
   --  expose events.
   --  "widget": a Gtk.Widget.Gtk_Widget
   --  "event": an event

end Gtk.Main;
