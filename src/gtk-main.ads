------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2012, AdaCore                     --
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
--  This package contains top-level subprograms that are used to initialize
--  GtkAda and interact with the main event loop.
--
--  It also provides a set of packages to set up idle functions, timeout
--  functions, and functions to be called before and after entering the
--  main loop.
--  </description>
--  <c_version>2.8.17</c_version>

with Gdk.Event;
with Gdk.Types;
with Gtk.Widget;
with Pango.Font;
with System;

package Gtk.Main is
   pragma Elaborate_Body;

   --------------------------------------
   -- Initialization and exit routines --
   --------------------------------------

   procedure Init;
   --  Initialize GtkAda's internal structures.
   --  This subprogram should be called before any other one in GtkAda.
   --  If GtkAda could not be initialized (no access to the display, etc.), the
   --  application exits with an error

   function Init_Check return Boolean;
   --  Initialize GtkAda's internal structures.
   --  Return False if there was an error (no access to the display, etc.)

   procedure Disable_Setlocale;
   --  Prevents Init, Init_Check and Parse_Args from automatic calling
   --  Set_Locale (LC_ALL, ""). You would want to use this function if you
   --  wanted to set the locale for your program to something other than the
   --  user's locale, or if you wanted to set different values for different
   --  locale categories.
   --
   --  Most programs should not need to call this function.

   function Check_Version
     (Required_Major : Guint := Gtk.Major_Version;
      Required_Minor : Guint := Gtk.Minor_Version;
      Required_Micro : Guint := Gtk.Micro_Version)
      return String;
   --  Checks that the GTK+ library in use is compatible with the given
   --  version. Generally you would pass in the constants Gtk.Major_Version,
   --  Gtk.Minor_Version, Gtk.Micro_Version as the three arguments to this
   --  function; that produces a check that the library in use is compatible
   --  with the version of GTK+ the application or module was compiled against.
   --
   --  Compatibility is defined by two things: first the version of the running
   --  library is newer than the version
   --  required_major.required_minor.required_micro. Second the running library
   --  must be binary compatible with the version
   --  required_major.required_minor.required_micro (same major version.)
   --
   --  This function is primarily for GTK+ modules; the module can call this
   --  function to check that it wasn't loaded into an incompatible version of
   --  GTK+. However, such a check isn't completely reliable, since the module
   --  may be linked against an old version of GTK+ and calling the old version
   --  of gtk_check_version(), but still get loaded into an application using a
   --  newer version of GTK+.
   --
   --  Return value: %NULL if the GTK+ library is compatible with the given
   --  version, or a string describing the version mismatch.

   function Get_Default_Language return Pango.Font.Pango_Language;
   --  Returns the Pango_Language for the default language currently in
   --  effect. (Note that this can change over the life of an
   --  application.)  The default language is derived from the current
   --  locale. It determines, for example, whether GTK+ uses the
   --  right-to-left or left-to-right text direction.

   -------------------
   -- The main loop --
   -------------------

   function Events_Pending return Boolean;
   --  Return True if there are some events waiting in the event queue.

   procedure Main;
   --  Start the main loop, and returns only when the main loop is exited.
   --  This subprogram can be called recursively, to start new internal
   --  loops. Each of these loops is exited through a call to Main_Quit.
   --  This is the recommended method to use when you want to popup a dialog
   --  and wait for the user answer before going any further.
   --  Note that this procedure can only be called within a single task.

   function Main_Level return Guint;
   --  Return the level of the current main loop.
   --  Since there can be nested loops, this returns the depth of the
   --  current one, starting from 1 (0 if there is none).

   procedure Main_Quit;
   --  Quit the current main loop.
   --  If this was the last active main loop, no more events will be processed
   --  by GtkAda.

   function Main_Iteration (Blocking : Boolean := True) return Boolean;
   --  Do one iteration of the main loop.
   --  Blocking indicates whether GtkAda should wait for an event to be
   --  available, or simply exit if there is none.
   --  Returns True if no main loop is running (ie Main_Quite was called for
   --  the innermost main loop).
   --  When doing some heavy calculations in an application, it is recommended
   --  that you check from time to time if there are any events pending and
   --  process them, so that your application still reacts to events.
   --  To do that, you would add a loop like:
   --
   --    while Gtk.Main.Events_Pending loop
   --        Dead := Gtk.Main.Main_Iteration;
   --    end loop;

   procedure Do_Event (Event : Gdk.Event.Gdk_Event);
   --  Process Event as if it was in the event queue.
   --  This function should almost never be used in your own application, this
   --  is the core function for event processing in GtkAda.
   --  The user should not free Event, this is already done by GtkAda.
   --
   --  While you should not call this function directly, you might want to know
   --  how exactly events are handled. So here is what this function does with
   --  the event:
   --  * Compress enter/leave notify events. If the event passed build an
   --    enter/leave pair together with the next event (peeked from GDK) both
   --    events are thrown away. This is to avoid a backlog of
   --    (de-)highlighting widgets crossed by the pointer.
   --
   --  * Find the widget which got the event. If the widget can't be determined
   --    the event is thrown away unless it belongs to a INCR transaction. In
   --    that case it is passed to gtk_selection_incr_event().
   --
   --  * Then the event is passed on a stack so you can query the currently
   --    handled event with gtk_get_current_event().
   --
   --  * The event is sent to a widget. If a grab is active all events for
   --    widgets that are not in the container in the grab widget are sent to
   --    the latter with a few exceptions:
   --       - Deletion and destruction events are still sent to the event
   --         widget for obvious reasons.
   --       - Events which directly relate to the visual representation of the
   --         event widget.
   --       - Leave events are delivered to the event widget if there was an
   --         enter event delivered to it before without the paired leave event
   --       - Drag events are not redirected because it is unclear what the
   --         semantics of that would be.
   --       - Another point of interest might be that all key events are first
   --         passed through the key snooper functions if there are any. Read
   --         the description of Key_Snooper_Install if you need this
   --         feature.
   --
   --  * After finishing the delivery the event is popped from the event stack.

   procedure Propagate_Event
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event);
   --  Sends an event to a widget, propagating the event to parent widgets
   --  if the event remains unhandled. Events received by GTK+ from GDK
   --  normally begin in Do_Event. Depending on the type of
   --  event, existence of modal dialogs, grabs, etc., the event may be
   --  propagated; if so, this function is used. Propagate_Event
   --  calls Gtk.Widget.Event on each widget it decides to send the
   --  event to.  So Gtk.Widget.Event is the lowest-level function; it
   --  simply emits the "event" and possibly an event-specific signal on a
   --  widget. Propagate_Event is a bit higher-level, and
   --  Do_Event is the highest level.
   --
   --  All that said, you most likely don't want to use any of these
   --  functions; synthesizing events is rarely needed. Consider asking on
   --  the mailing list for better ways to achieve your goals. For
   --  example, use gdk_window_invalidate_rect() or
   --  gtk_widget_queue_draw() instead of making up expose events.

   function Get_Event_Widget
     (Event : Gdk.Event.Gdk_Event) return Gtk.Widget.Gtk_Widget;
   --  Return the widget to which Event applies.

   function Get_Current_Event return Gdk.Event.Gdk_Event;
   --  Return a copy of the event being processed by gtk+. The returned
   --  value must be freed by the caller.
   --  If there is no current event, null is returned.

   procedure Get_Current_Event_State
     (State             : out Gdk.Types.Gdk_Modifier_Type;
      Had_Current_Event : out Boolean);
   --  If there is a current event and it has a state field, place
   --  that state field in State and set Had_Current_Event to True, otherwise
   --  to False.

   function Get_Current_Event_Time return Guint32;
   --  If there is a current event and it has a timestamp, return that
   --  timestamp, otherwise return Gdk.Types.Current_Time

   ----------
   -- Keys --
   ----------

   type Key_Snooper_Func is
     access function (Widget : System.Address;
                      Event  : Gdk.Event.Gdk_Event_Key;
                      Data   : System.Address) return Gboolean;
   pragma Convention (C, Key_Snooper_Func);
   --  This function is called before normal event delivery, and can be used to
   --  implement custom key event handling (for instance to create macros, or
   --  any other advanced feature).
   --  Since this is a fairly low-level function, no high-level interface is
   --  provided, and you need to convert Widget yourself to the appropriate
   --  Gtk_Widget type, with, for instance:
   --      Ada_Widget := Gtk.Widget.Convert (Widget);
   --  This function should return True to stop further event processing by
   --  gtk+ (ie no widget will receive it), or False to continue with normal
   --  event processing (for instance when you have handled the key).

   type Key_Snooper_Id is new Guint;

   function Key_Snooper_Install
     (Snooper   : Key_Snooper_Func;
      Func_Data : System.Address) return Key_Snooper_Id;
   --  Install a new key snooper function, which will get called before events
   --  are delivered normally.

   procedure Key_Snooper_Remove
     (Snooper_Handler_Id : Key_Snooper_Id);
   --  Remove the snooper with the given Id

   --------------------
   -- Grab functions --
   --------------------

   procedure Grab_Add (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Add a new widget to the grab list.
   --  The widget at the front of this list gets all the events even if it does
   --  not have the focus. This feature should be used with care.
   --  If you want a whole window to get the events, it is better to use
   --  Gtk.Window.Set_Modal instead which does the grabbing and ungrabbing for
   --  you.
   --  The grab is only done for the application. Events outside the
   --  application are still sent to their respective windows.
   --
   --  See also Gtk.Window.Gtk_Window_Group

   procedure Grab_Remove (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Remove a widget from the grab list.

   function Grab_Get_Current return Gtk.Widget.Gtk_Widget;
   --  Return the widget that currently has the focus.

private
   pragma Import (C, Main_Level, "gtk_main_level");
   pragma Import (C, Main_Quit, "gtk_main_quit");
   pragma Import (C, Main, "gtk_main");
   pragma Import (C, Get_Current_Event, "gtk_get_current_event");
   pragma Import (C, Disable_Setlocale, "gtk_disable_setlocale");
   pragma Import (C, Get_Current_Event_Time, "gtk_get_current_event_time");
   pragma Import (C, Get_Default_Language, "gtk_get_default_language");
   pragma Import (C, Key_Snooper_Remove, "gtk_key_snooper_remove");
   pragma Import (C, Key_Snooper_Install, "gtk_key_snooper_install");

   --  The following two subprograms are specific to Win32
   --  No binding: gtk_init_abi_check
   --  No binding: gtk_init_check_abi_check

   --  No binding: gtk_get_option_group
   --  No binding: gtk_init_with_args
   --  No binding: gtk_parse_args
   --  No binding: gtk_main_iteration

   --  This function are not bound, we only use gtk_quit_add_full
   --  No binding: gtk_quit_add
   --  No binding: gtk_quit_remove_by_data

   --  These functions are intended as callbacks, but do not apply to GtkAda
   --  No binding: gtk_true
   --  No binding: gtk_false

   --  These functions were never bound, and are now obsolesent anyway
   --  No binding: gtk_input_add_full
   --  No binding: gtk_input_remove

end Gtk.Main;
