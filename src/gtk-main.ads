-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--
--  This package contains top-level subprograms that are used to initialize
--  GtkAda and interact with the main event loop.
--
--  It also provides a set of packages to set up idle functions, timeout
--  functions, and functions to be called before and after entering the
--  main loop.
--
--  </description>
--  <c_version>1.3.6</c_version>

with Gdk.Event;
with Gtk.Widget;
with Gtk.Object;
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

   procedure Gtk_Exit (Error_Code : Gint);
   --  Terminate GtkAda.
   --  Deprecated, use Main_Quit instead.
   --  pragma Deprecated (Gtk_Exit);

   function Set_Locale return String;
   --  Read and parse the local settings, such as time format, ...
   --  Return the name of the local settings, which can also be set with
   --  the environment variable LOCALE

   procedure Set_Locale;
   --  Read and parse the local settings, such as time format, ...

   -----------------------------
   -- Init and Quit functions --
   -----------------------------

   type Init_Function is access procedure (Data : System.Address);
   --  Function called just before starting the main loop.
   --  This can be registered with Init_Add below.

   procedure Init_Add (Func : Init_Function; Data : System.Address);
   --  Register a function to be called just before starting a main loop.
   --  This function is called only once, even if a new main loop is started
   --  recursively.

   type Quit_Handler_Id is new Guint;
   --  registration ID for functions that will be called before the
   --  main loop exits.

   type Quit_Function is access function return Boolean;
   --  Type of function that can be called when the main loop exits.
   --  It should return False if it should not be called again when another
   --  main loop exits.

   function Quit_Add
     (Main_Level : Guint; Func : Quit_Function) return Quit_Handler_Id;
   --  Register a new function to be called when the current main loop exits.
   --  The function will be called once when the current main loop exists.
   --  If it returns False, it will then be deleted from the list of
   --  quit functions, and won't be called again next time a main loop is
   --  exited.
   --  The function will only be called when exiting a main loop at level
   --  Main_Level. If Main_Level is 0, the function will be called for the
   --  current main_loop.

   --  <doc_ignore>
   generic
      type Data_Type (<>) is private;
   package Quit is
      type Quit_Function is access function (Data : Data_Type) return Boolean;

      function Quit_Add
        (Main_Level : Guint;
         Func       : Quit_Function;
         Data       : Data_Type) return Quit_Handler_Id;
   end Quit;
   --  !!Warning!!: This package needs to be instantiated at library level
   --  since it calls some internal functions as callback.
   --  </doc_ignore>

   function Quit_Add_Destroy
     (Main_Level : Guint;
      Object     : access Gtk.Object.Gtk_Object_Record'Class)
      return Quit_Handler_Id;
   --  Ensure that Object is destroyed when exiting the main loop at Main_Level
   --  (or the current main loop level is 0).

   procedure Quit_Remove (Id : Quit_Handler_Id);
   --  Remove a Quit Handler, that has been previously set by Quit_Add.

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
   --  Returns True if no main loop is running
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

   function Get_Event_Widget
     (Event : Gdk.Event.Gdk_Event) return Gtk.Widget.Gtk_Widget;
   --  Return the widget to which Event applies.

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

   procedure Grab_Remove (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Remove a widget from the grab list.

   function Grab_Get_Current return Gtk.Widget.Gtk_Widget;
   --  Return the widget that currently has the focus.

   ----------
   -- Idle --
   ----------
   --  GtkAda gives the possibility to register Idle functions.
   --  These are called every time there is no more event to process in the
   --  queue, and are used for instance internally to redraw widgets (so that
   --  the application keeps reacting to user input even if there is a heavy
   --  redrawing to do).
   --  The Idle function returns a boolean, which should be True if the idle
   --  remains active and should be called again, or False if the idle should
   --  be unregistered.
   --  The priority of these idle callbacks can also be modified, so that
   --  the scheduling calls one callback before another.
   --
   --  Two versions are given, either with a user data or with none.

   type Idle_Handler_Id is new Guint;
   --  Id for Idle handlers.

   type Idle_Priority   is new Guint;
   --  Priorities that can be set for idle handlers.
   --  The higher the priority, the less urgent the task. Handlers whose
   --  priority is lower will be called before others.

   Priority_High_Idle    : constant Idle_Priority := 100;
   Priority_Default_Idle : constant Idle_Priority := 200;
   Priority_Low_Idle     : constant Idle_Priority := 300;

   type Idle_Callback is access function return Boolean;
   --  Function that can be called automatically whenever GtkAda is not
   --  processing events.
   --  It should return True if the function should be called again as soon
   --  as possible, False if it should be unregistered.

   function Idle_Add (Cb       : in Idle_Callback;
                      Priority : in Idle_Priority := Priority_Default_Idle)
                     return Idle_Handler_Id;
   --  Register an idle callback with no user data.

   --  <doc_ignore>
   generic
      type Data_Type (<>) is private;
   package Idle is
      type Callback is access function (D : in Data_Type) return Boolean;
      function Add (Cb       : in Callback;
                    D        : in Data_Type;
                    Priority : in Idle_Priority := Priority_Default_Idle)
                   return Idle_Handler_Id;
   end Idle;
   --  !!Warning!! The instances of this package must be declared at library
   --  level, as they are some accesses to internal functions that happen
   --  when the callback is called.
   --  </doc_ignore>

   procedure Idle_Remove (Id : in Idle_Handler_Id);
   --  Remove an idle callback, when its Id is known.

   -------------
   -- Timeout --
   -------------
   --  A timeout is a function that is called after a specified amount
   --  of time. You can of course use Ada tasks for the same role, however
   --  this might provide an easier way of doing things.
   --
   --  In case your timeout function takes longer to execute than the specific
   --  delay (for instance it takes 200ms for an internal of 100ms), then
   --  no invocation is queued, and they are simply discarded. There is no
   --  queue of expired timers. On the other hand, standard events are still
   --  processed, but slowly (since they have the same priority as timeouts).
   --
   --  The redrawing and resizing of widgets, which are being done as idles
   --  with lower priority will not take place.

   type Timeout_Handler_Id is new Guint;
   --  Id for Timeout handlers.

   type Timeout_Callback is access function return Boolean;
   --  Function that can be called automatically at precise time intervals.
   --  It should return True if the function should be called again as soon
   --  as possible, False if it should be unregistered.

   function Timeout_Add
     (Interval : in Guint32;
      Func : Timeout_Callback) return Timeout_Handler_Id;
   --  Add a new timeout. Func will be called after Interval milliseconds.
   --  The function will be called as long as it returns True.

   --  <doc_ignore>
   generic
      type Data_Type (<>) is private;
   package Timeout is
      type Callback is access function (D : in Data_Type) return Boolean;

      function Add
        (Interval : Guint32;
         Func     : Callback;
         D        : Data_Type) return Timeout_Handler_Id;
      --  Adds a new timeout. Func will be called after Interval milliseconds.
   end Timeout;
   --  !!Warning!! The instances of this package must be declared at library
   --  level, as they are some accesses to internal functions that happen
   --  when the callback is called.
   --  </doc_ignore>

   procedure Timeout_Remove (Id : Timeout_Handler_Id);
   --  Unregister a timeout function.

   -----------
   -- Input --
   -----------
   --  The following functions are used to react when new data is available on
   --  a file descriptor (file, socket, pipe, ...)
   --  They have not been bound, since apparently there is no easy way in Ada
   --  to get the file descriptor for an open file, and there is no standard
   --  socket package.
   --  Instead, you should consider having a separate task that takes care of
   --  monitoring over sources of input in your application, or use the low
   --  level Gdk.Input package.

   --  gtk_input_add_full
   --  gtk_input_remove
   --  key_snooper_install
   --  key_snooper_remove
   --  gtk_get_current_event
   --  gtk_get_current_event_time
   --  gtk_get_current_event_state


private
   pragma Import (C, Gtk_Exit, "gtk_exit");
   pragma Import (C, Main_Level, "gtk_main_level");
   pragma Import (C, Main_Quit, "gtk_main_quit");
   pragma Import (C, Main, "gtk_main");
   pragma Import (C, Idle_Remove, "gtk_idle_remove");
   pragma Import (C, Timeout_Remove, "gtk_timeout_remove");
   pragma Import (C, Init_Add, "gtk_init_add");
   pragma Import (C, Quit_Remove, "gtk_main_quit_remove");
end Gtk.Main;
