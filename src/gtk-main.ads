-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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
--  <c_version>1.2.6</c_version>

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
   --  Initializes GtkAda's internal structures.
   --  This subprogram should be called before any other one in GtkAda.
   --  If GtkAda could not be initialized (no access to the display, etc.), the
   --  application exits with an error

   function Init_Check return Boolean;
   --  Initializes GtkAda's internal structures.
   --  Returns False if there was an error (no access to the display, etc.)

   procedure Gtk_Exit (Error_Code : in Gint);
   --  Terminates GtkAda cleanly, and quits the application.

   function Set_Locale return String;
   --  Read and parses the local settings, such as time format, ...
   --  Returns the name of the local settings, which can also be set with
   --  the environment variable LOCALE

   procedure Set_Locale;
   --  Read and parses the local settings, such as time format, ...

   -----------------------------
   -- Init and Quit functions --
   -----------------------------

   type Init_Function is access procedure (Data : System.Address);
   procedure Init_Add (Func : Init_Function; Data : System.Address);
   --  Registers a function to be called just before starting a main loop.
   --  This function is called only once, even if a new main loop is started
   --  recursively.

   type Quit_Handler_Id is new Guint;

   type Quit_Function is access function return Boolean;
   function Quit_Add (Main_Level : Guint;
                      Func       : Quit_Function)
                     return Quit_Handler_Id;
   --  Registers a new function to be called when the current main loop
   --  exists.
   --  The function will be called once when the current main loop exists.
   --  If it returns FALSE, it will then be deleted from the list of
   --  quit functions, and won't be called again next time a main loop is
   --  exited.
   --  The function will only be called when exiting a main loop at level
   --  MAIN_LEVEL. If MAIN_LEVEL is 0, the function will be called for the
   --  current main_loop.

   --  <doc_ignore>
   generic
      type Data_Type (<>) is private;
   package Quit is
      type Quit_Function is access function (Data : Data_Type) return Boolean;
      function Quit_Add (Main_Level : Guint;
                         Func       : Quit_Function;
                         Data       : Data_Type)
                        return Quit_Handler_Id;
   end Quit;
   --  !!Warning!!: This package needs to be instantiated at library level
   --  since it calls some internal functions as callback.
   --  </doc_ignore>

   function Quit_Add_Destroy
     (Main_Level : Guint;
      Object     : access Gtk.Object.Gtk_Object_Record'Class)
     return Quit_Handler_Id;
   --  Make sure that OBJECT is destroyed when existing the main loop at level
   --  MAIN_LEVEL (or the current main loop is MAIN_LEVEL is 0).

   procedure Quit_Remove (Id : Quit_Handler_Id);
   --  Removes a Quit Handler, that has been previously set by Quit_Add.

   -------------------
   -- The main loop --
   -------------------

   function Events_Pending return Boolean;
   --  Returns True if there are some events waiting in the event queue.

   procedure Main;
   --  Start the main loop, and returns only when the main loop is exited.
   --  This subprogram can be called recursively, to start new internal
   --  loops. Each of these loops is exited through a call to Main_Quit.
   --  This is the recommended technics to use when you want to popup a dialog
   --  and wait for the user answer before going any further.

   function Main_Level return Guint;
   --  Returns the level of the current main loop
   --  Since there can be imbricated loops, this returns the depth of the
   --  current one, starting from 1 (0 if there is none).

   procedure Main_Quit;
   --  Quits the current main loop.
   --  If this was the last active main loop, no more events will be processed
   --  by GtkAda.

   function Main_Iteration (Blocking : in Boolean := True) return Boolean;
   --  Does one iteration of the main loop.
   --  BLOCKING indicates whether GtkAda should wait for an event to be
   --  available, or simply exit if there is none.
   --  Returns True if no main loop is running
   --  When doing some heavy calculations in an application, it is recommended
   --  that you check from time to time if there are any events pending and
   --  process them, so that your application still reacts to events.
   --  To do that, you would add a loop like:
   --    while Gtk.Main.Events_Pending loop
   --        Dead := Gtk.Main.Main_Iteration;
   --    end loop;

   procedure Do_Event (Event : in Gdk.Event.Gdk_Event);
   --  Processes EVENT as if it was in the event queue.
   --  This function should almost never be used in your own application, this
   --  is the core function for event processing in GtkAda.
   --  The user should not free EVENT, this is already done by GtkAda.

   function Get_Event_Widget (Event : in Gdk.Event.Gdk_Event)
                             return Gtk.Widget.Gtk_Widget;
   --  Returns the widget to which EVENT applies

   --------------------
   -- Grab functions --
   --------------------

   procedure Grab_Add (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds a new widget to the grab list.
   --  The widget at the front of this list gets all the events even if it does
   --  not have the focus. This feature should be used with care.
   --  If you want a whole window to get the events, it is better to use
   --  Gtk.Window.Set_Modal instead which does the grabbing abd ungrabbing for
   --  you.
   --  The grab is only done for the application. Events outside the
   --  application are still send to their respective windows.

   procedure Grab_Remove (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes a widget from the grab list.

   function Grab_Get_Current return Gtk.Widget.Gtk_Widget;
   --  Returns the widget that currently has the focus

   ----------
   -- Idle --
   ----------
   --  GtkAda gives the possibility to register Idle functions.
   --  These are called every time there is no more event to process in the
   --  queue, and are used for instance internally to redraw widgets (so that
   --  the application keeps reacting to user input even if there is a heavy
   --  redrawing to do).
   --  The Idle function returns a boolean, which should be TRUE if the idle
   --  remains activate and should be called again, or FALSE if the idle should
   --  be unregistered.
   --  The priority of these idle callbacks can also be modified, so that
   --  the scheduling calls one callback before another.
   --
   --  Two versions are given, either with a user data or with none.

   type Idle_Handler_Id is new Guint;
   type Idle_Priority   is new Guint;
   Priority_High_Idle    : constant Idle_Priority := 100;
   Priority_Default_Idle : constant Idle_Priority := 200;
   Priority_Low_Idle     : constant Idle_Priority := 300;

   type Idle_Callback is access function return Boolean;
   function Idle_Add (Cb       : in Idle_Callback;
                      Priority : in Idle_Priority := Priority_Default_Idle)
                     return Idle_Handler_Id;
   --  Registers an idle callback with no user data.

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
   --  Removes an idle callback, when its ID is known.

   -------------
   -- Timeout --
   -------------
   --  A timeout is a function that is called after a specified amount
   --  of time. You can of course use Ada tasks for the same role, however
   --  this might provide an easier way of doing things.

   type Timeout_Handler_Id is new Guint;

   type Timeout_Callback is access function return Boolean;
   function Timeout_Add (Interval : in Guint32;
                         Func : Timeout_Callback)
                        return Timeout_Handler_Id;
   --  Adds a new timeout. FUNC will be called after INTERVAL milliseconds.
   --  The function will be as long as it returns TRUE.

   --  <doc_ignore>
   generic
      type Data_Type (<>) is private;
   package Timeout is
      type Callback is access function (D : in Data_Type) return Boolean;
      function Add (Interval : in Guint32;
                    Func     : in Callback;
                    D        : in Data_Type)
                    return      Timeout_Handler_Id;
      --  Adds a new timeout. FUNC will be called after INTERNAL milliseconds.
   end Timeout;
   --  !!Warning!! The instances of this package must be declared at library
   --  level, as they are some accesses to internal functions that happen
   --  when the callback is called.
   --  </doc_ignore>

   procedure Timeout_Remove (Id : in Timeout_Handler_Id);
   --  Unregisters a timeout function.

   -----------
   -- Input --
   -----------
   --  The following functions are used to react when new data is available on
   --  a file descriptor (file, socket, pipe, ...)
   --  They have not been bound, since apparently there is no easy way in Ada
   --  to get the file descriptor for an open file, and there is no standard
   --  socket package.
   --  Instead, you should consider having a separate task that takes care of
   --  monitoring over sources of input in your application.

   --  gtk_input_add_full
   --  gtk_input_remove
   --  key_snooper_install
   --  key_snooper_remove


private
   --  Some services can be directly binded...

   pragma Import (C, Gtk_Exit, "gtk_exit");
   pragma Import (C, Main_Level, "gtk_main_level");
   pragma Import (C, Main_Quit, "gtk_main_quit");
   pragma Import (C, Main, "gtk_main");
   pragma Import (C, Idle_Remove, "gtk_idle_remove");
   pragma Import (C, Timeout_Remove, "gtk_timeout_remove");
   pragma Import (C, Init_Add, "gtk_init_add");
   pragma Import (C, Quit_Remove, "gtk_main_quit_remove");
end Gtk.Main;

--  Should show how to start a new main loop to wait for a dialog's answer
