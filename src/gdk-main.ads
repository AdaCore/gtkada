-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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
--  This package provides routines to handle initialization and set up of the
--  Gdk library.
--  </description>

with Glib; use Glib;

with Gdk.Cursor;
with Gdk.Types;
with Gdk.Window;

package Gdk.Main is

   procedure Init;
   --  Initialize the library for use.
   --  The command line arguments are modified to reflect any arguments
   --  which were not handled. (Such arguments should either
   --  be handled by the application or dismissed).

   procedure Gdk_Exit (Error_Code : in Gint);
   --  Restore the library to an un-itialized state and exits
   --  the program using the "exit" system call.
   --  Error_Code is the error value to pass to "exit".
   --  Allocated structures are freed and the program exits cleanly.

   function Set_Locale return String;
   --  Initialize handling of internationalization of strings.
   --  @pxref{Package_Gtkada.Intl} for more details.

   procedure Set_Locale;
   --  Drops the string returned by the Set_Locale function;

   procedure Set_Use_Xshm (Use_Xshm : in Boolean := True);
   --  Set whether shared memory (when supported by the graphic server) should
   --  be used.

   function Get_Use_Xshm return Boolean;
   --  Return whether shared memory on the graphic server is used.

   function Get_Display return String;
   --  Return the name of the display.

   function Time_Get return Guint32;
   --  Get the number of milliseconds since the library was initialized.
   --  This time value is accurate to milliseconds even though
   --  a more accurate time down to the microsecond could be
   --  returned.
   --  Note that this function is currently not supported under Win32 systems.

   function Timer_Get return Guint32;
   --  Return the current timer interval.
   --  This interval is in units of milliseconds.

   procedure Timer_Set (Milliseconds : in Guint32);
   --  Set the timer interval.
   --  Milliseconds is the new value for the timer.
   --  As a side effect, calls to Gdk.Event.Get will last for a maximum
   --  of time of Milliseconds. However, a value of 0 milliseconds will cause
   --  Gdk.Event.Get to block indefinitely until an event is received.

   procedure Timer_Enable;
   --  Enable the Gdk timer.

   procedure Timer_Disable;
   --  Disable the Gdk timer.

   function Screen_Width return Gint;
   --  Return the width of the screen.

   function Screen_Height return Gint;
   --  Return the height of the screen.

   function Screen_Width_MM return Gint;
   --  Return the width of the screen in millimeters.

   function Screen_Height_MM return Gint;
   --  Return the height of the screen in millimeters.

   procedure Flush;
   --  Flush the queue of graphic events and then wait
   --  until all requests have been received and processed.

   procedure Beep;
   --  Emit a beep.

   procedure Key_Repeat_Disable;
   --  Disable the key repeat behavior.

   procedure Key_Repeat_Restore;
   --  Restore the key repet behavior.

   function Pointer_Grab
     (Window       : in Gdk.Window.Gdk_Window;
      Owner_Events : in Boolean := True;
      Event_Mask   : in Gdk.Types.Gdk_Event_Mask;
      Confine_To   : in Gdk.Window.Gdk_Window := Gdk.Window.Null_Window;
      Cursor       : in Gdk.Cursor.Gdk_Cursor := Gdk.Cursor.Null_Cursor;
      Time         : in Guint32) return Boolean;
   --  Grab the pointer to a specific window.
   --    - Window is the window which will receive the grab
   --    - Owner_Events specifies whether events will be reported as is,
   --      or relative to Window
   --    - Event_Mask masks only interesting events
   --    - Confine_To limits the cursor movement to the specified window
   --    - Cursor changes the cursor for the duration of the grab
   --    - Time specifies the time
   --  Requires a corresponding call to Pointer_Ungrab

   procedure Pointer_Ungrab (Time : in Guint32);
   --  Release any pointer grab.

   function Pointer_Is_Grabbed return Boolean;
   --  Tell wether there is an active pointer grab in effect.

   function Keyboard_Grab
     (Window       : in Gdk.Window.Gdk_Window;
      Owner_Events : in Boolean := True;
      Time         : in Guint32) return Boolean;
   --  Grab the keyboard to a specific window.
   --    - Window is the window which will receive the grab
   --    - Owner_Events specifies whether events will be reported as is,
   --      or relative to Window
   --    - Time specifies the time
   --  Requires a corresponding call to Keyboard_Ungrab

   procedure Keyboard_Ungrab (Time : in Guint32);
   --  Release any keyboard grab.

private
   pragma Import (C, Gdk_Exit, "gdk_exit");
   pragma Import (C, Time_Get, "gdk_time_get");
   pragma Import (C, Timer_Get, "gdk_timer_get");
   pragma Import (C, Timer_Set, "gdk_timer_set");
   pragma Import (C, Timer_Enable, "gdk_timer_enable");
   pragma Import (C, Timer_Disable, "gdk_timer_disable");
   pragma Import (C, Screen_Width, "gdk_screen_width");
   pragma Import (C, Screen_Height, "gdk_screen_height");
   pragma Import (C, Screen_Width_MM, "gdk_screen_width_mm");
   pragma Import (C, Screen_Height_MM, "gdk_screen_height_mm");
   pragma Import (C, Flush, "gdk_flush");
   pragma Import (C, Beep, "gdk_beep");
   pragma Import (C, Key_Repeat_Disable, "gdk_key_repeat_disable");
   pragma Import (C, Key_Repeat_Restore, "gdk_key_repeat_restore");
end Gdk.Main;
