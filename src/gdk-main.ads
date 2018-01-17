------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2018, AdaCore                     --
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
--  This package provides routines to handle initialization and set up of the
--  Gdk library.
--  </description>
--  <c_version>1.3.6</c_version>
--  <group>Gdk, the low-level API</group>

with Glib; use Glib;
with Gdk.Event;
with Gdk.Types;

package Gdk.Main is

   procedure Init;
   --  Initialize the library for use.
   --  The command line arguments are modified to reflect any arguments
   --  which were not handled. (Such arguments should either
   --  be handled by the application or dismissed).

   procedure Gdk_Exit (Error_Code : Gint);
   --  Restore the library to an un-itialized state and exits
   --  the program using the "exit" system call.
   --  Error_Code is the error value to pass to "exit".
   --  Allocated structures are freed and the program exits cleanly.
   --  This function is deprecated.

   function Get_Display return String;
   --  Return the name of the display.

   function Pointer_Grab
     (Window       : Gdk.Gdk_Window;
      Owner_Events : Boolean := True;
      Event_Mask   : Gdk.Event.Gdk_Event_Mask;
      Confine_To   : Gdk.Gdk_Window := null;
      Cursor       : Gdk.Gdk_Cursor := null;
      Time         : Guint32 := 0) return Gdk.Types.Gdk_Grab_Status;
   --  Grab the pointer to a specific window.
   --    - Window is the window which will receive the grab
   --    - Owner_Events specifies whether events will be reported as is,
   --      or relative to Window
   --    - Event_Mask masks only interesting events
   --    - Confine_To limits the cursor movement to the specified window
   --    - Cursor changes the cursor for the duration of the grab
   --    - Time specifies the time
   --  Requires a corresponding call to Pointer_Ungrab
   --
   --  This is obsolescent in gtk-3, use Gdk.Device.Grab instead

   procedure Pointer_Ungrab (Time : Guint32 := 0);
   --  Release any pointer grab.

   function Pointer_Is_Grabbed return Boolean;
   --  Tell wether there is an active pointer grab in effect.

   function Keyboard_Grab
     (Window       : Gdk.Gdk_Window;
      Owner_Events : Boolean := True;
      Time         : Guint32 := 0) return Gdk.Types.Gdk_Grab_Status;
   --  Grab the keyboard to a specific window.
   --    - Window is the window which will receive the grab
   --    - Owner_Events specifies whether events will be reported as is,
   --      or relative to Window
   --    - Time specifies the time
   --  Requires a corresponding call to Keyboard_Ungrab

   procedure Keyboard_Ungrab (Time : Guint32 := 0);
   --  Release any keyboard grab.

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

   procedure Set_Double_Click_Time (Msec : Guint);

private
   pragma Import (C, Gdk_Exit, "gdk_exit");
   pragma Import (C, Screen_Width, "gdk_screen_width");
   pragma Import (C, Screen_Height, "gdk_screen_height");
   pragma Import (C, Screen_Width_MM, "gdk_screen_width_mm");
   pragma Import (C, Screen_Height_MM, "gdk_screen_height_mm");
   pragma Import (C, Set_Double_Click_Time, "gdk_set_double_click_time");
   pragma Import (C, Flush, "gdk_flush");
   pragma Import (C, Beep, "gdk_beep");

end Gdk.Main;

--  missing:
--  gdk_wcstombs
--  gdk_mbstowcs
