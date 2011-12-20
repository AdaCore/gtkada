------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with Ada.Text_IO; use Ada.Text_IO;
with Gtk.Main;

package body Hello_Package is

   --  This is a callback function.

   procedure Hello_Callback (Widget : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Widget);
   begin
      Put_Line ("Hello World");
   end Hello_Callback;

   function Delete_Event
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event)
     return Boolean
   is
      pragma Unreferenced (Event);
      pragma Unreferenced (Widget);
   begin
      --  If you return False in the "delete_event" signal handler,
      --  GtkAda will emit the "destroy" signal. Returning True means
      --  you don't want the window to be destroyed. This is useful
      --  for popping up 'are you sure you want to quit?' type
      --  dialogs.

      Put_Line ("delete event occurred");

      --  Change True to False and the main window will be destroyed
      --  with a "delete_event".

      return True;
   end Delete_Event;

   --   Another callback
   procedure Destroy (Widget : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Widget);
   begin
      Gtk.Main.Main_Quit;
   end Destroy;

end Hello_Package;
