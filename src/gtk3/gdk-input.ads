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

--  This package provides low level handling of file descriptors in the
--  GtkAda event loop. Note that this package is currently not supported
--  under Windows.
--  <group>Gdk, the low-level API</group>

with Gdk.Event;
with Gdk.Types;
with Glib;

package Gdk.Input is

   procedure Init;

   procedure Gdk_Exit;

   procedure Set_Extension_Events
     (Window : Gdk.Gdk_Window;
      Mask   : Gdk.Event.Gdk_Event_Mask;
      Mode   : Gdk.Types.Gdk_Extension_Mode);
   --  ??? Check that Mask is indeed of type GdkEventMask.
   --  The C code defines it as Gint...

   generic
      type Data_Type (<>) is private;
   package Input_Add is

      type Data_Access is access all Data_Type;
      pragma Convention (C, Data_Access);

      type Gdk_Input_Function is access procedure
        (Data      : Data_Access;
         Source    : Glib.Gint;
         Condition : Gdk.Types.Gdk_Input_Condition);

      function Add
        (Source    : Glib.Gint;
         Condition : Gdk.Types.Gdk_Input_Condition;
         Func      : Gdk_Input_Function;
         Data      : Data_Access) return Glib.Gint;

   private
      pragma Import (C, Add, "gdk_input_add");
   end Input_Add;

   procedure Remove (Tag : Glib.Gint);

   --  to bind: gdk_input_motion_events
   --    This returns the list of past motion events in the window, between two
   --    times. This might give a finer granularity than getting the
   --    Motion_Events themselves, and might be more efficient.
   --    Not supported by all XServers though.

private
   pragma Import (C, Init, "gdk_input_init");
   pragma Import (C, Gdk_Exit, "gdk_input_exit");
   pragma Import (C, Set_Extension_Events, "gdk_input_set_extension_events");
   pragma Import (C, Remove, "gdk_input_remove");
end Gdk.Input;
