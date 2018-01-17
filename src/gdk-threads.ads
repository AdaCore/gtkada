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
--
--  This package provides simple primitives to write multi-threaded
--  applications with GtkAda. See the GtkAda User's Guide for more details
--  (section Tasking with GtkAda).
--
--  </description>
--  <c_version>1.3.6</c_version>
--  <group>Gdk, the low-level API</group>

with System;

package Gdk.Threads is

   procedure G_Init (Vtable : System.Address := System.Null_Address);
   --  Initialize the Glib internal threading support.
   --  This procedure must be called before any call to Enter or Leave.
   --  The parameter Vtable should never be used for now.

   procedure Init;
   --  Initialize the Gdk internal threading support.
   --  This function must be called after G_Init and before any call to
   --  Enter or Leave.

   procedure Enter;
   --  Take the GtkAda global lock.
   --  See the GtkAda User's Guide for more details (section Tasking with
   --  GtkAda).

   procedure Leave;
   --  Release the GtkAda global lock.
   --  See the GtkAda User's Guide for more details (section Tasking with
   --  GtkAda).

private
   pragma Linker_Options ("-lgthread-2.0");
   --  This is needed to resolve g_thread_init

   pragma Import (C, G_Init, "g_thread_init");
   pragma Import (C, Init, "gdk_threads_init");
   pragma Import (C, Enter, "gdk_threads_enter");
   pragma Import (C, Leave, "gdk_threads_leave");
end Gdk.Threads;
