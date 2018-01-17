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
--  This is the top level package of the Gdk hierarchy.
--  It provides the type definitions used to access underlying C structures.
--
--  </description>
--  <group>Gdk, the low-level API</group>

with Glib;
pragma Warnings (Off);
with System;
pragma Warnings (On);

package Gdk is
   --  A Gdk_Window, in gtk+, is really a GObject. That means it comes with
   --  reference counting. Changing this in GtkAda would break a lot of code
   --  though, so we only bind it as a C_Proxy, but provide Gdk.Window.Ref
   --  and Gdk.Window.Unref to access the reference counting support.
   type Gdk_Window is new Glib.C_Proxy;

   type Gdk_Cursor is new Glib.C_Proxy;  --  private type in C
   type Gdk_Visual is new Glib.C_Proxy;  --  private type in C
   type Gdk_Window_Attr is new Glib.C_Proxy;
end Gdk;
