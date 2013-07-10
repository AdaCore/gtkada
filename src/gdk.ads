------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2013, AdaCore                     --
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
   pragma Preelaborate;

   type Gdk_GC is new Glib.C_Proxy;
   type Gdk_Window is new Glib.C_Proxy;
   type Gdk_Cursor is new Glib.C_Proxy;
   type Gdk_Colormap is new Glib.C_Proxy;
   type Gdk_Visual is new Glib.C_Proxy;
   type Gdk_Font is new Glib.C_Proxy;
   type Gdk_Image is new Glib.C_Proxy;
   type Gdk_Region is new Glib.C_Proxy;
   type Gdk_Window_Attr is new Glib.C_Proxy;
end Gdk;
