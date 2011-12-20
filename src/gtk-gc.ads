------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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
--  This package provides a convenitn function to create a new graphic
--  context. Such contexts are needed in several places in GtkAda, in
--  particular for the drawing routines, and this function provides a
--  convenient way to create them.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Miscellaneous</group>
--  <see>Gdk.GC</see>

with Gdk.GC;

package Gtk.GC is

   function Get
     (Depth       : Gint;
      Colormap    : Gdk.Gdk_Colormap;
      Values      : Gdk.GC.Gdk_GC_Values;
      Values_Mask : Gdk.GC.Gdk_GC_Values_Mask)
      return Gdk_GC;
   --  Create a new GC with the matching attributes.
   --  If such a graphic context already exists, it is returned, which is much
   --  faster than creating a new one. Creating a new context requires a
   --  round-trip to the server (X11 for instance), and can be slow.
   --  You shouldn't modify the attributes of the returned context, since that
   --  might impact other parts of the code that have queried it.

   procedure Release (Gc : Gdk_GC);
   --  Decrease the reference counting for the GC. If it reaches 0, then
   --  calling Get will create a new one the next time it is called with the
   --  same attributes.

private
   pragma Import (C, Get, "gtk_gc_get");
   pragma Import (C, Release, "gtk_gc_release");
end Gtk.GC;
