-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
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

with Glib; use Glib;
with Gdk;
with Gtk.Widget;

package Gdk.Window is

   type Gdk_Window is new Root_Type with private;

   function Get_Window (Widget : in Gtk.Widget.Gtk_Widget'Class)
                        return Gdk.Window.Gdk_Window'Class;
   --  FIXME : This should be in Gtk.Window.

   procedure Clear_Area (Window : in Gdk_Window'Class;
                         X      : in Gint;
                         Y      : in Gint;
                         Width  : in Gint;
                         Height : in Gint);
   --  mapping: Clear_Area gdk.h gdk_window_clear_area


private

   type Gdk_Window is new Root_Type with null record;

end Gdk.Window;
