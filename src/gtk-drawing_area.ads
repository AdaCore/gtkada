-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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
--  This widget provides an empty canvas on which the application can draw
--  anything.
--  Note that this widget is simply an empty space, and that you need to
--  connect it to events to make it useful. For instance, you might want to do
--  one of the following :
--
--  * Connect it to "expose_event": The handlers are called every time the
--    widget needs to be redrawn. You can then draw anything you want on the
--    canvas, after getting its associated window with a call to
--    Gtk.Widget.Get_Window. Note that the event mask is automatically set up
--    to accept expose_events.
--
--  * Connect it to "button_press_event" and "button_release_event" events,
--    when you want it to react to user input. Note that you need to set up the
--    event mask with a call to Gtk.Widget.Set_Events.
--
--  See also the Double_Buffer widget provided in the GtkAda examples for an
--  advanced example that demonstrates how to use double buffering, to avoid
--  flickering in your drawings.
--
--  </description>
--  <c_version>1.3.11</c_version>

with Gtk.Widget;

package Gtk.Drawing_Area is

   type Gtk_Drawing_Area_Record is new
     Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Drawing_Area is access all Gtk_Drawing_Area_Record'Class;

   procedure Gtk_New (Drawing_Area : out Gtk_Drawing_Area);
   --  Create a new blank Drawing_Area.
   --  Note that the background of the widget is uninitialized, and that you
   --  have to draw on it yourself.

   procedure Initialize (Drawing_Area : access Gtk_Drawing_Area_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Drawing_Area.

   procedure Size
     (Darea  : access Gtk_Drawing_Area_Record;
      Width  : Gint;
      Height : Gint);
   --  Request a new size for the area.
   --  This queues a resize request for the area.
   --  pragma Deprecated (Size);

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Drawing_Area_Record is new Gtk.Widget.Gtk_Widget_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_drawing_area_get_type");
end Gtk.Drawing_Area;
