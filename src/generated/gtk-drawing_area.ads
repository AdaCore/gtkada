------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
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

--  <description>
--  This widget provides an empty canvas on which the application can draw
--  anything.
--
--  Note that this widget is simply an empty space, and that you need to
--  connect it to events to make it useful. For instance, you might want to do
--  one of the following :
--
--  * Connect it to "expose_event": The handlers are called every time the
--  widget needs to be redrawn. You can then draw anything you want on the
--  canvas, after getting its associated window with a call to
--  Gtk.Widget.Get_Window. Note that the event mask is automatically set up to
--  accept expose_events.
--
--  * Connect it to "button_press_event" and "button_release_event" events,
--  when you want it to react to user input. Note that you need to set up the
--  event mask with a call to Gtk.Widget.Set_Events.
--
--  See also the Double_Buffer widget provided in the GtkAda examples for an
--  advanced example that demonstrates how to use double buffering, to avoid
--  flickering in your drawings.
--
--  </description>
--  <group>Drawing</group>
--  <testgtk>libart_demo.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Glib.Types;    use Glib.Types;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Widget;    use Gtk.Widget;

package Gtk.Drawing_Area is

   type Gtk_Drawing_Area_Record is new Gtk_Widget_Record with null record;
   type Gtk_Drawing_Area is access all Gtk_Drawing_Area_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Drawing_Area : out Gtk_Drawing_Area);
   procedure Initialize
      (Drawing_Area : access Gtk_Drawing_Area_Record'Class);
   --  Creates a new drawing area.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_drawing_area_get_type");

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Drawing_Area_Record, Gtk_Drawing_Area);
   function "+"
     (Widget : access Gtk_Drawing_Area_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Drawing_Area
   renames Implements_Buildable.To_Object;

end Gtk.Drawing_Area;
