
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
--  The Gtk_Fixed widget is a container which can place child widgets at fixed
--  positions and with fixed sizes, given in pixels.
--
--  Note that it is usually bad practice to use the Gtk_Fixed container in
--  GtkAda. Instead, you should consider using one of the other many containers
--  available, that will allow you to handle resizing of your windows, as well
--  as font size changes easily.
--
--  </description>
--  <screenshot>gtk-fixed</screenshot>
--  <group>Layout containers</group>
--  <testgtk>create_fixed.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Glib.Types;    use Glib.Types;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Container; use Gtk.Container;
with Gtk.Widget;    use Gtk.Widget;

package Gtk.Fixed is

   type Gtk_Fixed_Record is new Gtk_Container_Record with null record;
   type Gtk_Fixed is access all Gtk_Fixed_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Fixed : out Gtk_Fixed);
   procedure Initialize (Fixed : access Gtk_Fixed_Record'Class);
   --  Creates a new Gtk.Fixed.Gtk_Fixed.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_fixed_get_type");

   -------------
   -- Methods --
   -------------

   procedure Move
      (Fixed  : access Gtk_Fixed_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
       X      : Gint;
       Y      : Gint);
   --  Move a child of a GtkFixed container to the given position. X indicates
   --  the horizontal position to place the widget at. Y is the vertical
   --  position to place the widget at.
   --  "widget": the child widget.
   --  "x": the horizontal position to move the widget to.
   --  "y": the vertical position to move the widget to.

   procedure Put
      (Fixed  : access Gtk_Fixed_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
       X      : Gint;
       Y      : Gint);
   --  Add Widget to a Fixed container at the given position. X indicates the
   --  horizontal position to place the widget at. Y is the vertical position
   --  to place the widget at.
   --  "widget": the widget to add.
   --  "x": the horizontal position to place the widget at.
   --  "y": the vertical position to place the widget at.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Fixed_Record, Gtk_Fixed);
   function "+"
     (Widget : access Gtk_Fixed_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Fixed
   renames Implements_Buildable.To_Object;

end Gtk.Fixed;
