------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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
--  The Gtk.Bin.Gtk_Bin widget is a container with just one child. It is not
--  very useful itself, but it is useful for deriving subclasses, since it
--  provides common code needed for handling a single child widget.
--
--  Many GTK+ widgets are subclasses of Gtk.Bin.Gtk_Bin, including
--  Gtk.Window.Gtk_Window, Gtk.Button.Gtk_Button, Gtk.Frame.Gtk_Frame,
--  Gtk.Handle_Box.Gtk_Handle_Box or Gtk.Scrolled_Window.Gtk_Scrolled_Window.
--
--  </description>
--  <group>Abstract base classes</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Glib.Types;    use Glib.Types;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Container; use Gtk.Container;
with Gtk.Widget;    use Gtk.Widget;

package Gtk.Bin is

   type Gtk_Bin_Record is new Gtk_Container_Record with null record;
   type Gtk_Bin is access all Gtk_Bin_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_bin_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Bin : not null access Gtk_Bin_Record) return Gtk.Widget.Gtk_Widget;
   --  Gets the child of the Gtk.Bin.Gtk_Bin, or null if the bin contains no
   --  child widget. The returned widget does not have a reference added, so
   --  you do not need to unref it.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Bin_Record, Gtk_Bin);
   function "+"
     (Widget : access Gtk_Bin_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Bin
   renames Implements_Gtk_Buildable.To_Object;

end Gtk.Bin;
