-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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
--  The Gtk_Fixed widget is a container which can place child widgets at fixed
--  positions and with fixed sizes, given in pixels.
--
--  Note that it is usually bad practice to use the Gtk_Fixed container in
--  GtkAda. Instead, you should consider using one of the other many containers
--  available, that will allow you to handle resizing of your windows, as well
--  as font size changes easily.
--
--  </description>
--  <c_version>1.2.8</c_version>

with Gtk.Container;
with Gtk.Widget;

package Gtk.Fixed is

   type Gtk_Fixed_Record is new Gtk.Container.Gtk_Container_Record
     with private;
   type Gtk_Fixed is access all Gtk_Fixed_Record'Class;

   procedure Gtk_New (Fixed : out Gtk_Fixed);
   --  Create a new fixed container.

   procedure Initialize (Fixed : access Gtk_Fixed_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Fixed.

   function Get_Children (Fixed : access Gtk_Fixed_Record)
     return Widget.Widget_List.Glist;
   --  Return the list of Widgets contained in a Gtk_Fixed.

   procedure Move
     (Fixed  : access Gtk_Fixed_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      X      : in Gint16;
      Y      : in Gint16);
   --  Move a child of a GtkFixed container to the given position.
   --  X indicates the horizontal position to place the widget at.
   --  Y is the vertical position to place the widget at.

   procedure Put
     (Fixed  : access Gtk_Fixed_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      X      : in Gint16;
      Y      : in Gint16);
   --  Add Widget to a Fixed container at the given position.
   --  X indicates the horizontal position to place the widget at.
   --  Y is the vertical position to place the widget at.

   ----------------------
   -- Support for Gate --
   ----------------------

   procedure Generate (N : in Node_Ptr; File : in File_Type);
   --  Gate internal function

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Fixed_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

   pragma Import (C, Get_Type, "gtk_fixed_get_type");

end Gtk.Fixed;
