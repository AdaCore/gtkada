------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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
--  Gtk_Cell_Renderer_Spin renders text in a cell like Gtk_Cell_Renderer_Text
--  from which it is derived. But while Gtk_Cell_Renderer_Text offers a simple
--  entry to edit the text, Gtk_Cell_Renderer_Spin offers a Gtk_Spin_Button
--  widget. Of course, that means that the text has to be parseable as a
--  floating point number.
--
--  The range of the spinbutton is taken from the adjustment property of the
--  cell renderer, which can be set explicitly or mapped to a column in the
--  tree model, like all properties of cell renders. Gtk_Cell_Renderer_Spin
--  also has properties for the climb rate and the number of digits to display.
--  Other Gtk_Spin_Button properties can be set in a handler for the
--  start-editing signal.
--
--  The Gtk_Cell_Renderer_Spin cell renderer was added in GTK+ 2.10.
--  </description>
--  <c_version>2.16.6</c_version>

with Glib.Properties;
with Gtk.Cell_Renderer_Text;

package Gtk.Cell_Renderer_Spin is

   type Gtk_Cell_Renderer_Spin_Record is
     new Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text_Record with private;
   type Gtk_Cell_Renderer_Spin is
     access all Gtk_Cell_Renderer_Spin_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Cell_Renderer_Spin);
   --  Creates a new Gtk_Cell_Renderer_Spin.

   procedure Initialize (Widget : access Gtk_Cell_Renderer_Spin_Record'Class);
   --  Creates a new Gtk_Cell_Renderer_Spin.

   function Get_Type return GType;
   --  Return the internal value associated with this widget.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  Name:  Adjustment_Property
   --  Type:  Object
   --  Descr: The adjustment that holds the value of the spinbutton.
   --
   --  Name:  Climb_Rate_Property
   --  Type:  Double
   --  Descr: The acceleration rate when you hold down a button
   --
   --  Name:  Digits_Property
   --  Type:  Uint
   --  Descr: The number of decimal places to display
   --
   --  </properties>

   Adjustment_Property : constant Glib.Properties.Property_Object;
   Climb_Rate_Property : constant Glib.Properties.Property_Double;
   Digits_Property     : constant Glib.Properties.Property_Uint;

private
   type Gtk_Cell_Renderer_Spin_Record is
     new Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text_Record with null record;

   Adjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("adjustment");
   Climb_Rate_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("climb-rate");
   Digits_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("digits");

   pragma Import (C, Get_Type, "gtk_cell_renderer_spin_get_type");
end Gtk.Cell_Renderer_Spin;
