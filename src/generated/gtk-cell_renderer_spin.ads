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
--  Gtk.Cell_Renderer_Spin.Gtk_Cell_Renderer_Spin renders text in a cell like
--  Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text from which it is derived. But
--  while Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text offers a simple entry
--  to edit the text, Gtk.Cell_Renderer_Spin.Gtk_Cell_Renderer_Spin offers a
--  Gtk.Spin_Button.Gtk_Spin_Button widget. Of course, that means that the text
--  has to be parseable as a floating point number.
--
--  The range of the spinbutton is taken from the adjustment property of the
--  cell renderer, which can be set explicitly or mapped to a column in the
--  tree model, like all properties of cell renders.
--  Gtk.Cell_Renderer_Spin.Gtk_Cell_Renderer_Spin also has properties for the
--  Gtk.Cell_Renderer_Spin.Gtk_Cell_Renderer_Spin:climb-rate and the number of
--  Gtk.Cell_Renderer_Spin.Gtk_Cell_Renderer_Spin:digits to display. Other
--  Gtk.Spin_Button.Gtk_Spin_Button properties can be set in a handler for the
--  Gtk.Cell_Renderer.Gtk_Cell_Renderer::editing-started signal.
--
--  The Gtk.Cell_Renderer_Spin.Gtk_Cell_Renderer_Spin cell renderer was added
--  in GTK+ 2.10.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                   use Glib;
with Glib.Properties;        use Glib.Properties;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;

package Gtk.Cell_Renderer_Spin is

   type Gtk_Cell_Renderer_Spin_Record is new Gtk_Cell_Renderer_Text_Record with null record;
   type Gtk_Cell_Renderer_Spin is access all Gtk_Cell_Renderer_Spin_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Cell_Renderer_Spin);
   procedure Initialize
      (Self : not null access Gtk_Cell_Renderer_Spin_Record'Class);
   --  Creates a new Gtk.Cell_Renderer_Spin.Gtk_Cell_Renderer_Spin.
   --  Since: gtk+ 2.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Cell_Renderer_Spin_New return Gtk_Cell_Renderer_Spin;
   --  Creates a new Gtk.Cell_Renderer_Spin.Gtk_Cell_Renderer_Spin.
   --  Since: gtk+ 2.10

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cell_renderer_spin_get_type");

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Adjustment_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Adjustment.Gtk_Adjustment
   --  The adjustment that holds the value of the spinbutton. This must be
   --  non-null for the cell renderer to be editable.

   Climb_Rate_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The acceleration rate when you hold down a button.

   The_Digits_Property : constant Glib.Properties.Property_Uint;
   --  The number of decimal places to display.

private
   The_Digits_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("digits");
   Climb_Rate_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("climb-rate");
   Adjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("adjustment");
end Gtk.Cell_Renderer_Spin;
