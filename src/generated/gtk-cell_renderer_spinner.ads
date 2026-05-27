------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Renders a spinning animation in a cell
--
--  `GtkCellRendererSpinner` renders a spinning animation in a cell, very
--  similar to `GtkSpinner`. It can often be used as an alternative to a
--  `GtkCellRendererProgress` for displaying indefinite activity, instead of
--  actual progress.
--
--  To start the animation in a cell, set the `GtkCellRendererSpinner:active`
--  property to True and increment the `GtkCellRendererSpinner:pulse` property
--  at regular intervals. The usual way to set the cell renderer properties for
--  each cell is to bind them to columns in your tree model using e.g.
--  gtk_tree_view_column_add_attribute.
--
--  <group>Trees and Lists</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.Properties;   use Glib.Properties;
with Gtk.Cell_Renderer; use Gtk.Cell_Renderer;
with Gtk.Enums;         use Gtk.Enums;

package Gtk.Cell_Renderer_Spinner is

   pragma Obsolescent;
   --  List views use widgets to display their contents. You should use [class@Gtk.Spinner] instead

   type Gtk_Cell_Renderer_Spinner_Record is new Gtk_Cell_Renderer_Record with null record;
   type Gtk_Cell_Renderer_Spinner is access all Gtk_Cell_Renderer_Spinner_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Cell_Renderer_Spinner);
   procedure Initialize
      (Self : not null access Gtk_Cell_Renderer_Spinner_Record'Class);
   --  Returns a new cell renderer which will show a spinner to indicate
   --  activity.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Cell_Renderer_Spinner_New return Gtk_Cell_Renderer_Spinner;
   --  Returns a new cell renderer which will show a spinner to indicate
   --  activity.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cell_renderer_spinner_get_type");

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Active_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the spinner is active (ie. shown) in the cell

   Pulse_Property : constant Glib.Properties.Property_Uint;
   --  Pulse of the spinner. Increment this value to draw the next frame of
   --  the spinner animation. Usually, you would update this value in a
   --  timeout.
   --
   --  By default, the `GtkSpinner` widget draws one full cycle of the
   --  animation, consisting of 12 frames, in 750 milliseconds.

   Size_Property : constant Gtk.Enums.Property_Gtk_Icon_Size;
   --  The `GtkIconSize` value that specifies the size of the rendered
   --  spinner.

private
   Size_Property : constant Gtk.Enums.Property_Gtk_Icon_Size :=
     Gtk.Enums.Build ("size");
   Pulse_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("pulse");
   Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("active");
end Gtk.Cell_Renderer_Spinner;
