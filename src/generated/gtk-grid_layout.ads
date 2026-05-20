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

--  Arranges child widgets in rows and columns.
--
--  Children have an "attach point" defined by the horizontal and vertical
--  index of the cell they occupy; children can span multiple rows or columns.
--  The layout properties for setting the attach points and spans are set using
--  the [classGtk.GridLayoutChild] associated to each child widget.
--
--  The behaviour of `GtkGridLayout` when several children occupy the same
--  grid cell is undefined.
--
--  `GtkGridLayout` can be used like a `GtkBoxLayout` if all children are
--  attached to the same row or column; however, if you only ever need a single
--  row or column, you should consider using `GtkBoxLayout`.
--
--  <group>Layout containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Properties;    use Glib.Properties;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Layout_Manager; use Gtk.Layout_Manager;

package Gtk.Grid_Layout is

   type Gtk_Grid_Layout_Record is new Gtk_Layout_Manager_Record with null record;
   type Gtk_Grid_Layout is access all Gtk_Grid_Layout_Record'Class;

   type Gtk_Grid_Layout_Child_Record is new Gtk_Layout_Child_Record with null record;
   type Gtk_Grid_Layout_Child is access all Gtk_Grid_Layout_Child_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Grid : out Gtk_Grid_Layout);
   procedure Initialize
      (Grid : not null access Gtk_Grid_Layout_Record'Class);
   --  Creates a new `GtkGridLayout`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Grid_Layout_New return Gtk_Grid_Layout;
   --  Creates a new `GtkGridLayout`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_grid_layout_get_type");

   function Grid_Layout_Child_Get_Type return Glib.GType;
   pragma Import (C, Grid_Layout_Child_Get_Type, "gtk_grid_layout_child_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Baseline_Row
      (Grid : not null access Gtk_Grid_Layout_Record) return Glib.Gint;
   --  Retrieves the row set with Gtk.Grid_Layout.Set_Baseline_Row.
   --  @return the global baseline row

   procedure Set_Baseline_Row
      (Grid : not null access Gtk_Grid_Layout_Record;
       Row  : Glib.Gint);
   --  Sets which row defines the global baseline for the entire grid.
   --  Each row in the grid can have its own local baseline, but only one of
   --  those is global, meaning it will be the baseline in the parent of the
   --  Grid.
   --  @param Row the row index

   function Get_Column_Homogeneous
      (Grid : not null access Gtk_Grid_Layout_Record) return Boolean;
   --  Checks whether all columns of Grid should have the same width.
   --  @return True if the columns are homogeneous, and False otherwise

   procedure Set_Column_Homogeneous
      (Grid        : not null access Gtk_Grid_Layout_Record;
       Homogeneous : Boolean);
   --  Sets whether all columns of Grid should have the same width.
   --  @param Homogeneous True to make columns homogeneous

   function Get_Column_Spacing
      (Grid : not null access Gtk_Grid_Layout_Record) return Guint;
   --  Retrieves the spacing set with Gtk.Grid_Layout.Set_Column_Spacing.
   --  @return the spacing between consecutive columns

   procedure Set_Column_Spacing
      (Grid    : not null access Gtk_Grid_Layout_Record;
       Spacing : Guint);
   --  Sets the amount of space to insert between consecutive columns.
   --  @param Spacing the amount of space between columns, in pixels

   function Get_Row_Baseline_Position
      (Grid : not null access Gtk_Grid_Layout_Record;
       Row  : Glib.Gint) return Gtk.Enums.Gtk_Baseline_Position;
   --  Returns the baseline position of Row.
   --  If no value has been set with
   --  [methodGtk.GridLayout.set_row_baseline_position], the default value of
   --  Gtk.Enums.Baseline_Position_Center is returned.
   --  @param Row a row index
   --  @return the baseline position of Row

   procedure Set_Row_Baseline_Position
      (Grid : not null access Gtk_Grid_Layout_Record;
       Row  : Glib.Gint;
       Pos  : Gtk.Enums.Gtk_Baseline_Position);
   --  Sets how the baseline should be positioned on Row of the grid, in case
   --  that row is assigned more space than is requested.
   --  @param Row a row index
   --  @param Pos a `GtkBaselinePosition`

   function Get_Row_Homogeneous
      (Grid : not null access Gtk_Grid_Layout_Record) return Boolean;
   --  Checks whether all rows of Grid should have the same height.
   --  @return True if the rows are homogeneous, and False otherwise

   procedure Set_Row_Homogeneous
      (Grid        : not null access Gtk_Grid_Layout_Record;
       Homogeneous : Boolean);
   --  Sets whether all rows of Grid should have the same height.
   --  @param Homogeneous True to make rows homogeneous

   function Get_Row_Spacing
      (Grid : not null access Gtk_Grid_Layout_Record) return Guint;
   --  Retrieves the spacing set with Gtk.Grid_Layout.Set_Row_Spacing.
   --  @return the spacing between consecutive rows

   procedure Set_Row_Spacing
      (Grid    : not null access Gtk_Grid_Layout_Record;
       Spacing : Guint);
   --  Sets the amount of space to insert between consecutive rows.
   --  @param Spacing the amount of space between rows, in pixels

   function Get_Column
      (Child : not null access Gtk_Grid_Layout_Child_Record)
       return Glib.Gint;
   --  Retrieves the column number to which Child attaches its left side.
   --  @return the column number

   procedure Set_Column
      (Child  : not null access Gtk_Grid_Layout_Child_Record;
       Column : Glib.Gint);
   --  Sets the column number to attach the left side of Child.
   --  @param Column the attach point for Child

   function Get_Column_Span
      (Child : not null access Gtk_Grid_Layout_Child_Record)
       return Glib.Gint;
   --  Retrieves the number of columns that Child spans to.
   --  @return the number of columns

   procedure Set_Column_Span
      (Child : not null access Gtk_Grid_Layout_Child_Record;
       Span  : Glib.Gint);
   --  Sets the number of columns Child spans to.
   --  @param Span the span of Child

   function Get_Row
      (Child : not null access Gtk_Grid_Layout_Child_Record)
       return Glib.Gint;
   --  Retrieves the row number to which Child attaches its top side.
   --  @return the row number

   procedure Set_Row
      (Child : not null access Gtk_Grid_Layout_Child_Record;
       Row   : Glib.Gint);
   --  Sets the row to place Child in.
   --  @param Row the row for Child

   function Get_Row_Span
      (Child : not null access Gtk_Grid_Layout_Child_Record)
       return Glib.Gint;
   --  Retrieves the number of rows that Child spans to.
   --  @return the number of row

   procedure Set_Row_Span
      (Child : not null access Gtk_Grid_Layout_Child_Record;
       Span  : Glib.Gint);
   --  Sets the number of rows Child spans to.
   --  @param Span the span of Child

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Baseline_Row_Property : constant Glib.Properties.Property_Int;
   --  The row to align to the baseline, when `GtkWidget:valign` is set to
   --  Gtk.Widget.Align_Baseline.

   Column_Homogeneous_Property : constant Glib.Properties.Property_Boolean;
   --  Whether all the columns in the grid have the same width.

   Column_Spacing_Property : constant Glib.Properties.Property_Int;
   --  The amount of space between to consecutive columns.

   Row_Homogeneous_Property : constant Glib.Properties.Property_Boolean;
   --  Whether all the rows in the grid have the same height.

   Row_Spacing_Property : constant Glib.Properties.Property_Int;
   --  The amount of space between to consecutive rows.

   Column_Property : constant Glib.Properties.Property_Int;
   --  The column to place the child in.

   Column_Span_Property : constant Glib.Properties.Property_Int;
   --  The number of columns the child spans to.

   Row_Property : constant Glib.Properties.Property_Int;
   --  The row to place the child in.

   Row_Span_Property : constant Glib.Properties.Property_Int;
   --  The number of rows the child spans to.

private
   Row_Span_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("row-span");
   Row_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("row");
   Column_Span_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("column-span");
   Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("column");
   Row_Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("row-spacing");
   Row_Homogeneous_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("row-homogeneous");
   Column_Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("column-spacing");
   Column_Homogeneous_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("column-homogeneous");
   Baseline_Row_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("baseline-row");
end Gtk.Grid_Layout;
