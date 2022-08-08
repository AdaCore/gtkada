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
--  GtkGrid is a container which arranges its child widgets in rows and
--  columns, with arbitrary positions and horizontal/vertical spans.
--
--  Children are added using Gtk.Grid.Attach. They can span multiple rows or
--  columns. It is also possible to add a child next to an existing child,
--  using Gtk.Grid.Attach_Next_To. The behaviour of GtkGrid when several
--  children occupy the same grid cell is undefined.
--
--  GtkGrid can be used like a Gtk.Box.Gtk_Box by just using
--  Gtk.Container.Add, which will place children next to each other in the
--  direction determined by the Gtk.Orientable.Gtk_Orientable:orientation
--  property. However, if all you want is a single row or column, then
--  Gtk.Box.Gtk_Box is the preferred widget.
--
--  # CSS nodes
--
--  GtkGrid uses a single CSS node with name grid.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Container;   use Gtk.Container;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Orientable;  use Gtk.Orientable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Grid is

   type Gtk_Grid_Record is new Gtk_Container_Record with null record;
   type Gtk_Grid is access all Gtk_Grid_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Grid);
   procedure Initialize (Self : not null access Gtk_Grid_Record'Class);
   --  Creates a new grid widget.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Grid_New return Gtk_Grid;
   --  Creates a new grid widget.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_grid_get_type");

   -------------
   -- Methods --
   -------------

   procedure Attach
      (Self   : not null access Gtk_Grid_Record;
       Child  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Left   : Glib.Gint;
       Top    : Glib.Gint;
       Width  : Glib.Gint := 1;
       Height : Glib.Gint := 1);
   --  Adds a widget to the grid.
   --  The position of Child is determined by Left and Top. The number of
   --  "cells" that Child will occupy is determined by Width and Height.
   --  "child": the widget to add
   --  "left": the column number to attach the left side of Child to
   --  "top": the row number to attach the top side of Child to
   --  "width": the number of columns that Child will span
   --  "height": the number of rows that Child will span

   procedure Attach_Next_To
      (Self    : not null access Gtk_Grid_Record;
       Child   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Sibling : access Gtk.Widget.Gtk_Widget_Record'Class;
       Side    : Gtk.Enums.Gtk_Position_Type;
       Width   : Glib.Gint := 1;
       Height  : Glib.Gint := 1);
   --  Adds a widget to the grid.
   --  The widget is placed next to Sibling, on the side determined by Side.
   --  When Sibling is null, the widget is placed in row (for left or right
   --  placement) or column 0 (for top or bottom placement), at the end
   --  indicated by Side.
   --  Attaching widgets labeled [1], [2], [3] with Sibling == null and Side
   --  == Gtk.Enums.Pos_Left yields a layout of [3][2][1].
   --  "child": the widget to add
   --  "sibling": the child of Grid that Child will be placed next to, or null
   --  to place Child at the beginning or end
   --  "side": the side of Sibling that Child is positioned next to
   --  "width": the number of columns that Child will span
   --  "height": the number of rows that Child will span

   function Get_Baseline_Row
      (Self : not null access Gtk_Grid_Record) return Glib.Gint;
   --  Returns which row defines the global baseline of Grid.
   --  Since: gtk+ 3.10

   procedure Set_Baseline_Row
      (Self : not null access Gtk_Grid_Record;
       Row  : Glib.Gint);
   --  Sets which row defines the global baseline for the entire grid. Each
   --  row in the grid can have its own local baseline, but only one of those
   --  is global, meaning it will be the baseline in the parent of the Grid.
   --  Since: gtk+ 3.10
   --  "row": the row index

   function Get_Child_At
      (Self : not null access Gtk_Grid_Record;
       Left : Glib.Gint;
       Top  : Glib.Gint) return Gtk.Widget.Gtk_Widget;
   --  Gets the child of Grid whose area covers the grid cell whose upper left
   --  corner is at Left, Top.
   --  Since: gtk+ 3.2
   --  "left": the left edge of the cell
   --  "top": the top edge of the cell

   function Get_Column_Homogeneous
      (Self : not null access Gtk_Grid_Record) return Boolean;
   --  Returns whether all columns of Grid have the same width.

   procedure Set_Column_Homogeneous
      (Self        : not null access Gtk_Grid_Record;
       Homogeneous : Boolean);
   --  Sets whether all columns of Grid will have the same width.
   --  "homogeneous": True to make columns homogeneous

   function Get_Column_Spacing
      (Self : not null access Gtk_Grid_Record) return Guint;
   --  Returns the amount of space between the columns of Grid.

   procedure Set_Column_Spacing
      (Self    : not null access Gtk_Grid_Record;
       Spacing : Guint);
   --  Sets the amount of space between columns of Grid.
   --  "spacing": the amount of space to insert between columns

   function Get_Row_Baseline_Position
      (Self : not null access Gtk_Grid_Record;
       Row  : Glib.Gint) return Gtk.Enums.Gtk_Baseline_Position;
   --  Returns the baseline position of Row as set by
   --  Gtk.Grid.Set_Row_Baseline_Position or the default value
   --  Gtk.Enums.Baseline_Position_Center.
   --  Since: gtk+ 3.10
   --  "row": a row index

   procedure Set_Row_Baseline_Position
      (Self : not null access Gtk_Grid_Record;
       Row  : Glib.Gint;
       Pos  : Gtk.Enums.Gtk_Baseline_Position);
   --  Sets how the baseline should be positioned on Row of the grid, in case
   --  that row is assigned more space than is requested.
   --  Since: gtk+ 3.10
   --  "row": a row index
   --  "pos": a Gtk.Enums.Gtk_Baseline_Position

   function Get_Row_Homogeneous
      (Self : not null access Gtk_Grid_Record) return Boolean;
   --  Returns whether all rows of Grid have the same height.

   procedure Set_Row_Homogeneous
      (Self        : not null access Gtk_Grid_Record;
       Homogeneous : Boolean);
   --  Sets whether all rows of Grid will have the same height.
   --  "homogeneous": True to make rows homogeneous

   function Get_Row_Spacing
      (Self : not null access Gtk_Grid_Record) return Guint;
   --  Returns the amount of space between the rows of Grid.

   procedure Set_Row_Spacing
      (Self    : not null access Gtk_Grid_Record;
       Spacing : Guint);
   --  Sets the amount of space between rows of Grid.
   --  "spacing": the amount of space to insert between rows

   procedure Insert_Column
      (Self     : not null access Gtk_Grid_Record;
       Position : Glib.Gint);
   --  Inserts a column at the specified position.
   --  Children which are attached at or to the right of this position are
   --  moved one column to the right. Children which span across this position
   --  are grown to span the new column.
   --  Since: gtk+ 3.2
   --  "position": the position to insert the column at

   procedure Insert_Next_To
      (Self    : not null access Gtk_Grid_Record;
       Sibling : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Side    : Gtk.Enums.Gtk_Position_Type);
   --  Inserts a row or column at the specified position.
   --  The new row or column is placed next to Sibling, on the side determined
   --  by Side. If Side is Gtk.Enums.Pos_Top or Gtk.Enums.Pos_Bottom, a row is
   --  inserted. If Side is Gtk.Enums.Pos_Left of Gtk.Enums.Pos_Right, a column
   --  is inserted.
   --  Since: gtk+ 3.2
   --  "sibling": the child of Grid that the new row or column will be placed
   --  next to
   --  "side": the side of Sibling that Child is positioned next to

   procedure Insert_Row
      (Self     : not null access Gtk_Grid_Record;
       Position : Glib.Gint);
   --  Inserts a row at the specified position.
   --  Children which are attached at or below this position are moved one row
   --  down. Children which span across this position are grown to span the new
   --  row.
   --  Since: gtk+ 3.2
   --  "position": the position to insert the row at

   procedure Remove_Column
      (Self     : not null access Gtk_Grid_Record;
       Position : Glib.Gint);
   --  Removes a column from the grid.
   --  Children that are placed in this column are removed, spanning children
   --  that overlap this column have their width reduced by one, and children
   --  after the column are moved to the left.
   --  Since: gtk+ 3.10
   --  "position": the position of the column to remove

   procedure Remove_Row
      (Self     : not null access Gtk_Grid_Record;
       Position : Glib.Gint);
   --  Removes a row from the grid.
   --  Children that are placed in this row are removed, spanning children
   --  that overlap this row have their height reduced by one, and children
   --  below the row are moved up.
   --  Since: gtk+ 3.10
   --  "position": the position of the row to remove

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Orientation
      (Self : not null access Gtk_Grid_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Grid_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Baseline_Row_Property : constant Glib.Properties.Property_Int;

   Column_Homogeneous_Property : constant Glib.Properties.Property_Boolean;

   Column_Spacing_Property : constant Glib.Properties.Property_Int;

   Row_Homogeneous_Property : constant Glib.Properties.Property_Boolean;

   Row_Spacing_Property : constant Glib.Properties.Property_Int;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Grid_Record, Gtk_Grid);
   function "+"
     (Widget : access Gtk_Grid_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Grid
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Grid_Record, Gtk_Grid);
   function "+"
     (Widget : access Gtk_Grid_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Grid
   renames Implements_Gtk_Orientable.To_Object;

private
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
end Gtk.Grid;
