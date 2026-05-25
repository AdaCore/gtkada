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

--  Arranges its child widgets in rows and columns.
--
--  <picture> <source srcset="grid-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkGrid" src="grid.png"> </picture>
--  It supports arbitrary positions and horizontal/vertical spans.
--
--  Children are added using [methodGtk.Grid.attach]. They can span multiple
--  rows or columns. It is also possible to add a child next to an existing
--  child, using [methodGtk.Grid.attach_next_to]. To remove a child from the
--  grid, use [methodGtk.Grid.remove].
--
--  The behaviour of `GtkGrid` when several children occupy the same grid cell
--  is undefined.
--
--  # GtkGrid as GtkBuildable
--
--  Every child in a `GtkGrid` has access to a custom [ifaceGtk.Buildable]
--  element, called `<layout>`. It can by used to specify a position in the
--  grid and optionally spans. All properties that can be used in the
--  `<layout>` element are implemented by [classGtk.GridLayoutChild].
--
--  It is implemented by `GtkWidget` using [classGtk.LayoutManager].
--
--  To showcase it, here is a simple example:
--
--  ```xml <object class="GtkGrid" id="my_grid"> <child> <object
--  class="GtkButton" id="button1"> <property name="label">Button 1</property>
--  <layout> <property name="column">0</property> <property
--  name="row">0</property> </layout> </object> </child> <child> <object
--  class="GtkButton" id="button2"> <property name="label">Button 2</property>
--  <layout> <property name="column">1</property> <property
--  name="row">0</property> </layout> </object> </child> <child> <object
--  class="GtkButton" id="button3"> <property name="label">Button 3</property>
--  <layout> <property name="column">2</property> <property
--  name="row">0</property> <property name="row-span">2</property> </layout>
--  </object> </child> <child> <object class="GtkButton" id="button4">
--  <property name="label">Button 4</property> <layout> <property
--  name="column">0</property> <property name="row">1</property> <property
--  name="column-span">2</property> </layout> </object> </child> </object> ```
--
--  It organizes the first two buttons side-by-side in one cell each. The
--  third button is in the last column but spans across two rows. This is
--  defined by the `row-span` property. The last button is located in the
--  second row and spans across two columns, which is defined by the
--  `column-span` property.
--
--  # CSS nodes
--
--  `GtkGrid` uses a single CSS node with name `grid`.
--
--  # Accessibility
--
--  Until GTK 4.10, `GtkGrid` used the [enumGtk.AccessibleRole.group] role.
--
--  Starting from GTK 4.12, `GtkGrid` uses the
--  [enumGtk.AccessibleRole.generic] role.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Orientable;        use Gtk.Orientable;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Grid is

   type Gtk_Grid_Record is new Gtk_Widget_Record with null record;
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
       Column : Glib.Gint;
       Row    : Glib.Gint;
       Width  : Glib.Gint := 1;
       Height : Glib.Gint := 1);
   --  Adds a widget to the grid.
   --  The position of Child is determined by Column and Row. The number of
   --  "cells" that Child will occupy is determined by Width and Height.
   --  @param Child the widget to add
   --  @param Column the column number to attach the left side of Child to
   --  @param Row the row number to attach the top side of Child to
   --  @param Width the number of columns that Child will span
   --  @param Height the number of rows that Child will span

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
   --  Attaching widgets labeled `[1]`, `[2]`, `[3]` with `Sibling == null`
   --  and `Side == Gtk.Enums.Pos_Left` yields a layout of `[3][2][1]`.
   --  @param Child the widget to add
   --  @param Sibling the child of Grid that Child will be placed next to, or
   --  null to place Child at the beginning or end
   --  @param Side the side of Sibling that Child is positioned next to
   --  @param Width the number of columns that Child will span
   --  @param Height the number of rows that Child will span

   function Get_Baseline_Row
      (Self : not null access Gtk_Grid_Record) return Glib.Gint;
   --  Returns which row defines the global baseline of Grid.
   --  @return the row index defining the global baseline

   procedure Set_Baseline_Row
      (Self : not null access Gtk_Grid_Record;
       Row  : Glib.Gint);
   --  Sets which row defines the global baseline for the entire grid.
   --  Each row in the grid can have its own local baseline, but only one of
   --  those is global, meaning it will be the baseline in the parent of the
   --  Grid.
   --  @param Row the row index

   function Get_Child_At
      (Self   : not null access Gtk_Grid_Record;
       Column : Glib.Gint;
       Row    : Glib.Gint) return Gtk.Widget.Gtk_Widget;
   --  Gets the child of Grid whose area covers the grid cell at Column, Row.
   --  @param Column the left edge of the cell
   --  @param Row the top edge of the cell
   --  @return the child at the given position

   function Get_Column_Homogeneous
      (Self : not null access Gtk_Grid_Record) return Boolean;
   --  Returns whether all columns of Grid have the same width.
   --  @return whether all columns of Grid have the same width.

   procedure Set_Column_Homogeneous
      (Self        : not null access Gtk_Grid_Record;
       Homogeneous : Boolean);
   --  Sets whether all columns of Grid will have the same width.
   --  @param Homogeneous True to make columns homogeneous

   function Get_Column_Spacing
      (Self : not null access Gtk_Grid_Record) return Guint;
   --  Returns the amount of space between the columns of Grid.
   --  @return the column spacing of Grid

   procedure Set_Column_Spacing
      (Self    : not null access Gtk_Grid_Record;
       Spacing : Guint);
   --  Sets the amount of space between columns of Grid.
   --  @param Spacing the amount of space to insert between columns

   function Get_Row_Baseline_Position
      (Self : not null access Gtk_Grid_Record;
       Row  : Glib.Gint) return Gtk.Enums.Gtk_Baseline_Position;
   --  Returns the baseline position of Row.
   --  See [methodGtk.Grid.set_row_baseline_position].
   --  @param Row a row index
   --  @return the baseline position of Row

   procedure Set_Row_Baseline_Position
      (Self : not null access Gtk_Grid_Record;
       Row  : Glib.Gint;
       Pos  : Gtk.Enums.Gtk_Baseline_Position);
   --  Sets how the baseline should be positioned on Row of the grid, in case
   --  that row is assigned more space than is requested.
   --  The default baseline position is Gtk.Enums.Baseline_Position_Center.
   --  @param Row a row index
   --  @param Pos a `GtkBaselinePosition`

   function Get_Row_Homogeneous
      (Self : not null access Gtk_Grid_Record) return Boolean;
   --  Returns whether all rows of Grid have the same height.
   --  @return whether all rows of Grid have the same height.

   procedure Set_Row_Homogeneous
      (Self        : not null access Gtk_Grid_Record;
       Homogeneous : Boolean);
   --  Sets whether all rows of Grid will have the same height.
   --  @param Homogeneous True to make rows homogeneous

   function Get_Row_Spacing
      (Self : not null access Gtk_Grid_Record) return Guint;
   --  Returns the amount of space between the rows of Grid.
   --  @return the row spacing of Grid

   procedure Set_Row_Spacing
      (Self    : not null access Gtk_Grid_Record;
       Spacing : Guint);
   --  Sets the amount of space between rows of Grid.
   --  @param Spacing the amount of space to insert between rows

   procedure Insert_Column
      (Self     : not null access Gtk_Grid_Record;
       Position : Glib.Gint);
   --  Inserts a column at the specified position.
   --  Children which are attached at or to the right of this position are
   --  moved one column to the right. Children which span across this position
   --  are grown to span the new column.
   --  @param Position the position to insert the column at

   procedure Insert_Next_To
      (Self    : not null access Gtk_Grid_Record;
       Sibling : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Side    : Gtk.Enums.Gtk_Position_Type);
   --  Inserts a row or column at the specified position.
   --  The new row or column is placed next to Sibling, on the side determined
   --  by Side. If Side is Gtk.Enums.Pos_Top or Gtk.Enums.Pos_Bottom, a row is
   --  inserted. If Side is Gtk.Enums.Pos_Left of Gtk.Enums.Pos_Right, a column
   --  is inserted.
   --  @param Sibling the child of Grid that the new row or column will be
   --  placed next to
   --  @param Side the side of Sibling that Child is positioned next to

   procedure Insert_Row
      (Self     : not null access Gtk_Grid_Record;
       Position : Glib.Gint);
   --  Inserts a row at the specified position.
   --  Children which are attached at or below this position are moved one row
   --  down. Children which span across this position are grown to span the new
   --  row.
   --  @param Position the position to insert the row at

   procedure Query_Child
      (Self   : not null access Gtk_Grid_Record;
       Child  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Column : out Glib.Gint;
       Row    : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint);
   --  Queries the attach points and spans of Child inside the given
   --  `GtkGrid`.
   --  @param Child a `GtkWidget` child of Grid
   --  @param Column the column used to attach the left side of Child
   --  @param Row the row used to attach the top side of Child
   --  @param Width the number of columns Child spans
   --  @param Height the number of rows Child spans

   procedure Remove
      (Self  : not null access Gtk_Grid_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes a child from Grid.
   --  The child must have been added with [methodGtk.Grid.attach] or
   --  [methodGtk.Grid.attach_next_to].
   --  @param Child the child widget to remove

   procedure Remove_Column
      (Self     : not null access Gtk_Grid_Record;
       Position : Glib.Gint);
   --  Removes a column from the grid.
   --  Children that are placed in this column are removed, spanning children
   --  that overlap this column have their width reduced by one, and children
   --  after the column are moved to the left.
   --  @param Position the position of the column to remove

   procedure Remove_Row
      (Self     : not null access Gtk_Grid_Record;
       Position : Glib.Gint);
   --  Removes a row from the grid.
   --  Children that are placed in this row are removed, spanning children
   --  that overlap this row have their height reduced by one, and children
   --  below the row are moved up.
   --  @param Position the position of the row to remove

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Grid_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Grid_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Grid_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Grid_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Grid_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Grid_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Grid_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Grid_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Grid_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Grid_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Grid_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Grid_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Grid_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Grid_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Grid_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

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
   --  The row to align to the baseline when valign is using baseline
   --  alignment.

   Column_Homogeneous_Property : constant Glib.Properties.Property_Boolean;
   --  If True, the columns are all the same width.

   Column_Spacing_Property : constant Glib.Properties.Property_Int;
   --  The amount of space between two consecutive columns.

   Row_Homogeneous_Property : constant Glib.Properties.Property_Boolean;
   --  If True, the rows are all the same height.

   Row_Spacing_Property : constant Glib.Properties.Property_Int;
   --  The amount of space between two consecutive rows.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"
   --
   --  - "Gtk.Orientable"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Grid_Record, Gtk_Grid);
   function "+"
     (Widget : access Gtk_Grid_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Grid
   renames Implements_Gtk_Accessible.To_Object;

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

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Grid_Record, Gtk_Grid);
   function "+"
     (Widget : access Gtk_Grid_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Grid
   renames Implements_Gtk_Constraint_Target.To_Object;

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
