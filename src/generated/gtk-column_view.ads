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

--  Presents a large dynamic list of items using multiple columns with
--  headers.
--
--  `GtkColumnView` uses the factories of its columns to generate a cell
--  widget for each column, for each visible item and displays them together as
--  the row for this item.
--
--  The [propertyGtk.ColumnView:show-row-separators] and
--  [propertyGtk.ColumnView:show-column-separators] properties offer a simple
--  way to display separators between the rows or columns.
--
--  `GtkColumnView` allows the user to select items according to the selection
--  characteristics of the model. For models that allow multiple selected
--  items, it is possible to turn on *rubberband selection*, using
--  [propertyGtk.ColumnView:enable-rubberband].
--
--  The column view supports sorting that can be customized by the user by
--  clicking on column headers. To set this up, the `GtkSorter` returned by
--  [methodGtk.ColumnView.get_sorter] must be attached to a sort model for the
--  data that the view is showing, and the columns must have sorters attached
--  to them by calling [methodGtk.ColumnViewColumn.set_sorter]. The initial
--  sort order can be set with [methodGtk.ColumnView.sort_by_column].
--
--  The column view also supports interactive resizing and reordering of
--  columns, via Drag-and-Drop of the column headers. This can be enabled or
--  disabled with the [propertyGtk.ColumnView:reorderable] and
--  [propertyGtk.ColumnViewColumn:resizable] properties.
--
--  To learn more about the list widget framework, see the
--  [overview](section-list-widget.html).
--
--  # CSS nodes
--
--  ```
--  columnview[.column-separators][.rich-list][.navigation-sidebar][.data-table]
--  ├── header │ ├── <column header> ┊ ┊ │ ╰── <column header> │ ├── listview │
--  ┊ ╰── [rubberband] ```
--
--  `GtkColumnView` uses a single CSS node named columnview. It may carry the
--  .column-separators style class, when
--  [propertyGtk.ColumnView:show-column-separators] property is set. Header
--  widgets appear below a node with name header. The rows are contained in a
--  `GtkListView` widget, so there is a listview node with the same structure
--  as for a standalone `GtkListView` widget. If
--  [propertyGtk.ColumnView:show-row-separators] is set, it will be passed on
--  to the list view, causing its CSS node to carry the .separators style
--  class. For rubberband selection, a node with name rubberband is used.
--
--  The main columnview node may also carry style classes to select the style
--  of [list presentation](section-list-widget.htmllist-styles): .rich-list,
--  .navigation-sidebar or .data-table.
--
--  # Accessibility
--
--  `GtkColumnView` uses the [enumGtk.AccessibleRole.tree_grid] role, header
--  title widgets are using the [enumGtk.AccessibleRole.column_header] role.
--  The row widgets are using the [enumGtk.AccessibleRole.row] role, and
--  individual cells are using the [enumGtk.AccessibleRole.grid_cell] role

pragma Warnings (Off, "*is already use-visible*");
with Glib;                   use Glib;
with Glib.List_Model;        use Glib.List_Model;
with Glib.Object;            use Glib.Object;
with Glib.Properties;        use Glib.Properties;
with Glib.Types;             use Glib.Types;
with Gtk.Accessible;         use Gtk.Accessible;
with Gtk.Atcontext;          use Gtk.Atcontext;
with Gtk.Buildable;          use Gtk.Buildable;
with Gtk.Column_View_Column; use Gtk.Column_View_Column;
with Gtk.Constraint_Target;  use Gtk.Constraint_Target;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.List_Item_Factory;  use Gtk.List_Item_Factory;
with Gtk.Scroll_Info;        use Gtk.Scroll_Info;
with Gtk.Selection_Model;    use Gtk.Selection_Model;
with Gtk.Sorter;             use Gtk.Sorter;
with Gtk.Widget;             use Gtk.Widget;

package Gtk.Column_View is

   type Gtk_Column_View_Record is new Gtk_Widget_Record with null record;
   type Gtk_Column_View is access all Gtk_Column_View_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self  : out Gtk_Column_View;
       Model : Gtk.Selection_Model.Gtk_Selection_Model);
   procedure Initialize
      (Self  : not null access Gtk_Column_View_Record'Class;
       Model : Gtk.Selection_Model.Gtk_Selection_Model);
   --  Creates a new `GtkColumnView`.
   --  You most likely want to call [methodGtk.ColumnView.append_column] to
   --  add columns next.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Model the list model to use

   function Gtk_Column_View_New
      (Model : Gtk.Selection_Model.Gtk_Selection_Model)
       return Gtk_Column_View;
   --  Creates a new `GtkColumnView`.
   --  You most likely want to call [methodGtk.ColumnView.append_column] to
   --  add columns next.
   --  @param Model the list model to use

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_column_view_get_type");

   -------------
   -- Methods --
   -------------

   procedure Append_Column
      (Self   : not null access Gtk_Column_View_Record;
       Column : not null access Gtk.Column_View_Column.Gtk_Column_View_Column_Record'Class);
   --  Appends the Column to the end of the columns in Self.
   --  @param Column a column that hasn't been added to a `GtkColumnView` yet

   function Get_Columns
      (Self : not null access Gtk_Column_View_Record)
       return Glib.List_Model.Glist_Model;
   --  Gets the list of columns in this column view.
   --  This list is constant over the lifetime of Self and can be used to
   --  monitor changes to the columns of Self by connecting to the
   --  [signalGio.ListModel::items-changed] signal.
   --  @return The list managing the columns

   function Get_Enable_Rubberband
      (Self : not null access Gtk_Column_View_Record) return Boolean;
   --  Returns whether rows can be selected by dragging with the mouse.
   --  @return true if rubberband selection is enabled

   procedure Set_Enable_Rubberband
      (Self              : not null access Gtk_Column_View_Record;
       Enable_Rubberband : Boolean);
   --  Sets whether selections can be changed by dragging with the mouse.
   --  @param Enable_Rubberband whether to enable rubberband selection

   function Get_Header_Factory
      (Self : not null access Gtk_Column_View_Record)
       return Gtk.List_Item_Factory.Gtk_List_Item_Factory;
   --  Gets the factory that's currently used to populate section headers.
   --  Since: gtk+ 4.12
   --  @return The factory in use

   procedure Set_Header_Factory
      (Self    : not null access Gtk_Column_View_Record;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class);
   --  Sets the factory to use for populating the [classGtk.ListHeader]
   --  objects used in section headers.
   --  If this factory is set to `NULL`, the list will not show section
   --  headers.
   --  Since: gtk+ 4.12
   --  @param Factory the factory to use

   function Get_Model
      (Self : not null access Gtk_Column_View_Record)
       return Gtk.Selection_Model.Gtk_Selection_Model;
   --  Gets the model that's currently used to read the items displayed.
   --  @return The model in use

   procedure Set_Model
      (Self  : not null access Gtk_Column_View_Record;
       Model : Gtk.Selection_Model.Gtk_Selection_Model);
   --  Sets the model to use.
   --  This must be a [ifaceGtk.SelectionModel].
   --  @param Model the model to use

   function Get_Reorderable
      (Self : not null access Gtk_Column_View_Record) return Boolean;
   --  Returns whether columns are reorderable.
   --  @return true if columns are reorderable

   procedure Set_Reorderable
      (Self        : not null access Gtk_Column_View_Record;
       Reorderable : Boolean);
   --  Sets whether columns should be reorderable by dragging.
   --  @param Reorderable whether columns should be reorderable

   function Get_Row_Factory
      (Self : not null access Gtk_Column_View_Record)
       return Gtk.List_Item_Factory.Gtk_List_Item_Factory;
   --  Gets the factory set via [methodGtk.ColumnView.set_row_factory].
   --  Since: gtk+ 4.12
   --  @return The factory

   procedure Set_Row_Factory
      (Self    : not null access Gtk_Column_View_Record;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class);
   --  Sets the factory used for configuring rows.
   --  The factory must be for configuring [classGtk.ColumnViewRow] objects.
   --  If this factory is not set - which is the default - then the defaults
   --  will be used.
   --  This factory is not used to set the widgets displayed in the individual
   --  cells. For that see [methodGtkcolumnviewcolumn.set_factory] and
   --  [classGtkcolumnviewcell].
   --  Since: gtk+ 4.12
   --  @param Factory The row factory

   function Get_Show_Column_Separators
      (Self : not null access Gtk_Column_View_Record) return Boolean;
   --  Returns whether the list should show separators between columns.
   --  @return true if the list shows column separators

   procedure Set_Show_Column_Separators
      (Self                   : not null access Gtk_Column_View_Record;
       Show_Column_Separators : Boolean);
   --  Sets whether the list should show separators between columns.
   --  @param Show_Column_Separators whether to show column separators

   function Get_Show_Row_Separators
      (Self : not null access Gtk_Column_View_Record) return Boolean;
   --  Returns whether the list should show separators between rows.
   --  @return true if the list shows separators

   procedure Set_Show_Row_Separators
      (Self                : not null access Gtk_Column_View_Record;
       Show_Row_Separators : Boolean);
   --  Sets whether the list should show separators between rows.
   --  @param Show_Row_Separators whether to show row separators

   function Get_Single_Click_Activate
      (Self : not null access Gtk_Column_View_Record) return Boolean;
   --  Returns whether rows will be activated on single click and selected on
   --  hover.
   --  @return true if rows are activated on single click

   procedure Set_Single_Click_Activate
      (Self                  : not null access Gtk_Column_View_Record;
       Single_Click_Activate : Boolean);
   --  Sets whether rows should be activated on single click and selected on
   --  hover.
   --  @param Single_Click_Activate whether to activate items on single click

   function Get_Sorter
      (Self : not null access Gtk_Column_View_Record)
       return Gtk.Sorter.Gtk_Sorter;
   --  Returns a special sorter that reflects the users sorting choices in the
   --  column view.
   --  To allow users to customizable sorting by clicking on column headers,
   --  this sorter needs to be set on the sort model underneath the model that
   --  is displayed by the view.
   --  See [methodGtk.ColumnViewColumn.set_sorter] for setting up per-column
   --  sorting.
   --  Here is an example: ```c gtk_column_view_column_set_sorter (column,
   --  sorter); gtk_column_view_append_column (view, column); sorter =
   --  g_object_ref (gtk_column_view_get_sorter (view))); model =
   --  gtk_sort_list_model_new (store, sorter); selection =
   --  gtk_no_selection_new (model); gtk_column_view_set_model (view,
   --  selection); ```
   --  @return the `GtkSorter` of Self

   function Get_Tab_Behavior
      (Self : not null access Gtk_Column_View_Record)
       return Gtk.Enums.Gtk_List_Tab_Behavior;
   --  Gets the behavior set for the <kbd>Tab</kbd> key.
   --  Since: gtk+ 4.12
   --  @return The behavior of the <kbd>Tab</kbd> key

   procedure Set_Tab_Behavior
      (Self         : not null access Gtk_Column_View_Record;
       Tab_Behavior : Gtk.Enums.Gtk_List_Tab_Behavior);
   --  Sets the <kbd>Tab</kbd> key behavior.
   --  This influences how the <kbd>Tab</kbd> and
   --  <kbd>Shift</kbd>+<kbd>Tab</kbd> keys move the focus in the columnview.
   --  Since: gtk+ 4.12
   --  @param Tab_Behavior The desired tab behavior

   procedure Insert_Column
      (Self     : not null access Gtk_Column_View_Record;
       Position : Guint;
       Column   : not null access Gtk.Column_View_Column.Gtk_Column_View_Column_Record'Class);
   --  Inserts a column at the given position in the columns of Self.
   --  If Column is already a column of Self, it will be repositioned.
   --  @param Position the position to insert Column at
   --  @param Column the column to insert

   procedure Remove_Column
      (Self   : not null access Gtk_Column_View_Record;
       Column : not null access Gtk.Column_View_Column.Gtk_Column_View_Column_Record'Class);
   --  Removes the Column from the list of columns of Self.
   --  @param Column a column that's part of Self

   procedure Scroll_To
      (Self   : not null access Gtk_Column_View_Record;
       Pos    : Guint;
       Column : access Gtk.Column_View_Column.Gtk_Column_View_Column_Record'Class;
       Flags  : Gtk.Enums.Gtk_List_Scroll_Flags;
       Scroll : Gtk.Scroll_Info.Gtk_Scroll_Info);
   --  Scroll to the row at the given position - or cell if a column is given
   --  - and performs the actions specified in Flags.
   --  This function works no matter if the columnview is shown or focused. If
   --  it isn't, then the changes will take effect once that happens.
   --  Since: gtk+ 4.12
   --  @param Pos position of the item. Must be less than the number of items
   --  in the view.
   --  @param Column The column to scroll to or `NULL` to not scroll columns
   --  @param Flags actions to perform
   --  @param Scroll details of how to perform the scroll operation or null to
   --  scroll into view

   procedure Sort_By_Column
      (Self      : not null access Gtk_Column_View_Record;
       Column    : access Gtk.Column_View_Column.Gtk_Column_View_Column_Record'Class;
       Direction : Gtk.Enums.Gtk_Sort_Type);
   --  Sets the sorting of the view.
   --  This function should be used to set up the initial sorting. At runtime,
   --  users can change the sorting of a column view by clicking on the list
   --  headers.
   --  This call only has an effect if the sorter returned by
   --  [methodGtk.ColumnView.get_sorter] is set on a sort model, and
   --  [methodGtk.ColumnViewColumn.set_sorter] has been called on Column to
   --  associate a sorter with the column.
   --  If Column is unset, the view will be unsorted.
   --  @param Column the column to sort by
   --  @param Direction the direction to sort in

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Column_View_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Column_View_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Column_View_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Column_View_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Column_View_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Column_View_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Column_View_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Column_View_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Column_View_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Column_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Column_View_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Column_View_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Column_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Column_View_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Column_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Columns_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.List_Model
   --  The list of columns.

   Enable_Rubberband_Property : constant Glib.Properties.Property_Boolean;
   --  Allow rubberband selection.

   Header_Factory_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.List_Item_Factory.Gtk_List_Item_Factory
   --  Factory for creating header widgets.
   --
   --  The factory must be for configuring [classGtk.ListHeader] objects.

   Model_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Selection_Model.Gtk_Selection_Model
   --  Model for the items displayed.

   Reorderable_Property : constant Glib.Properties.Property_Boolean;
   --  Whether columns are reorderable.

   Row_Factory_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.List_Item_Factory.Gtk_List_Item_Factory
   --  The factory used for configuring rows.
   --
   --  The factory must be for configuring [classGtk.ColumnViewRow] objects.

   Show_Column_Separators_Property : constant Glib.Properties.Property_Boolean;
   --  Show separators between columns.

   Show_Row_Separators_Property : constant Glib.Properties.Property_Boolean;
   --  Show separators between rows.

   Single_Click_Activate_Property : constant Glib.Properties.Property_Boolean;
   --  Activate rows on single click and select them on hover.

   Sorter_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Sorter.Gtk_Sorter
   --  Sorter with the sorting choices of the user.

   Tab_Behavior_Property : constant Gtk.Enums.Property_Gtk_List_Tab_Behavior;
   --  Behavior of the <kbd>Tab</kbd> key

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Column_View_Guint_Void is not null access procedure
     (Self     : access Gtk_Column_View_Record'Class;
      Position : Guint);

   type Cb_GObject_Guint_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Position : Guint);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gtk_Column_View_Record;
       Call  : Cb_Gtk_Column_View_Guint_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gtk_Column_View_Record;
       Call  : Cb_GObject_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a row has been activated by the user, usually via
   --  activating the GtkListBase|list.activate-item action.
   --
   --  This allows for a convenient way to handle activation in a columnview.
   --  See [methodGtk.ListItem.set_activatable] for details on how to use this
   --  signal.

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

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Column_View_Record, Gtk_Column_View);
   function "+"
     (Widget : access Gtk_Column_View_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Column_View
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Column_View_Record, Gtk_Column_View);
   function "+"
     (Widget : access Gtk_Column_View_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Column_View
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Column_View_Record, Gtk_Column_View);
   function "+"
     (Widget : access Gtk_Column_View_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Column_View
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Tab_Behavior_Property : constant Gtk.Enums.Property_Gtk_List_Tab_Behavior :=
     Gtk.Enums.Build ("tab-behavior");
   Sorter_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("sorter");
   Single_Click_Activate_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("single-click-activate");
   Show_Row_Separators_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-row-separators");
   Show_Column_Separators_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-column-separators");
   Row_Factory_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("row-factory");
   Reorderable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("reorderable");
   Model_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("model");
   Header_Factory_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("header-factory");
   Enable_Rubberband_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enable-rubberband");
   Columns_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("columns");
end Gtk.Column_View;
