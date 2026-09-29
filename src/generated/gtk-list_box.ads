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

--  Shows a vertical list.
--
--  <picture> <source srcset="list-box-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkListBox" src="list-box.png"> </picture>
--  A `GtkListBox` only contains `GtkListBoxRow` children. These rows can by
--  dynamically sorted and filtered, and headers can be added dynamically
--  depending on the row content. It also allows keyboard and mouse navigation
--  and selection like a typical list.
--
--  Using `GtkListBox` is often an alternative to `GtkTreeView`, especially
--  when the list contents has a more complicated layout than what is allowed
--  by a `GtkCellRenderer`, or when the contents is interactive (i.e. has a
--  button in it).
--
--  Although a `GtkListBox` must have only `GtkListBoxRow` children, you can
--  add any kind of widget to it via [methodGtk.ListBox.prepend],
--  [methodGtk.ListBox.append] and [methodGtk.ListBox.insert] and a
--  `GtkListBoxRow` widget will automatically be inserted between the list and
--  the widget.
--
--  `GtkListBoxRows` can be marked as activatable or selectable. If a row is
--  activatable, [signalGtk.ListBox::row-activated] will be emitted for it when
--  the user tries to activate it. If it is selectable, the row will be marked
--  as selected when the user tries to select it.
--
--  # GtkListBox as GtkBuildable
--
--  The `GtkListBox` implementation of the `GtkBuildable` interface supports
--  setting a child as the placeholder by specifying "placeholder" as the
--  "type" attribute of a `<child>` element. See
--  [methodGtk.ListBox.set_placeholder] for info.
--
--  # Shortcuts and Gestures
--
--  The following signals have default keybindings:
--
--  - [signalGtk.ListBox::move-cursor] - [signalGtk.ListBox::select-all] -
--  [signalGtk.ListBox::toggle-cursor-row] - [signalGtk.ListBox::unselect-all]
--
--  # CSS nodes
--
--  ``` list[.separators][.rich-list][.navigation-sidebar][.boxed-list] ╰──
--  row[.activatable] ```
--
--  `GtkListBox` uses a single CSS node named list. It may carry the
--  .separators style class, when the [propertyGtk.ListBox:show-separators]
--  property is set. Each `GtkListBoxRow` uses a single CSS node named row. The
--  row nodes get the .activatable style class added when appropriate.
--
--  It may also carry the .boxed-list style class. In this case, the list will
--  be automatically surrounded by a frame and have separators.
--
--  The main list node may also carry style classes to select the style of
--  [list presentation](section-list-widget.htmllist-styles): .rich-list,
--  .navigation-sidebar or .data-table.
--
--  # Accessibility
--
--  `GtkListBox` uses the [enumGtk.AccessibleRole.list] role and
--  `GtkListBoxRow` uses the [enumGtk.AccessibleRole.list_item] role.
--
--  <group>Trees and Lists</group>
--  <gtkada_demo>create_list_box_complex.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.List_Model;       use Glib.List_Model;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Adjustment;        use Gtk.Adjustment;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.List_Box_Row;      use Gtk.List_Box_Row;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.List_Box is

   type Gtk_List_Box_Record is new Gtk_Widget_Record with null record;
   type Gtk_List_Box is access all Gtk_List_Box_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_List_Box_Create_Widget_Func is access function (Item : System.Address) return Gtk.Widget.Gtk_Widget;
   --  Called for list boxes that are bound to a `GListModel` with
   --  Gtk.List_Box.Bind_Model for each item that gets added to the model.
   --  If the widget returned is not a Gtk.List_Box_Row.Gtk_List_Box_Row
   --  widget, then the widget will be inserted as the child of an intermediate
   --  Gtk.List_Box_Row.Gtk_List_Box_Row.
   --  @param Item the item from the model for which to create a widget for
   --  @return a `GtkWidget` that represents Item. Has
   --  transfer-ownership='full'.

   type Gtk_List_Box_Foreach_Func is access procedure
     (Box : not null access Gtk_List_Box_Record'Class;
      Row : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class);
   --  A function used by Gtk.List_Box.Selected_Foreach.
   --  It will be called on every selected child of the Box.
   --  @param Box a `GtkListBox`
   --  @param Row a `GtkListBoxRow`

   type Gtk_List_Box_Filter_Func is access function
     (Row : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class)
   return Boolean;
   --  Will be called whenever the row changes or is added and lets you
   --  control if the row should be visible or not.
   --  @param Row the row that may be filtered
   --  @return True if the row should be visible, False otherwise

   type Gtk_List_Box_Update_Header_Func is access procedure
     (Row    : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class;
      Before : access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class);
   --  Whenever Row changes or which row is before Row changes this is called,
   --  which lets you update the header on Row.
   --  You may remove or set a new one via [methodGtk.ListBoxRow.set_header]
   --  or just change the state of the current header widget.
   --  @param Row the row to update
   --  @param Before the row before Row, or null if it is first

   type Gtk_List_Box_Sort_Func is access function
     (Row1 : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class;
      Row2 : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class)
   return Glib.Gint;
   --  Compare two rows to determine which should be first.
   --  @param Row1 the first row
   --  @param Row2 the second row
   --  @return < 0 if Row1 should be before Row2, 0 if they are equal and > 0
   --  otherwise

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_List_Box);
   procedure Initialize (Self : not null access Gtk_List_Box_Record'Class);
   --  Creates a new `GtkListBox` container.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_List_Box_New return Gtk_List_Box;
   --  Creates a new `GtkListBox` container.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_list_box_get_type");

   -------------
   -- Methods --
   -------------

   procedure Append
      (Self  : not null access Gtk_List_Box_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Append a widget to the list.
   --  If a sort function is set, the widget will actually be inserted at the
   --  calculated position.
   --  @param Child the `GtkWidget` to add

   procedure Bind_Model
      (Self               : not null access Gtk_List_Box_Record;
       Model              : Glib.List_Model.Glist_Model;
       Create_Widget_Func : Gtk_List_Box_Create_Widget_Func);
   --  Binds Model to Box.
   --  If Box was already bound to a model, that previous binding is
   --  destroyed.
   --  The contents of Box are cleared and then filled with widgets that
   --  represent items from Model. Box is updated whenever Model changes. If
   --  Model is null, Box is left empty.
   --  It is undefined to add or remove widgets directly (for example, with
   --  [methodGtk.ListBox.insert]) while Box is bound to a model.
   --  Note that using a model is incompatible with the filtering and sorting
   --  functionality in `GtkListBox`. When using a model, filtering and sorting
   --  should be implemented by the model.
   --  @param Model the `GListModel` to be bound to Box
   --  @param Create_Widget_Func a function that creates widgets for items or
   --  null in case you also passed null as Model

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Bind_Model_User_Data is

      type Gtk_List_Box_Create_Widget_Func is access function
        (Item      : System.Address;
         User_Data : User_Data_Type) return Gtk.Widget.Gtk_Widget;
      --  Called for list boxes that are bound to a `GListModel` with
      --  Gtk.List_Box.Bind_Model for each item that gets added to the model.
      --  If the widget returned is not a Gtk.List_Box_Row.Gtk_List_Box_Row
      --  widget, then the widget will be inserted as the child of an intermediate
      --  Gtk.List_Box_Row.Gtk_List_Box_Row.
      --  @param Item the item from the model for which to create a widget for
      --  @param User_Data user data
      --  @return a `GtkWidget` that represents Item. Has
      --  transfer-ownership='full'.

      procedure Bind_Model
         (Self               : not null access Gtk.List_Box.Gtk_List_Box_Record'Class;
          Model              : Glib.List_Model.Glist_Model;
          Create_Widget_Func : Gtk_List_Box_Create_Widget_Func;
          User_Data          : User_Data_Type);
      --  Binds Model to Box.
      --  If Box was already bound to a model, that previous binding is
      --  destroyed.
      --  The contents of Box are cleared and then filled with widgets that
      --  represent items from Model. Box is updated whenever Model changes. If
      --  Model is null, Box is left empty.
      --  It is undefined to add or remove widgets directly (for example, with
      --  [methodGtk.ListBox.insert]) while Box is bound to a model.
      --  Note that using a model is incompatible with the filtering and
      --  sorting functionality in `GtkListBox`. When using a model, filtering
      --  and sorting should be implemented by the model.
      --  @param Model the `GListModel` to be bound to Box
      --  @param Create_Widget_Func a function that creates widgets for items
      --  or null in case you also passed null as Model
      --  @param User_Data user data passed to Create_Widget_Func

   end Bind_Model_User_Data;

   procedure Drag_Highlight_Row
      (Self : not null access Gtk_List_Box_Record;
       Row  : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class);
   --  Add a drag highlight to a row.
   --  This is a helper function for implementing DnD onto a `GtkListBox`. The
   --  passed in Row will be highlighted by setting the
   --  Gtk.Enums.Gtk_State_Flag_Drop_Active state and any previously
   --  highlighted row will be unhighlighted.
   --  The row will also be unhighlighted when the widget gets a drag leave
   --  event.
   --  @param Row a `GtkListBoxRow`

   procedure Drag_Unhighlight_Row
      (Self : not null access Gtk_List_Box_Record);
   --  If a row has previously been highlighted via
   --  Gtk.List_Box.Drag_Highlight_Row, it will have the highlight removed.

   function Get_Activate_On_Single_Click
      (Self : not null access Gtk_List_Box_Record) return Boolean;
   --  Returns whether rows activate on single clicks.
   --  @return True if rows are activated on single click, False otherwise

   procedure Set_Activate_On_Single_Click
      (Self   : not null access Gtk_List_Box_Record;
       Single : Boolean);
   --  If Single is True, rows will be activated when you click on them,
   --  otherwise you need to double-click.
   --  @param Single a boolean

   function Get_Adjustment
      (Self : not null access Gtk_List_Box_Record)
       return Gtk.Adjustment.Gtk_Adjustment;
   --  Gets the adjustment (if any) that the widget uses to for vertical
   --  scrolling.
   --  @return the adjustment. Has transfer-ownership='none'.

   procedure Set_Adjustment
      (Self       : not null access Gtk_List_Box_Record;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Sets the adjustment (if any) that the widget uses to for vertical
   --  scrolling.
   --  For instance, this is used to get the page size for PageUp/Down key
   --  handling.
   --  In the normal case when the Box is packed inside a `GtkScrolledWindow`
   --  the adjustment from that will be picked up automatically, so there is no
   --  need to manually do that.
   --  @param Adjustment the adjustment

   function Get_Row_At_Index
      (Self  : not null access Gtk_List_Box_Record;
       Index : Glib.Gint) return Gtk.List_Box_Row.Gtk_List_Box_Row;
   --  Gets the n-th child in the list (not counting headers).
   --  If Index_ is negative or larger than the number of items in the list,
   --  null is returned.
   --  @param Index the index of the row
   --  @return the child `GtkWidget`. Has transfer-ownership='none'.

   function Get_Row_At_Y
      (Self : not null access Gtk_List_Box_Record;
       Y    : Glib.Gint) return Gtk.List_Box_Row.Gtk_List_Box_Row;
   --  Gets the row at the Y position.
   --  @param Y position
   --  @return the row. Has transfer-ownership='none'.

   function Get_Selected_Row
      (Self : not null access Gtk_List_Box_Record)
       return Gtk.List_Box_Row.Gtk_List_Box_Row;
   --  Gets the selected row, or null if no rows are selected.
   --  Note that the box may allow multiple selection, in which case you
   --  should use [methodGtk.ListBox.selected_foreach] to find all selected
   --  rows.
   --  @return the selected row. Has transfer-ownership='none'.

   function Get_Selected_Rows
      (Self : not null access Gtk_List_Box_Record)
       return Gtk.List_Box_Row.List_Box_Row_List.Glist;
   --  Creates a list of all selected children.
   --  @return A `GList` containing the `GtkWidget` for each selected child.
   --  Free with g_list_free when done.

   function Get_Selection_Mode
      (Self : not null access Gtk_List_Box_Record)
       return Gtk.Enums.Gtk_Selection_Mode;
   --  Gets the selection mode of the listbox.
   --  @return a `GtkSelectionMode`

   procedure Set_Selection_Mode
      (Self : not null access Gtk_List_Box_Record;
       Mode : Gtk.Enums.Gtk_Selection_Mode);
   --  Sets how selection works in the listbox.
   --  @param Mode The `GtkSelectionMode`

   function Get_Show_Separators
      (Self : not null access Gtk_List_Box_Record) return Boolean;
   --  Returns whether the list box should show separators between rows.
   --  @return True if the list box shows separators

   procedure Set_Show_Separators
      (Self            : not null access Gtk_List_Box_Record;
       Show_Separators : Boolean);
   --  Sets whether the list box should show separators between rows.
   --  @param Show_Separators True to show separators

   function Get_Tab_Behavior
      (Self : not null access Gtk_List_Box_Record)
       return Gtk.Enums.Gtk_List_Tab_Behavior;
   --  Returns the behavior of the <kbd>Tab</kbd> and
   --  <kbd>Shift</kbd>+<kbd>Tab</kbd> keys.
   --  Since: gtk+ 4.18
   --  @return the tab behavior

   procedure Set_Tab_Behavior
      (Self     : not null access Gtk_List_Box_Record;
       Behavior : Gtk.Enums.Gtk_List_Tab_Behavior);
   --  Sets the behavior of the <kbd>Tab</kbd> and
   --  <kbd>Shift</kbd>+<kbd>Tab</kbd> keys.
   --  Since: gtk+ 4.18
   --  @param Behavior the tab behavior

   procedure Insert
      (Self     : not null access Gtk_List_Box_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position : Glib.Gint);
   --  Insert the Child into the Box at Position.
   --  If a sort function is set, the widget will actually be inserted at the
   --  calculated position.
   --  If Position is -1, or larger than the total number of items in the Box,
   --  then the Child will be appended to the end.
   --  @param Child the `GtkWidget` to add
   --  @param Position the position to insert Child in

   procedure Invalidate_Filter (Self : not null access Gtk_List_Box_Record);
   --  Update the filtering for all rows.
   --  Call this when result of the filter function on the Box is changed due
   --  to an external factor. For instance, this would be used if the filter
   --  function just looked for a specific search string and the entry with the
   --  search string has changed.

   procedure Invalidate_Headers (Self : not null access Gtk_List_Box_Record);
   --  Update the separators for all rows.
   --  Call this when result of the header function on the Box is changed due
   --  to an external factor.

   procedure Invalidate_Sort (Self : not null access Gtk_List_Box_Record);
   --  Update the sorting for all rows.
   --  Call this when result of the sort function on the Box is changed due to
   --  an external factor.

   procedure Prepend
      (Self  : not null access Gtk_List_Box_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Prepend a widget to the list.
   --  If a sort function is set, the widget will actually be inserted at the
   --  calculated position.
   --  @param Child the `GtkWidget` to add

   procedure Remove
      (Self  : not null access Gtk_List_Box_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes a child from Box.
   --  @param Child the child to remove

   procedure Remove_All (Self : not null access Gtk_List_Box_Record);
   --  Removes all rows from Box.
   --  This function does nothing if Box is backed by a model.
   --  Since: gtk+ 4.12

   procedure Select_All (Self : not null access Gtk_List_Box_Record);
   --  Select all children of Box, if the selection mode allows it.

   procedure Select_Row
      (Self : not null access Gtk_List_Box_Record;
       Row  : access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class);
   --  Make Row the currently selected row.
   --  @param Row The row to select

   procedure Selected_Foreach
      (Self : not null access Gtk_List_Box_Record;
       Func : Gtk_List_Box_Foreach_Func);
   --  Calls a function for each selected child.
   --  Note that the selection cannot be modified from within this function.
   --  @param Func the function to call for each selected child

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Selected_Foreach_User_Data is

      type Gtk_List_Box_Foreach_Func is access procedure
        (Box       : not null access Gtk.List_Box.Gtk_List_Box_Record'Class;
         Row       : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class;
         User_Data : User_Data_Type);
      --  A function used by Gtk.List_Box.Selected_Foreach.
      --  It will be called on every selected child of the Box.
      --  @param Box a `GtkListBox`
      --  @param Row a `GtkListBoxRow`
      --  @param User_Data user data

      procedure Selected_Foreach
         (Self : not null access Gtk.List_Box.Gtk_List_Box_Record'Class;
          Func : Gtk_List_Box_Foreach_Func;
          Data : User_Data_Type);
      --  Calls a function for each selected child.
      --  Note that the selection cannot be modified from within this
      --  function.
      --  @param Func the function to call for each selected child
      --  @param Data user data to pass to the function

   end Selected_Foreach_User_Data;

   procedure Set_Filter_Func
      (Self        : not null access Gtk_List_Box_Record;
       Filter_Func : Gtk_List_Box_Filter_Func);
   --  By setting a filter function on the Box one can decide dynamically
   --  which of the rows to show.
   --  For instance, to implement a search function on a list that filters the
   --  original list to only show the matching rows.
   --  The Filter_Func will be called for each row after the call, and it will
   --  continue to be called each time a row changes (via
   --  [methodGtk.ListBoxRow.changed]) or when
   --  [methodGtk.ListBox.invalidate_filter] is called.
   --  Note that using a filter function is incompatible with using a model
   --  (see [methodGtk.ListBox.bind_model]).
   --  @param Filter_Func callback that lets you filter which rows to show

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Filter_Func_User_Data is

      type Gtk_List_Box_Filter_Func is access function
        (Row       : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class;
         User_Data : User_Data_Type) return Boolean;
      --  Will be called whenever the row changes or is added and lets you
      --  control if the row should be visible or not.
      --  @param Row the row that may be filtered
      --  @param User_Data user data
      --  @return True if the row should be visible, False otherwise

      procedure Set_Filter_Func
         (Self        : not null access Gtk.List_Box.Gtk_List_Box_Record'Class;
          Filter_Func : Gtk_List_Box_Filter_Func;
          User_Data   : User_Data_Type);
      --  By setting a filter function on the Box one can decide dynamically
      --  which of the rows to show.
      --  For instance, to implement a search function on a list that filters
      --  the original list to only show the matching rows.
      --  The Filter_Func will be called for each row after the call, and it
      --  will continue to be called each time a row changes (via
      --  [methodGtk.ListBoxRow.changed]) or when
      --  [methodGtk.ListBox.invalidate_filter] is called.
      --  Note that using a filter function is incompatible with using a model
      --  (see [methodGtk.ListBox.bind_model]).
      --  @param Filter_Func callback that lets you filter which rows to show
      --  @param User_Data user data passed to Filter_Func

   end Set_Filter_Func_User_Data;

   procedure Set_Header_Func
      (Self          : not null access Gtk_List_Box_Record;
       Update_Header : Gtk_List_Box_Update_Header_Func);
   --  Sets a header function.
   --  By setting a header function on the Box one can dynamically add headers
   --  in front of rows, depending on the contents of the row and its position
   --  in the list.
   --  For instance, one could use it to add headers in front of the first
   --  item of a new kind, in a list sorted by the kind.
   --  The Update_Header can look at the current header widget using
   --  [methodGtk.ListBoxRow.get_header] and either update the state of the
   --  widget as needed, or set a new one using
   --  [methodGtk.ListBoxRow.set_header]. If no header is needed, set the
   --  header to null.
   --  Note that you may get many calls Update_Header to this for a particular
   --  row when e.g. changing things that don't affect the header. In this case
   --  it is important for performance to not blindly replace an existing
   --  header with an identical one.
   --  The Update_Header function will be called for each row after the call,
   --  and it will continue to be called each time a row changes (via
   --  [methodGtk.ListBoxRow.changed]) and when the row before changes (either
   --  by [methodGtk.ListBoxRow.changed] on the previous row, or when the
   --  previous row becomes a different row). It is also called for all rows
   --  when [methodGtk.ListBox.invalidate_headers] is called.
   --  @param Update_Header callback that lets you add row headers

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Header_Func_User_Data is

      type Gtk_List_Box_Update_Header_Func is access procedure
        (Row       : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class;
         Before    : access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class;
         User_Data : User_Data_Type);
      --  Whenever Row changes or which row is before Row changes this is called,
      --  which lets you update the header on Row.
      --  You may remove or set a new one via [methodGtk.ListBoxRow.set_header]
      --  or just change the state of the current header widget.
      --  @param Row the row to update
      --  @param Before the row before Row, or null if it is first
      --  @param User_Data user data

      procedure Set_Header_Func
         (Self          : not null access Gtk.List_Box.Gtk_List_Box_Record'Class;
          Update_Header : Gtk_List_Box_Update_Header_Func;
          User_Data     : User_Data_Type);
      --  Sets a header function.
      --  By setting a header function on the Box one can dynamically add
      --  headers in front of rows, depending on the contents of the row and
      --  its position in the list.
      --  For instance, one could use it to add headers in front of the first
      --  item of a new kind, in a list sorted by the kind.
      --  The Update_Header can look at the current header widget using
      --  [methodGtk.ListBoxRow.get_header] and either update the state of the
      --  widget as needed, or set a new one using
      --  [methodGtk.ListBoxRow.set_header]. If no header is needed, set the
      --  header to null.
      --  Note that you may get many calls Update_Header to this for a
      --  particular row when e.g. changing things that don't affect the
      --  header. In this case it is important for performance to not blindly
      --  replace an existing header with an identical one.
      --  The Update_Header function will be called for each row after the
      --  call, and it will continue to be called each time a row changes (via
      --  [methodGtk.ListBoxRow.changed]) and when the row before changes
      --  (either by [methodGtk.ListBoxRow.changed] on the previous row, or
      --  when the previous row becomes a different row). It is also called for
      --  all rows when [methodGtk.ListBox.invalidate_headers] is called.
      --  @param Update_Header callback that lets you add row headers
      --  @param User_Data user data passed to Update_Header

   end Set_Header_Func_User_Data;

   procedure Set_Placeholder
      (Self        : not null access Gtk_List_Box_Record;
       Placeholder : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the placeholder widget that is shown in the list when it doesn't
   --  display any visible children.
   --  @param Placeholder a `GtkWidget`

   procedure Set_Sort_Func
      (Self      : not null access Gtk_List_Box_Record;
       Sort_Func : Gtk_List_Box_Sort_Func);
   --  Sets a sort function.
   --  By setting a sort function on the Box one can dynamically reorder the
   --  rows of the list, based on the contents of the rows.
   --  The Sort_Func will be called for each row after the call, and will
   --  continue to be called each time a row changes (via
   --  [methodGtk.ListBoxRow.changed]) and when
   --  [methodGtk.ListBox.invalidate_sort] is called.
   --  Note that using a sort function is incompatible with using a model (see
   --  [methodGtk.ListBox.bind_model]).
   --  @param Sort_Func the sort function

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Sort_Func_User_Data is

      type Gtk_List_Box_Sort_Func is access function
        (Row1      : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class;
         Row2      : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class;
         User_Data : User_Data_Type) return Glib.Gint;
      --  Compare two rows to determine which should be first.
      --  @param Row1 the first row
      --  @param Row2 the second row
      --  @param User_Data user data
      --  @return < 0 if Row1 should be before Row2, 0 if they are equal and > 0
      --  otherwise

      procedure Set_Sort_Func
         (Self      : not null access Gtk.List_Box.Gtk_List_Box_Record'Class;
          Sort_Func : Gtk_List_Box_Sort_Func;
          User_Data : User_Data_Type);
      --  Sets a sort function.
      --  By setting a sort function on the Box one can dynamically reorder
      --  the rows of the list, based on the contents of the rows.
      --  The Sort_Func will be called for each row after the call, and will
      --  continue to be called each time a row changes (via
      --  [methodGtk.ListBoxRow.changed]) and when
      --  [methodGtk.ListBox.invalidate_sort] is called.
      --  Note that using a sort function is incompatible with using a model
      --  (see [methodGtk.ListBox.bind_model]).
      --  @param Sort_Func the sort function
      --  @param User_Data user data passed to Sort_Func

   end Set_Sort_Func_User_Data;

   procedure Unselect_All (Self : not null access Gtk_List_Box_Record);
   --  Unselect all children of Box, if the selection mode allows it.

   procedure Unselect_Row
      (Self : not null access Gtk_List_Box_Record;
       Row  : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class);
   --  Unselects a single row of Box, if the selection mode allows it.
   --  @param Row the row to unselect

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_List_Box_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_List_Box_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_List_Box_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_List_Box_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_List_Box_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_List_Box_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_List_Box_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_List_Box_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_List_Box_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_List_Box_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_List_Box_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_List_Box_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_List_Box_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_List_Box_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_List_Box_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Accept_Unpaired_Release_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to accept unpaired release events.

   Activate_On_Single_Click_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether children can be activated with a single click, or
   --  require a double-click.

   Selection_Mode_Property : constant Gtk.Enums.Property_Gtk_Selection_Mode;
   --  The selection mode used by the list box.

   Show_Separators_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to show separators between rows.

   Tab_Behavior_Property : constant Gtk.Enums.Property_Gtk_List_Tab_Behavior;
   --  Behavior of the <kbd>Tab</kbd> key

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_List_Box_Void is not null access procedure (Self : access Gtk_List_Box_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate_Cursor_Row : constant Glib.Signal_Name := "activate-cursor-row";
   procedure On_Activate_Cursor_Row
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_Gtk_List_Box_Void;
       After : Boolean := False);
   procedure On_Activate_Cursor_Row
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the cursor row is activated.

   type Cb_Gtk_List_Box_Gtk_Movement_Step_Gint_Boolean_Boolean_Void is not null access procedure
     (Self   : access Gtk_List_Box_Record'Class;
      Step   : Gtk.Enums.Gtk_Movement_Step;
      Count  : Glib.Gint;
      Extend : Boolean;
      Modify : Boolean);

   type Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Boolean_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Step   : Gtk.Enums.Gtk_Movement_Step;
      Count  : Glib.Gint;
      Extend : Boolean;
      Modify : Boolean);

   Signal_Move_Cursor : constant Glib.Signal_Name := "move-cursor";
   procedure On_Move_Cursor
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_Gtk_List_Box_Gtk_Movement_Step_Gint_Boolean_Boolean_Void;
       After : Boolean := False);
   procedure On_Move_Cursor
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the user initiates a cursor movement.
   --
   --  The default bindings for this signal come in two variants, the variant
   --  with the Shift modifier extends the selection, the variant without the
   --  Shift modifier does not. There are too many key combinations to list
   --  them all here.
   --
   --  - <kbd>←</kbd>, <kbd>→</kbd>, <kbd>↑</kbd>, <kbd>↓</kbd> move by
   --  individual children - <kbd>Home</kbd>, <kbd>End</kbd> move to the ends
   --  of the box - <kbd>PgUp</kbd>, <kbd>PgDn</kbd> move vertically by pages
   -- 
   --  Callback parameters:
   --    --  @param Step the granularity of the move, as a `GtkMovementStep`
   --    --  @param Count the number of Step units to move
   --    --  @param Extend whether to extend the selection
   --    --  @param Modify whether to modify the selection

   type Cb_Gtk_List_Box_Gtk_List_Box_Row_Void is not null access procedure
     (Self : access Gtk_List_Box_Record'Class;
      Row  : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class);

   type Cb_GObject_Gtk_List_Box_Row_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Row  : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class);

   Signal_Row_Activated : constant Glib.Signal_Name := "row-activated";
   procedure On_Row_Activated
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_Gtk_List_Box_Gtk_List_Box_Row_Void;
       After : Boolean := False);
   procedure On_Row_Activated
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_GObject_Gtk_List_Box_Row_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a row has been activated by the user.

   type Cb_Gtk_List_Box_Gtk_List_Box_Row_Or_Null_Void is not null access procedure
     (Self : access Gtk_List_Box_Record'Class;
      Row  : access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class);

   type Cb_GObject_Gtk_List_Box_Row_Or_Null_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Row  : access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class);

   Signal_Row_Selected : constant Glib.Signal_Name := "row-selected";
   procedure On_Row_Selected
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_Gtk_List_Box_Gtk_List_Box_Row_Or_Null_Void;
       After : Boolean := False);
   procedure On_Row_Selected
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_GObject_Gtk_List_Box_Row_Or_Null_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a new row is selected, or (with a null Row) when the
   --  selection is cleared.
   --
   --  When the Box is using Gtk.Enums.Selection_Multiple, this signal will
   --  not give you the full picture of selection changes, and you should use
   --  the [signalGtk.ListBox::selected-rows-changed] signal instead.

   Signal_Select_All : constant Glib.Signal_Name := "select-all";
   procedure On_Select_All
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_Gtk_List_Box_Void;
       After : Boolean := False);
   procedure On_Select_All
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to select all children of the box, if the selection mode
   --  permits it.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  The default binding for this signal is <kbd>Ctrl</kbd>-<kbd>a</kbd>.

   Signal_Selected_Rows_Changed : constant Glib.Signal_Name := "selected-rows-changed";
   procedure On_Selected_Rows_Changed
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_Gtk_List_Box_Void;
       After : Boolean := False);
   procedure On_Selected_Rows_Changed
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the set of selected rows changes.

   Signal_Toggle_Cursor_Row : constant Glib.Signal_Name := "toggle-cursor-row";
   procedure On_Toggle_Cursor_Row
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_Gtk_List_Box_Void;
       After : Boolean := False);
   procedure On_Toggle_Cursor_Row
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the cursor row is toggled.
   --
   --  The default bindings for this signal is <kbd>Ctrl</kbd>+<kbd>␣</kbd>.

   Signal_Unselect_All : constant Glib.Signal_Name := "unselect-all";
   procedure On_Unselect_All
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_Gtk_List_Box_Void;
       After : Boolean := False);
   procedure On_Unselect_All
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to unselect all children of the box, if the selection mode
   --  permits it.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  The default binding for this signal is
   --  <kbd>Ctrl</kbd>-<kbd>Shift</kbd>-<kbd>a</kbd>.

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
     (Gtk.Accessible.Gtk_Accessible, Gtk_List_Box_Record, Gtk_List_Box);
   function "+"
     (Widget : access Gtk_List_Box_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_List_Box
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_List_Box_Record, Gtk_List_Box);
   function "+"
     (Widget : access Gtk_List_Box_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_List_Box
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_List_Box_Record, Gtk_List_Box);
   function "+"
     (Widget : access Gtk_List_Box_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_List_Box
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Tab_Behavior_Property : constant Gtk.Enums.Property_Gtk_List_Tab_Behavior :=
     Gtk.Enums.Build ("tab-behavior");
   Show_Separators_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-separators");
   Selection_Mode_Property : constant Gtk.Enums.Property_Gtk_Selection_Mode :=
     Gtk.Enums.Build ("selection-mode");
   Activate_On_Single_Click_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("activate-on-single-click");
   Accept_Unpaired_Release_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("accept-unpaired-release");
end Gtk.List_Box;
