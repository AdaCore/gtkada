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
--  A GtkListBox is a vertical container that contains GtkListBoxRow children.
--  These rows can by dynamically sorted and filtered, and headers can be added
--  dynamically depending on the row content. It also allows keyboard and mouse
--  navigation and selection like a typical list.
--
--  Using GtkListBox is often an alternative to Gtk.Tree_View.Gtk_Tree_View,
--  especially when the list contents has a more complicated layout than what
--  is allowed by a Gtk.Cell_Renderer.Gtk_Cell_Renderer, or when the contents
--  is interactive (i.e. has a button in it).
--
--  Although a Gtk.List_Box.Gtk_List_Box must have only
--  Gtk.List_Box_Row.Gtk_List_Box_Row children you can add any kind of widget
--  to it via Gtk.Container.Add, and a Gtk.List_Box_Row.Gtk_List_Box_Row widget
--  will automatically be inserted between the list and the widget.
--
--  Gtk_List_Box_Rows can be marked as activatable or selectable. If a row is
--  activatable, Gtk.List_Box.Gtk_List_Box::row-activated will be emitted for
--  it when the user tries to activate it. If it is selectable, the row will be
--  marked as selected when the user tries to select it.
--
--  The GtkListBox widget was added in GTK+ 3.10.
--
--  # GtkListBox as GtkBuildable
--
--  The GtkListBox implementation of the Gtk.Buildable.Gtk_Buildable interface
--  supports setting a child as the placeholder by specifying "placeholder" as
--  the "type" attribute of a <child> element. See Gtk.List_Box.Set_Placeholder
--  for info.
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> list ╰── row[.activatable] ]|
--
--  GtkListBox uses a single CSS node named list. Each GtkListBoxRow uses a
--  single CSS node named row. The row nodes get the .activatable style class
--  added when appropriate.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Glib.List_Model;  use Glib.List_Model;
with Glib.Object;      use Glib.Object;
with Glib.Properties;  use Glib.Properties;
with Glib.Types;       use Glib.Types;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Buildable;    use Gtk.Buildable;
with Gtk.Container;    use Gtk.Container;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.List_Box_Row; use Gtk.List_Box_Row;
with Gtk.Widget;       use Gtk.Widget;

package Gtk.List_Box is

   type Gtk_List_Box_Record is new Gtk_Container_Record with null record;
   type Gtk_List_Box is access all Gtk_List_Box_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_List_Box_Create_Widget_Func is access function (Item : System.Address) return Gtk.Widget.Gtk_Widget;
   --  Called for list boxes that are bound to a Glib.List_Model.Glist_Model
   --  with Gtk.List_Box.Bind_Model for each item that gets added to the model.
   --  Versions of GTK+ prior to 3.18 called gtk_widget_show_all on the rows
   --  created by the GtkListBoxCreateWidgetFunc, but this forced all widgets
   --  inside the row to be shown, and is no longer the case. Applications
   --  should be updated to show the desired row widgets.
   --  Since: gtk+ 3.16
   --  "item": the item from the model for which to create a widget for

   type Gtk_List_Box_Foreach_Func is access procedure
     (Box : not null access Gtk_List_Box_Record'Class;
      Row : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class);
   --  A function used by Gtk.List_Box.Selected_Foreach. It will be called on
   --  every selected child of the Box.
   --  Since: gtk+ 3.14
   --  "box": a Gtk.List_Box.Gtk_List_Box
   --  "row": a Gtk.List_Box_Row.Gtk_List_Box_Row

   type Gtk_List_Box_Filter_Func is access function
     (Row : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class)
   return Boolean;
   --  Will be called whenever the row changes or is added and lets you
   --  control if the row should be visible or not.
   --  Since: gtk+ 3.10
   --  "row": the row that may be filtered

   type Gtk_List_Box_Update_Header_Func is access procedure
     (Row    : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class;
      Before : access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class);
   --  Whenever Row changes or which row is before Row changes this is called,
   --  which lets you update the header on Row. You may remove or set a new one
   --  via gtk_list_box_row_set_header or just change the state of the current
   --  header widget.
   --  Since: gtk+ 3.10
   --  "row": the row to update
   --  "before": the row before Row, or null if it is first

   type Gtk_List_Box_Sort_Func is access function
     (Row1 : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class;
      Row2 : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class)
   return Glib.Gint;
   --  Compare two rows to determine which should be first.
   --  Since: gtk+ 3.10
   --  "row1": the first row
   --  "row2": the second row

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_List_Box);
   procedure Initialize (Self : not null access Gtk_List_Box_Record'Class);
   --  Creates a new Gtk.List_Box.Gtk_List_Box container.
   --  Since: gtk+ 3.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_List_Box_New return Gtk_List_Box;
   --  Creates a new Gtk.List_Box.Gtk_List_Box container.
   --  Since: gtk+ 3.10

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_list_box_get_type");

   -------------
   -- Methods --
   -------------

   procedure Bind_Model
      (Self                : not null access Gtk_List_Box_Record;
       Model               : Glib.List_Model.Glist_Model;
       Create_Widget_Func  : Gtk_List_Box_Create_Widget_Func;
       User_Data_Free_Func : Glib.G_Destroy_Notify_Address);
   --  Binds Model to Box.
   --  If Box was already bound to a model, that previous binding is
   --  destroyed.
   --  The contents of Box are cleared and then filled with widgets that
   --  represent items from Model. Box is updated whenever Model changes. If
   --  Model is null, Box is left empty.
   --  It is undefined to add or remove widgets directly (for example, with
   --  Gtk.List_Box.Insert or Gtk.Container.Add) while Box is bound to a model.
   --  Note that using a model is incompatible with the filtering and sorting
   --  functionality in GtkListBox. When using a model, filtering and sorting
   --  should be implemented by the model.
   --  Since: gtk+ 3.16
   --  "model": the Glib.List_Model.Glist_Model to be bound to Box
   --  "create_widget_func": a function that creates widgets for items or null
   --  in case you also passed null as Model
   --  "user_data_free_func": function for freeing User_Data

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Bind_Model_User_Data is

      type Gtk_List_Box_Create_Widget_Func is access function
        (Item      : System.Address;
         User_Data : User_Data_Type) return Gtk.Widget.Gtk_Widget;
      --  Called for list boxes that are bound to a Glib.List_Model.Glist_Model
      --  with Gtk.List_Box.Bind_Model for each item that gets added to the model.
      --  Versions of GTK+ prior to 3.18 called gtk_widget_show_all on the rows
      --  created by the GtkListBoxCreateWidgetFunc, but this forced all widgets
      --  inside the row to be shown, and is no longer the case. Applications
      --  should be updated to show the desired row widgets.
      --  Since: gtk+ 3.16
      --  "item": the item from the model for which to create a widget for
      --  "user_data": user data

      procedure Bind_Model
         (Self                : not null access Gtk.List_Box.Gtk_List_Box_Record'Class;
          Model               : Glib.List_Model.Glist_Model;
          Create_Widget_Func  : Gtk_List_Box_Create_Widget_Func;
          User_Data           : User_Data_Type;
          User_Data_Free_Func : Glib.G_Destroy_Notify_Address);
      --  Binds Model to Box.
      --  If Box was already bound to a model, that previous binding is
      --  destroyed.
      --  The contents of Box are cleared and then filled with widgets that
      --  represent items from Model. Box is updated whenever Model changes. If
      --  Model is null, Box is left empty.
      --  It is undefined to add or remove widgets directly (for example, with
      --  Gtk.List_Box.Insert or Gtk.Container.Add) while Box is bound to a
      --  model.
      --  Note that using a model is incompatible with the filtering and
      --  sorting functionality in GtkListBox. When using a model, filtering
      --  and sorting should be implemented by the model.
      --  Since: gtk+ 3.16
      --  "model": the Glib.List_Model.Glist_Model to be bound to Box
      --  "create_widget_func": a function that creates widgets for items or
      --  null in case you also passed null as Model
      --  "user_data": user data passed to Create_Widget_Func
      --  "user_data_free_func": function for freeing User_Data

   end Bind_Model_User_Data;

   procedure Drag_Highlight_Row
      (Self : not null access Gtk_List_Box_Record;
       Row  : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class);
   --  This is a helper function for implementing DnD onto a
   --  Gtk.List_Box.Gtk_List_Box. The passed in Row will be highlighted via
   --  Gtk.Widget.Drag_Highlight, and any previously highlighted row will be
   --  unhighlighted.
   --  The row will also be unhighlighted when the widget gets a drag leave
   --  event.
   --  Since: gtk+ 3.10
   --  "row": a Gtk.List_Box_Row.Gtk_List_Box_Row

   procedure Drag_Unhighlight_Row
      (Self : not null access Gtk_List_Box_Record);
   --  If a row has previously been highlighted via
   --  Gtk.List_Box.Drag_Highlight_Row it will have the highlight removed.
   --  Since: gtk+ 3.10

   function Get_Activate_On_Single_Click
      (Self : not null access Gtk_List_Box_Record) return Boolean;
   --  Returns whether rows activate on single clicks.
   --  Since: gtk+ 3.10

   procedure Set_Activate_On_Single_Click
      (Self   : not null access Gtk_List_Box_Record;
       Single : Boolean);
   --  If Single is True, rows will be activated when you click on them,
   --  otherwise you need to double-click.
   --  Since: gtk+ 3.10
   --  "single": a boolean

   function Get_Adjustment
      (Self : not null access Gtk_List_Box_Record)
       return Gtk.Adjustment.Gtk_Adjustment;
   --  Gets the adjustment (if any) that the widget uses to for vertical
   --  scrolling.
   --  Since: gtk+ 3.10

   procedure Set_Adjustment
      (Self       : not null access Gtk_List_Box_Record;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Sets the adjustment (if any) that the widget uses to for vertical
   --  scrolling. For instance, this is used to get the page size for
   --  PageUp/Down key handling.
   --  In the normal case when the Box is packed inside a
   --  Gtk.Scrolled_Window.Gtk_Scrolled_Window the adjustment from that will be
   --  picked up automatically, so there is no need to manually do that.
   --  Since: gtk+ 3.10
   --  "adjustment": the adjustment, or null

   function Get_Row_At_Index
      (Self  : not null access Gtk_List_Box_Record;
       Index : Glib.Gint) return Gtk.List_Box_Row.Gtk_List_Box_Row;
   --  Gets the n-th child in the list (not counting headers). If _Index is
   --  negative or larger than the number of items in the list, null is
   --  returned.
   --  Since: gtk+ 3.10
   --  "index_": the index of the row

   function Get_Row_At_Y
      (Self : not null access Gtk_List_Box_Record;
       Y    : Glib.Gint) return Gtk.List_Box_Row.Gtk_List_Box_Row;
   --  Gets the row at the Y position.
   --  Since: gtk+ 3.10
   --  "y": position

   function Get_Selected_Row
      (Self : not null access Gtk_List_Box_Record)
       return Gtk.List_Box_Row.Gtk_List_Box_Row;
   --  Gets the selected row.
   --  Note that the box may allow multiple selection, in which case you
   --  should use Gtk.List_Box.Selected_Foreach to find all selected rows.
   --  Since: gtk+ 3.10

   function Get_Selected_Rows
      (Self : not null access Gtk_List_Box_Record)
       return Gtk.List_Box_Row.List_Box_Row_List.Glist;
   --  Creates a list of all selected children.
   --  Since: gtk+ 3.14

   function Get_Selection_Mode
      (Self : not null access Gtk_List_Box_Record)
       return Gtk.Enums.Gtk_Selection_Mode;
   --  Gets the selection mode of the listbox.
   --  Since: gtk+ 3.10

   procedure Set_Selection_Mode
      (Self : not null access Gtk_List_Box_Record;
       Mode : Gtk.Enums.Gtk_Selection_Mode);
   --  Sets how selection works in the listbox. See
   --  Gtk.Enums.Gtk_Selection_Mode for details.
   --  Since: gtk+ 3.10
   --  "mode": The Gtk.Enums.Gtk_Selection_Mode

   procedure Insert
      (Self     : not null access Gtk_List_Box_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position : Glib.Gint);
   --  Insert the Child into the Box at Position. If a sort function is set,
   --  the widget will actually be inserted at the calculated position and this
   --  function has the same effect of Gtk.Container.Add.
   --  If Position is -1, or larger than the total number of items in the Box,
   --  then the Child will be appended to the end.
   --  Since: gtk+ 3.10
   --  "child": the Gtk.Widget.Gtk_Widget to add
   --  "position": the position to insert Child in

   procedure Invalidate_Filter (Self : not null access Gtk_List_Box_Record);
   --  Update the filtering for all rows. Call this when result of the filter
   --  function on the Box is changed due to an external factor. For instance,
   --  this would be used if the filter function just looked for a specific
   --  search string and the entry with the search string has changed.
   --  Since: gtk+ 3.10

   procedure Invalidate_Headers (Self : not null access Gtk_List_Box_Record);
   --  Update the separators for all rows. Call this when result of the header
   --  function on the Box is changed due to an external factor.
   --  Since: gtk+ 3.10

   procedure Invalidate_Sort (Self : not null access Gtk_List_Box_Record);
   --  Update the sorting for all rows. Call this when result of the sort
   --  function on the Box is changed due to an external factor.
   --  Since: gtk+ 3.10

   procedure Prepend
      (Self  : not null access Gtk_List_Box_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Prepend a widget to the list. If a sort function is set, the widget
   --  will actually be inserted at the calculated position and this function
   --  has the same effect of Gtk.Container.Add.
   --  Since: gtk+ 3.10
   --  "child": the Gtk.Widget.Gtk_Widget to add

   procedure Select_All (Self : not null access Gtk_List_Box_Record);
   --  Select all children of Box, if the selection mode allows it.
   --  Since: gtk+ 3.14

   procedure Select_Row
      (Self : not null access Gtk_List_Box_Record;
       Row  : access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class);
   --  Make Row the currently selected row.
   --  Since: gtk+ 3.10
   --  "row": The row to select or null

   procedure Selected_Foreach
      (Self : not null access Gtk_List_Box_Record;
       Func : Gtk_List_Box_Foreach_Func);
   --  Calls a function for each selected child.
   --  Note that the selection cannot be modified from within this function.
   --  Since: gtk+ 3.14
   --  "func": the function to call for each selected child

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Selected_Foreach_User_Data is

      type Gtk_List_Box_Foreach_Func is access procedure
        (Box       : not null access Gtk.List_Box.Gtk_List_Box_Record'Class;
         Row       : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class;
         User_Data : User_Data_Type);
      --  A function used by Gtk.List_Box.Selected_Foreach. It will be called on
      --  every selected child of the Box.
      --  Since: gtk+ 3.14
      --  "box": a Gtk.List_Box.Gtk_List_Box
      --  "row": a Gtk.List_Box_Row.Gtk_List_Box_Row
      --  "user_data": user data

      procedure Selected_Foreach
         (Self : not null access Gtk.List_Box.Gtk_List_Box_Record'Class;
          Func : Gtk_List_Box_Foreach_Func;
          Data : User_Data_Type);
      --  Calls a function for each selected child.
      --  Note that the selection cannot be modified from within this
      --  function.
      --  Since: gtk+ 3.14
      --  "func": the function to call for each selected child
      --  "data": user data to pass to the function

   end Selected_Foreach_User_Data;

   procedure Set_Filter_Func
      (Self        : not null access Gtk_List_Box_Record;
       Filter_Func : Gtk_List_Box_Filter_Func);
   --  By setting a filter function on the Box one can decide dynamically
   --  which of the rows to show. For instance, to implement a search function
   --  on a list that filters the original list to only show the matching rows.
   --  The Filter_Func will be called for each row after the call, and it will
   --  continue to be called each time a row changes (via
   --  Gtk.List_Box_Row.Changed) or when Gtk.List_Box.Invalidate_Filter is
   --  called.
   --  Note that using a filter function is incompatible with using a model
   --  (see Gtk.List_Box.Bind_Model).
   --  Since: gtk+ 3.10
   --  "filter_func": callback that lets you filter which rows to show

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Filter_Func_User_Data is

      type Gtk_List_Box_Filter_Func is access function
        (Row       : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class;
         User_Data : User_Data_Type) return Boolean;
      --  Will be called whenever the row changes or is added and lets you
      --  control if the row should be visible or not.
      --  Since: gtk+ 3.10
      --  "row": the row that may be filtered
      --  "user_data": user data

      procedure Set_Filter_Func
         (Self        : not null access Gtk.List_Box.Gtk_List_Box_Record'Class;
          Filter_Func : Gtk_List_Box_Filter_Func;
          User_Data   : User_Data_Type);
      --  By setting a filter function on the Box one can decide dynamically
      --  which of the rows to show. For instance, to implement a search
      --  function on a list that filters the original list to only show the
      --  matching rows.
      --  The Filter_Func will be called for each row after the call, and it
      --  will continue to be called each time a row changes (via
      --  Gtk.List_Box_Row.Changed) or when Gtk.List_Box.Invalidate_Filter is
      --  called.
      --  Note that using a filter function is incompatible with using a model
      --  (see Gtk.List_Box.Bind_Model).
      --  Since: gtk+ 3.10
      --  "filter_func": callback that lets you filter which rows to show
      --  "user_data": user data passed to Filter_Func

   end Set_Filter_Func_User_Data;

   procedure Set_Header_Func
      (Self          : not null access Gtk_List_Box_Record;
       Update_Header : Gtk_List_Box_Update_Header_Func);
   --  By setting a header function on the Box one can dynamically add headers
   --  in front of rows, depending on the contents of the row and its position
   --  in the list. For instance, one could use it to add headers in front of
   --  the first item of a new kind, in a list sorted by the kind.
   --  The Update_Header can look at the current header widget using
   --  Gtk.List_Box_Row.Get_Header and either update the state of the widget as
   --  needed, or set a new one using Gtk.List_Box_Row.Set_Header. If no header
   --  is needed, set the header to null.
   --  Note that you may get many calls Update_Header to this for a particular
   --  row when e.g. changing things that don't affect the header. In this case
   --  it is important for performance to not blindly replace an existing
   --  header with an identical one.
   --  The Update_Header function will be called for each row after the call,
   --  and it will continue to be called each time a row changes (via
   --  Gtk.List_Box_Row.Changed) and when the row before changes (either by
   --  Gtk.List_Box_Row.Changed on the previous row, or when the previous row
   --  becomes a different row). It is also called for all rows when
   --  Gtk.List_Box.Invalidate_Headers is called.
   --  Since: gtk+ 3.10
   --  "update_header": callback that lets you add row headers

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Header_Func_User_Data is

      type Gtk_List_Box_Update_Header_Func is access procedure
        (Row       : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class;
         Before    : access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class;
         User_Data : User_Data_Type);
      --  Whenever Row changes or which row is before Row changes this is called,
      --  which lets you update the header on Row. You may remove or set a new one
      --  via gtk_list_box_row_set_header or just change the state of the current
      --  header widget.
      --  Since: gtk+ 3.10
      --  "row": the row to update
      --  "before": the row before Row, or null if it is first
      --  "user_data": user data

      procedure Set_Header_Func
         (Self          : not null access Gtk.List_Box.Gtk_List_Box_Record'Class;
          Update_Header : Gtk_List_Box_Update_Header_Func;
          User_Data     : User_Data_Type);
      --  By setting a header function on the Box one can dynamically add
      --  headers in front of rows, depending on the contents of the row and
      --  its position in the list. For instance, one could use it to add
      --  headers in front of the first item of a new kind, in a list sorted by
      --  the kind.
      --  The Update_Header can look at the current header widget using
      --  Gtk.List_Box_Row.Get_Header and either update the state of the widget
      --  as needed, or set a new one using Gtk.List_Box_Row.Set_Header. If no
      --  header is needed, set the header to null.
      --  Note that you may get many calls Update_Header to this for a
      --  particular row when e.g. changing things that don't affect the
      --  header. In this case it is important for performance to not blindly
      --  replace an existing header with an identical one.
      --  The Update_Header function will be called for each row after the
      --  call, and it will continue to be called each time a row changes (via
      --  Gtk.List_Box_Row.Changed) and when the row before changes (either by
      --  Gtk.List_Box_Row.Changed on the previous row, or when the previous
      --  row becomes a different row). It is also called for all rows when
      --  Gtk.List_Box.Invalidate_Headers is called.
      --  Since: gtk+ 3.10
      --  "update_header": callback that lets you add row headers
      --  "user_data": user data passed to Update_Header

   end Set_Header_Func_User_Data;

   procedure Set_Placeholder
      (Self        : not null access Gtk_List_Box_Record;
       Placeholder : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the placeholder widget that is shown in the list when it doesn't
   --  display any visible children.
   --  Since: gtk+ 3.10
   --  "placeholder": a Gtk.Widget.Gtk_Widget or null

   procedure Set_Sort_Func
      (Self      : not null access Gtk_List_Box_Record;
       Sort_Func : Gtk_List_Box_Sort_Func);
   --  By setting a sort function on the Box one can dynamically reorder the
   --  rows of the list, based on the contents of the rows.
   --  The Sort_Func will be called for each row after the call, and will
   --  continue to be called each time a row changes (via
   --  Gtk.List_Box_Row.Changed) and when Gtk.List_Box.Invalidate_Sort is
   --  called.
   --  Note that using a sort function is incompatible with using a model (see
   --  Gtk.List_Box.Bind_Model).
   --  Since: gtk+ 3.10
   --  "sort_func": the sort function

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Sort_Func_User_Data is

      type Gtk_List_Box_Sort_Func is access function
        (Row1      : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class;
         Row2      : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class;
         User_Data : User_Data_Type) return Glib.Gint;
      --  Compare two rows to determine which should be first.
      --  Since: gtk+ 3.10
      --  "row1": the first row
      --  "row2": the second row
      --  "user_data": user data

      procedure Set_Sort_Func
         (Self      : not null access Gtk.List_Box.Gtk_List_Box_Record'Class;
          Sort_Func : Gtk_List_Box_Sort_Func;
          User_Data : User_Data_Type);
      --  By setting a sort function on the Box one can dynamically reorder
      --  the rows of the list, based on the contents of the rows.
      --  The Sort_Func will be called for each row after the call, and will
      --  continue to be called each time a row changes (via
      --  Gtk.List_Box_Row.Changed) and when Gtk.List_Box.Invalidate_Sort is
      --  called.
      --  Note that using a sort function is incompatible with using a model
      --  (see Gtk.List_Box.Bind_Model).
      --  Since: gtk+ 3.10
      --  "sort_func": the sort function
      --  "user_data": user data passed to Sort_Func

   end Set_Sort_Func_User_Data;

   procedure Unselect_All (Self : not null access Gtk_List_Box_Record);
   --  Unselect all children of Box, if the selection mode allows it.
   --  Since: gtk+ 3.14

   procedure Unselect_Row
      (Self : not null access Gtk_List_Box_Record;
       Row  : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class);
   --  Unselects a single row of Box, if the selection mode allows it.
   --  Since: gtk+ 3.14
   --  "row": the row to unselected

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Activate_On_Single_Click_Property : constant Glib.Properties.Property_Boolean;

   Selection_Mode_Property : constant Gtk.Enums.Property_Gtk_Selection_Mode;

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

   type Cb_Gtk_List_Box_Gtk_Movement_Step_Gint_Void is not null access procedure
     (Self   : access Gtk_List_Box_Record'Class;
      Object : Gtk.Enums.Gtk_Movement_Step;
      P0     : Glib.Gint);

   type Cb_GObject_Gtk_Movement_Step_Gint_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Object : Gtk.Enums.Gtk_Movement_Step;
      P0     : Glib.Gint);

   Signal_Move_Cursor : constant Glib.Signal_Name := "move-cursor";
   procedure On_Move_Cursor
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_Gtk_List_Box_Gtk_Movement_Step_Gint_Void;
       After : Boolean := False);
   procedure On_Move_Cursor
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_GObject_Gtk_Movement_Step_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   -- 
   --  Callback parameters:

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
   --  The ::row-activated signal is emitted when a row has been activated by
   --  the user.

   Signal_Row_Selected : constant Glib.Signal_Name := "row-selected";
   procedure On_Row_Selected
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_Gtk_List_Box_Gtk_List_Box_Row_Void;
       After : Boolean := False);
   procedure On_Row_Selected
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_GObject_Gtk_List_Box_Row_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::row-selected signal is emitted when a new row is selected, or
   --  (with a null Row) when the selection is cleared.
   --
   --  When the Box is using GTK_SELECTION_MULTIPLE, this signal will not give
   --  you the full picture of selection changes, and you should use the
   --  Gtk.List_Box.Gtk_List_Box::selected-rows-changed signal instead.

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
   --  The ::select-all signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted to select all children of the box, if the selection
   --  mode permits it.
   --
   --  The default bindings for this signal is Ctrl-a.

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
   --  The ::selected-rows-changed signal is emitted when the set of selected
   --  rows changes.

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
   --  The ::unselect-all signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted to unselect all children of the box, if the selection
   --  mode permits it.
   --
   --  The default bindings for this signal is Ctrl-Shift-a.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

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

private
   Selection_Mode_Property : constant Gtk.Enums.Property_Gtk_Selection_Mode :=
     Gtk.Enums.Build ("selection-mode");
   Activate_On_Single_Click_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("activate-on-single-click");
end Gtk.List_Box;
