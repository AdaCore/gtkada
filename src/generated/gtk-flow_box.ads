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

--  Puts child widgets in a reflowing grid.
--
--  <picture> <source srcset="flow-box-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkFlowBox" src="flow-box.png"> </picture>
--  For instance, with the horizontal orientation, the widgets will be
--  arranged from left to right, starting a new row under the previous row when
--  necessary. Reducing the width in this case will require more rows, so a
--  larger height will be requested.
--
--  Likewise, with the vertical orientation, the widgets will be arranged from
--  top to bottom, starting a new column to the right when necessary. Reducing
--  the height will require more columns, so a larger width will be requested.
--
--  The size request of a `GtkFlowBox` alone may not be what you expect; if
--  you need to be able to shrink it along both axes and dynamically reflow its
--  children, you may have to wrap it in a `GtkScrolledWindow` to enable that.
--
--  The children of a `GtkFlowBox` can be dynamically sorted and filtered.
--
--  Although a `GtkFlowBox` must have only `GtkFlowBoxChild` children, you can
--  add any kind of widget to it via [methodGtk.FlowBox.insert], and a
--  `GtkFlowBoxChild` widget will automatically be inserted between the box and
--  the widget.
--
--  Also see [classGtk.ListBox].
--
--  # Shortcuts and Gestures
--
--  The following signals have default keybindings:
--
--  - [signalGtk.FlowBox::move-cursor] - [signalGtk.FlowBox::select-all] -
--  [signalGtk.FlowBox::toggle-cursor-child] -
--  [signalGtk.FlowBox::unselect-all]
--
--  # CSS nodes
--
--  ``` flowbox ├── flowboxchild │ ╰── <child> ├── flowboxchild │ ╰── <child>
--  ┊ ╰── [rubberband] ```
--
--  `GtkFlowBox` uses a single CSS node with name flowbox. `GtkFlowBoxChild`
--  uses a single CSS node with name flowboxchild. For rubberband selection, a
--  subnode with name rubberband is used.
--
--  # Accessibility
--
--  `GtkFlowBox` uses the [enumGtk.AccessibleRole.grid] role, and
--  `GtkFlowBoxChild` uses the [enumGtk.AccessibleRole.grid_cell] role.
--
--  <group>Layout containers</group>
--  <gtkada_demo>create_flow_box.adb</gtkada_demo>

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
with Gtk.Flow_Box_Child;    use Gtk.Flow_Box_Child;
with Gtk.Orientable;        use Gtk.Orientable;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Flow_Box is

   type Gtk_Flow_Box_Record is new Gtk_Widget_Record with null record;
   type Gtk_Flow_Box is access all Gtk_Flow_Box_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Flow_Box_Create_Widget_Func is access function (Item : System.Address) return Gtk.Widget.Gtk_Widget;
   --  Called for flow boxes that are bound to a `GListModel`.
   --  This function is called for each item that gets added to the model.
   --  @param Item the item from the model for which to create a widget for
   --  @return a `GtkWidget` that represents Item. Has
   --  transfer-ownership='full'.

   type Gtk_Flow_Box_Foreach_Func is access procedure
     (Box   : not null access Gtk_Flow_Box_Record'Class;
      Child : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class);
   --  A function used by Gtk.Flow_Box.Selected_Foreach.
   --  It will be called on every selected child of the Box.
   --  @param Box a `GtkFlowBox`
   --  @param Child a `GtkFlowBoxChild`

   type Gtk_Flow_Box_Filter_Func is access function
     (Child : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class)
   return Boolean;
   --  A function that will be called whenever a child changes or is added.
   --  It lets you control if the child should be visible or not.
   --  @param Child a `GtkFlowBoxChild` that may be filtered
   --  @return True if the row should be visible, False otherwise

   type Gtk_Flow_Box_Sort_Func is access function
     (Child1 : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class;
      Child2 : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class)
   return Glib.Gint;
   --  A function to compare two children to determine which should come
   --  first.
   --  @param Child1 the first child
   --  @param Child2 the second child
   --  @return < 0 if Child1 should be before Child2, 0 if they are equal, and
   --  > 0 otherwise

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Flow_Box);
   procedure Initialize (Self : not null access Gtk_Flow_Box_Record'Class);
   --  Creates a `GtkFlowBox`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Flow_Box_New return Gtk_Flow_Box;
   --  Creates a `GtkFlowBox`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_flow_box_get_type");

   -------------
   -- Methods --
   -------------

   procedure Append
      (Self  : not null access Gtk_Flow_Box_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds Child to the end of Self.
   --  If a sort function is set, the widget will actually be inserted at the
   --  calculated position.
   --  See also: [methodGtk.FlowBox.insert].
   --  Since: gtk+ 4.6
   --  @param Child the `GtkWidget` to add

   procedure Bind_Model
      (Self               : not null access Gtk_Flow_Box_Record;
       Model              : Glib.List_Model.Glist_Model;
       Create_Widget_Func : Gtk_Flow_Box_Create_Widget_Func);
   --  Binds Model to Box.
   --  If Box was already bound to a model, that previous binding is
   --  destroyed.
   --  The contents of Box are cleared and then filled with widgets that
   --  represent items from Model. Box is updated whenever Model changes. If
   --  Model is null, Box is left empty.
   --  It is undefined to add or remove widgets directly (for example, with
   --  [methodGtk.FlowBox.insert]) while Box is bound to a model.
   --  Note that using a model is incompatible with the filtering and sorting
   --  functionality in `GtkFlowBox`. When using a model, filtering and sorting
   --  should be implemented by the model.
   --  @param Model the `GListModel` to be bound to Box
   --  @param Create_Widget_Func a function that creates widgets for items

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Bind_Model_User_Data is

      type Gtk_Flow_Box_Create_Widget_Func is access function
        (Item      : System.Address;
         User_Data : User_Data_Type) return Gtk.Widget.Gtk_Widget;
      --  Called for flow boxes that are bound to a `GListModel`.
      --  This function is called for each item that gets added to the model.
      --  @param Item the item from the model for which to create a widget for
      --  @param User_Data user data from Gtk.Flow_Box.Bind_Model
      --  @return a `GtkWidget` that represents Item. Has
      --  transfer-ownership='full'.

      procedure Bind_Model
         (Self               : not null access Gtk.Flow_Box.Gtk_Flow_Box_Record'Class;
          Model              : Glib.List_Model.Glist_Model;
          Create_Widget_Func : Gtk_Flow_Box_Create_Widget_Func;
          User_Data          : User_Data_Type);
      --  Binds Model to Box.
      --  If Box was already bound to a model, that previous binding is
      --  destroyed.
      --  The contents of Box are cleared and then filled with widgets that
      --  represent items from Model. Box is updated whenever Model changes. If
      --  Model is null, Box is left empty.
      --  It is undefined to add or remove widgets directly (for example, with
      --  [methodGtk.FlowBox.insert]) while Box is bound to a model.
      --  Note that using a model is incompatible with the filtering and
      --  sorting functionality in `GtkFlowBox`. When using a model, filtering
      --  and sorting should be implemented by the model.
      --  @param Model the `GListModel` to be bound to Box
      --  @param Create_Widget_Func a function that creates widgets for items
      --  @param User_Data user data passed to Create_Widget_Func

   end Bind_Model_User_Data;

   function Get_Activate_On_Single_Click
      (Self : not null access Gtk_Flow_Box_Record) return Boolean;
   --  Returns whether children activate on single clicks.
   --  @return True if children are activated on single click, False otherwise

   procedure Set_Activate_On_Single_Click
      (Self   : not null access Gtk_Flow_Box_Record;
       Single : Boolean);
   --  If Single is True, children will be activated when you click on them,
   --  otherwise you need to double-click.
   --  @param Single True to emit child-activated on a single click

   function Get_Child_At_Index
      (Self : not null access Gtk_Flow_Box_Record;
       Idx  : Glib.Gint) return Gtk.Flow_Box_Child.Gtk_Flow_Box_Child;
   --  Gets the nth child in the Box.
   --  @param Idx the position of the child
   --  @return the child widget, which will always be a `GtkFlowBoxChild` or
   --  null in case no child widget with the given index exists. Has
   --  transfer-ownership='none'.

   function Get_Child_At_Pos
      (Self : not null access Gtk_Flow_Box_Record;
       X    : Glib.Gint;
       Y    : Glib.Gint) return Gtk.Flow_Box_Child.Gtk_Flow_Box_Child;
   --  Gets the child in the (X, Y) position.
   --  Both X and Y are assumed to be relative to the origin of Box.
   --  @param X the x coordinate of the child
   --  @param Y the y coordinate of the child
   --  @return the child widget, which will always be a `GtkFlowBoxChild` or
   --  null in case no child widget exists for the given x and y coordinates.
   --  Has transfer-ownership='none'.

   function Get_Column_Spacing
      (Self : not null access Gtk_Flow_Box_Record) return Guint;
   --  Gets the horizontal spacing.
   --  @return the horizontal spacing

   procedure Set_Column_Spacing
      (Self    : not null access Gtk_Flow_Box_Record;
       Spacing : Guint);
   --  Sets the horizontal space to add between children.
   --  @param Spacing the spacing to use

   function Get_Homogeneous
      (Self : not null access Gtk_Flow_Box_Record) return Boolean;
   --  Returns whether the box is homogeneous.
   --  @return True if the box is homogeneous.

   procedure Set_Homogeneous
      (Self        : not null access Gtk_Flow_Box_Record;
       Homogeneous : Boolean);
   --  Sets whether or not all children of Box are given equal space in the
   --  box.
   --  @param Homogeneous True to create equal allotments, False for variable
   --  allotments

   function Get_Max_Children_Per_Line
      (Self : not null access Gtk_Flow_Box_Record) return Guint;
   --  Gets the maximum number of children per line.
   --  @return the maximum number of children per line

   procedure Set_Max_Children_Per_Line
      (Self       : not null access Gtk_Flow_Box_Record;
       N_Children : Guint);
   --  Sets the maximum number of children to request and allocate space for
   --  in Box's orientation.
   --  Setting the maximum number of children per line limits the overall
   --  natural size request to be no more than N_Children children long in the
   --  given orientation.
   --  @param N_Children the maximum number of children per line

   function Get_Min_Children_Per_Line
      (Self : not null access Gtk_Flow_Box_Record) return Guint;
   --  Gets the minimum number of children per line.
   --  @return the minimum number of children per line

   procedure Set_Min_Children_Per_Line
      (Self       : not null access Gtk_Flow_Box_Record;
       N_Children : Guint);
   --  Sets the minimum number of children to line up in Box's orientation
   --  before flowing.
   --  @param N_Children the minimum number of children per line

   function Get_Row_Spacing
      (Self : not null access Gtk_Flow_Box_Record) return Guint;
   --  Gets the vertical spacing.
   --  @return the vertical spacing

   procedure Set_Row_Spacing
      (Self    : not null access Gtk_Flow_Box_Record;
       Spacing : Guint);
   --  Sets the vertical space to add between children.
   --  @param Spacing the spacing to use

   function Get_Selected_Children
      (Self : not null access Gtk_Flow_Box_Record)
       return Gtk.Flow_Box_Child.Flow_Box_Child_List.Glist;
   --  Creates a list of all selected children.
   --  @return A `GList` containing the `GtkWidget` for each selected child.
   --  Free with g_list_free when done.

   function Get_Selection_Mode
      (Self : not null access Gtk_Flow_Box_Record)
       return Gtk.Enums.Gtk_Selection_Mode;
   --  Gets the selection mode of Box.
   --  @return the `GtkSelectionMode`

   procedure Set_Selection_Mode
      (Self : not null access Gtk_Flow_Box_Record;
       Mode : Gtk.Enums.Gtk_Selection_Mode);
   --  Sets how selection works in Box.
   --  @param Mode the new selection mode

   procedure Insert
      (Self     : not null access Gtk_Flow_Box_Record;
       Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position : Glib.Gint);
   --  Inserts the Widget into Box at Position.
   --  If a sort function is set, the widget will actually be inserted at the
   --  calculated position.
   --  If Position is -1, or larger than the total number of children in the
   --  Box, then the Widget will be appended to the end.
   --  @param Widget the `GtkWidget` to add
   --  @param Position the position to insert Child in

   procedure Invalidate_Filter (Self : not null access Gtk_Flow_Box_Record);
   --  Updates the filtering for all children.
   --  Call this function when the result of the filter function on the Box is
   --  changed due to an external factor. For instance, this would be used if
   --  the filter function just looked for a specific search term, and the
   --  entry with the string has changed.

   procedure Invalidate_Sort (Self : not null access Gtk_Flow_Box_Record);
   --  Updates the sorting for all children.
   --  Call this when the result of the sort function on Box is changed due to
   --  an external factor.

   procedure Prepend
      (Self  : not null access Gtk_Flow_Box_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds Child to the start of Self.
   --  If a sort function is set, the widget will actually be inserted at the
   --  calculated position.
   --  See also: [methodGtk.FlowBox.insert].
   --  Since: gtk+ 4.6
   --  @param Child the `GtkWidget` to add

   procedure Remove
      (Self   : not null access Gtk_Flow_Box_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes a child from Box.
   --  @param Widget the child widget to remove

   procedure Remove_All (Self : not null access Gtk_Flow_Box_Record);
   --  Removes all children from Box.
   --  This function does nothing if Box is backed by a model.
   --  Since: gtk+ 4.12

   procedure Select_All (Self : not null access Gtk_Flow_Box_Record);
   --  Select all children of Box, if the selection mode allows it.

   procedure Select_Child
      (Self  : not null access Gtk_Flow_Box_Record;
       Child : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class);
   --  Selects a single child of Box, if the selection mode allows it.
   --  @param Child a child of Box

   procedure Selected_Foreach
      (Self : not null access Gtk_Flow_Box_Record;
       Func : Gtk_Flow_Box_Foreach_Func);
   --  Calls a function for each selected child.
   --  Note that the selection cannot be modified from within this function.
   --  @param Func the function to call for each selected child

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Selected_Foreach_User_Data is

      type Gtk_Flow_Box_Foreach_Func is access procedure
        (Box       : not null access Gtk.Flow_Box.Gtk_Flow_Box_Record'Class;
         Child     : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class;
         User_Data : User_Data_Type);
      --  A function used by Gtk.Flow_Box.Selected_Foreach.
      --  It will be called on every selected child of the Box.
      --  @param Box a `GtkFlowBox`
      --  @param Child a `GtkFlowBoxChild`
      --  @param User_Data user data

      procedure Selected_Foreach
         (Self : not null access Gtk.Flow_Box.Gtk_Flow_Box_Record'Class;
          Func : Gtk_Flow_Box_Foreach_Func;
          Data : User_Data_Type);
      --  Calls a function for each selected child.
      --  Note that the selection cannot be modified from within this
      --  function.
      --  @param Func the function to call for each selected child
      --  @param Data user data to pass to the function

   end Selected_Foreach_User_Data;

   procedure Set_Filter_Func
      (Self        : not null access Gtk_Flow_Box_Record;
       Filter_Func : Gtk_Flow_Box_Filter_Func);
   --  By setting a filter function on the Box one can decide dynamically
   --  which of the children to show.
   --  For instance, to implement a search function that only shows the
   --  children matching the search terms.
   --  The Filter_Func will be called for each child after the call, and it
   --  will continue to be called each time a child changes (via
   --  [methodGtk.FlowBoxChild.changed]) or when
   --  [methodGtk.FlowBox.invalidate_filter] is called.
   --  Note that using a filter function is incompatible with using a model
   --  (see [methodGtk.FlowBox.bind_model]).
   --  @param Filter_Func callback that lets you filter which children to show

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Filter_Func_User_Data is

      type Gtk_Flow_Box_Filter_Func is access function
        (Child     : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class;
         User_Data : User_Data_Type) return Boolean;
      --  A function that will be called whenever a child changes or is added.
      --  It lets you control if the child should be visible or not.
      --  @param Child a `GtkFlowBoxChild` that may be filtered
      --  @param User_Data user data
      --  @return True if the row should be visible, False otherwise

      procedure Set_Filter_Func
         (Self        : not null access Gtk.Flow_Box.Gtk_Flow_Box_Record'Class;
          Filter_Func : Gtk_Flow_Box_Filter_Func;
          User_Data   : User_Data_Type);
      --  By setting a filter function on the Box one can decide dynamically
      --  which of the children to show.
      --  For instance, to implement a search function that only shows the
      --  children matching the search terms.
      --  The Filter_Func will be called for each child after the call, and it
      --  will continue to be called each time a child changes (via
      --  [methodGtk.FlowBoxChild.changed]) or when
      --  [methodGtk.FlowBox.invalidate_filter] is called.
      --  Note that using a filter function is incompatible with using a model
      --  (see [methodGtk.FlowBox.bind_model]).
      --  @param Filter_Func callback that lets you filter which children to
      --  show
      --  @param User_Data user data passed to Filter_Func

   end Set_Filter_Func_User_Data;

   procedure Set_Hadjustment
      (Self       : not null access Gtk_Flow_Box_Record;
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Hooks up an adjustment to focus handling in Box.
   --  The adjustment is also used for autoscrolling during rubberband
   --  selection. See [methodGtk.ScrolledWindow.get_hadjustment] for a typical
   --  way of obtaining the adjustment, and [methodGtk.FlowBox.set_vadjustment]
   --  for setting the vertical adjustment.
   --  The adjustments have to be in pixel units and in the same coordinate
   --  system as the allocation for immediate children of the box.
   --  @param Adjustment an adjustment which should be adjusted when the focus
   --  is moved among the descendents of Container

   procedure Set_Sort_Func
      (Self      : not null access Gtk_Flow_Box_Record;
       Sort_Func : Gtk_Flow_Box_Sort_Func);
   --  By setting a sort function on the Box, one can dynamically reorder the
   --  children of the box, based on the contents of the children.
   --  The Sort_Func will be called for each child after the call, and will
   --  continue to be called each time a child changes (via
   --  [methodGtk.FlowBoxChild.changed]) and when
   --  [methodGtk.FlowBox.invalidate_sort] is called.
   --  Note that using a sort function is incompatible with using a model (see
   --  [methodGtk.FlowBox.bind_model]).
   --  @param Sort_Func the sort function

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Sort_Func_User_Data is

      type Gtk_Flow_Box_Sort_Func is access function
        (Child1    : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class;
         Child2    : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class;
         User_Data : User_Data_Type) return Glib.Gint;
      --  A function to compare two children to determine which should come
      --  first.
      --  @param Child1 the first child
      --  @param Child2 the second child
      --  @param User_Data user data
      --  @return < 0 if Child1 should be before Child2, 0 if they are equal, and
      --  > 0 otherwise

      procedure Set_Sort_Func
         (Self      : not null access Gtk.Flow_Box.Gtk_Flow_Box_Record'Class;
          Sort_Func : Gtk_Flow_Box_Sort_Func;
          User_Data : User_Data_Type);
      --  By setting a sort function on the Box, one can dynamically reorder
      --  the children of the box, based on the contents of the children.
      --  The Sort_Func will be called for each child after the call, and will
      --  continue to be called each time a child changes (via
      --  [methodGtk.FlowBoxChild.changed]) and when
      --  [methodGtk.FlowBox.invalidate_sort] is called.
      --  Note that using a sort function is incompatible with using a model
      --  (see [methodGtk.FlowBox.bind_model]).
      --  @param Sort_Func the sort function
      --  @param User_Data user data passed to Sort_Func

   end Set_Sort_Func_User_Data;

   procedure Set_Vadjustment
      (Self       : not null access Gtk_Flow_Box_Record;
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Hooks up an adjustment to focus handling in Box.
   --  The adjustment is also used for autoscrolling during rubberband
   --  selection. See [methodGtk.ScrolledWindow.get_vadjustment] for a typical
   --  way of obtaining the adjustment, and [methodGtk.FlowBox.set_hadjustment]
   --  for setting the horizontal adjustment.
   --  The adjustments have to be in pixel units and in the same coordinate
   --  system as the allocation for immediate children of the box.
   --  @param Adjustment an adjustment which should be adjusted when the focus
   --  is moved among the descendents of Container

   procedure Unselect_All (Self : not null access Gtk_Flow_Box_Record);
   --  Unselect all children of Box, if the selection mode allows it.

   procedure Unselect_Child
      (Self  : not null access Gtk_Flow_Box_Record;
       Child : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class);
   --  Unselects a single child of Box, if the selection mode allows it.
   --  @param Child a child of Box

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Flow_Box_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Flow_Box_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Flow_Box_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Flow_Box_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Flow_Box_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Flow_Box_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Flow_Box_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Flow_Box_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Flow_Box_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Flow_Box_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Flow_Box_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Flow_Box_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Flow_Box_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Flow_Box_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Flow_Box_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Orientation
      (Self : not null access Gtk_Flow_Box_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Flow_Box_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

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

   Column_Spacing_Property : constant Glib.Properties.Property_Uint;
   --  The amount of horizontal space between two children.

   Homogeneous_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether all children should be allocated the same size.

   Max_Children_Per_Line_Property : constant Glib.Properties.Property_Uint;
   --  The maximum amount of children to request space for consecutively in
   --  the given orientation.

   Min_Children_Per_Line_Property : constant Glib.Properties.Property_Uint;
   --  The minimum number of children to allocate consecutively in the given
   --  orientation.
   --
   --  Setting the minimum children per line ensures that a reasonably small
   --  height will be requested for the overall minimum width of the box.

   Row_Spacing_Property : constant Glib.Properties.Property_Uint;
   --  The amount of vertical space between two children.

   Selection_Mode_Property : constant Gtk.Enums.Property_Gtk_Selection_Mode;
   --  The selection mode used by the flow box.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Flow_Box_Void is not null access procedure (Self : access Gtk_Flow_Box_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate_Cursor_Child : constant Glib.Signal_Name := "activate-cursor-child";
   procedure On_Activate_Cursor_Child
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_Gtk_Flow_Box_Void;
       After : Boolean := False);
   procedure On_Activate_Cursor_Child
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the user activates the Box.
   --
   --  This is a [keybinding signal](class.SignalAction.html).

   type Cb_Gtk_Flow_Box_Gtk_Flow_Box_Child_Void is not null access procedure
     (Self  : access Gtk_Flow_Box_Record'Class;
      Child : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class);

   type Cb_GObject_Gtk_Flow_Box_Child_Void is not null access procedure
     (Self  : access Glib.Object.GObject_Record'Class;
      Child : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class);

   Signal_Child_Activated : constant Glib.Signal_Name := "child-activated";
   procedure On_Child_Activated
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_Gtk_Flow_Box_Gtk_Flow_Box_Child_Void;
       After : Boolean := False);
   procedure On_Child_Activated
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_GObject_Gtk_Flow_Box_Child_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a child has been activated by the user.

   type Cb_Gtk_Flow_Box_Gtk_Movement_Step_Gint_Boolean_Boolean_Boolean is not null access function
     (Self   : access Gtk_Flow_Box_Record'Class;
      Step   : Gtk.Enums.Gtk_Movement_Step;
      Count  : Glib.Gint;
      Extend : Boolean;
      Modify : Boolean) return Boolean;

   type Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Boolean_Boolean is not null access function
     (Self   : access Glib.Object.GObject_Record'Class;
      Step   : Gtk.Enums.Gtk_Movement_Step;
      Count  : Glib.Gint;
      Extend : Boolean;
      Modify : Boolean) return Boolean;

   Signal_Move_Cursor : constant Glib.Signal_Name := "move-cursor";
   procedure On_Move_Cursor
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_Gtk_Flow_Box_Gtk_Movement_Step_Gint_Boolean_Boolean_Boolean;
       After : Boolean := False);
   procedure On_Move_Cursor
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the user initiates a cursor movement.
   --
   --  This is a [keybinding signal](class.SignalAction.html). Applications
   --  should not connect to it, but may emit it with g_signal_emit_by_name if
   --  they need to control the cursor programmatically.
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

   Signal_Select_All : constant Glib.Signal_Name := "select-all";
   procedure On_Select_All
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_Gtk_Flow_Box_Void;
       After : Boolean := False);
   procedure On_Select_All
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to select all children of the box, if the selection mode
   --  permits it.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  The default bindings for this signal is <kbd>Ctrl</kbd>-<kbd>a</kbd>.

   Signal_Selected_Children_Changed : constant Glib.Signal_Name := "selected-children-changed";
   procedure On_Selected_Children_Changed
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_Gtk_Flow_Box_Void;
       After : Boolean := False);
   procedure On_Selected_Children_Changed
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the set of selected children changes.
   --
   --  Use [methodGtk.FlowBox.selected_foreach] or
   --  [methodGtk.FlowBox.get_selected_children] to obtain the selected
   --  children.

   Signal_Toggle_Cursor_Child : constant Glib.Signal_Name := "toggle-cursor-child";
   procedure On_Toggle_Cursor_Child
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_Gtk_Flow_Box_Void;
       After : Boolean := False);
   procedure On_Toggle_Cursor_Child
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to toggle the selection of the child that has the focus.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  The default binding for this signal is
   --  <kbd>Ctrl</kbd>-<kbd>Space</kbd>.

   Signal_Unselect_All : constant Glib.Signal_Name := "unselect-all";
   procedure On_Unselect_All
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_Gtk_Flow_Box_Void;
       After : Boolean := False);
   procedure On_Unselect_All
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to unselect all children of the box, if the selection mode
   --  permits it.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  The default bindings for this signal is
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
   --
   --  - "Gtk.Orientable"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Flow_Box_Record, Gtk_Flow_Box);
   function "+"
     (Widget : access Gtk_Flow_Box_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Flow_Box
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Flow_Box_Record, Gtk_Flow_Box);
   function "+"
     (Widget : access Gtk_Flow_Box_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Flow_Box
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Flow_Box_Record, Gtk_Flow_Box);
   function "+"
     (Widget : access Gtk_Flow_Box_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Flow_Box
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Flow_Box_Record, Gtk_Flow_Box);
   function "+"
     (Widget : access Gtk_Flow_Box_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Flow_Box
   renames Implements_Gtk_Orientable.To_Object;

private
   Selection_Mode_Property : constant Gtk.Enums.Property_Gtk_Selection_Mode :=
     Gtk.Enums.Build ("selection-mode");
   Row_Spacing_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("row-spacing");
   Min_Children_Per_Line_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("min-children-per-line");
   Max_Children_Per_Line_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("max-children-per-line");
   Homogeneous_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("homogeneous");
   Column_Spacing_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("column-spacing");
   Activate_On_Single_Click_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("activate-on-single-click");
   Accept_Unpaired_Release_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("accept-unpaired-release");
end Gtk.Flow_Box;
