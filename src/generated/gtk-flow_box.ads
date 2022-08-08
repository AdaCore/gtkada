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
--  A GtkFlowBox positions child widgets in sequence according to its
--  orientation.
--
--  For instance, with the horizontal orientation, the widgets will be
--  arranged from left to right, starting a new row under the previous row when
--  necessary. Reducing the width in this case will require more rows, so a
--  larger height will be requested.
--
--  Likewise, with the vertical orientation, the widgets will be arranged from
--  top to bottom, starting a new column to the right when necessary. Reducing
--  the height will require more columns, so a larger width will be requested.
--
--  The size request of a GtkFlowBox alone may not be what you expect; if you
--  need to be able to shrink it along both axes and dynamically reflow its
--  children, you may have to wrap it in a
--  Gtk.Scrolled_Window.Gtk_Scrolled_Window to enable that.
--
--  The children of a GtkFlowBox can be dynamically sorted and filtered.
--
--  Although a GtkFlowBox must have only Gtk.Flow_Box_Child.Gtk_Flow_Box_Child
--  children, you can add any kind of widget to it via Gtk.Container.Add, and a
--  GtkFlowBoxChild widget will automatically be inserted between the box and
--  the widget.
--
--  Also see Gtk.List_Box.Gtk_List_Box.
--
--  GtkFlowBox was added in GTK+ 3.12.
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> flowbox ├── flowboxchild │ ╰── <child> ├──
--  flowboxchild │ ╰── <child> ┊ ╰── [rubberband] ]|
--
--  GtkFlowBox uses a single CSS node with name flowbox. GtkFlowBoxChild uses
--  a single CSS node with name flowboxchild. For rubberband selection, a
--  subnode with name rubberband is used.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.List_Model;    use Glib.List_Model;
with Glib.Object;        use Glib.Object;
with Glib.Properties;    use Glib.Properties;
with Glib.Types;         use Glib.Types;
with Gtk.Adjustment;     use Gtk.Adjustment;
with Gtk.Buildable;      use Gtk.Buildable;
with Gtk.Container;      use Gtk.Container;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Flow_Box_Child; use Gtk.Flow_Box_Child;
with Gtk.Orientable;     use Gtk.Orientable;
with Gtk.Widget;         use Gtk.Widget;

package Gtk.Flow_Box is

   type Gtk_Flow_Box_Record is new Gtk_Container_Record with null record;
   type Gtk_Flow_Box is access all Gtk_Flow_Box_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Flow_Box_Create_Widget_Func is access function (Item : System.Address) return Gtk.Widget.Gtk_Widget;
   --  Called for flow boxes that are bound to a Glib.List_Model.Glist_Model
   --  with Gtk.Flow_Box.Bind_Model for each item that gets added to the model.
   --  Since: gtk+ 3.18
   --  "item": the item from the model for which to create a widget for

   type Gtk_Flow_Box_Foreach_Func is access procedure
     (Box   : not null access Gtk_Flow_Box_Record'Class;
      Child : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class);
   --  A function used by Gtk.Flow_Box.Selected_Foreach. It will be called on
   --  every selected child of the Box.
   --  Since: gtk+ 3.12
   --  "box": a Gtk.Flow_Box.Gtk_Flow_Box
   --  "child": a Gtk.Flow_Box_Child.Gtk_Flow_Box_Child

   type Gtk_Flow_Box_Filter_Func is access function
     (Child : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class)
   return Boolean;
   --  A function that will be called whenrever a child changes or is added.
   --  It lets you control if the child should be visible or not.
   --  Since: gtk+ 3.12
   --  "child": a Gtk.Flow_Box_Child.Gtk_Flow_Box_Child that may be filtered

   type Gtk_Flow_Box_Sort_Func is access function
     (Child1 : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class;
      Child2 : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class)
   return Glib.Gint;
   --  A function to compare two children to determine which should come
   --  first.
   --  Since: gtk+ 3.12
   --  "child1": the first child
   --  "child2": the second child

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Flow_Box);
   procedure Initialize (Self : not null access Gtk_Flow_Box_Record'Class);
   --  Creates a GtkFlowBox.
   --  Since: gtk+ 3.12
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Flow_Box_New return Gtk_Flow_Box;
   --  Creates a GtkFlowBox.
   --  Since: gtk+ 3.12

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_flow_box_get_type");

   -------------
   -- Methods --
   -------------

   procedure Bind_Model
      (Self                : not null access Gtk_Flow_Box_Record;
       Model               : Glib.List_Model.Glist_Model;
       Create_Widget_Func  : Gtk_Flow_Box_Create_Widget_Func;
       User_Data_Free_Func : Glib.G_Destroy_Notify_Address);
   --  Binds Model to Box.
   --  If Box was already bound to a model, that previous binding is
   --  destroyed.
   --  The contents of Box are cleared and then filled with widgets that
   --  represent items from Model. Box is updated whenever Model changes. If
   --  Model is null, Box is left empty.
   --  It is undefined to add or remove widgets directly (for example, with
   --  Gtk.Flow_Box.Insert or Gtk.Container.Add) while Box is bound to a model.
   --  Note that using a model is incompatible with the filtering and sorting
   --  functionality in GtkFlowBox. When using a model, filtering and sorting
   --  should be implemented by the model.
   --  Since: gtk+ 3.18
   --  "model": the Glib.List_Model.Glist_Model to be bound to Box
   --  "create_widget_func": a function that creates widgets for items
   --  "user_data_free_func": function for freeing User_Data

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Bind_Model_User_Data is

      type Gtk_Flow_Box_Create_Widget_Func is access function
        (Item      : System.Address;
         User_Data : User_Data_Type) return Gtk.Widget.Gtk_Widget;
      --  Called for flow boxes that are bound to a Glib.List_Model.Glist_Model
      --  with Gtk.Flow_Box.Bind_Model for each item that gets added to the model.
      --  Since: gtk+ 3.18
      --  "item": the item from the model for which to create a widget for
      --  "user_data": user data from Gtk.Flow_Box.Bind_Model

      procedure Bind_Model
         (Self                : not null access Gtk.Flow_Box.Gtk_Flow_Box_Record'Class;
          Model               : Glib.List_Model.Glist_Model;
          Create_Widget_Func  : Gtk_Flow_Box_Create_Widget_Func;
          User_Data           : User_Data_Type;
          User_Data_Free_Func : Glib.G_Destroy_Notify_Address);
      --  Binds Model to Box.
      --  If Box was already bound to a model, that previous binding is
      --  destroyed.
      --  The contents of Box are cleared and then filled with widgets that
      --  represent items from Model. Box is updated whenever Model changes. If
      --  Model is null, Box is left empty.
      --  It is undefined to add or remove widgets directly (for example, with
      --  Gtk.Flow_Box.Insert or Gtk.Container.Add) while Box is bound to a
      --  model.
      --  Note that using a model is incompatible with the filtering and
      --  sorting functionality in GtkFlowBox. When using a model, filtering
      --  and sorting should be implemented by the model.
      --  Since: gtk+ 3.18
      --  "model": the Glib.List_Model.Glist_Model to be bound to Box
      --  "create_widget_func": a function that creates widgets for items
      --  "user_data": user data passed to Create_Widget_Func
      --  "user_data_free_func": function for freeing User_Data

   end Bind_Model_User_Data;

   function Get_Activate_On_Single_Click
      (Self : not null access Gtk_Flow_Box_Record) return Boolean;
   --  Returns whether children activate on single clicks.
   --  Since: gtk+ 3.12

   procedure Set_Activate_On_Single_Click
      (Self   : not null access Gtk_Flow_Box_Record;
       Single : Boolean);
   --  If Single is True, children will be activated when you click on them,
   --  otherwise you need to double-click.
   --  Since: gtk+ 3.12
   --  "single": True to emit child-activated on a single click

   function Get_Child_At_Index
      (Self : not null access Gtk_Flow_Box_Record;
       Idx  : Glib.Gint) return Gtk.Flow_Box_Child.Gtk_Flow_Box_Child;
   --  Gets the nth child in the Box.
   --  Since: gtk+ 3.12
   --  "idx": the position of the child

   function Get_Child_At_Pos
      (Self : not null access Gtk_Flow_Box_Record;
       X    : Glib.Gint;
       Y    : Glib.Gint) return Gtk.Flow_Box_Child.Gtk_Flow_Box_Child;
   --  Gets the child in the (X, Y) position.
   --  Since: gtk+ 3.22.6
   --  "x": the x coordinate of the child
   --  "y": the y coordinate of the child

   function Get_Column_Spacing
      (Self : not null access Gtk_Flow_Box_Record) return Guint;
   --  Gets the horizontal spacing.
   --  Since: gtk+ 3.12

   procedure Set_Column_Spacing
      (Self    : not null access Gtk_Flow_Box_Record;
       Spacing : Guint);
   --  Sets the horizontal space to add between children. See the
   --  Gtk.Flow_Box.Gtk_Flow_Box:column-spacing property.
   --  Since: gtk+ 3.12
   --  "spacing": the spacing to use

   function Get_Homogeneous
      (Self : not null access Gtk_Flow_Box_Record) return Boolean;
   --  Returns whether the box is homogeneous (all children are the same
   --  size). See Gtk.Box.Set_Homogeneous.
   --  Since: gtk+ 3.12

   procedure Set_Homogeneous
      (Self        : not null access Gtk_Flow_Box_Record;
       Homogeneous : Boolean);
   --  Sets the Gtk.Flow_Box.Gtk_Flow_Box:homogeneous property of Box,
   --  controlling whether or not all children of Box are given equal space in
   --  the box.
   --  Since: gtk+ 3.12
   --  "homogeneous": True to create equal allotments, False for variable
   --  allotments

   function Get_Max_Children_Per_Line
      (Self : not null access Gtk_Flow_Box_Record) return Guint;
   --  Gets the maximum number of children per line.
   --  Since: gtk+ 3.12

   procedure Set_Max_Children_Per_Line
      (Self       : not null access Gtk_Flow_Box_Record;
       N_Children : Guint);
   --  Sets the maximum number of children to request and allocate space for
   --  in Box's orientation.
   --  Setting the maximum number of children per line limits the overall
   --  natural size request to be no more than N_Children children long in the
   --  given orientation.
   --  Since: gtk+ 3.12
   --  "n_children": the maximum number of children per line

   function Get_Min_Children_Per_Line
      (Self : not null access Gtk_Flow_Box_Record) return Guint;
   --  Gets the minimum number of children per line.
   --  Since: gtk+ 3.12

   procedure Set_Min_Children_Per_Line
      (Self       : not null access Gtk_Flow_Box_Record;
       N_Children : Guint);
   --  Sets the minimum number of children to line up in Box's orientation
   --  before flowing.
   --  Since: gtk+ 3.12
   --  "n_children": the minimum number of children per line

   function Get_Row_Spacing
      (Self : not null access Gtk_Flow_Box_Record) return Guint;
   --  Gets the vertical spacing.
   --  Since: gtk+ 3.12

   procedure Set_Row_Spacing
      (Self    : not null access Gtk_Flow_Box_Record;
       Spacing : Guint);
   --  Sets the vertical space to add between children. See the
   --  Gtk.Flow_Box.Gtk_Flow_Box:row-spacing property.
   --  Since: gtk+ 3.12
   --  "spacing": the spacing to use

   function Get_Selected_Children
      (Self : not null access Gtk_Flow_Box_Record)
       return Gtk.Widget.Widget_List.Glist;
   --  Creates a list of all selected children.
   --  Since: gtk+ 3.12

   function Get_Selection_Mode
      (Self : not null access Gtk_Flow_Box_Record)
       return Gtk.Enums.Gtk_Selection_Mode;
   --  Gets the selection mode of Box.
   --  Since: gtk+ 3.12

   procedure Set_Selection_Mode
      (Self : not null access Gtk_Flow_Box_Record;
       Mode : Gtk.Enums.Gtk_Selection_Mode);
   --  Sets how selection works in Box. See Gtk.Enums.Gtk_Selection_Mode for
   --  details.
   --  Since: gtk+ 3.12
   --  "mode": the new selection mode

   procedure Insert
      (Self     : not null access Gtk_Flow_Box_Record;
       Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position : Glib.Gint);
   --  Inserts the Widget into Box at Position.
   --  If a sort function is set, the widget will actually be inserted at the
   --  calculated position and this function has the same effect as
   --  Gtk.Container.Add.
   --  If Position is -1, or larger than the total number of children in the
   --  Box, then the Widget will be appended to the end.
   --  Since: gtk+ 3.12
   --  "widget": the Gtk.Widget.Gtk_Widget to add
   --  "position": the position to insert Child in

   procedure Invalidate_Filter (Self : not null access Gtk_Flow_Box_Record);
   --  Updates the filtering for all children.
   --  Call this function when the result of the filter function on the Box is
   --  changed due ot an external factor. For instance, this would be used if
   --  the filter function just looked for a specific search term, and the
   --  entry with the string has changed.
   --  Since: gtk+ 3.12

   procedure Invalidate_Sort (Self : not null access Gtk_Flow_Box_Record);
   --  Updates the sorting for all children.
   --  Call this when the result of the sort function on Box is changed due to
   --  an external factor.
   --  Since: gtk+ 3.12

   procedure Select_All (Self : not null access Gtk_Flow_Box_Record);
   --  Select all children of Box, if the selection mode allows it.
   --  Since: gtk+ 3.12

   procedure Select_Child
      (Self  : not null access Gtk_Flow_Box_Record;
       Child : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class);
   --  Selects a single child of Box, if the selection mode allows it.
   --  Since: gtk+ 3.12
   --  "child": a child of Box

   procedure Selected_Foreach
      (Self : not null access Gtk_Flow_Box_Record;
       Func : Gtk_Flow_Box_Foreach_Func);
   --  Calls a function for each selected child.
   --  Note that the selection cannot be modified from within this function.
   --  Since: gtk+ 3.12
   --  "func": the function to call for each selected child

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Selected_Foreach_User_Data is

      type Gtk_Flow_Box_Foreach_Func is access procedure
        (Box       : not null access Gtk.Flow_Box.Gtk_Flow_Box_Record'Class;
         Child     : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class;
         User_Data : User_Data_Type);
      --  A function used by Gtk.Flow_Box.Selected_Foreach. It will be called on
      --  every selected child of the Box.
      --  Since: gtk+ 3.12
      --  "box": a Gtk.Flow_Box.Gtk_Flow_Box
      --  "child": a Gtk.Flow_Box_Child.Gtk_Flow_Box_Child
      --  "user_data": user data

      procedure Selected_Foreach
         (Self : not null access Gtk.Flow_Box.Gtk_Flow_Box_Record'Class;
          Func : Gtk_Flow_Box_Foreach_Func;
          Data : User_Data_Type);
      --  Calls a function for each selected child.
      --  Note that the selection cannot be modified from within this
      --  function.
      --  Since: gtk+ 3.12
      --  "func": the function to call for each selected child
      --  "data": user data to pass to the function

   end Selected_Foreach_User_Data;

   procedure Set_Filter_Func
      (Self        : not null access Gtk_Flow_Box_Record;
       Filter_Func : Gtk_Flow_Box_Filter_Func);
   --  By setting a filter function on the Box one can decide dynamically
   --  which of the children to show. For instance, to implement a search
   --  function that only shows the children matching the search terms.
   --  The Filter_Func will be called for each child after the call, and it
   --  will continue to be called each time a child changes (via
   --  Gtk.Flow_Box_Child.Changed) or when Gtk.Flow_Box.Invalidate_Filter is
   --  called.
   --  Note that using a filter function is incompatible with using a model
   --  (see Gtk.Flow_Box.Bind_Model).
   --  Since: gtk+ 3.12
   --  "filter_func": callback that lets you filter which children to show

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Filter_Func_User_Data is

      type Gtk_Flow_Box_Filter_Func is access function
        (Child     : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class;
         User_Data : User_Data_Type) return Boolean;
      --  A function that will be called whenrever a child changes or is added.
      --  It lets you control if the child should be visible or not.
      --  Since: gtk+ 3.12
      --  "child": a Gtk.Flow_Box_Child.Gtk_Flow_Box_Child that may be filtered
      --  "user_data": user data

      procedure Set_Filter_Func
         (Self        : not null access Gtk.Flow_Box.Gtk_Flow_Box_Record'Class;
          Filter_Func : Gtk_Flow_Box_Filter_Func;
          User_Data   : User_Data_Type);
      --  By setting a filter function on the Box one can decide dynamically
      --  which of the children to show. For instance, to implement a search
      --  function that only shows the children matching the search terms.
      --  The Filter_Func will be called for each child after the call, and it
      --  will continue to be called each time a child changes (via
      --  Gtk.Flow_Box_Child.Changed) or when Gtk.Flow_Box.Invalidate_Filter is
      --  called.
      --  Note that using a filter function is incompatible with using a model
      --  (see Gtk.Flow_Box.Bind_Model).
      --  Since: gtk+ 3.12
      --  "filter_func": callback that lets you filter which children to show
      --  "user_data": user data passed to Filter_Func

   end Set_Filter_Func_User_Data;

   procedure Set_Hadjustment
      (Self       : not null access Gtk_Flow_Box_Record;
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Hooks up an adjustment to focus handling in Box. The adjustment is also
   --  used for autoscrolling during rubberband selection. See
   --  Gtk.Scrolled_Window.Get_Hadjustment for a typical way of obtaining the
   --  adjustment, and Gtk.Flow_Box.Set_Vadjustmentfor setting the vertical
   --  adjustment.
   --  The adjustments have to be in pixel units and in the same coordinate
   --  system as the allocation for immediate children of the box.
   --  Since: gtk+ 3.12
   --  "adjustment": an adjustment which should be adjusted when the focus is
   --  moved among the descendents of Container

   procedure Set_Sort_Func
      (Self      : not null access Gtk_Flow_Box_Record;
       Sort_Func : Gtk_Flow_Box_Sort_Func);
   --  By setting a sort function on the Box, one can dynamically reorder the
   --  children of the box, based on the contents of the children.
   --  The Sort_Func will be called for each child after the call, and will
   --  continue to be called each time a child changes (via
   --  Gtk.Flow_Box_Child.Changed) and when Gtk.Flow_Box.Invalidate_Sort is
   --  called.
   --  Note that using a sort function is incompatible with using a model (see
   --  Gtk.Flow_Box.Bind_Model).
   --  Since: gtk+ 3.12
   --  "sort_func": the sort function

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
      --  Since: gtk+ 3.12
      --  "child1": the first child
      --  "child2": the second child
      --  "user_data": user data

      procedure Set_Sort_Func
         (Self      : not null access Gtk.Flow_Box.Gtk_Flow_Box_Record'Class;
          Sort_Func : Gtk_Flow_Box_Sort_Func;
          User_Data : User_Data_Type);
      --  By setting a sort function on the Box, one can dynamically reorder
      --  the children of the box, based on the contents of the children.
      --  The Sort_Func will be called for each child after the call, and will
      --  continue to be called each time a child changes (via
      --  Gtk.Flow_Box_Child.Changed) and when Gtk.Flow_Box.Invalidate_Sort is
      --  called.
      --  Note that using a sort function is incompatible with using a model
      --  (see Gtk.Flow_Box.Bind_Model).
      --  Since: gtk+ 3.12
      --  "sort_func": the sort function
      --  "user_data": user data passed to Sort_Func

   end Set_Sort_Func_User_Data;

   procedure Set_Vadjustment
      (Self       : not null access Gtk_Flow_Box_Record;
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Hooks up an adjustment to focus handling in Box. The adjustment is also
   --  used for autoscrolling during rubberband selection. See
   --  Gtk.Scrolled_Window.Get_Vadjustment for a typical way of obtaining the
   --  adjustment, and Gtk.Flow_Box.Set_Hadjustmentfor setting the horizontal
   --  adjustment.
   --  The adjustments have to be in pixel units and in the same coordinate
   --  system as the allocation for immediate children of the box.
   --  Since: gtk+ 3.12
   --  "adjustment": an adjustment which should be adjusted when the focus is
   --  moved among the descendents of Container

   procedure Unselect_All (Self : not null access Gtk_Flow_Box_Record);
   --  Unselect all children of Box, if the selection mode allows it.
   --  Since: gtk+ 3.12

   procedure Unselect_Child
      (Self  : not null access Gtk_Flow_Box_Record;
       Child : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class);
   --  Unselects a single child of Box, if the selection mode allows it.
   --  Since: gtk+ 3.12
   --  "child": a child of Box

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

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
   --  The ::activate-cursor-child signal is a [keybinding
   --  signal][GtkBindingSignal] which gets emitted when the user activates the
   --  Box.

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
   --  The ::child-activated signal is emitted when a child has been activated
   --  by the user.

   type Cb_Gtk_Flow_Box_Gtk_Movement_Step_Gint_Boolean is not null access function
     (Self  : access Gtk_Flow_Box_Record'Class;
      Step  : Gtk.Enums.Gtk_Movement_Step;
      Count : Glib.Gint) return Boolean;

   type Cb_GObject_Gtk_Movement_Step_Gint_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Step  : Gtk.Enums.Gtk_Movement_Step;
      Count : Glib.Gint) return Boolean;

   Signal_Move_Cursor : constant Glib.Signal_Name := "move-cursor";
   procedure On_Move_Cursor
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_Gtk_Flow_Box_Gtk_Movement_Step_Gint_Boolean;
       After : Boolean := False);
   procedure On_Move_Cursor
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_GObject_Gtk_Movement_Step_Gint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::move-cursor signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when the user initiates a cursor movement.
   --
   --  Applications should not connect to it, but may emit it with
   --  g_signal_emit_by_name if they need to control the cursor
   --  programmatically.
   --
   --  The default bindings for this signal come in two variants, the variant
   --  with the Shift modifier extends the selection, the variant without the
   --  Shift modifer does not. There are too many key combinations to list them
   --  all here. - Arrow keys move by individual children - Home/End keys move
   --  to the ends of the box - PageUp/PageDown keys move vertically by pages
   -- 
   --  Callback parameters:
   --    --  "step": the granularity fo the move, as a Gtk.Enums.Gtk_Movement_Step
   --    --  "count": the number of Step units to move
   --    --  Returns True to stop other handlers from being invoked for the event.
   -- False to propagate the event further.

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
   --  The ::select-all signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted to select all children of the box, if the selection
   --  mode permits it.
   --
   --  The default bindings for this signal is Ctrl-a.

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
   --  The ::selected-children-changed signal is emitted when the set of
   --  selected children changes.
   --
   --  Use Gtk.Flow_Box.Selected_Foreach or Gtk.Flow_Box.Get_Selected_Children
   --  to obtain the selected children.

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
   --  The ::toggle-cursor-child signal is a [keybinding
   --  signal][GtkBindingSignal] which toggles the selection of the child that
   --  has the focus.
   --
   --  The default binding for this signal is Ctrl-Space.

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
   --
   --  - "Orientable"

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
end Gtk.Flow_Box;
