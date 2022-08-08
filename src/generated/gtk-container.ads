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
--  A GTK+ user interface is constructed by nesting widgets inside widgets.
--  Container widgets are the inner nodes in the resulting tree of widgets:
--  they contain other widgets. So, for example, you might have a
--  Gtk.Window.Gtk_Window containing a Gtk.Frame.Gtk_Frame containing a
--  Gtk.Label.Gtk_Label. If you wanted an image instead of a textual label
--  inside the frame, you might replace the Gtk.Label.Gtk_Label widget with a
--  Gtk.Image.Gtk_Image widget.
--
--  There are two major kinds of container widgets in GTK+. Both are
--  subclasses of the abstract GtkContainer base class.
--
--  The first type of container widget has a single child widget and derives
--  from Gtk.Bin.Gtk_Bin. These containers are decorators, which add some kind
--  of functionality to the child. For example, a Gtk.Button.Gtk_Button makes
--  its child into a clickable button; a Gtk.Frame.Gtk_Frame draws a frame
--  around its child and a Gtk.Window.Gtk_Window places its child widget inside
--  a top-level window.
--
--  The second type of container can have more than one child; its purpose is
--  to manage layout. This means that these containers assign sizes and
--  positions to their children. For example, a Gtk.Box.Gtk_Hbox arranges its
--  children in a horizontal row, and a Gtk.Grid.Gtk_Grid arranges the widgets
--  it contains in a two-dimensional grid.
--
--  For implementations of Gtk.Container.Gtk_Container the virtual method
--  Gtk.Container_Class.Gtk_Container_Class.forall is always required, since
--  it's used for drawing and other internal operations on the children. If the
--  Gtk.Container.Gtk_Container implementation expect to have non internal
--  children it's needed to implement both
--  Gtk.Container_Class.Gtk_Container_Class.add and
--  Gtk.Container_Class.Gtk_Container_Class.remove. If the GtkContainer
--  implementation has internal children, they should be added with
--  Gtk.Widget.Set_Parent on init and removed with Gtk.Widget.Unparent in the
--  Gtk.Widget.GObject_Class.destroy implementation. See more about
--  implementing custom widgets at https://wiki.gnome.org/HowDoI/CustomWidgets
--
--  # Height for width geometry management
--
--  GTK+ uses a height-for-width (and width-for-height) geometry management
--  system. Height-for-width means that a widget can change how much vertical
--  space it needs, depending on the amount of horizontal space that it is
--  given (and similar for width-for-height).
--
--  There are some things to keep in mind when implementing container widgets
--  that make use of GTK+'s height for width geometry management system. First,
--  it's important to note that a container must prioritize one of its
--  dimensions, that is to say that a widget or container can only have a
--  Gtk.Enums.Gtk_Size_Request_Mode that is Gtk.Enums.Height_For_Width or
--  Gtk.Enums.Width_For_Height. However, every widget and container must be
--  able to respond to the APIs for both dimensions, i.e. even if a widget has
--  a request mode that is height-for-width, it is possible that its parent
--  will request its sizes using the width-for-height APIs.
--
--  To ensure that everything works properly, here are some guidelines to
--  follow when implementing height-for-width (or width-for-height) containers.
--
--  Each request mode involves 2 virtual methods. Height-for-width apis run
--  through Gtk.Widget.Get_Preferred_Width and then through
--  Gtk.Widget.Get_Preferred_Height_For_Width. When handling requests in the
--  opposite Gtk.Enums.Gtk_Size_Request_Mode it is important that every widget
--  request at least enough space to display all of its content at all times.
--
--  When Gtk.Widget.Get_Preferred_Height is called on a container that is
--  height-for-width, the container must return the height for its minimum
--  width. This is easily achieved by simply calling the reverse apis
--  implemented for itself as follows:
--
--  |[<!-- language="C" --> static void foo_container_get_preferred_height
--  (GtkWidget *widget, gint *min_height, gint *nat_height) { if
--  (i_am_in_height_for_width_mode) { gint min_width;
--
--  GTK_WIDGET_GET_CLASS (widget)->get_preferred_width (widget, &min_width,
--  NULL); GTK_WIDGET_GET_CLASS (widget)->get_preferred_height_for_width
--  (widget, min_width, min_height, nat_height); } else { ... many containers
--  support both request modes, execute the real width-for-height request here
--  by returning the collective heights of all widgets that are stacked
--  vertically (or whatever is appropriate for this container) ... } } ]|
--
--  Similarly, when Gtk.Widget.Get_Preferred_Width_For_Height is called for a
--  container or widget that is height-for-width, it then only needs to return
--  the base minimum width like so:
--
--  |[<!-- language="C" --> static void
--  foo_container_get_preferred_width_for_height (GtkWidget *widget, gint
--  for_height, gint *min_width, gint *nat_width) { if
--  (i_am_in_height_for_width_mode) { GTK_WIDGET_GET_CLASS
--  (widget)->get_preferred_width (widget, min_width, nat_width); } else { ...
--  execute the real width-for-height request here based on the required width
--  of the children collectively if the container were to be allocated the said
--  height ... } } ]|
--
--  Height for width requests are generally implemented in terms of a virtual
--  allocation of widgets in the input orientation. Assuming an
--  height-for-width request mode, a container would implement the
--  get_preferred_height_for_width virtual function by first calling
--  Gtk.Widget.Get_Preferred_Width for each of its children.
--
--  For each potential group of children that are lined up horizontally, the
--  values returned by Gtk.Widget.Get_Preferred_Width should be collected in an
--  array of Gtk_Requested_Size structures. Any child spacing should be removed
--  from the input For_Width and then the collective size should be allocated
--  using the gtk_distribute_natural_allocation convenience function.
--
--  The container will then move on to request the preferred height for each
--  child by using Gtk.Widget.Get_Preferred_Height_For_Width and using the
--  sizes stored in the Gtk_Requested_Size array.
--
--  To allocate a height-for-width container, it's again important to consider
--  that a container must prioritize one dimension over the other. So if a
--  container is a height-for-width container it must first allocate all
--  widgets horizontally using a Gtk_Requested_Size array and
--  gtk_distribute_natural_allocation and then add any extra space (if and
--  where appropriate) for the widget to expand.
--
--  After adding all the expand space, the container assumes it was allocated
--  sufficient height to fit all of its content. At this time, the container
--  must use the total horizontal sizes of each widget to request the
--  height-for-width of each of its children and store the requests in a
--  Gtk_Requested_Size array for any widgets that stack vertically (for tabular
--  containers this can be generalized into the heights and widths of rows and
--  columns). The vertical space must then again be distributed using
--  gtk_distribute_natural_allocation while this time considering the allocated
--  height of the widget minus any vertical spacing that the container adds.
--  Then vertical expand space should be added where appropriate and available
--  and the container should go on to actually allocating the child widgets.
--
--  See [GtkWidget's geometry management section][geometry-management] to
--  learn more about implementing height-for-width geometry management for
--  widgets.
--
--  # Child properties
--
--  GtkContainer introduces child properties. These are object properties that
--  are not specific to either the container or the contained widget, but
--  rather to their relation. Typical examples of child properties are the
--  position or pack-type of a widget which is contained in a Gtk.Box.Gtk_Box.
--
--  Use gtk_container_class_install_child_property to install child properties
--  for a container class and gtk_container_class_find_child_property or
--  gtk_container_class_list_child_properties to get information about existing
--  child properties.
--
--  To set the value of a child property, use
--  Gtk.Container.Child_Set_Property, gtk_container_child_set or
--  gtk_container_child_set_valist. To obtain the value of a child property,
--  use Gtk.Container.Child_Get_Property, gtk_container_child_get or
--  gtk_container_child_get_valist. To emit notification about child property
--  changes, use Gtk.Widget.Child_Notify.
--
--  # GtkContainer as GtkBuildable
--
--  The GtkContainer implementation of the GtkBuildable interface supports a
--  <packing> element for children, which can contain multiple <property>
--  elements that specify child properties for the child.
--
--  Since 2.16, child properties can also be marked as translatable using the
--  same "translatable", "comments" and "context" attributes that are used for
--  regular properties.
--
--  Since 3.16, containers can have a <focus-chain> element containing
--  multiple <widget> elements, one for each child that should be added to the
--  focus chain. The "name" attribute gives the id of the widget.
--
--  An example of these properties in UI definitions: |[ <object
--  class="GtkBox"> <child> <object class="GtkEntry" id="entry1"/> <packing>
--  <property name="pack-type">start</property> </packing> </child> <child>
--  <object class="GtkEntry" id="entry2"/> </child> <focus-chain> <widget
--  name="entry1"/> <widget name="entry2"/> </focus-chain> </object> ]|
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;           use Cairo;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Glib.Values;     use Glib.Values;
with Gtk.Adjustment;  use Gtk.Adjustment;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Container is

   type Gtk_Container_Record is new Gtk_Widget_Record with null record;
   type Gtk_Container is access all Gtk_Container_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Callback is access procedure
     (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  The type of the callback functions used for e.g. iterating over the
   --  children of a container, see gtk_container_foreach.
   --  "widget": the widget to operate on

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_container_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add
      (Container : not null access Gtk_Container_Record;
       Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds Widget to Container. Typically used for simple containers such as
   --  Gtk.Window.Gtk_Window, Gtk.Frame.Gtk_Frame, or Gtk.Button.Gtk_Button;
   --  for more complicated layout containers such as Gtk.Box.Gtk_Box or
   --  Gtk.Grid.Gtk_Grid, this function will pick default packing parameters
   --  that may not be correct. So consider functions such as
   --  Gtk.Box.Pack_Start and Gtk.Grid.Attach as an alternative to
   --  Gtk.Container.Add in those cases. A widget may be added to only one
   --  container at a time; you can't place the same widget inside two
   --  different containers.
   --  Note that some containers, such as
   --  Gtk.Scrolled_Window.Gtk_Scrolled_Window or Gtk.List_Box.Gtk_List_Box,
   --  may add intermediate children between the added widget and the
   --  container.
   --  "widget": a widget to be placed inside Container

   procedure Check_Resize (Container : not null access Gtk_Container_Record);

   procedure Child_Get_Property
      (Container     : not null access Gtk_Container_Record;
       Child         : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Property_Name : UTF8_String;
       Value         : in out Glib.Values.GValue);
   --  Gets the value of a child property for Child and Container.
   --  "child": a widget which is a child of Container
   --  "property_name": the name of the property to get
   --  "value": a location to return the value

   procedure Child_Set_Property
      (Container     : not null access Gtk_Container_Record;
       Child         : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Property_Name : UTF8_String;
       Value         : Glib.Values.GValue);
   --  Sets a child property for Child and Container.
   --  "child": a widget which is a child of Container
   --  "property_name": the name of the property to set
   --  "value": the value to set the property to

   procedure Child_Notify
      (Container      : not null access Gtk_Container_Record;
       Child          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Child_Property : UTF8_String);
   --  Emits a Gtk.Widget.Gtk_Widget::child-notify signal for the [child
   --  property][child-properties] Child_Property on the child.
   --  This is an analogue of g_object_notify for child properties.
   --  Also see Gtk.Widget.Child_Notify.
   --  Since: gtk+ 3.2
   --  "child": the child widget
   --  "child_property": the name of a child property installed on the class
   --  of Container

   procedure Child_Notify_By_Pspec
      (Container : not null access Gtk_Container_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Pspec     : in out Glib.Param_Spec);
   --  Emits a Gtk.Widget.Gtk_Widget::child-notify signal for the [child
   --  property][child-properties] specified by Pspec on the child.
   --  This is an analogue of g_object_notify_by_pspec for child properties.
   --  Since: gtk+ 3.18
   --  "child": the child widget
   --  "pspec": the Glib.Param_Spec of a child property instealled on the
   --  class of Container

   function Child_Type
      (Container : not null access Gtk_Container_Record) return GType;
   --  Returns the type of the children supported by the container.
   --  Note that this may return G_TYPE_NONE to indicate that no more children
   --  can be added, e.g. for a Gtk.Paned.Gtk_Paned which already has two
   --  children.

   procedure Forall
      (Container : not null access Gtk_Container_Record;
       Callback  : Gtk_Callback);
   --  Invokes Callback on each direct child of Container, including children
   --  that are considered "internal" (implementation details of the
   --  container). "Internal" children generally weren't added by the user of
   --  the container, but were added by the container implementation itself.
   --  Most applications should use Gtk.Container.Foreach, rather than
   --  Gtk.Container.Forall.
   --  "callback": a callback

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Forall_User_Data is

      type Gtk_Callback is access procedure
        (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
         Data   : User_Data_Type);
      --  The type of the callback functions used for e.g. iterating over the
      --  children of a container, see gtk_container_foreach.
      --  "widget": the widget to operate on
      --  "data": user-supplied data

      procedure Forall
         (Container     : not null access Gtk.Container.Gtk_Container_Record'Class;
          Callback      : Gtk_Callback;
          Callback_Data : User_Data_Type);
      --  Invokes Callback on each direct child of Container, including
      --  children that are considered "internal" (implementation details of
      --  the container). "Internal" children generally weren't added by the
      --  user of the container, but were added by the container implementation
      --  itself.
      --  Most applications should use Gtk.Container.Foreach, rather than
      --  Gtk.Container.Forall.
      --  "callback": a callback
      --  "callback_data": callback user data

   end Forall_User_Data;

   procedure Foreach
      (Container : not null access Gtk_Container_Record;
       Callback  : Gtk_Callback);
   --  Invokes Callback on each non-internal child of Container. See
   --  Gtk.Container.Forall for details on what constitutes an "internal"
   --  child. For all practical purposes, this function should iterate over
   --  precisely those child widgets that were added to the container by the
   --  application with explicit add calls.
   --  It is permissible to remove the child from the Callback handler.
   --  Most applications should use Gtk.Container.Foreach, rather than
   --  Gtk.Container.Forall.
   --  "callback": a callback

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Foreach_User_Data is

      type Gtk_Callback is access procedure
        (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
         Data   : User_Data_Type);
      --  The type of the callback functions used for e.g. iterating over the
      --  children of a container, see Gtk.Container.Foreach.
      --  "widget": the widget to operate on
      --  "data": user-supplied data

      procedure Foreach
         (Container     : not null access Gtk.Container.Gtk_Container_Record'Class;
          Callback      : Gtk_Callback;
          Callback_Data : User_Data_Type);
      --  Invokes Callback on each non-internal child of Container. See
      --  Gtk.Container.Forall for details on what constitutes an "internal"
      --  child. For all practical purposes, this function should iterate over
      --  precisely those child widgets that were added to the container by the
      --  application with explicit add calls.
      --  It is permissible to remove the child from the Callback handler.
      --  Most applications should use Gtk.Container.Foreach, rather than
      --  Gtk.Container.Forall.
      --  "callback": a callback
      --  "callback_data": callback user data

   end Foreach_User_Data;

   function Get_Border_Width
      (Container : not null access Gtk_Container_Record) return Guint;
   --  Retrieves the border width of the container. See
   --  Gtk.Container.Set_Border_Width.

   procedure Set_Border_Width
      (Container    : not null access Gtk_Container_Record;
       Border_Width : Guint);
   --  Sets the border width of the container.
   --  The border width of a container is the amount of space to leave around
   --  the outside of the container. The only exception to this is
   --  Gtk.Window.Gtk_Window; because toplevel windows can't leave space
   --  outside, they leave the space inside. The border is added on all sides
   --  of the container. To add space to only one side, use a specific
   --  Gtk.Widget.Gtk_Widget:margin property on the child widget, for example
   --  Gtk.Widget.Gtk_Widget:margin-top.
   --  "border_width": amount of blank space to leave outside the container.
   --  Valid values are in the range 0-65535 pixels.

   function Get_Children
      (Container : not null access Gtk_Container_Record)
       return Gtk.Widget.Widget_List.Glist;
   --  Returns the container's non-internal children. See Gtk.Container.Forall
   --  for details on what constitutes an "internal" child.

   function Get_Focus_Child
      (Container : not null access Gtk_Container_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the current focus child widget inside Container. This is not
   --  the currently focused widget. That can be obtained by calling
   --  Gtk.Window.Get_Focus.
   --  Since: gtk+ 2.14

   procedure Set_Focus_Child
      (Container : not null access Gtk_Container_Record;
       Child     : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets, or unsets if Child is null, the focused child of Container.
   --  This function emits the GtkContainer::set_focus_child signal of
   --  Container. Implementations of Gtk.Container.Gtk_Container can override
   --  the default behaviour by overriding the class closure of this signal.
   --  This is function is mostly meant to be used by widgets. Applications
   --  can use Gtk.Widget.Grab_Focus to manually set the focus to a specific
   --  widget.
   --  "child": a Gtk.Widget.Gtk_Widget, or null

   function Get_Focus_Hadjustment
      (Container : not null access Gtk_Container_Record)
       return Gtk.Adjustment.Gtk_Adjustment;
   --  Retrieves the horizontal focus adjustment for the container. See
   --  gtk_container_set_focus_hadjustment ().

   procedure Set_Focus_Hadjustment
      (Container  : not null access Gtk_Container_Record;
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Hooks up an adjustment to focus handling in a container, so when a
   --  child of the container is focused, the adjustment is scrolled to show
   --  that widget. This function sets the horizontal alignment. See
   --  Gtk.Scrolled_Window.Get_Hadjustment for a typical way of obtaining the
   --  adjustment and Gtk.Container.Set_Focus_Vadjustment for setting the
   --  vertical adjustment.
   --  The adjustments have to be in pixel units and in the same coordinate
   --  system as the allocation for immediate children of the container.
   --  "adjustment": an adjustment which should be adjusted when the focus is
   --  moved among the descendents of Container

   function Get_Focus_Vadjustment
      (Container : not null access Gtk_Container_Record)
       return Gtk.Adjustment.Gtk_Adjustment;
   --  Retrieves the vertical focus adjustment for the container. See
   --  Gtk.Container.Set_Focus_Vadjustment.

   procedure Set_Focus_Vadjustment
      (Container  : not null access Gtk_Container_Record;
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Hooks up an adjustment to focus handling in a container, so when a
   --  child of the container is focused, the adjustment is scrolled to show
   --  that widget. This function sets the vertical alignment. See
   --  Gtk.Scrolled_Window.Get_Vadjustment for a typical way of obtaining the
   --  adjustment and Gtk.Container.Set_Focus_Hadjustment for setting the
   --  horizontal adjustment.
   --  The adjustments have to be in pixel units and in the same coordinate
   --  system as the allocation for immediate children of the container.
   --  "adjustment": an adjustment which should be adjusted when the focus is
   --  moved among the descendents of Container

   function Get_Path_For_Child
      (Container : not null access Gtk_Container_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk.Widget.Gtk_Widget_Path;
   --  Returns a newly created widget path representing all the widget
   --  hierarchy from the toplevel down to and including Child.
   --  "child": a child of Container

   function Get_Resize_Mode
      (Container : not null access Gtk_Container_Record)
       return Gtk.Enums.Gtk_Resize_Mode;
   pragma Obsolescent (Get_Resize_Mode);
   --  Returns the resize mode for the container. See
   --  gtk_container_set_resize_mode ().
   --  Deprecated since 3.12, 1

   procedure Set_Resize_Mode
      (Container   : not null access Gtk_Container_Record;
       Resize_Mode : Gtk.Enums.Gtk_Resize_Mode);
   pragma Obsolescent (Set_Resize_Mode);
   --  Sets the resize mode for the container.
   --  The resize mode of a container determines whether a resize request will
   --  be passed to the container's parent, queued for later execution or
   --  executed immediately.
   --  Deprecated since 3.12, 1
   --  "resize_mode": the new resize mode

   procedure Propagate_Draw
      (Container : not null access Gtk_Container_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Cr        : Cairo.Cairo_Context);
   --  When a container receives a call to the draw function, it must send
   --  synthetic Gtk.Widget.Gtk_Widget::draw calls to all children that don't
   --  have their own Gdk_Windows. This function provides a convenient way of
   --  doing this. A container, when it receives a call to its
   --  Gtk.Widget.Gtk_Widget::draw function, calls Gtk.Container.Propagate_Draw
   --  once for each child, passing in the Cr the container received.
   --  Gtk.Container.Propagate_Draw takes care of translating the origin of
   --  Cr, and deciding whether the draw needs to be sent to the child. It is a
   --  convenient and optimized way of getting the same effect as calling
   --  Gtk.Widget.Draw on the child directly.
   --  In most cases, a container can simply either inherit the
   --  Gtk.Widget.Gtk_Widget::draw implementation from
   --  Gtk.Container.Gtk_Container, or do some drawing and then chain to the
   --  ::draw implementation from Gtk.Container.Gtk_Container.
   --  "child": a child of Container
   --  "cr": Cairo context as passed to the container. If you want to use Cr
   --  in container's draw function, consider using cairo_save and
   --  cairo_restore before calling this function.

   procedure Remove
      (Container : not null access Gtk_Container_Record;
       Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes Widget from Container. Widget must be inside Container. Note
   --  that Container will own a reference to Widget, and that this may be the
   --  last reference held; so removing a widget from its container can destroy
   --  that widget. If you want to use Widget again, you need to add a
   --  reference to it before removing it from a container, using g_object_ref.
   --  If you don't want to use Widget again it's usually more efficient to
   --  simply destroy it directly using Gtk.Widget.Destroy since this will
   --  remove it from the container and help break any circular reference count
   --  cycles.
   --  "widget": a current child of Container

   procedure Resize_Children
      (Container : not null access Gtk_Container_Record);
   pragma Obsolescent (Resize_Children);
   --  Deprecated since 3.10, 1

   procedure Set_Focus_Chain
      (Container         : not null access Gtk_Container_Record;
       Focusable_Widgets : Gtk.Widget.Widget_List.Glist);
   pragma Obsolescent (Set_Focus_Chain);
   --  Sets a focus chain, overriding the one computed automatically by GTK+.
   --  In principle each widget in the chain should be a descendant of the
   --  container, but this is not enforced by this method, since it's allowed
   --  to set the focus chain before you pack the widgets, or have a widget in
   --  the chain that isn't always packed. The necessary checks are done when
   --  the focus chain is actually traversed.
   --  Deprecated since 3.24, 1
   --  "focusable_widgets": the new focus chain

   procedure Set_Reallocate_Redraws
      (Container     : not null access Gtk_Container_Record;
       Needs_Redraws : Boolean);
   pragma Obsolescent (Set_Reallocate_Redraws);
   --  Sets the Reallocate_Redraws flag of the container to the given value.
   --  Containers requesting reallocation redraws get automatically redrawn if
   --  any of their children changed allocation.
   --  Deprecated since 3.14, 1
   --  "needs_redraws": the new value for the container's Reallocate_Redraws
   --  flag

   procedure Unset_Focus_Chain
      (Container : not null access Gtk_Container_Record);
   pragma Obsolescent (Unset_Focus_Chain);
   --  Removes a focus chain explicitly set with
   --  Gtk.Container.Set_Focus_Chain.
   --  Deprecated since 3.24, 1

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Border_Width_Property : constant Glib.Properties.Property_Uint;

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  Flags: write

   Resize_Mode_Property : constant Gtk.Enums.Property_Gtk_Resize_Mode;

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Container_Gtk_Widget_Void is not null access procedure
     (Self   : access Gtk_Container_Record'Class;
      Object : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   type Cb_GObject_Gtk_Widget_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Object : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   Signal_Add : constant Glib.Signal_Name := "add";
   procedure On_Add
      (Self  : not null access Gtk_Container_Record;
       Call  : Cb_Gtk_Container_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Add
      (Self  : not null access Gtk_Container_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   type Cb_Gtk_Container_Void is not null access procedure (Self : access Gtk_Container_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Check_Resize : constant Glib.Signal_Name := "check-resize";
   procedure On_Check_Resize
      (Self  : not null access Gtk_Container_Record;
       Call  : Cb_Gtk_Container_Void;
       After : Boolean := False);
   procedure On_Check_Resize
      (Self  : not null access Gtk_Container_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   Signal_Remove : constant Glib.Signal_Name := "remove";
   procedure On_Remove
      (Self  : not null access Gtk_Container_Record;
       Call  : Cb_Gtk_Container_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Remove
      (Self  : not null access Gtk_Container_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   Signal_Set_Focus_Child : constant Glib.Signal_Name := "set-focus-child";
   procedure On_Set_Focus_Child
      (Self  : not null access Gtk_Container_Record;
       Call  : Cb_Gtk_Container_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Set_Focus_Child
      (Self  : not null access Gtk_Container_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Container_Record, Gtk_Container);
   function "+"
     (Widget : access Gtk_Container_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Container
   renames Implements_Gtk_Buildable.To_Object;

private
   Resize_Mode_Property : constant Gtk.Enums.Property_Gtk_Resize_Mode :=
     Gtk.Enums.Build ("resize-mode");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
   Border_Width_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("border-width");
end Gtk.Container;
