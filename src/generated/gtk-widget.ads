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

--  The base class for all widgets.
--
--  It manages the widget lifecycle, layout, states and style.
--
--  ### Height-for-width Geometry Management
--
--  GTK uses a height-for-width (and width-for-height) geometry management
--  system. Height-for-width means that a widget can change how much vertical
--  space it needs, depending on the amount of horizontal space that it is
--  given (and similar for width-for-height). The most common example is a
--  label that reflows to fill up the available width, wraps to fewer lines,
--  and therefore needs less height.
--
--  Height-for-width geometry management is implemented in GTK by way of two
--  virtual methods:
--
--  - [vfuncGtk.Widget.get_request_mode] - [vfuncGtk.Widget.measure]
--
--  There are some important things to keep in mind when implementing
--  height-for-width and when using it in widget implementations.
--
--  If you implement a direct `GtkWidget` subclass that supports
--  height-for-width or width-for-height geometry management for itself or its
--  child widgets, the [vfuncGtk.Widget.get_request_mode] virtual function must
--  be implemented as well and return the widget's preferred request mode. The
--  default implementation of this virtual function returns
--  Gtk.Enums.Constant_Size, which means that the widget will only ever get -1
--  passed as the for_size value to its [vfuncGtk.Widget.measure]
--  implementation.
--
--  The geometry management system will query a widget hierarchy in only one
--  orientation at a time. When widgets are initially queried for their minimum
--  sizes it is generally done in two initial passes in the
--  [enumGtk.SizeRequestMode] chosen by the toplevel.
--
--  For example, when queried in the normal Gtk.Enums.Height_For_Width mode:
--
--  First, the default minimum and natural width for each widget in the
--  interface will be computed using [methodGtk.Widget.measure] with an
--  orientation of Gtk.Enums.Orientation_Horizontal and a for_size of -1.
--  Because the preferred widths for each widget depend on the preferred widths
--  of their children, this information propagates up the hierarchy, and
--  finally a minimum and natural width is determined for the entire toplevel.
--  Next, the toplevel will use the minimum width to query for the minimum
--  height contextual to that width using [methodGtk.Widget.measure] with an
--  orientation of Gtk.Enums.Orientation_Vertical and a for_size of the just
--  computed width. This will also be a highly recursive operation. The minimum
--  height for the minimum width is normally used to set the minimum size
--  constraint on the toplevel.
--
--  After the toplevel window has initially requested its size in both
--  dimensions it can go on to allocate itself a reasonable size (or a size
--  previously specified with [methodGtk.Window.set_default_size]). During the
--  recursive allocation process it's important to note that request cycles
--  will be recursively executed while widgets allocate their children. Each
--  widget, once allocated a size, will go on to first share the space in one
--  orientation among its children and then request each child's height for its
--  target allocated width or its width for allocated height, depending. In
--  this way a widget will typically be requested its size a number of times
--  before actually being allocated a size. The size a widget is finally
--  allocated can of course differ from the size it has requested. For this
--  reason, `GtkWidget` caches a small number of results to avoid re-querying
--  for the same sizes in one allocation cycle.
--
--  If a widget does move content around to intelligently use up the allocated
--  size then it must support the request in both `GtkSizeRequestMode`s even if
--  the widget in question only trades sizes in a single orientation.
--
--  For instance, a [classGtk.Label] that does height-for-width word wrapping
--  will not expect to have [vfuncGtk.Widget.measure] with an orientation of
--  Gtk.Enums.Orientation_Vertical called because that call is specific to a
--  width-for-height request. In this case the label must return the height
--  required for its own minimum possible width. By following this rule any
--  widget that handles height-for-width or width-for-height requests will
--  always be allocated at least enough space to fit its own content.
--
--  Here are some examples of how a Gtk.Enums.Height_For_Width widget
--  generally deals with width-for-height requests:
--
--  ```c static void foo_widget_measure (GtkWidget *widget, GtkOrientation
--  orientation, int for_size, int *minimum_size, int *natural_size, int
--  *minimum_baseline, int *natural_baseline) { if (orientation ==
--  GTK_ORIENTATION_HORIZONTAL) { // Calculate minimum and natural width } else
--  // VERTICAL { if (i_am_in_height_for_width_mode) { int min_width, dummy;
--
--  // First, get the minimum width of our widget GTK_WIDGET_GET_CLASS
--  (widget)->measure (widget, GTK_ORIENTATION_HORIZONTAL, -1, &min_width,
--  &dummy, &dummy, &dummy);
--
--  // Now use the minimum width to retrieve the minimum and natural height to
--  display // that width. GTK_WIDGET_GET_CLASS (widget)->measure (widget,
--  GTK_ORIENTATION_VERTICAL, min_width, minimum_size, natural_size, &dummy,
--  &dummy); } else { // ... some widgets do both. } } } ```
--
--  Often a widget needs to get its own request during size request or
--  allocation. For example, when computing height it may need to also compute
--  width. Or when deciding how to use an allocation, the widget may need to
--  know its natural size. In these cases, the widget should be careful to call
--  its virtual methods directly, like in the code example above.
--
--  It will not work to use the wrapper function [methodGtk.Widget.measure]
--  inside your own [vfuncGtk.Widget.size_allocate] implementation. These
--  return a request adjusted by [classGtk.SizeGroup], the widget's align and
--  expand flags, as well as its CSS style.
--
--  If a widget used the wrappers inside its virtual method implementations,
--  then the adjustments (such as widget margins) would be applied twice. GTK
--  therefore does not allow this and will warn if you try to do it.
--
--  Of course if you are getting the size request for another widget, such as
--  a child widget, you must use [methodGtk.Widget.measure]; otherwise, you
--  would not properly consider widget margins, [classGtk.SizeGroup], and so
--  forth.
--
--  GTK also supports baseline vertical alignment of widgets. This means that
--  widgets are positioned such that the typographical baseline of widgets in
--  the same row are aligned. This happens if a widget supports baselines, has
--  a vertical alignment using baselines, and is inside a widget that supports
--  baselines and has a natural "row" that it aligns to the baseline, or a
--  baseline assigned to it by the grandparent.
--
--  Baseline alignment support for a widget is also done by the
--  [vfuncGtk.Widget.measure] virtual function. It allows you to report both a
--  minimum and natural size.
--
--  If a widget ends up baseline aligned it will be allocated all the space in
--  the parent as if it was Gtk.Widget.Align_Fill, but the selected baseline
--  can be found via [methodGtk.Widget.get_baseline]. If the baseline has a
--  value other than -1 you need to align the widget such that the baseline
--  appears at the position.
--
--  ### GtkWidget as GtkBuildable
--
--  The `GtkWidget` implementation of the `GtkBuildable` interface supports
--  various custom elements to specify additional aspects of widgets that are
--  not directly expressed as properties.
--
--  If the widget uses a [classGtk.LayoutManager], `GtkWidget` supports a
--  custom `<layout>` element, used to define layout properties:
--
--  ```xml <object class="GtkGrid" id="my_grid"> <child> <object
--  class="GtkLabel" id="label1"> <property name="label">Description</property>
--  <layout> <property name="column">0</property> <property
--  name="row">0</property> <property name="row-span">1</property> <property
--  name="column-span">1</property> </layout> </object> </child> <child>
--  <object class="GtkEntry" id="description_entry"> <layout> <property
--  name="column">1</property> <property name="row">0</property> <property
--  name="row-span">1</property> <property name="column-span">1</property>
--  </layout> </object> </child> </object> ```
--
--  `GtkWidget` allows style information such as style classes to be
--  associated with widgets, using the custom `<style>` element:
--
--  ```xml <object class="GtkButton" id="button1"> <style> <class
--  name="my-special-button-class"/> <class name="dark-button"/> </style>
--  </object> ```
--
--  `GtkWidget` allows defining accessibility information, such as properties,
--  relations, and states, using the custom `<accessibility>` element:
--
--  ```xml <object class="GtkButton" id="button1"> <accessibility> <property
--  name="label">Download</property> <relation
--  name="labelled-by">label1</relation> </accessibility> </object> ```
--
--  ### Building composite widgets from template XML
--
--  `GtkWidget `exposes some facilities to automate the procedure of creating
--  composite widgets using "templates".
--
--  To create composite widgets with `GtkBuilder` XML, one must associate the
--  interface description with the widget class at class initialization time
--  using [methodGtk.WidgetClass.set_template].
--
--  The interface description semantics expected in composite template
--  descriptions is slightly different from regular [classGtk.Builder] XML.
--
--  Unlike regular interface descriptions,
--  [methodGtk.WidgetClass.set_template] will expect a `<template>` tag as a
--  direct child of the toplevel `<interface>` tag. The `<template>` tag must
--  specify the "class" attribute which must be the type name of the widget.
--  Optionally, the "parent" attribute may be specified to specify the direct
--  parent type of the widget type; this is ignored by `GtkBuilder` but can be
--  used by UI design tools to introspect what kind of properties and internal
--  children exist for a given type when the actual type does not exist.
--
--  The XML which is contained inside the `<template>` tag behaves as if it
--  were added to the `<object>` tag defining the widget itself. You may set
--  properties on a widget by inserting `<property>` tags into the `<template>`
--  tag, and also add `<child>` tags to add children and extend a widget in the
--  normal way you would with `<object>` tags.
--
--  Additionally, `<object>` tags can also be added before and after the
--  initial `<template>` tag in the normal way, allowing one to define
--  auxiliary objects which might be referenced by other widgets declared as
--  children of the `<template>` tag.
--
--  Since, unlike the `<object>` tag, the `<template>` tag does not contain an
--  "id" attribute, if you need to refer to the instance of the object itself
--  that the template will create, simply refer to the template class name in
--  an applicable element content.
--
--  Here is an example of a template definition, which includes an example of
--  this in the `<signal>` tag:
--
--  ```xml <interface> <template class="FooWidget" parent="GtkBox"> <property
--  name="orientation">horizontal</property> <property
--  name="spacing">4</property> <child> <object class="GtkButton"
--  id="hello_button"> <property name="label">Hello World</property> <signal
--  name="clicked" handler="hello_button_clicked" object="FooWidget"
--  swapped="yes"/> </object> </child> <child> <object class="GtkButton"
--  id="goodbye_button"> <property name="label">Goodbye World</property>
--  </object> </child> </template> </interface> ```
--
--  Typically, you'll place the template fragment into a file that is bundled
--  with your project, using `GResource`. In order to load the template, you
--  need to call [methodGtk.WidgetClass.set_template_from_resource] from the
--  class initialization of your `GtkWidget` type:
--
--  ```c static void foo_widget_class_init (FooWidgetClass *klass) { // ...
--
--  gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (klass),
--  "/com/example/ui/foowidget.ui"); } ```
--
--  You will also need to call [methodGtk.Widget.init_template] from the
--  instance initialization function:
--
--  ```c static void foo_widget_init (FooWidget *self) {
--  gtk_widget_init_template (GTK_WIDGET (self));
--
--  // Initialize the rest of the widget... } ```
--
--  as well as calling [methodGtk.Widget.dispose_template] from the dispose
--  function:
--
--  ```c static void foo_widget_dispose (GObject *gobject) { FooWidget *self =
--  FOO_WIDGET (gobject);
--
--  // Dispose objects for which you have a reference...
--
--  // Clear the template children for this widget type
--  gtk_widget_dispose_template (GTK_WIDGET (self), FOO_TYPE_WIDGET);
--
--  G_OBJECT_CLASS (foo_widget_parent_class)->dispose (gobject); } ```
--
--  You can access widgets defined in the template using the
--  [methodGtk.Widget.get_template_child] function, but you will typically
--  declare a pointer in the instance private data structure of your type using
--  the same name as the widget in the template definition, and call
--  [methodGtk.WidgetClass.bind_template_child_full] (or one of its wrapper
--  macros [funcGtk.widget_class_bind_template_child] and
--  [funcGtk.widget_class_bind_template_child_private]) with that name, e.g.
--
--  ```c typedef struct { GtkWidget *hello_button; GtkWidget *goodbye_button;
--  } FooWidgetPrivate;
--
--  G_DEFINE_TYPE_WITH_PRIVATE (FooWidget, foo_widget, GTK_TYPE_BOX)
--
--  static void foo_widget_dispose (GObject *gobject) {
--  gtk_widget_dispose_template (GTK_WIDGET (gobject), FOO_TYPE_WIDGET);
--
--  G_OBJECT_CLASS (foo_widget_parent_class)->dispose (gobject); }
--
--  static void foo_widget_class_init (FooWidgetClass *klass) { // ...
--  G_OBJECT_CLASS (klass)->dispose = foo_widget_dispose;
--
--  gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (klass),
--  "/com/example/ui/foowidget.ui");
--  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (klass),
--  FooWidget, hello_button); gtk_widget_class_bind_template_child_private
--  (GTK_WIDGET_CLASS (klass), FooWidget, goodbye_button); }
--
--  static void foo_widget_init (FooWidget *widget) { gtk_widget_init_template
--  (GTK_WIDGET (widget)); } ```
--
--  You can also use [methodGtk.WidgetClass.bind_template_callback_full] (or
--  is wrapper macro [funcGtk.widget_class_bind_template_callback]) to connect
--  a signal callback defined in the template with a function visible in the
--  scope of the class, e.g.
--
--  ```c // the signal handler has the instance and user data swapped //
--  because of the swapped="yes" attribute in the template XML static void
--  hello_button_clicked (FooWidget *self, GtkButton *button) { g_print
--  ("Hello, world!\n"); }
--
--  static void foo_widget_class_init (FooWidgetClass *klass) { // ...
--  gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (klass),
--  "/com/example/ui/foowidget.ui"); gtk_widget_class_bind_template_callback
--  (GTK_WIDGET_CLASS (klass), hello_button_clicked); } ```

pragma Warnings (Off, "*is already use-visible*");
with Cairo;                   use Cairo;
with GNAT.Strings;            use GNAT.Strings;
with Glib;                    use Glib;
with Glib.Action_Group;       use Glib.Action_Group;
with Glib.GSlist;             use Glib.GSlist;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Glist;              use Glib.Glist;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Variant;            use Glib.Variant;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Layout_Manager;      use Gtk.Layout_Manager;
with Pango.Context;           use Pango.Context;
with Pango.Font_Map;          use Pango.Font_Map;
with Pango.Layout;            use Pango.Layout;

package Gtk.Widget is

   type Gtk_Widget_Record is new GObject_Record with null record;
   type Gtk_Widget is access all Gtk_Widget_Record'Class;

   type Gtk_Align is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Align);
   --  Controls how a widget deals with extra space in a single dimension.
   --
   --  Alignment only matters if the widget receives a "too large" allocation,
   --  for example if you packed the widget with the
   --  [propertyGtk.Widget:hexpand] property inside a [classBox], then the
   --  widget might get extra space. If you have for example a 16x16 icon
   --  inside a 32x32 space, the icon could be scaled and stretched, it could
   --  be centered, or it could be positioned to one side of the space.
   --
   --  Note that in horizontal context `GTK_ALIGN_START` and `GTK_ALIGN_END`
   --  are interpreted relative to text direction.
   --
   --  Baseline support is optional for containers and widgets, and is only
   --  available for vertical alignment. `GTK_ALIGN_BASELINE_CENTER` and
   --  `GTK_ALIGN_BASELINE_FILL` are treated similar to `GTK_ALIGN_CENTER` and
   --  `GTK_ALIGN_FILL`, except that it positions the widget to line up the
   --  baselines, where that is supported.

   Align_Fill : constant Gtk_Align := 0;
   Align_Start : constant Gtk_Align := 1;
   Align_End : constant Gtk_Align := 2;
   Align_Center : constant Gtk_Align := 3;
   Align_Baseline_Fill : constant Gtk_Align := 4;
   Align_Baseline : constant Gtk_Align := 4;
   Align_Baseline_Center : constant Gtk_Align := 5;

   type Gtk_Requisition is record
      Width : Glib.Gint;
      Height : Glib.Gint;
   end record;
   pragma Convention (C, Gtk_Requisition);

   function From_Object_Free (B : access Gtk_Requisition) return Gtk_Requisition;
   pragma Inline (From_Object_Free);
   --  Represents the desired size of a widget.
   --
   --  See [GtkWidget's geometry management
   --  section](class.Widget.htmlheight-for-width-geometry-management) for more
   --  information.

   function Convert (R : Gtk.Widget.Gtk_Widget) return System.Address;
   function Convert (R : System.Address) return Gtk.Widget.Gtk_Widget;
   package Widget_List is new Generic_List (Gtk.Widget.Gtk_Widget);

   package Widget_SList is new Generic_SList (Gtk.Widget.Gtk_Widget);

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Align_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Align);
   type Property_Gtk_Align is new Gtk_Align_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_widget_get_type");

   -------------
   -- Methods --
   -------------

   procedure Action_Set_Enabled
      (Widget      : not null access Gtk_Widget_Record;
       Action_Name : UTF8_String;
       Enabled     : Boolean);
   --  Enables or disables an action installed with
   --  [methodGtk.WidgetClass.install_action].
   --  @param Action_Name action name, such as "clipboard.paste"
   --  @param Enabled whether the action is now enabled

   function Activate
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Activates the widget.
   --  The activation will emit the signal set using
   --  [methodGtk.WidgetClass.set_activate_signal] during class initialization.
   --  Activation is what happens when you press <kbd>Enter</kbd> on a widget.
   --  If you wish to handle the activation keybinding yourself, it is
   --  recommended to use [methodGtk.WidgetClass.add_shortcut] with an action
   --  created with [ctorGtk.SignalAction.new].
   --  If Widget is not activatable, the function returns false.
   --  @return true if the widget was activated

   function Activate_Action_Variant
      (Widget : not null access Gtk_Widget_Record;
       Name   : UTF8_String;
       Args   : Glib.Variant.Gvariant) return Boolean;
   --  Activates an action for the widget.
   --  The action is looked up in the action groups associated with Widget and
   --  its ancestors.
   --  If the action is in an action group added with
   --  [methodGtk.Widget.insert_action_group], the Name is expected to be
   --  prefixed with the prefix that was used when the group was inserted.
   --  The arguments must match the actions expected parameter type, as
   --  returned by [methodGio.Action.get_parameter_type].
   --  @param Name the name of the action to activate
   --  @param Args parameters to use
   --  @return true if the action was activated

   procedure Activate_Default (Widget : not null access Gtk_Widget_Record);
   --  Activates the `default.activate` action for the widget.
   --  The action is looked up in the same was as for
   --  [methodGtk.Widget.activate_action].

   procedure Add_Css_Class
      (Widget    : not null access Gtk_Widget_Record;
       Css_Class : UTF8_String);
   --  Adds a style class to the widget.
   --  After calling this function, the widget's style will match for
   --  Css_Class, according to CSS matching rules.
   --  Use [methodGtk.Widget.remove_css_class] to remove the style again.
   --  @param Css_Class style class to add to Widget, without the leading
   --  period

   procedure Add_Mnemonic_Label
      (Widget : not null access Gtk_Widget_Record;
       Label  : not null access Gtk_Widget_Record'Class);
   --  Adds a widget to the list of mnemonic labels for this widget.
   --  See [methodGtk.Widget.list_mnemonic_labels].
   --  Note that the list of mnemonic labels for the widget is cleared when
   --  the widget is destroyed, so the caller must make sure to update its
   --  internal state at this point as well.
   --  @param Label a widget that acts as a mnemonic label for Widget

   function Contains
      (Widget : not null access Gtk_Widget_Record;
       X      : Gdouble;
       Y      : Gdouble) return Boolean;
   --  Tests if a given point is contained in the widget.
   --  The coordinates for (x, y) must be in widget coordinates, so (0, 0) is
   --  assumed to be the top left of Widget's content area.
   --  @param X X coordinate to test, relative to Widget's origin
   --  @param Y Y coordinate to test, relative to Widget's origin
   --  @return true if Widget contains the point (x, y)

   function Create_Pango_Context
      (Widget : not null access Gtk_Widget_Record)
       return Pango.Context.Pango_Context;
   --  Creates a new `PangoContext` that is configured for the widget.
   --  The `PangoContext` will have the appropriate font map, font options,
   --  font description, and base direction set.
   --  See also [methodGtk.Widget.get_pango_context].
   --  @return the new `PangoContext`

   function Create_Pango_Layout
      (Widget : not null access Gtk_Widget_Record;
       Text   : UTF8_String := "") return Pango.Layout.Pango_Layout;
   --  Creates a new `PangoLayout` that is configured for the widget.
   --  The `PangoLayout` will have the appropriate font map, font description,
   --  and base direction set.
   --  If you keep a `PangoLayout` created in this way around, you need to
   --  re-create it when the widgets `PangoContext` is replaced. This can be
   --  tracked by listening to changes of the [propertyGtk.Widget:root]
   --  property on the widget.
   --  @param Text text to set on the layout
   --  @return the new `PangoLayout`

   procedure Dispose_Template
      (Widget      : not null access Gtk_Widget_Record;
       Widget_Type : GType);
   --  Clears the template children for the widget.
   --  This function is the opposite of [methodGtk.Widget.init_template], and
   --  it is used to clear all the template children from a widget instance. If
   --  you bound a template child to a field in the instance structure, or in
   --  the instance private data structure, the field will be set to `NULL`
   --  after this function returns.
   --  You should call this function inside the `GObjectClass.dispose`
   --  implementation of any widget that called
   --  [methodGtk.Widget.init_template]. Typically, you will want to call this
   --  function last, right before chaining up to the parent type's dispose
   --  implementation, e.g.
   --  ```c static void some_widget_dispose (GObject *gobject) { SomeWidget
   --  *self = SOME_WIDGET (gobject);
   --  // Clear the template data for SomeWidget gtk_widget_dispose_template
   --  (GTK_WIDGET (self), SOME_TYPE_WIDGET);
   --  G_OBJECT_CLASS (some_widget_parent_class)->dispose (gobject); } ```
   --  Since: gtk+ 4.8
   --  @param Widget_Type the type of the widget to finalize the template for

   function Drag_Check_Threshold
      (Widget    : not null access Gtk_Widget_Record;
       Start_X   : Glib.Gint;
       Start_Y   : Glib.Gint;
       Current_X : Glib.Gint;
       Current_Y : Glib.Gint) return Boolean;
   --  Checks to see if a drag movement has passed the GTK drag threshold.
   --  @param Start_X X coordinate of start of drag
   --  @param Start_Y Y coordinate of start of drag
   --  @param Current_X current X coordinate
   --  @param Current_Y current Y coordinate
   --  @return true if the drag threshold has been passed

   procedure Error_Bell (Widget : not null access Gtk_Widget_Record);
   --  Notifies the user about an input-related error on the widget.
   --  If the [propertyGtk.Settings:gtk-error-bell] setting is true, it calls
   --  [methodGdk.Surface.beep], otherwise it does nothing.
   --  Note that the effect of [methodGdk.Surface.beep] can be configured in
   --  many ways, depending on the windowing backend and the desktop
   --  environment or window manager that is used.

   function Get_Allocated_Baseline
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   pragma Obsolescent (Get_Allocated_Baseline);
   --  Returns the baseline that has currently been allocated to the widget.
   --  This function is intended to be used when implementing handlers for the
   --  `GtkWidget`Class.snapshot function, and when allocating child widgets in
   --  `GtkWidget`Class.size_allocate.
   --  Deprecated since 4.12, 1
   --  @return the baseline of the Widget, or -1 if none

   function Get_Allocated_Height
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   pragma Obsolescent (Get_Allocated_Height);
   --  Returns the height that has currently been allocated to the widget.
   --  To learn more about widget sizes, see the coordinate system
   --  [overview](coordinates.html).
   --  Deprecated since 4.12, 1
   --  @return the height of the Widget

   function Get_Allocated_Width
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   pragma Obsolescent (Get_Allocated_Width);
   --  Returns the width that has currently been allocated to the widget.
   --  To learn more about widget sizes, see the coordinate system
   --  [overview](coordinates.html).
   --  Deprecated since 4.12, 1
   --  @return the width of the Widget

   function Get_Ancestor
      (Widget      : not null access Gtk_Widget_Record;
       Widget_Type : GType) return Gtk_Widget;
   --  Gets the first ancestor of the widget with type Widget_Type.
   --  For example, `gtk_widget_get_ancestor (widget, GTK_TYPE_BOX)` gets the
   --  first `GtkBox` that's an ancestor of Widget. No reference will be added
   --  to the returned widget; it should not be unreferenced.
   --  Note that unlike [methodGtk.Widget.is_ancestor], this function
   --  considers Widget to be an ancestor of itself.
   --  @param Widget_Type ancestor type
   --  @return the ancestor widget

   function Get_Baseline
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   --  Returns the baseline that has currently been allocated to the widget.
   --  This function is intended to be used when implementing handlers for the
   --  `GtkWidgetClass.snapshot` function, and when allocating child widgets in
   --  `GtkWidgetClass.size_allocate`.
   --  Since: gtk+ 4.12
   --  @return the baseline of the Widget, or -1 if none

   function Get_Can_Focus
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether the input focus can enter the widget or any of its
   --  children.
   --  See [methodGtk.Widget.set_can_focus].
   --  @return true if the input focus can enter Widget

   procedure Set_Can_Focus
      (Widget    : not null access Gtk_Widget_Record;
       Can_Focus : Boolean);
   --  Sets whether the input focus can enter the widget or any of its
   --  children.
   --  Applications should set Can_Focus to false to mark a widget as for
   --  pointer/touch use only.
   --  Note that having Can_Focus be true is only one of the necessary
   --  conditions for being focusable. A widget must also be sensitive and
   --  focusable and not have an ancestor that is marked as not can-focus in
   --  order to receive input focus.
   --  See [methodGtk.Widget.grab_focus] for actually setting the input focus
   --  on a widget.
   --  @param Can_Focus whether the input focus can enter the widget or any of
   --  its children

   function Get_Can_Target
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Queries whether the widget can be the target of pointer events.
   --  @return true if Widget can receive pointer events

   procedure Set_Can_Target
      (Widget     : not null access Gtk_Widget_Record;
       Can_Target : Boolean);
   --  Sets whether the widget can be the target of pointer events.
   --  @param Can_Target whether this widget should be able to receive pointer
   --  events

   function Get_Child_Visible
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Gets the value set with [methodGtk.Widget.set_child_visible].
   --  If you feel a need to use this function, your code probably needs
   --  reorganization.
   --  This function is only useful for widget implementations and should
   --  never be called by an application.
   --  @return true if the widget is mapped with the parent

   procedure Set_Child_Visible
      (Widget        : not null access Gtk_Widget_Record;
       Child_Visible : Boolean);
   --  Sets whether the widget should be mapped along with its parent.
   --  The child visibility can be set for widget before it is added to a
   --  container with [methodGtk.Widget.set_parent], to avoid mapping children
   --  unnecessary before immediately unmapping them. However it will be reset
   --  to its default state of true when the widget is removed from a
   --  container.
   --  Note that changing the child visibility of a widget does not queue a
   --  resize on the widget. Most of the time, the size of a widget is computed
   --  from all visible children, whether or not they are mapped. If this is
   --  not the case, the container can queue a resize itself.
   --  This function is only useful for widget implementations and should
   --  never be called by an application.
   --  @param Child_Visible whether Widget should be mapped along with its
   --  parent

   function Get_Css_Classes
      (Widget : not null access Gtk_Widget_Record)
       return GNAT.Strings.String_List;
   --  Returns the list of style classes applied to the widget.
   --  @return a `NULL`-terminated list of css classes currently applied to
   --  Widget

   procedure Set_Css_Classes
      (Widget  : not null access Gtk_Widget_Record;
       Classes : GNAT.Strings.String_List);
   --  Replaces the current style classes of the widget with Classes.
   --  @param Classes `NULL`-terminated list of style classes

   function Get_Css_Name
      (Widget : not null access Gtk_Widget_Record) return UTF8_String;
   --  Returns the CSS name of the widget.
   --  @return the CSS name

   function Get_First_Child
      (Widget : not null access Gtk_Widget_Record) return Gtk_Widget;
   --  Returns the widget's first child.
   --  This function is primarily meant for widget implementations.
   --  @return the widget's first child

   function Get_Focus_Child
      (Widget : not null access Gtk_Widget_Record) return Gtk_Widget;
   --  Returns the focus child of the widget.
   --  @return the current focus child of Widget

   procedure Set_Focus_Child
      (Widget : not null access Gtk_Widget_Record;
       Child  : access Gtk_Widget_Record'Class);
   --  Set the focus child of the widget.
   --  This function is only suitable for widget implementations. If you want
   --  a certain widget to get the input focus, call
   --  [methodGtk.Widget.grab_focus] on it.
   --  @param Child a direct child widget of Widget or `NULL` to unset the
   --  focus child

   function Get_Focus_On_Click
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Returns whether the widget should grab focus when it is clicked with
   --  the mouse.
   --  See [methodGtk.Widget.set_focus_on_click].
   --  @return true if the widget should grab focus when it is clicked with
   --  the mouse

   procedure Set_Focus_On_Click
      (Widget         : not null access Gtk_Widget_Record;
       Focus_On_Click : Boolean);
   --  Sets whether the widget should grab focus when it is clicked with the
   --  mouse.
   --  Making mouse clicks not grab focus is useful in places like toolbars
   --  where you don't want the keyboard focus removed from the main area of
   --  the application.
   --  @param Focus_On_Click whether the widget should grab focus when clicked
   --  with the mouse

   function Get_Focusable
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether the widget can own the input focus.
   --  See [methodGtk.Widget.set_focusable].
   --  @return true if Widget can own the input focus

   procedure Set_Focusable
      (Widget    : not null access Gtk_Widget_Record;
       Focusable : Boolean);
   --  Sets whether the widget can own the input focus.
   --  Widget implementations should set Focusable to true in their init
   --  function if they want to receive keyboard input.
   --  Note that having Focusable be true is only one of the necessary
   --  conditions for being focusable. A widget must also be sensitive and
   --  can-focus and not have an ancestor that is marked as not can-focus in
   --  order to receive input focus.
   --  See [methodGtk.Widget.grab_focus] for actually setting the input focus
   --  on a widget.
   --  @param Focusable whether or not Widget can own the input focus

   function Get_Font_Map
      (Widget : not null access Gtk_Widget_Record)
       return Pango.Font_Map.Pango_Font_Map;
   --  Gets the font map of the widget.
   --  See [methodGtk.Widget.set_font_map].
   --  @return the font map of Widget

   procedure Set_Font_Map
      (Widget   : not null access Gtk_Widget_Record;
       Font_Map : access Pango.Font_Map.Pango_Font_Map_Record'Class);
   --  Sets the font map to use for text rendering in the widget.
   --  The font map is the object that is used to look up fonts. Setting a
   --  custom font map can be useful in special situations, e.g. when you need
   --  to add application-specific fonts to the set of available fonts.
   --  When not set, the widget will inherit the font map from its parent.
   --  @param Font_Map a `PangoFontMap`

   function Get_Font_Options
      (Widget : not null access Gtk_Widget_Record)
       return Cairo.Cairo_Font_Options;
   pragma Obsolescent (Get_Font_Options);
   --  Returns the `cairo_font_options_t` of the widget.
   --  Seee [methodGtk.Widget.set_font_options].
   --  Deprecated since 4.16, 1
   --  @return the `cairo_font_options_t` of widget

   procedure Set_Font_Options
      (Widget  : not null access Gtk_Widget_Record;
       Options : in out Cairo.Cairo_Font_Options);
   pragma Obsolescent (Set_Font_Options);
   --  Sets the `cairo_font_options_t` used for text rendering in the widget.
   --  When not set, the default font options for the `GdkDisplay` will be
   --  used.
   --  Deprecated since 4.16, 1
   --  @param Options a `cairo_font_options_t` struct to unset any previously
   --  set default font options

   function Get_Halign
      (Widget : not null access Gtk_Widget_Record) return Gtk_Align;
   --  Gets the horizontal alignment of the widget.
   --  For backwards compatibility reasons this method will never return one
   --  of the baseline alignments, but instead it will convert it to
   --  [enumGtk.Align.fill] or [enumGtk.Align.center].
   --  Baselines are not supported for horizontal alignment.
   --  @return the horizontal alignment of Widget

   procedure Set_Halign
      (Widget : not null access Gtk_Widget_Record;
       Align  : Gtk_Align);
   --  Sets the horizontal alignment of the widget.
   --  @param Align the horizontal alignment

   function Get_Has_Tooltip
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Returns the current value of the `has-tooltip` property.
   --  @return current value of `has-tooltip` on Widget

   procedure Set_Has_Tooltip
      (Widget      : not null access Gtk_Widget_Record;
       Has_Tooltip : Boolean);
   --  Sets the `has-tooltip` property on the widget.
   --  @param Has_Tooltip whether or not Widget has a tooltip

   function Get_Height
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   --  Returns the content height of the widget.
   --  This function returns the height passed to its size-allocate
   --  implementation, which is the height you should be using in
   --  [vfuncGtk.Widget.snapshot].
   --  For pointer events, see [methodGtk.Widget.contains].
   --  To learn more about widget sizes, see the coordinate system
   --  [overview](coordinates.html).
   --  @return The height of Widget

   function Get_Hexpand
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Gets whether the widget would like any available extra horizontal
   --  space.
   --  When a user resizes a window, widgets with expand set to true generally
   --  receive the extra space. For example, a list or scrollable area or
   --  document in your window would often be set to expand.
   --  Widgets with children should use [methodGtk.Widget.compute_expand]
   --  rather than this function, to see whether any of its children, has the
   --  expand flag set. If any child of a widget wants to expand, the parent
   --  may ask to expand also.
   --  This function only looks at the widget's own hexpand flag, rather than
   --  computing whether the entire widget tree rooted at this widget wants to
   --  expand.
   --  @return whether hexpand flag is set

   procedure Set_Hexpand
      (Widget : not null access Gtk_Widget_Record;
       Expand : Boolean);
   --  Sets whether the widget would like any available extra horizontal
   --  space.
   --  When a user resizes a window, widgets with expand set to true generally
   --  receive the extra space. For example, a list or scrollable area or
   --  document in your window would often be set to expand.
   --  Call this function to set the expand flag if you would like your widget
   --  to become larger horizontally when the window has extra room.
   --  By default, widgets automatically expand if any of their children want
   --  to expand. (To see if a widget will automatically expand given its
   --  current children and state, call [methodGtk.Widget.compute_expand]. A
   --  widget can decide how the expandability of children affects its own
   --  expansion by overriding the `compute_expand` virtual method on
   --  `GtkWidget`.).
   --  Setting hexpand explicitly with this function will override the
   --  automatic expand behavior.
   --  This function forces the widget to expand or not to expand, regardless
   --  of children. The override occurs because [methodGtk.Widget.set_hexpand]
   --  sets the hexpand-set property (see [methodGtk.Widget.set_hexpand_set])
   --  which causes the widget's hexpand value to be used, rather than looking
   --  at children and widget state.
   --  @param Expand whether to expand

   function Get_Hexpand_Set
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Gets whether the `hexpand` flag has been explicitly set.
   --  If [propertyGtk.Widget:hexpand] property is set, then it overrides any
   --  computed expand value based on child widgets. If `hexpand` is not set,
   --  then the expand value depends on whether any children of the widget
   --  would like to expand.
   --  There are few reasons to use this function, but it's here for
   --  completeness and consistency.
   --  @return whether hexpand has been explicitly set

   procedure Set_Hexpand_Set
      (Widget : not null access Gtk_Widget_Record;
       Set    : Boolean);
   --  Sets whether the hexpand flag will be used.
   --  The [propertyGtk.Widget:hexpand-set] property will be set automatically
   --  when you call [methodGtk.Widget.set_hexpand] to set hexpand, so the most
   --  likely reason to use this function would be to unset an explicit expand
   --  flag.
   --  If hexpand is set, then it overrides any computed expand value based on
   --  child widgets. If hexpand is not set, then the expand value depends on
   --  whether any children of the widget would like to expand.
   --  There are few reasons to use this function, but it's here for
   --  completeness and consistency.
   --  @param Set value for hexpand-set property

   function Get_Last_Child
      (Widget : not null access Gtk_Widget_Record) return Gtk_Widget;
   --  Returns the widget's last child.
   --  This function is primarily meant for widget implementations.
   --  @return the widget's last child

   function Get_Layout_Manager
      (Widget : not null access Gtk_Widget_Record)
       return Gtk.Layout_Manager.Gtk_Layout_Manager;
   --  Retrieves the layout manager of the widget.
   --  See [methodGtk.Widget.set_layout_manager].
   --  @return the layout manager of Widget

   procedure Set_Layout_Manager
      (Widget         : not null access Gtk_Widget_Record;
       Layout_Manager : access Gtk.Layout_Manager.Gtk_Layout_Manager_Record'Class);
   --  Sets the layout manager to use for measuring and allocating children of
   --  the widget.
   --  @param Layout_Manager a layout manager

   function Get_Limit_Events
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Gets the value of the [propertyGtk.Widget:limit-events] property.
   --  Since: gtk+ 4.18

   procedure Set_Limit_Events
      (Widget       : not null access Gtk_Widget_Record;
       Limit_Events : Boolean);
   --  Sets whether the widget acts like a modal dialog, with respect to event
   --  delivery.
   --  Since: gtk+ 4.18
   --  @param Limit_Events whether to limit events

   function Get_Mapped
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Returns whether the widget is mapped.
   --  @return true if the widget is mapped

   function Get_Margin_Bottom
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   --  Gets the bottom margin of the widget.
   --  @return The bottom margin of Widget

   procedure Set_Margin_Bottom
      (Widget : not null access Gtk_Widget_Record;
       Margin : Glib.Gint);
   --  Sets the bottom margin of the widget.
   --  @param Margin the bottom margin

   function Get_Margin_End
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   --  Gets the end margin of the widget.
   --  @return The end margin of Widget

   procedure Set_Margin_End
      (Widget : not null access Gtk_Widget_Record;
       Margin : Glib.Gint);
   --  Sets the end margin of the widget.
   --  @param Margin the end margin

   function Get_Margin_Start
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   --  Gets the start margin of the widget.
   --  @return The start margin of Widget

   procedure Set_Margin_Start
      (Widget : not null access Gtk_Widget_Record;
       Margin : Glib.Gint);
   --  Sets the start margin of the widget.
   --  @param Margin the start margin

   function Get_Margin_Top
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   --  Gets the top margin of the widget.
   --  @return The top margin of Widget

   procedure Set_Margin_Top
      (Widget : not null access Gtk_Widget_Record;
       Margin : Glib.Gint);
   --  Sets the top margin of the widget.
   --  @param Margin the top margin

   function Get_Name
      (Widget : not null access Gtk_Widget_Record) return UTF8_String;
   --  Retrieves the name of a widget.
   --  See [methodGtk.Widget.set_name] for the significance of widget names.
   --  @return name of the widget

   procedure Set_Name
      (Widget : not null access Gtk_Widget_Record;
       Name   : UTF8_String);
   --  Sets a widgets name.
   --  Setting a name allows you to refer to the widget from a CSS file. You
   --  can apply a style to widgets with a particular name in the CSS file. See
   --  the documentation for the CSS syntax (on the same page as the docs for
   --  [classGtk.StyleContext].
   --  Note that the CSS syntax has certain special characters to delimit and
   --  represent elements in a selector (period, #, >, *...), so using these
   --  will make your widget impossible to match by name. Any combination of
   --  alphanumeric symbols, dashes and underscores will suffice.
   --  @param Name name for the widget

   function Get_Next_Sibling
      (Widget : not null access Gtk_Widget_Record) return Gtk_Widget;
   --  Returns the widget's next sibling.
   --  This function is primarily meant for widget implementations.
   --  @return the widget's next sibling

   function Get_Opacity
      (Widget : not null access Gtk_Widget_Record) return Gdouble;
   --  Fetches the requested opacity for the widget.
   --  See [methodGtk.Widget.set_opacity].
   --  @return the requested opacity for this widget

   procedure Set_Opacity
      (Widget  : not null access Gtk_Widget_Record;
       Opacity : Gdouble);
   --  Requests the widget to be rendered partially transparent.
   --  An opacity of 0 is fully transparent and an opacity of 1 is fully
   --  opaque.
   --  Opacity works on both toplevel widgets and child widgets, although
   --  there are some limitations: For toplevel widgets, applying opacity
   --  depends on the capabilities of the windowing system. On X11, this has
   --  any effect only on X displays with a compositing manager, see
   --  [methodGdk.Display.is_composited]. On Windows and Wayland it will always
   --  work, although setting a window's opacity after the window has been
   --  shown may cause some flicker.
   --  Note that the opacity is inherited through inclusion — if you set a
   --  toplevel to be partially translucent, all of its content will appear
   --  translucent, since it is ultimatively rendered on that toplevel. The
   --  opacity value itself is not inherited by child widgets (since that would
   --  make widgets deeper in the hierarchy progressively more translucent). As
   --  a consequence, [classGtk.Popover] instances and other [ifaceGtk.Native]
   --  widgets with their own surface will use their own opacity value, and
   --  thus by default appear non-translucent, even if they are attached to a
   --  toplevel that is translucent.
   --  @param Opacity desired opacity, between 0 and 1

   function Get_Pango_Context
      (Widget : not null access Gtk_Widget_Record)
       return Pango.Context.Pango_Context;
   --  Gets a `PangoContext` that is configured for the widget.
   --  The `PangoContext` will have the appropriate font map, font
   --  description, and base direction set.
   --  Unlike the context returned by [methodGtk.Widget.create_pango_context],
   --  this context is owned by the widget (it can be used until the screen for
   --  the widget changes or the widget is removed from its toplevel), and will
   --  be updated to match any changes to the widget's attributes. This can be
   --  tracked by listening to changes of the [propertyGtk.Widget:root]
   --  property on the widget.
   --  @return the `PangoContext` for the widget

   function Get_Parent
      (Widget : not null access Gtk_Widget_Record) return Gtk_Widget;
   --  Returns the parent widget of the widget.
   --  @return the parent widget of Widget

   procedure Set_Parent
      (Widget : not null access Gtk_Widget_Record;
       Parent : not null access Gtk_Widget_Record'Class);
   --  Sets the parent widget of the widget.
   --  This takes care of details such as updating the state and style of the
   --  child to reflect its new location and resizing the parent. The opposite
   --  function is [methodGtk.Widget.unparent].
   --  This function is useful only when implementing subclasses of
   --  `GtkWidget`.
   --  @param Parent parent widget

   procedure Get_Preferred_Size
      (Widget       : not null access Gtk_Widget_Record;
       Minimum_Size : out Gtk_Requisition;
       Natural_Size : out Gtk_Requisition);
   --  Retrieves the minimum and natural size of a widget, taking into account
   --  the widget's preference for height-for-width management.
   --  This is used to retrieve a suitable size by container widgets which do
   --  not impose any restrictions on the child placement. It can be used to
   --  deduce toplevel window and menu sizes as well as child widgets in
   --  free-form containers such as `GtkFixed`.
   --  Handle with care. Note that the natural height of a height-for-width
   --  widget will generally be a smaller size than the minimum height, since
   --  the required height for the natural width is generally smaller than the
   --  required height for the minimum width.
   --  Use [methodGtk.Widget.measure] if you want to support baseline
   --  alignment.
   --  @param Minimum_Size location for storing the minimum size
   --  @param Natural_Size location for storing the natural size

   function Get_Prev_Sibling
      (Widget : not null access Gtk_Widget_Record) return Gtk_Widget;
   --  Returns the widget's previous sibling.
   --  This function is primarily meant for widget implementations.
   --  @return the widget's previous sibling

   function Get_Realized
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether the widget is realized.
   --  @return true if Widget is realized

   function Get_Receives_Default
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether the widget is always treated as the default widget
   --  within its toplevel when it has the focus, even if another widget is the
   --  default.
   --  See [methodGtk.Widget.set_receives_default].
   --  @return true if Widget acts as the default widget when focused

   procedure Set_Receives_Default
      (Widget           : not null access Gtk_Widget_Record;
       Receives_Default : Boolean);
   --  Sets whether the widget will be treated as the default widget within
   --  its toplevel when it has the focus, even if another widget is the
   --  default.
   --  @param Receives_Default whether or not Widget can be a default widget

   function Get_Scale_Factor
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   --  Retrieves the internal scale factor that maps from window coordinates
   --  to the actual device pixels.
   --  On traditional systems this is 1, on high density outputs, it can be a
   --  higher value (typically 2).
   --  See [methodGdk.Surface.get_scale_factor].
   --  Note that modern systems may support *fractional* scaling, where the
   --  scale factor is not an integer. On such systems, this function will
   --  return the next higher integer value, but you probably want to use
   --  [methodGdk.Surface.get_scale] to get the fractional scale value.
   --  @return the scale factor for Widget

   function Get_Sensitive
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Returns the widget's sensitivity.
   --  This function returns the value that has been set using
   --  [methodGtk.Widget.set_sensitive]).
   --  The effective sensitivity of a widget is however determined by both its
   --  own and its parent widget's sensitivity. See
   --  [methodGtk.Widget.is_sensitive].
   --  @return true if the widget is sensitive

   procedure Set_Sensitive
      (Widget    : not null access Gtk_Widget_Record;
       Sensitive : Boolean := True);
   --  Sets the sensitivity of the widget.
   --  A widget is sensitive if the user can interact with it. Insensitive
   --  widgets are "grayed out" and the user can't interact with them.
   --  Insensitive widgets are known as "inactive", "disabled", or "ghosted" in
   --  some other toolkits.
   --  @param Sensitive true to make the widget sensitive

   procedure Get_Size_Request
      (Widget : not null access Gtk_Widget_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint);
   --  Gets the size request that was explicitly set for the widget.
   --  A value of -1 stored in Width or Height indicates that that dimension
   --  has not been set explicitly and the natural requisition of the widget
   --  will be used instead.
   --  See [methodGtk.Widget.set_size_request].
   --  To get the size a widget will actually request, call
   --  [methodGtk.Widget.measure] instead of this function.
   --  @param Width return location for width
   --  @param Height return location for height

   procedure Set_Size_Request
      (Widget : not null access Gtk_Widget_Record;
       Width  : Glib.Gint := -1;
       Height : Glib.Gint := -1);
   --  Sets the minimum size of the widget.
   --  That is, the widget's size request will be at least Width by Height.
   --  You can use this function to force a widget to be larger than it
   --  normally would be.
   --  In most cases, [methodGtk.Window.set_default_size] is a better choice
   --  for toplevel windows than this function; setting the default size will
   --  still allow users to shrink the window. Setting the size request will
   --  force them to leave the window at least as large as the size request.
   --  Note the inherent danger of setting any fixed size - themes,
   --  translations into other languages, different fonts, and user action can
   --  all change the appropriate size for a given widget. So, it is basically
   --  impossible to hardcode a size that will always work.
   --  The size request of a widget is the smallest size a widget can accept
   --  while still functioning well and drawing itself correctly. However in
   --  some strange cases a widget may be allocated less than its requested
   --  size, and in many cases a widget may be allocated more space than it
   --  requested.
   --  If the size request in a given direction is -1 (unset), then the
   --  "natural" size request of the widget will be used instead.
   --  The size request set here does not include any margin from the
   --  properties [propertyGtk.Widget:margin-start],
   --  [propertyGtk.Widget:margin-end], [propertyGtk.Widget:margin-top], and
   --  [propertyGtk.Widget:margin-bottom], but it does include pretty much all
   --  other padding or border properties set by any subclass of `GtkWidget`.
   --  @param Width width Widget should request, or -1 to unset
   --  @param Height height Widget should request, or -1 to unset

   function Get_Template_Child
      (Widget      : not null access Gtk_Widget_Record;
       Widget_Type : GType;
       Name        : UTF8_String) return Glib.Object.GObject;
   --  Fetches an object build from the template XML for Widget_Type in the
   --  widget.
   --  This will only report children which were previously declared with
   --  [methodGtk.WidgetClass.bind_template_child_full] or one of its variants.
   --  This function is only meant to be called for code which is private to
   --  the Widget_Type which declared the child and is meant for language
   --  bindings which cannot easily make use of the GObject structure offsets.
   --  @param Widget_Type The type of the widget class that defines the child
   --  in the template
   --  @param Name ID of the child defined in the template XML
   --  @return the object built in the template XML with the id Name

   function Get_Tooltip_Markup
      (Widget : not null access Gtk_Widget_Record) return UTF8_String;
   --  Gets the contents of the tooltip for the widget.
   --  If the tooltip has not been set using
   --  [methodGtk.Widget.set_tooltip_markup], this function returns `NULL`.
   --  @return the tooltip text

   procedure Set_Tooltip_Markup
      (Widget : not null access Gtk_Widget_Record;
       Markup : UTF8_String := "");
   --  Sets the contents of the tooltip for widget.
   --  Markup must contain Pango markup.
   --  This function will take care of setting the
   --  [propertyGtk.Widget:has-tooltip] as a side effect, and of the default
   --  handler for the [signalGtk.Widget::query-tooltip] signal.
   --  See also [methodGtk.Tooltip.set_markup].
   --  @param Markup the contents of the tooltip for Widget

   function Get_Tooltip_Text
      (Widget : not null access Gtk_Widget_Record) return UTF8_String;
   --  Gets the contents of the tooltip for the widget.
   --  If the Widget's tooltip was set using
   --  [methodGtk.Widget.set_tooltip_markup], this function will return the
   --  escaped text.
   --  @return the tooltip text

   procedure Set_Tooltip_Text
      (Widget : not null access Gtk_Widget_Record;
       Text   : UTF8_String := "");
   --  Sets the contents of the tooltip for the widget.
   --  If Text contains any markup, it will be escaped.
   --  This function will take care of setting
   --  [propertyGtk.Widget:has-tooltip] as a side effect, and of the default
   --  handler for the [signalGtk.Widget::query-tooltip] signal.
   --  See also [methodGtk.Tooltip.set_text].
   --  @param Text the contents of the tooltip for Widget

   function Get_Valign
      (Widget : not null access Gtk_Widget_Record) return Gtk_Align;
   --  Gets the vertical alignment of the widget.
   --  @return the vertical alignment of Widget

   procedure Set_Valign
      (Widget : not null access Gtk_Widget_Record;
       Align  : Gtk_Align);
   --  Sets the vertical alignment of the widget.
   --  @param Align the vertical alignment

   function Get_Vexpand
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Gets whether the widget would like any available extra vertical space.
   --  See [methodGtk.Widget.get_hexpand] for more detail.
   --  @return whether vexpand flag is set

   procedure Set_Vexpand
      (Widget : not null access Gtk_Widget_Record;
       Expand : Boolean);
   --  Sets whether the widget would like any available extra vertical space.
   --  See [methodGtk.Widget.set_hexpand] for more detail.
   --  @param Expand whether to expand

   function Get_Vexpand_Set
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Gets whether the `vexpand` flag has been explicitly set.
   --  See [methodGtk.Widget.get_hexpand_set] for more detail.
   --  @return whether vexpand has been explicitly set

   procedure Set_Vexpand_Set
      (Widget : not null access Gtk_Widget_Record;
       Set    : Boolean);
   --  Sets whether the vexpand flag will be used.
   --  See [methodGtk.Widget.set_hexpand_set] for more detail.
   --  @param Set value for vexpand-set property

   function Get_Visible
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether the widget is visible.
   --  If you want to take into account whether the widget's parent is also
   --  marked as visible, use [methodGtk.Widget.is_visible] instead.
   --  This function does not check if the widget is obscured in any way.
   --  See [methodGtk.Widget.set_visible].
   --  @return true if the widget is visible

   procedure Set_Visible
      (Widget  : not null access Gtk_Widget_Record;
       Visible : Boolean);
   --  Sets the visibility state of Widget.
   --  Note that setting this to true doesn't mean the widget is actually
   --  viewable, see [methodGtk.Widget.get_visible].
   --  @param Visible whether the widget should be shown or not

   function Get_Width
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   --  Returns the content width of the widget.
   --  This function returns the width passed to its size-allocate
   --  implementation, which is the width you should be using in
   --  [vfuncGtk.Widget.snapshot].
   --  For pointer events, see [methodGtk.Widget.contains].
   --  To learn more about widget sizes, see the coordinate system
   --  [overview](coordinates.html).
   --  @return The width of Widget

   function Grab_Focus
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Causes Widget to have the keyboard focus for the window that it belongs
   --  to.
   --  If Widget is not focusable, or its [vfuncGtk.Widget.grab_focus]
   --  implementation cannot transfer the focus to a descendant of Widget that
   --  is focusable, it will not take focus and false will be returned.
   --  Calling [methodGtk.Widget.grab_focus] on an already focused widget is
   --  allowed, should not have an effect, and return true.
   --  @return true if focus is now inside Widget

   function Has_Css_Class
      (Widget    : not null access Gtk_Widget_Record;
       Css_Class : UTF8_String) return Boolean;
   --  Returns whether a style class is currently applied to the widget.
   --  @param Css_Class style class, without the leading period
   --  @return true if Css_Class is currently applied to Widget

   function Has_Default
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether the widget is the current default widget within its
   --  toplevel.
   --  @return true if Widget is the current default widget within its
   --  toplevel

   function Has_Focus
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines if the widget has the global input focus.
   --  See [methodGtk.Widget.is_focus] for the difference between having the
   --  global input focus, and only having the focus within a toplevel.
   --  @return true if the widget has the global input focus

   function Has_Visible_Focus
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines if the widget should show a visible indication that it has
   --  the global input focus.
   --  This is a convenience function that takes into account whether focus
   --  indication should currently be shown in the toplevel window of Widget.
   --  See [methodGtk.Window.get_focus_visible] for more information about
   --  focus indication.
   --  To find out if the widget has the global input focus, use
   --  [methodGtk.Widget.has_focus].
   --  @return true if the widget should display a "focus rectangle"

   procedure Hide (Widget : not null access Gtk_Widget_Record);
   pragma Obsolescent (Hide);
   --  Reverses the effects of [method.Gtk.Widget.show].
   --  This is causing the widget to be hidden (invisible to the user).
   --  Deprecated since 4.10, 1

   function In_Destruction
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Returns whether the widget is currently being destroyed.
   --  This information can sometimes be used to avoid doing unnecessary work.
   --  @return true if Widget is being destroyed

   procedure Init_Template (Widget : not null access Gtk_Widget_Record);
   --  Creates and initializes child widgets defined in templates.
   --  This function must be called in the instance initializer for any class
   --  which assigned itself a template using
   --  [methodGtk.WidgetClass.set_template].
   --  It is important to call this function in the instance initializer of a
   --  widget subclass and not in `GObject.constructed` or
   --  `GObject.constructor` for two reasons:
   --  - derived widgets will assume that the composite widgets defined by its
   --  parent classes have been created in their relative instance initializers
   --  - when calling `g_object_new` on a widget with composite templates, it's
   --  important to build the composite widgets before the construct properties
   --  are set. Properties passed to `g_object_new` should take precedence over
   --  properties set in the private template XML
   --  A good rule of thumb is to call this function as the first thing in an
   --  instance initialization function.

   procedure Insert_Action_Group
      (Widget : not null access Gtk_Widget_Record;
       Name   : UTF8_String;
       Group  : Glib.Action_Group.Gaction_Group);
   --  Inserts an action group into the widget's actions.
   --  Children of Widget that implement [ifaceGtk.Actionable] can then be
   --  associated with actions in Group by setting their "action-name" to
   --  Prefix.`action-name`.
   --  Note that inheritance is defined for individual actions. I.e. even if
   --  you insert a group with prefix Prefix, actions with the same prefix will
   --  still be inherited from the parent, unless the group contains an action
   --  with the same name.
   --  If Group is `NULL`, a previously inserted group for Name is removed
   --  from Widget.
   --  @param Name the prefix for actions in Group
   --  @param Group an action group

   procedure Insert_After
      (Widget           : not null access Gtk_Widget_Record;
       Parent           : not null access Gtk_Widget_Record'Class;
       Previous_Sibling : access Gtk_Widget_Record'Class);
   --  Sets the parent widget of the widget.
   --  In contrast to [methodGtk.Widget.set_parent], this function inserts
   --  Widget at a specific position into the list of children of the Parent
   --  widget.
   --  It will be placed after Previous_Sibling, or at the beginning if
   --  Previous_Sibling is `NULL`.
   --  After calling this function, `gtk_widget_get_prev_sibling (widget)`
   --  will return Previous_Sibling.
   --  If Parent is already set as the parent widget of Widget, this function
   --  can also be used to reorder Widget in the child widget list of Parent.
   --  This function is primarily meant for widget implementations; if you are
   --  just using a widget, you *must* use its own API for adding children.
   --  @param Parent the parent widget to insert Widget into
   --  @param Previous_Sibling the new previous sibling of Widget

   procedure Insert_Before
      (Widget       : not null access Gtk_Widget_Record;
       Parent       : not null access Gtk_Widget_Record'Class;
       Next_Sibling : access Gtk_Widget_Record'Class);
   --  Sets the parent widget of the widget.
   --  In contrast to [methodGtk.Widget.set_parent], this function inserts
   --  Widget at a specific position into the list of children of the Parent
   --  widget.
   --  It will be placed before Next_Sibling, or at the end if Next_Sibling is
   --  `NULL`.
   --  After calling this function, `gtk_widget_get_next_sibling (widget)`
   --  will return Next_Sibling.
   --  If Parent is already set as the parent widget of Widget, this function
   --  can also be used to reorder Widget in the child widget list of Parent.
   --  This function is primarily meant for widget implementations; if you are
   --  just using a widget, you *must* use its own API for adding children.
   --  @param Parent the parent widget to insert Widget into
   --  @param Next_Sibling the new next sibling of Widget

   function Is_Ancestor
      (Widget   : not null access Gtk_Widget_Record;
       Ancestor : not null access Gtk_Widget_Record'Class) return Boolean;
   --  Determines whether the widget is a descendent of Ancestor.
   --  @param Ancestor another `GtkWidget`
   --  @return true if Ancestor contains Widget as a child, grandchild, great
   --  grandchild, etc

   function Is_Drawable
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether the widget can be drawn to.
   --  A widget can be drawn if it is mapped and visible.
   --  @return true if Widget is drawable

   function Is_Focus
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines if the widget is the focus widget within its toplevel.
   --  This does not mean that the [propertyGtk.Widget:has-focus] property is
   --  necessarily set; [propertyGtk.Widget:has-focus] will only be set if the
   --  toplevel widget additionally has the global input focus.
   --  @return true if the widget is the focus widget

   function Is_Sensitive
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Returns the widget's effective sensitivity.
   --  This means it is sensitive itself and also its parent widget is
   --  sensitive.
   --  @return true if the widget is effectively sensitive

   function Is_Visible
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether the widget and all its parents are marked as
   --  visible.
   --  This function does not check if the widget is obscured in any way.
   --  See also [methodGtk.Widget.get_visible] and
   --  [methodGtk.Widget.set_visible].
   --  @return true if the widget and all its parents are visible

   function List_Mnemonic_Labels
      (Widget : not null access Gtk_Widget_Record) return Widget_List.Glist;
   --  Returns the widgets for which this widget is the target of a mnemonic.
   --  Typically, these widgets will be labels. See, for example,
   --  [methodGtk.Label.set_mnemonic_widget].
   --  The widgets in the list are not individually referenced. If you want to
   --  iterate through the list and perform actions involving callbacks that
   --  might destroy the widgets, you must call `g_list_foreach (result,
   --  (GFunc)g_object_ref, NULL)` first, and then unref all the widgets
   --  afterwards.

   procedure Map (Widget : not null access Gtk_Widget_Record);
   --  Causes a widget to be mapped if it isn't already.
   --  This function is only for use in widget implementations.

   function Mnemonic_Activate
      (Widget        : not null access Gtk_Widget_Record;
       Group_Cycling : Boolean) return Boolean;
   --  Emits the [signalGtk.Widget::mnemonic-activate] signal.
   --  @param Group_Cycling true if there are other widgets with the same
   --  mnemonic
   --  @return true if the signal has been handled

   procedure Queue_Allocate (Widget : not null access Gtk_Widget_Record);
   --  Flags the widget for a rerun of the [vfuncGtk.Widget.size_allocate]
   --  function.
   --  Use this function instead of [methodGtk.Widget.queue_resize] when the
   --  Widget's size request didn't change but it wants to reposition its
   --  contents.
   --  An example user of this function is [methodGtk.Widget.set_halign].
   --  This function is only for use in widget implementations.

   procedure Queue_Draw (Widget : not null access Gtk_Widget_Record);
   --  Schedules this widget to be redrawn.
   --  The redraw will happen in the paint phase of the current or the next
   --  frame.
   --  This means Widget's [vfuncGtk.Widget.snapshot] implementation will be
   --  called.

   procedure Queue_Resize (Widget : not null access Gtk_Widget_Record);
   --  Flags a widget to have its size renegotiated.
   --  This should be called when a widget for some reason has a new size
   --  request. For example, when you change the text in a [classGtk.Label],
   --  the label queues a resize to ensure there's enough space for the new
   --  text.
   --  Note that you cannot call Gtk.Widget.Queue_Resize on a widget from
   --  inside its implementation of the [vfuncGtk.Widget.size_allocate] virtual
   --  method. Calls to Gtk.Widget.Queue_Resize from inside
   --  [vfuncGtk.Widget.size_allocate] will be silently ignored.
   --  This function is only for use in widget implementations.

   procedure Realize (Widget : not null access Gtk_Widget_Record);
   --  Creates the GDK resources associated with a widget.
   --  Normally realization happens implicitly; if you show a widget and all
   --  its parent containers, then the widget will be realized and mapped
   --  automatically.
   --  Realizing a widget requires all the widget's parent widgets to be
   --  realized; calling this function realizes the widget's parents in
   --  addition to Widget itself. If a widget is not yet inside a toplevel
   --  window when you realize it, bad things will happen.
   --  This function is primarily used in widget implementations, and isn't
   --  very useful otherwise. Many times when you think you might need it, a
   --  better approach is to connect to a signal that will be called after the
   --  widget is realized automatically, such as [signalGtk.Widget::realize].

   procedure Remove_Css_Class
      (Widget    : not null access Gtk_Widget_Record;
       Css_Class : UTF8_String);
   --  Removes a style from the widget.
   --  After this, the style of Widget will stop matching for Css_Class.
   --  @param Css_Class style class to remove from Widget, without the leading
   --  period

   procedure Remove_Mnemonic_Label
      (Widget : not null access Gtk_Widget_Record;
       Label  : not null access Gtk_Widget_Record'Class);
   --  Removes a widget from the list of mnemonic labels for this widget.
   --  See [methodGtk.Widget.list_mnemonic_labels].
   --  The widget must have previously been added to the list with
   --  [methodGtk.Widget.add_mnemonic_label].
   --  @param Label a widget that is a mnemonic label for Widget

   procedure Remove_Tick_Callback
      (Widget : not null access Gtk_Widget_Record;
       Id     : Guint);
   --  Removes a tick callback previously registered with
   --  [methodGtk.Widget.add_tick_callback].
   --  @param Id an ID returned by [methodGtk.Widget.add_tick_callback]

   procedure Set_Cursor_From_Name
      (Widget : not null access Gtk_Widget_Record;
       Name   : UTF8_String := "");
   --  Sets the cursor to be shown when the pointer hovers over the widget.
   --  This is a utility function that creates a cursor via
   --  [ctorGdk.Cursor.new_from_name] and then sets it on Widget with
   --  [methodGtk.Widget.set_cursor]. See those functions for details.
   --  On top of that, this function allows Name to be `NULL`, which will do
   --  the same as calling [methodGtk.Widget.set_cursor] with a `NULL` cursor.
   --  @param Name the name of the cursor

   function Should_Layout
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Returns whether the widget should contribute to the measuring and
   --  allocation of its parent.
   --  This is false for invisible children, but also for children that have
   --  their own surface, such as [classGtk.Popover] instances.
   --  @return true if child should be included in measuring and allocating

   procedure Show (Widget : not null access Gtk_Widget_Record);
   pragma Obsolescent (Show);
   --  Flags a widget to be displayed.
   --  Any widget that isn't shown will not appear on the screen.
   --  Remember that you have to show the containers containing a widget, in
   --  addition to the widget itself, before it will appear onscreen.
   --  When a toplevel widget is shown, it is immediately realized and mapped;
   --  other shown widgets are realized and mapped when their toplevel widget
   --  is realized and mapped.
   --  Deprecated since 4.10, 1

   procedure Translate_Coordinates
      (Widget      : not null access Gtk_Widget_Record;
       Dest_Widget : not null access Gtk_Widget_Record'Class;
       Src_X       : Gdouble;
       Src_Y       : Gdouble;
       Dest_X      : out Gdouble;
       Dest_Y      : out Gdouble;
       Result      : out Boolean);
   pragma Obsolescent (Translate_Coordinates);
   --  Translates coordinates relative to Src_Widget's allocation to
   --  coordinates relative to Dest_Widget's allocations.
   --  In order to perform this operation, both widget must share a common
   --  ancestor. If that is not the case, Dest_X and Dest_Y are set to 0 and
   --  false is returned.
   --  Deprecated since 4.12, 1
   --  @param Dest_Widget another widget
   --  @param Src_X X position in widget coordinates of Src_Widget
   --  @param Src_Y Y position in widget coordinates of Src_Widget
   --  @param Dest_X location to store X position in widget coordinates of
   --  Dest_Widget
   --  @param Dest_Y location to store Y position in widget coordinates of
   --  Dest_Widget
   --  @return true if Src_Widget and Dest_Widget have a common ancestor,
   --  false otherwise

   procedure Trigger_Tooltip_Query
      (Widget : not null access Gtk_Widget_Record);
   --  Triggers a tooltip query on the display of the widget.

   procedure Unmap (Widget : not null access Gtk_Widget_Record);
   --  Causes a widget to be unmapped if it's currently mapped.
   --  This function is only for use in widget implementations.

   procedure Unparent (Widget : not null access Gtk_Widget_Record);
   --  Removes Widget from its parent.
   --  This function is only for use in widget implementations, typically in
   --  dispose.

   procedure Unrealize (Widget : not null access Gtk_Widget_Record);
   --  Causes a widget to be unrealized.
   --  This frees all GDK resources associated with the widget.
   --  This function is only useful in widget implementations.

   ----------------------
   -- GtkAda additions --
   ----------------------

   --------------------------------------------
   --  Overriding GtkWidget virtual methods  --
   --------------------------------------------

   --  The subprograms below let you write a custom widget in Ada by deriving
   --  from Gtk_Widget_Record and overriding the GtkWidgetClass virtual
   --  methods measure, size_allocate, realize and snapshot.
   --
   --  Install the handlers on the class record from the Class_Init callback
   --  given to Glib.Object.Initialize_Class_Record (see the worked example in
   --  glib-object.ads). Each handler receives the widget as the raw C pointer;
   --  recover the Ada object with Glib.Object.Get_User_Data, e.g.:
   --
   --     declare
   --        Stub : My_Widget_Record;  --  the exact type you expect
   --     begin
   --        W := My_Widget (Glib.Object.Get_User_Data (Widget, Stub));
   --     end;
   --
   --  An overriding handler that still needs the parent class behaviour can
   --  chain to it through the matching Inherited_* procedure (passing the
   --  class record created by Initialize_Class_Record).

   type Measure_Handler is access procedure
     (Widget                             : System.Address;
      Orientation                        : Gtk.Enums.Gtk_Orientation;
      For_Size                           : Glib.Gint;
      Minimum, Natural                   : out Glib.Gint;
      Minimum_Baseline, Natural_Baseline : out Glib.Gint);
   pragma Convention (C, Measure_Handler);
   --  Report the widget's minimum and natural size for Orientation. For_Size
   --  is the available size in the opposite orientation, or -1 when unknown.
   --  Set Minimum_Baseline/Natural_Baseline to -1 when no baseline applies.

   type Size_Allocate_Handler is access procedure
     (Widget : System.Address; Width, Height, Baseline : Glib.Gint);
   pragma Convention (C, Size_Allocate_Handler);
   --  Position and size the widget (and its children) within the allocated
   --  Width x Height area. Baseline is -1 when unused.

   type Realize_Handler is access procedure (Widget : System.Address);
   pragma Convention (C, Realize_Handler);
   --  Called when the widget is realized.

   type Snapshot_Handler is access procedure
     (Widget : System.Address; Snapshot : System.Address);
   pragma Convention (C, Snapshot_Handler);
   --  Render the widget. Snapshot is the underlying C GtkSnapshot to draw
   --  into.

   procedure Set_Default_Measure_Handler
     (Klass : Glib.Object.GObject_Class; Handler : Measure_Handler);
   pragma Import (C, Set_Default_Measure_Handler,
      "ada_WIDGET_CLASS_override_measure");
   --  Override the class "measure" vfunc. Only needed when writing your own
   --  widgets.

   procedure Set_Default_Size_Allocate_Handler
     (Klass : Glib.Object.GObject_Class; Handler : Size_Allocate_Handler);
   pragma Import (C, Set_Default_Size_Allocate_Handler,
      "ada_WIDGET_CLASS_override_size_allocate");
   --  Override the class "size_allocate" vfunc.

   procedure Set_Default_Realize_Handler
     (Klass : Glib.Object.GObject_Class; Handler : Realize_Handler);
   pragma Import (C, Set_Default_Realize_Handler,
      "ada_WIDGET_CLASS_override_realize");
   --  Override the class "realize" vfunc.

   procedure Set_Default_Snapshot_Handler
     (Klass : Glib.Object.GObject_Class; Handler : Snapshot_Handler);
   pragma Import (C, Set_Default_Snapshot_Handler,
      "ada_WIDGET_CLASS_override_snapshot");
   --  Override the class "snapshot" vfunc.

   procedure Inherited_Measure
     (Klass                              : Glib.Object.Ada_GObject_Class;
      Widget                             : access Gtk_Widget_Record'Class;
      Orientation                        : Gtk.Enums.Gtk_Orientation;
      For_Size                           : Glib.Gint;
      Minimum, Natural                   : out Glib.Gint;
      Minimum_Baseline, Natural_Baseline : out Glib.Gint);
   --  Call the parent class "measure" implementation. Useful when an
   --  overriding handler still needs the inherited behaviour.

   procedure Inherited_Size_Allocate
     (Klass                   : Glib.Object.Ada_GObject_Class;
      Widget                  : access Gtk_Widget_Record'Class;
      Width, Height, Baseline : Glib.Gint);
   --  Call the parent class "size_allocate" implementation.

   procedure Inherited_Realize
     (Klass  : Glib.Object.Ada_GObject_Class;
      Widget : access Gtk_Widget_Record'Class);
   --  Call the parent class "realize" implementation.

   procedure Inherited_Snapshot
     (Klass    : Glib.Object.Ada_GObject_Class;
      Widget   : access Gtk_Widget_Record'Class;
      Snapshot : System.Address);
   --  Call the parent class "snapshot" implementation.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Css_Classes_Property : constant Glib.Properties.Property_String :=
   Glib.Properties.Build ("css-classes");--  Unknown type: unspecified

   Can_Focus_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the widget or any of its descendents can accept the input
   --  focus.
   --
   --  This property is meant to be set by widget implementations, typically
   --  in their instance init function.

   Can_Target_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the widget can receive pointer events.

   Css_Name_Property : constant Glib.Properties.Property_String;
   --  The name of this widget in the CSS tree.
   --
   --  This property is meant to be set by widget implementations, typically
   --  in their instance init function.

   Cursor_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gdk.Cursor
   --  The cursor used by Widget.

   Focus_On_Click_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the widget should grab focus when it is clicked with the mouse.
   --
   --  This property is only relevant for widgets that can take focus.

   Focusable_Property : constant Glib.Properties.Property_Boolean;
   --  Whether this widget itself will accept the input focus.

   Halign_Property : constant Gtk.Widget.Property_Gtk_Align;
   --  Type: Gtk_Align
   --  How to distribute horizontal space if widget gets extra space.

   Has_Default_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the widget is the default widget.

   Has_Focus_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the widget has the input focus.

   Has_Tooltip_Property : constant Glib.Properties.Property_Boolean;
   --  Enables or disables the emission of the
   --  [signalGtk.Widget::query-tooltip] signal on Widget.
   --
   --  A true value indicates that Widget can have a tooltip, in this case the
   --  widget will be queried using [signalGtk.Widget::query-tooltip] to
   --  determine whether it will provide a tooltip or not.

   Height_Request_Property : constant Glib.Properties.Property_Int;
   --  Overrides for height request of the widget.
   --
   --  If this is -1, the natural request will be used.

   Hexpand_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to expand horizontally.

   Hexpand_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to use the `hexpand` property.

   Layout_Manager_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Layout_Manager.Gtk_Layout_Manager
   --  The [classGtk.LayoutManager] instance to use to compute the preferred
   --  size of the widget, and allocate its children.
   --
   --  This property is meant to be set by widget implementations, typically
   --  in their instance init function.

   Limit_Events_Property : constant Glib.Properties.Property_Boolean;
   --  Makes this widget act like a modal dialog, with respect to event
   --  delivery.
   --
   --  Global event controllers will not handle events with targets inside the
   --  widget, unless they are set up to ignore propagation limits. See
   --  [methodGtk.EventController.set_propagation_limit].

   Margin_Bottom_Property : constant Glib.Properties.Property_Int;
   --  Margin on bottom side of widget.
   --
   --  This property adds margin outside of the widget's normal size request,
   --  the margin will be added in addition to the size from
   --  [methodGtk.Widget.set_size_request] for example.

   Margin_End_Property : constant Glib.Properties.Property_Int;
   --  Margin on end of widget, horizontally.
   --
   --  This property supports left-to-right and right-to-left text directions.
   --
   --  This property adds margin outside of the widget's normal size request,
   --  the margin will be added in addition to the size from
   --  [methodGtk.Widget.set_size_request] for example.

   Margin_Start_Property : constant Glib.Properties.Property_Int;
   --  Margin on start of widget, horizontally.
   --
   --  This property supports left-to-right and right-to-left text directions.
   --
   --  This property adds margin outside of the widget's normal size request,
   --  the margin will be added in addition to the size from
   --  [methodGtk.Widget.set_size_request] for example.

   Margin_Top_Property : constant Glib.Properties.Property_Int;
   --  Margin on top side of widget.
   --
   --  This property adds margin outside of the widget's normal size request,
   --  the margin will be added in addition to the size from
   --  [methodGtk.Widget.set_size_request] for example.

   Name_Property : constant Glib.Properties.Property_String;
   --  The name of the widget.

   Opacity_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The requested opacity of the widget.

   Overflow_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Overflow
   --  How content outside the widget's content area is treated.
   --
   --  This property is meant to be set by widget implementations, typically
   --  in their instance init function.

   Parent_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk_Widget
   --  The parent widget of this widget.

   Receives_Default_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the widget will receive the default action when it is focused.

   Root_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Root.Gtk_Root
   --  The `GtkRoot` widget of the widget tree containing this widget.
   --
   --  This will be `NULL` if the widget is not contained in a root widget.

   Scale_Factor_Property : constant Glib.Properties.Property_Int;
   --  The scale factor of the widget.

   Sensitive_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the widget responds to input.

   Tooltip_Markup_Property : constant Glib.Properties.Property_String;
   --  Sets the text of tooltip to be the given string, which is marked up
   --  with Pango markup.
   --
   --  Also see [methodGtk.Tooltip.set_markup].
   --
   --  This is a convenience property which will take care of getting the
   --  tooltip shown if the given string is not `NULL`:
   --  [propertyGtk.Widget:has-tooltip] will automatically be set to true and
   --  there will be taken care of [signalGtk.Widget::query-tooltip] in the
   --  default signal handler.
   --
   --  Note that if both [propertyGtk.Widget:tooltip-text] and
   --  [propertyGtk.Widget:tooltip-markup] are set, the last one wins.

   Tooltip_Text_Property : constant Glib.Properties.Property_String;
   --  Sets the text of tooltip to be the given string.
   --
   --  Also see [methodGtk.Tooltip.set_text].
   --
   --  This is a convenience property which will take care of getting the
   --  tooltip shown if the given string is not `NULL`:
   --  [propertyGtk.Widget:has-tooltip] will automatically be set to true and
   --  there will be taken care of [signalGtk.Widget::query-tooltip] in the
   --  default signal handler.
   --
   --  Note that if both [propertyGtk.Widget:tooltip-text] and
   --  [propertyGtk.Widget:tooltip-markup] are set, the last one wins.

   Valign_Property : constant Gtk.Widget.Property_Gtk_Align;
   --  Type: Gtk_Align
   --  How to distribute vertical space if widget gets extra space.

   Vexpand_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to expand vertically.

   Vexpand_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to use the `vexpand` property.

   Visible_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the widget is visible.

   Width_Request_Property : constant Glib.Properties.Property_Int;
   --  Overrides for width request of the widget.
   --
   --  If this is -1, the natural request will be used.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Widget_Void is not null access procedure (Self : access Gtk_Widget_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Destroy : constant Glib.Signal_Name := "destroy";
   procedure On_Destroy
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Destroy
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Signals that all holders of a reference to the widget should release
   --  the reference that they hold.
   --
   --  May result in finalization of the widget if all references are
   --  released.
   --
   --  This signal is not suitable for saving widget state.

   type Cb_Gtk_Widget_Gtk_Text_Direction_Void is not null access procedure
     (Self               : access Gtk_Widget_Record'Class;
      Previous_Direction : Gtk.Enums.Gtk_Text_Direction);

   type Cb_GObject_Gtk_Text_Direction_Void is not null access procedure
     (Self               : access Glib.Object.GObject_Record'Class;
      Previous_Direction : Gtk.Enums.Gtk_Text_Direction);

   Signal_Direction_Changed : constant Glib.Signal_Name := "direction-changed";
   procedure On_Direction_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Text_Direction_Void;
       After : Boolean := False);
   procedure On_Direction_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Text_Direction_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the text direction of a widget changes.

   Signal_Hide : constant Glib.Signal_Name := "hide";
   procedure On_Hide
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Hide
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when Widget is hidden.

   type Cb_Gtk_Widget_Gtk_Direction_Type_Boolean is not null access function
     (Self      : access Gtk_Widget_Record'Class;
      Direction : Gtk.Enums.Gtk_Direction_Type) return Boolean;

   type Cb_GObject_Gtk_Direction_Type_Boolean is not null access function
     (Self      : access Glib.Object.GObject_Record'Class;
      Direction : Gtk.Enums.Gtk_Direction_Type) return Boolean;

   Signal_Keynav_Failed : constant Glib.Signal_Name := "keynav-failed";
   procedure On_Keynav_Failed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Direction_Type_Boolean;
       After : Boolean := False);
   procedure On_Keynav_Failed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Direction_Type_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted if keyboard navigation fails.
   --
   --  See [methodGtk.Widget.keynav_failed] for details.
   -- 
   --  Callback parameters:
   --    --  @param Direction the direction of movement

   Signal_Map : constant Glib.Signal_Name := "map";
   procedure On_Map
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Map
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when Widget is going to be mapped.
   --
   --  A widget is mapped when the widget is visible (which is controlled with
   --  [propertyGtk.Widget:visible]) and all its parents up to the toplevel
   --  widget are also visible.
   --
   --  The `::map` signal can be used to determine whether a widget will be
   --  drawn, for instance it can resume an animation that was stopped during
   --  the emission of [signalGtk.Widget::unmap].

   type Cb_Gtk_Widget_Boolean_Boolean is not null access function
     (Self          : access Gtk_Widget_Record'Class;
      Group_Cycling : Boolean) return Boolean;

   type Cb_GObject_Boolean_Boolean is not null access function
     (Self          : access Glib.Object.GObject_Record'Class;
      Group_Cycling : Boolean) return Boolean;

   Signal_Mnemonic_Activate : constant Glib.Signal_Name := "mnemonic-activate";
   procedure On_Mnemonic_Activate
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Boolean_Boolean;
       After : Boolean := False);
   procedure On_Mnemonic_Activate
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a widget is activated via a mnemonic.
   --
   --  The default handler for this signal activates Widget if Group_Cycling
   --  is false, or just makes Widget grab focus if Group_Cycling is true.
   -- 
   --  Callback parameters:
   --    --  @param Group_Cycling true if there are other widgets with the same
   --    --  mnemonic

   type Cb_Gtk_Widget_Gtk_Direction_Type_Void is not null access procedure
     (Self      : access Gtk_Widget_Record'Class;
      Direction : Gtk.Enums.Gtk_Direction_Type);

   type Cb_GObject_Gtk_Direction_Type_Void is not null access procedure
     (Self      : access Glib.Object.GObject_Record'Class;
      Direction : Gtk.Enums.Gtk_Direction_Type);

   Signal_Move_Focus : constant Glib.Signal_Name := "move-focus";
   procedure On_Move_Focus
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Direction_Type_Void;
       After : Boolean := False);
   procedure On_Move_Focus
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Direction_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the focus is moved.
   --
   --  The `::move-focus` signal is a [keybinding
   --  signal](class.SignalAction.html).
   --
   --  The default bindings for this signal are <kbd>Tab</kbd> to move
   --  forward, and <kbd>Shift</kbd>+<kbd>Tab</kbd> to move backward.

   type Cb_Gtk_Widget_Gint_Gint_Boolean_GObject_Boolean is not null access function
     (Self          : access Gtk_Widget_Record'Class;
      X             : Glib.Gint;
      Y             : Glib.Gint;
      Keyboard_Mode : Boolean;
      Tooltip       : not null access Glib.Object.GObject_Record'Class)
   return Boolean;

   type Cb_GObject_Gint_Gint_Boolean_GObject_Boolean is not null access function
     (Self          : access Glib.Object.GObject_Record'Class;
      X             : Glib.Gint;
      Y             : Glib.Gint;
      Keyboard_Mode : Boolean;
      Tooltip       : not null access Glib.Object.GObject_Record'Class)
   return Boolean;

   Signal_Query_Tooltip : constant Glib.Signal_Name := "query-tooltip";
   procedure On_Query_Tooltip
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gint_Gint_Boolean_GObject_Boolean;
       After : Boolean := False);
   procedure On_Query_Tooltip
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gint_Gint_Boolean_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the widget's tooltip is about to be shown.
   --
   --  This happens when the [propertyGtk.Widget:has-tooltip] property is true
   --  and the hover timeout has expired with the cursor hovering above Widget;
   --  or emitted when Widget got focus in keyboard mode.
   --
   --  Using the given coordinates, the signal handler should determine
   --  whether a tooltip should be shown for Widget. If this is the case true
   --  should be returned, false otherwise. Note that if Keyboard_Mode is true,
   --  the values of X and Y are undefined and should not be used.
   --
   --  The signal handler is free to manipulate Tooltip with the therefore
   --  destined function calls.
   -- 
   --  Callback parameters:
   --    --  @param X the x coordinate of the cursor position in widget coordinates
   --    --  @param Y the y coordinate of the cursor position in widget coordinates
   --    --  @param Keyboard_Mode true if the tooltip was triggered using the
   --    --  keyboard
   --    --  @param Tooltip a `GtkTooltip`

   Signal_Realize : constant Glib.Signal_Name := "realize";
   procedure On_Realize
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Realize
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when Widget is associated with a `GdkSurface`.
   --
   --  This means that [methodGtk.Widget.realize] has been called or the
   --  widget has been mapped (that is, it is going to be drawn).

   Signal_Show : constant Glib.Signal_Name := "show";
   procedure On_Show
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Show
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when Widget is shown.

   type Cb_Gtk_Widget_Gtk_State_Flags_Void is not null access procedure
     (Self  : access Gtk_Widget_Record'Class;
      Flags : Gtk.Enums.Gtk_State_Flags);

   type Cb_GObject_Gtk_State_Flags_Void is not null access procedure
     (Self  : access Glib.Object.GObject_Record'Class;
      Flags : Gtk.Enums.Gtk_State_Flags);

   Signal_State_Flags_Changed : constant Glib.Signal_Name := "state-flags-changed";
   procedure On_State_Flags_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_State_Flags_Void;
       After : Boolean := False);
   procedure On_State_Flags_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_State_Flags_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the widget state changes.
   --
   --  See [methodGtk.Widget.get_state_flags].

   Signal_Unmap : constant Glib.Signal_Name := "unmap";
   procedure On_Unmap
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Unmap
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when Widget is going to be unmapped.
   --
   --  A widget is unmapped when either it or any of its parents up to the
   --  toplevel widget have been set as hidden.
   --
   --  As `::unmap` indicates that a widget will not be shown any longer, it
   --  can be used to, for example, stop an animation on the widget.

   Signal_Unrealize : constant Glib.Signal_Name := "unrealize";
   procedure On_Unrealize
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Unrealize
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the `GdkSurface` associated with Widget is destroyed.
   --
   --  This means that [methodGtk.Widget.unrealize] has been called or the
   --  widget has been unmapped (that is, it is going to be hidden).

private
   Width_Request_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width-request");
   Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible");
   Vexpand_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("vexpand-set");
   Vexpand_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("vexpand");
   Valign_Property : constant Gtk.Widget.Property_Gtk_Align :=
     Gtk.Widget.Build ("valign");
   Tooltip_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("tooltip-text");
   Tooltip_Markup_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("tooltip-markup");
   Sensitive_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("sensitive");
   Scale_Factor_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("scale-factor");
   Root_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("root");
   Receives_Default_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("receives-default");
   Parent_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("parent");
   Overflow_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("overflow");
   Opacity_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("opacity");
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
   Margin_Top_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("margin-top");
   Margin_Start_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("margin-start");
   Margin_End_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("margin-end");
   Margin_Bottom_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("margin-bottom");
   Limit_Events_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("limit-events");
   Layout_Manager_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("layout-manager");
   Hexpand_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("hexpand-set");
   Hexpand_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("hexpand");
   Height_Request_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("height-request");
   Has_Tooltip_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-tooltip");
   Has_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-focus");
   Has_Default_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-default");
   Halign_Property : constant Gtk.Widget.Property_Gtk_Align :=
     Gtk.Widget.Build ("halign");
   Focusable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("focusable");
   Focus_On_Click_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("focus-on-click");
   Cursor_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("cursor");
   Css_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("css-name");
   Can_Target_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("can-target");
   Can_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("can-focus");
end Gtk.Widget;
