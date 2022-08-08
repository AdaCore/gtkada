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
--  GtkWidget is the base class all widgets in GTK+ derive from. It manages
--  the widget lifecycle, states and style.
--
--  # Height-for-width Geometry Management # {geometry-management}
--
--  GTK+ uses a height-for-width (and width-for-height) geometry management
--  system. Height-for-width means that a widget can change how much vertical
--  space it needs, depending on the amount of horizontal space that it is
--  given (and similar for width-for-height). The most common example is a
--  label that reflows to fill up the available width, wraps to fewer lines,
--  and therefore needs less height.
--
--  Height-for-width geometry management is implemented in GTK+ by way of five
--  virtual methods:
--
--  - Gtk.Widget.GObject_Class.get_request_mode -
--  Gtk.Widget.GObject_Class.get_preferred_width -
--  Gtk.Widget.GObject_Class.get_preferred_height -
--  Gtk.Widget.GObject_Class.get_preferred_height_for_width -
--  Gtk.Widget.GObject_Class.get_preferred_width_for_height -
--  Gtk.Widget.GObject_Class.get_preferred_height_and_baseline_for_width
--
--  There are some important things to keep in mind when implementing
--  height-for-width and when using it in container implementations.
--
--  The geometry management system will query a widget hierarchy in only one
--  orientation at a time. When widgets are initially queried for their minimum
--  sizes it is generally done in two initial passes in the
--  Gtk.Enums.Gtk_Size_Request_Mode chosen by the toplevel.
--
--  For example, when queried in the normal Gtk.Enums.Height_For_Width mode:
--  First, the default minimum and natural width for each widget in the
--  interface will be computed using Gtk.Widget.Get_Preferred_Width. Because
--  the preferred widths for each container depend on the preferred widths of
--  their children, this information propagates up the hierarchy, and finally a
--  minimum and natural width is determined for the entire toplevel. Next, the
--  toplevel will use the minimum width to query for the minimum height
--  contextual to that width using Gtk.Widget.Get_Preferred_Height_For_Width,
--  which will also be a highly recursive operation. The minimum height for the
--  minimum width is normally used to set the minimum size constraint on the
--  toplevel (unless Gtk.Window.Set_Geometry_Hints is explicitly used instead).
--
--  After the toplevel window has initially requested its size in both
--  dimensions it can go on to allocate itself a reasonable size (or a size
--  previously specified with Gtk.Window.Set_Default_Size). During the
--  recursive allocation process it's important to note that request cycles
--  will be recursively executed while container widgets allocate their
--  children. Each container widget, once allocated a size, will go on to first
--  share the space in one orientation among its children and then request each
--  child's height for its target allocated width or its width for allocated
--  height, depending. In this way a Gtk.Widget.Gtk_Widget will typically be
--  requested its size a number of times before actually being allocated a
--  size. The size a widget is finally allocated can of course differ from the
--  size it has requested. For this reason, Gtk.Widget.Gtk_Widget caches a
--  small number of results to avoid re-querying for the same sizes in one
--  allocation cycle.
--
--  See [GtkContainer's geometry management
--  section][container-geometry-management] to learn more about how
--  height-for-width allocations are performed by container widgets.
--
--  If a widget does move content around to intelligently use up the allocated
--  size then it must support the request in both Gtk_Size_Request_Modes even
--  if the widget in question only trades sizes in a single orientation.
--
--  For instance, a Gtk.Label.Gtk_Label that does height-for-width word
--  wrapping will not expect to have
--  Gtk.Widget.GObject_Class.get_preferred_height called because that call is
--  specific to a width-for-height request. In this case the label must return
--  the height required for its own minimum possible width. By following this
--  rule any widget that handles height-for-width or width-for-height requests
--  will always be allocated at least enough space to fit its own content.
--
--  Here are some examples of how a Gtk.Enums.Height_For_Width widget
--  generally deals with width-for-height requests, for
--  Gtk.Widget.GObject_Class.get_preferred_height it will do:
--
--  |[<!-- language="C" --> static void foo_widget_get_preferred_height
--  (GtkWidget *widget, gint *min_height, gint *nat_height) { if
--  (i_am_in_height_for_width_mode) { gint min_width, nat_width;
--
--  GTK_WIDGET_GET_CLASS (widget)->get_preferred_width (widget, &min_width,
--  &nat_width); GTK_WIDGET_GET_CLASS (widget)->get_preferred_height_for_width
--  (widget, min_width, min_height, nat_height); } else { ... some widgets do
--  both. For instance, if a GtkLabel is rotated to 90 degrees it will return
--  the minimum and natural height for the rotated label here. } } ]|
--
--  And in Gtk.Widget.GObject_Class.get_preferred_width_for_height it will
--  simply return the minimum and natural width: |[<!-- language="C" --> static
--  void foo_widget_get_preferred_width_for_height (GtkWidget *widget, gint
--  for_height, gint *min_width, gint *nat_width) { if
--  (i_am_in_height_for_width_mode) { GTK_WIDGET_GET_CLASS
--  (widget)->get_preferred_width (widget, min_width, nat_width); } else { ...
--  again if a widget is sometimes operating in width-for-height mode (like a
--  rotated GtkLabel) it can go ahead and do its real width for height
--  calculation here. } } ]|
--
--  Often a widget needs to get its own request during size request or
--  allocation. For example, when computing height it may need to also compute
--  width. Or when deciding how to use an allocation, the widget may need to
--  know its natural size. In these cases, the widget should be careful to call
--  its virtual methods directly, like this:
--
--  |[<!-- language="C" --> GTK_WIDGET_GET_CLASS(widget)->get_preferred_width
--  (widget, &min, &natural); ]|
--
--  It will not work to use the wrapper functions, such as
--  Gtk.Widget.Get_Preferred_Width inside your own size request implementation.
--  These return a request adjusted by Gtk.Size_Group.Gtk_Size_Group and by the
--  Gtk.Widget.GObject_Class.adjust_size_request virtual method. If a widget
--  used the wrappers inside its virtual method implementations, then the
--  adjustments (such as widget margins) would be applied twice. GTK+ therefore
--  does not allow this and will warn if you try to do it.
--
--  Of course if you are getting the size request for another widget, such as
--  a child of a container, you must use the wrapper APIs. Otherwise, you would
--  not properly consider widget margins, Gtk.Size_Group.Gtk_Size_Group, and so
--  forth.
--
--  Since 3.10 GTK+ also supports baseline vertical alignment of widgets. This
--  means that widgets are positioned such that the typographical baseline of
--  widgets in the same row are aligned. This happens if a widget supports
--  baselines, has a vertical alignment of Gtk.Widget.Align_Baseline, and is
--  inside a container that supports baselines and has a natural "row" that it
--  aligns to the baseline, or a baseline assigned to it by the grandparent.
--
--  Baseline alignment support for a widget is done by the
--  Gtk.Widget.GObject_Class.get_preferred_height_and_baseline_for_width
--  virtual function. It allows you to report a baseline in combination with
--  the minimum and natural height. If there is no baseline you can return -1
--  to indicate this. The default implementation of this virtual function calls
--  into the Gtk.Widget.GObject_Class.get_preferred_height and
--  Gtk.Widget.GObject_Class.get_preferred_height_for_width, so if baselines
--  are not supported it doesn't need to be implemented.
--
--  If a widget ends up baseline aligned it will be allocated all the space in
--  the parent as if it was Gtk.Widget.Align_Fill, but the selected baseline
--  can be found via Gtk.Widget.Get_Allocated_Baseline. If this has a value
--  other than -1 you need to align the widget such that the baseline appears
--  at the position.
--
--  # Style Properties
--
--  Gtk.Widget.Gtk_Widget introduces "style properties" - these are basically
--  object properties that are stored not on the object, but in the style
--  object associated to the widget. Style properties are set in [resource
--  files][gtk3-Resource-Files]. This mechanism is used for configuring such
--  things as the location of the scrollbar arrows through the theme, giving
--  theme authors more control over the look of applications without the need
--  to write a theme engine in C.
--
--  Use Gtk.Widget.Install_Style_Property to install style properties for a
--  widget class, Gtk.Widget.Find_Style_Property or
--  gtk_widget_class_list_style_properties to get information about existing
--  style properties and Gtk.Widget.Style_Get_Property, gtk_widget_style_get or
--  gtk_widget_style_get_valist to obtain the value of a style property.
--
--  # GtkWidget as GtkBuildable
--
--  The GtkWidget implementation of the GtkBuildable interface supports a
--  custom <accelerator> element, which has attributes named "key", "modifiers"
--  and "signal" and allows to specify accelerators.
--
--  An example of a UI definition fragment specifying an accelerator: |[
--  <object class="GtkButton"> <accelerator key="q"
--  modifiers="GDK_CONTROL_MASK" signal="clicked"/> </object> ]|
--
--  In addition to accelerators, GtkWidget also support a custom <accessible>
--  element, which supports actions and relations. Properties on the accessible
--  implementation of an object can be set by accessing the internal child
--  "accessible" of a Gtk.Widget.Gtk_Widget.
--
--  An example of a UI definition fragment specifying an accessible: |[
--  <object class="GtkLabel" id="label1"/> <property name="label">I am a Label
--  for a Button</property> </object> <object class="GtkButton" id="button1">
--  <accessibility> <action action_name="click" translatable="yes">Click the
--  button.</action> <relation target="label1" type="labelled-by"/>
--  </accessibility> <child internal-child="accessible"> <object
--  class="AtkObject" id="a11y-button1"> <property
--  name="accessible-name">Clickable Button</property> </object> </child>
--  </object> ]|
--
--  Finally, GtkWidget allows style information such as style classes to be
--  associated with widgets, using the custom <style> element: |[ <object
--  class="GtkButton" id="button1"> <style> <class
--  name="my-special-button-class"/> <class name="dark-button"/> </style>
--  </object> ]|
--
--  # Building composite widgets from template XML ## {composite-templates}
--
--  GtkWidget exposes some facilities to automate the procedure of creating
--  composite widgets using Gtk.Builder.Gtk_Builder interface description
--  language.
--
--  To create composite widgets with Gtk.Builder.Gtk_Builder XML, one must
--  associate the interface description with the widget class at class
--  initialization time using gtk_widget_class_set_template.
--
--  The interface description semantics expected in composite template
--  descriptions is slightly different from regular Gtk.Builder.Gtk_Builder
--  XML.
--
--  Unlike regular interface descriptions, gtk_widget_class_set_template will
--  expect a <template> tag as a direct child of the toplevel <interface> tag.
--  The <template> tag must specify the "class" attribute which must be the
--  type name of the widget. Optionally, the "parent" attribute may be
--  specified to specify the direct parent type of the widget type, this is
--  ignored by the GtkBuilder but required for Glade to introspect what kind of
--  properties and internal children exist for a given type when the actual
--  type does not exist.
--
--  The XML which is contained inside the <template> tag behaves as if it were
--  added to the <object> tag defining Widget itself. You may set properties on
--  Widget by inserting <property> tags into the <template> tag, and also add
--  <child> tags to add children and extend Widget in the normal way you would
--  with <object> tags.
--
--  Additionally, <object> tags can also be added before and after the initial
--  <template> tag in the normal way, allowing one to define auxiliary objects
--  which might be referenced by other widgets declared as children of the
--  <template> tag.
--
--  An example of a GtkBuilder Template Definition: |[ <interface> <template
--  class="FooWidget" parent="GtkBox"> <property
--  name="orientation">GTK_ORIENTATION_HORIZONTAL</property> <property
--  name="spacing">4</property> <child> <object class="GtkButton"
--  id="hello_button"> <property name="label">Hello World</property> <signal
--  name="clicked" handler="hello_button_clicked" object="FooWidget"
--  swapped="yes"/> </object> </child> <child> <object class="GtkButton"
--  id="goodbye_button"> <property name="label">Goodbye World</property>
--  </object> </child> </template> </interface> ]|
--
--  Typically, you'll place the template fragment into a file that is bundled
--  with your project, using Gresource.Gresource. In order to load the
--  template, you need to call gtk_widget_class_set_template_from_resource from
--  the class initialization of your Gtk.Widget.Gtk_Widget type:
--
--  |[<!-- language="C" --> static void foo_widget_class_init (FooWidgetClass
--  *klass) { // ...
--
--  gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (klass),
--  "/com/example/ui/foowidget.ui"); } ]|
--
--  You will also need to call Gtk.Widget.Init_Template from the instance
--  initialization function:
--
--  |[<!-- language="C" --> static void foo_widget_init (FooWidget *self) { //
--  ... gtk_widget_init_template (GTK_WIDGET (self)); } ]|
--
--  You can access widgets defined in the template using the
--  Gtk.Widget.Get_Template_Child function, but you will typically declare a
--  pointer in the instance private data structure of your type using the same
--  name as the widget in the template definition, and call
--  gtk_widget_class_bind_template_child_private with that name, e.g.
--
--  |[<!-- language="C" --> typedef struct { GtkWidget *hello_button;
--  GtkWidget *goodbye_button; } FooWidgetPrivate;
--
--  G_DEFINE_TYPE_WITH_PRIVATE (FooWidget, foo_widget, GTK_TYPE_BOX)
--
--  static void foo_widget_class_init (FooWidgetClass *klass) { // ...
--  gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (klass),
--  "/com/example/ui/foowidget.ui");
--  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (klass),
--  FooWidget, hello_button); gtk_widget_class_bind_template_child_private
--  (GTK_WIDGET_CLASS (klass), FooWidget, goodbye_button); }
--
--  static void foo_widget_init (FooWidget *widget) {
--
--  } ]|
--
--  You can also use gtk_widget_class_bind_template_callback to connect a
--  signal callback defined in the template with a function visible in the
--  scope of the class, e.g.
--
--  |[<!-- language="C" --> // the signal handler has the instance and user
--  data swapped // because of the swapped="yes" attribute in the template XML
--  static void hello_button_clicked (FooWidget *self, GtkButton *button) {
--  g_print ("Hello, world!\n"); }
--
--  static void foo_widget_class_init (FooWidgetClass *klass) { // ...
--  gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (klass),
--  "/com/example/ui/foowidget.ui"); gtk_widget_class_bind_template_callback
--  (GTK_WIDGET_CLASS (klass), hello_button_clicked); } ]|
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;                   use Cairo;
with Cairo.Region;            use Cairo.Region;
with GNAT.Strings;            use GNAT.Strings;
with Gdk;                     use Gdk;
with Gdk.Color;               use Gdk.Color;
with Gdk.Device;              use Gdk.Device;
with Gdk.Display;             use Gdk.Display;
with Gdk.Drag_Contexts;       use Gdk.Drag_Contexts;
with Gdk.Event;               use Gdk.Event;
with Gdk.Frame_Clock;         use Gdk.Frame_Clock;
with Gdk.Pixbuf;              use Gdk.Pixbuf;
with Gdk.RGBA;                use Gdk.RGBA;
with Gdk.Rectangle;           use Gdk.Rectangle;
with Gdk.Screen;              use Gdk.Screen;
with Gdk.Types;               use Gdk.Types;
with Gdk.Visual;              use Gdk.Visual;
with Glib;                    use Glib;
with Glib.Action_Group;       use Glib.Action_Group;
with Glib.GSlist;             use Glib.GSlist;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Glist;              use Glib.Glist;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Values;             use Glib.Values;
with Gtk.Accel_Group;         use Gtk.Accel_Group;
with Gtk.Builder;             use Gtk.Builder;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Selection_Data;      use Gtk.Selection_Data;
with Gtk.Style;               use Gtk.Style;
with Gtk.Target_List;         use Gtk.Target_List;
with Pango.Context;           use Pango.Context;
with Pango.Font;              use Pango.Font;
with Pango.Font_Map;          use Pango.Font_Map;
with Pango.Layout;            use Pango.Layout;

package Gtk.Widget is

   type Gtk_Widget_Record is new GObject_Record with null record;
   type Gtk_Widget is access all Gtk_Widget_Record'Class;

   type Gtk_Align is (
      Align_Fill,
      Align_Start,
      Align_End,
      Align_Center,
      Align_Baseline);
   pragma Convention (C, Gtk_Align);
   --  Controls how a widget deals with extra space in a single (x or y)
   --  dimension.
   --
   --  Alignment only matters if the widget receives a "too large" allocation,
   --  for example if you packed the widget with the
   --  Gtk.Widget.Gtk_Widget:expand flag inside a Gtk.Box.Gtk_Box, then the
   --  widget might get extra space. If you have for example a 16x16 icon
   --  inside a 32x32 space, the icon could be scaled and stretched, it could
   --  be centered, or it could be positioned to one side of the space.
   --
   --  Note that in horizontal context Gtk_Align_Start and Gtk_Align_End are
   --  interpreted relative to text direction.
   --
   --  GTK_ALIGN_BASELINE support for it is optional for containers and
   --  widgets, and it is only supported for vertical alignment. When its not
   --  supported by a child or a container it is treated as Gtk_Align_Fill.

   type Gtk_Widget_Help_Type is (
      Widget_Help_Tooltip,
      Widget_Help_Whats_This);
   pragma Convention (C, Gtk_Widget_Help_Type);
   --  Kinds of widget-specific help. Used by the ::show-help signal.

   type Gtk_Requisition is record
      Width : Glib.Gint := 0;
      Height : Glib.Gint := 0;
   end record;
   pragma Convention (C, Gtk_Requisition);

   function From_Object_Free (B : access Gtk_Requisition) return Gtk_Requisition;
   pragma Inline (From_Object_Free);
   --  A Gtk.Widget.Gtk_Requisition-struct represents the desired size of a
   --  widget. See [GtkWidget's geometry management
   --  section][geometry-management] for more information.

   type Gtk_Widget_Path is new Glib.C_Proxy;
   function From_Object_Free (B : access Gtk_Widget_Path) return Gtk_Widget_Path;
   pragma Inline (From_Object_Free);
   --  GtkWidgetPath is a boxed type that represents a widget hierarchy from
   --  the topmost widget, typically a toplevel, to any child. This widget path
   --  abstraction is used in Gtk.Style_Context.Gtk_Style_Context on behalf of
   --  the real widget in order to query style information.
   --
   --  If you are using GTK+ widgets, you probably will not need to use this
   --  API directly, as there is Gtk.Widget.Get_Path, and the style context
   --  returned by gtk_widget_get_style_context will be automatically updated
   --  on widget hierarchy changes.
   --
   --  The widget path generation is generally simple:
   --
   --  ## Defining a button within a window
   --
   --  |[<!-- language="C" --> { GtkWidgetPath *path;
   --
   --  path = gtk_widget_path_new (); gtk_widget_path_append_type (path,
   --  GTK_TYPE_WINDOW); gtk_widget_path_append_type (path, GTK_TYPE_BUTTON); }
   --  ]|
   --
   --  Although more complex information, such as widget names, or different
   --  classes (property that may be used by other widget types) and
   --  intermediate regions may be included:
   --
   --  ## Defining the first tab widget in a notebook
   --
   --  |[<!-- language="C" --> { GtkWidgetPath *path; guint pos;
   --
   --  path = gtk_widget_path_new ();
   --
   --  pos = gtk_widget_path_append_type (path, GTK_TYPE_NOTEBOOK);
   --  gtk_widget_path_iter_add_region (path, pos, "tab", GTK_REGION_EVEN |
   --  GTK_REGION_FIRST);
   --
   --  pos = gtk_widget_path_append_type (path, GTK_TYPE_LABEL);
   --  gtk_widget_path_iter_set_name (path, pos, "first tab label"); } ]|
   --
   --  All this information will be used to match the style information that
   --  applies to the described widget.

   function Convert (R : Gtk.Widget.Gtk_Widget) return System.Address;
   function Convert (R : System.Address) return Gtk.Widget.Gtk_Widget;
   package Widget_List is new Generic_List (Gtk.Widget.Gtk_Widget);

   package Widget_SList is new Generic_SList (Gtk.Widget.Gtk_Widget);

   subtype Gtk_Allocation is Gdk.Rectangle.Gdk_Rectangle;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Tick_Callback is access function
     (Widget      : not null access Gtk_Widget_Record'Class;
      Frame_Clock : not null access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class)
   return Boolean;
   --  Callback type for adding a function to update animations. See
   --  Gtk.Widget.Add_Tick_Callback.
   --  Since: gtk+ 3.8
   --  "widget": the widget
   --  "frame_clock": the frame clock for the widget (same as calling
   --  gtk_widget_get_frame_clock)

   type Gtk_Builder_Connect_Func is access procedure
     (Builder        : not null access Gtk.Builder.Gtk_Builder_Record'Class;
      Object         : not null access Glib.Object.GObject_Record'Class;
      Signal_Name    : UTF8_String;
      Handler_Name   : UTF8_String;
      Connect_Object : access Glib.Object.GObject_Record'Class;
      Flags          : Glib.G_Connect_Flags);
   --  This is the signature of a function used to connect signals. It is used
   --  by the Gtk.Builder.Connect_Signals and Gtk.Builder.Connect_Signals_Full
   --  methods. It is mainly intended for interpreted language bindings, but
   --  could be useful where the programmer wants more control over the signal
   --  connection process. Note that this function can only be called once,
   --  subsequent calls will do nothing.
   --  Since: gtk+ 2.12
   --  "builder": a Gtk.Builder.Gtk_Builder
   --  "object": object to connect a signal to
   --  "signal_name": name of the signal
   --  "handler_name": name of the handler
   --  "connect_object": a Glib.Object.GObject, if non-null, use
   --  g_signal_connect_object
   --  "flags": Glib.G_Connect_Flags to use

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Align_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Align);
   type Property_Gtk_Align is new Gtk_Align_Properties.Property;

   package Gtk_Widget_Help_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Widget_Help_Type);
   type Property_Gtk_Widget_Help_Type is new Gtk_Widget_Help_Type_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_widget_get_type");

   -------------
   -- Methods --
   -------------

   function Activate
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  For widgets that can be "activated" (buttons, menu items, etc.) this
   --  function activates them. Activation is what happens when you press Enter
   --  on a widget during key navigation. If Widget isn't activatable, the
   --  function returns False.

   procedure Add_Accelerator
      (Widget       : not null access Gtk_Widget_Record;
       Accel_Signal : Glib.Signal_Name;
       Accel_Group  : not null access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class;
       Accel_Key    : Gdk.Types.Gdk_Key_Type;
       Accel_Mods   : Gdk.Types.Gdk_Modifier_Type;
       Accel_Flags  : Gtk.Accel_Group.Gtk_Accel_Flags);
   --  Installs an accelerator for this Widget in Accel_Group that causes
   --  Accel_Signal to be emitted if the accelerator is activated. The
   --  Accel_Group needs to be added to the widget's toplevel via
   --  Gtk.Window.Add_Accel_Group, and the signal must be of type
   --  G_SIGNAL_ACTION. Accelerators added through this function are not user
   --  changeable during runtime. If you want to support accelerators that can
   --  be changed by the user, use Gtk.Accel_Map.Add_Entry and
   --  Gtk.Widget.Set_Accel_Path or Gtk.Menu_Item.Set_Accel_Path instead.
   --  "accel_signal": widget signal to emit on accelerator activation
   --  "accel_group": accel group for this widget, added to its toplevel
   --  "accel_key": GDK keyval of the accelerator
   --  "accel_mods": modifier key combination of the accelerator
   --  "accel_flags": flag accelerators, e.g. Gtk.Target_List.Accel_Visible

   procedure Add_Device_Events
      (Widget : not null access Gtk_Widget_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class;
       Events : Gdk.Event.Gdk_Event_Mask);
   --  Adds the device events in the bitfield Events to the event mask for
   --  Widget. See Gtk.Widget.Set_Device_Events for details.
   --  Since: gtk+ 3.0
   --  "device": a Gdk.Device.Gdk_Device
   --  "events": an event mask, see Gdk.Event.Gdk_Event_Mask

   procedure Add_Events
      (Widget : not null access Gtk_Widget_Record;
       Events : Gdk.Event.Gdk_Event_Mask);
   --  Adds the events in the bitfield Events to the event mask for Widget.
   --  See Gtk.Widget.Set_Events and the [input handling overview][event-masks]
   --  for details.
   --  "events": an event mask, see Gdk.Event.Gdk_Event_Mask

   procedure Add_Mnemonic_Label
      (Widget : not null access Gtk_Widget_Record;
       Label  : not null access Gtk_Widget_Record'Class);
   --  Adds a widget to the list of mnemonic labels for this widget. (See
   --  Gtk.Widget.List_Mnemonic_Labels). Note the list of mnemonic labels for
   --  the widget is cleared when the widget is destroyed, so the caller must
   --  make sure to update its internal state at this point as well, by using a
   --  connection to the Gtk.Widget.Gtk_Widget::destroy signal or a weak
   --  notifier.
   --  Since: gtk+ 2.4
   --  "label": a Gtk.Widget.Gtk_Widget that acts as a mnemonic label for
   --  Widget

   function Add_Tick_Callback
      (Widget   : not null access Gtk_Widget_Record;
       Callback : Gtk_Tick_Callback) return Guint;
   --  Queues an animation frame update and adds a callback to be called
   --  before each frame. Until the tick callback is removed, it will be called
   --  frequently (usually at the frame rate of the output device or as quickly
   --  as the application can be repainted, whichever is slower). For this
   --  reason, is most suitable for handling graphics that change every frame
   --  or every few frames. The tick callback does not automatically imply a
   --  relayout or repaint. If you want a repaint or relayout, and aren't
   --  changing widget properties that would trigger that (for example,
   --  changing the text of a Gtk.Label.Gtk_Label), then you will have to call
   --  Gtk.Widget.Queue_Resize or Gtk.Widget.Queue_Draw_Area yourself.
   --  Gdk.Frame_Clock.Get_Frame_Time should generally be used for timing
   --  continuous animations and
   --  Gdk.Frame_Timings.Get_Predicted_Presentation_Time if you are trying to
   --  display isolated frames at particular times.
   --  This is a more convenient alternative to connecting directly to the
   --  Gdk.Frame_Clock.Gdk_Frame_Clock::update signal of
   --  Gdk.Frame_Clock.Gdk_Frame_Clock, since you don't have to worry about
   --  when a Gdk.Frame_Clock.Gdk_Frame_Clock is assigned to a widget.
   --  Since: gtk+ 3.8
   --  "callback": function to call for updating animations

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Add_Tick_Callback_User_Data is

      type Gtk_Tick_Callback is access function
        (Widget      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
         Frame_Clock : not null access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class;
         User_Data   : User_Data_Type) return Boolean;
      --  Callback type for adding a function to update animations. See
      --  Gtk.Widget.Add_Tick_Callback.
      --  Since: gtk+ 3.8
      --  "widget": the widget
      --  "frame_clock": the frame clock for the widget (same as calling
      --  gtk_widget_get_frame_clock)
      --  "user_data": user data passed to Gtk.Widget.Add_Tick_Callback.

      function Add_Tick_Callback
         (Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
          Callback  : Gtk_Tick_Callback;
          User_Data : User_Data_Type) return Guint;
      --  Queues an animation frame update and adds a callback to be called
      --  before each frame. Until the tick callback is removed, it will be
      --  called frequently (usually at the frame rate of the output device or
      --  as quickly as the application can be repainted, whichever is slower).
      --  For this reason, is most suitable for handling graphics that change
      --  every frame or every few frames. The tick callback does not
      --  automatically imply a relayout or repaint. If you want a repaint or
      --  relayout, and aren't changing widget properties that would trigger
      --  that (for example, changing the text of a Gtk.Label.Gtk_Label), then
      --  you will have to call Gtk.Widget.Queue_Resize or
      --  Gtk.Widget.Queue_Draw_Area yourself.
      --  Gdk.Frame_Clock.Get_Frame_Time should generally be used for timing
      --  continuous animations and
      --  Gdk.Frame_Timings.Get_Predicted_Presentation_Time if you are trying
      --  to display isolated frames at particular times.
      --  This is a more convenient alternative to connecting directly to the
      --  Gdk.Frame_Clock.Gdk_Frame_Clock::update signal of
      --  Gdk.Frame_Clock.Gdk_Frame_Clock, since you don't have to worry about
      --  when a Gdk.Frame_Clock.Gdk_Frame_Clock is assigned to a widget.
      --  Since: gtk+ 3.8
      --  "callback": function to call for updating animations
      --  "user_data": data to pass to Callback

   end Add_Tick_Callback_User_Data;

   function Can_Activate_Accel
      (Widget    : not null access Gtk_Widget_Record;
       Signal_Id : Guint) return Boolean;
   --  Determines whether an accelerator that activates the signal identified
   --  by Signal_Id can currently be activated. This is done by emitting the
   --  Gtk.Widget.Gtk_Widget::can-activate-accel signal on Widget; if the
   --  signal isn't overridden by a handler or in a derived widget, then the
   --  default check is that the widget must be sensitive, and the widget and
   --  all its ancestors mapped.
   --  Since: gtk+ 2.4
   --  "signal_id": the ID of a signal installed on Widget

   function Child_Focus
      (Widget    : not null access Gtk_Widget_Record;
       Direction : Gtk.Enums.Gtk_Direction_Type) return Boolean;
   --  This function is used by custom widget implementations; if you're
   --  writing an app, you'd use Gtk.Widget.Grab_Focus to move the focus to a
   --  particular widget, and Gtk.Container.Set_Focus_Chain to change the focus
   --  tab order. So you may want to investigate those functions instead.
   --  Gtk.Widget.Child_Focus is called by containers as the user moves around
   --  the window using keyboard shortcuts. Direction indicates what kind of
   --  motion is taking place (up, down, left, right, tab forward, tab
   --  backward). Gtk.Widget.Child_Focus emits the Gtk.Widget.Gtk_Widget::focus
   --  signal; widgets override the default handler for this signal in order to
   --  implement appropriate focus behavior.
   --  The default ::focus handler for a widget should return True if moving
   --  in Direction left the focus on a focusable location inside that widget,
   --  and False if moving in Direction moved the focus outside the widget. If
   --  returning True, widgets normally call Gtk.Widget.Grab_Focus to place the
   --  focus accordingly; if returning False, they don't modify the current
   --  focus location.
   --  "direction": direction of focus movement

   procedure Child_Notify
      (Widget         : not null access Gtk_Widget_Record;
       Child_Property : UTF8_String);
   --  Emits a Gtk.Widget.Gtk_Widget::child-notify signal for the [child
   --  property][child-properties] Child_Property on Widget.
   --  This is the analogue of g_object_notify for child properties.
   --  Also see Gtk.Container.Child_Notify.
   --  "child_property": the name of a child property installed on the class
   --  of Widget's parent

   function Compute_Expand
      (Widget      : not null access Gtk_Widget_Record;
       Orientation : Gtk.Enums.Gtk_Orientation) return Boolean;
   --  Computes whether a container should give this widget extra space when
   --  possible. Containers should check this, rather than looking at
   --  Gtk.Widget.Get_Hexpand or Gtk.Widget.Get_Vexpand.
   --  This function already checks whether the widget is visible, so
   --  visibility does not need to be checked separately. Non-visible widgets
   --  are not expanded.
   --  The computed expand value uses either the expand setting explicitly set
   --  on the widget itself, or, if none has been explicitly set, the widget
   --  may expand if some of its children do.
   --  "orientation": expand direction

   function Create_Pango_Context
      (Widget : not null access Gtk_Widget_Record)
       return Pango.Context.Pango_Context;
   --  Creates a new Pango.Context.Pango_Context with the appropriate font
   --  map, font options, font description, and base direction for drawing text
   --  for this widget. See also Gtk.Widget.Get_Pango_Context.

   function Create_Pango_Layout
      (Widget : not null access Gtk_Widget_Record;
       Text   : UTF8_String := "") return Pango.Layout.Pango_Layout;
   --  Creates a new Pango.Layout.Pango_Layout with the appropriate font map,
   --  font description, and base direction for drawing text for this widget.
   --  If you keep a Pango.Layout.Pango_Layout created in this way around, you
   --  need to re-create it when the widget Pango.Context.Pango_Context is
   --  replaced. This can be tracked by using the
   --  Gtk.Widget.Gtk_Widget::screen-changed signal on the widget.
   --  "text": text to set on the layout (can be null)

   procedure Destroy (Widget : not null access Gtk_Widget_Record);
   --  Destroys a widget.
   --  When a widget is destroyed all references it holds on other objects
   --  will be released:
   --  - if the widget is inside a container, it will be removed from its
   --  parent - if the widget is a container, all its children will be
   --  destroyed, recursively - if the widget is a top level, it will be
   --  removed from the list of top level widgets that GTK+ maintains
   --  internally
   --  It's expected that all references held on the widget will also be
   --  released; you should connect to the Gtk.Widget.Gtk_Widget::destroy
   --  signal if you hold a reference to Widget and you wish to remove it when
   --  this function is called. It is not necessary to do so if you are
   --  implementing a Gtk.Container.Gtk_Container, as you'll be able to use the
   --  Gtk.Container_Class.Gtk_Container_Class.remove virtual function for
   --  that.
   --  It's important to notice that Gtk.Widget.Destroy will only cause the
   --  Widget to be finalized if no additional references, acquired using
   --  g_object_ref, are held on it. In case additional references are in
   --  place, the Widget will be in an "inert" state after calling this
   --  function; Widget will still point to valid memory, allowing you to
   --  release the references you hold, but you may not query the widget's own
   --  state.
   --  You should typically call this function on top level widgets, and
   --  rarely on child widgets.
   --  See also: Gtk.Container.Remove

   procedure Destroyed
      (Widget         : not null access Gtk_Widget_Record;
       Widget_Pointer : in out Gtk_Widget);
   --  This function sets *Widget_Pointer to null if Widget_Pointer != null.
   --  It's intended to be used as a callback connected to the "destroy" signal
   --  of a widget. You connect Gtk.Widget.Destroyed as a signal handler, and
   --  pass the address of your widget variable as user data. Then when the
   --  widget is destroyed, the variable will be set to null. Useful for
   --  example to avoid multiple copies of the same dialog.
   --  "widget_pointer": address of a variable that contains Widget

   function Device_Is_Shadowed
      (Widget : not null access Gtk_Widget_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class)
       return Boolean;
   --  Returns True if Device has been shadowed by a GTK+ device grab on
   --  another widget, so it would stop sending events to Widget. This may be
   --  used in the Gtk.Widget.Gtk_Widget::grab-notify signal to check for
   --  specific devices. See Gtk.Main.Device_Grab_Add.
   --  Since: gtk+ 3.0
   --  "device": a Gdk.Device.Gdk_Device

   function Drag_Begin_With_Coordinates
      (Widget  : not null access Gtk_Widget_Record;
       Targets : Gtk.Target_List.Gtk_Target_List;
       Actions : Gdk.Drag_Contexts.Gdk_Drag_Action;
       Button  : Glib.Gint;
       Event   : Gdk.Event.Gdk_Event;
       X       : Glib.Gint;
       Y       : Glib.Gint) return Gdk.Drag_Contexts.Drag_Context;
   --  Initiates a drag on the source side. The function only needs to be used
   --  when the application is starting drags itself, and is not needed when
   --  gtk_drag_source_set is used.
   --  The Event is used to retrieve the timestamp that will be used
   --  internally to grab the pointer. If Event is null, then GDK_CURRENT_TIME
   --  will be used. However, you should try to pass a real event in all cases,
   --  since that can be used to get information about the drag.
   --  Generally there are three cases when you want to start a drag by hand
   --  by calling this function:
   --  1. During a Gtk.Widget.Gtk_Widget::button-press-event handler, if you
   --  want to start a drag immediately when the user presses the mouse button.
   --  Pass the Event that you have in your
   --  Gtk.Widget.Gtk_Widget::button-press-event handler.
   --  2. During a Gtk.Widget.Gtk_Widget::motion-notify-event handler, if you
   --  want to start a drag when the mouse moves past a certain threshold
   --  distance after a button-press. Pass the Event that you have in your
   --  Gtk.Widget.Gtk_Widget::motion-notify-event handler.
   --  3. During a timeout handler, if you want to start a drag after the
   --  mouse button is held down for some time. Try to save the last event that
   --  you got from the mouse, using Gdk.Event.Copy, and pass it to this
   --  function (remember to free the event with Gdk.Event.Free when you are
   --  done). If you really cannot pass a real event, pass null instead.
   --  Since: gtk+ 3.10
   --  "targets": The targets (data formats) in which the source can provide
   --  the data
   --  "actions": A bitmask of the allowed drag actions for this drag
   --  "button": The button the user clicked to start the drag
   --  "event": The event that triggered the start of the drag, or null if
   --  none can be obtained.
   --  "x": The initial x coordinate to start dragging from, in the coordinate
   --  space of Widget. If -1 is passed, the coordinates are retrieved from
   --  Event or the current pointer position
   --  "y": The initial y coordinate to start dragging from, in the coordinate
   --  space of Widget. If -1 is passed, the coordinates are retrieved from
   --  Event or the current pointer position

   function Drag_Check_Threshold
      (Widget    : not null access Gtk_Widget_Record;
       Start_X   : Glib.Gint;
       Start_Y   : Glib.Gint;
       Current_X : Glib.Gint;
       Current_Y : Glib.Gint) return Boolean;
   --  Checks to see if a mouse drag starting at (Start_X, Start_Y) and ending
   --  at (Current_X, Current_Y) has passed the GTK+ drag threshold, and thus
   --  should trigger the beginning of a drag-and-drop operation.
   --  "start_x": X coordinate of start of drag
   --  "start_y": Y coordinate of start of drag
   --  "current_x": current X coordinate
   --  "current_y": current Y coordinate

   procedure Drag_Dest_Add_Image_Targets
      (Widget : not null access Gtk_Widget_Record);
   --  Add the image targets supported by
   --  Gtk.Selection_Data.Gtk_Selection_Data to the target list of the drag
   --  destination. The targets are added with Info = 0. If you need another
   --  value, use Gtk.Target_List.Add_Image_Targets and
   --  gtk_drag_dest_set_target_list.
   --  Since: gtk+ 2.6

   procedure Drag_Dest_Add_Text_Targets
      (Widget : not null access Gtk_Widget_Record);
   --  Add the text targets supported by Gtk.Selection_Data.Gtk_Selection_Data
   --  to the target list of the drag destination. The targets are added with
   --  Info = 0. If you need another value, use
   --  Gtk.Target_List.Add_Text_Targets and gtk_drag_dest_set_target_list.
   --  Since: gtk+ 2.6

   procedure Drag_Dest_Add_Uri_Targets
      (Widget : not null access Gtk_Widget_Record);
   --  Add the URI targets supported by Gtk.Selection_Data.Gtk_Selection_Data
   --  to the target list of the drag destination. The targets are added with
   --  Info = 0. If you need another value, use Gtk.Target_List.Add_Uri_Targets
   --  and gtk_drag_dest_set_target_list.
   --  Since: gtk+ 2.6

   function Drag_Dest_Get_Track_Motion
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Returns whether the widget has been configured to always emit
   --  Gtk.Widget.Gtk_Widget::drag-motion signals.
   --  Since: gtk+ 2.10

   procedure Drag_Dest_Set_Track_Motion
      (Widget       : not null access Gtk_Widget_Record;
       Track_Motion : Boolean);
   --  Tells the widget to emit Gtk.Widget.Gtk_Widget::drag-motion and
   --  Gtk.Widget.Gtk_Widget::drag-leave events regardless of the targets and
   --  the Gtk.Tool_Palette.Dest_Default_Motion flag.
   --  This may be used when a widget wants to do generic actions regardless
   --  of the targets that the source offers.
   --  Since: gtk+ 2.10
   --  "track_motion": whether to accept all targets

   procedure Drag_Dest_Set_Proxy
      (Widget          : not null access Gtk_Widget_Record;
       Proxy_Window    : Gdk.Gdk_Window;
       Protocol        : Gdk.Drag_Contexts.Gdk_Drag_Protocol;
       Use_Coordinates : Boolean);
   pragma Obsolescent (Drag_Dest_Set_Proxy);
   --  Sets this widget as a proxy for drops to another window.
   --  Deprecated since 3.22, 1
   --  "proxy_window": the window to which to forward drag events
   --  "protocol": the drag protocol which the Proxy_Window accepts (You can
   --  use gdk_drag_get_protocol to determine this)
   --  "use_coordinates": If True, send the same coordinates to the
   --  destination, because it is an embedded subwindow.

   procedure Drag_Dest_Unset (Widget : not null access Gtk_Widget_Record);
   --  Clears information about a drop destination set with gtk_drag_dest_set.
   --  The widget will no longer receive notification of drags.

   procedure Drag_Get_Data
      (Widget  : not null access Gtk_Widget_Record;
       Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class;
       Target  : Gdk.Types.Gdk_Atom;
       Time    : Guint32);
   --  Gets the data associated with a drag. When the data is received or the
   --  retrieval fails, GTK+ will emit a
   --  Gtk.Widget.Gtk_Widget::drag-data-received signal. Failure of the
   --  retrieval is indicated by the length field of the Selection_Data signal
   --  parameter being negative. However, when Gtk.Widget.Drag_Get_Data is
   --  called implicitely because the Gtk.Tool_Palette.Dest_Default_Drop was
   --  set, then the widget will not receive notification of failed drops.
   --  "context": the drag context
   --  "target": the target (form of the data) to retrieve
   --  "time_": a timestamp for retrieving the data. This will generally be
   --  the time received in a Gtk.Widget.Gtk_Widget::drag-motion or
   --  Gtk.Widget.Gtk_Widget::drag-drop signal

   procedure Drag_Highlight (Widget : not null access Gtk_Widget_Record);
   --  Highlights a widget as a currently hovered drop target. To end the
   --  highlight, call Gtk.Widget.Drag_Unhighlight. GTK+ calls this
   --  automatically if Gtk.Tool_Palette.Dest_Default_Highlight is set.

   procedure Drag_Source_Add_Image_Targets
      (Widget : not null access Gtk_Widget_Record);
   --  Add the writable image targets supported by
   --  Gtk.Selection_Data.Gtk_Selection_Data to the target list of the drag
   --  source. The targets are added with Info = 0. If you need another value,
   --  use Gtk.Target_List.Add_Image_Targets and
   --  gtk_drag_source_set_target_list.
   --  Since: gtk+ 2.6

   procedure Drag_Source_Add_Text_Targets
      (Widget : not null access Gtk_Widget_Record);
   --  Add the text targets supported by Gtk.Selection_Data.Gtk_Selection_Data
   --  to the target list of the drag source. The targets are added with Info =
   --  0. If you need another value, use Gtk.Target_List.Add_Text_Targets and
   --  gtk_drag_source_set_target_list.
   --  Since: gtk+ 2.6

   procedure Drag_Source_Add_Uri_Targets
      (Widget : not null access Gtk_Widget_Record);
   --  Add the URI targets supported by Gtk.Selection_Data.Gtk_Selection_Data
   --  to the target list of the drag source. The targets are added with Info =
   --  0. If you need another value, use Gtk.Target_List.Add_Uri_Targets and
   --  gtk_drag_source_set_target_list.
   --  Since: gtk+ 2.6

   procedure Drag_Source_Set_Icon_Name
      (Widget    : not null access Gtk_Widget_Record;
       Icon_Name : UTF8_String);
   --  Sets the icon that will be used for drags from a particular source to a
   --  themed icon. See the docs for Gtk.Icon_Theme.Gtk_Icon_Theme for more
   --  details.
   --  Since: gtk+ 2.8
   --  "icon_name": name of icon to use

   procedure Drag_Source_Set_Icon_Pixbuf
      (Widget : not null access Gtk_Widget_Record;
       Pixbuf : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Sets the icon that will be used for drags from a particular widget from
   --  a Gdk.Pixbuf.Gdk_Pixbuf. GTK+ retains a reference for Pixbuf and will
   --  release it when it is no longer needed.
   --  "pixbuf": the Gdk.Pixbuf.Gdk_Pixbuf for the drag icon

   procedure Drag_Source_Set_Icon_Stock
      (Widget   : not null access Gtk_Widget_Record;
       Stock_Id : UTF8_String);
   pragma Obsolescent (Drag_Source_Set_Icon_Stock);
   --  Sets the icon that will be used for drags from a particular source to a
   --  stock icon.
   --  Deprecated since 3.10, 1
   --  "stock_id": the ID of the stock icon to use

   procedure Drag_Source_Unset (Widget : not null access Gtk_Widget_Record);
   --  Undoes the effects of gtk_drag_source_set.

   procedure Drag_Unhighlight (Widget : not null access Gtk_Widget_Record);
   --  Removes a highlight set by Gtk.Widget.Drag_Highlight from a widget.

   procedure Draw
      (Widget : not null access Gtk_Widget_Record;
       Cr     : Cairo.Cairo_Context);
   --  Draws Widget to Cr. The top left corner of the widget will be drawn to
   --  the currently set origin point of Cr.
   --  You should pass a cairo context as Cr argument that is in an original
   --  state. Otherwise the resulting drawing is undefined. For example
   --  changing the operator using cairo_set_operator or the line width using
   --  cairo_set_line_width might have unwanted side effects. You may however
   --  change the context's transform matrix - like with cairo_scale,
   --  cairo_translate or cairo_set_matrix and clip region with cairo_clip
   --  prior to calling this function. Also, it is fine to modify the context
   --  with cairo_save and cairo_push_group prior to calling this function.
   --  Note that special-purpose widgets may contain special code for
   --  rendering to the screen and might appear differently on screen and when
   --  rendered using Gtk.Widget.Draw.
   --  Since: gtk+ 3.0
   --  "cr": a cairo context to draw to

   procedure Ensure_Style (Widget : not null access Gtk_Widget_Record);
   pragma Obsolescent (Ensure_Style);
   --  Ensures that Widget has a style (Widget->style).
   --  Not a very useful function; most of the time, if you want the style,
   --  the widget is realized, and realized widgets are guaranteed to have a
   --  style already.
   --  Deprecated since 3.0, 1

   procedure Error_Bell (Widget : not null access Gtk_Widget_Record);
   --  Notifies the user about an input-related error on this widget. If the
   --  Gtk.Settings.Gtk_Settings:gtk-error-bell setting is True, it calls
   --  Gdk.Window.Beep, otherwise it does nothing.
   --  Note that the effect of Gdk.Window.Beep can be configured in many ways,
   --  depending on the windowing backend and the desktop environment or window
   --  manager that is used.
   --  Since: gtk+ 2.12

   function Event
      (Widget : not null access Gtk_Widget_Record;
       Event  : Gdk.Event.Gdk_Event) return Boolean;
   --  Rarely-used function. This function is used to emit the event signals
   --  on a widget (those signals should never be emitted without using this
   --  function to do so). If you want to synthesize an event though, don't use
   --  this function; instead, use Gtk.Main.Main_Do_Event so the event will
   --  behave as if it were in the event queue. Don't synthesize expose events;
   --  instead, use Gdk.Window.Invalidate_Rect to invalidate a region of the
   --  window.
   --  "event": a Gdk.Event.Gdk_Event

   procedure Freeze_Child_Notify
      (Widget : not null access Gtk_Widget_Record);
   --  Stops emission of Gtk.Widget.Gtk_Widget::child-notify signals on
   --  Widget. The signals are queued until Gtk.Widget.Thaw_Child_Notify is
   --  called on Widget.
   --  This is the analogue of g_object_freeze_notify for child properties.

   function Get_Action_Group
      (Widget : not null access Gtk_Widget_Record;
       Prefix : UTF8_String) return Glib.Action_Group.Gaction_Group;
   --  Retrieves the Glib.Action_Group.Gaction_Group that was registered using
   --  Prefix. The resulting Glib.Action_Group.Gaction_Group may have been
   --  registered to Widget or any Gtk.Widget.Gtk_Widget in its ancestry.
   --  If no action group was found matching Prefix, then null is returned.
   --  Since: gtk+ 3.16
   --  "prefix": The "prefix" of the action group.

   function Get_Allocated_Baseline
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   --  Returns the baseline that has currently been allocated to Widget. This
   --  function is intended to be used when implementing handlers for the
   --  Gtk.Widget.Gtk_Widget::draw function, and when allocating child widgets
   --  in Gtk.Widget.Gtk_Widget::size_allocate.
   --  Since: gtk+ 3.10

   function Get_Allocated_Height
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   --  Returns the height that has currently been allocated to Widget. This
   --  function is intended to be used when implementing handlers for the
   --  Gtk.Widget.Gtk_Widget::draw function.

   procedure Get_Allocated_Size
      (Widget     : not null access Gtk_Widget_Record;
       Allocation : out Gtk_Allocation;
       Baseline   : out Glib.Gint);
   --  Retrieves the widget's allocated size.
   --  This function returns the last values passed to
   --  Gtk.Widget.Size_Allocate_With_Baseline. The value differs from the size
   --  returned in Gtk.Widget.Get_Allocation in that functions like
   --  Gtk.Widget.Set_Halign can adjust the allocation, but not the value
   --  returned by this function.
   --  If a widget is not visible, its allocated size is 0.
   --  Since: gtk+ 3.20
   --  "allocation": a pointer to a Gtk_Allocation to copy to
   --  "baseline": a pointer to an integer to copy to

   function Get_Allocated_Width
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   --  Returns the width that has currently been allocated to Widget. This
   --  function is intended to be used when implementing handlers for the
   --  Gtk.Widget.Gtk_Widget::draw function.

   procedure Get_Allocation
      (Widget     : not null access Gtk_Widget_Record;
       Allocation : out Gtk_Allocation);
   --  Retrieves the widget's allocation.
   --  Note, when implementing a Gtk.Container.Gtk_Container: a widget's
   --  allocation will be its "adjusted" allocation, that is, the widget's
   --  parent container typically calls Gtk.Widget.Size_Allocate with an
   --  allocation, and that allocation is then adjusted (to handle margin and
   --  alignment for example) before assignment to the widget.
   --  Gtk.Widget.Get_Allocation returns the adjusted allocation that was
   --  actually assigned to the widget. The adjusted allocation is guaranteed
   --  to be completely contained within the Gtk.Widget.Size_Allocate
   --  allocation, however. So a Gtk.Container.Gtk_Container is guaranteed that
   --  its children stay inside the assigned bounds, but not that they have
   --  exactly the bounds the container assigned. There is no way to get the
   --  original allocation assigned by Gtk.Widget.Size_Allocate, since it isn't
   --  stored; if a container implementation needs that information it will
   --  have to track it itself.
   --  Since: gtk+ 2.18
   --  "allocation": a pointer to a Gtk_Allocation to copy to

   procedure Set_Allocation
      (Widget     : not null access Gtk_Widget_Record;
       Allocation : in out Gtk_Allocation);
   --  Sets the widget's allocation. This should not be used directly, but
   --  from within a widget's size_allocate method.
   --  The allocation set should be the "adjusted" or actual allocation. If
   --  you're implementing a Gtk.Container.Gtk_Container, you want to use
   --  Gtk.Widget.Size_Allocate instead of Gtk.Widget.Set_Allocation. The
   --  GtkWidgetClass::adjust_size_allocation virtual method adjusts the
   --  allocation inside Gtk.Widget.Size_Allocate to create an adjusted
   --  allocation.
   --  Since: gtk+ 2.18
   --  "allocation": a pointer to a Gtk_Allocation to copy from

   function Get_Ancestor
      (Widget      : not null access Gtk_Widget_Record;
       Widget_Type : GType) return Gtk_Widget;
   --  Gets the first ancestor of Widget with type Widget_Type. For example,
   --  `gtk_widget_get_ancestor (widget, GTK_TYPE_BOX)` gets the first
   --  Gtk.Box.Gtk_Box that's an ancestor of Widget. No reference will be added
   --  to the returned widget; it should not be unreferenced. See note about
   --  checking for a toplevel Gtk.Window.Gtk_Window in the docs for
   --  Gtk.Widget.Get_Toplevel.
   --  Note that unlike Gtk.Widget.Is_Ancestor, Gtk.Widget.Get_Ancestor
   --  considers Widget to be an ancestor of itself.
   --  "widget_type": ancestor type

   function Get_App_Paintable
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether the application intends to draw on the widget in an
   --  Gtk.Widget.Gtk_Widget::draw handler.
   --  See Gtk.Widget.Set_App_Paintable
   --  Since: gtk+ 2.18

   procedure Set_App_Paintable
      (Widget        : not null access Gtk_Widget_Record;
       App_Paintable : Boolean);
   --  Sets whether the application intends to draw on the widget in an
   --  Gtk.Widget.Gtk_Widget::draw handler.
   --  This is a hint to the widget and does not affect the behavior of the
   --  GTK+ core; many widgets ignore this flag entirely. For widgets that do
   --  pay attention to the flag, such as Gtk.Event_Box.Gtk_Event_Box and
   --  Gtk.Window.Gtk_Window, the effect is to suppress default themed drawing
   --  of the widget's background. (Children of the widget will still be
   --  drawn.) The application is then entirely responsible for drawing the
   --  widget background.
   --  Note that the background is still drawn when the widget is mapped.
   --  "app_paintable": True if the application will paint on the widget

   function Get_Can_Default
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether Widget can be a default widget. See
   --  Gtk.Widget.Set_Can_Default.
   --  Since: gtk+ 2.18

   procedure Set_Can_Default
      (Widget      : not null access Gtk_Widget_Record;
       Can_Default : Boolean);
   --  Specifies whether Widget can be a default widget. See
   --  Gtk.Widget.Grab_Default for details about the meaning of "default".
   --  Since: gtk+ 2.18
   --  "can_default": whether or not Widget can be a default widget.

   function Get_Can_Focus
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether Widget can own the input focus. See
   --  Gtk.Widget.Set_Can_Focus.
   --  Since: gtk+ 2.18

   procedure Set_Can_Focus
      (Widget    : not null access Gtk_Widget_Record;
       Can_Focus : Boolean);
   --  Specifies whether Widget can own the input focus. See
   --  Gtk.Widget.Grab_Focus for actually setting the input focus on a widget.
   --  Since: gtk+ 2.18
   --  "can_focus": whether or not Widget can own the input focus.

   procedure Get_Child_Requisition
      (Widget      : not null access Gtk_Widget_Record;
       Requisition : out Gtk_Requisition);
   pragma Obsolescent (Get_Child_Requisition);
   --  This function is only for use in widget implementations. Obtains
   --  Widget->requisition, unless someone has forced a particular geometry on
   --  the widget (e.g. with Gtk.Widget.Set_Size_Request), in which case it
   --  returns that geometry instead of the widget's requisition.
   --  This function differs from Gtk.Widget.Size_Request in that it retrieves
   --  the last size request value from Widget->requisition, while
   --  Gtk.Widget.Size_Request actually calls the "size_request" method on
   --  Widget to compute the size request and fill in Widget->requisition, and
   --  only then returns Widget->requisition.
   --  Because this function does not call the "size_request" method, it can
   --  only be used when you know that Widget->requisition is up-to-date, that
   --  is, Gtk.Widget.Size_Request has been called since the last time a resize
   --  was queued. In general, only container implementations have this
   --  information; applications should use Gtk.Widget.Size_Request.
   --  Deprecated since 3.0, 1
   --  "requisition": a Gtk.Widget.Gtk_Requisition to be filled in

   function Get_Child_Visible
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Gets the value set with Gtk.Widget.Set_Child_Visible. If you feel a
   --  need to use this function, your code probably needs reorganization.
   --  This function is only useful for container implementations and never
   --  should be called by an application.

   procedure Set_Child_Visible
      (Widget     : not null access Gtk_Widget_Record;
       Is_Visible : Boolean);
   --  Sets whether Widget should be mapped along with its when its parent is
   --  mapped and Widget has been shown with Gtk.Widget.Show.
   --  The child visibility can be set for widget before it is added to a
   --  container with Gtk.Widget.Set_Parent, to avoid mapping children
   --  unnecessary before immediately unmapping them. However it will be reset
   --  to its default state of True when the widget is removed from a
   --  container.
   --  Note that changing the child visibility of a widget does not queue a
   --  resize on the widget. Most of the time, the size of a widget is computed
   --  from all visible children, whether or not they are mapped. If this is
   --  not the case, the container can queue a resize itself.
   --  This function is only useful for container implementations and never
   --  should be called by an application.
   --  "is_visible": if True, Widget should be mapped along with its parent.

   procedure Get_Clip
      (Widget : not null access Gtk_Widget_Record;
       Clip   : out Gtk_Allocation);
   --  Retrieves the widget's clip area.
   --  The clip area is the area in which all of Widget's drawing will happen.
   --  Other toolkits call it the bounding box.
   --  Historically, in GTK+ the clip area has been equal to the allocation
   --  retrieved via Gtk.Widget.Get_Allocation.
   --  Since: gtk+ 3.14
   --  "clip": a pointer to a Gtk_Allocation to copy to

   procedure Set_Clip
      (Widget : not null access Gtk_Widget_Record;
       Clip   : in out Gtk_Allocation);
   --  Sets the widget's clip. This must not be used directly, but from within
   --  a widget's size_allocate method. It must be called after
   --  Gtk.Widget.Set_Allocation (or after chaining up to the parent class),
   --  because that function resets the clip.
   --  The clip set should be the area that Widget draws on. If Widget is a
   --  Gtk.Container.Gtk_Container, the area must contain all children's clips.
   --  If this function is not called by Widget during a ::size-allocate
   --  handler, the clip will be set to Widget's allocation.
   --  Since: gtk+ 3.14
   --  "clip": a pointer to a Gtk_Allocation to copy from

   function Get_Composite_Name
      (Widget : not null access Gtk_Widget_Record) return UTF8_String;
   pragma Obsolescent (Get_Composite_Name);
   --  Obtains the composite name of a widget.
   --  Deprecated since 3.10, 1

   procedure Set_Composite_Name
      (Widget : not null access Gtk_Widget_Record;
       Name   : UTF8_String);
   pragma Obsolescent (Set_Composite_Name);
   --  Sets a widgets composite name. The widget must be a composite child of
   --  its parent; see Gtk.Widget.Push_Composite_Child.
   --  Deprecated since 3.10, 1
   --  "name": the name to set

   function Get_Device_Enabled
      (Widget : not null access Gtk_Widget_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class)
       return Boolean;
   --  Returns whether Device can interact with Widget and its children. See
   --  Gtk.Widget.Set_Device_Enabled.
   --  Since: gtk+ 3.0
   --  "device": a Gdk.Device.Gdk_Device

   procedure Set_Device_Enabled
      (Widget  : not null access Gtk_Widget_Record;
       Device  : not null access Gdk.Device.Gdk_Device_Record'Class;
       Enabled : Boolean);
   --  Enables or disables a Gdk.Device.Gdk_Device to interact with Widget and
   --  all its children.
   --  It does so by descending through the Gdk.Gdk_Window hierarchy and
   --  enabling the same mask that is has for core events (i.e. the one that
   --  Gdk.Window.Get_Events returns).
   --  Since: gtk+ 3.0
   --  "device": a Gdk.Device.Gdk_Device
   --  "enabled": whether to enable the device

   function Get_Device_Events
      (Widget : not null access Gtk_Widget_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class)
       return Gdk.Event.Gdk_Event_Mask;
   --  Returns the events mask for the widget corresponding to an specific
   --  device. These are the events that the widget will receive when Device
   --  operates on it.
   --  Since: gtk+ 3.0
   --  "device": a Gdk.Device.Gdk_Device

   procedure Set_Device_Events
      (Widget : not null access Gtk_Widget_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class;
       Events : Gdk.Event.Gdk_Event_Mask);
   --  Sets the device event mask (see Gdk.Event.Gdk_Event_Mask) for a widget.
   --  The event mask determines which events a widget will receive from
   --  Device. Keep in mind that different widgets have different default event
   --  masks, and by changing the event mask you may disrupt a widget's
   --  functionality, so be careful. This function must be called while a
   --  widget is unrealized. Consider Gtk.Widget.Add_Device_Events for widgets
   --  that are already realized, or if you want to preserve the existing event
   --  mask. This function can't be used with windowless widgets (which return
   --  False from Gtk.Widget.Get_Has_Window); to get events on those widgets,
   --  place them inside a Gtk.Event_Box.Gtk_Event_Box and receive events on
   --  the event box.
   --  Since: gtk+ 3.0
   --  "device": a Gdk.Device.Gdk_Device
   --  "events": event mask

   function Get_Direction
      (Widget : not null access Gtk_Widget_Record)
       return Gtk.Enums.Gtk_Text_Direction;
   --  Gets the reading direction for a particular widget. See
   --  Gtk.Widget.Set_Direction.

   procedure Set_Direction
      (Widget : not null access Gtk_Widget_Record;
       Dir    : Gtk.Enums.Gtk_Text_Direction);
   --  Sets the reading direction on a particular widget. This direction
   --  controls the primary direction for widgets containing text, and also the
   --  direction in which the children of a container are packed. The ability
   --  to set the direction is present in order so that correct localization
   --  into languages with right-to-left reading directions can be done.
   --  Generally, applications will let the default reading direction present,
   --  except for containers where the containers are arranged in an order that
   --  is explicitly visual rather than logical (such as buttons for text
   --  justification).
   --  If the direction is set to Gtk.Enums.Text_Dir_None, then the value set
   --  by Gtk.Widget.Set_Default_Direction will be used.
   --  "dir": the new direction

   function Get_Display
      (Widget : not null access Gtk_Widget_Record)
       return Gdk.Display.Gdk_Display;
   --  Get the Gdk.Display.Gdk_Display for the toplevel window associated with
   --  this widget. This function can only be called after the widget has been
   --  added to a widget hierarchy with a Gtk.Window.Gtk_Window at the top.
   --  In general, you should only create display specific resources when a
   --  widget has been realized, and you should free those resources when the
   --  widget is unrealized.
   --  Since: gtk+ 2.2

   function Get_Double_Buffered
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether the widget is double buffered.
   --  See Gtk.Widget.Set_Double_Buffered
   --  Since: gtk+ 2.18

   procedure Set_Double_Buffered
      (Widget          : not null access Gtk_Widget_Record;
       Double_Buffered : Boolean);
   pragma Obsolescent (Set_Double_Buffered);
   --  Widgets are double buffered by default; you can use this function to
   --  turn off the buffering. "Double buffered" simply means that
   --  Gdk.Window.Begin_Draw_Frame and Gdk.Window.End_Draw_Frame are called
   --  automatically around expose events sent to the widget.
   --  Gdk.Window.Begin_Draw_Frame diverts all drawing to a widget's window to
   --  an offscreen buffer, and Gdk.Window.End_Draw_Frame draws the buffer to
   --  the screen. The result is that users see the window update in one smooth
   --  step, and don't see individual graphics primitives being rendered.
   --  In very simple terms, double buffered widgets don't flicker, so you
   --  would only use this function to turn off double buffering if you had
   --  special needs and really knew what you were doing.
   --  Note: if you turn off double-buffering, you have to handle expose
   --  events, since even the clearing to the background color or pixmap will
   --  not happen automatically (as it is done in Gdk.Window.Begin_Draw_Frame).
   --  In 3.10 GTK and GDK have been restructured for translucent drawing.
   --  Since then expose events for double-buffered widgets are culled into a
   --  single event to the toplevel GDK window. If you now unset double
   --  buffering, you will cause a separate rendering pass for every widget.
   --  This will likely cause rendering problems - in particular related to
   --  stacking - and usually increases rendering times significantly.
   --  Deprecated since 3.14, 1
   --  "double_buffered": True to double-buffer a widget

   function Get_Events
      (Widget : not null access Gtk_Widget_Record)
       return Gdk.Event.Gdk_Event_Mask;
   --  Returns the event mask (see Gdk.Event.Gdk_Event_Mask) for the widget.
   --  These are the events that the widget will receive.
   --  Note: Internally, the widget event mask will be the logical OR of the
   --  event mask set through Gtk.Widget.Set_Events or Gtk.Widget.Add_Events,
   --  and the event mask necessary to cater for every
   --  Gtk.Event_Controller.Gtk_Event_Controller created for the widget.

   procedure Set_Events
      (Widget : not null access Gtk_Widget_Record;
       Events : Gdk.Event.Gdk_Event_Mask);
   --  Sets the event mask (see Gdk.Event.Gdk_Event_Mask) for a widget. The
   --  event mask determines which events a widget will receive. Keep in mind
   --  that different widgets have different default event masks, and by
   --  changing the event mask you may disrupt a widget's functionality, so be
   --  careful. This function must be called while a widget is unrealized.
   --  Consider Gtk.Widget.Add_Events for widgets that are already realized, or
   --  if you want to preserve the existing event mask. This function can't be
   --  used with widgets that have no window. (See Gtk.Widget.Get_Has_Window).
   --  To get events on those widgets, place them inside a
   --  Gtk.Event_Box.Gtk_Event_Box and receive events on the event box.
   --  "events": event mask

   function Get_Focus_On_Click
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Returns whether the widget should grab focus when it is clicked with
   --  the mouse. See Gtk.Widget.Set_Focus_On_Click.
   --  Since: gtk+ 3.20

   procedure Set_Focus_On_Click
      (Widget         : not null access Gtk_Widget_Record;
       Focus_On_Click : Boolean);
   --  Sets whether the widget should grab focus when it is clicked with the
   --  mouse. Making mouse clicks not grab focus is useful in places like
   --  toolbars where you don't want the keyboard focus removed from the main
   --  area of the application.
   --  Since: gtk+ 3.20
   --  "focus_on_click": whether the widget should grab focus when clicked
   --  with the mouse

   function Get_Font_Map
      (Widget : not null access Gtk_Widget_Record)
       return Pango.Font_Map.Pango_Font_Map;
   --  Gets the font map that has been set with Gtk.Widget.Set_Font_Map.
   --  Since: gtk+ 3.18

   procedure Set_Font_Map
      (Widget   : not null access Gtk_Widget_Record;
       Font_Map : access Pango.Font_Map.Pango_Font_Map_Record'Class);
   --  Sets the font map to use for Pango rendering. When not set, the widget
   --  will inherit the font map from its parent.
   --  Since: gtk+ 3.18
   --  "font_map": a Pango.Font_Map.Pango_Font_Map, or null to unset any
   --  previously set font map

   function Get_Font_Options
      (Widget : not null access Gtk_Widget_Record)
       return Cairo.Cairo_Font_Options;
   --  Returns the Cairo.Cairo_Font_Options used for Pango rendering. When not
   --  set, the defaults font options for the Gdk.Screen.Gdk_Screen will be
   --  used.
   --  Since: gtk+ 3.18

   procedure Set_Font_Options
      (Widget  : not null access Gtk_Widget_Record;
       Options : in out Cairo.Cairo_Font_Options);
   --  Sets the Cairo.Cairo_Font_Options used for Pango rendering in this
   --  widget. When not set, the default font options for the
   --  Gdk.Screen.Gdk_Screen will be used.
   --  Since: gtk+ 3.18
   --  "options": a Cairo.Cairo_Font_Options, or null to unset any previously
   --  set default font options.

   function Get_Frame_Clock
      (Widget : not null access Gtk_Widget_Record)
       return Gdk.Frame_Clock.Gdk_Frame_Clock;
   --  Obtains the frame clock for a widget. The frame clock is a global
   --  "ticker" that can be used to drive animations and repaints. The most
   --  common reason to get the frame clock is to call
   --  Gdk.Frame_Clock.Get_Frame_Time, in order to get a time to use for
   --  animating. For example you might record the start of the animation with
   --  an initial value from Gdk.Frame_Clock.Get_Frame_Time, and then update
   --  the animation by calling Gdk.Frame_Clock.Get_Frame_Time again during
   --  each repaint.
   --  Gdk.Frame_Clock.Request_Phase will result in a new frame on the clock,
   --  but won't necessarily repaint any widgets. To repaint a widget, you have
   --  to use Gtk.Widget.Queue_Draw which invalidates the widget (thus
   --  scheduling it to receive a draw on the next frame).
   --  Gtk.Widget.Queue_Draw will also end up requesting a frame on the
   --  appropriate frame clock.
   --  A widget's frame clock will not change while the widget is mapped.
   --  Reparenting a widget (which implies a temporary unmap) can change the
   --  widget's frame clock.
   --  Unrealized widgets do not have a frame clock.
   --  Since: gtk+ 3.8

   function Get_Halign
      (Widget : not null access Gtk_Widget_Record) return Gtk_Align;
   --  Gets the value of the Gtk.Widget.Gtk_Widget:halign property.
   --  For backwards compatibility reasons this method will never return
   --  Gtk.Widget.Align_Baseline, but instead it will convert it to
   --  Gtk.Widget.Align_Fill. Baselines are not supported for horizontal
   --  alignment.

   procedure Set_Halign
      (Widget : not null access Gtk_Widget_Record;
       Align  : Gtk_Align);
   --  Sets the horizontal alignment of Widget. See the
   --  Gtk.Widget.Gtk_Widget:halign property.
   --  "align": the horizontal alignment

   function Get_Has_Tooltip
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Returns the current value of the has-tooltip property. See
   --  Gtk.Widget.Gtk_Widget:has-tooltip for more information.
   --  Since: gtk+ 2.12

   procedure Set_Has_Tooltip
      (Widget      : not null access Gtk_Widget_Record;
       Has_Tooltip : Boolean);
   --  Sets the has-tooltip property on Widget to Has_Tooltip. See
   --  Gtk.Widget.Gtk_Widget:has-tooltip for more information.
   --  Since: gtk+ 2.12
   --  "has_tooltip": whether or not Widget has a tooltip.

   function Get_Has_Window
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether Widget has a Gdk.Gdk_Window of its own. See
   --  Gtk.Widget.Set_Has_Window.
   --  Since: gtk+ 2.18

   procedure Set_Has_Window
      (Widget     : not null access Gtk_Widget_Record;
       Has_Window : Boolean);
   --  Specifies whether Widget has a Gdk.Gdk_Window of its own. Note that all
   --  realized widgets have a non-null "window" pointer (gtk_widget_get_window
   --  never returns a null window when a widget is realized), but for many of
   --  them it's actually the Gdk.Gdk_Window of one of its parent widgets.
   --  Widgets that do not create a %window for themselves in
   --  Gtk.Widget.Gtk_Widget::realize must announce this by calling this
   --  function with Has_Window = False.
   --  This function should only be called by widget implementations, and they
   --  should call it in their init function.
   --  Since: gtk+ 2.18
   --  "has_window": whether or not Widget has a window.

   function Get_Hexpand
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Gets whether the widget would like any available extra horizontal
   --  space. When a user resizes a Gtk.Window.Gtk_Window, widgets with
   --  expand=TRUE generally receive the extra space. For example, a list or
   --  scrollable area or document in your window would often be set to expand.
   --  Containers should use Gtk.Widget.Compute_Expand rather than this
   --  function, to see whether a widget, or any of its children, has the
   --  expand flag set. If any child of a widget wants to expand, the parent
   --  may ask to expand also.
   --  This function only looks at the widget's own hexpand flag, rather than
   --  computing whether the entire widget tree rooted at this widget wants to
   --  expand.

   procedure Set_Hexpand
      (Widget : not null access Gtk_Widget_Record;
       Expand : Boolean);
   --  Sets whether the widget would like any available extra horizontal
   --  space. When a user resizes a Gtk.Window.Gtk_Window, widgets with
   --  expand=TRUE generally receive the extra space. For example, a list or
   --  scrollable area or document in your window would often be set to expand.
   --  Call this function to set the expand flag if you would like your widget
   --  to become larger horizontally when the window has extra room.
   --  By default, widgets automatically expand if any of their children want
   --  to expand. (To see if a widget will automatically expand given its
   --  current children and state, call Gtk.Widget.Compute_Expand. A container
   --  can decide how the expandability of children affects the expansion of
   --  the container by overriding the compute_expand virtual method on
   --  Gtk.Widget.Gtk_Widget.).
   --  Setting hexpand explicitly with this function will override the
   --  automatic expand behavior.
   --  This function forces the widget to expand or not to expand, regardless
   --  of children. The override occurs because Gtk.Widget.Set_Hexpand sets the
   --  hexpand-set property (see Gtk.Widget.Set_Hexpand_Set) which causes the
   --  widget's hexpand value to be used, rather than looking at children and
   --  widget state.
   --  "expand": whether to expand

   function Get_Hexpand_Set
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Gets whether Gtk.Widget.Set_Hexpand has been used to explicitly set the
   --  expand flag on this widget.
   --  If hexpand is set, then it overrides any computed expand value based on
   --  child widgets. If hexpand is not set, then the expand value depends on
   --  whether any children of the widget would like to expand.
   --  There are few reasons to use this function, but it's here for
   --  completeness and consistency.

   procedure Set_Hexpand_Set
      (Widget : not null access Gtk_Widget_Record;
       Set    : Boolean);
   --  Sets whether the hexpand flag (see Gtk.Widget.Get_Hexpand) will be
   --  used.
   --  The hexpand-set property will be set automatically when you call
   --  Gtk.Widget.Set_Hexpand to set hexpand, so the most likely reason to use
   --  this function would be to unset an explicit expand flag.
   --  If hexpand is set, then it overrides any computed expand value based on
   --  child widgets. If hexpand is not set, then the expand value depends on
   --  whether any children of the widget would like to expand.
   --  There are few reasons to use this function, but it's here for
   --  completeness and consistency.
   --  "set": value for hexpand-set property

   function Get_Mapped
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Whether the widget is mapped.
   --  Since: gtk+ 2.20

   procedure Set_Mapped
      (Widget : not null access Gtk_Widget_Record;
       Mapped : Boolean);
   --  Marks the widget as being mapped.
   --  This function should only ever be called in a derived widget's "map" or
   --  "unmap" implementation.
   --  Since: gtk+ 2.20
   --  "mapped": True to mark the widget as mapped

   function Get_Margin_Bottom
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   --  Gets the value of the Gtk.Widget.Gtk_Widget:margin-bottom property.
   --  Since: gtk+ 3.0

   procedure Set_Margin_Bottom
      (Widget : not null access Gtk_Widget_Record;
       Margin : Glib.Gint);
   --  Sets the bottom margin of Widget. See the
   --  Gtk.Widget.Gtk_Widget:margin-bottom property.
   --  Since: gtk+ 3.0
   --  "margin": the bottom margin

   function Get_Margin_End
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   --  Gets the value of the Gtk.Widget.Gtk_Widget:margin-end property.
   --  Since: gtk+ 3.12

   procedure Set_Margin_End
      (Widget : not null access Gtk_Widget_Record;
       Margin : Glib.Gint);
   --  Sets the end margin of Widget. See the Gtk.Widget.Gtk_Widget:margin-end
   --  property.
   --  Since: gtk+ 3.12
   --  "margin": the end margin

   function Get_Margin_Left
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   pragma Obsolescent (Get_Margin_Left);
   --  Gets the value of the Gtk.Widget.Gtk_Widget:margin-left property.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.12, 1

   procedure Set_Margin_Left
      (Widget : not null access Gtk_Widget_Record;
       Margin : Glib.Gint);
   pragma Obsolescent (Set_Margin_Left);
   --  Sets the left margin of Widget. See the
   --  Gtk.Widget.Gtk_Widget:margin-left property.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.12, 1
   --  "margin": the left margin

   function Get_Margin_Right
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   pragma Obsolescent (Get_Margin_Right);
   --  Gets the value of the Gtk.Widget.Gtk_Widget:margin-right property.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.12, 1

   procedure Set_Margin_Right
      (Widget : not null access Gtk_Widget_Record;
       Margin : Glib.Gint);
   pragma Obsolescent (Set_Margin_Right);
   --  Sets the right margin of Widget. See the
   --  Gtk.Widget.Gtk_Widget:margin-right property.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.12, 1
   --  "margin": the right margin

   function Get_Margin_Start
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   --  Gets the value of the Gtk.Widget.Gtk_Widget:margin-start property.
   --  Since: gtk+ 3.12

   procedure Set_Margin_Start
      (Widget : not null access Gtk_Widget_Record;
       Margin : Glib.Gint);
   --  Sets the start margin of Widget. See the
   --  Gtk.Widget.Gtk_Widget:margin-start property.
   --  Since: gtk+ 3.12
   --  "margin": the start margin

   function Get_Margin_Top
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   --  Gets the value of the Gtk.Widget.Gtk_Widget:margin-top property.
   --  Since: gtk+ 3.0

   procedure Set_Margin_Top
      (Widget : not null access Gtk_Widget_Record;
       Margin : Glib.Gint);
   --  Sets the top margin of Widget. See the Gtk.Widget.Gtk_Widget:margin-top
   --  property.
   --  Since: gtk+ 3.0
   --  "margin": the top margin

   function Get_Modifier_Mask
      (Widget : not null access Gtk_Widget_Record;
       Intent : Gdk_Modifier_Intent) return Gdk.Types.Gdk_Modifier_Type;
   --  Returns the modifier mask the Widget's windowing system backend uses
   --  for a particular purpose.
   --  See gdk_keymap_get_modifier_mask.
   --  Since: gtk+ 3.4
   --  "intent": the use case for the modifier mask

   function Get_Name
      (Widget : not null access Gtk_Widget_Record) return UTF8_String;
   --  Retrieves the name of a widget. See Gtk.Widget.Set_Name for the
   --  significance of widget names.

   procedure Set_Name
      (Widget : not null access Gtk_Widget_Record;
       Name   : UTF8_String);
   --  Widgets can be named, which allows you to refer to them from a CSS
   --  file. You can apply a style to widgets with a particular name in the CSS
   --  file. See the documentation for the CSS syntax (on the same page as the
   --  docs for Gtk.Style_Context.Gtk_Style_Context).
   --  Note that the CSS syntax has certain special characters to delimit and
   --  represent elements in a selector (period, #, >, *...), so using these
   --  will make your widget impossible to match by name. Any combination of
   --  alphanumeric symbols, dashes and underscores will suffice.
   --  "name": name for the widget

   function Get_No_Show_All
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Returns the current value of the Gtk.Widget.Gtk_Widget:no-show-all
   --  property, which determines whether calls to Gtk.Widget.Show_All will
   --  affect this widget.
   --  Since: gtk+ 2.4

   procedure Set_No_Show_All
      (Widget      : not null access Gtk_Widget_Record;
       No_Show_All : Boolean);
   --  Sets the Gtk.Widget.Gtk_Widget:no-show-all property, which determines
   --  whether calls to Gtk.Widget.Show_All will affect this widget.
   --  This is mostly for use in constructing widget hierarchies with
   --  externally controlled visibility, see Gtk.UI_Manager.Gtk_UI_Manager.
   --  Since: gtk+ 2.4
   --  "no_show_all": the new value for the "no-show-all" property

   function Get_Opacity
      (Widget : not null access Gtk_Widget_Record) return Gdouble;
   --  Fetches the requested opacity for this widget. See
   --  Gtk.Widget.Set_Opacity.
   --  Since: gtk+ 3.8

   procedure Set_Opacity
      (Widget  : not null access Gtk_Widget_Record;
       Opacity : Gdouble);
   --  Request the Widget to be rendered partially transparent, with opacity 0
   --  being fully transparent and 1 fully opaque. (Opacity values are clamped
   --  to the [0,1] range.). This works on both toplevel widget, and child
   --  widgets, although there are some limitations:
   --  For toplevel widgets this depends on the capabilities of the windowing
   --  system. On X11 this has any effect only on X screens with a compositing
   --  manager running. See Gtk.Widget.Is_Composited. On Windows it should work
   --  always, although setting a window's opacity after the window has been
   --  shown causes it to flicker once on Windows.
   --  For child widgets it doesn't work if any affected widget has a native
   --  window, or disables double buffering.
   --  Since: gtk+ 3.8
   --  "opacity": desired opacity, between 0 and 1

   function Get_Pango_Context
      (Widget : not null access Gtk_Widget_Record)
       return Pango.Context.Pango_Context;
   --  Gets a Pango.Context.Pango_Context with the appropriate font map, font
   --  description, and base direction for this widget. Unlike the context
   --  returned by Gtk.Widget.Create_Pango_Context, this context is owned by
   --  the widget (it can be used until the screen for the widget changes or
   --  the widget is removed from its toplevel), and will be updated to match
   --  any changes to the widget's attributes. This can be tracked by using the
   --  Gtk.Widget.Gtk_Widget::screen-changed signal on the widget.

   function Get_Parent
      (Widget : not null access Gtk_Widget_Record) return Gtk_Widget;
   --  Returns the parent container of Widget.

   procedure Set_Parent
      (Widget : not null access Gtk_Widget_Record;
       Parent : not null access Gtk_Widget_Record'Class);
   --  This function is useful only when implementing subclasses of
   --  Gtk.Container.Gtk_Container. Sets the container as the parent of Widget,
   --  and takes care of some details such as updating the state and style of
   --  the child to reflect its new location. The opposite function is
   --  Gtk.Widget.Unparent.
   --  "parent": parent container

   function Get_Parent_Window
      (Widget : not null access Gtk_Widget_Record) return Gdk.Gdk_Window;
   --  Gets Widget's parent window, or null if it does not have one.

   procedure Set_Parent_Window
      (Widget        : not null access Gtk_Widget_Record;
       Parent_Window : Gdk.Gdk_Window);
   --  Sets a non default parent window for Widget.
   --  For Gtk.Window.Gtk_Window classes, setting a Parent_Window effects
   --  whether the window is a toplevel window or can be embedded into other
   --  widgets.
   --  For Gtk.Window.Gtk_Window classes, this needs to be called before the
   --  window is realized.
   --  "parent_window": the new parent window.

   function Get_Path
      (Widget : not null access Gtk_Widget_Record) return Gtk_Widget_Path;
   --  Returns the Gtk.Widget.Gtk_Widget_Path representing Widget, if the
   --  widget is not connected to a toplevel widget, a partial path will be
   --  created.

   procedure Get_Pointer
      (Widget : not null access Gtk_Widget_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint);
   pragma Obsolescent (Get_Pointer);
   --  Obtains the location of the mouse pointer in widget coordinates. Widget
   --  coordinates are a bit odd; for historical reasons, they are defined as
   --  Widget->window coordinates for widgets that return True for
   --  Gtk.Widget.Get_Has_Window; and are relative to Widget->allocation.x,
   --  Widget->allocation.y otherwise.
   --  Deprecated since 3.4, 1
   --  "x": return location for the X coordinate, or null
   --  "y": return location for the Y coordinate, or null

   procedure Get_Preferred_Height
      (Widget         : not null access Gtk_Widget_Record;
       Minimum_Height : out Glib.Gint;
       Natural_Height : out Glib.Gint);
   --  Retrieves a widget's initial minimum and natural height.
   --  This call is specific to width-for-height requests.
   --  The returned request will be modified by the
   --  GtkWidgetClass::adjust_size_request virtual method and by any
   --  Gtk_Size_Groups that have been applied. That is, the returned request is
   --  the one that should be used for layout, not necessarily the one returned
   --  by the widget itself.
   --  Since: gtk+ 3.0
   --  "minimum_height": location to store the minimum height, or null
   --  "natural_height": location to store the natural height, or null

   procedure Get_Preferred_Height_And_Baseline_For_Width
      (Widget           : not null access Gtk_Widget_Record;
       Width            : Glib.Gint;
       Minimum_Height   : out Glib.Gint;
       Natural_Height   : out Glib.Gint;
       Minimum_Baseline : out Glib.Gint;
       Natural_Baseline : out Glib.Gint);
   --  Retrieves a widget's minimum and natural height and the corresponding
   --  baselines if it would be given the specified Width, or the default
   --  height if Width is -1. The baselines may be -1 which means that no
   --  baseline is requested for this widget.
   --  The returned request will be modified by the
   --  GtkWidgetClass::adjust_size_request and
   --  GtkWidgetClass::adjust_baseline_request virtual methods and by any
   --  Gtk_Size_Groups that have been applied. That is, the returned request is
   --  the one that should be used for layout, not necessarily the one returned
   --  by the widget itself.
   --  Since: gtk+ 3.10
   --  "width": the width which is available for allocation, or -1 if none
   --  "minimum_height": location for storing the minimum height, or null
   --  "natural_height": location for storing the natural height, or null
   --  "minimum_baseline": location for storing the baseline for the minimum
   --  height, or null
   --  "natural_baseline": location for storing the baseline for the natural
   --  height, or null

   procedure Get_Preferred_Height_For_Width
      (Widget         : not null access Gtk_Widget_Record;
       Width          : Glib.Gint;
       Minimum_Height : out Glib.Gint;
       Natural_Height : out Glib.Gint);
   --  Retrieves a widget's minimum and natural height if it would be given
   --  the specified Width.
   --  The returned request will be modified by the
   --  GtkWidgetClass::adjust_size_request virtual method and by any
   --  Gtk_Size_Groups that have been applied. That is, the returned request is
   --  the one that should be used for layout, not necessarily the one returned
   --  by the widget itself.
   --  Since: gtk+ 3.0
   --  "width": the width which is available for allocation
   --  "minimum_height": location for storing the minimum height, or null
   --  "natural_height": location for storing the natural height, or null

   procedure Get_Preferred_Size
      (Widget       : not null access Gtk_Widget_Record;
       Minimum_Size : out Gtk_Requisition;
       Natural_Size : out Gtk_Requisition);
   --  Retrieves the minimum and natural size of a widget, taking into account
   --  the widget's preference for height-for-width management.
   --  This is used to retrieve a suitable size by container widgets which do
   --  not impose any restrictions on the child placement. It can be used to
   --  deduce toplevel window and menu sizes as well as child widgets in
   --  free-form containers such as GtkLayout.
   --  Handle with care. Note that the natural height of a height-for-width
   --  widget will generally be a smaller size than the minimum height, since
   --  the required height for the natural width is generally smaller than the
   --  required height for the minimum width.
   --  Use Gtk.Widget.Get_Preferred_Height_And_Baseline_For_Width if you want
   --  to support baseline alignment.
   --  Since: gtk+ 3.0
   --  "minimum_size": location for storing the minimum size, or null
   --  "natural_size": location for storing the natural size, or null

   procedure Get_Preferred_Width
      (Widget        : not null access Gtk_Widget_Record;
       Minimum_Width : out Glib.Gint;
       Natural_Width : out Glib.Gint);
   --  Retrieves a widget's initial minimum and natural width.
   --  This call is specific to height-for-width requests.
   --  The returned request will be modified by the
   --  GtkWidgetClass::adjust_size_request virtual method and by any
   --  Gtk_Size_Groups that have been applied. That is, the returned request is
   --  the one that should be used for layout, not necessarily the one returned
   --  by the widget itself.
   --  Since: gtk+ 3.0
   --  "minimum_width": location to store the minimum width, or null
   --  "natural_width": location to store the natural width, or null

   procedure Get_Preferred_Width_For_Height
      (Widget        : not null access Gtk_Widget_Record;
       Height        : Glib.Gint;
       Minimum_Width : out Glib.Gint;
       Natural_Width : out Glib.Gint);
   --  Retrieves a widget's minimum and natural width if it would be given the
   --  specified Height.
   --  The returned request will be modified by the
   --  GtkWidgetClass::adjust_size_request virtual method and by any
   --  Gtk_Size_Groups that have been applied. That is, the returned request is
   --  the one that should be used for layout, not necessarily the one returned
   --  by the widget itself.
   --  Since: gtk+ 3.0
   --  "height": the height which is available for allocation
   --  "minimum_width": location for storing the minimum width, or null
   --  "natural_width": location for storing the natural width, or null

   function Get_Realized
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether Widget is realized.
   --  Since: gtk+ 2.20

   procedure Set_Realized
      (Widget   : not null access Gtk_Widget_Record;
       Realized : Boolean);
   --  Marks the widget as being realized. This function must only be called
   --  after all Gdk_Windows for the Widget have been created and registered.
   --  This function should only ever be called in a derived widget's
   --  "realize" or "unrealize" implementation.
   --  Since: gtk+ 2.20
   --  "realized": True to mark the widget as realized

   function Get_Receives_Default
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether Widget is always treated as the default widget
   --  within its toplevel when it has the focus, even if another widget is the
   --  default.
   --  See Gtk.Widget.Set_Receives_Default.
   --  Since: gtk+ 2.18

   procedure Set_Receives_Default
      (Widget           : not null access Gtk_Widget_Record;
       Receives_Default : Boolean);
   --  Specifies whether Widget will be treated as the default widget within
   --  its toplevel when it has the focus, even if another widget is the
   --  default.
   --  See Gtk.Widget.Grab_Default for details about the meaning of "default".
   --  Since: gtk+ 2.18
   --  "receives_default": whether or not Widget can be a default widget.

   function Get_Request_Mode
      (Widget : not null access Gtk_Widget_Record)
       return Gtk.Enums.Gtk_Size_Request_Mode;
   --  Gets whether the widget prefers a height-for-width layout or a
   --  width-for-height layout.
   --  Gtk.Bin.Gtk_Bin widgets generally propagate the preference of their
   --  child, container widgets need to request something either in context of
   --  their children or in context of their allocation capabilities.
   --  Since: gtk+ 3.0

   procedure Get_Requisition
      (Widget      : not null access Gtk_Widget_Record;
       Requisition : out Gtk_Requisition);
   pragma Obsolescent (Get_Requisition);
   --  Retrieves the widget's requisition.
   --  This function should only be used by widget implementations in order to
   --  figure whether the widget's requisition has actually changed after some
   --  internal state change (so that they can call Gtk.Widget.Queue_Resize
   --  instead of Gtk.Widget.Queue_Draw).
   --  Normally, Gtk.Widget.Size_Request should be used.
   --  Since: gtk+ 2.20
   --  Deprecated since 3.0, 1
   --  "requisition": a pointer to a Gtk.Widget.Gtk_Requisition to copy to

   function Get_Root_Window
      (Widget : not null access Gtk_Widget_Record) return Gdk.Gdk_Window;
   pragma Obsolescent (Get_Root_Window);
   --  Get the root window where this widget is located. This function can
   --  only be called after the widget has been added to a widget hierarchy
   --  with Gtk.Window.Gtk_Window at the top.
   --  The root window is useful for such purposes as creating a popup
   --  Gdk.Gdk_Window associated with the window. In general, you should only
   --  create display specific resources when a widget has been realized, and
   --  you should free those resources when the widget is unrealized.
   --  Since: gtk+ 2.2
   --  Deprecated since 3.12, 1

   function Get_Scale_Factor
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint;
   --  Retrieves the internal scale factor that maps from window coordinates
   --  to the actual device pixels. On traditional systems this is 1, on high
   --  density outputs, it can be a higher value (typically 2).
   --  See Gdk.Window.Get_Scale_Factor.
   --  Since: gtk+ 3.10

   function Get_Screen
      (Widget : not null access Gtk_Widget_Record)
       return Gdk.Screen.Gdk_Screen;
   --  Get the Gdk.Screen.Gdk_Screen from the toplevel window associated with
   --  this widget. This function can only be called after the widget has been
   --  added to a widget hierarchy with a Gtk.Window.Gtk_Window at the top.
   --  In general, you should only create screen specific resources when a
   --  widget has been realized, and you should free those resources when the
   --  widget is unrealized.
   --  Since: gtk+ 2.2

   function Get_Sensitive
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Returns the widget's sensitivity (in the sense of returning the value
   --  that has been set using Gtk.Widget.Set_Sensitive).
   --  The effective sensitivity of a widget is however determined by both its
   --  own and its parent widget's sensitivity. See Gtk.Widget.Is_Sensitive.
   --  Since: gtk+ 2.18

   procedure Set_Sensitive
      (Widget    : not null access Gtk_Widget_Record;
       Sensitive : Boolean := True);
   --  Sets the sensitivity of a widget. A widget is sensitive if the user can
   --  interact with it. Insensitive widgets are "grayed out" and the user
   --  can't interact with them. Insensitive widgets are known as "inactive",
   --  "disabled", or "ghosted" in some other toolkits.
   --  "sensitive": True to make the widget sensitive

   procedure Get_Size_Request
      (Widget : not null access Gtk_Widget_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint);
   --  Gets the size request that was explicitly set for the widget using
   --  Gtk.Widget.Set_Size_Request. A value of -1 stored in Width or Height
   --  indicates that that dimension has not been set explicitly and the
   --  natural requisition of the widget will be used instead. See
   --  Gtk.Widget.Set_Size_Request. To get the size a widget will actually
   --  request, call Gtk.Widget.Get_Preferred_Size instead of this function.
   --  "width": return location for width, or null
   --  "height": return location for height, or null

   procedure Set_Size_Request
      (Widget : not null access Gtk_Widget_Record;
       Width  : Glib.Gint := -1;
       Height : Glib.Gint := -1);
   --  Sets the minimum size of a widget; that is, the widget's size request
   --  will be at least Width by Height. You can use this function to force a
   --  widget to be larger than it normally would be.
   --  In most cases, Gtk.Window.Set_Default_Size is a better choice for
   --  toplevel windows than this function; setting the default size will still
   --  allow users to shrink the window. Setting the size request will force
   --  them to leave the window at least as large as the size request. When
   --  dealing with window sizes, Gtk.Window.Set_Geometry_Hints can be a useful
   --  function as well.
   --  Note the inherent danger of setting any fixed size - themes,
   --  translations into other languages, different fonts, and user action can
   --  all change the appropriate size for a given widget. So, it's basically
   --  impossible to hardcode a size that will always be correct.
   --  The size request of a widget is the smallest size a widget can accept
   --  while still functioning well and drawing itself correctly. However in
   --  some strange cases a widget may be allocated less than its requested
   --  size, and in many cases a widget may be allocated more space than it
   --  requested.
   --  If the size request in a given direction is -1 (unset), then the
   --  "natural" size request of the widget will be used instead.
   --  The size request set here does not include any margin from the
   --  Gtk.Widget.Gtk_Widget properties margin-left, margin-right, margin-top,
   --  and margin-bottom, but it does include pretty much all other padding or
   --  border properties set by any subclass of Gtk.Widget.Gtk_Widget.
   --  "width": width Widget should request, or -1 to unset
   --  "height": height Widget should request, or -1 to unset

   procedure Size_Request
      (Widget      : not null access Gtk_Widget_Record;
       Requisition : out Gtk_Requisition);
   pragma Obsolescent (Size_Request);
   --  This function is typically used when implementing a
   --  Gtk.Container.Gtk_Container subclass. Obtains the preferred size of a
   --  widget. The container uses this information to arrange its child widgets
   --  and decide what size allocations to give them with
   --  Gtk.Widget.Size_Allocate.
   --  You can also call this function from an application, with some caveats.
   --  Most notably, getting a size request requires the widget to be
   --  associated with a screen, because font information may be needed.
   --  Multihead-aware applications should keep this in mind.
   --  Also remember that the size request is not necessarily the size a
   --  widget will actually be allocated.
   --  Deprecated since 3.0, 1
   --  "requisition": a Gtk.Widget.Gtk_Requisition to be filled in

   function Get_State
      (Widget : not null access Gtk_Widget_Record)
       return Gtk.Enums.Gtk_State_Type;
   pragma Obsolescent (Get_State);
   --  Returns the widget's state. See Gtk.Widget.Set_State.
   --  Since: gtk+ 2.18
   --  Deprecated since 3.0, 1

   procedure Set_State
      (Widget : not null access Gtk_Widget_Record;
       State  : Gtk.Enums.Gtk_State_Type);
   pragma Obsolescent (Set_State);
   --  This function is for use in widget implementations. Sets the state of a
   --  widget (insensitive, prelighted, etc.) Usually you should set the state
   --  using wrapper functions such as Gtk.Widget.Set_Sensitive.
   --  Deprecated since 3.0, 1
   --  "state": new state for Widget

   function Get_State_Flags
      (Widget : not null access Gtk_Widget_Record)
       return Gtk.Enums.Gtk_State_Flags;
   --  Returns the widget state as a flag set. It is worth mentioning that the
   --  effective Gtk.Enums.Gtk_State_Flag_Insensitive state will be returned,
   --  that is, also based on parent insensitivity, even if Widget itself is
   --  sensitive.
   --  Also note that if you are looking for a way to obtain the
   --  Gtk.Enums.Gtk_State_Flags to pass to a
   --  Gtk.Style_Context.Gtk_Style_Context method, you should look at
   --  Gtk.Style_Context.Get_State.
   --  Since: gtk+ 3.0

   procedure Set_State_Flags
      (Widget : not null access Gtk_Widget_Record;
       Flags  : Gtk.Enums.Gtk_State_Flags;
       Clear  : Boolean);
   --  This function is for use in widget implementations. Turns on flag
   --  values in the current widget state (insensitive, prelighted, etc.).
   --  This function accepts the values Gtk.Enums.Gtk_State_Flag_Dir_Ltr and
   --  Gtk.Enums.Gtk_State_Flag_Dir_Rtl but ignores them. If you want to set
   --  the widget's direction, use Gtk.Widget.Set_Direction.
   --  It is worth mentioning that any other state than
   --  Gtk.Enums.Gtk_State_Flag_Insensitive, will be propagated down to all
   --  non-internal children if Widget is a Gtk.Container.Gtk_Container, while
   --  Gtk.Enums.Gtk_State_Flag_Insensitive itself will be propagated down to
   --  all Gtk.Container.Gtk_Container children by different means than turning
   --  on the state flag down the hierarchy, both Gtk.Widget.Get_State_Flags
   --  and Gtk.Widget.Is_Sensitive will make use of these.
   --  Since: gtk+ 3.0
   --  "flags": State flags to turn on
   --  "clear": Whether to clear state before turning on Flags

   function Get_Style
      (Widget : not null access Gtk_Widget_Record)
       return Gtk.Style.Gtk_Style;
   pragma Obsolescent (Get_Style);
   --  Simply an accessor function that returns Widget->style.
   --  Deprecated since 3.0, 1

   procedure Set_Style
      (Widget : not null access Gtk_Widget_Record;
       Style  : access Gtk.Style.Gtk_Style_Record'Class);
   pragma Obsolescent (Set_Style);
   --  Used to set the Gtk.Style.Gtk_Style for a widget (Widget->style). Since
   --  GTK 3, this function does nothing, the passed in style is ignored.
   --  Deprecated since 3.0, 1
   --  "style": a Gtk.Style.Gtk_Style, or null to remove the effect of a
   --  previous call to Gtk.Widget.Set_Style and go back to the default style

   function Get_Support_Multidevice
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Returns True if Widget is multiple pointer aware. See
   --  Gtk.Widget.Set_Support_Multidevice for more information.

   procedure Set_Support_Multidevice
      (Widget              : not null access Gtk_Widget_Record;
       Support_Multidevice : Boolean);
   --  Enables or disables multiple pointer awareness. If this setting is
   --  True, Widget will start receiving multiple, per device enter/leave
   --  events. Note that if custom Gdk_Windows are created in
   --  Gtk.Widget.Gtk_Widget::realize, Gdk.Window.Set_Support_Multidevice will
   --  have to be called manually on them.
   --  Since: gtk+ 3.0
   --  "support_multidevice": True to support input from multiple devices.

   function Get_Template_Child
      (Widget      : not null access Gtk_Widget_Record;
       Widget_Type : GType;
       Name        : UTF8_String) return Glib.Object.GObject;
   --  Fetch an object build from the template XML for Widget_Type in this
   --  Widget instance.
   --  This will only report children which were previously declared with
   --  Gtk.Widget.Bind_Template_Child_Full or one of its variants.
   --  This function is only meant to be called for code which is private to
   --  the Widget_Type which declared the child and is meant for language
   --  bindings which cannot easily make use of the GObject structure offsets.
   --  "widget_type": The GType to get a template child for
   --  "name": The "id" of the child defined in the template XML

   function Get_Tooltip_Markup
      (Widget : not null access Gtk_Widget_Record) return UTF8_String;
   --  Gets the contents of the tooltip for Widget.
   --  Since: gtk+ 2.12

   procedure Set_Tooltip_Markup
      (Widget : not null access Gtk_Widget_Record;
       Markup : UTF8_String := "");
   --  Sets Markup as the contents of the tooltip, which is marked up with the
   --  [Pango text markup language][PangoMarkupFormat].
   --  This function will take care of setting
   --  Gtk.Widget.Gtk_Widget:has-tooltip to True and of the default handler for
   --  the Gtk.Widget.Gtk_Widget::query-tooltip signal.
   --  See also the Gtk.Widget.Gtk_Widget:tooltip-markup property and
   --  Gtk.Tooltip.Set_Markup.
   --  Since: gtk+ 2.12
   --  "markup": the contents of the tooltip for Widget, or null

   function Get_Tooltip_Text
      (Widget : not null access Gtk_Widget_Record) return UTF8_String;
   --  Gets the contents of the tooltip for Widget.
   --  Since: gtk+ 2.12

   procedure Set_Tooltip_Text
      (Widget : not null access Gtk_Widget_Record;
       Text   : UTF8_String := "");
   --  Sets Text as the contents of the tooltip. This function will take care
   --  of setting Gtk.Widget.Gtk_Widget:has-tooltip to True and of the default
   --  handler for the Gtk.Widget.Gtk_Widget::query-tooltip signal.
   --  See also the Gtk.Widget.Gtk_Widget:tooltip-text property and
   --  Gtk.Tooltip.Set_Text.
   --  Since: gtk+ 2.12
   --  "text": the contents of the tooltip for Widget

   function Get_Tooltip_Window
      (Widget : not null access Gtk_Widget_Record) return Gtk_Widget;
   --  Returns the Gtk.Window.Gtk_Window of the current tooltip. This can be
   --  the GtkWindow created by default, or the custom tooltip window set using
   --  Gtk.Widget.Set_Tooltip_Window.
   --  Since: gtk+ 2.12

   procedure Set_Tooltip_Window
      (Widget        : not null access Gtk_Widget_Record;
       Custom_Window : access Gtk_Widget_Record'Class);
   --  Replaces the default window used for displaying tooltips with
   --  Custom_Window. GTK+ will take care of showing and hiding Custom_Window
   --  at the right moment, to behave likewise as the default tooltip window.
   --  If Custom_Window is null, the default tooltip window will be used.
   --  Since: gtk+ 2.12
   --  "custom_window": a Gtk.Window.Gtk_Window, or null

   function Get_Toplevel
      (Widget : not null access Gtk_Widget_Record) return Gtk_Widget;
   --  This function returns the topmost widget in the container hierarchy
   --  Widget is a part of. If Widget has no parent widgets, it will be
   --  returned as the topmost widget. No reference will be added to the
   --  returned widget; it should not be unreferenced.
   --  Note the difference in behavior vs. Gtk.Widget.Get_Ancestor;
   --  `gtk_widget_get_ancestor (widget, GTK_TYPE_WINDOW)` would return null if
   --  Widget wasn't inside a toplevel window, and if the window was inside a
   --  Gtk.Window.Gtk_Window-derived widget which was in turn inside the
   --  toplevel Gtk.Window.Gtk_Window. While the second case may seem unlikely,
   --  it actually happens when a Gtk.Plug.Gtk_Plug is embedded inside a
   --  Gtk.Socket.Gtk_Socket within the same application.
   --  To reliably find the toplevel Gtk.Window.Gtk_Window, use
   --  Gtk.Widget.Get_Toplevel and call GTK_IS_WINDOW on the result. For
   --  instance, to get the title of a widget's toplevel window, one might use:
   --  |[<!-- language="C" --> static const char * get_widget_toplevel_title
   --  (GtkWidget *widget) { GtkWidget *toplevel = gtk_widget_get_toplevel
   --  (widget); if (GTK_IS_WINDOW (toplevel)) { return gtk_window_get_title
   --  (GTK_WINDOW (toplevel)); }
   --  return NULL; } ]|

   function Get_Valign
      (Widget : not null access Gtk_Widget_Record) return Gtk_Align;
   --  Gets the value of the Gtk.Widget.Gtk_Widget:valign property.
   --  For backwards compatibility reasons this method will never return
   --  Gtk.Widget.Align_Baseline, but instead it will convert it to
   --  Gtk.Widget.Align_Fill. If your widget want to support baseline aligned
   --  children it must use Gtk.Widget.Get_Valign_With_Baseline, or
   --  `g_object_get (widget, "valign", &value, NULL)`, which will also report
   --  the true value.

   procedure Set_Valign
      (Widget : not null access Gtk_Widget_Record;
       Align  : Gtk_Align);
   --  Sets the vertical alignment of Widget. See the
   --  Gtk.Widget.Gtk_Widget:valign property.
   --  "align": the vertical alignment

   function Get_Valign_With_Baseline
      (Widget : not null access Gtk_Widget_Record) return Gtk_Align;
   --  Gets the value of the Gtk.Widget.Gtk_Widget:valign property, including
   --  Gtk.Widget.Align_Baseline.
   --  Since: gtk+ 3.10

   function Get_Vexpand
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Gets whether the widget would like any available extra vertical space.
   --  See Gtk.Widget.Get_Hexpand for more detail.

   procedure Set_Vexpand
      (Widget : not null access Gtk_Widget_Record;
       Expand : Boolean);
   --  Sets whether the widget would like any available extra vertical space.
   --  See Gtk.Widget.Set_Hexpand for more detail.
   --  "expand": whether to expand

   function Get_Vexpand_Set
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Gets whether Gtk.Widget.Set_Vexpand has been used to explicitly set the
   --  expand flag on this widget.
   --  See Gtk.Widget.Get_Hexpand_Set for more detail.

   procedure Set_Vexpand_Set
      (Widget : not null access Gtk_Widget_Record;
       Set    : Boolean);
   --  Sets whether the vexpand flag (see Gtk.Widget.Get_Vexpand) will be
   --  used.
   --  See Gtk.Widget.Set_Hexpand_Set for more detail.
   --  "set": value for vexpand-set property

   function Get_Visible
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether the widget is visible. If you want to take into
   --  account whether the widget's parent is also marked as visible, use
   --  Gtk.Widget.Is_Visible instead.
   --  This function does not check if the widget is obscured in any way.
   --  See Gtk.Widget.Set_Visible.
   --  Since: gtk+ 2.18

   procedure Set_Visible
      (Widget  : not null access Gtk_Widget_Record;
       Visible : Boolean);
   --  Sets the visibility state of Widget. Note that setting this to True
   --  doesn't mean the widget is actually viewable, see
   --  Gtk.Widget.Get_Visible.
   --  This function simply calls Gtk.Widget.Show or Gtk.Widget.Hide but is
   --  nicer to use when the visibility of the widget depends on some
   --  condition.
   --  Since: gtk+ 2.18
   --  "visible": whether the widget should be shown or not

   function Get_Visual
      (Widget : not null access Gtk_Widget_Record)
       return Gdk.Visual.Gdk_Visual;
   --  Gets the visual that will be used to render Widget.

   procedure Set_Visual
      (Widget : not null access Gtk_Widget_Record;
       Visual : Gdk.Visual.Gdk_Visual);
   --  Sets the visual that should be used for by widget and its children for
   --  creating Gdk_Windows. The visual must be on the same
   --  Gdk.Screen.Gdk_Screen as returned by Gtk.Widget.Get_Screen, so handling
   --  the Gtk.Widget.Gtk_Widget::screen-changed signal is necessary.
   --  Setting a new Visual will not cause Widget to recreate its windows, so
   --  you should call this function before Widget is realized.
   --  "visual": visual to be used or null to unset a previous one

   function Get_Window
      (Widget : not null access Gtk_Widget_Record) return Gdk.Gdk_Window;
   --  Returns the widget's window if it is realized, null otherwise
   --  Since: gtk+ 2.14

   procedure Set_Window
      (Widget : not null access Gtk_Widget_Record;
       Window : Gdk.Gdk_Window);
   --  Sets a widget's window. This function should only be used in a widget's
   --  Gtk.Widget.Gtk_Widget::realize implementation. The %window passed is
   --  usually either new window created with gdk_window_new, or the window of
   --  its parent widget as returned by Gtk.Widget.Get_Parent_Window.
   --  Widgets must indicate whether they will create their own Gdk.Gdk_Window
   --  by calling Gtk.Widget.Set_Has_Window. This is usually done in the
   --  widget's init function.
   --  Note that this function does not add any reference to Window.
   --  Since: gtk+ 2.18
   --  "window": a Gdk.Gdk_Window

   procedure Grab_Add (Widget : not null access Gtk_Widget_Record);
   --  Makes Widget the current grabbed widget.
   --  This means that interaction with other widgets in the same application
   --  is blocked and mouse as well as keyboard events are delivered to this
   --  widget.
   --  If Widget is not sensitive, it is not set as the current grabbed widget
   --  and this function does nothing.

   procedure Grab_Default (Widget : not null access Gtk_Widget_Record);
   --  Causes Widget to become the default widget. Widget must be able to be a
   --  default widget; typically you would ensure this yourself by calling
   --  Gtk.Widget.Set_Can_Default with a True value. The default widget is
   --  activated when the user presses Enter in a window. Default widgets must
   --  be activatable, that is, Gtk.Widget.Activate should affect them. Note
   --  that Gtk.GEntry.Gtk_Entry widgets require the "activates-default"
   --  property set to True before they activate the default widget when Enter
   --  is pressed and the Gtk.GEntry.Gtk_Entry is focused.

   procedure Grab_Focus (Widget : not null access Gtk_Widget_Record);
   --  Causes Widget to have the keyboard focus for the Gtk.Window.Gtk_Window
   --  it's inside. Widget must be a focusable widget, such as a
   --  Gtk.GEntry.Gtk_Entry; something like Gtk.Frame.Gtk_Frame won't work.
   --  More precisely, it must have the GTK_CAN_FOCUS flag set. Use
   --  Gtk.Widget.Set_Can_Focus to modify that flag.
   --  The widget also needs to be realized and mapped. This is indicated by
   --  the related signals. Grabbing the focus immediately after creating the
   --  widget will likely fail and cause critical warnings.

   procedure Grab_Remove (Widget : not null access Gtk_Widget_Record);
   --  Removes the grab from the given widget.
   --  You have to pair calls to Gtk.Widget.Grab_Add and
   --  Gtk.Widget.Grab_Remove.
   --  If Widget does not have the grab, this function does nothing.

   function Has_Default
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether Widget is the current default widget within its
   --  toplevel. See Gtk.Widget.Set_Can_Default.
   --  Since: gtk+ 2.18

   function Has_Focus
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines if the widget has the global input focus. See
   --  Gtk.Widget.Is_Focus for the difference between having the global input
   --  focus, and only having the focus within a toplevel.
   --  Since: gtk+ 2.18

   function Has_Grab
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether the widget is currently grabbing events, so it is
   --  the only widget receiving input events (keyboard and mouse).
   --  See also Gtk.Widget.Grab_Add.
   --  Since: gtk+ 2.18

   function Has_Rc_Style
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   pragma Obsolescent (Has_Rc_Style);
   --  Determines if the widget style has been looked up through the rc
   --  mechanism.
   --  Since: gtk+ 2.20
   --  Deprecated since 3.0, 1

   function Has_Screen
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Checks whether there is a Gdk.Screen.Gdk_Screen is associated with this
   --  widget. All toplevel widgets have an associated screen, and all widgets
   --  added into a hierarchy with a toplevel window at the top.
   --  Since: gtk+ 2.2

   function Has_Visible_Focus
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines if the widget should show a visible indication that it has
   --  the global input focus. This is a convenience function for use in ::draw
   --  handlers that takes into account whether focus indication should
   --  currently be shown in the toplevel window of Widget. See
   --  Gtk.Window.Get_Focus_Visible for more information about focus
   --  indication.
   --  To find out if the widget has the global input focus, use
   --  Gtk.Widget.Has_Focus.
   --  Since: gtk+ 3.2

   procedure Hide (Widget : not null access Gtk_Widget_Record);
   --  Reverses the effects of Gtk.Widget.Show, causing the widget to be
   --  hidden (invisible to the user).

   function Hide_On_Delete
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Utility function; intended to be connected to the
   --  Gtk.Widget.Gtk_Widget::delete-event signal on a Gtk.Window.Gtk_Window.
   --  The function calls Gtk.Widget.Hide on its argument, then returns True.
   --  If connected to ::delete-event, the result is that clicking the close
   --  button for a window (on the window frame, top right corner usually) will
   --  hide but not destroy the window. By default, GTK+ destroys windows when
   --  ::delete-event is received.

   function In_Destruction
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Returns whether the widget is currently being destroyed. This
   --  information can sometimes be used to avoid doing unnecessary work.

   procedure Init_Template (Widget : not null access Gtk_Widget_Record);
   --  Creates and initializes child widgets defined in templates. This
   --  function must be called in the instance initializer for any class which
   --  assigned itself a template using gtk_widget_class_set_template
   --  It is important to call this function in the instance initializer of a
   --  Gtk.Widget.Gtk_Widget subclass and not in
   --  Glib.Object.GObject.constructed or Glib.Object.GObject.constructor for
   --  two reasons.
   --  One reason is that generally derived widgets will assume that parent
   --  class composite widgets have been created in their instance
   --  initializers.
   --  Another reason is that when calling g_object_new on a widget with
   --  composite templates, it's important to build the composite widgets
   --  before the construct properties are set. Properties passed to
   --  g_object_new should take precedence over properties set in the private
   --  template XML.
   --  Since: gtk+ 3.10

   procedure Input_Shape_Combine_Region
      (Widget : not null access Gtk_Widget_Record;
       Region : Cairo.Region.Cairo_Region);
   --  Sets an input shape for this widget's GDK window. This allows for
   --  windows which react to mouse click in a nonrectangular region, see
   --  Gdk.Window.Input_Shape_Combine_Region for more information.
   --  Since: gtk+ 3.0
   --  "region": shape to be added, or null to remove an existing shape

   function Intersect
      (Widget       : not null access Gtk_Widget_Record;
       Area         : Gdk.Rectangle.Gdk_Rectangle;
       Intersection : access Gdk.Rectangle.Gdk_Rectangle) return Boolean;
   --  Computes the intersection of a Widget's area and Area, storing the
   --  intersection in Intersection, and returns True if there was an
   --  intersection. Intersection may be null if you're only interested in
   --  whether there was an intersection.
   --  "area": a rectangle
   --  "intersection": rectangle to store intersection of Widget and Area

   function Is_Ancestor
      (Widget   : not null access Gtk_Widget_Record;
       Ancestor : not null access Gtk_Widget_Record'Class) return Boolean;
   --  Determines whether Widget is somewhere inside Ancestor, possibly with
   --  intermediate containers.
   --  "ancestor": another Gtk.Widget.Gtk_Widget

   function Is_Composited
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   pragma Obsolescent (Is_Composited);
   --  Whether Widget can rely on having its alpha channel drawn correctly. On
   --  X11 this function returns whether a compositing manager is running for
   --  Widget's screen.
   --  Please note that the semantics of this call will change in the future
   --  if used on a widget that has a composited window in its hierarchy (as
   --  set by Gdk.Window.Set_Composited).
   --  Since: gtk+ 2.10
   --  Deprecated since 3.22, 1

   function Is_Drawable
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether Widget can be drawn to. A widget can be drawn to if
   --  it is mapped and visible.
   --  Since: gtk+ 2.18

   function Is_Focus
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines if the widget is the focus widget within its toplevel. (This
   --  does not mean that the Gtk.Widget.Gtk_Widget:has-focus property is
   --  necessarily set; Gtk.Widget.Gtk_Widget:has-focus will only be set if the
   --  toplevel widget additionally has the global input focus.)

   function Is_Sensitive
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Returns the widget's effective sensitivity, which means it is sensitive
   --  itself and also its parent widget is sensitive
   --  Since: gtk+ 2.18

   function Is_Toplevel
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether Widget is a toplevel widget.
   --  Currently only Gtk.Window.Gtk_Window and Gtk.Invisible.Gtk_Invisible
   --  (and out-of-process Gtk_Plugs) are toplevel widgets. Toplevel widgets
   --  have no parent widget.
   --  Since: gtk+ 2.18

   function Is_Visible
      (Widget : not null access Gtk_Widget_Record) return Boolean;
   --  Determines whether the widget and all its parents are marked as
   --  visible.
   --  This function does not check if the widget is obscured in any way.
   --  See also Gtk.Widget.Get_Visible and Gtk.Widget.Set_Visible
   --  Since: gtk+ 3.8

   function Keynav_Failed
      (Widget    : not null access Gtk_Widget_Record;
       Direction : Gtk.Enums.Gtk_Direction_Type) return Boolean;
   --  This function should be called whenever keyboard navigation within a
   --  single widget hits a boundary. The function emits the
   --  Gtk.Widget.Gtk_Widget::keynav-failed signal on the widget and its return
   --  value should be interpreted in a way similar to the return value of
   --  Gtk.Widget.Child_Focus:
   --  When True is returned, stay in the widget, the failed keyboard
   --  navigation is OK and/or there is nowhere we can/should move the focus
   --  to.
   --  When False is returned, the caller should continue with keyboard
   --  navigation outside the widget, e.g. by calling Gtk.Widget.Child_Focus on
   --  the widget's toplevel.
   --  The default ::keynav-failed handler returns False for
   --  Gtk.Enums.Dir_Tab_Forward and Gtk.Enums.Dir_Tab_Backward. For the other
   --  values of Gtk.Enums.Gtk_Direction_Type it returns True.
   --  Whenever the default handler returns True, it also calls
   --  Gtk.Widget.Error_Bell to notify the user of the failed keyboard
   --  navigation.
   --  A use case for providing an own implementation of ::keynav-failed
   --  (either by connecting to it or by overriding it) would be a row of
   --  Gtk.GEntry.Gtk_Entry widgets where the user should be able to navigate
   --  the entire row with the cursor keys, as e.g. known from user interfaces
   --  that require entering license keys.
   --  Since: gtk+ 2.12
   --  "direction": direction of focus movement

   function List_Action_Prefixes
      (Widget : not null access Gtk_Widget_Record)
       return GNAT.Strings.String_List;
   --  Retrieves a null-terminated array of strings containing the prefixes of
   --  Glib.Action_Group.Gaction_Group's available to Widget.
   --  Since: gtk+ 3.16

   function List_Mnemonic_Labels
      (Widget : not null access Gtk_Widget_Record) return Widget_List.Glist;
   --  Returns a newly allocated list of the widgets, normally labels, for
   --  which this widget is the target of a mnemonic (see for example,
   --  Gtk.Label.Set_Mnemonic_Widget).
   --  The widgets in the list are not individually referenced. If you want to
   --  iterate through the list and perform actions involving callbacks that
   --  might destroy the widgets, you must call `g_list_foreach (result,
   --  (GFunc)g_object_ref, NULL)` first, and then unref all the widgets
   --  afterwards.
   --  Since: gtk+ 2.4

   procedure Map (Widget : not null access Gtk_Widget_Record);
   --  This function is only for use in widget implementations. Causes a
   --  widget to be mapped if it isn't already.

   function Mnemonic_Activate
      (Widget        : not null access Gtk_Widget_Record;
       Group_Cycling : Boolean) return Boolean;
   --  Emits the Gtk.Widget.Gtk_Widget::mnemonic-activate signal.
   --  "group_cycling": True if there are other widgets with the same mnemonic

   procedure Modify_Base
      (Widget : not null access Gtk_Widget_Record;
       State  : Gtk.Enums.Gtk_State_Type;
       Color  : Gdk.Color.Gdk_Color);
   pragma Obsolescent (Modify_Base);
   --  Sets the base color for a widget in a particular state. All other style
   --  values are left untouched. The base color is the background color used
   --  along with the text color (see Gtk.Widget.Modify_Text) for widgets such
   --  as Gtk.GEntry.Gtk_Entry and Gtk.Text_View.Gtk_Text_View. See also
   --  gtk_widget_modify_style.
   --  > Note that "no window" widgets (which have the GTK_NO_WINDOW > flag
   --  set) draw on their parent container's window and thus may > not draw any
   --  background themselves. This is the case for e.g. > Gtk.Label.Gtk_Label.
   --  > > To modify the background of such widgets, you have to set the > base
   --  color on their parent; if you want to set the background > of a
   --  rectangular area around a label, try placing the label in > a
   --  Gtk.Event_Box.Gtk_Event_Box widget and setting the base color on that.
   --  Deprecated since 3.0, 1
   --  "state": the state for which to set the base color
   --  "color": the color to assign (does not need to be allocated), or null
   --  to undo the effect of previous calls to of Gtk.Widget.Modify_Base.

   procedure Modify_Bg
      (Widget : not null access Gtk_Widget_Record;
       State  : Gtk.Enums.Gtk_State_Type;
       Color  : Gdk.Color.Gdk_Color);
   pragma Obsolescent (Modify_Bg);
   --  Sets the background color for a widget in a particular state.
   --  All other style values are left untouched. See also
   --  gtk_widget_modify_style.
   --  > Note that "no window" widgets (which have the GTK_NO_WINDOW > flag
   --  set) draw on their parent container's window and thus may > not draw any
   --  background themselves. This is the case for e.g. > Gtk.Label.Gtk_Label.
   --  > > To modify the background of such widgets, you have to set the >
   --  background color on their parent; if you want to set the background > of
   --  a rectangular area around a label, try placing the label in > a
   --  Gtk.Event_Box.Gtk_Event_Box widget and setting the background color on
   --  that.
   --  Deprecated since 3.0, 1
   --  "state": the state for which to set the background color
   --  "color": the color to assign (does not need to be allocated), or null
   --  to undo the effect of previous calls to of Gtk.Widget.Modify_Bg.

   procedure Modify_Cursor
      (Widget    : not null access Gtk_Widget_Record;
       Primary   : Gdk.Color.Gdk_Color;
       Secondary : Gdk.Color.Gdk_Color);
   pragma Obsolescent (Modify_Cursor);
   --  Sets the cursor color to use in a widget, overriding the
   --  Gtk.Widget.Gtk_Widget cursor-color and secondary-cursor-color style
   --  properties.
   --  All other style values are left untouched. See also
   --  gtk_widget_modify_style.
   --  Since: gtk+ 2.12
   --  Deprecated since 3.0, 1
   --  "primary": the color to use for primary cursor (does not need to be
   --  allocated), or null to undo the effect of previous calls to of
   --  Gtk.Widget.Modify_Cursor.
   --  "secondary": the color to use for secondary cursor (does not need to be
   --  allocated), or null to undo the effect of previous calls to of
   --  Gtk.Widget.Modify_Cursor.

   procedure Modify_Fg
      (Widget : not null access Gtk_Widget_Record;
       State  : Gtk.Enums.Gtk_State_Type;
       Color  : Gdk.Color.Gdk_Color);
   pragma Obsolescent (Modify_Fg);
   --  Sets the foreground color for a widget in a particular state.
   --  All other style values are left untouched. See also
   --  gtk_widget_modify_style.
   --  Only states between State_Normal and State_Insensitive are valid.
   --  Deprecated since 3.0, 1
   --  "state": the state for which to set the foreground color
   --  "color": the color to assign (does not need to be allocated), or null
   --  to undo the effect of previous calls to of Gtk.Widget.Modify_Fg.

   procedure Modify_Font
      (Widget    : not null access Gtk_Widget_Record;
       Font_Desc : Pango.Font.Pango_Font_Description);
   pragma Obsolescent (Modify_Font);
   --  Sets the font to use for a widget.
   --  All other style values are left untouched. See also
   --  gtk_widget_modify_style.
   --  Deprecated since 3.0, 1
   --  "font_desc": the font description to use, or null to undo the effect of
   --  previous calls to Gtk.Widget.Modify_Font

   procedure Modify_Text
      (Widget : not null access Gtk_Widget_Record;
       State  : Gtk.Enums.Gtk_State_Type;
       Color  : Gdk.Color.Gdk_Color);
   pragma Obsolescent (Modify_Text);
   --  Sets the text color for a widget in a particular state.
   --  All other style values are left untouched. The text color is the
   --  foreground color used along with the base color (see
   --  Gtk.Widget.Modify_Base) for widgets such as Gtk.GEntry.Gtk_Entry and
   --  Gtk.Text_View.Gtk_Text_View. See also gtk_widget_modify_style.
   --  Deprecated since 3.0, 1
   --  "state": the state for which to set the text color
   --  "color": the color to assign (does not need to be allocated), or null
   --  to undo the effect of previous calls to of Gtk.Widget.Modify_Text.

   procedure Override_Background_Color
      (Widget : not null access Gtk_Widget_Record;
       State  : Gtk.Enums.Gtk_State_Flags;
       Color  : Gdk.RGBA.Gdk_RGBA);
   pragma Obsolescent (Override_Background_Color);
   --  Sets the background color to use for a widget.
   --  All other style values are left untouched. See
   --  Gtk.Widget.Override_Color.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.16, 1
   --  "state": the state for which to set the background color
   --  "color": the color to assign, or null to undo the effect of previous
   --  calls to Gtk.Widget.Override_Background_Color

   procedure Override_Color
      (Widget : not null access Gtk_Widget_Record;
       State  : Gtk.Enums.Gtk_State_Flags;
       Color  : Gdk.RGBA.Gdk_RGBA);
   pragma Obsolescent (Override_Color);
   --  Sets the color to use for a widget.
   --  All other style values are left untouched.
   --  This function does not act recursively. Setting the color of a
   --  container does not affect its children. Note that some widgets that you
   --  may not think of as containers, for instance Gtk_Buttons, are actually
   --  containers.
   --  This API is mostly meant as a quick way for applications to change a
   --  widget appearance. If you are developing a widgets library and intend
   --  this change to be themeable, it is better done by setting meaningful CSS
   --  classes in your widget/container implementation through
   --  Gtk.Style_Context.Add_Class.
   --  This way, your widget library can install a
   --  Gtk.Css_Provider.Gtk_Css_Provider with the
   --  GTK_STYLE_PROVIDER_PRIORITY_FALLBACK priority in order to provide a
   --  default styling for those widgets that need so, and this theming may
   --  fully overridden by the user's theme.
   --  Note that for complex widgets this may bring in undesired results (such
   --  as uniform background color everywhere), in these cases it is better to
   --  fully style such widgets through a Gtk.Css_Provider.Gtk_Css_Provider
   --  with the GTK_STYLE_PROVIDER_PRIORITY_APPLICATION priority.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.16, 1
   --  "state": the state for which to set the color
   --  "color": the color to assign, or null to undo the effect of previous
   --  calls to Gtk.Widget.Override_Color

   procedure Override_Cursor
      (Widget           : not null access Gtk_Widget_Record;
       Cursor           : Gdk.RGBA.Gdk_RGBA;
       Secondary_Cursor : Gdk.RGBA.Gdk_RGBA);
   pragma Obsolescent (Override_Cursor);
   --  Sets the cursor color to use in a widget, overriding the cursor-color
   --  and secondary-cursor-color style properties. All other style values are
   --  left untouched. See also gtk_widget_modify_style.
   --  Note that the underlying properties have the Gdk.Color.Gdk_Color type,
   --  so the alpha value in Primary and Secondary will be ignored.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.16, 1
   --  "cursor": the color to use for primary cursor (does not need to be
   --  allocated), or null to undo the effect of previous calls to of
   --  Gtk.Widget.Override_Cursor.
   --  "secondary_cursor": the color to use for secondary cursor (does not
   --  need to be allocated), or null to undo the effect of previous calls to
   --  of Gtk.Widget.Override_Cursor.

   procedure Override_Font
      (Widget    : not null access Gtk_Widget_Record;
       Font_Desc : Pango.Font.Pango_Font_Description);
   pragma Obsolescent (Override_Font);
   --  Sets the font to use for a widget. All other style values are left
   --  untouched. See Gtk.Widget.Override_Color.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.16, 1
   --  "font_desc": the font description to use, or null to undo the effect of
   --  previous calls to Gtk.Widget.Override_Font

   procedure Override_Symbolic_Color
      (Widget : not null access Gtk_Widget_Record;
       Name   : UTF8_String;
       Color  : Gdk.RGBA.Gdk_RGBA);
   pragma Obsolescent (Override_Symbolic_Color);
   --  Sets a symbolic color for a widget.
   --  All other style values are left untouched. See
   --  Gtk.Widget.Override_Color for overriding the foreground or background
   --  color.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.16, 1
   --  "name": the name of the symbolic color to modify
   --  "color": the color to assign (does not need to be allocated), or null
   --  to undo the effect of previous calls to
   --  Gtk.Widget.Override_Symbolic_Color

   procedure Queue_Allocate (Widget : not null access Gtk_Widget_Record);
   --  This function is only for use in widget implementations.
   --  Flags the widget for a rerun of the GtkWidgetClass::size_allocate
   --  function. Use this function instead of Gtk.Widget.Queue_Resize when the
   --  Widget's size request didn't change but it wants to reposition its
   --  contents.
   --  An example user of this function is Gtk.Widget.Set_Halign.
   --  Since: gtk+ 3.20

   procedure Queue_Compute_Expand
      (Widget : not null access Gtk_Widget_Record);
   --  Mark Widget as needing to recompute its expand flags. Call this
   --  function when setting legacy expand child properties on the child of a
   --  container.
   --  See Gtk.Widget.Compute_Expand.

   procedure Queue_Draw (Widget : not null access Gtk_Widget_Record);
   --  Equivalent to calling Gtk.Widget.Queue_Draw_Area for the entire area of
   --  a widget.

   procedure Queue_Draw_Area
      (Widget : not null access Gtk_Widget_Record;
       X      : Glib.Gint;
       Y      : Glib.Gint;
       Width  : Glib.Gint;
       Height : Glib.Gint);
   --  Convenience function that calls Gtk.Widget.Queue_Draw_Region on the
   --  region created from the given coordinates.
   --  The region here is specified in widget coordinates. Widget coordinates
   --  are a bit odd; for historical reasons, they are defined as
   --  Widget->window coordinates for widgets that return True for
   --  Gtk.Widget.Get_Has_Window, and are relative to Widget->allocation.x,
   --  Widget->allocation.y otherwise.
   --  Width or Height may be 0, in this case this function does nothing.
   --  Negative values for Width and Height are not allowed.
   --  "x": x coordinate of upper-left corner of rectangle to redraw
   --  "y": y coordinate of upper-left corner of rectangle to redraw
   --  "width": width of region to draw
   --  "height": height of region to draw

   procedure Queue_Draw_Region
      (Widget : not null access Gtk_Widget_Record;
       Region : Cairo.Region.Cairo_Region);
   --  Invalidates the area of Widget defined by Region by calling
   --  Gdk.Window.Invalidate_Region on the widget's window and all its child
   --  windows. Once the main loop becomes idle (after the current batch of
   --  events has been processed, roughly), the window will receive expose
   --  events for the union of all regions that have been invalidated.
   --  Normally you would only use this function in widget implementations.
   --  You might also use it to schedule a redraw of a
   --  Gtk.Drawing_Area.Gtk_Drawing_Area or some portion thereof.
   --  Since: gtk+ 3.0
   --  "region": region to draw

   procedure Queue_Resize (Widget : not null access Gtk_Widget_Record);
   --  This function is only for use in widget implementations. Flags a widget
   --  to have its size renegotiated; should be called when a widget for some
   --  reason has a new size request. For example, when you change the text in
   --  a Gtk.Label.Gtk_Label, Gtk.Label.Gtk_Label queues a resize to ensure
   --  there's enough space for the new text.
   --  Note that you cannot call Gtk.Widget.Queue_Resize on a widget from
   --  inside its implementation of the GtkWidgetClass::size_allocate virtual
   --  method. Calls to Gtk.Widget.Queue_Resize from inside
   --  GtkWidgetClass::size_allocate will be silently ignored.

   procedure Queue_Resize_No_Redraw
      (Widget : not null access Gtk_Widget_Record);
   --  This function works like Gtk.Widget.Queue_Resize, except that the
   --  widget is not invalidated.
   --  Since: gtk+ 2.4

   procedure Realize (Widget : not null access Gtk_Widget_Record);
   --  Creates the GDK (windowing system) resources associated with a widget.
   --  For example, Widget->window will be created when a widget is realized.
   --  Normally realization happens implicitly; if you show a widget and all
   --  its parent containers, then the widget will be realized and mapped
   --  automatically.
   --  Realizing a widget requires all the widget's parent widgets to be
   --  realized; calling Gtk.Widget.Realize realizes the widget's parents in
   --  addition to Widget itself. If a widget is not yet inside a toplevel
   --  window when you realize it, bad things will happen.
   --  This function is primarily used in widget implementations, and isn't
   --  very useful otherwise. Many times when you think you might need it, a
   --  better approach is to connect to a signal that will be called after the
   --  widget is realized automatically, such as Gtk.Widget.Gtk_Widget::draw.
   --  Or simply g_signal_connect () to the Gtk.Widget.Gtk_Widget::realize
   --  signal.

   function Region_Intersect
      (Widget : not null access Gtk_Widget_Record;
       Region : Cairo.Region.Cairo_Region) return Cairo.Region.Cairo_Region;
   pragma Obsolescent (Region_Intersect);
   --  Computes the intersection of a Widget's area and Region, returning the
   --  intersection. The result may be empty, use cairo_region_is_empty to
   --  check.
   --  Deprecated since 3.14, 1
   --  "region": a cairo_region_t, in the same coordinate system as
   --  Widget->allocation. That is, relative to Widget->window for widgets
   --  which return False from Gtk.Widget.Get_Has_Window; relative to the
   --  parent window of Widget->window otherwise.

   procedure Register_Window
      (Widget : not null access Gtk_Widget_Record;
       Window : Gdk.Gdk_Window);
   --  Registers a Gdk.Gdk_Window with the widget and sets it up so that the
   --  widget receives events for it. Call Gtk.Widget.Unregister_Window when
   --  destroying the window.
   --  Before 3.8 you needed to call Gdk.Window.Set_User_Data directly to set
   --  this up. This is now deprecated and you should use
   --  Gtk.Widget.Register_Window instead. Old code will keep working as is,
   --  although some new features like transparency might not work perfectly.
   --  Since: gtk+ 3.8
   --  "window": a Gdk.Gdk_Window

   function Remove_Accelerator
      (Widget      : not null access Gtk_Widget_Record;
       Accel_Group : not null access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class;
       Accel_Key   : Gdk.Types.Gdk_Key_Type;
       Accel_Mods  : Gdk.Types.Gdk_Modifier_Type) return Boolean;
   --  Removes an accelerator from Widget, previously installed with
   --  Gtk.Widget.Add_Accelerator.
   --  "accel_group": accel group for this widget
   --  "accel_key": GDK keyval of the accelerator
   --  "accel_mods": modifier key combination of the accelerator

   procedure Remove_Mnemonic_Label
      (Widget : not null access Gtk_Widget_Record;
       Label  : not null access Gtk_Widget_Record'Class);
   --  Removes a widget from the list of mnemonic labels for this widget. (See
   --  Gtk.Widget.List_Mnemonic_Labels). The widget must have previously been
   --  added to the list with Gtk.Widget.Add_Mnemonic_Label.
   --  Since: gtk+ 2.4
   --  "label": a Gtk.Widget.Gtk_Widget that was previously set as a mnemonic
   --  label for Widget with Gtk.Widget.Add_Mnemonic_Label.

   procedure Remove_Tick_Callback
      (Widget : not null access Gtk_Widget_Record;
       Id     : Guint);
   --  Removes a tick callback previously registered with
   --  Gtk.Widget.Add_Tick_Callback.
   --  Since: gtk+ 3.8
   --  "id": an id returned by Gtk.Widget.Add_Tick_Callback

   function Render_Icon
      (Widget   : not null access Gtk_Widget_Record;
       Stock_Id : UTF8_String;
       Size     : Gtk.Enums.Gtk_Icon_Size;
       Detail   : UTF8_String := "") return Gdk.Pixbuf.Gdk_Pixbuf;
   pragma Obsolescent (Render_Icon);
   --  A convenience function that uses the theme settings for Widget to look
   --  up Stock_Id and render it to a pixbuf. Stock_Id should be a stock icon
   --  ID such as GTK_STOCK_OPEN or GTK_STOCK_OK. Size should be a size such as
   --  GTK_ICON_SIZE_MENU. Detail should be a string that identifies the widget
   --  or code doing the rendering, so that theme engines can special-case
   --  rendering for that widget or code.
   --  The pixels in the returned Gdk.Pixbuf.Gdk_Pixbuf are shared with the
   --  rest of the application and should not be modified. The pixbuf should be
   --  freed after use with g_object_unref.
   --  Deprecated since 3.0, 1
   --  "stock_id": a stock ID
   --  "size": a stock size (Gtk.Enums.Gtk_Icon_Size). A size of
   --  `(GtkIconSize)-1` means render at the size of the source and don't scale
   --  (if there are multiple source sizes, GTK+ picks one of the available
   --  sizes).
   --  "detail": render detail to pass to theme engine

   function Render_Icon_Pixbuf
      (Widget   : not null access Gtk_Widget_Record;
       Stock_Id : UTF8_String;
       Size     : Gtk.Enums.Gtk_Icon_Size) return Gdk.Pixbuf.Gdk_Pixbuf;
   pragma Obsolescent (Render_Icon_Pixbuf);
   --  A convenience function that uses the theme engine and style settings
   --  for Widget to look up Stock_Id and render it to a pixbuf. Stock_Id
   --  should be a stock icon ID such as GTK_STOCK_OPEN or GTK_STOCK_OK. Size
   --  should be a size such as GTK_ICON_SIZE_MENU.
   --  The pixels in the returned Gdk.Pixbuf.Gdk_Pixbuf are shared with the
   --  rest of the application and should not be modified. The pixbuf should be
   --  freed after use with g_object_unref.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.10, 1
   --  "stock_id": a stock ID
   --  "size": a stock size (Gtk.Enums.Gtk_Icon_Size). A size of
   --  `(GtkIconSize)-1` means render at the size of the source and don't scale
   --  (if there are multiple source sizes, GTK+ picks one of the available
   --  sizes).

   procedure Reparent
      (Widget     : not null access Gtk_Widget_Record;
       New_Parent : not null access Gtk_Widget_Record'Class);
   pragma Obsolescent (Reparent);
   --  Moves a widget from one Gtk.Container.Gtk_Container to another,
   --  handling reference count issues to avoid destroying the widget.
   --  Deprecated since 3.14, 1
   --  "new_parent": a Gtk.Container.Gtk_Container to move the widget into

   procedure Reset_Rc_Styles (Widget : not null access Gtk_Widget_Record);
   pragma Obsolescent (Reset_Rc_Styles);
   --  Reset the styles of Widget and all descendents, so when they are looked
   --  up again, they get the correct values for the currently loaded RC file
   --  settings.
   --  This function is not useful for applications.
   --  Deprecated since 3.0, 1

   procedure Reset_Style (Widget : not null access Gtk_Widget_Record);
   --  Updates the style context of Widget and all descendants by updating its
   --  widget path. Gtk_Containers may want to use this on a child when
   --  reordering it in a way that a different style might apply to it. See
   --  also Gtk.Container.Get_Path_For_Child.
   --  Since: gtk+ 3.0

   function Send_Expose
      (Widget : not null access Gtk_Widget_Record;
       Event  : Gdk.Event.Gdk_Event) return Glib.Gint;
   pragma Obsolescent (Send_Expose);
   --  Very rarely-used function. This function is used to emit an expose
   --  event on a widget. This function is not normally used directly. The only
   --  time it is used is when propagating an expose event to a windowless
   --  child widget (gtk_widget_get_has_window is False), and that is normally
   --  done using Gtk.Container.Propagate_Draw.
   --  If you want to force an area of a window to be redrawn, use
   --  Gdk.Window.Invalidate_Rect or Gdk.Window.Invalidate_Region. To cause the
   --  redraw to be done immediately, follow that call with a call to
   --  Gdk.Window.Process_Updates.
   --  Deprecated since 3.22, 1
   --  "event": a expose Gdk.Event.Gdk_Event

   function Send_Focus_Change
      (Widget : not null access Gtk_Widget_Record;
       Event  : Gdk.Event.Gdk_Event) return Boolean;
   --  Sends the focus change Event to Widget
   --  This function is not meant to be used by applications. The only time it
   --  should be used is when it is necessary for a Gtk.Widget.Gtk_Widget to
   --  assign focus to a widget that is semantically owned by the first widget
   --  even though it's not a direct child - for instance, a search entry in a
   --  floating window similar to the quick search in
   --  Gtk.Tree_View.Gtk_Tree_View.
   --  An example of its usage is:
   --  |[<!-- language="C" --> GdkEvent *fevent = gdk_event_new
   --  (GDK_FOCUS_CHANGE);
   --  fevent->focus_change.type = GDK_FOCUS_CHANGE; fevent->focus_change.in =
   --  TRUE; fevent->focus_change.window = _gtk_widget_get_window (widget); if
   --  (fevent->focus_change.window != NULL) g_object_ref
   --  (fevent->focus_change.window);
   --  gtk_widget_send_focus_change (widget, fevent);
   --  gdk_event_free (event); ]|
   --  Since: gtk+ 2.20
   --  "event": a Gdk.Event.Gdk_Event of type GDK_FOCUS_CHANGE

   procedure Set_Accel_Path
      (Widget      : not null access Gtk_Widget_Record;
       Accel_Path  : UTF8_String := "";
       Accel_Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class);
   --  Given an accelerator group, Accel_Group, and an accelerator path,
   --  Accel_Path, sets up an accelerator in Accel_Group so whenever the key
   --  binding that is defined for Accel_Path is pressed, Widget will be
   --  activated. This removes any accelerators (for any accelerator group)
   --  installed by previous calls to Gtk.Widget.Set_Accel_Path. Associating
   --  accelerators with paths allows them to be modified by the user and the
   --  modifications to be saved for future use. (See Gtk.Accel_Map.Save.)
   --  This function is a low level function that would most likely be used by
   --  a menu creation system like Gtk.UI_Manager.Gtk_UI_Manager. If you use
   --  Gtk.UI_Manager.Gtk_UI_Manager, setting up accelerator paths will be done
   --  automatically.
   --  Even when you you aren't using Gtk.UI_Manager.Gtk_UI_Manager, if you
   --  only want to set up accelerators on menu items
   --  Gtk.Menu_Item.Set_Accel_Path provides a somewhat more convenient
   --  interface.
   --  Note that Accel_Path string will be stored in a Glib.GQuark. Therefore,
   --  if you pass a static string, you can save some memory by interning it
   --  first with g_intern_static_string.
   --  "accel_path": path used to look up the accelerator
   --  "accel_group": a Gtk.Accel_Group.Gtk_Accel_Group.

   procedure Set_Redraw_On_Allocate
      (Widget             : not null access Gtk_Widget_Record;
       Redraw_On_Allocate : Boolean);
   --  Sets whether the entire widget is queued for drawing when its size
   --  allocation changes. By default, this setting is True and the entire
   --  widget is redrawn on every size change. If your widget leaves the upper
   --  left unchanged when made bigger, turning this setting off will improve
   --  performance.
   --  Note that for widgets where Gtk.Widget.Get_Has_Window is False setting
   --  this flag to False turns off all allocation on resizing: the widget will
   --  not even redraw if its position changes; this is to allow containers
   --  that don't draw anything to avoid excess invalidations. If you set this
   --  flag on a widget with no window that does draw on Widget->window, you
   --  are responsible for invalidating both the old and new allocation of the
   --  widget when the widget is moved and responsible for invalidating regions
   --  newly when the widget increases size.
   --  "redraw_on_allocate": if True, the entire widget will be redrawn when
   --  it is allocated to a new size. Otherwise, only the new portion of the
   --  widget will be redrawn.

   procedure Shape_Combine_Region
      (Widget : not null access Gtk_Widget_Record;
       Region : Cairo.Region.Cairo_Region);
   --  Sets a shape for this widget's GDK window. This allows for transparent
   --  windows etc., see Gdk.Window.Shape_Combine_Region for more information.
   --  Since: gtk+ 3.0
   --  "region": shape to be added, or null to remove an existing shape

   procedure Show (Widget : not null access Gtk_Widget_Record);
   --  Flags a widget to be displayed. Any widget that isn't shown will not
   --  appear on the screen. If you want to show all the widgets in a
   --  container, it's easier to call Gtk.Widget.Show_All on the container,
   --  instead of individually showing the widgets.
   --  Remember that you have to show the containers containing a widget, in
   --  addition to the widget itself, before it will appear onscreen.
   --  When a toplevel container is shown, it is immediately realized and
   --  mapped; other shown widgets are realized and mapped when their toplevel
   --  container is realized and mapped.

   procedure Show_All (Widget : not null access Gtk_Widget_Record);
   --  Recursively shows a widget, and any child widgets (if the widget is a
   --  container).

   procedure Show_Now (Widget : not null access Gtk_Widget_Record);
   --  Shows a widget. If the widget is an unmapped toplevel widget (i.e. a
   --  Gtk.Window.Gtk_Window that has not yet been shown), enter the main loop
   --  and wait for the window to actually be mapped. Be careful; because the
   --  main loop is running, anything can happen during this function.

   procedure Size_Allocate
      (Widget     : not null access Gtk_Widget_Record;
       Allocation : Gtk_Allocation);
   --  This function is only used by Gtk.Container.Gtk_Container subclasses,
   --  to assign a size and position to their child widgets.
   --  In this function, the allocation may be adjusted. It will be forced to
   --  a 1x1 minimum size, and the adjust_size_allocation virtual method on the
   --  child will be used to adjust the allocation. Standard adjustments
   --  include removing the widget's margins, and applying the widget's
   --  Gtk.Widget.Gtk_Widget:halign and Gtk.Widget.Gtk_Widget:valign
   --  properties.
   --  For baseline support in containers you need to use
   --  Gtk.Widget.Size_Allocate_With_Baseline instead.
   --  "allocation": position and size to be allocated to Widget

   procedure Size_Allocate_With_Baseline
      (Widget     : not null access Gtk_Widget_Record;
       Allocation : in out Gtk_Allocation;
       Baseline   : Glib.Gint);
   --  This function is only used by Gtk.Container.Gtk_Container subclasses,
   --  to assign a size, position and (optionally) baseline to their child
   --  widgets.
   --  In this function, the allocation and baseline may be adjusted. It will
   --  be forced to a 1x1 minimum size, and the adjust_size_allocation virtual
   --  and adjust_baseline_allocation methods on the child will be used to
   --  adjust the allocation and baseline. Standard adjustments include
   --  removing the widget's margins, and applying the widget's
   --  Gtk.Widget.Gtk_Widget:halign and Gtk.Widget.Gtk_Widget:valign
   --  properties.
   --  If the child widget does not have a valign of Gtk.Widget.Align_Baseline
   --  the baseline argument is ignored and -1 is used instead.
   --  Since: gtk+ 3.10
   --  "allocation": position and size to be allocated to Widget
   --  "baseline": The baseline of the child, or -1

   procedure Style_Attach (Widget : not null access Gtk_Widget_Record);
   pragma Obsolescent (Style_Attach);
   --  This function attaches the widget's Gtk.Style.Gtk_Style to the widget's
   --  Gdk.Gdk_Window. It is a replacement for
   --  |[ widget->style = gtk_style_attach (widget->style, widget->window); ]|
   --  and should only ever be called in a derived widget's "realize"
   --  implementation which does not chain up to its parent class' "realize"
   --  implementation, because one of the parent classes (finally
   --  Gtk.Widget.Gtk_Widget) would attach the style itself.
   --  Since: gtk+ 2.20
   --  Deprecated since 3.0, 1

   procedure Style_Get_Property
      (Widget        : not null access Gtk_Widget_Record;
       Property_Name : UTF8_String;
       Value         : in out Glib.Values.GValue);
   --  Gets the value of a style property of Widget.
   --  "property_name": the name of a style property
   --  "value": location to return the property value

   procedure Thaw_Child_Notify (Widget : not null access Gtk_Widget_Record);
   --  Reverts the effect of a previous call to
   --  Gtk.Widget.Freeze_Child_Notify. This causes all queued
   --  Gtk.Widget.Gtk_Widget::child-notify signals on Widget to be emitted.

   procedure Translate_Coordinates
      (Widget      : not null access Gtk_Widget_Record;
       Dest_Widget : not null access Gtk_Widget_Record'Class;
       Src_X       : Glib.Gint;
       Src_Y       : Glib.Gint;
       Dest_X      : out Glib.Gint;
       Dest_Y      : out Glib.Gint;
       Result      : out Boolean);
   --  Translate coordinates relative to Src_Widget's allocation to
   --  coordinates relative to Dest_Widget's allocations. In order to perform
   --  this operation, both widgets must be realized, and must share a common
   --  toplevel.
   --  "dest_widget": a Gtk.Widget.Gtk_Widget
   --  "src_x": X position relative to Src_Widget
   --  "src_y": Y position relative to Src_Widget
   --  "dest_x": location to store X position relative to Dest_Widget
   --  "dest_y": location to store Y position relative to Dest_Widget

   procedure Trigger_Tooltip_Query
      (Widget : not null access Gtk_Widget_Record);
   --  Triggers a tooltip query on the display where the toplevel of Widget is
   --  located. See Gtk.Tooltip.Trigger_Tooltip_Query for more information.
   --  Since: gtk+ 2.12

   procedure Unmap (Widget : not null access Gtk_Widget_Record);
   --  This function is only for use in widget implementations. Causes a
   --  widget to be unmapped if it's currently mapped.

   procedure Unparent (Widget : not null access Gtk_Widget_Record);
   --  This function is only for use in widget implementations. Should be
   --  called by implementations of the remove method on
   --  Gtk.Container.Gtk_Container, to dissociate a child from the container.

   procedure Unrealize (Widget : not null access Gtk_Widget_Record);
   --  This function is only useful in widget implementations. Causes a widget
   --  to be unrealized (frees all GDK resources associated with the widget,
   --  such as Widget->window).

   procedure Unregister_Window
      (Widget : not null access Gtk_Widget_Record;
       Window : Gdk.Gdk_Window);
   --  Unregisters a Gdk.Gdk_Window from the widget that was previously set up
   --  with Gtk.Widget.Register_Window. You need to call this when the window
   --  is no longer used by the widget, such as when you destroy it.
   --  Since: gtk+ 3.8
   --  "window": a Gdk.Gdk_Window

   procedure Unset_State_Flags
      (Widget : not null access Gtk_Widget_Record;
       Flags  : Gtk.Enums.Gtk_State_Flags);
   --  This function is for use in widget implementations. Turns off flag
   --  values for the current widget state (insensitive, prelighted, etc.). See
   --  Gtk.Widget.Set_State_Flags.
   --  Since: gtk+ 3.0
   --  "flags": State flags to turn off

   procedure Bind_Template_Child_Full
      (Self           : Glib.Object.GObject_Class;
       Name           : UTF8_String;
       Internal_Child : Boolean;
       Struct_Offset  : Gssize);
   --  Automatically assign an object declared in the class template XML to be
   --  set to a location on a freshly built instance's private data, or
   --  alternatively accessible via Gtk.Widget.Get_Template_Child.
   --  The struct can point either into the public instance, then you should
   --  use G_STRUCT_OFFSET(WidgetType, member) for Struct_Offset, or in the
   --  private struct, then you should use G_PRIVATE_OFFSET(WidgetType,
   --  member).
   --  An explicit strong reference will be held automatically for the
   --  duration of your instance's life cycle, it will be released
   --  automatically when Gobject.Class.Gobject_Class.dispose runs on your
   --  instance and if a Struct_Offset that is != 0 is specified, then the
   --  automatic location in your instance public or private data will be set
   --  to null. You can however access an automated child pointer the first
   --  time your classes Gobject.Class.Gobject_Class.dispose runs, or
   --  alternatively in Gtk.Widget.GObject_Class.destroy.
   --  If Internal_Child is specified, Gtk_Buildable_Iface.get_internal_child
   --  will be automatically implemented by the Gtk.Widget.Gtk_Widget class so
   --  there is no need to implement it manually.
   --  The wrapper macros gtk_widget_class_bind_template_child,
   --  gtk_widget_class_bind_template_child_internal,
   --  gtk_widget_class_bind_template_child_private and
   --  gtk_widget_class_bind_template_child_internal_private might be more
   --  convenient to use.
   --  Note that this must be called from a composite widget classes class
   --  initializer after calling gtk_widget_class_set_template.
   --  Since: gtk+ 3.10
   --  "name": The "id" of the child defined in the template XML
   --  "internal_child": Whether the child should be accessible as an
   --  "internal-child" when this class is used in GtkBuilder XML
   --  "struct_offset": The structure offset into the composite widget's
   --  instance public or private structure where the automated child pointer
   --  should be set, or 0 to not assign the pointer.

   function Find_Style_Property
      (Self          : Glib.Object.GObject_Class;
       Property_Name : UTF8_String) return Glib.Param_Spec;
   --  Finds a style property of a widget class by name.
   --  Since: gtk+ 2.2
   --  "property_name": the name of the style property to find

   function Get_Css_Name
      (Self : Glib.Object.GObject_Class) return UTF8_String;
   --  Gets the name used by this class for matching in CSS code. See
   --  Gtk.Widget.Set_Css_Name for details.
   --  Since: gtk+ 3.20

   procedure Set_Css_Name
      (Self : Glib.Object.GObject_Class;
       Name : UTF8_String);
   --  Sets the name to be used for CSS matching of widgets.
   --  If this function is not called for a given class, the name of the
   --  parent class is used.
   --  Since: gtk+ 3.20
   --  "name": name to use

   procedure Install_Style_Property
      (Self  : Glib.Object.GObject_Class;
       Pspec : Glib.Param_Spec);
   pragma Import (C, Install_Style_Property, "gtk_widget_class_install_style_property");
   --  Installs a style property on a widget class. The parser for the style
   --  property is determined by the value type of Pspec.
   --  "pspec": the Glib.Param_Spec for the property

   procedure Set_Connect_Func
      (Self                 : Glib.Object.GObject_Class;
       Connect_Func         : Gtk_Builder_Connect_Func;
       Connect_Data_Destroy : Glib.G_Destroy_Notify_Address);
   --  For use in language bindings, this will override the default
   --  Gtk_Builder_Connect_Func to be used when parsing GtkBuilder XML from
   --  this class's template data.
   --  Note that this must be called from a composite widget classes class
   --  initializer after calling gtk_widget_class_set_template.
   --  Since: gtk+ 3.10
   --  "connect_func": The Gtk_Builder_Connect_Func to use when connecting
   --  signals in the class template
   --  "connect_data_destroy": The Glib.G_Destroy_Notify_Address to free
   --  Connect_Data, this will only be used at class finalization time, when no
   --  classes of type Widget_Type are in use anymore.

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Connect_Func_User_Data is

      type Gtk_Builder_Connect_Func is access procedure
        (Builder        : not null access Gtk.Builder.Gtk_Builder_Record'Class;
         Object         : not null access Glib.Object.GObject_Record'Class;
         Signal_Name    : UTF8_String;
         Handler_Name   : UTF8_String;
         Connect_Object : access Glib.Object.GObject_Record'Class;
         Flags          : Glib.G_Connect_Flags;
         User_Data      : User_Data_Type);
      --  This is the signature of a function used to connect signals. It is used
      --  by the Gtk.Builder.Connect_Signals and Gtk.Builder.Connect_Signals_Full
      --  methods. It is mainly intended for interpreted language bindings, but
      --  could be useful where the programmer wants more control over the signal
      --  connection process. Note that this function can only be called once,
      --  subsequent calls will do nothing.
      --  Since: gtk+ 2.12
      --  "builder": a Gtk.Builder.Gtk_Builder
      --  "object": object to connect a signal to
      --  "signal_name": name of the signal
      --  "handler_name": name of the handler
      --  "connect_object": a Glib.Object.GObject, if non-null, use
      --  g_signal_connect_object
      --  "flags": Glib.G_Connect_Flags to use
      --  "user_data": user data

      procedure Set_Connect_Func
         (Self                 : Glib.Object.GObject_Class;
          Connect_Func         : Gtk_Builder_Connect_Func;
          Connect_Data         : User_Data_Type;
          Connect_Data_Destroy : Glib.G_Destroy_Notify_Address);
      --  For use in language bindings, this will override the default
      --  Gtk_Builder_Connect_Func to be used when parsing GtkBuilder XML from
      --  this class's template data.
      --  Note that this must be called from a composite widget classes class
      --  initializer after calling gtk_widget_class_set_template.
      --  Since: gtk+ 3.10
      --  "connect_func": The Gtk_Builder_Connect_Func to use when connecting
      --  signals in the class template
      --  "connect_data": The data to pass to Connect_Func
      --  "connect_data_destroy": The Glib.G_Destroy_Notify_Address to free
      --  Connect_Data, this will only be used at class finalization time, when
      --  no classes of type Widget_Type are in use anymore.

   end Set_Connect_Func_User_Data;

   ----------------------
   -- GtkAda additions --
   ----------------------

   ------------------------------------
   -- Override default size handling --
   ------------------------------------

   type Gtk_Requisition_Access is access all Gtk_Requisition;
   type Gtk_Allocation_Access is access all Gtk_Allocation;
   pragma Convention (C, Gtk_Requisition_Access);
   pragma Convention (C, Gtk_Allocation_Access);

   function Get_Requisition
     (Value : Glib.Values.GValue) return Gtk_Requisition_Access;
   function Get_Allocation
     (Value : Glib.Values.GValue) return Gtk_Allocation_Access;
   --  Convert values received as callback parameters

   type Size_Allocate_Handler is access procedure
     (Widget : System.Address; Allocation : Gtk_Allocation);
   pragma Convention (C, Size_Allocate_Handler);
   --  Widget is the gtk+ C widget, that needs to be converted to Ada through
   --  a call to:
   --    declare
   --       Stub : Gtk_Widget_Record; --  or the exact type you expect
   --    begin
   --       My_Widget := Gtk_Widget (Glib.Object.Get_User_Data (Widget, Stub);
   --    end;

   procedure Set_Default_Size_Allocate_Handler
     (Klass   : Glib.Object.GObject_Class;
      Handler : Size_Allocate_Handler);
   pragma Import (C, Set_Default_Size_Allocate_Handler,
      "ada_WIDGET_CLASS_override_size_allocate");
   --  Override the default size_allocate handler for this class. This handler
   --  is automatically called in several cases (when a widget is dynamically
   --  resized for instance), not through a signal. Thus, if you need to
   --  override the default behavior provided by one of the standard
   --  containers, you can not simply use Gtk.Handlers.Emit_Stop_By_Name, and
   --  you must override the default handler. Note also that this handler
   --  is automatically inherited by children of this class.
   --
   --  This function is not needed unless you are writting your own
   --  widgets, and should be reserved for advanced customization of the
   --  standard widgets.

   type Preferred_Size_Handler is access procedure
     (Widget       : System.Address;
      Minimum_Size : out Glib.Gint;
      Natural_Size : out Glib.Gint);
   pragma Convention (C, Preferred_Size_Handler);

   type Preferred_Size_For_Handler is access procedure
     (Widget       : System.Address;
      Ref          : Glib.Gint;--  known width or height
      Minimum_Size : out Glib.Gint;
      Natural_Size : out Glib.Gint);
   pragma Convention (C, Preferred_Size_For_Handler);

   procedure Set_Default_Get_Preferred_Width_Handler
     (Klass   : Glib.Object.GObject_Class;
      Handler : Preferred_Size_Handler);
   procedure Set_Default_Get_Preferred_Height_Handler
     (Klass   : Glib.Object.GObject_Class;
      Handler : Preferred_Size_Handler);
   procedure Set_Default_Get_Preferred_Height_For_Width_Handler
     (Klass   : Glib.Object.GObject_Class;
      Handler : Preferred_Size_For_Handler);
   procedure Set_Default_Get_Preferred_Width_For_Height_Handler
     (Klass   : Glib.Object.GObject_Class;
      Handler : Preferred_Size_For_Handler);
   pragma Import (C, Set_Default_Get_Preferred_Width_Handler,
      "ada_WIDGET_CLASS_override_get_preferred_width");
   pragma Import (C, Set_Default_Get_Preferred_Height_Handler,
      "ada_WIDGET_CLASS_override_get_preferred_height");
   pragma Import (C, Set_Default_Get_Preferred_Height_For_Width_Handler,
      "ada_WIDGET_CLASS_override_get_preferred_height_for_width");
   pragma Import (C, Set_Default_Get_Preferred_Width_For_Height_Handler,
      "ada_WIDGET_CLASS_override_get_preferred_width_for_height");
   --  Override the computation of a widget's preferred sizes.
   --  You will only need to override this computation if you are writting
   --  your own container widgets.

   procedure Inherited_Get_Preferred_Width
     (Klass      : Glib.Object.Ada_GObject_Class;
      Widget     : access Gtk_Widget_Record'Class;
      Minimum_Size, Natural_Size : out Glib.Gint);
   procedure Inherited_Get_Preferred_Width_For_Height
     (Klass      : Glib.Object.Ada_GObject_Class;
      Widget     : access Gtk_Widget_Record'Class;
      Height     : Glib.Gint;
      Minimum_Size, Natural_Size : out Glib.Gint);
   procedure Inherited_Get_Preferred_Height
     (Klass      : Glib.Object.Ada_GObject_Class;
      Widget     : access Gtk_Widget_Record'Class;
      Minimum_Size, Natural_Size : out Glib.Gint);
   procedure Inherited_Get_Preferred_Height_For_Width
     (Klass      : Glib.Object.Ada_GObject_Class;
      Widget     : access Gtk_Widget_Record'Class;
      Width      : Glib.Gint;
      Minimum_Size, Natural_Size : out Glib.Gint);
   --  Call the default implementation

   procedure Inherited_Size_Allocate
     (Klass      : Glib.Object.Ada_GObject_Class;
      Widget     : access Gtk_Widget_Record'Class;
      Allocation : Gtk_Allocation);
   --  Call the inherited size_allocate. This is useful if you have overloaded it in
   --  your own class, but still need to call the standard implementation.

   type Realize_Handler is access procedure (Widget : System.Address);
   pragma Convention (C, Realize_Handler);
   procedure Set_Default_Realize_Handler
     (Klass   : Glib.Object.GObject_Class;
      Handler : Realize_Handler);
   pragma Import (C, Set_Default_Realize_Handler,
      "ada_WIDGET_CLASS_override_realize");
   --  Override the handler for the "realize" signal. This handler should
   --  create the window for the widget

   procedure Inherited_Realize
     (Klass      : Glib.Object.Ada_GObject_Class;
      Widget     : access Gtk_Widget_Record'Class);
   --  Call the inherited realize.

   ---------------------------
   -- Override Draw handler --
   ---------------------------

   generic
   with function Draw
     (W  : access Gtk_Widget_Record'Class;
      Cr : Cairo.Cairo_Context) return Boolean;
   function Proxy_Draw
     (W  : System.Address; Cr : Cairo.Cairo_Context) return Gboolean;
   pragma Convention (C, Proxy_Draw);

   type Draw_Handler is access function
     (W  : System.Address;
      Cr : Cairo.Cairo_Context) return Gboolean;
   pragma Convention (C, Draw_Handler);
   --  A function responsible for drawing a widget.

   procedure Set_Default_Draw_Handler
     (Klass : Glib.Object.GObject_Class; Handler : Draw_Handler);
   --  Override the default drawing function. This in general gives more
   --  control than connection to Signal_Draw, however a widget is responsible
   --  for drawing its children.
   --  Use the generic Proxy_Draw to create a suitable callback.

   function Inherited_Draw
     (Klass  : Glib.Object.Ada_GObject_Class;
      Widget : access Gtk_Widget_Record'Class;
      Cr     : Cairo.Cairo_Context) return Boolean;
   --  Call the inherited draw. This is useful if you have overloaded draw in
   --  your own class, but still need to draw the child widgets that do not
   --  have their own window (the others will already get their own "draw"
   --  event.
   --  See http://developer.gnome.org/gtk3/3.0/chap-drawing-model.html
   --  for an explanation of the gtk+ drawing model.

   ---------------
   -- Functions --
   ---------------

   function Get_Default_Direction return Gtk.Enums.Gtk_Text_Direction;
   --  Obtains the current default reading direction. See
   --  Gtk.Widget.Set_Default_Direction.

   procedure Set_Default_Direction (Dir : Gtk.Enums.Gtk_Text_Direction);
   --  Sets the default reading direction for widgets where the direction has
   --  not been explicitly set by Gtk.Widget.Set_Direction.
   --  "dir": the new default direction. This cannot be
   --  Gtk.Enums.Text_Dir_None.

   function Get_Default_Style return Gtk.Style.Gtk_Style;
   pragma Obsolescent (Get_Default_Style);
   --  Returns the default style used by all widgets initially.
   --  Deprecated since 3.0, 1

   procedure Pop_Composite_Child;
   pragma Obsolescent (Pop_Composite_Child);
   --  Cancels the effect of a previous call to
   --  Gtk.Widget.Push_Composite_Child.
   --  Deprecated since 3.10, 1

   procedure Push_Composite_Child;
   pragma Obsolescent (Push_Composite_Child);
   --  Makes all newly-created widgets as composite children until the
   --  corresponding Gtk.Widget.Pop_Composite_Child call.
   --  A composite child is a child that's an implementation detail of the
   --  container it's inside and should not be visible to people using the
   --  container. Composite children aren't treated differently by GTK+ (but
   --  see Gtk.Container.Foreach vs. Gtk.Container.Forall), but e.g. GUI
   --  builders might want to treat them in a different way.
   --  Deprecated since 3.10, 1

   procedure Transform_To_Window
      (Cr     : Cairo.Cairo_Context;
       Widget : not null access Gtk_Widget_Record'Class;
       Window : Gdk.Gdk_Window);
   --  Transforms the given cairo context Cr that from Widget-relative
   --  coordinates to Window-relative coordinates. If the Widget's window is
   --  not an ancestor of Window, no modification will be applied.
   --  This is the inverse to the transformation GTK applies when preparing an
   --  expose event to be emitted with the Gtk.Widget.Gtk_Widget::draw signal.
   --  It is intended to help porting multiwindow widgets from GTK+ 2 to the
   --  rendering architecture of GTK+ 3.
   --  Since: gtk+ 3.0
   --  "cr": the cairo context to transform
   --  "widget": the widget the context is currently centered for
   --  "window": the window to transform the context to

   function Should_Draw_Window
      (Cr     : Cairo.Cairo_Context;
       Window : Gdk.Gdk_Window) return Boolean;
   --  This function is supposed to be called in Gtk.Widget.Gtk_Widget::draw
   --  implementations for widgets that support multiple windows. Cr must be
   --  untransformed from invoking of the draw function. This function will
   --  return True if the contents of the given Window are supposed to be drawn
   --  and False otherwise. Note that when the drawing was not initiated by the
   --  windowing system this function will return True for all windows, so you
   --  need to draw the bottommost window first. Also, do not use "else if"
   --  statements to check which window should be drawn.
   --  Since: gtk+ 3.0
   --  "cr": a cairo context
   --  "window": the window to check. Window may not be an input-only window.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   App_Paintable_Property : constant Glib.Properties.Property_Boolean;

   Can_Default_Property : constant Glib.Properties.Property_Boolean;

   Can_Focus_Property : constant Glib.Properties.Property_Boolean;

   Composite_Child_Property : constant Glib.Properties.Property_Boolean;

   Double_Buffered_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the widget is double buffered.

   Events_Property : constant Gdk.Event.Property_Gdk_Event_Mask;
   --  Type: Gdk.Event.Gdk_Event_Mask

   Expand_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to expand in both directions. Setting this sets both
   --  Gtk.Widget.Gtk_Widget:hexpand and Gtk.Widget.Gtk_Widget:vexpand

   Focus_On_Click_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the widget should grab focus when it is clicked with the mouse.
   --
   --  This property is only relevant for widgets that can take focus.
   --
   --  Before 3.20, several widgets (GtkButton, GtkFileChooserButton,
   --  GtkComboBox) implemented this property individually.

   Halign_Property : constant Gtk.Widget.Property_Gtk_Align;
   --  Type: Gtk_Align
   --  How to distribute horizontal space if widget gets extra space, see
   --  Gtk.Widget.Gtk_Align

   Has_Default_Property : constant Glib.Properties.Property_Boolean;

   Has_Focus_Property : constant Glib.Properties.Property_Boolean;

   Has_Tooltip_Property : constant Glib.Properties.Property_Boolean;
   --  Enables or disables the emission of
   --  Gtk.Widget.Gtk_Widget::query-tooltip on Widget. A value of True
   --  indicates that Widget can have a tooltip, in this case the widget will
   --  be queried using Gtk.Widget.Gtk_Widget::query-tooltip to determine
   --  whether it will provide a tooltip or not.
   --
   --  Note that setting this property to True for the first time will change
   --  the event masks of the GdkWindows of this widget to include leave-notify
   --  and motion-notify events. This cannot and will not be undone when the
   --  property is set to False again.

   Height_Request_Property : constant Glib.Properties.Property_Int;

   Hexpand_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to expand horizontally. See Gtk.Widget.Set_Hexpand.

   Hexpand_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to use the Gtk.Widget.Gtk_Widget:hexpand property. See
   --  Gtk.Widget.Get_Hexpand_Set.

   Is_Focus_Property : constant Glib.Properties.Property_Boolean;

   Margin_Property : constant Glib.Properties.Property_Int;
   --  Sets all four sides' margin at once. If read, returns max margin on any
   --  side.

   Margin_Bottom_Property : constant Glib.Properties.Property_Int;
   --  Margin on bottom side of widget.
   --
   --  This property adds margin outside of the widget's normal size request,
   --  the margin will be added in addition to the size from
   --  Gtk.Widget.Set_Size_Request for example.

   Margin_End_Property : constant Glib.Properties.Property_Int;
   --  Margin on end of widget, horizontally. This property supports
   --  left-to-right and right-to-left text directions.
   --
   --  This property adds margin outside of the widget's normal size request,
   --  the margin will be added in addition to the size from
   --  Gtk.Widget.Set_Size_Request for example.

   Margin_Left_Property : constant Glib.Properties.Property_Int;
   --  Margin on left side of widget.
   --
   --  This property adds margin outside of the widget's normal size request,
   --  the margin will be added in addition to the size from
   --  Gtk.Widget.Set_Size_Request for example.

   Margin_Right_Property : constant Glib.Properties.Property_Int;
   --  Margin on right side of widget.
   --
   --  This property adds margin outside of the widget's normal size request,
   --  the margin will be added in addition to the size from
   --  Gtk.Widget.Set_Size_Request for example.

   Margin_Start_Property : constant Glib.Properties.Property_Int;
   --  Margin on start of widget, horizontally. This property supports
   --  left-to-right and right-to-left text directions.
   --
   --  This property adds margin outside of the widget's normal size request,
   --  the margin will be added in addition to the size from
   --  Gtk.Widget.Set_Size_Request for example.

   Margin_Top_Property : constant Glib.Properties.Property_Int;
   --  Margin on top side of widget.
   --
   --  This property adds margin outside of the widget's normal size request,
   --  the margin will be added in addition to the size from
   --  Gtk.Widget.Set_Size_Request for example.

   Name_Property : constant Glib.Properties.Property_String;

   No_Show_All_Property : constant Glib.Properties.Property_Boolean;

   Opacity_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The requested opacity of the widget. See Gtk.Widget.Set_Opacity for
   --  more details about window opacity.
   --
   --  Before 3.8 this was only available in GtkWindow

   Parent_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Container.Gtk_Container

   Receives_Default_Property : constant Glib.Properties.Property_Boolean;

   Scale_Factor_Property : constant Glib.Properties.Property_Int;
   --  The scale factor of the widget. See Gtk.Widget.Get_Scale_Factor for
   --  more details about widget scaling.

   Sensitive_Property : constant Glib.Properties.Property_Boolean;

   Style_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Style.Gtk_Style
   --  The style of the widget, which contains information about how it will
   --  look (colors, etc).

   Tooltip_Markup_Property : constant Glib.Properties.Property_String;
   --  Sets the text of tooltip to be the given string, which is marked up
   --  with the [Pango text markup language][PangoMarkupFormat]. Also see
   --  Gtk.Tooltip.Set_Markup.
   --
   --  This is a convenience property which will take care of getting the
   --  tooltip shown if the given string is not null:
   --  Gtk.Widget.Gtk_Widget:has-tooltip will automatically be set to True and
   --  there will be taken care of Gtk.Widget.Gtk_Widget::query-tooltip in the
   --  default signal handler.
   --
   --  Note that if both Gtk.Widget.Gtk_Widget:tooltip-text and
   --  Gtk.Widget.Gtk_Widget:tooltip-markup are set, the last one wins.

   Tooltip_Text_Property : constant Glib.Properties.Property_String;
   --  Sets the text of tooltip to be the given string.
   --
   --  Also see Gtk.Tooltip.Set_Text.
   --
   --  This is a convenience property which will take care of getting the
   --  tooltip shown if the given string is not null:
   --  Gtk.Widget.Gtk_Widget:has-tooltip will automatically be set to True and
   --  there will be taken care of Gtk.Widget.Gtk_Widget::query-tooltip in the
   --  default signal handler.
   --
   --  Note that if both Gtk.Widget.Gtk_Widget:tooltip-text and
   --  Gtk.Widget.Gtk_Widget:tooltip-markup are set, the last one wins.

   Valign_Property : constant Gtk.Widget.Property_Gtk_Align;
   --  Type: Gtk_Align
   --  How to distribute vertical space if widget gets extra space, see
   --  Gtk.Widget.Gtk_Align

   Vexpand_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to expand vertically. See Gtk.Widget.Set_Vexpand.

   Vexpand_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to use the Gtk.Widget.Gtk_Widget:vexpand property. See
   --  Gtk.Widget.Get_Vexpand_Set.

   Visible_Property : constant Glib.Properties.Property_Boolean;

   Width_Request_Property : constant Glib.Properties.Property_Int;

   Window_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gdk.Window
   --  The widget's window if it is realized, null otherwise.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Widget_Void is not null access procedure (Self : access Gtk_Widget_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Accel_Closures_Changed : constant Glib.Signal_Name := "accel-closures-changed";
   procedure On_Accel_Closures_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Accel_Closures_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   type Cb_Gtk_Widget_Gdk_Event_Button_Boolean is not null access function
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean;

   type Cb_GObject_Gdk_Event_Button_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean;

   Signal_Button_Press_Event : constant Glib.Signal_Name := "button-press-event";
   procedure On_Button_Press_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Button_Boolean;
       After : Boolean := False);
   procedure On_Button_Press_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Button_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::button-press-event signal will be emitted when a button
   --  (typically from a mouse) is pressed.
   --
   --  To receive this signal, the Gdk.Gdk_Window associated to the widget
   --  needs to enable the GDK_BUTTON_PRESS_MASK mask.
   --
   --  This signal will be sent to the grab widget if there is one.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Button which triggered this signal.
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

   Signal_Button_Release_Event : constant Glib.Signal_Name := "button-release-event";
   procedure On_Button_Release_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Button_Boolean;
       After : Boolean := False);
   procedure On_Button_Release_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Button_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::button-release-event signal will be emitted when a button
   --  (typically from a mouse) is released.
   --
   --  To receive this signal, the Gdk.Gdk_Window associated to the widget
   --  needs to enable the GDK_BUTTON_RELEASE_MASK mask.
   --
   --  This signal will be sent to the grab widget if there is one.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Button which triggered this signal.
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

   type Cb_Gtk_Widget_Guint_Boolean is not null access function
     (Self      : access Gtk_Widget_Record'Class;
      Signal_Id : Guint) return Boolean;

   type Cb_GObject_Guint_Boolean is not null access function
     (Self      : access Glib.Object.GObject_Record'Class;
      Signal_Id : Guint) return Boolean;

   Signal_Can_Activate_Accel : constant Glib.Signal_Name := "can-activate-accel";
   procedure On_Can_Activate_Accel
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Guint_Boolean;
       After : Boolean := False);
   procedure On_Can_Activate_Accel
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Guint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Determines whether an accelerator that activates the signal identified
   --  by Signal_Id can currently be activated. This signal is present to allow
   --  applications and derived widgets to override the default
   --  Gtk.Widget.Gtk_Widget handling for determining whether an accelerator
   --  can be activated.
   -- 
   --  Callback parameters:
   --    --  "signal_id": the ID of a signal installed on Widget
   --    --  Returns True if the signal can be activated.

   type Cb_Gtk_Widget_Param_Spec_Void is not null access procedure
     (Self           : access Gtk_Widget_Record'Class;
      Child_Property : Glib.Param_Spec);

   type Cb_GObject_Param_Spec_Void is not null access procedure
     (Self           : access Glib.Object.GObject_Record'Class;
      Child_Property : Glib.Param_Spec);

   Signal_Child_Notify : constant Glib.Signal_Name := "child-notify";
   procedure On_Child_Notify
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Param_Spec_Void;
       After : Boolean := False);
   procedure On_Child_Notify
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Param_Spec_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::child-notify signal is emitted for each [child
   --  property][child-properties] that has changed on an object. The signal's
   --  detail holds the property name.

   Signal_Composited_Changed : constant Glib.Signal_Name := "composited-changed";
   procedure On_Composited_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Composited_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::composited-changed signal is emitted when the composited status
   --  of Widgets screen changes. See Gdk.Screen.Is_Composited.

   type Cb_Gtk_Widget_Gdk_Event_Configure_Boolean is not null access function
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Configure) return Boolean;

   type Cb_GObject_Gdk_Event_Configure_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Configure) return Boolean;

   Signal_Configure_Event : constant Glib.Signal_Name := "configure-event";
   procedure On_Configure_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Configure_Boolean;
       After : Boolean := False);
   procedure On_Configure_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Configure_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::configure-event signal will be emitted when the size, position or
   --  stacking of the Widget's window has changed.
   --
   --  To receive this signal, the Gdk.Gdk_Window associated to the widget
   --  needs to enable the GDK_STRUCTURE_MASK mask. GDK will enable this mask
   --  automatically for all new windows.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Configure which triggered this signal.
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

   type Cb_Gtk_Widget_Gdk_Event_Expose_Boolean is not null access function
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Expose) return Boolean;

   type Cb_GObject_Gdk_Event_Expose_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Expose) return Boolean;

   Signal_Damage_Event : constant Glib.Signal_Name := "damage-event";
   procedure On_Damage_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Expose_Boolean;
       After : Boolean := False);
   procedure On_Damage_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Expose_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a redirected window belonging to Widget gets drawn into.
   --  The region/area members of the event shows what area of the redirected
   --  drawable was drawn into.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Expose event
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

   type Cb_Gtk_Widget_Gdk_Event_Boolean is not null access function
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean;

   type Cb_GObject_Gdk_Event_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean;

   Signal_Delete_Event : constant Glib.Signal_Name := "delete-event";
   procedure On_Delete_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Boolean;
       After : Boolean := False);
   procedure On_Delete_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::delete-event signal is emitted if a user requests that a toplevel
   --  window is closed. The default handler for this signal destroys the
   --  window. Connecting Gtk.Widget.Hide_On_Delete to this signal will cause
   --  the window to be hidden instead, so that it can later be shown again
   --  without reconstructing it.
   -- 
   --  Callback parameters:
   --    --  "event": the event which triggered this signal
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

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
   --  the reference that they hold. May result in finalization of the widget
   --  if all references are released.
   --
   --  This signal is not suitable for saving widget state.

   Signal_Destroy_Event : constant Glib.Signal_Name := "destroy-event";
   procedure On_Destroy_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Boolean;
       After : Boolean := False);
   procedure On_Destroy_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::destroy-event signal is emitted when a Gdk.Gdk_Window is
   --  destroyed. You rarely get this signal, because most widgets disconnect
   --  themselves from their window before they destroy it, so no widget owns
   --  the window at destroy time.
   --
   --  To receive this signal, the Gdk.Gdk_Window associated to the widget
   --  needs to enable the GDK_STRUCTURE_MASK mask. GDK will enable this mask
   --  automatically for all new windows.
   -- 
   --  Callback parameters:
   --    --  "event": the event which triggered this signal
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

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
   --  The ::direction-changed signal is emitted when the text direction of a
   --  widget changes.

   type Cb_Gtk_Widget_Drag_Context_Void is not null access procedure
     (Self    : access Gtk_Widget_Record'Class;
      Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class);

   type Cb_GObject_Drag_Context_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class);

   Signal_Drag_Begin : constant Glib.Signal_Name := "drag-begin";
   procedure On_Drag_Begin
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Drag_Context_Void;
       After : Boolean := False);
   procedure On_Drag_Begin
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Drag_Context_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::drag-begin signal is emitted on the drag source when a drag is
   --  started. A typical reason to connect to this signal is to set up a
   --  custom drag icon with e.g. Gtk.Widget.Drag_Source_Set_Icon_Pixbuf.
   --
   --  Note that some widgets set up a drag icon in the default handler of
   --  this signal, so you may have to use g_signal_connect_after to override
   --  what the default handler did.

   Signal_Drag_Data_Delete : constant Glib.Signal_Name := "drag-data-delete";
   procedure On_Drag_Data_Delete
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Drag_Context_Void;
       After : Boolean := False);
   procedure On_Drag_Data_Delete
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Drag_Context_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::drag-data-delete signal is emitted on the drag source when a drag
   --  with the action Gdk.Drag_Contexts.Action_Move is successfully completed.
   --  The signal handler is responsible for deleting the data that has been
   --  dropped. What "delete" means depends on the context of the drag
   --  operation.

   type Cb_Gtk_Widget_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void is not null access procedure
     (Self    : access Gtk_Widget_Record'Class;
      Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class;
      Data    : Gtk.Selection_Data.Gtk_Selection_Data;
      Info    : Guint;
      Time    : Guint);

   type Cb_GObject_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class;
      Data    : Gtk.Selection_Data.Gtk_Selection_Data;
      Info    : Guint;
      Time    : Guint);

   Signal_Drag_Data_Get : constant Glib.Signal_Name := "drag-data-get";
   procedure On_Drag_Data_Get
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void;
       After : Boolean := False);
   procedure On_Drag_Data_Get
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::drag-data-get signal is emitted on the drag source when the drop
   --  site requests the data which is dragged. It is the responsibility of the
   --  signal handler to fill Data with the data in the format which is
   --  indicated by Info. See gtk_selection_data_set and
   --  Gtk.Selection_Data.Set_Text.
   -- 
   --  Callback parameters:
   --    --  "context": the drag context
   --    --  "data": the Gtk.Selection_Data.Gtk_Selection_Data to be filled with the
   --    --  dragged data
   --    --  "info": the info that has been registered with the target in the
   --    --  Gtk.Target_List.Gtk_Target_List
   --    --  "time": the timestamp at which the data was requested

   type Cb_Gtk_Widget_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void is not null access procedure
     (Self    : access Gtk_Widget_Record'Class;
      Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class;
      X       : Glib.Gint;
      Y       : Glib.Gint;
      Data    : Gtk.Selection_Data.Gtk_Selection_Data;
      Info    : Guint;
      Time    : Guint);

   type Cb_GObject_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class;
      X       : Glib.Gint;
      Y       : Glib.Gint;
      Data    : Gtk.Selection_Data.Gtk_Selection_Data;
      Info    : Guint;
      Time    : Guint);

   Signal_Drag_Data_Received : constant Glib.Signal_Name := "drag-data-received";
   procedure On_Drag_Data_Received
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void;
       After : Boolean := False);
   procedure On_Drag_Data_Received
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::drag-data-received signal is emitted on the drop site when the
   --  dragged data has been received. If the data was received in order to
   --  determine whether the drop will be accepted, the handler is expected to
   --  call gdk_drag_status and not finish the drag. If the data was received
   --  in response to a Gtk.Widget.Gtk_Widget::drag-drop signal (and this is
   --  the last target to be received), the handler for this signal is expected
   --  to process the received data and then call Gtk.Dnd.Finish, setting the
   --  Success parameter depending on whether the data was processed
   --  successfully.
   --
   --  Applications must create some means to determine why the signal was
   --  emitted and therefore whether to call gdk_drag_status or Gtk.Dnd.Finish.
   --
   --  The handler may inspect the selected action with
   --  Gdk.Drag_Contexts.Get_Selected_Action before calling Gtk.Dnd.Finish,
   --  e.g. to implement Gdk.Drag_Contexts.Action_Ask as shown in the following
   --  example: |[<!-- language="C" --> void drag_data_received (GtkWidget
   --  *widget, GdkDragContext *context, gint x, gint y, GtkSelectionData
   --  *data, guint info, guint time) { if ((data->length >= 0) &&
   --  (data->format == 8)) { GdkDragAction action;
   --
   --  // handle data here
   --
   --  action = gdk_drag_context_get_selected_action (context); if (action ==
   --  GDK_ACTION_ASK) { GtkWidget *dialog; gint response;
   --
   --  dialog = gtk_message_dialog_new (NULL, GTK_DIALOG_MODAL |
   --  GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_INFO, GTK_BUTTONS_YES_NO,
   --  "Move the data ?\n"); response = gtk_dialog_run (GTK_DIALOG (dialog));
   --  gtk_widget_destroy (dialog);
   --
   --  if (response == GTK_RESPONSE_YES) action = GDK_ACTION_MOVE; else action
   --  = GDK_ACTION_COPY; }
   --
   --  gtk_drag_finish (context, TRUE, action == GDK_ACTION_MOVE, time); }
   --  else gtk_drag_finish (context, FALSE, FALSE, time); } ]|
   -- 
   --  Callback parameters:
   --    --  "context": the drag context
   --    --  "x": where the drop happened
   --    --  "y": where the drop happened
   --    --  "data": the received data
   --    --  "info": the info that has been registered with the target in the
   --    --  Gtk.Target_List.Gtk_Target_List
   --    --  "time": the timestamp at which the data was received

   type Cb_Gtk_Widget_Drag_Context_Gint_Gint_Guint_Boolean is not null access function
     (Self    : access Gtk_Widget_Record'Class;
      Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class;
      X       : Glib.Gint;
      Y       : Glib.Gint;
      Time    : Guint) return Boolean;

   type Cb_GObject_Drag_Context_Gint_Gint_Guint_Boolean is not null access function
     (Self    : access Glib.Object.GObject_Record'Class;
      Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class;
      X       : Glib.Gint;
      Y       : Glib.Gint;
      Time    : Guint) return Boolean;

   Signal_Drag_Drop : constant Glib.Signal_Name := "drag-drop";
   procedure On_Drag_Drop
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Drag_Context_Gint_Gint_Guint_Boolean;
       After : Boolean := False);
   procedure On_Drag_Drop
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Drag_Context_Gint_Gint_Guint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::drag-drop signal is emitted on the drop site when the user drops
   --  the data onto the widget. The signal handler must determine whether the
   --  cursor position is in a drop zone or not. If it is not in a drop zone,
   --  it returns False and no further processing is necessary. Otherwise, the
   --  handler returns True. In this case, the handler must ensure that
   --  Gtk.Dnd.Finish is called to let the source know that the drop is done.
   --  The call to Gtk.Dnd.Finish can be done either directly or in a
   --  Gtk.Widget.Gtk_Widget::drag-data-received handler which gets triggered
   --  by calling Gtk.Widget.Drag_Get_Data to receive the data for one or more
   --  of the supported targets.
   -- 
   --  Callback parameters:
   --    --  "context": the drag context
   --    --  "x": the x coordinate of the current cursor position
   --    --  "y": the y coordinate of the current cursor position
   --    --  "time": the timestamp of the motion event
   --    --  Returns whether the cursor position is in a drop zone

   Signal_Drag_End : constant Glib.Signal_Name := "drag-end";
   procedure On_Drag_End
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Drag_Context_Void;
       After : Boolean := False);
   procedure On_Drag_End
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Drag_Context_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::drag-end signal is emitted on the drag source when a drag is
   --  finished. A typical reason to connect to this signal is to undo things
   --  done in Gtk.Widget.Gtk_Widget::drag-begin.

   type Cb_Gtk_Widget_Drag_Context_Gtk_Drag_Result_Boolean is not null access function
     (Self    : access Gtk_Widget_Record'Class;
      Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class;
      Result  : Gtk.Enums.Gtk_Drag_Result) return Boolean;

   type Cb_GObject_Drag_Context_Gtk_Drag_Result_Boolean is not null access function
     (Self    : access Glib.Object.GObject_Record'Class;
      Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class;
      Result  : Gtk.Enums.Gtk_Drag_Result) return Boolean;

   Signal_Drag_Failed : constant Glib.Signal_Name := "drag-failed";
   procedure On_Drag_Failed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Drag_Context_Gtk_Drag_Result_Boolean;
       After : Boolean := False);
   procedure On_Drag_Failed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Drag_Context_Gtk_Drag_Result_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::drag-failed signal is emitted on the drag source when a drag has
   --  failed. The signal handler may hook custom code to handle a failed DnD
   --  operation based on the type of error, it returns True is the failure has
   --  been already handled (not showing the default "drag operation failed"
   --  animation), otherwise it returns False.
   -- 
   --  Callback parameters:
   --    --  "context": the drag context
   --    --  "result": the result of the drag operation
   --    --  Returns True if the failed drag operation has been already handled.

   type Cb_Gtk_Widget_Drag_Context_Guint_Void is not null access procedure
     (Self    : access Gtk_Widget_Record'Class;
      Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class;
      Time    : Guint);

   type Cb_GObject_Drag_Context_Guint_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class;
      Time    : Guint);

   Signal_Drag_Leave : constant Glib.Signal_Name := "drag-leave";
   procedure On_Drag_Leave
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Drag_Context_Guint_Void;
       After : Boolean := False);
   procedure On_Drag_Leave
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Drag_Context_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::drag-leave signal is emitted on the drop site when the cursor
   --  leaves the widget. A typical reason to connect to this signal is to undo
   --  things done in Gtk.Widget.Gtk_Widget::drag-motion, e.g. undo
   --  highlighting with Gtk.Widget.Drag_Unhighlight.
   --
   --  Likewise, the Gtk.Widget.Gtk_Widget::drag-leave signal is also emitted
   --  before the ::drag-drop signal, for instance to allow cleaning up of a
   --  preview item created in the Gtk.Widget.Gtk_Widget::drag-motion signal
   --  handler.
   -- 
   --  Callback parameters:
   --    --  "context": the drag context
   --    --  "time": the timestamp of the motion event

   Signal_Drag_Motion : constant Glib.Signal_Name := "drag-motion";
   procedure On_Drag_Motion
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Drag_Context_Gint_Gint_Guint_Boolean;
       After : Boolean := False);
   procedure On_Drag_Motion
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Drag_Context_Gint_Gint_Guint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::drag-motion signal is emitted on the drop site when the user
   --  moves the cursor over the widget during a drag. The signal handler must
   --  determine whether the cursor position is in a drop zone or not. If it is
   --  not in a drop zone, it returns False and no further processing is
   --  necessary. Otherwise, the handler returns True. In this case, the
   --  handler is responsible for providing the necessary information for
   --  displaying feedback to the user, by calling gdk_drag_status.
   --
   --  If the decision whether the drop will be accepted or rejected can't be
   --  made based solely on the cursor position and the type of the data, the
   --  handler may inspect the dragged data by calling Gtk.Widget.Drag_Get_Data
   --  and defer the gdk_drag_status call to the
   --  Gtk.Widget.Gtk_Widget::drag-data-received handler. Note that you must
   --  pass GTK_DEST_DEFAULT_DROP, GTK_DEST_DEFAULT_MOTION or
   --  GTK_DEST_DEFAULT_ALL to gtk_drag_dest_set when using the drag-motion
   --  signal that way.
   --
   --  Also note that there is no drag-enter signal. The drag receiver has to
   --  keep track of whether he has received any drag-motion signals since the
   --  last Gtk.Widget.Gtk_Widget::drag-leave and if not, treat the drag-motion
   --  signal as an "enter" signal. Upon an "enter", the handler will typically
   --  highlight the drop site with Gtk.Widget.Drag_Highlight. |[<!--
   --  language="C" --> static void drag_motion (GtkWidget *widget,
   --  GdkDragContext *context, gint x, gint y, guint time) { GdkAtom target;
   --
   --  PrivateData *private_data = GET_PRIVATE_DATA (widget);
   --
   --  if (!private_data->drag_highlight) { private_data->drag_highlight = 1;
   --  gtk_drag_highlight (widget); }
   --
   --  target = gtk_drag_dest_find_target (widget, context, NULL); if (target
   --  == GDK_NONE) gdk_drag_status (context, 0, time); else {
   --  private_data->pending_status = gdk_drag_context_get_suggested_action
   --  (context); gtk_drag_get_data (widget, context, target, time); }
   --
   --  return TRUE; }
   --
   --  static void drag_data_received (GtkWidget *widget, GdkDragContext
   --  *context, gint x, gint y, GtkSelectionData *selection_data, guint info,
   --  guint time) { PrivateData *private_data = GET_PRIVATE_DATA (widget);
   --
   --  if (private_data->suggested_action) { private_data->suggested_action =
   --  0;
   --
   --  // We are getting this data due to a request in drag_motion, // rather
   --  than due to a request in drag_drop, so we are just // supposed to call
   --  gdk_drag_status, not actually paste in // the data.
   --
   --  str = gtk_selection_data_get_text (selection_data); if
   --  (!data_is_acceptable (str)) gdk_drag_status (context, 0, time); else
   --  gdk_drag_status (context, private_data->suggested_action, time); } else
   --  { // accept the drop } } ]|
   -- 
   --  Callback parameters:
   --    --  "context": the drag context
   --    --  "x": the x coordinate of the current cursor position
   --    --  "y": the y coordinate of the current cursor position
   --    --  "time": the timestamp of the motion event
   --    --  Returns whether the cursor position is in a drop zone

   type Cb_Gtk_Widget_Cairo_Context_Boolean is not null access function
     (Self : access Gtk_Widget_Record'Class;
      Cr   : Cairo.Cairo_Context) return Boolean;

   type Cb_GObject_Cairo_Context_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class;
      Cr   : Cairo.Cairo_Context) return Boolean;

   Signal_Draw : constant Glib.Signal_Name := "draw";
   procedure On_Draw
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Cairo_Context_Boolean;
       After : Boolean := False);
   procedure On_Draw
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Cairo_Context_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when a widget is supposed to render itself. The
   --  Widget's top left corner must be painted at the origin of the passed in
   --  context and be sized to the values returned by
   --  Gtk.Widget.Get_Allocated_Width and Gtk.Widget.Get_Allocated_Height.
   --
   --  Signal handlers connected to this signal can modify the cairo context
   --  passed as Cr in any way they like and don't need to restore it. The
   --  signal emission takes care of calling cairo_save before and
   --  cairo_restore after invoking the handler.
   --
   --  The signal handler will get a Cr with a clip region already set to the
   --  widget's dirty region, i.e. to the area that needs repainting.
   --  Complicated widgets that want to avoid redrawing themselves completely
   --  can get the full extents of the clip region with
   --  gdk_cairo_get_clip_rectangle, or they can get a finer-grained
   --  representation of the dirty region with cairo_copy_clip_rectangle_list.
   -- 
   --  Callback parameters:
   --    --  "cr": the cairo context to draw to
   --    --  Returns True to stop other handlers from being invoked for the event.
   -- False to propagate the event further.

   type Cb_Gtk_Widget_Gdk_Event_Crossing_Boolean is not null access function
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Crossing) return Boolean;

   type Cb_GObject_Gdk_Event_Crossing_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Crossing) return Boolean;

   Signal_Enter_Notify_Event : constant Glib.Signal_Name := "enter-notify-event";
   procedure On_Enter_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Crossing_Boolean;
       After : Boolean := False);
   procedure On_Enter_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Crossing_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::enter-notify-event will be emitted when the pointer enters the
   --  Widget's window.
   --
   --  To receive this signal, the Gdk.Gdk_Window associated to the widget
   --  needs to enable the GDK_ENTER_NOTIFY_MASK mask.
   --
   --  This signal will be sent to the grab widget if there is one.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Crossing which triggered this signal.
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

   Signal_Event : constant Glib.Signal_Name := "event";
   procedure On_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Boolean;
       After : Boolean := False);
   procedure On_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The GTK+ main loop will emit three signals for each GDK event delivered
   --  to a widget: one generic ::event signal, another, more specific, signal
   --  that matches the type of event delivered (e.g.
   --  Gtk.Widget.Gtk_Widget::key-press-event) and finally a generic
   --  Gtk.Widget.Gtk_Widget::event-after signal.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event which triggered this signal
   --    --  Returns True to stop other handlers from being invoked for the event
   -- and to cancel the emission of the second specific ::event signal.
   --   False to propagate the event further and to allow the emission of
   --   the second signal. The ::event-after signal is emitted regardless of
   --   the return value.

   type Cb_Gtk_Widget_Gdk_Event_Void is not null access procedure
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event);

   type Cb_GObject_Gdk_Event_Void is not null access procedure
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event);

   Signal_Event_After : constant Glib.Signal_Name := "event-after";
   procedure On_Event_After
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Void;
       After : Boolean := False);
   procedure On_Event_After
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  After the emission of the Gtk.Widget.Gtk_Widget::event signal and
   --  (optionally) the second more specific signal, ::event-after will be
   --  emitted regardless of the previous two signals handlers return values.

   type Cb_Gtk_Widget_Gtk_Direction_Type_Boolean is not null access function
     (Self      : access Gtk_Widget_Record'Class;
      Direction : Gtk.Enums.Gtk_Direction_Type) return Boolean;

   type Cb_GObject_Gtk_Direction_Type_Boolean is not null access function
     (Self      : access Glib.Object.GObject_Record'Class;
      Direction : Gtk.Enums.Gtk_Direction_Type) return Boolean;

   Signal_Focus : constant Glib.Signal_Name := "focus";
   procedure On_Focus
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Direction_Type_Boolean;
       After : Boolean := False);
   procedure On_Focus
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Direction_Type_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   -- 
   --  Callback parameters:
   --    --  Returns True to stop other handlers from being invoked for the event. False to propagate the event further.

   type Cb_Gtk_Widget_Gdk_Event_Focus_Boolean is not null access function
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Focus) return Boolean;

   type Cb_GObject_Gdk_Event_Focus_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Focus) return Boolean;

   Signal_Focus_In_Event : constant Glib.Signal_Name := "focus-in-event";
   procedure On_Focus_In_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Focus_Boolean;
       After : Boolean := False);
   procedure On_Focus_In_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Focus_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::focus-in-event signal will be emitted when the keyboard focus
   --  enters the Widget's window.
   --
   --  To receive this signal, the Gdk.Gdk_Window associated to the widget
   --  needs to enable the GDK_FOCUS_CHANGE_MASK mask.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Focus which triggered this signal.
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

   Signal_Focus_Out_Event : constant Glib.Signal_Name := "focus-out-event";
   procedure On_Focus_Out_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Focus_Boolean;
       After : Boolean := False);
   procedure On_Focus_Out_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Focus_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::focus-out-event signal will be emitted when the keyboard focus
   --  leaves the Widget's window.
   --
   --  To receive this signal, the Gdk.Gdk_Window associated to the widget
   --  needs to enable the GDK_FOCUS_CHANGE_MASK mask.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Focus which triggered this signal.
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

   type Cb_Gtk_Widget_Gdk_Event_Grab_Broken_Boolean is not null access function
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Grab_Broken) return Boolean;

   type Cb_GObject_Gdk_Event_Grab_Broken_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Grab_Broken) return Boolean;

   Signal_Grab_Broken_Event : constant Glib.Signal_Name := "grab-broken-event";
   procedure On_Grab_Broken_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Grab_Broken_Boolean;
       After : Boolean := False);
   procedure On_Grab_Broken_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Grab_Broken_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a pointer or keyboard grab on a window belonging to Widget
   --  gets broken.
   --
   --  On X11, this happens when the grab window becomes unviewable (i.e. it
   --  or one of its ancestors is unmapped), or if the same application grabs
   --  the pointer or keyboard again.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Grab_Broken event
   --    --  Returns True to stop other handlers from being invoked for
   --   the event. False to propagate the event further.

   Signal_Grab_Focus : constant Glib.Signal_Name := "grab-focus";
   procedure On_Grab_Focus
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Grab_Focus
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   type Cb_Gtk_Widget_Boolean_Void is not null access procedure
     (Self        : access Gtk_Widget_Record'Class;
      Was_Grabbed : Boolean);

   type Cb_GObject_Boolean_Void is not null access procedure
     (Self        : access Glib.Object.GObject_Record'Class;
      Was_Grabbed : Boolean);

   Signal_Grab_Notify : constant Glib.Signal_Name := "grab-notify";
   procedure On_Grab_Notify
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Boolean_Void;
       After : Boolean := False);
   procedure On_Grab_Notify
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::grab-notify signal is emitted when a widget becomes shadowed by a
   --  GTK+ grab (not a pointer or keyboard grab) on another widget, or when it
   --  becomes unshadowed due to a grab being removed.
   --
   --  A widget is shadowed by a Gtk.Widget.Grab_Add when the topmost grab
   --  widget in the grab stack of its window group is not its ancestor.

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
   --  The ::hide signal is emitted when Widget is hidden, for example with
   --  Gtk.Widget.Hide.

   type Cb_Gtk_Widget_Gtk_Widget_Void is not null access procedure
     (Self              : access Gtk_Widget_Record'Class;
      Previous_Toplevel : access Gtk_Widget_Record'Class);

   type Cb_GObject_Gtk_Widget_Void is not null access procedure
     (Self              : access Glib.Object.GObject_Record'Class;
      Previous_Toplevel : access Gtk_Widget_Record'Class);

   Signal_Hierarchy_Changed : constant Glib.Signal_Name := "hierarchy-changed";
   procedure On_Hierarchy_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Hierarchy_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::hierarchy-changed signal is emitted when the anchored state of a
   --  widget changes. A widget is "anchored" when its toplevel ancestor is a
   --  Gtk.Window.Gtk_Window. This signal is emitted when a widget changes from
   --  un-anchored to anchored or vice-versa.

   type Cb_Gtk_Widget_Gdk_Event_Key_Boolean is not null access function
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Key) return Boolean;

   type Cb_GObject_Gdk_Event_Key_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Key) return Boolean;

   Signal_Key_Press_Event : constant Glib.Signal_Name := "key-press-event";
   procedure On_Key_Press_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Key_Boolean;
       After : Boolean := False);
   procedure On_Key_Press_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Key_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::key-press-event signal is emitted when a key is pressed. The
   --  signal emission will reoccur at the key-repeat rate when the key is kept
   --  pressed.
   --
   --  To receive this signal, the Gdk.Gdk_Window associated to the widget
   --  needs to enable the GDK_KEY_PRESS_MASK mask.
   --
   --  This signal will be sent to the grab widget if there is one.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Key which triggered this signal.
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

   Signal_Key_Release_Event : constant Glib.Signal_Name := "key-release-event";
   procedure On_Key_Release_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Key_Boolean;
       After : Boolean := False);
   procedure On_Key_Release_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Key_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::key-release-event signal is emitted when a key is released.
   --
   --  To receive this signal, the Gdk.Gdk_Window associated to the widget
   --  needs to enable the GDK_KEY_RELEASE_MASK mask.
   --
   --  This signal will be sent to the grab widget if there is one.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Key which triggered this signal.
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

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
   --  Gets emitted if keyboard navigation fails. See Gtk.Widget.Keynav_Failed
   --  for details.
   -- 
   --  Callback parameters:
   --    --  "direction": the direction of movement
   --    --  Returns True if stopping keyboard navigation is fine, False
   --          if the emitting widget should try to handle the keyboard
   --          navigation attempt in its parent container(s).

   Signal_Leave_Notify_Event : constant Glib.Signal_Name := "leave-notify-event";
   procedure On_Leave_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Crossing_Boolean;
       After : Boolean := False);
   procedure On_Leave_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Crossing_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::leave-notify-event will be emitted when the pointer leaves the
   --  Widget's window.
   --
   --  To receive this signal, the Gdk.Gdk_Window associated to the widget
   --  needs to enable the GDK_LEAVE_NOTIFY_MASK mask.
   --
   --  This signal will be sent to the grab widget if there is one.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Crossing which triggered this signal.
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

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
   --  The ::map signal is emitted when Widget is going to be mapped, that is
   --  when the widget is visible (which is controlled with
   --  Gtk.Widget.Set_Visible) and all its parents up to the toplevel widget
   --  are also visible. Once the map has occurred,
   --  Gtk.Widget.Gtk_Widget::map-event will be emitted.
   --
   --  The ::map signal can be used to determine whether a widget will be
   --  drawn, for instance it can resume an animation that was stopped during
   --  the emission of Gtk.Widget.Gtk_Widget::unmap.

   type Cb_Gtk_Widget_Gdk_Event_Any_Boolean is not null access function
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Any) return Boolean;

   type Cb_GObject_Gdk_Event_Any_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Any) return Boolean;

   Signal_Map_Event : constant Glib.Signal_Name := "map-event";
   procedure On_Map_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Any_Boolean;
       After : Boolean := False);
   procedure On_Map_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Any_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::map-event signal will be emitted when the Widget's window is
   --  mapped. A window is mapped when it becomes visible on the screen.
   --
   --  To receive this signal, the Gdk.Gdk_Window associated to the widget
   --  needs to enable the GDK_STRUCTURE_MASK mask. GDK will enable this mask
   --  automatically for all new windows.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Any which triggered this signal.
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

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
   --  The default handler for this signal activates Widget if Group_Cycling
   --  is False, or just makes Widget grab focus if Group_Cycling is True.
   -- 
   --  Callback parameters:
   --    --  "group_cycling": True if there are other widgets with the same mnemonic
   --    --  Returns True to stop other handlers from being invoked for the event.
   -- False to propagate the event further.

   type Cb_Gtk_Widget_Gdk_Event_Motion_Boolean is not null access function
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Motion) return Boolean;

   type Cb_GObject_Gdk_Event_Motion_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Motion) return Boolean;

   Signal_Motion_Notify_Event : constant Glib.Signal_Name := "motion-notify-event";
   procedure On_Motion_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Motion_Boolean;
       After : Boolean := False);
   procedure On_Motion_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Motion_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::motion-notify-event signal is emitted when the pointer moves over
   --  the widget's Gdk.Gdk_Window.
   --
   --  To receive this signal, the Gdk.Gdk_Window associated to the widget
   --  needs to enable the GDK_POINTER_MOTION_MASK mask.
   --
   --  This signal will be sent to the grab widget if there is one.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Motion which triggered this signal.
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

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

   Signal_Parent_Set : constant Glib.Signal_Name := "parent-set";
   procedure On_Parent_Set
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Parent_Set
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::parent-set signal is emitted when a new parent has been set on a
   --  widget.

   type Cb_Gtk_Widget_Boolean is not null access function
     (Self : access Gtk_Widget_Record'Class) return Boolean;

   type Cb_GObject_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class)
   return Boolean;

   Signal_Popup_Menu : constant Glib.Signal_Name := "popup-menu";
   procedure On_Popup_Menu
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Boolean;
       After : Boolean := False);
   procedure On_Popup_Menu
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal gets emitted whenever a widget should pop up a context
   --  menu. This usually happens through the standard key binding mechanism;
   --  by pressing a certain key while a widget is focused, the user can cause
   --  the widget to pop up a menu. For example, the Gtk.GEntry.Gtk_Entry
   --  widget creates a menu with clipboard commands. See the [Popup Menu
   --  Migration Checklist][checklist-popup-menu] for an example of how to use
   --  this signal.
   -- 
   --  Callback parameters:
   --    --  Returns True if a menu was activated

   type Cb_Gtk_Widget_Gdk_Event_Property_Boolean is not null access function
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Property) return Boolean;

   type Cb_GObject_Gdk_Event_Property_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Property) return Boolean;

   Signal_Property_Notify_Event : constant Glib.Signal_Name := "property-notify-event";
   procedure On_Property_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Property_Boolean;
       After : Boolean := False);
   procedure On_Property_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Property_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::property-notify-event signal will be emitted when a property on
   --  the Widget's window has been changed or deleted.
   --
   --  To receive this signal, the Gdk.Gdk_Window associated to the widget
   --  needs to enable the GDK_PROPERTY_CHANGE_MASK mask.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Property which triggered this signal.
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

   type Cb_Gtk_Widget_Gdk_Event_Proximity_Boolean is not null access function
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Proximity) return Boolean;

   type Cb_GObject_Gdk_Event_Proximity_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Proximity) return Boolean;

   Signal_Proximity_In_Event : constant Glib.Signal_Name := "proximity-in-event";
   procedure On_Proximity_In_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Proximity_Boolean;
       After : Boolean := False);
   procedure On_Proximity_In_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Proximity_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  To receive this signal the Gdk.Gdk_Window associated to the widget
   --  needs to enable the GDK_PROXIMITY_IN_MASK mask.
   --
   --  This signal will be sent to the grab widget if there is one.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Proximity which triggered this signal.
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

   Signal_Proximity_Out_Event : constant Glib.Signal_Name := "proximity-out-event";
   procedure On_Proximity_Out_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Proximity_Boolean;
       After : Boolean := False);
   procedure On_Proximity_Out_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Proximity_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  To receive this signal the Gdk.Gdk_Window associated to the widget
   --  needs to enable the GDK_PROXIMITY_OUT_MASK mask.
   --
   --  This signal will be sent to the grab widget if there is one.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Proximity which triggered this signal.
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

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
   --  Emitted when Gtk.Widget.Gtk_Widget:has-tooltip is True and the hover
   --  timeout has expired with the cursor hovering "above" Widget; or emitted
   --  when Widget got focus in keyboard mode.
   --
   --  Using the given coordinates, the signal handler should determine
   --  whether a tooltip should be shown for Widget. If this is the case True
   --  should be returned, False otherwise. Note that if Keyboard_Mode is True,
   --  the values of X and Y are undefined and should not be used.
   --
   --  The signal handler is free to manipulate Tooltip with the therefore
   --  destined function calls.
   -- 
   --  Callback parameters:
   --    --  "x": the x coordinate of the cursor position where the request has been
   --    --  emitted, relative to Widget's left side
   --    --  "y": the y coordinate of the cursor position where the request has been
   --    --  emitted, relative to Widget's top
   --    --  "keyboard_mode": True if the tooltip was triggered using the keyboard
   --    --  "tooltip": a Gtk.Tooltip.Gtk_Tooltip
   --    --  Returns True if Tooltip should be shown right now, False otherwise.

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
   --  The ::realize signal is emitted when Widget is associated with a
   --  Gdk.Gdk_Window, which means that Gtk.Widget.Realize has been called or
   --  the widget has been mapped (that is, it is going to be drawn).

   type Cb_Gtk_Widget_Gdk_Screen_Void is not null access procedure
     (Self            : access Gtk_Widget_Record'Class;
      Previous_Screen : access Gdk.Screen.Gdk_Screen_Record'Class);

   type Cb_GObject_Gdk_Screen_Void is not null access procedure
     (Self            : access Glib.Object.GObject_Record'Class;
      Previous_Screen : access Gdk.Screen.Gdk_Screen_Record'Class);

   Signal_Screen_Changed : constant Glib.Signal_Name := "screen-changed";
   procedure On_Screen_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Screen_Void;
       After : Boolean := False);
   procedure On_Screen_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Screen_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::screen-changed signal gets emitted when the screen of a widget
   --  has changed.

   type Cb_Gtk_Widget_Gdk_Event_Scroll_Boolean is not null access function
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Scroll) return Boolean;

   type Cb_GObject_Gdk_Event_Scroll_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Scroll) return Boolean;

   Signal_Scroll_Event : constant Glib.Signal_Name := "scroll-event";
   procedure On_Scroll_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Scroll_Boolean;
       After : Boolean := False);
   procedure On_Scroll_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Scroll_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::scroll-event signal is emitted when a button in the 4 to 7 range
   --  is pressed. Wheel mice are usually configured to generate button press
   --  events for buttons 4 and 5 when the wheel is turned.
   --
   --  To receive this signal, the Gdk.Gdk_Window associated to the widget
   --  needs to enable the GDK_SCROLL_MASK mask.
   --
   --  This signal will be sent to the grab widget if there is one.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Scroll which triggered this signal.
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

   type Cb_Gtk_Widget_Gdk_Event_Selection_Boolean is not null access function
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Selection) return Boolean;

   type Cb_GObject_Gdk_Event_Selection_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Selection) return Boolean;

   Signal_Selection_Clear_Event : constant Glib.Signal_Name := "selection-clear-event";
   procedure On_Selection_Clear_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Selection_Boolean;
       After : Boolean := False);
   procedure On_Selection_Clear_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Selection_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::selection-clear-event signal will be emitted when the the
   --  Widget's window has lost ownership of a selection.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Selection which triggered this signal.
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

   type Cb_Gtk_Widget_Gtk_Selection_Data_Guint_Guint_Void is not null access procedure
     (Self : access Gtk_Widget_Record'Class;
      Data : Gtk.Selection_Data.Gtk_Selection_Data;
      Info : Guint;
      Time : Guint);

   type Cb_GObject_Gtk_Selection_Data_Guint_Guint_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Data : Gtk.Selection_Data.Gtk_Selection_Data;
      Info : Guint;
      Time : Guint);

   Signal_Selection_Get : constant Glib.Signal_Name := "selection-get";
   procedure On_Selection_Get
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Selection_Data_Guint_Guint_Void;
       After : Boolean := False);
   procedure On_Selection_Get
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Selection_Data_Guint_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   -- 
   --  Callback parameters:

   Signal_Selection_Notify_Event : constant Glib.Signal_Name := "selection-notify-event";
   procedure On_Selection_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Selection_Boolean;
       After : Boolean := False);
   procedure On_Selection_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Selection_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   -- 
   --  Callback parameters:
   --    --  Returns True to stop other handlers from being invoked for the event. False to propagate the event further.

   type Cb_Gtk_Widget_Gtk_Selection_Data_Guint_Void is not null access procedure
     (Self : access Gtk_Widget_Record'Class;
      Data : Gtk.Selection_Data.Gtk_Selection_Data;
      Time : Guint);

   type Cb_GObject_Gtk_Selection_Data_Guint_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Data : Gtk.Selection_Data.Gtk_Selection_Data;
      Time : Guint);

   Signal_Selection_Received : constant Glib.Signal_Name := "selection-received";
   procedure On_Selection_Received
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Selection_Data_Guint_Void;
       After : Boolean := False);
   procedure On_Selection_Received
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Selection_Data_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   -- 
   --  Callback parameters:

   Signal_Selection_Request_Event : constant Glib.Signal_Name := "selection-request-event";
   procedure On_Selection_Request_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Selection_Boolean;
       After : Boolean := False);
   procedure On_Selection_Request_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Selection_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::selection-request-event signal will be emitted when another
   --  client requests ownership of the selection owned by the Widget's window.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Selection which triggered this signal.
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

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
   --  The ::show signal is emitted when Widget is shown, for example with
   --  Gtk.Widget.Show.

   type Cb_Gtk_Widget_Gtk_Widget_Help_Type_Boolean is not null access function
     (Self      : access Gtk_Widget_Record'Class;
      Help_Type : Gtk_Widget_Help_Type) return Boolean;

   type Cb_GObject_Gtk_Widget_Help_Type_Boolean is not null access function
     (Self      : access Glib.Object.GObject_Record'Class;
      Help_Type : Gtk_Widget_Help_Type) return Boolean;

   Signal_Show_Help : constant Glib.Signal_Name := "show-help";
   procedure On_Show_Help
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Widget_Help_Type_Boolean;
       After : Boolean := False);
   procedure On_Show_Help
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Widget_Help_Type_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   -- 
   --  Callback parameters:
   --    --  Returns True to stop other handlers from being invoked for the event.
   -- False to propagate the event further.

   type Cb_Gtk_Widget_Gtk_Allocation_Void is not null access procedure
     (Self       : access Gtk_Widget_Record'Class;
      Allocation : Gtk_Allocation);

   type Cb_GObject_Gtk_Allocation_Void is not null access procedure
     (Self       : access Glib.Object.GObject_Record'Class;
      Allocation : Gtk_Allocation);

   Signal_Size_Allocate : constant Glib.Signal_Name := "size-allocate";
   procedure On_Size_Allocate
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Allocation_Void;
       After : Boolean := False);
   procedure On_Size_Allocate
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Allocation_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   type Cb_Gtk_Widget_Gtk_State_Type_Void is not null access procedure
     (Self  : access Gtk_Widget_Record'Class;
      State : Gtk.Enums.Gtk_State_Type);

   type Cb_GObject_Gtk_State_Type_Void is not null access procedure
     (Self  : access Glib.Object.GObject_Record'Class;
      State : Gtk.Enums.Gtk_State_Type);

   Signal_State_Changed : constant Glib.Signal_Name := "state-changed";
   procedure On_State_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_State_Type_Void;
       After : Boolean := False);
   procedure On_State_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_State_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::state-changed signal is emitted when the widget state changes.
   --  See Gtk.Widget.Get_State.

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
   --  The ::state-flags-changed signal is emitted when the widget state
   --  changes, see Gtk.Widget.Get_State_Flags.

   type Cb_Gtk_Widget_Gtk_Style_Void is not null access procedure
     (Self           : access Gtk_Widget_Record'Class;
      Previous_Style : access Gtk.Style.Gtk_Style_Record'Class);

   type Cb_GObject_Gtk_Style_Void is not null access procedure
     (Self           : access Glib.Object.GObject_Record'Class;
      Previous_Style : access Gtk.Style.Gtk_Style_Record'Class);

   Signal_Style_Set : constant Glib.Signal_Name := "style-set";
   procedure On_Style_Set
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Style_Void;
       After : Boolean := False);
   procedure On_Style_Set
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Style_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::style-set signal is emitted when a new style has been set on a
   --  widget. Note that style-modifying functions like Gtk.Widget.Modify_Base
   --  also cause this signal to be emitted.
   --
   --  Note that this signal is emitted for changes to the deprecated
   --  Gtk.Style.Gtk_Style. To track changes to the
   --  Gtk.Style_Context.Gtk_Style_Context associated with a widget, use the
   --  Gtk.Widget.Gtk_Widget::style-updated signal.

   Signal_Style_Updated : constant Glib.Signal_Name := "style-updated";
   procedure On_Style_Updated
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Style_Updated
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::style-updated signal is a convenience signal that is emitted when
   --  the Gtk.Style_Context.Gtk_Style_Context::changed signal is emitted on
   --  the Widget's associated Gtk.Style_Context.Gtk_Style_Context as returned
   --  by gtk_widget_get_style_context.
   --
   --  Note that style-modifying functions like Gtk.Widget.Override_Color also
   --  cause this signal to be emitted.

   Signal_Touch_Event : constant Glib.Signal_Name := "touch-event";
   procedure On_Touch_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Boolean;
       After : Boolean := False);
   procedure On_Touch_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

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
   --  The ::unmap signal is emitted when Widget is going to be unmapped,
   --  which means that either it or any of its parents up to the toplevel
   --  widget have been set as hidden.
   --
   --  As ::unmap indicates that a widget will not be shown any longer, it can
   --  be used to, for example, stop an animation on the widget.

   Signal_Unmap_Event : constant Glib.Signal_Name := "unmap-event";
   procedure On_Unmap_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Any_Boolean;
       After : Boolean := False);
   procedure On_Unmap_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Any_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::unmap-event signal will be emitted when the Widget's window is
   --  unmapped. A window is unmapped when it becomes invisible on the screen.
   --
   --  To receive this signal, the Gdk.Gdk_Window associated to the widget
   --  needs to enable the GDK_STRUCTURE_MASK mask. GDK will enable this mask
   --  automatically for all new windows.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Any which triggered this signal
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

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
   --  The ::unrealize signal is emitted when the Gdk.Gdk_Window associated
   --  with Widget is destroyed, which means that Gtk.Widget.Unrealize has been
   --  called or the widget has been unmapped (that is, it is going to be
   --  hidden).

   type Cb_Gtk_Widget_Gdk_Event_Visibility_Boolean is not null access function
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Visibility) return Boolean;

   type Cb_GObject_Gdk_Event_Visibility_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Visibility) return Boolean;

   Signal_Visibility_Notify_Event : constant Glib.Signal_Name := "visibility-notify-event";
   procedure On_Visibility_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Visibility_Boolean;
       After : Boolean := False);
   procedure On_Visibility_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Visibility_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::visibility-notify-event will be emitted when the Widget's window
   --  is obscured or unobscured.
   --
   --  To receive this signal the Gdk.Gdk_Window associated to the widget
   --  needs to enable the GDK_VISIBILITY_NOTIFY_MASK mask.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Visibility which triggered this
   --    --  signal.
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

   type Cb_Gtk_Widget_Gdk_Event_Window_State_Boolean is not null access function
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Window_State) return Boolean;

   type Cb_GObject_Gdk_Event_Window_State_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Window_State) return Boolean;

   Signal_Window_State_Event : constant Glib.Signal_Name := "window-state-event";
   procedure On_Window_State_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Window_State_Boolean;
       After : Boolean := False);
   procedure On_Window_State_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Window_State_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::window-state-event will be emitted when the state of the toplevel
   --  window associated to the Widget changes.
   --
   --  To receive this signal the Gdk.Gdk_Window associated to the widget
   --  needs to enable the GDK_STRUCTURE_MASK mask. GDK will enable this mask
   --  automatically for all new windows.
   -- 
   --  Callback parameters:
   --    --  "event": the Gdk.Event.Gdk_Event_Window_State which triggered this
   --    --  signal.
   --    --  Returns True to stop other handlers from being invoked for the
   --   event. False to propagate the event further.

private
   Window_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("window");
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
   Style_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("style");
   Sensitive_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("sensitive");
   Scale_Factor_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("scale-factor");
   Receives_Default_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("receives-default");
   Parent_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("parent");
   Opacity_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("opacity");
   No_Show_All_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("no-show-all");
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
   Margin_Top_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("margin-top");
   Margin_Start_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("margin-start");
   Margin_Right_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("margin-right");
   Margin_Left_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("margin-left");
   Margin_End_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("margin-end");
   Margin_Bottom_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("margin-bottom");
   Margin_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("margin");
   Is_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-focus");
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
   Focus_On_Click_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("focus-on-click");
   Expand_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("expand");
   Events_Property : constant Gdk.Event.Property_Gdk_Event_Mask :=
     Gdk.Event.Build ("events");
   Double_Buffered_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("double-buffered");
   Composite_Child_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("composite-child");
   Can_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("can-focus");
   Can_Default_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("can-default");
   App_Paintable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("app-paintable");
end Gtk.Widget;
