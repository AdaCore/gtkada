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

--  Reads XML descriptions of a user interface and instantiates the described
--  objects.
--
--  To create a `GtkBuilder` from a user interface description, call
--  [ctorGtk.Builder.new_from_file], [ctorGtk.Builder.new_from_resource] or
--  [ctorGtk.Builder.new_from_string].
--
--  In the (unusual) case that you want to add user interface descriptions
--  from multiple sources to the same `GtkBuilder` you can call
--  [ctorGtk.Builder.new] to get an empty builder and populate it by (multiple)
--  calls to [methodGtk.Builder.add_from_file],
--  [methodGtk.Builder.add_from_resource] or
--  [methodGtk.Builder.add_from_string].
--
--  A `GtkBuilder` holds a reference to all objects that it has constructed
--  and drops these references when it is finalized. This finalization can
--  cause the destruction of non-widget objects or widgets which are not
--  contained in a toplevel window. For toplevel windows constructed by a
--  builder, it is the responsibility of the user to call
--  [methodGtk.Window.destroy] to get rid of them and all the widgets they
--  contain.
--
--  The functions [methodGtk.Builder.get_object] and
--  [methodGtk.Builder.get_objects] can be used to access the widgets in the
--  interface by the names assigned to them inside the UI description. Toplevel
--  windows returned by these functions will stay around until the user
--  explicitly destroys them with [methodGtk.Window.destroy]. Other widgets
--  will either be part of a larger hierarchy constructed by the builder (in
--  which case you should not have to worry about their lifecycle), or without
--  a parent, in which case they have to be added to some container to make use
--  of them. Non-widget objects need to be reffed with g_object_ref to keep
--  them beyond the lifespan of the builder.
--
--  ## GtkBuilder UI Definitions
--
--  `GtkBuilder` parses textual descriptions of user interfaces which are
--  specified in XML format. We refer to these descriptions as "GtkBuilder UI
--  definitions" or just "UI definitions" if the context is clear.
--
--  ### Structure of UI definitions
--
--  UI definition files are always encoded in UTF-8.
--
--  The toplevel element is `<interface>`. It optionally takes a "domain"
--  attribute, which will make the builder look for translated strings using
--  `dgettext` in the domain specified. This can also be done by calling
--  [methodGtk.Builder.set_translation_domain] on the builder. For example:
--
--  ```xml <?xml version="1.0" encoding="UTF-8"?> <interface
--  domain="your-app"> ... </interface> ```
--
--  ### Requirements
--
--  The target toolkit version(s) are described by `<requires>` elements, the
--  "lib" attribute specifies the widget library in question (currently the
--  only supported value is "gtk") and the "version" attribute specifies the
--  target version in the form "`<major>`.`<minor>`". `GtkBuilder` will error
--  out if the version requirements are not met. For example:
--
--  ```xml <?xml version="1.0" encoding="UTF-8"?> <interface
--  domain="your-app"> <requires lib="gtk" version="4.0" /> </interface> ```
--
--  ### Objects
--
--  Objects are defined as children of the `<interface>` element.
--
--  Objects are described by `<object>` elements, which can contain
--  `<property>` elements to set properties, `<signal>` elements which connect
--  signals to handlers, and `<child>` elements, which describe child objects.
--
--  Typically, the specific kind of object represented by an `<object>`
--  element is specified by the "class" attribute. If the type has not been
--  loaded yet, GTK tries to find the `get_type` function from the class name
--  by applying heuristics. This works in most cases, but if necessary, it is
--  possible to specify the name of the `get_type` function explicitly with the
--  "type-func" attribute. If your UI definition is referencing internal types,
--  you should make sure to call `g_type_ensure` for each object type before
--  parsing the UI definition.
--
--  Objects may be given a name with the "id" attribute, which allows the
--  application to retrieve them from the builder with
--  [methodGtk.Builder.get_object]. An id is also necessary to use the object
--  as property value in other parts of the UI definition. GTK reserves ids
--  starting and ending with `___` (three consecutive underscores) for its own
--  purposes.
--
--  ### Properties
--
--  Setting properties of objects is pretty straightforward with the
--  `<property>` element: the "name" attribute specifies the name of the
--  property, and the content of the element specifies the value:
--
--  ```xml <object class="GtkButton"> <property name="label">Hello,
--  world</property> </object> ```
--
--  If the "translatable" attribute is set to a true value, GTK uses `gettext`
--  (or `dgettext` if the builder has a translation domain set) to find a
--  translation for the value. This happens before the value is parsed, so it
--  can be used for properties of any type, but it is probably most useful for
--  string properties. It is also possible to specify a context to disambiguate
--  short strings, and comments which may help the translators:
--
--  ```xml <object class="GtkButton"> <property name="label"
--  translatable="yes" context="button" comments="A classic">Hello,
--  world</property> </object> ```
--
--  The xgettext tool that is part of gettext can extract these strings, but
--  note that it only looks for translatable="yes".
--
--  `GtkBuilder` can parse textual representations for the most common
--  property types:
--
--  - characters - strings - integers - floating-point numbers - booleans
--  (strings like "TRUE", "t", "yes", "y", "1" are interpreted as true values,
--  strings like "FALSE", "f", "no", "n", "0" are interpreted as false values)
--  - string lists (separated by newlines) - enumeration types (can be
--  specified by their full C identifier their short name used when registering
--  the enumeration type, or their integer value) - flag types (can be
--  specified by their C identifier or short name, optionally combined with "|"
--  for bitwise OR, or a single integer value e.g.,
--  "GTK_INPUT_HINT_EMOJI|GTK_INPUT_HINT_LOWERCASE", or "emoji|lowercase" or
--  520). - colors (in the format understood by [methodGdk.RGBA.parse]) -
--  transforms (in the format understood by [funcGsk.Transform.parse]) - Pango
--  attribute lists (in the format understood by
--  [methodPango.AttrList.to_string]) - Pango tab arrays (in the format
--  understood by [methodPango.TabArray.to_string]) - Pango font descriptions
--  (in the format understood by [funcPango.FontDescription.from_string]) -
--  `GVariant` (in the format understood by [funcGlib.Variant.parse]) -
--  textures (can be specified as an object id, a resource path or a filename
--  of an image file to load relative to the Builder file or the CWD if
--  [methodGtk.Builder.add_from_string] was used) - GFile (like textures, can
--  be specified as an object id, a URI or a filename of a file to load
--  relative to the Builder file or the CWD if
--  [methodGtk.Builder.add_from_string] was used)
--
--  Objects can be referred to by their name and by default refer to objects
--  declared in the local XML fragment and objects exposed via
--  [methodGtk.Builder.expose_object]. In general, `GtkBuilder` allows forward
--  references to objects declared in the local XML; an object doesn't have to
--  be constructed before it can be referred to. The exception to this rule is
--  that an object has to be constructed before it can be used as the value of
--  a construct-only property.
--
--  ### Child objects
--
--  Many widgets have properties for child widgets, such as
--  [propertyGtk.Expander:child]. In this case, the preferred way to specify
--  the child widget in a ui file is to simply set the property:
--
--  ```xml <object class="GtkExpander"> <property name="child"> <object
--  class="GtkLabel"> ... </object> </property> </object> ```
--
--  Generic containers that can contain an arbitrary number of children, such
--  as [classGtk.Box] instead use the `<child>` element. A `<child>` element
--  contains an `<object>` element which describes the child object. Most
--  often, child objects are widgets inside a container, but they can also be,
--  e.g., actions in an action group, or columns in a tree model.
--
--  Any object type that implements the [ifaceGtk.Buildable] interface can
--  specify how children may be added to it. Since many objects and widgets
--  that are included with GTK already implement the `GtkBuildable` interface,
--  typically child objects can be added using the `<child>` element without
--  having to be concerned about the underlying implementation.
--
--  See the [`GtkWidget`
--  documentation](class.Widget.htmlgtkwidget-as-gtkbuildable) for many
--  examples of using `GtkBuilder` with widgets, including setting child
--  objects using the `<child>` element.
--
--  A noteworthy special case to the general rule that only objects
--  implementing `GtkBuildable` may specify how to handle the `<child>` element
--  is that `GtkBuilder` provides special support for adding objects to a
--  [classGio.ListStore] by using the `<child>` element. For instance:
--
--  ```xml <object class="GListStore"> <property
--  name="item-type">MyObject</property> <child> <object class="MyObject" />
--  </child> ... </object> ```
--
--  ### Property bindings
--
--  It is also possible to bind a property value to another object's property
--  value using the attributes "bind-source" to specify the source object of
--  the binding, and optionally, "bind-property" and "bind-flags" to specify
--  the source property and source binding flags respectively. Internally,
--  `GtkBuilder` implements this using [classGobject.Binding] objects.
--
--  For instance, in the example below the "label" property of the
--  `bottom_label` widget is bound to the "label" property of the `top_button`
--  widget:
--
--  ```xml <object class="GtkBox"> <property
--  name="orientation">vertical</property> <child> <object class="GtkButton"
--  id="top_button"> <property name="label">Hello, world</property> </object>
--  </child> <child> <object class="GtkLabel" id="bottom_label"> <property
--  name="label" bind-source="top_button" bind-property="label"
--  bind-flags="sync-create" /> </object> </child> </object> ```
--
--  For more information, see the documentation of the
--  [methodGobject.Object.bind_property] method.
--
--  Please note that another way to set up bindings between objects in .ui
--  files is to use the `GtkExpression` methodology. See the [`GtkExpression`
--  documentation](class.Expression.htmlgtkexpression-in-ui-files) for more
--  information.
--
--  ### Internal children
--
--  Sometimes it is necessary to refer to widgets which have implicitly been
--  constructed by GTK as part of a composite widget, to set properties on them
--  or to add further children (e.g. the content area of a `GtkDialog`). This
--  can be achieved by setting the "internal-child" property of the `<child>`
--  element to a true value. Note that `GtkBuilder` still requires an
--  `<object>` element for the internal child, even if it has already been
--  constructed.
--
--  ### Specialized children
--
--  A number of widgets have different places where a child can be added (e.g.
--  tabs vs. page content in notebooks). This can be reflected in a UI
--  definition by specifying the "type" attribute on a `<child>` The possible
--  values for the "type" attribute are described in the sections describing
--  the widget-specific portions of UI definitions.
--
--  ### Signal handlers and function pointers
--
--  Signal handlers are set up with the `<signal>` element. The "name"
--  attribute specifies the name of the signal, and the "handler" attribute
--  specifies the function to connect to the signal.
--
--  ```xml <object class="GtkButton" id="hello_button"> <signal name="clicked"
--  handler="hello_button__clicked" /> </object> ```
--
--  The remaining attributes, "after", "swapped" and "object", have the same
--  meaning as the corresponding parameters of the
--  [funcGobject.signal_connect_object] or [funcGobject.signal_connect_data]
--  functions:
--
--  - "after" matches the `G_CONNECT_AFTER` flag, and will ensure that the
--  handler is called after the default class closure for the signal -
--  "swapped" matches the `G_CONNECT_SWAPPED` flag, and will swap the instance
--  and closure arguments when invoking the signal handler - "object" will bind
--  the signal handler to the lifetime of the object referenced by the
--  attribute
--
--  By default "swapped" will be set to "yes" if not specified otherwise, in
--  the case where "object" is set, for convenience. A "last_modification_time"
--  attribute is also allowed, but it does not have a meaning to the builder.
--
--  When compiling applications for Windows, you must declare signal callbacks
--  with the `G_MODULE_EXPORT` decorator, or they will not be put in the symbol
--  table:
--
--  ```c G_MODULE_EXPORT void hello_button__clicked (GtkButton *button,
--  gpointer data) { // ... } ```
--
--  On Linux and Unix, this is not necessary; applications should instead be
--  compiled with the `-Wl,--export-dynamic` argument inside their compiler
--  flags, and linked against `gmodule-export-2.0`.
--
--  ## Example UI Definition
--
--  ```xml <interface> <object class="GtkDialog" id="dialog1"> <child
--  internal-child="content_area"> <object class="GtkBox"> <child
--  internal-child="action_area"> <object class="GtkBox"> <child> <object
--  class="GtkButton" id="ok_button"> <property name="label"
--  translatable="yes">_Ok</property> <property
--  name="use-underline">True</property> <signal name="clicked"
--  handler="ok_button_clicked"/> </object> </child> </object> </child>
--  </object> </child> </object> </interface> ```
--
--  ## Using GtkBuildable for extending UI definitions
--
--  Objects can implement the [ifaceGtk.Buildable] interface to add custom
--  elements and attributes to the XML. Typically, any extension will be
--  documented in each type that implements the interface.
--
--  ## Menus
--
--  In addition to objects with properties that are created with `<object>`
--  and `<property>` elements, `GtkBuilder` also allows to parse XML menu
--  definitions as used by [classGio.Menu] when exporting menu models over
--  D-Bus, and as described in the [classGtk.PopoverMenu] documentation. Menus
--  can be defined as toplevel elements, or as property values for properties
--  of type `GMenuModel`.
--
--  ## Templates
--
--  When describing a [classGtk.Widget], you can use the `<template>` tag to
--  describe a UI bound to a specific widget type. GTK will automatically load
--  the UI definition when instantiating the type, and bind children and signal
--  handlers to instance fields and function symbols.
--
--  For more information, see the [`GtkWidget`
--  documentation](class.Widget.htmlbuilding-composite-widgets-from-template-xml)
--  for details.

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;            use GNAT.Strings;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Values;             use Glib.Values;
with Gtk.Builder_Scope;       use Gtk.Builder_Scope;
with Gtk.Enums;               use Gtk.Enums;

package Gtk.Builder is

   type Gtk_Builder_Record is new GObject_Record with null record;
   type Gtk_Builder is access all Gtk_Builder_Record'Class;

   type Gtk_Builder_Error is (
      Builder_Error_Invalid_Type_Function,
      Builder_Error_Unhandled_Tag,
      Builder_Error_Missing_Attribute,
      Builder_Error_Invalid_Attribute,
      Builder_Error_Invalid_Tag,
      Builder_Error_Missing_Property_Value,
      Builder_Error_Invalid_Value,
      Builder_Error_Version_Mismatch,
      Builder_Error_Duplicate_Id,
      Builder_Error_Object_Type_Refused,
      Builder_Error_Template_Mismatch,
      Builder_Error_Invalid_Property,
      Builder_Error_Invalid_Signal,
      Builder_Error_Invalid_Id,
      Builder_Error_Invalid_Function);
   pragma Convention (C, Gtk_Builder_Error);
   --  Error codes that identify various errors that can occur while using
   --  `GtkBuilder`.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Builder_Error_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Builder_Error);
   type Property_Gtk_Builder_Error is new Gtk_Builder_Error_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Builder);
   procedure Initialize (Self : not null access Gtk_Builder_Record'Class);
   --  Creates a new empty builder object.
   --  This function is only useful if you intend to make multiple calls to
   --  [methodGtk.Builder.add_from_file], [methodGtk.Builder.add_from_resource]
   --  or [methodGtk.Builder.add_from_string] in order to merge multiple UI
   --  descriptions into a single builder.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Builder_New return Gtk_Builder;
   --  Creates a new empty builder object.
   --  This function is only useful if you intend to make multiple calls to
   --  [methodGtk.Builder.add_from_file], [methodGtk.Builder.add_from_resource]
   --  or [methodGtk.Builder.add_from_string] in order to merge multiple UI
   --  descriptions into a single builder.

   procedure Gtk_New_From_File
      (Self     : out Gtk_Builder;
       Filename : UTF8_String);
   procedure Initialize_From_File
      (Self     : not null access Gtk_Builder_Record'Class;
       Filename : UTF8_String);
   --  Parses the UI definition in the file Filename.
   --  If there is an error opening the file or parsing the description then
   --  the program will be aborted. You should only ever attempt to parse user
   --  interface descriptions that are shipped as part of your program.
   --  Initialize_From_File does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Filename filename of user interface description file

   function Gtk_Builder_New_From_File
      (Filename : UTF8_String) return Gtk_Builder;
   --  Parses the UI definition in the file Filename.
   --  If there is an error opening the file or parsing the description then
   --  the program will be aborted. You should only ever attempt to parse user
   --  interface descriptions that are shipped as part of your program.
   --  @param Filename filename of user interface description file

   procedure Gtk_New_From_Resource
      (Self          : out Gtk_Builder;
       Resource_Path : UTF8_String);
   procedure Initialize_From_Resource
      (Self          : not null access Gtk_Builder_Record'Class;
       Resource_Path : UTF8_String);
   --  Parses the UI definition at Resource_Path.
   --  If there is an error locating the resource or parsing the description,
   --  then the program will be aborted.
   --  Initialize_From_Resource does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Resource_Path a `GResource` resource path

   function Gtk_Builder_New_From_Resource
      (Resource_Path : UTF8_String) return Gtk_Builder;
   --  Parses the UI definition at Resource_Path.
   --  If there is an error locating the resource or parsing the description,
   --  then the program will be aborted.
   --  @param Resource_Path a `GResource` resource path

   procedure Gtk_New_From_String
      (Self   : out Gtk_Builder;
       String : UTF8_String;
       Length : Gssize);
   procedure Initialize_From_String
      (Self   : not null access Gtk_Builder_Record'Class;
       String : UTF8_String;
       Length : Gssize);
   --  Parses the UI definition in String.
   --  If String is null-terminated, then Length should be -1. If Length is
   --  not -1, then it is the length of String.
   --  If there is an error parsing String then the program will be aborted.
   --  You should not attempt to parse user interface description from
   --  untrusted sources.
   --  Initialize_From_String does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param String a user interface (XML) description
   --  @param Length the length of String, or -1

   function Gtk_Builder_New_From_String
      (String : UTF8_String;
       Length : Gssize) return Gtk_Builder;
   --  Parses the UI definition in String.
   --  If String is null-terminated, then Length should be -1. If Length is
   --  not -1, then it is the length of String.
   --  If there is an error parsing String then the program will be aborted.
   --  You should not attempt to parse user interface description from
   --  untrusted sources.
   --  @param String a user interface (XML) description
   --  @param Length the length of String, or -1

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_builder_get_type");

   -------------
   -- Methods --
   -------------

   function Add_From_File
      (Self     : not null access Gtk_Builder_Record;
       Filename : UTF8_String) return Boolean;
   --  Parses a file containing a UI definition and merges it with the current
   --  contents of Builder.
   --  This function is useful if you need to call
   --  [methodGtk.Builder.set_current_object]) to add user data to callbacks
   --  before loading GtkBuilder UI. Otherwise, you probably want
   --  [ctorGtk.Builder.new_from_file] instead.
   --  If an error occurs, 0 will be returned and Error will be assigned a
   --  `GError` from the `GTK_BUILDER_ERROR`, `G_MARKUP_ERROR` or
   --  `G_FILE_ERROR` domains.
   --  It's not really reasonable to attempt to handle failures of this call.
   --  You should not use this function with untrusted files (ie: files that
   --  are not part of your application). Broken `GtkBuilder` files can easily
   --  crash your program, and it's possible that memory was leaked leading up
   --  to the reported failure. The only reasonable thing to do when an error
   --  is detected is to call `g_error`.
   --  @param Filename the name of the file to parse
   --  @return True on success, False if an error occurred

   function Add_From_Resource
      (Self          : not null access Gtk_Builder_Record;
       Resource_Path : UTF8_String) return Boolean;
   --  Parses a resource file containing a UI definition and merges it with
   --  the current contents of Builder.
   --  This function is useful if you need to call
   --  [methodGtk.Builder.set_current_object] to add user data to callbacks
   --  before loading GtkBuilder UI. Otherwise, you probably want
   --  [ctorGtk.Builder.new_from_resource] instead.
   --  If an error occurs, 0 will be returned and Error will be assigned a
   --  `GError` from the GTK_BUILDER_ERROR, G_MARKUP_ERROR or G_RESOURCE_ERROR
   --  domain.
   --  It's not really reasonable to attempt to handle failures of this call.
   --  The only reasonable thing to do when an error is detected is to call
   --  g_error.
   --  @param Resource_Path the path of the resource file to parse
   --  @return True on success, False if an error occurred

   function Add_From_String
      (Self   : not null access Gtk_Builder_Record;
       Buffer : UTF8_String) return Boolean;
   --  Parses a string containing a UI definition and merges it with the
   --  current contents of Builder.
   --  This function is useful if you need to call
   --  [methodGtk.Builder.set_current_object] to add user data to callbacks
   --  before loading `GtkBuilder` UI. Otherwise, you probably want
   --  [ctorGtk.Builder.new_from_string] instead.
   --  Upon errors False will be returned and Error will be assigned a
   --  `GError` from the GTK_BUILDER_ERROR, G_MARKUP_ERROR or
   --  G_VARIANT_PARSE_ERROR domain.
   --  It's not really reasonable to attempt to handle failures of this call.
   --  The only reasonable thing to do when an error is detected is to call
   --  g_error.
   --  @param Buffer the string to parse
   --  @return True on success, False if an error occurred

   function Add_Objects_From_File
      (Self       : not null access Gtk_Builder_Record;
       Filename   : UTF8_String;
       Object_Ids : GNAT.Strings.String_List) return Boolean;
   --  Parses a file containing a UI definition building only the requested
   --  objects and merges them with the current contents of Builder.
   --  Upon errors, 0 will be returned and Error will be assigned a `GError`
   --  from the GTK_BUILDER_ERROR, G_MARKUP_ERROR or G_FILE_ERROR domain.
   --  If you are adding an object that depends on an object that is not its
   --  child (for instance a `GtkTreeView` that depends on its `GtkTreeModel`),
   --  you have to explicitly list all of them in Object_Ids.
   --  @param Filename the name of the file to parse
   --  @param Object_Ids nul-terminated array of objects to build
   --  @return True on success, False if an error occurred

   function Add_Objects_From_Resource
      (Self          : not null access Gtk_Builder_Record;
       Resource_Path : UTF8_String;
       Object_Ids    : GNAT.Strings.String_List) return Boolean;
   --  Parses a resource file containing a UI definition, building only the
   --  requested objects and merges them with the current contents of Builder.
   --  Upon errors, 0 will be returned and Error will be assigned a `GError`
   --  from the GTK_BUILDER_ERROR, G_MARKUP_ERROR or G_RESOURCE_ERROR domain.
   --  If you are adding an object that depends on an object that is not its
   --  child (for instance a `GtkTreeView` that depends on its `GtkTreeModel`),
   --  you have to explicitly list all of them in Object_Ids.
   --  @param Resource_Path the path of the resource file to parse
   --  @param Object_Ids nul-terminated array of objects to build
   --  @return True on success, False if an error occurred

   function Add_Objects_From_String
      (Self       : not null access Gtk_Builder_Record;
       Buffer     : UTF8_String;
       Object_Ids : GNAT.Strings.String_List) return Boolean;
   --  Parses a string containing a UI definition, building only the requested
   --  objects and merges them with the current contents of Builder.
   --  Upon errors False will be returned and Error will be assigned a
   --  `GError` from the GTK_BUILDER_ERROR or G_MARKUP_ERROR domain.
   --  If you are adding an object that depends on an object that is not its
   --  child (for instance a `GtkTreeView` that depends on its `GtkTreeModel`),
   --  you have to explicitly list all of them in Object_Ids.
   --  @param Buffer the string to parse
   --  @param Object_Ids nul-terminated array of objects to build
   --  @return True on success, False if an error occurred

   function Create_Closure
      (Self          : not null access Gtk_Builder_Record;
       Function_Name : UTF8_String;
       Flags         : Gtk.Enums.Gtk_Builder_Closure_Flags;
       Object        : access Glib.Object.GObject_Record'Class)
       return System.Address;
   --  Creates a closure to invoke the function called Function_Name.
   --  This is using the create_closure implementation of Builder's
   --  [ifaceGtk.BuilderScope].
   --  If no closure could be created, null will be returned and Error will be
   --  set.
   --  @param Function_Name name of the function to look up
   --  @param Flags closure creation flags
   --  @param Object Object to create the closure with
   --  @return A new closure for invoking Function_Name

   procedure Expose_Object
      (Self   : not null access Gtk_Builder_Record;
       Name   : UTF8_String;
       Object : not null access Glib.Object.GObject_Record'Class);
   --  Add Object to the Builder object pool so it can be referenced just like
   --  any other object built by builder.
   --  Only a single object may be added using Name. However, it is not an
   --  error to expose the same object under multiple names.
   --  `gtk_builder_get_object` may be used to determine if an object has
   --  already been added with Name.
   --  @param Name the name of the object exposed to the builder
   --  @param Object the object to expose

   function Extend_With_Template
      (Self          : not null access Gtk_Builder_Record;
       Object        : not null access Glib.Object.GObject_Record'Class;
       Template_Type : GType;
       Buffer        : UTF8_String;
       Length        : Gssize) return Boolean;
   --  Main private entry point for building composite components from
   --  template XML.
   --  Most likely you do not need to call this function in applications as
   --  templates are handled by `GtkWidget`.
   --  @param Object the object that is being extended
   --  @param Template_Type the type that the template is for
   --  @param Buffer the string to parse
   --  @param Length the length of Buffer (may be -1 if Buffer is
   --  nul-terminated)
   --  @return A positive value on success, 0 if an error occurred

   function Get_Current_Object
      (Self : not null access Gtk_Builder_Record) return Glib.Object.GObject;
   --  Gets the current object set via Gtk.Builder.Set_Current_Object.
   --  @return the current object

   procedure Set_Current_Object
      (Self           : not null access Gtk_Builder_Record;
       Current_Object : access Glib.Object.GObject_Record'Class);
   --  Sets the current object for the Builder.
   --  The current object can be thought of as the `this` object that the
   --  builder is working for and will often be used as the default object when
   --  an object is optional.
   --  [methodGtk.Widget.init_template] for example will set the current
   --  object to the widget the template is inited for. For functions like
   --  [ctorGtk.Builder.new_from_resource], the current object will be null.
   --  @param Current_Object the new current object

   function Get_Object
      (Self : not null access Gtk_Builder_Record;
       Name : UTF8_String) return Glib.Object.GObject;
   --  Gets the object named Name.
   --  Note that this function does not increment the reference count of the
   --  returned object.
   --  @param Name name of object to get
   --  @return the object named Name

   function Get_Objects
      (Self : not null access Gtk_Builder_Record)
       return Glib.Object.Object_Simple_List.Glist;
   --  Gets all objects that have been constructed by Builder.
   --  Note that this function does not increment the reference counts of the
   --  returned objects.

   function Get_Scope
      (Self : not null access Gtk_Builder_Record)
       return Gtk.Builder_Scope.Gtk_Builder_Scope;
   --  Gets the scope in use that was set via Gtk.Builder.Set_Scope.
   --  @return the current scope

   procedure Set_Scope
      (Self  : not null access Gtk_Builder_Record;
       Scope : Gtk.Builder_Scope.Gtk_Builder_Scope);
   --  Sets the scope the builder should operate in.
   --  If Scope is null, a new [classGtk.BuilderCScope] will be created.
   --  @param Scope the scope to use

   function Get_Translation_Domain
      (Self : not null access Gtk_Builder_Record) return UTF8_String;
   --  Gets the translation domain of Builder.
   --  @return the translation domain

   procedure Set_Translation_Domain
      (Self   : not null access Gtk_Builder_Record;
       Domain : UTF8_String := "");
   --  Sets the translation domain of Builder.
   --  @param Domain the translation domain

   function Get_Type_From_Name
      (Self      : not null access Gtk_Builder_Record;
       Type_Name : UTF8_String) return GType;
   --  Looks up a type by name.
   --  This is using the virtual function that `GtkBuilder` has for that
   --  purpose. This is mainly used when implementing the `GtkBuildable`
   --  interface on a type.
   --  @param Type_Name type name to lookup
   --  @return the `GType` found for Type_Name or G_TYPE_INVALID if no type
   --  was found

   function Value_From_String
      (Self   : not null access Gtk_Builder_Record;
       Pspec  : in out Glib.Param_Spec;
       String : UTF8_String;
       Value  : access Glib.Values.GValue) return Boolean;
   --  Demarshals a value from a string.
   --  This function calls g_value_init on the Value argument, so it need not
   --  be initialised beforehand.
   --  Can handle char, uchar, boolean, int, uint, long, ulong, enum, flags,
   --  float, double, string, `GdkRGBA` and `GtkAdjustment` type values.
   --  Upon errors False will be returned and Error will be assigned a
   --  `GError` from the GTK_BUILDER_ERROR domain.
   --  @param Pspec the `GParamSpec` for the property
   --  @param String the string representation of the value
   --  @param Value the `GValue` to store the result in
   --  @return True on success

   function Value_From_String_Type
      (Self     : not null access Gtk_Builder_Record;
       The_Type : GType;
       String   : UTF8_String;
       Value    : access Glib.Values.GValue) return Boolean;
   --  Demarshals a value from a string.
   --  Unlike [methodGtk.Builder.value_from_string], this function takes a
   --  `GType` instead of `GParamSpec`.
   --  Calls g_value_init on the Value argument, so it need not be initialised
   --  beforehand.
   --  Upon errors False will be returned and Error will be assigned a
   --  `GError` from the GTK_BUILDER_ERROR domain.
   --  @param The_Type the `GType` of the value
   --  @param String the string representation of the value
   --  @param Value the `GValue` to store the result in
   --  @return True on success

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Current_Object_Property : constant Glib.Properties.Property_Object;
   --  Type: Glib.Object.GObject
   --  The object the builder is evaluating for.

   Scope_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Builder_Scope.Gtk_Builder_Scope
   --  The scope the builder is operating in

   Translation_Domain_Property : constant Glib.Properties.Property_String;
   --  The translation domain used when translating property values that have
   --  been marked as translatable.
   --
   --  If the translation domain is null, `GtkBuilder` uses gettext, otherwise
   --  g_dgettext.

private
   Translation_Domain_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("translation-domain");
   Scope_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("scope");
   Current_Object_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("current-object");
end Gtk.Builder;
