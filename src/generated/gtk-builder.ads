------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2013, AdaCore                     --
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
--  A GtkBuilder is an auxiliary object that reads textual descriptions of a
--  user interface and instantiates the described objects. To pass a
--  description to a GtkBuilder, call Gtk.Builder.Add_From_File or
--  Gtk.Builder.Add_From_String. These functions can be called multiple times;
--  the builder merges the content of all descriptions.
--
--  A GtkBuilder holds a reference to all objects that it has constructed and
--  drops these references when it is finalized. This finalization can cause
--  the destruction of non-widget objects or widgets which are not contained in
--  a toplevel window. For toplevel windows constructed by a builder, it is the
--  responsibility of the user to call Gtk.Widget.Destroy to get rid of them
--  and all the widgets they contain.
--
--  The functions Gtk.Builder.Get_Object and Gtk.Builder.Get_Objects can be
--  used to access the widgets in the interface by the names assigned to them
--  inside the UI description. Toplevel windows returned by these functions
--  will stay around until the user explicitly destroys them with
--  Gtk.Widget.Destroy. Other widgets will either be part of a larger hierarchy
--  constructed by the builder (in which case you should not have to worry
--  about their lifecycle), or without a parent, in which case they have to be
--  added to some container to make use of them. Non-widget objects need to be
--  reffed with g_object_ref to keep them beyond the lifespan of the builder.
--
--  The function Gtk.Builder.Connect_Signals and variants thereof can be used
--  to connect handlers to the named signals in the description.
--
--  == GtkBuilder UI Definitions ==
--
--  GtkBuilder parses textual descriptions of user interfaces which are
--  specified in an XML format which can be roughly described by the RELAX NG
--  schema below. We refer to these descriptions as 'GtkBuilder UI definitions'
--  or just 'UI definitions' if the context is clear. Do not confuse GtkBuilder
--  UI Definitions with <link linkend="XML-UI">GtkUIManager UI
--  Definitions</link>, which are more limited in scope. It is common to use
--  '.ui' as the filename extension for files containing GtkBuilder UI
--  definitions.
--
--    <xi:include xmlns:xi="http://www.w3.org/2001/XInclude" parse="text" href="../../../../gtk/gtkbuilder.rnc">
--    <xi:fallback>FIXME: MISSING XINCLUDE CONTENT</xi:fallback>
--    </xi:include>
--  The toplevel element is <interface>. It optionally takes a "domain"
--  attribute, which will make the builder look for translated strings using
--  dgettext in the domain specified. This can also be done by calling
--  Gtk.Builder.Set_Translation_Domain on the builder. Objects are described by
--  <object> elements, which can contain <property> elements to set properties,
--  <signal> elements which connect signals to handlers, and <child> elements,
--  which describe child objects (most often widgets inside a container, but
--  also e.g. actions in an action group, or columns in a tree model). A
--  <child> element contains an <object> element which describes the child
--  object. The target toolkit version(s) are described by <requires> elements,
--  the "lib" attribute specifies the widget library in question (currently the
--  only supported value is "gtk+") and the "version" attribute specifies the
--  target version in the form "<major>.<minor>". The builder will error out if
--  the version requirements are not met.
--
--  Typically, the specific kind of object represented by an <object> element
--  is specified by the "class" attribute. If the type has not been loaded yet,
--  GTK+ tries to find the <function>_get_type</function> from the class name
--  by applying heuristics. This works in most cases, but if necessary, it is
--  possible to specify the name of the <function>_get_type</function>
--  explictly with the "type-func" attribute. As a special case, GtkBuilder
--  allows to use an object that has been constructed by a
--  Gtk.UI_Manager.Gtk_UI_Manager in another part of the UI definition by
--  specifying the id of the Gtk.UI_Manager.Gtk_UI_Manager in the "constructor"
--  attribute and the name of the object in the "id" attribute.
--
--  Objects must be given a name with the "id" attribute, which allows the
--  application to retrieve them from the builder with Gtk.Builder.Get_Object.
--  An id is also necessary to use the object as property value in other parts
--  of the UI definition.
--
--  Note:
--
--  Prior to 2.20, GtkBuilder was setting the "name" property of constructed
--  widgets to the "id" attribute. In GTK+ 2.20 or newer, you have to use
--  Gtk.Buildable.Get_Name instead of Gtk.Widget.Get_Name to obtain the "id",
--  or set the "name" property in your UI definition.
--
--  Setting properties of objects is pretty straightforward with the
--  <property> element: the "name" attribute specifies the name of the
--  property, and the content of the element specifies the value. If the
--  "translatable" attribute is set to a true value, GTK+ uses gettext (or
--  dgettext if the builder has a translation domain set) to find a translation
--  for the value. This happens before the value is parsed, so it can be used
--  for properties of any type, but it is probably most useful for string
--  properties. It is also possible to specify a context to disambiguate short
--  strings, and comments which may help the translators.
--
--  GtkBuilder can parse textual representations for the most common property
--  types: characters, strings, integers, floating-point numbers, booleans
--  (strings like "TRUE", "t", "yes", "y", "1" are interpreted as True, strings
--  like "FALSE, "f", "no", "n", "0" are interpreted as False), enumerations
--  (can be specified by their name, nick or integer value), flags (can be
--  specified by their name, nick, integer value, optionally combined with "|",
--  e.g. "GTK_VISIBLE|GTK_REALIZED") and colors (in a format understood by
--  gdk_color_parse). Pixbufs can be specified as a filename of an image file
--  to load. Objects can be referred to by their name and by default refer to
--  objects declared in the local xml fragment and objects exposed via
--  Gtk.Builder.Expose_Object.
--
--  In general, GtkBuilder allows forward references to objects &mdash
--  declared in the local xml; an object doesn't have to be constructed before
--  it can be referred to. The exception to this rule is that an object has to
--  be constructed before it can be used as the value of a construct-only
--  property.
--
--  Signal handlers are set up with the <signal> element. The "name" attribute
--  specifies the name of the signal, and the "handler" attribute specifies the
--  function to connect to the signal. By default, GTK+ tries to find the
--  handler using g_module_symbol, but this can be changed by passing a custom
--  Gtk_Builder_Connect_Func to Gtk.Builder.Connect_Signals_Full. The remaining
--  attributes, "after", "swapped" and "object", have the same meaning as the
--  corresponding parameters of the g_signal_connect_object or
--  g_signal_connect_data functions. A "last_modification_time" attribute is
--  also allowed, but it does not have a meaning to the builder.
--
--  Sometimes it is necessary to refer to widgets which have implicitly been
--  constructed by GTK+ as part of a composite widget, to set properties on
--  them or to add further children (e.g. the Vbox of a Gtk.Dialog.Gtk_Dialog).
--  This can be achieved by setting the "internal-child" propery of the <child>
--  element to a true value. Note that GtkBuilder still requires an <object>
--  element for the internal child, even if it has already been constructed.
--
--  A number of widgets have different places where a child can be added (e.g.
--  tabs vs. page content in notebooks). This can be reflected in a UI
--  definition by specifying the "type" attribute on a <child>. The possible
--  values for the "type" attribute are described in the sections describing
--  the widget-specific portions of UI definitions.
--
--  == A GtkBuilder UI Definition ==
--
--    <interface>
--    <object class="GtkDialog" id="dialog1">
--    <child internal-child="vbox">
--    <object class="GtkVBox" id="vbox1">
--    <property name="border-width">10</property>
--    <child internal-child="action_area">
--    <object class="GtkHButtonBox" id="hbuttonbox1">
--    <property name="border-width">20</property>
--    <child>
--    <object class="GtkButton" id="ok_button">
--    <property name="label">gtk-ok</property>
--    <property name="use-stock">TRUE</property>
--    <signal name="clicked" handler="ok_button_clicked"/>
--    </object>
--    </child>
--    </object>
--    </child>
--    </object>
--    </child>
--    </object>
--    </interface>
--  Beyond this general structure, several object classes define their own XML
--  DTD fragments for filling in the ANY placeholders in the DTD above. Note
--  that a custom element in a <child> element gets parsed by the custom tag
--  handler of the parent object, while a custom element in an <object> element
--  gets parsed by the custom tag handler of the object.
--
--  These XML fragments are explained in the documentation of the respective
--  objects, see <link linkend="GtkWidget-BUILDER-UI">GtkWidget</link>, <link
--  linkend="GtkLabel-BUILDER-UI">GtkLabel</link>, <link
--  linkend="GtkWindow-BUILDER-UI">GtkWindow</link>, <link
--  linkend="GtkContainer-BUILDER-UI">GtkContainer</link>, <link
--  linkend="GtkDialog-BUILDER-UI">GtkDialog</link>, <link
--  linkend="GtkCellLayout-BUILDER-UI">GtkCellLayout</link>, <link
--  linkend="GtkColorSelectionDialog-BUILDER-UI">GtkColorSelectionDialog</link>,
--  <link
--  linkend="GtkFontSelectionDialog-BUILDER-UI">GtkFontSelectionDialog</link>,
--  <link linkend="GtkExpander-BUILDER-UI">GtkExpander</link>, <link
--  linkend="GtkFrame-BUILDER-UI">GtkFrame</link>, <link
--  linkend="GtkListStore-BUILDER-UI">GtkListStore</link>, <link
--  linkend="GtkTreeStore-BUILDER-UI">GtkTreeStore</link>, <link
--  linkend="GtkNotebook-BUILDER-UI">GtkNotebook</link>, <link
--  linkend="GtkSizeGroup-BUILDER-UI">GtkSizeGroup</link>, <link
--  linkend="GtkTreeView-BUILDER-UI">GtkTreeView</link>, <link
--  linkend="GtkUIManager-BUILDER-UI">GtkUIManager</link>, <link
--  linkend="GtkActionGroup-BUILDER-UI">GtkActionGroup</link>. <link
--  linkend="GtkMenuItem-BUILDER-UI">GtkMenuItem</link>, <link
--  linkend="GtkMenuToolButton-BUILDER-UI">GtkMenuToolButton</link>, <link
--  linkend="GtkAssistant-BUILDER-UI">GtkAssistant</link>, <link
--  linkend="GtkScale-BUILDER-UI">GtkScale</link>, <link
--  linkend="GtkComboBoxText-BUILDER-UI">GtkComboBoxText</link>, <link
--  linkend="GtkRecentFilter-BUILDER-UI">GtkRecentFilter</link>, <link
--  linkend="GtkFileFilter-BUILDER-UI">GtkFileFilter</link>, <link
--  linkend="GtkTextTagTable-BUILDER-UI">GtkTextTagTable</link>.
--
--  == Embedding other XML ==
--
--  Apart from the language for UI descriptions that has been explained in the
--  previous section, GtkBuilder can also parse XML fragments of <link
--  linkend="gio-GMenu-Markup">GMenu markup</link>. The resulting
--  Glib.Menu.Gmenu object and its named submenus are available via
--  Gtk.Builder.Get_Object like other constructed objects.
--
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;    use GNAT.Strings;
with Glib;            use Glib;
with Glib.Error;      use Glib.Error;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Values;     use Glib.Values;

package Gtk.Builder is

   type Gtk_Builder_Record is new GObject_Record with null record;
   type Gtk_Builder is access all Gtk_Builder_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Builder_Connect_Func is access procedure
     (Builder        : not null access Gtk_Builder_Record'Class;
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

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Builder : out Gtk_Builder);
   procedure Initialize (Builder : not null access Gtk_Builder_Record'Class);
   --  Creates a new builder object.
   --  Since: gtk+ 2.12

   function Gtk_Builder_New return Gtk_Builder;
   --  Creates a new builder object.
   --  Since: gtk+ 2.12

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_builder_get_type");

   -------------
   -- Methods --
   -------------

   function Add_From_File
      (Builder  : not null access Gtk_Builder_Record;
       Filename : UTF8_String;
       Error    : access Glib.Error.GError) return Guint;
   --  Parses a file containing a <link linkend="BUILDER-UI">GtkBuilder UI
   --  definition</link> and merges it with the current contents of Builder.
   --  Upon errors 0 will be returned and Error will be assigned a
   --  Gerror.Gerror from the GTK_BUILDER_ERROR, G_MARKUP_ERROR or G_FILE_ERROR
   --  domain.
   --  Since: gtk+ 2.12
   --  "filename": the name of the file to parse

   function Add_From_Resource
      (Builder       : not null access Gtk_Builder_Record;
       Resource_Path : UTF8_String;
       Error         : access Glib.Error.GError) return Guint;
   --  Parses a resource file containing a <link
   --  linkend="BUILDER-UI">GtkBuilder UI definition</link> and merges it with
   --  the current contents of Builder.
   --  Upon errors 0 will be returned and Error will be assigned a
   --  Gerror.Gerror from the GTK_BUILDER_ERROR, G_MARKUP_ERROR or
   --  G_RESOURCE_ERROR domain.
   --  Since: gtk+ 3.4
   --  "resource_path": the path of the resource file to parse

   function Add_From_String
      (Builder : not null access Gtk_Builder_Record;
       Buffer  : UTF8_String;
       Error   : access Glib.Error.GError) return Guint;
   --  Parses a string containing a <link linkend="BUILDER-UI">GtkBuilder UI
   --  definition</link> and merges it with the current contents of Builder.
   --  Upon errors 0 will be returned and Error will be assigned a
   --  Gerror.Gerror from the GTK_BUILDER_ERROR or G_MARKUP_ERROR domain.
   --  Since: gtk+ 2.12
   --  "buffer": the string to parse

   function Add_Objects_From_File
      (Builder    : not null access Gtk_Builder_Record;
       Filename   : UTF8_String;
       Object_Ids : GNAT.Strings.String_List;
       Error      : access Glib.Error.GError) return Guint;
   --  Parses a file containing a <link linkend="BUILDER-UI">GtkBuilder UI
   --  definition</link> building only the requested objects and merges them
   --  with the current contents of Builder.
   --  Upon errors 0 will be returned and Error will be assigned a
   --  Gerror.Gerror from the GTK_BUILDER_ERROR, G_MARKUP_ERROR or G_FILE_ERROR
   --  domain.
   --  Note:
   --  If you are adding an object that depends on an object that is not its
   --  child (for instance a Gtk.Tree_View.Gtk_Tree_View that depends on its
   --  Gtk.Tree_Model.Gtk_Tree_Model), you have to explicitely list all of them
   --  in Object_Ids.
   --  Since: gtk+ 2.14
   --  "filename": the name of the file to parse
   --  "object_ids": nul-terminated array of objects to build

   function Add_Objects_From_Resource
      (Builder       : not null access Gtk_Builder_Record;
       Resource_Path : UTF8_String;
       Object_Ids    : GNAT.Strings.String_List;
       Error         : access Glib.Error.GError) return Guint;
   --  Parses a resource file containing a <link
   --  linkend="BUILDER-UI">GtkBuilder UI definition</link> building only the
   --  requested objects and merges them with the current contents of Builder.
   --  Upon errors 0 will be returned and Error will be assigned a
   --  Gerror.Gerror from the GTK_BUILDER_ERROR, G_MARKUP_ERROR or
   --  G_RESOURCE_ERROR domain.
   --  Note:
   --  If you are adding an object that depends on an object that is not its
   --  child (for instance a Gtk.Tree_View.Gtk_Tree_View that depends on its
   --  Gtk.Tree_Model.Gtk_Tree_Model), you have to explicitely list all of them
   --  in Object_Ids.
   --  Since: gtk+ 3.4
   --  "resource_path": the path of the resource file to parse
   --  "object_ids": nul-terminated array of objects to build

   function Add_Objects_From_String
      (Builder    : not null access Gtk_Builder_Record;
       Buffer     : UTF8_String;
       Length     : Gsize;
       Object_Ids : GNAT.Strings.String_List;
       Error      : access Glib.Error.GError) return Guint;
   --  Parses a string containing a <link linkend="BUILDER-UI">GtkBuilder UI
   --  definition</link> building only the requested objects and merges them
   --  with the current contents of Builder.
   --  Upon errors 0 will be returned and Error will be assigned a
   --  Gerror.Gerror from the GTK_BUILDER_ERROR or G_MARKUP_ERROR domain.
   --  Note:
   --  If you are adding an object that depends on an object that is not its
   --  child (for instance a Gtk.Tree_View.Gtk_Tree_View that depends on its
   --  Gtk.Tree_Model.Gtk_Tree_Model), you have to explicitely list all of them
   --  in Object_Ids.
   --  Since: gtk+ 2.14
   --  "buffer": the string to parse
   --  "length": the length of Buffer (may be -1 if Buffer is nul-terminated)
   --  "object_ids": nul-terminated array of objects to build

   procedure Connect_Signals
      (Builder   : not null access Gtk_Builder_Record;
       User_Data : System.Address);
   --  This method is a simpler variation of Gtk.Builder.Connect_Signals_Full.
   --  It uses GModule's introspective features (by opening the module null) to
   --  look at the application's symbol table. From here it tries to match the
   --  signal handler names given in the interface description with symbols in
   --  the application and connects the signals. Note that this function can
   --  only be called once, subsequent calls will do nothing.
   --  Note that this function will not work correctly if GModule is not
   --  supported on the platform.
   --  When compiling applications for Windows, you must declare signal
   --  callbacks with G_MODULE_EXPORT, or they will not be put in the symbol
   --  table. On Linux and Unices, this is not necessary; applications should
   --  instead be compiled with the -Wl,--export-dynamic CFLAGS, and linked
   --  against gmodule-export-2.0.
   --  Since: gtk+ 2.12
   --  "user_data": a pointer to a structure sent in as user data to all
   --  signals

   procedure Connect_Signals_Full
      (Builder : not null access Gtk_Builder_Record;
       Func    : Gtk_Builder_Connect_Func);
   --  This function can be thought of the interpreted language binding
   --  version of Gtk.Builder.Connect_Signals, except that it does not require
   --  GModule to function correctly.
   --  Since: gtk+ 2.12
   --  "func": the function used to connect the signals

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Connect_Signals_Full_User_Data is

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

      procedure Connect_Signals_Full
         (Builder   : not null access Gtk.Builder.Gtk_Builder_Record'Class;
          Func      : Gtk_Builder_Connect_Func;
          User_Data : User_Data_Type);
      --  This function can be thought of the interpreted language binding
      --  version of Gtk.Builder.Connect_Signals, except that it does not
      --  require GModule to function correctly.
      --  Since: gtk+ 2.12
      --  "func": the function used to connect the signals
      --  "user_data": arbitrary data that will be passed to the connection
      --  function

   end Connect_Signals_Full_User_Data;

   procedure Expose_Object
      (Builder : not null access Gtk_Builder_Record;
       Name    : UTF8_String;
       Object  : not null access Glib.Object.GObject_Record'Class);
   --  Add Object to the Builder object pool so it can be referenced just like
   --  any other object built by builder.
   --  Since: gtk+ 3.8
   --  "name": the name of the object exposed to the builder
   --  "object": the object to expose

   function Get_Object
      (Builder : not null access Gtk_Builder_Record;
       Name    : UTF8_String) return Glib.Object.GObject;
   --  Gets the object named Name. Note that this function does not increment
   --  the reference count of the returned object.
   --  Since: gtk+ 2.12
   --  "name": name of object to get

   function Get_Objects
      (Builder : not null access Gtk_Builder_Record)
       return Glib.Object.Object_List.GSlist;
   --  Gets all objects that have been constructed by Builder. Note that this
   --  function does not increment the reference counts of the returned
   --  objects.
   --  Since: gtk+ 2.12

   function Get_Translation_Domain
      (Builder : not null access Gtk_Builder_Record) return UTF8_String;
   --  Gets the translation domain of Builder.
   --  Since: gtk+ 2.12

   procedure Set_Translation_Domain
      (Builder : not null access Gtk_Builder_Record;
       Domain  : UTF8_String := "");
   --  Sets the translation domain of Builder. See
   --  Gtk.Builder.Gtk_Builder:translation-domain.
   --  Since: gtk+ 2.12
   --  "domain": the translation domain or null

   function Get_Type_From_Name
      (Builder   : not null access Gtk_Builder_Record;
       Type_Name : UTF8_String) return GType;
   --  Looks up a type by name, using the virtual function that
   --  Gtk.Builder.Gtk_Builder has for that purpose. This is mainly used when
   --  implementing the Gtk.Buildable.Gtk_Buildable interface on a type.
   --  Since: gtk+ 2.12
   --  "type_name": type name to lookup

   procedure Value_From_String
      (Builder : not null access Gtk_Builder_Record;
       Pspec   : in out Glib.Param_Spec;
       String  : UTF8_String;
       Value   : out Glib.Values.GValue;
       Success : out Boolean);
   --  This function demarshals a value from a string. This function calls
   --  g_value_init on the Value argument, so it need not be initialised
   --  beforehand.
   --  This function can handle char, uchar, boolean, int, uint, long, ulong,
   --  enum, flags, float, double, string, Gdk.Color.Gdk_Color,
   --  Gdk.RGBA.Gdk_RGBA and Gtk.Adjustment.Gtk_Adjustment type values. Support
   --  for Gtk.Widget.Gtk_Widget type values is still to come.
   --  Upon errors False will be returned and Error will be assigned a
   --  Gerror.Gerror from the GTK_BUILDER_ERROR domain.
   --  Since: gtk+ 2.12
   --  "pspec": the Glib.Param_Spec for the property
   --  "string": the string representation of the value
   --  "value": the Glib.Values.GValue to store the result in

   function Value_From_String_Type
      (Builder  : not null access Gtk_Builder_Record;
       The_Type : GType;
       String   : UTF8_String;
       Value    : access Glib.Values.GValue) return Boolean;
   --  Like Gtk.Builder.Value_From_String, this function demarshals a value
   --  from a string, but takes a GType instead of Glib.Param_Spec. This
   --  function calls g_value_init on the Value argument, so it need not be
   --  initialised beforehand.
   --  Upon errors False will be returned and Error will be assigned a
   --  Gerror.Gerror from the GTK_BUILDER_ERROR domain.
   --  Since: gtk+ 2.12
   --  "type": the GType of the value
   --  "string": the string representation of the value
   --  "value": the Glib.Values.GValue to store the result in

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Translation_Domain_Property : constant Glib.Properties.Property_String;
   --  The translation domain used when translating property values that have
   --  been marked as translatable in interface descriptions. If the
   --  translation domain is null, Gtk.Builder.Gtk_Builder uses gettext,
   --  otherwise g_dgettext.

private
   Translation_Domain_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("translation-domain");
end Gtk.Builder;
