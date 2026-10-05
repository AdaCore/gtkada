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

--  The Glib.Settings.Gsettings class provides a convenient API for storing
--  and retrieving application settings.
--
--  Reads and writes can be considered to be non-blocking. Reading settings
--  with Glib.Settings.Gsettings is typically extremely fast: on approximately
--  the same order of magnitude (but slower than) a GHash_Table lookup. Writing
--  settings is also extremely fast in terms of time to return to your
--  application, but can be extremely expensive for other threads and other
--  processes. Many settings backends (including dconf) have lazy
--  initialisation which means in the common case of the user using their
--  computer without modifying any settings a lot of work can be avoided. For
--  dconf, the D-Bus service doesn't even need to be started in this case. For
--  this reason, you should only ever modify Glib.Settings.Gsettings keys in
--  response to explicit user action. Particular care should be paid to ensure
--  that modifications are not made during startup -- for example, when setting
--  the initial value of preferences widgets. The built-in Glib.Settings.Bind
--  functionality is careful not to write settings in response to notify
--  signals as a result of modifications that it makes to widgets.
--
--  When creating a GSettings instance, you have to specify a schema that
--  describes the keys in your settings and their types and default values, as
--  well as some other information.
--
--  Normally, a schema has a fixed path that determines where the settings are
--  stored in the conceptual global tree of settings. However, schemas can also
--  be '[relocatable][gsettings-relocatable]', i.e. not equipped with a fixed
--  path. This is useful e.g. when the schema describes an 'account', and you
--  want to be able to store a arbitrary number of accounts.
--
--  Paths must start with and end with a forward slash character ('/') and
--  must not contain two sequential slash characters. Paths should be chosen
--  based on a domain name associated with the program or library to which the
--  settings belong. Examples of paths are "/org/gtk/settings/file-chooser/"
--  and "/ca/desrt/dconf-editor/". Paths should not start with "/apps/",
--  "/desktop/" or "/system/" as they often did in GConf.
--
--  Unlike other configuration systems (like GConf), GSettings does not
--  restrict keys to basic types like strings and numbers. GSettings stores
--  values as Glib.Variant.Gvariant, and allows any Glib.Variant.Gvariant_Type
--  for keys. Key names are restricted to lowercase characters, numbers and
--  '-'. Furthermore, the names must begin with a lowercase character, must not
--  end with a '-', and must not contain consecutive dashes.
--
--  Similar to GConf, the default values in GSettings schemas can be
--  localized, but the localized values are stored in gettext catalogs and
--  looked up with the domain that is specified in the `gettext-domain`
--  attribute of the <schemalist> or <schema> elements and the category that is
--  specified in the `l10n` attribute of the <default> element. The string
--  which is translated includes all text in the <default> element, including
--  any surrounding quotation marks.
--
--  The `l10n` attribute must be set to `messages` or `time`, and sets the
--  [locale category for
--  translation](https://www.gnu.org/software/gettext/manual/html_node/Aspects.htmlindex-locale-categories-1).
--  The `messages` category should be used by default; use `time` for
--  translatable date or time formats. A translation comment can be added as an
--  XML comment immediately above the <default> element — it is recommended to
--  add these comments to aid translators understand the meaning and
--  implications of the default value. An optional translation `context`
--  attribute can be set on the <default> element to disambiguate multiple
--  defaults which use the same string.
--
--  For example:
--
--     <!-- Translators: A list of words which are not allowed to be typed, in
--          GVariant serialization syntax.
--          See: https://developer.gnome.org/glib/stable/gvariant-text.html -->
--     <default l10n='messages' context='Banned words'>['bad', 'words']</default>
--  Translations of default values must remain syntactically valid serialized
--  GVariants (e.g. retaining any surrounding quotation marks) or runtime
--  errors will occur.
--
--  GSettings uses schemas in a compact binary form that is created by the
--  [glib-compile-schemas][glib-compile-schemas] utility. The input is a schema
--  description in an XML format.
--
--  A DTD for the gschema XML format can be found here:
--  [gschema.dtd](https://gitlab.gnome.org/GNOME/glib/-/blob/HEAD/gio/gschema.dtd)
--
--  The [glib-compile-schemas][glib-compile-schemas] tool expects schema files
--  to have the extension `.gschema.xml`.
--
--  At runtime, schemas are identified by their id (as specified in the id
--  attribute of the <schema> element). The convention for schema ids is to use
--  a dotted name, similar in style to a D-Bus bus name, e.g.
--  "org.gnome.SessionManager". In particular, if the settings are for a
--  specific service that owns a D-Bus bus name, the D-Bus bus name and schema
--  id should match. For schemas which deal with settings not associated with
--  one named application, the id should not use StudlyCaps, e.g.
--  "org.gnome.font-rendering".
--
--  In addition to Glib.Variant.Gvariant types, keys can have types that have
--  enumerated types. These can be described by a <choice>, <enum> or <flags>
--  element, as seen in the [example][schema-enumerated]. The underlying type
--  of such a key is string, but you can use Glib.Settings.Get_Enum,
--  Glib.Settings.Set_Enum, Glib.Settings.Get_Flags, Glib.Settings.Set_Flags
--  access the numeric values corresponding to the string value of enum and
--  flags keys.
--
--  An example for default value:
--
--     <schemalist>
--       <schema id="org.gtk.Test" path="/org/gtk/Test/" gettext-domain="test">
--
--         <key name="greeting" type="s">
--           <default l10n="messages">"Hello, earthlings"</default>
--           <summary>A greeting</summary>
--           <description>
--             Greeting of the invading martians
--           </description>
--         </key>
--
--         <key name="box" type="(ii)">
--           <default>(20,30)</default>
--         </key>
--
--         <key name="empty-string" type="s">
--           <default>""</default>
--           <summary>Empty strings have to be provided in GVariant form</summary>
--         </key>
--
--       </schema>
--     </schemalist>
--  An example for ranges, choices and enumerated types:
--
--     <schemalist>
--
--       <enum id="org.gtk.Test.myenum">
--         <value nick="first" value="1"/>
--         <value nick="second" value="2"/>
--       </enum>
--
--       <flags id="org.gtk.Test.myflags">
--         <value nick="flag1" value="1"/>
--         <value nick="flag2" value="2"/>
--         <value nick="flag3" value="4"/>
--       </flags>
--
--       <schema id="org.gtk.Test">
--
--         <key name="key-with-range" type="i">
--           <range min="1" max="100"/>
--           <default>10</default>
--         </key>
--
--         <key name="key-with-choices" type="s">
--           <choices>
--             <choice value='Elisabeth'/>
--             <choice value='Annabeth'/>
--             <choice value='Joe'/>
--           </choices>
--           <aliases>
--             <alias value='Anna' target='Annabeth'/>
--             <alias value='Beth' target='Elisabeth'/>
--           </aliases>
--           <default>'Joe'</default>
--         </key>
--
--         <key name='enumerated-key' enum='org.gtk.Test.myenum'>
--           <default>'first'</default>
--         </key>
--
--         <key name='flags-key' flags='org.gtk.Test.myflags'>
--           <default>["flag1","flag2"]</default>
--         </key>
--       </schema>
--     </schemalist>
--  ## Vendor overrides
--
--  Default values are defined in the schemas that get installed by an
--  application. Sometimes, it is necessary for a vendor or distributor to
--  adjust these defaults. Since patching the XML source for the schema is
--  inconvenient and error-prone, [glib-compile-schemas][glib-compile-schemas]
--  reads so-called vendor override' files. These are keyfiles in the same
--  directory as the XML schema sources which can override default values. The
--  schema id serves as the group name in the key file, and the values are
--  expected in serialized GVariant form, as in the following example:
--
--     [org.gtk.Example]
--     key1='string'
--     key2=1.5
--
--
--  glib-compile-schemas expects schema files to have the extension
--  `.gschema.override`.
--
--  ## Binding
--
--  A very convenient feature of GSettings lets you bind Glib.Object.GObject
--  properties directly to settings, using Glib.Settings.Bind. Once a GObject
--  property has been bound to a setting, changes on either side are
--  automatically propagated to the other side. GSettings handles details like
--  mapping between GObject and GVariant types, and preventing infinite cycles.
--
--  This makes it very easy to hook up a preferences dialog to the underlying
--  settings. To make this even more convenient, GSettings looks for a boolean
--  property with the name "sensitivity" and automatically binds it to the
--  writability of the bound setting. If this 'magic' gets in the way, it can
--  be suppressed with the Glib.Settings.No_Sensitivity flag.
--
--  ## Relocatable schemas # {gsettings-relocatable}
--
--  A relocatable schema is one with no `path` attribute specified on its
--  <schema> element. By using Glib.Settings.G_New_With_Path, a
--  Glib.Settings.Gsettings object can be instantiated for a relocatable
--  schema, assigning a path to the instance. Paths passed to
--  Glib.Settings.G_New_With_Path will typically be constructed dynamically
--  from a constant prefix plus some form of instance identifier; but they must
--  still be valid GSettings paths. Paths could also be constant and used with
--  a globally installed schema originating from a dependency library.
--
--  For example, a relocatable schema could be used to store geometry
--  information for different windows in an application. If the schema ID was
--  `org.foo.MyApp.Window`, it could be instantiated for paths
--  `/org/foo/MyApp/main/`, `/org/foo/MyApp/document-1/`,
--  `/org/foo/MyApp/document-2/`, etc. If any of the paths are well-known they
--  can be specified as <child> elements in the parent schema, e.g.:
--
--     <schema id="org.foo.MyApp" path="/org/foo/MyApp/">
--       <child name="main" schema="org.foo.MyApp.Window"/>
--     </schema>
--  ## Build system integration # {gsettings-build-system}
--
--  GSettings comes with autotools integration to simplify compiling and
--  installing schemas. To add GSettings support to an application, add the
--  following to your `configure.ac`:
--
--     GLIB_GSETTINGS
--
--
--  In the appropriate `Makefile.am`, use the following snippet to compile and
--  install the named schema:
--
--     gsettings_SCHEMAS = org.foo.MyApp.gschema.xml
--     EXTRA_DIST = $(gsettings_SCHEMAS)
--
--     Gsettings_Rules@
--
--
--  No changes are needed to the build system to mark a schema XML file for
--  translation. Assuming it sets the `gettext-domain` attribute, a schema may
--  be marked for translation by adding it to `POTFILES.in`, assuming gettext
--  0.19 is in use (the preferred method for translation):
--
--     data/org.foo.MyApp.gschema.xml
--
--
--  Alternatively, if intltool 0.50.1 is in use:
--
--     [type: gettext/gsettings]data/org.foo.MyApp.gschema.xml
--
--
--  GSettings will use gettext to look up translations for the <summary> and
--  <description> elements, and also any <default> elements which have a `l10n`
--  attribute set. Translations must not be included in the `.gschema.xml` file
--  by the build system, for example by using intltool XML rules with a
--  `.gschema.xml.in` template.
--
--  If an enumerated type defined in a C header file is to be used in a
--  GSettings schema, it can either be defined manually using an <enum> element
--  in the schema XML, or it can be extracted automatically from the C header.
--  This approach is preferred, as it ensures the two representations are
--  always synchronised. To do so, add the following to the relevant
--  `Makefile.am`:
--
--     gsettings_ENUM_NAMESPACE = org.foo.MyApp
--     gsettings_ENUM_FILES = my-app-enums.h my-app-misc.h
--
--
--  `gsettings_ENUM_NAMESPACE` specifies the schema namespace for the enum
--  files, which are specified in `gsettings_ENUM_FILES`. This will generate a
--  `org.foo.MyApp.enums.xml` file containing the extracted enums, which will
--  be automatically included in the schema compilation, install and uninstall
--  rules. It should not be committed to version control or included in
--  `EXTRA_DIST`.
--
--  <group>GIO</group>
--  <gtkada_demo>create_settings.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;            use GNAT.Strings;
with Glib.Action;             use Glib.Action;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Settings_Backend;   use Glib.Settings_Backend;
with Glib.Settings_Schema;    use Glib.Settings_Schema;
with Glib.Variant;            use Glib.Variant;

package Glib.Settings is

   type Gsettings_Record is new GObject_Record with null record;
   type Gsettings is access all Gsettings_Record'Class;

   type GSettings_Bind_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, GSettings_Bind_Flags);
   --  Flags used when creating a binding.
   --
   --  These flags determine in which direction the binding works. The default
   --  is to synchronize in both directions.

   Default : constant GSettings_Bind_Flags := 0;
   Get : constant GSettings_Bind_Flags := 1;
   Set : constant GSettings_Bind_Flags := 2;
   No_Sensitivity : constant GSettings_Bind_Flags := 4;
   Get_No_Changes : constant GSettings_Bind_Flags := 8;
   Invert_Boolean : constant GSettings_Bind_Flags := 16;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package GSettings_Bind_Flags_Properties is
      new Generic_Internal_Flags_Property (GSettings_Bind_Flags);
   type Property_GSettings_Bind_Flags is new GSettings_Bind_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure G_New (Self : out Gsettings; Schema_Id : UTF8_String);
   --  Creates a new Glib.Settings.Gsettings object with the schema specified
   --  by Schema_Id.
   --  It is an error for the schema to not exist: schemas are an essential
   --  part of a program, as they provide type information. If schemas need to
   --  be dynamically loaded (for example, from an optional runtime
   --  dependency), Glib.Settings_Schema_Source.Lookup can be used to test for
   --  their existence before loading them.
   --  Signals on the newly created Glib.Settings.Gsettings object will be
   --  dispatched via the thread-default Gmain.Context.Gmain_Context in effect
   --  at the time of the call to Glib.Settings.G_New. The new
   --  Glib.Settings.Gsettings will hold a reference on the context. See
   --  g_main_context_push_thread_default.
   --  Since: gtk+ 2.26
   --  @param Schema_Id the id of the schema

   procedure Initialize
      (Self      : not null access Gsettings_Record'Class;
       Schema_Id : UTF8_String);
   --  Creates a new Glib.Settings.Gsettings object with the schema specified
   --  by Schema_Id.
   --  It is an error for the schema to not exist: schemas are an essential
   --  part of a program, as they provide type information. If schemas need to
   --  be dynamically loaded (for example, from an optional runtime
   --  dependency), Glib.Settings_Schema_Source.Lookup can be used to test for
   --  their existence before loading them.
   --  Signals on the newly created Glib.Settings.Gsettings object will be
   --  dispatched via the thread-default Gmain.Context.Gmain_Context in effect
   --  at the time of the call to Glib.Settings.G_New. The new
   --  Glib.Settings.Gsettings will hold a reference on the context. See
   --  g_main_context_push_thread_default.
   --  Since: gtk+ 2.26
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Schema_Id the id of the schema

   function Gsettings_New (Schema_Id : UTF8_String) return Gsettings;
   --  Creates a new Glib.Settings.Gsettings object with the schema specified
   --  by Schema_Id.
   --  It is an error for the schema to not exist: schemas are an essential
   --  part of a program, as they provide type information. If schemas need to
   --  be dynamically loaded (for example, from an optional runtime
   --  dependency), Glib.Settings_Schema_Source.Lookup can be used to test for
   --  their existence before loading them.
   --  Signals on the newly created Glib.Settings.Gsettings object will be
   --  dispatched via the thread-default Gmain.Context.Gmain_Context in effect
   --  at the time of the call to Glib.Settings.G_New. The new
   --  Glib.Settings.Gsettings will hold a reference on the context. See
   --  g_main_context_push_thread_default.
   --  Since: gtk+ 2.26
   --  @param Schema_Id the id of the schema

   procedure G_New_Full
      (Self    : out Gsettings;
       Schema  : Glib.Settings_Schema.Gsettings_Schema;
       Backend : access Glib.Settings_Backend.Gsettings_Backend_Record'Class;
       Path    : UTF8_String := "");
   --  Creates a new Glib.Settings.Gsettings object with a given schema,
   --  backend and path.
   --  It should be extremely rare that you ever want to use this function. It
   --  is made available for advanced use-cases (such as plugin systems that
   --  want to provide access to schemas loaded from custom locations, etc).
   --  At the most basic level, a Glib.Settings.Gsettings object is a pure
   --  composition of 4 things: a Glib.Settings_Schema.Gsettings_Schema, a
   --  Glib.Settings_Backend.Gsettings_Backend, a path within that backend, and
   --  a Gmain.Context.Gmain_Context to which signals are dispatched.
   --  This constructor therefore gives you full control over constructing
   --  Glib.Settings.Gsettings instances. The first 3 parameters are given
   --  directly as Schema, Backend and Path, and the main context is taken from
   --  the thread-default (as per Glib.Settings.G_New).
   --  If Backend is null then the default backend is used.
   --  If Path is null then the path from the schema is used. It is an error
   --  if Path is null and the schema has no path of its own or if Path is
   --  non-null and not equal to the path that the schema does have.
   --  Since: gtk+ 2.32
   --  @param Schema a Glib.Settings_Schema.Gsettings_Schema
   --  @param Backend a Glib.Settings_Backend.Gsettings_Backend
   --  @param Path the path to use

   procedure Initialize_Full
      (Self    : not null access Gsettings_Record'Class;
       Schema  : Glib.Settings_Schema.Gsettings_Schema;
       Backend : access Glib.Settings_Backend.Gsettings_Backend_Record'Class;
       Path    : UTF8_String := "");
   --  Creates a new Glib.Settings.Gsettings object with a given schema,
   --  backend and path.
   --  It should be extremely rare that you ever want to use this function. It
   --  is made available for advanced use-cases (such as plugin systems that
   --  want to provide access to schemas loaded from custom locations, etc).
   --  At the most basic level, a Glib.Settings.Gsettings object is a pure
   --  composition of 4 things: a Glib.Settings_Schema.Gsettings_Schema, a
   --  Glib.Settings_Backend.Gsettings_Backend, a path within that backend, and
   --  a Gmain.Context.Gmain_Context to which signals are dispatched.
   --  This constructor therefore gives you full control over constructing
   --  Glib.Settings.Gsettings instances. The first 3 parameters are given
   --  directly as Schema, Backend and Path, and the main context is taken from
   --  the thread-default (as per Glib.Settings.G_New).
   --  If Backend is null then the default backend is used.
   --  If Path is null then the path from the schema is used. It is an error
   --  if Path is null and the schema has no path of its own or if Path is
   --  non-null and not equal to the path that the schema does have.
   --  Since: gtk+ 2.32
   --  Initialize_Full does nothing if the object was already created with
   --  another call to Initialize* or G_New.
   --  @param Schema a Glib.Settings_Schema.Gsettings_Schema
   --  @param Backend a Glib.Settings_Backend.Gsettings_Backend
   --  @param Path the path to use

   function Gsettings_New_Full
      (Schema  : Glib.Settings_Schema.Gsettings_Schema;
       Backend : access Glib.Settings_Backend.Gsettings_Backend_Record'Class;
       Path    : UTF8_String := "") return Gsettings;
   --  Creates a new Glib.Settings.Gsettings object with a given schema,
   --  backend and path.
   --  It should be extremely rare that you ever want to use this function. It
   --  is made available for advanced use-cases (such as plugin systems that
   --  want to provide access to schemas loaded from custom locations, etc).
   --  At the most basic level, a Glib.Settings.Gsettings object is a pure
   --  composition of 4 things: a Glib.Settings_Schema.Gsettings_Schema, a
   --  Glib.Settings_Backend.Gsettings_Backend, a path within that backend, and
   --  a Gmain.Context.Gmain_Context to which signals are dispatched.
   --  This constructor therefore gives you full control over constructing
   --  Glib.Settings.Gsettings instances. The first 3 parameters are given
   --  directly as Schema, Backend and Path, and the main context is taken from
   --  the thread-default (as per Glib.Settings.G_New).
   --  If Backend is null then the default backend is used.
   --  If Path is null then the path from the schema is used. It is an error
   --  if Path is null and the schema has no path of its own or if Path is
   --  non-null and not equal to the path that the schema does have.
   --  Since: gtk+ 2.32
   --  @param Schema a Glib.Settings_Schema.Gsettings_Schema
   --  @param Backend a Glib.Settings_Backend.Gsettings_Backend
   --  @param Path the path to use

   procedure G_New_With_Backend
      (Self      : out Gsettings;
       Schema_Id : UTF8_String;
       Backend   : not null access Glib.Settings_Backend.Gsettings_Backend_Record'Class);
   --  Creates a new Glib.Settings.Gsettings object with the schema specified
   --  by Schema_Id and a given Glib.Settings_Backend.Gsettings_Backend.
   --  Creating a Glib.Settings.Gsettings object with a different backend
   --  allows accessing settings from a database other than the usual one. For
   --  example, it may make sense to pass a backend corresponding to the
   --  "defaults" settings database on the system to get a settings object that
   --  modifies the system default settings instead of the settings for this
   --  user.
   --  Since: gtk+ 2.26
   --  @param Schema_Id the id of the schema
   --  @param Backend the Glib.Settings_Backend.Gsettings_Backend to use

   procedure Initialize_With_Backend
      (Self      : not null access Gsettings_Record'Class;
       Schema_Id : UTF8_String;
       Backend   : not null access Glib.Settings_Backend.Gsettings_Backend_Record'Class);
   --  Creates a new Glib.Settings.Gsettings object with the schema specified
   --  by Schema_Id and a given Glib.Settings_Backend.Gsettings_Backend.
   --  Creating a Glib.Settings.Gsettings object with a different backend
   --  allows accessing settings from a database other than the usual one. For
   --  example, it may make sense to pass a backend corresponding to the
   --  "defaults" settings database on the system to get a settings object that
   --  modifies the system default settings instead of the settings for this
   --  user.
   --  Since: gtk+ 2.26
   --  Initialize_With_Backend does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Schema_Id the id of the schema
   --  @param Backend the Glib.Settings_Backend.Gsettings_Backend to use

   function Gsettings_New_With_Backend
      (Schema_Id : UTF8_String;
       Backend   : not null access Glib.Settings_Backend.Gsettings_Backend_Record'Class)
       return Gsettings;
   --  Creates a new Glib.Settings.Gsettings object with the schema specified
   --  by Schema_Id and a given Glib.Settings_Backend.Gsettings_Backend.
   --  Creating a Glib.Settings.Gsettings object with a different backend
   --  allows accessing settings from a database other than the usual one. For
   --  example, it may make sense to pass a backend corresponding to the
   --  "defaults" settings database on the system to get a settings object that
   --  modifies the system default settings instead of the settings for this
   --  user.
   --  Since: gtk+ 2.26
   --  @param Schema_Id the id of the schema
   --  @param Backend the Glib.Settings_Backend.Gsettings_Backend to use

   procedure G_New_With_Backend_And_Path
      (Self      : out Gsettings;
       Schema_Id : UTF8_String;
       Backend   : not null access Glib.Settings_Backend.Gsettings_Backend_Record'Class;
       Path      : UTF8_String);
   --  Creates a new Glib.Settings.Gsettings object with the schema specified
   --  by Schema_Id and a given Glib.Settings_Backend.Gsettings_Backend and
   --  path.
   --  This is a mix of Glib.Settings.G_New_With_Backend and
   --  Glib.Settings.G_New_With_Path.
   --  Since: gtk+ 2.26
   --  @param Schema_Id the id of the schema
   --  @param Backend the Glib.Settings_Backend.Gsettings_Backend to use
   --  @param Path the path to use

   procedure Initialize_With_Backend_And_Path
      (Self      : not null access Gsettings_Record'Class;
       Schema_Id : UTF8_String;
       Backend   : not null access Glib.Settings_Backend.Gsettings_Backend_Record'Class;
       Path      : UTF8_String);
   --  Creates a new Glib.Settings.Gsettings object with the schema specified
   --  by Schema_Id and a given Glib.Settings_Backend.Gsettings_Backend and
   --  path.
   --  This is a mix of Glib.Settings.G_New_With_Backend and
   --  Glib.Settings.G_New_With_Path.
   --  Since: gtk+ 2.26
   --  Initialize_With_Backend_And_Path does nothing if the object was already
   --  created with another call to Initialize* or G_New.
   --  @param Schema_Id the id of the schema
   --  @param Backend the Glib.Settings_Backend.Gsettings_Backend to use
   --  @param Path the path to use

   function Gsettings_New_With_Backend_And_Path
      (Schema_Id : UTF8_String;
       Backend   : not null access Glib.Settings_Backend.Gsettings_Backend_Record'Class;
       Path      : UTF8_String) return Gsettings;
   --  Creates a new Glib.Settings.Gsettings object with the schema specified
   --  by Schema_Id and a given Glib.Settings_Backend.Gsettings_Backend and
   --  path.
   --  This is a mix of Glib.Settings.G_New_With_Backend and
   --  Glib.Settings.G_New_With_Path.
   --  Since: gtk+ 2.26
   --  @param Schema_Id the id of the schema
   --  @param Backend the Glib.Settings_Backend.Gsettings_Backend to use
   --  @param Path the path to use

   procedure G_New_With_Path
      (Self      : out Gsettings;
       Schema_Id : UTF8_String;
       Path      : UTF8_String);
   --  Creates a new Glib.Settings.Gsettings object with the relocatable
   --  schema specified by Schema_Id and a given path.
   --  You only need to do this if you want to directly create a settings
   --  object with a schema that doesn't have a specified path of its own.
   --  That's quite rare.
   --  It is a programmer error to call this function for a schema that has an
   --  explicitly specified path.
   --  It is a programmer error if Path is not a valid path. A valid path
   --  begins and ends with '/' and does not contain two consecutive '/'
   --  characters.
   --  Since: gtk+ 2.26
   --  @param Schema_Id the id of the schema
   --  @param Path the path to use

   procedure Initialize_With_Path
      (Self      : not null access Gsettings_Record'Class;
       Schema_Id : UTF8_String;
       Path      : UTF8_String);
   --  Creates a new Glib.Settings.Gsettings object with the relocatable
   --  schema specified by Schema_Id and a given path.
   --  You only need to do this if you want to directly create a settings
   --  object with a schema that doesn't have a specified path of its own.
   --  That's quite rare.
   --  It is a programmer error to call this function for a schema that has an
   --  explicitly specified path.
   --  It is a programmer error if Path is not a valid path. A valid path
   --  begins and ends with '/' and does not contain two consecutive '/'
   --  characters.
   --  Since: gtk+ 2.26
   --  Initialize_With_Path does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Schema_Id the id of the schema
   --  @param Path the path to use

   function Gsettings_New_With_Path
      (Schema_Id : UTF8_String;
       Path      : UTF8_String) return Gsettings;
   --  Creates a new Glib.Settings.Gsettings object with the relocatable
   --  schema specified by Schema_Id and a given path.
   --  You only need to do this if you want to directly create a settings
   --  object with a schema that doesn't have a specified path of its own.
   --  That's quite rare.
   --  It is a programmer error to call this function for a schema that has an
   --  explicitly specified path.
   --  It is a programmer error if Path is not a valid path. A valid path
   --  begins and ends with '/' and does not contain two consecutive '/'
   --  characters.
   --  Since: gtk+ 2.26
   --  @param Schema_Id the id of the schema
   --  @param Path the path to use

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_settings_get_type");

   -------------
   -- Methods --
   -------------

   procedure Apply (Self : not null access Gsettings_Record);
   --  Applies any changes that have been made to the settings. This function
   --  does nothing unless Settings is in 'delay-apply' mode; see
   --  Glib.Settings.The_Delay. In the normal case settings are always applied
   --  immediately.

   procedure Bind
      (Self     : not null access Gsettings_Record;
       Key      : UTF8_String;
       Object   : not null access Glib.Object.GObject_Record'Class;
       Property : UTF8_String;
       Flags    : GSettings_Bind_Flags);
   --  Create a binding between the Key in the Settings object and the
   --  property Property of Object.
   --  The binding uses the default GIO mapping functions to map between the
   --  settings and property values. These functions handle booleans, numeric
   --  types and string types in a straightforward way. Use
   --  g_settings_bind_with_mapping if you need a custom mapping, or map
   --  between types that are not supported by the default mapping functions.
   --  Unless the Flags include Glib.Settings.No_Sensitivity, this function
   --  also establishes a binding between the writability of Key and the
   --  "sensitive" property of Object (if Object has a boolean property by that
   --  name). See Glib.Settings.Bind_Writable for more details about writable
   --  bindings.
   --  Note that the lifecycle of the binding is tied to Object, and that you
   --  can have only one binding per object property. If you bind the same
   --  property twice on the same object, the second binding overrides the
   --  first one.
   --  Since: gtk+ 2.26
   --  @param Key the key to bind
   --  @param Object a Glib.Object.GObject
   --  @param Property the name of the property to bind
   --  @param Flags flags for the binding

   procedure Bind_Writable
      (Self     : not null access Gsettings_Record;
       Key      : UTF8_String;
       Object   : not null access Glib.Object.GObject_Record'Class;
       Property : UTF8_String;
       Inverted : Boolean);
   --  Create a binding between the writability of Key in the Settings object
   --  and the property Property of Object. The property must be boolean;
   --  "sensitive" or "visible" properties of widgets are the most likely
   --  candidates.
   --  Writable bindings are always uni-directional; changes of the
   --  writability of the setting will be propagated to the object property,
   --  not the other way.
   --  When the Inverted argument is True, the binding inverts the value as it
   --  passes from the setting to the object, i.e. Property will be set to True
   --  if the key is not writable.
   --  Note that the lifecycle of the binding is tied to Object, and that you
   --  can have only one binding per object property. If you bind the same
   --  property twice on the same object, the second binding overrides the
   --  first one.
   --  Since: gtk+ 2.26
   --  @param Key the key to bind
   --  @param Object a Glib.Object.GObject
   --  @param Property the name of a boolean property to bind
   --  @param Inverted whether to 'invert' the value

   function Create_Action
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Glib.Action.Gaction;
   --  Creates a Glib.Action.Gaction corresponding to a given
   --  Glib.Settings.Gsettings key.
   --  The action has the same name as the key.
   --  The value of the key becomes the state of the action and the action is
   --  enabled when the key is writable. Changing the state of the action
   --  results in the key being written to. Changes to the value or writability
   --  of the key cause appropriate change notifications to be emitted for the
   --  action.
   --  For boolean-valued keys, action activations take no parameter and
   --  result in the toggling of the value. For all other types, activations
   --  take the new value for the key (which must have the correct type).
   --  Since: gtk+ 2.32
   --  @param Key the name of a key in Settings
   --  @return a new Glib.Action.Gaction

   procedure The_Delay (Self : not null access Gsettings_Record);
   --  Changes the Glib.Settings.Gsettings object into 'delay-apply' mode. In
   --  this mode, changes to Settings are not immediately propagated to the
   --  backend, but kept locally until Glib.Settings.Apply is called.
   --  Since: gtk+ 2.26

   function Get_Boolean
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Boolean;
   --  Gets the value that is stored at Key in Settings.
   --  A convenience variant of g_settings_get for booleans.
   --  It is a programmer error to give a Key that isn't specified as having a
   --  boolean type in the schema for Settings.
   --  Since: gtk+ 2.26
   --  @param Key the key to get the value for
   --  @return a boolean

   function Set_Boolean
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Boolean) return Boolean;
   --  Sets Key in Settings to Value.
   --  A convenience variant of g_settings_set for booleans.
   --  It is a programmer error to give a Key that isn't specified as having a
   --  boolean type in the schema for Settings.
   --  Since: gtk+ 2.26
   --  @param Key the name of the key to set
   --  @param Value the value to set it to
   --  @return True if setting the key succeeded, False if the key was not
   --  writable

   function Get_Child
      (Self : not null access Gsettings_Record;
       Name : UTF8_String) return Gsettings;
   --  Creates a child settings object which has a base path of
   --  `base-path/Name`, where `base-path` is the base path of Settings.
   --  The schema for the child settings object must have been declared in the
   --  schema of Settings using a `<child>` element.
   --  The created child settings object will inherit the
   --  Glib.Settings.Gsettings:delay-apply mode from Settings.
   --  Since: gtk+ 2.26
   --  @param Name the name of the child schema
   --  @return a 'child' settings object. Has transfer-ownership='full'.

   function Get_Default_Value
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Glib.Variant.Gvariant;
   --  Gets the "default value" of a key.
   --  This is the value that would be read if Glib.Settings.Reset were to be
   --  called on the key.
   --  Note that this may be a different value than returned by
   --  Glib.Settings_Schema_Key.Get_Default_Value if the system administrator
   --  has provided a default value.
   --  Comparing the return values of Glib.Settings.Get_Default_Value and
   --  Glib.Settings.Get_Value is not sufficient for determining if a value has
   --  been set because the user may have explicitly set the value to something
   --  that happens to be equal to the default. The difference here is that if
   --  the default changes in the future, the user's key will still be set.
   --  This function may be useful for adding an indication to a UI of what
   --  the default value was before the user set it.
   --  It is a programmer error to give a Key that isn't contained in the
   --  schema for Settings.
   --  Since: gtk+ 2.40
   --  @param Key the key to get the default value for
   --  @return the default value. Has transfer-ownership='full'.

   function Get_Double
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Gdouble;
   --  Gets the value that is stored at Key in Settings.
   --  A convenience variant of g_settings_get for doubles.
   --  It is a programmer error to give a Key that isn't specified as having a
   --  'double' type in the schema for Settings.
   --  Since: gtk+ 2.26
   --  @param Key the key to get the value for
   --  @return a double

   function Set_Double
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Gdouble) return Boolean;
   --  Sets Key in Settings to Value.
   --  A convenience variant of g_settings_set for doubles.
   --  It is a programmer error to give a Key that isn't specified as having a
   --  'double' type in the schema for Settings.
   --  Since: gtk+ 2.26
   --  @param Key the name of the key to set
   --  @param Value the value to set it to
   --  @return True if setting the key succeeded, False if the key was not
   --  writable

   function Get_Enum
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Glib.Gint;
   --  Gets the value that is stored in Settings for Key and converts it to
   --  the enum value that it represents.
   --  In order to use this function the type of the value must be a string
   --  and it must be marked in the schema file as an enumerated type.
   --  It is a programmer error to give a Key that isn't contained in the
   --  schema for Settings or is not marked as an enumerated type.
   --  If the value stored in the configuration database is not a valid value
   --  for the enumerated type then this function will return the default
   --  value.
   --  Since: gtk+ 2.26
   --  @param Key the key to get the value for
   --  @return the enum value

   function Set_Enum
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Glib.Gint) return Boolean;
   --  Looks up the enumerated type nick for Value and writes it to Key,
   --  within Settings.
   --  It is a programmer error to give a Key that isn't contained in the
   --  schema for Settings or is not marked as an enumerated type, or for Value
   --  not to be a valid value for the named type.
   --  After performing the write, accessing Key directly with
   --  Glib.Settings.Get_String will return the 'nick' associated with Value.
   --  @param Key a key, within Settings
   --  @param Value an enumerated value
   --  @return True, if the set succeeds

   function Get_Flags
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Guint;
   --  Gets the value that is stored in Settings for Key and converts it to
   --  the flags value that it represents.
   --  In order to use this function the type of the value must be an array of
   --  strings and it must be marked in the schema file as a flags type.
   --  It is a programmer error to give a Key that isn't contained in the
   --  schema for Settings or is not marked as a flags type.
   --  If the value stored in the configuration database is not a valid value
   --  for the flags type then this function will return the default value.
   --  Since: gtk+ 2.26
   --  @param Key the key to get the value for
   --  @return the flags value

   function Set_Flags
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Guint) return Boolean;
   --  Looks up the flags type nicks for the bits specified by Value, puts
   --  them in an array of strings and writes the array to Key, within
   --  Settings.
   --  It is a programmer error to give a Key that isn't contained in the
   --  schema for Settings or is not marked as a flags type, or for Value to
   --  contain any bits that are not value for the named type.
   --  After performing the write, accessing Key directly with
   --  Glib.Settings.Get_Strv will return an array of 'nicks'; one for each bit
   --  in Value.
   --  @param Key a key, within Settings
   --  @param Value a flags value
   --  @return True, if the set succeeds

   function Get_Has_Unapplied
      (Self : not null access Gsettings_Record) return Boolean;
   --  Returns whether the Glib.Settings.Gsettings object has any unapplied
   --  changes. This can only be the case if it is in 'delayed-apply' mode.
   --  Since: gtk+ 2.26
   --  @return True if Settings has unapplied changes

   function Get_Int
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Glib.Gint;
   --  Gets the value that is stored at Key in Settings.
   --  A convenience variant of g_settings_get for 32-bit integers.
   --  It is a programmer error to give a Key that isn't specified as having a
   --  int32 type in the schema for Settings.
   --  Since: gtk+ 2.26
   --  @param Key the key to get the value for
   --  @return an integer

   function Set_Int
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Glib.Gint) return Boolean;
   --  Sets Key in Settings to Value.
   --  A convenience variant of g_settings_set for 32-bit integers.
   --  It is a programmer error to give a Key that isn't specified as having a
   --  int32 type in the schema for Settings.
   --  Since: gtk+ 2.26
   --  @param Key the name of the key to set
   --  @param Value the value to set it to
   --  @return True if setting the key succeeded, False if the key was not
   --  writable

   function Get_Int64
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Gint64;
   --  Gets the value that is stored at Key in Settings.
   --  A convenience variant of g_settings_get for 64-bit integers.
   --  It is a programmer error to give a Key that isn't specified as having a
   --  int64 type in the schema for Settings.
   --  Since: gtk+ 2.50
   --  @param Key the key to get the value for
   --  @return a 64-bit integer

   function Set_Int64
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Gint64) return Boolean;
   --  Sets Key in Settings to Value.
   --  A convenience variant of g_settings_set for 64-bit integers.
   --  It is a programmer error to give a Key that isn't specified as having a
   --  int64 type in the schema for Settings.
   --  Since: gtk+ 2.50
   --  @param Key the name of the key to set
   --  @param Value the value to set it to
   --  @return True if setting the key succeeded, False if the key was not
   --  writable

   function Get_Range
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Glib.Variant.Gvariant;
   pragma Obsolescent (Get_Range);
   --  Queries the range of a key.
   --  Since: gtk+ 2.28
   --  Deprecated since 2.40, 1
   --  @param Key the key to query the range of

   function Get_String
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return UTF8_String;
   --  Gets the value that is stored at Key in Settings.
   --  A convenience variant of g_settings_get for strings.
   --  It is a programmer error to give a Key that isn't specified as having a
   --  string type in the schema for Settings.
   --  Since: gtk+ 2.26
   --  @param Key the key to get the value for
   --  @return a newly-allocated string

   function Set_String
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : UTF8_String) return Boolean;
   --  Sets Key in Settings to Value.
   --  A convenience variant of g_settings_set for strings.
   --  It is a programmer error to give a Key that isn't specified as having a
   --  string type in the schema for Settings.
   --  Since: gtk+ 2.26
   --  @param Key the name of the key to set
   --  @param Value the value to set it to
   --  @return True if setting the key succeeded, False if the key was not
   --  writable

   function Get_Strv
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return GNAT.Strings.String_List;
   --  A convenience variant of g_settings_get for string arrays.
   --  It is a programmer error to give a Key that isn't specified as having
   --  an array of strings type in the schema for Settings.
   --  Since: gtk+ 2.26
   --  @param Key the key to get the value for
   --  @return a newly-allocated, null-terminated array of strings, the value
   --  that is stored at Key in Settings.

   function Set_Strv
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : GNAT.Strings.String_List) return Boolean;
   --  Sets Key in Settings to Value.
   --  A convenience variant of g_settings_set for string arrays. If Value is
   --  null, then Key is set to be the empty array.
   --  It is a programmer error to give a Key that isn't specified as having
   --  an array of strings type in the schema for Settings.
   --  Since: gtk+ 2.26
   --  @param Key the name of the key to set
   --  @param Value the value to set it to, or null
   --  @return True if setting the key succeeded, False if the key was not
   --  writable

   function Get_Uint
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Guint;
   --  Gets the value that is stored at Key in Settings.
   --  A convenience variant of g_settings_get for 32-bit unsigned integers.
   --  It is a programmer error to give a Key that isn't specified as having a
   --  uint32 type in the schema for Settings.
   --  Since: gtk+ 2.30
   --  @param Key the key to get the value for
   --  @return an unsigned integer

   function Set_Uint
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Guint) return Boolean;
   --  Sets Key in Settings to Value.
   --  A convenience variant of g_settings_set for 32-bit unsigned integers.
   --  It is a programmer error to give a Key that isn't specified as having a
   --  uint32 type in the schema for Settings.
   --  Since: gtk+ 2.30
   --  @param Key the name of the key to set
   --  @param Value the value to set it to
   --  @return True if setting the key succeeded, False if the key was not
   --  writable

   function Get_Uint64
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Guint64;
   --  Gets the value that is stored at Key in Settings.
   --  A convenience variant of g_settings_get for 64-bit unsigned integers.
   --  It is a programmer error to give a Key that isn't specified as having a
   --  uint64 type in the schema for Settings.
   --  Since: gtk+ 2.50
   --  @param Key the key to get the value for
   --  @return a 64-bit unsigned integer

   function Set_Uint64
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Guint64) return Boolean;
   --  Sets Key in Settings to Value.
   --  A convenience variant of g_settings_set for 64-bit unsigned integers.
   --  It is a programmer error to give a Key that isn't specified as having a
   --  uint64 type in the schema for Settings.
   --  Since: gtk+ 2.50
   --  @param Key the name of the key to set
   --  @param Value the value to set it to
   --  @return True if setting the key succeeded, False if the key was not
   --  writable

   function Get_User_Value
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Glib.Variant.Gvariant;
   --  Checks the "user value" of a key, if there is one.
   --  The user value of a key is the last value that was set by the user.
   --  After calling Glib.Settings.Reset this function should always return
   --  null (assuming something is not wrong with the system configuration).
   --  It is possible that Glib.Settings.Get_Value will return a different
   --  value than this function. This can happen in the case that the user set
   --  a value for a key that was subsequently locked down by the system
   --  administrator -- this function will return the user's old value.
   --  This function may be useful for adding a "reset" option to a UI or for
   --  providing indication that a particular value has been changed.
   --  It is a programmer error to give a Key that isn't contained in the
   --  schema for Settings.
   --  Since: gtk+ 2.40
   --  @param Key the key to get the user value for
   --  @return the user's value, if set. Has transfer-ownership='full'.

   function Get_Value
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Glib.Variant.Gvariant;
   --  Gets the value that is stored in Settings for Key.
   --  It is a programmer error to give a Key that isn't contained in the
   --  schema for Settings.
   --  Since: gtk+ 2.26
   --  @param Key the key to get the value for
   --  @return a new Glib.Variant.Gvariant. Has transfer-ownership='full'.

   function Set_Value
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Glib.Variant.Gvariant) return Boolean;
   --  Sets Key in Settings to Value.
   --  It is a programmer error to give a Key that isn't contained in the
   --  schema for Settings or for Value to have the incorrect type, per the
   --  schema.
   --  If Value is floating then this function consumes the reference.
   --  Since: gtk+ 2.26
   --  @param Key the name of the key to set
   --  @param Value a Glib.Variant.Gvariant of the correct type
   --  @return True if setting the key succeeded, False if the key was not
   --  writable

   function Is_Writable
      (Self : not null access Gsettings_Record;
       Name : UTF8_String) return Boolean;
   --  Finds out if a key can be written or not
   --  Since: gtk+ 2.26
   --  @param Name the name of a key
   --  @return True if the key Name is writable

   function List_Children
      (Self : not null access Gsettings_Record)
       return GNAT.Strings.String_List;
   --  Gets the list of children on Settings.
   --  The list is exactly the list of strings for which it is not an error to
   --  call Glib.Settings.Get_Child.
   --  There is little reason to call this function from "normal" code, since
   --  you should already know what children are in your schema. This function
   --  may still be useful there for introspection reasons, however.
   --  You should free the return value with g_strfreev when you are done with
   --  it.
   --  @return a list of the children on Settings, in no defined order

   function List_Keys
      (Self : not null access Gsettings_Record)
       return GNAT.Strings.String_List;
   pragma Obsolescent (List_Keys);
   --  Introspects the list of keys on Settings.
   --  You should probably not be calling this function from "normal" code
   --  (since you should already know what keys are in your schema). This
   --  function is intended for introspection reasons.
   --  You should free the return value with g_strfreev when you are done with
   --  it.
   --  Deprecated since 2.46, 1
   --  @return a list of the keys on Settings, in no defined order

   function Range_Check
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Glib.Variant.Gvariant) return Boolean;
   pragma Obsolescent (Range_Check);
   --  Checks if the given Value is of the correct type and within the
   --  permitted range for Key.
   --  Since: gtk+ 2.28
   --  Deprecated since 2.40, 1
   --  @param Key the key to check
   --  @param Value the value to check
   --  @return True if Value is valid for Key

   procedure Reset
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String);
   --  Resets Key to its default value.
   --  This call resets the key, as much as possible, to its default value.
   --  That might be the value specified in the schema or the one set by the
   --  administrator.
   --  @param Key the name of a key

   procedure Revert (Self : not null access Gsettings_Record);
   --  Reverts all non-applied changes to the settings. This function does
   --  nothing unless Settings is in 'delay-apply' mode; see
   --  Glib.Settings.The_Delay. In the normal case settings are always applied
   --  immediately.
   --  Change notifications will be emitted for affected keys.

   ----------------------
   -- GtkAda additions --
   ----------------------

   Backend_Property : constant Glib.Properties.Property_Object :=
   Glib.Properties.Build ("backend");

   ---------------
   -- Functions --
   ---------------

   function List_Relocatable_Schemas return GNAT.Strings.String_List;
   pragma Obsolescent (List_Relocatable_Schemas);
   --  Deprecated.
   --  Since: gtk+ 2.28
   --  Deprecated since 2.40, 1
   --  @return a list of relocatable Glib.Settings.Gsettings schemas that are
   --  available, in no defined order. The list must not be modified or freed.

   function List_Schemas return GNAT.Strings.String_List;
   pragma Obsolescent (List_Schemas);
   --  Deprecated.
   --  Since: gtk+ 2.26
   --  Deprecated since 2.40, 1
   --  @return a list of Glib.Settings.Gsettings schemas that are available,
   --  in no defined order. The list must not be modified or freed.

   procedure Sync;
   --  Ensures that all pending operations are complete for the default
   --  backend.
   --  Writes made to a Glib.Settings.Gsettings are handled asynchronously.
   --  For this reason, it is very unlikely that the changes have it to disk by
   --  the time g_settings_set returns.
   --  This call will block until all of the writes have made it to the
   --  backend. Since the mainloop is not running, no change notifications will
   --  be dispatched during this call (but some may be queued by the time the
   --  call is done).

   procedure Unbind
      (Object   : not null access Glib.Object.GObject_Record'Class;
       Property : UTF8_String);
   --  Removes an existing binding for Property on Object.
   --  Note that bindings are automatically removed when the object is
   --  finalized, so it is rarely necessary to call this function.
   --  Since: gtk+ 2.26
   --  @param Object the object
   --  @param Property the property whose binding is removed

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Delay_Apply_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the Glib.Settings.Gsettings object is in 'delay-apply' mode.
   --  See Glib.Settings.The_Delay for details.

   Has_Unapplied_Property : constant Glib.Properties.Property_Boolean;
   --  If this property is True, the Glib.Settings.Gsettings object has
   --  outstanding changes that will be applied when Glib.Settings.Apply is
   --  called.

   Path_Property : constant Glib.Properties.Property_String;
   --  The path within the backend where the settings are stored.

   Schema_Property : constant Glib.Properties.Property_String;
   --  The name of the schema that describes the types of keys for this
   --  Glib.Settings.Gsettings object.
   --
   --  The type of this property is *not*
   --  Glib.Settings_Schema.Gsettings_Schema.
   --  Glib.Settings_Schema.Gsettings_Schema has only existed since version
   --  2.32 and unfortunately this name was used in previous versions to refer
   --  to the schema ID rather than the schema itself. Take care to use the
   --  'settings-schema' property if you wish to pass in a
   --  Glib.Settings_Schema.Gsettings_Schema.

   Schema_Id_Property : constant Glib.Properties.Property_String;
   --  The name of the schema that describes the types of keys for this
   --  Glib.Settings.Gsettings object.

   Settings_Schema_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Settings_Schema
   --  The Glib.Settings_Schema.Gsettings_Schema describing the types of keys
   --  for this Glib.Settings.Gsettings object.
   --
   --  Ideally, this property would be called 'schema'.
   --  Glib.Settings_Schema.Gsettings_Schema has only existed since version
   --  2.32, however, and before then the 'schema' property was used to refer
   --  to the ID of the schema rather than the schema itself. Take care.

   -------------
   -- Signals --
   -------------

   Signal_Change_Event : constant Glib.Signal_Name := "change-event";
   --  The "change-event" signal is emitted once per change event that affects
   --  this settings object. You should connect to this signal only if you are
   --  interested in viewing groups of changes before they are split out into
   --  multiple emissions of the "changed" signal. For most use cases it is
   --  more appropriate to use the "changed" signal.
   --
   --  In the event that the change event applies to one or more specified
   --  keys, Keys will be an array of Glib.GQuark of length N_Keys. In the
   --  event that the change event applies to the Glib.Settings.Gsettings
   --  object as a whole (ie: potentially every key has been changed) then Keys
   --  will be null and N_Keys will be 0.
   --
   --  The default handler for this signal invokes the "changed" signal for
   --  each affected key. If any other connected handler returns True then this
   --  default functionality will be suppressed.

   --  Callback for this signal:
   --    function Handler
   --       (Self   : access Gsettings_Record'Class;
   --        Keys   : array_of_GLib.Quark;
   --        N_Keys : Glib.Gint) return Boolean
   -- 
   --  Callback parameters:
   --    --  @param Keys an array of GQuarks for the changed keys, or null
   --    --  @param N_Keys the length of the Keys array, or 0

   type Cb_Gsettings_UTF8_String_Void is not null access procedure
     (Self : access Gsettings_Record'Class;
      Key  : UTF8_String);

   type Cb_GObject_UTF8_String_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Key  : UTF8_String);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : not null access Gsettings_Record;
       Call  : Cb_Gsettings_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : not null access Gsettings_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The "changed" signal is emitted when a key has potentially changed. You
   --  should call one of the g_settings_get calls to check the new value.
   --
   --  This signal supports detailed connections. You can connect to the
   --  detailed signal "changed::x" in order to only receive callbacks when key
   --  "x" changes.
   --
   --  Note that Settings only emits this signal if you have read Key at least
   --  once while a signal handler was already connected for Key.

   Signal_Writable_Change_Event : constant Glib.Signal_Name := "writable-change-event";
   --  The "writable-change-event" signal is emitted once per writability
   --  change event that affects this settings object. You should connect to
   --  this signal if you are interested in viewing groups of changes before
   --  they are split out into multiple emissions of the "writable-changed"
   --  signal. For most use cases it is more appropriate to use the
   --  "writable-changed" signal.
   --
   --  In the event that the writability change applies only to a single key,
   --  Key will be set to the Glib.GQuark for that key. In the event that the
   --  writability change affects the entire settings object, Key will be 0.
   --
   --  The default handler for this signal invokes the "writable-changed" and
   --  "changed" signals for each affected key. This is done because changes in
   --  writability might also imply changes in value (if for example, a new
   --  mandatory setting is introduced). If any other connected handler returns
   --  True then this default functionality will be suppressed.
   --    function Handler
   --       (Self : access Gsettings_Record'Class;
   --        Key  : Guint) return Boolean
   -- 
   --  Callback parameters:
   --    --  @param Key the quark of the key, or 0

   Signal_Writable_Changed : constant Glib.Signal_Name := "writable-changed";
   procedure On_Writable_Changed
      (Self  : not null access Gsettings_Record;
       Call  : Cb_Gsettings_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Writable_Changed
      (Self  : not null access Gsettings_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The "writable-changed" signal is emitted when the writability of a key
   --  has potentially changed. You should call Glib.Settings.Is_Writable in
   --  order to determine the new status.
   --
   --  This signal supports detailed connections. You can connect to the
   --  detailed signal "writable-changed::x" in order to only receive callbacks
   --  when the writability of "x" changes.

private
   Settings_Schema_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("settings-schema");
   Schema_Id_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("schema-id");
   Schema_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("schema");
   Path_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("path");
   Has_Unapplied_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-unapplied");
   Delay_Apply_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("delay-apply");
end Glib.Settings;
