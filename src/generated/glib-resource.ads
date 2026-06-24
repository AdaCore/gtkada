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

--  Applications and libraries often contain binary or textual data that is
--  really part of the application, rather than user data. For instance
--  Gtk.Builder.Gtk_Builder .ui files, splashscreen images, GMenu markup XML,
--  CSS files, icons, etc. These are often shipped as files in
--  `$datadir/appname`, or manually included as literal strings in the code.
--
--  The Glib.Resource.Gresource API and the
--  [glib-compile-resources][glib-compile-resources] program provide a
--  convenient and efficient alternative to this which has some nice
--  properties. You maintain the files as normal files, so its easy to edit
--  them, but during the build the files are combined into a binary bundle that
--  is linked into the executable. This means that loading the resource files
--  are efficient (as they are already in memory, shared with other instances)
--  and simple (no need to check for things like I/O errors or locate the files
--  in the filesystem). It also makes it easier to create relocatable
--  applications.
--
--  Resource files can also be marked as compressed. Such files will be
--  included in the resource bundle in a compressed form, but will be
--  automatically uncompressed when the resource is used. This is very useful
--  e.g. for larger text files that are parsed once (or rarely) and then thrown
--  away.
--
--  Resource files can also be marked to be preprocessed, by setting the value
--  of the `preprocess` attribute to a comma-separated list of preprocessing
--  options. The only options currently supported are:
--
--  `xml-stripblanks` which will use the xmllint command to strip ignorable
--  whitespace from the XML file. For this to work, the `XMLLINT` environment
--  variable must be set to the full path to the xmllint executable, or xmllint
--  must be in the `PATH`; otherwise the preprocessing step is skipped.
--
--  `to-pixdata` which will use the gdk-pixbuf-pixdata command to convert
--  images to the GdkPixdata format, which allows you to create pixbufs
--  directly using the data inside the resource file, rather than an
--  (uncompressed) copy of it. For this, the gdk-pixbuf-pixdata program must be
--  in the PATH, or the `GDK_PIXBUF_PIXDATA` environment variable must be set
--  to the full path to the gdk-pixbuf-pixdata executable; otherwise the
--  resource compiler will abort.
--
--  `json-stripblanks` which will use the `json-glib-format` command to strip
--  ignorable whitespace from the JSON file. For this to work, the
--  `JSON_GLIB_FORMAT` environment variable must be set to the full path to the
--  `json-glib-format` executable, or it must be in the `PATH`; otherwise the
--  preprocessing step is skipped. In addition, at least version 1.6 of
--  `json-glib-format` is required.
--
--  Resource files will be exported in the GResource namespace using the
--  combination of the given `prefix` and the filename from the `file` element.
--  The `alias` attribute can be used to alter the filename to expose them at a
--  different location in the resource namespace. Typically, this is used to
--  include files from a different source directory without exposing the source
--  directory in the resource namespace, as in the example below.
--
--  Resource bundles are created by the
--  [glib-compile-resources][glib-compile-resources] program which takes an XML
--  file that describes the bundle, and a set of files that the XML references.
--  These are combined into a binary resource bundle.
--
--  An example resource description:
--
--     <?xml version="1.0" encoding="UTF-8"?>
--     <gresources>
--       <gresource prefix="/org/gtk/Example">
--         <file>data/splashscreen.png</file>
--         <file compressed="true">dialog.ui</file>
--         <file preprocess="xml-stripblanks">menumarkup.xml</file>
--         <file alias="example.css">data/example.css</file>
--       </gresource>
--     </gresources>
--  This will create a resource bundle with the following files:
--
--     /org/gtk/Example/data/splashscreen.png
--     /org/gtk/Example/dialog.ui
--     /org/gtk/Example/menumarkup.xml
--     /org/gtk/Example/example.css
--
--
--  Note that all resources in the process share the same namespace, so use
--  Java-style path prefixes (like in the above example) to avoid conflicts.
--
--  You can then use [glib-compile-resources][glib-compile-resources] to
--  compile the XML to a binary bundle that you can load with
--  Glib.Resource.Load. However, its more common to use the --generate-source
--  and --generate-header arguments to create a source file and header to link
--  directly into your application. This will generate `get_resource`,
--  `register_resource` and `unregister_resource` functions, prefixed by the
--  `--c-name` argument passed to
--  [glib-compile-resources][glib-compile-resources]. `get_resource` returns
--  the generated Glib.Resource.Gresource object. The register and unregister
--  functions register the resource so its files can be accessed using
--  g_resources_lookup_data.
--
--  Once a Glib.Resource.Gresource has been created and registered all the
--  data in it can be accessed globally in the process by using API calls like
--  g_resources_open_stream to stream the data or g_resources_lookup_data to
--  get a direct pointer to the data. You can also use URIs like
--  "resource:///org/gtk/Example/data/splashscreen.png" with Gfile.Gfile to
--  access the resource data.
--
--  Some higher-level APIs, such as Gtk.Application.Gtk_Application, will
--  automatically load resources from certain well-known paths in the resource
--  namespace as a convenience. See the documentation for those APIs for
--  details.
--
--  There are two forms of the generated source, the default version uses the
--  compiler support for constructor and destructor functions (where available)
--  to automatically create and register the Glib.Resource.Gresource on startup
--  or library load time. If you pass `--manual-register`, two functions to
--  register/unregister the resource are created instead. This requires an
--  explicit initialization call in your application/library, but it works on
--  all platforms, even on the minor ones where constructors are not supported.
--  (Constructor support is available for at least Win32, Mac OS and Linux.)
--
--  Note that resource data can point directly into the data segment of e.g. a
--  library, so if you are unloading libraries during runtime you need to be
--  very careful with keeping around pointers to data from a resource, as this
--  goes away when the library is unloaded. However, in practice this is not
--  generally a problem, since most resource accesses are for your own
--  resources, and resource data is often used once, during parsing, and then
--  released.
--
--  When debugging a program or testing a change to an installed version, it
--  is often useful to be able to replace resources in the program or library,
--  without recompiling, for debugging or quick hacking and testing purposes.
--  Since GLib 2.50, it is possible to use the `G_RESOURCE_OVERLAYS`
--  environment variable to selectively overlay resources with replacements
--  from the filesystem. It is a G_SEARCHPATH_SEPARATOR-separated list of
--  substitutions to perform during resource lookups.
--
--  A substitution has the form
--
--     /org/gtk/libgtk=/home/desrt/gtk-overlay
--
--
--  The part before the `=` is the resource subpath for which the overlay
--  applies. The part after is a filesystem path which contains files and
--  subdirectories as you would like to be loaded as resources with the
--  equivalent names.
--
--  In the example above, if an application tried to load a resource with the
--  resource path `/org/gtk/libgtk/ui/gtkdialog.ui` then GResource would check
--  the filesystem path `/home/desrt/gtk-overlay/ui/gtkdialog.ui`. If a file
--  was found there, it would be used instead. This is an overlay, not an
--  outright replacement, which means that if a file is not found at that path,
--  the built-in version will be used instead. Whiteouts are not currently
--  supported.
--
--  Substitutions must start with a slash, and must not contain a trailing
--  slash before the '='. The path after the slash should ideally be absolute,
--  but this is not strictly required. It is possible to overlay the location
--  of a single resource with an individual file.

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;            use GNAT.Strings;
with Glib.Bytes;              use Glib.Bytes;
with Glib.Generic_Properties; use Glib.Generic_Properties;

package Glib.Resource is

   type Gresource is new Glib.C_Boxed with null record;
   Null_Gresource : constant Gresource;

   function From_Object (Object : System.Address) return Gresource;
   function From_Object_Free (B : access Gresource'Class) return Gresource;
   pragma Inline (From_Object_Free, From_Object);

   type Resource_Lookup_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Resource_Lookup_Flags);
   --  GResourceLookupFlags determine how resource path lookups are handled.

   G_Resource_Lookup_Flags_None : constant Resource_Lookup_Flags := 0;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Resource_Lookup_Flags_Properties is
      new Generic_Internal_Discrete_Property (Resource_Lookup_Flags);
   type Property_Resource_Lookup_Flags is new Resource_Lookup_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure G_New_From_Data
      (Self : out Gresource;
       Data : Glib.Bytes.Gbytes);
   --  Creates a GResource from a reference to the binary resource bundle.
   --  This will keep a reference to Data while the resource lives, so the data
   --  should not be modified or freed.
   --  If you want to use this resource in the global resource namespace you
   --  need to register it with Glib.Resource.Register.
   --  Note: Data must be backed by memory that is at least pointer aligned.
   --  Otherwise this function will internally create a copy of the memory
   --  since GLib 2.56, or in older versions fail and exit the process.
   --  If Data is empty or corrupt, G_RESOURCE_ERROR_INTERNAL will be
   --  returned.
   --  Since: gtk+ 2.32
   --  @param Data A Glib.Bytes.Gbytes

   function Gresource_New_From_Data
      (Data : Glib.Bytes.Gbytes) return Gresource;
   --  Creates a GResource from a reference to the binary resource bundle.
   --  This will keep a reference to Data while the resource lives, so the data
   --  should not be modified or freed.
   --  If you want to use this resource in the global resource namespace you
   --  need to register it with Glib.Resource.Register.
   --  Note: Data must be backed by memory that is at least pointer aligned.
   --  Otherwise this function will internally create a copy of the memory
   --  since GLib 2.56, or in older versions fail and exit the process.
   --  If Data is empty or corrupt, G_RESOURCE_ERROR_INTERNAL will be
   --  returned.
   --  Since: gtk+ 2.32
   --  @param Data A Glib.Bytes.Gbytes

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_resource_get_type");

   -------------
   -- Methods --
   -------------

   procedure Register (Self : Gresource);
   --  Registers the resource with the process-global set of resources. Once a
   --  resource is registered the files in it can be accessed with the global
   --  resource lookup functions like g_resources_lookup_data.
   --  Since: gtk+ 2.32

   procedure Unregister (Self : Gresource);
   --  Unregisters the resource from the process-global set of resources.
   --  Since: gtk+ 2.32

   function Enumerate_Children
      (Self         : Gresource;
       Path         : UTF8_String;
       Lookup_Flags : Resource_Lookup_Flags) return GNAT.Strings.String_List;
   --  Returns all the names of children at the specified Path in the
   --  resource. The return result is a null terminated list of strings which
   --  should be released with g_strfreev.
   --  If Path is invalid or does not exist in the Glib.Resource.Gresource,
   --  G_RESOURCE_ERROR_NOT_FOUND will be returned.
   --  Lookup_Flags controls the behaviour of the lookup.
   --  Since: gtk+ 2.32
   --  @param Path A pathname inside the resource
   --  @param Lookup_Flags A Glib.Resource.Resource_Lookup_Flags
   --  @return an array of constant strings

   function Get_Info
      (Self         : Gresource;
       Path         : UTF8_String;
       Lookup_Flags : Resource_Lookup_Flags;
       Size         : access Gsize;
       Flags        : access Guint32) return Boolean;
   --  Looks for a file at the specified Path in the resource and if found
   --  returns information about it.
   --  Lookup_Flags controls the behaviour of the lookup.
   --  Since: gtk+ 2.32
   --  @param Path A pathname inside the resource
   --  @param Lookup_Flags A Glib.Resource.Resource_Lookup_Flags
   --  @param Size a location to place the length of the contents of the file,
   --  or null if the length is not needed
   --  @param Flags a location to place the flags about the file, or null if
   --  the length is not needed
   --  @return True if the file was found. False if there were errors

   function Lookup_Data
      (Self         : Gresource;
       Path         : UTF8_String;
       Lookup_Flags : Resource_Lookup_Flags) return Glib.Bytes.Gbytes;
   --  Looks for a file at the specified Path in the resource and returns a
   --  Glib.Bytes.Gbytes that lets you directly access the data in memory.
   --  The data is always followed by a zero byte, so you can safely use the
   --  data as a C string. However, that byte is not included in the size of
   --  the GBytes.
   --  For uncompressed resource files this is a pointer directly into the
   --  resource bundle, which is typically in some readonly data section in the
   --  program binary. For compressed files we allocate memory on the heap and
   --  automatically uncompress the data.
   --  Lookup_Flags controls the behaviour of the lookup.
   --  Since: gtk+ 2.32
   --  @param Path A pathname inside the resource
   --  @param Lookup_Flags A Glib.Resource.Resource_Lookup_Flags
   --  @return Glib.Bytes.Gbytes or null on error. Free the returned object
   --  with Glib.Bytes.Unref

   function Ref (Self : Gresource) return Gresource;
   --  Atomically increments the reference count of Resource by one. This
   --  function is MT-safe and may be called from any thread.
   --  Since: gtk+ 2.32
   --  @return The passed in Glib.Resource.Gresource

   procedure Unref (Self : Gresource);
   --  Atomically decrements the reference count of Resource by one. If the
   --  reference count drops to 0, all memory allocated by the resource is
   --  released. This function is MT-safe and may be called from any thread.
   --  Since: gtk+ 2.32

   ---------------
   -- Functions --
   ---------------

   function Load (Filename : UTF8_String) return Gresource;
   --  Loads a binary resource bundle and creates a Glib.Resource.Gresource
   --  representation of it, allowing you to query it for data.
   --  If you want to use this resource in the global resource namespace you
   --  need to register it with Glib.Resource.Register.
   --  If Filename is empty or the data in it is corrupt,
   --  G_RESOURCE_ERROR_INTERNAL will be returned. If Filename doesn't exist,
   --  or there is an error in reading it, an error from g_mapped_file_new will
   --  be returned.
   --  Since: gtk+ 2.32
   --  @param Filename the path of a filename to load, in the GLib filename
   --  encoding
   --  @return a new Glib.Resource.Gresource, or null on error

private
   Null_Gresource : constant Gresource :=
      (Glib.C_Boxed with null record);

end Glib.Resource;
