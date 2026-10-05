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

--  This is an opaque structure type. You may not access it directly.
--
--  <group>GIO</group>
--  <gtkada_demo>create_settings.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;         use GNAT.Strings;
with Glib.Error;           use Glib.Error;
with Glib.Settings_Schema; use Glib.Settings_Schema;

package Glib.Settings_Schema_Source is

   type Gsettings_Schema_Source is new Glib.C_Boxed with null record;
   Null_Gsettings_Schema_Source : constant Gsettings_Schema_Source;

   function From_Object (Object : System.Address) return Gsettings_Schema_Source;
   function From_Object_Free (B : access Gsettings_Schema_Source'Class) return Gsettings_Schema_Source;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   procedure G_New_From_Directory
      (Self      : out Gsettings_Schema_Source;
       Directory : UTF8_String;
       Parent    : Gsettings_Schema_Source;
       Trusted   : Boolean;
       Error     : out Glib.Error.GError);
   --  Attempts to create a new schema source corresponding to the contents of
   --  the given directory.
   --  This function is not required for normal uses of
   --  Glib.Settings.Gsettings but it may be useful to authors of plugin
   --  management systems.
   --  The directory should contain a file called `gschemas.compiled` as
   --  produced by the [glib-compile-schemas][glib-compile-schemas] tool.
   --  If Trusted is True then `gschemas.compiled` is trusted not to be
   --  corrupted. This assumption has a performance advantage, but can result
   --  in crashes or inconsistent behaviour in the case of a corrupted file.
   --  Generally, you should set Trusted to True for files installed by the
   --  system and to False for files in the home directory.
   --  In either case, an empty file or some types of corruption in the file
   --  will result in Glib.Error_Enums.G_File_Error_Inval being returned.
   --  If Parent is non-null then there are two effects.
   --  First, if Glib.Settings_Schema_Source.Lookup is called with the
   --  Recursive flag set to True and the schema can not be found in the
   --  source, the lookup will recurse to the parent.
   --  Second, any references to other schemas specified within this source
   --  (ie: `child` or `extends`) references may be resolved from the Parent.
   --  For this second reason, except in very unusual situations, the Parent
   --  should probably be given as the default schema source, as returned by
   --  Glib.Settings_Schema_Source.Get_Default.
   --  Since: gtk+ 2.32
   --  @param Directory the filename of a directory
   --  @param Parent a Glib.Settings_Schema_Source.Gsettings_Schema_Source, or
   --  null
   --  @param Trusted True, if the directory is trusted
   --  @param Error the return location for a recoverable error

   function Gsettings_Schema_Source_New_From_Directory
      (Directory : UTF8_String;
       Parent    : Gsettings_Schema_Source;
       Trusted   : Boolean;
       Error     : out Glib.Error.GError) return Gsettings_Schema_Source;
   --  Attempts to create a new schema source corresponding to the contents of
   --  the given directory.
   --  This function is not required for normal uses of
   --  Glib.Settings.Gsettings but it may be useful to authors of plugin
   --  management systems.
   --  The directory should contain a file called `gschemas.compiled` as
   --  produced by the [glib-compile-schemas][glib-compile-schemas] tool.
   --  If Trusted is True then `gschemas.compiled` is trusted not to be
   --  corrupted. This assumption has a performance advantage, but can result
   --  in crashes or inconsistent behaviour in the case of a corrupted file.
   --  Generally, you should set Trusted to True for files installed by the
   --  system and to False for files in the home directory.
   --  In either case, an empty file or some types of corruption in the file
   --  will result in Glib.Error_Enums.G_File_Error_Inval being returned.
   --  If Parent is non-null then there are two effects.
   --  First, if Glib.Settings_Schema_Source.Lookup is called with the
   --  Recursive flag set to True and the schema can not be found in the
   --  source, the lookup will recurse to the parent.
   --  Second, any references to other schemas specified within this source
   --  (ie: `child` or `extends`) references may be resolved from the Parent.
   --  For this second reason, except in very unusual situations, the Parent
   --  should probably be given as the default schema source, as returned by
   --  Glib.Settings_Schema_Source.Get_Default.
   --  Since: gtk+ 2.32
   --  @param Directory the filename of a directory
   --  @param Parent a Glib.Settings_Schema_Source.Gsettings_Schema_Source, or
   --  null
   --  @param Trusted True, if the directory is trusted
   --  @param Error the return location for a recoverable error

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_settings_schema_source_get_type");

   -------------
   -- Methods --
   -------------

   procedure List_Schemas
      (Self            : Gsettings_Schema_Source;
       Recursive       : Boolean;
       Non_Relocatable : out GNAT.Strings.String_List_Access;
       Relocatable     : out GNAT.Strings.String_List_Access);
   --  Lists the schemas in a given source.
   --  If Recursive is True then include parent sources. If False then only
   --  include the schemas from one source (ie: one directory). You probably
   --  want True.
   --  Non-relocatable schemas are those for which you can call
   --  Glib.Settings.G_New. Relocatable schemas are those for which you must
   --  use Glib.Settings.G_New_With_Path.
   --  Do not call this function from normal programs. This is designed for
   --  use by database editors, commandline tools, etc.
   --  Since: gtk+ 2.40
   --  @param Recursive if we should recurse
   --  @param Non_Relocatable the list of non-relocatable schemas, in no
   --  defined order
   --  @param Relocatable the list of relocatable schemas, in no defined order

   function Lookup
      (Self      : Gsettings_Schema_Source;
       Schema_Id : UTF8_String;
       Recursive : Boolean) return Glib.Settings_Schema.Gsettings_Schema;
   --  Looks up a schema with the identifier Schema_Id in Source.
   --  This function is not required for normal uses of
   --  Glib.Settings.Gsettings but it may be useful to authors of plugin
   --  management systems or to those who want to introspect the content of
   --  schemas.
   --  If the schema isn't found directly in Source and Recursive is True then
   --  the parent sources will also be checked.
   --  If the schema isn't found, null is returned.
   --  Since: gtk+ 2.32
   --  @param Schema_Id a schema ID
   --  @param Recursive True if the lookup should be recursive
   --  @return a new Glib.Settings_Schema.Gsettings_Schema. Has
   --  transfer-ownership='full'.

   function Ref
      (Self : Gsettings_Schema_Source) return Gsettings_Schema_Source;
   --  Increase the reference count of Source, returning a new reference.
   --  Since: gtk+ 2.32
   --  @return a new reference to Source. Has transfer-ownership='full'.

   procedure Unref (Self : Gsettings_Schema_Source);
   --  Decrease the reference count of Source, possibly freeing it.
   --  Since: gtk+ 2.32

   ---------------
   -- Functions --
   ---------------

   function Get_Default return Gsettings_Schema_Source;
   --  Gets the default system schema source.
   --  This function is not required for normal uses of
   --  Glib.Settings.Gsettings but it may be useful to authors of plugin
   --  management systems or to those who want to introspect the content of
   --  schemas.
   --  If no schemas are installed, null will be returned.
   --  The returned source may actually consist of multiple schema sources
   --  from different directories, depending on which directories were given in
   --  `XDG_DATA_DIRS` and `GSETTINGS_SCHEMA_DIR`. For this reason, all lookups
   --  performed against the default source should probably be done
   --  recursively.
   --  Since: gtk+ 2.32
   --  @return the default schema source. Has transfer-ownership='none'.

private
   Null_Gsettings_Schema_Source : constant Gsettings_Schema_Source :=
      (Glib.C_Boxed with null record);

end Glib.Settings_Schema_Source;
