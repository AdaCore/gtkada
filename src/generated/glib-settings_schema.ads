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

--  The Glib.Settings_Schema_Source.Gsettings_Schema_Source and
--  Glib.Settings_Schema.Gsettings_Schema APIs provide a mechanism for advanced
--  control over the loading of schemas and a mechanism for introspecting their
--  content.
--
--  Plugin loading systems that wish to provide plugins a way to access
--  settings face the problem of how to make the schemas for these settings
--  visible to GSettings. Typically, a plugin will want to ship the schema
--  along with itself and it won't be installed into the standard system
--  directories for schemas.
--
--  Glib.Settings_Schema_Source.Gsettings_Schema_Source provides a mechanism
--  for dealing with this by allowing the creation of a new 'schema source'
--  from which schemas can be acquired. This schema source can then become part
--  of the metadata associated with the plugin and queried whenever the plugin
--  requires access to some settings.
--
--  Consider the following example:
--
--     typedef struct
--     {
--        ...
--        GSettingsSchemaSource *schema_source;
--        ...
--     } Plugin;
--
--     Plugin *
--     initialise_plugin (const gchar *dir)
--     {
--       Plugin *plugin;
--
--       ...
--
--       plugin->schema_source =
--         g_settings_schema_source_new_from_directory (dir,
--           g_settings_schema_source_get_default (), FALSE, NULL);
--
--       ...
--
--       return plugin;
--     }
--
--     ...
--
--     GSettings *
--     plugin_get_settings (Plugin      *plugin,
--                          const gchar *schema_id)
--     {
--       GSettingsSchema *schema;
--
--       if (schema_id == NULL)
--         schema_id = plugin->identifier;
--
--       schema = g_settings_schema_source_lookup (plugin->schema_source,
--                                                 schema_id, FALSE);
--
--       if (schema == NULL)
--         {
--           ... disable the plugin or abort, etc ...
--         }
--
--       return g_settings_new_full (schema, NULL, NULL);
--     }
--
--
--  The code above shows how hooks should be added to the code that
--  initialises (or enables) the plugin to create the schema source and how an
--  API can be added to the plugin system to provide a convenient way for the
--  plugin to access its settings, using the schemas that it ships.
--
--  From the standpoint of the plugin, it would need to ensure that it ships a
--  gschemas.compiled file as part of itself, and then simply do the following:
--
--     {
--       GSettings *settings;
--       gint some_value;
--
--       settings = plugin_get_settings (self, NULL);
--       some_value = g_settings_get_int (settings, "some-value");
--       ...
--     }
--
--
--  It's also possible that the plugin system expects the schema source files
--  (ie: .gschema.xml files) instead of a gschemas.compiled file. In that case,
--  the plugin loading system must compile the schemas for itself before
--  attempting to create the settings source.
--
--  <group>GIO</group>
--  <gtkada_demo>create_settings.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;             use GNAT.Strings;
with Glib.Settings_Schema_Key; use Glib.Settings_Schema_Key;

package Glib.Settings_Schema is

   type Gsettings_Schema is new Glib.C_Boxed with null record;
   Null_Gsettings_Schema : constant Gsettings_Schema;

   function From_Object (Object : System.Address) return Gsettings_Schema;
   function From_Object_Free (B : access Gsettings_Schema'Class) return Gsettings_Schema;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_settings_schema_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Id (Self : Gsettings_Schema) return UTF8_String;
   --  Get the ID of Schema.
   --  @return the ID

   function Get_Key
      (Self : Gsettings_Schema;
       Name : UTF8_String)
       return Glib.Settings_Schema_Key.Gsettings_Schema_Key;
   --  Gets the key named Name from Schema.
   --  It is a programmer error to request a key that does not exist. See
   --  Glib.Settings_Schema.List_Keys.
   --  Since: gtk+ 2.40
   --  @param Name the name of a key
   --  @return the Glib.Settings_Schema_Key.Gsettings_Schema_Key for Name. Has
   --  transfer-ownership='full'.

   function Get_Path (Self : Gsettings_Schema) return UTF8_String;
   --  Gets the path associated with Schema, or null.
   --  Schemas may be single-instance or relocatable. Single-instance schemas
   --  correspond to exactly one set of keys in the backend database: those
   --  located at the path returned by this function.
   --  Relocatable schemas can be referenced by other schemas and can
   --  therefore describe multiple sets of keys at different locations. For
   --  relocatable schemas, this function will return null.
   --  Since: gtk+ 2.32
   --  @return the path of the schema, or null

   function Has_Key
      (Self : Gsettings_Schema;
       Name : UTF8_String) return Boolean;
   --  Checks if Schema has a key named Name.
   --  Since: gtk+ 2.40
   --  @param Name the name of a key
   --  @return True if such a key exists

   function List_Children
      (Self : Gsettings_Schema) return GNAT.Strings.String_List;
   --  Gets the list of children in Schema.
   --  You should free the return value with g_strfreev when you are done with
   --  it.
   --  Since: gtk+ 2.44
   --  @return a list of the children on Settings, in no defined order

   function List_Keys
      (Self : Gsettings_Schema) return GNAT.Strings.String_List;
   --  Introspects the list of keys on Schema.
   --  You should probably not be calling this function from "normal" code
   --  (since you should already know what keys are in your schema). This
   --  function is intended for introspection reasons.
   --  Since: gtk+ 2.46
   --  @return a list of the keys on Schema, in no defined order

   function Ref (Self : Gsettings_Schema) return Gsettings_Schema;
   --  Increase the reference count of Schema, returning a new reference.
   --  Since: gtk+ 2.32
   --  @return a new reference to Schema. Has transfer-ownership='full'.

   procedure Unref (Self : Gsettings_Schema);
   --  Decrease the reference count of Schema, possibly freeing it.
   --  Since: gtk+ 2.32

private
   Null_Gsettings_Schema : constant Gsettings_Schema :=
      (Glib.C_Boxed with null record);

end Glib.Settings_Schema;
