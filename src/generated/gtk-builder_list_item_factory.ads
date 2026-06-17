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

--  Creates widgets by instantiating `GtkBuilder` UI templates.
--
--  The templates must extend the class that the parent widget expects. For
--  example, a factory provided to [propertyGtk.ListView:factory] must have a
--  template that extends [classGtk.ListItem].
--
--  Templates typically use [classGtk.Expression] to obtain data from the
--  items in the model.
--
--  Example: ```xml <interface> <template class="GtkListItem"> <property
--  name="child"> <object class="GtkLabel"> <property
--  name="xalign">0</property> <binding name="label"> <lookup name="name"
--  type="SettingsKey"> <lookup name="item">GtkListItem</lookup> </lookup>
--  </binding> </object> </property> </template> </interface> ```
--
--  A common approach is to embed such templates as CDATA marked sections into
--  a surrounding UI file. Note that if you use this approach, extracting
--  translatable strings with xgettext will not work for strings inside the
--  marked section.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Bytes;            use Glib.Bytes;
with Glib.Properties;       use Glib.Properties;
with Gtk.Builder_Scope;     use Gtk.Builder_Scope;
with Gtk.List_Item_Factory; use Gtk.List_Item_Factory;

package Gtk.Builder_List_Item_Factory is

   type Gtk_Builder_List_Item_Factory_Record is new Gtk_List_Item_Factory_Record with null record;
   type Gtk_Builder_List_Item_Factory is access all Gtk_Builder_List_Item_Factory_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New_From_Bytes
      (Self  : out Gtk_Builder_List_Item_Factory;
       Scope : Gtk.Builder_Scope.Gtk_Builder_Scope;
       Bytes : Glib.Bytes.Gbytes);
   procedure Initialize_From_Bytes
      (Self  : not null access Gtk_Builder_List_Item_Factory_Record'Class;
       Scope : Gtk.Builder_Scope.Gtk_Builder_Scope;
       Bytes : Glib.Bytes.Gbytes);
   --  Creates a new `GtkBuilderListItemFactory` that instantiates widgets
   --  using Bytes as the data to pass to `GtkBuilder`.
   --  Initialize_From_Bytes does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Scope A scope to use when instantiating
   --  @param Bytes the `GBytes` containing the UI definition to instantiate

   function Gtk_Builder_List_Item_Factory_New_From_Bytes
      (Scope : Gtk.Builder_Scope.Gtk_Builder_Scope;
       Bytes : Glib.Bytes.Gbytes) return Gtk_Builder_List_Item_Factory;
   --  Creates a new `GtkBuilderListItemFactory` that instantiates widgets
   --  using Bytes as the data to pass to `GtkBuilder`.
   --  @param Scope A scope to use when instantiating
   --  @param Bytes the `GBytes` containing the UI definition to instantiate

   procedure Gtk_New_From_Resource
      (Self          : out Gtk_Builder_List_Item_Factory;
       Scope         : Gtk.Builder_Scope.Gtk_Builder_Scope;
       Resource_Path : UTF8_String);
   procedure Initialize_From_Resource
      (Self          : not null access Gtk_Builder_List_Item_Factory_Record'Class;
       Scope         : Gtk.Builder_Scope.Gtk_Builder_Scope;
       Resource_Path : UTF8_String);
   --  Creates a new `GtkBuilderListItemFactory` that instantiates widgets
   --  using data read from the given Resource_Path to pass to `GtkBuilder`.
   --  Initialize_From_Resource does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Scope A scope to use when instantiating
   --  @param Resource_Path valid path to a resource that contains the UI
   --  definition

   function Gtk_Builder_List_Item_Factory_New_From_Resource
      (Scope         : Gtk.Builder_Scope.Gtk_Builder_Scope;
       Resource_Path : UTF8_String) return Gtk_Builder_List_Item_Factory;
   --  Creates a new `GtkBuilderListItemFactory` that instantiates widgets
   --  using data read from the given Resource_Path to pass to `GtkBuilder`.
   --  @param Scope A scope to use when instantiating
   --  @param Resource_Path valid path to a resource that contains the UI
   --  definition

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_builder_list_item_factory_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Bytes
      (Self : not null access Gtk_Builder_List_Item_Factory_Record)
       return Glib.Bytes.Gbytes;
   --  Gets the data used as the `GtkBuilder` UI template for constructing
   --  listitems.
   --  @return The `GtkBuilder` data

   function Get_Resource
      (Self : not null access Gtk_Builder_List_Item_Factory_Record)
       return UTF8_String;
   --  If the data references a resource, gets the path of that resource.
   --  @return The path to the resource

   function Get_Scope
      (Self : not null access Gtk_Builder_List_Item_Factory_Record)
       return Gtk.Builder_Scope.Gtk_Builder_Scope;
   --  Gets the scope used when constructing listitems.
   --  @return The scope used when constructing listitems

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Bytes_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GLib.Bytes
   --  `GBytes` containing the UI definition.

   Resource_Property : constant Glib.Properties.Property_String;
   --  Path of the resource containing the UI definition.

   Scope_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Builder_Scope.Gtk_Builder_Scope
   --  `GtkBuilderScope` to use when instantiating listitems

private
   Scope_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("scope");
   Resource_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("resource");
   Bytes_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("bytes");
end Gtk.Builder_List_Item_Factory;
