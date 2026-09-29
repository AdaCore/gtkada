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

--  A list model that wraps [methodGio.File.enumerate_children_async].
--
--  It presents a `GListModel` and fills it asynchronously with the
--  `GFileInfo`s returned from that function.
--
--  Enumeration will start automatically when the
--  [propertyGtk.DirectoryList:file] property is set.
--
--  While the `GtkDirectoryList` is being filled, the
--  [propertyGtk.DirectoryList:loading] property will be set to True. You can
--  listen to that property if you want to show information like a `GtkSpinner`
--  or a "Loading..." text.
--
--  If loading fails at any point, the [propertyGtk.DirectoryList:error]
--  property will be set to give more indication about the failure.
--
--  The `GFileInfo`s returned from a `GtkDirectoryList` have the
--  "standard::file" attribute set to the `GFile` they refer to. This way you
--  can get at the file that is referred to in the same way you would via
--  g_file_enumerator_get_child. This means you do not need access to the
--  `GtkDirectoryList`, but can access the `GFile` directly from the
--  `GFileInfo` when operating with a `GtkListView` or similar.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Error;      use Glib.Error;
with Glib.GFile;      use Glib.GFile;
with Glib.List_Model; use Glib.List_Model;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;

package Gtk.Directory_List is

   type Gtk_Directory_List_Record is new GObject_Record with null record;
   type Gtk_Directory_List is access all Gtk_Directory_List_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self       : out Gtk_Directory_List;
       Attributes : UTF8_String := "";
       File       : Glib.GFile.Gfile);
   procedure Initialize
      (Self       : not null access Gtk_Directory_List_Record'Class;
       Attributes : UTF8_String := "";
       File       : Glib.GFile.Gfile);
   --  Creates a new `GtkDirectoryList`.
   --  The `GtkDirectoryList` is querying the given File with the given
   --  Attributes.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Attributes The attributes to query with
   --  @param File The file to query

   function Gtk_Directory_List_New
      (Attributes : UTF8_String := "";
       File       : Glib.GFile.Gfile) return Gtk_Directory_List;
   --  Creates a new `GtkDirectoryList`.
   --  The `GtkDirectoryList` is querying the given File with the given
   --  Attributes.
   --  @param Attributes The attributes to query with
   --  @param File The file to query

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_directory_list_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Attributes
      (Self : not null access Gtk_Directory_List_Record) return UTF8_String;
   --  Gets the attributes queried on the children.
   --  @return The queried attributes

   procedure Set_Attributes
      (Self       : not null access Gtk_Directory_List_Record;
       Attributes : UTF8_String := "");
   --  Sets the Attributes to be enumerated and starts the enumeration.
   --  If Attributes is null, the list of file infos will still be created, it
   --  will just not contain any extra attributes.
   --  @param Attributes the attributes to enumerate

   function Get_Error
      (Self : not null access Gtk_Directory_List_Record)
       return Glib.Error.GError;
   --  Gets the loading error, if any.
   --  If an error occurs during the loading process, the loading process will
   --  finish and this property allows querying the error that happened. This
   --  error will persist until a file is loaded again.
   --  An error being set does not mean that no files were loaded, and all
   --  successfully queried files will remain in the list.
   --  @return The loading error or null if loading finished successfully

   function Get_File
      (Self : not null access Gtk_Directory_List_Record)
       return Glib.GFile.Gfile;
   --  Gets the file whose children are currently enumerated.
   --  @return The file whose children are enumerated

   procedure Set_File
      (Self : not null access Gtk_Directory_List_Record;
       File : Glib.GFile.Gfile);
   --  Sets the File to be enumerated and starts the enumeration.
   --  If File is null, the result will be an empty list.
   --  @param File the `GFile` to be enumerated

   function Get_Io_Priority
      (Self : not null access Gtk_Directory_List_Record) return Glib.Gint;
   --  Gets the IO priority set via Gtk.Directory_List.Set_Io_Priority.
   --  @return The IO priority.

   procedure Set_Io_Priority
      (Self        : not null access Gtk_Directory_List_Record;
       Io_Priority : Glib.Gint);
   --  Sets the IO priority to use while loading directories.
   --  Setting the priority while Self is loading will reprioritize the
   --  ongoing load as soon as possible.
   --  The default IO priority is G_PRIORITY_DEFAULT, which is higher than the
   --  GTK redraw priority. If you are loading a lot of directories in
   --  parallel, lowering it to something like G_PRIORITY_DEFAULT_IDLE may
   --  increase responsiveness.
   --  @param Io_Priority IO priority to use

   function Get_Monitored
      (Self : not null access Gtk_Directory_List_Record) return Boolean;
   --  Returns whether the directory list is monitoring the directory for
   --  changes.
   --  @return True if the directory is monitored

   procedure Set_Monitored
      (Self      : not null access Gtk_Directory_List_Record;
       Monitored : Boolean);
   --  Sets whether the directory list will monitor the directory for changes.
   --  If monitoring is enabled, the ::items-changed signal will be emitted
   --  when the directory contents change.
   --  When monitoring is turned on after the initial creation of the
   --  directory list, the directory is reloaded to avoid missing files that
   --  appeared between the initial loading and when monitoring was turned on.
   --  @param Monitored True to monitor the directory for changes

   function Is_Loading
      (Self : not null access Gtk_Directory_List_Record) return Boolean;
   --  Returns True if the children enumeration is currently in progress.
   --  Files will be added to Self from time to time while loading is going
   --  on. The order in which are added is undefined and may change in between
   --  runs.
   --  @return True if Self is loading

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   function Get_Item_Type
      (Self : not null access Gtk_Directory_List_Record) return GType;

   function Get_N_Items
      (Self : not null access Gtk_Directory_List_Record) return Guint;

   function Get_Item
      (Self     : not null access Gtk_Directory_List_Record;
       Position : Guint) return Glib.Object.GObject;

   procedure Items_Changed
      (Self     : not null access Gtk_Directory_List_Record;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Attributes_Property : constant Glib.Properties.Property_String;
   --  The attributes to query.

   Error_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GLib.Error
   --  Error encountered while loading files.

   Io_Priority_Property : constant Glib.Properties.Property_Int;
   --  Priority used when loading.

   Item_Type_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GType
   --  The type of items. See [methodGio.ListModel.get_item_type].

   Loading_Property : constant Glib.Properties.Property_Boolean;
   --  True if files are being loaded.

   Monitored_Property : constant Glib.Properties.Property_Boolean;
   --  True if the directory is monitored for changed.

   N_Items_Property : constant Glib.Properties.Property_Uint;
   --  The number of items. See [methodGio.ListModel.get_n_items].

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gio.ListModel"

   package Implements_Glist_Model is new Glib.Types.Implements
     (Glib.List_Model.Glist_Model, Gtk_Directory_List_Record, Gtk_Directory_List);
   function "+"
     (Widget : access Gtk_Directory_List_Record'Class)
   return Glib.List_Model.Glist_Model
   renames Implements_Glist_Model.To_Interface;
   function "-"
     (Interf : Glib.List_Model.Glist_Model)
   return Gtk_Directory_List
   renames Implements_Glist_Model.To_Object;

private
   N_Items_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n-items");
   Monitored_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("monitored");
   Loading_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("loading");
   Item_Type_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("item-type");
   Io_Priority_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("io-priority");
   Error_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("error");
   Attributes_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("attributes");
end Gtk.Directory_List;
