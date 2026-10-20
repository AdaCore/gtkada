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

--  Manages and looks up recently used files.
--
--  Each recently used file is identified by its URI, and has meta-data
--  associated to it, like the names and command lines of the applications that
--  have registered it, the number of time each application has registered the
--  same file, the mime type of the file and whether the file should be
--  displayed only by the applications that have registered it.
--
--  The recently used files list is per user.
--
--  `GtkRecentManager` acts like a database of all the recently used files.
--  You can create new `GtkRecentManager` objects, but it is more efficient to
--  use the default manager created by GTK.
--
--  Adding a new recently used file is as simple as:
--
--  ```c GtkRecentManager *manager;
--
--  manager = gtk_recent_manager_get_default (); gtk_recent_manager_add_item
--  (manager, file_uri); ```
--
--  The `GtkRecentManager` will try to gather all the needed information from
--  the file itself through GIO.
--
--  Looking up the meta-data associated with a recently used file given its
--  URI requires calling [methodGtk.RecentManager.lookup_item]:
--
--  ```c GtkRecentManager *manager; GtkRecentInfo *info; GError *error = NULL;
--
--  manager = gtk_recent_manager_get_default (); info =
--  gtk_recent_manager_lookup_item (manager, file_uri, &error); if (error) {
--  g_warning ("Could not find the file: %s", error->message); g_error_free
--  (error); } else { // Use the info object gtk_recent_info_unref (info); }
--  ```
--
--  In order to retrieve the list of recently used files, you can use
--  [methodGtk.RecentManager.get_items], which returns a list of
--  [structGtk.RecentInfo].
--
--  Note that the maximum age of the recently used files list is controllable
--  through the [propertyGtk.Settings:gtk-recent-files-max-age] property.
--
--  <group>Recent Files</group>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;            use GNAT.Strings;
with Glib;                    use Glib;
with Glib.Error;              use Glib.Error;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Glist;              use Glib.Glist;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Gtk.Recent_Info;         use Gtk.Recent_Info;

package Gtk.Recent_Manager is

   type Gtk_Recent_Manager_Record is new GObject_Record with null record;
   type Gtk_Recent_Manager is access all Gtk_Recent_Manager_Record'Class;

   type Gtk_Recent_Manager_Error is (
      Recent_Manager_Error_Not_Found,
      Recent_Manager_Error_Invalid_Uri,
      Recent_Manager_Error_Invalid_Encoding,
      Recent_Manager_Error_Not_Registered,
      Recent_Manager_Error_Read,
      Recent_Manager_Error_Write,
      Recent_Manager_Error_Unknown);
   pragma Convention (C, Gtk_Recent_Manager_Error);
   --  Error codes for `GtkRecentManager` operations

   Gtk_Recent_Manager_Error_Name   : constant UTF8_String := "gtk-recent-manager-error-quark";
   Gtk_Recent_Manager_Error_Domain : constant GQuark := Quark_From_String (Gtk_Recent_Manager_Error_Name);
   --  Used to identify error domain in a GError

   function Gtk_Recent_Manager_Error_Matches
     (Error : GError; Code : Gtk_Recent_Manager_Error) return Boolean
   is (Error_Matches
        (Error,
         Gtk_Recent_Manager_Error_Domain,
         Glib.Gint (Gtk_Recent_Manager_Error'Pos (Code))));
   --  Convenience helper to match error codes

   function Convert (R : Gtk.Recent_Info.Gtk_Recent_Info) return System.Address;
   function Convert (R : System.Address) return Gtk.Recent_Info.Gtk_Recent_Info;
   package Gtk_Recent_Info_List is new Generic_List (Gtk.Recent_Info.Gtk_Recent_Info);

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Recent_Manager_Error_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Recent_Manager_Error);
   type Property_Gtk_Recent_Manager_Error is new Gtk_Recent_Manager_Error_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Recent_Manager);
   procedure Initialize
      (Self : not null access Gtk_Recent_Manager_Record'Class);
   --  Creates a new recent manager object.
   --  Recent manager objects are used to handle the list of recently used
   --  resources. A `GtkRecentManager` object monitors the recently used
   --  resources list, and emits the [signalGtk.RecentManager::changed] signal
   --  each time something inside the list changes.
   --  `GtkRecentManager` objects are expensive: be sure to create them only
   --  when needed. You should use [funcGtk.RecentManager.get_default] instead.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Recent_Manager_New return Gtk_Recent_Manager;
   --  Creates a new recent manager object.
   --  Recent manager objects are used to handle the list of recently used
   --  resources. A `GtkRecentManager` object monitors the recently used
   --  resources list, and emits the [signalGtk.RecentManager::changed] signal
   --  each time something inside the list changes.
   --  `GtkRecentManager` objects are expensive: be sure to create them only
   --  when needed. You should use [funcGtk.RecentManager.get_default] instead.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_recent_manager_get_type");

   -------------
   -- Methods --
   -------------

   function Add_Item
      (Self : not null access Gtk_Recent_Manager_Record;
       URI  : UTF8_String) return Boolean;
   --  Adds a new resource, pointed by Uri, into the recently used resources
   --  list.
   --  This function automatically retrieves some of the needed metadata and
   --  setting other metadata to common default values; it then feeds the data
   --  to [methodGtk.RecentManager.add_full].
   --  See [methodGtk.RecentManager.add_full] if you want to explicitly define
   --  the metadata for the resource pointed by Uri.
   --  @param URI a valid URI
   --  @return True if the new item was successfully added to the recently
   --  used resources list

   function Get_Items
      (Self : not null access Gtk_Recent_Manager_Record)
       return Gtk_Recent_Info_List.Glist;
   --  Gets the list of recently used resources.
   --  @return a list of newly allocated `GtkRecentInfo objects`. Use
   --  [methodGtk.RecentInfo.unref] on each item inside the list, and then free
   --  the list itself using g_list_free.

   function Has_Item
      (Self : not null access Gtk_Recent_Manager_Record;
       URI  : UTF8_String) return Boolean;
   --  Checks whether there is a recently used resource registered with Uri
   --  inside the recent manager.
   --  @param URI a URI
   --  @return True if the resource was found, False otherwise

   function Lookup_Item
      (Self  : not null access Gtk_Recent_Manager_Record;
       URI   : UTF8_String;
       Error : out Glib.Error.GError) return Gtk.Recent_Info.Gtk_Recent_Info;
   --  Searches for a URI inside the recently used resources list, and returns
   --  a `GtkRecentInfo` containing information about the resource like its
   --  MIME type, or its display name.
   --  @param URI a URI
   --  @param Error the return location for a recoverable error
   --  @return a `GtkRecentInfo` containing information about the resource
   --  pointed by Uri, or null if the URI was not registered in the recently
   --  used resources list. Free with [methodGtk.RecentInfo.unref]. Has
   --  transfer-ownership='full'.

   function Move_Item
      (Self    : not null access Gtk_Recent_Manager_Record;
       URI     : UTF8_String;
       New_Uri : UTF8_String := "";
       Error   : out Glib.Error.GError) return Boolean;
   --  Changes the location of a recently used resource from Uri to New_Uri.
   --  Please note that this function will not affect the resource pointed by
   --  the URIs, but only the URI used in the recently used resources list.
   --  @param URI the URI of a recently used resource
   --  @param New_Uri the new URI of the recently used resource, or null to
   --  remove the item pointed by Uri in the list
   --  @param Error the return location for a recoverable error
   --  @return True on success

   function Purge_Items
      (Self  : not null access Gtk_Recent_Manager_Record;
       Error : out Glib.Error.GError) return Glib.Gint;
   --  Purges every item from the recently used resources list.
   --  @param Error the return location for a recoverable error
   --  @return the number of items that have been removed from the recently
   --  used resources list

   function Remove_Item
      (Self  : not null access Gtk_Recent_Manager_Record;
       URI   : UTF8_String;
       Error : out Glib.Error.GError) return Boolean;
   --  Removes a resource pointed by Uri from the recently used resources list
   --  handled by a recent manager.
   --  @param URI the URI of the item you wish to remove
   --  @param Error the return location for a recoverable error
   --  @return True if the item pointed by Uri has been successfully removed
   --  by the recently used resources list, and False otherwise

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Add_Full
     (Manager      : access Gtk_Recent_Manager_Record;
      Uri          : UTF8_String;
      Display_Name : UTF8_String := "";
      Description  : UTF8_String := "";
      Mime_Type    : UTF8_String;
      App_Name     : UTF8_String;
      App_Exec     : UTF8_String;
      Groups       : GNAT.Strings.String_List;
      Is_Private   : Boolean)
   return Boolean;
   --  Manager      : the Gtk_Recent_Manager on which to operate
   --  Uri          : pointer to resource
   --  Display_Name : a UTF-8 encoded string, containing the name of the
   --                 recently used resource to be displayed, or "".
   --  Description  : a UTF-8 encoded string, containing a short description
   --                 of the resource, or "".
   --  Mime_Type    : the MIME type of the resource.
   --  App_Name     : the name of the application that is registering this
   --                 recently used resource.
   --  App_Exec     : command line used to launch this resource; may contain
   --                 the "%f" and "%u" escape characters which will be
   --                 expanded to the resource file path and URI, respectively,
   --                 when the command line is retrieved.
   --  Groups       : a vector of strings containing groups names.
   --  Is_Private   : whether this resource should be displayed only by the
   --                 applications that have registered it or not.
   --
   --  Adds a new resource, pointed by Uri, into the recently used
   --  resources list, using the metadata specified.
   --
   --  The passed URI will be used to identify this resource inside the
   --  list.
   --
   --  In order to register the new recently used resource, metadata about
   --  the resource must be passed as well as the URI.  This metadata must
   --  contain the MIME type of the resource pointed by the URI; the name of
   --  the application that is registering the item, and a command line to be
   --  used when launching the item.
   --
   --  Optionally, it is possible to specify a UTF-8 string to be used when
   --  viewing the item instead of the last component of the URI; a short
   --  description of the item; whether the item should be considered private -
   --  that is, should be displayed only by the applications that have
   --  registered it.
   --
   --  Returns True if the new item was successfully added to the recently
   --  used resources list, False otherwise.

   ---------------
   -- Functions --
   ---------------

   function Get_Default return Gtk_Recent_Manager;
   --  Gets a unique instance of `GtkRecentManager` that you can share in your
   --  application without caring about memory management.
   --  @return A unique `GtkRecentManager`. Do not ref or unref it. Has
   --  transfer-ownership='none'.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Filename_Property : constant Glib.Properties.Property_String;
   --  The full path to the file to be used to store and read the recently
   --  used resources list

   Size_Property : constant Glib.Properties.Property_Int;
   --  The size of the recently used resources list.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Recent_Manager_Void is not null access procedure
     (Self : access Gtk_Recent_Manager_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : not null access Gtk_Recent_Manager_Record;
       Call  : Cb_Gtk_Recent_Manager_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : not null access Gtk_Recent_Manager_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the current recently used resources manager changes its
   --  contents.
   --
   --  This can happen either by calling [methodGtk.RecentManager.add_item] or
   --  by another application.

private
   Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("size");
   Filename_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("filename");
end Gtk.Recent_Manager;
