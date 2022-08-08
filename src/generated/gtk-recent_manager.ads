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
--  Gtk.Recent_Manager.Gtk_Recent_Manager provides a facility for adding,
--  removing and looking up recently used files. Each recently used file is
--  identified by its URI, and has meta-data associated to it, like the names
--  and command lines of the applications that have registered it, the number
--  of time each application has registered the same file, the mime type of the
--  file and whether the file should be displayed only by the applications that
--  have registered it.
--
--  The recently used files list is per user.
--
--  The Gtk.Recent_Manager.Gtk_Recent_Manager acts like a database of all the
--  recently used files. You can create new
--  Gtk.Recent_Manager.Gtk_Recent_Manager objects, but it is more efficient to
--  use the default manager created by GTK+.
--
--  Adding a new recently used file is as simple as:
--
--  |[<!-- language="C" --> GtkRecentManager *manager;
--
--  manager = gtk_recent_manager_get_default (); gtk_recent_manager_add_item
--  (manager, file_uri); ]|
--
--  The Gtk.Recent_Manager.Gtk_Recent_Manager will try to gather all the
--  needed information from the file itself through GIO.
--
--  Looking up the meta-data associated with a recently used file given its
--  URI requires calling Gtk.Recent_Manager.Lookup_Item:
--
--  |[<!-- language="C" --> GtkRecentManager *manager; GtkRecentInfo *info;
--  GError *error = NULL;
--
--  manager = gtk_recent_manager_get_default (); info =
--  gtk_recent_manager_lookup_item (manager, file_uri, &error); if (error) {
--  g_warning ("Could not find the file: %s", error->message); g_error_free
--  (error); } else { // Use the info object gtk_recent_info_unref (info); } ]|
--
--  In order to retrieve the list of recently used files, you can use
--  Gtk.Recent_Manager.Get_Items, which returns a list of
--  Gtk.Recent_Info.Gtk_Recent_Info-structs.
--
--  A Gtk.Recent_Manager.Gtk_Recent_Manager is the model used to populate the
--  contents of one, or more Gtk.Recent_Chooser.Gtk_Recent_Chooser
--  implementations.
--
--  Note that the maximum age of the recently used files list is controllable
--  through the Gtk.Settings.Gtk_Settings:gtk-recent-files-max-age property.
--
--  Recently used files are supported since GTK+ 2.10.
--
--  </description>
--  <description>
--  In case the default screen is being used, adding a new recently used file
--  is as simple as:
--
-- 
--      declare
--         Manager : constant Gtk_Recent_Manager := Get_Default;
--      begin
--         Add_Item (Manager, File_URI);
--      end;
--
--  While looking up a recently used file is as simple as using:
--
-- 
--      declare
--         Manager : constant Gtk_Recent_Manager := Get_Default;
--         Info    : Gtk_Recent_Info;
--         Error   : Glib.Error.GError;
--      begin
--         Lookup_Item (Info, Manager, File_URI, Error);
--         if Error /= null then
--            --  Use the info object
--            Unref (Info);
--         else
--            Put_Line
--              ("Could not find the file: " & Glib.Error.Get_Message (Error));
--            Glib.Error.Error_Free (Error);
--         end if;
--      end;
--      
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;    use GNAT.Strings;
with Glib;            use Glib;
with Glib.Glist;      use Glib.Glist;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Gtk.Recent_Info; use Gtk.Recent_Info;
with Gtkada.Types;    use Gtkada.Types;

package Gtk.Recent_Manager is

   type Gtk_Recent_Manager_Record is new GObject_Record with null record;
   type Gtk_Recent_Manager is access all Gtk_Recent_Manager_Record'Class;

   type Gtk_Recent_Data is record
      Display_Name : Gtkada.Types.Chars_Ptr;
      Description : Gtkada.Types.Chars_Ptr;
      Mime_Type : Gtkada.Types.Chars_Ptr;
      App_Name : Gtkada.Types.Chars_Ptr;
      App_Exec : Gtkada.Types.Chars_Ptr;
      Groups : Gtkada.Types.char_array_access;
      Is_Private : Boolean;
   end record;
   pragma Convention (C, Gtk_Recent_Data);

   function From_Object_Free (B : access Gtk_Recent_Data) return Gtk_Recent_Data;
   pragma Inline (From_Object_Free);
   --  Meta-data to be passed to gtk_recent_manager_add_full when registering
   --  a recently used resource.

   function Convert (R : Gtk.Recent_Info.Gtk_Recent_Info) return System.Address;
   function Convert (R : System.Address) return Gtk.Recent_Info.Gtk_Recent_Info;
   package Gtk_Recent_Info_List is new Generic_List (Gtk.Recent_Info.Gtk_Recent_Info);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Recent_Manager);
   procedure Initialize
      (Self : not null access Gtk_Recent_Manager_Record'Class);
   --  Creates a new recent manager object. Recent manager objects are used to
   --  handle the list of recently used resources. A
   --  Gtk.Recent_Manager.Gtk_Recent_Manager object monitors the recently used
   --  resources list, and emits the "changed" signal each time something
   --  inside the list changes.
   --  Gtk.Recent_Manager.Gtk_Recent_Manager objects are expensive: be sure to
   --  create them only when needed. You should use
   --  Gtk.Recent_Manager.Get_Default instead.
   --  Since: gtk+ 2.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Recent_Manager_New return Gtk_Recent_Manager;
   --  Creates a new recent manager object. Recent manager objects are used to
   --  handle the list of recently used resources. A
   --  Gtk.Recent_Manager.Gtk_Recent_Manager object monitors the recently used
   --  resources list, and emits the "changed" signal each time something
   --  inside the list changes.
   --  Gtk.Recent_Manager.Gtk_Recent_Manager objects are expensive: be sure to
   --  create them only when needed. You should use
   --  Gtk.Recent_Manager.Get_Default instead.
   --  Since: gtk+ 2.10

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
   --  to gtk_recent_manager_add_full.
   --  See gtk_recent_manager_add_full if you want to explicitly define the
   --  metadata for the resource pointed by Uri.
   --  Since: gtk+ 2.10
   --  "uri": a valid URI

   function Get_Items
      (Self : not null access Gtk_Recent_Manager_Record)
       return Gtk_Recent_Info_List.Glist;
   --  Gets the list of recently used resources.
   --  Since: gtk+ 2.10

   function Has_Item
      (Self : not null access Gtk_Recent_Manager_Record;
       URI  : UTF8_String) return Boolean;
   --  Checks whether there is a recently used resource registered with Uri
   --  inside the recent manager.
   --  Since: gtk+ 2.10
   --  "uri": a URI

   function Lookup_Item
      (Self : not null access Gtk_Recent_Manager_Record;
       URI  : UTF8_String) return Gtk.Recent_Info.Gtk_Recent_Info;
   --  Searches for a URI inside the recently used resources list, and returns
   --  a Gtk.Recent_Info.Gtk_Recent_Info-struct containing informations about
   --  the resource like its MIME type, or its display name.
   --  Since: gtk+ 2.10
   --  "uri": a URI

   function Move_Item
      (Self    : not null access Gtk_Recent_Manager_Record;
       URI     : UTF8_String;
       New_Uri : UTF8_String := "") return Boolean;
   --  Changes the location of a recently used resource from Uri to New_Uri.
   --  Please note that this function will not affect the resource pointed by
   --  the URIs, but only the URI used in the recently used resources list.
   --  Since: gtk+ 2.10
   --  "uri": the URI of a recently used resource
   --  "new_uri": the new URI of the recently used resource, or null to remove
   --  the item pointed by Uri in the list

   function Purge_Items
      (Self : not null access Gtk_Recent_Manager_Record) return Glib.Gint;
   --  Purges every item from the recently used resources list.
   --  Since: gtk+ 2.10

   function Remove_Item
      (Self : not null access Gtk_Recent_Manager_Record;
       URI  : UTF8_String) return Boolean;
   --  Removes a resource pointed by Uri from the recently used resources list
   --  handled by a recent manager.
   --  Since: gtk+ 2.10
   --  "uri": the URI of the item you wish to remove

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
   --  Gets a unique instance of Gtk.Recent_Manager.Gtk_Recent_Manager, that
   --  you can share in your application without caring about memory
   --  management.
   --  Since: gtk+ 2.10

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
   --  contents, either by calling Gtk.Recent_Manager.Add_Item or by another
   --  application.

private
   Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("size");
   Filename_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("filename");
end Gtk.Recent_Manager;
