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

--  Contains the metadata associated with an item in the recently used files
--  list.
--
--  Metadata associated with an item in the recently used files list. Ref
--  returns an additional reference; release owned references with Unref. The
--  dates returned by Get_Added, Get_Modified and Get_Visited are borrowed from
--  the manager's database. Take a Glib.Date_Time.Ref before retaining a date
--  across changes to that database.
--
--  <group>Recent Files</group>

pragma Warnings (Off, "*is already use-visible*");
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Strings;          use GNAT.Strings;
with Glib;                  use Glib;
with Glib.App_Info;         use Glib.App_Info;
with Glib.Date_Time;        use Glib.Date_Time;
with Glib.Error;            use Glib.Error;
with Glib.G_Icon;           use Glib.G_Icon;

package Gtk.Recent_Info is

   type Gtk_Recent_Info is new Glib.C_Boxed with null record;
   Null_Gtk_Recent_Info : constant Gtk_Recent_Info;

   function From_Object (Object : System.Address) return Gtk_Recent_Info;
   function From_Object_Free (B : access Gtk_Recent_Info'Class) return Gtk_Recent_Info;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_recent_info_get_type");

   -------------
   -- Methods --
   -------------

   function Create_App_Info
      (Self     : Gtk_Recent_Info;
       App_Name : UTF8_String := "";
       Error    : out Glib.Error.GError) return Glib.App_Info.Gapp_Info;
   --  Creates a `GAppInfo` for the specified `GtkRecentInfo`
   --  In case of error, Error will be set either with a
   --  GTK_RECENT_MANAGER_ERROR or a G_IO_ERROR
   --  @param App_Name the name of the application that should be mapped to a
   --  `GAppInfo`; if null is used then the default application for the MIME
   --  type is used
   --  @param Error the return location for a recoverable error
   --  @return the newly created `GAppInfo`

   function Exists (Self : Gtk_Recent_Info) return Boolean;
   --  Checks whether the resource pointed by Info still exists. At the moment
   --  this check is done only on resources pointing to local files.
   --  @return True if the resource exists

   function Get_Added
      (Self : Gtk_Recent_Info) return Glib.Date_Time.Gdate_Time;
   --  Gets the time when the resource was added to the recently used
   --  resources list.
   --  @return a `GDateTime` for the time when the resource was added. Has
   --  transfer-ownership='none'.

   function Get_Age (Self : Gtk_Recent_Info) return Glib.Gint;
   --  Gets the number of days elapsed since the last update of the resource
   --  pointed by Info.
   --  @return a positive integer containing the number of days elapsed since
   --  the time this resource was last modified

   function Get_Application_Info
      (Self     : Gtk_Recent_Info;
       App_Name : UTF8_String;
       App_Exec : out Ada.Strings.Unbounded.Unbounded_String;
       Count    : out Guint;
       Stamp    : out Glib.Date_Time.Gdate_Time) return Boolean;
   --  Gets the data regarding the application that has registered the
   --  resource pointed by Info.
   --  If the command line contains any escape characters defined inside the
   --  storage specification, they will be expanded.
   --  App_Exec is copied into an Ada unbounded string. Stamp is borrowed from
   --  Self. On failure, App_Exec is empty, Count is zero and Stamp is null.
   --  @return True if the application registered this resource, False
   --  otherwise.
   --  @param App_Name the name of the application that has registered this
   --  item
   --  @param App_Exec return location for the string containing the command
   --  line
   --  @param Count return location for the number of times this item was
   --  registered
   --  @param Stamp return location for the time this item was last registered
   --  for this application

   function Get_Applications
      (Self   : Gtk_Recent_Info;
       Length : access Gsize := null) return GNAT.Strings.String_List;
   --  Returns the names of applications that registered this resource.
   --  Length, when supplied, receives their count. The returned Ada strings
   --  belong to the caller; free each element with GNAT.Strings.Free. @return
   --  an Ada list of application names.
   --  @param Length return location for the length of the returned list

   function Get_Description (Self : Gtk_Recent_Info) return UTF8_String;
   --  Gets the (short) description of the resource.
   --  @return the description of the resource. The returned string is owned
   --  by the recent manager, and should not be freed.

   function Get_Display_Name (Self : Gtk_Recent_Info) return UTF8_String;
   --  Gets the name of the resource.
   --  If none has been defined, the basename of the resource is obtained.
   --  @return the display name of the resource. The returned string is owned
   --  by the recent manager, and should not be freed.

   function Get_Gicon (Self : Gtk_Recent_Info) return Glib.G_Icon.G_Icon;
   --  Retrieves the icon associated to the resource MIME type.
   --  @return a `GIcon` containing the icon

   function Get_Groups
      (Self   : Gtk_Recent_Info;
       Length : access Gsize := null) return GNAT.Strings.String_List;
   --  Returns the group names registered for this resource. Length, when
   --  supplied, receives their count. The returned Ada strings belong to the
   --  caller; free each element with GNAT.Strings.Free. @return an Ada list of
   --  group names.
   --  @param Length return location for the number of groups returned

   function Get_Mime_Type (Self : Gtk_Recent_Info) return UTF8_String;
   --  Gets the MIME type of the resource.
   --  @return the MIME type of the resource. The returned string is owned by
   --  the recent manager, and should not be freed.

   function Get_Modified
      (Self : Gtk_Recent_Info) return Glib.Date_Time.Gdate_Time;
   --  Gets the time when the meta-data for the resource was last modified.
   --  @return a `GDateTime` for the time when the resource was last modified.
   --  Has transfer-ownership='none'.

   function Get_Private_Hint (Self : Gtk_Recent_Info) return Boolean;
   --  Gets the value of the "private" flag.
   --  Resources in the recently used list that have this flag set to True
   --  should only be displayed by the applications that have registered them.
   --  @return True if the private flag was found, False otherwise

   function Get_Short_Name (Self : Gtk_Recent_Info) return UTF8_String;
   --  Computes a valid UTF-8 string that can be used as the name of the item
   --  in a menu or list.
   --  For example, calling this function on an item that refers to
   --  "file:///foo/bar.txt" will yield "bar.txt".
   --  @return A newly-allocated string in UTF-8 encoding free it with g_free

   function Get_Uri (Self : Gtk_Recent_Info) return UTF8_String;
   --  Gets the URI of the resource.
   --  @return the URI of the resource. The returned string is owned by the
   --  recent manager, and should not be freed.

   function Get_Uri_Display (Self : Gtk_Recent_Info) return UTF8_String;
   --  Gets a displayable version of the resource's URI.
   --  If the resource is local, it returns a local path; if the resource is
   --  not local, it returns the UTF-8 encoded content of
   --  [methodGtk.RecentInfo.get_uri].
   --  @return a newly allocated UTF-8 string containing the resource's URI or
   --  null. Use g_free when done using it.

   function Get_Visited
      (Self : Gtk_Recent_Info) return Glib.Date_Time.Gdate_Time;
   --  Gets the time when the meta-data for the resource was last visited.
   --  @return a `GDateTime` for the time when the resource was last visited.
   --  Has transfer-ownership='none'.

   function Has_Application
      (Self     : Gtk_Recent_Info;
       App_Name : UTF8_String) return Boolean;
   --  Checks whether an application registered this resource using App_Name.
   --  @param App_Name a string containing an application name
   --  @return True if an application with name App_Name was found, False
   --  otherwise

   function Has_Group
      (Self       : Gtk_Recent_Info;
       Group_Name : UTF8_String) return Boolean;
   --  Checks whether Group_Name appears inside the groups registered for the
   --  recently used item Info.
   --  @param Group_Name name of a group
   --  @return True if the group was found

   function Is_Local (Self : Gtk_Recent_Info) return Boolean;
   --  Checks whether the resource is local or not by looking at the scheme of
   --  its URI.
   --  @return True if the resource is local

   function Last_Application (Self : Gtk_Recent_Info) return UTF8_String;
   --  Gets the name of the last application that have registered the recently
   --  used resource represented by Info.
   --  @return an application name. Use g_free to free it.

   function Match
      (Self   : Gtk_Recent_Info;
       Info_B : Gtk_Recent_Info) return Boolean;
   --  Checks whether two `GtkRecentInfo` point to the same resource.
   --  @param Info_B a `GtkRecentInfo`
   --  @return True if both `GtkRecentInfo` point to the same resource, False
   --  otherwise

   function Ref (Self : Gtk_Recent_Info) return Gtk_Recent_Info;
   --  Increases the reference count of Recent_Info by one.
   --  @return the recent info object with its reference count increased by
   --  one. Has transfer-ownership='full'.

   procedure Unref (Self : Gtk_Recent_Info);
   --  Decreases the reference count of Info by one.
   --  If the reference count reaches zero, Info is deallocated, and the
   --  memory freed.

private
   Null_Gtk_Recent_Info : constant Gtk_Recent_Info :=
      (Glib.C_Boxed with null record);

end Gtk.Recent_Info;
