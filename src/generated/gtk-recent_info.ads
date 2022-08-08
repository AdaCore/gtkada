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
--  Gtk.Recent_Info.Gtk_Recent_Info-struct contains private data only, and
--  should be accessed using the provided API.
--
--  Gtk.Recent_Info.Gtk_Recent_Info constains all the meta-data associated
--  with an entry in the recently used files list.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Pixbuf;  use Gdk.Pixbuf;
with Glib;        use Glib;
with Glib.G_Icon; use Glib.G_Icon;

package Gtk.Recent_Info is

   type Gtk_Recent_Info is new Glib.C_Boxed with null record;
   Null_Gtk_Recent_Info : constant Gtk_Recent_Info;

   function From_Object (Object : System.Address) return Gtk_Recent_Info;
   function From_Object_Free (B : access Gtk_Recent_Info'Class) return Gtk_Recent_Info;
   pragma Inline (From_Object_Free, From_Object);

   subtype time_t is Long_Integer;
   --  Type to interface with C's time_t type.  To convert this to/from
   --  an Ada type, look at Ada.Calendar.Conversion_Operations and be
   --  sure to pay special attention to the ranges each type is capable
   --  of representing.

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
       App_Name : UTF8_String := "") return Glib.GApp_Info;
   --  Creates a Glib.GApp_Info for the specified
   --  Gtk.Recent_Info.Gtk_Recent_Info
   --  "app_name": the name of the application that should be mapped to a
   --  Glib.GApp_Info; if null is used then the default application for the
   --  MIME type is used

   function Exists (Self : Gtk_Recent_Info) return Boolean;
   --  Checks whether the resource pointed by Info still exists. At the moment
   --  this check is done only on resources pointing to local files.
   --  Since: gtk+ 2.10

   function Get_Added (Self : Gtk_Recent_Info) return time_t;
   --  Gets the timestamp (seconds from system's Epoch) when the resource was
   --  added to the recently used resources list.
   --  Since: gtk+ 2.10

   function Get_Age (Self : Gtk_Recent_Info) return Glib.Gint;
   --  Gets the number of days elapsed since the last update of the resource
   --  pointed by Info.
   --  Since: gtk+ 2.10

   function Get_Description (Self : Gtk_Recent_Info) return UTF8_String;
   --  Gets the (short) description of the resource.
   --  Since: gtk+ 2.10

   function Get_Display_Name (Self : Gtk_Recent_Info) return UTF8_String;
   --  Gets the name of the resource. If none has been defined, the basename
   --  of the resource is obtained.
   --  Since: gtk+ 2.10

   function Get_Gicon (Self : Gtk_Recent_Info) return Glib.G_Icon.G_Icon;
   --  Retrieves the icon associated to the resource MIME type.
   --  Since: gtk+ 2.22

   function Get_Icon
      (Self : Gtk_Recent_Info;
       Size : Glib.Gint) return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Retrieves the icon of size Size associated to the resource MIME type.
   --  Since: gtk+ 2.10
   --  "size": the size of the icon in pixels

   function Get_Mime_Type (Self : Gtk_Recent_Info) return UTF8_String;
   --  Gets the MIME type of the resource.
   --  Since: gtk+ 2.10

   function Get_Modified (Self : Gtk_Recent_Info) return time_t;
   --  Gets the timestamp (seconds from system's Epoch) when the meta-data for
   --  the resource was last modified.
   --  Since: gtk+ 2.10

   function Get_Private_Hint (Self : Gtk_Recent_Info) return Boolean;
   --  Gets the value of the "private" flag. Resources in the recently used
   --  list that have this flag set to True should only be displayed by the
   --  applications that have registered them.
   --  Since: gtk+ 2.10

   function Get_Short_Name (Self : Gtk_Recent_Info) return UTF8_String;
   --  Computes a valid UTF-8 string that can be used as the name of the item
   --  in a menu or list. For example, calling this function on an item that
   --  refers to "file:///foo/bar.txt" will yield "bar.txt".
   --  Since: gtk+ 2.10

   function Get_Uri (Self : Gtk_Recent_Info) return UTF8_String;
   --  Gets the URI of the resource.
   --  Since: gtk+ 2.10

   function Get_Uri_Display (Self : Gtk_Recent_Info) return UTF8_String;
   --  Gets a displayable version of the resource's URI. If the resource is
   --  local, it returns a local path; if the resource is not local, it returns
   --  the UTF-8 encoded content of Gtk.Recent_Info.Get_Uri.
   --  Since: gtk+ 2.10

   function Get_Visited (Self : Gtk_Recent_Info) return time_t;
   --  Gets the timestamp (seconds from system's Epoch) when the meta-data for
   --  the resource was last visited.
   --  Since: gtk+ 2.10

   function Has_Application
      (Self     : Gtk_Recent_Info;
       App_Name : UTF8_String) return Boolean;
   --  Checks whether an application registered this resource using App_Name.
   --  Since: gtk+ 2.10
   --  "app_name": a string containing an application name

   function Has_Group
      (Self       : Gtk_Recent_Info;
       Group_Name : UTF8_String) return Boolean;
   --  Checks whether Group_Name appears inside the groups registered for the
   --  recently used item Info.
   --  Since: gtk+ 2.10
   --  "group_name": name of a group

   function Is_Local (Self : Gtk_Recent_Info) return Boolean;
   --  Checks whether the resource is local or not by looking at the scheme of
   --  its URI.
   --  Since: gtk+ 2.10

   function Last_Application (Self : Gtk_Recent_Info) return UTF8_String;
   --  Gets the name of the last application that have registered the recently
   --  used resource represented by Info.
   --  Since: gtk+ 2.10

   function Match
      (Self   : Gtk_Recent_Info;
       Info_B : Gtk_Recent_Info) return Boolean;
   --  Checks whether two Gtk.Recent_Info.Gtk_Recent_Info-struct point to the
   --  same resource.
   --  Since: gtk+ 2.10
   --  "info_b": a Gtk.Recent_Info.Gtk_Recent_Info

   procedure Ref (Self : Gtk_Recent_Info);
   --  Increases the reference count of Recent_Info by one.
   --  Since: gtk+ 2.10

   procedure Unref (Self : Gtk_Recent_Info);
   --  Decreases the reference count of Info by one. If the reference count
   --  reaches zero, Info is deallocated, and the memory freed.
   --  Since: gtk+ 2.10

private

   Null_Gtk_Recent_Info : constant Gtk_Recent_Info := (Glib.C_Boxed with null record);

end Gtk.Recent_Info;
