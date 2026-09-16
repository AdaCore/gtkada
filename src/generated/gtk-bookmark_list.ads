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

--  A list model that wraps `GBookmarkFile`.
--
--  It presents a `GListModel` and fills it asynchronously with the
--  `GFileInfo`s returned from that function.
--
--  The `GFileInfo`s in the list have some attributes in the recent namespace
--  added: `recent::private` (boolean) and `recent:applications` (stringv).

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.List_Model; use Glib.List_Model;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;

package Gtk.Bookmark_List is

   type Gtk_Bookmark_List_Record is new GObject_Record with null record;
   type Gtk_Bookmark_List is access all Gtk_Bookmark_List_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self       : out Gtk_Bookmark_List;
       Filename   : UTF8_String := "";
       Attributes : UTF8_String := "");
   procedure Initialize
      (Self       : not null access Gtk_Bookmark_List_Record'Class;
       Filename   : UTF8_String := "";
       Attributes : UTF8_String := "");
   --  Creates a new `GtkBookmarkList` with the given Attributes.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Filename The bookmark file to load
   --  @param Attributes The attributes to query

   function Gtk_Bookmark_List_New
      (Filename   : UTF8_String := "";
       Attributes : UTF8_String := "") return Gtk_Bookmark_List;
   --  Creates a new `GtkBookmarkList` with the given Attributes.
   --  @param Filename The bookmark file to load
   --  @param Attributes The attributes to query

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_bookmark_list_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Attributes
      (Self : not null access Gtk_Bookmark_List_Record) return UTF8_String;
   --  Gets the attributes queried on the children.
   --  @return The queried attributes

   procedure Set_Attributes
      (Self       : not null access Gtk_Bookmark_List_Record;
       Attributes : UTF8_String := "");
   --  Sets the Attributes to be enumerated and starts the enumeration.
   --  If Attributes is null, no attributes will be queried, but a list of
   --  `GFileInfo`s will still be created.
   --  @param Attributes the attributes to enumerate

   function Get_Filename
      (Self : not null access Gtk_Bookmark_List_Record) return UTF8_String;
   --  Returns the filename of the bookmark file that this list is loading.
   --  @return the filename of the .xbel file

   function Get_Io_Priority
      (Self : not null access Gtk_Bookmark_List_Record) return Glib.Gint;
   --  Gets the IO priority to use while loading file.
   --  @return The IO priority.

   procedure Set_Io_Priority
      (Self        : not null access Gtk_Bookmark_List_Record;
       Io_Priority : Glib.Gint);
   --  Sets the IO priority to use while loading files.
   --  The default IO priority is G_PRIORITY_DEFAULT.
   --  @param Io_Priority IO priority to use

   function Is_Loading
      (Self : not null access Gtk_Bookmark_List_Record) return Boolean;
   --  Returns True if the files are currently being loaded.
   --  Files will be added to Self from time to time while loading is going
   --  on. The order in which are added is undefined and may change in between
   --  runs.
   --  @return True if Self is loading

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   function Get_Item_Type
      (Self : not null access Gtk_Bookmark_List_Record) return GType;

   function Get_N_Items
      (Self : not null access Gtk_Bookmark_List_Record) return Guint;

   function Get_Item
      (Self     : not null access Gtk_Bookmark_List_Record;
       Position : Guint) return Glib.Object.GObject;

   procedure Items_Changed
      (Self     : not null access Gtk_Bookmark_List_Record;
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

   Filename_Property : constant Glib.Properties.Property_String;
   --  The bookmark file to load.

   Io_Priority_Property : constant Glib.Properties.Property_Int;
   --  Priority used when loading.

   Item_Type_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GType
   --  The type of items. See [methodGio.ListModel.get_item_type].

   Loading_Property : constant Glib.Properties.Property_Boolean;
   --  True if files are being loaded.

   N_Items_Property : constant Glib.Properties.Property_Uint;
   --  The number of items. See [methodGio.ListModel.get_n_items].

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gio.ListModel"

   package Implements_Glist_Model is new Glib.Types.Implements
     (Glib.List_Model.Glist_Model, Gtk_Bookmark_List_Record, Gtk_Bookmark_List);
   function "+"
     (Widget : access Gtk_Bookmark_List_Record'Class)
   return Glib.List_Model.Glist_Model
   renames Implements_Glist_Model.To_Interface;
   function "-"
     (Interf : Glib.List_Model.Glist_Model)
   return Gtk_Bookmark_List
   renames Implements_Glist_Model.To_Object;

private
   N_Items_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n-items");
   Loading_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("loading");
   Item_Type_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("item-type");
   Io_Priority_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("io-priority");
   Filename_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("filename");
   Attributes_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("attributes");
end Gtk.Bookmark_List;
