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

--  A list model that wraps an array of strings.
--
--  The objects in the model are of type [classGtk.StringObject] and have a
--  "string" property that can be used inside expressions.
--
--  `GtkStringList` is well-suited for any place where you would typically use
--  a `char*[]`, but need a list model.
--
--  ## GtkStringList as GtkBuildable
--
--  The `GtkStringList` implementation of the `GtkBuildable` interface
--  supports adding items directly using the `<items>` element and specifying
--  `<item>` elements for each item. Each `<item>` element supports the regular
--  translation attributes "translatable", "context" and "comments".
--
--  Here is a UI definition fragment specifying a `GtkStringList`
--
--  ```xml <object class="GtkStringList"> <items> <item
--  translatable="yes">Factory</item> <item translatable="yes">Home</item>
--  <item translatable="yes">Subway</item> </items> </object> ```

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;    use GNAT.Strings;
with Glib;            use Glib;
with Glib.List_Model; use Glib.List_Model;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;

package Gtk.String_List is

   type Gtk_String_List_Record is new GObject_Record with null record;
   type Gtk_String_List is access all Gtk_String_List_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self    : out Gtk_String_List;
       Strings : GNAT.Strings.String_List);
   procedure Initialize
      (Self    : not null access Gtk_String_List_Record'Class;
       Strings : GNAT.Strings.String_List);
   --  Creates a new `GtkStringList` with the given Strings.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Strings The strings to put in the model

   function Gtk_String_List_New
      (Strings : GNAT.Strings.String_List) return Gtk_String_List;
   --  Creates a new `GtkStringList` with the given Strings.
   --  @param Strings The strings to put in the model

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_string_list_get_type");

   -------------
   -- Methods --
   -------------

   procedure Append
      (Self   : not null access Gtk_String_List_Record;
       String : UTF8_String);
   --  Appends String to Self.
   --  The String will be copied. See [methodGtk.StringList.take] for a way to
   --  avoid that.
   --  @param String the string to insert

   function Find
      (Self   : not null access Gtk_String_List_Record;
       String : UTF8_String) return Guint;
   --  Gets the position of the String in Self.
   --  If Self does not contain String item, `G_MAXUINT` is returned.
   --  Since: gtk+ 4.18
   --  @param String the string to find
   --  @return the position of the string

   function Get_String
      (Self     : not null access Gtk_String_List_Record;
       Position : Guint) return UTF8_String;
   --  Gets the string that is at Position in Self.
   --  If Self does not contain Position items, null is returned.
   --  This function returns the const char *. To get the object wrapping it,
   --  use g_list_model_get_item.
   --  @param Position the position to get the string for
   --  @return the string at the given position

   procedure Remove
      (Self     : not null access Gtk_String_List_Record;
       Position : Guint);
   --  Removes the string at Position from Self.
   --  Position must be smaller than the current length of the list.
   --  @param Position the position of the string that is to be removed

   procedure Splice
      (Self       : not null access Gtk_String_List_Record;
       Position   : Guint;
       N_Removals : Guint;
       Additions  : GNAT.Strings.String_List);
   --  Changes Self by removing N_Removals strings and adding Additions to it.
   --  This function is more efficient than [methodGtk.StringList.append] and
   --  [methodGtk.StringList.remove], because it only emits the ::items-changed
   --  signal once for the change.
   --  This function copies the strings in Additions.
   --  The parameters Position and N_Removals must be correct (ie: Position +
   --  N_Removals must be less than or equal to the length of the list at the
   --  time this function is called).
   --  @param Position the position at which to make the change
   --  @param N_Removals the number of strings to remove
   --  @param Additions The strings to add

   procedure Take
      (Self   : not null access Gtk_String_List_Record;
       String : UTF8_String);
   --  Adds String to self at the end, and takes ownership of it.
   --  This variant of [methodGtk.StringList.append] is convenient for
   --  formatting strings:
   --  ```c gtk_string_list_take (self, g_strdup_print ("%d dollars", lots));
   --  ```
   --  @param String the string to insert

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Item_Type
      (Self : not null access Gtk_String_List_Record) return GType;

   function Get_N_Items
      (Self : not null access Gtk_String_List_Record) return Guint;

   function Get_Item
      (Self     : not null access Gtk_String_List_Record;
       Position : Guint) return Glib.Object.GObject;

   procedure Items_Changed
      (Self     : not null access Gtk_String_List_Record;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Strings_Property : constant Glib.Properties.Property_String :=
   Glib.Properties.Build ("strings");--  Unknown type: unspecified

   Item_Type_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GType
   --  The type of items. See [methodGio.ListModel.get_item_type].

   N_Items_Property : constant Glib.Properties.Property_Uint;
   --  The number of items. See [methodGio.ListModel.get_n_items].

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gio.ListModel"
   --
   --  - "Gtk.Buildable"

   package Implements_Glist_Model is new Glib.Types.Implements
     (Glib.List_Model.Glist_Model, Gtk_String_List_Record, Gtk_String_List);
   function "+"
     (Widget : access Gtk_String_List_Record'Class)
   return Glib.List_Model.Glist_Model
   renames Implements_Glist_Model.To_Interface;
   function "-"
     (Interf : Glib.List_Model.Glist_Model)
   return Gtk_String_List
   renames Implements_Glist_Model.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_String_List_Record, Gtk_String_List);
   function "+"
     (Widget : access Gtk_String_List_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_String_List
   renames Implements_Gtk_Buildable.To_Object;

private
   N_Items_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n-items");
   Item_Type_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("item-type");
end Gtk.String_List;
