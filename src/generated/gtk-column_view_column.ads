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

--  Represents the columns in a `GtkColumnView`.
--
--  The main ingredient for a `GtkColumnViewColumn` is the
--  `GtkListItemFactory` that tells the columnview how to create cells for this
--  column from items in the model.
--
--  Columns have a title, and can optionally have a header menu set with
--  [methodGtk.ColumnViewColumn.set_header_menu].
--
--  A sorter can be associated with a column using
--  [methodGtk.ColumnViewColumn.set_sorter], to let users influence sorting by
--  clicking on the column header.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Menu_Model;       use Glib.Menu_Model;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Gtk.List_Item_Factory; use Gtk.List_Item_Factory;
with Gtk.Sorter;            use Gtk.Sorter;

package Gtk.Column_View_Column is

   type Gtk_Column_View_Column_Record is new GObject_Record with null record;
   type Gtk_Column_View_Column is access all Gtk_Column_View_Column_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self    : out Gtk_Column_View_Column;
       Title   : UTF8_String := "";
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class);
   procedure Initialize
      (Self    : not null access Gtk_Column_View_Column_Record'Class;
       Title   : UTF8_String := "";
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class);
   --  Creates a new `GtkColumnViewColumn` that uses the given Factory for
   --  mapping items to widgets.
   --  You most likely want to call [methodGtk.ColumnView.append_column] next.
   --  The function takes ownership of the argument, so you can write code
   --  like:
   --  ```c column = gtk_column_view_column_new (_("Name"),
   --  gtk_builder_list_item_factory_new_from_resource ("/name.ui")); ```
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Title Title to use for this column
   --  @param Factory The factory to populate items with

   function Gtk_Column_View_Column_New
      (Title   : UTF8_String := "";
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class)
       return Gtk_Column_View_Column;
   --  Creates a new `GtkColumnViewColumn` that uses the given Factory for
   --  mapping items to widgets.
   --  You most likely want to call [methodGtk.ColumnView.append_column] next.
   --  The function takes ownership of the argument, so you can write code
   --  like:
   --  ```c column = gtk_column_view_column_new (_("Name"),
   --  gtk_builder_list_item_factory_new_from_resource ("/name.ui")); ```
   --  @param Title Title to use for this column
   --  @param Factory The factory to populate items with

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_column_view_column_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Expand
      (Self : not null access Gtk_Column_View_Column_Record) return Boolean;
   --  Returns whether this column should expand.
   --  @return true if this column expands

   procedure Set_Expand
      (Self   : not null access Gtk_Column_View_Column_Record;
       Expand : Boolean);
   --  Sets the column to take available extra space.
   --  The extra space is shared equally amongst all columns that have are set
   --  to expand.
   --  @param Expand whether this column should expand to fill available space

   function Get_Factory
      (Self : not null access Gtk_Column_View_Column_Record)
       return Gtk.List_Item_Factory.Gtk_List_Item_Factory;
   --  Gets the factory that's currently used to populate list items for this
   --  column.
   --  @return The factory in use

   procedure Set_Factory
      (Self    : not null access Gtk_Column_View_Column_Record;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class);
   --  Sets the `GtkListItemFactory` to use for populating list items for this
   --  column.
   --  @param Factory the factory to use

   function Get_Fixed_Width
      (Self : not null access Gtk_Column_View_Column_Record)
       return Glib.Gint;
   --  Gets the fixed width of the column.
   --  @return the fixed with of the column

   procedure Set_Fixed_Width
      (Self        : not null access Gtk_Column_View_Column_Record;
       Fixed_Width : Glib.Gint);
   --  Sets the fixed width of the column.
   --  If Fixed_Width is -1, the fixed width of the column is unset.
   --  Setting a fixed width overrides the automatically calculated width.
   --  Interactive resizing also sets the "fixed-width" property.
   --  @param Fixed_Width the new fixed width, or -1

   function Get_Header_Menu
      (Self : not null access Gtk_Column_View_Column_Record)
       return Glib.Menu_Model.Gmenu_Model;
   --  Gets the menu model that is used to create the context menu for the
   --  column header.
   --  @return the `GMenuModel`

   procedure Set_Header_Menu
      (Self : not null access Gtk_Column_View_Column_Record;
       Menu : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Sets the menu model that is used to create the context menu for the
   --  column header.
   --  @param Menu a `GMenuModel`

   function Get_Id
      (Self : not null access Gtk_Column_View_Column_Record)
       return UTF8_String;
   --  Returns the ID set with [methodGtk.ColumnViewColumn.set_id].
   --  Since: gtk+ 4.10
   --  @return The column's ID

   procedure Set_Id
      (Self : not null access Gtk_Column_View_Column_Record;
       Id   : UTF8_String := "");
   --  Sets the id of this column.
   --  GTK makes no use of this, but applications can use it when storing
   --  column view configuration.
   --  It is up to callers to ensure uniqueness of IDs.
   --  Since: gtk+ 4.10
   --  @param Id ID to use for this column

   function Get_Resizable
      (Self : not null access Gtk_Column_View_Column_Record) return Boolean;
   --  Returns whether this column is resizable.
   --  @return true if this column is resizable

   procedure Set_Resizable
      (Self      : not null access Gtk_Column_View_Column_Record;
       Resizable : Boolean);
   --  Sets whether this column should be resizable by dragging.
   --  @param Resizable whether this column should be resizable

   function Get_Sorter
      (Self : not null access Gtk_Column_View_Column_Record)
       return Gtk.Sorter.Gtk_Sorter;
   --  Returns the sorter that is associated with the column.
   --  @return the `GtkSorter` of Self

   procedure Set_Sorter
      (Self   : not null access Gtk_Column_View_Column_Record;
       Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class);
   --  Associates a sorter with the column.
   --  If Sorter is unset, the column will not let users change the sorting by
   --  clicking on its header.
   --  This sorter can be made active by clicking on the column header, or by
   --  calling [methodGtk.ColumnView.sort_by_column].
   --  See [methodGtk.ColumnView.get_sorter] for the necessary steps for
   --  setting up customizable sorting for [classGtk.ColumnView].
   --  @param Sorter the `GtkSorter` to associate with Column

   function Get_Title
      (Self : not null access Gtk_Column_View_Column_Record)
       return UTF8_String;
   --  Returns the title set with [methodGtk.ColumnViewColumn.set_title].
   --  @return The column's title

   procedure Set_Title
      (Self  : not null access Gtk_Column_View_Column_Record;
       Title : UTF8_String := "");
   --  Sets the title of this column.
   --  The title is displayed in the header of a `GtkColumnView` for this
   --  column and is therefore user-facing text that should be translated.
   --  @param Title Title to use for this column

   function Get_Visible
      (Self : not null access Gtk_Column_View_Column_Record) return Boolean;
   --  Returns whether this column is visible.
   --  @return true if this column is visible

   procedure Set_Visible
      (Self    : not null access Gtk_Column_View_Column_Record;
       Visible : Boolean);
   --  Sets whether this column should be visible in views.
   --  @param Visible whether this column should be visible

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Column_View_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Column_View.Gtk_Column_View
   --  The `GtkColumnView` this column is a part of.

   Expand_Property : constant Glib.Properties.Property_Boolean;
   --  Column gets share of extra width allocated to the view.

   Factory_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.List_Item_Factory.Gtk_List_Item_Factory
   --  Factory for populating list items.
   --
   --  The factory must be for configuring [classGtk.ColumnViewCell] objects.

   Fixed_Width_Property : constant Glib.Properties.Property_Int;
   --  If not -1, this is the width that the column is allocated, regardless
   --  of the size of its content.

   Header_Menu_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.Menu_Model
   --  Menu model used to create the context menu for the column header.

   Id_Property : constant Glib.Properties.Property_String;
   --  An ID for the column.
   --
   --  GTK is not currently using the ID for anything, but it can be used by
   --  applications when saving column view configurations.
   --
   --  It is up to applications to ensure uniqueness of IDs.

   Resizable_Property : constant Glib.Properties.Property_Boolean;
   --  Whether this column is resizable.

   Sorter_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Sorter.Gtk_Sorter
   --  Sorter for sorting items according to this column.

   Title_Property : constant Glib.Properties.Property_String;
   --  Title displayed in the header.

   Visible_Property : constant Glib.Properties.Property_Boolean;
   --  Whether this column is visible.

private
   Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible");
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Sorter_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("sorter");
   Resizable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("resizable");
   Id_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("id");
   Header_Menu_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("header-menu");
   Fixed_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("fixed-width");
   Factory_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("factory");
   Expand_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("expand");
   Column_View_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("column-view");
end Gtk.Column_View_Column;
