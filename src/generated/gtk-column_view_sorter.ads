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

--  Sorts [classGtk.ColumnView] columns.
--
--  The sorter returned by [methodGtk.ColumnView.get_sorter] is a
--  `GtkColumnViewSorter`.
--
--  In column views, sorting can be configured by associating sorters with
--  columns, and users can invert sort order by clicking on column headers. The
--  API of `GtkColumnViewSorter` is designed to allow saving and restoring this
--  configuration.
--
--  If you are only interested in the primary sort column (i.e. the column
--  where a sort indicator is shown in the header), then you can just look at
--  [propertyGtk.ColumnViewSorter:primary-sort-column] and
--  [propertyGtk.ColumnViewSorter:primary-sort-order].
--
--  If you want to store the full sort configuration, including secondary sort
--  columns that are used for tie breaking, then you can use
--  [methodGtk.ColumnViewSorter.get_nth_sort_column]. To get notified about
--  changes, use [signalGtk.Sorter::changed].
--
--  To restore a saved sort configuration on a `GtkColumnView`, use code like:
--
--  ``` sorter = gtk_column_view_get_sorter (view); for (i =
--  gtk_column_view_sorter_get_n_sort_columns (sorter) - 1; i >= 0; i--) {
--  column = gtk_column_view_sorter_get_nth_sort_column (sorter, i, &order);
--  gtk_column_view_sort_by_column (view, column, order); } ```

pragma Warnings (Off, "*is already use-visible*");
with Glib;                   use Glib;
with Glib.Properties;        use Glib.Properties;
with Gtk.Column_View_Column; use Gtk.Column_View_Column;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Sorter;             use Gtk.Sorter;

package Gtk.Column_View_Sorter is

   type Gtk_Column_View_Sorter_Record is new Gtk_Sorter_Record with null record;
   type Gtk_Column_View_Sorter is access all Gtk_Column_View_Sorter_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_column_view_sorter_get_type");

   -------------
   -- Methods --
   -------------

   function Get_N_Sort_Columns
      (Self : not null access Gtk_Column_View_Sorter_Record) return Guint;
   --  Returns the number of columns by which the sorter sorts.
   --  If the sorter of the primary sort column does not determine a total
   --  order, then the secondary sorters are consulted to break the ties.
   --  Use the [signalGtk.Sorter::changed] signal to get notified when the
   --  number of sort columns changes.
   --  Since: gtk+ 4.10
   --  @return the number of sort columns

   function Get_Nth_Sort_Column
      (Self       : not null access Gtk_Column_View_Sorter_Record;
       Position   : Guint;
       Sort_Order : out Gtk.Enums.Gtk_Sort_Type)
       return Gtk.Column_View_Column.Gtk_Column_View_Column;
   --  Gets the Position'th sort column and its associated sort order.
   --  Use the [signalGtk.Sorter::changed] signal to get notified when sort
   --  columns change.
   --  Since: gtk+ 4.10
   --  @param Position the position of the sort column to retrieve (0 for the
   --  primary sort column)
   --  @param Sort_Order return location for the sort order
   --  @return the sort column at the Position
   --  Return has transfer-ownership='none'

   function Get_Primary_Sort_Column
      (Self : not null access Gtk_Column_View_Sorter_Record)
       return Gtk.Column_View_Column.Gtk_Column_View_Column;
   --  Returns the primary sort column.
   --  The primary sort column is the one that displays the triangle in a
   --  column view header.
   --  Since: gtk+ 4.10
   --  @return the primary sort column
   --  Return has transfer-ownership='none'

   function Get_Primary_Sort_Order
      (Self : not null access Gtk_Column_View_Sorter_Record)
       return Gtk.Enums.Gtk_Sort_Type;
   --  Returns the primary sort order.
   --  The primary sort order determines whether the triangle displayed in the
   --  column view header of the primary sort column points upwards or
   --  downwards.
   --  If there is no primary sort column, then this function returns
   --  `GTK_SORT_ASCENDING`.
   --  Since: gtk+ 4.10
   --  @return the primary sort order

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Primary_Sort_Column_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Column_View_Column.Gtk_Column_View_Column
   --  The primary sort column.
   --
   --  The primary sort column is the one that displays the triangle in a
   --  column view header.

   Primary_Sort_Order_Property : constant Gtk.Enums.Property_Gtk_Sort_Type;
   --  The primary sort order.
   --
   --  The primary sort order determines whether the triangle displayed in the
   --  column view header of the primary sort column points upwards or
   --  downwards.

private
   Primary_Sort_Order_Property : constant Gtk.Enums.Property_Gtk_Sort_Type :=
     Gtk.Enums.Build ("primary-sort-order");
   Primary_Sort_Column_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("primary-sort-column");
end Gtk.Column_View_Sorter;
