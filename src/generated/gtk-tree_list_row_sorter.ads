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

--  Applies a gives sorter to the levels in a tree.
--
--  Here is an example for setting up a column view with a tree model and a
--  `GtkTreeListSorter`:
--
--  ```c column_sorter = gtk_column_view_get_sorter (view); sorter =
--  gtk_tree_list_row_sorter_new (g_object_ref (column_sorter)); sort_model =
--  gtk_sort_list_model_new (tree_model, sorter); selection =
--  gtk_single_selection_new (sort_model); gtk_column_view_set_model (view,
--  G_LIST_MODEL (selection)); ```

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Gtk.Sorter;      use Gtk.Sorter;

package Gtk.Tree_List_Row_Sorter is

   type Gtk_Tree_List_Row_Sorter_Record is new Gtk_Sorter_Record with null record;
   type Gtk_Tree_List_Row_Sorter is access all Gtk_Tree_List_Row_Sorter_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gtk_Tree_List_Row_Sorter;
       Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class);
   procedure Initialize
      (Self   : not null access Gtk_Tree_List_Row_Sorter_Record'Class;
       Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class);
   --  Create a special-purpose sorter that applies the sorting of Sorter to
   --  the levels of a `GtkTreeListModel`.
   --  Note that this sorter relies on [propertyGtk.TreeListModel:passthrough]
   --  being False as it can only sort [classGtk.TreeListRow]s.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Sorter a `GtkSorter`

   function Gtk_Tree_List_Row_Sorter_New
      (Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class)
       return Gtk_Tree_List_Row_Sorter;
   --  Create a special-purpose sorter that applies the sorting of Sorter to
   --  the levels of a `GtkTreeListModel`.
   --  Note that this sorter relies on [propertyGtk.TreeListModel:passthrough]
   --  being False as it can only sort [classGtk.TreeListRow]s.
   --  @param Sorter a `GtkSorter`

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tree_list_row_sorter_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Sorter
      (Self : not null access Gtk_Tree_List_Row_Sorter_Record)
       return Gtk.Sorter.Gtk_Sorter;
   --  Returns the sorter used by Self.
   --  @return the sorter used
   --  Return has transfer-ownership='none'

   procedure Set_Sorter
      (Self   : not null access Gtk_Tree_List_Row_Sorter_Record;
       Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class);
   --  Sets the sorter to use for items with the same parent.
   --  This sorter will be passed the [propertyGtk.TreeListRow:item] of the
   --  tree list rows passed to Self.
   --  @param Sorter The sorter to use

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Sorter_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Sorter.Gtk_Sorter
   --  The underlying sorter

private
   Sorter_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("sorter");
end Gtk.Tree_List_Row_Sorter;
