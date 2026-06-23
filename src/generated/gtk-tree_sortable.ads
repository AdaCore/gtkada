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

--  The interface for sortable models used by GtkTreeView
--
--  `GtkTreeSortable` is an interface to be implemented by tree models which
--  support sorting. The `GtkTreeView` uses the methods provided by this
--  interface to sort the model.
--
--  <group>Trees and Lists</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Glib.Object;    use Glib.Object;
with Glib.Types;     use Glib.Types;
with Gtk.Enums;      use Gtk.Enums;
with Gtk.Tree_Model; use Gtk.Tree_Model;

package Gtk.Tree_Sortable is

   pragma Obsolescent;
   --  There is no replacement for this interface. You should use [class@Gtk.SortListModel] to wrap your list model instead

   type Gtk_Tree_Sortable is new Glib.Types.GType_Interface;
   Null_Gtk_Tree_Sortable : constant Gtk_Tree_Sortable;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Tree_Iter_Compare_Func is access function
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gint;
   --  A GtkTreeIterCompareFunc should return a negative integer, zero, or a
   --  positive integer if A sorts before B, A sorts with B, or A sorts after B
   --  respectively.
   --  If two iters compare as equal, their order in the sorted model is
   --  undefined. In order to ensure that the `GtkTreeSortable` behaves as
   --  expected, the GtkTreeIterCompareFunc must define a partial order on the
   --  model, i.e. it must be reflexive, antisymmetric and transitive.
   --  For example, if Model is a product catalogue, then a compare function
   --  for the "price" column could be one which returns `price_of(A) -
   --  price_of(B)`.
   --  @param Model The `GtkTreeModel` the comparison is within
   --  @param A A `GtkTreeIter` in Model
   --  @param B Another `GtkTreeIter` in Model
   --  @return a negative integer, zero or a positive integer depending on
   --  whether A sorts before, with or after B

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tree_sortable_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Sort_Column_Id
      (Self           : Gtk_Tree_Sortable;
       Sort_Column_Id : access Glib.Gint;
       Order          : access Gtk.Enums.Gtk_Sort_Type) return Boolean;
   pragma Obsolescent (Get_Sort_Column_Id);
   --  Fills in Sort_Column_Id and Order with the current sort column and the
   --  order. It returns True unless the Sort_Column_Id is
   --  GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID or
   --  GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID.
   --  Deprecated since 4.10, 1
   --  @param Sort_Column_Id The sort column id to be filled in
   --  @param Order The `GtkSortType` to be filled in
   --  @return True if the sort column is not one of the special sort column
   --  ids.

   procedure Set_Sort_Column_Id
      (Self           : Gtk_Tree_Sortable;
       Sort_Column_Id : Glib.Gint;
       Order          : Gtk.Enums.Gtk_Sort_Type);
   pragma Import (C, Set_Sort_Column_Id, "gtk_tree_sortable_set_sort_column_id");
   pragma Obsolescent (Set_Sort_Column_Id);
   --  Sets the current sort column to be Sort_Column_Id. The Sortable will
   --  resort itself to reflect this change, after emitting a
   --  `GtkTreeSortable::sort-column-changed` signal. Sort_Column_Id may either
   --  be a regular column id, or one of the following special values:
   --  - GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID: the default sort function
   --  will be used, if it is set
   --  - GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID: no sorting will occur
   --  Deprecated since 4.10, 1
   --  @param Sort_Column_Id the sort column id to set
   --  @param Order The sort order of the column

   function Has_Default_Sort_Func (Self : Gtk_Tree_Sortable) return Boolean;
   pragma Obsolescent (Has_Default_Sort_Func);
   --  Returns True if the model has a default sort function. This is used
   --  primarily by GtkTreeViewColumns in order to determine if a model can go
   --  back to the default state, or not.
   --  Deprecated since 4.10, 1
   --  @return True, if the model has a default sort function

   procedure Set_Default_Sort_Func
      (Self      : Gtk_Tree_Sortable;
       Sort_Func : Gtk_Tree_Iter_Compare_Func);
   pragma Obsolescent (Set_Default_Sort_Func);
   --  Sets the default comparison function used when sorting to be Sort_Func.
   --  If the current sort column id of Sortable is
   --  GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID, then the model will sort using
   --  this function.
   --  If Sort_Func is null, then there will be no default comparison
   --  function. This means that once the model has been sorted, it can't go
   --  back to the default state. In this case, when the current sort column id
   --  of Sortable is GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID, the model will
   --  be unsorted.
   --  Deprecated since 4.10, 1
   --  @param Sort_Func The comparison function

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Default_Sort_Func_User_Data is

      type Gtk_Tree_Iter_Compare_Func is access function
        (Model     : Gtk.Tree_Model.Gtk_Tree_Model;
         A         : Gtk.Tree_Model.Gtk_Tree_Iter;
         B         : Gtk.Tree_Model.Gtk_Tree_Iter;
         User_Data : User_Data_Type) return Glib.Gint;
      --  A GtkTreeIterCompareFunc should return a negative integer, zero, or a
      --  positive integer if A sorts before B, A sorts with B, or A sorts after B
      --  respectively.
      --  If two iters compare as equal, their order in the sorted model is
      --  undefined. In order to ensure that the `GtkTreeSortable` behaves as
      --  expected, the GtkTreeIterCompareFunc must define a partial order on the
      --  model, i.e. it must be reflexive, antisymmetric and transitive.
      --  For example, if Model is a product catalogue, then a compare function
      --  for the "price" column could be one which returns `price_of(A) -
      --  price_of(B)`.
      --  @param Model The `GtkTreeModel` the comparison is within
      --  @param A A `GtkTreeIter` in Model
      --  @param B Another `GtkTreeIter` in Model
      --  @param User_Data Data passed when the compare func is assigned e.g. by
      --  gtk_tree_sortable_set_sort_func
      --  @return a negative integer, zero or a positive integer depending on
      --  whether A sorts before, with or after B

      procedure Set_Default_Sort_Func
         (Self      : Gtk.Tree_Sortable.Gtk_Tree_Sortable;
          Sort_Func : Gtk_Tree_Iter_Compare_Func;
          User_Data : User_Data_Type);
      pragma Obsolescent (Set_Default_Sort_Func);
      --  Sets the default comparison function used when sorting to be
      --  Sort_Func. If the current sort column id of Sortable is
      --  GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID, then the model will sort
      --  using this function.
      --  If Sort_Func is null, then there will be no default comparison
      --  function. This means that once the model has been sorted, it can't go
      --  back to the default state. In this case, when the current sort column
      --  id of Sortable is GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID, the model
      --  will be unsorted.
      --  Deprecated since 4.10, 1
      --  @param Sort_Func The comparison function
      --  @param User_Data User data to pass to Sort_Func

   end Set_Default_Sort_Func_User_Data;

   procedure Set_Sort_Func
      (Self           : Gtk_Tree_Sortable;
       Sort_Column_Id : Glib.Gint;
       Sort_Func      : Gtk_Tree_Iter_Compare_Func);
   pragma Obsolescent (Set_Sort_Func);
   --  Sets the comparison function used when sorting to be Sort_Func. If the
   --  current sort column id of Sortable is the same as Sort_Column_Id, then
   --  the model will sort using this function.
   --  Deprecated since 4.10, 1
   --  @param Sort_Column_Id the sort column id to set the function for
   --  @param Sort_Func The comparison function

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Sort_Func_User_Data is

      type Gtk_Tree_Iter_Compare_Func is access function
        (Model     : Gtk.Tree_Model.Gtk_Tree_Model;
         A         : Gtk.Tree_Model.Gtk_Tree_Iter;
         B         : Gtk.Tree_Model.Gtk_Tree_Iter;
         User_Data : User_Data_Type) return Glib.Gint;
      --  A GtkTreeIterCompareFunc should return a negative integer, zero, or a
      --  positive integer if A sorts before B, A sorts with B, or A sorts after B
      --  respectively.
      --  If two iters compare as equal, their order in the sorted model is
      --  undefined. In order to ensure that the `GtkTreeSortable` behaves as
      --  expected, the GtkTreeIterCompareFunc must define a partial order on the
      --  model, i.e. it must be reflexive, antisymmetric and transitive.
      --  For example, if Model is a product catalogue, then a compare function
      --  for the "price" column could be one which returns `price_of(A) -
      --  price_of(B)`.
      --  @param Model The `GtkTreeModel` the comparison is within
      --  @param A A `GtkTreeIter` in Model
      --  @param B Another `GtkTreeIter` in Model
      --  @param User_Data Data passed when the compare func is assigned e.g. by
      --  Gtk.Tree_Sortable.Set_Sort_Func
      --  @return a negative integer, zero or a positive integer depending on
      --  whether A sorts before, with or after B

      procedure Set_Sort_Func
         (Self           : Gtk.Tree_Sortable.Gtk_Tree_Sortable;
          Sort_Column_Id : Glib.Gint;
          Sort_Func      : Gtk_Tree_Iter_Compare_Func;
          User_Data      : User_Data_Type);
      pragma Obsolescent (Set_Sort_Func);
      --  Sets the comparison function used when sorting to be Sort_Func. If
      --  the current sort column id of Sortable is the same as Sort_Column_Id,
      --  then the model will sort using this function.
      --  Deprecated since 4.10, 1
      --  @param Sort_Column_Id the sort column id to set the function for
      --  @param Sort_Func The comparison function
      --  @param User_Data User data to pass to Sort_Func

   end Set_Sort_Func_User_Data;

   procedure Sort_Column_Changed (Self : Gtk_Tree_Sortable);
   pragma Import (C, Sort_Column_Changed, "gtk_tree_sortable_sort_column_changed");
   pragma Obsolescent (Sort_Column_Changed);
   --  Emits a `GtkTreeSortable::sort-column-changed` signal on Sortable.
   --  Deprecated since 4.10, 1

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Tree_Sortable_Void is not null access procedure (Self : Gtk_Tree_Sortable);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Sort_Column_Changed : constant Glib.Signal_Name := "sort-column-changed";
   procedure On_Sort_Column_Changed
      (Self  : Gtk_Tree_Sortable;
       Call  : Cb_Gtk_Tree_Sortable_Void;
       After : Boolean := False);
   procedure On_Sort_Column_Changed
      (Self  : Gtk_Tree_Sortable;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::sort-column-changed signal is emitted when the sort column or
   --  sort order of Sortable is changed. The signal is emitted before the
   --  contents of Sortable are resorted.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Tree_Sortable"

   function "+" (W : Gtk_Tree_Sortable) return Gtk_Tree_Sortable;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Get_Sort_Column_Id is access function
     (Self           : Gtk_Tree_Sortable;
      Sort_Column_Id : access Glib.Gint;
      Order          : access Gtk.Enums.Gtk_Sort_Type) return Glib.Gboolean;
   pragma Obsolescent (Virtual_Get_Sort_Column_Id);
   pragma Convention (C, Virtual_Get_Sort_Column_Id);
   --  Fills in Sort_Column_Id and Order with the current sort column and the
   --  order. It returns True unless the Sort_Column_Id is
   --  GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID or
   --  GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID.
   --  Deprecated since 4.10, 1
   --  @param Sort_Column_Id The sort column id to be filled in
   --  @param Order The `GtkSortType` to be filled in
   --  @return True if the sort column is not one of the special sort column
   --  ids.

   type Virtual_Has_Default_Sort_Func is access function (Self : Gtk_Tree_Sortable) return Glib.Gboolean;
   pragma Obsolescent (Virtual_Has_Default_Sort_Func);
   pragma Convention (C, Virtual_Has_Default_Sort_Func);
   --  Returns True if the model has a default sort function. This is used
   --  primarily by GtkTreeViewColumns in order to determine if a model can go
   --  back to the default state, or not.
   --  Deprecated since 4.10, 1
   --  @return True, if the model has a default sort function

   type Virtual_Set_Default_Sort_Func is access procedure
     (Self      : Gtk_Tree_Sortable;
      Sort_Func : System.Address;
      User_Data : System.Address;
      Destroy   : Glib.G_Destroy_Notify_Address);
   pragma Obsolescent (Virtual_Set_Default_Sort_Func);
   pragma Convention (C, Virtual_Set_Default_Sort_Func);
   --  Sets the default comparison function used when sorting to be Sort_Func.
   --  If the current sort column id of Sortable is
   --  GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID, then the model will sort using
   --  this function.
   --  If Sort_Func is null, then there will be no default comparison
   --  function. This means that once the model has been sorted, it can't go
   --  back to the default state. In this case, when the current sort column id
   --  of Sortable is GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID, the model will
   --  be unsorted.
   --  Deprecated since 4.10, 1
   --  @param Sort_Func The comparison function
   --  @param User_Data User data to pass to Sort_Func
   --  @param Destroy Destroy notifier of User_Data

   type Virtual_Set_Sort_Column_Id is access procedure
     (Self           : Gtk_Tree_Sortable;
      Sort_Column_Id : Glib.Gint;
      Order          : Gtk.Enums.Gtk_Sort_Type);
   pragma Obsolescent (Virtual_Set_Sort_Column_Id);
   pragma Convention (C, Virtual_Set_Sort_Column_Id);
   --  Sets the current sort column to be Sort_Column_Id. The Sortable will
   --  resort itself to reflect this change, after emitting a
   --  `GtkTreeSortable::sort-column-changed` signal. Sort_Column_Id may either
   --  be a regular column id, or one of the following special values:
   --  - GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID: the default sort function
   --  will be used, if it is set
   --  - GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID: no sorting will occur
   --  Deprecated since 4.10, 1
   --  @param Sort_Column_Id the sort column id to set
   --  @param Order The sort order of the column

   type Virtual_Set_Sort_Func is access procedure
     (Self           : Gtk_Tree_Sortable;
      Sort_Column_Id : Glib.Gint;
      Sort_Func      : System.Address;
      User_Data      : System.Address;
      Destroy        : Glib.G_Destroy_Notify_Address);
   pragma Obsolescent (Virtual_Set_Sort_Func);
   pragma Convention (C, Virtual_Set_Sort_Func);
   --  Sets the comparison function used when sorting to be Sort_Func. If the
   --  current sort column id of Sortable is the same as Sort_Column_Id, then
   --  the model will sort using this function.
   --  Deprecated since 4.10, 1
   --  @param Sort_Column_Id the sort column id to set the function for
   --  @param Sort_Func The comparison function
   --  @param User_Data User data to pass to Sort_Func
   --  @param Destroy Destroy notifier of User_Data

   type Virtual_Sort_Column_Changed is access procedure (Self : Gtk_Tree_Sortable);
   pragma Obsolescent (Virtual_Sort_Column_Changed);
   pragma Convention (C, Virtual_Sort_Column_Changed);
   --  Emits a `GtkTreeSortable::sort-column-changed` signal on Sortable.
   --  Deprecated since 4.10, 1

   subtype Tree_Sortable_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Get_Sort_Column_Id
     (Self    : Tree_Sortable_Interface_Descr;
      Handler : Virtual_Get_Sort_Column_Id);
   pragma Import (C, Set_Get_Sort_Column_Id, "gtkada_Tree_Sortable_set_get_sort_column_id");

   procedure Set_Has_Default_Sort_Func
     (Self    : Tree_Sortable_Interface_Descr;
      Handler : Virtual_Has_Default_Sort_Func);
   pragma Import (C, Set_Has_Default_Sort_Func, "gtkada_Tree_Sortable_set_has_default_sort_func");

   procedure Set_Set_Default_Sort_Func
     (Self    : Tree_Sortable_Interface_Descr;
      Handler : Virtual_Set_Default_Sort_Func);
   pragma Import (C, Set_Set_Default_Sort_Func, "gtkada_Tree_Sortable_set_set_default_sort_func");

   procedure Set_Set_Sort_Column_Id
     (Self    : Tree_Sortable_Interface_Descr;
      Handler : Virtual_Set_Sort_Column_Id);
   pragma Import (C, Set_Set_Sort_Column_Id, "gtkada_Tree_Sortable_set_set_sort_column_id");

   procedure Set_Set_Sort_Func
     (Self    : Tree_Sortable_Interface_Descr;
      Handler : Virtual_Set_Sort_Func);
   pragma Import (C, Set_Set_Sort_Func, "gtkada_Tree_Sortable_set_set_sort_func");

   procedure Set_Sort_Column_Changed
     (Self    : Tree_Sortable_Interface_Descr;
      Handler : Virtual_Sort_Column_Changed);
   pragma Import (C, Set_Sort_Column_Changed, "gtkada_Tree_Sortable_set_sort_column_changed");
   --  See Glib.Object.Add_Interface

private

   Null_Gtk_Tree_Sortable : constant Gtk_Tree_Sortable :=
      Gtk_Tree_Sortable (Glib.Types.Null_Interface);
end Gtk.Tree_Sortable;
