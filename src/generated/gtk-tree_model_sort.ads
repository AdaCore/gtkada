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

--  A GtkTreeModel which makes an underlying tree model sortable
--
--  The `GtkTreeModelSort` is a model which implements the `GtkTreeSortable`
--  interface. It does not hold any data itself, but rather is created with a
--  child model and proxies its data. It has identical column types to this
--  child model, and the changes in the child are propagated. The primary
--  purpose of this model is to provide a way to sort a different model without
--  modifying it. Note that the sort function used by `GtkTreeModelSort` is not
--  guaranteed to be stable.
--
--  The use of this is best demonstrated through an example. In the following
--  sample code we create two `GtkTreeView` widgets each with a view of the
--  same data. As the model is wrapped here by a `GtkTreeModelSort`, the two
--  `GtkTreeView`s can each sort their view of the data without affecting the
--  other. By contrast, if we simply put the same model in each widget, then
--  sorting the first would sort the second.
--
--  ## Using a `GtkTreeModelSort`
--
--  ```c { GtkTreeView *tree_view1; GtkTreeView *tree_view2; GtkTreeModel
--  *sort_model1; GtkTreeModel *sort_model2; GtkTreeModel *child_model;
--
--  // get the child model child_model = get_my_model ();
--
--  // Create the first tree sort_model1 = gtk_tree_model_sort_new_with_model
--  (child_model); tree_view1 = gtk_tree_view_new_with_model (sort_model1);
--
--  // Create the second tree sort_model2 = gtk_tree_model_sort_new_with_model
--  (child_model); tree_view2 = gtk_tree_view_new_with_model (sort_model2);
--
--  // Now we can sort the two models independently
--  gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE (sort_model1),
--  COLUMN_1, GTK_SORT_ASCENDING); gtk_tree_sortable_set_sort_column_id
--  (GTK_TREE_SORTABLE (sort_model2), COLUMN_1, GTK_SORT_DESCENDING); } ```
--
--  To demonstrate how to access the underlying child model from the sort
--  model, the next example will be a callback for the `GtkTreeSelection`
--  `GtkTreeSelection::changed` signal. In this callback, we get a string from
--  COLUMN_1 of the model. We then modify the string, find the same selected
--  row on the child model, and change the row there.
--
--  ## Accessing the child model of in a selection changed callback
--
--  ```c void selection_changed (GtkTreeSelection *selection, gpointer data) {
--  GtkTreeModel *sort_model = NULL; GtkTreeModel *child_model; GtkTreeIter
--  sort_iter; GtkTreeIter child_iter; char *some_data = NULL; char
--  *modified_data;
--
--  // Get the current selected row and the model. if (!
--  gtk_tree_selection_get_selected (selection, &sort_model, &sort_iter))
--  return;
--
--  // Look up the current value on the selected row and get // a new value to
--  change it to. gtk_tree_model_get (GTK_TREE_MODEL (sort_model), &sort_iter,
--  COLUMN_1, &some_data, -1);
--
--  modified_data = change_the_data (some_data); g_free (some_data);
--
--  // Get an iterator on the child model, instead of the sort model.
--  gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT
--  (sort_model), &child_iter, &sort_iter);
--
--  // Get the child model and change the value of the row. In this //
--  example, the child model is a GtkListStore. It could be any other // type
--  of model, though. child_model = gtk_tree_model_sort_get_model
--  (GTK_TREE_MODEL_SORT (sort_model)); gtk_list_store_set (GTK_LIST_STORE
--  (child_model), &child_iter, COLUMN_1, &modified_data, -1); g_free
--  (modified_data); } ```
--
--  <group>Trees and Lists</group>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Content_Provider; use Gdk.Content_Provider;
with Glib;                 use Glib;
with Glib.Properties;      use Glib.Properties;
with Glib.Types;           use Glib.Types;
with Glib.Values;          use Glib.Values;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Tree_Drag_Source; use Gtk.Tree_Drag_Source;
with Gtk.Tree_Model;       use Gtk.Tree_Model;
with Gtk.Tree_Sortable;    use Gtk.Tree_Sortable;

package Gtk.Tree_Model_Sort is

   pragma Obsolescent;
   --  Use [class@Gtk.SortListModel] instead

   type Gtk_Tree_Model_Sort_Record is new Gtk_Root_Tree_Model_Record with null record;
   type Gtk_Tree_Model_Sort is access all Gtk_Tree_Model_Sort_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Tree_Model_Foreach_Func is access function
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  Type of the callback passed to Gtk.Tree_Model.Foreach to iterate over
   --  the rows in a tree model.
   --  @param Model the `GtkTreeModel` being iterated
   --  @param Path the current `GtkTreePath`
   --  @param Iter the current `GtkTreeIter`
   --  @return True to stop iterating, False to continue

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

   procedure Gtk_New_With_Model
      (Self        : out Gtk_Tree_Model_Sort;
       Child_Model : Gtk.Tree_Model.Gtk_Tree_Model);
   procedure Initialize_With_Model
      (Self        : not null access Gtk_Tree_Model_Sort_Record'Class;
       Child_Model : Gtk.Tree_Model.Gtk_Tree_Model);
   --  Creates a new `GtkTreeModelSort`, with Child_Model as the child model.
   --  Initialize_With_Model does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Child_Model A `GtkTreeModel`

   function Gtk_Tree_Model_Sort_New_With_Model
      (Child_Model : Gtk.Tree_Model.Gtk_Tree_Model)
       return Gtk_Tree_Model_Sort;
   --  Creates a new `GtkTreeModelSort`, with Child_Model as the child model.
   --  @param Child_Model A `GtkTreeModel`

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tree_model_sort_get_type");

   -------------
   -- Methods --
   -------------

   procedure Clear_Cache (Self : not null access Gtk_Tree_Model_Sort_Record);
   pragma Obsolescent (Clear_Cache);
   --  This function should almost never be called. It clears the
   --  Tree_Model_Sort of any cached iterators that haven't been reffed with
   --  Gtk.Tree_Model.Ref_Node. This might be useful if the child model being
   --  sorted is static (and doesn't change often) and there has been a lot of
   --  unreffed access to nodes. As a side effect of this function, all
   --  unreffed iters will be invalid.
   --  Deprecated since 4.10, 1

   function Convert_Child_Iter_To_Iter
      (Self       : not null access Gtk_Tree_Model_Sort_Record;
       Sort_Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Child_Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   pragma Obsolescent (Convert_Child_Iter_To_Iter);
   --  Sets Sort_Iter to point to the row in Tree_Model_Sort that corresponds
   --  to the row pointed at by Child_Iter. If Sort_Iter was not set, False is
   --  returned. Note: a boolean is only returned since 2.14.
   --  Deprecated since 4.10, 1
   --  @param Sort_Iter An uninitialized `GtkTreeIter`
   --  @param Child_Iter A valid `GtkTreeIter` pointing to a row on the child
   --  model
   --  @return True, if Sort_Iter was set, i.e. if Sort_Iter is a valid
   --  iterator pointer to a visible row in the child model.

   function Convert_Child_Path_To_Path
      (Self       : not null access Gtk_Tree_Model_Sort_Record;
       Child_Path : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gtk.Tree_Model.Gtk_Tree_Path;
   pragma Obsolescent (Convert_Child_Path_To_Path);
   --  Converts Child_Path to a path relative to Tree_Model_Sort. That is,
   --  Child_Path points to a path in the child model. The returned path will
   --  point to the same row in the sorted model. If Child_Path isn't a valid
   --  path on the child model, then null is returned.
   --  Deprecated since 4.10, 1
   --  @param Child_Path A `GtkTreePath` to convert
   --  @return A newly allocated `GtkTreePath`

   procedure Convert_Iter_To_Child_Iter
      (Self        : not null access Gtk_Tree_Model_Sort_Record;
       Child_Iter  : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Sorted_Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Convert_Iter_To_Child_Iter);
   --  Sets Child_Iter to point to the row pointed to by Sorted_Iter.
   --  Deprecated since 4.10, 1
   --  @param Child_Iter An uninitialized `GtkTreeIter`
   --  @param Sorted_Iter A valid `GtkTreeIter` pointing to a row on
   --  Tree_Model_Sort.

   function Convert_Path_To_Child_Path
      (Self        : not null access Gtk_Tree_Model_Sort_Record;
       Sorted_Path : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gtk.Tree_Model.Gtk_Tree_Path;
   pragma Obsolescent (Convert_Path_To_Child_Path);
   --  Converts Sorted_Path to a path on the child model of Tree_Model_Sort.
   --  That is, Sorted_Path points to a location in Tree_Model_Sort. The
   --  returned path will point to the same location in the model not being
   --  sorted. If Sorted_Path does not point to a location in the child model,
   --  null is returned.
   --  Deprecated since 4.10, 1
   --  @param Sorted_Path A `GtkTreePath` to convert
   --  @return A newly allocated `GtkTreePath`

   function Get_Model
      (Self : not null access Gtk_Tree_Model_Sort_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Returns the model the `GtkTreeModelSort` is sorting.
   --  @return the "child model" being sorted

   function Iter_Is_Valid
      (Self : not null access Gtk_Tree_Model_Sort_Record;
       Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   pragma Obsolescent (Iter_Is_Valid);
   --  > This function is slow. Only use it for debugging and/or testing >
   --  purposes.
   --  Checks if the given iter is a valid iter for this `GtkTreeModelSort`.
   --  Deprecated since 4.10, 1
   --  @param Iter A `GtkTreeIter`
   --  @return True if the iter is valid, False if the iter is invalid.

   procedure Reset_Default_Sort_Func
      (Self : not null access Gtk_Tree_Model_Sort_Record);
   pragma Obsolescent (Reset_Default_Sort_Func);
   --  This resets the default sort function to be in the "unsorted" state.
   --  That is, it is in the same order as the child model. It will re-sort the
   --  model to be in the same order as the child model only if the
   --  `GtkTreeModelSort` is in "unsorted" state.
   --  Deprecated since 4.10, 1

   procedure Foreach
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Func       : Gtk_Tree_Model_Foreach_Func);
   pragma Obsolescent (Foreach);
   --  Calls Func on each node in model in a depth-first fashion.
   --  If Func returns True, then the tree ceases to be walked, and
   --  Gtk.Tree_Model.Foreach returns.
   --  Deprecated since 4.10, 1
   --  @param Func a function to be called on each row

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Foreach_User_Data is

      type Gtk_Tree_Model_Foreach_Func is access function
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : User_Data_Type) return Boolean;
      --  Type of the callback passed to Gtk.Tree_Model.Foreach to iterate over
      --  the rows in a tree model.
      --  @param Model the `GtkTreeModel` being iterated
      --  @param Path the current `GtkTreePath`
      --  @param Iter the current `GtkTreeIter`
      --  @param Data The user data passed to Gtk.Tree_Model.Foreach
      --  @return True to stop iterating, False to continue

      procedure Foreach
         (Tree_Model : not null access Gtk.Tree_Model_Sort.Gtk_Tree_Model_Sort_Record'Class;
          Func       : Gtk_Tree_Model_Foreach_Func;
          User_Data  : User_Data_Type);
      pragma Obsolescent (Foreach);
      --  Calls Func on each node in model in a depth-first fashion.
      --  If Func returns True, then the tree ceases to be walked, and
      --  Gtk.Tree_Model.Foreach returns.
      --  Deprecated since 4.10, 1
      --  @param Func a function to be called on each row
      --  @param User_Data user data to passed to Func

   end Foreach_User_Data;

   procedure Set_Default_Sort_Func
      (Self      : not null access Gtk_Tree_Model_Sort_Record;
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
      --  Gtk.Tree_Sortable.Set_Sort_Func
      --  @return a negative integer, zero or a positive integer depending on
      --  whether A sorts before, with or after B

      procedure Set_Default_Sort_Func
         (Self      : not null access Gtk.Tree_Model_Sort.Gtk_Tree_Model_Sort_Record'Class;
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
      (Self           : not null access Gtk_Tree_Model_Sort_Record;
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
         (Self           : not null access Gtk.Tree_Model_Sort.Gtk_Tree_Model_Sort_Record'Class;
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

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   function Drag_Data_Delete
      (Self : not null access Gtk_Tree_Model_Sort_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   pragma Obsolescent (Drag_Data_Delete);

   function Drag_Data_Get
      (Self : not null access Gtk_Tree_Model_Sort_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gdk.Content_Provider.Gdk_Content_Provider;
   pragma Obsolescent (Drag_Data_Get);

   function Row_Draggable
      (Self : not null access Gtk_Tree_Model_Sort_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   pragma Obsolescent (Row_Draggable);

   function Get_Column_Type
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Index      : Glib.Gint) return GType;
   pragma Obsolescent (Get_Column_Type);

   function Get_Flags
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record)
       return Gtk.Tree_Model.Tree_Model_Flags;
   pragma Obsolescent (Get_Flags);

   function Get_Iter
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Get_Iter);

   function Get_Iter_First
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record)
       return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Get_Iter_First);

   function Get_Iter_From_String
      (Tree_Model  : not null access Gtk_Tree_Model_Sort_Record;
       Path_String : UTF8_String) return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Get_Iter_From_String);

   function Get_N_Columns
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record)
       return Glib.Gint;
   pragma Obsolescent (Get_N_Columns);

   function Get_Path
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Path;
   pragma Obsolescent (Get_Path);

   function Get_String_From_Iter
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return UTF8_String;
   pragma Obsolescent (Get_String_From_Iter);

   procedure Get_Value
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Column     : Glib.Gint;
       Value      : out Glib.Values.GValue);
   pragma Obsolescent (Get_Value);

   function Children
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Children);

   function Has_Child
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   pragma Obsolescent (Has_Child);

   function N_Children
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
       return Glib.Gint;
   pragma Obsolescent (N_Children);

   procedure Next
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Next);

   function Nth_Child
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
       N          : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Nth_Child);

   function Parent
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Child      : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Parent);

   procedure Previous
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Previous);

   procedure Ref_Node
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Ref_Node);

   procedure Row_Changed
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Row_Changed);

   procedure Row_Deleted
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path);
   pragma Obsolescent (Row_Deleted);

   procedure Row_Has_Child_Toggled
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Row_Has_Child_Toggled);

   procedure Row_Inserted
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Row_Inserted);

   procedure Rows_Reordered
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       New_Order  : Gint_Array);
   pragma Obsolescent (Rows_Reordered);

   procedure Rows_Reordered_With_Length
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       New_Order  : Gint_Array;
       Length     : Glib.Gint);
   pragma Obsolescent (Rows_Reordered_With_Length);

   procedure Unref_Node
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Unref_Node);

   function Get_Sort_Column_Id
      (Self           : not null access Gtk_Tree_Model_Sort_Record;
       Sort_Column_Id : access Glib.Gint;
       Order          : access Gtk.Enums.Gtk_Sort_Type) return Boolean;
   pragma Obsolescent (Get_Sort_Column_Id);

   procedure Set_Sort_Column_Id
      (Self           : not null access Gtk_Tree_Model_Sort_Record;
       Sort_Column_Id : Glib.Gint;
       Order          : Gtk.Enums.Gtk_Sort_Type);
   pragma Obsolescent (Set_Sort_Column_Id);

   function Has_Default_Sort_Func
      (Self : not null access Gtk_Tree_Model_Sort_Record) return Boolean;
   pragma Obsolescent (Has_Default_Sort_Func);

   procedure Sort_Column_Changed
      (Self : not null access Gtk_Tree_Model_Sort_Record);
   pragma Obsolescent (Sort_Column_Changed);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Model_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Tree_Model.Gtk_Tree_Model
   --  The model of the tree model sort.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.TreeDragSource"
   --
   --  - "Gtk.TreeModel"
   --
   --  - "Gtk.TreeSortable"

   package Implements_Gtk_Tree_Drag_Source is new Glib.Types.Implements
     (Gtk.Tree_Drag_Source.Gtk_Tree_Drag_Source, Gtk_Tree_Model_Sort_Record, Gtk_Tree_Model_Sort);
   function "+"
     (Widget : access Gtk_Tree_Model_Sort_Record'Class)
   return Gtk.Tree_Drag_Source.Gtk_Tree_Drag_Source
   renames Implements_Gtk_Tree_Drag_Source.To_Interface;
   function "-"
     (Interf : Gtk.Tree_Drag_Source.Gtk_Tree_Drag_Source)
   return Gtk_Tree_Model_Sort
   renames Implements_Gtk_Tree_Drag_Source.To_Object;

   package Implements_Gtk_Tree_Model is new Glib.Types.Implements
     (Gtk.Tree_Model.Gtk_Tree_Model, Gtk_Tree_Model_Sort_Record, Gtk_Tree_Model_Sort);
   function "+"
     (Widget : access Gtk_Tree_Model_Sort_Record'Class)
   return Gtk.Tree_Model.Gtk_Tree_Model
   renames Implements_Gtk_Tree_Model.To_Interface;
   function "-"
     (Interf : Gtk.Tree_Model.Gtk_Tree_Model)
   return Gtk_Tree_Model_Sort
   renames Implements_Gtk_Tree_Model.To_Object;

   package Implements_Gtk_Tree_Sortable is new Glib.Types.Implements
     (Gtk.Tree_Sortable.Gtk_Tree_Sortable, Gtk_Tree_Model_Sort_Record, Gtk_Tree_Model_Sort);
   function "+"
     (Widget : access Gtk_Tree_Model_Sort_Record'Class)
   return Gtk.Tree_Sortable.Gtk_Tree_Sortable
   renames Implements_Gtk_Tree_Sortable.To_Interface;
   function "-"
     (Interf : Gtk.Tree_Sortable.Gtk_Tree_Sortable)
   return Gtk_Tree_Model_Sort
   renames Implements_Gtk_Tree_Sortable.To_Object;

private
   Model_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("model");
end Gtk.Tree_Model_Sort;
