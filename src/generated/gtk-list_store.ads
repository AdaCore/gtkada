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

--  A list-like data structure that can be used with the [classGtk.TreeView].
--
--  The `GtkListStore` object is a list model for use with a `GtkTreeView`
--  widget. It implements the `GtkTreeModel` interface, and consequentialy, can
--  use all of the methods available there. It also implements the
--  `GtkTreeSortable` interface so it can be sorted by the view. Finally, it
--  also implements the tree [drag](iface.TreeDragSource.html) and
--  [drop](iface.TreeDragDest.html) interfaces.
--
--  The `GtkListStore` can accept most `GType`s as a column type, though it
--  can't accept all custom types. Internally, it will keep a copy of data
--  passed in (such as a string or a boxed pointer). Columns that accept
--  `GObject`s are handled a little differently. The `GtkListStore` will keep a
--  reference to the object instead of copying the value. As a result, if the
--  object is modified, it is up to the application writer to call
--  [methodGtk.TreeModel.row_changed] to emit the
--  [signalGtk.TreeModel::row_changed] signal. This most commonly affects lists
--  with [classGdk.Texture]s stored.
--
--  An example for creating a simple list store:
--
--  ```c enum { COLUMN_STRING, COLUMN_INT, COLUMN_BOOLEAN, N_COLUMNS };
--
--  { GtkListStore *list_store; GtkTreePath *path; GtkTreeIter iter; int i;
--
--  list_store = gtk_list_store_new (N_COLUMNS, G_TYPE_STRING, G_TYPE_INT,
--  G_TYPE_BOOLEAN);
--
--  for (i = 0; i < 10; i++) { char *some_data;
--
--  some_data = get_some_data (i);
--
--  // Add a new row to the model gtk_list_store_append (list_store, &iter);
--  gtk_list_store_set (list_store, &iter, COLUMN_STRING, some_data,
--  COLUMN_INT, i, COLUMN_BOOLEAN, FALSE, -1);
--
--  // As the store will keep a copy of the string internally, // we free
--  some_data. g_free (some_data); }
--
--  // Modify a particular row path = gtk_tree_path_new_from_string ("4");
--  gtk_tree_model_get_iter (GTK_TREE_MODEL (list_store), &iter, path);
--  gtk_tree_path_free (path); gtk_list_store_set (list_store, &iter,
--  COLUMN_BOOLEAN, TRUE, -1); } ```
--
--  `GtkListStore` is deprecated since GTK 4.10, and should not be used in
--  newly written code. You should use [classGio.ListStore] instead, and the
--  various list models provided by GTK.
--
--  ## Performance Considerations
--
--  Internally, the `GtkListStore` was originally implemented with a linked
--  list with a tail pointer. As a result, it was fast at data insertion and
--  deletion, and not fast at random data access. The `GtkListStore` sets the
--  `GTK_TREE_MODEL_ITERS_PERSIST` flag, which means that `GtkTreeIter`s can be
--  cached while the row exists. Thus, if access to a particular row is needed
--  often and your code is expected to run on older versions of GTK, it is
--  worth keeping the iter around.
--
--  ## Atomic Operations
--
--  It is important to note that only the methods
--  gtk_list_store_insert_with_values and gtk_list_store_insert_with_valuesv
--  are atomic, in the sense that the row is being appended to the store and
--  the values filled in in a single operation with regard to `GtkTreeModel`
--  signaling. In contrast, using e.g. Gtk.List_Store.Append and then
--  gtk_list_store_set will first create a row, which triggers the
--  `GtkTreeModel::row-inserted` signal on `GtkListStore`. The row, however, is
--  still empty, and any signal handler connecting to
--  `GtkTreeModel::row-inserted` on this particular store should be prepared
--  for the situation that the row might be empty. This is especially important
--  if you are wrapping the `GtkListStore` inside a `GtkTreeModel`Filter and
--  are using a `GtkTreeModel`FilterVisibleFunc. Using any of the non-atomic
--  operations to append rows to the `GtkListStore` will cause the
--  `GtkTreeModel`FilterVisibleFunc to be visited with an empty row first; the
--  function must be prepared for that.
--
--  ## GtkListStore as GtkBuildable
--
--  The GtkListStore implementation of the [ifaceGtk.Buildable] interface
--  allows to specify the model columns with a `<columns>` element that may
--  contain multiple `<column>` elements, each specifying one model column. The
--  "type" attribute specifies the data type for the column.
--
--  Additionally, it is possible to specify content for the list store in the
--  UI definition, with the `<data>` element. It can contain multiple `<row>`
--  elements, each specifying to content for one row of the list model. Inside
--  a `<row>`, the `<col>` elements specify the content for individual cells.
--
--  Note that it is probably more common to define your models in the code,
--  and one might consider it a layering violation to specify the content of a
--  list store in a UI definition, data, not presentation, and common wisdom is
--  to separate the two, as far as possible.
--
--  An example of a UI Definition fragment for a list store:
--
--  ```xml <object class="GtkListStore"> <columns> <column type="gchararray"/>
--  <column type="gchararray"/> <column type="gint"/> </columns> <data> <row>
--  <col id="0">John</col> <col id="1">Doe</col> <col id="2">25</col> </row>
--  <row> <col id="0">Johan</col> <col id="1">Dahlin</col> <col id="2">50</col>
--  </row> </data> </object> ```

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Content_Provider; use Gdk.Content_Provider;
with Glib;                 use Glib;
with Glib.Types;           use Glib.Types;
with Glib.Values;          use Glib.Values;
with Gtk.Buildable;        use Gtk.Buildable;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Tree_Drag_Dest;   use Gtk.Tree_Drag_Dest;
with Gtk.Tree_Drag_Source; use Gtk.Tree_Drag_Source;
with Gtk.Tree_Model;       use Gtk.Tree_Model;
with Gtk.Tree_Sortable;    use Gtk.Tree_Sortable;

package Gtk.List_Store is

   pragma Obsolescent;
   --  Use [class@Gio.ListStore] instead

   type Gtk_List_Store_Record is new Gtk_Root_Tree_Model_Record with null record;
   type Gtk_List_Store is access all Gtk_List_Store_Record'Class;

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

   procedure Gtk_New (Self : out Gtk_List_Store; Types : GType_Array);
   procedure Initialize
      (Self  : not null access Gtk_List_Store_Record'Class;
       Types : GType_Array);
   --  Creates a new `GtkListStore`.
   --  This function is meant to be used by language bindings.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Types an array of `GType` types for the columns, from first to
   --  last

   function Gtk_List_Store_Newv (Types : GType_Array) return Gtk_List_Store;
   --  Creates a new `GtkListStore`.
   --  This function is meant to be used by language bindings.
   --  @param Types an array of `GType` types for the columns, from first to
   --  last

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_list_store_get_type");

   -------------
   -- Methods --
   -------------

   procedure Append
      (Self : not null access Gtk_List_Store_Record;
       Iter : out Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Append);
   --  Appends a new row to List_Store. Iter will be changed to point to this
   --  new row. The row will be empty after this function is called. To fill in
   --  values, you need to call gtk_list_store_set or Gtk.List_Store.Set_Value.
   --  Deprecated since 4.10, 1
   --  @param Iter An unset `GtkTreeIter` to set to the appended row

   procedure Clear (Self : not null access Gtk_List_Store_Record);
   pragma Obsolescent (Clear);
   --  Removes all rows from the list store.
   --  Deprecated since 4.10, 1

   procedure Insert
      (Self     : not null access Gtk_List_Store_Record;
       Iter     : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Position : Glib.Gint);
   pragma Obsolescent (Insert);
   --  Creates a new row at Position. Iter will be changed to point to this
   --  new row. If Position is -1 or is larger than the number of rows on the
   --  list, then the new row will be appended to the list. The row will be
   --  empty after this function is called. To fill in values, you need to call
   --  gtk_list_store_set or Gtk.List_Store.Set_Value.
   --  Deprecated since 4.10, 1
   --  @param Iter An unset `GtkTreeIter` to set to the new row
   --  @param Position position to insert the new row, or -1 for last

   procedure Insert_After
      (Self    : not null access Gtk_List_Store_Record;
       Iter    : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Sibling : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Insert_After);
   --  Inserts a new row after Sibling. If Sibling is null, then the row will
   --  be prepended to the beginning of the list. Iter will be changed to point
   --  to this new row. The row will be empty after this function is called. To
   --  fill in values, you need to call gtk_list_store_set or
   --  Gtk.List_Store.Set_Value.
   --  Deprecated since 4.10, 1
   --  @param Iter An unset `GtkTreeIter` to set to the new row
   --  @param Sibling A valid `GtkTreeIter`

   procedure Insert_Before
      (Self    : not null access Gtk_List_Store_Record;
       Iter    : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Sibling : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Insert_Before);
   --  Inserts a new row before Sibling. If Sibling is null, then the row will
   --  be appended to the end of the list. Iter will be changed to point to
   --  this new row. The row will be empty after this function is called. To
   --  fill in values, you need to call gtk_list_store_set or
   --  Gtk.List_Store.Set_Value.
   --  Deprecated since 4.10, 1
   --  @param Iter An unset `GtkTreeIter` to set to the new row
   --  @param Sibling A valid `GtkTreeIter`

   function Iter_Is_Valid
      (Self : not null access Gtk_List_Store_Record;
       Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   pragma Obsolescent (Iter_Is_Valid);
   --  Checks if the given iter is a valid iter for this `GtkListStore`.
   --  This function is slow. Only use it for debugging and/or testing
   --  purposes.
   --  Deprecated since 4.10, 1
   --  @param Iter the iterator to check
   --  @return True if the iter is valid, False if the iter is invalid.

   procedure Move_After
      (Self     : not null access Gtk_List_Store_Record;
       Iter     : Gtk.Tree_Model.Gtk_Tree_Iter;
       Position : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Move_After);
   --  Moves Iter in Store to the position after Position. Note that this
   --  function only works with unsorted stores. If Position is null, Iter will
   --  be moved to the start of the list.
   --  Deprecated since 4.10, 1
   --  @param Iter A `GtkTreeIter`
   --  @param Position A `GtkTreeIter`

   procedure Move_Before
      (Self     : not null access Gtk_List_Store_Record;
       Iter     : Gtk.Tree_Model.Gtk_Tree_Iter;
       Position : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Move_Before);
   --  Moves Iter in Store to the position before Position. Note that this
   --  function only works with unsorted stores. If Position is null, Iter will
   --  be moved to the end of the list.
   --  Deprecated since 4.10, 1
   --  @param Iter A `GtkTreeIter`
   --  @param Position A `GtkTreeIter`

   procedure Prepend
      (Self : not null access Gtk_List_Store_Record;
       Iter : out Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Prepend);
   --  Prepends a new row to List_Store. Iter will be changed to point to this
   --  new row. The row will be empty after this function is called. To fill in
   --  values, you need to call gtk_list_store_set or Gtk.List_Store.Set_Value.
   --  Deprecated since 4.10, 1
   --  @param Iter An unset `GtkTreeIter` to set to the prepend row

   procedure Remove
      (Self : not null access Gtk_List_Store_Record;
       Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Remove);
   --  Removes the given row from the list store. After being removed, Iter is
   --  set to be the next valid row, or invalidated if it pointed to the last
   --  row in List_Store.
   --  Deprecated since 4.10, 1
   --  @param Iter A valid `GtkTreeIter`

   procedure Reorder
      (Self      : not null access Gtk_List_Store_Record;
       New_Order : Gint_Array);
   pragma Obsolescent (Reorder);
   --  Reorders Store to follow the order indicated by New_Order. Note that
   --  this function only works with unsorted stores.
   --  Deprecated since 4.10, 1
   --  @param New_Order an array of integers mapping the new position of each
   --  child to its old position before the re-ordering, i.e.
   --  New_Order`[newpos] = oldpos`. It must have exactly as many items as the
   --  list store's length.

   procedure Set_Column_Types
      (Self  : not null access Gtk_List_Store_Record;
       Types : GType_Array);
   pragma Obsolescent (Set_Column_Types);
   --  Sets the types of the columns of a list store.
   --  This function is meant primarily for objects that inherit from
   --  `GtkListStore`, and should only be used when constructing a new
   --  instance.
   --  This function cannot be called after a row has been added, or a method
   --  on the `GtkTreeModel` interface is called.
   --  Deprecated since 4.10, 1
   --  @param Types An array length n of `GType`s

   procedure Set_Value
      (Self   : not null access Gtk_List_Store_Record;
       Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
       Column : Glib.Gint;
       Value  : Glib.Values.GValue);
   pragma Obsolescent (Set_Value);
   --  Sets the data in the cell specified by Iter and Column. The type of
   --  Value must be convertible to the type of the column.
   --  Deprecated since 4.10, 1
   --  @param Iter A valid `GtkTreeIter` for the row being modified
   --  @param Column column number to modify
   --  @param Value new value for the cell

   procedure Swap
      (Self : not null access Gtk_List_Store_Record;
       A    : Gtk.Tree_Model.Gtk_Tree_Iter;
       B    : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Swap);
   --  Swaps A and B in Store. Note that this function only works with
   --  unsorted stores.
   --  Deprecated since 4.10, 1
   --  @param A A `GtkTreeIter`
   --  @param B Another `GtkTreeIter`

   procedure Foreach
      (Tree_Model : not null access Gtk_List_Store_Record;
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
         (Tree_Model : not null access Gtk.List_Store.Gtk_List_Store_Record'Class;
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
      (Self      : not null access Gtk_List_Store_Record;
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
         (Self      : not null access Gtk.List_Store.Gtk_List_Store_Record'Class;
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
      (Self           : not null access Gtk_List_Store_Record;
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
         (Self           : not null access Gtk.List_Store.Gtk_List_Store_Record'Class;
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

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Set
     (Self   : access Gtk_List_Store_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Gint;
      Value  : UTF8_String);
   procedure Set
     (Self   : access Gtk_List_Store_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Gint;
      Value  : Boolean);
   procedure Set
     (Self   : access Gtk_List_Store_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Gint;
      Value  : Gint);
   --   procedure Set
   --     (Self   : access Gtk_List_Store_Record;
   --      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
   --      Column : Gint;
   --      Value  : Gdk.Pixbuf.Gdk_Pixbuf);

   procedure Set
     (Self    : not null access Gtk_List_Store_Record;
      Iter    : Gtk.Tree_Model.Gtk_Tree_Iter;
      Columns : Glib.Gint_Array;
      Values  : Glib.Values.GValue_Array);
   pragma Precondition (Columns'Length = Values'Length);
   --  A variant of Set which takes the columns and valus as two arrays.
   --  This is more efficient when changing multiple values than calling
   --  one of the Set procedures above multiple times.

   procedure Set
     (Self   : not null access Gtk_List_Store_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Values : Glib.Values.GValue_Array);
   --  A variant of the above that is used to set all the columns.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Drag_Data_Received
      (Self  : not null access Gtk_List_Store_Record;
       Dest  : Gtk.Tree_Model.Gtk_Tree_Path;
       Value : in out Glib.Values.GValue) return Boolean;
   pragma Obsolescent (Drag_Data_Received);

   function Row_Drop_Possible
      (Self      : not null access Gtk_List_Store_Record;
       Dest_Path : Gtk.Tree_Model.Gtk_Tree_Path;
       Value     : in out Glib.Values.GValue) return Boolean;
   pragma Obsolescent (Row_Drop_Possible);

   function Drag_Data_Delete
      (Self : not null access Gtk_List_Store_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   pragma Obsolescent (Drag_Data_Delete);

   function Drag_Data_Get
      (Self : not null access Gtk_List_Store_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gdk.Content_Provider.Gdk_Content_Provider;
   pragma Obsolescent (Drag_Data_Get);

   function Row_Draggable
      (Self : not null access Gtk_List_Store_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   pragma Obsolescent (Row_Draggable);

   function Get_Column_Type
      (Tree_Model : not null access Gtk_List_Store_Record;
       Index      : Glib.Gint) return GType;
   pragma Obsolescent (Get_Column_Type);

   function Get_Flags
      (Tree_Model : not null access Gtk_List_Store_Record)
       return Gtk.Tree_Model.Tree_Model_Flags;
   pragma Obsolescent (Get_Flags);

   function Get_Iter
      (Tree_Model : not null access Gtk_List_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Get_Iter);

   function Get_Iter_First
      (Tree_Model : not null access Gtk_List_Store_Record)
       return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Get_Iter_First);

   function Get_Iter_From_String
      (Tree_Model  : not null access Gtk_List_Store_Record;
       Path_String : UTF8_String) return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Get_Iter_From_String);

   function Get_N_Columns
      (Tree_Model : not null access Gtk_List_Store_Record) return Glib.Gint;
   pragma Obsolescent (Get_N_Columns);

   function Get_Path
      (Tree_Model : not null access Gtk_List_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Path;
   pragma Obsolescent (Get_Path);

   function Get_String_From_Iter
      (Tree_Model : not null access Gtk_List_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return UTF8_String;
   pragma Obsolescent (Get_String_From_Iter);

   procedure Get_Value
      (Tree_Model : not null access Gtk_List_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Column     : Glib.Gint;
       Value      : out Glib.Values.GValue);
   pragma Obsolescent (Get_Value);

   function Children
      (Tree_Model : not null access Gtk_List_Store_Record;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Children);

   function Has_Child
      (Tree_Model : not null access Gtk_List_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   pragma Obsolescent (Has_Child);

   function N_Children
      (Tree_Model : not null access Gtk_List_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
       return Glib.Gint;
   pragma Obsolescent (N_Children);

   procedure Next
      (Tree_Model : not null access Gtk_List_Store_Record;
       Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Next);

   function Nth_Child
      (Tree_Model : not null access Gtk_List_Store_Record;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
       N          : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Nth_Child);

   function Parent
      (Tree_Model : not null access Gtk_List_Store_Record;
       Child      : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Parent);

   procedure Previous
      (Tree_Model : not null access Gtk_List_Store_Record;
       Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Previous);

   procedure Ref_Node
      (Tree_Model : not null access Gtk_List_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Ref_Node);

   procedure Row_Changed
      (Tree_Model : not null access Gtk_List_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Row_Changed);

   procedure Row_Deleted
      (Tree_Model : not null access Gtk_List_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path);
   pragma Obsolescent (Row_Deleted);

   procedure Row_Has_Child_Toggled
      (Tree_Model : not null access Gtk_List_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Row_Has_Child_Toggled);

   procedure Row_Inserted
      (Tree_Model : not null access Gtk_List_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Row_Inserted);

   procedure Rows_Reordered
      (Tree_Model : not null access Gtk_List_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       New_Order  : Gint_Array);
   pragma Obsolescent (Rows_Reordered);

   procedure Rows_Reordered_With_Length
      (Tree_Model : not null access Gtk_List_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       New_Order  : Gint_Array;
       Length     : Glib.Gint);
   pragma Obsolescent (Rows_Reordered_With_Length);

   procedure Unref_Node
      (Tree_Model : not null access Gtk_List_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Unref_Node);

   function Get_Sort_Column_Id
      (Self           : not null access Gtk_List_Store_Record;
       Sort_Column_Id : access Glib.Gint;
       Order          : access Gtk.Enums.Gtk_Sort_Type) return Boolean;
   pragma Obsolescent (Get_Sort_Column_Id);

   procedure Set_Sort_Column_Id
      (Self           : not null access Gtk_List_Store_Record;
       Sort_Column_Id : Glib.Gint;
       Order          : Gtk.Enums.Gtk_Sort_Type);
   pragma Obsolescent (Set_Sort_Column_Id);

   function Has_Default_Sort_Func
      (Self : not null access Gtk_List_Store_Record) return Boolean;
   pragma Obsolescent (Has_Default_Sort_Func);

   procedure Sort_Column_Changed
      (Self : not null access Gtk_List_Store_Record);
   pragma Obsolescent (Sort_Column_Changed);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.TreeDragDest"
   --
   --  - "Gtk.TreeDragSource"
   --
   --  - "Gtk.TreeModel"
   --
   --  - "Gtk.TreeSortable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_List_Store_Record, Gtk_List_Store);
   function "+"
     (Widget : access Gtk_List_Store_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_List_Store
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Tree_Drag_Dest is new Glib.Types.Implements
     (Gtk.Tree_Drag_Dest.Gtk_Tree_Drag_Dest, Gtk_List_Store_Record, Gtk_List_Store);
   function "+"
     (Widget : access Gtk_List_Store_Record'Class)
   return Gtk.Tree_Drag_Dest.Gtk_Tree_Drag_Dest
   renames Implements_Gtk_Tree_Drag_Dest.To_Interface;
   function "-"
     (Interf : Gtk.Tree_Drag_Dest.Gtk_Tree_Drag_Dest)
   return Gtk_List_Store
   renames Implements_Gtk_Tree_Drag_Dest.To_Object;

   package Implements_Gtk_Tree_Drag_Source is new Glib.Types.Implements
     (Gtk.Tree_Drag_Source.Gtk_Tree_Drag_Source, Gtk_List_Store_Record, Gtk_List_Store);
   function "+"
     (Widget : access Gtk_List_Store_Record'Class)
   return Gtk.Tree_Drag_Source.Gtk_Tree_Drag_Source
   renames Implements_Gtk_Tree_Drag_Source.To_Interface;
   function "-"
     (Interf : Gtk.Tree_Drag_Source.Gtk_Tree_Drag_Source)
   return Gtk_List_Store
   renames Implements_Gtk_Tree_Drag_Source.To_Object;

   package Implements_Gtk_Tree_Model is new Glib.Types.Implements
     (Gtk.Tree_Model.Gtk_Tree_Model, Gtk_List_Store_Record, Gtk_List_Store);
   function "+"
     (Widget : access Gtk_List_Store_Record'Class)
   return Gtk.Tree_Model.Gtk_Tree_Model
   renames Implements_Gtk_Tree_Model.To_Interface;
   function "-"
     (Interf : Gtk.Tree_Model.Gtk_Tree_Model)
   return Gtk_List_Store
   renames Implements_Gtk_Tree_Model.To_Object;

   package Implements_Gtk_Tree_Sortable is new Glib.Types.Implements
     (Gtk.Tree_Sortable.Gtk_Tree_Sortable, Gtk_List_Store_Record, Gtk_List_Store);
   function "+"
     (Widget : access Gtk_List_Store_Record'Class)
   return Gtk.Tree_Sortable.Gtk_Tree_Sortable
   renames Implements_Gtk_Tree_Sortable.To_Interface;
   function "-"
     (Interf : Gtk.Tree_Sortable.Gtk_Tree_Sortable)
   return Gtk_List_Store
   renames Implements_Gtk_Tree_Sortable.To_Object;

end Gtk.List_Store;
