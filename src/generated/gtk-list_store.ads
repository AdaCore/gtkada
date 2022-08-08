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
--  The Gtk.List_Store.Gtk_List_Store object is a list model for use with a
--  Gtk.Tree_View.Gtk_Tree_View widget. It implements the
--  Gtk.Tree_Model.Gtk_Tree_Model interface, and consequentialy, can use all of
--  the methods available there. It also implements the
--  Gtk.Tree_Sortable.Gtk_Tree_Sortable interface so it can be sorted by the
--  view. Finally, it also implements the tree [drag and
--  drop][gtk3-GtkTreeView-drag-and-drop] interfaces.
--
--  The Gtk.List_Store.Gtk_List_Store can accept most GObject types as a
--  column type, though it can't accept all custom types. Internally, it will
--  keep a copy of data passed in (such as a string or a boxed pointer).
--  Columns that accept GObjects are handled a little differently. The
--  Gtk.List_Store.Gtk_List_Store will keep a reference to the object instead
--  of copying the value. As a result, if the object is modified, it is up to
--  the application writer to call Gtk.Tree_Model.Row_Changed to emit the
--  Gtk.Tree_Model.Gtk_Tree_Model::row_changed signal. This most commonly
--  affects lists with Gdk_Pixbufs stored.
--
--  An example for creating a simple list store: |[<!-- language="C" --> enum
--  { COLUMN_STRING, COLUMN_INT, COLUMN_BOOLEAN, N_COLUMNS };
--
--  { GtkListStore *list_store; GtkTreePath *path; GtkTreeIter iter; gint i;
--
--  list_store = gtk_list_store_new (N_COLUMNS, G_TYPE_STRING, G_TYPE_INT,
--  G_TYPE_BOOLEAN);
--
--  for (i = 0; i < 10; i++) { gchar *some_data;
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
--  COLUMN_BOOLEAN, TRUE, -1); } ]|
--
--  # Performance Considerations
--
--  Internally, the Gtk.List_Store.Gtk_List_Store was implemented with a
--  linked list with a tail pointer prior to GTK+ 2.6. As a result, it was fast
--  at data insertion and deletion, and not fast at random data access. The
--  Gtk.List_Store.Gtk_List_Store sets the GTK_TREE_MODEL_ITERS_PERSIST flag,
--  which means that Gtk_Tree_Iters can be cached while the row exists. Thus,
--  if access to a particular row is needed often and your code is expected to
--  run on older versions of GTK+, it is worth keeping the iter around.
--
--  # Atomic Operations
--
--  It is important to note that only the methods
--  gtk_list_store_insert_with_values and gtk_list_store_insert_with_valuesv
--  are atomic, in the sense that the row is being appended to the store and
--  the values filled in in a single operation with regard to
--  Gtk.Tree_Model.Gtk_Tree_Model signaling. In contrast, using e.g.
--  Gtk.List_Store.Append and then gtk_list_store_set will first create a row,
--  which triggers the Gtk.Tree_Model.Gtk_Tree_Model::row-inserted signal on
--  Gtk.List_Store.Gtk_List_Store. The row, however, is still empty, and any
--  signal handler connecting to Gtk.Tree_Model.Gtk_Tree_Model::row-inserted on
--  this particular store should be prepared for the situation that the row
--  might be empty. This is especially important if you are wrapping the
--  Gtk.List_Store.Gtk_List_Store inside a
--  Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter and are using a
--  Gtk_Tree_Model_Filter_Visible_Func. Using any of the non-atomic operations
--  to append rows to the Gtk.List_Store.Gtk_List_Store will cause the
--  Gtk_Tree_Model_Filter_Visible_Func to be visited with an empty row first;
--  the function must be prepared for that.
--
--  # GtkListStore as GtkBuildable
--
--  The GtkListStore implementation of the GtkBuildable interface allows to
--  specify the model columns with a <columns> element that may contain
--  multiple <column> elements, each specifying one model column. The "type"
--  attribute specifies the data type for the column.
--
--  Additionally, it is possible to specify content for the list store in the
--  UI definition, with the <data> element. It can contain multiple elements,
--  each specifying to content for one row of the list model. Inside a , the
--  <col> elements specify the content for individual cells.
--
--  Note that it is probably more common to define your models in the code,
--  and one might consider it a layering violation to specify the content of a
--  list store in a UI definition, data, not presentation, and common wisdom is
--  to separate the two, as far as possible.
--
--  An example of a UI Definition fragment for a list store: |[<!--
--  language="C" --> <object class="GtkListStore"> <columns> <column
--  type="gchararray"/> <column type="gchararray"/> <column type="gint"/>
--  </columns> <data> <col id="0">John</col> <col id="1">Doe</col> <col
--  id="2">25</col> <col id="0">Johan</col> <col id="1">Dahlin</col> <col
--  id="2">50</col> </data> </object> ]|
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Pixbuf;           use Gdk.Pixbuf;
with Glib;                 use Glib;
with Glib.Types;           use Glib.Types;
with Glib.Values;          use Glib.Values;
with Gtk.Buildable;        use Gtk.Buildable;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Selection_Data;   use Gtk.Selection_Data;
with Gtk.Tree_Drag_Dest;   use Gtk.Tree_Drag_Dest;
with Gtk.Tree_Drag_Source; use Gtk.Tree_Drag_Source;
with Gtk.Tree_Model;       use Gtk.Tree_Model;
with Gtk.Tree_Sortable;    use Gtk.Tree_Sortable;

package Gtk.List_Store is

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
   --  "model": the Gtk.Tree_Model.Gtk_Tree_Model being iterated
   --  "path": the current Gtk.Tree_Model.Gtk_Tree_Path
   --  "iter": the current Gtk.Tree_Model.Gtk_Tree_Iter

   type Gtk_Tree_Iter_Compare_Func is access function
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gint;
   --  A GtkTreeIterCompareFunc should return a negative integer, zero, or a
   --  positive integer if A sorts before B, A sorts with B, or A sorts after B
   --  respectively. If two iters compare as equal, their order in the sorted
   --  model is undefined. In order to ensure that the
   --  Gtk.Tree_Sortable.Gtk_Tree_Sortable behaves as expected, the
   --  GtkTreeIterCompareFunc must define a partial order on the model, i.e. it
   --  must be reflexive, antisymmetric and transitive.
   --  For example, if Model is a product catalogue, then a compare function
   --  for the "price" column could be one which returns `price_of(A) -
   --  price_of(B)`.
   --  "model": The Gtk.Tree_Model.Gtk_Tree_Model the comparison is within
   --  "a": A Gtk.Tree_Model.Gtk_Tree_Iter in Model
   --  "b": Another Gtk.Tree_Model.Gtk_Tree_Iter in Model

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (List_Store : out Gtk_List_Store; Types : GType_Array);
   procedure Initialize
      (List_Store : not null access Gtk_List_Store_Record'Class;
       Types      : GType_Array);
   --  Non-vararg creation function. Used primarily by language bindings.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "types": an array of GType types for the columns, from first to last

   function Gtk_List_Store_Newv (Types : GType_Array) return Gtk_List_Store;
   --  Non-vararg creation function. Used primarily by language bindings.
   --  "types": an array of GType types for the columns, from first to last

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_list_store_get_type");

   -------------
   -- Methods --
   -------------

   procedure Append
      (List_Store : not null access Gtk_List_Store_Record;
       Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Appends a new row to List_Store. Iter will be changed to point to this
   --  new row. The row will be empty after this function is called. To fill in
   --  values, you need to call gtk_list_store_set or Gtk.List_Store.Set_Value.
   --  "iter": An unset Gtk.Tree_Model.Gtk_Tree_Iter to set to the appended
   --  row

   procedure Clear (List_Store : not null access Gtk_List_Store_Record);
   --  Removes all rows from the list store.

   procedure Insert
      (List_Store : not null access Gtk_List_Store_Record;
       Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Position   : Glib.Gint);
   --  Creates a new row at Position. Iter will be changed to point to this
   --  new row. If Position is -1 or is larger than the number of rows on the
   --  list, then the new row will be appended to the list. The row will be
   --  empty after this function is called. To fill in values, you need to call
   --  gtk_list_store_set or Gtk.List_Store.Set_Value.
   --  "iter": An unset Gtk.Tree_Model.Gtk_Tree_Iter to set to the new row
   --  "position": position to insert the new row, or -1 for last

   procedure Insert_After
      (List_Store : not null access Gtk_List_Store_Record;
       Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Sibling    : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Inserts a new row after Sibling. If Sibling is null, then the row will
   --  be prepended to the beginning of the list. Iter will be changed to point
   --  to this new row. The row will be empty after this function is called. To
   --  fill in values, you need to call gtk_list_store_set or
   --  Gtk.List_Store.Set_Value.
   --  "iter": An unset Gtk.Tree_Model.Gtk_Tree_Iter to set to the new row
   --  "sibling": A valid Gtk.Tree_Model.Gtk_Tree_Iter, or null

   procedure Insert_Before
      (List_Store : not null access Gtk_List_Store_Record;
       Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Sibling    : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Inserts a new row before Sibling. If Sibling is null, then the row will
   --  be appended to the end of the list. Iter will be changed to point to
   --  this new row. The row will be empty after this function is called. To
   --  fill in values, you need to call gtk_list_store_set or
   --  Gtk.List_Store.Set_Value.
   --  "iter": An unset Gtk.Tree_Model.Gtk_Tree_Iter to set to the new row
   --  "sibling": A valid Gtk.Tree_Model.Gtk_Tree_Iter, or null

   function Iter_Is_Valid
      (List_Store : not null access Gtk_List_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  > This function is slow. Only use it for debugging and/or testing >
   --  purposes.
   --  Checks if the given iter is a valid iter for this
   --  Gtk.List_Store.Gtk_List_Store.
   --  Since: gtk+ 2.2
   --  "iter": A Gtk.Tree_Model.Gtk_Tree_Iter.

   procedure Move_After
      (List_Store : not null access Gtk_List_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Position   : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Moves Iter in Store to the position after Position. Note that this
   --  function only works with unsorted stores. If Position is null, Iter will
   --  be moved to the start of the list.
   --  Since: gtk+ 2.2
   --  "iter": A Gtk.Tree_Model.Gtk_Tree_Iter.
   --  "position": A Gtk.Tree_Model.Gtk_Tree_Iter or null.

   procedure Move_Before
      (List_Store : not null access Gtk_List_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Position   : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Moves Iter in Store to the position before Position. Note that this
   --  function only works with unsorted stores. If Position is null, Iter will
   --  be moved to the end of the list.
   --  Since: gtk+ 2.2
   --  "iter": A Gtk.Tree_Model.Gtk_Tree_Iter.
   --  "position": A Gtk.Tree_Model.Gtk_Tree_Iter, or null.

   procedure Prepend
      (List_Store : not null access Gtk_List_Store_Record;
       Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Prepends a new row to List_Store. Iter will be changed to point to this
   --  new row. The row will be empty after this function is called. To fill in
   --  values, you need to call gtk_list_store_set or Gtk.List_Store.Set_Value.
   --  "iter": An unset Gtk.Tree_Model.Gtk_Tree_Iter to set to the prepend row

   procedure Remove
      (List_Store : not null access Gtk_List_Store_Record;
       Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Removes the given row from the list store. After being removed, Iter is
   --  set to be the next valid row, or invalidated if it pointed to the last
   --  row in List_Store.
   --  "iter": A valid Gtk.Tree_Model.Gtk_Tree_Iter

   procedure Reorder
      (List_Store : not null access Gtk_List_Store_Record;
       New_Order  : Gint_Array);
   --  Reorders Store to follow the order indicated by New_Order. Note that
   --  this function only works with unsorted stores.
   --  Since: gtk+ 2.2
   --  "new_order": an array of integers mapping the new position of each
   --  child to its old position before the re-ordering, i.e.
   --  New_Order`[newpos] = oldpos`. It must have exactly as many items as the
   --  list store's length.

   procedure Set_Column_Types
      (List_Store : not null access Gtk_List_Store_Record;
       Types      : GType_Array);
   --  This function is meant primarily for GObjects that inherit from
   --  Gtk.List_Store.Gtk_List_Store, and should only be used when constructing
   --  a new Gtk.List_Store.Gtk_List_Store. It will not function after a row
   --  has been added, or a method on the Gtk.Tree_Model.Gtk_Tree_Model
   --  interface is called.
   --  "types": An array length n of GTypes

   procedure Set_Value
      (List_Store : not null access Gtk_List_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Column     : Glib.Gint;
       Value      : Glib.Values.GValue);
   --  Sets the data in the cell specified by Iter and Column. The type of
   --  Value must be convertible to the type of the column.
   --  "iter": A valid Gtk.Tree_Model.Gtk_Tree_Iter for the row being modified
   --  "column": column number to modify
   --  "value": new value for the cell

   procedure Swap
      (List_Store : not null access Gtk_List_Store_Record;
       A          : Gtk.Tree_Model.Gtk_Tree_Iter;
       B          : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Swaps A and B in Store. Note that this function only works with
   --  unsorted stores.
   --  Since: gtk+ 2.2
   --  "a": A Gtk.Tree_Model.Gtk_Tree_Iter.
   --  "b": Another Gtk.Tree_Model.Gtk_Tree_Iter.

   procedure Foreach
      (Tree_Model : not null access Gtk_List_Store_Record;
       Func       : Gtk_Tree_Model_Foreach_Func);
   --  Calls func on each node in model in a depth-first fashion.
   --  If Func returns True, then the tree ceases to be walked, and
   --  Gtk.Tree_Model.Foreach returns.
   --  "func": a function to be called on each row

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
      --  "model": the Gtk.Tree_Model.Gtk_Tree_Model being iterated
      --  "path": the current Gtk.Tree_Model.Gtk_Tree_Path
      --  "iter": the current Gtk.Tree_Model.Gtk_Tree_Iter
      --  "data": The user data passed to Gtk.Tree_Model.Foreach

      procedure Foreach
         (Tree_Model : not null access Gtk.List_Store.Gtk_List_Store_Record'Class;
          Func       : Gtk_Tree_Model_Foreach_Func;
          User_Data  : User_Data_Type);
      --  Calls func on each node in model in a depth-first fashion.
      --  If Func returns True, then the tree ceases to be walked, and
      --  Gtk.Tree_Model.Foreach returns.
      --  "func": a function to be called on each row
      --  "user_data": user data to passed to Func

   end Foreach_User_Data;

   procedure Set_Default_Sort_Func
      (Sortable  : not null access Gtk_List_Store_Record;
       Sort_Func : Gtk_Tree_Iter_Compare_Func);
   --  Sets the default comparison function used when sorting to be Sort_Func.
   --  If the current sort column id of Sortable is
   --  GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID, then the model will sort using
   --  this function.
   --  If Sort_Func is null, then there will be no default comparison
   --  function. This means that once the model has been sorted, it can't go
   --  back to the default state. In this case, when the current sort column id
   --  of Sortable is GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID, the model will
   --  be unsorted.
   --  "sort_func": The comparison function

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
      --  respectively. If two iters compare as equal, their order in the sorted
      --  model is undefined. In order to ensure that the
      --  Gtk.Tree_Sortable.Gtk_Tree_Sortable behaves as expected, the
      --  GtkTreeIterCompareFunc must define a partial order on the model, i.e. it
      --  must be reflexive, antisymmetric and transitive.
      --  For example, if Model is a product catalogue, then a compare function
      --  for the "price" column could be one which returns `price_of(A) -
      --  price_of(B)`.
      --  "model": The Gtk.Tree_Model.Gtk_Tree_Model the comparison is within
      --  "a": A Gtk.Tree_Model.Gtk_Tree_Iter in Model
      --  "b": Another Gtk.Tree_Model.Gtk_Tree_Iter in Model
      --  "user_data": Data passed when the compare func is assigned e.g. by
      --  Gtk.Tree_Sortable.Set_Sort_Func

      procedure Set_Default_Sort_Func
         (Sortable  : not null access Gtk.List_Store.Gtk_List_Store_Record'Class;
          Sort_Func : Gtk_Tree_Iter_Compare_Func;
          User_Data : User_Data_Type);
      --  Sets the default comparison function used when sorting to be
      --  Sort_Func. If the current sort column id of Sortable is
      --  GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID, then the model will sort
      --  using this function.
      --  If Sort_Func is null, then there will be no default comparison
      --  function. This means that once the model has been sorted, it can't go
      --  back to the default state. In this case, when the current sort column
      --  id of Sortable is GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID, the model
      --  will be unsorted.
      --  "sort_func": The comparison function
      --  "user_data": User data to pass to Sort_Func, or null

   end Set_Default_Sort_Func_User_Data;

   procedure Set_Sort_Func
      (Sortable       : not null access Gtk_List_Store_Record;
       Sort_Column_Id : Glib.Gint;
       Sort_Func      : Gtk_Tree_Iter_Compare_Func);
   --  Sets the comparison function used when sorting to be Sort_Func. If the
   --  current sort column id of Sortable is the same as Sort_Column_Id, then
   --  the model will sort using this function.
   --  "sort_column_id": the sort column id to set the function for
   --  "sort_func": The comparison function

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
      --  respectively. If two iters compare as equal, their order in the sorted
      --  model is undefined. In order to ensure that the
      --  Gtk.Tree_Sortable.Gtk_Tree_Sortable behaves as expected, the
      --  GtkTreeIterCompareFunc must define a partial order on the model, i.e. it
      --  must be reflexive, antisymmetric and transitive.
      --  For example, if Model is a product catalogue, then a compare function
      --  for the "price" column could be one which returns `price_of(A) -
      --  price_of(B)`.
      --  "model": The Gtk.Tree_Model.Gtk_Tree_Model the comparison is within
      --  "a": A Gtk.Tree_Model.Gtk_Tree_Iter in Model
      --  "b": Another Gtk.Tree_Model.Gtk_Tree_Iter in Model
      --  "user_data": Data passed when the compare func is assigned e.g. by
      --  Gtk.Tree_Sortable.Set_Sort_Func

      procedure Set_Sort_Func
         (Sortable       : not null access Gtk.List_Store.Gtk_List_Store_Record'Class;
          Sort_Column_Id : Glib.Gint;
          Sort_Func      : Gtk_Tree_Iter_Compare_Func;
          User_Data      : User_Data_Type);
      --  Sets the comparison function used when sorting to be Sort_Func. If
      --  the current sort column id of Sortable is the same as Sort_Column_Id,
      --  then the model will sort using this function.
      --  "sort_column_id": the sort column id to set the function for
      --  "sort_func": The comparison function
      --  "user_data": User data to pass to Sort_Func, or null

   end Set_Sort_Func_User_Data;

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Set
     (Tree_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : UTF8_String);
   procedure Set
     (Tree_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Boolean);
   procedure Set
     (Tree_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Gint);
   procedure Set
     (Tree_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Gdk.Pixbuf.Gdk_Pixbuf);

   procedure Set
     (Self       : not null access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Columns    : Glib.Gint_Array;
      Values     : Glib.Values.GValue_Array);
   pragma Precondition (Columns'Length = Values'Length);
   --  A variant of Set which takes the columns and valus as two arrays.
   --  This is more efficient when changing multiple values than calling
   --  one of the Set procedures above multiple times.

   procedure Set
     (Self       : not null access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Values     : Glib.Values.GValue_Array);
   --  A variant of the above that is used to set all the columns.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Drag_Data_Received
      (Self           : not null access Gtk_List_Store_Record;
       Dest           : Gtk.Tree_Model.Gtk_Tree_Path;
       Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data)
       return Boolean;

   function Row_Drop_Possible
      (Self           : not null access Gtk_List_Store_Record;
       Dest_Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data)
       return Boolean;

   function Drag_Data_Delete
      (Self : not null access Gtk_List_Store_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;

   function Drag_Data_Get
      (Self           : not null access Gtk_List_Store_Record;
       Path           : Gtk.Tree_Model.Gtk_Tree_Path;
       Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data)
       return Boolean;

   function Row_Draggable
      (Self : not null access Gtk_List_Store_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;

   function Get_Column_Type
      (Tree_Model : not null access Gtk_List_Store_Record;
       Index      : Glib.Gint) return GType;

   function Get_Flags
      (Tree_Model : not null access Gtk_List_Store_Record)
       return Gtk.Tree_Model.Tree_Model_Flags;

   function Get_Iter
      (Tree_Model : not null access Gtk_List_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Get_Iter_First
      (Tree_Model : not null access Gtk_List_Store_Record)
       return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Get_Iter_From_String
      (Tree_Model  : not null access Gtk_List_Store_Record;
       Path_String : UTF8_String) return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Get_N_Columns
      (Tree_Model : not null access Gtk_List_Store_Record) return Glib.Gint;

   function Get_Path
      (Tree_Model : not null access Gtk_List_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Path;

   function Get_String_From_Iter
      (Tree_Model : not null access Gtk_List_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return UTF8_String;

   procedure Get_Value
      (Tree_Model : not null access Gtk_List_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Column     : Glib.Gint;
       Value      : out Glib.Values.GValue);

   function Children
      (Tree_Model : not null access Gtk_List_Store_Record;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Has_Child
      (Tree_Model : not null access Gtk_List_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;

   function N_Children
      (Tree_Model : not null access Gtk_List_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
       return Glib.Gint;

   procedure Next
      (Tree_Model : not null access Gtk_List_Store_Record;
       Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);

   function Nth_Child
      (Tree_Model : not null access Gtk_List_Store_Record;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
       N          : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Parent
      (Tree_Model : not null access Gtk_List_Store_Record;
       Child      : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Iter;

   procedure Previous
      (Tree_Model : not null access Gtk_List_Store_Record;
       Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);

   procedure Ref_Node
      (Tree_Model : not null access Gtk_List_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);

   procedure Row_Changed
      (Tree_Model : not null access Gtk_List_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);

   procedure Row_Deleted
      (Tree_Model : not null access Gtk_List_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path);

   procedure Row_Has_Child_Toggled
      (Tree_Model : not null access Gtk_List_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);

   procedure Row_Inserted
      (Tree_Model : not null access Gtk_List_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);

   procedure Rows_Reordered
      (Tree_Model : not null access Gtk_List_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       New_Order  : Gint_Array);

   procedure Rows_Reordered_With_Length
      (Tree_Model : not null access Gtk_List_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       New_Order  : Gint_Array;
       Length     : Glib.Gint);

   procedure Unref_Node
      (Tree_Model : not null access Gtk_List_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);

   procedure Get_Sort_Column_Id
      (Sortable       : not null access Gtk_List_Store_Record;
       Sort_Column_Id : out Glib.Gint;
       Order          : out Gtk.Enums.Gtk_Sort_Type);

   procedure Set_Sort_Column_Id
      (Sortable       : not null access Gtk_List_Store_Record;
       Sort_Column_Id : Glib.Gint;
       Order          : Gtk.Enums.Gtk_Sort_Type);

   function Has_Default_Sort_Func
      (Sortable : not null access Gtk_List_Store_Record) return Boolean;

   procedure Sort_Column_Changed
      (Sortable : not null access Gtk_List_Store_Record);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "TreeDragDest"
   --
   --  - "TreeDragSource"
   --
   --  - "TreeModel"
   --
   --  - "TreeSortable"

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
