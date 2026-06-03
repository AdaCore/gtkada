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

--  A tree-like data structure that can be used with the [classGtk.TreeView].
--
--  The `GtkTreeStore` object is a list model for use with a `GtkTreeView`
--  widget. It implements the [ifaceGtk.TreeModel] interface, and consequently,
--  can use all of the methods available there. It also implements the
--  [ifaceGtk.TreeSortable] interface so it can be sorted by the view. Finally,
--  it also implements the tree [drag][ifaceGtk.TreeDragSource] and
--  [drop][ifaceGtk.TreeDragDest] interfaces.
--
--  `GtkTreeStore` is deprecated since GTK 4.10, and should not be used in
--  newly written code. You should use [classGtk.TreeListModel] for a tree-like
--  model object.
--
--  ## GtkTreeStore as GtkBuildable
--
--  The GtkTreeStore implementation of the `GtkBuildable` interface allows to
--  specify the model columns with a `<columns>` element that may contain
--  multiple `<column>` elements, each specifying one model column. The "type"
--  attribute specifies the data type for the column.
--
--  An example of a UI Definition fragment for a tree store:
--
--  ```xml <object class="GtkTreeStore"> <columns> <column type="gchararray"/>
--  <column type="gchararray"/> <column type="gint"/> </columns> </object> ```

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Content_Provider; use Gdk.Content_Provider;
with Glib;                 use Glib;
with Glib.Types;           use Glib.Types;
with Glib.Values;          use Glib.Values;
with Gtk.Buildable;        use Gtk.Buildable;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Tree_Drag_Source; use Gtk.Tree_Drag_Source;
with Gtk.Tree_Model;       use Gtk.Tree_Model;
with Gtk.Tree_Sortable;    use Gtk.Tree_Sortable;

package Gtk.Tree_Store is

   pragma Obsolescent;
   --  Use [class@Gtk.TreeListModel] instead

   type Gtk_Tree_Store_Record is new Gtk_Root_Tree_Model_Record with null record;
   type Gtk_Tree_Store is access all Gtk_Tree_Store_Record'Class;

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

   procedure Gtk_New (Self : out Gtk_Tree_Store; Types : GType_Array);
   procedure Initialize
      (Self  : not null access Gtk_Tree_Store_Record'Class;
       Types : GType_Array);
   --  Creates a new tree store.
   --  This constructor is meant for language bindings.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Types an array of `GType` types for the columns, from first to
   --  last

   function Gtk_Tree_Store_Newv (Types : GType_Array) return Gtk_Tree_Store;
   --  Creates a new tree store.
   --  This constructor is meant for language bindings.
   --  @param Types an array of `GType` types for the columns, from first to
   --  last

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tree_store_get_type");

   -------------
   -- Methods --
   -------------

   procedure Append
      (Self   : not null access Gtk_Tree_Store_Record;
       Iter   : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Parent : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Append);
   --  Appends a new row to Tree_Store.
   --  If Parent is non-null, then it will append the new row after the last
   --  child of Parent, otherwise it will append a row to the top level.
   --  The Iter parameter will be changed to point to this new row. The row
   --  will be empty after this function is called. To fill in values, you need
   --  to call gtk_tree_store_set or Gtk.Tree_Store.Set_Value.
   --  Deprecated since 4.10, 1
   --  @param Iter An unset `GtkTreeIter` to set to the appended row
   --  @param Parent A valid `GtkTreeIter`

   procedure Clear (Self : not null access Gtk_Tree_Store_Record);
   pragma Obsolescent (Clear);
   --  Removes all rows from Tree_Store
   --  Deprecated since 4.10, 1

   procedure Insert
      (Self     : not null access Gtk_Tree_Store_Record;
       Iter     : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Parent   : Gtk.Tree_Model.Gtk_Tree_Iter;
       Position : Glib.Gint);
   pragma Obsolescent (Insert);
   --  Creates a new row at Position.
   --  If parent is non-null, then the row will be made a child of Parent.
   --  Otherwise, the row will be created at the toplevel.
   --  If Position is `-1` or is larger than the number of rows at that level,
   --  then the new row will be inserted to the end of the list.
   --  The Iter parameter will be changed to point to this new row. The row
   --  will be empty after this function is called. To fill in values, you need
   --  to call gtk_tree_store_set or Gtk.Tree_Store.Set_Value.
   --  Deprecated since 4.10, 1
   --  @param Iter An unset `GtkTreeIter` to set to the new row
   --  @param Parent A valid `GtkTreeIter`
   --  @param Position position to insert the new row, or -1 for last

   procedure Insert_After
      (Self    : not null access Gtk_Tree_Store_Record;
       Iter    : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Parent  : Gtk.Tree_Model.Gtk_Tree_Iter;
       Sibling : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Insert_After);
   --  Inserts a new row after Sibling.
   --  If Sibling is null, then the row will be prepended to Parent's
   --  children.
   --  If Parent and Sibling are null, then the row will be prepended to the
   --  toplevel.
   --  If both Sibling and Parent are set, then Parent must be the parent of
   --  Sibling. When Sibling is set, Parent is optional.
   --  The Iter parameter will be changed to point to this new row. The row
   --  will be empty after this function is called. To fill in values, you need
   --  to call gtk_tree_store_set or Gtk.Tree_Store.Set_Value.
   --  Deprecated since 4.10, 1
   --  @param Iter An unset `GtkTreeIter` to set to the new row
   --  @param Parent A valid `GtkTreeIter`
   --  @param Sibling A valid `GtkTreeIter`

   procedure Insert_Before
      (Self    : not null access Gtk_Tree_Store_Record;
       Iter    : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Parent  : Gtk.Tree_Model.Gtk_Tree_Iter;
       Sibling : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Insert_Before);
   --  Inserts a new row before Sibling.
   --  If Sibling is null, then the row will be appended to Parent's children.
   --  If Parent and Sibling are null, then the row will be appended to the
   --  toplevel.
   --  If both Sibling and Parent are set, then Parent must be the parent of
   --  Sibling. When Sibling is set, Parent is optional.
   --  The Iter parameter will be changed to point to this new row. The row
   --  will be empty after this function is called. To fill in values, you need
   --  to call gtk_tree_store_set or Gtk.Tree_Store.Set_Value.
   --  Deprecated since 4.10, 1
   --  @param Iter An unset `GtkTreeIter` to set to the new row
   --  @param Parent A valid `GtkTreeIter`
   --  @param Sibling A valid `GtkTreeIter`

   function Is_Ancestor
      (Self       : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Descendant : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   pragma Obsolescent (Is_Ancestor);
   --  Checks if Iter is an ancestor of Descendant.
   --  Deprecated since 4.10, 1
   --  @param Iter A valid `GtkTreeIter`
   --  @param Descendant A valid `GtkTreeIter`
   --  @return true if Iter is an ancestor of Descendant, and false otherwise

   function Iter_Depth
      (Self : not null access Gtk_Tree_Store_Record;
       Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gint;
   pragma Obsolescent (Iter_Depth);
   --  Returns the depth of the position pointed by the iterator
   --  The depth will be 0 for anything on the root level, 1 for anything down
   --  a level, etc.
   --  Deprecated since 4.10, 1
   --  @param Iter A valid `GtkTreeIter`
   --  @return The depth of the position pointed by the iterator

   function Iter_Is_Valid
      (Self : not null access Gtk_Tree_Store_Record;
       Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   pragma Obsolescent (Iter_Is_Valid);
   --  Checks if the given iter is a valid iter for this `GtkTreeStore`.
   --  This function is slow. Only use it for debugging and/or testing
   --  purposes.
   --  Deprecated since 4.10, 1
   --  @param Iter the iterator to check
   --  @return true if the iter is valid, and false otherwise

   procedure Move_After
      (Self     : not null access Gtk_Tree_Store_Record;
       Iter     : Gtk.Tree_Model.Gtk_Tree_Iter;
       Position : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Move_After);
   --  Moves Iter in Tree_Store to the position after Position.
   --  Iter and Position should be in the same level.
   --  Note that this function only works with unsorted stores.
   --  If Position is null, Iter will be moved to the start of the level.
   --  Deprecated since 4.10, 1
   --  @param Iter A `GtkTreeIter`.
   --  @param Position A `GtkTreeIter`.

   procedure Move_Before
      (Self     : not null access Gtk_Tree_Store_Record;
       Iter     : Gtk.Tree_Model.Gtk_Tree_Iter;
       Position : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Move_Before);
   --  Moves Iter in Tree_Store to the position before Position.
   --  Iter and Position should be in the same level.
   --  Note that this function only works with unsorted stores.
   --  If Position is null, Iter will be moved to the end of the level.
   --  Deprecated since 4.10, 1
   --  @param Iter A `GtkTreeIter`
   --  @param Position A `GtkTreeIter`

   procedure Prepend
      (Self   : not null access Gtk_Tree_Store_Record;
       Iter   : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Parent : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Prepend);
   --  Prepends a new row to Tree_Store.
   --  If Parent is non-null, then it will prepend the new row before the
   --  first child of Parent, otherwise it will prepend a row to the top level.
   --  The `iter` parameter will be changed to point to this new row. The row
   --  will be empty after this function is called. To fill in values, you need
   --  to call gtk_tree_store_set or Gtk.Tree_Store.Set_Value.
   --  Deprecated since 4.10, 1
   --  @param Iter An unset `GtkTreeIter` to set to the prepended row
   --  @param Parent A valid `GtkTreeIter`

   procedure Remove
      (Self : not null access Gtk_Tree_Store_Record;
       Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Remove);
   --  Removes Iter from Tree_Store.
   --  After being removed, Iter is set to the next valid row at that level,
   --  or invalidated if it previously pointed to the last one.
   --  Deprecated since 4.10, 1
   --  @param Iter A valid `GtkTreeIter`

   procedure Reorder
      (Self      : not null access Gtk_Tree_Store_Record;
       Parent    : Gtk.Tree_Model.Gtk_Tree_Iter;
       New_Order : Gint_Array);
   pragma Obsolescent (Reorder);
   --  Reorders the children of Parent in Tree_Store to follow the order
   --  indicated by New_Order.
   --  Note that this function only works with unsorted stores.
   --  Deprecated since 4.10, 1
   --  @param Parent the parent of the children to re-order
   --  @param New_Order an array of integers mapping the new position of each
   --  child to its old position before the re-ordering, i.e.
   --  `new_order[newpos] = oldpos`

   procedure Set_Column_Types
      (Self  : not null access Gtk_Tree_Store_Record;
       Types : GType_Array);
   pragma Obsolescent (Set_Column_Types);
   --  Sets the type of the columns in a tree store.
   --  This function is meant primarily for types that inherit from
   --  `GtkTreeStore`, and should only be used when constructing a new
   --  `GtkTreeStore`.
   --  This functions cannot be called after a row has been added, or a method
   --  on the `GtkTreeModel` interface is called on the tree store.
   --  Deprecated since 4.10, 1
   --  @param Types An array of `GType` types, one for each column

   procedure Set_Value
      (Self   : not null access Gtk_Tree_Store_Record;
       Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
       Column : Glib.Gint;
       Value  : Glib.Values.GValue);
   pragma Obsolescent (Set_Value);
   --  Sets the data in the cell specified by Iter and Column.
   --  The type of Value must be convertible to the type of the column.
   --  Deprecated since 4.10, 1
   --  @param Iter A valid `GtkTreeIter` for the row being modified
   --  @param Column column number to modify
   --  @param Value new value for the cell

   procedure Swap
      (Self : not null access Gtk_Tree_Store_Record;
       A    : Gtk.Tree_Model.Gtk_Tree_Iter;
       B    : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Swap);
   --  Swaps A and B in the same level of Tree_Store.
   --  Note that this function only works with unsorted stores.
   --  Deprecated since 4.10, 1
   --  @param A A `GtkTreeIter`.
   --  @param B Another `GtkTreeIter`.

   procedure Foreach
      (Tree_Model : not null access Gtk_Tree_Store_Record;
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
         (Tree_Model : not null access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
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
      (Self      : not null access Gtk_Tree_Store_Record;
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
         (Self      : not null access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
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
      (Self           : not null access Gtk_Tree_Store_Record;
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
         (Self           : not null access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
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

   generic
   type Data_Type is private;
   package Generic_Set is
      type Data_Type_Access is access all Data_Type;

      procedure Set
        (Tree_Store : access Gtk_Tree_Store_Record'Class;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Data_Type_Access);
      --  Generic procedure used to store access objects in the model.
      --  For GObject and all of its descendents (including all widgets),
      --  you should use the Set procedure below that takes a GObject as
      --  parameter.
      --
      --  Please see the example at the end for more information on how to
      --  create your own Set procedures adapted to your model. Also consider
      --  using Set_Value for complex cases

      function Get
        (Tree_Store : access Gtk_Tree_Store_Record'Class;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint) return Data_Type_Access;
      --  Generic procedure used to get access objects back from the model.
      --  For GObject and all of its descendents (including all widgets),
      --  you should use the Get_Object function defined in Gtk-Tree_Model
      --  that returns a GObject.

   end Generic_Set;

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : UTF8_String);
   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Boolean);
   procedure Set_Ulong
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Gulong);
   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Gint);
   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Glib.C_Proxy);
   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Address    : System.Address);
   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Glib.Object.GObject);

   procedure Set
     (Self       : not null access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Columns    : Glib.Gint_Array;
      Values     : Glib.Values.GValue_Array);
   pragma Precondition (Columns'Length = Values'Length);
   --  A variant of Set which takes the columns and valus as two arrays.
   --  This is more efficient when changing multiple values than calling
   --  one of the Set procedures above multiple times.

   procedure Set
     (Self       : not null access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Values     : Glib.Values.GValue_Array);
   --  A variant of the above that is used to set all the columns.

   function Freeze_Sort
     (Self : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class)
   return Gint;
   --  Freeze the sorting in the tree view, and returns the current
   --  sort_column_id, which should be used when thawing. (See Thaw_Sort)

   procedure Thaw_Sort
     (Self      : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
      Column_Id : Gint);
   --  Thaw a frozen tree_view. Column_Id should be the value returned by
   --  the corresponding call to Freeze_Sort.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Drag_Data_Delete
      (Self : not null access Gtk_Tree_Store_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   pragma Obsolescent (Drag_Data_Delete);

   function Drag_Data_Get
      (Self : not null access Gtk_Tree_Store_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gdk.Content_Provider.Gdk_Content_Provider;
   pragma Obsolescent (Drag_Data_Get);

   function Row_Draggable
      (Self : not null access Gtk_Tree_Store_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   pragma Obsolescent (Row_Draggable);

   function Get_Column_Type
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Index      : Glib.Gint) return GType;
   pragma Obsolescent (Get_Column_Type);

   function Get_Flags
      (Tree_Model : not null access Gtk_Tree_Store_Record)
       return Gtk.Tree_Model.Tree_Model_Flags;
   pragma Obsolescent (Get_Flags);

   function Get_Iter
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Get_Iter);

   function Get_Iter_First
      (Tree_Model : not null access Gtk_Tree_Store_Record)
       return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Get_Iter_First);

   function Get_Iter_From_String
      (Tree_Model  : not null access Gtk_Tree_Store_Record;
       Path_String : UTF8_String) return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Get_Iter_From_String);

   function Get_N_Columns
      (Tree_Model : not null access Gtk_Tree_Store_Record) return Glib.Gint;
   pragma Obsolescent (Get_N_Columns);

   function Get_Path
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Path;
   pragma Obsolescent (Get_Path);

   function Get_String_From_Iter
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return UTF8_String;
   pragma Obsolescent (Get_String_From_Iter);

   procedure Get_Value
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Column     : Glib.Gint;
       Value      : out Glib.Values.GValue);
   pragma Obsolescent (Get_Value);

   function Children
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Children);

   function Has_Child
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   pragma Obsolescent (Has_Child);

   function N_Children
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
       return Glib.Gint;
   pragma Obsolescent (N_Children);

   procedure Next
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Next);

   function Nth_Child
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
       N          : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Nth_Child);

   function Parent
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Child      : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Parent);

   procedure Previous
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Previous);

   procedure Ref_Node
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Ref_Node);

   procedure Row_Changed
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Row_Changed);

   procedure Row_Deleted
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path);
   pragma Obsolescent (Row_Deleted);

   procedure Row_Has_Child_Toggled
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Row_Has_Child_Toggled);

   procedure Row_Inserted
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Row_Inserted);

   procedure Rows_Reordered
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       New_Order  : Gint_Array);
   pragma Obsolescent (Rows_Reordered);

   procedure Rows_Reordered_With_Length
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       New_Order  : Gint_Array;
       Length     : Glib.Gint);
   pragma Obsolescent (Rows_Reordered_With_Length);

   procedure Unref_Node
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Unref_Node);

   function Get_Sort_Column_Id
      (Self           : not null access Gtk_Tree_Store_Record;
       Sort_Column_Id : access Glib.Gint;
       Order          : access Gtk.Enums.Gtk_Sort_Type) return Boolean;
   pragma Obsolescent (Get_Sort_Column_Id);

   procedure Set_Sort_Column_Id
      (Self           : not null access Gtk_Tree_Store_Record;
       Sort_Column_Id : Glib.Gint;
       Order          : Gtk.Enums.Gtk_Sort_Type);
   pragma Obsolescent (Set_Sort_Column_Id);

   function Has_Default_Sort_Func
      (Self : not null access Gtk_Tree_Store_Record) return Boolean;
   pragma Obsolescent (Has_Default_Sort_Func);

   procedure Sort_Column_Changed
      (Self : not null access Gtk_Tree_Store_Record);
   pragma Obsolescent (Sort_Column_Changed);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.TreeDragSource"
   --
   --  - "Gtk.TreeModel"
   --
   --  - "Gtk.TreeSortable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Tree_Store_Record, Gtk_Tree_Store);
   function "+"
     (Widget : access Gtk_Tree_Store_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Tree_Store
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Tree_Drag_Source is new Glib.Types.Implements
     (Gtk.Tree_Drag_Source.Gtk_Tree_Drag_Source, Gtk_Tree_Store_Record, Gtk_Tree_Store);
   function "+"
     (Widget : access Gtk_Tree_Store_Record'Class)
   return Gtk.Tree_Drag_Source.Gtk_Tree_Drag_Source
   renames Implements_Gtk_Tree_Drag_Source.To_Interface;
   function "-"
     (Interf : Gtk.Tree_Drag_Source.Gtk_Tree_Drag_Source)
   return Gtk_Tree_Store
   renames Implements_Gtk_Tree_Drag_Source.To_Object;

   package Implements_Gtk_Tree_Model is new Glib.Types.Implements
     (Gtk.Tree_Model.Gtk_Tree_Model, Gtk_Tree_Store_Record, Gtk_Tree_Store);
   function "+"
     (Widget : access Gtk_Tree_Store_Record'Class)
   return Gtk.Tree_Model.Gtk_Tree_Model
   renames Implements_Gtk_Tree_Model.To_Interface;
   function "-"
     (Interf : Gtk.Tree_Model.Gtk_Tree_Model)
   return Gtk_Tree_Store
   renames Implements_Gtk_Tree_Model.To_Object;

   package Implements_Gtk_Tree_Sortable is new Glib.Types.Implements
     (Gtk.Tree_Sortable.Gtk_Tree_Sortable, Gtk_Tree_Store_Record, Gtk_Tree_Store);
   function "+"
     (Widget : access Gtk_Tree_Store_Record'Class)
   return Gtk.Tree_Sortable.Gtk_Tree_Sortable
   renames Implements_Gtk_Tree_Sortable.To_Interface;
   function "-"
     (Interf : Gtk.Tree_Sortable.Gtk_Tree_Sortable)
   return Gtk_Tree_Store
   renames Implements_Gtk_Tree_Sortable.To_Object;

end Gtk.Tree_Store;
