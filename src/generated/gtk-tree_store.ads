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
--  The Gtk.Tree_Store.Gtk_Tree_Store object is a list model for use with a
--  Gtk.Tree_View.Gtk_Tree_View widget. It implements the
--  Gtk.Tree_Model.Gtk_Tree_Model interface, and consequentially, can use all
--  of the methods available there. It also implements the
--  Gtk.Tree_Sortable.Gtk_Tree_Sortable interface so it can be sorted by the
--  view. Finally, it also implements the tree [drag and
--  drop][gtk3-GtkTreeView-drag-and-drop] interfaces.
--
--  # GtkTreeStore as GtkBuildable
--
--  The GtkTreeStore implementation of the Gtk.Buildable.Gtk_Buildable
--  interface allows to specify the model columns with a <columns> element that
--  may contain multiple <column> elements, each specifying one model column.
--  The "type" attribute specifies the data type for the column.
--
--  An example of a UI Definition fragment for a tree store: |[ <object
--  class="GtkTreeStore"> <columns> <column type="gchararray"/> <column
--  type="gchararray"/> <column type="gint"/> </columns> </object> ]|
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
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

package Gtk.Tree_Store is

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

   procedure Gtk_New (Tree_Store : out Gtk_Tree_Store; Types : GType_Array);
   procedure Initialize
      (Tree_Store : not null access Gtk_Tree_Store_Record'Class;
       Types      : GType_Array);
   --  Non vararg creation function. Used primarily by language bindings.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "types": an array of GType types for the columns, from first to last

   function Gtk_Tree_Store_Newv (Types : GType_Array) return Gtk_Tree_Store;
   --  Non vararg creation function. Used primarily by language bindings.
   --  "types": an array of GType types for the columns, from first to last

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tree_store_get_type");

   -------------
   -- Methods --
   -------------

   procedure Append
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Appends a new row to Tree_Store. If Parent is non-null, then it will
   --  append the new row after the last child of Parent, otherwise it will
   --  append a row to the top level. Iter will be changed to point to this new
   --  row. The row will be empty after this function is called. To fill in
   --  values, you need to call gtk_tree_store_set or Gtk.Tree_Store.Set_Value.
   --  "iter": An unset Gtk.Tree_Model.Gtk_Tree_Iter to set to the appended
   --  row
   --  "parent": A valid Gtk.Tree_Model.Gtk_Tree_Iter, or null

   procedure Clear (Tree_Store : not null access Gtk_Tree_Store_Record);
   --  Removes all rows from Tree_Store

   procedure Insert
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
       Position   : Glib.Gint);
   --  Creates a new row at Position. If parent is non-null, then the row will
   --  be made a child of Parent. Otherwise, the row will be created at the
   --  toplevel. If Position is -1 or is larger than the number of rows at that
   --  level, then the new row will be inserted to the end of the list. Iter
   --  will be changed to point to this new row. The row will be empty after
   --  this function is called. To fill in values, you need to call
   --  gtk_tree_store_set or Gtk.Tree_Store.Set_Value.
   --  "iter": An unset Gtk.Tree_Model.Gtk_Tree_Iter to set to the new row
   --  "parent": A valid Gtk.Tree_Model.Gtk_Tree_Iter, or null
   --  "position": position to insert the new row, or -1 for last

   procedure Insert_After
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
       Sibling    : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Inserts a new row after Sibling. If Sibling is null, then the row will
   --  be prepended to Parent 's children. If Parent and Sibling are null, then
   --  the row will be prepended to the toplevel. If both Sibling and Parent
   --  are set, then Parent must be the parent of Sibling. When Sibling is set,
   --  Parent is optional.
   --  Iter will be changed to point to this new row. The row will be empty
   --  after this function is called. To fill in values, you need to call
   --  gtk_tree_store_set or Gtk.Tree_Store.Set_Value.
   --  "iter": An unset Gtk.Tree_Model.Gtk_Tree_Iter to set to the new row
   --  "parent": A valid Gtk.Tree_Model.Gtk_Tree_Iter, or null
   --  "sibling": A valid Gtk.Tree_Model.Gtk_Tree_Iter, or null

   procedure Insert_Before
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
       Sibling    : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Inserts a new row before Sibling. If Sibling is null, then the row will
   --  be appended to Parent 's children. If Parent and Sibling are null, then
   --  the row will be appended to the toplevel. If both Sibling and Parent are
   --  set, then Parent must be the parent of Sibling. When Sibling is set,
   --  Parent is optional.
   --  Iter will be changed to point to this new row. The row will be empty
   --  after this function is called. To fill in values, you need to call
   --  gtk_tree_store_set or Gtk.Tree_Store.Set_Value.
   --  "iter": An unset Gtk.Tree_Model.Gtk_Tree_Iter to set to the new row
   --  "parent": A valid Gtk.Tree_Model.Gtk_Tree_Iter, or null
   --  "sibling": A valid Gtk.Tree_Model.Gtk_Tree_Iter, or null

   function Is_Ancestor
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Descendant : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  Returns True if Iter is an ancestor of Descendant. That is, Iter is the
   --  parent (or grandparent or great-grandparent) of Descendant.
   --  "iter": A valid Gtk.Tree_Model.Gtk_Tree_Iter
   --  "descendant": A valid Gtk.Tree_Model.Gtk_Tree_Iter

   function Iter_Depth
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gint;
   --  Returns the depth of Iter. This will be 0 for anything on the root
   --  level, 1 for anything down a level, etc.
   --  "iter": A valid Gtk.Tree_Model.Gtk_Tree_Iter

   function Iter_Is_Valid
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  WARNING: This function is slow. Only use it for debugging and/or
   --  testing purposes.
   --  Checks if the given iter is a valid iter for this
   --  Gtk.Tree_Store.Gtk_Tree_Store.
   --  Since: gtk+ 2.2
   --  "iter": A Gtk.Tree_Model.Gtk_Tree_Iter.

   procedure Move_After
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Position   : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Moves Iter in Tree_Store to the position after Position. Iter and
   --  Position should be in the same level. Note that this function only works
   --  with unsorted stores. If Position is null, Iter will be moved to the
   --  start of the level.
   --  Since: gtk+ 2.2
   --  "iter": A Gtk.Tree_Model.Gtk_Tree_Iter.
   --  "position": A Gtk.Tree_Model.Gtk_Tree_Iter.

   procedure Move_Before
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Position   : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Moves Iter in Tree_Store to the position before Position. Iter and
   --  Position should be in the same level. Note that this function only works
   --  with unsorted stores. If Position is null, Iter will be moved to the end
   --  of the level.
   --  Since: gtk+ 2.2
   --  "iter": A Gtk.Tree_Model.Gtk_Tree_Iter.
   --  "position": A Gtk.Tree_Model.Gtk_Tree_Iter or null.

   procedure Prepend
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Prepends a new row to Tree_Store. If Parent is non-null, then it will
   --  prepend the new row before the first child of Parent, otherwise it will
   --  prepend a row to the top level. Iter will be changed to point to this
   --  new row. The row will be empty after this function is called. To fill in
   --  values, you need to call gtk_tree_store_set or Gtk.Tree_Store.Set_Value.
   --  "iter": An unset Gtk.Tree_Model.Gtk_Tree_Iter to set to the prepended
   --  row
   --  "parent": A valid Gtk.Tree_Model.Gtk_Tree_Iter, or null

   procedure Remove
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Removes Iter from Tree_Store. After being removed, Iter is set to the
   --  next valid row at that level, or invalidated if it previously pointed to
   --  the last one.
   --  "iter": A valid Gtk.Tree_Model.Gtk_Tree_Iter

   procedure Reorder
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
       New_Order  : Gint_Array);
   --  Reorders the children of Parent in Tree_Store to follow the order
   --  indicated by New_Order. Note that this function only works with unsorted
   --  stores.
   --  Since: gtk+ 2.2
   --  "parent": A Gtk.Tree_Model.Gtk_Tree_Iter, or null
   --  "new_order": an array of integers mapping the new position of each
   --  child to its old position before the re-ordering, i.e.
   --  New_Order`[newpos] = oldpos`.

   procedure Set_Column_Types
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Types      : GType_Array);
   --  This function is meant primarily for GObjects that inherit from
   --  Gtk.Tree_Store.Gtk_Tree_Store, and should only be used when constructing
   --  a new Gtk.Tree_Store.Gtk_Tree_Store. It will not function after a row
   --  has been added, or a method on the Gtk.Tree_Model.Gtk_Tree_Model
   --  interface is called.
   --  "types": An array of GType types, one for each column

   procedure Set_Value
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Column     : Glib.Gint;
       Value      : Glib.Values.GValue);
   --  Sets the data in the cell specified by Iter and Column. The type of
   --  Value must be convertible to the type of the column.
   --  "iter": A valid Gtk.Tree_Model.Gtk_Tree_Iter for the row being modified
   --  "column": column number to modify
   --  "value": new value for the cell

   procedure Swap
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       A          : Gtk.Tree_Model.Gtk_Tree_Iter;
       B          : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Swaps A and B in the same level of Tree_Store. Note that this function
   --  only works with unsorted stores.
   --  Since: gtk+ 2.2
   --  "a": A Gtk.Tree_Model.Gtk_Tree_Iter.
   --  "b": Another Gtk.Tree_Model.Gtk_Tree_Iter.

   procedure Foreach
      (Tree_Model : not null access Gtk_Tree_Store_Record;
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
         (Tree_Model : not null access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
          Func       : Gtk_Tree_Model_Foreach_Func;
          User_Data  : User_Data_Type);
      --  Calls func on each node in model in a depth-first fashion.
      --  If Func returns True, then the tree ceases to be walked, and
      --  Gtk.Tree_Model.Foreach returns.
      --  "func": a function to be called on each row
      --  "user_data": user data to passed to Func

   end Foreach_User_Data;

   procedure Set_Default_Sort_Func
      (Sortable  : not null access Gtk_Tree_Store_Record;
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
         (Sortable  : not null access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
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
      (Sortable       : not null access Gtk_Tree_Store_Record;
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
         (Sortable       : not null access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
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
     (Tree : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class)
   return Gint;
   --  Freeze the sorting in the tree view, and returns the current
   --  sort_column_id, which should be used when thawing. (See Thaw_Sort)

   procedure Thaw_Sort
     (Tree      : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
      Column_Id : Gint);
   --  Thaw a frozen tree_view. Column_Id should be the value returned by
   --  the corresponding call to Freeze_Sort.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Drag_Data_Received
      (Self           : not null access Gtk_Tree_Store_Record;
       Dest           : Gtk.Tree_Model.Gtk_Tree_Path;
       Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data)
       return Boolean;

   function Row_Drop_Possible
      (Self           : not null access Gtk_Tree_Store_Record;
       Dest_Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data)
       return Boolean;

   function Drag_Data_Delete
      (Self : not null access Gtk_Tree_Store_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;

   function Drag_Data_Get
      (Self           : not null access Gtk_Tree_Store_Record;
       Path           : Gtk.Tree_Model.Gtk_Tree_Path;
       Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data)
       return Boolean;

   function Row_Draggable
      (Self : not null access Gtk_Tree_Store_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;

   function Get_Column_Type
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Index      : Glib.Gint) return GType;

   function Get_Flags
      (Tree_Model : not null access Gtk_Tree_Store_Record)
       return Gtk.Tree_Model.Tree_Model_Flags;

   function Get_Iter
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Get_Iter_First
      (Tree_Model : not null access Gtk_Tree_Store_Record)
       return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Get_Iter_From_String
      (Tree_Model  : not null access Gtk_Tree_Store_Record;
       Path_String : UTF8_String) return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Get_N_Columns
      (Tree_Model : not null access Gtk_Tree_Store_Record) return Glib.Gint;

   function Get_Path
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Path;

   function Get_String_From_Iter
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return UTF8_String;

   procedure Get_Value
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Column     : Glib.Gint;
       Value      : out Glib.Values.GValue);

   function Children
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Has_Child
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;

   function N_Children
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
       return Glib.Gint;

   procedure Next
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);

   function Nth_Child
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
       N          : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Parent
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Child      : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Iter;

   procedure Previous
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);

   procedure Ref_Node
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);

   procedure Row_Changed
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);

   procedure Row_Deleted
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path);

   procedure Row_Has_Child_Toggled
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);

   procedure Row_Inserted
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);

   procedure Rows_Reordered
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       New_Order  : Gint_Array);

   procedure Rows_Reordered_With_Length
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       New_Order  : Gint_Array;
       Length     : Glib.Gint);

   procedure Unref_Node
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);

   procedure Get_Sort_Column_Id
      (Sortable       : not null access Gtk_Tree_Store_Record;
       Sort_Column_Id : out Glib.Gint;
       Order          : out Gtk.Enums.Gtk_Sort_Type);

   procedure Set_Sort_Column_Id
      (Sortable       : not null access Gtk_Tree_Store_Record;
       Sort_Column_Id : Glib.Gint;
       Order          : Gtk.Enums.Gtk_Sort_Type);

   function Has_Default_Sort_Func
      (Sortable : not null access Gtk_Tree_Store_Record) return Boolean;

   procedure Sort_Column_Changed
      (Sortable : not null access Gtk_Tree_Store_Record);

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
     (Gtk.Buildable.Gtk_Buildable, Gtk_Tree_Store_Record, Gtk_Tree_Store);
   function "+"
     (Widget : access Gtk_Tree_Store_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Tree_Store
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Tree_Drag_Dest is new Glib.Types.Implements
     (Gtk.Tree_Drag_Dest.Gtk_Tree_Drag_Dest, Gtk_Tree_Store_Record, Gtk_Tree_Store);
   function "+"
     (Widget : access Gtk_Tree_Store_Record'Class)
   return Gtk.Tree_Drag_Dest.Gtk_Tree_Drag_Dest
   renames Implements_Gtk_Tree_Drag_Dest.To_Interface;
   function "-"
     (Interf : Gtk.Tree_Drag_Dest.Gtk_Tree_Drag_Dest)
   return Gtk_Tree_Store
   renames Implements_Gtk_Tree_Drag_Dest.To_Object;

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
