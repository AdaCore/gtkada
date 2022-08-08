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
--  The Gtk.Tree_Model.Gtk_Tree_Model interface defines a generic tree
--  interface for use by the Gtk.Tree_View.Gtk_Tree_View widget. It is an
--  abstract interface, and is designed to be usable with any appropriate data
--  structure. The programmer just has to implement this interface on their own
--  data type for it to be viewable by a Gtk.Tree_View.Gtk_Tree_View widget.
--
--  The model is represented as a hierarchical tree of strongly-typed,
--  columned data. In other words, the model can be seen as a tree where every
--  node has different values depending on which column is being queried. The
--  type of data found in a column is determined by using the GType system (ie.
--  G_TYPE_INT, GTK_TYPE_BUTTON, G_TYPE_POINTER, etc). The types are
--  homogeneous per column across all nodes. It is important to note that this
--  interface only provides a way of examining a model and observing changes.
--  The implementation of each individual model decides how and if changes are
--  made.
--
--  In order to make life simpler for programmers who do not need to write
--  their own specialized model, two generic models are provided — the
--  Gtk.Tree_Store.Gtk_Tree_Store and the Gtk.List_Store.Gtk_List_Store. To use
--  these, the developer simply pushes data into these models as necessary.
--  These models provide the data structure as well as all appropriate tree
--  interfaces. As a result, implementing drag and drop, sorting, and storing
--  data is trivial. For the vast majority of trees and lists, these two models
--  are sufficient.
--
--  Models are accessed on a node/column level of granularity. One can query
--  for the value of a model at a certain node and a certain column on that
--  node. There are two structures used to reference a particular node in a
--  model. They are the Gtk.Tree_Model.Gtk_Tree_Path-struct and the
--  Gtk.Tree_Model.Gtk_Tree_Iter-struct ("iter" is short for iterator). Most of
--  the interface consists of operations on a
--  Gtk.Tree_Model.Gtk_Tree_Iter-struct.
--
--  A path is essentially a potential node. It is a location on a model that
--  may or may not actually correspond to a node on a specific model. The
--  Gtk.Tree_Model.Gtk_Tree_Path-struct can be converted into either an array
--  of unsigned integers or a string. The string form is a list of numbers
--  separated by a colon. Each number refers to the offset at that level. Thus,
--  the path `0` refers to the root node and the path `2:4` refers to the fifth
--  child of the third node.
--
--  By contrast, a Gtk.Tree_Model.Gtk_Tree_Iter-struct is a reference to a
--  specific node on a specific model. It is a generic struct with an integer
--  and three generic pointers. These are filled in by the model in a
--  model-specific way. One can convert a path to an iterator by calling
--  Gtk.Tree_Model.Get_Iter. These iterators are the primary way of accessing a
--  model and are similar to the iterators used by
--  Gtk.Text_Buffer.Gtk_Text_Buffer. They are generally statically allocated on
--  the stack and only used for a short time. The model interface defines a set
--  of operations using them for navigating the model.
--
--  It is expected that models fill in the iterator with private data. For
--  example, the Gtk.List_Store.Gtk_List_Store model, which is internally a
--  simple linked list, stores a list node in one of the pointers. The
--  Gtk.Tree_Model_Sort.Gtk_Tree_Model_Sort stores an array and an offset in
--  two of the pointers. Additionally, there is an integer field. This field is
--  generally filled with a unique stamp per model. This stamp is for catching
--  errors resulting from using invalid iterators with a model.
--
--  The lifecycle of an iterator can be a little confusing at first. Iterators
--  are expected to always be valid for as long as the model is unchanged (and
--  doesn't emit a signal). The model is considered to own all outstanding
--  iterators and nothing needs to be done to free them from the user's point
--  of view. Additionally, some models guarantee that an iterator is valid for
--  as long as the node it refers to is valid (most notably the
--  Gtk.Tree_Store.Gtk_Tree_Store and Gtk.List_Store.Gtk_List_Store). Although
--  generally uninteresting, as one always has to allow for the case where
--  iterators do not persist beyond a signal, some very important performance
--  enhancements were made in the sort model. As a result, the
--  GTK_TREE_MODEL_ITERS_PERSIST flag was added to indicate this behavior.
--
--  To help show some common operation of a model, some examples are provided.
--  The first example shows three ways of getting the iter at the location
--  `3:2:5`. While the first method shown is easier, the second is much more
--  common, as you often get paths from callbacks.
--
--  ## Acquiring a Gtk.Tree_Model.Gtk_Tree_Iter-struct
--
--  |[<!-- language="C" --> // Three ways of getting the iter pointing to the
--  location GtkTreePath *path; GtkTreeIter iter; GtkTreeIter parent_iter;
--
--  // get the iterator from a string gtk_tree_model_get_iter_from_string
--  (model, &iter, "3:2:5");
--
--  // get the iterator from a path path = gtk_tree_path_new_from_string
--  ("3:2:5"); gtk_tree_model_get_iter (model, &iter, path); gtk_tree_path_free
--  (path);
--
--  // walk the tree to find the iterator gtk_tree_model_iter_nth_child
--  (model, &iter, NULL, 3); parent_iter = iter; gtk_tree_model_iter_nth_child
--  (model, &iter, &parent_iter, 2); parent_iter = iter;
--  gtk_tree_model_iter_nth_child (model, &iter, &parent_iter, 5); ]|
--
--  This second example shows a quick way of iterating through a list and
--  getting a string and an integer from each row. The populate_model function
--  used below is not shown, as it is specific to the
--  Gtk.List_Store.Gtk_List_Store. For information on how to write such a
--  function, see the Gtk.List_Store.Gtk_List_Store documentation.
--
--  ## Reading data from a Gtk.Tree_Model.Gtk_Tree_Model
--
--  |[<!-- language="C" --> enum { STRING_COLUMN, INT_COLUMN, N_COLUMNS };
--
--  ...
--
--  GtkTreeModel *list_store; GtkTreeIter iter; gboolean valid; gint row_count
--  = 0;
--
--  // make a new list_store list_store = gtk_list_store_new (N_COLUMNS,
--  G_TYPE_STRING, G_TYPE_INT);
--
--  // Fill the list store with data populate_model (list_store);
--
--  // Get the first iter in the list, check it is valid and walk // through
--  the list, reading each row.
--
--  valid = gtk_tree_model_get_iter_first (list_store, &iter); while (valid) {
--  gchar *str_data; gint int_data;
--
--  // Make sure you terminate calls to gtk_tree_model_get with a "-1" value
--  gtk_tree_model_get (list_store, &iter, STRING_COLUMN, &str_data,
--  INT_COLUMN, &int_data, -1);
--
--  // Do something with the data g_print ("Row %d: (%s,%d)\n", row_count,
--  str_data, int_data); g_free (str_data);
--
--  valid = gtk_tree_model_iter_next (list_store, &iter); row_count++; } ]|
--
--  The Gtk.Tree_Model.Gtk_Tree_Model interface contains two methods for
--  reference counting: Gtk.Tree_Model.Ref_Node and Gtk.Tree_Model.Unref_Node.
--  These two methods are optional to implement. The reference counting is
--  meant as a way for views to let models know when nodes are being displayed.
--  Gtk.Tree_View.Gtk_Tree_View will take a reference on a node when it is
--  visible, which means the node is either in the toplevel or expanded. Being
--  displayed does not mean that the node is currently directly visible to the
--  user in the viewport. Based on this reference counting scheme a caching
--  model, for example, can decide whether or not to cache a node based on the
--  reference count. A file-system based model would not want to keep the
--  entire file hierarchy in memory, but just the folders that are currently
--  expanded in every current view.
--
--  When working with reference counting, the following rules must be taken
--  into account:
--
--  - Never take a reference on a node without owning a reference on its
--  parent. This means that all parent nodes of a referenced node must be
--  referenced as well.
--
--  - Outstanding references on a deleted node are not released. This is not
--  possible because the node has already been deleted by the time the
--  row-deleted signal is received.
--
--  - Models are not obligated to emit a signal on rows of which none of its
--  siblings are referenced. To phrase this differently, signals are only
--  required for levels in which nodes are referenced. For the root level
--  however, signals must be emitted at all times (however the root level is
--  always referenced when any view is attached).
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Glist;              use Glib.Glist;
with Glib.Object;             use Glib.Object;
with Glib.Types;              use Glib.Types;
with Glib.Values;             use Glib.Values;

package Gtk.Tree_Model is

   type Gtk_Tree_Model is new Glib.Types.GType_Interface;
   Null_Gtk_Tree_Model : constant Gtk_Tree_Model;

   type Tree_Model_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Tree_Model_Flags);
   --  These flags indicate various properties of a
   --  Gtk.Tree_Model.Gtk_Tree_Model.
   --
   --  They are returned by Gtk.Tree_Model.Get_Flags, and must be static for
   --  the lifetime of the object. A more complete description of
   --  GTK_TREE_MODEL_ITERS_PERSIST can be found in the overview of this
   --  section.

   Tree_Model_Iters_Persist : constant Tree_Model_Flags := 1;
   Tree_Model_List_Only : constant Tree_Model_Flags := 2;

   type Gtk_Tree_Iter is private;
   function From_Object_Free (B : access Gtk_Tree_Iter) return Gtk_Tree_Iter;
   pragma Inline (From_Object_Free);
   --  The Gtk.Tree_Model.Gtk_Tree_Iter is the primary structure for accessing
   --  a Gtk.Tree_Model.Gtk_Tree_Model. Models are expected to put a unique
   --  integer in the Stamp member, and put model-specific data in the three
   --  User_Data members.

   Null_Iter : constant Gtk_Tree_Iter;

   type Gtk_Tree_Path is new Glib.C_Boxed with null record;
   Null_Gtk_Tree_Path : constant Gtk_Tree_Path;

   function From_Object (Object : System.Address) return Gtk_Tree_Path;
   function From_Object_Free (B : access Gtk_Tree_Path'Class) return Gtk_Tree_Path;
   pragma Inline (From_Object_Free, From_Object);

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Tree_Model_Foreach_Func is access function
     (Model : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path;
      Iter  : Gtk_Tree_Iter) return Boolean;
   --  Type of the callback passed to Gtk.Tree_Model.Foreach to iterate over
   --  the rows in a tree model.
   --  "model": the Gtk.Tree_Model.Gtk_Tree_Model being iterated
   --  "path": the current Gtk.Tree_Model.Gtk_Tree_Path
   --  "iter": the current Gtk.Tree_Model.Gtk_Tree_Iter

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Tree_Model_Flags_Properties is
      new Generic_Internal_Discrete_Property (Tree_Model_Flags);
   type Property_Tree_Model_Flags is new Tree_Model_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_Filter_New
      (Tree_Model : out Gtk_Tree_Model;
       Root       : Gtk_Tree_Path);
   --  Creates a new Gtk.Tree_Model.Gtk_Tree_Model, with Child_Model as the
   --  child_model and Root as the virtual root.
   --  Since: gtk+ 2.4
   --  "root": A Gtk.Tree_Model.Gtk_Tree_Path or null.

   function Gtk_Tree_Model_Filter_New
      (Root : Gtk_Tree_Path) return Gtk_Tree_Model;
   --  Creates a new Gtk.Tree_Model.Gtk_Tree_Model, with Child_Model as the
   --  child_model and Root as the virtual root.
   --  Since: gtk+ 2.4
   --  "root": A Gtk.Tree_Model.Gtk_Tree_Path or null.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tree_model_get_type");

   function Iter_Get_Type return Glib.GType;
   pragma Import (C, Iter_Get_Type, "gtk_tree_iter_get_type");

   procedure Gtk_New (Path : out Gtk_Tree_Path);
   --  Creates a new Gtk.Tree_Model.Gtk_Tree_Path-struct. This refers to a
   --  row.

   function Gtk_Tree_Path_New return Gtk_Tree_Path;
   --  Creates a new Gtk.Tree_Model.Gtk_Tree_Path-struct. This refers to a
   --  row.

   procedure Gtk_New_First (Path : out Gtk_Tree_Path);
   --  Creates a new Gtk.Tree_Model.Gtk_Tree_Path-struct.
   --  The string representation of this path is "0".

   function Gtk_Tree_Path_New_First return Gtk_Tree_Path;
   --  Creates a new Gtk.Tree_Model.Gtk_Tree_Path-struct.
   --  The string representation of this path is "0".

   procedure Gtk_New_From_Indicesv
      (Path    : out Gtk_Tree_Path;
       Indices : Gint_Array;
       Length  : Gsize);
   --  Creates a new path with the given Indices array of Length.
   --  Since: gtk+ 3.12
   --  "indices": array of indices
   --  "length": length of Indices array

   function Gtk_Tree_Path_New_From_Indicesv
      (Indices : Gint_Array;
       Length  : Gsize) return Gtk_Tree_Path;
   --  Creates a new path with the given Indices array of Length.
   --  Since: gtk+ 3.12
   --  "indices": array of indices
   --  "length": length of Indices array

   procedure Gtk_New (Self : out Gtk_Tree_Path; Path : UTF8_String);
   --  Creates a new Gtk.Tree_Model.Gtk_Tree_Path-struct initialized to Path.
   --  Path is expected to be a colon separated list of numbers. For example,
   --  the string "10:4:0" would create a path of depth 3 pointing to the 11th
   --  child of the root node, the 5th child of that 11th child, and the 1st
   --  child of that 5th child. If an invalid path string is passed in, null is
   --  returned.
   --  "path": The string representation of a path

   function Gtk_Tree_Path_New_From_String
      (Path : UTF8_String) return Gtk_Tree_Path;
   --  Creates a new Gtk.Tree_Model.Gtk_Tree_Path-struct initialized to Path.
   --  Path is expected to be a colon separated list of numbers. For example,
   --  the string "10:4:0" would create a path of depth 3 pointing to the 11th
   --  child of the root node, the 5th child of that 11th child, and the 1st
   --  child of that 5th child. If an invalid path string is passed in, null is
   --  returned.
   --  "path": The string representation of a path

   function Path_Get_Type return Glib.GType;
   pragma Import (C, Path_Get_Type, "gtk_tree_path_get_type");

   -------------
   -- Methods --
   -------------

   procedure Foreach
      (Tree_Model : Gtk_Tree_Model;
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
         (Tree_Model : Gtk.Tree_Model.Gtk_Tree_Model;
          Func       : Gtk_Tree_Model_Foreach_Func;
          User_Data  : User_Data_Type);
      --  Calls func on each node in model in a depth-first fashion.
      --  If Func returns True, then the tree ceases to be walked, and
      --  Gtk.Tree_Model.Foreach returns.
      --  "func": a function to be called on each row
      --  "user_data": user data to passed to Func

   end Foreach_User_Data;

   function Get_Column_Type
      (Tree_Model : Gtk_Tree_Model;
       Index      : Glib.Gint) return GType;
   pragma Import (C, Get_Column_Type, "gtk_tree_model_get_column_type");
   --  Returns the type of the column.
   --  "index_": the column index

   function Get_Flags (Tree_Model : Gtk_Tree_Model) return Tree_Model_Flags;
   pragma Import (C, Get_Flags, "gtk_tree_model_get_flags");
   --  Returns a set of flags supported by this interface.
   --  The flags are a bitwise combination of Gtk.Tree_Model.Tree_Model_Flags.
   --  The flags supported should not change during the lifetime of the
   --  Tree_Model.

   function Get_Iter
      (Tree_Model : Gtk_Tree_Model;
       Path       : Gtk_Tree_Path) return Gtk_Tree_Iter;
   --  Sets Iter to a valid iterator pointing to Path. If Path does not exist,
   --  Iter is set to an invalid iterator and False is returned.
   --  "path": the Gtk.Tree_Model.Gtk_Tree_Path-struct

   function Get_Iter_First
      (Tree_Model : Gtk_Tree_Model) return Gtk_Tree_Iter;
   --  Initializes Iter with the first iterator in the tree (the one at the
   --  path "0") and returns True. Returns False if the tree is empty.

   function Get_Iter_From_String
      (Tree_Model  : Gtk_Tree_Model;
       Path_String : UTF8_String) return Gtk_Tree_Iter;
   --  Sets Iter to a valid iterator pointing to Path_String, if it exists.
   --  Otherwise, Iter is left invalid and False is returned.
   --  "path_string": a string representation of a
   --  Gtk.Tree_Model.Gtk_Tree_Path-struct

   function Get_N_Columns (Tree_Model : Gtk_Tree_Model) return Glib.Gint;
   pragma Import (C, Get_N_Columns, "gtk_tree_model_get_n_columns");
   --  Returns the number of columns supported by Tree_Model.

   function Get_Path
      (Tree_Model : Gtk_Tree_Model;
       Iter       : Gtk_Tree_Iter) return Gtk_Tree_Path;
   --  Returns a newly-created Gtk.Tree_Model.Gtk_Tree_Path-struct referenced
   --  by Iter.
   --  This path should be freed with Gtk.Tree_Model.Path_Free.
   --  "iter": the Gtk.Tree_Model.Gtk_Tree_Iter-struct

   function Get_String_From_Iter
      (Tree_Model : Gtk_Tree_Model;
       Iter       : Gtk_Tree_Iter) return UTF8_String;
   --  Generates a string representation of the iter.
   --  This string is a ":" separated list of numbers. For example, "4:10:0:3"
   --  would be an acceptable return value for this string.
   --  Since: gtk+ 2.2
   --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter-struct

   procedure Get_Value
      (Tree_Model : Gtk_Tree_Model;
       Iter       : Gtk_Tree_Iter;
       Column     : Glib.Gint;
       Value      : out Glib.Values.GValue);
   pragma Import (C, Get_Value, "gtk_tree_model_get_value");
   --  Initializes and sets Value to that at Column.
   --  When done with Value, g_value_unset needs to be called to free any
   --  allocated memory.
   --  "iter": the Gtk.Tree_Model.Gtk_Tree_Iter-struct
   --  "column": the column to lookup the value at
   --  "value": an empty Glib.Values.GValue to set

   function Children
      (Tree_Model : Gtk_Tree_Model;
       Parent     : Gtk_Tree_Iter) return Gtk_Tree_Iter;
   --  Sets Iter to point to the first child of Parent.
   --  If Parent has no children, False is returned and Iter is set to be
   --  invalid. Parent will remain a valid node after this function has been
   --  called.
   --  If Parent is null returns the first node, equivalent to
   --  `gtk_tree_model_get_iter_first (tree_model, iter);`
   --  "parent": the Gtk.Tree_Model.Gtk_Tree_Iter-struct, or null

   function Has_Child
      (Tree_Model : Gtk_Tree_Model;
       Iter       : Gtk_Tree_Iter) return Boolean;
   --  Returns True if Iter has children, False otherwise.
   --  "iter": the Gtk.Tree_Model.Gtk_Tree_Iter-struct to test for children

   function N_Children
      (Tree_Model : Gtk_Tree_Model;
       Iter       : Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
       return Glib.Gint;
   --  Returns the number of children that Iter has.
   --  As a special case, if Iter is null, then the number of toplevel nodes
   --  is returned.
   --  "iter": the Gtk.Tree_Model.Gtk_Tree_Iter-struct, or null

   procedure Next (Tree_Model : Gtk_Tree_Model; Iter : in out Gtk_Tree_Iter);
   --  Sets Iter to point to the node following it at the current level.
   --  If there is no next Iter, False is returned and Iter is set to be
   --  invalid.
   --  "iter": the Gtk.Tree_Model.Gtk_Tree_Iter-struct

   procedure Next (Path : Gtk_Tree_Path);
   --  Moves the Path to point to the next node at the current depth.

   function Nth_Child
      (Tree_Model : Gtk_Tree_Model;
       Parent     : Gtk_Tree_Iter;
       N          : Glib.Gint) return Gtk_Tree_Iter;
   --  Sets Iter to be the child of Parent, using the given index.
   --  The first index is 0. If N is too big, or Parent has no children, Iter
   --  is set to an invalid iterator and False is returned. Parent will remain
   --  a valid node after this function has been called. As a special case, if
   --  Parent is null, then the N-th root node is set.
   --  "parent": the Gtk.Tree_Model.Gtk_Tree_Iter-struct to get the child
   --  from, or null.
   --  "n": the index of the desired child

   function Parent
      (Tree_Model : Gtk_Tree_Model;
       Child      : Gtk_Tree_Iter) return Gtk_Tree_Iter;
   --  Sets Iter to be the parent of Child.
   --  If Child is at the toplevel, and doesn't have a parent, then Iter is
   --  set to an invalid iterator and False is returned. Child will remain a
   --  valid node after this function has been called.
   --  Iter will be initialized before the lookup is performed, so Child and
   --  Iter cannot point to the same memory location.
   --  "child": the Gtk.Tree_Model.Gtk_Tree_Iter-struct

   procedure Previous
      (Tree_Model : Gtk_Tree_Model;
       Iter       : in out Gtk_Tree_Iter);
   --  Sets Iter to point to the previous node at the current level.
   --  If there is no previous Iter, False is returned and Iter is set to be
   --  invalid.
   --  Since: gtk+ 3.0
   --  "iter": the Gtk.Tree_Model.Gtk_Tree_Iter-struct

   procedure Ref_Node (Tree_Model : Gtk_Tree_Model; Iter : Gtk_Tree_Iter);
   pragma Import (C, Ref_Node, "gtk_tree_model_ref_node");
   --  Lets the tree ref the node.
   --  This is an optional method for models to implement. To be more
   --  specific, models may ignore this call as it exists primarily for
   --  performance reasons.
   --  This function is primarily meant as a way for views to let caching
   --  models know when nodes are being displayed (and hence, whether or not to
   --  cache that node). Being displayed means a node is in an expanded branch,
   --  regardless of whether the node is currently visible in the viewport. For
   --  example, a file-system based model would not want to keep the entire
   --  file-hierarchy in memory, just the sections that are currently being
   --  displayed by every current view.
   --  A model should be expected to be able to get an iter independent of its
   --  reffed state.
   --  "iter": the Gtk.Tree_Model.Gtk_Tree_Iter-struct

   procedure Row_Changed
      (Tree_Model : Gtk_Tree_Model;
       Path       : Gtk_Tree_Path;
       Iter       : Gtk_Tree_Iter);
   --  Emits the Gtk.Tree_Model.Gtk_Tree_Model::row-changed signal on
   --  Tree_Model.
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path-struct pointing to the changed
   --  row
   --  "iter": a valid Gtk.Tree_Model.Gtk_Tree_Iter-struct pointing to the
   --  changed row

   procedure Row_Deleted (Tree_Model : Gtk_Tree_Model; Path : Gtk_Tree_Path);
   --  Emits the Gtk.Tree_Model.Gtk_Tree_Model::row-deleted signal on
   --  Tree_Model.
   --  This should be called by models after a row has been removed. The
   --  location pointed to by Path should be the location that the row
   --  previously was at. It may not be a valid location anymore.
   --  Nodes that are deleted are not unreffed, this means that any
   --  outstanding references on the deleted node should not be released.
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path-struct pointing to the previous
   --  location of the deleted row

   procedure Row_Has_Child_Toggled
      (Tree_Model : Gtk_Tree_Model;
       Path       : Gtk_Tree_Path;
       Iter       : Gtk_Tree_Iter);
   --  Emits the Gtk.Tree_Model.Gtk_Tree_Model::row-has-child-toggled signal
   --  on Tree_Model. This should be called by models after the child state of
   --  a node changes.
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path-struct pointing to the changed
   --  row
   --  "iter": a valid Gtk.Tree_Model.Gtk_Tree_Iter-struct pointing to the
   --  changed row

   procedure Row_Inserted
      (Tree_Model : Gtk_Tree_Model;
       Path       : Gtk_Tree_Path;
       Iter       : Gtk_Tree_Iter);
   --  Emits the Gtk.Tree_Model.Gtk_Tree_Model::row-inserted signal on
   --  Tree_Model.
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path-struct pointing to the inserted
   --  row
   --  "iter": a valid Gtk.Tree_Model.Gtk_Tree_Iter-struct pointing to the
   --  inserted row

   procedure Rows_Reordered
      (Tree_Model : Gtk_Tree_Model;
       Path       : Gtk_Tree_Path;
       Iter       : Gtk_Tree_Iter;
       New_Order  : Gint_Array);
   --  Emits the Gtk.Tree_Model.Gtk_Tree_Model::rows-reordered signal on
   --  Tree_Model.
   --  This should be called by models when their rows have been reordered.
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path-struct pointing to the tree node
   --  whose children have been reordered
   --  "iter": a valid Gtk.Tree_Model.Gtk_Tree_Iter-struct pointing to the
   --  node whose children have been reordered, or null if the depth of Path is
   --  0
   --  "new_order": an array of integers mapping the current position of each
   --  child to its old position before the re-ordering, i.e.
   --  New_Order`[newpos] = oldpos`

   procedure Rows_Reordered_With_Length
      (Tree_Model : Gtk_Tree_Model;
       Path       : Gtk_Tree_Path;
       Iter       : Gtk_Tree_Iter;
       New_Order  : Gint_Array;
       Length     : Glib.Gint);
   --  Emits the Gtk.Tree_Model.Gtk_Tree_Model::rows-reordered signal on
   --  Tree_Model.
   --  This should be called by models when their rows have been reordered.
   --  Since: gtk+ 3.10
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path-struct pointing to the tree node
   --  whose children have been reordered
   --  "iter": a valid Gtk.Tree_Model.Gtk_Tree_Iter-struct pointing to the
   --  node whose children have been reordered, or null if the depth of Path is
   --  0
   --  "new_order": an array of integers mapping the current position of each
   --  child to its old position before the re-ordering, i.e.
   --  New_Order`[newpos] = oldpos`
   --  "length": length of New_Order array

   procedure Unref_Node (Tree_Model : Gtk_Tree_Model; Iter : Gtk_Tree_Iter);
   pragma Import (C, Unref_Node, "gtk_tree_model_unref_node");
   --  Lets the tree unref the node.
   --  This is an optional method for models to implement. To be more
   --  specific, models may ignore this call as it exists primarily for
   --  performance reasons. For more information on what this means, see
   --  Gtk.Tree_Model.Ref_Node.
   --  Please note that nodes that are deleted are not unreffed.
   --  "iter": the Gtk.Tree_Model.Gtk_Tree_Iter-struct

   function Iter_Copy (Self : Gtk_Tree_Iter) return Gtk_Tree_Iter;
   pragma Import (C, Iter_Copy, "gtk_tree_iter_copy");
   --  Creates a dynamically allocated tree iterator as a copy of Iter.
   --  This function is not intended for use in applications, because you can
   --  just copy the structs by value (`GtkTreeIter new_iter = iter;`). You
   --  must free this iter with Gtk.Tree_Model.Free.

   procedure Free (Self : Gtk_Tree_Iter);
   pragma Import (C, Free, "gtk_tree_iter_free");
   --  Frees an iterator that has been allocated by Gtk.Tree_Model.Iter_Copy.
   --  This function is mainly used for language bindings.

   procedure Append_Index (Path : Gtk_Tree_Path; Index : Glib.Gint);
   --  Appends a new index to a path.
   --  As a result, the depth of the path is increased.
   --  "index_": the index

   function Compare
      (Path : Gtk_Tree_Path;
       B    : Gtk_Tree_Path) return Glib.Gint;
   --  Compares two paths.
   --  If A appears before B in a tree, then -1 is returned. If B appears
   --  before A, then 1 is returned. If the two nodes are equal, then 0 is
   --  returned.
   --  "b": a Gtk.Tree_Model.Gtk_Tree_Path-struct to compare with

   function Copy (Path : Gtk_Tree_Path) return Gtk_Tree_Path;
   --  Creates a new Gtk.Tree_Model.Gtk_Tree_Path-struct as a copy of Path.

   procedure Down (Path : Gtk_Tree_Path);
   --  Moves Path to point to the first child of the current path.

   procedure Path_Free (Path : Gtk_Tree_Path);
   --  Frees Path. If Path is null, it simply returns.

   function Get_Depth (Path : Gtk_Tree_Path) return Glib.Gint;
   --  Returns the current depth of Path.

   function Get_Indices (Path : Gtk_Tree_Path) return Glib.Gint_Array;
   --  Returns the current indices of Path.
   --  This is an array of integers, each representing a node in a tree. This
   --  value should not be freed.
   --  The length of the array can be obtained with Gtk.Tree_Model.Get_Depth.

   function Is_Ancestor
      (Path       : Gtk_Tree_Path;
       Descendant : Gtk_Tree_Path) return Boolean;
   --  Returns True if Descendant is a descendant of Path.
   --  "descendant": another Gtk.Tree_Model.Gtk_Tree_Path-struct

   function Is_Descendant
      (Path     : Gtk_Tree_Path;
       Ancestor : Gtk_Tree_Path) return Boolean;
   --  Returns True if Path is a descendant of Ancestor.
   --  "ancestor": another Gtk.Tree_Model.Gtk_Tree_Path-struct

   procedure Prepend_Index (Path : Gtk_Tree_Path; Index : Glib.Gint);
   --  Prepends a new index to a path.
   --  As a result, the depth of the path is increased.
   --  "index_": the index

   function Prev (Path : Gtk_Tree_Path) return Boolean;
   --  Moves the Path to point to the previous node at the current depth, if
   --  it exists.

   function To_String (Path : Gtk_Tree_Path) return UTF8_String;
   --  Generates a string representation of the path.
   --  This string is a ":" separated list of numbers. For example, "4:10:0:3"
   --  would be an acceptable return value for this string.

   function Up (Path : Gtk_Tree_Path) return Boolean;
   --  Moves the Path to point to its parent node, if it has a parent.

   ----------------------
   -- GtkAda additions --
   ----------------------

   type Gtk_Root_Tree_Model_Record is new Glib.Object.GObject_Record
   with null record;
   type Gtk_Root_Tree_Model is
      access all Gtk_Root_Tree_Model_Record'Class;
      --  A common base type for all objects that implement GtkTreeModel. This
      --  is used to conveniently provide a number of primitive operations.

      function Get_Int
        (Tree_Model : access Gtk_Root_Tree_Model_Record;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint) return Gint;
      function Get_Ulong
        (Tree_Model : access Gtk_Root_Tree_Model_Record;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint) return Gulong;
      function Get_Boolean
        (Tree_Model : access Gtk_Root_Tree_Model_Record;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint) return Boolean;
      function Get_Object
        (Tree_Model : access Gtk_Root_Tree_Model_Record;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint) return Glib.Object.GObject;
      function Get_C_Proxy
        (Tree_Model : access Gtk_Root_Tree_Model_Record;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint) return Glib.C_Proxy;
      function Get_String
        (Tree_Model : access Gtk_Root_Tree_Model_Record;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint) return UTF8_String;
      function Get_Address
        (Tree_Model : access Gtk_Root_Tree_Model_Record;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint) return System.Address;
      --  Get the value of one cell of the model

      function Get_Int
        (Tree_Model : Gtk_Tree_Model;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint) return Gint;
      function Get_Ulong
        (Tree_Model : Gtk_Tree_Model;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint) return Gulong;
      function Get_Boolean
        (Tree_Model : Gtk_Tree_Model;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint) return Boolean;
      function Get_Object
        (Tree_Model : Gtk_Tree_Model;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint) return Glib.Object.GObject;
      function Get_C_Proxy
        (Tree_Model : Gtk_Tree_Model;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint) return Glib.C_Proxy;
      function Get_String
        (Tree_Model : Gtk_Tree_Model;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint) return UTF8_String;
      function Get_Address
        (Tree_Model : Gtk_Tree_Model;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint) return System.Address;

      package Implements_Gtk_Tree_Model is new Glib.Types.Implements
        (Gtk_Tree_Model, Gtk_Root_Tree_Model_Record, Gtk_Root_Tree_Model);
      function To_Interface
        (Widget : access Gtk_Root_Tree_Model_Record'Class)
      return Gtk_Tree_Model renames Implements_Gtk_Tree_Model.To_Interface;
      function "-"
        (Interf : Gtk_Tree_Model) return Gtk_Root_Tree_Model
      renames Implements_Gtk_Tree_Model.To_Object;
      --  Convert from the gtk+ interface to an actual object. The return type
      --  depends on the exact model, and will likely be an instance of
      --  Gtk_Tree_Store'Class or Gtk_List_Store'Class depending on how you
      --  created it.

   function "=" (Left : Gtk_Tree_Iter; Right : Gtk_Tree_Iter) return Boolean;

   procedure Set_Tree_Iter
     (Val  : in out Glib.Values.GValue;
      Iter : Gtk_Tree_Iter);
   pragma Import (C, Set_Tree_Iter, "g_value_set_pointer");
   --  Set the value of the given GValue to Iter.
   --  Note that Iter is stored by reference, which means no copy of Iter
   --  is made. Iter should remain allocated as long as Val is being used.

   procedure Get_Tree_Iter
     (Val  : Glib.Values.GValue;
      Iter : out Gtk_Tree_Iter);
   --  Extract the iterator from the given GValue.
   --  Note that the iterator returned is a copy of the iterator referenced
   --  by the give GValue. Modifying the iterator returned does not modify
   --  the iterator referenced by the GValue.

   function Get_Tree_Iter (Val : Glib.Values.GValue) return Gtk_Tree_Iter;
   --  Extract the iterator from the given GValue.

   function To_Address (Iter : Gtk_Tree_Iter) return System.Address;
   pragma Convention (C, To_Address);
   --  Return address of the specified iterator.
   --  Note: To_Address needs a pass-by-reference semantic to work properly
   --  On some ABIs (e.g. IA64), Gtk_Tree_Iter is passed by copy, since it's
   --  a "small enough" record.

   function Iter_Or_Null (Iter : System.Address) return System.Address;
   --  Internal function for GtkAda

   function Get_Tree_Path (Val : Glib.Values.GValue) return Gtk_Tree_Path;
   --  Extract the path from the given GValue.

   -----------
   -- Lists --
   -----------

   function Convert (R : Gtk.Tree_Model.Gtk_Tree_Path) return System.Address;
   function Convert (R : System.Address) return Gtk.Tree_Model.Gtk_Tree_Path;
   package Gtk_Tree_Path_List is new Generic_List (Gtk.Tree_Model.Gtk_Tree_Path);

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Void is not null access procedure
     (Self : Gtk_Tree_Model;
      Path : Gtk_Tree_Path;
      Iter : Gtk_Tree_Iter);

   type Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Path : Gtk_Tree_Path;
      Iter : Gtk_Tree_Iter);

   Signal_Row_Changed : constant Glib.Signal_Name := "row-changed";
   procedure On_Row_Changed
      (Self  : Gtk_Tree_Model;
       Call  : Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Void;
       After : Boolean := False);
   procedure On_Row_Changed
      (Self  : Gtk_Tree_Model;
       Call  : Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when a row in the model has changed.
   -- 
   --  Callback parameters:
   --    --  "path": a Gtk.Tree_Model.Gtk_Tree_Path-struct identifying the changed
   --    --  row
   --    --  "iter": a valid Gtk.Tree_Model.Gtk_Tree_Iter-struct pointing to the
   --    --  changed row

   type Cb_Gtk_Tree_Model_Gtk_Tree_Path_Void is not null access procedure
     (Self : Gtk_Tree_Model;
      Path : Gtk_Tree_Path);

   type Cb_GObject_Gtk_Tree_Path_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Path : Gtk_Tree_Path);

   Signal_Row_Deleted : constant Glib.Signal_Name := "row-deleted";
   procedure On_Row_Deleted
      (Self  : Gtk_Tree_Model;
       Call  : Cb_Gtk_Tree_Model_Gtk_Tree_Path_Void;
       After : Boolean := False);
   procedure On_Row_Deleted
      (Self  : Gtk_Tree_Model;
       Call  : Cb_GObject_Gtk_Tree_Path_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when a row has been deleted.
   --
   --  Note that no iterator is passed to the signal handler, since the row is
   --  already deleted.
   --
   --  This should be called by models after a row has been removed. The
   --  location pointed to by Path should be the location that the row
   --  previously was at. It may not be a valid location anymore.

   Signal_Row_Has_Child_Toggled : constant Glib.Signal_Name := "row-has-child-toggled";
   procedure On_Row_Has_Child_Toggled
      (Self  : Gtk_Tree_Model;
       Call  : Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Void;
       After : Boolean := False);
   procedure On_Row_Has_Child_Toggled
      (Self  : Gtk_Tree_Model;
       Call  : Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when a row has gotten the first child row or
   --  lost its last child row.
   -- 
   --  Callback parameters:
   --    --  "path": a Gtk.Tree_Model.Gtk_Tree_Path-struct identifying the row
   --    --  "iter": a valid Gtk.Tree_Model.Gtk_Tree_Iter-struct pointing to the row

   Signal_Row_Inserted : constant Glib.Signal_Name := "row-inserted";
   procedure On_Row_Inserted
      (Self  : Gtk_Tree_Model;
       Call  : Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Void;
       After : Boolean := False);
   procedure On_Row_Inserted
      (Self  : Gtk_Tree_Model;
       Call  : Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when a new row has been inserted in the model.
   --
   --  Note that the row may still be empty at this point, since it is a
   --  common pattern to first insert an empty row, and then fill it with the
   --  desired values.
   -- 
   --  Callback parameters:
   --    --  "path": a Gtk.Tree_Model.Gtk_Tree_Path-struct identifying the new row
   --    --  "iter": a valid Gtk.Tree_Model.Gtk_Tree_Iter-struct pointing to the new
   --    --  row

   type Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void is not null access procedure
     (Self      : Gtk_Tree_Model;
      Path      : Gtk_Tree_Path;
      Iter      : Gtk_Tree_Iter;
      New_Order : System.Address);

   type Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void is not null access procedure
     (Self      : access Glib.Object.GObject_Record'Class;
      Path      : Gtk_Tree_Path;
      Iter      : Gtk_Tree_Iter;
      New_Order : System.Address);

   Signal_Rows_Reordered : constant Glib.Signal_Name := "rows-reordered";
   procedure On_Rows_Reordered
      (Self  : Gtk_Tree_Model;
       Call  : Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void;
       After : Boolean := False);
   procedure On_Rows_Reordered
      (Self  : Gtk_Tree_Model;
       Call  : Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when the children of a node in the
   --  Gtk.Tree_Model.Gtk_Tree_Model have been reordered.
   --
   --  Note that this signal is not emitted when rows are reordered by DND,
   --  since this is implemented by removing and then reinserting the row.
   -- 
   --  Callback parameters:
   --    --  "path": a Gtk.Tree_Model.Gtk_Tree_Path-struct identifying the tree node
   --    --  whose children have been reordered
   --    --  "iter": a valid Gtk.Tree_Model.Gtk_Tree_Iter-struct pointing to the
   --    --  node whose children have been reordered, or null if the depth of Path is
   --    --  0
   --    --  "new_order": an array of integers mapping the current position of each
   --    --  child to its old position before the re-ordering, i.e.
   --    --  New_Order`[newpos] = oldpos`

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Tree_Model"

   function "+" (W : Gtk_Tree_Model) return Gtk_Tree_Model;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Get_Column_Type is access function
     (Tree_Model : Gtk_Tree_Model;
      Index      : Glib.Gint) return GType;
   pragma Convention (C, Virtual_Get_Column_Type);
   --  Returns the type of the column.
   --  "index_": the column index

   type Virtual_Get_Flags is access function (Tree_Model : Gtk_Tree_Model) return Tree_Model_Flags;
   pragma Convention (C, Virtual_Get_Flags);
   --  Returns a set of flags supported by this interface.
   --  The flags are a bitwise combination of Gtk.Tree_Model.Tree_Model_Flags.
   --  The flags supported should not change during the lifetime of the
   --  Tree_Model.

   type Virtual_Get_Iter is access function
     (Tree_Model : Gtk_Tree_Model;
      Iter       : access Gtk_Tree_Iter;
      Path       : System.Address) return Glib.Gboolean;
   pragma Convention (C, Virtual_Get_Iter);
   --  Sets Iter to a valid iterator pointing to Path. If Path does not exist,
   --  Iter is set to an invalid iterator and False is returned.
   --  "iter": the uninitialized Gtk.Tree_Model.Gtk_Tree_Iter-struct
   --  "path": the Gtk.Tree_Model.Gtk_Tree_Path-struct

   type Virtual_Get_N_Columns is access function (Tree_Model : Gtk_Tree_Model) return Glib.Gint;
   pragma Convention (C, Virtual_Get_N_Columns);
   --  Returns the number of columns supported by Tree_Model.

   type Virtual_Get_Path is access function
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter) return System.Address;
   pragma Convention (C, Virtual_Get_Path);
   --  Returns a newly-created Gtk.Tree_Model.Gtk_Tree_Path-struct referenced
   --  by Iter.
   --  This path should be freed with gtk_tree_path_free.
   --  "iter": the Gtk.Tree_Model.Gtk_Tree_Iter-struct

   type Virtual_Get_Value is access procedure
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter;
      Column     : Glib.Gint;
      Value      : out Glib.Values.GValue);
   pragma Convention (C, Virtual_Get_Value);
   --  Initializes and sets Value to that at Column.
   --  When done with Value, g_value_unset needs to be called to free any
   --  allocated memory.
   --  "iter": the Gtk.Tree_Model.Gtk_Tree_Iter-struct
   --  "column": the column to lookup the value at
   --  "value": an empty Glib.Values.GValue to set

   type Virtual_Iter_Children is access function
     (Tree_Model : Gtk_Tree_Model;
      Iter       : access Gtk_Tree_Iter;
      Parent     : access Gtk_Tree_Iter) return Glib.Gboolean;
   pragma Convention (C, Virtual_Iter_Children);
   --  Sets Iter to point to the first child of Parent.
   --  If Parent has no children, False is returned and Iter is set to be
   --  invalid. Parent will remain a valid node after this function has been
   --  called.
   --  If Parent is null returns the first node, equivalent to
   --  `gtk_tree_model_get_iter_first (tree_model, iter);`
   --  "iter": the new Gtk.Tree_Model.Gtk_Tree_Iter-struct to be set to the
   --  child
   --  "parent": the Gtk.Tree_Model.Gtk_Tree_Iter-struct, or null

   type Virtual_Iter_Has_Child is access function
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter) return Glib.Gboolean;
   pragma Convention (C, Virtual_Iter_Has_Child);
   --  Returns True if Iter has children, False otherwise.
   --  "iter": the Gtk.Tree_Model.Gtk_Tree_Iter-struct to test for children

   type Virtual_Iter_N_Children is access function
     (Tree_Model : Gtk_Tree_Model;
      Iter       : access Gtk_Tree_Iter) return Glib.Gint;
   pragma Convention (C, Virtual_Iter_N_Children);
   --  Returns the number of children that Iter has.
   --  As a special case, if Iter is null, then the number of toplevel nodes
   --  is returned.
   --  "iter": the Gtk.Tree_Model.Gtk_Tree_Iter-struct, or null

   type Virtual_Iter_Next is access function
     (Tree_Model : Gtk_Tree_Model;
      Iter       : access Gtk_Tree_Iter) return Glib.Gboolean;
   pragma Convention (C, Virtual_Iter_Next);
   --  Sets Iter to point to the node following it at the current level.
   --  If there is no next Iter, False is returned and Iter is set to be
   --  invalid.
   --  "iter": the Gtk.Tree_Model.Gtk_Tree_Iter-struct

   type Virtual_Iter_Nth_Child is access function
     (Tree_Model : Gtk_Tree_Model;
      Iter       : access Gtk_Tree_Iter;
      Parent     : access Gtk_Tree_Iter;
      N          : Glib.Gint) return Glib.Gboolean;
   pragma Convention (C, Virtual_Iter_Nth_Child);
   --  Sets Iter to be the child of Parent, using the given index.
   --  The first index is 0. If N is too big, or Parent has no children, Iter
   --  is set to an invalid iterator and False is returned. Parent will remain
   --  a valid node after this function has been called. As a special case, if
   --  Parent is null, then the N-th root node is set.
   --  "iter": the Gtk.Tree_Model.Gtk_Tree_Iter-struct to set to the nth child
   --  "parent": the Gtk.Tree_Model.Gtk_Tree_Iter-struct to get the child
   --  from, or null.
   --  "n": the index of the desired child

   type Virtual_Iter_Parent is access function
     (Tree_Model : Gtk_Tree_Model;
      Iter       : access Gtk_Tree_Iter;
      Child      : Gtk_Tree_Iter) return Glib.Gboolean;
   pragma Convention (C, Virtual_Iter_Parent);
   --  Sets Iter to be the parent of Child.
   --  If Child is at the toplevel, and doesn't have a parent, then Iter is
   --  set to an invalid iterator and False is returned. Child will remain a
   --  valid node after this function has been called.
   --  Iter will be initialized before the lookup is performed, so Child and
   --  Iter cannot point to the same memory location.
   --  "iter": the new Gtk.Tree_Model.Gtk_Tree_Iter-struct to set to the
   --  parent
   --  "child": the Gtk.Tree_Model.Gtk_Tree_Iter-struct

   type Virtual_Iter_Previous is access function
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter) return Glib.Gboolean;
   pragma Convention (C, Virtual_Iter_Previous);
   --  Sets Iter to point to the previous node at the current level.
   --  If there is no previous Iter, False is returned and Iter is set to be
   --  invalid.
   --  Since: gtk+ 3.0
   --  "iter": the Gtk.Tree_Model.Gtk_Tree_Iter-struct

   type Virtual_Ref_Node is access procedure (Tree_Model : Gtk_Tree_Model; Iter : Gtk_Tree_Iter);
   pragma Convention (C, Virtual_Ref_Node);
   --  Lets the tree ref the node.
   --  This is an optional method for models to implement. To be more
   --  specific, models may ignore this call as it exists primarily for
   --  performance reasons.
   --  This function is primarily meant as a way for views to let caching
   --  models know when nodes are being displayed (and hence, whether or not to
   --  cache that node). Being displayed means a node is in an expanded branch,
   --  regardless of whether the node is currently visible in the viewport. For
   --  example, a file-system based model would not want to keep the entire
   --  file-hierarchy in memory, just the sections that are currently being
   --  displayed by every current view.
   --  A model should be expected to be able to get an iter independent of its
   --  reffed state.
   --  "iter": the Gtk.Tree_Model.Gtk_Tree_Iter-struct

   type Virtual_Row_Changed is access procedure
     (Tree_Model : Gtk_Tree_Model;
      Path       : System.Address;
      Iter       : Gtk_Tree_Iter);
   pragma Convention (C, Virtual_Row_Changed);
   --  Emits the Gtk.Tree_Model.Gtk_Tree_Model::row-changed signal on
   --  Tree_Model.
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path-struct pointing to the changed
   --  row
   --  "iter": a valid Gtk.Tree_Model.Gtk_Tree_Iter-struct pointing to the
   --  changed row

   type Virtual_Row_Deleted is access procedure (Tree_Model : Gtk_Tree_Model; Path : System.Address);
   pragma Convention (C, Virtual_Row_Deleted);
   --  Emits the Gtk.Tree_Model.Gtk_Tree_Model::row-deleted signal on
   --  Tree_Model.
   --  This should be called by models after a row has been removed. The
   --  location pointed to by Path should be the location that the row
   --  previously was at. It may not be a valid location anymore.
   --  Nodes that are deleted are not unreffed, this means that any
   --  outstanding references on the deleted node should not be released.
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path-struct pointing to the previous
   --  location of the deleted row

   type Virtual_Row_Has_Child_Toggled is access procedure
     (Tree_Model : Gtk_Tree_Model;
      Path       : System.Address;
      Iter       : Gtk_Tree_Iter);
   pragma Convention (C, Virtual_Row_Has_Child_Toggled);
   --  Emits the Gtk.Tree_Model.Gtk_Tree_Model::row-has-child-toggled signal
   --  on Tree_Model. This should be called by models after the child state of
   --  a node changes.
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path-struct pointing to the changed
   --  row
   --  "iter": a valid Gtk.Tree_Model.Gtk_Tree_Iter-struct pointing to the
   --  changed row

   type Virtual_Row_Inserted is access procedure
     (Tree_Model : Gtk_Tree_Model;
      Path       : System.Address;
      Iter       : Gtk_Tree_Iter);
   pragma Convention (C, Virtual_Row_Inserted);
   --  Emits the Gtk.Tree_Model.Gtk_Tree_Model::row-inserted signal on
   --  Tree_Model.
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path-struct pointing to the inserted
   --  row
   --  "iter": a valid Gtk.Tree_Model.Gtk_Tree_Iter-struct pointing to the
   --  inserted row

   type Virtual_Rows_Reordered is access procedure
     (Tree_Model : Gtk_Tree_Model;
      Path       : System.Address;
      Iter       : Gtk_Tree_Iter;
      New_Order  : in out Glib.Gint);
   pragma Convention (C, Virtual_Rows_Reordered);
   --  Emits the Gtk.Tree_Model.Gtk_Tree_Model::rows-reordered signal on
   --  Tree_Model.
   --  This should be called by models when their rows have been reordered.
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path-struct pointing to the tree node
   --  whose children have been reordered
   --  "iter": a valid Gtk.Tree_Model.Gtk_Tree_Iter-struct pointing to the
   --  node whose children have been reordered, or null if the depth of Path is
   --  0
   --  "new_order": an array of integers mapping the current position of each
   --  child to its old position before the re-ordering, i.e.
   --  New_Order`[newpos] = oldpos`

   type Virtual_Unref_Node is access procedure (Tree_Model : Gtk_Tree_Model; Iter : Gtk_Tree_Iter);
   pragma Convention (C, Virtual_Unref_Node);
   --  Lets the tree unref the node.
   --  This is an optional method for models to implement. To be more
   --  specific, models may ignore this call as it exists primarily for
   --  performance reasons. For more information on what this means, see
   --  Gtk.Tree_Model.Ref_Node.
   --  Please note that nodes that are deleted are not unreffed.
   --  "iter": the Gtk.Tree_Model.Gtk_Tree_Iter-struct

   subtype Tree_Model_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Get_Column_Type
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Get_Column_Type);
   pragma Import (C, Set_Get_Column_Type, "gtkada_Tree_Model_set_get_column_type");

   procedure Set_Get_Flags
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Get_Flags);
   pragma Import (C, Set_Get_Flags, "gtkada_Tree_Model_set_get_flags");

   procedure Set_Get_Iter
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Get_Iter);
   pragma Import (C, Set_Get_Iter, "gtkada_Tree_Model_set_get_iter");

   procedure Set_Get_N_Columns
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Get_N_Columns);
   pragma Import (C, Set_Get_N_Columns, "gtkada_Tree_Model_set_get_n_columns");

   procedure Set_Get_Path
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Get_Path);
   pragma Import (C, Set_Get_Path, "gtkada_Tree_Model_set_get_path");

   procedure Set_Get_Value
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Get_Value);
   pragma Import (C, Set_Get_Value, "gtkada_Tree_Model_set_get_value");

   procedure Set_Iter_Children
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Iter_Children);
   pragma Import (C, Set_Iter_Children, "gtkada_Tree_Model_set_iter_children");

   procedure Set_Iter_Has_Child
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Iter_Has_Child);
   pragma Import (C, Set_Iter_Has_Child, "gtkada_Tree_Model_set_iter_has_child");

   procedure Set_Iter_N_Children
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Iter_N_Children);
   pragma Import (C, Set_Iter_N_Children, "gtkada_Tree_Model_set_iter_n_children");

   procedure Set_Iter_Next
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Iter_Next);
   pragma Import (C, Set_Iter_Next, "gtkada_Tree_Model_set_iter_next");

   procedure Set_Iter_Nth_Child
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Iter_Nth_Child);
   pragma Import (C, Set_Iter_Nth_Child, "gtkada_Tree_Model_set_iter_nth_child");

   procedure Set_Iter_Parent
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Iter_Parent);
   pragma Import (C, Set_Iter_Parent, "gtkada_Tree_Model_set_iter_parent");

   procedure Set_Iter_Previous
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Iter_Previous);
   pragma Import (C, Set_Iter_Previous, "gtkada_Tree_Model_set_iter_previous");

   procedure Set_Ref_Node
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Ref_Node);
   pragma Import (C, Set_Ref_Node, "gtkada_Tree_Model_set_ref_node");

   procedure Set_Row_Changed
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Row_Changed);
   pragma Import (C, Set_Row_Changed, "gtkada_Tree_Model_set_row_changed");

   procedure Set_Row_Deleted
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Row_Deleted);
   pragma Import (C, Set_Row_Deleted, "gtkada_Tree_Model_set_row_deleted");

   procedure Set_Row_Has_Child_Toggled
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Row_Has_Child_Toggled);
   pragma Import (C, Set_Row_Has_Child_Toggled, "gtkada_Tree_Model_set_row_has_child_toggled");

   procedure Set_Row_Inserted
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Row_Inserted);
   pragma Import (C, Set_Row_Inserted, "gtkada_Tree_Model_set_row_inserted");

   procedure Set_Rows_Reordered
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Rows_Reordered);
   pragma Import (C, Set_Rows_Reordered, "gtkada_Tree_Model_set_rows_reordered");

   procedure Set_Unref_Node
     (Self    : Tree_Model_Interface_Descr;
      Handler : Virtual_Unref_Node);
   pragma Import (C, Set_Unref_Node, "gtkada_Tree_Model_set_unref_node");
   --  See Glib.Object.Add_Interface

private
type Gtk_Tree_Iter is record
   Stamp : Glib.Gint := 0;
   User_Data : System.Address := System.Null_Address;
   User_Data2 : System.Address := System.Null_Address;
   User_Data3 : System.Address := System.Null_Address;
end record;
pragma Convention (C, Gtk_Tree_Iter);


Null_Gtk_Tree_Model : constant Gtk_Tree_Model :=
   Gtk_Tree_Model (Glib.Types.Null_Interface);

   Null_Iter : constant Gtk_Tree_Iter :=
     (0, System.Null_Address, System.Null_Address, System.Null_Address);
       

   Null_Gtk_Tree_Path : constant Gtk_Tree_Path := (Glib.C_Boxed with null record);

end Gtk.Tree_Model;
