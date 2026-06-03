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

--  A `GtkTreeModel` which hides parts of an underlying tree model
--
--  A `GtkTreeModelFilter` is a tree model which wraps another tree model, and
--  can do the following things:
--
--  - Filter specific rows, based on data from a "visible column", a column
--  storing booleans indicating whether the row should be filtered or not, or
--  based on the return value of a "visible function", which gets a model, iter
--  and user_data and returns a boolean indicating whether the row should be
--  filtered or not.
--
--  - Modify the "appearance" of the model, using a modify function. This is
--  extremely powerful and allows for just changing some values and also for
--  creating a completely different model based on the given child model.
--
--  - Set a different root node, also known as a "virtual root". You can pass
--  in a `GtkTreePath` indicating the root node for the filter at construction
--  time.
--
--  The basic API is similar to `GtkTreeModelSort`. For an example on its
--  usage, see the section on `GtkTreeModelSort`.
--
--  When using `GtkTreeModelFilter`, it is important to realize that
--  `GtkTreeModelFilter` maintains an internal cache of all nodes which are
--  visible in its clients. The cache is likely to be a subtree of the tree
--  exposed by the child model. `GtkTreeModelFilter` will not cache the entire
--  child model when unnecessary to not compromise the caching mechanism that
--  is exposed by the reference counting scheme. If the child model implements
--  reference counting, unnecessary signals may not be emitted because of
--  reference counting rule 3, see the `GtkTreeModel` documentation. (Note that
--  e.g. `GtkTreeStore` does not implement reference counting and will always
--  emit all signals, even when the receiving node is not visible).
--
--  Because of this, limitations for possible visible functions do apply. In
--  general, visible functions should only use data or properties from the node
--  for which the visibility state must be determined, its siblings or its
--  parents. Usually, having a dependency on the state of any child node is not
--  possible, unless references are taken on these explicitly. When no such
--  reference exists, no signals may be received for these child nodes (see
--  reference counting rule number 3 in the `GtkTreeModel` section).
--
--  Determining the visibility state of a given node based on the state of its
--  child nodes is a frequently occurring use case. Therefore,
--  `GtkTreeModelFilter` explicitly supports this. For example, when a node
--  does not have any children, you might not want the node to be visible. As
--  soon as the first row is added to the node's child level (or the last row
--  removed), the node's visibility should be updated.
--
--  This introduces a dependency from the node on its child nodes. In order to
--  accommodate this, `GtkTreeModelFilter` must make sure the necessary signals
--  are received from the child model. This is achieved by building, for all
--  nodes which are exposed as visible nodes to `GtkTreeModelFilter`'s clients,
--  the child level (if any) and take a reference on the first node in this
--  level. Furthermore, for every row-inserted, row-changed or row-deleted
--  signal (also these which were not handled because the node was not cached),
--  `GtkTreeModelFilter` will check if the visibility state of any parent node
--  has changed.
--
--  Beware, however, that this explicit support is limited to these two cases.
--  For example, if you want a node to be visible only if two nodes in a
--  child's child level (2 levels deeper) are visible, you are on your own. In
--  this case, either rely on `GtkTreeStore` to emit all signals because it
--  does not implement reference counting, or for models that do implement
--  reference counting, obtain references on these child levels yourself.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Content_Provider; use Gdk.Content_Provider;
with Glib;                 use Glib;
with Glib.Properties;      use Glib.Properties;
with Glib.Types;           use Glib.Types;
with Glib.Values;          use Glib.Values;
with Gtk.Tree_Drag_Source; use Gtk.Tree_Drag_Source;
with Gtk.Tree_Model;       use Gtk.Tree_Model;

package Gtk.Tree_Model_Filter is

   pragma Obsolescent;
   --  Use [class@Gtk.FilterListModel] instead.

   type Gtk_Tree_Model_Filter_Record is new Gtk_Root_Tree_Model_Record with null record;
   type Gtk_Tree_Model_Filter is access all Gtk_Tree_Model_Filter_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Tree_Model_Filter_Modify_Func is access procedure
     (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Value  : out Glib.Values.GValue;
      Column : Glib.Gint);
   --  A function which calculates display values from raw values in the
   --  model. It must fill Value with the display value for the column Column
   --  in the row indicated by Iter.
   --  Since this function is called for each data access, it's not a
   --  particularly efficient operation.
   --  @param Model the `GtkTreeModelFilter`
   --  @param Iter a `GtkTreeIter` pointing to the row whose display values
   --  are determined
   --  @param Value A `GValue` which is already initialized for with the
   --  correct type for the column Column.
   --  @param Column the column whose display value is determined

   type Gtk_Tree_Model_Filter_Visible_Func is access function
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  A function which decides whether the row indicated by Iter is visible.
   --  @param Model the child model of the `GtkTreeModelFilter`
   --  @param Iter a `GtkTreeIter` pointing to the row in Model whose
   --  visibility is determined
   --  @return Whether the row indicated by Iter is visible.

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

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self        : out Gtk_Tree_Model_Filter;
       Child_Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Root        : Gtk.Tree_Model.Gtk_Tree_Path := Null_Gtk_Tree_Path);
   procedure Initialize
      (Self        : not null access Gtk_Tree_Model_Filter_Record'Class;
       Child_Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Root        : Gtk.Tree_Model.Gtk_Tree_Path := Null_Gtk_Tree_Path);
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Tree_Model_Filter_Filter_New
      (Child_Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Root        : Gtk.Tree_Model.Gtk_Tree_Path := Null_Gtk_Tree_Path)
       return Gtk_Tree_Model_Filter;

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tree_model_filter_get_type");

   -------------
   -- Methods --
   -------------

   procedure Clear_Cache
      (Self : not null access Gtk_Tree_Model_Filter_Record);
   pragma Obsolescent (Clear_Cache);
   --  This function should almost never be called. It clears the Filter of
   --  any cached iterators that haven't been reffed with
   --  Gtk.Tree_Model.Ref_Node. This might be useful if the child model being
   --  filtered is static (and doesn't change often) and there has been a lot
   --  of unreffed access to nodes. As a side effect of this function, all
   --  unreffed iters will be invalid.
   --  Deprecated since 4.10, 1

   procedure Convert_Child_Iter_To_Iter
      (Self        : not null access Gtk_Tree_Model_Filter_Record;
       Filter_Iter : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Child_Iter  : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Convert_Child_Iter_To_Iter);
   --  Sets Filter_Iter to point to the row in Filter that corresponds to the
   --  row pointed at by Child_Iter. If Filter_Iter was not set, False is
   --  returned.
   --  Deprecated since 4.10, 1
   --  @param Filter_Iter An uninitialized `GtkTreeIter`
   --  @param Child_Iter A valid `GtkTreeIter` pointing to a row on the child
   --  model.

   function Convert_Child_Path_To_Path
      (Self       : not null access Gtk_Tree_Model_Filter_Record;
       Child_Path : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gtk.Tree_Model.Gtk_Tree_Path;
   pragma Obsolescent (Convert_Child_Path_To_Path);
   --  Converts Child_Path to a path relative to Filter. That is, Child_Path
   --  points to a path in the child model. The rerturned path will point to
   --  the same row in the filtered model. If Child_Path isn't a valid path on
   --  the child model or points to a row which is not visible in Filter, then
   --  null is returned.
   --  Deprecated since 4.10, 1
   --  @param Child_Path A `GtkTreePath` to convert.
   --  @return A newly allocated `GtkTreePath`

   procedure Convert_Iter_To_Child_Iter
      (Self        : not null access Gtk_Tree_Model_Filter_Record;
       Child_Iter  : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Filter_Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Convert_Iter_To_Child_Iter);
   --  Sets Child_Iter to point to the row pointed to by Filter_Iter.
   --  Deprecated since 4.10, 1
   --  @param Child_Iter An uninitialized `GtkTreeIter`
   --  @param Filter_Iter A valid `GtkTreeIter` pointing to a row on Filter.

   function Convert_Path_To_Child_Path
      (Self        : not null access Gtk_Tree_Model_Filter_Record;
       Filter_Path : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gtk.Tree_Model.Gtk_Tree_Path;
   pragma Obsolescent (Convert_Path_To_Child_Path);
   --  Converts Filter_Path to a path on the child model of Filter. That is,
   --  Filter_Path points to a location in Filter. The returned path will point
   --  to the same location in the model not being filtered. If Filter_Path
   --  does not point to a location in the child model, null is returned.
   --  Deprecated since 4.10, 1
   --  @param Filter_Path A `GtkTreePath` to convert.
   --  @return A newly allocated `GtkTreePath`

   function Get_Model
      (Self : not null access Gtk_Tree_Model_Filter_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model;
   pragma Obsolescent (Get_Model);
   --  Returns a pointer to the child model of Filter.
   --  Deprecated since 4.10, 1
   --  @return A pointer to a `GtkTreeModel`

   procedure Refilter (Self : not null access Gtk_Tree_Model_Filter_Record);
   pragma Obsolescent (Refilter);
   --  Emits ::row_changed for each row in the child model, which causes the
   --  filter to re-evaluate whether a row is visible or not.
   --  Deprecated since 4.10, 1

   procedure Set_Modify_Func
      (Self  : not null access Gtk_Tree_Model_Filter_Record;
       Types : GType_Array;
       Func  : Gtk_Tree_Model_Filter_Modify_Func);
   pragma Obsolescent (Set_Modify_Func);
   --  With the N_Columns and Types parameters, you give an array of column
   --  types for this model (which will be exposed to the parent model/view).
   --  The Func, Data and Destroy parameters are for specifying the modify
   --  function. The modify function will get called for each data access, the
   --  goal of the modify function is to return the data which should be
   --  displayed at the location specified using the parameters of the modify
   --  function.
   --  Note that Gtk.Tree_Model_Filter.Set_Modify_Func can only be called once
   --  for a given filter model.
   --  Deprecated since 4.10, 1
   --  @param Types The `GType`s of the columns.
   --  @param Func A `GtkTreeModelFilterModifyFunc`

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Modify_Func_User_Data is

      type Gtk_Tree_Model_Filter_Modify_Func is access procedure
        (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
         Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
         Value  : out Glib.Values.GValue;
         Column : Glib.Gint;
         Data   : User_Data_Type);
      --  A function which calculates display values from raw values in the
      --  model. It must fill Value with the display value for the column Column
      --  in the row indicated by Iter.
      --  Since this function is called for each data access, it's not a
      --  particularly efficient operation.
      --  @param Model the `GtkTreeModelFilter`
      --  @param Iter a `GtkTreeIter` pointing to the row whose display values
      --  are determined
      --  @param Value A `GValue` which is already initialized for with the
      --  correct type for the column Column.
      --  @param Column the column whose display value is determined
      --  @param Data user data given to Gtk.Tree_Model_Filter.Set_Modify_Func

      procedure Set_Modify_Func
         (Self  : not null access Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter_Record'Class;
          Types : GType_Array;
          Func  : Gtk_Tree_Model_Filter_Modify_Func;
          Data  : User_Data_Type);
      pragma Obsolescent (Set_Modify_Func);
      --  With the N_Columns and Types parameters, you give an array of column
      --  types for this model (which will be exposed to the parent
      --  model/view). The Func, Data and Destroy parameters are for specifying
      --  the modify function. The modify function will get called for each
      --  data access, the goal of the modify function is to return the data
      --  which should be displayed at the location specified using the
      --  parameters of the modify function.
      --  Note that Gtk.Tree_Model_Filter.Set_Modify_Func can only be called
      --  once for a given filter model.
      --  Deprecated since 4.10, 1
      --  @param Types The `GType`s of the columns.
      --  @param Func A `GtkTreeModelFilterModifyFunc`
      --  @param Data User data to pass to the modify function

   end Set_Modify_Func_User_Data;

   procedure Set_Visible_Column
      (Self   : not null access Gtk_Tree_Model_Filter_Record;
       Column : Glib.Gint);
   pragma Obsolescent (Set_Visible_Column);
   --  Sets Column of the child_model to be the column where Filter should
   --  look for visibility information. Columns should be a column of type
   --  G_TYPE_BOOLEAN, where True means that a row is visible, and False if
   --  not.
   --  Note that Gtk.Tree_Model_Filter.Set_Visible_Func or
   --  Gtk.Tree_Model_Filter.Set_Visible_Column can only be called once for a
   --  given filter model.
   --  Deprecated since 4.10, 1
   --  @param Column A `int` which is the column containing the visible
   --  information

   procedure Set_Visible_Func
      (Self : not null access Gtk_Tree_Model_Filter_Record;
       Func : Gtk_Tree_Model_Filter_Visible_Func);
   pragma Obsolescent (Set_Visible_Func);
   --  Sets the visible function used when filtering the Filter to be Func.
   --  The function should return True if the given row should be visible and
   --  False otherwise.
   --  If the condition calculated by the function changes over time (e.g.
   --  because it depends on some global parameters), you must call
   --  Gtk.Tree_Model_Filter.Refilter to keep the visibility information of the
   --  model up-to-date.
   --  Note that Func is called whenever a row is inserted, when it may still
   --  be empty. The visible function should therefore take special care of
   --  empty rows, like in the example below.
   --  ```c static gboolean visible_func (GtkTreeModel *model, GtkTreeIter
   --  *iter, gpointer data) { // Visible if row is non-empty and first column
   --  is "HI" char *str; gboolean visible = FALSE;
   --  gtk_tree_model_get (model, iter, 0, &str, -1); if (str && strcmp (str,
   --  "HI") == 0) visible = TRUE; g_free (str);
   --  return visible; } ```
   --  Note that Gtk.Tree_Model_Filter.Set_Visible_Func or
   --  Gtk.Tree_Model_Filter.Set_Visible_Column can only be called once for a
   --  given filter model.
   --  Deprecated since 4.10, 1
   --  @param Func A `GtkTreeModelFilterVisibleFunc`, the visible function

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Visible_Func_User_Data is

      type Gtk_Tree_Model_Filter_Visible_Func is access function
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : User_Data_Type) return Boolean;
      --  A function which decides whether the row indicated by Iter is visible.
      --  @param Model the child model of the `GtkTreeModelFilter`
      --  @param Iter a `GtkTreeIter` pointing to the row in Model whose
      --  visibility is determined
      --  @param Data user data given to Gtk.Tree_Model_Filter.Set_Visible_Func
      --  @return Whether the row indicated by Iter is visible.

      procedure Set_Visible_Func
         (Self : not null access Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter_Record'Class;
          Func : Gtk_Tree_Model_Filter_Visible_Func;
          Data : User_Data_Type);
      pragma Obsolescent (Set_Visible_Func);
      --  Sets the visible function used when filtering the Filter to be Func.
      --  The function should return True if the given row should be visible
      --  and False otherwise.
      --  If the condition calculated by the function changes over time (e.g.
      --  because it depends on some global parameters), you must call
      --  Gtk.Tree_Model_Filter.Refilter to keep the visibility information of
      --  the model up-to-date.
      --  Note that Func is called whenever a row is inserted, when it may
      --  still be empty. The visible function should therefore take special
      --  care of empty rows, like in the example below.
      --  ```c static gboolean visible_func (GtkTreeModel *model, GtkTreeIter
      --  *iter, gpointer data) { // Visible if row is non-empty and first
      --  column is "HI" char *str; gboolean visible = FALSE;
      --  gtk_tree_model_get (model, iter, 0, &str, -1); if (str && strcmp
      --  (str, "HI") == 0) visible = TRUE; g_free (str);
      --  return visible; } ```
      --  Note that Gtk.Tree_Model_Filter.Set_Visible_Func or
      --  Gtk.Tree_Model_Filter.Set_Visible_Column can only be called once for
      --  a given filter model.
      --  Deprecated since 4.10, 1
      --  @param Func A `GtkTreeModelFilterVisibleFunc`, the visible function
      --  @param Data User data to pass to the visible function

   end Set_Visible_Func_User_Data;

   procedure Foreach
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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
         (Tree_Model : not null access Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter_Record'Class;
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

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   function Drag_Data_Delete
      (Self : not null access Gtk_Tree_Model_Filter_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   pragma Obsolescent (Drag_Data_Delete);

   function Drag_Data_Get
      (Self : not null access Gtk_Tree_Model_Filter_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gdk.Content_Provider.Gdk_Content_Provider;
   pragma Obsolescent (Drag_Data_Get);

   function Row_Draggable
      (Self : not null access Gtk_Tree_Model_Filter_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   pragma Obsolescent (Row_Draggable);

   function Get_Column_Type
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Index      : Glib.Gint) return GType;
   pragma Obsolescent (Get_Column_Type);

   function Get_Flags
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record)
       return Gtk.Tree_Model.Tree_Model_Flags;
   pragma Obsolescent (Get_Flags);

   function Get_Iter
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Get_Iter);

   function Get_Iter_First
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record)
       return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Get_Iter_First);

   function Get_Iter_From_String
      (Tree_Model  : not null access Gtk_Tree_Model_Filter_Record;
       Path_String : UTF8_String) return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Get_Iter_From_String);

   function Get_N_Columns
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record)
       return Glib.Gint;
   pragma Obsolescent (Get_N_Columns);

   function Get_Path
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Path;
   pragma Obsolescent (Get_Path);

   function Get_String_From_Iter
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return UTF8_String;
   pragma Obsolescent (Get_String_From_Iter);

   procedure Get_Value
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Column     : Glib.Gint;
       Value      : out Glib.Values.GValue);
   pragma Obsolescent (Get_Value);

   function Children
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Children);

   function Has_Child
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   pragma Obsolescent (Has_Child);

   function N_Children
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
       return Glib.Gint;
   pragma Obsolescent (N_Children);

   procedure Next
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Next);

   function Nth_Child
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
       N          : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Nth_Child);

   function Parent
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Child      : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Obsolescent (Parent);

   procedure Previous
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Previous);

   procedure Ref_Node
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Ref_Node);

   procedure Row_Changed
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Row_Changed);

   procedure Row_Deleted
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path);
   pragma Obsolescent (Row_Deleted);

   procedure Row_Has_Child_Toggled
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Row_Has_Child_Toggled);

   procedure Row_Inserted
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Row_Inserted);

   procedure Rows_Reordered
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       New_Order  : Gint_Array);
   pragma Obsolescent (Rows_Reordered);

   procedure Rows_Reordered_With_Length
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       New_Order  : Gint_Array;
       Length     : Glib.Gint);
   pragma Obsolescent (Rows_Reordered_With_Length);

   procedure Unref_Node
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Unref_Node);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Child_Model_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Tree_Model.Gtk_Tree_Model
   --  The child model of the tree model filter.

   Virtual_Root_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Tree_Model.Gtk_Tree_Path
   --  The virtual root of the tree model filter.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.TreeDragSource"
   --
   --  - "Gtk.TreeModel"

   package Implements_Gtk_Tree_Drag_Source is new Glib.Types.Implements
     (Gtk.Tree_Drag_Source.Gtk_Tree_Drag_Source, Gtk_Tree_Model_Filter_Record, Gtk_Tree_Model_Filter);
   function "+"
     (Widget : access Gtk_Tree_Model_Filter_Record'Class)
   return Gtk.Tree_Drag_Source.Gtk_Tree_Drag_Source
   renames Implements_Gtk_Tree_Drag_Source.To_Interface;
   function "-"
     (Interf : Gtk.Tree_Drag_Source.Gtk_Tree_Drag_Source)
   return Gtk_Tree_Model_Filter
   renames Implements_Gtk_Tree_Drag_Source.To_Object;

   package Implements_Gtk_Tree_Model is new Glib.Types.Implements
     (Gtk.Tree_Model.Gtk_Tree_Model, Gtk_Tree_Model_Filter_Record, Gtk_Tree_Model_Filter);
   function "+"
     (Widget : access Gtk_Tree_Model_Filter_Record'Class)
   return Gtk.Tree_Model.Gtk_Tree_Model
   renames Implements_Gtk_Tree_Model.To_Interface;
   function "-"
     (Interf : Gtk.Tree_Model.Gtk_Tree_Model)
   return Gtk_Tree_Model_Filter
   renames Implements_Gtk_Tree_Model.To_Object;

private
   Virtual_Root_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("virtual-root");
   Child_Model_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("child-model");
end Gtk.Tree_Model_Filter;
