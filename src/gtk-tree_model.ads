-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gtk;
with Glib.Object;
with Glib.Values;

package Gtk.Tree_Model is

   type Gtk_Tree_Model_Record is new Glib.Object.GObject_Record with private;
   type Gtk_Tree_Model is access all Gtk_Tree_Model_Record'Class;

   type Gtk_Tree_Iter is private;

   Null_Iter : constant Gtk_Tree_Iter;

   type Gtk_Tree_Path_Record is limited private;
   type Gtk_Tree_Path is access Gtk_Tree_Path_Record;

   --   ??? The functionalities of Gtk_Tree_Row_References have not yet
   --   been tested.

--    type Gtk_Tree_Row_Reference_Record is
--      new Glib.Object.GObject_Record with private;

--    type Gtk_Tree_Row_Reference is
--      access all Gtk_Tree_Row_Reference_Record'Class;

   type Tree_Model_Flags is mod 2 ** 32;

   Tree_Model_Iters_Persist : constant Tree_Model_Flags;
   Tree_Model_List_Only     : constant Tree_Model_Flags;

   procedure Gtk_New (Widget : out Gtk_Tree_Path);
   --  Creates a new Gtk_Tree_Path.

   procedure Initialize (Widget : in out Gtk_Tree_Path);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Gtk_New (Widget : out Gtk_Tree_Path;
                      Path : String);
   --  Creates a new Gtk_Tree_Path from a path string.

   procedure Initialize (Widget : in out Gtk_Tree_Path;
                         Path : String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Tree_Path_To_String (Path   : Gtk_Tree_Path)
                                 return String;
   --  Generates a string representation of the path.
   --  This string is a ':' separated list of numbers.
   --  For example, "4:10:0:3" would be an acceptable return value.

--    procedure Gtk_New (Widget: out Gtk_Tree_Model);

--    procedure Initialize (Widget: access Gtk_Tree_Model_Record'Class);
--    --  Internal initialization function.
--    --  See the section "Creating your own widgets" in the documentation.

   procedure Tree_Path_Append_Index
     (Path  : Gtk_Tree_Path;
      Index : Gint);
   --  Appends a new index to a path.  As a result, the depth of the path is
   --  increased.

   procedure Tree_Path_Prepend_Index
     (Path  : Gtk_Tree_Path;
      Index : Gint);
   --  Prepends a new index to a path.  As a result, the depth of the path is
   --  increased.

   function Tree_Path_Get_Depth (Path   : Gtk_Tree_Path)
                                 return Gint;
   --  Returns the current depth of Path.

   --  ??? The following function returns an array of Gint. Must be properly
   --  mapped.

   --    function Tree_Path_Get_Indices (Path   : Gtk_Tree_Path)
   --                                return Gint;

   procedure Tree_Path_Free (Path : Gtk_Tree_Path);
   --  Free Path.

   function Tree_Path_Copy (Path   : Gtk_Tree_Path)
                            return Gtk_Tree_Path;
   --  Create a new Gtk_Tree_Path as a copy of Path.

   function Tree_Path_Compare
     (A      : Gtk_Tree_Path;
      B      : Gtk_Tree_Path)
      return Gint;
   --  Compares two paths.  If A appears before B in a tree, then -1 is
   --  returned.
   --  If B appears before A, then 1 is returned.  If the two nodes are equal,
   --  then 0 is returned.

   procedure Tree_Path_Next (Path : Gtk_Tree_Path);
   --  Moves the Path to point to the next node at the current depth.

   function Tree_Path_Prev (Path   : Gtk_Tree_Path)
                            return Boolean;
   --  Moves Path to point to the previous node at the current depth,
   --  if it exists.
   --  Return True if Path has a previous node, and the move was made.

   function Tree_Path_Up (Path   : Gtk_Tree_Path)
                          return Boolean;
   --  Moves the Path to point to it's parent node, if it has a parent.
   --  Return True if Path has a parent, and the move was made.

   procedure Tree_Path_Down (Path : Gtk_Tree_Path);
   --  Moves Path to point to the first child of the current path.

   function Tree_Path_Is_Ancestor
     (Path       : Gtk_Tree_Path;
      Descendant : Gtk_Tree_Path)
     return Boolean;
   --  Return True if Descendant is contained inside Path.

   function Tree_Path_Is_Descendant
     (Path     : Gtk_Tree_Path;
      Ancestor : Gtk_Tree_Path)
     return Boolean;
   --  Return True if Path is contained inside Ancestor.

--    procedure Gtk_New
--      (Widget : out Gtk_Tree_Row_Reference;
--       Model : access Gtk_Tree_Model_Record'Class;
--       Path  : Gtk_Tree_Path);

--    procedure Initialize
--      (Widget : access Gtk_Tree_Row_Reference_Record'Class;
--       Model : access Gtk_Tree_Model_Record'Class;
--       Path  : Gtk_Tree_Path);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

--    procedure Gtk_New
--      (Widget : out Gtk_Tree_Row_Reference;
--       Proxy : out Glib.Object.GObject;
--       Model : access Gtk_Tree_Model_Record'Class;
--       Path  : Gtk_Tree_Path);

--    procedure Initialize
--      (Widget : access Gtk_Tree_Row_Reference_Record'Class;
--       Proxy : out Glib.Object.GObject;
--       Model : access Gtk_Tree_Model_Record'Class;
--       Path  : Gtk_Tree_Path);

   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

--    function Tree_Row_Reference_Get_Path
--      (Reference : access Gtk_Tree_Row_Reference_Record'Class)
--      return Gtk_Tree_Path;

--    function Tree_Row_Reference_Valid
--      (Reference : access Gtk_Tree_Row_Reference_Record'Class)
--      return Boolean;

--    procedure Tree_Row_Reference_Free
--      (Reference : access Gtk_Tree_Row_Reference_Record'Class);

--    procedure Tree_Row_Reference_Inserted
--      (Proxy : out Glib.Object.GObject;
--       Path  : Gtk_Tree_Path);

--    procedure Tree_Row_Reference_Deleted
--      (Proxy : out Glib.Object.GObject;
--       Path  : Gtk_Tree_Path);

--    procedure Tree_Row_Reference_Reordered
--      (Proxy     : out Glib.Object.GObject;
--       Path      : Gtk_Tree_Path;
--       Iter      : Gtk_Tree_Iter;
--       New_Order : out Gint);

   function Tree_Iter_Copy (Iter   : Gtk_Tree_Iter)
                            return Gtk_Tree_Iter;
   --  Creates a dynamically allocated tree iterator as a copy of Iter.
   --  This iter must be freed using Tree_Iter_Free.

   procedure Tree_Iter_Free (Iter : Gtk_Tree_Iter);
   --  Free memory allocated to Iter.

   function Tree_Model_Get_N_Columns
     (Tree_Model : access Gtk_Tree_Model_Record'Class)
     return Gint;
   --  Return the number of columns supported by Tree_Model.

   function Tree_Model_Get_Column_Type
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Index      : Gint)
     return GType;
   --  Return the type of the column.

   procedure Tree_Model_Get_Iter
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Iter       : out Gtk_Tree_Iter;
      Path       : Gtk_Tree_Path;
      Success    : out Boolean);
   --  Sets Iter to a valid iterator pointing to Path.

   procedure Tree_Model_Get_Iter_From_String
     (Tree_Model  : access Gtk_Tree_Model_Record'Class;
      Iter        : out Gtk_Tree_Iter;
      Path_String : String;
      Success     : out Boolean);
   --  Sets Iter to a valid iterator pointing to Path_String, if it
   --  exists. Otherwise, Iter is left invalid and success set to False.

   procedure Tree_Model_Get_Iter_Root
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Iter       : out Gtk_Tree_Iter;
      Success    : out Boolean);
   --  Initialize Iter with the root iterator in the tree (the one at the root
   --  path). Success indicates whether Iter was set.

   function Tree_Model_Get_Path
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Iter       : Gtk_Tree_Iter)
     return Gtk_Tree_Path;
   --  Returns a newly created Gtk_Tree_Path referenced by Iter.
   --  This path should be freed with Tree_Path_Free.

   procedure Tree_Model_Get_Value
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint;
      Value      : out Glib.Values.GValue);
   --  Sets initializes and sets Value to that at Column.
   --  Value must be freed by the user.

   procedure Tree_Model_Iter_Next
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Iter       : in out Gtk_Tree_Iter;
      Success    : out Boolean);
   --  Sets Iter to point to the node following it at the current level.
   --  If there is none, Iter is set to be invalid and Success is set
   --  to False.

   procedure Tree_Model_Iter_Children
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Iter       : out Gtk_Tree_Iter;
      Parent     : Gtk_Tree_Iter;
      Success    : out Boolean);
   --  Sets Iter to point to the first child of Parent.
   --  If Parent has no children, Success is set to False and @iter is set
   --  to be invalid.  Parent will remain a valid node after this function
   --  has been called.

   function Tree_Model_Iter_Has_Child
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Iter       : Gtk_Tree_Iter)
     return Boolean;
   --  Return True if Iter has children, False otherwise.

   function Tree_Model_Iter_N_Children
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Iter       : Gtk_Tree_Iter)
     return Gint;
   --  Returns the number of children that Iter has.  As a special case, if
   --  Iter is Null_Iter, then the number of toplevel nodes is returned.

   function Tree_Model_Iter_Nth_Child
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Iter       : Gtk_Tree_Iter;
      Parent     : Gtk_Tree_Iter;
      N          : Gint)
      return Boolean;
   --  Sets Iter to be the child of Parent, using the given index.
   --  The First index is 0. If Index is too big, or Parent has no children,
   --  Iter is Set to an invalid iterator and False is returned.
   --  Parent will remain a Valid  node after this function has been called.
   --  If Parent is Null_Iter, then the nth root node is set.

   function Tree_Model_Iter_Parent
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Iter       : Gtk_Tree_Iter;
      Child      : Gtk_Tree_Iter)
      return Boolean;
   --  Sets Iter to be the parent of Child.  If Child is at the toplevel,
   --  and doesn't have a parent, then Iter is set to an invalid iterator
   --  and False is returned.  Child will remain a valid node after this
   --  function has been called.

   procedure Tree_Model_Ref_Node
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Iter       : Gtk_Tree_Iter);
   --  Lets the tree reference the node.  This is an optional method for
   --  models to implement.  To be more specific, models may ignore this
   --  call as it exists  primarily for performance reasons.   This function
   --  is primarily meant as a way for views to let caching model know  when
   --  nodes are being displayed (and hence, whether or not to cache that node)
   --  For example, a file-system based model would not want to keep the
   --  entire file-heirarchy in memory, just the sections that are currently
   --  being  displayed by every current view.

   procedure Tree_Model_Unref_Node
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Iter       : Gtk_Tree_Iter);
   --  Lets the tree unref the node.  This is an optional method for models
   --  to implement.  To be more specific, models may ignore this call as it
   --  exists  primarily for performance reasons.  For more information on what
   --  this means, please see Tree_Model_Ref_Node. Please note that nodes that
   --  are deleted are not unreferenced.

   --  ??? gtk_tree_model_get not bound: variable number of arguments

   generic
      type Data_Type is private;
   package Model_Data is

      type Data_Access is access Data_Type;

      function Get
        (Tree_Model : access Gtk_Tree_Model_Record'Class;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint)
        return Data_Type;
      --  Gets the value of one cell in the row referenced by Iter.

   end Model_Data;

   function Get_String
        (Tree_Model : access Gtk_Tree_Model_Record'Class;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint)
        return String;
   --  procedure Tree_Model_Get_Valist
   --  (Tree_Model : access Gtk_Tree_Model_Record'Class;
   --   Iter       : Gtk_Tree_Iter;
   --   Var_Args   : va_list);

   --    procedure Tree_Model_Foreach
   --      (Model     : access Gtk_Tree_Model_Record'Class;
   --       Func      : Gtk_Tree_Model_Foreach_Func;
   --       User_Data : gpointer);

   procedure Tree_Model_Row_Changed
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter);
   --  Emits the "row_changed" signal on Tree_Model.

   procedure Tree_Model_Row_Inserted
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter);
   --  Emits the "row_inserted" signal on Tree_Model.

   procedure Tree_Model_Row_Has_Child_Toggled
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter);
   --  Emits the "row_has_child_toggled" signal on Tree_Model.
   --  This should be called by models after the child state of a node changes.

   procedure Tree_Model_Row_Deleted
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path);
   --  Emits the "row_has_child_toggled" signal on Tree_Model.
   --  This should be called by models after the child state of a node changes.

   procedure Tree_Model_Rows_Reordered
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter;
      New_Order  : out Gint);

private

   Tree_Model_Iters_Persist : constant Tree_Model_Flags := 2 ** 0;
   Tree_Model_List_Only     : constant Tree_Model_Flags := 2 ** 1;

   type Gtk_Tree_Model_Record is new Glib.Object.GObject_Record
     with null record;

   type Gtk_Tree_Iter is record
      Stamp      : Gint;
      User_Data  : System.Address;
      User_Data2 : System.Address;
      User_Data3 : System.Address;
   end record;
   pragma Convention (C, Gtk_Tree_Iter);

   Null_Iter : constant Gtk_Tree_Iter :=
     (0, System.Null_Address, System.Null_Address, System.Null_Address);

   for Null_Iter'Address use System.Null_Address;

   type Gtk_Tree_Path_Record is null record;

   type Gtk_Tree_Row_Reference_Record is new Glib.Object.GObject_Record
     with null record;

   pragma Import (C, Get_Type, "gtk_tree_model_get_type");

end Gtk.Tree_Model;
