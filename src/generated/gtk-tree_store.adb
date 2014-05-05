------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2014, AdaCore                     --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with Glib.Object;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Interfaces.C.Strings;       use Interfaces.C.Strings;
pragma Warnings(On);

package body Gtk.Tree_Store is

   function Freeze_Sort
     (Tree : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class)
   return Gint
   is
      function Internal (Tree : System.Address) return Gint;
      pragma Import (C, Internal, "ada_gtk_tree_view_freeze_sort");
   begin
      return Internal (Get_Object (Tree));
   end Freeze_Sort;

   procedure Thaw_Sort
     (Tree : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
      Column_Id : Gint)
   is
      procedure Internal (Tree : System.Address; Id : Gint);
      pragma Import (C, Internal, "ada_gtk_tree_view_thaw_sort");
   begin
      Internal (Get_Object (Tree), Column_Id);
   end Thaw_Sort;

   package body Generic_Set is

      pragma Warnings (Off);
      --  Disable warnings on strict aliasing, since there's no aliasing
      --  issue here.

      function To_Address is new
      Ada.Unchecked_Conversion (Data_Type_Access, System.Address);

      function To_Access is new
      Ada.Unchecked_Conversion (System.Address, Data_Type_Access);

      pragma Warnings (On);

      procedure Set
        (Tree_Store : access Gtk_Tree_Store_Record'Class;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Data_Type_Access) is
      begin
         Set (Tree_Store, Iter, Column, To_Address (Value));
      end Set;

      function Get
        (Tree_Store : access Gtk_Tree_Store_Record'Class;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint) return Data_Type_Access is
      begin
         return To_Access (Get_Address (+Tree_Store, Iter, Column));
      end Get;
   end Generic_Set;

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Address    : System.Address)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : System.Address);
      pragma Import (C, Internal, "ada_gtk_tree_store_set_ptr");
   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Address);
   end Set;

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : UTF8_String)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : UTF8_String);
      pragma Import (C, Internal, "ada_gtk_tree_store_set_ptr");
   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Value & ASCII.NUL);
   end Set;

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Gint)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Gint);
      pragma Import (C, Internal, "ada_gtk_tree_store_set_int");
   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Value);
   end Set;

   procedure Set_Ulong
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Gulong)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Gulong);
      pragma Import (C, Internal, "ada_gtk_tree_store_set_ulong");
   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Value);
   end Set_Ulong;

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Boolean)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Gboolean);
      pragma Import (C, Internal, "ada_gtk_tree_store_set_int");
   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Boolean'Pos (Value));
   end Set;

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Glib.C_Proxy)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Glib.C_Proxy);
      pragma Import (C, Internal, "ada_gtk_tree_store_set_ptr");
   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Value);
   end Set;

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Glib.Object.GObject)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : System.Address);
      pragma Import (C, Internal, "ada_gtk_tree_store_set_ptr");
   begin
      if Value = null then
         Internal (Get_Object (Tree_Store), Iter, Column, System.Null_Address);
      else
         Internal (Get_Object (Tree_Store), Iter, Column, Get_Object (Value));
      end if;
   end Set;

   procedure C_Gtk_Tree_Model_Foreach
      (Tree_Model : System.Address;
       Func       : System.Address;
       User_Data  : System.Address);
   pragma Import (C, C_Gtk_Tree_Model_Foreach, "gtk_tree_model_foreach");
   --  Calls func on each node in model in a depth-first fashion.
   --  If Func returns True, then the tree ceases to be walked, and
   --  Gtk.Tree_Model.Foreach returns.
   --  "func": a function to be called on each row
   --  "user_data": user data to passed to Func

   procedure C_Gtk_Tree_Sortable_Set_Default_Sort_Func
      (Sortable  : System.Address;
       Sort_Func : System.Address;
       User_Data : System.Address;
       Destroy   : System.Address);
   pragma Import (C, C_Gtk_Tree_Sortable_Set_Default_Sort_Func, "gtk_tree_sortable_set_default_sort_func");
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
   --  "user_data": User data to pass to Sort_Func, or null
   --  "destroy": Destroy notifier of User_Data, or null

   procedure C_Gtk_Tree_Sortable_Set_Sort_Func
      (Sortable       : System.Address;
       Sort_Column_Id : Gint;
       Sort_Func      : System.Address;
       User_Data      : System.Address;
       Destroy        : System.Address);
   pragma Import (C, C_Gtk_Tree_Sortable_Set_Sort_Func, "gtk_tree_sortable_set_sort_func");
   --  Sets the comparison function used when sorting to be Sort_Func. If the
   --  current sort column id of Sortable is the same as Sort_Column_Id, then
   --  the model will sort using this function.
   --  "sort_column_id": the sort column id to set the function for
   --  "sort_func": The comparison function
   --  "user_data": User data to pass to Sort_Func, or null
   --  "destroy": Destroy notifier of User_Data, or null

   function To_Gtk_Tree_Model_Foreach_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_Model_Foreach_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Tree_Model_Foreach_Func, System.Address);

   function To_Gtk_Tree_Iter_Compare_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_Iter_Compare_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Tree_Iter_Compare_Func, System.Address);

   function Internal_Gtk_Tree_Iter_Compare_Func
      (Model     : Gtk.Tree_Model.Gtk_Tree_Model;
       A         : access Gtk.Tree_Model.Gtk_Tree_Iter;
       B         : access Gtk.Tree_Model.Gtk_Tree_Iter;
       User_Data : System.Address) return Gint;
   pragma Convention (C, Internal_Gtk_Tree_Iter_Compare_Func);
   --  "model": The Gtk.Tree_Model.Gtk_Tree_Model the comparison is within
   --  "a": A Gtk.Tree_Model.Gtk_Tree_Iter in Model
   --  "b": Another Gtk.Tree_Model.Gtk_Tree_Iter in Model
   --  "user_data": Data passed when the compare func is assigned e.g. by
   --  Gtk.Tree_Sortable.Set_Sort_Func

   function Internal_Gtk_Tree_Model_Foreach_Func
      (Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Path  : System.Address;
       Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Data  : System.Address) return Integer;
   pragma Convention (C, Internal_Gtk_Tree_Model_Foreach_Func);
   --  "model": the Gtk.Tree_Model.Gtk_Tree_Model being iterated
   --  "path": the current Gtk.Tree_Model.Gtk_Tree_Path
   --  "iter": the current Gtk.Tree_Model.Gtk_Tree_Iter
   --  "data": The user data passed to Gtk.Tree_Model.Foreach

   -----------------------------------------
   -- Internal_Gtk_Tree_Iter_Compare_Func --
   -----------------------------------------

   function Internal_Gtk_Tree_Iter_Compare_Func
      (Model     : Gtk.Tree_Model.Gtk_Tree_Model;
       A         : access Gtk.Tree_Model.Gtk_Tree_Iter;
       B         : access Gtk.Tree_Model.Gtk_Tree_Iter;
       User_Data : System.Address) return Gint
   is
      Func : constant Gtk_Tree_Iter_Compare_Func := To_Gtk_Tree_Iter_Compare_Func (User_Data);
   begin
      return Func (Model, A.all, B.all);
   end Internal_Gtk_Tree_Iter_Compare_Func;

   ------------------------------------------
   -- Internal_Gtk_Tree_Model_Foreach_Func --
   ------------------------------------------

   function Internal_Gtk_Tree_Model_Foreach_Func
      (Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Path  : System.Address;
       Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Data  : System.Address) return Integer
   is
      Func : constant Gtk_Tree_Model_Foreach_Func := To_Gtk_Tree_Model_Foreach_Func (Data);
   begin
      return Boolean'Pos (Func (Model, From_Object (Path), Iter.all));
   end Internal_Gtk_Tree_Model_Foreach_Func;

   package Type_Conversion_Gtk_Tree_Store is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tree_Store_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Tree_Store);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Tree_Store : out Gtk_Tree_Store; Types : GType_Array) is
   begin
      Tree_Store := new Gtk_Tree_Store_Record;
      Gtk.Tree_Store.Initialize (Tree_Store, Types);
   end Gtk_New;

   -------------------------
   -- Gtk_Tree_Store_Newv --
   -------------------------

   function Gtk_Tree_Store_Newv (Types : GType_Array) return Gtk_Tree_Store is
      Tree_Store : constant Gtk_Tree_Store := new Gtk_Tree_Store_Record;
   begin
      Gtk.Tree_Store.Initialize (Tree_Store, Types);
      return Tree_Store;
   end Gtk_Tree_Store_Newv;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Tree_Store : not null access Gtk_Tree_Store_Record'Class;
       Types      : GType_Array)
   is
      function Internal
         (N_Columns : Gint;
          Types     : GType_Array) return System.Address;
      pragma Import (C, Internal, "gtk_tree_store_newv");
   begin
      if not Tree_Store.Is_Created then
         Set_Object (Tree_Store, Internal (Types'Length, Types));
      end if;
   end Initialize;

   ------------
   -- Append --
   ------------

   procedure Append
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
         (Tree_Store : System.Address;
          Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
          Parent     : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_append");
      Tmp_Iter : aliased Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Internal (Get_Object (Tree_Store), Tmp_Iter, Iter_Or_Null (Parent'Address));
      Iter := Tmp_Iter;
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (Tree_Store : not null access Gtk_Tree_Store_Record) is
      procedure Internal (Tree_Store : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_clear");
   begin
      Internal (Get_Object (Tree_Store));
   end Clear;

   -------------
   -- Foreach --
   -------------

   procedure Foreach
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Func       : Gtk_Tree_Model_Foreach_Func)
   is
   begin
      if Func = null then
         C_Gtk_Tree_Model_Foreach (Get_Object (Tree_Model), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Tree_Model_Foreach (Get_Object (Tree_Model), Internal_Gtk_Tree_Model_Foreach_Func'Address, To_Address (Func));
      end if;
   end Foreach;

   package body Foreach_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Tree_Model_Foreach_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Tree_Model_Foreach_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Tree_Model_Foreach_Func, System.Address);

      function Internal_Cb
         (Model : Gtk.Tree_Model.Gtk_Tree_Model;
          Path  : System.Address;
          Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Data  : System.Address) return Integer;
      pragma Convention (C, Internal_Cb);
      --  Type of the callback passed to Gtk.Tree_Model.Foreach to iterate
      --  over the rows in a tree model.
      --  "model": the Gtk.Tree_Model.Gtk_Tree_Model being iterated
      --  "path": the current Gtk.Tree_Model.Gtk_Tree_Path
      --  "iter": the current Gtk.Tree_Model.Gtk_Tree_Iter
      --  "data": The user data passed to Gtk.Tree_Model.Foreach

      -------------
      -- Foreach --
      -------------

      procedure Foreach
         (Tree_Model : not null access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
          Func       : Gtk_Tree_Model_Foreach_Func;
          User_Data  : User_Data_Type)
      is
      begin
         if Func = null then
            C_Gtk_Tree_Model_Foreach (Get_Object (Tree_Model), System.Null_Address, System.Null_Address);
         else
            C_Gtk_Tree_Model_Foreach (Get_Object (Tree_Model), Internal_Cb'Address, Users.Build (To_Address (Func), User_Data));
         end if;
      end Foreach;

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Model : Gtk.Tree_Model.Gtk_Tree_Model;
          Path  : System.Address;
          Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Data  : System.Address) return Integer
      is
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         return Boolean'Pos (To_Gtk_Tree_Model_Foreach_Func (D.Func) (Model, From_Object (Path), Iter.all, D.Data.all));
      end Internal_Cb;

   end Foreach_User_Data;

   ------------
   -- Insert --
   ------------

   procedure Insert
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
       Position   : Gint)
   is
      procedure Internal
         (Tree_Store : System.Address;
          Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
          Parent     : System.Address;
          Position   : Gint);
      pragma Import (C, Internal, "gtk_tree_store_insert");
      Tmp_Iter : aliased Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Internal (Get_Object (Tree_Store), Tmp_Iter, Iter_Or_Null (Parent'Address), Position);
      Iter := Tmp_Iter;
   end Insert;

   ------------------
   -- Insert_After --
   ------------------

   procedure Insert_After
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
       Sibling    : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
         (Tree_Store : System.Address;
          Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
          Parent     : System.Address;
          Sibling    : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_insert_after");
      Tmp_Iter : aliased Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Internal (Get_Object (Tree_Store), Tmp_Iter, Iter_Or_Null (Parent'Address), Iter_Or_Null (Sibling'Address));
      Iter := Tmp_Iter;
   end Insert_After;

   -------------------
   -- Insert_Before --
   -------------------

   procedure Insert_Before
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
       Sibling    : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
         (Tree_Store : System.Address;
          Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
          Parent     : System.Address;
          Sibling    : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_insert_before");
      Tmp_Iter : aliased Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Internal (Get_Object (Tree_Store), Tmp_Iter, Iter_Or_Null (Parent'Address), Iter_Or_Null (Sibling'Address));
      Iter := Tmp_Iter;
   end Insert_Before;

   -----------------
   -- Is_Ancestor --
   -----------------

   function Is_Ancestor
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Descendant : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      function Internal
         (Tree_Store : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
          Descendant : Gtk.Tree_Model.Gtk_Tree_Iter) return Integer;
      pragma Import (C, Internal, "gtk_tree_store_is_ancestor");
   begin
      return Internal (Get_Object (Tree_Store), Iter, Descendant) /= 0;
   end Is_Ancestor;

   ----------------
   -- Iter_Depth --
   ----------------

   function Iter_Depth
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint
   is
      function Internal
         (Tree_Store : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint;
      pragma Import (C, Internal, "gtk_tree_store_iter_depth");
   begin
      return Internal (Get_Object (Tree_Store), Iter);
   end Iter_Depth;

   -------------------
   -- Iter_Is_Valid --
   -------------------

   function Iter_Is_Valid
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      function Internal
         (Tree_Store : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Integer;
      pragma Import (C, Internal, "gtk_tree_store_iter_is_valid");
   begin
      return Internal (Get_Object (Tree_Store), Iter) /= 0;
   end Iter_Is_Valid;

   ----------------
   -- Move_After --
   ----------------

   procedure Move_After
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Position   : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
         (Tree_Store : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
          Position   : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_move_after");
   begin
      Internal (Get_Object (Tree_Store), Iter, Iter_Or_Null (Position'Address));
   end Move_After;

   -----------------
   -- Move_Before --
   -----------------

   procedure Move_Before
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Position   : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
         (Tree_Store : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
          Position   : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_move_before");
   begin
      Internal (Get_Object (Tree_Store), Iter, Iter_Or_Null (Position'Address));
   end Move_Before;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
         (Tree_Store : System.Address;
          Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
          Parent     : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_prepend");
      Tmp_Iter : aliased Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Internal (Get_Object (Tree_Store), Tmp_Iter, Iter_Or_Null (Parent'Address));
      Iter := Tmp_Iter;
   end Prepend;

   ------------
   -- Remove --
   ------------

   procedure Remove
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
            procedure Internal
         (Tree_Store : System.Address;
          Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_store_remove");
      Tmp_Iter : aliased Gtk.Tree_Model.Gtk_Tree_Iter := Iter;
   begin
      Internal (Get_Object (Tree_Store), Tmp_Iter);
      Iter := Tmp_Iter;

Iter := Gtk.Tree_Model.Null_Iter;
   end Remove;

   -------------
   -- Reorder --
   -------------

   procedure Reorder
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
       New_Order  : Gint_Array)
   is
      procedure Internal
         (Tree_Store : System.Address;
          Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
          New_Order  : Gint_Array);
      pragma Import (C, Internal, "gtk_tree_store_reorder");
   begin
      Internal (Get_Object (Tree_Store), Parent, New_Order);
   end Reorder;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Types      : GType_Array)
   is
      procedure Internal
         (Tree_Store : System.Address;
          N_Columns  : Gint;
          Types      : GType_Array);
      pragma Import (C, Internal, "gtk_tree_store_set_column_types");
   begin
      Internal (Get_Object (Tree_Store), Types'Length, Types);
   end Set_Column_Types;

   ---------------------------
   -- Set_Default_Sort_Func --
   ---------------------------

   procedure Set_Default_Sort_Func
      (Sortable  : not null access Gtk_Tree_Store_Record;
       Sort_Func : Gtk_Tree_Iter_Compare_Func)
   is
   begin
      if Sort_Func = null then
         C_Gtk_Tree_Sortable_Set_Default_Sort_Func (Get_Object (Sortable), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Tree_Sortable_Set_Default_Sort_Func (Get_Object (Sortable), Internal_Gtk_Tree_Iter_Compare_Func'Address, To_Address (Sort_Func), System.Null_Address);
      end if;
   end Set_Default_Sort_Func;

   package body Set_Default_Sort_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Tree_Iter_Compare_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Tree_Iter_Compare_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Tree_Iter_Compare_Func, System.Address);

      function Internal_Cb
         (Model     : Gtk.Tree_Model.Gtk_Tree_Model;
          A         : access Gtk.Tree_Model.Gtk_Tree_Iter;
          B         : access Gtk.Tree_Model.Gtk_Tree_Iter;
          User_Data : System.Address) return Gint;
      pragma Convention (C, Internal_Cb);
      --  A GtkTreeIterCompareFunc should return a negative integer, zero, or
      --  a positive integer if A sorts before B, A sorts with B, or A sorts
      --  after B respectively. If two iters compare as equal, their order in
      --  the sorted model is undefined. In order to ensure that the
      --  Gtk.Tree_Sortable.Gtk_Tree_Sortable behaves as expected, the
      --  GtkTreeIterCompareFunc must define a partial order on the model, i.e.
      --  it must be reflexive, antisymmetric and transitive.
      --  For example, if Model is a product catalogue, then a compare
      --  function for the "price" column could be one which returns
      --  'price_of(A) - price_of(B)'.
      --  "model": The Gtk.Tree_Model.Gtk_Tree_Model the comparison is within
      --  "a": A Gtk.Tree_Model.Gtk_Tree_Iter in Model
      --  "b": Another Gtk.Tree_Model.Gtk_Tree_Iter in Model
      --  "user_data": Data passed when the compare func is assigned e.g. by
      --  Gtk.Tree_Sortable.Set_Sort_Func

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Model     : Gtk.Tree_Model.Gtk_Tree_Model;
          A         : access Gtk.Tree_Model.Gtk_Tree_Iter;
          B         : access Gtk.Tree_Model.Gtk_Tree_Iter;
          User_Data : System.Address) return Gint
      is
         D : constant Users.Internal_Data_Access := Users.Convert (User_Data);
      begin
         return To_Gtk_Tree_Iter_Compare_Func (D.Func) (Model, A.all, B.all, D.Data.all);
      end Internal_Cb;

      ---------------------------
      -- Set_Default_Sort_Func --
      ---------------------------

      procedure Set_Default_Sort_Func
         (Sortable  : not null access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
          Sort_Func : Gtk_Tree_Iter_Compare_Func;
          User_Data : User_Data_Type)
      is
      begin
         if Sort_Func = null then
            C_Gtk_Tree_Sortable_Set_Default_Sort_Func (Get_Object (Sortable), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            C_Gtk_Tree_Sortable_Set_Default_Sort_Func (Get_Object (Sortable), Internal_Cb'Address, Users.Build (To_Address (Sort_Func), User_Data), Users.Free_Data'Address);
         end if;
      end Set_Default_Sort_Func;

   end Set_Default_Sort_Func_User_Data;

   -------------------
   -- Set_Sort_Func --
   -------------------

   procedure Set_Sort_Func
      (Sortable       : not null access Gtk_Tree_Store_Record;
       Sort_Column_Id : Gint;
       Sort_Func      : Gtk_Tree_Iter_Compare_Func)
   is
   begin
      if Sort_Func = null then
         C_Gtk_Tree_Sortable_Set_Sort_Func (Get_Object (Sortable), Sort_Column_Id, System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Tree_Sortable_Set_Sort_Func (Get_Object (Sortable), Sort_Column_Id, Internal_Gtk_Tree_Iter_Compare_Func'Address, To_Address (Sort_Func), System.Null_Address);
      end if;
   end Set_Sort_Func;

   package body Set_Sort_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Tree_Iter_Compare_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Tree_Iter_Compare_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Tree_Iter_Compare_Func, System.Address);

      function Internal_Cb
         (Model     : Gtk.Tree_Model.Gtk_Tree_Model;
          A         : access Gtk.Tree_Model.Gtk_Tree_Iter;
          B         : access Gtk.Tree_Model.Gtk_Tree_Iter;
          User_Data : System.Address) return Gint;
      pragma Convention (C, Internal_Cb);
      --  A GtkTreeIterCompareFunc should return a negative integer, zero, or
      --  a positive integer if A sorts before B, A sorts with B, or A sorts
      --  after B respectively. If two iters compare as equal, their order in
      --  the sorted model is undefined. In order to ensure that the
      --  Gtk.Tree_Sortable.Gtk_Tree_Sortable behaves as expected, the
      --  GtkTreeIterCompareFunc must define a partial order on the model, i.e.
      --  it must be reflexive, antisymmetric and transitive.
      --  For example, if Model is a product catalogue, then a compare
      --  function for the "price" column could be one which returns
      --  'price_of(A) - price_of(B)'.
      --  "model": The Gtk.Tree_Model.Gtk_Tree_Model the comparison is within
      --  "a": A Gtk.Tree_Model.Gtk_Tree_Iter in Model
      --  "b": Another Gtk.Tree_Model.Gtk_Tree_Iter in Model
      --  "user_data": Data passed when the compare func is assigned e.g. by
      --  Gtk.Tree_Sortable.Set_Sort_Func

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Model     : Gtk.Tree_Model.Gtk_Tree_Model;
          A         : access Gtk.Tree_Model.Gtk_Tree_Iter;
          B         : access Gtk.Tree_Model.Gtk_Tree_Iter;
          User_Data : System.Address) return Gint
      is
         D : constant Users.Internal_Data_Access := Users.Convert (User_Data);
      begin
         return To_Gtk_Tree_Iter_Compare_Func (D.Func) (Model, A.all, B.all, D.Data.all);
      end Internal_Cb;

      -------------------
      -- Set_Sort_Func --
      -------------------

      procedure Set_Sort_Func
         (Sortable       : not null access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
          Sort_Column_Id : Gint;
          Sort_Func      : Gtk_Tree_Iter_Compare_Func;
          User_Data      : User_Data_Type)
      is
      begin
         if Sort_Func = null then
            C_Gtk_Tree_Sortable_Set_Sort_Func (Get_Object (Sortable), Sort_Column_Id, System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            C_Gtk_Tree_Sortable_Set_Sort_Func (Get_Object (Sortable), Sort_Column_Id, Internal_Cb'Address, Users.Build (To_Address (Sort_Func), User_Data), Users.Free_Data'Address);
         end if;
      end Set_Sort_Func;

   end Set_Sort_Func_User_Data;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Column     : Gint;
       Value      : Glib.Values.GValue)
   is
      procedure Internal
         (Tree_Store : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
          Column     : Gint;
          Value      : Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_tree_store_set_value");
   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Value);
   end Set_Value;

   ----------
   -- Swap --
   ----------

   procedure Swap
      (Tree_Store : not null access Gtk_Tree_Store_Record;
       A          : Gtk.Tree_Model.Gtk_Tree_Iter;
       B          : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
         (Tree_Store : System.Address;
          A          : Gtk.Tree_Model.Gtk_Tree_Iter;
          B          : Gtk.Tree_Model.Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_store_swap");
   begin
      Internal (Get_Object (Tree_Store), A, B);
   end Swap;

   --------------
   -- Children --
   --------------

   function Children
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      function Internal
        (Tree_Model : Gtk_Tree_Model;
         Iter       : System.Address;
         Parent     : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_children");
      It : aliased Gtk_Tree_Iter;
   begin
      if Internal (+Tree_Model, It'Address, Iter_Or_Null (Parent'Address)) /= 0 then
         return It;
      else
         return Null_Iter;
      end if;
   end Children;

   ----------------------
   -- Drag_Data_Delete --
   ----------------------

   function Drag_Data_Delete
      (Self : not null access Gtk_Tree_Store_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
   is
      function Internal
         (Self : System.Address;
          Path : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_tree_drag_source_drag_data_delete");
   begin
      return Internal (Get_Object (Self), Get_Object (Path)) /= 0;
   end Drag_Data_Delete;

   -------------------
   -- Drag_Data_Get --
   -------------------

   function Drag_Data_Get
      (Self           : not null access Gtk_Tree_Store_Record;
       Path           : Gtk.Tree_Model.Gtk_Tree_Path;
       Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data)
       return Boolean
   is
      function Internal
         (Self           : System.Address;
          Path           : System.Address;
          Selection_Data : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_tree_drag_source_drag_data_get");
   begin
      return Internal (Get_Object (Self), Get_Object (Path), Get_Object (Selection_Data)) /= 0;
   end Drag_Data_Get;

   ------------------------
   -- Drag_Data_Received --
   ------------------------

   function Drag_Data_Received
      (Self           : not null access Gtk_Tree_Store_Record;
       Dest           : Gtk.Tree_Model.Gtk_Tree_Path;
       Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data)
       return Boolean
   is
      function Internal
         (Self           : System.Address;
          Dest           : System.Address;
          Selection_Data : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_tree_drag_dest_drag_data_received");
   begin
      return Internal (Get_Object (Self), Get_Object (Dest), Get_Object (Selection_Data)) /= 0;
   end Drag_Data_Received;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   function Get_Column_Type
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Index      : Gint) return GType
   is
      function Internal
         (Tree_Model : System.Address;
          Index      : Gint) return GType;
      pragma Import (C, Internal, "gtk_tree_model_get_column_type");
   begin
      return Internal (Get_Object (Tree_Model), Index);
   end Get_Column_Type;

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags
      (Tree_Model : not null access Gtk_Tree_Store_Record)
       return Gtk.Tree_Model.Tree_Model_Flags
   is
      function Internal
         (Tree_Model : System.Address)
          return Gtk.Tree_Model.Tree_Model_Flags;
      pragma Import (C, Internal, "gtk_tree_model_get_flags");
   begin
      return Internal (Get_Object (Tree_Model));
   end Get_Flags;

   --------------
   -- Get_Iter --
   --------------

   function Get_Iter
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      function Internal
        (Tree_Model : Gtk_Tree_Model;
         Iter : System.Address; Path : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_get_iter");
      Iter : aliased Gtk_Tree_Iter;
   begin
      if Internal (+Tree_Model, Iter'Address, Get_Object (Path)) /= 0 then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Get_Iter;

   --------------------
   -- Get_Iter_First --
   --------------------

   function Get_Iter_First
      (Tree_Model : not null access Gtk_Tree_Store_Record)
       return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      function Internal
         (Tree_Model : Gtk_Tree_Model; Iter : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_get_iter_first");
      Iter : aliased Gtk_Tree_Iter;
   begin
      if Internal (+Tree_Model, Iter'Address) /= 0 then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Get_Iter_First;

   --------------------------
   -- Get_Iter_From_String --
   --------------------------

   function Get_Iter_From_String
      (Tree_Model  : not null access Gtk_Tree_Store_Record;
       Path_String : UTF8_String) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      function Internal
        (Tree_Model : Gtk_Tree_Model;
         Iter : System.Address; Str : String) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_get_iter_from_string");
      Iter : aliased Gtk_Tree_Iter;
   begin
      if Internal
        (+Tree_Model, Iter'Address, Path_String & ASCII.NUL) /= 0
      then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Get_Iter_From_String;

   -------------------
   -- Get_N_Columns --
   -------------------

   function Get_N_Columns
      (Tree_Model : not null access Gtk_Tree_Store_Record) return Gint
   is
      function Internal (Tree_Model : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_get_n_columns");
   begin
      return Internal (Get_Object (Tree_Model));
   end Get_N_Columns;

   --------------
   -- Get_Path --
   --------------

   function Get_Path
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Path
   is
      function Internal
         (Tree_Model : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return System.Address;
      pragma Import (C, Internal, "gtk_tree_model_get_path");
   begin
      return From_Object (Internal (Get_Object (Tree_Model), Iter));
   end Get_Path;

   ------------------------
   -- Get_Sort_Column_Id --
   ------------------------

   procedure Get_Sort_Column_Id
      (Sortable       : not null access Gtk_Tree_Store_Record;
       Sort_Column_Id : out Gint;
       Order          : out Gtk.Enums.Gtk_Sort_Type)
   is
      procedure Internal
         (Sortable       : System.Address;
          Sort_Column_Id : out Gint;
          Order          : out Gtk.Enums.Gtk_Sort_Type);
      pragma Import (C, Internal, "gtk_tree_sortable_get_sort_column_id");
   begin
      Internal (Get_Object (Sortable), Sort_Column_Id, Order);
   end Get_Sort_Column_Id;

   --------------------------
   -- Get_String_From_Iter --
   --------------------------

   function Get_String_From_Iter
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return UTF8_String
   is
      function Internal
         (Tree_Model : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
          return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_tree_model_get_string_from_iter");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Tree_Model), Iter));
   end Get_String_From_Iter;

   ---------------
   -- Get_Value --
   ---------------

   procedure Get_Value
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Column     : Gint;
       Value      : out Glib.Values.GValue)
   is
      procedure Internal
         (Tree_Model : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
          Column     : Gint;
          Value      : out Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_tree_model_get_value");
   begin
      Internal (Get_Object (Tree_Model), Iter, Column, Value);
   end Get_Value;

   ---------------
   -- Has_Child --
   ---------------

   function Has_Child
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      function Internal
         (Tree_Model : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Integer;
      pragma Import (C, Internal, "gtk_tree_model_iter_has_child");
   begin
      return Internal (Get_Object (Tree_Model), Iter) /= 0;
   end Has_Child;

   ---------------------------
   -- Has_Default_Sort_Func --
   ---------------------------

   function Has_Default_Sort_Func
      (Sortable : not null access Gtk_Tree_Store_Record) return Boolean
   is
      function Internal (Sortable : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_tree_sortable_has_default_sort_func");
   begin
      return Internal (Get_Object (Sortable)) /= 0;
   end Has_Default_Sort_Func;

   ----------------
   -- N_Children --
   ----------------

   function N_Children
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
       return Gint
   is
      function Internal
          (Tree_Model : Gtk_Tree_Model; Iter : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_n_children");
   begin
      return Internal (+Tree_Model, Iter_Or_Null (Iter'Address));
   end N_Children;

   ----------
   -- Next --
   ----------

   procedure Next
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      function Internal
         (Tree_Model : Gtk_Tree_Model; Iter : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_next");
      Local_Iter : aliased Gtk_Tree_Iter := Iter;
   begin
      if Internal (+Tree_Model, Local_Iter'Address) = 0 then
         Iter := Null_Iter;
      else
         Iter := Local_Iter;
      end if;
   end Next;

   ---------------
   -- Nth_Child --
   ---------------

   function Nth_Child
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
       N          : Gint) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      function Internal
        (Tree_Model : Gtk_Tree_Model;
         Iter, Parent : System.Address; N : Gint) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_nth_child");
      Iter : aliased Gtk_Tree_Iter;
   begin
      if Internal (+Tree_Model, Iter'Address, Iter_Or_Null (Parent'Address), N) /= 0 then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Nth_Child;

   ------------
   -- Parent --
   ------------

   function Parent
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Child      : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      function Internal
        (Tree_Model : Gtk_Tree_Model;
         Iter       : System.Address;
         Child      : Gtk_Tree_Iter) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_parent");
      Iter : aliased Gtk_Tree_Iter;
   begin
      if Internal (+Tree_Model, Iter'Address, Child) /= 0 then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Parent;

   --------------
   -- Previous --
   --------------

   procedure Previous
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      function Internal
         (Tree_Model : Gtk_Tree_Model; Iter : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_previous");
      Local_Iter : aliased Gtk_Tree_Iter := Iter;
   begin
      if Internal (+Tree_Model, Local_Iter'Address) = 0 then
         Iter := Null_Iter;
      else
         Iter := Local_Iter;
      end if;
   end Previous;

   --------------
   -- Ref_Node --
   --------------

   procedure Ref_Node
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
         (Tree_Model : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_model_ref_node");
   begin
      Internal (Get_Object (Tree_Model), Iter);
   end Ref_Node;

   -----------------
   -- Row_Changed --
   -----------------

   procedure Row_Changed
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
         (Tree_Model : System.Address;
          Path       : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_model_row_changed");
   begin
      Internal (Get_Object (Tree_Model), Get_Object (Path), Iter);
   end Row_Changed;

   -----------------
   -- Row_Deleted --
   -----------------

   procedure Row_Deleted
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal
         (Tree_Model : System.Address;
          Path       : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_row_deleted");
   begin
      Internal (Get_Object (Tree_Model), Get_Object (Path));
   end Row_Deleted;

   -------------------
   -- Row_Draggable --
   -------------------

   function Row_Draggable
      (Self : not null access Gtk_Tree_Store_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
   is
      function Internal
         (Self : System.Address;
          Path : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_tree_drag_source_row_draggable");
   begin
      return Internal (Get_Object (Self), Get_Object (Path)) /= 0;
   end Row_Draggable;

   -----------------------
   -- Row_Drop_Possible --
   -----------------------

   function Row_Drop_Possible
      (Self           : not null access Gtk_Tree_Store_Record;
       Dest_Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data)
       return Boolean
   is
      function Internal
         (Self           : System.Address;
          Dest_Path      : System.Address;
          Selection_Data : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_tree_drag_dest_row_drop_possible");
   begin
      return Internal (Get_Object (Self), Get_Object (Dest_Path), Get_Object (Selection_Data)) /= 0;
   end Row_Drop_Possible;

   ---------------------------
   -- Row_Has_Child_Toggled --
   ---------------------------

   procedure Row_Has_Child_Toggled
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
         (Tree_Model : System.Address;
          Path       : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_model_row_has_child_toggled");
   begin
      Internal (Get_Object (Tree_Model), Get_Object (Path), Iter);
   end Row_Has_Child_Toggled;

   ------------------
   -- Row_Inserted --
   ------------------

   procedure Row_Inserted
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
         (Tree_Model : System.Address;
          Path       : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_model_row_inserted");
   begin
      Internal (Get_Object (Tree_Model), Get_Object (Path), Iter);
   end Row_Inserted;

   --------------------
   -- Rows_Reordered --
   --------------------

   procedure Rows_Reordered
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       New_Order  : Gint_Array)
   is
      procedure Internal
         (Tree_Model : System.Address;
          Path       : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
          New_Order  : Gint_Array);
      pragma Import (C, Internal, "gtk_tree_model_rows_reordered");
   begin
      Internal (Get_Object (Tree_Model), Get_Object (Path), Iter, New_Order);
   end Rows_Reordered;

   ------------------------
   -- Set_Sort_Column_Id --
   ------------------------

   procedure Set_Sort_Column_Id
      (Sortable       : not null access Gtk_Tree_Store_Record;
       Sort_Column_Id : Gint;
       Order          : Gtk.Enums.Gtk_Sort_Type)
   is
      procedure Internal
         (Sortable       : System.Address;
          Sort_Column_Id : Gint;
          Order          : Gtk.Enums.Gtk_Sort_Type);
      pragma Import (C, Internal, "gtk_tree_sortable_set_sort_column_id");
   begin
      Internal (Get_Object (Sortable), Sort_Column_Id, Order);
   end Set_Sort_Column_Id;

   -------------------------
   -- Sort_Column_Changed --
   -------------------------

   procedure Sort_Column_Changed
      (Sortable : not null access Gtk_Tree_Store_Record)
   is
      procedure Internal (Sortable : System.Address);
      pragma Import (C, Internal, "gtk_tree_sortable_sort_column_changed");
   begin
      Internal (Get_Object (Sortable));
   end Sort_Column_Changed;

   ----------------
   -- Unref_Node --
   ----------------

   procedure Unref_Node
      (Tree_Model : not null access Gtk_Tree_Store_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
         (Tree_Model : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_model_unref_node");
   begin
      Internal (Get_Object (Tree_Model), Iter);
   end Unref_Node;

end Gtk.Tree_Store;
