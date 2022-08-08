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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with Glib.Object;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;

package body Gtk.Tree_Model_Sort is

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
       Sort_Column_Id : Glib.Gint;
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
       User_Data : System.Address) return Glib.Gint;
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
       Data  : System.Address) return Glib.Gboolean;
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
       User_Data : System.Address) return Glib.Gint
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
       Data  : System.Address) return Glib.Gboolean
   is
      Func : constant Gtk_Tree_Model_Foreach_Func := To_Gtk_Tree_Model_Foreach_Func (Data);
   begin
      return Boolean'Pos (Func (Model, From_Object (Path), Iter.all));
   end Internal_Gtk_Tree_Model_Foreach_Func;

   package Type_Conversion_Gtk_Tree_Model_Sort is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tree_Model_Sort_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Tree_Model_Sort);

   ------------------------
   -- Gtk_New_With_Model --
   ------------------------

   procedure Gtk_New_With_Model
      (Self        : out Gtk_Tree_Model_Sort;
       Child_Model : Gtk.Tree_Model.Gtk_Tree_Model)
   is
   begin
      Self := new Gtk_Tree_Model_Sort_Record;
      Gtk.Tree_Model_Sort.Initialize_With_Model (Self, Child_Model);
   end Gtk_New_With_Model;

   ----------------------------------------
   -- Gtk_Tree_Model_Sort_New_With_Model --
   ----------------------------------------

   function Gtk_Tree_Model_Sort_New_With_Model
      (Child_Model : Gtk.Tree_Model.Gtk_Tree_Model)
       return Gtk_Tree_Model_Sort
   is
      Self : constant Gtk_Tree_Model_Sort := new Gtk_Tree_Model_Sort_Record;
   begin
      Gtk.Tree_Model_Sort.Initialize_With_Model (Self, Child_Model);
      return Self;
   end Gtk_Tree_Model_Sort_New_With_Model;

   ---------------------------
   -- Initialize_With_Model --
   ---------------------------

   procedure Initialize_With_Model
      (Self        : not null access Gtk_Tree_Model_Sort_Record'Class;
       Child_Model : Gtk.Tree_Model.Gtk_Tree_Model)
   is
      function Internal
         (Child_Model : Gtk.Tree_Model.Gtk_Tree_Model) return System.Address;
      pragma Import (C, Internal, "gtk_tree_model_sort_new_with_model");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Child_Model));
      end if;
   end Initialize_With_Model;

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache (Self : not null access Gtk_Tree_Model_Sort_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_sort_clear_cache");
   begin
      Internal (Get_Object (Self));
   end Clear_Cache;

   --------------------------------
   -- Convert_Child_Iter_To_Iter --
   --------------------------------

   function Convert_Child_Iter_To_Iter
      (Self       : not null access Gtk_Tree_Model_Sort_Record;
       Sort_Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Child_Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      function Internal
         (Self          : System.Address;
          Acc_Sort_Iter : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Child_Iter    : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_model_sort_convert_child_iter_to_iter");
      Acc_Sort_Iter     : aliased Gtk.Tree_Model.Gtk_Tree_Iter;
      Tmp_Acc_Sort_Iter : aliased Gtk.Tree_Model.Gtk_Tree_Iter;
      Tmp_Return        : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Acc_Sort_Iter'Access, Child_Iter);
      Acc_Sort_Iter := Tmp_Acc_Sort_Iter;
      Sort_Iter.all := Acc_Sort_Iter;
      return Tmp_Return /= 0;
   end Convert_Child_Iter_To_Iter;

   --------------------------------
   -- Convert_Child_Path_To_Path --
   --------------------------------

   function Convert_Child_Path_To_Path
      (Self       : not null access Gtk_Tree_Model_Sort_Record;
       Child_Path : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gtk.Tree_Model.Gtk_Tree_Path
   is
      function Internal
         (Self       : System.Address;
          Child_Path : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_model_sort_convert_child_path_to_path");
   begin
      return From_Object (Internal (Get_Object (Self), Get_Object (Child_Path)));
   end Convert_Child_Path_To_Path;

   --------------------------------
   -- Convert_Iter_To_Child_Iter --
   --------------------------------

   procedure Convert_Iter_To_Child_Iter
      (Self        : not null access Gtk_Tree_Model_Sort_Record;
       Child_Iter  : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Sorted_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
         (Self        : System.Address;
          Child_Iter  : out Gtk.Tree_Model.Gtk_Tree_Iter;
          Sorted_Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_model_sort_convert_iter_to_child_iter");
      Tmp_Child_Iter : aliased Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Internal (Get_Object (Self), Tmp_Child_Iter, Sorted_Iter);
      Child_Iter := Tmp_Child_Iter;
   end Convert_Iter_To_Child_Iter;

   --------------------------------
   -- Convert_Path_To_Child_Path --
   --------------------------------

   function Convert_Path_To_Child_Path
      (Self        : not null access Gtk_Tree_Model_Sort_Record;
       Sorted_Path : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gtk.Tree_Model.Gtk_Tree_Path
   is
      function Internal
         (Self        : System.Address;
          Sorted_Path : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_model_sort_convert_path_to_child_path");
   begin
      return From_Object (Internal (Get_Object (Self), Get_Object (Sorted_Path)));
   end Convert_Path_To_Child_Path;

   -------------
   -- Foreach --
   -------------

   procedure Foreach
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
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
          Data  : System.Address) return Glib.Gboolean;
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
         (Tree_Model : not null access Gtk.Tree_Model_Sort.Gtk_Tree_Model_Sort_Record'Class;
          Func       : Gtk_Tree_Model_Foreach_Func;
          User_Data  : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Tree_Model_Foreach (Get_Object (Tree_Model), System.Null_Address, System.Null_Address);
         else
            D := Users.Build (To_Address (Func), User_Data);
            C_Gtk_Tree_Model_Foreach (Get_Object (Tree_Model), Internal_Cb'Address, D);
            Users.Free_Data (D);
         end if;
      end Foreach;

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Model : Gtk.Tree_Model.Gtk_Tree_Model;
          Path  : System.Address;
          Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Data  : System.Address) return Glib.Gboolean
      is
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         return Boolean'Pos (To_Gtk_Tree_Model_Foreach_Func (D.Func) (Model, From_Object (Path), Iter.all, D.Data.all));
      end Internal_Cb;

   end Foreach_User_Data;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
      (Self : not null access Gtk_Tree_Model_Sort_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model
   is
      function Internal
         (Self : System.Address) return Gtk.Tree_Model.Gtk_Tree_Model;
      pragma Import (C, Internal, "gtk_tree_model_sort_get_model");
   begin
      return Internal (Get_Object (Self));
   end Get_Model;

   -------------------
   -- Iter_Is_Valid --
   -------------------

   function Iter_Is_Valid
      (Self : not null access Gtk_Tree_Model_Sort_Record;
       Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      function Internal
         (Self : System.Address;
          Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_model_sort_iter_is_valid");
   begin
      return Internal (Get_Object (Self), Iter) /= 0;
   end Iter_Is_Valid;

   -----------------------------
   -- Reset_Default_Sort_Func --
   -----------------------------

   procedure Reset_Default_Sort_Func
      (Self : not null access Gtk_Tree_Model_Sort_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_sort_reset_default_sort_func");
   begin
      Internal (Get_Object (Self));
   end Reset_Default_Sort_Func;

   ---------------------------
   -- Set_Default_Sort_Func --
   ---------------------------

   procedure Set_Default_Sort_Func
      (Sortable  : not null access Gtk_Tree_Model_Sort_Record;
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
          User_Data : System.Address) return Glib.Gint;
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
      --  `price_of(A) - price_of(B)`.
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
          User_Data : System.Address) return Glib.Gint
      is
         D : constant Users.Internal_Data_Access := Users.Convert (User_Data);
      begin
         return To_Gtk_Tree_Iter_Compare_Func (D.Func) (Model, A.all, B.all, D.Data.all);
      end Internal_Cb;

      ---------------------------
      -- Set_Default_Sort_Func --
      ---------------------------

      procedure Set_Default_Sort_Func
         (Sortable  : not null access Gtk.Tree_Model_Sort.Gtk_Tree_Model_Sort_Record'Class;
          Sort_Func : Gtk_Tree_Iter_Compare_Func;
          User_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Sort_Func = null then
            C_Gtk_Tree_Sortable_Set_Default_Sort_Func (Get_Object (Sortable), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Sort_Func), User_Data);
            C_Gtk_Tree_Sortable_Set_Default_Sort_Func (Get_Object (Sortable), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Default_Sort_Func;

   end Set_Default_Sort_Func_User_Data;

   -------------------
   -- Set_Sort_Func --
   -------------------

   procedure Set_Sort_Func
      (Sortable       : not null access Gtk_Tree_Model_Sort_Record;
       Sort_Column_Id : Glib.Gint;
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
          User_Data : System.Address) return Glib.Gint;
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
      --  `price_of(A) - price_of(B)`.
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
          User_Data : System.Address) return Glib.Gint
      is
         D : constant Users.Internal_Data_Access := Users.Convert (User_Data);
      begin
         return To_Gtk_Tree_Iter_Compare_Func (D.Func) (Model, A.all, B.all, D.Data.all);
      end Internal_Cb;

      -------------------
      -- Set_Sort_Func --
      -------------------

      procedure Set_Sort_Func
         (Sortable       : not null access Gtk.Tree_Model_Sort.Gtk_Tree_Model_Sort_Record'Class;
          Sort_Column_Id : Glib.Gint;
          Sort_Func      : Gtk_Tree_Iter_Compare_Func;
          User_Data      : User_Data_Type)
      is
         D : System.Address;
      begin
         if Sort_Func = null then
            C_Gtk_Tree_Sortable_Set_Sort_Func (Get_Object (Sortable), Sort_Column_Id, System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Sort_Func), User_Data);
            C_Gtk_Tree_Sortable_Set_Sort_Func (Get_Object (Sortable), Sort_Column_Id, Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Sort_Func;

   end Set_Sort_Func_User_Data;

   --------------
   -- Children --
   --------------

   function Children
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
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
      (Self : not null access Gtk_Tree_Model_Sort_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
   is
      function Internal
         (Self : System.Address;
          Path : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_drag_source_drag_data_delete");
   begin
      return Internal (Get_Object (Self), Get_Object (Path)) /= 0;
   end Drag_Data_Delete;

   -------------------
   -- Drag_Data_Get --
   -------------------

   function Drag_Data_Get
      (Self           : not null access Gtk_Tree_Model_Sort_Record;
       Path           : Gtk.Tree_Model.Gtk_Tree_Path;
       Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data)
       return Boolean
   is
      function Internal
         (Self           : System.Address;
          Path           : System.Address;
          Selection_Data : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_drag_source_drag_data_get");
   begin
      return Internal (Get_Object (Self), Get_Object (Path), Get_Object (Selection_Data)) /= 0;
   end Drag_Data_Get;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   function Get_Column_Type
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Index      : Glib.Gint) return GType
   is
      function Internal
         (Tree_Model : System.Address;
          Index      : Glib.Gint) return GType;
      pragma Import (C, Internal, "gtk_tree_model_get_column_type");
   begin
      return Internal (Get_Object (Tree_Model), Index);
   end Get_Column_Type;

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record)
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
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record)
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
      (Tree_Model  : not null access Gtk_Tree_Model_Sort_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record)
       return Glib.Gint
   is
      function Internal (Tree_Model : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_tree_model_get_n_columns");
   begin
      return Internal (Get_Object (Tree_Model));
   end Get_N_Columns;

   --------------
   -- Get_Path --
   --------------

   function Get_Path
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
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
      (Sortable       : not null access Gtk_Tree_Model_Sort_Record;
       Sort_Column_Id : out Glib.Gint;
       Order          : out Gtk.Enums.Gtk_Sort_Type)
   is
      procedure Internal
         (Sortable       : System.Address;
          Sort_Column_Id : out Glib.Gint;
          Order          : out Gtk.Enums.Gtk_Sort_Type);
      pragma Import (C, Internal, "gtk_tree_sortable_get_sort_column_id");
   begin
      Internal (Get_Object (Sortable), Sort_Column_Id, Order);
   end Get_Sort_Column_Id;

   --------------------------
   -- Get_String_From_Iter --
   --------------------------

   function Get_String_From_Iter
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return UTF8_String
   is
      function Internal
         (Tree_Model : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
          return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_tree_model_get_string_from_iter");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Tree_Model), Iter));
   end Get_String_From_Iter;

   ---------------
   -- Get_Value --
   ---------------

   procedure Get_Value
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       Column     : Glib.Gint;
       Value      : out Glib.Values.GValue)
   is
      procedure Internal
         (Tree_Model : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
          Column     : Glib.Gint;
          Value      : out Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_tree_model_get_value");
   begin
      Internal (Get_Object (Tree_Model), Iter, Column, Value);
   end Get_Value;

   ---------------
   -- Has_Child --
   ---------------

   function Has_Child
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      function Internal
         (Tree_Model : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_model_iter_has_child");
   begin
      return Internal (Get_Object (Tree_Model), Iter) /= 0;
   end Has_Child;

   ---------------------------
   -- Has_Default_Sort_Func --
   ---------------------------

   function Has_Default_Sort_Func
      (Sortable : not null access Gtk_Tree_Model_Sort_Record) return Boolean
   is
      function Internal (Sortable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_sortable_has_default_sort_func");
   begin
      return Internal (Get_Object (Sortable)) /= 0;
   end Has_Default_Sort_Func;

   ----------------
   -- N_Children --
   ----------------

   function N_Children
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
       return Glib.Gint
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
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
       N          : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter
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
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
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
      (Self : not null access Gtk_Tree_Model_Sort_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
   is
      function Internal
         (Self : System.Address;
          Path : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_drag_source_row_draggable");
   begin
      return Internal (Get_Object (Self), Get_Object (Path)) /= 0;
   end Row_Draggable;

   ---------------------------
   -- Row_Has_Child_Toggled --
   ---------------------------

   procedure Row_Has_Child_Toggled
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
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

   --------------------------------
   -- Rows_Reordered_With_Length --
   --------------------------------

   procedure Rows_Reordered_With_Length
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Path       : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
       New_Order  : Gint_Array;
       Length     : Glib.Gint)
   is
      procedure Internal
         (Tree_Model : System.Address;
          Path       : System.Address;
          Iter       : System.Address;
          New_Order  : System.Address;
          Length     : Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_model_rows_reordered_with_length");
   begin
      Internal (Get_Object (Tree_Model), Get_Object (Path), Iter_Or_Null (Iter'Address), New_Order (New_Order'First)'Address, Length);
   end Rows_Reordered_With_Length;

   ------------------------
   -- Set_Sort_Column_Id --
   ------------------------

   procedure Set_Sort_Column_Id
      (Sortable       : not null access Gtk_Tree_Model_Sort_Record;
       Sort_Column_Id : Glib.Gint;
       Order          : Gtk.Enums.Gtk_Sort_Type)
   is
      procedure Internal
         (Sortable       : System.Address;
          Sort_Column_Id : Glib.Gint;
          Order          : Gtk.Enums.Gtk_Sort_Type);
      pragma Import (C, Internal, "gtk_tree_sortable_set_sort_column_id");
   begin
      Internal (Get_Object (Sortable), Sort_Column_Id, Order);
   end Set_Sort_Column_Id;

   -------------------------
   -- Sort_Column_Changed --
   -------------------------

   procedure Sort_Column_Changed
      (Sortable : not null access Gtk_Tree_Model_Sort_Record)
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
      (Tree_Model : not null access Gtk_Tree_Model_Sort_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
         (Tree_Model : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_model_unref_node");
   begin
      Internal (Get_Object (Tree_Model), Iter);
   end Unref_Node;

end Gtk.Tree_Model_Sort;
