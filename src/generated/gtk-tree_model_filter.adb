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

package body Gtk.Tree_Model_Filter is

   procedure C_Gtk_Tree_Model_Filter_Set_Modify_Func
      (Self      : System.Address;
       N_Columns : Glib.Gint;
       Types     : Glib.GType_Array;
       Func      : System.Address;
       Data      : System.Address;
       Destroy   : System.Address);
   pragma Import (C, C_Gtk_Tree_Model_Filter_Set_Modify_Func, "gtk_tree_model_filter_set_modify_func");
   --  With the N_Columns and Types parameters, you give an array of column
   --  types for this model (which will be exposed to the parent model/view).
   --  The Func, Data and Destroy parameters are for specifying the modify
   --  function. The modify function will get called for each data access, the
   --  goal of the modify function is to return the data which should be
   --  displayed at the location specified using the parameters of the modify
   --  function.
   --  Note that Gtk.Tree_Model_Filter.Set_Modify_Func can only be called once
   --  for a given filter model.
   --  Since: gtk+ 2.4
   --  "n_columns": The number of columns in the filter model.
   --  "types": The GTypes of the columns.
   --  "func": A Gtk_Tree_Model_Filter_Modify_Func
   --  "data": User data to pass to the modify function, or null.
   --  "destroy": Destroy notifier of Data, or null.

   procedure C_Gtk_Tree_Model_Filter_Set_Visible_Func
      (Self    : System.Address;
       Func    : System.Address;
       Data    : System.Address;
       Destroy : System.Address);
   pragma Import (C, C_Gtk_Tree_Model_Filter_Set_Visible_Func, "gtk_tree_model_filter_set_visible_func");
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
   --  |[<!-- language="C" --> static gboolean visible_func (GtkTreeModel
   --  *model, GtkTreeIter *iter, gpointer data) { // Visible if row is
   --  non-empty and first column is "HI" gchar *str; gboolean visible = FALSE;
   --  gtk_tree_model_get (model, iter, 0, &str, -1); if (str && strcmp (str,
   --  "HI") == 0) visible = TRUE; g_free (str);
   --  return visible; } ]|
   --  Note that Gtk.Tree_Model_Filter.Set_Visible_Func or
   --  Gtk.Tree_Model_Filter.Set_Visible_Column can only be called once for a
   --  given filter model.
   --  Since: gtk+ 2.4
   --  "func": A Gtk_Tree_Model_Filter_Visible_Func, the visible function
   --  "data": User data to pass to the visible function, or null
   --  "destroy": Destroy notifier of Data, or null

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

   function To_Gtk_Tree_Model_Filter_Modify_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_Model_Filter_Modify_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Tree_Model_Filter_Modify_Func, System.Address);

   function To_Gtk_Tree_Model_Filter_Visible_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_Model_Filter_Visible_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Tree_Model_Filter_Visible_Func, System.Address);

   function To_Gtk_Tree_Model_Foreach_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_Model_Foreach_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Tree_Model_Foreach_Func, System.Address);

   procedure Internal_Gtk_Tree_Model_Filter_Modify_Func
      (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter   : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Value  : in out Glib.Values.GValue;
       Column : Glib.Gint;
       Data   : System.Address);
   pragma Convention (C, Internal_Gtk_Tree_Model_Filter_Modify_Func);
   --  "model": the Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter
   --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter pointing to the row whose
   --  display values are determined
   --  "value": A Glib.Values.GValue which is already initialized for with the
   --  correct type for the column Column.
   --  "column": the column whose display value is determined
   --  "data": user data given to Gtk.Tree_Model_Filter.Set_Modify_Func

   function Internal_Gtk_Tree_Model_Filter_Visible_Func
      (Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Data  : System.Address) return Glib.Gboolean;
   pragma Convention (C, Internal_Gtk_Tree_Model_Filter_Visible_Func);
   --  "model": the child model of the
   --  Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter
   --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter pointing to the row in Model
   --  whose visibility is determined
   --  "data": user data given to Gtk.Tree_Model_Filter.Set_Visible_Func

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

   ------------------------------------------------
   -- Internal_Gtk_Tree_Model_Filter_Modify_Func --
   ------------------------------------------------

   procedure Internal_Gtk_Tree_Model_Filter_Modify_Func
      (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter   : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Value  : in out Glib.Values.GValue;
       Column : Glib.Gint;
       Data   : System.Address)
   is
      Func : constant Gtk_Tree_Model_Filter_Modify_Func := To_Gtk_Tree_Model_Filter_Modify_Func (Data);
   begin
      Func (Model, Iter.all, Value, Column);
   end Internal_Gtk_Tree_Model_Filter_Modify_Func;

   -------------------------------------------------
   -- Internal_Gtk_Tree_Model_Filter_Visible_Func --
   -------------------------------------------------

   function Internal_Gtk_Tree_Model_Filter_Visible_Func
      (Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Data  : System.Address) return Glib.Gboolean
   is
      Func : constant Gtk_Tree_Model_Filter_Visible_Func := To_Gtk_Tree_Model_Filter_Visible_Func (Data);
   begin
      return Boolean'Pos (Func (Model, Iter.all));
   end Internal_Gtk_Tree_Model_Filter_Visible_Func;

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

   package Type_Conversion_Gtk_Tree_Model_Filter is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tree_Model_Filter_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Tree_Model_Filter);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self        : out Gtk_Tree_Model_Filter;
       Child_Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Root        : Gtk.Tree_Model.Gtk_Tree_Path := Null_Gtk_Tree_Path)
   is
   begin
      Self := new Gtk_Tree_Model_Filter_Record;
      Gtk.Tree_Model_Filter.Initialize (Self, Child_Model, Root);
   end Gtk_New;

   --------------------------------------
   -- Gtk_Tree_Model_Filter_Filter_New --
   --------------------------------------

   function Gtk_Tree_Model_Filter_Filter_New
      (Child_Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Root        : Gtk.Tree_Model.Gtk_Tree_Path := Null_Gtk_Tree_Path)
       return Gtk_Tree_Model_Filter
   is
      Self : constant Gtk_Tree_Model_Filter := new Gtk_Tree_Model_Filter_Record;
   begin
      Gtk.Tree_Model_Filter.Initialize (Self, Child_Model, Root);
      return Self;
   end Gtk_Tree_Model_Filter_Filter_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self        : not null access Gtk_Tree_Model_Filter_Record'Class;
       Child_Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Root        : Gtk.Tree_Model.Gtk_Tree_Path := Null_Gtk_Tree_Path)
   is
      function Internal
         (Child_Model : Gtk.Tree_Model.Gtk_Tree_Model;
          Root        : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_model_filter_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Child_Model, Get_Object (Root)));
      end if;
   end Initialize;

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache
      (Self : not null access Gtk_Tree_Model_Filter_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_filter_clear_cache");
   begin
      Internal (Get_Object (Self));
   end Clear_Cache;

   --------------------------------
   -- Convert_Child_Iter_To_Iter --
   --------------------------------

   procedure Convert_Child_Iter_To_Iter
      (Self        : not null access Gtk_Tree_Model_Filter_Record;
       Filter_Iter : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Child_Iter  : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
         (Self        : System.Address;
          Filter_Iter : out Gtk.Tree_Model.Gtk_Tree_Iter;
          Child_Iter  : Gtk.Tree_Model.Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_model_filter_convert_child_iter_to_iter");
      Tmp_Filter_Iter : aliased Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Internal (Get_Object (Self), Tmp_Filter_Iter, Child_Iter);
      Filter_Iter := Tmp_Filter_Iter;
   end Convert_Child_Iter_To_Iter;

   --------------------------------
   -- Convert_Child_Path_To_Path --
   --------------------------------

   function Convert_Child_Path_To_Path
      (Self       : not null access Gtk_Tree_Model_Filter_Record;
       Child_Path : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gtk.Tree_Model.Gtk_Tree_Path
   is
      function Internal
         (Self       : System.Address;
          Child_Path : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_model_filter_convert_child_path_to_path");
   begin
      return From_Object (Internal (Get_Object (Self), Get_Object (Child_Path)));
   end Convert_Child_Path_To_Path;

   --------------------------------
   -- Convert_Iter_To_Child_Iter --
   --------------------------------

   procedure Convert_Iter_To_Child_Iter
      (Self        : not null access Gtk_Tree_Model_Filter_Record;
       Child_Iter  : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Filter_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
         (Self        : System.Address;
          Child_Iter  : out Gtk.Tree_Model.Gtk_Tree_Iter;
          Filter_Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_model_filter_convert_iter_to_child_iter");
      Tmp_Child_Iter : aliased Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Internal (Get_Object (Self), Tmp_Child_Iter, Filter_Iter);
      Child_Iter := Tmp_Child_Iter;
   end Convert_Iter_To_Child_Iter;

   --------------------------------
   -- Convert_Path_To_Child_Path --
   --------------------------------

   function Convert_Path_To_Child_Path
      (Self        : not null access Gtk_Tree_Model_Filter_Record;
       Filter_Path : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gtk.Tree_Model.Gtk_Tree_Path
   is
      function Internal
         (Self        : System.Address;
          Filter_Path : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_model_filter_convert_path_to_child_path");
   begin
      return From_Object (Internal (Get_Object (Self), Get_Object (Filter_Path)));
   end Convert_Path_To_Child_Path;

   -------------
   -- Foreach --
   -------------

   procedure Foreach
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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
         (Tree_Model : not null access Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter_Record'Class;
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
      (Self : not null access Gtk_Tree_Model_Filter_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model
   is
      function Internal
         (Self : System.Address) return Gtk.Tree_Model.Gtk_Tree_Model;
      pragma Import (C, Internal, "gtk_tree_model_filter_get_model");
   begin
      return Internal (Get_Object (Self));
   end Get_Model;

   --------------
   -- Refilter --
   --------------

   procedure Refilter (Self : not null access Gtk_Tree_Model_Filter_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_filter_refilter");
   begin
      Internal (Get_Object (Self));
   end Refilter;

   ---------------------
   -- Set_Modify_Func --
   ---------------------

   procedure Set_Modify_Func
      (Self  : not null access Gtk_Tree_Model_Filter_Record;
       Types : Glib.GType_Array;
       Func  : Gtk_Tree_Model_Filter_Modify_Func)
   is
   begin
      if Func = null then
         C_Gtk_Tree_Model_Filter_Set_Modify_Func (Get_Object (Self), Types'Length, Types, System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Tree_Model_Filter_Set_Modify_Func (Get_Object (Self), Types'Length, Types, Internal_Gtk_Tree_Model_Filter_Modify_Func'Address, To_Address (Func), System.Null_Address);
      end if;
   end Set_Modify_Func;

   package body Set_Modify_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Tree_Model_Filter_Modify_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Tree_Model_Filter_Modify_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Tree_Model_Filter_Modify_Func, System.Address);

      procedure Internal_Cb
         (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
          Iter   : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Value  : in out Glib.Values.GValue;
          Column : Glib.Gint;
          Data   : System.Address);
      pragma Convention (C, Internal_Cb);
      --  A function which calculates display values from raw values in the
      --  model. It must fill Value with the display value for the column
      --  Column in the row indicated by Iter.
      --  Since this function is called for each data access, it's not a
      --  particularly efficient operation.
      --  "model": the Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter
      --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter pointing to the row whose
      --  display values are determined
      --  "value": A Glib.Values.GValue which is already initialized for with
      --  the correct type for the column Column.
      --  "column": the column whose display value is determined
      --  "data": user data given to Gtk.Tree_Model_Filter.Set_Modify_Func

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
          Iter   : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Value  : in out Glib.Values.GValue;
          Column : Glib.Gint;
          Data   : System.Address)
      is
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         To_Gtk_Tree_Model_Filter_Modify_Func (D.Func) (Model, Iter.all, Value, Column, D.Data.all);
      end Internal_Cb;

      ---------------------
      -- Set_Modify_Func --
      ---------------------

      procedure Set_Modify_Func
         (Self  : not null access Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter_Record'Class;
          Types : Glib.GType_Array;
          Func  : Gtk_Tree_Model_Filter_Modify_Func;
          Data  : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Tree_Model_Filter_Set_Modify_Func (Get_Object (Self), Types'Length, Types, System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Func), Data);
            C_Gtk_Tree_Model_Filter_Set_Modify_Func (Get_Object (Self), Types'Length, Types, Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Modify_Func;

   end Set_Modify_Func_User_Data;

   ------------------------
   -- Set_Visible_Column --
   ------------------------

   procedure Set_Visible_Column
      (Self   : not null access Gtk_Tree_Model_Filter_Record;
       Column : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Column : Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_model_filter_set_visible_column");
   begin
      Internal (Get_Object (Self), Column);
   end Set_Visible_Column;

   ----------------------
   -- Set_Visible_Func --
   ----------------------

   procedure Set_Visible_Func
      (Self : not null access Gtk_Tree_Model_Filter_Record;
       Func : Gtk_Tree_Model_Filter_Visible_Func)
   is
   begin
      if Func = null then
         C_Gtk_Tree_Model_Filter_Set_Visible_Func (Get_Object (Self), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Tree_Model_Filter_Set_Visible_Func (Get_Object (Self), Internal_Gtk_Tree_Model_Filter_Visible_Func'Address, To_Address (Func), System.Null_Address);
      end if;
   end Set_Visible_Func;

   package body Set_Visible_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Tree_Model_Filter_Visible_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Tree_Model_Filter_Visible_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Tree_Model_Filter_Visible_Func, System.Address);

      function Internal_Cb
         (Model : Gtk.Tree_Model.Gtk_Tree_Model;
          Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Data  : System.Address) return Glib.Gboolean;
      pragma Convention (C, Internal_Cb);
      --  A function which decides whether the row indicated by Iter is
      --  visible.
      --  "model": the child model of the
      --  Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter
      --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter pointing to the row in Model
      --  whose visibility is determined
      --  "data": user data given to Gtk.Tree_Model_Filter.Set_Visible_Func

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Model : Gtk.Tree_Model.Gtk_Tree_Model;
          Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Data  : System.Address) return Glib.Gboolean
      is
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         return Boolean'Pos (To_Gtk_Tree_Model_Filter_Visible_Func (D.Func) (Model, Iter.all, D.Data.all));
      end Internal_Cb;

      ----------------------
      -- Set_Visible_Func --
      ----------------------

      procedure Set_Visible_Func
         (Self : not null access Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter_Record'Class;
          Func : Gtk_Tree_Model_Filter_Visible_Func;
          Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Tree_Model_Filter_Set_Visible_Func (Get_Object (Self), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Func), Data);
            C_Gtk_Tree_Model_Filter_Set_Visible_Func (Get_Object (Self), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Visible_Func;

   end Set_Visible_Func_User_Data;

   --------------
   -- Children --
   --------------

   function Children
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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
      (Self : not null access Gtk_Tree_Model_Filter_Record;
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
      (Self           : not null access Gtk_Tree_Model_Filter_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record)
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
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record)
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
      (Tree_Model  : not null access Gtk_Tree_Model_Filter_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record)
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
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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

   --------------------------
   -- Get_String_From_Iter --
   --------------------------

   function Get_String_From_Iter
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      function Internal
         (Tree_Model : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_model_iter_has_child");
   begin
      return Internal (Get_Object (Tree_Model), Iter) /= 0;
   end Has_Child;

   ----------------
   -- N_Children --
   ----------------

   function N_Children
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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
      (Self : not null access Gtk_Tree_Model_Filter_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
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

   ----------------
   -- Unref_Node --
   ----------------

   procedure Unref_Node
      (Tree_Model : not null access Gtk_Tree_Model_Filter_Record;
       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
         (Tree_Model : System.Address;
          Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_model_unref_node");
   begin
      Internal (Get_Object (Tree_Model), Iter);
   end Unref_Node;

end Gtk.Tree_Model_Filter;
