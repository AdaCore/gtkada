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

with Gtk; use Gtk;
with Interfaces.C.Strings;
with System;

package body Gtk.Tree_Model is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Tree_Path)
   is
   begin
      Widget := new Gtk_Tree_Path_Record;
      Gtk.Tree_Model.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : in out Gtk_Tree_Path)
   is
      function Internal return Gtk_Tree_Path;
      pragma Import (C, Internal, "gtk_tree_path_new");
   begin
      Widget := Internal;
   end Initialize;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Tree_Path;
                      Path : String)
   is
   begin
      Widget := new Gtk_Tree_Path_Record;
      Initialize (Widget, Path);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : in out Gtk_Tree_Path;
                         Path : String)
   is
      function Internal (Path   : String)
                         return Gtk_Tree_Path;
      pragma Import (C, Internal, "gtk_tree_path_new_from_string");
   begin
      Widget := Internal (Path & ASCII.NUL);
   end Initialize;

   -------------------------
   -- Tree_Path_To_String --
   -------------------------

   function Tree_Path_To_String (Path   : Gtk.Tree_Model.Gtk_Tree_Path)
                                 return String
   is
      function Internal (Path   : System.Address)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_tree_path_to_string");
   begin
      return Interfaces.C.Strings.Value (Internal (Path.all'Address));
   end Tree_Path_To_String;

--    -------------
--    -- Gtk_New --
--    -------------

--    procedure Gtk_New (Widget: out Gtk_Tree_Model)
--    is
--    begin
--       Widget := new Gtk_Tree_Model_Record;
--       Initialize (Widget);
--    end Gtk_New;

--    ----------------
--    -- Initialize --
--    ----------------

--    procedure Initialize (Widget: access Gtk_Tree_Model_Record'Class)
--    is
--       function Internal return System.Address;
--       pragma Import (C, Internal, "gtk_tree_path_new_root");
--    begin
--       Set_Object (Widget, Internal);
--       Initialize_User_Data (Widget);
--    end Initialize;

   ----------------------------
   -- Tree_Path_Append_Index --
   ----------------------------

   procedure Tree_Path_Append_Index
     (Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Index : Gint)
   is
      procedure Internal
        (Path  : System.Address;
         Index : Gint);
      pragma Import (C, Internal, "gtk_tree_path_append_index");
   begin
      Internal (Path.all'Address,
                Index);
   end Tree_Path_Append_Index;

   -----------------------------
   -- Tree_Path_Prepend_Index --
   -----------------------------

   procedure Tree_Path_Prepend_Index
     (Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Index : Gint)
   is
      procedure Internal
        (Path  : System.Address;
         Index : Gint);
      pragma Import (C, Internal, "gtk_tree_path_prepend_index");
   begin
      Internal (Path.all'Address,
                Index);
   end Tree_Path_Prepend_Index;

   -------------------------
   -- Tree_Path_Get_Depth --
   -------------------------

   function Tree_Path_Get_Depth (Path   : Gtk.Tree_Model.Gtk_Tree_Path)
                                 return Gint
   is
      function Internal (Path   : System.Address)
                         return Gint;
      pragma Import (C, Internal, "gtk_tree_path_get_depth");
   begin
      return Internal (Path.all'Address);
   end Tree_Path_Get_Depth;

--    ---------------------------
--    -- Tree_Path_Get_Indices --
--    ---------------------------

--    function Tree_Path_Get_Indices (Path   : Gtk.Tree_Model.Gtk_Tree_Path)
--                                   return Gint
--    is
--       function Internal (Path   : System.Address)
--                         return Gint;
--       pragma Import (C, Internal, "gtk_tree_path_get_indices");
--    begin
--       return Internal (Path.all'Address);
--    end Tree_Path_Get_Indices;

   --------------------
   -- Tree_Path_Free --
   --------------------

   procedure Tree_Path_Free (Path : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal (Path : System.Address);
      pragma Import (C, Internal, "gtk_tree_path_free");
   begin
      Internal (Path.all'Address);
   end Tree_Path_Free;

   --------------------
   -- Tree_Path_Copy --
   --------------------

   function Tree_Path_Copy (Path   : Gtk.Tree_Model.Gtk_Tree_Path)
                            return Gtk.Tree_Model.Gtk_Tree_Path
   is
      function Internal (Path   : System.Address)
                         return Gtk.Tree_Model.Gtk_Tree_Path;
      pragma Import (C, Internal, "gtk_tree_path_copy");
   begin
      return Internal (Path.all'Address);
   end Tree_Path_Copy;

   -----------------------
   -- Tree_Path_Compare --
   -----------------------

   function Tree_Path_Compare
     (A      : Gtk.Tree_Model.Gtk_Tree_Path;
      B      : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gint
   is
      function Internal
        (A      : System.Address;
         B      : System.Address)
         return Gint;
      pragma Import (C, Internal, "gtk_tree_path_compare");
   begin
      return Internal (A.all'Address,
                       B.all'Address);
   end Tree_Path_Compare;

   --------------------
   -- Tree_Path_Next --
   --------------------

   procedure Tree_Path_Next (Path : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal (Path : System.Address);
      pragma Import (C, Internal, "gtk_tree_path_next");
   begin
      Internal (Path.all'Address);
   end Tree_Path_Next;

   --------------------
   -- Tree_Path_Prev --
   --------------------

   function Tree_Path_Prev (Path   : Gtk.Tree_Model.Gtk_Tree_Path)
                            return Boolean
   is
      function Internal (Path   : System.Address)
                         return Gint;
      pragma Import (C, Internal, "gtk_tree_path_prev");
   begin
      return Boolean'Val (Internal (Path.all'Address));
   end Tree_Path_Prev;

   ------------------
   -- Tree_Path_Up --
   ------------------

   function Tree_Path_Up (Path   : Gtk.Tree_Model.Gtk_Tree_Path)
                          return Boolean
   is
      function Internal (Path   : System.Address)
                         return Gint;
      pragma Import (C, Internal, "gtk_tree_path_up");
   begin
      return Boolean'Val (Internal (Path.all'Address));
   end Tree_Path_Up;

   --------------------
   -- Tree_Path_Down --
   --------------------

   procedure Tree_Path_Down (Path : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal (Path : System.Address);
      pragma Import (C, Internal, "gtk_tree_path_down");
   begin
      Internal (Path.all'Address);
   end Tree_Path_Down;

   ---------------------------
   -- Tree_Path_Is_Ancestor --
   ---------------------------

   function Tree_Path_Is_Ancestor
     (Path       : Gtk.Tree_Model.Gtk_Tree_Path;
      Descendant : Gtk.Tree_Model.Gtk_Tree_Path)
      return Boolean
   is
      function Internal
        (Path       : System.Address;
         Descendant : System.Address)
         return Gint;
      pragma Import (C, Internal, "gtk_tree_path_is_ancestor");
   begin
      return Boolean'Val (Internal (Path.all'Address,
                                    Descendant.all'Address));
   end Tree_Path_Is_Ancestor;

   -----------------------------
   -- Tree_Path_Is_Descendant --
   -----------------------------

   function Tree_Path_Is_Descendant
     (Path     : Gtk.Tree_Model.Gtk_Tree_Path;
      Ancestor : Gtk.Tree_Model.Gtk_Tree_Path)
      return Boolean
   is
      function Internal
        (Path     : System.Address;
         Ancestor : System.Address)
         return Gint;
      pragma Import (C, Internal, "gtk_tree_path_is_descendant");
   begin
      return Boolean'Val (Internal (Path.all'Address,
                                    Ancestor.all'Address));
   end Tree_Path_Is_Descendant;

--    -------------
--    -- Gtk_New --
--    -------------

--    procedure Gtk_New
--      (Widget : out Gtk_Tree_Row_Reference;
--       Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
--       Path  : Gtk.Tree_Model.Gtk_Tree_Path)
--    is
--    begin
--       Widget := new Gtk_Tree_Row_Reference_Record;
--       Initialize (Widget, Model, Path);
--    end Gtk_New;

--    ----------------
--    -- Initialize --
--    ----------------

--    procedure Initialize
--      (Widget : access Gtk_Tree_Row_Reference_Record'Class;
--       Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
--       Path  : Gtk.Tree_Model.Gtk_Tree_Path)
--    is
--       function Internal
--         (Model  : System.Address;
--          Path   : System.Address)
--         return System.Address;
--       pragma Import (C, Internal, "gtk_tree_row_reference_new");
--    begin
--       Set_Object (Widget, Internal (Get_Object (Model),
--                                     Path.all'Address));
--       Initialize_User_Data (Widget);
--    end Initialize;

--    -------------
--    -- Gtk_New --
--    -------------

--    procedure Gtk_New
--      (Widget : out Gtk_Tree_Row_Reference;
--       Proxy : out Glib.Object.GObject;
--       Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
--       Path  : Gtk.Tree_Model.Gtk_Tree_Path)
--    is
--    begin
--       Widget := new Gtk_Tree_Row_Reference_Record;
--       Initialize (Widget, Proxy, Model, Path);
--    end Gtk_New;

--    ----------------
--    -- Initialize --
--    ----------------

--    procedure Initialize
--      (Widget : access Gtk_Tree_Row_Reference_Record'Class;
--       Proxy : out Glib.Object.GObject;
--       Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
--       Path  : Gtk.Tree_Model.Gtk_Tree_Path)
--    is
--       function Internal
--         (Proxy  : System.Address;
--          Model  : System.Address;
--          Path   : System.Address)
--          return System.Address;
--       pragma Import (C, Internal, "gtk_tree_row_reference_new_proxy");
--    begin
--       Set_Object (Widget, Internal (Get_Object (Proxy),
--                                     Get_Object (Model),
--                                     Path.all'Address));
--       Initialize_User_Data (Widget);
--    end Initialize;

--    ---------------------------------
--    -- Tree_Row_Reference_Get_Path --
--    ---------------------------------

--    function Tree_Row_Reference_Get_Path
--      (Reference : access Gtk.Tree_Model.Gtk_Tree_Row_Reference_Record'Class)
--      return Gtk.Tree_Model.Gtk_Tree_Path
--    is
--       function Internal (Reference : System.Address)
--                          return Gtk_Tree_Path;
--       pragma Import (C, Internal, "gtk_tree_row_reference_get_path");
--    begin
--       return Internal (Get_Object (Reference));
--    end Tree_Row_Reference_Get_Path;

--    ------------------------------
--    -- Tree_Row_Reference_Valid --
--    ------------------------------

--    function Tree_Row_Reference_Valid
--      (Reference : access Gtk.Tree_Model.Gtk_Tree_Row_Reference_Record'Class)
--      return Boolean
--    is
--       function Internal (Reference : System.Address)
--                          return Gint;
--       pragma Import (C, Internal, "gtk_tree_row_reference_valid");
--    begin
--       return Boolean'Val (Internal (Get_Object (Reference)));
--    end Tree_Row_Reference_Valid;

--    -----------------------------
--    -- Tree_Row_Reference_Free --
--    -----------------------------

--    procedure Tree_Row_Reference_Free
--      (Reference : access Gtk.Tree_Model.Gtk_Tree_Row_Reference_Record'Class)
--    is
--       procedure Internal (Reference : System.Address);
--       pragma Import (C, Internal, "gtk_tree_row_reference_free");
--    begin
--       Internal (Get_Object (Reference));
--    end Tree_Row_Reference_Free;

--    ---------------------------------
--    -- Tree_Row_Reference_Inserted --
--    ---------------------------------

--    procedure Tree_Row_Reference_Inserted
--      (Proxy : out Glib.Object.GObject;
--       Path  : Gtk.Tree_Model.Gtk_Tree_Path)
--    is
--       procedure Internal
--         (Proxy : System.Address;
--          Path  : System.Address);
--       pragma Import (C, Internal, "gtk_tree_row_reference_inserted");
--    begin
--       Internal (Get_Object (Proxy),
--                 Path.all'Address);
--    end Tree_Row_Reference_Inserted;

--    --------------------------------
--    -- Tree_Row_Reference_Deleted --
--    --------------------------------

--    procedure Tree_Row_Reference_Deleted
--      (Proxy : out Glib.Object.GObject;
--       Path  : Gtk.Tree_Model.Gtk_Tree_Path)
--    is
--       procedure Internal
--         (Proxy : System.Address;
--          Path  : System.Address);
--       pragma Import (C, Internal, "gtk_tree_row_reference_deleted");
--    begin
--       Internal (Get_Object (Proxy),
--                 Path.all'Address);
--    end Tree_Row_Reference_Deleted;

--    ----------------------------------
--    -- Tree_Row_Reference_Reordered --
--    ----------------------------------

--    procedure Tree_Row_Reference_Reordered
--      (Proxy     : out Glib.Object.GObject;
--       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
--       Iter      : Gtk_Tree_Iter;
--       New_Order : out Gint)
--    is
--       procedure Internal
--         (Proxy     : System.Address;
--          Path      : System.Address;
--          Iter      : System.Address;
--          New_Order : out Gint);
--       pragma Import (C, Internal, "gtk_tree_row_reference_reordered");
--    begin
--       Internal (Get_Object (Proxy),
--                 Path.all'Address,
--                 Iter'Address,
--                 New_Order);
--    end Tree_Row_Reference_Reordered;

   --------------------
   -- Tree_Iter_Copy --
   --------------------

   function Tree_Iter_Copy (Iter   : Gtk_Tree_Iter)
                            return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      function Internal (Iter   : System.Address)
                         return Gtk.Tree_Model.Gtk_Tree_Iter;
      pragma Import (C, Internal, "gtk_tree_iter_copy");

      B : Gtk_Tree_Iter;
   begin
      B := Internal (Iter'Address);
      return B;
   end Tree_Iter_Copy;

   --------------------
   -- Tree_Iter_Free --
   --------------------

   procedure Tree_Iter_Free (Iter : Gtk_Tree_Iter)
   is
      procedure Internal (Iter : System.Address);
      pragma Import (C, Internal, "gtk_tree_iter_free");
   begin
      Internal (Iter'Address);
   end Tree_Iter_Free;

   ------------------------------
   -- Tree_Model_Get_N_Columns --
   ------------------------------

   function Tree_Model_Get_N_Columns
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class)
     return Gint
   is
      function Internal (Tree_Model : System.Address)
                         return Gint;
      pragma Import (C, Internal, "gtk_tree_model_get_n_columns");
   begin
      return Internal (Get_Object (Tree_Model));
   end Tree_Model_Get_N_Columns;

   --------------------------------
   -- Tree_Model_Get_Column_Type --
   --------------------------------

   function Tree_Model_Get_Column_Type
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Index      : Gint)
      return GType
   is
      function Internal
        (Tree_Model : System.Address;
         Index      : Gint)
         return GType;
      pragma Import (C, Internal, "gtk_tree_model_get_column_type");
   begin
      return Internal (Get_Object (Tree_Model),
                       Index);
   end Tree_Model_Get_Column_Type;

   -------------------------
   -- Tree_Model_Get_Iter --
   -------------------------

   procedure Tree_Model_Get_Iter
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter       : out Gtk_Tree_Iter;
      Path       : Gtk.Tree_Model.Gtk_Tree_Path;
      Success    : out Boolean)
   is
      function Internal
        (Tree_Model : System.Address;
         Iter       : System.Address;
         Path       : System.Address)
         return Gint;
      pragma Import (C, Internal, "gtk_tree_model_get_iter");

   begin
      Success := Boolean'Val (Internal (Get_Object (Tree_Model),
                                        Iter'Address,
                                        Path.all'Address));
   end Tree_Model_Get_Iter;

   -------------------------------------
   -- Tree_Model_Get_Iter_From_String --
   -------------------------------------

   procedure Tree_Model_Get_Iter_From_String
     (Tree_Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter        : out Gtk_Tree_Iter;
      Path_String : String;
      Success     : out Boolean)
   is
      function Internal
        (Tree_Model  : System.Address;
         Iter        : System.Address;
         Path_String : String)
         return Gint;
      pragma Import (C, Internal, "gtk_tree_model_get_iter_from_string");
   begin
      Success := Boolean'Val (Internal (Get_Object (Tree_Model),
                                        Iter'Address,
                                        Path_String & ASCII.NUL));
   end Tree_Model_Get_Iter_From_String;

   ------------------------------
   -- Tree_Model_Get_Iter_Root --
   ------------------------------

   procedure Tree_Model_Get_Iter_Root
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter       : out Gtk_Tree_Iter;
      Success    : out Boolean)
   is
      function Internal
        (Tree_Model : System.Address;
         Iter       : System.Address)
         return Gint;
      pragma Import (C, Internal, "gtk_tree_model_get_iter_root");
   begin
      Success := Boolean'Val (Internal (Get_Object (Tree_Model),
                                    Iter'Address));
   end Tree_Model_Get_Iter_Root;

   -------------------------
   -- Tree_Model_Get_Path --
   -------------------------

   function Tree_Model_Get_Path
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter       : Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      function Internal
        (Tree_Model : System.Address;
         Iter       : System.Address)
         return Gtk.Tree_Model.Gtk_Tree_Path;
      pragma Import (C, Internal, "gtk_tree_model_get_path");
   begin
      return Internal (Get_Object (Tree_Model),
                       Iter'Address);
   end Tree_Model_Get_Path;

   --------------------------
   -- Tree_Model_Get_Value --
   --------------------------

   procedure Tree_Model_Get_Value
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint;
      Value      : out Glib.Values.GValue)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Iter       : System.Address;
         Column     : Gint;
         Value      : out Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_tree_model_get_value");
   begin
      Internal (Get_Object (Tree_Model),
                Iter'Address,
                Column,
                Value);
   end Tree_Model_Get_Value;

   --------------------------
   -- Tree_Model_Iter_Next --
   --------------------------

   procedure Tree_Model_Iter_Next
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter       : in out Gtk_Tree_Iter;
      Success    : out Boolean)
   is
      function Internal
        (Tree_Model : System.Address;
         Iter       : System.Address)
         return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_next");
   begin
      Success := Boolean'Val (Internal (Get_Object (Tree_Model),
                                        Iter'Address));
   end Tree_Model_Iter_Next;

   ------------------------------
   -- Tree_Model_Iter_Children --
   ------------------------------

   procedure Tree_Model_Iter_Children
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter       : out Gtk_Tree_Iter;
      Parent     : Gtk_Tree_Iter;
      Success    : out Boolean)
   is
      function Internal
        (Tree_Model : System.Address;
         Iter       : System.Address;
         Parent     : System.Address)
         return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_children");
   begin
      Success := Boolean'Val (Internal (Get_Object (Tree_Model),
                                        Iter'Address,
                                        Parent'Address));
   end Tree_Model_Iter_Children;

   -------------------------------
   -- Tree_Model_Iter_Has_Child --
   -------------------------------

   function Tree_Model_Iter_Has_Child
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter       : Gtk_Tree_Iter)
      return Boolean
   is
      function Internal
        (Tree_Model : System.Address;
         Iter       : System.Address)
         return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_has_child");
   begin
      return Boolean'Val (Internal (Get_Object (Tree_Model),
                                    Iter'Address));
   end Tree_Model_Iter_Has_Child;

   --------------------------------
   -- Tree_Model_Iter_N_Children --
   --------------------------------

   function Tree_Model_Iter_N_Children
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter       : Gtk_Tree_Iter)
      return Gint
   is
      function Internal
        (Tree_Model : System.Address;
         Iter       : System.Address)
         return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_n_children");
   begin
      if Iter = Null_Iter then
         return Internal (Get_Object (Tree_Model),
                          System.Null_Address);
      else
         return Internal (Get_Object (Tree_Model),
                          Iter'Address);
      end if;
   end Tree_Model_Iter_N_Children;

   -------------------------------
   -- Tree_Model_Iter_Nth_Child --
   -------------------------------

   function Tree_Model_Iter_Nth_Child
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter       : Gtk_Tree_Iter;
      Parent     : Gtk_Tree_Iter;
      N          : Gint)
      return Boolean
   is
      function Internal
        (Tree_Model : System.Address;
         Iter       : System.Address;
         Parent     : System.Address;
         N          : Gint)
         return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_nth_child");
   begin
      return Boolean'Val (Internal (Get_Object (Tree_Model),
                                    Iter'Address,
                                    Parent'Address,
                                    N));
   end Tree_Model_Iter_Nth_Child;

   ----------------------------
   -- Tree_Model_Iter_Parent --
   ----------------------------

   function Tree_Model_Iter_Parent
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter       : Gtk_Tree_Iter;
      Child      : Gtk_Tree_Iter)
      return Boolean
   is
      function Internal
        (Tree_Model : System.Address;
         Iter       : System.Address;
         Child      : System.Address)
         return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_parent");
   begin
      return Boolean'Val (Internal (Get_Object (Tree_Model),
                                    Iter'Address,
                                    Child'Address));
   end Tree_Model_Iter_Parent;

   -------------------------
   -- Tree_Model_Ref_Node --
   -------------------------

   procedure Tree_Model_Ref_Node
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter       : Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Iter       : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_ref_node");
   begin
      Internal (Get_Object (Tree_Model),
                Iter'Address);
   end Tree_Model_Ref_Node;

   ---------------------------
   -- Tree_Model_Unref_Node --
   ---------------------------

   procedure Tree_Model_Unref_Node
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter       : Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Iter       : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_unref_node");
   begin
      Internal (Get_Object (Tree_Model),
                Iter'Address);
   end Tree_Model_Unref_Node;

   ----------------
   -- Model_Data --
   ----------------

   package body Model_Data is

      function Get
        (Tree_Model : access Gtk_Tree_Model_Record'Class;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint)
        return Data_Type
      is
         procedure Internal
           (Tree_Model : System.Address;
            Iter       : System.Address;
            Column     : Gint;
            Value      : System.Address);
         pragma Import (C, Internal, "ada_gtk_tree_model_get");

         Value : Data_Access := new Data_Type;
      begin
         Internal (Get_Object (Tree_Model),
                   Iter'Address,
                   Column,
                   Value.all'Address);
         return Value.all;
      end Get;

   end Model_Data;

--    ---------------------------
--    -- Tree_Model_Get_Valist --
--    ---------------------------

--    procedure Tree_Model_Get_Valist
--      (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
--       Iter       : Gtk_Tree_Iter;
--       Var_Args   : va_list)
--    is
--       procedure Internal
--         (Tree_Model : System.Address;
--          Iter       : System.Address;
--          Var_Args   : Gint);
--       pragma Import (C, Internal, "gtk_tree_model_get_valist");
--    begin
--       Internal (Get_Object (Tree_Model),
--                 Iter'Address,
--                 va_list'Pos (Var_Args));
--    end Tree_Model_Get_Valist;

--    ------------------------
--    -- Tree_Model_Foreach --
--    ------------------------

--    procedure Tree_Model_Foreach
--      (Model     : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
--       Func      : Gtk_Tree_Model_Foreach_Func;
--       User_Data : gpointer)
--    is
--       procedure Internal
--         (Model     : System.Address;
--          Func      : Gint;
--          User_Data : Integer);
--       pragma Import (C, Internal, "gtk_tree_model_foreach");
--    begin
--       Internal (Get_Object (Model),
--                 Gtk_Tree_Model_Foreach_Func'Pos (Func),
--                 User_Data);
--    end Tree_Model_Foreach;

   ----------------------------
   -- Tree_Model_Row_Changed --
   ----------------------------

   procedure Tree_Model_Row_Changed
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Path       : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Path       : System.Address;
         Iter       : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_row_changed");
   begin
      Internal (Get_Object (Tree_Model),
                Path.all'Address,
                Iter'Address);
   end Tree_Model_Row_Changed;

   -----------------------------
   -- Tree_Model_Row_Inserted --
   -----------------------------

   procedure Tree_Model_Row_Inserted
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Path       : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Path       : System.Address;
         Iter       : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_row_inserted");
   begin
      Internal (Get_Object (Tree_Model),
                Path.all'Address,
                Iter'Address);
   end Tree_Model_Row_Inserted;

   --------------------------------------
   -- Tree_Model_Row_Has_Child_Toggled --
   --------------------------------------

   procedure Tree_Model_Row_Has_Child_Toggled
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Path       : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Path       : System.Address;
         Iter       : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_row_has_child_toggled");
   begin
      Internal (Get_Object (Tree_Model),
                Path.all'Address,
                Iter'Address);
   end Tree_Model_Row_Has_Child_Toggled;

   ----------------------------
   -- Tree_Model_Row_Deleted --
   ----------------------------

   procedure Tree_Model_Row_Deleted
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Path       : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Path       : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_row_deleted");
   begin
      Internal (Get_Object (Tree_Model),
                Path.all'Address);
   end Tree_Model_Row_Deleted;

   -------------------------------
   -- Tree_Model_Rows_Reordered --
   -------------------------------

   procedure Tree_Model_Rows_Reordered
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Path       : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter;
      New_Order  : out Gint)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Path       : System.Address;
         Iter       : System.Address;
         New_Order  : out Gint);
      pragma Import (C, Internal, "gtk_tree_model_rows_reordered");
   begin
      Internal (Get_Object (Tree_Model),
                Path.all'Address,
                Iter'Address,
                New_Order);
   end Tree_Model_Rows_Reordered;

end Gtk.Tree_Model;
