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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Glib.Object;           use Glib.Object;
with Gtk;                   use Gtk;
with Interfaces.C.Strings;
with System;                use System;
with Unchecked_Conversion;

package body Gtk.Tree_Model is

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags (Model : access Gtk_Tree_Model_Record)
      return Tree_Model_Flags
   is
      function Internal (M : System.Address) return Tree_Model_Flags;
      pragma Import (C, Internal, "gtk_tree_model_get_flags");
   begin
      return Internal (Get_Object (Model));
   end Get_Flags;

   -------------
   -- Gtk_New --
   -------------

   function Gtk_New (Path : String := "") return Gtk_Tree_Path is
      function Internal1 (Path   : String) return Gtk_Tree_Path;
      pragma Import (C, Internal1, "gtk_tree_path_new_from_string");

      function Internal2 return Gtk_Tree_Path;
      pragma Import (C, Internal2, "gtk_tree_path_new");
   begin
      if Path = "" then
         return Internal2;
      else
         return Internal1 (Path & ASCII.NUL);
      end if;
   end Gtk_New;

   ---------------
   -- To_String --
   ---------------

   function To_String (Path : Gtk_Tree_Path) return String is
      function Internal (Path   : Gtk_Tree_Path) return chars_ptr;
      pragma Import (C, Internal, "gtk_tree_path_to_string");
   begin
      return Interfaces.C.Strings.Value (Internal (Path));
   end To_String;

   -----------------
   -- Get_Indices --
   -----------------

   function Get_Indices (Path : Gtk_Tree_Path) return Gint_Array is
      type Big_Gint_Array is array (Natural) of Gint;
      type Big_Gint_Array_Access is access Big_Gint_Array;
      function To_Big_Gint is new Unchecked_Conversion
        (System.Address, Big_Gint_Array_Access);

      function Internal (Path : Gtk_Tree_Path) return System.Address;
      pragma Import (C, Internal, "gtk_tree_path_get_indices");

      Result : Gint_Array (0 .. Integer (Get_Depth (Path)) - 1);
      R      : Big_Gint_Array_Access := To_Big_Gint (Internal (Path));
   begin
      for J in Result'Range loop
         Result (J) := R (J);
      end loop;
      return Result;
   end Get_Indices;

   ----------
   -- Prev --
   ----------

   function Prev (Path : Gtk_Tree_Path) return Boolean is
      function Internal (Path : Gtk_Tree_Path) return Gint;
      pragma Import (C, Internal, "gtk_tree_path_prev");
   begin
      return Internal (Path) /= 0;
   end Prev;

   --------
   -- Up --
   --------

   function Up (Path : Gtk_Tree_Path) return Boolean is
      function Internal (Path : Gtk_Tree_Path) return Gint;
      pragma Import (C, Internal, "gtk_tree_path_up");
   begin
      return Internal (Path) /= 0;
   end Up;

   -----------------
   -- Is_Ancestor --
   -----------------

   function Is_Ancestor (Path, Descendant : Gtk_Tree_Path) return Boolean is
      function Internal (Path, Descendant : Gtk_Tree_Path) return Gint;
      pragma Import (C, Internal, "gtk_tree_path_is_ancestor");
   begin
      return Internal (Path, Descendant) /= 0;
   end Is_Ancestor;

   -------------------
   -- Is_Descendant --
   -------------------

   function Is_Descendant (Path, Ancestor : Gtk_Tree_Path) return Boolean is
      function Internal (Path, Ancestor : Gtk_Tree_Path) return Gint;
      pragma Import (C, Internal, "gtk_tree_path_is_descendant");
   begin
      return Internal (Path, Ancestor) /= 0;
   end Is_Descendant;

   -------------
   -- Gtk_New --
   -------------

   function Gtk_New
     (Model : access Gtk_Tree_Model_Record;  Path : Gtk_Tree_Path)
      return Gtk_Tree_Row_Reference
   is
      function Internal
        (Model  : System.Address; Path : Gtk_Tree_Path)
         return Gtk_Tree_Row_Reference;
      pragma Import (C, Internal, "gtk_tree_row_reference_new");
   begin
      return Internal (Get_Object (Model), Path);
   end Gtk_New;

   -----------
   -- Valid --
   -----------

   function Valid (Reference : Gtk_Tree_Row_Reference) return Boolean is
      function Internal (Reference : Gtk_Tree_Row_Reference) return Gint;
      pragma Import (C, Internal, "gtk_tree_row_reference_valid");
   begin
      return Internal (Reference) /= 0;
   end Valid;

   -------------------
   -- Get_N_Columns --
   -------------------

   function Get_N_Columns
     (Tree_Model : access Gtk_Tree_Model_Record) return Gint
   is
      function Internal (Tree_Model : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_get_n_columns");
   begin
      return Internal (Get_Object (Tree_Model));
   end Get_N_Columns;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   function Get_Column_Type
     (Tree_Model : access Gtk_Tree_Model_Record; Index : Gint) return GType
   is
      function Internal (Tree_Model : System.Address; Index : Gint)
         return GType;
      pragma Import (C, Internal, "gtk_tree_model_get_column_type");
   begin
      return Internal (Get_Object (Tree_Model), Index);
   end Get_Column_Type;

   --------------
   -- Get_Iter --
   --------------

   function Get_Iter
     (Tree_Model : access Gtk_Tree_Model_Record;
      Path       : Gtk_Tree_Path) return Gtk_Tree_Iter
   is
      function Internal
        (Tree_Model, Iter : System.Address; Path : Gtk_Tree_Path)
         return Gint;
      pragma Import (C, Internal, "gtk_tree_model_get_iter");
      Iter : aliased Gtk_Tree_Iter;
   begin
      if Internal (Get_Object (Tree_Model), Iter'Address, Path) /= 0 then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Get_Iter;

   --------------------------
   -- Get_Iter_From_String --
   --------------------------

   function Get_Iter_From_String
     (Tree_Model  : access Gtk_Tree_Model_Record; Path_String : String)
      return Gtk_Tree_Iter
   is
      function Internal
        (Tree_Model, Iter  : System.Address; Str : String) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_get_iter_from_string");
      Iter : aliased Gtk_Tree_Iter;
   begin
      if Internal
        (Get_Object (Tree_Model), Iter'Address, Path_String & ASCII.NUL) /= 0
      then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Get_Iter_From_String;

   -------------------
   -- Get_Iter_Root --
   -------------------

   function Get_Iter_Root (Tree_Model : access Gtk_Tree_Model_Record)
     return Gtk_Tree_Iter
   is
      function Internal (Tree_Model, Iter : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_get_iter_root");
      Iter : aliased Gtk_Tree_Iter;
   begin
      if Internal (Get_Object (Tree_Model), Iter'Address) /= 0 then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Get_Iter_Root;

   --------------
   -- Get_Path --
   --------------

   function Get_Path
     (Tree_Model : access Gtk_Tree_Model_Record; Iter : Gtk_Tree_Iter)
      return Gtk_Tree_Path
   is
      function Internal (Tree_Model : System.Address; Iter : Gtk_Tree_Iter)
         return Gtk_Tree_Path;
      pragma Import (C, Internal, "gtk_tree_model_get_path");
   begin
      return Internal (Get_Object (Tree_Model), Iter);
   end Get_Path;

   ---------------
   -- Get_Value --
   ---------------

   procedure Get_Value
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record;
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
      Internal (Get_Object (Tree_Model), Iter'Address, Column, Value);
   end Get_Value;

   ----------
   -- Next --
   ----------

   procedure Next
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record;
      Iter       : in out Gtk_Tree_Iter)
   is
      function Internal (Tree_Model, Iter : System.Address)
         return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_next");
   begin
      if Internal (Get_Object (Tree_Model), Iter'Address) = 0 then
         Iter := Null_Iter;
      end if;
   end Next;

   --------------
   -- Children --
   --------------

   function Children
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record;
      Parent     : Gtk_Tree_Iter) return Gtk_Tree_Iter
   is
      function Internal
        (Tree_Model, Iter, Parent : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_children");
      Iter : aliased Gtk_Tree_Iter;
   begin
      if Internal
        (Get_Object (Tree_Model), Iter'Address, Parent'Address) /= 0
      then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Children;

   ---------------
   -- Has_Child --
   ---------------

   function Has_Child
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter) return Boolean
   is
      function Internal (Tree_Model, Iter : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_has_child");
   begin
      return Internal (Get_Object (Tree_Model), Iter'Address) /= 0;
   end Has_Child;

   ----------------
   -- N_Children --
   ----------------

   function N_Children
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter := Null_Iter) return Gint
   is
      function Internal (Tree_Model, Iter : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_n_children");
   begin
      if Iter = Null_Iter then
         return Internal (Get_Object (Tree_Model), System.Null_Address);
      else
         return Internal (Get_Object (Tree_Model), Iter'Address);
      end if;
   end N_Children;

   ---------------
   -- Nth_Child --
   ---------------

   function Nth_Child
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record;
      Parent     : Gtk_Tree_Iter;
      N          : Gint) return Gtk_Tree_Iter
   is
      function Internal
        (Tree_Model, Iter, Parent : System.Address; N : Gint)
         return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_nth_child");
      Iter : aliased Gtk_Tree_Iter;
   begin
      if Internal
        (Get_Object (Tree_Model), Iter'Address, Parent'Address, N) /= 0
      then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Nth_Child;

   ------------
   -- Parent --
   ------------

   function Parent
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record;
      Child      : Gtk_Tree_Iter) return Gtk_Tree_Iter
   is
      function Internal
        (Tree_Model, Iter, Child : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_parent");
      Iter : aliased Gtk_Tree_Iter;
   begin
      if Internal
        (Get_Object (Tree_Model), Iter'Address, Child'Address) /= 0
      then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Parent;

   --------------
   -- Ref_Node --
   --------------

   procedure Ref_Node
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter)
   is
      procedure Internal (Tree_Model, Iter : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_ref_node");
   begin
      Internal (Get_Object (Tree_Model), Iter'Address);
   end Ref_Node;

   ----------------
   -- Unref_Node --
   ----------------

   procedure Unref_Node
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter)
   is
      procedure Internal (Tree_Model, Iter : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_unref_node");
   begin
      Internal (Get_Object (Tree_Model), Iter'Address);
   end Unref_Node;

   ----------------
   -- Model_Data --
   ----------------

   function Model_Data_Get
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint)
      return Data_Type
   is
      procedure Internal
        (Tree_Model : System.Address;
         Iter       : System.Address;
         Column     : Gint;
         Value      : System.Address;
         Final      : Gint := -1);
      pragma Import (C, Internal, "gtk_tree_model_get");

      Value : aliased Data_Type;

      --  Packing is used to compensate for possible different representation
      --  between Ada and C, for instance for booleans. We can't apply a
      --  convention to Data_Type.
      Packing : array (1 .. 10) of Character;
      pragma Warnings (Off, Packing);
   begin
      Internal (Get_Object (Tree_Model), Iter'Address, Column, Value'Address);
      return Value;
   end Model_Data_Get;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint)
     return String
   is
      procedure Internal
        (Tree_Model : System.Address;
         Iter       : System.Address;
         Column     : Gint;
         Value      : System.Address;
         Final      : Gint := -1);
      pragma Import (C, Internal, "gtk_tree_model_get");
      A : chars_ptr;
   begin
      Internal (Get_Object (Tree_Model),
                Iter'Address,
                Column,
                A'Address);

      if A = Null_Ptr then
         return "";
      else
         declare
            Result : constant String := Value (A);
         begin
            Free (A);
            return Result;
         end;
      end if;
   end Get_String;

   -----------------
   -- Row_Changed --
   -----------------

   procedure Row_Changed
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Path       : Gtk_Tree_Path;
         Iter       : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_row_changed");
   begin
      Internal (Get_Object (Tree_Model), Path, Iter'Address);
   end Row_Changed;

   ------------------
   -- Row_Inserted --
   ------------------

   procedure Row_Inserted
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Path       : Gtk_Tree_Path;
         Iter       : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_row_inserted");
   begin
      Internal (Get_Object (Tree_Model), Path, Iter'Address);
   end Row_Inserted;

   ---------------------------
   -- Row_Has_Child_Toggled --
   ---------------------------

   procedure Row_Has_Child_Toggled
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Path       : Gtk_Tree_Path;
         Iter       : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_row_has_child_toggled");
   begin
      Internal (Get_Object (Tree_Model), Path, Iter'Address);
   end Row_Has_Child_Toggled;

   -----------------
   -- Row_Deleted --
   -----------------

   procedure Row_Deleted
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Path       : Gtk_Tree_Path);
      pragma Import (C, Internal, "gtk_tree_model_row_deleted");
   begin
      Internal (Get_Object (Tree_Model), Path);
   end Row_Deleted;

   --------------------
   -- Rows_Reordered --
   --------------------

   procedure Rows_Reordered
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter;
      New_Order  : Gint_Array)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Path       : Gtk_Tree_Path;
         Iter       : System.Address;
         New_Order  : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_rows_reordered");
   begin
      Internal
        (Get_Object (Tree_Model), Path, Iter'Address, New_Order'Address);
   end Rows_Reordered;

end Gtk.Tree_Model;
