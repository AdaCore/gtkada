------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
with Gtkada.Bindings;          use Gtkada.Bindings;
with Interfaces.C.Strings;     use Interfaces.C.Strings;

package body Gtk.Tree_Model is

   function Get_Int
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Gint
   is
      procedure Internal
        (Tree_Model : Gtk_Tree_Model;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint;
         Value      : out Gint;
         Dummy      : Gint := -1);
      pragma Import (C, Internal, "gtk_tree_model_get");
      A : Gint;
   begin
      Internal (Tree_Model, Iter, Column, A);
      return A;
   end Get_Int;

   function Get_Boolean
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Boolean is
   begin
      return Get_Int (Tree_Model, Iter, Column) /= 0;
   end Get_Boolean;

   function Get_Object
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Glib.Object.GObject
   is
      Stub : Glib.Object.GObject_Record;
   begin
      return Get_User_Data
        (Get_Address (Tree_Model, Iter, Column), Stub);
   end Get_Object;

   function Get_C_Proxy
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Glib.C_Proxy
   is
      procedure Internal
        (Tree_Model : Gtk_Tree_Model;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint;
         Value      : out Glib.C_Proxy;
         Dummy      : Gint := -1);
      pragma Import (C, Internal, "gtk_tree_model_get");
      A : Glib.C_Proxy;
   begin
      Internal (Tree_Model, Iter, Column, A);
      return A;
   end Get_C_Proxy;

   function Get_String
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return UTF8_String
   is
      procedure Internal
        (Tree_Model : Gtk_Tree_Model;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint;
         Value      : out Interfaces.C.Strings.chars_ptr;
         Dummy      : Gint := -1);
      pragma Import (C, Internal, "gtk_tree_model_get");
      A : Interfaces.C.Strings.chars_ptr;
   begin
      Internal (Tree_Model, Iter, Column, A);
      if A = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         declare
            S : constant String := Interfaces.C.Strings.Value (A);
         begin
            Interfaces.C.Strings.Free (A);
            return S;
         end;
      end if;
   end Get_String;

   function Get_Address
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return System.Address
   is
      procedure Internal
        (Tree_Model : Gtk_Tree_Model;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint;
         Value      : out System.Address;
         Dummy      : Gint := -1);
      pragma Import (C, Internal, "gtk_tree_model_get");
      A : System.Address;
   begin
      Internal (Tree_Model, Iter, Column, A);
      return A;
   end Get_Address;

   function From_Object_Free (B : access Gtk_Tree_Iter) return Gtk_Tree_Iter is
      Result : constant Gtk_Tree_Iter := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function "=" (Left : Gtk_Tree_Iter; Right : Gtk_Tree_Iter) return Boolean is
      use type System.Address;
   begin
      if Left.Stamp = Right.Stamp then
         if Left.Stamp = 0 then
            --  Stamp = 0 means the iterator is null iterator, we need not to
            --  compare other fields in this case.
            return True;
         else
            return Left.User_Data = Right.User_Data
            and then Left.User_Data2 = Right.User_Data2
            and then Left.User_Data3 = Right.User_Data3;
         end if;
      else
         return False;
      end if;
   end "=";

   procedure Get_Tree_Iter
     (Val  : Glib.Values.GValue;
      Iter : out Gtk_Tree_Iter)
   is
      function Internal (Source : System.Address) return Gtk_Tree_Iter;
      pragma Import (C, Internal, "gtk_tree_iter_copy");
   begin
      Iter := Internal (Glib.Values.Get_Address (Val));
   end Get_Tree_Iter;

   function Get_Tree_Iter (Val : Glib.Values.GValue) return Gtk_Tree_Iter is
      Result : Gtk_Tree_Iter;
   begin
      Get_Tree_Iter (Val, Result);
      return Result;
   end Get_Tree_Iter;

   function To_Address (Iter : Gtk_Tree_Iter) return System.Address is
   begin
      return Iter'Address;
   end To_Address;

   function From_Object_Free
     (B : access Gtk_Tree_Path'Class) return Gtk_Tree_Path
   is
      Result : constant Gtk_Tree_Path := Gtk_Tree_Path (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gtk_Tree_Path is
      S : Gtk_Tree_Path;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   function Get_Tree_Path (Val : Glib.Values.GValue) return Gtk_Tree_Path is
   begin
      return From_Object (Glib.Values.Get_Address (Val));
   end Get_Tree_Path;

   procedure C_Gtk_Tree_Model_Foreach
      (Tree_Model : Gtk_Tree_Model;
       Func       : System.Address;
       User_Data  : System.Address);
   pragma Import (C, C_Gtk_Tree_Model_Foreach, "gtk_tree_model_foreach");
   --  Calls func on each node in model in a depth-first fashion.
   --  If Func returns True, then the tree ceases to be walked, and
   --  Gtk.Tree_Model.Foreach returns.
   --  "func": a function to be called on each row
   --  "user_data": user data to passed to Func

   function To_Gtk_Tree_Model_Foreach_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_Model_Foreach_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Tree_Model_Foreach_Func, System.Address);

   function Internal_Gtk_Tree_Model_Foreach_Func
      (Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Path  : System.Address;
       Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
       Data  : System.Address) return Integer;
   pragma Convention (C, Internal_Gtk_Tree_Model_Foreach_Func);
   --  "model": the Gtk.Tree_Model.Gtk_Tree_Model being iterated
   --  "path": the current Gtk.Tree_Model.Gtk_Tree_Path
   --  "iter": the current Gtk.Tree_Model.Gtk_Tree_Iter
   --  "data": The user data passed to Gtk.Tree_Model.Foreach

   ------------------------------------------
   -- Internal_Gtk_Tree_Model_Foreach_Func --
   ------------------------------------------

   function Internal_Gtk_Tree_Model_Foreach_Func
      (Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Path  : System.Address;
       Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
       Data  : System.Address) return Integer
   is
      Func : constant Gtk_Tree_Model_Foreach_Func := To_Gtk_Tree_Model_Foreach_Func (Data);
   begin
      return Boolean'Pos (Func (Model, From_Object (Path), Iter));
   end Internal_Gtk_Tree_Model_Foreach_Func;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Path : out Gtk_Tree_Path) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tree_path_new");
   begin
      Path.Set_Object (Internal);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Tree_Path; Path : UTF8_String) is
      function Internal
         (Path : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_tree_path_new_from_string");
      Tmp_Path   : Interfaces.C.Strings.chars_ptr := New_String (Path);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Path);
      Free (Tmp_Path);
      Self.Set_Object (Tmp_Return);
   end Gtk_New;

   -------------------
   -- Gtk_New_First --
   -------------------

   procedure Gtk_New_First (Path : out Gtk_Tree_Path) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tree_path_new_first");
   begin
      Path.Set_Object (Internal);
   end Gtk_New_First;

   ------------------
   -- Append_Index --
   ------------------

   procedure Append_Index (Path : Gtk_Tree_Path; Index : Gint) is
      procedure Internal (Path : System.Address; Index : Gint);
      pragma Import (C, Internal, "gtk_tree_path_append_index");
   begin
      Internal (Get_Object (Path), Index);
   end Append_Index;

   -------------
   -- Compare --
   -------------

   function Compare (Path : Gtk_Tree_Path; B : Gtk_Tree_Path) return Gint is
      function Internal
         (Path : System.Address;
          B    : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_path_compare");
   begin
      return Internal (Get_Object (Path), Get_Object (B));
   end Compare;

   ----------
   -- Copy --
   ----------

   function Copy (Path : Gtk_Tree_Path) return Gtk_Tree_Path is
      function Internal (Path : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_path_copy");
   begin
      return From_Object (Internal (Get_Object (Path)));
   end Copy;

   ----------
   -- Down --
   ----------

   procedure Down (Path : Gtk_Tree_Path) is
      procedure Internal (Path : System.Address);
      pragma Import (C, Internal, "gtk_tree_path_down");
   begin
      Internal (Get_Object (Path));
   end Down;

   -------------
   -- Foreach --
   -------------

   procedure Foreach
      (Tree_Model : Gtk_Tree_Model;
       Func       : Gtk_Tree_Model_Foreach_Func)
   is
   begin
      if Func = null then
         C_Gtk_Tree_Model_Foreach (Tree_Model, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Tree_Model_Foreach (Tree_Model, Internal_Gtk_Tree_Model_Foreach_Func'Address, To_Address (Func));
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
          Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
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
         (Tree_Model : Gtk.Tree_Model.Gtk_Tree_Model;
          Func       : Gtk_Tree_Model_Foreach_Func;
          User_Data  : User_Data_Type)
      is
      begin
         if Func = null then
            C_Gtk_Tree_Model_Foreach (Tree_Model, System.Null_Address, System.Null_Address);
         else
            C_Gtk_Tree_Model_Foreach (Tree_Model, Internal_Cb'Address, Users.Build (To_Address (Func), User_Data));
         end if;
      end Foreach;

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Model : Gtk.Tree_Model.Gtk_Tree_Model;
          Path  : System.Address;
          Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
          Data  : System.Address) return Integer
      is
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         return Boolean'Pos (To_Gtk_Tree_Model_Foreach_Func (D.Func) (Model, From_Object (Path), Iter, D.Data.all));
      end Internal_Cb;

   end Foreach_User_Data;

   ---------------
   -- Get_Depth --
   ---------------

   function Get_Depth (Path : Gtk_Tree_Path) return Gint is
      function Internal (Path : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_path_get_depth");
   begin
      return Internal (Get_Object (Path));
   end Get_Depth;

   -----------------
   -- Get_Indices --
   -----------------

   function Get_Indices (Path : Gtk_Tree_Path) return Glib.Gint_Array is
      Depth : constant Integer := Integer (Get_Depth (Path));

      subtype Result_Array is Gint_Array (0 .. Depth - 1);
      type Result_Array_Access is access all Result_Array;
      pragma Convention (C, Result_Array_Access);

      function Internal (Path : System.Address) return Result_Array_Access;
      pragma Import (C, Internal, "gtk_tree_path_get_indices");

   begin
      --  Do not free the result of gtk_tree_path_get_indices since this is
      --  not a copy, but the currently used data.

      return Internal (Get_Object (Path)).all;
   end Get_Indices;

   --------------
   -- Get_Iter --
   --------------

   function Get_Iter
      (Tree_Model : Gtk_Tree_Model;
       Path       : Gtk_Tree_Path) return Gtk_Tree_Iter
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
      (Tree_Model : Gtk_Tree_Model) return Gtk_Tree_Iter
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
      (Tree_Model  : Gtk_Tree_Model;
       Path_String : UTF8_String) return Gtk_Tree_Iter
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

   --------------
   -- Get_Path --
   --------------

   function Get_Path
      (Tree_Model : Gtk_Tree_Model;
       Iter       : Gtk_Tree_Iter) return Gtk_Tree_Path
   is
      function Internal
         (Tree_Model : Gtk_Tree_Model;
          Iter       : Gtk_Tree_Iter) return System.Address;
      pragma Import (C, Internal, "gtk_tree_model_get_path");
   begin
      return From_Object (Internal (Tree_Model, Iter));
   end Get_Path;

   --------------------------
   -- Get_String_From_Iter --
   --------------------------

   function Get_String_From_Iter
      (Tree_Model : Gtk_Tree_Model;
       Iter       : Gtk_Tree_Iter) return UTF8_String
   is
      function Internal
         (Tree_Model : Gtk_Tree_Model;
          Iter       : Gtk_Tree_Iter) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_tree_model_get_string_from_iter");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Tree_Model, Iter));
   end Get_String_From_Iter;

   ---------------
   -- Has_Child --
   ---------------

   function Has_Child
      (Tree_Model : Gtk_Tree_Model;
       Iter       : Gtk_Tree_Iter) return Boolean
   is
      function Internal
         (Tree_Model : Gtk_Tree_Model;
          Iter       : Gtk_Tree_Iter) return Integer;
      pragma Import (C, Internal, "gtk_tree_model_iter_has_child");
   begin
      return Boolean'Val (Internal (Tree_Model, Iter));
   end Has_Child;

   -----------------
   -- Is_Ancestor --
   -----------------

   function Is_Ancestor
      (Path       : Gtk_Tree_Path;
       Descendant : Gtk_Tree_Path) return Boolean
   is
      function Internal
         (Path       : System.Address;
          Descendant : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_tree_path_is_ancestor");
   begin
      return Boolean'Val (Internal (Get_Object (Path), Get_Object (Descendant)));
   end Is_Ancestor;

   -------------------
   -- Is_Descendant --
   -------------------

   function Is_Descendant
      (Path     : Gtk_Tree_Path;
       Ancestor : Gtk_Tree_Path) return Boolean
   is
      function Internal
         (Path     : System.Address;
          Ancestor : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_tree_path_is_descendant");
   begin
      return Boolean'Val (Internal (Get_Object (Path), Get_Object (Ancestor)));
   end Is_Descendant;

   ----------
   -- Next --
   ----------

   procedure Next (Path : Gtk_Tree_Path) is
      procedure Internal (Path : System.Address);
      pragma Import (C, Internal, "gtk_tree_path_next");
   begin
      Internal (Get_Object (Path));
   end Next;

   ---------------
   -- Nth_Child --
   ---------------

   function Nth_Child
      (Tree_Model : Gtk_Tree_Model;
       Parent     : Gtk_Tree_Iter;
       N          : Gint) return Gtk_Tree_Iter
   is
      function Internal
        (Tree_Model : Gtk_Tree_Model;
         Iter, Parent : System.Address; N : Gint) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_nth_child");
      Iter : aliased Gtk_Tree_Iter;
      P    : System.Address := System.Null_Address;
   begin
      if Parent /= Null_Iter then
         P := Parent'Address;
      end if;
      if Internal (+Tree_Model, Iter'Address, P, N) /= 0 then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Nth_Child;

   ------------
   -- Parent --
   ------------

   function Parent
      (Tree_Model : Gtk_Tree_Model;
       Child      : Gtk_Tree_Iter) return Gtk_Tree_Iter
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

   ---------------
   -- Path_Free --
   ---------------

   procedure Path_Free (Path : Gtk_Tree_Path) is
      procedure Internal (Path : System.Address);
      pragma Import (C, Internal, "gtk_tree_path_free");
   begin
      Internal (Get_Object (Path));
   end Path_Free;

   -------------------
   -- Prepend_Index --
   -------------------

   procedure Prepend_Index (Path : Gtk_Tree_Path; Index : Gint) is
      procedure Internal (Path : System.Address; Index : Gint);
      pragma Import (C, Internal, "gtk_tree_path_prepend_index");
   begin
      Internal (Get_Object (Path), Index);
   end Prepend_Index;

   ----------
   -- Prev --
   ----------

   function Prev (Path : Gtk_Tree_Path) return Boolean is
      function Internal (Path : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_tree_path_prev");
   begin
      return Boolean'Val (Internal (Get_Object (Path)));
   end Prev;

   -----------------
   -- Row_Changed --
   -----------------

   procedure Row_Changed
      (Tree_Model : Gtk_Tree_Model;
       Path       : Gtk_Tree_Path;
       Iter       : Gtk_Tree_Iter)
   is
      procedure Internal
         (Tree_Model : Gtk_Tree_Model;
          Path       : System.Address;
          Iter       : Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_model_row_changed");
   begin
      Internal (Tree_Model, Get_Object (Path), Iter);
   end Row_Changed;

   -----------------
   -- Row_Deleted --
   -----------------

   procedure Row_Deleted (Tree_Model : Gtk_Tree_Model; Path : Gtk_Tree_Path) is
      procedure Internal
         (Tree_Model : Gtk_Tree_Model;
          Path       : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_row_deleted");
   begin
      Internal (Tree_Model, Get_Object (Path));
   end Row_Deleted;

   ---------------------------
   -- Row_Has_Child_Toggled --
   ---------------------------

   procedure Row_Has_Child_Toggled
      (Tree_Model : Gtk_Tree_Model;
       Path       : Gtk_Tree_Path;
       Iter       : Gtk_Tree_Iter)
   is
      procedure Internal
         (Tree_Model : Gtk_Tree_Model;
          Path       : System.Address;
          Iter       : Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_model_row_has_child_toggled");
   begin
      Internal (Tree_Model, Get_Object (Path), Iter);
   end Row_Has_Child_Toggled;

   ------------------
   -- Row_Inserted --
   ------------------

   procedure Row_Inserted
      (Tree_Model : Gtk_Tree_Model;
       Path       : Gtk_Tree_Path;
       Iter       : Gtk_Tree_Iter)
   is
      procedure Internal
         (Tree_Model : Gtk_Tree_Model;
          Path       : System.Address;
          Iter       : Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_model_row_inserted");
   begin
      Internal (Tree_Model, Get_Object (Path), Iter);
   end Row_Inserted;

   --------------------
   -- Rows_Reordered --
   --------------------

   procedure Rows_Reordered
      (Tree_Model : Gtk_Tree_Model;
       Path       : Gtk_Tree_Path;
       Iter       : Gtk_Tree_Iter;
       New_Order  : in out Gint)
   is
      procedure Internal
         (Tree_Model : Gtk_Tree_Model;
          Path       : System.Address;
          Iter       : Gtk_Tree_Iter;
          New_Order  : in out Gint);
      pragma Import (C, Internal, "gtk_tree_model_rows_reordered");
   begin
      Internal (Tree_Model, Get_Object (Path), Iter, New_Order);
   end Rows_Reordered;

   ---------------
   -- To_String --
   ---------------

   function To_String (Path : Gtk_Tree_Path) return UTF8_String is
      function Internal
         (Path : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_tree_path_to_string");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Path)));
   end To_String;

   --------
   -- Up --
   --------

   function Up (Path : Gtk_Tree_Path) return Boolean is
      function Internal (Path : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_tree_path_up");
   begin
      return Boolean'Val (Internal (Get_Object (Path)));
   end Up;

   function "+" (W : Gtk_Tree_Model) return Gtk_Tree_Model is
   begin
      return W;
   end "+";

   function Convert (R : Gtk.Tree_Model.Gtk_Tree_Path) return System.Address is
   begin
      return Get_Object (R);
   end Convert;

   function Convert (R : System.Address) return Gtk.Tree_Model.Gtk_Tree_Path is
   begin
      return From_Object(R);
   end Convert;

end Gtk.Tree_Model;
