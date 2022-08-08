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
with Gtk.Arguments;            use Gtk.Arguments;
with Gtkada.Bindings;          use Gtkada.Bindings;
with Gtkada.Types;             use Gtkada.Types;

package body Gtk.Tree_Model is

   function Get_Int
     (Tree_Model : access Gtk_Root_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Gint is
   begin
      return Get_Int (To_Interface (Tree_Model), Iter, Column);
   end Get_Int;

   function Get_Int
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Gint
   is
      procedure Internal
        (Tree_Model : Gtk_Tree_Model;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint;
         Value      : out Gint);
      pragma Import (C, Internal, "ada_gtk_tree_model_get");
      A : Gint;
   begin
      if Iter = Null_Iter then
         raise Program_Error with "Get_Int with null_iter";
      end if;
      Internal (Tree_Model, Iter, Column, A);
      return A;
   end Get_Int;

   function Get_Ulong
     (Tree_Model : access Gtk_Root_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Gulong is
   begin
      return Get_Ulong (To_Interface (Tree_Model), Iter, Column);
   end Get_Ulong;

   function Get_Ulong
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Gulong
   is
      procedure Internal
        (Tree_Model : Gtk_Tree_Model;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint;
         Value      : out Gulong);
      pragma Import (C, Internal, "ada_gtk_tree_model_get");
      A : Gulong;
   begin
      if Iter = Null_Iter then
         raise Program_Error with "Get_Ulong with null_iter";
      end if;
      Internal (Tree_Model, Iter, Column, A);
      return A;
   end Get_Ulong;

   function Get_Boolean
     (Tree_Model : access Gtk_Root_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Boolean is
   begin
      return Get_Boolean (To_Interface (Tree_Model), Iter, Column);
   end Get_Boolean;

   function Get_Boolean
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Boolean is
   begin
      return Get_Int (Tree_Model, Iter, Column) /= 0;
   end Get_Boolean;

   function Get_Object
     (Tree_Model : access Gtk_Root_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Glib.Object.GObject
   is
   begin
      return Get_Object (To_Interface (Tree_Model), Iter, Column);
   end Get_Object;

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
     (Tree_Model : access Gtk_Root_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Glib.C_Proxy is
   begin
      return Get_C_Proxy (To_Interface (Tree_Model), Iter, Column);
   end Get_C_Proxy;

   function Get_C_Proxy
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Glib.C_Proxy
   is
      procedure Internal
        (Tree_Model : Gtk_Tree_Model;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint;
         Value      : out Glib.C_Proxy);
      pragma Import (C, Internal, "ada_gtk_tree_model_get");
      A : Glib.C_Proxy;
   begin
      if Iter = Null_Iter then
         raise Program_Error with "Get_C_Proxy with null_iter";
      end if;
      Internal (Tree_Model, Iter, Column, A);
      return A;
   end Get_C_Proxy;

   function Get_String
     (Tree_Model : access Gtk_Root_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return UTF8_String
   is
   begin
      return Get_String (To_Interface (Tree_Model), Iter, Column);
   end Get_String;

   function Get_String
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return UTF8_String
   is
      procedure Internal
        (Tree_Model : Gtk_Tree_Model;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint;
         Value      : out Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "ada_gtk_tree_model_get");
      A : Gtkada.Types.Chars_Ptr;
   begin
      if Iter = Null_Iter then
         raise Program_Error with "Get_String with null_iter";
      end if;
      Internal (Tree_Model, Iter, Column, A);
      if A = Gtkada.Types.Null_Ptr then
         return "";
      else
         declare
            S : constant String := Gtkada.Types.Value (A);

            procedure C_Free (S : Gtkada.Types.Chars_Ptr);
            pragma Import (C, C_Free, "free");

         begin
            --  Since A was allocated by gtk+ via malloc(), and not via
            --  System.Memory, we should not be using Gtkada.Types.g_free
            --  which goes through System.Memory. So we call free() directly
            --  instead.
            C_Free (A);
            return S;
         end;
      end if;
   end Get_String;

   function Get_Address
     (Tree_Model : access Gtk_Root_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return System.Address
   is
   begin
      return Get_Address (To_Interface (Tree_Model), Iter, Column);
   end Get_Address;

   function Get_Address
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return System.Address
   is
      procedure Internal
        (Tree_Model : Gtk_Tree_Model;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint;
         Value      : out System.Address);
      pragma Import (C, Internal, "ada_gtk_tree_model_get");
      A : System.Address;
   begin
      if Iter = Null_Iter then
         raise Program_Error with "Get_Address with null_iter";
      end if;
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

   type Gtk_Tree_Iter_Access is access Gtk_Tree_Iter;
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_Iter_Access);
   use type System.Address;

   function Iter_Or_Null (Iter : System.Address) return System.Address is
   begin
      if Convert (Iter).Stamp = 0 then--  null iter
         return System.Null_Address;
      else
         return Iter;
      end if;
   end Iter_Or_Null;

   procedure Get_Tree_Iter
     (Val  : Glib.Values.GValue;
      Iter : out Gtk_Tree_Iter) is
   begin
      Iter := Convert (Glib.Values.Get_Address (Val)).all;
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
       Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Data  : System.Address) return Glib.Gboolean;
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
       Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Data  : System.Address) return Glib.Gboolean
   is
      Func : constant Gtk_Tree_Model_Foreach_Func := To_Gtk_Tree_Model_Foreach_Func (Data);
   begin
      return Boolean'Pos (Func (Model, From_Object (Path), Iter.all));
   end Internal_Gtk_Tree_Model_Foreach_Func;

   --------------------
   -- Gtk_Filter_New --
   --------------------

   procedure Gtk_Filter_New
      (Tree_Model : out Gtk_Tree_Model;
       Root       : Gtk_Tree_Path)
   is
      function Internal (Root : System.Address) return Gtk_Tree_Model;
      pragma Import (C, Internal, "gtk_tree_model_filter_new");
   begin
      Tree_Model := Internal (Get_Object (Root));
   end Gtk_Filter_New;

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
         (Path : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_tree_path_new_from_string");
      Tmp_Path   : Gtkada.Types.Chars_Ptr := New_String (Path);
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

   ---------------------------
   -- Gtk_New_From_Indicesv --
   ---------------------------

   procedure Gtk_New_From_Indicesv
      (Path    : out Gtk_Tree_Path;
       Indices : Gint_Array;
       Length  : Gsize)
   is
      function Internal
         (Indices : System.Address;
          Length  : Gsize) return System.Address;
      pragma Import (C, Internal, "gtk_tree_path_new_from_indicesv");
   begin
      Path.Set_Object (Internal (Indices (Indices'First)'Address, Length));
   end Gtk_New_From_Indicesv;

   -------------------------------
   -- Gtk_Tree_Model_Filter_New --
   -------------------------------

   function Gtk_Tree_Model_Filter_New
      (Root : Gtk_Tree_Path) return Gtk_Tree_Model
   is
      function Internal (Root : System.Address) return Gtk_Tree_Model;
      pragma Import (C, Internal, "gtk_tree_model_filter_new");
      Tree_Model : Gtk_Tree_Model;
   begin
      Tree_Model := Internal (Get_Object (Root));
      return Tree_Model;
   end Gtk_Tree_Model_Filter_New;

   -----------------------
   -- Gtk_Tree_Path_New --
   -----------------------

   function Gtk_Tree_Path_New return Gtk_Tree_Path is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tree_path_new");
      Path : Gtk_Tree_Path;
   begin
      Path.Set_Object (Internal);
      return Path;
   end Gtk_Tree_Path_New;

   -----------------------------
   -- Gtk_Tree_Path_New_First --
   -----------------------------

   function Gtk_Tree_Path_New_First return Gtk_Tree_Path is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tree_path_new_first");
      Path : Gtk_Tree_Path;
   begin
      Path.Set_Object (Internal);
      return Path;
   end Gtk_Tree_Path_New_First;

   -------------------------------------
   -- Gtk_Tree_Path_New_From_Indicesv --
   -------------------------------------

   function Gtk_Tree_Path_New_From_Indicesv
      (Indices : Gint_Array;
       Length  : Gsize) return Gtk_Tree_Path
   is
      function Internal
         (Indices : System.Address;
          Length  : Gsize) return System.Address;
      pragma Import (C, Internal, "gtk_tree_path_new_from_indicesv");
      Path : Gtk_Tree_Path;
   begin
      Path.Set_Object (Internal (Indices (Indices'First)'Address, Length));
      return Path;
   end Gtk_Tree_Path_New_From_Indicesv;

   -----------------------------------
   -- Gtk_Tree_Path_New_From_String --
   -----------------------------------

   function Gtk_Tree_Path_New_From_String
      (Path : UTF8_String) return Gtk_Tree_Path
   is
      function Internal
         (Path : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_tree_path_new_from_string");
      Tmp_Path   : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Return : System.Address;
      Self       : Gtk_Tree_Path;
   begin
      Tmp_Return := Internal (Tmp_Path);
      Free (Tmp_Path);
      Self.Set_Object (Tmp_Return);
      return Self;
   end Gtk_Tree_Path_New_From_String;

   ------------------
   -- Append_Index --
   ------------------

   procedure Append_Index (Path : Gtk_Tree_Path; Index : Glib.Gint) is
      procedure Internal (Path : System.Address; Index : Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_path_append_index");
   begin
      Internal (Get_Object (Path), Index);
   end Append_Index;

   --------------
   -- Children --
   --------------

   function Children
      (Tree_Model : Gtk_Tree_Model;
       Parent     : Gtk_Tree_Iter) return Gtk_Tree_Iter
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

   -------------
   -- Compare --
   -------------

   function Compare
      (Path : Gtk_Tree_Path;
       B    : Gtk_Tree_Path) return Glib.Gint
   is
      function Internal
         (Path : System.Address;
          B    : System.Address) return Glib.Gint;
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
         (Tree_Model : Gtk.Tree_Model.Gtk_Tree_Model;
          Func       : Gtk_Tree_Model_Foreach_Func;
          User_Data  : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Tree_Model_Foreach (Tree_Model, System.Null_Address, System.Null_Address);
         else
            D := Users.Build (To_Address (Func), User_Data);
            C_Gtk_Tree_Model_Foreach (Tree_Model, Internal_Cb'Address, D);
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
   -- Get_Depth --
   ---------------

   function Get_Depth (Path : Gtk_Tree_Path) return Glib.Gint is
      function Internal (Path : System.Address) return Glib.Gint;
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

      --  Do not free the result of gtk_tree_path_get_indices since this is
      --  not a copy, but the currently used data.
      Result : constant Result_Array_Access := Internal (Get_Object (Path));

   begin
      if Result = null then
         return (0 .. -1 => 0);
      else
         return Result.all;
      end if;
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
          Iter       : Gtk_Tree_Iter) return Gtkada.Types.Chars_Ptr;
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
          Iter       : Gtk_Tree_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_model_iter_has_child");
   begin
      return Internal (Tree_Model, Iter) /= 0;
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
          Descendant : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_path_is_ancestor");
   begin
      return Internal (Get_Object (Path), Get_Object (Descendant)) /= 0;
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
          Ancestor : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_path_is_descendant");
   begin
      return Internal (Get_Object (Path), Get_Object (Ancestor)) /= 0;
   end Is_Descendant;

   ----------------
   -- N_Children --
   ----------------

   function N_Children
      (Tree_Model : Gtk_Tree_Model;
       Iter       : Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
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

   procedure Next (Tree_Model : Gtk_Tree_Model; Iter : in out Gtk_Tree_Iter) is
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
       N          : Glib.Gint) return Gtk_Tree_Iter
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

   procedure Prepend_Index (Path : Gtk_Tree_Path; Index : Glib.Gint) is
      procedure Internal (Path : System.Address; Index : Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_path_prepend_index");
   begin
      Internal (Get_Object (Path), Index);
   end Prepend_Index;

   ----------
   -- Prev --
   ----------

   function Prev (Path : Gtk_Tree_Path) return Boolean is
      function Internal (Path : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_path_prev");
   begin
      return Internal (Get_Object (Path)) /= 0;
   end Prev;

   --------------
   -- Previous --
   --------------

   procedure Previous
      (Tree_Model : Gtk_Tree_Model;
       Iter       : in out Gtk_Tree_Iter)
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
       New_Order  : Gint_Array)
   is
      procedure Internal
         (Tree_Model : Gtk_Tree_Model;
          Path       : System.Address;
          Iter       : Gtk_Tree_Iter;
          New_Order  : Gint_Array);
      pragma Import (C, Internal, "gtk_tree_model_rows_reordered");
   begin
      Internal (Tree_Model, Get_Object (Path), Iter, New_Order);
   end Rows_Reordered;

   --------------------------------
   -- Rows_Reordered_With_Length --
   --------------------------------

   procedure Rows_Reordered_With_Length
      (Tree_Model : Gtk_Tree_Model;
       Path       : Gtk_Tree_Path;
       Iter       : Gtk_Tree_Iter;
       New_Order  : Gint_Array;
       Length     : Glib.Gint)
   is
      procedure Internal
         (Tree_Model : Gtk_Tree_Model;
          Path       : System.Address;
          Iter       : System.Address;
          New_Order  : System.Address;
          Length     : Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_model_rows_reordered_with_length");
   begin
      Internal (Tree_Model, Get_Object (Path), Iter_Or_Null (Iter'Address), New_Order (New_Order'First)'Address, Length);
   end Rows_Reordered_With_Length;

   ---------------
   -- To_String --
   ---------------

   function To_String (Path : Gtk_Tree_Path) return UTF8_String is
      function Internal
         (Path : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_tree_path_to_string");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Path)));
   end To_String;

   --------
   -- Up --
   --------

   function Up (Path : Gtk_Tree_Path) return Boolean is
      function Internal (Path : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_path_up");
   begin
      return Internal (Get_Object (Path)) /= 0;
   end Up;

   function Convert (R : Gtk.Tree_Model.Gtk_Tree_Path) return System.Address is
   begin
      return Get_Object (R);
   end Convert;

   function Convert (R : System.Address) return Gtk.Tree_Model.Gtk_Tree_Path is
   begin
      return From_Object(R);
   end Convert;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Tree_Model_Gtk_Tree_Path_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Tree_Model_Gtk_Tree_Path_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Tree_Path_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Tree_Path_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void);

   procedure Connect
      (Object  : Gtk_Tree_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Void;
       After   : Boolean);

   procedure Connect
      (Object  : Gtk_Tree_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_Model_Gtk_Tree_Path_Void;
       After   : Boolean);

   procedure Connect
      (Object  : Gtk_Tree_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : Gtk_Tree_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : Gtk_Tree_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Tree_Path_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : Gtk_Tree_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void);

   procedure Marsh_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Void);

   procedure Marsh_GObject_Gtk_Tree_Path_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Tree_Path_Void);

   procedure Marsh_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void);

   procedure Marsh_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Void);

   procedure Marsh_Gtk_Tree_Model_Gtk_Tree_Path_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Tree_Model_Gtk_Tree_Path_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Gtk_Tree_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Gtk_Tree_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_Model_Gtk_Tree_Path_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Tree_Model_Gtk_Tree_Path_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Gtk_Tree_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : Gtk_Tree_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : Gtk_Tree_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Tree_Path_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Tree_Path_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : Gtk_Tree_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------------------------------------------
   -- Marsh_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void --
   ------------------------------------------------------------

   procedure Marsh_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Tree_Model.From_Object (Unchecked_To_Address (Params, 1)), Unchecked_To_Gtk_Tree_Iter (Params, 2), Unchecked_To_Address (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void;

   ----------------------------------------------------
   -- Marsh_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Void --
   ----------------------------------------------------

   procedure Marsh_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Tree_Model.From_Object (Unchecked_To_Address (Params, 1)), Unchecked_To_Gtk_Tree_Iter (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Void;

   --------------------------------------
   -- Marsh_GObject_Gtk_Tree_Path_Void --
   --------------------------------------

   procedure Marsh_GObject_Gtk_Tree_Path_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Tree_Path_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Tree_Model.From_Object (Unchecked_To_Address (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Tree_Path_Void;

   -------------------------------------------------------------------
   -- Marsh_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void --
   -------------------------------------------------------------------

   procedure Marsh_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Tree_Model := Gtk_Tree_Model (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj, Gtk.Tree_Model.From_Object (Unchecked_To_Address (Params, 1)), Unchecked_To_Gtk_Tree_Iter (Params, 2), Unchecked_To_Address (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void;

   -----------------------------------------------------------
   -- Marsh_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Void --
   -----------------------------------------------------------

   procedure Marsh_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Tree_Model := Gtk_Tree_Model (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj, Gtk.Tree_Model.From_Object (Unchecked_To_Address (Params, 1)), Unchecked_To_Gtk_Tree_Iter (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Void;

   ---------------------------------------------
   -- Marsh_Gtk_Tree_Model_Gtk_Tree_Path_Void --
   ---------------------------------------------

   procedure Marsh_Gtk_Tree_Model_Gtk_Tree_Path_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Tree_Model_Gtk_Tree_Path_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Tree_Model := Gtk_Tree_Model (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj, Gtk.Tree_Model.From_Object (Unchecked_To_Address (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Tree_Model_Gtk_Tree_Path_Void;

   --------------------
   -- On_Row_Changed --
   --------------------

   procedure On_Row_Changed
      (Self  : Gtk_Tree_Model;
       Call  : Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "row-changed" & ASCII.NUL, Call, After);
   end On_Row_Changed;

   --------------------
   -- On_Row_Changed --
   --------------------

   procedure On_Row_Changed
      (Self  : Gtk_Tree_Model;
       Call  : Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "row-changed" & ASCII.NUL, Call, After, Slot);
   end On_Row_Changed;

   --------------------
   -- On_Row_Deleted --
   --------------------

   procedure On_Row_Deleted
      (Self  : Gtk_Tree_Model;
       Call  : Cb_Gtk_Tree_Model_Gtk_Tree_Path_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "row-deleted" & ASCII.NUL, Call, After);
   end On_Row_Deleted;

   --------------------
   -- On_Row_Deleted --
   --------------------

   procedure On_Row_Deleted
      (Self  : Gtk_Tree_Model;
       Call  : Cb_GObject_Gtk_Tree_Path_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "row-deleted" & ASCII.NUL, Call, After, Slot);
   end On_Row_Deleted;

   ------------------------------
   -- On_Row_Has_Child_Toggled --
   ------------------------------

   procedure On_Row_Has_Child_Toggled
      (Self  : Gtk_Tree_Model;
       Call  : Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "row-has-child-toggled" & ASCII.NUL, Call, After);
   end On_Row_Has_Child_Toggled;

   ------------------------------
   -- On_Row_Has_Child_Toggled --
   ------------------------------

   procedure On_Row_Has_Child_Toggled
      (Self  : Gtk_Tree_Model;
       Call  : Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "row-has-child-toggled" & ASCII.NUL, Call, After, Slot);
   end On_Row_Has_Child_Toggled;

   ---------------------
   -- On_Row_Inserted --
   ---------------------

   procedure On_Row_Inserted
      (Self  : Gtk_Tree_Model;
       Call  : Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "row-inserted" & ASCII.NUL, Call, After);
   end On_Row_Inserted;

   ---------------------
   -- On_Row_Inserted --
   ---------------------

   procedure On_Row_Inserted
      (Self  : Gtk_Tree_Model;
       Call  : Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "row-inserted" & ASCII.NUL, Call, After, Slot);
   end On_Row_Inserted;

   -----------------------
   -- On_Rows_Reordered --
   -----------------------

   procedure On_Rows_Reordered
      (Self  : Gtk_Tree_Model;
       Call  : Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "rows-reordered" & ASCII.NUL, Call, After);
   end On_Rows_Reordered;

   -----------------------
   -- On_Rows_Reordered --
   -----------------------

   procedure On_Rows_Reordered
      (Self  : Gtk_Tree_Model;
       Call  : Cb_GObject_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "rows-reordered" & ASCII.NUL, Call, After, Slot);
   end On_Rows_Reordered;

   function "+" (W : Gtk_Tree_Model) return Gtk_Tree_Model is
   begin
      return W;
   end "+";

end Gtk.Tree_Model;
