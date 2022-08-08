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
with Glib.Values;              use Glib.Values;
with Gtk.Arguments;            use Gtk.Arguments;
with Gtkada.Bindings;          use Gtkada.Bindings;

package body Gtk.Tree_Sortable is

   procedure C_Gtk_Tree_Sortable_Set_Default_Sort_Func
      (Sortable  : Gtk_Tree_Sortable;
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
      (Sortable       : Gtk_Tree_Sortable;
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

   ---------------------------
   -- Has_Default_Sort_Func --
   ---------------------------

   function Has_Default_Sort_Func
      (Sortable : Gtk_Tree_Sortable) return Boolean
   is
      function Internal (Sortable : Gtk_Tree_Sortable) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_sortable_has_default_sort_func");
   begin
      return Internal (Sortable) /= 0;
   end Has_Default_Sort_Func;

   ---------------------------
   -- Set_Default_Sort_Func --
   ---------------------------

   procedure Set_Default_Sort_Func
      (Sortable  : Gtk_Tree_Sortable;
       Sort_Func : Gtk_Tree_Iter_Compare_Func)
   is
   begin
      if Sort_Func = null then
         C_Gtk_Tree_Sortable_Set_Default_Sort_Func (Sortable, System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Tree_Sortable_Set_Default_Sort_Func (Sortable, Internal_Gtk_Tree_Iter_Compare_Func'Address, To_Address (Sort_Func), System.Null_Address);
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
         (Sortable  : Gtk.Tree_Sortable.Gtk_Tree_Sortable;
          Sort_Func : Gtk_Tree_Iter_Compare_Func;
          User_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Sort_Func = null then
            C_Gtk_Tree_Sortable_Set_Default_Sort_Func (Sortable, System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Sort_Func), User_Data);
            C_Gtk_Tree_Sortable_Set_Default_Sort_Func (Sortable, Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Default_Sort_Func;

   end Set_Default_Sort_Func_User_Data;

   -------------------
   -- Set_Sort_Func --
   -------------------

   procedure Set_Sort_Func
      (Sortable       : Gtk_Tree_Sortable;
       Sort_Column_Id : Glib.Gint;
       Sort_Func      : Gtk_Tree_Iter_Compare_Func)
   is
   begin
      if Sort_Func = null then
         C_Gtk_Tree_Sortable_Set_Sort_Func (Sortable, Sort_Column_Id, System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Tree_Sortable_Set_Sort_Func (Sortable, Sort_Column_Id, Internal_Gtk_Tree_Iter_Compare_Func'Address, To_Address (Sort_Func), System.Null_Address);
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
         (Sortable       : Gtk.Tree_Sortable.Gtk_Tree_Sortable;
          Sort_Column_Id : Glib.Gint;
          Sort_Func      : Gtk_Tree_Iter_Compare_Func;
          User_Data      : User_Data_Type)
      is
         D : System.Address;
      begin
         if Sort_Func = null then
            C_Gtk_Tree_Sortable_Set_Sort_Func (Sortable, Sort_Column_Id, System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Sort_Func), User_Data);
            C_Gtk_Tree_Sortable_Set_Sort_Func (Sortable, Sort_Column_Id, Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Sort_Func;

   end Set_Sort_Func_User_Data;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Tree_Sortable_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Tree_Sortable_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : Gtk_Tree_Sortable;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_Sortable_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : Gtk_Tree_Sortable;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Tree_Sortable_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Tree_Sortable_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Gtk_Tree_Sortable;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_Sortable_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Tree_Sortable_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : Gtk_Tree_Sortable;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------
   -- Marsh_GObject_Void --
   ------------------------

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   ----------------------------------
   -- Marsh_Gtk_Tree_Sortable_Void --
   ----------------------------------

   procedure Marsh_Gtk_Tree_Sortable_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Tree_Sortable_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Tree_Sortable := Gtk_Tree_Sortable (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Tree_Sortable_Void;

   ----------------------------
   -- On_Sort_Column_Changed --
   ----------------------------

   procedure On_Sort_Column_Changed
      (Self  : Gtk_Tree_Sortable;
       Call  : Cb_Gtk_Tree_Sortable_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "sort-column-changed" & ASCII.NUL, Call, After);
   end On_Sort_Column_Changed;

   ----------------------------
   -- On_Sort_Column_Changed --
   ----------------------------

   procedure On_Sort_Column_Changed
      (Self  : Gtk_Tree_Sortable;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "sort-column-changed" & ASCII.NUL, Call, After, Slot);
   end On_Sort_Column_Changed;

   function "+" (W : Gtk_Tree_Sortable) return Gtk_Tree_Sortable is
   begin
      return W;
   end "+";

end Gtk.Tree_Sortable;
