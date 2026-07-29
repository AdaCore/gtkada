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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Glib.List_Store is

   --  Equal_Func handling
   function To_Address is new Ada.Unchecked_Conversion
     (Equal_Func, System.Address);

   --  Compare_Data_Func handling
   function To_Address is new Ada.Unchecked_Conversion
     (Compare_Data_Func, System.Address);

   function To_Compare_Data_Func is new Ada.Unchecked_Conversion
     (System.Address, Compare_Data_Func);

   function Internal_Compare_Data_Func
     (A         : System.Address;
      B         : System.Address;
      User_Data : System.Address) return Glib.Gint;
   pragma Convention (C, Internal_Compare_Data_Func);
   --  @param A a value
   --  @param B a value to compare with
   --  @param User_Data user data

   ---------------------------------
   -- Internal_Compare_Data_Func --
   ---------------------------------

   function Internal_Compare_Data_Func
     (A         : System.Address;
      B         : System.Address;
      User_Data : System.Address) return Glib.Gint
   is
      Func : constant Compare_Data_Func := To_Compare_Data_Func (User_Data);
      Stub_A, Stub_B : GObject_Record;
   begin
      return
      Func
        (Glib.Object.Get_User_Data (A, Stub_A),
         Glib.Object.Get_User_Data (B, Stub_B));
   end Internal_Compare_Data_Func;

   package Type_Conversion_Glist_Store is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Glist_Store_Record);
   pragma Unreferenced (Type_Conversion_Glist_Store);

   -----------
   -- G_New --
   -----------

   procedure G_New
      (Self      : out Glist_Store;
       Item_Type : GType := Glib.GType_Object)
   is
   begin
      Self := new Glist_Store_Record;
      Glib.List_Store.Initialize (Self, Item_Type);
   end G_New;

   ---------------------
   -- Glist_Store_New --
   ---------------------

   function Glist_Store_New
      (Item_Type : GType := Glib.GType_Object) return Glist_Store
   is
      Self : constant Glist_Store := new Glist_Store_Record;
   begin
      Glib.List_Store.Initialize (Self, Item_Type);
      return Self;
   end Glist_Store_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self      : not null access Glist_Store_Record'Class;
       Item_Type : GType := Glib.GType_Object)
   is
      function Internal (Item_Type : GType) return System.Address;
      pragma Import (C, Internal, "g_list_store_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Item_Type));
      end if;
   end Initialize;

   ------------
   -- Append --
   ------------

   procedure Append
      (Self : not null access Glist_Store_Record;
       Item : not null access GObject_Record'Class)
   is
      procedure Internal (Self : System.Address; Obj_Ptr : System.Address);
      pragma Import (C, Internal, "g_list_store_append");
   begin
      Internal (Get_Object (Self), Get_Object (Item));
   end Append;

   ----------
   -- Find --
   ----------

   function Find
      (Self     : not null access Glist_Store_Record;
       Item     : not null access GObject_Record'Class;
       Position : access Guint := null) return Boolean
   is
      function Internal
        (Self     : System.Address;
         Obj_Ptr  : System.Address;
         Position : access Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "g_list_store_find");
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return :=
        Internal
          (Get_Object (Self),
           Get_Object (Item),
           Position);
      return Tmp_Return /= 0;
   end Find;

   --------------------------
   -- Find_With_Equal_Func --
   --------------------------

   function Find_With_Equal_Func
      (Self     : not null access Glist_Store_Record;
       Item     : not null access GObject_Record'Class;
       Func     : Equal_Func;
       Position : access Guint := null) return Boolean
   is
      Fn_Equal : constant System.Address :=
        (if Func = null then System.Null_Address else To_Address (Func));
      function Internal
        (Self     : System.Address;
         Obj_Ptr  : System.Address;
         Func     : System.Address;
         Position : access Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "g_list_store_find_with_equal_func");
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return :=
        Internal
          (Get_Object (Self),
           Get_Object (Item),
           Fn_Equal,
           Position);
      return Tmp_Return /= 0;
   end Find_With_Equal_Func;

   ------------
   -- Insert --
   ------------

   procedure Insert
      (Self     : not null access Glist_Store_Record;
       Position : Guint;
       Item     : not null access GObject_Record'Class)
   is
      procedure Internal
        (Self : System.Address; Position : Guint; Obj_Ptr : System.Address);
      pragma Import (C, Internal, "g_list_store_insert");
   begin
      Internal
        (Get_Object (Self),
         Position,
         Get_Object (Item));
   end Insert;

   -------------------
   -- Insert_Sorted --
   -------------------

   function Insert_Sorted
      (Self : not null access Glist_Store_Record;
       Item : not null access GObject_Record'Class;
       Func : Compare_Data_Func) return Guint
   is
      Func_Compare : constant System.Address :=
        (if Func = null then System.Null_Address else To_Address (Func));
      Wrapper      : constant System.Address :=
        (if Func = null
         then System.Null_Address
         else Internal_Compare_Data_Func'Address);
      function Internal
        (Self      : System.Address;
         Item      : System.Address;
         Func      : System.Address;
         User_Data : System.Address) return Guint;
      pragma Import (C, Internal, "g_list_store_insert_sorted");
   begin
      return
        Internal
          (Get_Object (Self),
           Get_Object (Item),
           Wrapper,
           Func_Compare);
   end Insert_Sorted;

   ------------
   -- Remove --
   ------------

   procedure Remove
      (Self     : not null access Glist_Store_Record;
       Position : Guint)
   is
      procedure Internal (Self : System.Address; Position : Guint);
      pragma Import (C, Internal, "g_list_store_remove");
   begin
      Internal (Get_Object (Self), Position);
   end Remove;

   ----------------
   -- Remove_All --
   ----------------

   procedure Remove_All (Self : not null access Glist_Store_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_list_store_remove_all");
   begin
      Internal (Get_Object (Self));
   end Remove_All;

   ----------
   -- Sort --
   ----------

   procedure Sort
      (Self : not null access Glist_Store_Record;
       Func : Compare_Data_Func)
   is
      Func_Compare : constant System.Address :=
        (if Func = null
         then System.Null_Address
         else To_Address (Func));
      Wrapper : constant System.Address :=
        (if Func = null
         then System.Null_Address
         else Internal_Compare_Data_Func'Address);
      procedure Internal
        (Self      : System.Address;
         Func      : System.Address;
         User_Data : System.Address);
      pragma Import (C, Internal, "g_list_store_sort");
   begin
      Internal (Get_Object (Self),  Wrapper, Func_Compare);
   end Sort;

   ------------
   -- Splice --
   ------------

   procedure Splice
      (Self       : not null access Glist_Store_Record;
       Position   : Guint;
       N_Removals : Guint;
       Additions  : Array_GObject)
   is
      procedure Internal
         (Self        : System.Address;
          Position    : Guint;
          N_Removals  : Guint;
          Additions   : Array_GObject;
          N_Additions : Guint);
      pragma Import (C, Internal, "g_list_store_splice");
   begin
      Internal (Get_Object (Self), Position, N_Removals, Additions, Additions'Length);
   end Splice;

   --------------
   -- Get_Item --
   --------------

   function Get_Item
      (Self     : not null access Glist_Store_Record;
       Position : Guint) return Glib.Object.GObject
   is
      function Internal
         (Self     : System.Address;
          Position : Guint) return System.Address;
      pragma Import (C, Internal, "g_list_model_get_object");
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      return Get_User_Data (Internal (Get_Object (Self), Position), Stub_GObject);
   end Get_Item;

   -------------------
   -- Get_Item_Type --
   -------------------

   function Get_Item_Type
      (Self : not null access Glist_Store_Record) return GType
   is
      function Internal (Self : System.Address) return GType;
      pragma Import (C, Internal, "g_list_model_get_item_type");
   begin
      return Internal (Get_Object (Self));
   end Get_Item_Type;

   -----------------
   -- Get_N_Items --
   -----------------

   function Get_N_Items
      (Self : not null access Glist_Store_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "g_list_model_get_n_items");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Items;

   -------------------
   -- Items_Changed --
   -------------------

   procedure Items_Changed
      (Self     : not null access Glist_Store_Record;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Guint;
          Removed  : Guint;
          Added    : Guint);
      pragma Import (C, Internal, "g_list_model_items_changed");
   begin
      Internal (Get_Object (Self), Position, Removed, Added);
   end Items_Changed;

end Glib.List_Store;
