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

package body Gtk.Tree_List_Model is

   function C_New
     (Root         : Glib.List_Model.Glist_Model;
      Passthrough  : Glib.Gboolean;
      Autoexpand   : Glib.Gboolean;
      Create_Func  : System.Address;
      User_Data    : System.Address;
      User_Destroy : System.Address)
   return System.Address;
   pragma Import (C, C_New, "gtk_tree_list_model_new");

   function To_Gtk_Tree_List_Model_Create_Model_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_List_Model_Create_Model_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Tree_List_Model_Create_Model_Func, System.Address);

   function Internal_Gtk_Tree_List_Model_Create_Model_Func
     (Item      : System.Address;
      User_Data : System.Address) return Glib.List_Model.Glist_Model;
   pragma Convention (C, Internal_Gtk_Tree_List_Model_Create_Model_Func);
   --  @param Item The item to map
   --  @param User_Data user data

   procedure Internal_Destroy (Data : System.Address) is null;
   pragma Convention (C, Internal_Destroy);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self        : out Gtk_Tree_List_Model;
      Root        : Glib.List_Model.Glist_Model;
      Passthrough : Boolean;
      Autoexpand  : Boolean;
      Create_Func : Gtk_Tree_List_Model_Create_Model_Func) is
   begin
      Self := new Gtk_Tree_List_Model_Record;
      Gtk.Tree_List_Model.Initialize (Self, Root, Passthrough, Autoexpand, Create_Func);
   end Gtk_New;

   -----------------------------
   -- Gtk_Tree_List_Model_New --
   -----------------------------

   function Gtk_Tree_List_Model_New
     (Root        : Glib.List_Model.Glist_Model;
      Passthrough : Boolean;
      Autoexpand  : Boolean;
      Create_Func : Gtk_Tree_List_Model_Create_Model_Func)
   return Gtk_Tree_List_Model
   is
      Self : constant Gtk_Tree_List_Model := new Gtk_Tree_List_Model_Record;
   begin
      Gtk.Tree_List_Model.Initialize (Self, Root, Passthrough, Autoexpand, Create_Func);
      return Self;
   end Gtk_Tree_List_Model_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self        : not null access Gtk_Tree_List_Model_Record'Class;
      Root        : Glib.List_Model.Glist_Model;
      Passthrough : Boolean;
      Autoexpand  : Boolean;
      Create_Func : Gtk_Tree_List_Model_Create_Model_Func) is
   begin
      if not Self.Is_Created then
         if Create_Func = null then
            Set_Object
              (Self,
               C_New
                 (Root,
                  Boolean'Pos (Passthrough),
                  Boolean'Pos (Autoexpand),
                  System.Null_Address,
                  System.Null_Address,
                  Internal_Destroy'Address));
         else
            Set_Object
              (Self,
               C_New
                 (Root,
                  Boolean'Pos (Passthrough),
                  Boolean'Pos (Autoexpand),
                  Internal_Gtk_Tree_List_Model_Create_Model_Func'Address,
                  To_Address (Create_Func),
                  Internal_Destroy'Address));
         end if;
      end if;
   end Initialize;

   ----------------------------------------------------
   -- Internal_Gtk_Tree_List_Model_Create_Model_Func --
   ----------------------------------------------------

   function Internal_Gtk_Tree_List_Model_Create_Model_Func
     (Item      : System.Address;
      User_Data : System.Address) return Glib.List_Model.Glist_Model
   is
      Func         : constant Gtk_Tree_List_Model_Create_Model_Func :=
      To_Gtk_Tree_List_Model_Create_Model_Func (User_Data);
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      return Func (Get_User_Data (Item, Stub_GObject));
   exception
      when others => return Glib.List_Model.Null_Glist_Model;
   end Internal_Gtk_Tree_List_Model_Create_Model_Func;

   -----------------------------------
   -- Gtk_Tree_List_Model_User_Data --
   -----------------------------------

   package body Gtk_Tree_List_Model_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Tree_List_Model_Create_Model_User_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Tree_List_Model_Create_Model_User_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Tree_List_Model_Create_Model_User_Func, System.Address);

      function Internal_Cb
        (Item      : System.Address;
         User_Data : System.Address) return Glib.List_Model.Glist_Model;
      pragma Convention (C, Internal_Cb);

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
        (Item      : System.Address;
         User_Data : System.Address) return Glib.List_Model.Glist_Model
      is
         D            : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_GObject : Glib.Object.GObject_Record;
      begin
         return To_Gtk_Tree_List_Model_Create_Model_User_Func (D.Func)
           (Get_User_Data (Item, Stub_GObject), D.Data.all);
      exception
         when others => return Glib.List_Model.Null_Glist_Model;
      end Internal_Cb;

      ------------------
      -- Gtk_New_User --
      ------------------

      procedure Gtk_New_User
        (Self         : out Gtk_Tree_List_Model;
         Root         : Glib.List_Model.Glist_Model;
         Passthrough  : Boolean;
         Autoexpand   : Boolean;
         Create_Func  : Gtk_Tree_List_Model_Create_Model_User_Func;
         User_Data    : User_Data_Type)
      is
      begin
         Self := new Gtk_Tree_List_Model_Record;
         Gtk_Tree_List_Model_User_Data.Initialize_User
           (Self, Root, Passthrough, Autoexpand, Create_Func, User_Data);
      end Gtk_New_User;

      ----------------------------------
      -- Gtk_Tree_List_Model_New_User --
      ----------------------------------

      function Gtk_Tree_List_Model_New_User
        (Root         : Glib.List_Model.Glist_Model;
         Passthrough  : Boolean;
         Autoexpand   : Boolean;
         Create_Func  : Gtk_Tree_List_Model_Create_Model_User_Func;
         User_Data    : User_Data_Type)
      return Gtk_Tree_List_Model
      is
         Self : constant Gtk_Tree_List_Model := new Gtk_Tree_List_Model_Record;
      begin
         Gtk_Tree_List_Model_User_Data.Initialize_User
           (Self, Root, Passthrough, Autoexpand, Create_Func, User_Data);
         return Self;
      end Gtk_Tree_List_Model_New_User;

      ---------------------
      -- Initialize_User --
      ---------------------

      procedure Initialize_User
        (Self         : not null access Gtk_Tree_List_Model_Record'Class;
         Root         : Glib.List_Model.Glist_Model;
         Passthrough  : Boolean;
         Autoexpand   : Boolean;
         Create_Func  : Gtk_Tree_List_Model_Create_Model_User_Func;
         User_Data    : User_Data_Type)
      is
         D : System.Address;
      begin
         if not Self.Is_Created then
            if Create_Func = null then
               Set_Object
                 (Self,
                  C_New
                    (Root,
                     Boolean'Pos (Passthrough),
                     Boolean'Pos (Autoexpand),
                     System.Null_Address,
                     System.Null_Address,
                     Users.Free_Data'Address));
            else
               D := Users.Build (To_Address (Create_Func), User_Data);
               Set_Object
                 (Self,
                  C_New
                    (Root,
                     Boolean'Pos (Passthrough),
                     Boolean'Pos (Autoexpand),
                     Internal_Cb'Address,
                     D,
                     Users.Free_Data'Address));
            end if;
         end if;
      end Initialize_User;

   end Gtk_Tree_List_Model_User_Data;

   package Type_Conversion_Gtk_Tree_List_Model is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tree_List_Model_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Tree_List_Model);

   --------------------
   -- Get_Autoexpand --
   --------------------

   function Get_Autoexpand
      (Self : not null access Gtk_Tree_List_Model_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_list_model_get_autoexpand");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Autoexpand;

   -------------------
   -- Get_Child_Row --
   -------------------

   function Get_Child_Row
      (Self     : not null access Gtk_Tree_List_Model_Record;
       Position : Guint) return Gtk.Tree_List_Row.Gtk_Tree_List_Row
   is
      function Internal
         (Self     : System.Address;
          Position : Guint) return System.Address;
      pragma Import (C, Internal, "gtk_tree_list_model_get_child_row");
      Stub_Gtk_Tree_List_Row : Gtk.Tree_List_Row.Gtk_Tree_List_Row_Record;
   begin
      return Gtk.Tree_List_Row.Gtk_Tree_List_Row (Get_User_Data (Internal (Get_Object (Self), Position), Stub_Gtk_Tree_List_Row));
   end Get_Child_Row;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
      (Self : not null access Gtk_Tree_List_Model_Record)
       return Glib.List_Model.Glist_Model
   is
      function Internal
         (Self : System.Address) return Glib.List_Model.Glist_Model;
      pragma Import (C, Internal, "gtk_tree_list_model_get_model");
   begin
      return Internal (Get_Object (Self));
   end Get_Model;

   ---------------------
   -- Get_Passthrough --
   ---------------------

   function Get_Passthrough
      (Self : not null access Gtk_Tree_List_Model_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_list_model_get_passthrough");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Passthrough;

   -------------
   -- Get_Row --
   -------------

   function Get_Row
      (Self     : not null access Gtk_Tree_List_Model_Record;
       Position : Guint) return Gtk.Tree_List_Row.Gtk_Tree_List_Row
   is
      function Internal
         (Self     : System.Address;
          Position : Guint) return System.Address;
      pragma Import (C, Internal, "gtk_tree_list_model_get_row");
      Stub_Gtk_Tree_List_Row : Gtk.Tree_List_Row.Gtk_Tree_List_Row_Record;
   begin
      return Gtk.Tree_List_Row.Gtk_Tree_List_Row (Get_User_Data (Internal (Get_Object (Self), Position), Stub_Gtk_Tree_List_Row));
   end Get_Row;

   --------------------
   -- Set_Autoexpand --
   --------------------

   procedure Set_Autoexpand
      (Self       : not null access Gtk_Tree_List_Model_Record;
       Autoexpand : Boolean)
   is
      procedure Internal (Self : System.Address; Autoexpand : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_list_model_set_autoexpand");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Autoexpand));
   end Set_Autoexpand;

   --------------
   -- Get_Item --
   --------------

   function Get_Item
      (Self     : not null access Gtk_Tree_List_Model_Record;
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
      (Self : not null access Gtk_Tree_List_Model_Record) return GType
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
      (Self : not null access Gtk_Tree_List_Model_Record) return Guint
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
      (Self     : not null access Gtk_Tree_List_Model_Record;
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

end Gtk.Tree_List_Model;
