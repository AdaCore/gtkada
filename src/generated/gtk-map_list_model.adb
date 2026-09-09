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

package body Gtk.Map_List_Model is

   function C_New
     (Model        : Glib.List_Model.Glist_Model;
      Map_Func     : System.Address;
      User_Data    : System.Address;
      User_Destroy : System.Address)
   return System.Address;
   pragma Import (C, C_New, "gtk_map_list_model_new");

   procedure C_Gtk_Map_List_Model_Set_Map_Func
     (Self         : System.Address;
      Map_Func     : System.Address;
      User_Data    : System.Address;
      User_Destroy : System.Address);
   pragma Import (C, C_Gtk_Map_List_Model_Set_Map_Func, "gtk_map_list_model_set_map_func");
   --  Sets the function used to map items.
   --  The function will be called whenever an item needs to be mapped and
   --  must return the item to use for the given input item.
   --  Note that `GtkMapListModel` may call this function multiple times on
   --  the same item, because it may delete items it doesn't need anymore.
   --  GTK makes no effort to ensure that Map_Func conforms to the item type
   --  of Self. It assumes that the caller knows what they are doing and the
   --  map function returns items of the appropriate type.
   --  @param Map_Func map function
   --  @param User_Data user data passed to Map_Func
   --  @param User_Destroy destroy notifier for User_Data

   function To_Gtk_Map_List_Model_Map_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Map_List_Model_Map_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Map_List_Model_Map_Func, System.Address);

   function Internal_Gtk_Map_List_Model_Map_Func
     (Item      : System.Address;
      User_Data : System.Address) return System.Address;
   pragma Convention (C, Internal_Gtk_Map_List_Model_Map_Func);
   --  @param Item The item to map
   --  @param User_Data user data

   procedure Internal_Destroy (Data : System.Address) is null;
   pragma Convention (C, Internal_Destroy);

   ----------------------------
   -- Gtk_Map_List_Model_New --
   ----------------------------

   function Gtk_Map_List_Model_New
     (Model    : Glib.List_Model.Glist_Model;
      Map_Func : Gtk_Map_List_Model_Map_Func)
   return Gtk_Map_List_Model
   is
      Self : constant Gtk_Map_List_Model := new Gtk_Map_List_Model_Record;
   begin
      Gtk.Map_List_Model.Initialize (Self, Model, Map_Func);
      return Self;
   end Gtk_Map_List_Model_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self     : out Gtk_Map_List_Model;
      Model    : Glib.List_Model.Glist_Model;
      Map_Func : Gtk_Map_List_Model_Map_Func) is
   begin
      Self := new Gtk_Map_List_Model_Record;
      Gtk.Map_List_Model.Initialize (Self, Model, Map_Func);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : not null access Gtk_Map_List_Model_Record'Class;
      Model    : Glib.List_Model.Glist_Model;
      Map_Func : Gtk_Map_List_Model_Map_Func) is
   begin
      if not Self.Is_Created then
         if Map_Func = null then
            Set_Object
              (Self,
               C_New
                 (Model,
                  System.Null_Address,
                  System.Null_Address,
                  Internal_Destroy'Address));
         else
            Set_Object
              (Self,
               C_New
                 (Model,
                  Internal_Gtk_Map_List_Model_Map_Func'Address,
                  To_Address (Map_Func),
                  Internal_Destroy'Address));
         end if;
      end if;
   end Initialize;

   ------------------------------------------
   -- Internal_Gtk_Map_List_Model_Map_Func --
   ------------------------------------------

   function Internal_Gtk_Map_List_Model_Map_Func
     (Item      : System.Address;
      User_Data : System.Address) return System.Address
   is
      Func         : constant Gtk_Map_List_Model_Map_Func :=
      To_Gtk_Map_List_Model_Map_Func (User_Data);
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      return Get_Object (Func (Get_User_Data (Item, Stub_GObject)));
   exception
      when others => return System.Null_Address;
   end Internal_Gtk_Map_List_Model_Map_Func;

   ------------------
   -- Set_Map_Func --
   ------------------

   procedure Set_Map_Func
     (Self     : not null access Gtk_Map_List_Model_Record;
      Map_Func : Gtk_Map_List_Model_Map_Func)
   is
   begin
      if Map_Func = null then
         C_Gtk_Map_List_Model_Set_Map_Func
           (Get_Object (Self),
            System.Null_Address,
            System.Null_Address,
            Internal_Destroy'Address);
      else
         C_Gtk_Map_List_Model_Set_Map_Func
           (Get_Object (Self),
            Internal_Gtk_Map_List_Model_Map_Func'Address,
            To_Address (Map_Func),
            Internal_Destroy'Address);
      end if;
   end Set_Map_Func;

   ------------------------------
   -- Map_List_Model_User_Data --
   ------------------------------

   package body Map_List_Model_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Map_List_Model_Map_User_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Map_List_Model_Map_User_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Map_List_Model_Map_User_Func, System.Address);

      function Internal_Cb
        (Item      : System.Address;
         User_Data : System.Address) return System.Address;
      pragma Convention (C, Internal_Cb);
      --  User function that is called to map an Item of the original model to
      --  an item expected by the map model.
      --  The returned items must conform to the item type of the model they
      --  are used with.
      --  Parameter Item has transfer-ownership='full'
      --  Parameter Item has transfer-ownership='full'
      --  @param Item The item to map
      --  @param User_Data user data

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
        (Item      : System.Address;
         User_Data : System.Address) return System.Address
      is
         D            : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_GObject : Glib.Object.GObject_Record;
      begin
         return Get_Object
           (To_Gtk_Map_List_Model_Map_User_Func (D.Func)
              (Get_User_Data (Item, Stub_GObject), D.Data.all));
      exception
         when others => return System.Null_Address;
      end Internal_Cb;

      ------------------
      -- Gtk_New_User --
      ------------------

      procedure Gtk_New_User
        (Self      : out Gtk_Map_List_Model;
         Model     : Glib.List_Model.Glist_Model;
         Map_Func  : Gtk_Map_List_Model_Map_User_Func;
         User_Data : User_Data_Type) is
      begin
         Self := new Gtk_Map_List_Model_Record;
         Map_List_Model_User_Data.Initialize_User (Self, Model, Map_Func, User_Data);
      end Gtk_New_User;

      ---------------------------------
      -- Gtk_Map_List_Model_New_User --
      ---------------------------------

      function Gtk_Map_List_Model_New_User
        (Model     : Glib.List_Model.Glist_Model;
         Map_Func  : Gtk_Map_List_Model_Map_User_Func;
         User_Data : User_Data_Type)
      return Gtk_Map_List_Model
      is
         Self : constant Gtk_Map_List_Model := new Gtk_Map_List_Model_Record;
      begin
         Map_List_Model_User_Data.Initialize_User (Self, Model, Map_Func, User_Data);
         return Self;
      end Gtk_Map_List_Model_New_User;

      ---------------------
      -- Initialize_User --
      ---------------------

      procedure Initialize_User
        (Self      : not null access Gtk_Map_List_Model_Record'Class;
         Model     : Glib.List_Model.Glist_Model;
         Map_Func  : Gtk_Map_List_Model_Map_User_Func;
         User_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if not Self.Is_Created then
            if Map_Func = null then
               Set_Object
                 (Self,
                  C_New
                    (Model,
                     System.Null_Address,
                     System.Null_Address,
                     Users.Free_Data'Address));
            else
               D := Users.Build (To_Address (Map_Func), User_Data);
               Set_Object
                 (Self,
                  C_New
                    (Model,
                     Internal_Cb'Address,
                     D,
                     Users.Free_Data'Address));
            end if;
         end if;
      end Initialize_User;

      ------------------
      -- Set_Map_Func --
      ------------------

      procedure Set_Map_Func
        (Self      : not null access Gtk.Map_List_Model.Gtk_Map_List_Model_Record'Class;
         Map_Func  : Gtk_Map_List_Model_Map_User_Func;
         User_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Map_Func = null then
            C_Gtk_Map_List_Model_Set_Map_Func
              (Get_Object (Self),
               System.Null_Address,
               System.Null_Address,
               Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Map_Func), User_Data);
            C_Gtk_Map_List_Model_Set_Map_Func
              (Get_Object (Self),
               Internal_Cb'Address,
               D,
               Users.Free_Data'Address);
         end if;
      end Set_Map_Func;

   end Map_List_Model_User_Data;

   package Type_Conversion_Gtk_Map_List_Model is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Map_List_Model_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Map_List_Model);

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
      (Self : not null access Gtk_Map_List_Model_Record)
       return Glib.List_Model.Glist_Model
   is
      function Internal
         (Self : System.Address) return Glib.List_Model.Glist_Model;
      pragma Import (C, Internal, "gtk_map_list_model_get_model");
   begin
      return Internal (Get_Object (Self));
   end Get_Model;

   -------------
   -- Has_Map --
   -------------

   function Has_Map
      (Self : not null access Gtk_Map_List_Model_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_map_list_model_has_map");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Has_Map;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
      (Self  : not null access Gtk_Map_List_Model_Record;
       Model : Glib.List_Model.Glist_Model)
   is
      procedure Internal
         (Self  : System.Address;
          Model : Glib.List_Model.Glist_Model);
      pragma Import (C, Internal, "gtk_map_list_model_set_model");
   begin
      Internal (Get_Object (Self), Model);
   end Set_Model;

   --------------
   -- Get_Item --
   --------------

   function Get_Item
      (Self     : not null access Gtk_Map_List_Model_Record;
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
      (Self : not null access Gtk_Map_List_Model_Record) return GType
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
      (Self : not null access Gtk_Map_List_Model_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "g_list_model_get_n_items");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Items;

   -----------------
   -- Get_Section --
   -----------------

   procedure Get_Section
      (Self      : not null access Gtk_Map_List_Model_Record;
       Position  : Guint;
       Out_Start : out Guint;
       Out_End   : out Guint)
   is
      procedure Internal
         (Self      : System.Address;
          Position  : Guint;
          Out_Start : out Guint;
          Out_End   : out Guint);
      pragma Import (C, Internal, "gtk_section_model_get_section");
   begin
      Internal (Get_Object (Self), Position, Out_Start, Out_End);
   end Get_Section;

   -------------------
   -- Items_Changed --
   -------------------

   procedure Items_Changed
      (Self     : not null access Gtk_Map_List_Model_Record;
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

   ----------------------
   -- Sections_Changed --
   ----------------------

   procedure Sections_Changed
      (Self     : not null access Gtk_Map_List_Model_Record;
       Position : Guint;
       N_Items  : Guint)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Guint;
          N_Items  : Guint);
      pragma Import (C, Internal, "gtk_section_model_sections_changed");
   begin
      Internal (Get_Object (Self), Position, N_Items);
   end Sections_Changed;

end Gtk.Map_List_Model;
