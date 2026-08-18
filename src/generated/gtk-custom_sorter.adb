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

package body Gtk.Custom_Sorter is

   function C_Gtk_Custom_Sorter_New
     (Sort_Func    : System.Address;
      User_Data    : System.Address;
      User_Destroy : System.Address)
   return System.Address;
   pragma Import (C, C_Gtk_Custom_Sorter_New, "gtk_custom_sorter_new");

   procedure C_Gtk_Custom_Sorter_Set_Sort_Func
     (Self         : System.Address;
      Sort_Func    : System.Address;
      User_Data    : System.Address;
      User_Destroy : System.Address);
   pragma Import (C, C_Gtk_Custom_Sorter_Set_Sort_Func, "gtk_custom_sorter_set_sort_func");
   --  Sets (or unsets) the function used for sorting items.
   --  If Sort_Func is null, all items are considered equal.
   --  If the sort func changes its sorting behavior, Gtk.Sorter.Changed needs
   --  to be called.
   --  If a previous function was set, its User_Destroy will be called now.
   --  @param Sort_Func function to sort items
   --  @param User_Data user data to pass to Match_Func
   --  @param User_Destroy destroy notify for User_Data

   function To_Compare_Data_Func is new Ada.Unchecked_Conversion
     (System.Address, Glib.List_Store.Compare_Data_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Glib.List_Store.Compare_Data_Func, System.Address);

   function Internal_Gtk_Sort_Func
     (A         : System.Address;
      B         : System.Address;
      User_Data : System.Address) return Glib.Gint;
   pragma Convention (C, Internal_Gtk_Sort_Func);

   procedure Internal_Destroy (Data : System.Address) is null;
   pragma Convention (C, Internal_Destroy);

   ---------------------------
   -- Gtk_Custom_Sorter_New --
   ---------------------------

   function Gtk_Custom_Sorter_New
     (Sort_Func : Glib.List_Store.Compare_Data_Func)
   return Gtk_Custom_Sorter
   is
      Self : constant Gtk_Custom_Sorter := new Gtk_Custom_Sorter_Record;
   begin
      Gtk.Custom_Sorter.Initialize (Self, Sort_Func);
      return Self;
   end Gtk_Custom_Sorter_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self      : out Gtk_Custom_Sorter;
      Sort_Func : Glib.List_Store.Compare_Data_Func) is
   begin
      Self := new Gtk_Custom_Sorter_Record;
      Gtk.Custom_Sorter.Initialize (Self, Sort_Func);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self      : not null access Gtk_Custom_Sorter_Record'Class;
      Sort_Func : Glib.List_Store.Compare_Data_Func) is
   begin
      if not Self.Is_Created then
         if Sort_Func = null then
            Set_Object
              (Self, C_Gtk_Custom_Sorter_New
                 (System.Null_Address, System.Null_Address, Internal_Destroy'Address));
         else
            Set_Object
              (Self, C_Gtk_Custom_Sorter_New
                 (Internal_Gtk_Sort_Func'Address, To_Address (Sort_Func), Internal_Destroy'Address));
         end if;
      end if;
   end Initialize;

   -------------------
   -- Set_Sort_Func --
   -------------------

   procedure Set_Sort_Func
     (Self      : not null access Gtk_Custom_Sorter_Record;
      Sort_Func : Glib.List_Store.Compare_Data_Func)
   is
   begin
      if Sort_Func = null then
         C_Gtk_Custom_Sorter_Set_Sort_Func
           (Get_Object (Self), System.Null_Address, System.Null_Address, Internal_Destroy'Address);
      else
         C_Gtk_Custom_Sorter_Set_Sort_Func
           (Get_Object (Self), Internal_Gtk_Sort_Func'Address, To_Address (Sort_Func), Internal_Destroy'Address);
      end if;
   end Set_Sort_Func;

   ----------------------------
   -- Internal_Gtk_Sort_Func --
   ----------------------------

   function Internal_Gtk_Sort_Func
     (A         : System.Address;
      B         : System.Address;
      User_Data : System.Address) return Glib.Gint
   is
      Stub : Glib.Object.GObject_Record;
   begin
      return To_Compare_Data_Func (User_Data)
        (Get_User_Data (A, Stub), Get_User_Data (B, Stub));
   exception
      when others => return 0;
   end Internal_Gtk_Sort_Func;

   -----------------------------
   -- Custom_Sorter_User_Data --
   -----------------------------

   package body Custom_Sorter_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gcompare_Data_Func is new Ada.Unchecked_Conversion
        (System.Address, Gcompare_Data_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gcompare_Data_Func, System.Address);

      function Internal_Cb
        (A         : System.Address;
         B         : System.Address;
         User_Data : System.Address) return Glib.Gint;
      pragma Convention (C, Internal_Cb);

      --------------------------------
      -- Gtk_Custom_Sorter_New_User --
      --------------------------------

      function Gtk_Custom_Sorter_New_User
        (Sort_Func : Gcompare_Data_Func;
         User_Data : User_Data_Type)
      return Gtk_Custom_Sorter
      is
         Self : constant Gtk_Custom_Sorter := new Gtk_Custom_Sorter_Record;
      begin
         Initialize_User (Self, Sort_Func, User_Data);
         return Self;
      end Gtk_Custom_Sorter_New_User;

      ------------------
      -- Gtk_New_User --
      ------------------

      procedure Gtk_New_User
        (Self      : out Gtk_Custom_Sorter;
         Sort_Func : Gcompare_Data_Func;
         User_Data : User_Data_Type) is
      begin
         Self := new Gtk_Custom_Sorter_Record;
         Initialize_User (Self, Sort_Func, User_Data);
      end Gtk_New_User;

      ---------------------
      -- Initialize_User --
      ---------------------

      procedure Initialize_User
        (Self      : not null access Gtk_Custom_Sorter_Record'Class;
         Sort_Func : Gcompare_Data_Func;
         User_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if not Self.Is_Created then
            if Sort_Func = null then
               Set_Object
                 (Self, C_Gtk_Custom_Sorter_New
                    (System.Null_Address, System.Null_Address, Users.Free_Data'Address));
            else
               D := Users.Build (To_Address (Sort_Func), User_Data);
               Set_Object
                 (Self, C_Gtk_Custom_Sorter_New
                    (Internal_Cb'Address, D, Users.Free_Data'Address));
            end if;
         end if;
      end Initialize_User;

      ------------------------
      -- Set_Sort_Func_User --
      ------------------------

      procedure Set_Sort_Func_User
        (Self      : not null access Gtk.Custom_Sorter.Gtk_Custom_Sorter_Record'Class;
         Sort_Func : Gcompare_Data_Func;
         User_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Sort_Func = null then
            C_Gtk_Custom_Sorter_Set_Sort_Func
              (Get_Object (Self), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Sort_Func), User_Data);
            C_Gtk_Custom_Sorter_Set_Sort_Func
              (Get_Object (Self), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Sort_Func_User;

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
        (A         : System.Address;
         B         : System.Address;
         User_Data : System.Address) return Glib.Gint
      is
         D    : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub : Glib.Object.GObject_Record;
      begin
         return To_Gcompare_Data_Func (D.Func)
           (Get_User_Data (A, Stub), Get_User_Data (B, Stub), D.Data.all);
      exception
         when others => return 0;
      end Internal_Cb;
   end Custom_Sorter_User_Data;

   package Type_Conversion_Gtk_Custom_Sorter is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Custom_Sorter_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Custom_Sorter);

end Gtk.Custom_Sorter;
