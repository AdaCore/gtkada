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

package body Gtk.Custom_Filter is

   function C_Gtk_Custom_Filter_New
     (Match_Func   : System.Address;
      User_Data    : System.Address;
      User_Destroy : System.Address)
   return System.Address;
   pragma Import (C, C_Gtk_Custom_Filter_New, "gtk_custom_filter_new");

   procedure C_Gtk_Custom_Filter_Set_Filter_Func
     (Self         : System.Address;
      Match_Func   : System.Address;
      User_Data    : System.Address;
      User_Destroy : System.Address);
   pragma Import (C, C_Gtk_Custom_Filter_Set_Filter_Func, "gtk_custom_filter_set_filter_func");
   --  Sets the function used for filtering items.
   --  If Match_Func is `NULL`, the filter matches all items.
   --  If the filter func changes its filtering behavior,
   --  [methodGtk.Filter.changed] needs to be called.
   --  If a previous function was set, its User_Destroy will be called.
   --  @param Match_Func function to filter items
   --  @param User_Data user data to pass to Match_Func
   --  @param User_Destroy destroy notify for User_Data

   function To_Gtk_Custom_Filter_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Custom_Filter_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Custom_Filter_Func, System.Address);

   function Internal_Gtk_Filter_Func
     (Item      : System.Address;
      User_Data : System.Address) return Glib.Gboolean;
   pragma Convention (C, Internal_Gtk_Filter_Func);
   --  @param Item the item to be matched
   --  @param User_Data Gtk_Custom_Filter_Func

   procedure Internal_Destroy (Data : System.Address) is null;
   pragma Convention (C, Internal_Destroy);

   ---------------------------
   -- Gtk_Custom_Filter_New --
   ---------------------------

   function Gtk_Custom_Filter_New
     (Match_Func : Gtk_Custom_Filter_Func) return Gtk_Custom_Filter
   is
      Self : constant Gtk_Custom_Filter := new Gtk_Custom_Filter_Record;
   begin
      Gtk.Custom_Filter.Initialize (Self, Match_Func);
      return Self;
   end Gtk_Custom_Filter_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self       : out Gtk_Custom_Filter;
      Match_Func : Gtk_Custom_Filter_Func) is
   begin
      Self := new Gtk_Custom_Filter_Record;
      Gtk.Custom_Filter.Initialize (Self, Match_Func);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : not null access Gtk_Custom_Filter_Record'Class;
      Match_Func : Gtk_Custom_Filter_Func) is
   begin
      if not Self.Is_Created then
         if Match_Func = null then
            Set_Object
              (Self, C_Gtk_Custom_Filter_New (System.Null_Address, System.Null_Address, Internal_Destroy'Address));
         else
            Set_Object
              (Self,
               C_Gtk_Custom_Filter_New
                 (Internal_Gtk_Filter_Func'Address, To_Address (Match_Func), Internal_Destroy'Address));
         end if;
      end if;
   end Initialize;

   ---------------------
   -- Set_Filter_Func --
   ---------------------

   procedure Set_Filter_Func
     (Self       : not null access Gtk_Custom_Filter_Record;
      Match_Func : Gtk_Custom_Filter_Func) is
   begin
      if Match_Func = null then
         C_Gtk_Custom_Filter_Set_Filter_Func
           (Get_Object (Self), System.Null_Address, System.Null_Address, Internal_Destroy'Address);
      else
         C_Gtk_Custom_Filter_Set_Filter_Func
           (Get_Object (Self), Internal_Gtk_Filter_Func'Address, To_Address (Match_Func), Internal_Destroy'Address);
      end if;
   end Set_Filter_Func;

   ------------------------------
   -- Internal_Gtk_Filter_Func --
   ------------------------------

   function Internal_Gtk_Filter_Func
     (Item      : System.Address;
      User_Data : System.Address) return Glib.Gboolean
   is
      Stub : Glib.Object.GObject_Record;
   begin
      return Boolean'Pos
        (To_Gtk_Custom_Filter_Func (User_Data) (Get_User_Data (Item, Stub)));
   exception
      when others => return 0;
   end Internal_Gtk_Filter_Func;

   -----------------------------
   -- Custom_Filter_User_Data --
   -----------------------------

   package body Custom_Filter_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Custom_Filter_Func_User is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Custom_Filter_Func_User);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Custom_Filter_Func_User, System.Address);

      function Internal_Cb
        (Item      : System.Address;
         User_Data : System.Address) return Glib.Gboolean;
      pragma Convention (C, Internal_Cb);
      --  @param Item the item to be matched
      --  @param User_Data user data

      --------------------------------
      -- Gtk_Custom_Filter_New_User --
      --------------------------------

      function Gtk_Custom_Filter_New_User
        (Match_Func : Gtk_Custom_Filter_Func_User;
         User_Data  : User_Data_Type)
      return Gtk_Custom_Filter
      is
         Self : constant Gtk_Custom_Filter := new Gtk_Custom_Filter_Record;
      begin
         Initialize_User (Self, Match_Func, User_Data);
         return Self;
      end Gtk_Custom_Filter_New_User;

      ------------------
      -- Gtk_New_User --
      ------------------

      procedure Gtk_New_User
        (Self       : out Gtk_Custom_Filter;
         Match_Func : Gtk_Custom_Filter_Func_User;
         User_Data  : User_Data_Type) is
      begin
         Self := new Gtk_Custom_Filter_Record;
         Initialize_User (Self, Match_Func, User_Data);
      end Gtk_New_User;

      ---------------------
      -- Initialize_User --
      ---------------------

      procedure Initialize_User
        (Self       : not null access Gtk_Custom_Filter_Record'Class;
         Match_Func : Gtk_Custom_Filter_Func_User;
         User_Data  : User_Data_Type)
      is
         D : System.Address;
      begin
         if not Self.Is_Created then
            if Match_Func = null then
               Set_Object
                 (Self, C_Gtk_Custom_Filter_New (System.Null_Address, System.Null_Address, Users.Free_Data'Address));
            else
               D := Users.Build (To_Address (Match_Func), User_Data);
               Set_Object
                 (Self,
                  C_Gtk_Custom_Filter_New (Internal_Cb'Address, D, Users.Free_Data'Address));
            end if;
         end if;
      end Initialize_User;

      --------------------------
      -- Set_Filter_Func_User --
      --------------------------

      procedure Set_Filter_Func_User
        (Self       : not null access Gtk.Custom_Filter.Gtk_Custom_Filter_Record'Class;
         Match_Func : Gtk_Custom_Filter_Func_User;
         User_Data  : User_Data_Type)
      is
         D : System.Address;
      begin
         if Match_Func = null then
            C_Gtk_Custom_Filter_Set_Filter_Func
              (Get_Object (Self), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Match_Func), User_Data);
            C_Gtk_Custom_Filter_Set_Filter_Func
              (Get_Object (Self), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Filter_Func_User;

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
        (Item      : System.Address;
         User_Data : System.Address) return Glib.Gboolean
      is
         D    : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub : Glib.Object.GObject_Record;
      begin
         return Boolean'Pos
           (To_Gtk_Custom_Filter_Func_User (D.Func) (Get_User_Data (Item, Stub), D.Data.all));
      exception
         when others => return 0;
      end Internal_Cb;

   end Custom_Filter_User_Data;

   package Type_Conversion_Gtk_Custom_Filter is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Custom_Filter_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Custom_Filter);

end Gtk.Custom_Filter;
