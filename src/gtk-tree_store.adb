-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2003 ACT-Europe                 --
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

with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk; use Gtk;
with System;
with Ada.Unchecked_Conversion;

package body Gtk.Tree_Store is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Tree_Store : out Gtk_Tree_Store;
      Types      : GType_Array) is
   begin
      Tree_Store := new Gtk_Tree_Store_Record;
      Initialize (Tree_Store, Types);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Tree_Store : access Gtk_Tree_Store_Record'Class;
      Types      : GType_Array)
   is
      function Internal
        (N_Columns : Gint;
         Types     : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_store_newv");

   begin
      Set_Object (Tree_Store, Internal (Types'Length, Types'Address));
   end Initialize;

   -----------------
   -- Generic_Set --
   -----------------

   package body Generic_Set is

      function To_Address is new
        Ada.Unchecked_Conversion (Data_Type_Access, System.Address);

      procedure Set
        (Tree_Store : access Gtk_Tree_Store_Record;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Data_Type_Access)
      is
         procedure Internal
           (Tree_Store : System.Address;
            Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
            Column     : Gint;
            Value      : System.Address;
            Final      : Gint := -1);
         pragma Import (C, Internal, "gtk_tree_store_set");

      begin
         Internal
           (Get_Object (Tree_Store), Iter, Column, To_Address (Value));
      end Set;

   end Generic_Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : UTF8_String)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : UTF8_String;
         Final      : Gint := -1);
      pragma Import (C, Internal, "gtk_tree_store_set");

   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Value & ASCII.NUL);
   end Set;

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Gint)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Gint;
         Final      : Gint := -1);
      pragma Import (C, Internal, "gtk_tree_store_set");

   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Value);
   end Set;

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Boolean)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Gboolean;
         Final      : Gint := -1);
      pragma Import (C, Internal, "gtk_tree_store_set");

   begin
      Internal (Get_Object (Tree_Store), Iter, Column, To_Gboolean (Value));
   end Set;

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Glib.C_Proxy)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Glib.C_Proxy;
         Final      : Gint := -1);
      pragma Import (C, Internal, "gtk_tree_store_set");

   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Value);
   end Set;

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Glib.Object.GObject)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : System.Address;
         Final      : Gint := -1);
      pragma Import (C, Internal, "gtk_tree_store_set");

   begin
      if Value = null then
         Internal (Get_Object (Tree_Store), Iter, Column, System.Null_Address);

      else
         Internal (Get_Object (Tree_Store), Iter, Column, Get_Object (Value));
      end if;
   end Set;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types
     (Tree_Store : access Gtk_Tree_Store_Record;
      Types      : GType_Array)
   is
      procedure Internal
        (Tree_Store : System.Address;
         N_Columns  : Gint;
         Types      : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_set_column_types");

   begin
      Internal (Get_Object (Tree_Store), Types'Length, Types'Address);
   end Set_Column_Types;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Glib.Values.GValue)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_tree_store_set_value");

   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Value);
   end Set_Value;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_store_remove");

   begin
      Internal (Get_Object (Tree_Store), Iter);
      Iter := Gtk.Tree_Model.Null_Iter;
   end Remove;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Position   : Gint)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
         Parent     : System.Address;
         Position   : Gint);
      pragma Import (C, Internal, "gtk_tree_store_insert");

   begin
      if Parent = Null_Iter then
         Internal
           (Get_Object (Tree_Store), Iter, System.Null_Address, Position);
      else
         Internal (Get_Object (Tree_Store), Iter, Parent'Address, Position);
      end if;
   end Insert;

   -------------------
   -- Insert_Before --
   -------------------

   procedure Insert_Before
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Sibling    : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
         Parent     : System.Address;
         Sibling    : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_insert_before");

      P, S : System.Address := System.Null_Address;

   begin
      if Parent /= Null_Iter then
         P := Parent'Address;
      end if;

      if Sibling /= Null_Iter then
         S := Sibling'Address;
      end if;

      Internal (Get_Object (Tree_Store), Iter, P, S);
   end Insert_Before;

   ------------------
   -- Insert_After --
   ------------------

   procedure Insert_After
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Sibling    : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
         Parent     : System.Address;
         Sibling    : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_insert_after");

      P, S : System.Address := System.Null_Address;

   begin
      if Parent /= Null_Iter then
         P := Parent'Address;
      end if;

      if Sibling /= Null_Iter then
         S := Sibling'Address;
      end if;

      Internal (Get_Object (Tree_Store), Iter, P, S);
   end Insert_After;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
         Parent     : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_prepend");

   begin
      if Parent = Null_Iter then
         Internal (Get_Object (Tree_Store), Iter, System.Null_Address);
      else
         Internal (Get_Object (Tree_Store), Iter, Parent'Address);
      end if;
   end Prepend;

   ------------
   -- Append --
   ------------

   procedure Append
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
         Parent     : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_append");

   begin
      if Parent = Null_Iter then
         Internal (Get_Object (Tree_Store), Iter, System.Null_Address);
      else
         Internal (Get_Object (Tree_Store), Iter, Parent'Address);
      end if;
   end Append;

   -----------------
   -- Is_Ancestor --
   -----------------

   function Is_Ancestor
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Descendant : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      function Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Descendant : Gtk.Tree_Model.Gtk_Tree_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_store_is_ancestor");

   begin
      return To_Boolean (Internal
        (Get_Object (Tree_Store), Iter, Descendant));
   end Is_Ancestor;

   ----------------
   -- Iter_Depth --
   ----------------

   function Iter_Depth
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint
   is
      function Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint;
      pragma Import (C, Internal, "gtk_tree_store_iter_depth");

   begin
      return Internal (Get_Object (Tree_Store), Iter);
   end Iter_Depth;

   -----------
   -- Clear --
   -----------

   procedure Clear (Tree_Store : access Gtk_Tree_Store_Record) is
      procedure Internal (Tree_Store : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_clear");

   begin
      Internal (Get_Object (Tree_Store));
   end Clear;

end Gtk.Tree_Store;
