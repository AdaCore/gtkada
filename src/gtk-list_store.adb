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

with Glib.Object; use Glib.Object;
with Gtk.Tree_Model;
with Gtk; use Gtk;
with System;

package body Gtk.List_Store is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget    : out Gtk_List_Store;
      N_Columns : Gint;
      Types     : GType_Array)
   is
   begin
      Widget := new Gtk_List_Store_Record;
      Initialize (Widget, N_Columns, Types);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget    : access Gtk_List_Store_Record'Class;
      N_Columns : Gint;
      Types     : GType_Array)
   is
      function Internal
        (N_Columns : Gint;
         Types     : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_list_store_newv");

      Temp_Array : GType_Array (1 .. Types'Length + 1);
   begin
      Temp_Array (1 .. Types'Length) := Types;
      Temp_Array (Temp_Array'Last) := GType_Invalid;

      Set_Object (Widget, Internal (N_Columns,
                                    Temp_Array (Temp_Array'First)'Address));
      Initialize_User_Data (Widget);
   end Initialize;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (List_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : System.Address)
   is
      procedure Internal
        (List_Store : System.Address;
         Iter       : System.Address;
         Column     : Gint;
         Value      : System.Address);
      pragma Import (C, Internal, "ada_gtk_list_store_set_value");
   begin
      Internal (Get_Object (List_Store),
                Iter'Address,
                Column,
                Value);
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (List_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : in out Glib.Values.GValue)
   is
      procedure Internal
        (List_Store : System.Address;
         Iter       : System.Address;
         Column     : Gint;
         Value      : System.Address);
      pragma Import (C, Internal, "gtk_list_store_set_value");
   begin
      Internal (Get_Object (List_Store),
                Iter'Address,
                Column,
                Value'Address);
   end Set_Value;

--    ----------------
--    -- Set_Valist --
--    ----------------

--    procedure Set_Valist
--      (List_Store : access Gtk_List_Store_Record;
--       Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
--       Var_Args   : va_list)
--    is
--       procedure Internal
--         (List_Store : System.Address;
--          Iter       : System.Address;
--          Var_Args   : Gint);
--       pragma Import (C, Internal, "gtk_list_store_set_valist");
--    begin
--       Internal (Get_Object (List_Store),
--                 Get_Object (Iter),
--                 va_list'Pos (Var_Args));
--    end Set_Valist;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (List_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
        (List_Store : System.Address;
         Iter       : System.Address);
      pragma Import (C, Internal, "gtk_list_store_remove");
   begin
      Internal (Get_Object (List_Store),
                Iter'Address);
   end Remove;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (List_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Position   : Gint)
   is
      procedure Internal
        (List_Store : System.Address;
         Iter       : System.Address;
         Position   : Gint);
      pragma Import (C, Internal, "gtk_list_store_insert");
   begin
      Internal (Get_Object (List_Store),
                Iter'Address,
                Position);
   end Insert;

   -------------------
   -- Insert_Before --
   -------------------

   procedure Insert_Before
     (List_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Sibling    : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
        (List_Store : System.Address;
         Iter       : System.Address;
         Sibling    : System.Address);
      pragma Import (C, Internal, "gtk_list_store_insert_before");
   begin
      Internal (Get_Object (List_Store),
                Iter'Address,
                Sibling'Address);
   end Insert_Before;

   ------------------
   -- Insert_After --
   ------------------

   procedure Insert_After
     (List_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Sibling    : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
        (List_Store : System.Address;
         Iter       : System.Address;
         Sibling    : System.Address);
      pragma Import (C, Internal, "gtk_list_store_insert_after");
   begin
      Internal (Get_Object (List_Store),
                Iter'Address,
                Sibling'Address);
   end Insert_After;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (List_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
        (List_Store : System.Address;
         Iter       : System.Address);
      pragma Import (C, Internal, "gtk_list_store_prepend");
   begin
      Internal (Get_Object (List_Store),
                Iter'Address);
   end Prepend;

   ------------
   -- Append --
   ------------

   procedure Append
     (List_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
        (List_Store : System.Address;
         Iter       : System.Address);
      pragma Import (C, Internal, "gtk_list_store_append");
   begin
      Internal (Get_Object (List_Store),
                Iter'Address);
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (List_Store : access Gtk_List_Store_Record)
   is
      procedure Internal (List_Store : System.Address);
      pragma Import (C, Internal, "gtk_list_store_clear");
   begin
      Internal (Get_Object (List_Store));
   end Clear;

end Gtk.List_Store;
