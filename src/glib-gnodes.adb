-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

with System.Address_To_Access_Conversions;

package body Glib.Gnodes is

   -----------------------
   --  Child_Position   --
   -----------------------

   function Child_Position (Node  : in Gnode;
                            Child : in Gnode) return Gint is
      function Internal (Node  : in System.Address;
                         Child : in System.Address) return Gint;
      pragma Import (C, Internal, "g_node_child_position");
   begin
      return Internal (Get_Object (Node), Get_Object (Child));
   end Child_Position;


   -------------
   --  Depth  --
   -------------

   function Depth (Node : in Gnode) return Guint is
      function Internal (Node : in System.Address) return Guint;
      pragma Import (C, Internal, "g_node_depth");
   begin
      return Internal (Get_Object (Node));
   end Depth;

   ---------------
   --  Destroy  --
   ---------------

   procedure Destroy (Node : in out Gnode) is
      procedure Internal (Node : in System.Address);
      pragma Import (C, Internal, "g_node_destroy");
   begin
      Internal (Get_Object (Node));
      Set_Object (Node, System.Null_Address);
   end Destroy;


   -------------------
   --  First_Child  --
   -------------------

   function First_Child (Node : in Gnode) return Gnode is
      function Internal (Node : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gnode_first_child");
      Result : Gnode;
   begin
      Set_Object (Result, Internal (Get_Object (Node)));
      return Result;
   end First_Child;


   ---------------------
   --  First_Sibling  --
   ---------------------

   function First_Sibling (Node : in Gnode) return Gnode is
      function Internal (Node : in System.Address) return System.Address;
      pragma Import (C, Internal, "g_node_first_sibling");
      Result : Gnode;
   begin
      Set_Object (Result, Internal (Get_Object (Node)));
      return Result;
   end First_Sibling;


   ----------------
   --  Get_Root  --
   ----------------

   function Get_Root (Node : in Gnode) return Gnode is
      function Internal (Node : in System.Address) return System.Address;
      pragma Import (C, Internal, "g_node_get_root");
      Result : Gnode;
   begin
      Set_Object (Result, Internal (Get_Object (Node)));
      return Result;
   end Get_Root;


   --------------
   --  Insert  --
   --------------

   procedure Insert (Parent   : in out Gnode;
                     Position : in     Gint;
                     Node     : in out Gnode) is
      procedure Internal (Parent   : in System.Address;
                          Position : in Gint;
                          Node     : in System.Address);
      pragma Import (C, Internal, "g_node_insert");
   begin
      Internal (Get_Object (Parent), Position, Get_Object (Node));
   end Insert;


   ---------------------
   --  Insert_Before  --
   ---------------------

   procedure Insert_Before (Parent  : in out Gnode;
                            Sibling : in out Gnode;
                            Node    : in out Gnode) is
      procedure Internal (Parent  : in System.Address;
                        Sibling : in System.Address;
                        Node    : in System.Address);
      pragma Import (C, Internal, "g_node_insert_before");
   begin
      Internal (Get_Object (Parent), Get_Object (Sibling), Get_Object (Node));
   end Insert_Before;


   -------------------
   --  Is_Ancestor  --
   -------------------

   function Is_Ancestor (Node       : in Gnode;
                         Descendant : in Gnode) return Boolean is
      function Internal (Node       : in System.Address;
                         Descendant : in System.Address) return Gboolean;
      pragma Import (C, Internal, "g_node_is_ancestor");
   begin
      return To_Boolean (Internal (Get_Object (Node),
                                   Get_Object (Descendant)));
   end Is_Ancestor;


   ---------------
   --  Is_Leaf  --
   ---------------

   function Is_Leaf (Node : in Gnode) return Boolean is
      function Internal (Node : in System.Address) return Gboolean;
      pragma Import (C, Internal, "ada_gnode_is_leaf");
   begin
      return To_Boolean (Internal (Get_Object (Node)));
   end Is_Leaf;


   ---------------
   --  Is_Root  --
   ---------------

   function Is_Root (Node : in Gnode) return Boolean is
      function Internal (Node : in System.Address) return Gboolean;
      pragma Import (C, Internal, "ada_gnode_is_root");
   begin
      return To_Boolean (Internal (Get_Object (Node)));
   end Is_Root;


   ------------------
   --  Last_Child  --
   ------------------

   function Last_Child (Node : in Gnode) return Gnode is
      function Internal (Node : in System.Address) return System.Address;
      pragma Import (C, Internal, "g_node_last_child");
      Result : Gnode;
   begin
      Set_Object (Result, Internal (Get_Object (Node)));
      return Result;
   end Last_Child;


   ---------------------
   --  Last_Sibbling  --
   ---------------------

   function Last_Sibling (Node : in Gnode) return Gnode is
      function Internal (Node : in System.Address) return System.Address;
      pragma Import (C, Internal, "g_node_last_sibling");
      Result : Gnode;
   begin
      Set_Object (Result, Internal (Get_Object (Node)));
      return Result;
   end Last_Sibling;


   ------------------
   --  Max_Height  --
   ------------------

   function Max_Height (Root : in Gnode) return Guint is
      function Internal (Root : in System.Address) return Guint;
      pragma Import (C, Internal, "g_node_max_height");
   begin
      return Internal (Get_Object (Root));
   end Max_Height;


   ---------------
   --  N_Nodes  --
   ---------------

   function N_Nodes (Root  : in Gnode;
                     Flags : in Glib_Traverse_Flags) return Guint is
      function Internal (Root  : in System.Address;
                         Flags : in Gint) return Guint;
      pragma Import (C, Internal, "g_node_n_nodes");
   begin
      return Internal (Get_Object (Root), Glib_Traverse_Flags'Pos (Flags));
   end N_Nodes;


   --------------------
   --  Next_Sibling  --
   --------------------

   function Next_Sibling (Node : in Gnode) return Gnode is
      function Internal (Node : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gnode_next_sibling");
      Result : Gnode;
   begin
      Set_Object (Result, Internal (Get_Object (Node)));
      return Result;
   end Next_Sibling;


   -----------------
   --  Nth_Child  --
   -----------------

   function Nth_Child (Node : in Gnode;
                       N    : in Guint) return Gnode is
      function Internal (Node : in System.Address;
                         N    : in Guint) return System.Address;
      pragma Import (C, Internal, "g_node_nth_child");
      Result : Gnode;
   begin
      Set_Object (Result, Internal (Get_Object (Node), N));
      return Result;
   end Nth_Child;


   ---------------
   --  Prepend  --
   ---------------

   procedure Prepend (Parent : in out Gnode;
                      Node   : in out Gnode) is
      procedure Internal (Parent : in System.Address;
                          Node   : in System.Address);
      pragma Import (C, Internal, "g_node_prepend");
   begin
      Internal (Get_Object (Parent), Get_Object (Node));
   end Prepend;


   --------------------
   --  Prev_Sibling  --
   --------------------

   function Prev_Sibling (Node : in Gnode) return Gnode is
      function Internal (Node : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gnode_prev_sibling");
      Result : Gnode;
   begin
      Set_Object (Result, Internal (Get_Object (Node)));
      return Result;
   end Prev_Sibling;


   ------------------------
   --  Reverse_Children  --
   ------------------------

   procedure Reverse_Children (Node : in out Gnode) is
      procedure Internal (Node : in System.Address);
      pragma Import (C, Internal, "g_node_reverse_children");
   begin
      Internal (Get_Object (Node));
   end Reverse_Children;


   --------------
   --  Unlink  --
   --------------

   procedure Unlink (Node : in out Gnode) is
      procedure Internal (Node : in System.Address);
      pragma Import (C, Internal, "g_node_unlink");
   begin
      Internal (Get_Object (Node));
   end Unlink;


   package body Gnode_Data is

      package Convert is new System.Address_To_Access_Conversions
        (Object => Element);

      ----------------
      --  Glib_New  --
      ----------------

      procedure Glib_New (Node :    out Gnode;
                          Data : in     Element_Access) is
         Result : Gnode;
      begin
         Set_Object (Result,
                     Convert.To_Address (Convert.Object_Pointer (Data)));
         Node := Result;
      end Glib_New;

   end Gnode_Data;

end Glib.Gnodes;
