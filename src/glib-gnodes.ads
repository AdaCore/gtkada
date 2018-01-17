------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2018, AdaCore                     --
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

--  <group>Glib, the general-purpose library</group>
with Glib.Enums; use Glib.Enums;

package Glib.Gnodes is

   type Gnode is new C_Proxy;

   --  pop_allocator
   --  push_allocator

   function Child_Position (Node  : Gnode; Child : Gnode) return Gint;

   function Depth (Node : Gnode) return Guint;

   procedure Destroy (Node : in out Gnode);

   function First_Child (Node : Gnode) return Gnode;

   function First_Sibling (Node : Gnode) return Gnode;

   function Get_Root (Node : Gnode) return Gnode;

   procedure Insert
     (Parent   : in out Gnode;
      Position :        Gint;
      Node     : in out Gnode);

   procedure Insert_Before
     (Parent  : in out Gnode;
      Sibling : in out Gnode;
      Node    : in out Gnode);

   function Is_Ancestor
     (Node       : Gnode;
      Descendant : Gnode) return Boolean;

   function Is_Leaf (Node : Gnode) return Boolean;

   function Is_Root (Node : Gnode) return Boolean;

   function Last_Child (Node : Gnode) return Gnode;

   function Last_Sibling (Node : Gnode) return Gnode;

   function Max_Height (Root : Gnode) return Guint;

   function N_Nodes
     (Root  : Gnode; Flags : Glib_Traverse_Flags) return Guint;

   function Next_Sibling (Node : Gnode) return Gnode;

   function Nth_Child (Node : Gnode; N : Guint) return Gnode;

   procedure Prepend (Parent : in out Gnode; Node : in out Gnode);

   function Prev_Sibling (Node : Gnode) return Gnode;

   procedure Reverse_Children (Node : in out Gnode);

   procedure Unlink (Node : in out Gnode);

   generic
      type Element (<>) is private;
      type Element_Access is access all Element;
   package Gnode_Data is

      --  type Gnode_Traverse_Func
      --  type Gnode_Foreach_Func

      --  child_index
      --  children_foreach
      --  find
      --  find_child

      procedure Glib_New (Node : out Gnode; Data : Element_Access);

      --  traverse
   end Gnode_Data;

private
   pragma Import (C, Child_Position, "g_node_child_position");
   pragma Import (C, Depth, "g_node_depth");
   pragma Import (C, First_Child, "ada_gnode_first_child");
   pragma Import (C, First_Sibling, "g_node_first_sibling");
   pragma Import (C, Get_Root, "g_node_get_root");
   pragma Import (C, Insert, "g_node_insert");
   pragma Import (C, Insert_Before, "g_node_insert_before");
   pragma Import (C, Last_Child, "g_node_last_child");
   pragma Import (C, Last_Sibling, "g_node_last_sibling");
   pragma Import (C, Max_Height, "g_node_max_height");
   pragma Import (C, Next_Sibling, "ada_gnode_next_sibling");
   pragma Import (C, Nth_Child, "g_node_nth_child");
   pragma Import (C, Prepend, "g_node_prepend");
   pragma Import (C, Prev_Sibling, "ada_gnode_prev_sibling");
   pragma Import (C, Reverse_Children, "g_node_reverse_children");
   pragma Import (C, Unlink, "g_node_unlink");
end Glib.Gnodes;
