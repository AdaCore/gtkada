------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2022, AdaCore                     --
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

with Ada.Unchecked_Conversion;

package body Glib.Gnodes is

   ---------------
   --  Destroy  --
   ---------------

   procedure Destroy (Node : in out Gnode) is
      procedure Internal (Node : Gnode);
      pragma Import (C, Internal, "g_node_destroy");
   begin
      Internal (Node);
      Node := null;
   end Destroy;

   -------------------
   --  Is_Ancestor  --
   -------------------

   function Is_Ancestor (Node       : Gnode;
                         Descendant : Gnode) return Boolean is
      function Internal (Node       : Gnode;
                         Descendant : Gnode) return Gboolean;
      pragma Import (C, Internal, "g_node_is_ancestor");
   begin
      return Internal (Node, Descendant) /= 0;
   end Is_Ancestor;

   ---------------
   --  Is_Leaf  --
   ---------------

   function Is_Leaf (Node : Gnode) return Boolean is
      function Internal (Node : Gnode) return Gboolean;
      pragma Import (C, Internal, "ada_gnode_is_leaf");
   begin
      return Internal (Node) /= 0;
   end Is_Leaf;

   ---------------
   --  Is_Root  --
   ---------------

   function Is_Root (Node : Gnode) return Boolean is
      function Internal (Node : Gnode) return Gboolean;
      pragma Import (C, Internal, "ada_gnode_is_root");
   begin
      return Internal (Node) /= 0;
   end Is_Root;

   ---------------
   --  N_Nodes  --
   ---------------

   function N_Nodes (Root  : Gnode;
                     Flags : Glib_Traverse_Flags) return Guint is
      function Internal
        (Root : Gnode; Flags : Glib_Traverse_Flags) return Guint;
      pragma Import (C, Internal, "g_node_n_nodes");

   begin
      return Internal (Root, Flags);
   end N_Nodes;

   ----------------
   -- Gnode_Data --
   ----------------

   package body Gnode_Data is

      function Convert is new Ada.Unchecked_Conversion (Element_Access, Gnode);

      ----------------
      --  Glib_New  --
      ----------------

      procedure Glib_New (Node : out Gnode;
                          Data :     Element_Access) is
      begin
         Node := Convert (Data);
      end Glib_New;

   end Gnode_Data;

end Glib.Gnodes;
