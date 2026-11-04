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

--  A render node that can contain other render nodes.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Gsk.Render_Node; use Gsk.Render_Node;

package Gsk.Container_Node is

   type Gsk_Container_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Container_Node is access all Gsk_Container_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_container_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : Gsk_Container_Node;
       Idx  : Guint) return Gsk.Render_Node.Gsk_Render_Node;
   --  Gets one of the children of Container.
   --  @param Idx the position of the child to get
   --  @return the Idx'th child of Container. Has transfer-ownership='none'.

   function Get_N_Children (Self : Gsk_Container_Node) return Guint;
   --  Retrieves the number of direct children of Node.
   --  @return the number of children of the `GskRenderNode`

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Gtk_New
     (Self     : out Gsk_Container_Node;
      Children : Gsk.Render_Node.Gsk_Render_Node_Array);
   procedure Initialize
     (Self     : not null access Gsk_Container_Node_Record'Class;
      Children : Gsk.Render_Node.Gsk_Render_Node_Array);
   --  Creates a new `GskRenderNode` instance for holding the given Children.
   --  The new node will acquire a reference to each of the children.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Children The children of the node

   function Gsk_Container_Node_New
     (Children : Gsk_Render_Node_Array) return Gsk_Container_Node;
   --  Creates a new `GskRenderNode` instance for holding the given Children.
   --  The new node will acquire a reference to each of the children.
   --  @param Children The children of the node

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Container_Node_Record;

private
   for Gsk_Container_Node_Record'External_Tag use "GskContainerNode";
end Gsk.Container_Node;
