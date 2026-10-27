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

--  A render node that uses Porter/Duff compositing operators to combine its
--  child with the background.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Gsk.Enums;       use Gsk.Enums;
with Gsk.Render_Node; use Gsk.Render_Node;

package Gsk.Composite_Node is

   type Gsk_Composite_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Composite_Node is access all Gsk_Composite_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self  : out Gsk_Composite_Node;
       Child : Gsk.Render_Node.Gsk_Render_Node;
       Mask  : Gsk.Render_Node.Gsk_Render_Node;
       Op    : Gsk.Enums.Gsk_Porter_Duff);
   procedure Initialize
      (Self  : not null access Gsk_Composite_Node_Record'Class;
       Child : Gsk.Render_Node.Gsk_Render_Node;
       Mask  : Gsk.Render_Node.Gsk_Render_Node;
       Op    : Gsk.Enums.Gsk_Porter_Duff);
   --  Creates a `GskRenderNode` that will composite the child onto the
   --  background with the given operator wherever the mask is set.
   --  Note that various operations can modify the background outside of the
   --  child's bounds, so the mask may cause visual changes outside of the
   --  child.
   --  Since: gtk+ 4.22
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Child The child to composite
   --  @param Mask The mask where the compositing will apply
   --  @param Op The compositing operator

   function Gsk_Composite_Node_New
      (Child : Gsk.Render_Node.Gsk_Render_Node;
       Mask  : Gsk.Render_Node.Gsk_Render_Node;
       Op    : Gsk.Enums.Gsk_Porter_Duff) return Gsk_Composite_Node;
   --  Creates a `GskRenderNode` that will composite the child onto the
   --  background with the given operator wherever the mask is set.
   --  Note that various operations can modify the background outside of the
   --  child's bounds, so the mask may cause visual changes outside of the
   --  child.
   --  Since: gtk+ 4.22
   --  @param Child The child to composite
   --  @param Mask The mask where the compositing will apply
   --  @param Op The compositing operator

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_composite_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : Gsk_Composite_Node) return Gsk.Render_Node.Gsk_Render_Node;
   --  Gets the child node that is getting composited by the given Node.
   --  Since: gtk+ 4.22
   --  @return the child `GskRenderNode`. Has transfer-ownership='none'.

   function Get_Mask
      (Self : Gsk_Composite_Node) return Gsk.Render_Node.Gsk_Render_Node;
   --  Gets the mask node that describes the region where the compositing
   --  applies.
   --  Since: gtk+ 4.22
   --  @return the mask `GskRenderNode`. Has transfer-ownership='none'.

   function Get_Operator
      (Self : Gsk_Composite_Node) return Gsk.Enums.Gsk_Porter_Duff;
   --  Gets the compositing operator used by this node.
   --  Since: gtk+ 4.22
   --  @return The compositing operator

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Composite_Node_Record;

private
   for Gsk_Composite_Node_Record'External_Tag use "GskCompositeNode";
end Gsk.Composite_Node;
