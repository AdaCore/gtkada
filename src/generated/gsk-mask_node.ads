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

--  A render node masking one child node with another.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Gsk.Enums;       use Gsk.Enums;
with Gsk.Render_Node; use Gsk.Render_Node;

package Gsk.Mask_Node is

   type Gsk_Mask_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Mask_Node is access all Gsk_Mask_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self      : out Gsk_Mask_Node;
       Source    : Gsk.Render_Node.Gsk_Render_Node;
       Mask      : Gsk.Render_Node.Gsk_Render_Node;
       Mask_Mode : Gsk.Enums.Gsk_Mask_Mode);
   procedure Initialize
      (Self      : not null access Gsk_Mask_Node_Record'Class;
       Source    : Gsk.Render_Node.Gsk_Render_Node;
       Mask      : Gsk.Render_Node.Gsk_Render_Node;
       Mask_Mode : Gsk.Enums.Gsk_Mask_Mode);
   --  Creates a `GskRenderNode` that will mask a given node by another.
   --  The Mask_Mode determines how the 'mask values' are derived from the
   --  colors of the Mask. Applying the mask consists of multiplying the 'mask
   --  value' with the alpha of the source.
   --  Since: gtk+ 4.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Source The source node to be drawn
   --  @param Mask The node to be used as mask
   --  @param Mask_Mode The mask mode to use

   function Gsk_Mask_Node_New
      (Source    : Gsk.Render_Node.Gsk_Render_Node;
       Mask      : Gsk.Render_Node.Gsk_Render_Node;
       Mask_Mode : Gsk.Enums.Gsk_Mask_Mode) return Gsk_Mask_Node;
   --  Creates a `GskRenderNode` that will mask a given node by another.
   --  The Mask_Mode determines how the 'mask values' are derived from the
   --  colors of the Mask. Applying the mask consists of multiplying the 'mask
   --  value' with the alpha of the source.
   --  Since: gtk+ 4.10
   --  @param Source The source node to be drawn
   --  @param Mask The node to be used as mask
   --  @param Mask_Mode The mask mode to use

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_mask_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Mask
      (Self : Gsk_Mask_Node) return Gsk.Render_Node.Gsk_Render_Node;
   --  Retrieves the mask `GskRenderNode` child of the Node.
   --  Since: gtk+ 4.10
   --  @return the mask child node. Has transfer-ownership='none'.

   function Get_Mask_Mode
      (Self : Gsk_Mask_Node) return Gsk.Enums.Gsk_Mask_Mode;
   --  Retrieves the mask mode used by Node.
   --  Since: gtk+ 4.10
   --  @return the mask mode

   function Get_Source
      (Self : Gsk_Mask_Node) return Gsk.Render_Node.Gsk_Render_Node;
   --  Retrieves the source `GskRenderNode` child of the Node.
   --  Since: gtk+ 4.10
   --  @return the source child node. Has transfer-ownership='none'.

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address) return Gsk_Mask_Node_Record;

private
   for Gsk_Mask_Node_Record'External_Tag use "GskMaskNode";
end Gsk.Mask_Node;
