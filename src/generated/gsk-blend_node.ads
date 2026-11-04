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

--  A render node applying a blending function between its two child nodes.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Gsk.Enums;       use Gsk.Enums;
with Gsk.Render_Node; use Gsk.Render_Node;

package Gsk.Blend_Node is

   type Gsk_Blend_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Blend_Node is access all Gsk_Blend_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self       : out Gsk_Blend_Node;
       Bottom     : Gsk.Render_Node.Gsk_Render_Node;
       Top        : Gsk.Render_Node.Gsk_Render_Node;
       Blend_Mode : Gsk.Enums.Gsk_Blend_Mode);
   procedure Initialize
      (Self       : not null access Gsk_Blend_Node_Record'Class;
       Bottom     : Gsk.Render_Node.Gsk_Render_Node;
       Top        : Gsk.Render_Node.Gsk_Render_Node;
       Blend_Mode : Gsk.Enums.Gsk_Blend_Mode);
   --  Creates a `GskRenderNode` that will use Blend_Mode to blend the Top
   --  node onto the Bottom node.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Bottom The bottom node to be drawn
   --  @param Top The node to be blended onto the Bottom node
   --  @param Blend_Mode The blend mode to use

   function Gsk_Blend_Node_New
      (Bottom     : Gsk.Render_Node.Gsk_Render_Node;
       Top        : Gsk.Render_Node.Gsk_Render_Node;
       Blend_Mode : Gsk.Enums.Gsk_Blend_Mode) return Gsk_Blend_Node;
   --  Creates a `GskRenderNode` that will use Blend_Mode to blend the Top
   --  node onto the Bottom node.
   --  @param Bottom The bottom node to be drawn
   --  @param Top The node to be blended onto the Bottom node
   --  @param Blend_Mode The blend mode to use

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_blend_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Blend_Mode
      (Self : Gsk_Blend_Node) return Gsk.Enums.Gsk_Blend_Mode;
   --  Retrieves the blend mode used by Node.
   --  @return the blend mode

   function Get_Bottom_Child
      (Self : Gsk_Blend_Node) return Gsk.Render_Node.Gsk_Render_Node;
   --  Retrieves the bottom `GskRenderNode` child of the Node.
   --  @return the bottom child node. Has transfer-ownership='none'.

   function Get_Top_Child
      (Self : Gsk_Blend_Node) return Gsk.Render_Node.Gsk_Render_Node;
   --  Retrieves the top `GskRenderNode` child of the Node.
   --  @return the top child node. Has transfer-ownership='none'.

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address) return Gsk_Blend_Node_Record;

private
   for Gsk_Blend_Node_Record'External_Tag use "GskBlendNode";
end Gsk.Blend_Node;
