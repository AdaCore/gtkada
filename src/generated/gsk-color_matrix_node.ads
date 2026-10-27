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

--  A render node controlling the color matrix of its single child node.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Graphene.Matrix; use Graphene.Matrix;
with Graphene.Vec4;   use Graphene.Vec4;
with Gsk.Render_Node; use Gsk.Render_Node;

package Gsk.Color_Matrix_Node is

   type Gsk_Color_Matrix_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Color_Matrix_Node is access all Gsk_Color_Matrix_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self         : out Gsk_Color_Matrix_Node;
       Child        : Gsk.Render_Node.Gsk_Render_Node;
       Color_Matrix : not null access Graphene.Matrix.Graphene_Matrix_T;
       Color_Offset : not null access Graphene.Vec4.Graphene_Vec4_T);
   procedure Initialize
      (Self         : not null access Gsk_Color_Matrix_Node_Record'Class;
       Child        : Gsk.Render_Node.Gsk_Render_Node;
       Color_Matrix : not null access Graphene.Matrix.Graphene_Matrix_T;
       Color_Offset : not null access Graphene.Vec4.Graphene_Vec4_T);
   --  Creates a `GskRenderNode` that will drawn the Child with Color_Matrix.
   --  In particular, the node will transform colors by applying
   --  pixel = transpose(color_matrix) * pixel + color_offset
   --  for every pixel. The transformation operates on unpremultiplied colors,
   --  with color components ordered R, G, B, A.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Child The node to draw
   --  @param Color_Matrix The matrix to apply
   --  @param Color_Offset Values to add to the color

   function Gsk_Color_Matrix_Node_New
      (Child        : Gsk.Render_Node.Gsk_Render_Node;
       Color_Matrix : not null access Graphene.Matrix.Graphene_Matrix_T;
       Color_Offset : not null access Graphene.Vec4.Graphene_Vec4_T)
       return Gsk_Color_Matrix_Node;
   --  Creates a `GskRenderNode` that will drawn the Child with Color_Matrix.
   --  In particular, the node will transform colors by applying
   --  pixel = transpose(color_matrix) * pixel + color_offset
   --  for every pixel. The transformation operates on unpremultiplied colors,
   --  with color components ordered R, G, B, A.
   --  @param Child The node to draw
   --  @param Color_Matrix The matrix to apply
   --  @param Color_Offset Values to add to the color

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_color_matrix_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : Gsk_Color_Matrix_Node) return Gsk.Render_Node.Gsk_Render_Node;
   --  Gets the child node that is getting its colors modified by the given
   --  Node.
   --  @return The child that is getting its colors modified. Has
   --  transfer-ownership='none'.

   function Get_Color_Matrix
      (Self : Gsk_Color_Matrix_Node)
       return access constant Graphene.Matrix.Graphene_Matrix_T;
   --  Retrieves the color matrix used by the Node.
   --  @return a 4x4 color matrix

   function Get_Color_Offset
      (Self : Gsk_Color_Matrix_Node)
       return access constant Graphene.Vec4.Graphene_Vec4_T;
   --  Retrieves the color offset used by the Node.
   --  @return a color vector

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Color_Matrix_Node_Record;

private
   for Gsk_Color_Matrix_Node_Record'External_Tag use "GskColorMatrixNode";
end Gsk.Color_Matrix_Node;
