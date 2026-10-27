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

--  A render node controlling the opacity of its single child node.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Gsk.Render_Node; use Gsk.Render_Node;
with Interfaces.C;    use Interfaces.C;

package Gsk.Opacity_Node is

   type Gsk_Opacity_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Opacity_Node is access all Gsk_Opacity_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self    : out Gsk_Opacity_Node;
       Child   : Gsk.Render_Node.Gsk_Render_Node;
       Opacity : Interfaces.C.C_float);
   procedure Initialize
      (Self    : not null access Gsk_Opacity_Node_Record'Class;
       Child   : Gsk.Render_Node.Gsk_Render_Node;
       Opacity : Interfaces.C.C_float);
   --  Creates a `GskRenderNode` that will drawn the Child with reduced
   --  Opacity.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Child The node to draw
   --  @param Opacity The opacity to apply

   function Gsk_Opacity_Node_New
      (Child   : Gsk.Render_Node.Gsk_Render_Node;
       Opacity : Interfaces.C.C_float) return Gsk_Opacity_Node;
   --  Creates a `GskRenderNode` that will drawn the Child with reduced
   --  Opacity.
   --  @param Child The node to draw
   --  @param Opacity The opacity to apply

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_opacity_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : Gsk_Opacity_Node) return Gsk.Render_Node.Gsk_Render_Node;
   --  Gets the child node that is getting opacityed by the given Node.
   --  @return The child that is getting opacityed. Has
   --  transfer-ownership='none'.

   function Get_Opacity
      (Self : Gsk_Opacity_Node) return Interfaces.C.C_float;
   --  Gets the transparency factor for an opacity node.
   --  @return the opacity factor

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Opacity_Node_Record;

private
   for Gsk_Opacity_Node_Record'External_Tag use "GskOpacityNode";
end Gsk.Opacity_Node;
