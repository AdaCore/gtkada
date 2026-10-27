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

--  A render node for a Cairo surface.

pragma Warnings (Off, "*is already use-visible*");
with Cairo;           use Cairo;
with Glib;            use Glib;
with Graphene.Rect;   use Graphene.Rect;
with Gsk.Render_Node; use Gsk.Render_Node;

package Gsk.Cairo_Node is

   type Gsk_Cairo_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Cairo_Node is access all Gsk_Cairo_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gsk_Cairo_Node;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T);
   procedure Initialize
      (Self   : not null access Gsk_Cairo_Node_Record'Class;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T);
   --  Creates a `GskRenderNode` that will render a cairo surface into the
   --  area given by Bounds.
   --  You can draw to the cairo surface using
   --  [methodGsk.CairoNode.get_draw_context].
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Bounds the rectangle to render to

   function Gsk_Cairo_Node_New
      (Bounds : not null access Graphene.Rect.Graphene_Rect_T)
       return Gsk_Cairo_Node;
   --  Creates a `GskRenderNode` that will render a cairo surface into the
   --  area given by Bounds.
   --  You can draw to the cairo surface using
   --  [methodGsk.CairoNode.get_draw_context].
   --  @param Bounds the rectangle to render to

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_cairo_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Draw_Context
      (Self : Gsk_Cairo_Node) return Cairo.Cairo_Context;
   --  Creates a Cairo context for drawing using the surface associated to the
   --  render node.
   --  If no surface exists yet, a surface will be created optimized for
   --  rendering to Renderer.
   --  @return a Cairo context used for drawing; use cairo_destroy when done
   --  drawing

   function Get_Surface (Self : Gsk_Cairo_Node) return Cairo.Cairo_Surface;
   --  Retrieves the Cairo surface used by the render node.
   --  @return a Cairo surface

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address) return Gsk_Cairo_Node_Record;

private
   for Gsk_Cairo_Node_Record'External_Tag use "GskCairoNode";
end Gsk.Cairo_Node;
