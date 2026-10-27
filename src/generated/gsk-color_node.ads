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

--  A render node for a solid color.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.RGBA;        use Gdk.RGBA;
with Glib;            use Glib;
with Graphene.Rect;   use Graphene.Rect;
with Gsk.Render_Node; use Gsk.Render_Node;

package Gsk.Color_Node is

   type Gsk_Color_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Color_Node is access all Gsk_Color_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gsk_Color_Node;
       Rgba   : not null access Gdk.RGBA.Gdk_RGBA;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T);
   procedure Initialize
      (Self   : not null access Gsk_Color_Node_Record'Class;
       Rgba   : not null access Gdk.RGBA.Gdk_RGBA;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T);
   --  Creates a `GskRenderNode` that will render the color specified by Rgba
   --  into the area given by Bounds.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Rgba a `GdkRGBA` specifying a color
   --  @param Bounds the rectangle to render the color into

   function Gsk_Color_Node_New
      (Rgba   : not null access Gdk.RGBA.Gdk_RGBA;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T)
       return Gsk_Color_Node;
   --  Creates a `GskRenderNode` that will render the color specified by Rgba
   --  into the area given by Bounds.
   --  @param Rgba a `GdkRGBA` specifying a color
   --  @param Bounds the rectangle to render the color into

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_color_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Color
      (Self : Gsk_Color_Node) return access constant Gdk.RGBA.Gdk_RGBA;
   --  Retrieves the color of the given Node.
   --  The value returned by this function will not be correct if the render
   --  node was created for a non-sRGB color.
   --  @return the color of the node

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address) return Gsk_Color_Node_Record;

private
   for Gsk_Color_Node_Record'External_Tag use "GskColorNode";
end Gsk.Color_Node;
