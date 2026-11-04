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

--  A render node for a `GdkTexture`.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Texture;     use Gdk.Texture;
with Glib;            use Glib;
with Graphene.Rect;   use Graphene.Rect;
with Gsk.Render_Node; use Gsk.Render_Node;

package Gsk.Texture_Node is

   type Gsk_Texture_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Texture_Node is access all Gsk_Texture_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self    : out Gsk_Texture_Node;
       Texture : not null access Gdk.Texture.Gdk_Texture_Record'Class;
       Bounds  : not null access Graphene.Rect.Graphene_Rect_T);
   procedure Initialize
      (Self    : not null access Gsk_Texture_Node_Record'Class;
       Texture : not null access Gdk.Texture.Gdk_Texture_Record'Class;
       Bounds  : not null access Graphene.Rect.Graphene_Rect_T);
   --  Creates a `GskRenderNode` that will render the given Texture into the
   --  area given by Bounds.
   --  Note that GSK applies linear filtering when textures are scaled and
   --  transformed. See [classGsk.TextureScaleNode] for a way to influence
   --  filtering.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Texture the `GdkTexture`
   --  @param Bounds the rectangle to render the texture into

   function Gsk_Texture_Node_New
      (Texture : not null access Gdk.Texture.Gdk_Texture_Record'Class;
       Bounds  : not null access Graphene.Rect.Graphene_Rect_T)
       return Gsk_Texture_Node;
   --  Creates a `GskRenderNode` that will render the given Texture into the
   --  area given by Bounds.
   --  Note that GSK applies linear filtering when textures are scaled and
   --  transformed. See [classGsk.TextureScaleNode] for a way to influence
   --  filtering.
   --  @param Texture the `GdkTexture`
   --  @param Bounds the rectangle to render the texture into

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_texture_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Texture
      (Self : Gsk_Texture_Node) return Gdk.Texture.Gdk_Texture;
   --  Retrieves the `GdkTexture` used when creating this `GskRenderNode`.
   --  @return the `GdkTexture`. Has transfer-ownership='none'.

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Texture_Node_Record;

private
   for Gsk_Texture_Node_Record'External_Tag use "GskTextureNode";
end Gsk.Texture_Node;
