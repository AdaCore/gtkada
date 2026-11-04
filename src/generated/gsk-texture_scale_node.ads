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

--  A render node for a `GdkTexture`, with control over scaling.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Texture;     use Gdk.Texture;
with Glib;            use Glib;
with Graphene.Rect;   use Graphene.Rect;
with Gsk.Enums;       use Gsk.Enums;
with Gsk.Render_Node; use Gsk.Render_Node;

package Gsk.Texture_Scale_Node is

   type Gsk_Texture_Scale_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Texture_Scale_Node is access all Gsk_Texture_Scale_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self    : out Gsk_Texture_Scale_Node;
       Texture : not null access Gdk.Texture.Gdk_Texture_Record'Class;
       Bounds  : not null access Graphene.Rect.Graphene_Rect_T;
       Filter  : Gsk.Enums.Gsk_Scaling_Filter);
   procedure Initialize
      (Self    : not null access Gsk_Texture_Scale_Node_Record'Class;
       Texture : not null access Gdk.Texture.Gdk_Texture_Record'Class;
       Bounds  : not null access Graphene.Rect.Graphene_Rect_T;
       Filter  : Gsk.Enums.Gsk_Scaling_Filter);
   --  Creates a node that scales the texture to the size given by the bounds
   --  using the filter and then places it at the bounds' position.
   --  Note that further scaling and other transformations which are applied
   --  to the node will apply linear filtering to the resulting texture, as
   --  usual.
   --  This node is intended for tight control over scaling applied to a
   --  texture, such as in image editors and requires the application to be
   --  aware of the whole render tree as further transforms may be applied that
   --  conflict with the desired effect of this node.
   --  Since: gtk+ 4.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Texture the texture to scale
   --  @param Bounds the size of the texture to scale to
   --  @param Filter how to scale the texture

   function Gsk_Texture_Scale_Node_New
      (Texture : not null access Gdk.Texture.Gdk_Texture_Record'Class;
       Bounds  : not null access Graphene.Rect.Graphene_Rect_T;
       Filter  : Gsk.Enums.Gsk_Scaling_Filter) return Gsk_Texture_Scale_Node;
   --  Creates a node that scales the texture to the size given by the bounds
   --  using the filter and then places it at the bounds' position.
   --  Note that further scaling and other transformations which are applied
   --  to the node will apply linear filtering to the resulting texture, as
   --  usual.
   --  This node is intended for tight control over scaling applied to a
   --  texture, such as in image editors and requires the application to be
   --  aware of the whole render tree as further transforms may be applied that
   --  conflict with the desired effect of this node.
   --  Since: gtk+ 4.10
   --  @param Texture the texture to scale
   --  @param Bounds the size of the texture to scale to
   --  @param Filter how to scale the texture

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_texture_scale_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Filter
      (Self : Gsk_Texture_Scale_Node) return Gsk.Enums.Gsk_Scaling_Filter;
   --  Retrieves the `GskScalingFilter` used when creating this
   --  `GskRenderNode`.
   --  Since: gtk+ 4.10
   --  @return the `GskScalingFilter`

   function Get_Texture
      (Self : Gsk_Texture_Scale_Node) return Gdk.Texture.Gdk_Texture;
   --  Retrieves the `GdkTexture` used when creating this `GskRenderNode`.
   --  Since: gtk+ 4.10
   --  @return the `GdkTexture`. Has transfer-ownership='none'.

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Texture_Scale_Node_Record;

private
   for Gsk_Texture_Scale_Node_Record'External_Tag use "GskTextureScaleNode";
end Gsk.Texture_Scale_Node;
