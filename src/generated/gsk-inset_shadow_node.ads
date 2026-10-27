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

--  A render node for an inset shadow.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.RGBA;         use Gdk.RGBA;
with Glib;             use Glib;
with Gsk.Render_Node;  use Gsk.Render_Node;
with Gsk.Rounded_Rect; use Gsk.Rounded_Rect;
with Interfaces.C;     use Interfaces.C;

package Gsk.Inset_Shadow_Node is

   type Gsk_Inset_Shadow_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Inset_Shadow_Node is access all Gsk_Inset_Shadow_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self        : out Gsk_Inset_Shadow_Node;
       Outline     : not null access Gsk.Rounded_Rect.Gsk_Rounded_Rect;
       Color       : not null access Gdk.RGBA.Gdk_RGBA;
       Dx          : Interfaces.C.C_float;
       Dy          : Interfaces.C.C_float;
       Spread      : Interfaces.C.C_float;
       Blur_Radius : Interfaces.C.C_float);
   procedure Initialize
      (Self        : not null access Gsk_Inset_Shadow_Node_Record'Class;
       Outline     : not null access Gsk.Rounded_Rect.Gsk_Rounded_Rect;
       Color       : not null access Gdk.RGBA.Gdk_RGBA;
       Dx          : Interfaces.C.C_float;
       Dy          : Interfaces.C.C_float;
       Spread      : Interfaces.C.C_float;
       Blur_Radius : Interfaces.C.C_float);
   --  Creates a `GskRenderNode` that will render an inset shadow into the box
   --  given by Outline.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Outline outline of the region containing the shadow
   --  @param Color color of the shadow
   --  @param Dx horizontal offset of shadow
   --  @param Dy vertical offset of shadow
   --  @param Spread how far the shadow spreads towards the inside
   --  @param Blur_Radius how much blur to apply to the shadow

   function Gsk_Inset_Shadow_Node_New
      (Outline     : not null access Gsk.Rounded_Rect.Gsk_Rounded_Rect;
       Color       : not null access Gdk.RGBA.Gdk_RGBA;
       Dx          : Interfaces.C.C_float;
       Dy          : Interfaces.C.C_float;
       Spread      : Interfaces.C.C_float;
       Blur_Radius : Interfaces.C.C_float) return Gsk_Inset_Shadow_Node;
   --  Creates a `GskRenderNode` that will render an inset shadow into the box
   --  given by Outline.
   --  @param Outline outline of the region containing the shadow
   --  @param Color color of the shadow
   --  @param Dx horizontal offset of shadow
   --  @param Dy vertical offset of shadow
   --  @param Spread how far the shadow spreads towards the inside
   --  @param Blur_Radius how much blur to apply to the shadow

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_inset_shadow_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Blur_Radius
      (Self : Gsk_Inset_Shadow_Node) return Interfaces.C.C_float;
   --  Retrieves the blur radius to apply to the shadow.
   --  @return the blur radius, in pixels

   function Get_Color
      (Self : Gsk_Inset_Shadow_Node)
       return access constant Gdk.RGBA.Gdk_RGBA;
   --  Retrieves the color of the inset shadow.
   --  The value returned by this function will not be correct if the render
   --  node was created for a non-sRGB color.
   --  @return the color of the shadow

   function Get_Dx
      (Self : Gsk_Inset_Shadow_Node) return Interfaces.C.C_float;
   --  Retrieves the horizontal offset of the inset shadow.
   --  @return an offset, in pixels

   function Get_Dy
      (Self : Gsk_Inset_Shadow_Node) return Interfaces.C.C_float;
   --  Retrieves the vertical offset of the inset shadow.
   --  @return an offset, in pixels

   function Get_Outline
      (Self : Gsk_Inset_Shadow_Node)
       return Gsk.Rounded_Rect.Gsk_Rounded_Rect;
   --  Retrieves the outline rectangle of the inset shadow.
   --  @return a rounded rectangle

   function Get_Spread
      (Self : Gsk_Inset_Shadow_Node) return Interfaces.C.C_float;
   --  Retrieves how much the shadow spreads inwards.
   --  @return the size of the shadow, in pixels

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Inset_Shadow_Node_Record;

private
   for Gsk_Inset_Shadow_Node_Record'External_Tag use "GskInsetShadowNode";
end Gsk.Inset_Shadow_Node;
