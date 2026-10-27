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

--  A render node for a conic gradient.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Graphene.Point;  use Graphene.Point;
with Graphene.Rect;   use Graphene.Rect;
with Gsk.Render_Node; use Gsk.Render_Node;
with Interfaces.C;    use Interfaces.C;

package Gsk.Conic_Gradient_Node is

   type Gsk_Conic_Gradient_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Conic_Gradient_Node is access all Gsk_Conic_Gradient_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_conic_gradient_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Angle
      (Self : Gsk_Conic_Gradient_Node) return Interfaces.C.C_float;
   --  Retrieves the angle for the gradient in radians, normalized in [0, 2 *
   --  PI].
   --  The angle is starting at the top and going clockwise, as expressed in
   --  the css specification:
   --  angle = 90 - Gsk.Conic_Gradient_Node.Get_Rotation
   --  Since: gtk+ 4.2
   --  @return the angle for the gradient

   function Get_Center
      (Self : Gsk_Conic_Gradient_Node)
       return access constant Graphene.Point.Graphene_Point_T;
   --  Retrieves the center pointer for the gradient.
   --  @return the center point for the gradient

   function Get_Color_Stops
      (Self : Gsk_Conic_Gradient_Node)
       return Gsk.Render_Node.Gsk_Color_Stop_Array;
   --  Retrieves the color stops in the gradient.
   --  @return the color stops in the gradient

   function Get_N_Color_Stops (Self : Gsk_Conic_Gradient_Node) return Gsize;
   --  Retrieves the number of color stops in the gradient.
   --  @return the number of color stops

   function Get_Rotation
      (Self : Gsk_Conic_Gradient_Node) return Interfaces.C.C_float;
   --  Retrieves the rotation for the gradient in degrees.
   --  @return the rotation for the gradient

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Gtk_New
     (Self        : out Gsk_Conic_Gradient_Node;
      Bounds      : not null access Graphene.Rect.Graphene_Rect_T;
      Center      : not null access Graphene.Point.Graphene_Point_T;
      Rotation    : Interfaces.C.C_float;
      Color_Stops : Gsk.Render_Node.Gsk_Color_Stop_Array);
   procedure Initialize
     (Self        : not null access Gsk_Conic_Gradient_Node_Record'Class;
      Bounds      : not null access Graphene.Rect.Graphene_Rect_T;
      Center      : not null access Graphene.Point.Graphene_Point_T;
      Rotation    : Interfaces.C.C_float;
      Color_Stops : Gsk.Render_Node.Gsk_Color_Stop_Array);
   --  Creates a `GskRenderNode` that draws a conic gradient.
   --  The conic gradient starts around Center in the direction of Rotation. A
   --  rotation of 0 means that the gradient points up. Color stops are then
   --  added clockwise.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Bounds the bounds of the node
   --  @param Center the center of the gradient
   --  @param Rotation the rotation of the gradient in degrees
   --  @param Color_Stops a pointer to an array of `GskColorStop` defining the
   --  gradient. The offsets of all color stops must be increasing. The first
   --  stop's offset must be >= 0 and the last stop's offset must be <= 1.

   function Gsk_Conic_Gradient_Node_New
     (Bounds      : not null access Graphene.Rect.Graphene_Rect_T;
      Center      : not null access Graphene.Point.Graphene_Point_T;
      Rotation    : Interfaces.C.C_float;
      Color_Stops : Gsk.Render_Node.Gsk_Color_Stop_Array)
   return Gsk_Conic_Gradient_Node;
   --  Creates a `GskRenderNode` that draws a conic gradient.
   --  The conic gradient starts around Center in the direction of Rotation. A
   --  rotation of 0 means that the gradient points up. Color stops are then
   --  added clockwise.
   --  @param Bounds the bounds of the node
   --  @param Center the center of the gradient
   --  @param Rotation the rotation of the gradient in degrees
   --  @param Color_Stops a pointer to an array of `GskColorStop` defining the
   --  gradient. The offsets of all color stops must be increasing. The first
   --  stop's offset must be >= 0 and the last stop's offset must be <= 1.

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Conic_Gradient_Node_Record;

private
   for Gsk_Conic_Gradient_Node_Record'External_Tag use "GskConicGradientNode";
end Gsk.Conic_Gradient_Node;
