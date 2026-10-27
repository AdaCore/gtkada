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

--  A render node for a radial gradient.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Graphene.Point;  use Graphene.Point;
with Graphene.Rect;   use Graphene.Rect;
with Gsk.Render_Node; use Gsk.Render_Node;
with Interfaces.C;    use Interfaces.C;

package Gsk.Radial_Gradient_Node is

   type Gsk_Radial_Gradient_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Radial_Gradient_Node is access all Gsk_Radial_Gradient_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_radial_gradient_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Center
      (Self : Gsk_Radial_Gradient_Node)
       return access constant Graphene.Point.Graphene_Point_T;
   --  Retrieves the center pointer for the gradient.
   --  @return the center point for the gradient

   function Get_Color_Stops
      (Self : Gsk_Radial_Gradient_Node)
       return Gsk.Render_Node.Gsk_Color_Stop_Array;
   --  Retrieves the color stops in the gradient.
   --  @return the color stops in the gradient

   function Get_End
      (Self : Gsk_Radial_Gradient_Node) return Interfaces.C.C_float;
   --  Retrieves the end value for the gradient.
   --  @return the end value for the gradient

   function Get_Hradius
      (Self : Gsk_Radial_Gradient_Node) return Interfaces.C.C_float;
   --  Retrieves the horizontal radius for the gradient.
   --  @return the horizontal radius for the gradient

   function Get_N_Color_Stops (Self : Gsk_Radial_Gradient_Node) return Gsize;
   --  Retrieves the number of color stops in the gradient.
   --  @return the number of color stops

   function Get_Start
      (Self : Gsk_Radial_Gradient_Node) return Interfaces.C.C_float;
   --  Retrieves the start value for the gradient.
   --  @return the start value for the gradient

   function Get_Vradius
      (Self : Gsk_Radial_Gradient_Node) return Interfaces.C.C_float;
   --  Retrieves the vertical radius for the gradient.
   --  @return the vertical radius for the gradient

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Gtk_New
     (Self        : out Gsk_Radial_Gradient_Node;
      Bounds      : not null access Graphene.Rect.Graphene_Rect_T;
      Center      : not null access Graphene.Point.Graphene_Point_T;
      Hradius     : Interfaces.C.C_float;
      Vradius     : Interfaces.C.C_float;
      Start       : Interfaces.C.C_float;
      The_End     : Interfaces.C.C_float;
      Color_Stops : Gsk.Render_Node.Gsk_Color_Stop_Array);
   procedure Initialize
     (Self        : not null access Gsk_Radial_Gradient_Node_Record'Class;
      Bounds      : not null access Graphene.Rect.Graphene_Rect_T;
      Center      : not null access Graphene.Point.Graphene_Point_T;
      Hradius     : Interfaces.C.C_float;
      Vradius     : Interfaces.C.C_float;
      Start       : Interfaces.C.C_float;
      The_End     : Interfaces.C.C_float;
      Color_Stops : Gsk.Render_Node.Gsk_Color_Stop_Array);
   --  Creates a `GskRenderNode` that draws a radial gradient.
   --  The radial gradient starts around Center. The size of the gradient is
   --  dictated by Hradius in horizontal orientation and by Vradius in vertical
   --  orientation.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Bounds the bounds of the node
   --  @param Center the center of the gradient
   --  @param Hradius the horizontal radius
   --  @param Vradius the vertical radius
   --  @param Start a percentage >= 0 that defines the start of the gradient
   --  around Center
   --  @param The_End a percentage >= 0 that defines the end of the gradient
   --  around Center
   --  @param Color_Stops a pointer to an array of `GskColorStop` defining the
   --  gradient. The offsets of all color stops must be increasing. The first
   --  stop's offset must be >= 0 and the last stop's offset must be <= 1.

   function Gsk_Radial_Gradient_Node_New
     (Bounds        : not null access Graphene.Rect.Graphene_Rect_T;
      Center        : not null access Graphene.Point.Graphene_Point_T;
      Hradius       : Interfaces.C.C_float;
      Vradius       : Interfaces.C.C_float;
      Start         : Interfaces.C.C_float;
      The_End       : Interfaces.C.C_float;
      Color_Stops   : Gsk.Render_Node.Gsk_Color_Stop_Array)
   return Gsk_Radial_Gradient_Node;
   --  Creates a `GskRenderNode` that draws a radial gradient.
   --  The radial gradient starts around Center. The size of the gradient is
   --  dictated by Hradius in horizontal orientation and by Vradius in vertical
   --  orientation.
   --  @param Bounds the bounds of the node
   --  @param Center the center of the gradient
   --  @param Hradius the horizontal radius
   --  @param Vradius the vertical radius
   --  @param Start a percentage >= 0 that defines the start of the gradient
   --  around Center
   --  @param The_End a percentage >= 0 that defines the end of the gradient
   --  around Center
   --  @param Color_Stops a pointer to an array of `GskColorStop` defining the
   --  gradient. The offsets of all color stops must be increasing. The first
   --  stop's offset must be >= 0 and the last stop's offset must be <= 1.

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Radial_Gradient_Node_Record;

private
   for Gsk_Radial_Gradient_Node_Record'External_Tag use "GskRadialGradientNode";
end Gsk.Radial_Gradient_Node;
