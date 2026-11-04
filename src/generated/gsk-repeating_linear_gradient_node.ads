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

--  A render node for a repeating linear gradient.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Graphene.Point;  use Graphene.Point;
with Graphene.Rect;   use Graphene.Rect;
with Gsk.Render_Node; use Gsk.Render_Node;

package Gsk.Repeating_Linear_Gradient_Node is

   type Gsk_Repeating_Linear_Gradient_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Repeating_Linear_Gradient_Node is access all Gsk_Repeating_Linear_Gradient_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_repeating_linear_gradient_node_get_type");

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Gtk_New
     (Self        : out Gsk_Repeating_Linear_Gradient_Node;
      Bounds      : not null access Graphene.Rect.Graphene_Rect_T;
      Start       : not null access Graphene.Point.Graphene_Point_T;
      The_End     : not null access Graphene.Point.Graphene_Point_T;
      Color_Stops : Gsk.Render_Node.Gsk_Color_Stop_Array);
   procedure Initialize
     (Self        : not null access Gsk_Repeating_Linear_Gradient_Node_Record'Class;
      Bounds      : not null access Graphene.Rect.Graphene_Rect_T;
      Start       : not null access Graphene.Point.Graphene_Point_T;
      The_End     : not null access Graphene.Point.Graphene_Point_T;
      Color_Stops : Gsk.Render_Node.Gsk_Color_Stop_Array);
   --  Creates a `GskRenderNode` that will create a repeating linear gradient
   --  from the given points and color stops, and render that into the area
   --  given by Bounds.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Bounds the rectangle to render the linear gradient into
   --  @param Start the point at which the linear gradient will begin
   --  @param The_End the point at which the linear gradient will finish
   --  @param Color_Stops a pointer to an array of `GskColorStop` defining the
   --  gradient. The offsets of all color stops must be increasing. The first
   --  stop's offset must be >= 0 and the last stop's offset must be <= 1.

   function Gsk_Repeating_Linear_Gradient_Node_New
     (Bounds      : not null access Graphene.Rect.Graphene_Rect_T;
      Start       : not null access Graphene.Point.Graphene_Point_T;
      The_End     : not null access Graphene.Point.Graphene_Point_T;
      Color_Stops : Gsk.Render_Node.Gsk_Color_Stop_Array)
   return Gsk_Repeating_Linear_Gradient_Node;
   --  Creates a `GskRenderNode` that will create a repeating linear gradient
   --  from the given points and color stops, and render that into the area
   --  given by Bounds.
   --  @param Bounds the rectangle to render the linear gradient into
   --  @param Start the point at which the linear gradient will begin
   --  @param The_End the point at which the linear gradient will finish
   --  @param Color_Stops a pointer to an array of `GskColorStop` defining the
   --  gradient. The offsets of all color stops must be increasing. The first
   --  stop's offset must be >= 0 and the last stop's offset must be <= 1.

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Repeating_Linear_Gradient_Node_Record;

private
   for Gsk_Repeating_Linear_Gradient_Node_Record'External_Tag use "GskRepeatingLinearGradientNode";
end Gsk.Repeating_Linear_Gradient_Node;
