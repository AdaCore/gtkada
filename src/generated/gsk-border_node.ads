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

--  A render node for a border.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.RGBA;         use Gdk.RGBA;
with Glib;             use Glib;
with Gsk.Render_Node;  use Gsk.Render_Node;
with Gsk.Rounded_Rect; use Gsk.Rounded_Rect;

package Gsk.Border_Node is

   type Gsk_Border_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Border_Node is access all Gsk_Border_Node_Record'Class;

   type Gfloat_Array4 is array (1 .. 4) of Gfloat
   with Convention => C;

   type Gdk_Rgba_Array4 is array (1 .. 4) of Gdk.RGBA.Gdk_RGBA
   with Convention => C;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self         : out Gsk_Border_Node;
       Outline      : Gsk.Rounded_Rect.Gsk_Rounded_Rect;
       Border_Width : Gfloat_Array4;
       Border_Color : Gdk_Rgba_Array4);
   procedure Initialize
      (Self         : not null access Gsk_Border_Node_Record'Class;
       Outline      : Gsk.Rounded_Rect.Gsk_Rounded_Rect;
       Border_Width : Gfloat_Array4;
       Border_Color : Gdk_Rgba_Array4);
   --  Creates a `GskRenderNode` that will stroke a border rectangle inside
   --  the given Outline.
   --  The 4 sides of the border can have different widths and colors.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Outline a `GskRoundedRect` describing the outline of the border
   --  @param Border_Width the stroke width of the border on the top, right,
   --  bottom and left side respectively.
   --  @param Border_Color the color used on the top, right, bottom and left
   --  side.

   function Gsk_Border_Node_New
      (Outline      : Gsk.Rounded_Rect.Gsk_Rounded_Rect;
       Border_Width : Gfloat_Array4;
       Border_Color : Gdk_Rgba_Array4) return Gsk_Border_Node;
   --  Creates a `GskRenderNode` that will stroke a border rectangle inside
   --  the given Outline.
   --  The 4 sides of the border can have different widths and colors.
   --  @param Outline a `GskRoundedRect` describing the outline of the border
   --  @param Border_Width the stroke width of the border on the top, right,
   --  bottom and left side respectively.
   --  @param Border_Color the color used on the top, right, bottom and left
   --  side.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_border_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Colors (Self : Gsk_Border_Node) return Gdk_Rgba_Array4;
   --  Retrieves the colors of the border.
   --  @return an array of 4 `GdkRGBA` structs for the top, right, bottom and
   --  left color of the border

   function Get_Outline
      (Self : Gsk_Border_Node) return Gsk.Rounded_Rect.Gsk_Rounded_Rect;
   --  Retrieves the outline of the border.
   --  @return the outline of the border

   function Get_Widths (Self : Gsk_Border_Node) return Gfloat_Array4;
   --  Retrieves the stroke widths of the border.
   --  @return an array of 4 floats for the top, right, bottom and left stroke
   --  width of the border, respectively

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Border_Node_Record;

private
   for Gsk_Border_Node_Record'External_Tag use "GskBorderNode";
end Gsk.Border_Node;
