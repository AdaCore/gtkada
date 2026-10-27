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

--  A render node drawing a set of glyphs.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.RGBA;           use Gdk.RGBA;
with Glib;               use Glib;
with Graphene.Point;     use Graphene.Point;
with Gsk.Render_Node;    use Gsk.Render_Node;
with Pango.Font;         use Pango.Font;
with Pango.Glyph_String; use Pango.Glyph_String;

package Gsk.Text_Node is

   type Gsk_Text_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Text_Node is access all Gsk_Text_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gsk_Text_Node;
       Font   : not null access Pango.Font.Pango_Font_Record'Class;
       Glyphs : not null access Pango.Glyph_String.Pango_Glyph_String;
       Color  : not null access Gdk.RGBA.Gdk_RGBA;
       Offset : not null access Graphene.Point.Graphene_Point_T);
   procedure Initialize
      (Self   : not null access Gsk_Text_Node_Record'Class;
       Font   : not null access Pango.Font.Pango_Font_Record'Class;
       Glyphs : not null access Pango.Glyph_String.Pango_Glyph_String;
       Color  : not null access Gdk.RGBA.Gdk_RGBA;
       Offset : not null access Graphene.Point.Graphene_Point_T);
   --  Creates a render node that renders the given glyphs.
   --  Note that Color may not be used if the font contains color glyphs.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Font the `PangoFont` containing the glyphs
   --  @param Glyphs the `PangoGlyphString` to render
   --  @param Color the foreground color to render with
   --  @param Offset offset of the baseline

   function Gsk_Text_Node_New
      (Font   : not null access Pango.Font.Pango_Font_Record'Class;
       Glyphs : not null access Pango.Glyph_String.Pango_Glyph_String;
       Color  : not null access Gdk.RGBA.Gdk_RGBA;
       Offset : not null access Graphene.Point.Graphene_Point_T)
       return Gsk_Text_Node;
   --  Creates a render node that renders the given glyphs.
   --  Note that Color may not be used if the font contains color glyphs.
   --  @param Font the `PangoFont` containing the glyphs
   --  @param Glyphs the `PangoGlyphString` to render
   --  @param Color the foreground color to render with
   --  @param Offset offset of the baseline

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_text_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Color
      (Self : Gsk_Text_Node) return access constant Gdk.RGBA.Gdk_RGBA;
   --  Retrieves the color used by the text Node.
   --  The value returned by this function will not be correct if the render
   --  node was created for a non-sRGB color.
   --  @return the text color

   function Get_Font (Self : Gsk_Text_Node) return Pango.Font.Pango_Font;
   --  Returns the font used by the text Node.
   --  @return the font. Has transfer-ownership='none'.

   function Get_Glyphs
      (Self : Gsk_Text_Node)
       return Pango.Glyph_String.Pango_Glyph_Info_Array;
   --  Retrieves the glyph information in the Node.
   --  @return the glyph information

   function Get_Num_Glyphs (Self : Gsk_Text_Node) return Guint;
   --  Retrieves the number of glyphs in the text node.
   --  @return the number of glyphs

   function Get_Offset
      (Self : Gsk_Text_Node)
       return access constant Graphene.Point.Graphene_Point_T;
   --  Retrieves the offset applied to the text.
   --  @return a point with the horizontal and vertical offsets

   function Has_Color_Glyphs (Self : Gsk_Text_Node) return Boolean;
   --  Checks whether the text Node has color glyphs.
   --  Since: gtk+ 4.2
   --  @return True if the text node has color glyphs

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address) return Gsk_Text_Node_Record;

private
   for Gsk_Text_Node_Record'External_Tag use "GskTextNode";
end Gsk.Text_Node;
