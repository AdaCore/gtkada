------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2026, AdaCore                     --
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

--  Utilities for manipulating font faces.
--
--  <c_version>1.8.8</c_version>
--  <group>Cairo</group>

with Gtkada.Types;

package Cairo.Scaled_Font is

   function Reference
     (Scaled_Font : Cairo_Scaled_Font)
      return        Cairo_Scaled_Font;
   --  Increases the reference count on scaled_font by one. This prevents
   --  scaled_font from being destroyed until a matching call to
   --  Cairo.Scaled_Font.Destroy is made.
   --
   --  The number of references to a Cairo_Scaled_Font can be get using
   --  Cairo.Scaled_Font.Get_Reference_Count.
   --
   --  @param Scaled_Font a Cairo.Scaled_Font.T, (may be null in which case
   --  this function does nothing)
   --  @return the referenced Cairo_Scaled_Font

   procedure Destroy (Scaled_Font : Cairo_Scaled_Font);
   --  Decreases the reference count on font by one. If the result
   --  is zero, then font and all associated resources are freed.
   --  See Cairo.Scaled_Font.Reference.
   --
   --  @param Scaled_Font a Cairo_Scaled_Font

   function Get_Reference_Count
     (Scaled_Font : Cairo_Scaled_Font)
      return        Guint;
   --  Returns the current reference count of scaled_font.
   --
   --  @since 1.4
   --  @param Scaled_Font a Cairo_Scaled_Font
   --  @return the current reference count of scaled_font. If the object is a
   --  nil object, 0 will be returned.

   function Status (Scaled_Font : Cairo_Scaled_Font) return Cairo_Status;
   --  Checks whether an error has previously occurred for this
   --  scaled_font.
   --
   --  @param Scaled_Font a Cairo_Scaled_Font
   --  @return Cairo_Status_Success or another error such as
   --  Cairo_Status_No_Memory.

   function Get_Type
     (Scaled_Font : Cairo_Scaled_Font)
      return        Cairo_Font_Type;
   --  This function returns the type of the backend used to create
   --  a scaled font. See Cairo_Font_Type for available types.
   --
   --  @since 1.2
   --  @param Scaled_Font a Cairo_Scaled_Font
   --  @return The type of scaled_font.

   procedure Extents
     (Scaled_Font : Cairo_Scaled_Font;
      Extents     : access Cairo_Font_Extents);
   --  Gets the metrics for a Cairo_Scaled_Font.
   --
   --  @param Scaled_Font a Cairo_Scaled_Font
   --  @param Extents a Cairo_Font_Extents which to store the retrieved
   --  Extents.

   procedure Text_Extents
     (Scaled_Font : Cairo_Scaled_Font;
      Utf8        : Gtkada.Types.Chars_Ptr;
      Extents     : access Cairo_Text_Extents);
   --  Gets the extents for a string of text. The extents describe a
   --  user-space rectangle that encloses the "inked" portion of the text
   --  drawn at the origin (0,0) (as it would be drawn by Cairo_Show_Text
   --  if the cairo graphics state were set to the same font_face,
   --  font_matrix, ctm, and font_options as scaled_font).  Additionally,
   --  the x_advance and y_advance values indicate the amount by which the
   --  current point would be advanced by Cairo_Show_Text.
   --
   --  Note that whitespace characters do not directly contribute to the
   --  size of the rectangle (extents.width and extents.height). They do
   --  contribute indirectly by changing the position of non-whitespace
   --  characters. In particular, trailing whitespace characters are
   --  likely to not affect the size of the rectangle, though they will
   --  affect the x_advance and y_advance values.
   --
   --  @since 1.2
   --  @param Scaled_Font a Cairo_Scaled_Font
   --  @param Utf8 a NUL-terminated string of text, encoded in UTF-8
   --  @param Extents a Cairo_Text_Extents which to store the retrieved
   --  Extents.

   procedure Glyph_Extents
     (Scaled_Font : Cairo_Scaled_Font;
      Glyphs      : access Cairo_Glyph;
      Num_Glyphs  : Gint;
      Extents     : access Cairo_Text_Extents);
   --  Gets the extents for an array of glyphs. The extents describe a
   --  user-space rectangle that encloses the "inked" portion of the
   --  glyphs, (as they would be drawn by Cairo.Show_Glyphs if the cairo
   --  graphics state were set to the same Font_Face, Font_Matrix, Ctm,
   --  and Font_Options as Scaled_Font).  Additionally, the x_advance and
   --  Y_Advance values indicate the amount by which the current point
   --  would be advanced by Cairo.Show_Glyphs.
   --
   --  Note that whitespace glyphs do not contribute to the size of the
   --  rectangle (extents.width and extents.height).
   --
   --  @param Scaled_Font a Cairo_Scaled_Font
   --  @param Glyphs an array of glyph IDs with X and Y offsets.
   --  @param Num_Glyphs the number of glyphs in the glyphs array
   --  @param Extents a Cairo_Text_Extents which to store the retrieved
   --  Extents.

   function Get_Font_Face
     (Scaled_Font : Cairo_Scaled_Font)
      return        Cairo_Font_Face;
   --  Gets the font face that this scaled font was created for.
   --
   --  @since 1.2
   --  @param Scaled_Font a Cairo_Scaled_Font
   --  @return The Cairo_Font_Face with which Scaled_Font was created.

   procedure Get_Font_Matrix
     (Scaled_Font : Cairo_Scaled_Font;
      Font_Matrix : access Cairo_Matrix);
   --  Stores the font matrix with which Scaled_Font was created into
   --  matrix.
   --
   --  @since 1.2
   --  @param Scaled_Font a Cairo_Scaled_Font
   --  @param Font_Matrix return value for the matrix

   procedure Get_Ctm
     (Scaled_Font : Cairo_Scaled_Font;
      Ctm         : access Cairo_Matrix);
   --  Stores the CTM with which Scaled_Font was created into Ctm.
   --
   --  @since 1.2
   --  @param Scaled_Font a Cairo_Scaled_Font
   --  @param Ctm return value for the CTM

   procedure Get_Scale_Matrix
     (Scaled_Font  : Cairo_Scaled_Font;
      Scale_Matrix : access Cairo_Matrix);
   --  Stores the scale matrix of scaled_font into matrix.
   --  The scale matrix is product of the font matrix and the ctm
   --  associated with the scaled font, and hence is the matrix mapping from
   --  font space to device space.
   --
   --  @since 1.8
   --  @param Scaled_Font a Cairo_Scaled_Font
   --  @param Scale_Matrix return value for the matrix

   procedure Get_Font_Options
     (Scaled_Font : Cairo_Scaled_Font;
      Options     : Cairo_Font_Options);
   --  Stores the font options with which scaled_font was created into
   --  options.
   --
   --  @since 1.2
   --  @param Scaled_Font a Cairo_Scaled_Font
   --  @param Options return value for the font Options

private

   pragma Import (C, Reference, "cairo_scaled_font_reference");
   pragma Import (C, Destroy, "cairo_scaled_font_destroy");
   pragma Import
     (C,
      Get_Reference_Count,
      "cairo_scaled_font_get_reference_count");
   pragma Import (C, Status, "cairo_scaled_font_status");
   pragma Import (C, Get_Type, "cairo_scaled_font_get_type");
   pragma Import (C, Extents, "cairo_scaled_font_extents");
   pragma Import (C, Text_Extents, "cairo_scaled_font_text_extents");
   pragma Import (C, Glyph_Extents, "cairo_scaled_font_glyph_extents");
   pragma Import (C, Get_Font_Face, "cairo_scaled_font_get_font_face");
   pragma Import (C, Get_Font_Matrix, "cairo_scaled_font_get_font_matrix");
   pragma Import (C, Get_Ctm, "cairo_scaled_font_get_ctm");
   pragma Import (C, Get_Scale_Matrix, "cairo_scaled_font_get_scale_matrix");
   pragma Import (C, Get_Font_Options, "cairo_scaled_font_get_font_options");

   --  Not bound :
--     pragma Import (C, Text_To_Glyphs, "cairo_scaled_font_text_to_glyphs");
--     pragma Import (C, Create, "cairo_scaled_font_create");
--     pragma Import (C, Get_User_Data, "cairo_scaled_font_get_user_data");
--     pragma Import (C, Set_User_Data, "cairo_scaled_font_set_user_data");
end Cairo.Scaled_Font;
