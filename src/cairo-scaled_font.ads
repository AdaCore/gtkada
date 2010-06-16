-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

pragma Ada_2005;

with System;
with Interfaces.C.Strings;

with Glib; use Glib;

package Cairo.Scaled_Font is

   function Create
     (Font_Face   : Cairo_Font_Face;
      Font_Matrix : access constant Cairo_Matrix;
      Ctm         : access constant Cairo_Matrix;
      Options     : access constant Cairo_Font_Options)
      return        Cairo_Scaled_Font;
   --  Font_Face: a Cairo_Font_Face
   --  Font_Matrix: font space to user space transformation matrix for the
   --        font. In the simplest case of a N point font, this matrix is
   --        just a scale by N, but it can also be used to shear the font
   --        or stretch it unequally along the two axes. See
   --        Cairo_Set_Font_Matrix.
   --  Ctm: user to device transformation matrix with which the font will
   --        be used.
   --  Options: Options to use when getting metrics for the font and
   --            rendering with it.
   --
   --  Creates a Cairo_Scaled_Font object from a font face and matrices that
   --  describe the size of the font and the environment in which it will
   --  be used.
   --
   --  Return value: a newly created Cairo_Scaled_Font. Destroy with
   --   Cairo.Scaled_Font.Destroy

   function Reference
     (Scaled_Font : Cairo_Scaled_Font)
      return        Cairo_Scaled_Font;
   --  Scaled_Font: a Cairo.Scaled_Font.T, (may be NULL in which case
   --  this function does nothing)
   --
   --  Increases the reference count on scaled_font by one. This prevents
   --  scaled_font from being destroyed until a matching call to
   --  Cairo.Scaled_Font.Destroy is made.
   --
   --  The number of references to a Cairo_Scaled_Font can be get using
   --  Cairo.Scaled_Font.Get_Reference_Count.
   --
   --  Returns: the referenced Cairo_Scaled_Font

   procedure Destroy (Scaled_Font : Cairo_Scaled_Font);
   --  Scaled_Font: a Cairo_Scaled_Font
   --
   --  Decreases the reference count on font by one. If the result
   --  is zero, then font and all associated resources are freed.
   --  See Cairo.Scaled_Font.Reference.

   function Get_Reference_Count
     (Scaled_Font : Cairo_Scaled_Font)
      return        Guint;
   --  Scaled_Font: a Cairo_Scaled_Font
   --
   --  Returns the current reference count of scaled_font.
   --
   --  Return value: the current reference count of scaled_font.  If the
   --  object is a nil object, 0 will be returned.
   --
   --  Since: 1.4

   function Status (Scaled_Font : Cairo_Scaled_Font) return Cairo_Status;
   --  Scaled_Font: a Cairo_Scaled_Font
   --
   --  Checks whether an error has previously occurred for this
   --  scaled_font.
   --
   --  Return value: CAIRO_STATUS_SUCCESS or another error such as
   --    CAIRO_STATUS_NO_MEMORY.

   function Get_Type
     (Scaled_Font : Cairo_Scaled_Font)
      return        Cairo_Font_Type;
   --  Scaled_Font: a Cairo_Scaled_Font
   --
   --  This function returns the type of the backend used to create
   --  a scaled font. See Cairo_Font_Type for available types.
   --
   --  Return value: The type of scaled_font.
   --
   --  Since: 1.2

   function Get_User_Data
     (Scaled_Font : Cairo_Scaled_Font;
      Key         : access constant Cairo_User_Data_Key)
      return        System.Address;
   --  Scaled_Font: a Cairo_Scaled_Font
   --  Key: the address of the Cairo_User_Data_Key the user data was
   --  attached to
   --
   --  Return user data previously attached to scaled_font using the
   --  specified key.  If no user data has been attached with the given
   --  key this function returns NULL.
   --
   --  Return value: the user data previously attached or NULL.
   --
   --  Since: 1.4

   function Set_User_Data
     (Scaled_Font : Cairo_Scaled_Font;
      Key         : access constant Cairo_User_Data_Key;
      User_Data   : System.Address;
      Destroy     : access procedure (Arg1 : System.Address))
      return        Cairo_Status;
   --  Scaled_Font: a Cairo_Scaled_Font
   --  Key: the address of a Cairo_User_Data_Key to attach the user data to
   --  User_Data: the user data to attach to the Cairo_Scaled_Font
   --  Destroy: a Cairo_Destroy_Func which will be called when the
   --  Cairo_Context is destroyed or when new user data is attached using the
   --  same key.
   --
   --  Attach user data to scaled_font.  To remove user data from a surface,
   --  call this function with the key that was used to set it and NULL
   --  for data.
   --
   --  Return value: CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a
   --  slot could not be allocated for the user data.
   --
   --  Since: 1.4

   procedure Extents
     (Scaled_Font : Cairo_Scaled_Font;
      Extents     : access Cairo_Font_Extents);
   --  Scaled_Font: a Cairo_Scaled_Font
   --  Extents: a Cairo_Font_Extents which to store the retrieved Extents.
   --
   --  Gets the metrics for a Cairo_Scaled_Font.

   procedure Text_Extents
     (Scaled_Font : Cairo_Scaled_Font;
      Utf8        : Interfaces.C.Strings.chars_ptr;
      Extents     : Cairo_Text_Extents);
   --  Scaled_Font: a Cairo_Scaled_Font
   --  Utf8: a NUL-terminated string of text, encoded in UTF-8
   --  Extents: a Cairo_Text_Extents which to store the retrieved Extents.
   --
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
   --  Since: 1.2

   procedure Glyph_Extents
     (Scaled_Font : Cairo_Scaled_Font;
      Glyphs      : access constant Cairo_Glyph;
      Num_Glyphs  : Gint;
      Extents     : Cairo_Text_Extents);
   --  Scaled_Font: a Cairo_Scaled_Font
   --  Glyphs: an array of glyph IDs with X and Y offsets.
   --  Num_Glyphs: the number of glyphs in the glyphs array
   --  Extents: a Cairo_Text_Extents which to store the retrieved Extents.
   --
   --  Gets the extents for an array of glyphs. The extents describe a
   --  user-space rectangle that encloses the "inked" portion of the
   --  glyphs, (as they would be drawn by Cairo_Show_Glyphs if the cairo
   --  graphics state were set to the same font_face, font_matrix, ctm,
   --  and font_options as scaled_font).  Additionally, the x_advance and
   --  y_advance values indicate the amount by which the current point
   --  would be advanced by Cairo_Show_Glyphs.
   --
   --  Note that whitespace glyphs do not contribute to the size of the
   --  rectangle (extents.width and extents.height).

   function Text_To_Glyphs
     (Scaled_Font   : Cairo_Scaled_Font;
      X             : Gdouble;
      Y             : Gdouble;
      Utf8          : Interfaces.C.Strings.chars_ptr;
      Utf8_Len      : Gint;
      Glyphs        : System.Address;
      Num_Glyphs    : access Gint;
      Clusters      : System.Address;
      Num_Clusters  : access Gint;
      Cluster_Flags : Cairo_Text_Cluster_Flags)
      return          Cairo_Status;
   --  X: X position to place first glyph
   --  Y: Y position to place first glYph
   --  Scaled_Font: a Cairo_Scaled_Font
   --  Utf8: a string of text encoded in UTF-8
   --  Utf8_Len: length of utf8 in bytes, or -1 if it is NUL-terminated
   --  Glyphs: pointer to array of Glyphs to fill
   --  Num_Glyphs: pointer to number of glyphs
   --  Clusters: pointer to array of cluster mapping information to fill, or
   --NULL
   --  Num_Clusters: pointer to number of clusters, or NULL
   --  Cluster_Flags: pointer to location to store cluster flags corresponding
   --to the
   --                  output clusters, or NULL
   --
   --  Converts UTF-8 text to an array of glyphs, optionally with cluster
   --  mapping, that can be used to render later using scaled_font.
   --
   --  If glyphs initially points to a non-NULL value, that array is used
   --  as a glyph buffer, and num_glyphs should point to the number of glyph
   --  entries available there.  If the provided glyph array is too short for
   --  the conversion, a new glyph array is allocated using
   --Cairo_Glyph_Allocate
   --  and placed in glyphs.  Upon return, num_glyphs always contains the
   --  number of generated glyphs.  If the value glyphs points to has changed
   --  after the call, the user is responsible for freeing the allocated glyph
   --  array using Cairo_Glyph_Free.  This may happen even if the provided
   --  array was large enough.
   --
   --  If clusters is not NULL, num_clusters and cluster_flags should not be
   --NULL,
   --  and cluster mapping will be computed.
   --  The semantics of how cluster array allocation works is similar to the
   --glyph
   --  array.  That is,
   --  if clusters initially points to a non-NULL value, that array is used
   --  as a cluster buffer, and num_clusters should point to the number of
   --cluster
   --  entries available there.  If the provided cluster array is too short for
   --  the conversion, a new cluster array is allocated using
   --Cairo_Text_Cluster_Allocate
   --  and placed in clusters.  Upon return, num_clusters always contains the
   --  number of generated clusters.  If the value clusters points at has
   --changed
   --  after the call, the user is responsible for freeing the allocated
   --cluster
   --  array using Cairo_Text_Cluster_Free.  This may happen even if the
   --provided
   --  array was large enough.
   --
   --  In the simplest case, glyphs and clusters can point to NULL initially
   --  and a suitable array will be allocated.  In code:
   --  <informalexample><programlisting>
   --  Cairo_Status status;
   --
   --  Cairo_Glyph *glyphs = NULL;
   --  int num_glyphs;
   --  Cairo_Text_Cluster *clusters = NULL;
   --  int num_clusters;
   --  Cairo_Text_Cluster_Flags cluster_flags;
   --
   --  status = Cairo.Scaled_Font.Text_To_Glyphs (scaled_font,
   --                                             x, y,
   --                                             utf8, utf8_len,
   --                                             &amp;glyphs, &amp;num_glyphs,
   --                                             &amp;clusters,
   --&amp;num_clusters, &amp;cluster_flags);
   --
   --  if (status == CAIRO_STATUS_SUCCESS) {
   --      Cairo_Show_Text_Glyphs (cr,
   --                              utf8, utf8_len,
   --                              *glyphs, *num_glyphs,
   --                              *clusters, *num_clusters, *cluster_flags);
   --
   --      Cairo_Glyph_Free (*glyphs);
   --      Cairo_Text_Cluster_Free (*clusters);
   --  }
   --  </programlisting></informalexample>
   --
   --  If no cluster mapping is needed:
   --  <informalexample><programlisting>
   --  Cairo_Status status;
   --
   --  Cairo_Glyph *glyphs = NULL;
   --  int num_glyphs;
   --
   --  status = Cairo.Scaled_Font.Text_To_Glyphs (scaled_font,
   --                                             x, y,
   --                                             utf8, utf8_len,
   --                                             &amp;glyphs, &amp;num_glyphs,
   --                                             NULL, NULL,
   --                                             NULL);
   --
   --  if (status == CAIRO_STATUS_SUCCESS) {
   --      Cairo_Show_Glyphs (cr, *glyphs, *num_glyphs);
   --      Cairo_Glyph_Free (*glyphs);
   --  }
   --  </programlisting></informalexample>
   --
   --  If stack-based glyph and cluster arrays are to be used for small
   --  arrays:
   --  <informalexample><programlisting>
   --  Cairo_Status status;
   --
   --  Cairo_Glyph stack_glyphs[40];
   --  Cairo_Glyph *glyphs = stack_glyphs;
   --  int num_glyphs = sizeof (stack_glyphs) / sizeof (stack_glyphs[0]);
   --  Cairo_Text_Cluster stack_clusters[40];
   --  Cairo_Text_Cluster *clusters = stack_clusters;
   --  int num_clusters = sizeof (stack_clusters) / sizeof (stack_clusters[0]);
   --  Cairo_Text_Cluster_Flags cluster_flags;
   --
   --  status = Cairo.Scaled_Font.Text_To_Glyphs (scaled_font,
   --                                             x, y,
   --                                             utf8, utf8_len,
   --                                             &amp;glyphs, &amp;num_glyphs,
   --                                             &amp;clusters,
   --&amp;num_clusters, &amp;cluster_flags);
   --
   --  if (status == CAIRO_STATUS_SUCCESS) {
   --      Cairo_Show_Text_Glyphs (cr,
   --                              utf8, utf8_len,
   --                              *glyphs, *num_glyphs,
   --                              *clusters, *num_clusters, *cluster_flags);
   --
   --      if (glyphs != stack_glyphs)
   --          Cairo_Glyph_Free (*glyphs);
   --      if (clusters != stack_clusters)
   --          Cairo_Text_Cluster_Free (*clusters);
   --  }
   --  </programlisting></informalexample>
   --
   --  For details of how clusters, num_clusters, and cluster_flags map input
   --  UTF-8 text to the output glyphs see Cairo_Show_Text_Glyphs.
   --
   --  The output values can be readily passed to Cairo_Show_Text_Glyphs
   --  Cairo_Show_Glyphs, or related functions, assuming that the exact
   --  same scaled_font is used for the operation.
   --
   --  Return value: CAIRO_STATUS_SUCCESS upon success, or an error status
   --  if the input values are wrong or if conversion failed.  If the input
   --  values are correct but the conversion failed, the error status is also
   --  set on scaled_font.
   --
   --  Since: 1.8

   function Get_Font_Face
     (Scaled_Font : Cairo_Scaled_Font)
      return        Cairo_Font_Face;
   --  Scaled_Font: a Cairo_Scaled_Font
   --
   --  Gets the font face that this scaled font was created for.
   --
   --  Return value: The Cairo_Font_Face with which scaled_font was
   --  created.
   --
   --  Since: 1.2

   procedure Get_Font_Matrix
     (Scaled_Font : Cairo_Scaled_Font;
      Font_Matrix : access Cairo_Matrix);
   --  Scaled_Font: a Cairo_Scaled_Font
   --  Font_Matrix: return value for the matrix
   --
   --  Stores the font matrix with which scaled_font was created into
   --  matrix.
   --
   --  Since: 1.2

   procedure Get_Ctm
     (Scaled_Font : Cairo_Scaled_Font;
      Ctm         : access Cairo_Matrix);
   --  Scaled_Font: a Cairo_Scaled_Font
   --  Ctm: return value for the CTM
   --
   --  Stores the CTM with which scaled_font was created into ctm.
   --
   --  Since: 1.2

   procedure Get_Scale_Matrix
     (Scaled_Font  : Cairo_Scaled_Font;
      Scale_Matrix : access Cairo_Matrix);
   --  Scaled_Font: a Cairo_Scaled_Font
   --  Scale_Matrix: return value for the matrix
   --
   --  Stores the scale matrix of scaled_font into matrix.
   --  The scale matrix is product of the font matrix and the ctm
   --  associated with the scaled font, and hence is the matrix mapping from
   --  font space to device space.
   --
   --  Since: 1.8

   procedure Get_Font_Options
     (Scaled_Font : Cairo_Scaled_Font;
      Options     : Cairo_Font_Options);
   --  Scaled_Font: a Cairo_Scaled_Font
   --  Options: return value for the font Options
   --
   --  Stores the font options with which scaled_font was created into
   --  options.
   --
   --  Since: 1.2

private

   pragma Import (C, Create, "cairo_scaled_font_create");
   pragma Import (C, Reference, "cairo_scaled_font_reference");
   pragma Import (C, Destroy, "cairo_scaled_font_destroy");
   pragma Import
     (C,
      Get_Reference_Count,
      "cairo_scaled_font_get_reference_count");
   pragma Import (C, Status, "cairo_scaled_font_status");
   pragma Import (C, Get_Type, "cairo_scaled_font_get_type");
   pragma Import (C, Get_User_Data, "cairo_scaled_font_get_user_data");
   pragma Import (C, Set_User_Data, "cairo_scaled_font_set_user_data");
   pragma Import (C, Extents, "cairo_scaled_font_extents");
   pragma Import (C, Text_Extents, "cairo_scaled_font_text_extents");
   pragma Import (C, Glyph_Extents, "cairo_scaled_font_glyph_extents");
   pragma Import (C, Text_To_Glyphs, "cairo_scaled_font_text_to_glyphs");
   pragma Import (C, Get_Font_Face, "cairo_scaled_font_get_font_face");
   pragma Import (C, Get_Font_Matrix, "cairo_scaled_font_get_font_matrix");
   pragma Import (C, Get_Ctm, "cairo_scaled_font_get_ctm");
   pragma Import (C, Get_Scale_Matrix, "cairo_scaled_font_get_scale_matrix");
   pragma Import (C, Get_Font_Options, "cairo_scaled_font_get_font_options");

end Cairo.Scaled_Font;
