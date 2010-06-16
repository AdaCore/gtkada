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

with Interfaces.C.Strings;

package Cairo.Misc is

   function Cairo_Glyph_Allocate
     (Num_Glyphs : Gint)
      return       access Cairo_Glyph;
   --  Num_Glyphs: number of glyphs to allocate
   --
   --  Allocates an array of Cairo_Glyph_T's.
   --  This function is only useful in implementations of
   --  Cairo_User_Scaled_Font_Text_To_Glyphs_Func where the user
   --  needs to allocate an array of glyphs that cairo will free.
   --  For all other uses, user can use their own allocation method
   --  for glyphs.
   --
   --  This function returns NULL if num_glyphs is not positive,
   --  or if out of memory.  That means, the NULL return value
   --  signals out-of-memory only if num_glyphs was positive.
   --
   --  Returns: the newly allocated array of glyphs that should be
   --           freed using Cairo_Glyph_Free
   --
   --  Since: 1.8

   procedure Cairo_Glyph_Free (Glyphs : access Cairo_Glyph);
   --  Glyphs: array of Glyphs to free, or NULL
   --
   --  Frees an array of Cairo_Glyph_T's allocated using Cairo_Glyph_Allocate.
   --  This function is only useful to free glyph array returned
   --  by Cairo.Scaled_Font.Text_To_Glyphs where cairo returns
   --  an array of glyphs that the user will free.
   --  For all other uses, user can use their own allocation method
   --  for glyphs.
   --
   --  Since: 1.8

   function Cairo_Text_Cluster_Allocate
     (Num_Clusters : Gint)
      return         Cairo_Text_Cluster;
   --  Num_Clusters: number of text_clusters to allocate
   --
   --  Allocates an array of Cairo_Text_Cluster_T's.
   --  This function is only useful in implementations of
   --  Cairo_User_Scaled_Font_Text_To_Glyphs_Func where the user
   --  needs to allocate an array of text clusters that cairo will free.
   --  For all other uses, user can use their own allocation method
   --  for text clusters.
   --
   --  This function returns NULL if num_clusters is not positive,
   --  or if out of memory.  That means, the NULL return value
   --  signals out-of-memory only if num_clusters was positive.
   --
   --  Returns: the newly allocated array of text clusters that should be
   --           freed using Cairo_Text_Cluster_Free
   --
   --  Since: 1.8

   procedure Cairo_Text_Cluster_Free (Clusters : Cairo_Text_Cluster);
   --  Clusters: array of text Clusters to free, or NULL
   --
   --  Frees an array of Cairo_Text_Cluster's allocated using
   --  Cairo_Text_Cluster_Allocate.
   --  This function is only useful to free text cluster array returned
   --  by Cairo.Scaled_Font.Text_To_Glyphs where cairo returns
   --  an array of text clusters that the user will free.
   --  For all other uses, user can use their own allocation method
   --  for text clusters.
   --
   --  Since: 1.8

   function Cairo_Status_To_String
     (Status : Cairo_Status)
      return   Interfaces.C.Strings.chars_ptr;
   --  Status: a cairo Status
   --
   --  Provides a human-readable description of a Cairo_Status.
   --
   --  Returns: a string representation of the status

private

   pragma Import (C, Cairo_Glyph_Allocate, "cairo_glyph_allocate");
   pragma Import (C, Cairo_Glyph_Free, "cairo_glyph_free");
   pragma Import
     (C,
      Cairo_Text_Cluster_Allocate,
      "cairo_text_cluster_allocate");
   pragma Import (C, Cairo_Text_Cluster_Free, "cairo_text_cluster_free");
   pragma Import (C, Cairo_Status_To_String, "cairo_status_to_string");

end Cairo.Misc;
