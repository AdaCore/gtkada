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

--  The Pango.Glyph_String.Pango_Glyph_String structure is used to store
--  strings of glyphs with geometry and visual attribute information. The
--  storage for the glyph information is owned by the structure which
--  simplifies memory management.

pragma Warnings (Off, "*is already use-visible*");
with Glib;       use Glib;
with Pango.Font; use Pango.Font;
with Pango.Item; use Pango.Item;

package Pango.Glyph_String is

   type Pango_Glyph_Vis_Attr is record
      Is_Cluster_Start : Guint;
   end record;
   pragma Convention (C, Pango_Glyph_Vis_Attr);
   --  The PangoGlyphVisAttr is used to communicate information between the
   --  shaping phase and the rendering phase. More attributes may be added in
   --  the future.

   type Pango_Glyph_Geometry is record
      Width : Glib.Gint32;
      X_Offset : Glib.Gint32;
      Y_Offset : Glib.Gint32;
   end record;
   pragma Convention (C, Pango_Glyph_Geometry);
   --  The Pango.Glyph_String.Pango_Glyph_Geometry structure contains width
   --  and positioning information for a single glyph.

   type Pango_Glyph_Info is record
      Glyph : Pango_Glyph;
      Geometry : Pango_Glyph_Geometry;
      Attr : Pango_Glyph_Vis_Attr;
   end record;
   pragma Convention (C, Pango_Glyph_Info);
   --  The Pango.Glyph_String.Pango_Glyph_Info structure represents a single
   --  glyph together with positioning information and visual attributes. It
   --  contains the following fields.

   type Pango_Glyph_Info_Array is
      array (Natural range <>) of Pango_Glyph_Info;
      pragma Convention (C, Pango_Glyph_Info_Array);

   type Pango_Glyph_String is record
      Num_Glyphs : Glib.Gint := 0;
      Glyphs : System.Address := System.Null_Address;
      Log_Clusters : access Glib.Gint := null;
      Space : Glib.Gint := 0;
   end record;
   pragma Convention (C, Pango_Glyph_String);
   --  The Pango.Glyph_String.Pango_Glyph_String structure is used to store
   --  strings of glyphs with geometry and visual attribute information. The
   --  storage for the glyph information is owned by the structure which
   --  simplifies memory management.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "pango_glyph_string_get_type");

   -------------
   -- Methods --
   -------------

   function Copy
      (Self : access Pango_Glyph_String) return access Pango_Glyph_String;
   pragma Import (C, Copy, "pango_glyph_string_copy");
   --  Copy a glyph string and associated storage.
   --  @return the newly allocated Pango.Glyph_String.Pango_Glyph_String,
   --  which should be freed with Pango.Glyph_String.Free, or null if String
   --  was null.

   procedure Extents
      (Self         : access Pango_Glyph_String;
       Font         : not null access Pango.Font.Pango_Font_Record'Class;
       Ink_Rect     : out Pango_Rectangle;
       Logical_Rect : out Pango_Rectangle);
   --  Compute the logical and ink extents of a glyph string. See the
   --  documentation for Pango.Font.Get_Glyph_Extents for details about the
   --  interpretation of the rectangles.
   --  Examples of logical (red) and ink (green) rects:
   --  ![](rects1.png) ![](rects2.png)
   --  @param Font a Pango.Font.Pango_Font
   --  @param Ink_Rect rectangle used to store the extents of the glyph string
   --  as drawn or null to indicate that the result is not needed.
   --  @param Logical_Rect rectangle used to store the logical extents of the
   --  glyph string or null to indicate that the result is not needed.

   procedure Extents_Range
      (Self         : access Pango_Glyph_String;
       Start        : Glib.Gint;
       The_End      : Glib.Gint;
       Font         : not null access Pango.Font.Pango_Font_Record'Class;
       Ink_Rect     : out Pango_Rectangle;
       Logical_Rect : out Pango_Rectangle);
   --  Computes the extents of a sub-portion of a glyph string. The extents
   --  are relative to the start of the glyph string range (the origin of their
   --  coordinate system is at the start of the range, not at the start of the
   --  entire glyph string).
   --  @param Start start index
   --  @param The_End end index (the range is the set of bytes with indices
   --  such that start <= index < end)
   --  @param Font a Pango.Font.Pango_Font
   --  @param Ink_Rect rectangle used to store the extents of the glyph string
   --  range as drawn or null to indicate that the result is not needed.
   --  @param Logical_Rect rectangle used to store the logical extents of the
   --  glyph string range or null to indicate that the result is not needed.

   procedure Free (Self : access Pango_Glyph_String);
   pragma Import (C, Free, "pango_glyph_string_free");
   --  Free a glyph string and associated storage.

   procedure Get_Logical_Widths
      (Self            : access Pango_Glyph_String;
       Text            : UTF8_String;
       Length          : Glib.Gint;
       Embedding_Level : Glib.Gint;
       Logical_Widths  : Gint_Array);
   --  Given a Pango.Glyph_String.Pango_Glyph_String resulting from
   --  pango_shape and the corresponding text, determine the screen width
   --  corresponding to each character. When multiple characters compose a
   --  single cluster, the width of the entire cluster is divided equally among
   --  the characters.
   --  See also pango_glyph_item_get_logical_widths.
   --  @param Text the text corresponding to the glyphs
   --  @param Length the length of Text, in bytes
   --  @param Embedding_Level the embedding level of the string
   --  @param Logical_Widths an array whose length is the number of characters
   --  in text (equal to g_utf8_strlen (text, length) unless text has NUL
   --  bytes) to be filled in with the resulting character widths.

   function Get_Width (Self : access Pango_Glyph_String) return Glib.Gint;
   pragma Import (C, Get_Width, "pango_glyph_string_get_width");
   --  Computes the logical width of the glyph string as can also be computed
   --  using Pango.Glyph_String.Extents. However, since this only computes the
   --  width, it's much faster. This is in fact only a convenience function
   --  that computes the sum of geometry.width for each glyph in the Glyphs.
   --  Since: gtk+ 1.14
   --  @return the logical width of the glyph string.

   procedure Index_To_X
      (Self     : access Pango_Glyph_String;
       Text     : UTF8_String;
       Length   : Glib.Gint;
       Analysis : access Pango.Item.Pango_Analysis;
       Index    : Glib.Gint;
       Trailing : Boolean;
       X_Pos    : out Glib.Gint);
   --  Converts from character position to x position. (X position is measured
   --  from the left edge of the run). Character positions are computed by
   --  dividing up each cluster into equal portions.
   --  @param Text the text for the run
   --  @param Length the number of bytes (not characters) in Text.
   --  @param Analysis the analysis information return from pango_itemize
   --  @param Index the byte index within Text
   --  @param Trailing whether we should compute the result for the beginning
   --  (False) or end (True) of the character.
   --  @param X_Pos location to store result

   procedure Set_Size
      (Self    : access Pango_Glyph_String;
       New_Len : Glib.Gint);
   pragma Import (C, Set_Size, "pango_glyph_string_set_size");
   --  Resize a glyph string to the given length.
   --  @param New_Len the new length of the string.

   procedure X_To_Index
      (Self     : access Pango_Glyph_String;
       Text     : UTF8_String;
       Length   : Glib.Gint;
       Analysis : access Pango.Item.Pango_Analysis;
       X_Pos    : Glib.Gint;
       Index    : out Glib.Gint;
       Trailing : out Glib.Gint);
   --  Convert from x offset to character position. Character positions are
   --  computed by dividing up each cluster into equal portions. In scripts
   --  where positioning within a cluster is not allowed (such as Thai), the
   --  returned value may not be a valid cursor position; the caller must
   --  combine the result with the logical attributes for the text to compute
   --  the valid cursor position.
   --  @param Text the text for the run
   --  @param Length the number of bytes (not characters) in text.
   --  @param Analysis the analysis information return from pango_itemize
   --  @param X_Pos the x offset (in Pango units)
   --  @param Index location to store calculated byte index within Text
   --  @param Trailing location to store a boolean indicating whether the user
   --  clicked on the leading or trailing edge of the character.

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Pango_Glyph_Vis_Attr) return Pango_Glyph_Vis_Attr;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   function From_Object_Free
     (B : not null access Pango_Glyph_Geometry) return Pango_Glyph_Geometry;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   function From_Object_Free
     (B : not null access Pango_Glyph_Info) return Pango_Glyph_Info;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   function From_Object_Free
     (B : not null access Pango_Glyph_String) return Pango_Glyph_String;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   function Pango_Glyph_String_New return access Pango_Glyph_String;
   --  Create a new Pango.Glyph_String.Pango_Glyph_String.

end Pango.Glyph_String;
