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

package Cairo.User_Font is

   function Face_Create return Cairo_Font_Face;
   --
   --  Creates a new user font-face.
   --
   --  Use the setter functions to associate callbacks with the returned
   --  user font.  The only mandatory callback is render_glyph.
   --
   --  After the font-face is created, the user can attach arbitrary data
   --  (the actual font data) to it using Cairo.Font_Face.Set_User_Data
   --  and access it from the user-font callbacks by using
   --  Cairo.Scaled_Font.Get_Font_Face followed by
   --  Cairo.Font_Face.Get_User_Data.
   --
   --  Return value: a newly created Cairo_Font_Face. Free with
   --   Cairo.Font_Face.Destroy when you are done using it.
   --
   --  Since: 1.8

   procedure Face_Set_Init_Func
     (Font_Face : Cairo_Font_Face;
      Init_Func : access function
     (Arg1 : Cairo_Scaled_Font;
      Arg2 : Cairo_Context;
      Arg3 : access Cairo_Font_Extents)
      return Cairo_Status);
   --  Font_Face: A user font face
   --  Init_Func: The init callback, or NULL
   --
   --  Sets the scaled-font initialization function of a user-font.
   --  See Cairo_User_Scaled_Font_Init_Func for details of how the callback
   --  works.
   --
   --  The font-face should not be immutable or a
   --  CAIRO_STATUS_USER_FONT_IMMUTABLE error will occur.  A user font-face is
   --  immutable as soon as a scaled-font is created from it.
   --
   --  Since: 1.8

   procedure Face_Set_Render_Glyph_Func
     (Font_Face         : Cairo_Font_Face;
      Render_Glyph_Func : access function
     (Arg1 : Cairo_Scaled_Font;
      Arg2 : Gulong;
      Arg3 : Cairo_Context;
      Arg4 : Cairo_Text_Extents)
      return Cairo_Status);
   --  Font_Face: A user font face
   --  Render_Glyph_Func: The render_glyph callback, or NULL
   --
   --  Sets the glyph rendering function of a user-font.
   --  See Cairo_User_Scaled_Font_Render_Glyph_Func for details of how the
   --  callback works.
   --
   --  The font-face should not be immutable or a
   --  CAIRO_STATUS_USER_FONT_IMMUTABLE error will occur.  A user font-face is
   --  immutable as soon as a scaled-font is created from it.
   --
   --  The render_glyph callback is the only mandatory callback of a user-font.
   --  If the callback is NULL and a glyph is tried to be rendered using
   --  font_face, a CAIRO_STATUS_USER_FONT_ERROR will occur.
   --
   --  Since: 1.8

   procedure Face_Set_Text_To_Glyphs_Func
     (Font_Face           : Cairo_Font_Face;
      Text_To_Glyphs_Func : access function
     (Arg1 : Cairo_Scaled_Font;
      Arg2 : Interfaces.C.Strings.chars_ptr;
      Arg3 : Gint;
      Arg4 : System.Address;
      Arg5 : access Gint;
      Arg6 : System.Address;
      Arg7 : access Gint;
      Arg8 : Cairo_Text_Cluster_Flags)
      return Cairo_Status);
   --  Font_Face: A user font face
   --  Text_To_Glyphs_Func: The text_to_glyphs callback, or NULL
   --
   --  Sets th text-to-glyphs conversion function of a user-font.
   --  See Cairo_User_Scaled_Font_Text_To_Glyphs_Func for details of how the
   --  callback works.
   --
   --  The font-face should not be immutable or a
   --  CAIRO_STATUS_USER_FONT_IMMUTABLE error will occur.  A user font-face is
   --  immutable as soon as a scaled-font is created from it.
   --
   --  Since: 1.8

   procedure Face_Set_Unicode_To_Glyph_Func
     (Font_Face             : Cairo_Font_Face;
      Unicode_To_Glyph_Func : access function
     (Arg1 : Cairo_Scaled_Font;
      Arg2 : Gulong;
      Arg3 : access Gulong)
      return Cairo_Status);
   --  Font_Face: A user font face
   --  Unicode_To_Glyph_Func: The unicode_to_glyph callback, or NULL
   --
   --  Sets the unicode-to-glyph conversion function of a user-font.
   --  See Cairo_User_Scaled_Font_Unicode_To_Glyph_Func for details of how the
   --  callback works.
   --
   --  The font-face should not be immutable or a
   --  CAIRO_STATUS_USER_FONT_IMMUTABLE error will occur.  A user font-face is
   --  immutable as soon as a scaled-font is created from it.
   --
   --  Since: 1.8

   function Face_Get_Init_Func
     (Font_Face : Cairo_Font_Face)
      return  access  function
     (Arg1 : Cairo_Scaled_Font;
      Arg2 : Cairo_Context;
      Arg3 : access Cairo_Font_Extents)
      return Cairo_Status;
   --  Font_Face: A user font face
   --
   --  Gets the scaled-font initialization function of a user-font.
   --
   --  Return value: The init callback of font_face
   --  or NULL if none set or an error has occurred.
   --
   --  Since: 1.8

   function Face_Get_Render_Glyph_Func
     (Font_Face : Cairo_Font_Face)
      return  access  function
     (Arg1 : Cairo_Scaled_Font;
      Arg2 : Gulong;
      Arg3 : Cairo_Context;
      Arg4 : Cairo_Text_Extents)
      return Cairo_Status;
   --  Font_Face: A user font face
   --
   --  Gets the glyph rendering function of a user-font.
   --
   --  Return value: The render_glyph callback of font_face
   --  or NULL if none set or an error has occurred.
   --
   --  Since: 1.8

   function Face_Get_Text_To_Glyphs_Func
     (Font_Face : Cairo_Font_Face)
      return  access  function
     (Arg1 : Cairo_Scaled_Font;
      Arg2 : Interfaces.C.Strings.chars_ptr;
      Arg3 : Gint;
      Arg4 : System.Address;
      Arg5 : access Gint;
      Arg6 : System.Address;
      Arg7 : access Gint;
      Arg8 : Cairo_Text_Cluster_Flags)
      return Cairo_Status;
   --  Font_Face: A user font face
   --
   --  Gets the text-to-glyphs conversion function of a user-font.
   --
   --  Return value: The text_to_glyphs callback of font_face
   --  or NULL if none set or an error occurred.
   --
   --  Since: 1.8

   function Face_Get_Unicode_To_Glyph_Func
     (Font_Face : Cairo_Font_Face)
      return  access  function
     (Arg1 : Cairo_Scaled_Font;
      Arg2 : Gulong;
      Arg3 : access Gulong)
      return Cairo_Status;
   --  Font_Face: A user font face
   --
   --  Gets the unicode-to-glyph conversion function of a user-font.
   --
   --  Return value: The unicode_to_glyph callback of font_face
   --  or NULL if none set or an error occurred.
   --
   --  Since: 1.8

private

   pragma Import (C, Face_Create, "cairo_user_font_face_create");
   pragma Import
     (C,
      Face_Set_Init_Func,
      "cairo_user_font_face_set_init_func");
   pragma Import
     (C,
      Face_Set_Render_Glyph_Func,
      "cairo_user_font_face_set_render_glyph_func");
   pragma Import
     (C,
      Face_Set_Text_To_Glyphs_Func,
      "cairo_user_font_face_set_text_to_glyphs_func");
   pragma Import
     (C,
      Face_Set_Unicode_To_Glyph_Func,
      "cairo_user_font_face_set_unicode_to_glyph_func");
   pragma Import
     (C,
      Face_Get_Init_Func,
      "cairo_user_font_face_get_init_func");
   pragma Import
     (C,
      Face_Get_Render_Glyph_Func,
      "cairo_user_font_face_get_render_glyph_func");
   pragma Import
     (C,
      Face_Get_Text_To_Glyphs_Func,
      "cairo_user_font_face_get_text_to_glyphs_func");
   pragma Import
     (C,
      Face_Get_Unicode_To_Glyph_Func,
      "cairo_user_font_face_get_unicode_to_glyph_func");

end Cairo.User_Font;
