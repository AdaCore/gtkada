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

package Cairo.Font_Face is

   function Reference (Font_Face : Cairo_Font_Face) return Cairo_Font_Face;
   --  Font_Face: a Cairo.Font_Face.T, (may be NULL in which case this
   --  function does nothing).
   --
   --  Increases the reference count on font_face by one. This prevents
   --  font_face from being destroyed until a matching call to
   --  Cairo.Font_Face.Destroy is made.
   --
   --  The number of references to a Cairo_Font_Face can be get using
   --  Cairo.Font_Face.Get_Reference_Count.
   --
   --  Return value: the referenced Cairo_Font_Face.

   procedure Destroy (Font_Face : Cairo_Font_Face);
   --  Font_Face: a Cairo_Font_Face
   --
   --  Decreases the reference count on font_face by one. If the result
   --  is zero, then font_face and all associated resources are freed.
   --  See Cairo.Font_Face.Reference.

   function Get_Reference_Count (Font_Face : Cairo_Font_Face) return Guint;
   --  Font_Face: a Cairo_Font_Face
   --
   --  Returns the current reference count of font_face.
   --
   --  Return value: the current reference count of font_face.  If the
   --  object is a nil object, 0 will be returned.
   --
   --  Since: 1.4

   function Status (Font_Face : Cairo_Font_Face) return Cairo_Status;
   --  Font_Face: a Cairo_Font_Face
   --
   --  Checks whether an error has previously occurred for this
   --  font face
   --
   --  Return value: CAIRO_STATUS_SUCCESS or another error such as
   --    CAIRO_STATUS_NO_MEMORY.

   function Get_Type (Font_Face : Cairo_Font_Face) return Cairo_Font_Type;
   --  Font_Face: a font face
   --
   --  This function returns the type of the backend used to create
   --  a font face. See Cairo_Font_Type for available types.
   --
   --  Return value: The type of font_face.
   --
   --  Since: 1.2

   function Get_User_Data
     (Font_Face : Cairo_Font_Face;
      Key       : access constant Cairo_User_Data_Key)
      return      System.Address;
   --  Font_Face: a Cairo_Font_Face
   --  Key: the address of the Cairo_User_Data_Key the user data was
   --  attached to
   --
   --  Return user data previously attached to font_face using the specified
   --  key.  If no user data has been attached with the given key this
   --  function returns NULL.
   --
   --  Return value: the user data previously attached or NULL.

   function Set_User_Data
     (Font_Face : Cairo_Font_Face;
      Key       : access constant Cairo_User_Data_Key;
      User_Data : System.Address;
      Destroy   : access procedure (Arg1 : System.Address))
      return      Cairo_Status;
   --  Font_Face: a Cairo_Font_Face
   --  Key: the address of a Cairo_User_Data_Key to attach the user data to
   --  User_Data: the user data to attach to the font face
   --  Destroy: a Cairo_Destroy_Func which will be called when the
   --  font face is destroyed or when new user data is attached using the
   --  same key.
   --
   --  Attach user data to font_face.  To remove user data from a font face,
   --  call this function with the key that was used to set it and NULL
   --  for data.
   --
   --  Return value: CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a
   --  slot could not be allocated for the user data.

   function Cairo_Toy_Font_Face_Create
     (Family : Interfaces.C.Strings.chars_ptr;
      Slant  : Cairo_Font_Slant;
      Weight : Cairo_Font_Weight)
      return   Cairo_Font_Face;
   --  Family: a font Family name, encoded in UTF-8
   --  Slant: the Slant for the font
   --  Weight: the Weight for the font
   --
   --  Creates a font face from a triplet of family, slant, and weight.
   --  These font faces are used in implementation of the the Cairo_Context
   --"toy"
   --  font API.
   --
   --  If family is the zero-length string "", the platform-specific default
   --  family is assumed.  The default family then can be queried using
   --  Cairo_Toy_Font_Face_Get_Family.
   --
   --  The Cairo_Select_Font_Face function uses this to create font faces.
   --  See that function for limitations of toy font faces.
   --
   --  Return value: a newly created Cairo_Font_Face. Free with
   --   Cairo.Font_Face.Destroy when you are done using it.
   --
   --  Since: 1.8

   function Cairo_Toy_Font_Face_Get_Family
     (Font_Face : Cairo_Font_Face)
      return      Interfaces.C.Strings.chars_ptr;
   --  Font_Face: A toy font face
   --
   --  Gets the familly name of a toy font.
   --
   --  Return value: The family name.  This string is owned by the font face
   --  and remains valid as long as the font face is alive (referenced).
   --
   --  Since: 1.8

   function Cairo_Toy_Font_Face_Get_Slant
     (Font_Face : Cairo_Font_Face)
      return      Cairo_Font_Slant;
   --  Font_Face: A toy font face
   --
   --  Gets the slant a toy font.
   --
   --  Return value: The slant value
   --
   --  Since: 1.8

   function Cairo_Toy_Font_Face_Get_Weight
     (Font_Face : Cairo_Font_Face)
      return      Cairo_Font_Weight;
   --  Font_Face: A toy font face
   --
   --  Gets the weight a toy font.
   --
   --  Return value: The weight value
   --
   --  Since: 1.8

private

   pragma Import (C, Reference, "cairo_font_face_reference");
   pragma Import (C, Destroy, "cairo_font_face_destroy");
   pragma Import
     (C,
      Get_Reference_Count,
      "cairo_font_face_get_reference_count");
   pragma Import (C, Status, "cairo_font_face_status");
   pragma Import (C, Get_Type, "cairo_font_face_get_type");
   pragma Import (C, Get_User_Data, "cairo_font_face_get_user_data");
   pragma Import (C, Set_User_Data, "cairo_font_face_set_user_data");
   pragma Import
     (C,
      Cairo_Toy_Font_Face_Create,
      "cairo_toy_font_face_create");
   pragma Import
     (C,
      Cairo_Toy_Font_Face_Get_Family,
      "cairo_toy_font_face_get_family");
   pragma Import
     (C,
      Cairo_Toy_Font_Face_Get_Slant,
      "cairo_toy_font_face_get_slant");
   pragma Import
     (C,
      Cairo_Toy_Font_Face_Get_Weight,
      "cairo_toy_font_face_get_weight");

end Cairo.Font_Face;
