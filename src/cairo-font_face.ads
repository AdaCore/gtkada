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

--  A set of utilities to handle the Cairo_Font_Face type, and the Cairo
--  'toy' text API.
--
--  <c_version>1.8.8</c_version>
--  <group>Cairo</group>

with System;
with Gtkada.Types;

package Cairo.Font_Face is

   function Reference (Font_Face : Cairo_Font_Face) return Cairo_Font_Face;
   --  Increases the reference count on Font_Face by one. This prevents
   --  Font_Face from being destroyed until a matching call to
   --  Cairo.Font_Face.Destroy is made.
   --
   --  The number of references to a Cairo_Font_Face can be get using
   --  Cairo.Font_Face.Get_Reference_Count.
   --
   --  @param Font_Face a Cairo_Font_Face, (may be Null_Font_Face in which
   --  case this function does nothing).
   --  @return the referenced Cairo_Font_Face.

   procedure Destroy (Font_Face : Cairo_Font_Face);
   --  Decreases the reference count on Font_Face by one. If the result
   --  is zero, then Font_Face and all associated resources are freed.
   --  See Cairo.Font_Face.Reference.
   --
   --  @param Font_Face a Cairo_Font_Face

   function Get_Reference_Count (Font_Face : Cairo_Font_Face) return Guint;
   --  Returns the current reference count of Font_Face.
   --
   --  @since 1.4
   --  @param Font_Face a Cairo_Font_Face
   --  @return the current reference count of Font_Face. If the object is a
   --  nil object, 0 will be returned.

   function Status (Font_Face : Cairo_Font_Face) return Cairo_Status;
   --  Checks whether an error has previously occurred for this
   --  font face
   --
   --  @param Font_Face a Cairo_Font_Face
   --  @return Cairo_Status_Success or another error such as
   --  Cairo_Status_No_Memory.

   function Get_Type (Font_Face : Cairo_Font_Face) return Cairo_Font_Type;
   --  This function returns the type of the backend used to create
   --  a font face. See Cairo_Font_Type for available types.
   --
   --  @since 1.2
   --  @param Font_Face a font face
   --  @return The type of Font_Face.

   function Get_User_Data
     (Font_Face : Cairo_Font_Face;
      Key       : access Cairo_User_Data_Key) return System.Address;
   --  Return user data previously attached to Font_Face using the specified
   --  key.  If no user data has been attached with the given key this
   --  function returns System.Null_Address.
   --
   --  @param Font_Face a Cairo_Font_Face
   --  @param Key the address of the Cairo_User_Data_Key the user data was
   --  attached to
   --  @return the user data previously attached or System.Null_Address.

   function Set_User_Data
     (Font_Face : Cairo_Font_Face;
      Key       : access Cairo_User_Data_Key;
      User_Data : System.Address;
      Destroy   : Cairo_Destroy_Func) return Cairo_Status;
   --  Attach user data to Font_Face.  To remove user data from a font face,
   --  call this function with the key that was used to set it and
   --  System.Null_Address for data.
   --
   --  @param Font_Face a Cairo_Font_Face
   --  @param Key the address of a Cairo_User_Data_Key to attach the user data
   --  to
   --  @param User_Data the user data to attach to the font face
   --  @param Destroy a Cairo_Destroy_Func which will be called when the font
   --  face is destroyed or when new user data is attached using the same key.
   --  @return Cairo_Status_Success or Cairo_Status_No_Memory if a slot could
   --  not be allocated for the user data.

   function Toy_Font_Face_Create
     (Family : Gtkada.Types.Chars_Ptr;
      Slant  : Cairo_Font_Slant;
      Weight : Cairo_Font_Weight)
      return   Cairo_Font_Face;
   --  Creates a font face from a triplet of family, slant, and weight.
   --  These font faces are used in implementation of the the Cairo_Context
   --  "toy" font API.
   --
   --  If Family is the zero-length string "", the platform-specific default
   --  family is assumed.  The default family then can be queried using
   --  Toy_Font_Face_Get_Family.
   --
   --  The Cairo_Select_Font_Face function uses this to create font faces.
   --  See that function for limitations of toy font faces.
   --
   --  @since 1.8
   --  @param Family a font Family name, encoded in UTF-8
   --  @param Slant the Slant for the font
   --  @param Weight the Weight for the font
   --  @return a newly created Cairo_Font_Face. Free with
   --  Cairo.Font_Face.Destroy when you are done using it.

   function Toy_Font_Face_Get_Family
     (Font_Face : Cairo_Font_Face)
      return      Gtkada.Types.Chars_Ptr;
   --  Gets the familly name of a toy font.
   --
   --  @since 1.8
   --  @param Font_Face A toy font face
   --  @return The family name. This string is owned by the font face and
   --  remains valid as long as the font face is alive (referenced).

   function Toy_Font_Face_Get_Slant
     (Font_Face : Cairo_Font_Face)
      return      Cairo_Font_Slant;
   --  Gets the slant a toy font.
   --
   --  @since 1.8
   --  @param Font_Face A toy font face
   --  @return The slant value

   function Toy_Font_Face_Get_Weight
     (Font_Face : Cairo_Font_Face)
      return      Cairo_Font_Weight;
   --  Gets the weight a toy font.
   --
   --  @since 1.8
   --  @param Font_Face A toy font face
   --  @return The weight value

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
      Toy_Font_Face_Create,
      "cairo_toy_font_face_create");
   pragma Import
     (C,
      Toy_Font_Face_Get_Family,
      "cairo_toy_font_face_get_family");
   pragma Import
     (C,
      Toy_Font_Face_Get_Slant,
      "cairo_toy_font_face_get_slant");
   pragma Import
     (C,
      Toy_Font_Face_Get_Weight,
      "cairo_toy_font_face_get_weight");

end Cairo.Font_Face;
