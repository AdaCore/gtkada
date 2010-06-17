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

package Cairo.Png is

   function Cairo_Surface_Write_To_Png
     (Surface  : Cairo_Surface;
      Filename : Interfaces.C.Strings.chars_ptr)
      return     Cairo_Status;
   --  Surface: a Cairo_Surface with pixel contents
   --  Filename: the name of a file to write to
   --
   --  Writes the contents of surface to a new file filename as a PNG
   --  image.
   --
   --  Return value: Cairo_Status_Success if the PNG file was written
   --  successfully. Otherwise, Cairo_Status_No_Memory if memory could not
   --  be allocated for the operation or
   --  Cairo_Status_Surface_Type_Mismatch if the surface does not have
   --  pixel contents, or Cairo_Status_Write_Error if an I/O error occurs
   --  while attempting to write the file.

   function Cairo_Surface_Write_To_Png_Stream
     (Surface    : Cairo_Surface;
      Write_Func : access function
     (Arg1 : System.Address;
      Arg2 : access Guchar;
      Arg3 : Guint)
      return Cairo_Status;
      Closure    : System.Address)
      return       Cairo_Status;
   --  Surface: a Cairo_Surface with pixel contents
   --  Write_Func: a Cairo_Write_Func
   --  Closure: Closure data for the write function
   --
   --  Writes the image surface to the write function.
   --
   --  Return value: Cairo_Status_Success if the PNG file was written
   --  successfully.  Otherwise, Cairo_Status_No_Memory is returned if
   --  memory could not be allocated for the operation,
   --  Cairo_Status_Surface_Type_Mismatch if the surface does not have
   --  pixel contents.

   function Cairo_Image_Surface_Create_From_Png
     (Filename : Interfaces.C.Strings.chars_ptr)
      return     Cairo_Surface;
   --  Filename: name of PNG file to load
   --
   --  Creates a new image surface and initializes the contents to the
   --  given PNG file.
   --
   --  Return value: a new Cairo_Surface initialized with the contents
   --  of the PNG file, or a "nil" surface if any error occurred. A nil
   --  surface can be checked for with Cairo.Surface.Status (Surface) which
   --  may return one of the following values:
   --
   --  Cairo_Status_No_Memory
   --  Cairo_Status_File_Not_Found
   --  Cairo_Status_Read_Error
   --
   --  Alternatively, you can allow errors to propagate through the drawing
   --  operations and check the status on the context upon completion
   --  using Cairo_Status.

   function Cairo_Image_Surface_Create_From_Png_Stream
     (Read_Func : access function
     (Arg1 : System.Address;
      Arg2 : access Guchar;
      Arg3 : Guint)
      return Cairo_Status;
      Closure   : System.Address)
      return      Cairo_Surface;
   --  Read_Func: function called to read the data of the file
   --  Closure: data to pass to read_func.
   --
   --  Creates a new image surface from PNG data read incrementally
   --  via the read_func function.
   --
   --  Return value: a new Cairo_Surface initialized with the contents
   --  of the PNG file or a "nil" surface if the data read is not a valid PNG
   --  image or memory could not be allocated for the operation.  A nil
   --  surface can be checked for with Cairo.Surface.Status (Surface) which
   --  may return one of the following values:
   --
   --  Cairo_Status_No_Memory
   --  Cairo_Status_Read_Error
   --
   --  Alternatively, you can allow errors to propagate through the drawing
   --  operations and check the status on the context upon completion
   --  using Cairo_Status.

private

   pragma Import
     (C,
      Cairo_Surface_Write_To_Png,
      "cairo_surface_write_to_png");
   pragma Import
     (C,
      Cairo_Surface_Write_To_Png_Stream,
      "cairo_surface_write_to_png_stream");
   pragma Import
     (C,
      Cairo_Image_Surface_Create_From_Png,
      "cairo_image_surface_create_from_png");
   pragma Import
     (C,
      Cairo_Image_Surface_Create_From_Png_Stream,
      "cairo_image_surface_create_from_png_stream");

end Cairo.Png;
