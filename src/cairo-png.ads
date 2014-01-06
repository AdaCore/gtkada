------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2014, AdaCore                     --
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

--  <description>
--  Reading and writing PNG images.
--  </description>
--
--  <c_version>1.8.8</c_version>
--  <group>Cairo</group>

package Cairo.Png is

   function Write_To_Png
     (Surface  : Cairo_Surface;
      Filename : String)
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

   function Create_From_Png
     (Filename : String)
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

end Cairo.Png;
