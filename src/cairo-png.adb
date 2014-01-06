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

package body Cairo.Png is

   ------------------
   -- Write_To_Png --
   ------------------

   function Write_To_Png
     (Surface  : Cairo_Surface;
      Filename : String)
      return Cairo_Status
   is
      function C_Cairo_Surface_Write_To_Png
        (Surface  : Cairo_Surface;
         Filename : System.Address) return Cairo_Status;
      pragma Import
        (C,
         C_Cairo_Surface_Write_To_Png,
         "cairo_surface_write_to_png");

      S : constant String := Filename & ASCII.NUL;
   begin
      return C_Cairo_Surface_Write_To_Png
        (Surface, S (S'First)'Address);

   end Write_To_Png;

   ---------------------
   -- Create_From_Png --
   ---------------------

   function Create_From_Png
     (Filename : String)
      return     Cairo_Surface
   is
      function C_Cairo_Image_Surface_Create_From_Png
        (Filename : System.Address) return Cairo_Surface;

      pragma Import
        (C,
         C_Cairo_Image_Surface_Create_From_Png,
         "cairo_image_surface_create_from_png");

      S : constant String := Filename & ASCII.NUL;
   begin
      return C_Cairo_Image_Surface_Create_From_Png (S (S'First)'Address);
   end Create_From_Png;

end Cairo.Png;
