------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2011-2014, AdaCore                     --
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

package Cairo.PDF is

   function Create
     (Filename         : String;
      Width_In_Points  : Gdouble;
      Height_In_Points : Gdouble) return Cairo_Surface;
   --  Creates a PDF surface of the specified size in points to be written to
   --  filename.
   --
   --  filename :
   --   a filename for the PDF output (must be writable), NULL may be used to
   --   specify no output. This will generate a PDF surface that may be queried
   --   and used as a source, without generating a temporary file.
   --  width_in_points:
   --   width of the surface, in points (1 point == 1/72.0 inch)
   --  height_in_points:
   --   height of the surface, in points (1 point == 1/72.0 inch)
   --  Returns:
   --   a pointer to the newly created surface. The caller owns the surface and
   --   should call cairo_surface_destroy() when done with it. This function
   --   always returns a valid pointer, but it will return a pointer to a "nil"
   --   surface if an error such as out of memory occurs. You can use
   --   cairo_surface_status() to check for this.
   --
   --  Since 1.2

end Cairo.PDF;
