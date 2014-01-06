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

package Cairo.SVG is

   function Create
     (Filename        : String;
      Width_In_Point  : Gdouble;
      Height_In_Point : Gdouble) return Cairo_Surface;
   --  Creates a SVG surface of the specified size in points to be written to
   --  filename.
   --
   --  The SVG surface backend recognizes the following MIME types for the data
   --  attached to a surface (see Cairo.Surface.Set_Mime_Data) when it is
   --  used as a source pattern for drawing on this surface:
   --  CAIRO_MIME_TYPE_JPEG, CAIRO_MIME_TYPE_PNG, CAIRO_MIME_TYPE_URI. If any
   --  of them is specified, the SVG backend emits a href with the content of
   --  MIME data instead of a surface snapshot (PNG, Base64-encoded) in the
   --  corresponding image tag.
   --
   --  The unofficial MIME type CAIRO_MIME_TYPE_URI is examined first. If
   --  present, the URI is emitted as is: assuring the correctness of URI is
   --  left to the client code.
   --  If CAIRO_MIME_TYPE_URI is not present, but CAIRO_MIME_TYPE_JPEG or
   --  CAIRO_MIME_TYPE_PNG is specified, the corresponding data is
   --  Base64-encoded and emitted.
   --
   --  filename :
   --   a filename for the SVG output (must be writable), NULL may be used to
   --   specify no output. This will generate a SVG surface that may be queried
   --   and used as a source, without generating a temporary file.
   --  width_in_points :
   --   width of the surface, in points (1 point == 1/72.0 inch)
   --  height_in_points :
   --   height of the surface, in points (1 point == 1/72.0 inch)
   --  Returns :
   --   a pointer to the newly created surface. The caller owns the surface and
   --   should call Cairo.Surface.Destroy when done with it. This function
   --   always returns a valid pointer, but it will return a pointer to a "nil"
   --   surface if an error such as out of memory occurs. You can use
   --   Cairo.Surface.Status to check for this.
   --
   --  Since 1.2

end Cairo.SVG;
