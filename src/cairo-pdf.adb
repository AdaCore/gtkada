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

package body Cairo.PDF is

   ------------
   -- Create --
   ------------

   function Create
     (Filename         : String;
      Width_In_Points  : Gdouble;
      Height_In_Points : Gdouble)
      return Cairo_Surface
   is
      function Internal
        (Filename : System.Address; W, H : Gdouble) return Cairo_Surface;
      pragma Import (C, Internal, "cairo_pdf_surface_create");
      C_Str : constant String := Filename & ASCII.NUL;
   begin
      return Internal
        (C_Str (C_Str'First)'Address, Width_In_Points, Height_In_Points);
   end Create;

end Cairo.PDF;
