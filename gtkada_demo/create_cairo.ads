------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

with Testcairo_Drawing;  use Testcairo_Drawing;

package Create_Cairo is

   function Help return String;

   procedure Run_Rectangles is new Run (Rectangles);
   procedure Run_Transparency is new Run (Transparency);
   procedure Run_Operators is new Run (Operators);
   procedure Run_Matrix is new Run (Matrix);
   procedure Run_Transformations is new Run (Transformations);
   procedure Run_Paths is new Run (Paths);
   procedure Run_Patterns is new Run (Patterns);
   procedure Run_Clip_And_Paint is new Run (Clip_And_Paint);
   procedure Run_Surface_And_Png is new Run (Surface_And_Png);
   procedure Run_Toy_Text is new Run (Toy_Text);
   procedure Run_Pango_Text is new Run (Pango_Text);
   procedure Run_Image is new Run (Image);
end Create_Cairo;
