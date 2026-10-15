------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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


pragma Warnings (Off, "*is already use-visible*");
with Glib.Generic_Properties; use Glib.Generic_Properties;

package Gsk.Enums is

   type Gsk_Transform_Category is (
      Gsk_Transform_Category_Unknown,
      Gsk_Transform_Category_Any,
      Gsk_Transform_Category_3D,
      Gsk_Transform_Category_2D,
      Gsk_Transform_Category_2D_Affine,
      Gsk_Transform_Category_2D_Translate,
      Gsk_Transform_Category_Identity);
   pragma Convention (C, Gsk_Transform_Category);
   --  The categories of matrices relevant for GSK and GTK.
   --
   --  Note that any category includes matrices of all later categories. So if
   --  you want to for example check if a matrix is a 2D matrix, `category >=
   --  GSK_TRANSFORM_CATEGORY_2D` is the way to do this.
   --
   --  Also keep in mind that rounding errors may cause matrices to not
   --  conform to their categories. Otherwise, matrix operations done via
   --  multiplication will not worsen categories. So for the matrix
   --  multiplication `C = A * B`, `category(C) = MIN (category(A),
   --  category(B))`.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gsk_Transform_Category_Properties is
      new Generic_Internal_Discrete_Property (Gsk_Transform_Category);
   type Property_Gsk_Transform_Category is new Gsk_Transform_Category_Properties.Property;

end Gsk.Enums;
