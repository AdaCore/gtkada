
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
with Interfaces.C; use Interfaces.C;

package Graphene.Config is

   type Graphene_Simd4f is record
      X : Interfaces.C.C_float;
      Y : Interfaces.C.C_float;
      Z : Interfaces.C.C_float;
      W : Interfaces.C.C_float;
   end record;
   pragma Convention (C, Graphene_Simd4f);

   function From_Object_Free
     (B : not null access Graphene_Simd4f) return Graphene_Simd4f;
   pragma Inline (From_Object_Free);


   type Graphene_Simd4x4f is record
      X : Graphene_Simd4f;
      Y : Graphene_Simd4f;
      Z : Graphene_Simd4f;
      W : Graphene_Simd4f;
   end record;
   pragma Convention (C, Graphene_Simd4x4f);

   function From_Object_Free
     (B : not null access Graphene_Simd4x4f) return Graphene_Simd4x4f;
   pragma Inline (From_Object_Free);

   ----------------------
   -- GtkAda additions --
   ----------------------

   for Graphene_Simd4f'Alignment use 16;

   for Graphene_Simd4x4f'Alignment use 16;

end Graphene.Config;
