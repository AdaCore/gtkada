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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");

package body Graphene.Vec3 is

   function From_Object_Free (B : access Graphene_Vec3_T) return Graphene_Vec3_T is
      Result : constant Graphene_Vec3_T := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   -----------
   -- Equal --
   -----------

   function Equal
      (Self : not null access Graphene_Vec3_T;
       V2   : not null access Graphene_Vec3_T) return Boolean
   is
      function Internal
         (Self : access Graphene_Vec3_T;
          V2   : access Graphene_Vec3_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_vec3_equal");
   begin
      return Internal (Self, V2) /= 0;
   end Equal;

   ----------
   -- Near --
   ----------

   function Near
      (Self    : not null access Graphene_Vec3_T;
       V2      : not null access Graphene_Vec3_T;
       Epsilon : Interfaces.C.C_float) return Boolean
   is
      function Internal
         (Self    : access Graphene_Vec3_T;
          V2      : access Graphene_Vec3_T;
          Epsilon : Interfaces.C.C_float) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_vec3_near");
   begin
      return Internal (Self, V2, Epsilon) /= 0;
   end Near;

end Graphene.Vec3;
