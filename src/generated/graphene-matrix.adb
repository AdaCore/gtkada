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

package body Graphene.Matrix is

   ---------------
   -- Decompose --
   ---------------

   function Decompose
      (Self        : not null access Graphene_Matrix_T;
       Translate   : not null access Graphene.Vec3.Graphene_Vec3_T;
       Scale       : not null access Graphene.Vec3.Graphene_Vec3_T;
       Rotate      : not null access Graphene.Quaternion.Graphene_Quaternion_T;
       Shear       : not null access Graphene.Vec3.Graphene_Vec3_T;
       Perspective : not null access Graphene.Vec4.Graphene_Vec4_T)
       return Boolean
   is
      function Internal
         (Self        : access Graphene_Matrix_T;
          Translate   : access Graphene.Vec3.Graphene_Vec3_T;
          Scale       : access Graphene.Vec3.Graphene_Vec3_T;
          Rotate      : access Graphene.Quaternion.Graphene_Quaternion_T;
          Shear       : access Graphene.Vec3.Graphene_Vec3_T;
          Perspective : access Graphene.Vec4.Graphene_Vec4_T)
          return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_matrix_decompose");
   begin
      return Internal (Self, Translate, Scale, Rotate, Shear, Perspective) /= 0;
   end Decompose;

   -----------
   -- Equal --
   -----------

   function Equal
      (Self : not null access Graphene_Matrix_T;
       B    : not null access Graphene_Matrix_T) return Boolean
   is
      function Internal
         (Self : access Graphene_Matrix_T;
          B    : access Graphene_Matrix_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_matrix_equal");
   begin
      return Internal (Self, B) /= 0;
   end Equal;

   ----------------
   -- Equal_Fast --
   ----------------

   function Equal_Fast
      (Self : not null access Graphene_Matrix_T;
       B    : not null access Graphene_Matrix_T) return Boolean
   is
      function Internal
         (Self : access Graphene_Matrix_T;
          B    : access Graphene_Matrix_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_matrix_equal_fast");
   begin
      return Internal (Self, B) /= 0;
   end Equal_Fast;

   -------------
   -- Inverse --
   -------------

   function Inverse
      (Self : not null access Graphene_Matrix_T;
       Res  : not null access Graphene_Matrix_T) return Boolean
   is
      function Internal
         (Self : access Graphene_Matrix_T;
          Res  : access Graphene_Matrix_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_matrix_inverse");
   begin
      return Internal (Self, Res) /= 0;
   end Inverse;

   -----------
   -- Is_2D --
   -----------

   function Is_2D (Self : not null access Graphene_Matrix_T) return Boolean is
      function Internal
         (Self : access Graphene_Matrix_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_matrix_is_2d");
   begin
      return Internal (Self) /= 0;
   end Is_2D;

   -------------------------
   -- Is_Backface_Visible --
   -------------------------

   function Is_Backface_Visible
      (Self : not null access Graphene_Matrix_T) return Boolean
   is
      function Internal
         (Self : access Graphene_Matrix_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_matrix_is_backface_visible");
   begin
      return Internal (Self) /= 0;
   end Is_Backface_Visible;

   -----------------
   -- Is_Identity --
   -----------------

   function Is_Identity
      (Self : not null access Graphene_Matrix_T) return Boolean
   is
      function Internal
         (Self : access Graphene_Matrix_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_matrix_is_identity");
   begin
      return Internal (Self) /= 0;
   end Is_Identity;

   -----------------
   -- Is_Singular --
   -----------------

   function Is_Singular
      (Self : not null access Graphene_Matrix_T) return Boolean
   is
      function Internal
         (Self : access Graphene_Matrix_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_matrix_is_singular");
   begin
      return Internal (Self) /= 0;
   end Is_Singular;

   ----------
   -- Near --
   ----------

   function Near
      (Self    : not null access Graphene_Matrix_T;
       B       : not null access Graphene_Matrix_T;
       Epsilon : Interfaces.C.C_float) return Boolean
   is
      function Internal
         (Self    : access Graphene_Matrix_T;
          B       : access Graphene_Matrix_T;
          Epsilon : Interfaces.C.C_float) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_matrix_near");
   begin
      return Internal (Self, B, Epsilon) /= 0;
   end Near;

   -----------
   -- To_2D --
   -----------

   function To_2D
      (Self : not null access Graphene_Matrix_T;
       Xx   : out Gdouble;
       Yx   : out Gdouble;
       Xy   : out Gdouble;
       Yy   : out Gdouble;
       X_0  : out Gdouble;
       Y_0  : out Gdouble) return Boolean
   is
      function Internal
         (Self    : access Graphene_Matrix_T;
          Acc_Xx  : access Gdouble;
          Acc_Yx  : access Gdouble;
          Acc_Xy  : access Gdouble;
          Acc_Yy  : access Gdouble;
          Acc_X_0 : access Gdouble;
          Acc_Y_0 : access Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_matrix_to_2d");
      Acc_Xx     : aliased Gdouble;
      Acc_Yx     : aliased Gdouble;
      Acc_Xy     : aliased Gdouble;
      Acc_Yy     : aliased Gdouble;
      Acc_X_0    : aliased Gdouble;
      Acc_Y_0    : aliased Gdouble;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Acc_Xx'Access, Acc_Yx'Access, Acc_Xy'Access, Acc_Yy'Access, Acc_X_0'Access, Acc_Y_0'Access);
      Xx := Acc_Xx;
      Yx := Acc_Yx;
      Xy := Acc_Xy;
      Yy := Acc_Yy;
      X_0 := Acc_X_0;
      Y_0 := Acc_Y_0;
      return Tmp_Return /= 0;
   end To_2D;

   -----------------------
   -- Untransform_Point --
   -----------------------

   function Untransform_Point
      (Self   : not null access Graphene_Matrix_T;
       P      : not null access Graphene.Point.Graphene_Point_T;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T;
       Res    : not null access Graphene.Point.Graphene_Point_T)
       return Boolean
   is
      function Internal
         (Self   : access Graphene_Matrix_T;
          P      : access Graphene.Point.Graphene_Point_T;
          Bounds : access Graphene.Rect.Graphene_Rect_T;
          Res    : access Graphene.Point.Graphene_Point_T)
          return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_matrix_untransform_point");
   begin
      return Internal (Self, P, Bounds, Res) /= 0;
   end Untransform_Point;

   ----------------------
   -- From_Object_Free --
   ----------------------

   function From_Object_Free
     (B : not null access Graphene_Matrix_T) return Graphene_Matrix_T
   is
      Result : constant Graphene_Matrix_T := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

end Graphene.Matrix;
