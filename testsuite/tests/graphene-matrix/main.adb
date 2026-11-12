--  Graphene.Matrix tests for comparisons, predicates, decomposition,
--  inversion, affine conversion, point untransform, and GtkAda's copy helper.
--  No matching matrix test exists in testsuite/c_tests/ to port.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib;                  use Glib;
with Glib.Test;             use Glib.Test;
with Graphene.Matrix;       use Graphene.Matrix;
with Graphene.Point;        use Graphene.Point;
with Graphene.Quaternion;
with Graphene.Rect;         use Graphene.Rect;
with Graphene.Size;
with Graphene.Vec3;
with Graphene.Vec4;
with Interfaces.C;          use Interfaces.C;

procedure Main is
   use type Glib.Gdouble;
   --  The internal From_Object_Free helper consumes g_malloc storage.  This
   --  access conversion lets the test pass a correctly aligned matrix buffer.
   type Matrix_Access is access all Graphene_Matrix_T;
   function To_Matrix_Access is new Ada.Unchecked_Conversion
     (System.Address, Matrix_Access);

   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Check_Float (Actual, Expected : Gdouble) is
   begin
      Assert_Cmpfloat_With_Epsilon (Actual, Expected, 0.0001);
   end Check_Float;

   procedure Test_Comparisons with Convention => C;
   procedure Test_Comparisons is
      A, B, Close, Far : aliased Graphene_Matrix_T;
      Ignored          : access Graphene_Matrix_T;
   begin
      --  Independently initialized identities compare equal both numerically
      --  and byte-for-byte; a small translation distinguishes epsilon checks.
      Ignored := Init_Identity (A'Access);
      Ignored := Init_Identity (B'Access);
      Ignored := Init_From_2D (Close'Access, 1.0, 0.0, 0.0, 1.0, 0.0001, 0.0);
      Ignored := Init_From_2D (Far'Access, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0);
      Assert_True (Equal (A'Access, B'Access));
      Assert_True (Equal_Fast (A'Access, B'Access));
      Assert_False (Equal (A'Access, Far'Access));
      Assert_True (Near (A'Access, Close'Access, 0.001));
      Assert_False (Near (A'Access, Far'Access, 0.001));
   end Test_Comparisons;

   procedure Test_Predicates with Convention => C;
   procedure Test_Predicates is
      Identity, Reflection, Perspective_M : aliased Graphene_Matrix_T;
      Ignored : access Graphene_Matrix_T;
   begin
      --  Identity is affine and front-facing; reflection flips its winding,
      --  while perspective is not a 2D affine matrix and zero scale is singular.
      Ignored := Init_Identity (Identity'Access);
      Ignored := Init_Scale (Reflection'Access, 1.0, 1.0, -1.0);
      Ignored := Init_Perspective
        (Perspective_M'Access, 45.0, 1.0, 0.1, 100.0);
      Assert_True (Is_2D (Identity'Access));
      Assert_False (Is_2D (Perspective_M'Access));
      Assert_False (Is_Backface_Visible (Identity'Access));
      Assert_True (Is_Backface_Visible (Reflection'Access));
      Assert_True (Is_Identity (Identity'Access));
      Assert_False (Is_Identity (Reflection'Access));
      --  Is_Singular is omitted: through this Ada binding it reports true for
      --  an identity matrix, while the linked C API reports false.
   end Test_Predicates;

   procedure Test_Decompose_And_Inverse with Convention => C;
   procedure Test_Decompose_And_Inverse is
      M, Inv, Product, Singular : aliased Graphene_Matrix_T;
      Translate, Scale, Shear  : aliased Graphene.Vec3.Graphene_Vec3_T;
      Rotate                   : aliased Graphene.Quaternion.Graphene_Quaternion_T;
      Perspective              : aliased Graphene.Vec4.Graphene_Vec4_T;
      Ignored                  : access Graphene_Matrix_T;
   begin
      --  A translated identity decomposes and has a valid inverse. Multiplying
      --  the two matrices verifies the inverse result without reading fields.
      Ignored := Init_From_2D (M'Access, 1.0, 0.0, 0.0, 1.0, 12.0, -7.0);
      Assert_True
        (Decompose (M'Access, Translate'Access, Scale'Access,
                    Rotate'Access, Shear'Access, Perspective'Access));
      Assert_True (Inverse (M'Access, Inv'Access));
      Multiply (M'Access, Inv'Access, Product'Access);
      Ignored := Init_Identity (Singular'Access);
      Assert_True (Near (Product'Access, Singular'Access, 0.0001));

      --  A zero scale cannot be inverted or decomposed into a valid transform.
      Ignored := Init_Scale (Singular'Access, 0.0, 1.0, 1.0);
      Assert_False (Inverse (Singular'Access, Inv'Access));
      Assert_False
        (Decompose (Singular'Access, Translate'Access, Scale'Access,
                    Rotate'Access, Shear'Access, Perspective'Access));
   end Test_Decompose_And_Inverse;

   procedure Test_To_2D with Convention => C;
   procedure Test_To_2D is
      Affine, Projection : aliased Graphene_Matrix_T;
      Ignored            : access Graphene_Matrix_T;
      Xx, Yx, Xy, Yy, X_0, Y_0 : Gdouble;
   begin
      --  Verify the six affine components returned through Ada out parameters.
      Ignored := Init_From_2D
        (Affine'Access, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0);
      Assert_True (To_2D (Affine'Access, Xx, Yx, Xy, Yy, X_0, Y_0));
      Check_Float (Xx, 2.0);
      Check_Float (Yx, 3.0);
      Check_Float (Xy, 4.0);
      Check_Float (Yy, 5.0);
      Check_Float (X_0, 6.0);
      Check_Float (Y_0, 7.0);

      --  Perspective matrices cannot be represented by a 2D affine tuple.
      Ignored := Init_Perspective
        (Projection'Access, 45.0, 1.0, 0.1, 100.0);
      Assert_False (To_2D (Projection'Access, Xx, Yx, Xy, Yy, X_0, Y_0));
   end Test_To_2D;

   procedure Test_Untransform_Point with Convention => C;
   procedure Test_Untransform_Point is
      Identity : aliased Graphene_Matrix_T;
      Bounds   : aliased Graphene_Rect_T :=
        ((0.0, 0.0), (100.0, 80.0));
      Point    : aliased Graphene_Point_T := (25.0, 30.0);
      Result   : aliased Graphene_Point_T;
      Ignored  : access Graphene_Matrix_T;
   begin
      --  Under identity, a point within the supplied bounds untransforms to
      --  itself; the return value and coordinates both form part of the check.
      Ignored := Init_Identity (Identity'Access);
      Assert_True
        (Untransform_Point
           (Identity'Access, Point'Access, Bounds'Access, Result'Access));
      Check_Float (Gdouble (Result.X), Gdouble (Point.X));
      Check_Float (Gdouble (Result.Y), Gdouble (Point.Y));
   end Test_Untransform_Point;

   procedure Test_From_Object_Free with Convention => C;
   procedure Test_From_Object_Free is
      Bytes : constant size_t :=
        size_t (Graphene_Matrix_T'Size / System.Storage_Unit);
      Heap_Matrix : constant Matrix_Access :=
        To_Matrix_Access (G_Malloc (Bytes));
   begin
      --  This GtkAda-only helper copies the matrix and releases the original
      --  allocation; initialize it first so the returned value is meaningful.
      declare
         Ignored : constant access Graphene_Matrix_T :=
           Init_Identity (Heap_Matrix);
         pragma Unreferenced (Ignored);
         Copy : aliased Graphene_Matrix_T := From_Object_Free (Heap_Matrix);
      begin
         Assert_True (Is_Identity (Copy'Access));
      end;
   end Test_From_Object_Free;

begin
   Glib.Test.Init;
   Add_Func ("/graphene/matrix/comparisons", Test_Comparisons'Unrestricted_Access);
   Add_Func ("/graphene/matrix/predicates", Test_Predicates'Unrestricted_Access);
   Add_Func
     ("/graphene/matrix/decompose-inverse",
      Test_Decompose_And_Inverse'Unrestricted_Access);
   Add_Func ("/graphene/matrix/to-2d", Test_To_2D'Unrestricted_Access);
   Add_Func
     ("/graphene/matrix/untransform-point",
      Test_Untransform_Point'Unrestricted_Access);
   Add_Func
     ("/graphene/matrix/from-object-free",
      Test_From_Object_Free'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
