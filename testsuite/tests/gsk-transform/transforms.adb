--  Gsk.Transform tests for composition, conversions and ownership.
--  No matching transform test exists in testsuite/c_tests/ to port.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with Glib;               use Glib;
with Glib.Object;
with Glib.String;        use Glib.String;
with Glib.Test;          use Glib.Test;
with Graphene.Point;     use Graphene.Point;
with Graphene.Rect;      use Graphene.Rect;
with Graphene.Point3d;   use Graphene.Point3d;
with Graphene.Vec3;      use Graphene.Vec3;
with Graphene.Matrix;    use Graphene.Matrix;
with Gsk.Enums;          use Gsk.Enums;
with Gsk.Transform;      use Gsk.Transform;
with Gtk.Fixed;          use Gtk.Fixed;
with Gtk.Fixed_Layout;   use Gtk.Fixed_Layout;
with Gtk.Label;          use Gtk.Label;
with Gtk.Layout_Manager; use Gtk.Layout_Manager;
with Gtk.Main;
with Interfaces.C;       use Interfaces.C;
with System;

procedure Transforms is
   use type System.Address;

   --  From_Object_Free consumes a g_malloc-allocated Ada wrapper, so use an
   --  unchecked address conversion only to supply correctly aligned storage.
   type Transform_Access is access all Gsk_Transform;
   function To_Transform_Access is new Ada.Unchecked_Conversion
     (System.Address, Transform_Access);

   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Near (Actual, Expected : C_float) is
   begin
      Assert_Cmpfloat_With_Epsilon
        (Gdouble (Actual), Gdouble (Expected), 0.001);
   end Near;

   procedure Test_Composition with
     Convention => C;
   procedure Test_Composition is
      T, Saved, Inverse, Parsed, Singular : Gsk_Transform;
      Translation : aliased Graphene_Point_T := (40.0, 60.0);
      Point : aliased Graphene_Point_T := (1.0, 2.0);
      Result, Restored                    : aliased Graphene_Point_T;
   begin
      T := Gsk_Transform_New;
      Assert_True (T.Get_Category = Gsk_Transform_Category_Identity);
      T     := T.Translate (Translation'Access);
      Saved := T.Ref;
      T     := T.Rotate (90.0);
      T     := T.Scale (2.0, 3.0);
      T.Transform_Point (Point'Access, Result'Access);
      Near (Result.X, 34.0);
      Near (Result.Y, 62.0);
      --  Composing consumes the caller's reference, leaving Saved valid.
      Saved.Transform_Point (Point'Access, Restored'Access);
      Near (Restored.X, 41.0);
      Near (Restored.Y, 62.0);
      Saved.Unref;
      Inverse := T.Ref.Invert;
      Inverse.Transform_Point (Result'Access, Restored'Access);
      Near (Restored.X, Point.X);
      Near (Restored.Y, Point.Y);
      Assert_True (Parse (T.To_String, Parsed));
      Assert_True (T.Equal (Parsed));
      Parsed.Unref;
      Assert_False (Parse ("invalid transform", Parsed));
      Assert_Null (Get_Object (Parsed));
      Singular := Scale (Gsk_Transform_New, 0.0, 1.0);
      Singular := Singular.Invert;
      Assert_Null (Get_Object (Singular));
      Inverse.Unref;
      T.Unref;
   end Test_Composition;

   procedure Test_3D with
     Convention => C;
   procedure Test_3D is
      T       : Gsk_Transform;
      P       : aliased Graphene_Point3D_T := (10.0, 20.0, 30.0);
      Input   : aliased Graphene_Point3D_T := (1.0, 2.0, 3.0);
      Result  : aliased Graphene_Point3D_T;
      Axis    : aliased Graphene_Vec3_T;
      Ignored : access Graphene_Vec3_T;
      M       : aliased Graphene_Matrix_T;
   begin
      Ignored := Init (Axis'Access, 0.0, 1.0, 0.0);
      T       := Translate_3D (Gsk_Transform_New, P'Access);
      T       := T.Rotate_3D (90.0, Axis'Access);
      T       := T.Scale_3D (2.0, 3.0, 4.0);
      T.To_Matrix (M'Access);
      Transform_Point3D (M'Access, Input'Access, Result'Access);
      Near (Result.X, 22.0);
      Near (Result.Y, 26.0);
      Near (Result.Z, 28.0);
      Assert_True (T.Get_Category = Gsk_Transform_Category_3D);
      T := T.Perspective (600.0);
      Assert_True (T.Get_Category = Gsk_Transform_Category_Any);
      T.Unref;
   end Test_3D;

   procedure Test_Skew with
     Convention => C;
   procedure Test_Skew is
      T : Gsk_Transform           := Skew (Gsk_Transform_New, 45.0, 0.0);
      Bounds : aliased Graphene_Rect_T := ((0.0, 0.0), (100.0, 40.0));
      Result                 : aliased Graphene_Rect_T;
      XX, YX, XY, YY, DX, DY : C_float;
   begin
      T.Transform_Bounds (Bounds'Access, Result'Access);
      Near (Result.Origin.X, 0.0);
      Near (Result.Origin.Y, 0.0);
      Near (Result.Size.Width, 140.0);
      Near (Result.Size.Height, 40.0);
      T.To_2D (XX, YX, XY, YY, DX, DY);
      Near (XX, 1.0);
      Near (YX, 0.0);
      Near (XY, 1.0);
      Near (YY, 1.0);
      Near (DX, 0.0);
      Near (DY, 0.0);
      T.Unref;
   end Test_Skew;

   procedure Test_Affine_Conversions with
     Convention => C;
   procedure Test_Affine_Conversions is
      T, By_Procedure : Gsk_Transform;
      M, Matrix_Result : aliased Graphene_Matrix_T;
      Ignored         : access Graphene_Matrix_T;
      Output          : Gstring := Gstring_New ("prefix:");
      DX, DY : C_float;
      Skew_X, Skew_Y, Scale_X, Scale_Y, Angle : C_float;
   begin
      --  The procedure constructor returns the same identity category as the
      --  function constructor, and gives the caller an owned reference.
      Gtk_New (By_Procedure);
      Assert_True
        (By_Procedure.Get_Category = Gsk_Transform_Category_Identity);
      By_Procedure.Unref;

      --  Convert an affine Graphene matrix to a transform, then recover its
      --  full 4x4 representation. Matrix transforms are categorized as 3D.
      Ignored := Init_From_2D (M'Access, 2.0, 0.0, 0.0, 3.0, 5.0, 7.0);
      T := Matrix (Gsk_Transform_New, M'Access);
      T.To_Matrix (Matrix_Result'Access);
      Assert_True (Equal (M'Access, Matrix_Result'Access));
      T.Unref;

      --  Matrix_2D accepts the same six affine values directly. Its
      --  decomposition and affine forms expose the scale and translation.
      T := Matrix_2D (Gsk_Transform_New, 2.0, 0.0, 0.0, 3.0, 5.0, 7.0);
      T.To_2D_Components
        (Skew_X, Skew_Y, Scale_X, Scale_Y, Angle, DX, DY);
      Near (Skew_X, 0.0);
      Near (Skew_Y, 0.0);
      Near (Scale_X, 2.0);
      Near (Scale_Y, 3.0);
      Near (Angle, 0.0);
      Near (DX, 5.0);
      Near (DY, 7.0);
      T.To_Affine (Scale_X, Scale_Y, DX, DY);
      Near (Scale_X, 2.0);
      Near (Scale_Y, 3.0);
      Near (DX, 5.0);
      Near (DY, 7.0);

      --  Print appends the canonical transform representation to an existing
      --  GString; compare it with To_String and retain the original prefix.
      T.Print (Output);
      Assert_Cmpstr_Eq (Free_And_Steal (Output), "prefix:" & T.To_String);
      T.Unref;
   end Test_Affine_Conversions;

   procedure Test_Transform_And_Translation with
     Convention => C;
   procedure Test_Transform_And_Translation is
      First, Second, Combined : Gsk_Transform;
      A : aliased Graphene_Point_T := (2.0, 3.0);
      B : aliased Graphene_Point_T := (4.0, 5.0);
      DX, DY : C_float;
   begin
      --  The translation accessor recovers the coordinates used to build a
      --  pure translation transform.
      First := Translate (Gsk_Transform_New, A'Access);
      First.To_Translate (DX, DY);
      Near (DX, 2.0);
      Near (DY, 3.0);

      --  Transform composes two translations. The operation consumes First,
      --  while Second remains independently owned by this test.
      Second := Translate (Gsk_Transform_New, B'Access);
      Combined := First.Transform (Second);
      Combined.To_Translate (DX, DY);
      Near (DX, 6.0);
      Near (DY, 8.0);
      Combined.Unref;
      Second.Unref;
   end Test_Transform_And_Translation;

   procedure Test_From_Object_Free with
     Convention => C;
   procedure Test_From_Object_Free is
      Bytes : constant size_t :=
        size_t (Gsk_Transform'Size / System.Storage_Unit);
      Heap_Transform : constant Transform_Access :=
        To_Transform_Access (G_Malloc (Bytes));
   begin
      --  Store an owned identity wrapper in C-allocated storage. The helper
      --  copies it and frees that storage, leaving the transform reference
      --  owned by the returned value.
      Heap_Transform.all := Gsk_Transform_New;
      declare
         Copy : constant Gsk_Transform := From_Object_Free (Heap_Transform);
      begin
         Assert_True (Copy.Get_Category = Gsk_Transform_Category_Identity);
         Copy.Unref;
      end;
   end Test_From_Object_Free;

   procedure Test_Layout with
     Convention => C;
   procedure Test_Layout_Constructors with
     Convention => C;

   --  Both constructor forms create a native GtkFixedLayout. Calling
   --  Initialize again verifies its documented no-op behavior on created
   --  objects, including preservation of the original GObject instances.
   procedure Test_Layout_Constructors is
      By_Function : constant Gtk_Fixed_Layout := Gtk_Fixed_Layout_New;
      By_Procedure : Gtk_Fixed_Layout;
      Function_Object : constant System.Address :=
        Glib.Object.Get_Object (By_Function);
   begin
      Gtk_New (By_Procedure);

      --  Dynamic types and non-null addresses confirm both wrappers hold
      --  separately created native layout managers.
      Assert_True (Gtk.Fixed_Layout.Get_Type /= 0);
      Assert_True (By_Function.Get_Type = Gtk.Fixed_Layout.Get_Type);
      Assert_True (By_Procedure.Get_Type = Gtk.Fixed_Layout.Get_Type);
      Assert_True (Function_Object /= System.Null_Address);
      Assert_True
        (Glib.Object.Get_Object (By_Procedure) /= System.Null_Address);

      Gtk.Fixed_Layout.Initialize (By_Function);
      Gtk.Fixed_Layout.Initialize (By_Procedure);
      Assert_True
        (Glib.Object.Get_Object (By_Function) = Function_Object);
      Assert_True (By_Procedure.Get_Type = Gtk.Fixed_Layout.Get_Type);

      --  Release the references acquired by the two constructors.
      By_Function.Unref;
      By_Procedure.Unref;
   end Test_Layout_Constructors;

   procedure Test_Layout is
      Container    : Gtk_Fixed;
      Child        : Gtk_Label;
      Layout_Child : Gtk_Fixed_Layout_Child;
      T            : Gsk_Transform;
      P            : aliased Graphene_Point_T := (12.5, 25.25);
      Origin       : aliased Graphene_Point_T := (0.0, 0.0);
      Result       : aliased Graphene_Point_T;
      X, Y         : Gdouble;
   begin
      Gtk_New (Container);
      Container.Ref_Sink;
      Gtk_New (Child, "transformed child");
      Container.Put (Child, 0.0, 0.0);
      T := Translate (Gsk_Transform_New, P'Access);
      T := T.Scale (2.0, 3.0);
      Container.Set_Child_Transform (Child, T);
      Layout_Child :=
        Gtk_Fixed_Layout_Child
          (Container.Get_Layout_Manager.Get_Layout_Child (Child));
      Assert_True (Layout_Child.Get_Transform.Equal (T));
      T.Unref;
      --  The layout owns a reference after the caller drops its own.
      Container.Get_Child_Transform (Child).Transform_Point
        (Origin'Access, Result'Access);
      Near (Result.X, 12.5);
      Near (Result.Y, 25.25);
      Container.Get_Layout_Manager.Allocate (Container, 400, 300, -1);
      Container.Get_Child_Position (Child, X, Y);
      Assert_Cmpfloat_Eq (X, 12.5);
      Assert_Cmpfloat_Eq (Y, 25.25);
      Layout_Child.Set_Transform (Null_Gsk_Transform);
      Assert_True
        (Container.Get_Child_Transform (Child).Equal (Null_Gsk_Transform));
      Container.Unref;
   end Test_Layout;
begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func
     ("/gsk/transform/composition", Test_Composition'Unrestricted_Access);
   Add_Func ("/gsk/transform/3d", Test_3D'Unrestricted_Access);
   Add_Func ("/gsk/transform/skew", Test_Skew'Unrestricted_Access);
   Add_Func
     ("/gsk/transform/affine-conversions",
      Test_Affine_Conversions'Unrestricted_Access);
   Add_Func
     ("/gsk/transform/transform-translation",
      Test_Transform_And_Translation'Unrestricted_Access);
   Add_Func
     ("/gsk/transform/from-object-free",
      Test_From_Object_Free'Unrestricted_Access);
   Add_Func
     ("/gsk/transform/fixed-layout-constructors",
      Test_Layout_Constructors'Unrestricted_Access);
   Add_Func ("/gsk/transform/fixed-layout", Test_Layout'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Transforms;
