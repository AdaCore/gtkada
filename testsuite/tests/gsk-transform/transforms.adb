with Ada.Command_Line;
with Glib;               use Glib;
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

   procedure Test_Layout with
     Convention => C;
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
   Add_Func ("/gsk/transform/fixed-layout", Test_Layout'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Transforms;
