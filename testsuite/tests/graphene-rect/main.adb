--  Graphene.Rect tests for vertices, predicates, intersections, and the
--  GtkAda copy-and-free helper. No graphene_rect C test exists to port.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib.Test;       use Glib.Test;
with Graphene.Point;
with Graphene.Rect;   use Graphene.Rect;
with Graphene.Vec2;
with Interfaces.C;    use Interfaces.C;

procedure Main is
   use type Interfaces.C.C_float;

   --  From_Object_Free releases a g_malloc allocation, as used by Graphene's
   --  heap-returning boxed APIs.
   type Rect_Access is access all Graphene_Rect_T;
   function To_Rect_Access is new Ada.Unchecked_Conversion
     (System.Address, Rect_Access);

   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Check_Float (Actual, Expected : C_float) is
   begin
      --  Graphene stores coordinates as single precision floats.
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual), Glib.Gdouble (Expected), 0.0001);
   end Check_Float;

   function Make_Rect
     (X, Y, Width, Height : C_float) return Graphene_Rect_T
   is
      Result : aliased Graphene_Rect_T;
      Ignored : access Graphene_Rect_T;
   begin
      --  Init returns its output pointer; only the initialized value matters.
      Ignored := Graphene.Rect.Init
        (Result'Access, X, Y, Width, Height);
      return Result;
   end Make_Rect;

   procedure Test_Predicates_And_Vertices with Convention => C;
   procedure Test_Predicates_And_Vertices is
      Whole : aliased Graphene_Rect_T := Make_Rect (0.0, 0.0, 10.0, 8.0);
      Same  : aliased Graphene_Rect_T := Make_Rect (0.0, 0.0, 10.0, 8.0);
      Part  : aliased Graphene_Rect_T := Make_Rect (2.0, 1.0, 3.0, 4.0);
      Away  : aliased Graphene_Rect_T := Make_Rect (20.0, 20.0, 2.0, 2.0);
      Inside_Point : aliased Graphene.Point.Graphene_Point_T := (5.0, 4.0);
      Outside_Point : aliased Graphene.Point.Graphene_Point_T := (12.0, 4.0);
      Vertices : Graphene_Vec2_Array4;

      --  Check the four corner coordinates without depending on vertex order.
      type Corner is record
         X, Y : C_float;
      end record;
      Corners : constant array (Positive range 1 .. 4) of Corner :=
        ((0.0, 0.0), (10.0, 0.0), (10.0, 8.0), (0.0, 8.0));
   begin
      --  Exercise both truth values for point and rectangle containment.
      Assert_True (Contains_Point (Whole'Access, Inside_Point'Access));
      Assert_False (Contains_Point (Whole'Access, Outside_Point'Access));
      Assert_True (Contains_Rect (Whole'Access, Part'Access));
      Assert_False (Contains_Rect (Part'Access, Whole'Access));

      --  Equality must accept matching rectangles and reject different ones.
      Assert_True (Equal (Whole'Access, Same'Access));
      Assert_False (Equal (Whole'Access, Part'Access));

      --  Compare the returned corners as a set because their order is not
      --  needed to verify the public geometry contract.
      Get_Vertices (Whole'Access, Vertices);
      for Expected of Corners loop
         declare
            Found : Boolean := False;
         begin
            for Actual of Vertices loop
               declare
                  --  Vec2 fields are private, so inspect a local through its
                  --  public coordinate accessors.
                  Value : aliased Graphene.Vec2.Graphene_Vec2_T := Actual;
               begin
                  if Graphene.Vec2.Get_X (Value'Access) = Expected.X
                    and then Graphene.Vec2.Get_Y (Value'Access) = Expected.Y
                  then
                     Found := True;
                  end if;
               end;
            end loop;
            Assert_True (Found);
         end;
      end loop;
   end Test_Predicates_And_Vertices;

   procedure Test_Intersection with Convention => C;
   procedure Test_Intersection is
      Whole : aliased Graphene_Rect_T := Make_Rect (0.0, 0.0, 10.0, 8.0);
      Part  : aliased Graphene_Rect_T := Make_Rect (2.0, 1.0, 3.0, 4.0);
      Away  : aliased Graphene_Rect_T := Make_Rect (20.0, 20.0, 2.0, 2.0);
      Result : aliased Graphene_Rect_T;
   begin
      --  The overlapping case fills the supplied output rectangle.
      Assert_True (Intersection (Whole'Access, Part'Access, Result'Access));
      Check_Float (Get_X (Result'Access), 2.0);
      Check_Float (Get_Y (Result'Access), 1.0);
      Check_Float (Get_Width (Result'Access), 3.0);
      Check_Float (Get_Height (Result'Access), 4.0);

      --  A disjoint pair reports false and writes the documented zero rect.
      Assert_False (Intersection (Whole'Access, Away'Access, Result'Access));
      Check_Float (Get_X (Result'Access), 0.0);
      Check_Float (Get_Y (Result'Access), 0.0);
      Check_Float (Get_Width (Result'Access), 0.0);
      Check_Float (Get_Height (Result'Access), 0.0);
   end Test_Intersection;

   procedure Test_Copy_And_Free with Convention => C;
   procedure Test_Copy_And_Free is
      Source : aliased Graphene_Rect_T := Make_Rect (3.0, 4.0, 5.0, 6.0);
      Copy   : aliased Graphene_Rect_T;
      Bytes  : constant size_t :=
        size_t (Graphene_Rect_T'Size / System.Storage_Unit);
      Heap_Rect : constant Rect_Access :=
        To_Rect_Access (G_Malloc (Bytes));
      Ignored : access Graphene_Rect_T;
   begin
      --  Initialize the C-allocated record before the helper copies and frees it.
      Ignored := Graphene.Rect.Init
        (Heap_Rect, 3.0, 4.0, 5.0, 6.0);
      Copy := From_Object_Free (Heap_Rect);

      --  The returned value remains intact after the heap allocation is freed.
      Check_Float (Get_X (Copy'Access), 3.0);
      Check_Float (Get_Y (Copy'Access), 4.0);
      Check_Float (Get_Width (Copy'Access), 5.0);
      Check_Float (Get_Height (Copy'Access), 6.0);
   end Test_Copy_And_Free;

begin
   Glib.Test.Init;
   Add_Func
     ("/graphene/rect/predicates-vertices",
      Test_Predicates_And_Vertices'Unrestricted_Access);
   Add_Func
     ("/graphene/rect/intersection",
      Test_Intersection'Unrestricted_Access);
   Add_Func
     ("/graphene/rect/copy-free",
      Test_Copy_And_Free'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
