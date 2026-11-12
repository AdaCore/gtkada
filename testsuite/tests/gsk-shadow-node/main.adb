--  Exercise the GtkAda shadow render-node constructors and accessors.
--  No focused C testsuite case covers gsk_shadow_node_new or its getters.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Gdk.RGBA;
with Glib;
with Glib.Bytes;
with Glib.Test;
with Graphene.Rect;
with Gsk.Color_Node;
with Gsk.Enums;
with Gsk.Render_Node;
with Gsk.Shadow_Node;
with Gtk.Main;
with Interfaces.C;
with System;

procedure Main is
   use Glib.Test;
   use type Glib.Gsize;
   use type Interfaces.C.C_float;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type Gsk.Render_Node.Gsk_Render_Node;
   use type System.Address;

   --  Deallocating a wrapper runs its controlled finalizer and releases its
   --  reference to the immutable GSK node.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Constructors_And_Accessors with Convention => C;
   procedure Test_Deserialization_Create with Convention => C;
   procedure Test_From_Object_Free with Convention => C;

   --  Compare all fields returned by Get_Shadow with the supplied C record.
   procedure Check_Shadow
     (Actual, Expected : Gsk.Shadow_Node.Gsk_Shadow)
   is
   begin
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual.Color.Red),
         Glib.Gdouble (Expected.Color.Red), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual.Color.Green),
         Glib.Gdouble (Expected.Color.Green), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual.Color.Blue),
         Glib.Gdouble (Expected.Color.Blue), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual.Color.Alpha),
         Glib.Gdouble (Expected.Color.Alpha), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual.Dx), Glib.Gdouble (Expected.Dx), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual.Dy), Glib.Gdouble (Expected.Dy), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual.Radius), Glib.Gdouble (Expected.Radius), 0.001);
   end Check_Shadow;

   --  Exercise the function constructor, Gtk_New, and Initialize's no-op
   --  branch, then check child identity and each shadow parameter.
   procedure Test_Constructors_And_Accessors is
      --  This leaf node makes the test independent of widgets and a renderer.
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 2.0, Y => 3.0), (Width => 40.0, Height => 25.0));
      Child_Color : aliased Gdk.RGBA.Gdk_RGBA := (0.1, 0.3, 0.6, 1.0);
      Child : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node
          (Gsk.Color_Node.Gsk_Color_Node_New
             (Child_Color'Access, Bounds'Access));
      First : constant Gsk.Shadow_Node.Gsk_Shadow :=
        ((Color => (0.8, 0.2, 0.1, 0.7), Dx => 1.5, Dy => -2.0,
          Radius => 3.25));
      Second : constant Gsk.Shadow_Node.Gsk_Shadow :=
        ((Color => (0.2, 0.7, 0.4, 0.9), Dx => -4.0, Dy => 5.5,
          Radius => 6.75));
      Other : constant Gsk.Shadow_Node.Gsk_Shadow :=
        ((Color => (0.9, 0.9, 0.9, 1.0), Dx => 20.0, Dy => 21.0,
          Radius => 22.0));
      Shadows : constant Gsk.Shadow_Node.Gsk_Shadow_Array :=
        (0 => First, 1 => Second);
      --  Distinct replacement data makes Initialize's guard observable.
      Replacement : constant Gsk.Shadow_Node.Gsk_Shadow_Array :=
        (0 => Other);
      Node : Gsk.Shadow_Node.Gsk_Shadow_Node :=
        Gsk.Shadow_Node.Gsk_Shadow_Node_New (Child, Shadows);
      Gtk_Node : Gsk.Shadow_Node.Gsk_Shadow_Node;
      Retrieved_Child : Gsk.Render_Node.Gsk_Render_Node;
      Node_As_Render : Gsk.Render_Node.Gsk_Render_Node;
      Gtk_Node_As_Render : Gsk.Render_Node.Gsk_Render_Node;
   begin
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Node));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Node)) =
             Gsk.Enums.Gsk_Shadow_Node);
      Assert_True (Gsk.Shadow_Node.Get_N_Shadows (Node) = 2);

      --  The child getter returns a separately referenced generic wrapper.
      Retrieved_Child := Gsk.Shadow_Node.Get_Child (Node);
      Assert_True
        (Gsk.Render_Node.Get_Object (Retrieved_Child) =
           Gsk.Render_Node.Get_Object (Child));
      Free_Node (Retrieved_Child);

      --  Indexes are zero-based, matching GSK's C array access convention.
      Check_Shadow (Gsk.Shadow_Node.Get_Shadow (Node, 0), First);
      Check_Shadow (Gsk.Shadow_Node.Get_Shadow (Node, 1), Second);

      --  Reinitializing an immutable, created wrapper must preserve its data.
      Gsk.Shadow_Node.Initialize (Node, Child, Replacement);
      Assert_True (Gsk.Shadow_Node.Get_N_Shadows (Node) = 2);
      Check_Shadow (Gsk.Shadow_Node.Get_Shadow (Node, 0), First);

      --  Gtk_New follows the procedure-style constructor path independently.
      Gsk.Shadow_Node.Gtk_New (Gtk_Node, Child, Replacement);
      Assert_True (Gsk.Shadow_Node.Get_N_Shadows (Gtk_Node) = 1);
      Check_Shadow (Gsk.Shadow_Node.Get_Shadow (Gtk_Node, 0), Other);

      --  Release the wrappers explicitly so each owned render-node reference
      --  is finalized once, including the child shared by both shadow nodes.
      Node_As_Render := Gsk.Render_Node.Gsk_Render_Node (Node);
      Gtk_Node_As_Render := Gsk.Render_Node.Gsk_Render_Node (Gtk_Node);
      Free_Node (Node_As_Render);
      Free_Node (Gtk_Node_As_Render);
      declare
         Child_As_Render : Gsk.Render_Node.Gsk_Render_Node :=
           Gsk.Render_Node.Gsk_Render_Node (Child);
      begin
         Free_Node (Child_As_Render);
      end;
   end Test_Constructors_And_Accessors;

   --  A round trip through GSK deserialization dispatches to this node type's
   --  Create override while checking the reconstructed public properties.
   procedure Test_Deserialization_Create is
      --  Serialization stays entirely in memory and needs no display output.
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 1.0, Y => 2.0), (Width => 30.0, Height => 20.0));
      Child_Color : aliased Gdk.RGBA.Gdk_RGBA := (0.25, 0.5, 0.75, 1.0);
      Child : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node
          (Gsk.Color_Node.Gsk_Color_Node_New
             (Child_Color'Access, Bounds'Access));
      Expected : constant Gsk.Shadow_Node.Gsk_Shadow :=
        ((Color => (0.4, 0.3, 0.2, 0.8), Dx => 3.0, Dy => -1.0,
          Radius => 4.5));
      Shadows : constant Gsk.Shadow_Node.Gsk_Shadow_Array :=
        (0 => Expected);
      Node : Gsk.Shadow_Node.Gsk_Shadow_Node :=
        Gsk.Shadow_Node.Gsk_Shadow_Node_New (Child, Shadows);
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Shadow : Gsk.Shadow_Node.Gsk_Shadow_Node;
   begin
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (Node));
      Assert_True (Serialized.Get_Size > 0);
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) = Gsk.Enums.Gsk_Shadow_Node);
      Restored_Shadow := Gsk.Shadow_Node.Gsk_Shadow_Node (Restored);
      Assert_True (Gsk.Shadow_Node.Get_N_Shadows (Restored_Shadow) = 1);
      Check_Shadow
        (Gsk.Shadow_Node.Get_Shadow (Restored_Shadow, 0), Expected);

      Free_Node (Restored);
      declare
         Original : Gsk.Render_Node.Gsk_Render_Node :=
           Gsk.Render_Node.Gsk_Render_Node (Node);
         Child_As_Render : Gsk.Render_Node.Gsk_Render_Node :=
           Gsk.Render_Node.Gsk_Render_Node (Child);
      begin
         Free_Node (Original);
         Free_Node (Child_As_Render);
      end;
   end Test_Deserialization_Create;

   --  From_Object_Free consumes a GLib-allocated record, so allocate its
   --  input with g_malloc and verify the copied value after conversion.
   procedure Test_From_Object_Free is
      function G_Malloc (Size : Glib.Gsize) return System.Address;
      pragma Import (C, G_Malloc, "g_malloc");

      type Shadow_Access is access all Gsk.Shadow_Node.Gsk_Shadow;
      function To_Shadow_Access is new Ada.Unchecked_Conversion
        (System.Address, Shadow_Access);

      Expected : constant Gsk.Shadow_Node.Gsk_Shadow :=
        ((Color => (0.15, 0.35, 0.55, 0.75), Dx => 1.0, Dy => 2.0,
          Radius => 3.0));
      Actual : Gsk.Shadow_Node.Gsk_Shadow;
      Size : constant Glib.Gsize :=
        --  g_malloc takes byte counts, so round the Ada bit size upward.
        Glib.Gsize
          ((Gsk.Shadow_Node.Gsk_Shadow'Size + System.Storage_Unit - 1)
           / System.Storage_Unit);
      Wrapper : constant Shadow_Access := To_Shadow_Access (G_Malloc (Size));
   begin
      Wrapper.all := Expected;
      Actual := Gsk.Shadow_Node.From_Object_Free (Wrapper);
      Check_Shadow (Actual, Expected);
   end Test_From_Object_Free;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/gsk/shadow-node/constructors-accessors",
      Test_Constructors_And_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/shadow-node/deserialization-create",
      Test_Deserialization_Create'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/shadow-node/from-object-free",
      Test_From_Object_Free'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
