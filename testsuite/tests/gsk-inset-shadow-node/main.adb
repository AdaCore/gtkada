--  Exercise the GtkAda inset-shadow render-node constructors and accessors.
--  There is no matching focused C test in testsuite/c_tests/ to port.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Gdk.RGBA;
with Glib.Test;
with Glib.Bytes;
with Graphene.Rect;
with Gsk.Enums;
with Gsk.Inset_Shadow_Node;
with Gsk.Render_Node;
with Gsk.Rounded_Rect;
with Gtk.Main;
with Interfaces.C;

procedure Main is
   use Glib.Test;
   use type Interfaces.C.C_float;
   use type Glib.Gfloat;
   use type Glib.Gsize;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type Gsk.Render_Node.Gsk_Render_Node;

   --  The render-node wrapper owns the GSK reference; this deallocator calls
   --  its finalizer once each node is no longer needed.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Constructors_And_Accessors with Convention => C;
   procedure Test_Deserialization_Create with Convention => C;

   --  Check every inset-shadow-specific property against the construction
   --  values, including the rectangular bounds and corner radius.
   procedure Check_Properties
     (Node : Gsk.Inset_Shadow_Node.Gsk_Inset_Shadow_Node;
      Dx, Dy, Spread, Blur : Interfaces.C.C_float;
      Color : Gdk.RGBA.Gdk_RGBA;
      Outline : Gsk.Rounded_Rect.Gsk_Rounded_Rect)
   is
      Actual_Color : constant access constant Gdk.RGBA.Gdk_RGBA :=
        Gsk.Inset_Shadow_Node.Get_Color (Node);
      Actual_Outline : constant Gsk.Rounded_Rect.Gsk_Rounded_Rect :=
        Gsk.Inset_Shadow_Node.Get_Outline (Node);
   begin
      --  The node stores the supplied sRGB color and normalized outline.
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Inset_Shadow_Node.Get_Dx (Node)),
         Glib.Gdouble (Dx), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Inset_Shadow_Node.Get_Dy (Node)),
         Glib.Gdouble (Dy), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Inset_Shadow_Node.Get_Spread (Node)),
         Glib.Gdouble (Spread), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Inset_Shadow_Node.Get_Blur_Radius (Node)),
         Glib.Gdouble (Blur), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Color.Red), Glib.Gdouble (Color.Red), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Color.Green), Glib.Gdouble (Color.Green), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Color.Blue), Glib.Gdouble (Color.Blue), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Color.Alpha), Glib.Gdouble (Color.Alpha), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Outline.Bounds.Origin.X),
         Glib.Gdouble (Outline.Bounds.Origin.X), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Outline.Bounds.Origin.Y),
         Glib.Gdouble (Outline.Bounds.Origin.Y), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Outline.Bounds.Size.Width),
         Glib.Gdouble (Outline.Bounds.Size.Width), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Outline.Bounds.Size.Height),
         Glib.Gdouble (Outline.Bounds.Size.Height), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Outline.Corner (1).Width),
         Glib.Gdouble (Outline.Corner (1).Width), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Outline.Corner (1).Height),
         Glib.Gdouble (Outline.Corner (1).Height), 0.001);
   end Check_Properties;

   --  Both the function constructor and Gtk_New delegate to Initialize. A
   --  second Initialize call must leave an already-created immutable node as
   --  it was, so check properties before and after that call.
   procedure Test_Constructors_And_Accessors is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 8.0, Y => 12.0), (Width => 96.0, Height => 48.0));
      Outline : aliased Gsk.Rounded_Rect.Gsk_Rounded_Rect;
      Outline_Result : access Gsk.Rounded_Rect.Gsk_Rounded_Rect;
      Color : aliased Gdk.RGBA.Gdk_RGBA := (0.2, 0.4, 0.7, 0.8);
      Other_Color : aliased Gdk.RGBA.Gdk_RGBA := (0.9, 0.1, 0.3, 1.0);
      Node : Gsk.Inset_Shadow_Node.Gsk_Inset_Shadow_Node;
      Gtk_Node : Gsk.Inset_Shadow_Node.Gsk_Inset_Shadow_Node;
      Node_As_Render : Gsk.Render_Node.Gsk_Render_Node;
      Gtk_Node_As_Render : Gsk.Render_Node.Gsk_Render_Node;
   begin
      Outline_Result := Gsk.Rounded_Rect.Init_From_Rect
        (Outline'Access, Bounds'Access, 6.0);
      Assert_True (Outline_Result /= null);

      Node := Gsk.Inset_Shadow_Node.Gsk_Inset_Shadow_Node_New
        (Outline'Access, Color'Access, 2.5, -3.0, 4.25, 7.5);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Node));
      --  The generic node type confirms the constructor made the requested
      --  concrete kind of render node.
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Node)) =
             Gsk.Enums.Gsk_Inset_Shadow_Node);
      Check_Properties
        (Node, 2.5, -3.0, 4.25, 7.5, Color, Outline);

      --  Initialize is a no-op for a wrapper that already owns a node.
      Gsk.Inset_Shadow_Node.Initialize
        (Node, Outline'Access, Other_Color'Access, 50.0, 60.0, 70.0, 80.0);
      Check_Properties
        (Node, 2.5, -3.0, 4.25, 7.5, Color, Outline);

      --  Also exercise the procedure-style constructor independently.
      Gsk.Inset_Shadow_Node.Gtk_New
        (Gtk_Node, Outline'Access, Other_Color'Access, -1.0, 2.0, 3.0, 4.0);
      Check_Properties
        (Gtk_Node, -1.0, 2.0, 3.0, 4.0, Other_Color, Outline);

      Node_As_Render := Gsk.Render_Node.Gsk_Render_Node (Node);
      Gtk_Node_As_Render := Gsk.Render_Node.Gsk_Render_Node (Gtk_Node);
      Free_Node (Node_As_Render);
      Free_Node (Gtk_Node_As_Render);
   end Test_Constructors_And_Accessors;

   --  Deserialization uses GtkAda's dispatching render-node factory. Casting
   --  its result back to the concrete type exercises this unit's Create hook.
   procedure Test_Deserialization_Create is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 1.0, Y => 2.0), (Width => 30.0, Height => 40.0));
      Outline : aliased Gsk.Rounded_Rect.Gsk_Rounded_Rect;
      Color : aliased Gdk.RGBA.Gdk_RGBA := (0.35, 0.55, 0.15, 0.65);
      Node : Gsk.Inset_Shadow_Node.Gsk_Inset_Shadow_Node;
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Inset : Gsk.Inset_Shadow_Node.Gsk_Inset_Shadow_Node;
   begin
      Outline := Gsk.Rounded_Rect.Init_From_Rect
        (Outline'Access, Bounds'Access, 3.0).all;
      Node := Gsk.Inset_Shadow_Node.Gsk_Inset_Shadow_Node_New
        (Outline'Access, Color'Access, 1.25, 2.5, 3.75, 5.0);
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (Node));
      Assert_True (Serialized.Get_Size > 0);

      --  Recovered properties must match the source node after parsing.
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) = Gsk.Enums.Gsk_Inset_Shadow_Node);
      Restored_Inset :=
        Gsk.Inset_Shadow_Node.Gsk_Inset_Shadow_Node (Restored);
      Check_Properties
        (Restored_Inset, 1.25, 2.5, 3.75, 5.0, Color, Outline);

      Free_Node (Restored);
      --  Free Node through the generic access type to run the controlled
      --  finalizer and release the GSK node exactly once.
      declare
         Original : Gsk.Render_Node.Gsk_Render_Node :=
           Gsk.Render_Node.Gsk_Render_Node (Node);
      begin
         Free_Node (Original);
      end;
   end Test_Deserialization_Create;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/gsk/inset-shadow-node/constructors-accessors",
      Test_Constructors_And_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/inset-shadow-node/deserialization-create",
      Test_Deserialization_Create'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
