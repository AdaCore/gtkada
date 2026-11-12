--  Exercise the GtkAda outset-shadow render-node constructors and accessors.
--  There is no matching focused C test in testsuite/c_tests/ to port.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Gdk.RGBA;
with Glib.Bytes;
with Glib.Test;
with Graphene.Rect;
with Gsk.Enums;
with Gsk.Outset_Shadow_Node;
with Gsk.Render_Node;
with Gsk.Rounded_Rect;
with Gtk.Main;
with Interfaces.C;

procedure Main is
   use Glib.Test;
   use type Interfaces.C.C_float;
   use type Glib.Gsize;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type Gsk.Render_Node.Gsk_Render_Node;

   --  The render-node wrapper owns its GSK reference; deallocation runs the
   --  wrapper finalizer and releases that reference exactly once.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Constructors_And_Accessors with Convention => C;
   procedure Test_Deserialization_Create with Convention => C;

   --  Check every outset-shadow getter against the values used to construct
   --  the node, including the rounded outline and all four color components.
   procedure Check_Properties
     (Node : Gsk.Outset_Shadow_Node.Gsk_Outset_Shadow_Node;
      Dx, Dy, Spread, Blur : Interfaces.C.C_float;
      Color : Gdk.RGBA.Gdk_RGBA;
      Outline : Gsk.Rounded_Rect.Gsk_Rounded_Rect)
   is
      Actual_Color : constant access constant Gdk.RGBA.Gdk_RGBA :=
        Gsk.Outset_Shadow_Node.Get_Color (Node);
      Actual_Outline : constant Gsk.Rounded_Rect.Gsk_Rounded_Rect :=
        Gsk.Outset_Shadow_Node.Get_Outline (Node);
   begin
      --  C float values are compared with a small tolerance for conversion.
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Outset_Shadow_Node.Get_Dx (Node)),
         Glib.Gdouble (Dx), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Outset_Shadow_Node.Get_Dy (Node)),
         Glib.Gdouble (Dy), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Outset_Shadow_Node.Get_Spread (Node)),
         Glib.Gdouble (Spread), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Outset_Shadow_Node.Get_Blur_Radius (Node)),
         Glib.Gdouble (Blur), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Color.Red), Glib.Gdouble (Color.Red), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Color.Green), Glib.Gdouble (Color.Green), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Color.Blue), Glib.Gdouble (Color.Blue), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Color.Alpha), Glib.Gdouble (Color.Alpha), 0.001);
      --  Bounds and a corner radius confirm the full rounded-rectangle value.
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

   --  Exercise both constructors and Initialize's already-created guard; the
   --  immutable node must retain its original properties after reinitializing.
   procedure Test_Constructors_And_Accessors is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 8.0, Y => 12.0), (Width => 96.0, Height => 48.0));
      Outline : aliased Gsk.Rounded_Rect.Gsk_Rounded_Rect;
      Outline_Result : access Gsk.Rounded_Rect.Gsk_Rounded_Rect;
      Color : aliased Gdk.RGBA.Gdk_RGBA := (0.2, 0.4, 0.7, 0.8);
      Other_Color : aliased Gdk.RGBA.Gdk_RGBA := (0.9, 0.1, 0.3, 1.0);
      Node : Gsk.Outset_Shadow_Node.Gsk_Outset_Shadow_Node;
      Gtk_Node : Gsk.Outset_Shadow_Node.Gsk_Outset_Shadow_Node;
      Node_As_Render : Gsk.Render_Node.Gsk_Render_Node;
      Gtk_Node_As_Render : Gsk.Render_Node.Gsk_Render_Node;
   begin
      Outline_Result := Gsk.Rounded_Rect.Init_From_Rect
        (Outline'Access, Bounds'Access, 6.0);
      Assert_True (Outline_Result /= null);

      Node := Gsk.Outset_Shadow_Node.Gsk_Outset_Shadow_Node_New
        (Outline'Access, Color'Access, 2.5, -3.0, 4.25, 7.5);
      --  Confirm both the generic wrapper and the concrete GSK node kind.
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Node));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Node)) =
             Gsk.Enums.Gsk_Outset_Shadow_Node);
      Check_Properties (Node, 2.5, -3.0, 4.25, 7.5, Color, Outline);

      --  A second call does nothing because the wrapper already owns a node.
      Gsk.Outset_Shadow_Node.Initialize
        (Node, Outline'Access, Other_Color'Access, 50.0, 60.0, 70.0, 80.0);
      Check_Properties (Node, 2.5, -3.0, 4.25, 7.5, Color, Outline);

      --  Gtk_New is the procedure-style constructor and accepts distinct data.
      Gsk.Outset_Shadow_Node.Gtk_New
        (Gtk_Node, Outline'Access, Other_Color'Access, -1.0, 2.0, 3.0, 4.0);
      Check_Properties (Gtk_Node, -1.0, 2.0, 3.0, 4.0, Other_Color, Outline);

      Node_As_Render := Gsk.Render_Node.Gsk_Render_Node (Node);
      Gtk_Node_As_Render := Gsk.Render_Node.Gsk_Render_Node (Gtk_Node);
      Free_Node (Node_As_Render);
      Free_Node (Gtk_Node_As_Render);
   end Test_Constructors_And_Accessors;

   --  Deserialization recreates concrete render-node wrappers through the
   --  dispatching factory, which invokes this unit's Create override.
   procedure Test_Deserialization_Create is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 1.0, Y => 2.0), (Width => 30.0, Height => 40.0));
      Outline : aliased Gsk.Rounded_Rect.Gsk_Rounded_Rect;
      Color : aliased Gdk.RGBA.Gdk_RGBA := (0.35, 0.55, 0.15, 0.65);
      Node : Gsk.Outset_Shadow_Node.Gsk_Outset_Shadow_Node;
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Outset : Gsk.Outset_Shadow_Node.Gsk_Outset_Shadow_Node;
   begin
      Outline := Gsk.Rounded_Rect.Init_From_Rect
        (Outline'Access, Bounds'Access, 3.0).all;
      Node := Gsk.Outset_Shadow_Node.Gsk_Outset_Shadow_Node_New
        (Outline'Access, Color'Access, 1.25, 2.5, 3.75, 5.0);
      --  The in-memory bytes make this check independent of files or displays.
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (Node));
      Assert_True (Serialized.Get_Size > 0);

      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) =
           Gsk.Enums.Gsk_Outset_Shadow_Node);
      Restored_Outset := Gsk.Outset_Shadow_Node.Gsk_Outset_Shadow_Node
        (Restored);
      Check_Properties
        (Restored_Outset, 1.25, 2.5, 3.75, 5.0, Color, Outline);

      Free_Node (Restored);
      --  Free through the generic access type so the wrapper finalizer runs.
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
     ("/gsk/outset-shadow-node/constructors-accessors",
      Test_Constructors_And_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/outset-shadow-node/deserialization-create",
      Test_Deserialization_Create'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
