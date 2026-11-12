--  Exercises the Gtk.Snapshot binding: build a snapshot, drive the transform
--  stack and a clip, append a solid colour, and turn the result into a
--  paintable. A second test drives the four Graphene-dependent entry points
--  (translate_3d, rotate_3d, transform_matrix, push_color_matrix). The point
--  is to prove the generated package links and runs -- in particular that the
--  Graphene records marshal with the right layout -- not to inspect the
--  produced render node (which needs Gsk).

with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Glib.Test;        use Glib.Test;
with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with System;
with Cairo;
with Gdk.Paintable;    use Gdk.Paintable;
with Gdk.Memory_Texture;
with Gdk.RGBA;         use Gdk.RGBA;
with Gdk.Texture;
with Glib.Bytes;
with Gtk.Main;
with Gtk.Label;
with Gtk.Style_Context;
with Gtk.Snapshot;     use Gtk.Snapshot;
with Graphene.Rect;    use Graphene.Rect;
with Graphene.Point;   use Graphene.Point;
with Graphene.Size;    use Graphene.Size;
with Gsk.Component_Transfer;
with Gsk.Enums;
with Gsk.Glshader;
with Gsk.Path;
with Gsk.Render_Node;
with Gsk.Rounded_Rect;
with Gsk.Shadow_Node;
with Gsk.Stroke;
with Gsk.Transform;
with Pango.Enums;
with Pango.Layout;

--  Graphene.Matrix, Graphene.Vec3 and Graphene.Vec4 all export an Init
--  family, Equal, Free and Get_X, so they are deliberately left without a
--  `use` clause: the calls below are qualified instead.
with Graphene.Matrix;
with Graphene.Point3d;
with Graphene.Vec3;
with Graphene.Vec4;

procedure Snapshot is

   use type Cairo.Cairo_Status;

   --  Gsk render nodes use an Ada wrapper that owns a C reference; freeing
   --  the wrapper runs its finalizer and releases that reference.
   procedure Free_Render_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Paint
   with Convention => C;

   procedure Test_Transform_3D
   with Convention => C;

   procedure Test_Append_Nodes
   with Convention => C;

   procedure Test_Push_Nodes
   with Convention => C;

   procedure Test_Text_Rendering
   with Convention => C;

   procedure Test_Legacy_Gl_Shader
   with Convention => C;

   procedure Test_Paint is
      Snap      : constant Gtk_Snapshot := Gtk_Snapshot_New;
      Color     : constant Gdk_RGBA :=
        (Red => 0.20, Green => 0.50, Blue => 0.85, Alpha => 1.0);
      Bounds    : aliased Graphene.Rect.Graphene_Rect_T :=
        (origin => (x => 0.0, y => 0.0),
         size   => (width => 100.0, height => 100.0));
      Point     : aliased Graphene.Point.Graphene_Point_T :=
        (x => 10.0, y => 20.0);
      Size      : aliased Graphene.Size.Graphene_Size_T  :=
        (width => 100.0, height => 100.0);
      Paintable : Gdk_Paintable;
   begin
      Assert_Nonnull (Get_Object (Snap));

      --  Transform stack.
      Snap.Save;
      Snap.Translate (Point'Access);
      Snap.Scale (2.0, 2.0);
      Snap.Rotate (45.0);

      --  A clip push must be balanced by a pop.
      Snap.Push_Clip (Bounds'Access);
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;

      Snap.Restore;

      --  Consumes the snapshot and hands back a paintable.
      Paintable := Snap.To_Paintable (Size'Access);
      Assert_Nonnull (System.Address (Paintable));
   end Test_Paint;

   procedure Test_Transform_3D is
      Snap   : constant Gtk_Snapshot := Gtk_Snapshot_New;
      Color  : constant Gdk_RGBA :=
        (Red => 0.20, Green => 0.50, Blue => 0.85, Alpha => 1.0);
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        (origin => (x => 0.0, y => 0.0),
         size   => (width => 100.0, height => 100.0));
      Point  : aliased Graphene.Point3d.Graphene_Point3D_T :=
        (X => 10.0, Y => 20.0, Z => 30.0);

      --  Unlike the records above, these three are opaque SIMD wrappers and
      --  cannot be built with an aggregate; they must be filled in by their
      --  Init functions.
      Axis   : aliased Graphene.Vec3.Graphene_Vec3_T;
      Matrix : aliased Graphene.Matrix.Graphene_Matrix_T;
      Offset : aliased Graphene.Vec4.Graphene_Vec4_T;
   begin
      Assert_Nonnull (Get_Object (Snap));

      declare
         --  The Init family has no procedure form: each returns the access it
         --  was handed, which is of no use here.
         Ignored_Axis   : constant access Graphene.Vec3.Graphene_Vec3_T :=
           Graphene.Vec3.Init (Axis'Access, 0.0, 0.0, 1.0);
         Ignored_Matrix : constant access Graphene.Matrix.Graphene_Matrix_T :=
           Graphene.Matrix.Init_Identity (Matrix'Access);
         Ignored_Offset : constant access Graphene.Vec4.Graphene_Vec4_T :=
           Graphene.Vec4.Init (Offset'Access, 0.0, 0.0, 0.0, 0.0);
         pragma Unreferenced (Ignored_Axis, Ignored_Matrix, Ignored_Offset);
      begin
         --  A wrong record layout shows up here, before GTK ever sees it.
         Assert_True (Graphene.Matrix.Is_Identity (Matrix'Access));
         Assert_Cmpfloat_With_Epsilon
           (Gdouble (Graphene.Vec3.Get_Z (Axis'Access)), 1.0, 0.0001);
      end;

      Snap.Save;
      Snap.Translate_3D (Point'Access);
      Snap.Rotate_3D (45.0, Axis'Access);
      Snap.Transform_Matrix (Matrix'Access);

      --  A colour-matrix push must be balanced by a pop, and wants a
      --  non-empty child node underneath it.
      Snap.Push_Color_Matrix (Matrix'Access, Offset'Access);
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;

      Snap.Restore;
   end Test_Transform_3D;

   --  Exercise the append helpers with valid data and verify that they build
   --  a render node. These calls do not need a renderer or a mapped window.
   procedure Test_Append_Nodes is
      Snap       : constant Gtk_Snapshot := Gtk_Snapshot_New;
      Source     : constant Gtk_Snapshot := Gtk_Snapshot_New;
      Color      : constant Gdk_RGBA := (0.25, 0.5, 0.75, 1.0);
      Other      : constant Gdk_RGBA := (0.9, 0.1, 0.2, 1.0);
      Bounds     : aliased Graphene_Rect_T := ((0.0, 0.0), (20.0, 20.0));
      Center     : aliased Graphene_Point_T := (10.0, 10.0);
      Start_Point : aliased Graphene_Point_T := (0.0, 0.0);
      End_Point   : aliased Graphene_Point_T := (20.0, 20.0);
      Stops      : Gsk.Render_Node.Gsk_Color_Stop_Array (1 .. 2) :=
        ((Offset => 0.0, Color => Color), (Offset => 1.0, Color => Other));
      Widths     : constant Gfloat_Array4 := (1.0, 2.0, 3.0, 4.0);
      Colors     : constant Gdk_Rgba_Array4 := (others => Color);
      Outline    : aliased Gsk.Rounded_Rect.Gsk_Rounded_Rect;
      Outline_Init : access Gsk.Rounded_Rect.Gsk_Rounded_Rect;
      Pixels     : constant Glib.Guint8_Array (0 .. 3) :=
        (16#FF#, 16#20#, 16#40#, 16#FF#);
      Bytes      : constant Glib.Bytes.Gbytes :=
        Glib.Bytes.Gbytes_New (Pixels, Glib.Gsize (Pixels'Length));
      Texture    : constant Gdk.Memory_Texture.Gdk_Memory_Texture :=
        Gdk.Memory_Texture.Gdk_Memory_Texture_New
          (1, 1, Gdk.Texture.Gdk_Memory_R8G8B8A8, Bytes, 4);
      Label      : constant Gtk.Label.Gtk_Label := Gtk.Label.Gtk_Label_New ("Snapshot");
      Layout     : constant Pango.Layout.Pango_Layout :=
        Label.Create_Pango_Layout ("Snapshot append layout");
      Cairo_Context : Cairo.Cairo_Context;
      Node       : Gsk.Render_Node.Gsk_Render_Node;
      Result_Node : Gsk.Render_Node.Gsk_Render_Node;
   begin
      Label.Ref_Sink;
      Outline_Init :=
        Gsk.Rounded_Rect.Init_From_Rect (Outline'Access, Bounds'Access, 4.0);

      --  Both outline based helpers accept the same normalized rounded rect.
      Snap.Append_Border (Outline, Widths, Colors);
      Snap.Append_Inset_Shadow (Outline, Color, 1.0, 1.0, 1.0, 2.0);
      Snap.Append_Outset_Shadow (Outline, Other, 1.0, 1.0, 1.0, 2.0);

      --  The cairo context belongs to the caller and is destroyed after use.
      Cairo_Context := Snap.Append_Cairo (Bounds'Access);
      Cairo.Set_Source_Rgba (Cairo_Context, 0.2, 0.4, 0.8, 1.0);
      Cairo.Paint (Cairo_Context);
      Assert_True (Cairo.Status (Cairo_Context) = Cairo.Cairo_Status_Success);
      Cairo.Destroy (Cairo_Context);

      --  Gradient stops are passed as a contiguous C array.
      Snap.Append_Conic_Gradient
        (Bounds'Access, Center'Access, 0.0, Stops);
      Snap.Append_Linear_Gradient
        (Bounds'Access, Start_Point'Access, End_Point'Access, Stops);
      Snap.Append_Radial_Gradient
        (Bounds'Access, Center'Access, 10.0, 10.0, 0.0, 10.0, Stops);
      Snap.Append_Repeating_Linear_Gradient
        (Bounds'Access, Start_Point'Access, End_Point'Access, Stops);
      Snap.Append_Repeating_Radial_Gradient
        (Bounds'Access, Center'Access, 10.0, 10.0, 0.0, 10.0, Stops);

      --  The same in-memory texture works for the default and explicit filter.
      Snap.Append_Texture (Texture, Bounds'Access);
      Snap.Append_Scaled_Texture
        (Texture, Gsk.Enums.Gsk_Scaling_Filter_Nearest, Bounds'Access);
      Snap.Append_Layout (Layout, Color);

      --  Append_Node retains the node while the source snapshot is released.
      Source.Append_Color (Color, Bounds'Access);
      Node := Source.To_Node;
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Node));
      Snap.Append_Node (Node);
      Result_Node := Snap.To_Node;
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Result_Node));

      Free_Render_Node (Result_Node);
      Free_Render_Node (Node);
      Glib.Object.Unref (Glib.Object.GObject (Snap));
      Glib.Object.Unref (Glib.Object.GObject (Source));
      Glib.Object.Unref (Glib.Object.GObject (Texture));
      Glib.Bytes.Unref (Bytes);
      Layout.Unref;
      Label.Unref;
   end Test_Append_Nodes;

   --  Exercise effect and grouping pushes. Every push is balanced according
   --  to the GTK API, including operations that require two child nodes.
   procedure Test_Push_Nodes is
      Snap       : constant Gtk_Snapshot := Gtk_Snapshot_New;
      Color      : constant Gdk_RGBA := (0.25, 0.5, 0.75, 1.0);
      Bounds     : aliased Graphene_Rect_T := ((0.0, 0.0), (20.0, 20.0));
      Child_Bounds : access Graphene_Rect_T := null;
      Outline    : aliased Gsk.Rounded_Rect.Gsk_Rounded_Rect;
      Outline_Init : access Gsk.Rounded_Rect.Gsk_Rounded_Rect;
      Path       : constant Gsk.Path.Gsk_Path :=
        Gsk.Path.Parse ("M 0 0 L 20 0 L 20 20 L 0 20 Z");
      Stroke     : constant Gsk.Stroke.Gsk_Stroke := Gsk.Stroke.Gsk_Stroke_New (2.0);
      Shadows    : constant Gsk.Shadow_Node.Gsk_Shadow_Array (1 .. 1) :=
        (1 => (Color => Color, Dx => 1.0, Dy => 1.0, Radius => 2.0));
      Transfer   : constant Gsk.Component_Transfer.Gsk_Component_Transfer :=
        Gsk.Component_Transfer.Gsk_Component_Transfer_New_Identity;
      Transform  : constant Gsk.Transform.Gsk_Transform :=
        Gsk.Transform.Null_Gsk_Transform;
      Result_Node : Gsk.Render_Node.Gsk_Render_Node;
   begin
      Outline_Init :=
        Gsk.Rounded_Rect.Init_From_Rect (Outline'Access, Bounds'Access, 4.0);

      --  Single-child effects wrap one recorded color node apiece.
      Snap.Push_Blur (2.0);
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;
      Snap.Push_Component_Transfer (Transfer, Transfer, Transfer, Transfer);
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;
      Snap.Push_Isolation (Gsk.Enums.Gsk_Isolation_None);
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;
      Snap.Push_Opacity (0.75);
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;
      Snap.Push_Repeat (Bounds'Access, Child_Bounds);
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;
      Snap.Push_Rounded_Clip (Outline);
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;
      Snap.Push_Shadow (Shadows);
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;
      --  Path fill and stroke pushes record an image clipped to their path.
      Snap.Push_Fill (Path, Gsk.Enums.Gsk_Fill_Rule_Winding);
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;
      Snap.Push_Stroke (Path, Stroke);
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;

      --  Blend, mask, composite, and cross-fade collect two child images.
      --  The first Pop closes the first image; the second closes the effect.
      Snap.Push_Blend (Gsk.Enums.Gsk_Blend_Mode_Multiply);
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;
      Snap.Push_Composite (Gsk.Enums.Gsk_Porter_Duff_Source_Over_Dest);
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;
      Snap.Push_Mask (Gsk.Enums.Gsk_Mask_Mode_Alpha);
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;
      Snap.Push_Cross_Fade (0.5);
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;

      --  push_copy keeps a copy available until the matching Pop.
      --  Paste index zero selects the most recently captured copy.
      Snap.Push_Copy;
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Append_Paste (Bounds'Access, 0);
      Snap.Pop;

      --  Exercise the remaining 3D and general transform entry points.
      Snap.Perspective (100.0);
      Snap.Scale_3D (1.0, 1.0, 1.0);
      Snap.Transform (Transform);
      Snap.Append_Color (Color, Bounds'Access);
      Result_Node := Snap.To_Node;
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Result_Node));

      Free_Render_Node (Result_Node);
      Gsk.Stroke.Free (Stroke);
      Gsk.Path.Unref (Path);
      Gsk.Component_Transfer.Free (Transfer);
      Glib.Object.Unref (Glib.Object.GObject (Snap));
   end Test_Push_Nodes;

   --  These deprecated style helpers remain callable and need a real Pango
   --  layout, so use one owned by a label and its associated style context.
   procedure Test_Text_Rendering is
      Snap    : constant Gtk_Snapshot := Gtk_Snapshot_New;
      Label   : constant Gtk.Label.Gtk_Label := Gtk.Label.Gtk_Label_New ("caret");
      Context : Gtk.Style_Context.Gtk_Style_Context;
      Layout  : Pango.Layout.Pango_Layout;
      Result_Node : Gsk.Render_Node.Gsk_Render_Node;
   begin
      Label.Ref_Sink;
      Context := Gtk.Style_Context.Get_Style_Context (Label);
      Layout := Label.Create_Pango_Layout ("rendered text");
      --  Both render helpers append to this snapshot using the label's theme.
      Snap.Render_Layout (Context, 0.0, 0.0, Layout);
      Snap.Render_Insertion_Cursor
        (Context, 0.0, 0.0, Layout, 1, Pango.Enums.Pango_Direction_Ltr);
      Result_Node := Snap.To_Node;
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Result_Node));

      Free_Render_Node (Result_Node);
      Layout.Unref;
      Label.Unref;
      Glib.Object.Unref (Glib.Object.GObject (Snap));
   end Test_Text_Rendering;

   --  The legacy shader node is deprecated, but its snapshot stack can still
   --  be exercised without compiling or displaying the shader.
   procedure Test_Legacy_Gl_Shader is
      Shader_Source : constant String :=
        "uniform sampler2D u_texture1;" & ASCII.LF &
        "void mainImage(out vec4 fragColor, in vec2 fragCoord," & ASCII.LF &
        "               in vec2 resolution, in vec2 uv)" & ASCII.LF &
        "{" & ASCII.LF &
        "  fragColor = GskTexture(u_texture1, uv);" & ASCII.LF &
        "}" & ASCII.LF;
      Source_Data : Glib.Guint8_Array (1 .. Shader_Source'Length);
      Empty_Data  : Glib.Guint8_Array (1 .. 0);
      Source      : Glib.Bytes.Gbytes;
      Args        : constant Glib.Bytes.Gbytes :=
        Glib.Bytes.Gbytes_New (Empty_Data, 0);
      Shader      : Gsk.Glshader.Gsk_Glshader;
      Snap        : constant Gtk_Snapshot := Gtk_Snapshot_New;
      Bounds      : aliased Graphene_Rect_T := ((0.0, 0.0), (20.0, 20.0));
      Color       : constant Gdk_RGBA := (0.25, 0.5, 0.75, 1.0);
      Result_Node : Gsk.Render_Node.Gsk_Render_Node;
   begin
      --  Store the GLSL as bytes because Gtk.GLShader's constructor consumes
      --  a GBytes block rather than a NUL-terminated Ada string.
      for Index in Shader_Source'Range loop
         Source_Data (Index) :=
           Glib.Guint8 (Character'Pos (Shader_Source (Index)));
      end loop;
      Source :=
        Glib.Bytes.Gbytes_New (Source_Data, Glib.Gsize (Source_Data'Length));
      Shader := Gsk.Glshader.Gsk_Glshader_New_From_Bytes (Source);

      --  One child is captured as the sampler texture, then the shader node
      --  itself is closed with the ordinary Pop call.
      --  No renderer is needed because this test checks node construction.
      --  Push_Gl_Shader takes ownership of Args, so it must not be unrefed
      --  after this call.
      Snap.Push_Gl_Shader (Shader, Bounds'Access, Args);
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Gl_Shader_Pop_Texture;
      Snap.Pop;
      Result_Node := Snap.To_Node;
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Result_Node));

      Free_Render_Node (Result_Node);
      Glib.Object.Unref (Glib.Object.GObject (Snap));
      Glib.Object.Unref (Glib.Object.GObject (Shader));
      Glib.Bytes.Unref (Source);
   end Test_Legacy_Gl_Shader;

begin
   Glib.Test.Init;

   --  Widgets and snapshots cannot be created before GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/snapshot/paint", Test_Paint'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/snapshot/transform-3d", Test_Transform_3D'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/snapshot/append-nodes", Test_Append_Nodes'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/snapshot/push-nodes", Test_Push_Nodes'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/snapshot/text-rendering", Test_Text_Rendering'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/snapshot/legacy-gl-shader",
      Test_Legacy_Gl_Shader'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Snapshot;
