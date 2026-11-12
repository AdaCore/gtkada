--  Direct Ada tests for GtkAda's Gsk.GLShader bindings.
--  The GTK C testsuite has no focused gsk_gl_shader test to port. These cases
--  check byte and resource constructors, parsed uniform metadata and values,
--  and the recoverable compile error returned by the configured Cairo renderer.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;
with Glib;            use Glib;
with Glib.Bytes;
with Glib.Error;
with Glib.Main;
with Glib.Object;
with Glib.Resource;
with Glib.Test;       use Glib.Test;
with Graphene.Rect;
with Graphene.Vec2;
with Graphene.Vec3;
with Graphene.Vec4;
with Gdk.RGBA;
with Gsk.Color_Node;
with Gsk.Enums;
with Gsk.Glshader;
with Gsk.Glshader_Node;
with Gsk.Render_Node;
with Gsk.Renderer;
with Gtk.Main;
with Gtk.Window;
with Interfaces.C;
with System;

procedure Main is
   use type Glib.Error.GError;
   use type Gsk.Glshader.Gsk_Glshader;
   use type Gsk.Enums.Gsk_GLUniform_Type;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type Gsk.Renderer.Gsk_Renderer;
   use type Glib.Resource.Gresource;
   use type Glib.Gsize;
   use type Interfaces.C.C_float;
   use type System.Address;

   Shader_Text : constant String :=
     --  Keep every declaration on its own line so GTK's parser recognizes it.
     "uniform bool u_enabled;" & ASCII.LF &
     "uniform float u_gain;" & ASCII.LF &
     "uniform int u_count;" & ASCII.LF &
     "uniform uint u_flags;" & ASCII.LF &
     "uniform vec2 u_offset;" & ASCII.LF &
     "uniform vec3 u_color;" & ASCII.LF &
     "uniform vec4 u_tint;" & ASCII.LF &
     "uniform sampler2D u_texture2;" & ASCII.LF &
     "void mainImage(out vec4 fragColor, in vec2 fragCoord," & ASCII.LF &
     "               in vec2 resolution, in vec2 uv)" & ASCII.LF &
     "{" & ASCII.LF &
     "  fragColor = u_tint + vec4(u_color, 1.0) * u_gain;" & ASCII.LF &
     "}" & ASCII.LF;

   Resource_Path : constant String :=
     "/org/gtkada/coverage/gsk-glshader/shader.glsl";

   --  Build a GBytes value from the exact GLSL source, without depending on a
   --  file in the test runner's working directory.
   function Bytes_From_String (Text : String) return Glib.Bytes.Gbytes is
      Data : Glib.Guint8_Array (1 .. Text'Length);
   begin
      for Index in Text'Range loop
         Data (Index - Text'First + 1) :=
           Glib.Guint8 (Character'Pos (Text (Index)));
      end loop;
      return Glib.Bytes.Gbytes_New (Data, Glib.Gsize (Data'Length));
   end Bytes_From_String;

   --  GTK's shader argument block is a packed sequence of native C scalar
   --  values and vector components. Copy each value's representation into
   --  the byte array using a C-convention four-byte array.
   type Byte_Quad is array (1 .. 4) of Glib.Guint8;
   pragma Convention (C, Byte_Quad);

   function Float_Bytes is new Ada.Unchecked_Conversion
     (Interfaces.C.C_float, Byte_Quad);
   function Int_Bytes is new Ada.Unchecked_Conversion
     (Glib.Gint32, Byte_Quad);
   function Uint_Bytes is new Ada.Unchecked_Conversion
     (Glib.Guint32, Byte_Quad);

   procedure Store
     (Data   : in out Glib.Guint8_Array;
      Offset : Glib.Gint;
      Bytes  : Byte_Quad);
   procedure Store_Float
     (Data   : in out Glib.Guint8_Array;
      Offset : Glib.Gint;
      Value  : Interfaces.C.C_float);

   procedure Test_Bytes_And_Metadata with Convention => C;
   procedure Test_Uniform_Arguments with Convention => C;
   procedure Test_Resource_Constructors with Convention => C;
   procedure Test_Compile_Error with Convention => C;
   procedure Test_Glshader_Node with Convention => C;

   --  Render-node wrappers own their GSK references and must be deallocated
   --  through the class-wide access type so their controlled finalizers run.
   procedure Free_Render_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Store
     (Data   : in out Glib.Guint8_Array;
      Offset : Glib.Gint;
      Bytes  : Byte_Quad)
   is
      First : constant Natural := Natural (Offset);
   begin
      for Index in Bytes'Range loop
         Data (First + Index) := Bytes (Index);
      end loop;
   end Store;

   procedure Store_Float
     (Data   : in out Glib.Guint8_Array;
      Offset : Glib.Gint;
      Value  : Interfaces.C.C_float) is
   begin
      Store (Data, Offset, Float_Bytes (Value));
   end Store_Float;

   --  The byte constructor exposes source bytes and parses every supported
   --  scalar/vector uniform while counting sampler declarations separately.
   procedure Test_Bytes_And_Metadata is
      Source : constant Glib.Bytes.Gbytes := Bytes_From_String (Shader_Text);
      Shader : constant Gsk.Glshader.Gsk_Glshader :=
        Gsk.Glshader.Gsk_Glshader_New_From_Bytes (Source);
      Via_Procedure : Gsk.Glshader.Gsk_Glshader;
      Initialized : constant Gsk.Glshader.Gsk_Glshader :=
        new Gsk.Glshader.Gsk_Glshader_Record;
      Source_View : Glib.Bytes.Gbytes;
      Expected_Names : constant array (Glib.Gint range 0 .. 6) of String (1 .. 9) :=
        ("u_enabled", "u_gain   ", "u_count  ", "u_flags  ",
         "u_offset ", "u_color  ", "u_tint   ");
      Expected_Types : constant array (Glib.Gint range 0 .. 6) of
        Gsk.Enums.Gsk_GLUniform_Type :=
          (Gsk.Enums.Gsk_Gl_Uniform_Type_Bool,
           Gsk.Enums.Gsk_Gl_Uniform_Type_Float,
           Gsk.Enums.Gsk_Gl_Uniform_Type_Int,
           Gsk.Enums.Gsk_Gl_Uniform_Type_Uint,
           Gsk.Enums.Gsk_Gl_Uniform_Type_Vec2,
           Gsk.Enums.Gsk_Gl_Uniform_Type_Vec3,
           Gsk.Enums.Gsk_Gl_Uniform_Type_Vec4);
      Expected_Offsets : constant array (Glib.Gint range 0 .. 6) of Glib.Gint :=
        (0, 4, 8, 12, 16, 24, 36);
   begin
      --  The function constructor gives this test the primary shader object.
      Source_View := Gsk.Glshader.Get_Source (Shader);
      Assert_Cmpuint_Eq
        (Glib.Guint (Glib.Bytes.Get_Size (Source_View)),
         Glib.Guint (Shader_Text'Length));
      Assert_Cmpstr_Eq (Gsk.Glshader.Get_Resource (Shader), "");
      --  The sampler is counted separately and only the highest texture index
      --  determines how many texture inputs the shader requires.
      Assert_Cmpint_Eq (Gsk.Glshader.Get_N_Uniforms (Shader), 7);
      Assert_Cmpint_Eq (Gsk.Glshader.Get_N_Textures (Shader), 2);
      Assert_True (Gsk.Glshader.Get_Args_Size (Shader) = 52);

      --  The parser preserves declaration order and packs each accepted type
      --  directly after the previous type, including the three-vector value.
      for Index in Expected_Names'Range loop
         declare
            Name : constant String :=
              Gsk.Glshader.Get_Uniform_Name (Shader, Index);
         begin
            Assert_Cmpstr_Eq
              (Name, Ada.Strings.Fixed.Trim
                 (Expected_Names (Index), Ada.Strings.Right));
         end;
         Assert_Cmpint_Eq
           (Gsk.Glshader.Find_Uniform_By_Name
              (Shader, Gsk.Glshader.Get_Uniform_Name (Shader, Index)), Index);
         Assert_True
           (Gsk.Glshader.Get_Uniform_Offset (Shader, Index) =
              Expected_Offsets (Index));
         Assert_True
           (Gsk.Glshader.Get_Uniform_Type (Shader, Index) =
              Expected_Types (Index));
      end loop;
      --  A name that has no matching declaration uses GTK's documented -1.
      Assert_Cmpint_Eq
        (Gsk.Glshader.Find_Uniform_By_Name (Shader, "missing"), -1);

      --  The out-parameter constructor and Initialize path create equivalent
      --  objects from the same immutable source bytes.
      Gsk.Glshader.Gtk_New_From_Bytes (Via_Procedure, Source);
      Assert_True
        (Gsk.Glshader.Get_N_Uniforms (Shader) =
           Gsk.Glshader.Get_N_Uniforms (Via_Procedure));
      Gsk.Glshader.Initialize_From_Bytes (Initialized, Source);
      Assert_True (Glib.Object.Is_Created (Initialized.all));
      --  Initializing an existing object is specified to leave it unchanged.
      Gsk.Glshader.Initialize_From_Bytes (Initialized, Source);
      Assert_True (Gsk.Glshader.Get_Resource (Initialized) = "");

      Glib.Object.Unref (Glib.Object.GObject (Initialized));
      Glib.Object.Unref (Glib.Object.GObject (Shader));
      Glib.Object.Unref (Glib.Object.GObject (Via_Procedure));
      Glib.Bytes.Unref (Source);
   end Test_Bytes_And_Metadata;

   --  Fill values at the offsets reported by GTK, then check every typed
   --  accessor, including Graphene's out-parameter vector conversions.
   procedure Test_Uniform_Arguments is
      Source : Glib.Bytes.Gbytes := Bytes_From_String (Shader_Text);
      Shader : Gsk.Glshader.Gsk_Glshader;
      Data : Glib.Guint8_Array (1 .. 52) := (others => 0);
      Args : Glib.Bytes.Gbytes;
      Bool_Index : Glib.Gint;
      Float_Index : Glib.Gint;
      Int_Index : Glib.Gint;
      Uint_Index : Glib.Gint;
      Vec2_Index : Glib.Gint;
      Vec3_Index : Glib.Gint;
      Vec4_Index : Glib.Gint;
      V2 : aliased Graphene.Vec2.Graphene_Vec2_T;
      V3 : aliased Graphene.Vec3.Graphene_Vec3_T;
      V4 : aliased Graphene.Vec4.Graphene_Vec4_T;
   begin
      Shader := Gsk.Glshader.Gsk_Glshader_New_From_Bytes (Source);
      --  Resolve indices by name so the values below follow the shader's
      --  declarations rather than relying on hard-coded parser positions.
      Bool_Index := Gsk.Glshader.Find_Uniform_By_Name (Shader, "u_enabled");
      Float_Index := Gsk.Glshader.Find_Uniform_By_Name (Shader, "u_gain");
      Int_Index := Gsk.Glshader.Find_Uniform_By_Name (Shader, "u_count");
      Uint_Index := Gsk.Glshader.Find_Uniform_By_Name (Shader, "u_flags");
      Vec2_Index := Gsk.Glshader.Find_Uniform_By_Name (Shader, "u_offset");
      Vec3_Index := Gsk.Glshader.Find_Uniform_By_Name (Shader, "u_color");
      Vec4_Index := Gsk.Glshader.Find_Uniform_By_Name (Shader, "u_tint");

      --  Store native C representations at GTK's reported packed offsets.
      Store (Data, Gsk.Glshader.Get_Uniform_Offset (Shader, Bool_Index),
             Uint_Bytes (1));
      Store_Float (Data, Gsk.Glshader.Get_Uniform_Offset (Shader, Float_Index), 1.25);
      Store (Data, Gsk.Glshader.Get_Uniform_Offset (Shader, Int_Index),
             Int_Bytes (-17));
      Store (Data, Gsk.Glshader.Get_Uniform_Offset (Shader, Uint_Index),
             Uint_Bytes (42));
      Store_Float (Data, Gsk.Glshader.Get_Uniform_Offset (Shader, Vec2_Index), 2.5);
      Store_Float (Data, Gsk.Glshader.Get_Uniform_Offset (Shader, Vec2_Index) + 4, -4.0);
      Store_Float (Data, Gsk.Glshader.Get_Uniform_Offset (Shader, Vec3_Index), 0.25);
      Store_Float (Data, Gsk.Glshader.Get_Uniform_Offset (Shader, Vec3_Index) + 4, 0.5);
      Store_Float (Data, Gsk.Glshader.Get_Uniform_Offset (Shader, Vec3_Index) + 8, 0.75);
      Store_Float (Data, Gsk.Glshader.Get_Uniform_Offset (Shader, Vec4_Index), 1.0);
      Store_Float (Data, Gsk.Glshader.Get_Uniform_Offset (Shader, Vec4_Index) + 4, -2.0);
      Store_Float (Data, Gsk.Glshader.Get_Uniform_Offset (Shader, Vec4_Index) + 8, 3.0);
      Store_Float (Data, Gsk.Glshader.Get_Uniform_Offset (Shader, Vec4_Index) + 12, -4.0);
      Args := Glib.Bytes.Gbytes_New (Data, Glib.Gsize (Data'Length));

      Assert_True (Gsk.Glshader.Get_Arg_Bool (Shader, Args, Bool_Index));
      Assert_True
        (abs (Gsk.Glshader.Get_Arg_Float (Shader, Args, Float_Index) - 1.25) < 0.001);
      Assert_True (Gsk.Glshader.Get_Arg_Int (Shader, Args, Int_Index) = -17);
      Assert_True (Gsk.Glshader.Get_Arg_Uint (Shader, Args, Uint_Index) = 42);

      --  Each vector getter writes a Graphene value whose individual
      --  components are checked through Graphene's public accessors.
      Gsk.Glshader.Get_Arg_Vec2 (Shader, Args, Vec2_Index, V2'Access);
      Assert_Cmpfloat_With_Epsilon (Glib.Gdouble (Graphene.Vec2.Get_X (V2'Access)), 2.5, 0.001);
      Assert_Cmpfloat_With_Epsilon (Glib.Gdouble (Graphene.Vec2.Get_Y (V2'Access)), -4.0, 0.001);
      Gsk.Glshader.Get_Arg_Vec3 (Shader, Args, Vec3_Index, V3'Access);
      Assert_Cmpfloat_With_Epsilon (Glib.Gdouble (Graphene.Vec3.Get_X (V3'Access)), 0.25, 0.001);
      Assert_Cmpfloat_With_Epsilon (Glib.Gdouble (Graphene.Vec3.Get_Y (V3'Access)), 0.5, 0.001);
      Assert_Cmpfloat_With_Epsilon (Glib.Gdouble (Graphene.Vec3.Get_Z (V3'Access)), 0.75, 0.001);
      Gsk.Glshader.Get_Arg_Vec4 (Shader, Args, Vec4_Index, V4'Access);
      Assert_Cmpfloat_With_Epsilon (Glib.Gdouble (Graphene.Vec4.Get_X (V4'Access)), 1.0, 0.001);
      Assert_Cmpfloat_With_Epsilon (Glib.Gdouble (Graphene.Vec4.Get_Y (V4'Access)), -2.0, 0.001);
      Assert_Cmpfloat_With_Epsilon (Glib.Gdouble (Graphene.Vec4.Get_Z (V4'Access)), 3.0, 0.001);
      Assert_Cmpfloat_With_Epsilon (Glib.Gdouble (Graphene.Vec4.Get_W (V4'Access)), -4.0, 0.001);

      Glib.Bytes.Unref (Args);
      Glib.Bytes.Unref (Source);
      Glib.Object.Unref (Glib.Object.GObject (Shader));
   end Test_Uniform_Arguments;

   --  Load a local bundle to exercise both resource constructors and their
   --  resource-path accessor without depending on installed application data.
   procedure Test_Resource_Constructors is
      Error : Glib.Error.GError;
      Resource : Glib.Resource.Gresource :=
        Glib.Resource.Load ("shader.gresource", Error);
      From_Function : Gsk.Glshader.Gsk_Glshader;
      From_Procedure : Gsk.Glshader.Gsk_Glshader;
      Initialized : constant Gsk.Glshader.Gsk_Glshader :=
        new Gsk.Glshader.Gsk_Glshader_Record;
   begin
      Assert_True (Error = null and Resource /= Glib.Resource.Null_Gresource);
      Resource.Register;
      --  The resource and byte constructors share source text but preserve
      --  whether a global resource path backs that source.
      From_Function := Gsk.Glshader.Gsk_Glshader_New_From_Resource (Resource_Path);
      Gsk.Glshader.Gtk_New_From_Resource (From_Procedure, Resource_Path);
      Gsk.Glshader.Initialize_From_Resource (Initialized, Resource_Path);

      Assert_Cmpstr_Eq (Gsk.Glshader.Get_Resource (From_Function), Resource_Path);
      Assert_Cmpstr_Eq (Gsk.Glshader.Get_Resource (From_Procedure), Resource_Path);
      Assert_Cmpstr_Eq (Gsk.Glshader.Get_Resource (Initialized), Resource_Path);
      Assert_Cmpint_Eq (Gsk.Glshader.Get_N_Uniforms (From_Function), 7);
      --  Reinitializing must retain the first resource path and source.
      Gsk.Glshader.Initialize_From_Resource (Initialized, Resource_Path);
      Assert_Cmpstr_Eq (Gsk.Glshader.Get_Resource (Initialized), Resource_Path);

      Resource.Unregister;
      Glib.Resource.Unref (Resource);
      Glib.Object.Unref (Glib.Object.GObject (Initialized));
      Glib.Object.Unref (Glib.Object.GObject (From_Procedure));
      Glib.Object.Unref (Glib.Object.GObject (From_Function));
   end Test_Resource_Constructors;

   --  Under the testsuite's Cairo renderer, compilation is unsupported and
   --  returns a documented recoverable GError instead of touching a GL API.
   procedure Test_Compile_Error is
      Source : Glib.Bytes.Gbytes := Bytes_From_String (Shader_Text);
      Shader : Gsk.Glshader.Gsk_Glshader;
      Window : Gtk.Window.Gtk_Window := Gtk.Window.Gtk_Window_New;
      Renderer : Gsk.Renderer.Gsk_Renderer;
      Error : Glib.Error.GError;
   begin
      Shader := Gsk.Glshader.Gsk_Glshader_New_From_Bytes (Source);
      Glib.Bytes.Unref (Source);

      Gtk.Window.Present (Window);
      --  Mapping the window creates its realized renderer; bounded nonblocking
      --  iterations avoid waiting on any external input in the test process.
      for Attempt in 1 .. 20 loop
         exit when Gtk.Window.Get_Renderer (Window) /= null;
         declare
            Did_Work : constant Boolean :=
              Glib.Main.Main_Context_Iteration (null, May_Block => False);
            pragma Unreferenced (Did_Work);
         begin
            null;
         end;
      end loop;
      Renderer := Gtk.Window.Get_Renderer (Window);
      --  Cairo is selected by the test runner, so GTK must report unsupported
      --  rather than attempting compilation in an OpenGL context.
      Assert_True (Renderer /= null);
      Assert_False (Gsk.Glshader.Compile (Shader, Renderer, Error));
      Assert_True (Error /= null);
      Assert_Cmpstr_Eq
        (Glib.Error.Get_Message (Error), "The renderer does not support gl shaders");

      Glib.Error.Error_Free (Error);
      Window.Destroy;
      Glib.Object.Unref (Glib.Object.GObject (Shader));
   end Test_Compile_Error;

   --  The GL shader node can be built without a renderer. A simple color node
   --  supplies a real child so the binding's Ada-to-C child-array conversion
   --  is exercised while the test remains independent of GPU support.
   procedure Test_Glshader_Node is
      Node_Shader_Text : constant String :=
        "uniform sampler2D u_texture1;" & ASCII.LF &
        "void mainImage(out vec4 fragColor, in vec2 fragCoord," & ASCII.LF &
        "               in vec2 resolution, in vec2 uv)" & ASCII.LF &
        "{" & ASCII.LF &
        "  fragColor = vec4(uv, 0.0, 1.0);" & ASCII.LF &
        "}" & ASCII.LF;
      Empty_Data : constant Glib.Guint8_Array (1 .. 0) := (others => 0);
      Source : Glib.Bytes.Gbytes := Bytes_From_String (Node_Shader_Text);
      Args : Glib.Bytes.Gbytes := Glib.Bytes.Gbytes_New (Empty_Data, 0);
      Shader : Gsk.Glshader.Gsk_Glshader :=
        Gsk.Glshader.Gsk_Glshader_New_From_Bytes (Source);
      Other_Shader : Gsk.Glshader.Gsk_Glshader :=
        Gsk.Glshader.Gsk_Glshader_New_From_Bytes (Source);
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 2.0, Y => 3.0), (Width => 24.0, Height => 16.0));
      Child_Color : aliased Gdk.RGBA.Gdk_RGBA := (0.2, 0.4, 0.6, 1.0);
      Child : Gsk.Color_Node.Gsk_Color_Node :=
        Gsk.Color_Node.Gsk_Color_Node_New
          (Child_Color'Access, Bounds'Access);
      Children : Gsk.Render_Node.Gsk_Render_Node_Array (1 .. 1) :=
        (1 => Gsk.Render_Node.Gsk_Render_Node (Child));
      Initialized : Gsk.Glshader_Node.Gsk_Glshader_Node :=
        new Gsk.Glshader_Node.Gsk_Glshader_Node_Record;
      Function_Node : Gsk.Glshader_Node.Gsk_Glshader_Node;
      Procedure_Node : Gsk.Glshader_Node.Gsk_Glshader_Node;
   begin
      --  Initialize creates an empty Ada wrapper once; the second call must
      --  preserve its original shader, as documented for GtkAda constructors.
      Gsk.Glshader_Node.Initialize
        (Initialized, Shader, Bounds'Access, Args, Children);
      Assert_True (Gsk.Render_Node.Is_Created (Initialized));
      Gsk.Glshader_Node.Initialize
        (Initialized, Other_Shader, Bounds'Access, Args, Children);
      Assert_True
        (Glib.Object.Get_Object (Gsk.Glshader_Node.Get_Shader (Initialized)) =
           Glib.Object.Get_Object (Shader));

      --  Exercise both public constructor styles and verify each resulting
      --  immutable node reports its kind and retains the supplied shader.
      Function_Node := Gsk.Glshader_Node.Gsk_Glshader_Node_New
        (Other_Shader, Bounds'Access, Args, Children);
      Gsk.Glshader_Node.Gtk_New
        (Procedure_Node, Shader, Bounds'Access, Args, Children);
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Function_Node)) =
             Gsk.Enums.Gsk_Gl_Shader_Node);
      Assert_True
        (Glib.Object.Get_Object
           (Gsk.Glshader_Node.Get_Shader (Function_Node)) =
             Glib.Object.Get_Object (Other_Shader));
      Assert_True
        (Glib.Object.Get_Object
           (Gsk.Glshader_Node.Get_Shader (Procedure_Node)) =
             Glib.Object.Get_Object (Shader));

      --  Free parent nodes before their separately held child/shader refs.
      Free_Render_Node (Gsk.Render_Node.Gsk_Render_Node (Initialized));
      Free_Render_Node (Gsk.Render_Node.Gsk_Render_Node (Function_Node));
      Free_Render_Node (Gsk.Render_Node.Gsk_Render_Node (Procedure_Node));
      Free_Render_Node (Gsk.Render_Node.Gsk_Render_Node (Child));
      Glib.Object.Unref (Glib.Object.GObject (Other_Shader));
      Glib.Object.Unref (Glib.Object.GObject (Shader));
      Glib.Bytes.Unref (Args);
      Glib.Bytes.Unref (Source);
   end Test_Glshader_Node;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/gsk-glshader/bytes-metadata", Test_Bytes_And_Metadata'Unrestricted_Access);
   Add_Func ("/gsk-glshader/uniform-arguments", Test_Uniform_Arguments'Unrestricted_Access);
   Add_Func ("/gsk-glshader/resource-constructors", Test_Resource_Constructors'Unrestricted_Access);
   Add_Func ("/gsk-glshader/compile-error", Test_Compile_Error'Unrestricted_Access);
   Add_Func ("/gsk-glshader/node", Test_Glshader_Node'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
