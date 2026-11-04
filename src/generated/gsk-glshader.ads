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

--  Implements a fragment shader using GLSL.
--
--  A fragment shader gets the coordinates being rendered as input and
--  produces the pixel values for that particular pixel. Additionally, the
--  shader can declare a set of other input arguments, called uniforms (as they
--  are uniform over all the calls to your shader in each instance of use). A
--  shader can also receive up to 4 textures that it can use as input when
--  producing the pixel data.
--
--  `GskGLShader` is usually used with Gtk.Snapshot.Push_Gl_Shader to produce
--  a [classGsk.GLShaderNode] in the rendering hierarchy, and then its input
--  textures are constructed by rendering the child nodes to textures before
--  rendering the shader node itself. (You can pass texture nodes as children
--  if you want to directly use a texture as input).
--
--  The actual shader code is GLSL code that gets combined with some other
--  code into the fragment shader. Since the exact capabilities of the GPU
--  driver differs between different OpenGL drivers and hardware, GTK adds some
--  defines that you can use to ensure your GLSL code runs on as many drivers
--  as it can.
--
--  If the OpenGL driver is GLES, then the shader language version is set to
--  100, and GSK_GLES will be defined in the shader.
--
--  Otherwise, if the OpenGL driver does not support the 3.2 core profile,
--  then the shader will run with language version 110 for GL2 and 130 for GL3,
--  and GSK_LEGACY will be defined in the shader.
--
--  If the OpenGL driver supports the 3.2 code profile, it will be used, the
--  shader language version is set to 150, and GSK_GL3 will be defined in the
--  shader.
--
--  The main function the shader must implement is:
--
--  ```glsl void mainImage(out vec4 fragColor, in vec2 fragCoord, in vec2
--  resolution, in vec2 uv) ```
--
--  Where the input Fragcoord is the coordinate of the pixel we're currently
--  rendering, relative to the boundary rectangle that was specified in the
--  `GskGLShaderNode`, and Resolution is the width and height of that
--  rectangle. This is in the typical GTK coordinate system with the origin in
--  the top left. Uv contains the u and v coordinates that can be used to index
--  a texture at the corresponding point. These coordinates are in the
--  [0..1]x[0..1] region, with 0, 0 being in the lower left corder (which is
--  typical for OpenGL).
--
--  The output Fragcolor should be a RGBA color (with premultiplied alpha)
--  that will be used as the output for the specified pixel location. Note that
--  this output will be automatically clipped to the clip region of the
--  glshader node.
--
--  In addition to the function arguments the shader can define up to 4
--  uniforms for textures which must be called u_textureN (i.e. u_texture1 to
--  u_texture4) as well as any custom uniforms you want of types int, uint,
--  bool, float, vec2, vec3 or vec4.
--
--  All textures sources contain premultiplied alpha colors, but if some there
--  are outer sources of colors there is a gsk_premultiply helper to compute
--  premultiplication when needed.
--
--  Note that GTK parses the uniform declarations, so each uniform has to be
--  on a line by itself with no other code, like so:
--
--  ```glsl uniform float u_time; uniform vec3 u_color; uniform sampler2D
--  u_texture1; uniform sampler2D u_texture2; ```
--
--  GTK uses the "gsk" namespace in the symbols it uses in the shader, so your
--  code should not use any symbols with the prefix gsk or GSK. There are some
--  helper functions declared that you can use:
--
--  ```glsl vec4 GskTexture(sampler2D sampler, vec2 texCoords); ```
--
--  This samples a texture (e.g. u_texture1) at the specified coordinates, and
--  contains some helper ifdefs to ensure that it works on all OpenGL versions.
--
--  You can compile the shader yourself using [methodGsk.GLShader.compile],
--  otherwise the GSK renderer will do it when it handling the glshader node.
--  If errors occurs, the returned Error will include the glsl sources, so you
--  can see what GSK was passing to the compiler. You can also set
--  GSK_DEBUG=shaders in the environment to see the sources and other relevant
--  information about all shaders that GSK is handling.
--
--  # An example shader
--
--  ```glsl uniform float position; uniform sampler2D u_texture1; uniform
--  sampler2D u_texture2;
--
--  void mainImage(out vec4 fragColor, in vec2 fragCoord, in vec2 resolution,
--  in vec2 uv) { vec4 source1 = GskTexture(u_texture1, uv); vec4 source2 =
--  GskTexture(u_texture2, uv);
--
--  fragColor = position * source1 + (1.0 - position) * source2; } ```

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Bytes;      use Glib.Bytes;
with Glib.Error;      use Glib.Error;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Graphene.Vec2;   use Graphene.Vec2;
with Graphene.Vec3;   use Graphene.Vec3;
with Graphene.Vec4;   use Graphene.Vec4;
with Gsk.Enums;       use Gsk.Enums;
with Gsk.Renderer;    use Gsk.Renderer;
with Interfaces.C;    use Interfaces.C;

package Gsk.Glshader is

   pragma Obsolescent;
   --  This feature was deprecated in GTK 4.16 after the new rendering
   --  infrastructure introduced in 4.14 did not support it. The lack of Vulkan
   --  integration would have made it a very hard feature to support. If you
   --  want to use OpenGL directly, you should look at
   --  [GtkGLArea](../gtk4/class.GLArea.html), which uses a different approach
   --  and is still well-supported.

   type Gsk_Glshader_Record is new GObject_Record with null record;
   type Gsk_Glshader is access all Gsk_Glshader_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New_From_Bytes
      (Self       : out Gsk_Glshader;
       Sourcecode : Glib.Bytes.Gbytes);
   procedure Initialize_From_Bytes
      (Self       : not null access Gsk_Glshader_Record'Class;
       Sourcecode : Glib.Bytes.Gbytes);
   --  Creates a `GskGLShader` that will render pixels using the specified
   --  code.
   --  Initialize_From_Bytes does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Sourcecode GLSL sourcecode for the shader, as a `GBytes`

   function Gsk_Glshader_New_From_Bytes
      (Sourcecode : Glib.Bytes.Gbytes) return Gsk_Glshader;
   --  Creates a `GskGLShader` that will render pixels using the specified
   --  code.
   --  @param Sourcecode GLSL sourcecode for the shader, as a `GBytes`

   procedure Gtk_New_From_Resource
      (Self          : out Gsk_Glshader;
       Resource_Path : UTF8_String);
   procedure Initialize_From_Resource
      (Self          : not null access Gsk_Glshader_Record'Class;
       Resource_Path : UTF8_String);
   --  Creates a `GskGLShader` that will render pixels using the specified
   --  code.
   --  Initialize_From_Resource does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Resource_Path path to a resource that contains the GLSL
   --  sourcecode for the shader

   function Gsk_Glshader_New_From_Resource
      (Resource_Path : UTF8_String) return Gsk_Glshader;
   --  Creates a `GskGLShader` that will render pixels using the specified
   --  code.
   --  @param Resource_Path path to a resource that contains the GLSL
   --  sourcecode for the shader

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_gl_shader_get_type");

   -------------
   -- Methods --
   -------------

   function Compile
      (Self     : not null access Gsk_Glshader_Record;
       Renderer : not null access Gsk.Renderer.Gsk_Renderer_Record'Class;
       Error    : out Glib.Error.GError) return Boolean;
   pragma Obsolescent (Compile);
   --  Tries to compile the Shader for the given Renderer.
   --  If there is a problem, this function returns False and reports an
   --  error. You should use this function before relying on the shader for
   --  rendering and use a fallback with a simpler shader or without shaders if
   --  it fails.
   --  Note that this will modify the rendering state (for example change the
   --  current GL context) and requires the renderer to be set up. This means
   --  that the widget has to be realized. Commonly you want to call this from
   --  the realize signal of a widget, or during widget snapshot.
   --  Deprecated since 4.16, 1
   --  @param Renderer a `GskRenderer`
   --  @param Error the return location for a recoverable error
   --  @return True on success, False if an error occurred

   function Find_Uniform_By_Name
      (Self : not null access Gsk_Glshader_Record;
       Name : UTF8_String) return Glib.Gint;
   pragma Obsolescent (Find_Uniform_By_Name);
   --  Looks for a uniform by the name Name, and returns the index of the
   --  uniform, or -1 if it was not found.
   --  Deprecated since 4.16, 1
   --  @param Name uniform name
   --  @return The index of the uniform, or -1

   function Get_Arg_Bool
      (Self : not null access Gsk_Glshader_Record;
       Args : Glib.Bytes.Gbytes;
       Idx  : Glib.Gint) return Boolean;
   pragma Obsolescent (Get_Arg_Bool);
   --  Gets the value of the uniform Idx in the Args block.
   --  The uniform must be of bool type.
   --  Deprecated since 4.16, 1
   --  @param Args uniform arguments
   --  @param Idx index of the uniform
   --  @return The value

   function Get_Arg_Float
      (Self : not null access Gsk_Glshader_Record;
       Args : Glib.Bytes.Gbytes;
       Idx  : Glib.Gint) return Interfaces.C.C_float;
   pragma Obsolescent (Get_Arg_Float);
   --  Gets the value of the uniform Idx in the Args block.
   --  The uniform must be of float type.
   --  Deprecated since 4.16, 1
   --  @param Args uniform arguments
   --  @param Idx index of the uniform
   --  @return The value

   function Get_Arg_Int
      (Self : not null access Gsk_Glshader_Record;
       Args : Glib.Bytes.Gbytes;
       Idx  : Glib.Gint) return Gint32;
   pragma Obsolescent (Get_Arg_Int);
   --  Gets the value of the uniform Idx in the Args block.
   --  The uniform must be of int type.
   --  Deprecated since 4.16, 1
   --  @param Args uniform arguments
   --  @param Idx index of the uniform
   --  @return The value

   function Get_Arg_Uint
      (Self : not null access Gsk_Glshader_Record;
       Args : Glib.Bytes.Gbytes;
       Idx  : Glib.Gint) return Guint32;
   pragma Obsolescent (Get_Arg_Uint);
   --  Gets the value of the uniform Idx in the Args block.
   --  The uniform must be of uint type.
   --  Deprecated since 4.16, 1
   --  @param Args uniform arguments
   --  @param Idx index of the uniform
   --  @return The value

   procedure Get_Arg_Vec2
      (Self      : not null access Gsk_Glshader_Record;
       Args      : Glib.Bytes.Gbytes;
       Idx       : Glib.Gint;
       Out_Value : not null access Graphene.Vec2.Graphene_Vec2_T);
   pragma Obsolescent (Get_Arg_Vec2);
   --  Gets the value of the uniform Idx in the Args block.
   --  The uniform must be of vec2 type.
   --  Deprecated since 4.16, 1
   --  @param Args uniform arguments
   --  @param Idx index of the uniform
   --  @param Out_Value location to store the uniform value in

   procedure Get_Arg_Vec3
      (Self      : not null access Gsk_Glshader_Record;
       Args      : Glib.Bytes.Gbytes;
       Idx       : Glib.Gint;
       Out_Value : not null access Graphene.Vec3.Graphene_Vec3_T);
   pragma Obsolescent (Get_Arg_Vec3);
   --  Gets the value of the uniform Idx in the Args block.
   --  The uniform must be of vec3 type.
   --  Deprecated since 4.16, 1
   --  @param Args uniform arguments
   --  @param Idx index of the uniform
   --  @param Out_Value location to store the uniform value in

   procedure Get_Arg_Vec4
      (Self      : not null access Gsk_Glshader_Record;
       Args      : Glib.Bytes.Gbytes;
       Idx       : Glib.Gint;
       Out_Value : not null access Graphene.Vec4.Graphene_Vec4_T);
   pragma Obsolescent (Get_Arg_Vec4);
   --  Gets the value of the uniform Idx in the Args block.
   --  The uniform must be of vec4 type.
   --  Deprecated since 4.16, 1
   --  @param Args uniform arguments
   --  @param Idx index of the uniform
   --  @param Out_Value location to store set the uniform value in

   function Get_Args_Size
      (Self : not null access Gsk_Glshader_Record) return Gsize;
   pragma Obsolescent (Get_Args_Size);
   --  Get the size of the data block used to specify arguments for this
   --  shader.
   --  Deprecated since 4.16, 1
   --  @return The size of the data block

   function Get_N_Textures
      (Self : not null access Gsk_Glshader_Record) return Glib.Gint;
   pragma Obsolescent (Get_N_Textures);
   --  Returns the number of textures that the shader requires.
   --  This can be used to check that the a passed shader works in your
   --  usecase. It is determined by looking at the highest u_textureN value
   --  that the shader defines.
   --  Deprecated since 4.16, 1
   --  @return The number of texture inputs required by Shader

   function Get_N_Uniforms
      (Self : not null access Gsk_Glshader_Record) return Glib.Gint;
   pragma Obsolescent (Get_N_Uniforms);
   --  Get the number of declared uniforms for this shader.
   --  Deprecated since 4.16, 1
   --  @return The number of declared uniforms

   function Get_Resource
      (Self : not null access Gsk_Glshader_Record) return UTF8_String;
   pragma Obsolescent (Get_Resource);
   --  Gets the resource path for the GLSL sourcecode being used to render
   --  this shader.
   --  Deprecated since 4.16, 1
   --  @return The resource path for the shader

   function Get_Source
      (Self : not null access Gsk_Glshader_Record) return Glib.Bytes.Gbytes;
   pragma Obsolescent (Get_Source);
   --  Gets the GLSL sourcecode being used to render this shader.
   --  Deprecated since 4.16, 1
   --  @return The source code for the shader. Has transfer-ownership='none'.

   function Get_Uniform_Name
      (Self : not null access Gsk_Glshader_Record;
       Idx  : Glib.Gint) return UTF8_String;
   pragma Obsolescent (Get_Uniform_Name);
   --  Get the name of the declared uniform for this shader at index Idx.
   --  Deprecated since 4.16, 1
   --  @param Idx index of the uniform
   --  @return The name of the declared uniform

   function Get_Uniform_Offset
      (Self : not null access Gsk_Glshader_Record;
       Idx  : Glib.Gint) return Glib.Gint;
   pragma Obsolescent (Get_Uniform_Offset);
   --  Get the offset into the data block where data for this uniforms is
   --  stored.
   --  Deprecated since 4.16, 1
   --  @param Idx index of the uniform
   --  @return The data offset

   function Get_Uniform_Type
      (Self : not null access Gsk_Glshader_Record;
       Idx  : Glib.Gint) return Gsk.Enums.Gsk_GLUniform_Type;
   pragma Obsolescent (Get_Uniform_Type);
   --  Get the type of the declared uniform for this shader at index Idx.
   --  Deprecated since 4.16, 1
   --  @param Idx index of the uniform
   --  @return The type of the declared uniform

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Resource_Property : constant Glib.Properties.Property_String;
   --  Resource containing the source code for the shader.
   --
   --  If the shader source is not coming from a resource, this will be null.

   Source_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GLib.Bytes
   --  The source code for the shader, as a `GBytes`.

private
   Source_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("source");
   Resource_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("resource");
end Gsk.Glshader;
