------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                       Copyright (C) 2026, AdaCore                        --
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

pragma Ada_2022;

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings;

with Gdk.GLContext;
with Glib;
with Gtk.Box;    use Gtk.Box;
with Gtk.Frame;  use Gtk.Frame;
with Gtk.GLArea; use Gtk.GLArea;
with Gtk.GRange;
with Gtk.Scale;  use Gtk.Scale;
with Gtk.Widget;

with Epoxy;
with OpenGL;
with System;

with Create_GL.GLSL;
with System.Storage_Elements;

package body Create_GL is

   use type Interfaces.C.ptrdiff_t;
   use type OpenGL.GLfloat;
   use type OpenGL.GLint;

   procedure On_Realize (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Resize
     (Widget : access Gtk.GLArea.Gtk_GLArea_Record'Class;
      Width  : Glib.Gint;
      Height : Glib.Gint);

   function On_Render
     (Widget    : access Gtk.GLArea.Gtk_GLArea_Record'Class;
      GLContext : not null access Gdk.GLContext.Gdk_GLContext_Record'Class)
      return Boolean;

   procedure Initialize_Shaders;

   procedure Initialize_Buffers;

   procedure On_Value_Changed
     (Self : access Gtk.GRange.Gtk_Range_Record'Class);

   type Vertex_Info is record
      Position : OpenGL.GLfloat_Vector_3;
      Color    : OpenGL.GLfloat_Vector_3;
   end record with Convention => C;

   GL_Area : Gtk_GLArea;
   X_Scale : Gtk_Scale;
   Y_Scale : Gtk_Scale;
   Z_Scale : Gtk_Scale;

   Vertex_Data : constant array (1 .. 3) of Vertex_Info :=
     [([0.0,   0.5,   0.0], [1.0, 0.0, 0.0]),
      ([0.5,  -0.366, 0.0], [0.0, 1.0, 0.0]),
      ([-0.5, -0.366, 0.0], [0.0, 0.0, 1.0])] with Convention => C;

   Program           : Epoxy.GLuint;
   P_Location        : Epoxy.GLuint;
   MV_Location       : Epoxy.GLuint;
   Position_Location : Epoxy.GLuint;
   Color_Location    : Epoxy.GLuint;
   VAO               : aliased Epoxy.GLuint;

   P  : OpenGL.GLfloat_Matrix_4x4 :=
     [[1.0, 0.0, 0.0, 0.0],
      [0.0, 1.0, 0.0, 0.0],
      [0.0, 0.0, 1.0, 0.0],
      [0.0, 0.0, 0.0, 1.0]];
   MV : OpenGL.GLfloat_Matrix_4x4 :=
     [[1.0, 0.0, 0.0, 0.0],
      [0.0, 1.0, 0.0, 0.0],
      [0.0, 0.0, 1.0, 0.0],
      [0.0, 0.0, 0.0, 1.0]];

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo show how you can use @GtkGLArea@ widget.";
   end Help;

   ------------------------
   -- Initialize_Buffers --
   ------------------------

   procedure Initialize_Buffers is
      Buffer : aliased Epoxy.GLuint;

      Dummy  : Vertex_Info;

   begin
      --  Create VAO to store other buffers
      Epoxy.glGenVertexArrays (1, VAO'Access);
      Epoxy.glBindVertexArray (VAO);

      --  Buffer for vertex data
      Epoxy.glGenBuffers (1, Buffer'Access);
      Epoxy.glBindBuffer (Epoxy.GL_ARRAY_BUFFER, buffer);
      Epoxy.glBufferData
        (Epoxy.GL_ARRAY_BUFFER,
         vertex_data'Size / System.Storage_Unit,
         vertex_data'Address,
         Epoxy.GL_STATIC_DRAW);

      --  Enable and set the position attribute
      Epoxy.glEnableVertexAttribArray (Position_Location);
      Epoxy.glVertexAttribPointer
        (Position_Location,
         Dummy.Position'Length,
         Epoxy.GL_FLOAT,
         Epoxy.GL_FALSE,
         Dummy'Size / System.Storage_Unit,
         System.Storage_Elements.To_Address (Dummy.Position'Position));

      --  Enable and set the color attribute
      Epoxy.glEnableVertexAttribArray (Color_Location);
      Epoxy.glVertexAttribPointer
        (Color_Location,
         Dummy.Color'Length,
         Epoxy.GL_FLOAT,
         Epoxy.GL_FALSE,
         Dummy'Size / System.Storage_Unit,
         System.Storage_Elements.To_Address (Dummy.Color'Position));

      --  Reset state and cleanup
      Epoxy.glBindBuffer (Epoxy.GL_ARRAY_BUFFER, 0);
      Epoxy.glBindVertexArray (0);
      Epoxy.glDeleteBuffers (1, Buffer'Access);
   end Initialize_Buffers;

   ------------------------
   -- Initialize_Shaders --
   ------------------------

   procedure Initialize_Shaders is

      function Create_Shader
        (Shader_Type : OpenGL.GLenum;
         Source      : Interfaces.C.Strings.chars_ptr_array)
         return Epoxy.GLuint;

      -------------------
      -- Create_Shader --
      -------------------

      function Create_Shader
        (Shader_Type : OpenGL.GLenum;
         Source      : Interfaces.C.Strings.chars_ptr_array)
         return Epoxy.GLuint
      is
         Shader : Epoxy.GLuint;
         Status : OpenGL.GLint;
         Length : OpenGL.GLint;

      begin
         Shader := Epoxy.glCreateShader (Shader_Type);
         Epoxy.glShaderSource
           (Shader,
            Source'Length,
            Source,
            [for S of Source =>
               OpenGL.GLint (Interfaces.C.Strings.Strlen (S))]);
         Epoxy.glCompileShader (Shader);

         Epoxy.glGetShaderiv (Shader, Epoxy.GL_COMPILE_STATUS, Status);

         if Status = Epoxy.GL_FALSE then
            Epoxy.glGetShaderiv (Shader, Epoxy.GL_INFO_LOG_LENGTH, Length);

            declare
               Log_Length : OpenGL.GLsizei;
               Log_Text   : Interfaces.C.char_array
                              (1 .. Interfaces.C.size_t (Length + 1));

            begin
               Epoxy.glGetShaderInfoLog (Shader, Length, Log_Length, Log_Text);

               Put_Line (Interfaces.C.To_Ada (Log_Text));
            end;

            raise Program_Error;
         end if;

         return Shader;
      end Create_Shader;

      Vertex   : Epoxy.GLuint;
      Fragment : Epoxy.GLuint;
      Status   : OpenGL.GLint;

   begin
      Vertex   := Create_Shader (Epoxy.GL_VERTEX_SHADER, GLSL.Vertex);
      Fragment := Create_Shader (Epoxy.GL_FRAGMENT_SHADER, GLSL.Fragment);

      Program := Epoxy.glCreateProgram.all;
      Epoxy.glAttachShader (Program, Vertex);
      Epoxy.glAttachShader (Program, Fragment);
      Epoxy.glBindFragDataLocation (Program, 0, GLSL.Output_Color);
      Epoxy.glLinkProgram (Program);

      Epoxy.glGetProgramiv (program, Epoxy.GL_LINK_STATUS, Status);

      if Status = Epoxy.GL_FALSE then
         raise Program_Error;
      end if;

      P_Location        :=
        Epoxy.GLuint (Epoxy.glGetUniformLocation (Program, GLSL.P));
      MV_Location       :=
        Epoxy.GLuint (Epoxy.glGetUniformLocation (Program, GLSL.MV));
      Position_Location :=
        Epoxy.GLuint (Epoxy.glGetAttribLocation (Program, GLSL.Position));
      Color_Location    :=
        Epoxy.GLuint (Epoxy.glGetAttribLocation (Program, GLSL.Color));

      Epoxy.glDetachShader (Program, Vertex);
      Epoxy.glDetachShader (Program, Fragment);

      Epoxy.glDeleteShader (Vertex);
      Epoxy.glDeleteShader (Fragment);
   end Initialize_Shaders;

   ----------------
   -- On_Realize --
   ----------------

   procedure On_Realize (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
      GL : Gtk_GLArea renames Gtk_GLArea (Widget);

   begin
      --  Make OpenGL context of the widget current
      Make_Current (GL);

      Initialize_Shaders;
      Initialize_Buffers;
   end On_Realize;

   ---------------
   -- On_Render --
   ---------------

   function On_Render
     (Widget    : access Gtk.GLArea.Gtk_GLArea_Record'Class;
      GLContext : not null access Gdk.GLContext.Gdk_GLContext_Record'Class)
      return Boolean is
      use type OpenGL.GLbitfield;
      GL : Gtk_GLArea renames Gtk_GLArea (Widget);
   begin
      Epoxy.glClearColor (0.5, 0.5, 0.5, 1.0);
      Epoxy.glClear (Epoxy.GL_COLOR_BUFFER_BIT + Epoxy.GL_DEPTH_BUFFER_BIT);

      Epoxy.glUseProgram (Program);
      Epoxy.glUniformMatrix4fv
        (OpenGL.GLint (P_Location), 1, Epoxy.GL_FALSE, P);
      Epoxy.glUniformMatrix4fv
        (OpenGL.GLint (MV_Location), 1, Epoxy.GL_FALSE, MV);
      Epoxy.glBindVertexArray (VAO);

      Epoxy.glDrawArrays (Epoxy.GL_TRIANGLES, 0, 3);

      Epoxy.glBindVertexArray (0);
      Epoxy.glUseProgram (0);

      return True;
   end On_Render;

   ---------------
   -- On_Resize --
   ---------------

   procedure On_Resize
     (Widget : access Gtk.GLArea.Gtk_GLArea_Record'Class;
      Width  : Glib.Gint;
      Height : Glib.Gint)
   is
      use type Glib.Gint;

      Aspect : constant OpenGL.GLfloat :=
        OpenGL.GLfloat (Width) / OpenGL.GLfloat (Height);

   begin
      if Width < Height then
         P (1, 1) := 1.0;
         P (2, 2) := Aspect;

      else
         P (1, 1) := 1.0 / Aspect;
         P (2, 2) := 1.0;
      end if;
   end On_Resize;

   ----------------------
   -- On_Value_Changed --
   ----------------------

   procedure On_Value_Changed
     (Self : access Gtk.GRange.Gtk_Range_Record'Class)
   is
      package Elementary_Functions is
        new Ada.Numerics.Generic_Elementary_Functions (OpenGL.GLfloat);
      use Elementary_Functions;

      A1 : constant OpenGL.GLfloat :=
        OpenGL.GLfloat (X_Scale.Get_Value) * Ada.Numerics.Pi / 180.0;
      A2 : constant OpenGL.GLfloat :=
        OpenGL.GLfloat (Y_Scale.Get_Value) * Ada.Numerics.Pi / 180.0;
      A3 : constant OpenGL.GLfloat :=
        OpenGL.GLfloat (Z_Scale.Get_Value) * Ada.Numerics.Pi / 180.0;

      C1 : constant OpenGL.GLfloat := Cos (A1);
      S1 : constant OpenGL.GLfloat := Sin (A1);
      C2 : constant OpenGL.GLfloat := Cos (A2);
      S2 : constant OpenGL.GLfloat := Sin (A2);
      C3 : constant OpenGL.GLfloat := Cos (A3);
      S3 : constant OpenGL.GLfloat := Sin (A3);

      C3C2   : constant OpenGL.GLfloat := C3 * C2;
      S3C1   : constant OpenGL.GLfloat := S3 * C1;
      C3S2S1 : constant OpenGL.GLfloat := C3 * S2 * S1;
      S3S1   : constant OpenGL.GLfloat := S3 * S1;
      C3S2C1 : constant OpenGL.GLfloat := C3 * S2 * C1;
      S3C2   : constant OpenGL.GLfloat := S3 * C2;
      C3C1   : constant OpenGL.GLfloat := C3 * C1;
      S3S2S1 : constant OpenGL.GLfloat := S3 * S2 * S1;
      C3S1   : constant OpenGL.GLfloat := C3 * S1;
      S3S2C1 : constant OpenGL.GLfloat := S3 * S2 * C1;
      C2S1   : constant OpenGL.GLfloat := C2 * S1;
      C2C1   : constant OpenGL.GLfloat := C2 * C1;

   begin
      --  Apply all three Euler angles rotations using the three matrices:
      --
      --  ⎡  c3 s3 0 ⎤ ⎡ c2  0 -s2 ⎤ ⎡ 1   0  0 ⎤
      --  ⎢ -s3 c3 0 ⎥ ⎢  0  1   0 ⎥ ⎢ 0  c1 s1 ⎥
      --  ⎣   0  0 1 ⎦ ⎣ s2  0  c2 ⎦ ⎣ 0 -s1 c1 ⎦

      MV :=
        [[C3C2,  S3C1 + C3S2S1, S3S1 - C3S2C1, 0.0],
         [-S3C2, C3C1 - S3S2S1, C3S1 + S3S2C1, 0.0],
         [S2,    -C2S1,         C2C1,          0.0],
         [0.0, 0.0, 0.0, 1.0]];

      GL_Area.Queue_Draw;
   end On_Value_Changed;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      use type Glib.Gdouble;

      Box   : Gtk_Box;

   begin
      Set_Label (Frame, "GtkGLArea");

      Gtk_New_Vbox (Box, False, 0);
      Add (Frame, Box);

      Gtk_New (GL_Area);
      GL_Area.On_Realize (On_Realize'Access);
      GL_Area.On_Resize (On_Resize'Access);
      GL_Area.On_Render (On_Render'Access);
      GL_Area.Set_Has_Depth_Buffer (True);
      Pack_Start (Box, GL_Area);

      X_Scale := Gtk_Hscale_New_With_Range (-180.0, 180.0, 1.0);
      X_Scale.Set_Value (0.0);
      X_Scale.On_Value_Changed (On_Value_Changed'Access);
      Pack_Start (Box, X_Scale, False);

      Y_Scale := Gtk_Hscale_New_With_Range (-180.0, 180.0, 1.0);
      Y_Scale.Set_Value (0.0);
      Y_Scale.On_Value_Changed (On_Value_Changed'Access);
      Pack_Start (Box, Y_Scale, False);

      Z_Scale := Gtk_Hscale_New_With_Range (-180.0, 180.0, 1.0);
      Z_Scale.Set_Value (0.0);
      Z_Scale.On_Value_Changed (On_Value_Changed'Access);
      Pack_Start (Box, Z_Scale, False);

      Show_All (Frame);
   end Run;

end Create_GL;
