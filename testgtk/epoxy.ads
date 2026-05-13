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

--  This package provides binding to Epoxy library, which hides platform
--  specific loading of OpenGL libraries.

with Interfaces.C.Strings;
with System;

with OpenGL;

package Epoxy is

   type GLboolean is new Interfaces.Integer_32;
   type GLuint is new Interfaces.Unsigned_32;

   type GLint_Array is array (Positive range <>) of OpenGL.GLint
     with Convention => C;

   GL_DEPTH_BUFFER_BIT : constant := 16#00000100#;
   GL_COLOR_BUFFER_BIT : constant := 16#00004000#;

   GL_FALSE                                            : constant :=        0;
   GL_TRIANGLES                                        : constant := 16#0004#;
   GL_FLOAT                                            : constant := 16#1406#;
   GL_ARRAY_BUFFER                                     : constant := 16#8892#;
   GL_STATIC_DRAW                                      : constant := 16#88E4#;
   GL_FRAGMENT_SHADER                                  : constant := 16#8B30#;
   GL_VERTEX_SHADER                                    : constant := 16#8B31#;
   GL_COMPILE_STATUS                                   : constant := 16#8B81#;
   GL_LINK_STATUS                                      : constant := 16#8B82#;
   GL_INFO_LOG_LENGTH                                  : constant := 16#8B84#;

   type PFNGLATTACHSHADERPROC is access procedure
     (program : epoxy.GLuint;
      shader  : epoxy.GLuint) with Convention => C;

   glAttachShader : constant not null PFNGLATTACHSHADERPROC
     with Import, Convention => C, External_Name => "epoxy_glAttachShader";

   type PFNGLBINDBUFFERPROC is access procedure
     (target : OpenGL.GLenum;
      buffer : Epoxy.GLuint) with Convention => C;

   glBindBuffer : constant not null PFNGLBINDBUFFERPROC
     with Import, Convention => C, External_Name => "epoxy_glBindBuffer";

   type PFNGLBINDFRAGDATALOCATIONPROC is access procedure
     (program : Epoxy.GLuint;
      color   : Epoxy.GLuint;
      name    : Interfaces.C.char_array)
     with Convention => C;

   glBindFragDataLocation : constant not null PFNGLBINDFRAGDATALOCATIONPROC
     with Import, Convention => C, External_Name => "epoxy_glBindFragDataLocation";

   type PFNGLBINDVERTEXARRAYPROC is access procedure
     (the_array : Epoxy.GLuint) with Convention => C;

   glBindVertexArray : constant not null PFNGLBINDVERTEXARRAYPROC
     with Import, Convention => C, External_Name => "epoxy_glBindVertexArray";

   type PFNGLBUFFERDATAPROC is access procedure
     (target : OpenGL.GLenum;
      size   : Interfaces.C.ptrdiff_t;
      data   : System.Address;
      usage  : OpenGL.GLenum) with Convention => C;

   glBufferData : constant not null PFNGLBUFFERDATAPROC
     with Import, Convention => C, External_Name => "epoxy_glBufferData";

   type PFNGLCOMPILESHADERPROC is access procedure (shader : epoxy.GLuint)
     with Convention => C;

   glCompileShader : constant not null PFNGLCOMPILESHADERPROC
     with Import, Convention => C, External_Name => "epoxy_glCompileShader";

   type PFNGLCLEARPROC is access procedure (mask : OpenGL.GLbitfield)
     with Convention => C;

   glClear : constant not null PFNGLCLEARPROC
     with Import, Convention => C, External_Name => "epoxy_glClear";

   type PFNGLCLEARCOLORPROC is access procedure
     (red   : OpenGL.GLfloat;
      green : OpenGL.GLfloat;
      blue  : OpenGL.GLfloat;
      alpha : OpenGL.GLfloat) with Convention => C;

   glClearColor : constant not null PFNGLCLEARCOLORPROC
     with Import, Convention => C, External_Name => "epoxy_glClearColor";

   type PFNGLCREATEPROGRAMPROC is access function return epoxy.GLuint
     with Convention => C;

   glCreateProgram : constant not null PFNGLCREATEPROGRAMPROC
     with Import, Convention => C, External_Name => "epoxy_glCreateProgram";

   type PFNGLCREATESHADERPROC is access function
     (the_type : OpenGL.GLenum) return epoxy.GLuint
        with Convention => C;

   glCreateShader : constant not null PFNGLCREATESHADERPROC
     with Import, Convention => C, External_Name => "epoxy_glCreateShader";

   type PFNGLDELETEBUFFERSPROC is access procedure
     (n       : OpenGL.GLsizei;
      buffers : access epoxy.GLuint) with Convention => C;

   glDeleteBuffers : constant not null PFNGLDELETEBUFFERSPROC
     with Import, Convention => C, External_Name => "epoxy_glDeleteBuffers";

   type PFNGLDELETESHADERPROC is access procedure (shader : epoxy.GLuint)
     with Convention => C;

   glDeleteShader : constant not null PFNGLDELETESHADERPROC
     with Import, Convention => C, External_Name => "epoxy_glDeleteShader";

   type PFNGLDETACHSHADERPROC is access procedure
     (program : epoxy.GLuint;
      shader  : epoxy.GLuint)
     with Convention => C;

   glDetachShader : constant not null PFNGLDETACHSHADERPROC
     with Import, Convention => C, External_Name => "epoxy_glDetachShader";

   type PFNGLDRAWARRAYSPROC is access procedure
     (mode  : OpenGL.GLenum;
      first : OpenGL.GLint;
      count : OpenGL.GLsizei) with Convention => C;

   glDrawArrays : constant not null PFNGLDRAWARRAYSPROC
     with Import, Convention => C, External_Name => "epoxy_glDrawArrays";

   type PFNGLENABLEVERTEXATTRIBARRAYPROC is access procedure
     (index : epoxy.GLuint) with Convention => C;

   glEnableVertexAttribArray : constant
     not null PFNGLENABLEVERTEXATTRIBARRAYPROC
       with Import,
            Convention    => C,
            External_Name => "epoxy_glEnableVertexAttribArray";

   type PFNGLFLUSHPROC is access procedure with Convention => C;

   glFlush : constant not null PFNGLFLUSHPROC
     with Import, Convention => C, External_Name => "epoxy_glFlush";

   type PFNGLGENBUFFERSPROC is access procedure
     (n       : OpenGL.GLsizei;
      buffers : access Epoxy.GLuint) with Convention => C;

   glGenBuffers : constant not null PFNGLGENBUFFERSPROC
     with Import, Convention => C, External_Name => "epoxy_glGenBuffers";

   type PFNGLGENVERTEXARRAYSPROC is access procedure
     (n      : OpenGL.GLsizei;
      arrays : access Epoxy.GLuint) with Convention => C;

   glGenVertexArrays : constant not null PFNGLGENVERTEXARRAYSPROC
     with Import, Convention => C, External_Name => "epoxy_glGenVertexArrays";

   type PFNGLGETATTRIBLOCATIONPROC is access function
     (program : epoxy.GLuint;
      name    : Interfaces.C.char_array) return OpenGL.GLint
        with Convention => C;

   glGetAttribLocation : constant not null PFNGLGETATTRIBLOCATIONPROC
     with Import,
          Convention => C,
          External_Name => "epoxy_glGetAttribLocation";

   type PFNGLGETERRORPROC is access function return OpenGL.GLenum
     with Convention => C;

   glGetError : constant not null PFNGLGETERRORPROC
     with Import, Convention => C, External_Name => "epoxy_glGetError";

   type PFNGLGETPROGRAMIVPROC is access procedure
     (program : epoxy.GLuint;
      pname   : OpenGL.GLenum;
      params  : out OpenGL.GLint) with Convention => C;

   glGetProgramiv : constant not null PFNGLGETPROGRAMIVPROC
     with Import, Convention => C, External_Name => "epoxy_glGetProgramiv";

   type PFNGLGETSHADERINFOLOGPROC is access procedure
     (shader  : epoxy.GLuint;
      bufSize : OpenGL.GLsizei;
      length  : out OpenGL.GLsizei;
      infoLog : out Interfaces.C.char_array) with Convention => C;

   glGetShaderInfoLog : constant not null PFNGLGETSHADERINFOLOGPROC
     with Import, Convention => C, External_Name => "epoxy_glGetShaderInfoLog";

   type PFNGLGETSHADERIVPROC is access procedure
     (shader : epoxy.GLuint;
      pname  : OpenGL.GLenum;
      params : out OpenGL.GLint) with Convention => C;

   glGetShaderiv : constant not null PFNGLGETSHADERIVPROC
     with Import, Convention => C, External_Name => "epoxy_glGetShaderiv";

   type PFNGLGETUNIFORMLOCATIONPROC is access function
     (program : epoxy.GLuint;
      name    : Interfaces.C.char_array) return OpenGL.GLint
     with Convention => C;

   glGetUniformLocation : constant not null PFNGLGETUNIFORMLOCATIONPROC
     with Import,
          Convention    => C,
          External_Name => "epoxy_glGetUniformLocation";

   type PFNGLLINKPROGRAMPROC is access procedure (program : epoxy.GLuint)
     with Convention => C;

   glLinkProgram : constant not null PFNGLLINKPROGRAMPROC
     with Import, Convention => C, External_Name => "epoxy_glLinkProgram";

   type PFNGLSHADERSOURCEPROC is access procedure
     (shader : epoxy.GLuint;
      count  : OpenGL.GLsizei;
      string : Interfaces.C.Strings.chars_ptr_array;
      length : GLint_Array) with Convention => C;

   glShaderSource : constant not null PFNGLSHADERSOURCEPROC
     with Import, Convention => C, External_Name => "epoxy_glShaderSource";

   type PFNGLUNIFORMMATRIX4FVPROC is access procedure
     (location  : OpenGL.GLint;
      count     : OpenGL.GLsizei;
      transpose : epoxy.GLboolean;
      value     : OpenGL.GLfloat_Matrix_4x4) with Convention => C;

   glUniformMatrix4fv : constant not null PFNGLUNIFORMMATRIX4FVPROC
     with Import, Convention => C, External_Name => "epoxy_glUniformMatrix4fv";

   type PFNGLUSEPROGRAMPROC is access procedure (program : epoxy.GLuint)
     with Convention => C;

   glUseProgram : constant not null PFNGLUSEPROGRAMPROC
     with Import, Convention => C, External_Name => "epoxy_glUseProgram";

   type PFNGLVERTEXATTRIBPOINTERPROC is access procedure
     (index      : epoxy.GLuint;
      size       : OpenGL.GLint;
      the_type   : OpenGL.GLenum;
      normalized : epoxy.GLboolean;
      stride     : OpenGL.GLsizei;
      pointer    : System.Address) with Convention => C;

   glVertexAttribPointer : constant not null PFNGLVERTEXATTRIBPOINTERPROC
     with Import,
          Convention    => C,
          External_Name => "epoxy_glVertexAttribPointer";

   type PFNGLVIEWPORTPROC is access procedure
     (x      : OpenGL.GLint;
      y      : OpenGL.GLint;
      width  : OpenGL.GLsizei;
      height : OpenGL.GLsizei) with Convention => C;

   glViewport : constant not null PFNGLVIEWPORTPROC
     with Import, Convention => C, External_Name => "epoxy_glViewport";

end Epoxy;
