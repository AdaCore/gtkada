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

with Ada.Characters.Latin_1;
with Interfaces.C.Strings;

private package Create_GL.GLSL is

   LF : Character renames Ada.Characters.Latin_1.LF;

   function "-" (Item : String) return Interfaces.C.Strings.chars_ptr
     renames Interfaces.C.Strings.New_String;

   Vertex   : constant Interfaces.C.Strings.chars_ptr_array :=
     [-("#version 130" & LF
      & "in vec3 position;" & LF
      & "in vec3 color;" & LF
      & "uniform mat4 p;" & LF
      & "uniform mat4 mv;" & LF
      & "smooth out vec4 vertexColor;" & LF
      & "void main() {" & LF
      & "  gl_Position = p * mv * vec4(position, 1.0);" & LF
      & "  vertexColor = vec4(color, 1.0);" & LF
      & "}" & LF)];

   Fragment : constant Interfaces.C.Strings.chars_ptr_array :=
     [-("#version 130" & LF
      & "smooth in vec4 vertexColor;" & LF
      & "out vec4 outputColor;" & LF
      & "void main() {" & LF
      & "  outputColor = vertexColor;" & LF
      & "}" & LF)];

   P            : constant Interfaces.C.char_array :=
     Interfaces.C.To_C ("p");
   MV           : constant Interfaces.C.char_array :=
     Interfaces.C.To_C ("mv");
   Position     : constant Interfaces.C.char_array :=
     Interfaces.C.To_C ("position");
   Color        : constant Interfaces.C.char_array :=
     Interfaces.C.To_C ("color");
   Output_Color : constant Interfaces.C.char_array :=
     Interfaces.C.To_C ("outputColor");

end Create_GL.GLSL;
