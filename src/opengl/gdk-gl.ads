-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

package Gdk.GL is

   type Gl_Configs is
     (Gdk_GL_None,
      Gdk_GL_Use_Gl,
      Gdk_GL_Buffer_Size,
      Gdk_GL_Level,
      Gdk_GL_Rgba,
      Gdk_GL_Doublebuffer,
      Gdk_GL_Stereo,
      Gdk_GL_Aux_Buffers,
      Gdk_GL_Red_Size,
      Gdk_GL_Green_Size,
      Gdk_GL_Blue_Size,
      Gdk_GL_Alpha_Size,
      Gdk_GL_Depth_Size,
      Gdk_GL_Stencil_Size,
      Gdk_GL_Accum_Red_Size,
      Gdk_GL_Accum_Green_Size,
      Gdk_GL_Accum_Blue_Size,
      Gdk_GL_Accum_Alpha_Size,

      --  Extensions
      Gdk_GL_X_Visual_Type_Ext,
      Gdk_GL_Transparent_Type_Ext,
      Gdk_GL_Transparent_Index_Value_Ext,
      Gdk_GL_Transparent_Red_Value_Ext,
      Gdk_GL_Transparent_Green_Value_Ext,
      Gdk_GL_Transparent_Blue_Value_Ext,
      Gdk_GL_Transparent_Alpha_Value_Ext);

   function Query return Boolean;
   --  Returns true if OpenGL is supported

private

   for Gl_Configs'Size use Integer'Size;
   for Gl_Configs use
     (Gdk_GL_None => 0,
      Gdk_GL_Use_Gl => 1,
      Gdk_GL_Buffer_Size => 2,
      Gdk_GL_Level => 3,
      Gdk_GL_Rgba => 4,
      Gdk_GL_Doublebuffer => 5,
      Gdk_GL_Stereo => 6,
      Gdk_GL_Aux_Buffers => 7,
      Gdk_GL_Red_Size => 8,
      Gdk_GL_Green_Size => 9,
      Gdk_GL_Blue_Size => 10,
      Gdk_GL_Alpha_Size => 11,
      Gdk_GL_Depth_Size => 12,
      Gdk_GL_Stencil_Size => 13,
      Gdk_GL_Accum_Red_Size => 14,
      Gdk_GL_Accum_Green_Size => 15,
      Gdk_GL_Accum_Blue_Size => 16,
      Gdk_GL_Accum_Alpha_Size => 17,

      --  Extensions
      Gdk_GL_X_Visual_Type_Ext => 16#22#,
      Gdk_GL_Transparent_Type_Ext => 16#23#,
      Gdk_GL_Transparent_Index_Value_Ext => 16#24#,
      Gdk_GL_Transparent_Red_Value_Ext => 16#25#,
      Gdk_GL_Transparent_Green_Value_Ext => 16#26#,
      Gdk_GL_Transparent_Blue_Value_Ext => 16#27#,
      Gdk_GL_Transparent_Alpha_Value_Ext => 16#28#);

end Gdk.gl;
