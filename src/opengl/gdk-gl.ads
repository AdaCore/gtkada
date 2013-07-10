------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2013, AdaCore                     --
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

package Gdk.GL is

   type GL_Configs is new Integer;
   for GL_Configs'Size use Integer'Size;

   Gdk_GL_None : constant GL_Configs := 0;
   Gdk_GL_Use_Gl : constant GL_Configs := 1;
   Gdk_GL_Buffer_Size : constant GL_Configs := 2;
   Gdk_GL_Level : constant GL_Configs := 3;
   Gdk_GL_Rgba : constant GL_Configs := 4;
   Gdk_GL_Doublebuffer : constant GL_Configs := 5;
   Gdk_GL_Stereo : constant GL_Configs := 6;
   Gdk_GL_Aux_Buffers : constant GL_Configs := 7;
   Gdk_GL_Red_Size : constant GL_Configs := 8;
   Gdk_GL_Green_Size : constant GL_Configs := 9;
   Gdk_GL_Blue_Size : constant GL_Configs := 10;
   Gdk_GL_Alpha_Size : constant GL_Configs := 11;
   Gdk_GL_Depth_Size : constant GL_Configs := 12;
   Gdk_GL_Stencil_Size : constant GL_Configs := 13;
   Gdk_GL_Accum_Red_Size : constant GL_Configs := 14;
   Gdk_GL_Accum_Green_Size : constant GL_Configs := 15;
   Gdk_GL_Accum_Blue_Size : constant GL_Configs := 16;
   Gdk_GL_Accum_Alpha_Size : constant GL_Configs := 17;

   --  Extensions
   Gdk_GL_X_Visual_Type_Ext : constant GL_Configs := 16#22#;
   Gdk_GL_Transparent_Type_Ext : constant GL_Configs := 16#23#;
   Gdk_GL_Transparent_Index_Value_Ext : constant GL_Configs := 16#24#;
   Gdk_GL_Transparent_Red_Value_Ext : constant GL_Configs := 16#25#;
   Gdk_GL_Transparent_Green_Value_Ext : constant GL_Configs := 16#26#;
   Gdk_GL_Transparent_Blue_Value_Ext : constant GL_Configs := 16#27#;
   Gdk_GL_Transparent_Alpha_Value_Ext : constant GL_Configs := 16#28#;

   function Query return Boolean;
   --  Returns true if OpenGL is supported

end Gdk.GL;
