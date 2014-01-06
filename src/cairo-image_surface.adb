------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2014, AdaCore                     --
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

package body Cairo.Image_Surface is

   ----------------------------
   -- Create_For_Data_ARGB32 --
   ----------------------------

   function Create_For_Data_ARGB32
     (Data   : ARGB32_Array_Access;
      Width  : Gint;
      Height : Gint)
      return Cairo_Surface
   is
      Stride : constant Gint := Cairo_Format_Stride_For_Width
        (Format => Cairo_Format_ARGB32,
         Width  => Width);
   begin
      return Create_For_Data_Generic
        (Data (Data'First)'Address,
         Cairo_Format_ARGB32,
         Width,
         Height,
         Stride);
   end Create_For_Data_ARGB32;

   ---------------------------
   -- Create_For_Data_RGB24 --
   ---------------------------

   function Create_For_Data_RGB24
     (Data   : RGB24_Array_Access;
      Width  : Gint;
      Height : Gint)
      return Cairo_Surface
   is
      Stride : constant Gint := Cairo_Format_Stride_For_Width
        (Format => Cairo_Format_RGB24,
         Width  => Width);
   begin
      return Create_For_Data_Generic
        (Data (Data'First)'Address,
         Cairo_Format_RGB24,
         Width,
         Height,
         Stride);
   end Create_For_Data_RGB24;

   ------------------------
   -- Create_For_Data_A8 --
   ------------------------

   function Create_For_Data_A8
     (Data   : Byte_Array_Access;
      Width  : Gint;
      Height : Gint)
      return Cairo_Surface
   is
      Stride : constant Gint := Cairo_Format_Stride_For_Width
        (Format => Cairo_Format_A8,
         Width  => Width);
   begin
      return Create_For_Data_Generic
        (Data (Data'First)'Address,
         Cairo_Format_A8,
         Width,
         Height,
         Stride);
   end Create_For_Data_A8;

end Cairo.Image_Surface;
