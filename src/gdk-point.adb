-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
--         General Public License for more details.                  --
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

package body Gdk.Point is


   ---------------
   --  Destroy  --
   ---------------

   procedure Destroy (Point : in out Gdk_Point) is
      procedure Internal (Point : in System.Address);
      pragma Import (C, Internal, "ada_gdk_point_destroy");
   begin
      Internal (Get_Object (Point));
      Set_Object (Point, System.Null_Address);
   end Destroy;


   -------------
   --  Get_X  --
   -------------

   function Get_X (Point : in Gdk_Point) return Gint16 is
      function Internal (Point : in System.Address) return Gint16;
      pragma Import (C, Internal, "ada_gdk_point_get_x");
   begin
      return Internal (Get_Object (Point));
   end Get_X;


   -------------
   --  Get_Y  --
   -------------

   function Get_Y (Point : in Gdk_Point) return Gint16 is
      function Internal (Point : in System.Address) return Gint16;
      pragma Import (C, Internal, "ada_gdk_point_get_y");
   begin
      return Internal (Get_Object (Point));
   end Get_Y;


   ---------------
   --  Gdk_New  --
   ---------------

   procedure Gdk_New (Point : out Gdk_Point) is
   begin
      Gdk_New (Point => Point, X => 0, Y => 0);
   end Gdk_New;


   ---------------
   --  Gdk_New  --
   ---------------

   procedure Gdk_New (Point :    out Gdk_Point;
                      X     : in     Gint16;
                      Y     : in     Gint16) is
      function Internal (X, Y : in Gint16) return System.Address;
      pragma Import (C, Internal, "ada_gdk_point_new_with_coordinates");
   begin
      Set_Object (Point, Internal (X, Y));
   end Gdk_New;


   -----------------------
   --  Set_Coordinates  --
   -----------------------

   procedure Set_Coordinates (Point : in out Gdk_Point;
                              X     : in     Gint16;
                              Y     : in     Gint16) is
      procedure Internal (Point : in System.Address;
                          X, Y  : in Gint16);
      pragma Import (C, Internal, "ada_gdk_point_set_coordinates");
   begin
      Internal (Get_Object (Point), X, Y);
   end Set_Coordinates;

end Gdk.Point;
