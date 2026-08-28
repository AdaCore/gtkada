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

--  A 3D box, described as the volume between a minimum and a maximum
--  vertices.

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Graphene.Point3d; use Graphene.Point3d;
limited with Graphene.Sphere;
with Graphene.Vec3;    use Graphene.Vec3;
with Interfaces.C;     use Interfaces.C;

package Graphene.Box is

   type Graphene_Box_T is record
      Min : Graphene.Vec3.Graphene_Vec3_T;
      Max : Graphene.Vec3.Graphene_Vec3_T;
   end record;
   pragma Convention (C, Graphene_Box_T);

   function From_Object_Free
     (B : not null access Graphene_Box_T) return Graphene_Box_T;
   pragma Inline (From_Object_Free);
   --  A 3D box, described as the volume between a minimum and a maximum
   --  vertices.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "graphene_box_get_type");

   -------------
   -- Methods --
   -------------

   function Contains_Box
      (Self : not null access Graphene_Box_T;
       B    : not null access Graphene_Box_T) return Boolean;
   --  Checks whether the Graphene.Box.Graphene_Box_T A contains the given
   --  Graphene.Box.Graphene_Box_T B.
   --  Since: gtk+ 1.2
   --  @param B a Graphene.Box.Graphene_Box_T
   --  @return `true` if the box is contained in the given box

   function Contains_Point
      (Self  : not null access Graphene_Box_T;
       Point : not null access Graphene.Point3d.Graphene_Point3D_T)
       return Boolean;
   --  Checks whether Box contains the given Point.
   --  Since: gtk+ 1.2
   --  @param Point the coordinates to check
   --  @return `true` if the point is contained in the given box

   function Equal
      (Self : not null access Graphene_Box_T;
       B    : not null access Graphene_Box_T) return Boolean;
   --  Checks whether the two given boxes are equal.
   --  Since: gtk+ 1.2
   --  @param B a Graphene.Box.Graphene_Box_T
   --  @return `true` if the boxes are equal

   procedure Expand
      (Self  : not null access Graphene_Box_T;
       Point : not null access Graphene.Point3d.Graphene_Point3D_T;
       Res   : not null access Graphene_Box_T);
   pragma Import (C, Expand, "graphene_box_expand");
   --  Expands the dimensions of Box to include the coordinates at Point.
   --  Since: gtk+ 1.2
   --  @param Point the coordinates of the point to include
   --  @param Res return location for the expanded box

   procedure Expand_Scalar
      (Self   : not null access Graphene_Box_T;
       Scalar : Interfaces.C.C_float;
       Res    : not null access Graphene_Box_T);
   pragma Import (C, Expand_Scalar, "graphene_box_expand_scalar");
   --  Expands the dimensions of Box by the given Scalar value.
   --  If Scalar is positive, the Graphene.Box.Graphene_Box_T will grow; if
   --  Scalar is negative, the Graphene.Box.Graphene_Box_T will shrink.
   --  Since: gtk+ 1.2
   --  @param Scalar a scalar value
   --  @param Res return location for the expanded box

   procedure Expand_Vec3
      (Self : not null access Graphene_Box_T;
       Vec  : not null access Graphene.Vec3.Graphene_Vec3_T;
       Res  : not null access Graphene_Box_T);
   pragma Import (C, Expand_Vec3, "graphene_box_expand_vec3");
   --  Expands the dimensions of Box to include the coordinates of the given
   --  vector.
   --  Since: gtk+ 1.2
   --  @param Vec the coordinates of the point to include, as a
   --  Graphene.Vec3.Graphene_Vec3_T
   --  @param Res return location for the expanded box

   procedure Free (Self : not null access Graphene_Box_T);
   pragma Import (C, Free, "graphene_box_free");
   --  Frees the resources allocated by graphene_box_alloc.
   --  Since: gtk+ 1.2

   procedure Get_Bounding_Sphere
      (Self   : not null access Graphene_Box_T;
       Sphere : not null access Graphene.Sphere.Graphene_Sphere_T);
   pragma Import (C, Get_Bounding_Sphere, "graphene_box_get_bounding_sphere");
   --  Computes the bounding Graphene.Sphere.Graphene_Sphere_T capable of
   --  containing the given Graphene.Box.Graphene_Box_T.
   --  Since: gtk+ 1.2
   --  @param Sphere return location for the bounding sphere

   procedure Get_Center
      (Self   : not null access Graphene_Box_T;
       Center : not null access Graphene.Point3d.Graphene_Point3D_T);
   pragma Import (C, Get_Center, "graphene_box_get_center");
   --  Retrieves the coordinates of the center of a
   --  Graphene.Box.Graphene_Box_T.
   --  Since: gtk+ 1.2
   --  @param Center return location for the coordinates of the center

   function Get_Depth
      (Self : not null access Graphene_Box_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Depth, "graphene_box_get_depth");
   --  Retrieves the size of the Box on the Z axis.
   --  Since: gtk+ 1.2
   --  @return the depth of the box

   function Get_Height
      (Self : not null access Graphene_Box_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Height, "graphene_box_get_height");
   --  Retrieves the size of the Box on the Y axis.
   --  Since: gtk+ 1.2
   --  @return the height of the box

   procedure Get_Max
      (Self : not null access Graphene_Box_T;
       Max  : not null access Graphene.Point3d.Graphene_Point3D_T);
   pragma Import (C, Get_Max, "graphene_box_get_max");
   --  Retrieves the coordinates of the maximum point of the given
   --  Graphene.Box.Graphene_Box_T.
   --  Since: gtk+ 1.2
   --  @param Max return location for the maximum point

   procedure Get_Min
      (Self : not null access Graphene_Box_T;
       Min  : not null access Graphene.Point3d.Graphene_Point3D_T);
   pragma Import (C, Get_Min, "graphene_box_get_min");
   --  Retrieves the coordinates of the minimum point of the given
   --  Graphene.Box.Graphene_Box_T.
   --  Since: gtk+ 1.2
   --  @param Min return location for the minimum point

   procedure Get_Size
      (Self : not null access Graphene_Box_T;
       Size : not null access Graphene.Vec3.Graphene_Vec3_T);
   pragma Import (C, Get_Size, "graphene_box_get_size");
   --  Retrieves the size of the box on all three axes, and stores it into the
   --  given Size vector.
   --  Since: gtk+ 1.2
   --  @param Size return location for the size

   function Get_Width
      (Self : not null access Graphene_Box_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Width, "graphene_box_get_width");
   --  Retrieves the size of the Box on the X axis.
   --  Since: gtk+ 1.2
   --  @return the width of the box

   function Init
      (Self : not null access Graphene_Box_T;
       Min  : access Graphene.Point3d.Graphene_Point3D_T;
       Max  : access Graphene.Point3d.Graphene_Point3D_T)
       return access Graphene_Box_T;
   pragma Import (C, Init, "graphene_box_init");
   --  Initializes the given Graphene.Box.Graphene_Box_T with two vertices.
   --  Since: gtk+ 1.2
   --  @param Min the coordinates of the minimum vertex
   --  @param Max the coordinates of the maximum vertex
   --  @return the initialized Graphene.Box.Graphene_Box_T

   function Init_From_Box
      (Self : not null access Graphene_Box_T;
       Src  : not null access Graphene_Box_T) return access Graphene_Box_T;
   pragma Import (C, Init_From_Box, "graphene_box_init_from_box");
   --  Initializes the given Graphene.Box.Graphene_Box_T with the vertices of
   --  another Graphene.Box.Graphene_Box_T.
   --  Since: gtk+ 1.2
   --  @param Src a Graphene.Box.Graphene_Box_T
   --  @return the initialized Graphene.Box.Graphene_Box_T

   function Init_From_Vec3
      (Self : not null access Graphene_Box_T;
       Min  : access Graphene.Vec3.Graphene_Vec3_T;
       Max  : access Graphene.Vec3.Graphene_Vec3_T)
       return access Graphene_Box_T;
   pragma Import (C, Init_From_Vec3, "graphene_box_init_from_vec3");
   --  Initializes the given Graphene.Box.Graphene_Box_T with two vertices
   --  stored inside Graphene.Vec3.Graphene_Vec3_T.
   --  Since: gtk+ 1.2
   --  @param Min the coordinates of the minimum vertex
   --  @param Max the coordinates of the maximum vertex
   --  @return the initialized Graphene.Box.Graphene_Box_T

   function Intersection
      (Self : not null access Graphene_Box_T;
       B    : not null access Graphene_Box_T;
       Res  : access Graphene_Box_T := null) return Boolean;
   --  Intersects the two given Graphene.Box.Graphene_Box_T.
   --  If the two boxes do not intersect, Res will contain a degenerate box
   --  initialized with Graphene.Box.Empty.
   --  Since: gtk+ 1.2
   --  @param B a Graphene.Box.Graphene_Box_T
   --  @param Res return location for the result
   --  @return true if the two boxes intersect

   procedure Union
      (Self : not null access Graphene_Box_T;
       B    : not null access Graphene_Box_T;
       Res  : not null access Graphene_Box_T);
   pragma Import (C, Union, "graphene_box_union");
   --  Unions the two given Graphene.Box.Graphene_Box_T.
   --  Since: gtk+ 1.2
   --  @param B the box to union to A
   --  @param Res return location for the result

   ----------------------
   -- GtkAda additions --
   ----------------------

   type Graphene_Vec3_Array8 is array (1 .. 8) of Graphene_Vec3_T;
   pragma Convention (C, Graphene_Vec3_Array8);

   procedure Get_Vertices
     (Self     : not null access Graphene_Box_T;
      Vertices : out Graphene_Vec3_Array8);
   --  Computes the vertices of the given Graphene.Box.Graphene_Box_T.
   --  Since: gtk+ 1.2
   --  @param Vertices return location for an array of 8
   --  Graphene.Vec3.Graphene_Vec3_T

   function Init_From_Points
     (Self   : not null access Graphene_Box_T;
      Points : Graphene.Point3d.Graphene_Point3d_Array) return access Graphene_Box_T;
   --  Initializes the given Graphene.Box.Graphene_Box_T with the given array
   --  of vertices.
   --  If Points'Length is 0, the returned box is initialized with
   --  Graphene.Box.Empty.
   --  Since: gtk+ 1.2
   --  @param Points an array of Graphene.Point3d.Graphene_Point3D_T
   --  @return the initialized Graphene.Box.Graphene_Box_T

   function Init_From_Vectors
     (Self    : not null access Graphene_Box_T;
      Vectors : Graphene.Vec3.Graphene_Vec3_Array) return access Graphene_Box_T;
   --  Initializes the given Graphene.Box.Graphene_Box_T with the given array
   --  of vertices.
   --  If Vectors'Length is 0, the returned box is initialized with
   --  Graphene.Box.Empty.
   --  Since: gtk+ 1.2
   --  @param Vectors an array of Graphene.Vec3.Graphene_Vec3_T
   --  @return the initialized Graphene.Box.Graphene_Box_T

   ---------------
   -- Functions --
   ---------------

   function Empty return access constant Graphene_Box_T;
   pragma Import (C, Empty, "graphene_box_empty");
   --  A degenerate Graphene.Box.Graphene_Box_T that can only be expanded.
   --  The returned value is owned by Graphene and should not be modified or
   --  freed.
   --  Since: gtk+ 1.2
   --  @return a Graphene.Box.Graphene_Box_T

   function Infinite return access constant Graphene_Box_T;
   pragma Import (C, Infinite, "graphene_box_infinite");
   --  A degenerate Graphene.Box.Graphene_Box_T that cannot be expanded.
   --  The returned value is owned by Graphene and should not be modified or
   --  freed.
   --  Since: gtk+ 1.2
   --  @return a Graphene.Box.Graphene_Box_T

   function Minus_One return access constant Graphene_Box_T;
   pragma Import (C, Minus_One, "graphene_box_minus_one");
   --  A Graphene.Box.Graphene_Box_T with the minimum vertex set at (-1, -1,
   --  -1) and the maximum vertex set at (0, 0, 0).
   --  The returned value is owned by Graphene and should not be modified or
   --  freed.
   --  Since: gtk+ 1.2
   --  @return a Graphene.Box.Graphene_Box_T

   function One return access constant Graphene_Box_T;
   pragma Import (C, One, "graphene_box_one");
   --  A Graphene.Box.Graphene_Box_T with the minimum vertex set at (0, 0, 0)
   --  and the maximum vertex set at (1, 1, 1).
   --  The returned value is owned by Graphene and should not be modified or
   --  freed.
   --  Since: gtk+ 1.2
   --  @return a Graphene.Box.Graphene_Box_T

   function One_Minus_One return access constant Graphene_Box_T;
   pragma Import (C, One_Minus_One, "graphene_box_one_minus_one");
   --  A Graphene.Box.Graphene_Box_T with the minimum vertex set at (-1, -1,
   --  -1) and the maximum vertex set at (1, 1, 1).
   --  The returned value is owned by Graphene and should not be modified or
   --  freed.
   --  Since: gtk+ 1.2
   --  @return a Graphene.Box.Graphene_Box_T

   function Zero return access constant Graphene_Box_T;
   pragma Import (C, Zero, "graphene_box_zero");
   --  A Graphene.Box.Graphene_Box_T with both the minimum and maximum
   --  vertices set at (0, 0, 0).
   --  The returned value is owned by Graphene and should not be modified or
   --  freed.
   --  Since: gtk+ 1.2
   --  @return a Graphene.Box.Graphene_Box_T

end Graphene.Box;
