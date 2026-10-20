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

--  Performs measurements on paths such as determining the length of the path.
--
--  Many measuring operations require sampling the path length at intermediate
--  points. Therefore, a `GskPathMeasure` has a tolerance that determines what
--  precision is required for such approximations.
--
--  A `GskPathMeasure` struct is a reference counted struct and should be
--  treated as opaque.
--
--  <gtkada_demo>create_paths.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Gsk.Path;       use Gsk.Path;
with Gsk.Path_Point; use Gsk.Path_Point;
with Interfaces.C;   use Interfaces.C;

package Gsk.Path_Measure is

   type Gsk_Path_Measure is new Glib.C_Boxed with null record;
   Null_Gsk_Path_Measure : constant Gsk_Path_Measure;

   function From_Object (Object : System.Address) return Gsk_Path_Measure;
   function From_Object_Free (B : access Gsk_Path_Measure'Class) return Gsk_Path_Measure;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gsk_Path_Measure; Path : Gsk.Path.Gsk_Path);
   --  Creates a measure object for the given Path with the default tolerance.
   --  Since: gtk+ 4.14
   --  @param Path the path to measure

   function Gsk_Path_Measure_New
      (Path : Gsk.Path.Gsk_Path) return Gsk_Path_Measure;
   --  Creates a measure object for the given Path with the default tolerance.
   --  Since: gtk+ 4.14
   --  @param Path the path to measure

   procedure Gtk_New_With_Tolerance
      (Self      : out Gsk_Path_Measure;
       Path      : Gsk.Path.Gsk_Path;
       Tolerance : Interfaces.C.C_float);
   --  Creates a measure object for the given Path and Tolerance.
   --  Since: gtk+ 4.14
   --  @param Path the path to measure
   --  @param Tolerance the tolerance for measuring operations

   function Gsk_Path_Measure_New_With_Tolerance
      (Path      : Gsk.Path.Gsk_Path;
       Tolerance : Interfaces.C.C_float) return Gsk_Path_Measure;
   --  Creates a measure object for the given Path and Tolerance.
   --  Since: gtk+ 4.14
   --  @param Path the path to measure
   --  @param Tolerance the tolerance for measuring operations

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_path_measure_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Length (Self : Gsk_Path_Measure) return Interfaces.C.C_float;
   --  Gets the length of the path being measured.
   --  The length is cached, so this function does not do any work.
   --  Since: gtk+ 4.14
   --  @return the length of the path measured by Self

   function Get_Path (Self : Gsk_Path_Measure) return Gsk.Path.Gsk_Path;
   --  Returns the path that the measure was created for.
   --  Since: gtk+ 4.14
   --  @return the path of Self. Has transfer-ownership='none'.

   function Get_Point
      (Self     : Gsk_Path_Measure;
       Distance : Interfaces.C.C_float;
       Result   : not null access Gsk.Path_Point.Gsk_Path_Point)
       return Boolean;
   --  Gets the point at the given distance into the path.
   --  An empty path has no points, so false is returned in that case.
   --  Since: gtk+ 4.14
   --  @param Distance the distance
   --  @param Result return location for the point
   --  @return true if Result was set

   function Get_Tolerance
      (Self : Gsk_Path_Measure) return Interfaces.C.C_float;
   --  Returns the tolerance that the measure was created with.
   --  Since: gtk+ 4.14
   --  @return the tolerance of Self

   function Ref (Self : Gsk_Path_Measure) return Gsk_Path_Measure;
   --  Increases the reference count of a `GskPathMeasure` by one.
   --  Since: gtk+ 4.14
   --  @return the passed in `GskPathMeasure`. Has transfer-ownership='full'.

   procedure Unref (Self : Gsk_Path_Measure);
   --  Decreases the reference count of a `GskPathMeasure` by one.
   --  If the resulting reference count is zero, frees the object.
   --  Since: gtk+ 4.14

private
   Null_Gsk_Path_Measure : constant Gsk_Path_Measure :=
      (Glib.C_Boxed with null record);

end Gsk.Path_Measure;
