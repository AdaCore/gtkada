------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2011-2014, AdaCore                     --
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

package body Cairo.Region is

   -----------
   -- Equal --
   -----------

   function "=" (A, B : Cairo_Region) return Boolean is
      function Internal (A, B : Cairo_Region) return Gboolean;
      pragma Import (C, Internal, "cairo_region_equal");
   begin
      return Internal (A, B) /= 0;
   end "=";

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Region : Cairo_Region) return Boolean is
      function Internal (Region : Cairo_Region) return Gboolean;
      pragma Import (C, Internal, "cairo_region_is_empty");
   begin
      return Internal (Region) /= 0;
   end Is_Empty;

   --------------------
   -- Contains_Point --
   --------------------

   function Contains_Point
     (Region : Cairo_Region;
      X      : Gint;
      Y      : Gint) return Boolean
   is
      function Internal (R : Cairo_Region; X, Y : Gint) return Gboolean;
      pragma Import (C, Internal, "cairo_region_contains_point");
   begin
      return Internal (Region, X, Y) /= 0;
   end Contains_Point;

end Cairo.Region;
