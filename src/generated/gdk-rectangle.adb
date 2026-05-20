------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");

package body Gdk.Rectangle is

   function From_Object_Free (B : access Gdk_Rectangle) return Gdk_Rectangle is
      Result : constant Gdk_Rectangle := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   --------------------
   -- Contains_Point --
   --------------------

   function Contains_Point
      (Self : Gdk_Rectangle;
       X    : Glib.Gint;
       Y    : Glib.Gint) return Boolean
   is
      function Internal
         (Self : Gdk_Rectangle;
          X    : Glib.Gint;
          Y    : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_rectangle_contains_point");
   begin
      return Internal (Self, X, Y) /= 0;
   end Contains_Point;

   -----------
   -- Equal --
   -----------

   function Equal
      (Self  : Gdk_Rectangle;
       Rect2 : Gdk_Rectangle) return Boolean
   is
      function Internal
         (Self  : Gdk_Rectangle;
          Rect2 : Gdk_Rectangle) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_rectangle_equal");
   begin
      return Internal (Self, Rect2) /= 0;
   end Equal;

   ---------------
   -- Intersect --
   ---------------

   procedure Intersect
      (Self         : Gdk_Rectangle;
       Src2         : Gdk_Rectangle;
       Dest         : out Gdk_Rectangle;
       Do_Intersect : out Boolean)
   is
      function Internal
         (Self     : Gdk_Rectangle;
          Src2     : Gdk_Rectangle;
          Acc_Dest : access Gdk_Rectangle) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_rectangle_intersect");
      Acc_Dest   : aliased Gdk_Rectangle;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Src2, Acc_Dest'Access);
      Dest := Acc_Dest;
      Do_Intersect := Tmp_Return /= 0;
   end Intersect;

end Gdk.Rectangle;
