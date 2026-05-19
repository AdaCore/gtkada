------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2018, AdaCore                     --
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

with System;

package body Gdk.Rectangle is

   ---------------
   -- Intersect --
   ---------------

   procedure Intersect
     (Src1      : Gdk_Rectangle;
      Src2      : Gdk_Rectangle;
      Dest      : out Gdk_Rectangle;
      Intersect : out Boolean)
   is
      function Internal
        (Src1, Src2 : Gdk_Rectangle;
         Dest       : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_rectangle_intersect");

      Dest_Rec : aliased Gdk_Rectangle;

   begin
      Intersect := Internal (Src1, Src2, Dest_Rec'Address) /= 0;
      Dest := Dest_Rec;
   end Intersect;

end Gdk.Rectangle;
