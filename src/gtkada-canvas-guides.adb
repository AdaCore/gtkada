------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2014, AdaCore                     --
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

package body Gtkada.Canvas.Guides is

   ------------------
   -- Do_Snap_Grid --
   ------------------

   function Do_Snap_Grid
     (Canvas : not null access Interactive_Canvas_Record'Class;
      Pos    : Gdouble;
      Size   : Gdouble) return Gdouble
   is
      G : constant Gint := Gint (Canvas.Grid_Size);
      X : constant Gint := Gint (Pos);
   begin
      if Canvas.Snap_Margin = 0 then
         return Pos;
      elsif X mod G < Canvas.Snap_Margin then
         return Gdouble (Gint (X / G) * G);
      elsif X mod G > G - Canvas.Snap_Margin then
         return Gdouble (G + Gint (X / G) * G);
      elsif (X + Gint (Size)) mod G < Canvas.Snap_Margin then
         return Gdouble (Gint ((X + Gint (Size)) / G) * G - Gint (Size));
      elsif (X + Gint (Size)) mod G > G - Canvas.Snap_Margin then
         return Gdouble (G + Gint ((X + Gint (Size)) / G) * G - Gint (Size));
      end if;
      return Gdouble (X);
   end Do_Snap_Grid;

end Gtkada.Canvas.Guides;
