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

with Cairo;   use Cairo;
with Glib;    use Glib;

package body Gtkada.Canvas_View.Views is

   ------------------
   -- Do_Snap_Grid --
   ------------------

   function Do_Snap_Grid
     (Grid_Size   : Model_Coordinate;
      Snap_Margin : Glib.Gint;
      Pos         : Model_Coordinate;
      Size        : Model_Coordinate) return Model_Coordinate
   is
      G : constant Gint := Gint (Grid_Size);
      X : constant Gint := Gint (Pos);
   begin
      if Snap_Margin = 0 then
         return Pos;
      elsif X mod G < Snap_Margin then
         return Gdouble (Gint (X / G) * G);
      elsif X mod G > G - Snap_Margin then
         return Gdouble (G + Gint (X / G) * G);
      elsif (X + Gint (Size)) mod G < Snap_Margin then
         return Gdouble (Gint ((X + Gint (Size)) / G) * G - Gint (Size));
      elsif (X + Gint (Size)) mod G > G - Snap_Margin then
         return Gdouble (G + Gint ((X + Gint (Size)) / G) * G - Gint (Size));
      end if;
      return Gdouble (X);
   end Do_Snap_Grid;

   ---------------------
   -- Draw_Grid_Lines --
   ---------------------

   procedure Draw_Grid_Lines
     (Style   : Gtkada.Style.Drawing_Style;
      Context : Draw_Context;
      Area    : Model_Rectangle;
      Size    : Model_Coordinate)
   is
      Tmp  : Gdouble;
   begin
      New_Path (Context.Cr);

      Tmp := Gdouble (Gint (Area.X / Size)) * Size;
      while Tmp < Area.X + Area.Width loop
         Move_To (Context.Cr, Tmp, Area.Y);
         Rel_Line_To (Context.Cr, 0.0, Area.Height);
         Tmp := Tmp + Size;
      end loop;

      Tmp := Gdouble (Gint (Area.Y / Size)) * Size;
      while Tmp < Area.Y + Area.Height loop
         Move_To (Context.Cr, Area.X, Tmp);
         Rel_Line_To (Context.Cr, Area.Width, 0.0);
         Tmp := Tmp + Size;
      end loop;

      Style.Finish_Path (Context.Cr);
   end Draw_Grid_Lines;

   --------------------
   -- Draw_Grid_Dots --
   --------------------

   procedure Draw_Grid_Dots
     (Style   : Gtkada.Style.Drawing_Style;
      Context : Draw_Context;
      Area    : Model_Rectangle;
      Size    : Model_Coordinate)
   is
      TmpX, TmpY  : Gdouble;
   begin
      New_Path (Context.Cr);

      TmpX := Gdouble (Gint (Area.X / Size)) * Size;
      while TmpX < Area.X + Area.Width loop
         TmpY := Gdouble (Gint (Area.Y / Size)) * Size;
         while TmpY < Area.Y + Area.Height loop
            Rectangle (Context.Cr, TmpX - 0.5, TmpY - 0.5, 1.0, 1.0);
            TmpY := TmpY + Size;
         end loop;

         TmpX := TmpX + Size;
      end loop;

      Style.Finish_Path (Context.Cr);
   end Draw_Grid_Dots;

end Gtkada.Canvas_View.Views;
