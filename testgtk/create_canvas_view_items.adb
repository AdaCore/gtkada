------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2014, AdaCore                          --
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

with Gdk.RGBA;            use Gdk.RGBA;
with Glib;                use Glib;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtkada.Canvas_View;  use Gtkada.Canvas_View;
with Gtkada.Style;        use Gtkada.Style;

package body Create_Canvas_View_Items is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo uses the various types of predefined item types";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      S : constant Gdouble := 0.86602540378443864676;  --  sqrt(3) / 2
      L : Gdouble;

      Canvas       : Canvas_View;
      Model        : List_Canvas_Model;
      Scrolled     : Gtk_Scrolled_Window;
      Black        : Drawing_Style;
      It           : Polyline_Item;
      Hexa         : Polyline_Item;
      Rect         : Rect_Item;

   begin
      Black := Gtk_New (Stroke => Black_RGBA);

      Gtk_New (Model);

      --  A drawing of a hexagone.

      L := 30.0;
      Hexa := Gtk_New_Polyline
        (Black,
         ((2.0 * L, L * S),
          (1.5 * L, L * S * 2.0),
          (0.5 * L, L * S * 2.0),
          (0.0,     L * S),
          (0.5 * L, 0.0),
          (1.5 * L, 0.0)),
         Close => True);
      Hexa.Set_Position ((0.0, 0.0));
      Model.Add (Hexa);

      --  A drawing of a UML actor

      Rect := Gtk_New_Rect (Black);
      Rect.Set_Position ((100.0, 0.0));
      Model.Add (Rect);

      It := Gtk_New_Polyline
        (Black,
         ((15.0, 1.0), (35.0, 1.0),
          (35.0, 21.0), (15.0, 21.0)),
         Close => True);
      It.Set_Position ((0.0, 1.0));
      Rect.Add_Child (It);

      It := Gtk_New_Polyline (Black, ((0.0, 31.0), (50.0, 31.0)));
      It.Set_Position ((0.0, 1.0));
      Rect.Add_Child (It);

      It := Gtk_New_Polyline (Black, ((25.0, 21.0), (25.0, 41.0)));
      It.Set_Position ((0.0, 1.0));
      Rect.Add_Child (It);

      It := Gtk_New_Polyline (Black, ((25.0, 41.0), (0.0, 61.0)));
      It.Set_Position ((0.0, 1.0));
      Rect.Add_Child (It);

      It := Gtk_New_Polyline (Black, ((25.0, 41.0), (50.0, 61.0)));
      It.Set_Position ((0.0, 1.0));
      Rect.Add_Child (It);

      --  Need to compute all coordinates

      Model.Refresh_Layout;

      --  Create the view once the model is populated, to avoid a refresh
      --  every time a new item is added.

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Frame.Add (Scrolled);

      Gtk_New (Canvas, Model);
      Unref (Model);
      Scrolled.Add (Canvas);

      Frame.Show_All;
   end Run;

end Create_Canvas_View_Items;
