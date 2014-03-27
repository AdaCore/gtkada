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

package body Create_Canvas_View_Links is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo illustrates various capabilities for links"
        & " in the canvas view widget."
        & ASCII.LF
        & "Links can be attached to any other items (boxes, but also other"
        & " links and texts)."
        & ASCII.LF
        & "Links can also have arrows and annotations, as illustrated by"
        & " the examples here.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Canvas        : Canvas_View;
      Model         : List_Canvas_Model;
      Scrolled      : Gtk_Scrolled_Window;
      Black, Red    : Drawing_Style;

      procedure Do_Example (Routing : Route_Style; X, Y : Model_Coordinate);
      procedure Do_Example (Routing : Route_Style; X, Y : Model_Coordinate) is
         L1, L2        : Canvas_Link;
         It1, It2, It3 : Rect_Item;
      begin
         It1 := Gtk_New_Rect (Black, 20.0, 20.0);
         It1.Set_Position ((X, Y));
         Model.Add (It1);

         It2 := Gtk_New_Rect (Black, 20.0, 20.0);
         It2.Set_Position ((X + 100.0, Y + 100.0));
         Model.Add (It2);

         It3 := Gtk_New_Rect (Black, 20.0, 20.0);
         It3.Set_Position ((X + 100.0, Y + 0.0));
         Model.Add (It3);

         L1 := Gtk_New
           (From => It1, To => It2, Style => Black,
            Routing => Routing,
            Anchor_From => (1.0, 0.5, Auto),
            Anchor_To   => (0.0, 0.9, Auto));
         Model.Add (L1);

         L2 := Gtk_New
           (From => L1, To => It3, Style => Red, Routing => Routing,
            Anchor_From => (0.5, 0.5, No_Clipping));
         Model.Add (L2);
      end Do_Example;

      procedure Link_Example (X, Y : Model_Coordinate; Style : Drawing_Style);
      procedure Link_Example (X, Y : Model_Coordinate; Style : Drawing_Style)
      is
         It1, It2 : Rect_Item;
         L        : Canvas_Link;
      begin
         It1 := Gtk_New_Rect (Black, 20.0, 20.0);
         It1.Set_Position ((X, Y));
         Model.Add (It1);

         It2 := Gtk_New_Rect (Black, 20.0, 20.0);
         It2.Set_Position ((X + 200.0, Y));
         Model.Add (It2);

         L := Gtk_New
           (From => It1, To => It2, Style => Style, Routing => Straight);
         Model.Add (L);
      end Link_Example;

   begin
      Gtk_New (Model);

      Black := Gtk_New;
      Red   := Gtk_New (Stroke => (1.0, 0.0, 0.0, 1.0));

      Do_Example (Straight,   0.0, 0.0);
      Do_Example (Orthogonal, 150.0, 0.0);

      Link_Example (0.0, 180.0,
                    Gtk_New (Line_Width => 2.0,
                             Arrow_To   => (Head   => Open,
                                            Length => 8.0,
                                            Angle  => 0.4,
                                            Stroke => Black_RGBA,
                                            Fill   => Black_RGBA),
                             Arrow_From => (Head   => Open,
                                            Length => 16.0,
                                            Angle  => 0.8,
                                            Stroke => (1.0, 0.0, 0.0, 1.0),
                                            Fill   => (1.0, 0.0, 0.0, 1.0))));

      Link_Example (0.0, 220.0,
                    Gtk_New (Line_Width => 4.0,
                             Dashes     => (2.0, 2.0),
                             Arrow_To   => (Head   => Solid,
                                            Length => 8.0,
                                            Angle  => 0.4,
                                            Stroke => Black_RGBA,
                                            Fill   => Black_RGBA),
                             Arrow_From => (Head   => Solid,
                                            Length => 16.0,
                                            Angle  => 0.8,
                                            Stroke => (1.0, 0.0, 0.0, 1.0),
                                            Fill   => (1.0, 0.0, 0.0, 1.0))));

      Link_Example (0.0, 260.0,
                    Gtk_New (Line_Width => 6.0,
                             Dashes     => (2.0, 4.0, 4.0, 6.0),
                             Arrow_To   => (Head   => Diamond,
                                            Length => 8.0,
                                            Angle  => 0.4,
                                            Stroke => Black_RGBA,
                                            Fill   => Black_RGBA),
                             Arrow_From => (Head   => Diamond,
                                            Length => 16.0,
                                            Angle  => 0.8,
                                            Stroke => (1.0, 0.0, 0.0, 1.0),
                                            Fill   => (1.0, 0.0, 0.0, 1.0))));

      Link_Example (0.0, 300.0,
                    Gtk_New (Line_Width => 2.0,
                             Symbol_To  => (Name   => Cross,
                                            others => <>),
                             Symbol_From => (Name => Strike,
                                             others => <>)));

      Link_Example (0.0, 340.0,
                    Gtk_New (Line_Width => 2.0,
                             Symbol_To  => (Name   => Double_Strike,
                                            others => <>)));

      Model.Refresh_Layout;

      --  Create the view once the model is populated, to avoid a refresh
      --  every time a new item is added.

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Always, Policy_Always);
      Frame.Add (Scrolled);

      Gtk_New (Canvas, Model);
      Unref (Model);
      Scrolled.Add (Canvas);

      Frame.Show_All;
   end Run;

end Create_Canvas_View_Links;
