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

with Cairo;               use Cairo;
with Gdk.RGBA;            use Gdk.RGBA;
with Glib;                use Glib;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtkada.Canvas_View;  use Gtkada.Canvas_View;
with Gtkada.Style;        use Gtkada.Style;
with Pango.Font;          use Pango.Font;

package body Create_Canvas_View_Composite is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo illustrates various capabilities for compositing items"
        & " in the canvas_view widget."
        & ASCII.LF
        & "The first item (red background) shows that @bautomatic size@B"
        & " computation occurs for items, based on the size requested by"
        & " their children and their margin."
        & ASCII.LF
        & "The second item shows a toplevel item with three children."
        & " The first two of these children are @bfloating@B, so that the"
        & " third is laid out at the same vertical position, and thus on top"
        & " of them."
        & ASCII.LF
        & "The third item illustrates @balignment@B. The first three children"
        & " specify an explicit width, and therefore the alignment property"
        & " has a visible effect. The next three children do not specify"
        & " a width, and therefore end up with their parent's width, and the"
        & " alignment has no effect. All children have extra @bmargins@B,"
        & " which explains why there is an empty space to their left and"
        & " right.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Canvas       : Canvas_View;
      Model        : List_Canvas_Model;
      Scrolled     : Gtk_Scrolled_Window;
      Red, Green, Blue, Font : Drawing_Style;

      procedure Do_Example
        (Layout : Child_Layout_Strategy; X, Y : Model_Coordinate);

      procedure Do_Example
        (Layout : Child_Layout_Strategy; X, Y : Model_Coordinate)
      is
         M : Margins;
         W, H, W2, H2 : Model_Coordinate;
         Rect, Rect2  : Rect_Item;
         Text         : Text_Item;
         Anchor_X, Anchor_Y : Gdouble := 0.0;
      begin
         --  rectangle 1

         Rect := Gtk_New_Rect (Red, Radius => 4.0);
         Rect.Set_Child_Layout (Layout);
         Rect.Set_Position ((X, Y));
         Model.Add (Rect);

         case Layout is
            when Vertical_Stack =>
               M := (Top => 10.0, Bottom => 20.0, others => 0.0);
            when Horizontal_Stack =>
               M := (Left => 10.0, Right => 20.0, others => 0.0);
         end case;

         Rect2 := Gtk_New_Rect (Blue, 10.0, 10.0);
         Rect.Add_Child (Rect2, Margin => M);
         Rect2 := Gtk_New_Rect (Green, 10.0, 10.0);
         Rect.Add_Child (Rect2, Margin => M);

         --  rectangle 2: testing floating items

         Rect := Gtk_New_Rect (Green);
         Model.Add (Rect);
         Rect.Set_Child_Layout (Layout);
         Rect.Set_Position ((X + 90.0, Y));

         Rect2 := Gtk_New_Rect (Red, 20.0, 20.0);
         Rect.Add_Child (Rect2, Align => Align_End, Float => True,
                         Margin => (others => 10.0));

         Rect2 := Gtk_New_Rect (Red, 20.0, 20.0);
         Rect.Add_Child (Rect2, Float => True,
                         Margin => (others => 10.0));

         Rect2 := Gtk_New_Rect (Blue, Width => 60.0, Height => 60.0);
         Rect.Add_Child (Rect2, Margin => (others => 5.0));

         Text := Gtk_New_Text (Font, "float");
         Rect.Add_Child (Text);

         --  rectangle 3: testing alignments

         case Layout is
            when Horizontal_Stack =>
               M := (Top => 10.0, Bottom => 10.0, others => 0.0);
               W2 := -1.0;
               H2 := 100.0;
               W := 30.0;
               H := -1.0;
            when Vertical_Stack =>
               M := (Left => 10.0, Right => 10.0, others => 0.0);
               W2 := -1.0; --  100.0;
               H2 := -1.0;
               W := -1.0;
               H := 30.0;
         end case;

         Rect := Gtk_New_Rect (Blue, Width => W2, Height => H2);
         Model.Add (Rect);
         Rect.Set_Child_Layout (Layout);
         Rect.Set_Position ((X + 220.0, Y + 0.0));

         if Layout = Vertical_Stack then
            Text := Gtk_New_Text (Font, "Alignments (forced size)");
            Rect.Add_Child (Text);
         end if;

         Rect2 := Gtk_New_Rect (Red, 30.0, 30.0);
         Rect.Add_Child (Rect2, Align  => Align_Start, Margin => M);

         Rect2 := Gtk_New_Rect (Red, 30.0, 30.0);
         Rect.Add_Child (Rect2, Align  => Align_Center, Margin => M);

         Rect2 := Gtk_New_Rect (Red, 30.0, 30.0);
         Rect.Add_Child (Rect2, Align  => Align_End, Margin => M);

         if Layout = Vertical_Stack then
            Text := Gtk_New_Text (Font, "Alignments (automatic size)");
            Rect.Add_Child (Text);
         end if;

         Rect2 := Gtk_New_Rect (Green, W, H);
         Rect.Add_Child (Rect2, Align  => Align_Start, Margin => M);

         Rect2 := Gtk_New_Rect (Green, W, H);
         Rect.Add_Child (Rect2, Align  => Align_Center, Margin => M);

         Rect2 := Gtk_New_Rect (Green, W, H);
         Rect.Add_Child (Rect2, Align  => Align_End, Margin => M);

         --  Playing with the anchors. We do not set a position for the
         --  items, so this position is the one that is computed automatically
         --  by the layout (horizontal or vertical).

         if Layout = Vertical_Stack then
            Text := Gtk_New_Text (Font, "Anchors (positions)");
            Rect.Add_Child (Text);
         end if;

         Rect2 := Gtk_New_Rect (Red, 30.0, 30.0);
         Rect2.Set_Position (Anchor_X => Anchor_X, Anchor_Y => Anchor_Y);
         Rect.Add_Child (Rect2, Margin => M);

         if Layout = Vertical_Stack then
            Anchor_X := 0.5;
         else
            Anchor_Y := 0.5;
         end if;

         Rect2 := Gtk_New_Rect (Red, 30.0, 30.0);
         Rect2.Set_Position (Anchor_X => Anchor_X, Anchor_Y => Anchor_Y);
         Rect.Add_Child (Rect2, Margin => M);

         if Layout = Vertical_Stack then
            Anchor_X := 1.0;
         else
            Anchor_Y := 1.0;
         end if;

         Rect2 := Gtk_New_Rect (Red, 30.0, 30.0);
         Rect2.Set_Position (Anchor_X => Anchor_X, Anchor_Y => Anchor_Y);
         Rect.Add_Child (Rect2, Margin => M);
      end Do_Example;

   begin
      Red := Gtk_New
        (Stroke => Black_RGBA,
         Fill   => Create_Rgba_Pattern ((1.0, 0.0, 0.0, 0.6)));
      Green := Gtk_New
        (Stroke => Black_RGBA,
         Fill   => Create_Rgba_Pattern ((0.0, 1.0, 0.0, 0.6)));
      Blue := Gtk_New
        (Stroke => Black_RGBA,
         Fill   => Create_Rgba_Pattern ((0.0, 0.0, 1.0, 0.6)));
      Font := Gtk_New
        (Stroke => Null_RGBA,
         Font   => (Name => From_String ("sans 9"), others => <>));

      Gtk_New (Model);

      Do_Example (Vertical_Stack, 0.0, 0.0);
      Do_Example (Horizontal_Stack, 0.0, 350.0);

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

end Create_Canvas_View_Composite;
