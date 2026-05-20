------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
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

with Ada.Numerics.Generic_Elementary_Functions; use Ada.Numerics;
with Ada.Text_IO;         use Ada.Text_IO;
with Cairo.Pattern;       use Cairo, Cairo.Pattern;
with Gdk.Pixbuf;          use Gdk.Pixbuf;
with Gdk.RGBA;            use Gdk.RGBA;
with Glib;                use Glib;
with Glib.Error;          use Glib.Error;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtkada.Canvas_View;  use Gtkada.Canvas_View;
with Gtkada.Canvas_View.Views; use Gtkada.Canvas_View.Views;
with Gtkada.Style;        use Gtkada.Style;
with Pango.Enums;         use Pango.Enums;
with Pango.Font;          use Pango.Font;

package body Create_Canvas_View_Items is

   package Gdouble_Elementary_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Gdouble);
   use Gdouble_Elementary_Functions;

   Left_Pointing_Double_Angle_Quotation_Mark : constant String :=
     Character'Val (16#C2#) & Character'Val (16#AB#);  --  unicode \u00ab

   Right_Pointing_Double_Angle_Quotation_Mark : constant String :=
     Character'Val (16#C2#) & Character'Val (16#BB#);  --  unicode \u00bb

   N_Ary_Summation : constant String :=
     Character'Val (16#E2#)   --  unicode \u2211
     & Character'Val (16#88#)
     & Character'Val (16#91#);

   type Polygon_Item_Record is new Polyline_Item_Record with null record;
   type Polygon_Item is access all Polygon_Item_Record'Class;
   function Gtk_New_Polygon
     (Style  : Gtkada.Style.Drawing_Style;
      Sides  : Positive;
      Radius : Gdouble) return Polygon_Item;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo uses the various types of predefined item types."
        & ASCII.LF
        & "It shows how they can be combined to create complex items."
        & ASCII.LF
        & "It also shows an example of using @bsloppy@B drawing. In this case"
        & " GtkAda does not draw straight lines, but displays them as if they"
        & " had been drawn manually.";
   end Help;

   ---------------------
   -- Gtk_New_Polygon --
   ---------------------

   function Gtk_New_Polygon
     (Style  : Gtkada.Style.Drawing_Style;
      Sides  : Positive;
      Radius : Gdouble) return Polygon_Item
   is
      R : constant Polygon_Item := new Polygon_Item_Record;
      Points : Point_Array (0 .. Sides - 1);
   begin
      for P in Points'Range loop
         Points (P) :=
           (Radius + Radius * Cos (2.0 * Pi * Gdouble (P) / Gdouble (Sides)),
            Radius + Radius * Sin (2.0 * Pi * Gdouble (P) / Gdouble (Sides)));
      end loop;
      Initialize_Polyline (R, Style, Points, Close => True);
      return R;
   end Gtk_New_Polygon;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Canvas        : Canvas_View;
      Model         : List_Canvas_Model;

      procedure Add_Sum_Block
        (Sloppy : Boolean := False; X, Y : Model_Coordinate);
      procedure Add_UML_Block
        (Sloppy : Boolean := False; X, Y : Model_Coordinate);
      procedure Add_UML_Actor
        (Sloppy : Boolean := False; X, Y : Model_Coordinate);
      procedure Add_Alignment (X, Y : Model_Coordinate);
      --  Add various blocks

      function Font_Name (Sloppy : Boolean) return String;
      --  Return the font to use

      function Font_Name (Sloppy : Boolean) return String is
      begin
         if Sloppy then
            return "Comic Sans MS";
         else
            return "Sans";
         end if;
      end Font_Name;

      procedure Add_Sum_Block
        (Sloppy : Boolean := False; X, Y : Model_Coordinate)
      is
         Filled, Black_Filled : Drawing_Style;
         Small_Font, Large_Font : Drawing_Style;
         It   : Polyline_Item;
         Rect : Rect_Item;
         Text : Text_Item;
      begin
         Filled := Gtk_New
           (Stroke => Black_RGBA,
            Sloppy => Sloppy,
            Fill   => Create_Rgba_Pattern ((0.0, 0.0, 1.0, 0.2)));
         Black_Filled := Gtk_New
           (Stroke => Black_RGBA,
            Sloppy => Sloppy,
            Fill   => Create_Rgba_Pattern (Black_RGBA));
         Small_Font := Gtk_New
           (Stroke => Null_RGBA,
            Sloppy => Sloppy,
            Font   => (Name   => From_String (Font_Name (Sloppy) & " 8"),
                       others => <>));
         Large_Font := Gtk_New
           (Stroke => Null_RGBA,
            Sloppy => Sloppy,
            Font   => (Name   => From_String (Font_Name (Sloppy) & " 16"),
                       others => <>));

         It := Gtk_New_Polyline
           (Filled, ((3.0, 0.0), (80.0, 40.0), (3.0, 80.0)));
         It.Set_Position ((X, Y));
         Model.Add (It);

         Rect := Gtk_New_Rect (Black_Filled, 6.0, 6.0);
         Rect.Set_Position ((0.0, 17.0));
         It.Add_Child (Rect);

         Text := Gtk_New_Text (Small_Font, "+");
         Text.Set_Position ((9.0, 20.0), Anchor_X => 0.0, Anchor_Y => 0.5);
         It.Add_Child (Text, Float => True);

         Rect := Gtk_New_Rect (Black_Filled, 6.0, 6.0);
         Rect.Set_Position ((0.0, 57.0));
         It.Add_Child (Rect);

         Text := Gtk_New_Text (Small_Font, "-");
         Text.Set_Position ((9.0, 60.0), Anchor_X => 0.0, Anchor_Y => 0.5);
         It.Add_Child (Text, Float => True);

         Text := Gtk_New_Text (Large_Font, N_Ary_Summation);
         Text.Set_Position ((15.0, 40.0), Anchor_X => 0.0, Anchor_Y => 0.5);
         It.Add_Child (Text, Float => True);
      end Add_Sum_Block;

      procedure Add_UML_Block
        (Sloppy : Boolean := False; X, Y : Model_Coordinate)
      is
         Title_Font, Font, Black, Filled : Drawing_Style;
         M    : Margins;
         Rect : Rect_Item;
         Text : Text_Item;
         Hr   : Hr_Item;
         Pattern : Cairo_Pattern;
      begin
         Pattern := Create_Linear (0.0, 0.0, 0.0, 1.0);
         Add_Color_Stop_Rgb (Pattern, 0.0, 1.0, 206.0 / 255.0, 154.0 / 255.0);
         Add_Color_Stop_Rgb (Pattern, 1.0, 1.0, 1.0, 216.0 / 255.0);

         Filled := Gtk_New
           (Stroke => Black_RGBA,
            Sloppy => Sloppy,
            Fill   => Pattern);

         Black := Gtk_New (Stroke => Black_RGBA, Sloppy => Sloppy);
         Title_Font := Gtk_New
           (Stroke => Null_RGBA,
            Sloppy => Sloppy,
            Font   => (Name   => From_String (Font_Name (Sloppy) & " 16"),
                       others => <>));
         Font := Gtk_New
           (Stroke => Null_RGBA,
            Sloppy => Sloppy,
            Font   => (Name   => From_String (Font_Name (Sloppy) & " 10"),
                       others => <>));

         M := (Left | Right => 5.0, Top => 0.0, Bottom => 2.0);

         Rect := Gtk_New_Rect (Filled);
         Rect.Set_Position ((X, Y));
         Model.Add (Rect);

         Text := Gtk_New_Text (Title_Font, "MyClass");
         Rect.Add_Child (Text, Margin => M);

         Text := Gtk_New_Text
           (Font,
            Left_Pointing_Double_Angle_Quotation_Mark & "custom"
            & Right_Pointing_Double_Angle_Quotation_Mark);
         Rect.Add_Child (Text, Margin => M);

         Hr := Gtk_New_Hr (Black, "attributes");
         Rect.Add_Child (Hr);

         Text := Gtk_New_Text (Font, "-id {readOnly}");
         Rect.Add_Child (Text, Margin => M);

         Text := Gtk_New_Text (Font, "-/name: string = 'foo'");
         Rect.Add_Child (Text, Margin => M);

         Text := Gtk_New_Text (Font, "overflowed_very_long_name");
         Rect.Add_Child (Text, Overflow => Overflow_Hide, Margin => M);
      end Add_UML_Block;

      procedure Add_UML_Actor
        (Sloppy : Boolean := False; X, Y : Model_Coordinate)
      is
         Invisible, Filled, Black : Drawing_Style;
         Ellipse : Ellipse_Item;
         Rect    : Rect_Item;
         It      : Polyline_Item;
      begin
         Black := Gtk_New (Stroke => Black_RGBA, Sloppy => Sloppy);
         Invisible := Gtk_New (Stroke => Null_RGBA, Sloppy => Sloppy);
         Filled := Gtk_New
           (Stroke => Black_RGBA,
            Sloppy => Sloppy,
            Fill   => Create_Rgba_Pattern ((0.0, 0.0, 1.0, 0.2)));

         Rect := Gtk_New_Rect (Invisible);
         Rect.Set_Position ((X, Y));
         Model.Add (Rect);

         Ellipse := Gtk_New_Ellipse (Filled, 20.0, 20.0);
         Ellipse.Set_Position ((15.0, 1.0));
         Rect.Add_Child (Ellipse);

         It := Gtk_New_Polyline
           (Black, ((0.0, 31.0), (50.0, 0.0)), Relative => True);
         It.Set_Position ((0.0, 1.0));
         Rect.Add_Child (It);

         It := Gtk_New_Polyline (Black, ((25.0, 21.0), (25.0, 41.0)));
         It.Set_Position ((0.0, 1.0));
         Rect.Add_Child (It);

         It := Gtk_New_Polyline
           (Black, ((25.0, 41.0), (-25.0, 20.0)), Relative => True);
         It.Set_Position ((0.0, 1.0));
         Rect.Add_Child (It);

         It := Gtk_New_Polyline (Black, ((25.0, 41.0), (50.0, 61.0)));
         It.Set_Position ((0.0, 1.0));
         Rect.Add_Child (It);
      end Add_UML_Actor;

      -------------------
      -- Add_Alignment --
      -------------------

      procedure Add_Alignment (X, Y : Model_Coordinate) is
         Font, Black : Drawing_Style;
         Text : Text_Item;
         It   : Rect_Item;
      begin
         Font := Gtk_New
           (Stroke => Null_RGBA,
            Font   => (Name => From_String ("sans 10"), others => <>));
         Black := Gtk_New;

         Text := Gtk_New_Text
           (Font, "Position of items can be the center of the item" & ASCII.LF
            & "The three boxes have the same position, but not the same"
            & " anchors");
         Text.Set_Position ((X + 150.0, Y + 80.0));
         Model.Add (Text);

         It := Gtk_New_Rect (Black, Width => 40.0, Height => 40.0);
         It.Set_Position
           ((X + 50.0, Y + 40.0), Anchor_X => 0.0, Anchor_Y => 0.0);
         Model.Add (It);

         It := Gtk_New_Rect (Black, Width => 40.0, Height => 40.0);
         It.Set_Position
           ((X + 50.0, Y + 80.0), Anchor_X => 0.5, Anchor_Y => 0.0);
         Model.Add (It);

         It := Gtk_New_Rect (Black, Width => 40.0, Height => 40.0);
         It.Set_Position
           ((X + 50.0, Y + 120.0), Anchor_X => 1.0, Anchor_Y => 0.0);
         Model.Add (It);
      end Add_Alignment;

      Scrolled      : Gtk_Scrolled_Window;
      Filled, Font  : Drawing_Style;
      Polygon       : Polygon_Item;
      Ellipse       : Ellipse_Item;
      Text          : Text_Item;
      Pattern       : Cairo_Pattern;
      Image         : Image_Item;
      Pixbuf        : Gdk_Pixbuf;
      Error         : GError;
      Rect1, Rect2  : Rect_Item;

   begin
      Font := Gtk_New
        (Stroke => Null_RGBA,
         Font   => (Name   => From_String ("sans 10"),
                    Color  => Black_RGBA,
                    Underline => Pango_Underline_Double,
                    others => <>));
      Filled := Gtk_New
        (Stroke => Black_RGBA,
         Fill => Create_Rgba_Pattern ((0.0, 0.0, 1.0, 0.2)));

      Gtk_New (Model);

      --  A drawing of a hexagone.

      Polygon := Gtk_New_Polygon (Filled, Sides => 6, Radius => 30.0);
      Polygon.Set_Position ((0.0, 0.0));
      Model.Add (Polygon);
      Text := Gtk_New_Text (Font, "hexa");
      Text.Set_Position ((12.0, 15.0));
      Polygon.Add_Child (Text);

      Polygon := Gtk_New_Polygon (Filled, Sides => 10, Radius => 50.0);
      Polygon.Set_Position ((100.0, 0.0));
      Model.Add (Polygon);

      --  A simple ellipse

      Pattern := Create_Radial
        (Cx0 => 0.5, Cy0 => 0.5, Radius0 => 0.0,
         Cx1 => 0.5, Cy1 => 0.5, Radius1 => 1.0);
      Add_Color_Stop_Rgb (Pattern, 0.0, 1.0, 206.0 / 255.0, 154.0 / 255.0);
      Add_Color_Stop_Rgb (Pattern, 1.0, 1.0, 1.0, 216.0 / 255.0);

      Filled := Gtk_New
        (Stroke => Black_RGBA,
         Fill   => Pattern);
      Ellipse := Gtk_New_Ellipse (Filled, 60.0, 30.0);
      Ellipse.Set_Position ((200.0, 0.0));
      Model.Add (Ellipse);

      --  Images

      Rect1 := Gtk_New_Rect (Gtk_New (Stroke => (0.0, 1.0, 0.0, 1.0)));
      Rect1.Set_Position ((300.0, 0.0));
      Model.Add (Rect1);

      Rect2 := Gtk_New_Rect (Gtk_New (Stroke => (0.0, 1.0, 0.0, 1.0)));
      Rect2.Set_Position ((400.0, 0.0));
      Model.Add (Rect2);

      Gdk_New_From_File (Pixbuf, "refresh.svg", Error);
      if Error /= null then
         Put_Line (Get_Message (Error));
         Error_Free (Error);
      else
         Image := Gtk_New_Image
           (Style => Gtk_New (Stroke => (0.0, 0.0, 0.0, 1.0)),
            Image => Pixbuf);
         Rect1.Add_Child (Image);

         Image := Gtk_New_Image
           (Style  => Gtk_New (Stroke => (0.0, 0.0, 0.0, 1.0)),
            Image  => Pixbuf,
            Allow_Rescale => False,
            Width  => 50.0,
            Height => 50.0);
         Rect2.Add_Child (Image);
      end if;

      Gdk_New_From_File (Pixbuf, "search_and_menu.png", Error);
      if Error /= null then
         Put_Line (Get_Message (Error));
         Error_Free (Error);
      else
         Image := Gtk_New_Image
           (Style => Gtk_New (Stroke => (0.0, 0.0, 0.0, 1.0)),
            Image => Pixbuf);
         Rect1.Add_Child (Image);

         Image := Gtk_New_Image
           (Style  => Gtk_New (Stroke => (0.0, 0.0, 0.0, 1.0)),
            Image  => Pixbuf,
            Width  => 50.0,
            Height => 50.0);
         Rect2.Add_Child (Image);
      end if;

      Add_UML_Actor (False, 0.0,   120.0);
      Add_UML_Block (False, 100.0, 120.0);
      Add_Sum_Block (False, 380.0, 120.0);

      Add_UML_Actor (True,  0.0,   320.0);
      Add_UML_Block (True,  100.0, 320.0);
      Add_Sum_Block (True,  380.0, 320.0);

      Add_Alignment (0.0, 490.0);

      --  Create the view once the model is populated, to avoid a refresh
      --  every time a new item is added.

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Frame.Add (Scrolled);

      Gtk_New (Canvas, Model);
      Canvas.On_Item_Event (On_Item_Event_Move_Item'Access);
      Unref (Model);
      Scrolled.Add (Canvas);

      Canvas.Scale_To_Fit;

      Frame.Show_All;
   end Run;

end Create_Canvas_View_Items;
