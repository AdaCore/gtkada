------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

with Glib; use Glib;
with Glib.Error;
with Glib.Object; use Glib.Object;

with Gdk.Cairo;
with Gdk.Pixbuf; use Gdk.Pixbuf;

with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Drawing_Area;    use Gtk.Drawing_Area;
with Gtk.Print_Context;   use Gtk.Print_Context;
with Gtk.Print_Operation; use Gtk.Print_Operation;
with Gtk.Window;          use Gtk.Window;

with Gtkada.Printing;     use Gtkada.Printing;

with Cairo;         use Cairo;
with Cairo.Matrix;  use Cairo.Matrix;
with Cairo.Pattern; use Cairo.Pattern;
with Cairo.Image_Surface; use Cairo.Image_Surface;
with Cairo.Font_Options; use Cairo.Font_Options;
with Cairo.Png; use Cairo.Png;

with Pango.Cairo;  use Pango.Cairo;
with Pango.Layout; use Pango.Layout;
with Pango.Font;   use Pango.Font;

package body Testcairo_Drawing is

   Two_Pi : constant Gdouble := Gdouble (2.0 * Ada.Numerics.Pi);
   Background : Gdk_Pixbuf := null;

   package Gdouble_Numerics is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);
   use Gdouble_Numerics;

   type Cairo_Demo_Record is new Gtk_Box_Record with record
      Test : Test_Type;
   end record;
   type Cairo_Demo is access all Cairo_Demo_Record'Class;

   type Testcairo_Print_Operation_Record is new Gtkada_Print_Operation_Record
   with record
      Widget : Cairo_Demo;
   end record;
   type Testcairo_Print_Operation is access all
     Testcairo_Print_Operation_Record'Class;

   overriding procedure Draw_Page
     (Op          : access Testcairo_Print_Operation_Record;
      Context     : Gtk_Print_Context;
      Page_Number : Gint);
   --  Handler responsible for printing pages

   ---------------
   -- On_Redraw --
   ---------------

   function On_Redraw
      (Demo : access GObject_Record'Class;
       Cr   : Cairo_Context) return Boolean
   is
      Win : constant Cairo_Demo := Cairo_Demo (Demo);
      D, D2, D3     : Gdouble;
      M, M2, M3     : Cairo_Matrix_Access;
      P             : Cairo_Pattern;
      Opt           : Cairo_Font_Options;
      Image_Surface : Cairo_Surface;
      Status        : Cairo_Status;
      Idx           : Natural;
      Error         : Glib.Error.GError;
   begin
      case Win.Test is
         when Image =>
            if Background = null then
               Gdk_New_From_File (Background, "background.png", Error);
            end if;

            if Background /= null then
               Gdk.Cairo.Set_Source_Pixbuf (Cr, Background, 10.0, 10.0);
            end if;

            Cairo.Paint (Cr);

         when Rectangles =>
            for J in reverse 1 .. 10 loop
               D := Gdouble (J);
               Set_Source_Rgb (Cr, D / 10.0, 0.5 - D / 20.0, 0.0);
               Rectangle (Cr, 0.0, 0.0, D * 10.0, D * 10.0);
               Fill (Cr);
            end loop;

         when Transparency =>
            for J in 1 .. 5 loop
               D := Gdouble (J);

               --  Create a transparent color
               Set_Source_Rgba (Cr, 0.0, 0.0, 1.0, 1.0 - D / 5.0);

               --  Draw a disk
               Arc (Cr     => Cr,
                    Xc     => 100.0 + D * 60.0,
                    Yc     => 100.0,
                    Radius => 100.0,
                    Angle1 => 0.0,
                    Angle2 => Two_Pi);
               Fill (Cr);
            end loop;

         when Operators =>
            declare
               Layout : Pango_Layout;
               Desc   : Pango_Font_Description;
            begin
               Layout := Create_Pango_Layout (Win);
               Desc := Pango.Font.From_String ("Verdana 9");
               Set_Font_Description (Layout, Desc);

               for Op in Cairo_Operator'Range loop
                  Idx := Cairo_Operator'Pos (Op);
                  Cairo.Save (Cr);
                  Cairo.Translate
                    (Cr,
                     170.0 * Gdouble (Idx mod 5),
                     120.0 * Gdouble (Idx / 5));
                  Cairo.Push_Group (Cr);
                  Cairo.Rectangle (Cr, 0.0, 0.0, 120.0, 80.0);
                  Cairo.Set_Source_Rgba (Cr, 0.7, 0.0, 0.0, 0.8);
                  Cairo.Fill (Cr);

                  Cairo.Set_Operator (Cr, Op);

                  Cairo.Rectangle (Cr, 20.0, 10.0, 120.0, 80.0);
                  Cairo.Set_Source_Rgba (Cr, 0.0, 0.0, 0.9, 0.4);
                  Cairo.Fill (Cr);
                  P := Cairo.Pop_Group (Cr);
                  Cairo.Set_Source (Cr, P);
                  Cairo.Paint (Cr);
                  Destroy (P);

                  Set_Text (Layout, Cairo_Operator'Image (Op));
                  Cairo.Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
                  Cairo.Move_To (Cr, 0.0, 95.0);
                  Pango.Cairo.Show_Layout  (Cr, Layout);

                  Cairo.Restore (Cr);
               end loop;

               Unref (Layout);
               Free (Desc);
            end;

         when Matrix =>
            M := new Cairo_Matrix;
            M2 := new Cairo_Matrix;
            M3 := new Cairo_Matrix;

            for J in 1 .. 50 loop
               D := Gdouble (J - 1) / 50.0;

               --  Create a color
               Set_Source_Rgba (Cr, 0.0, 1.0 - D, D, 0.7);

               --  Create a rotation matrix
               Init_Rotate (M, Two_Pi * D);

               --  Create a translation matrix
               Init_Translate (M2, 400.0 * D + 50.0, 200.0 * D + 50.0);

               --  Create a scale matrix
               Init_Scale (M3, 1.0 - D, 1.0 - D);

               --  We want first to scale, then rotate...
               Multiply (M, M, M3);

               --  ...then translate.
               Multiply (M, M, M2);

               --  Reset the transformation matrix on CR...
               Identity_Matrix (Cr);

               --  ... then apply our scale + rotate + translate matrix
               Transform (Cr, M);

               --  Draw a rectangle
               Rectangle (Cr, -50.0, -50.0, 100.0, 100.0);
               Fill (Cr);
            end loop;

            Unchecked_Free (M);
            Unchecked_Free (M2);
            Unchecked_Free (M3);

         when Transformations =>
            for J in 1 .. 50 loop
               D := Gdouble (J - 1) / 50.0;

               --  Create a color
               Set_Source_Rgba (Cr, 0.0, 1.0 - D, D, 0.7);

               --  Reset the transformation matrix on CR...
               Identity_Matrix (Cr);

               --  ... then apply our scale + rotate + translate matrix
               Translate (Cr, 400.0 * D + 50.0, 200.0 * D + 50.0);
               Rotate (Cr, Two_Pi * D);
               Scale (Cr, 1.0 - D, 1.0 - D);

               --  Draw a rectangle
               Rectangle (Cr, -50.0, -50.0, 100.0, 100.0);
               Fill (Cr);
            end loop;

         when Paths =>
            New_Path (Cr);

            --  Draw a sinusoid
            Move_To (Cr, 2.0, 50.0);
            for J in 2 .. 40 loop
               D := Gdouble (J) / 20.0;
               Line_To (Cr, 300.0 * D, 50.0 + 50.0 * Sin (Two_Pi * D * 2.0));
            end loop;

            Set_Source_Rgb (Cr, 0.2, 0.0, 0.5);
            Stroke (Cr);

            --  Draw a sinusoid using curves to go from one point to the next
            Move_To (Cr, 2.0, 100.0);
            for J in 2 .. 40 loop
               D  := Gdouble (J - 1) / 20.0;
               D2 := (Gdouble (J) - 1.6) / 20.0;
               D3 := (Gdouble (J) - 1.3) / 20.0;

               Curve_To (Cr,
                         300.0 * D2,
                         100.0 + 50.0 * Sin (Two_Pi * D2 * 2.0),

                         300.0 * D3,
                         100.0 + 50.0 * Sin (Two_Pi * D3 * 2.0),

                         300.0 * D,

                         100.0 + 50.0 * Sin (Two_Pi * D * 2.0));
            end loop;

            Set_Source_Rgb (Cr, 0.5, 0.0, 0.2);
            Stroke (Cr);

            --  Draw a sinusoid using a dashed line
            Move_To (Cr, 2.0, 150.0);
            for J in 2 .. 40 loop
               D := Gdouble (J - 1) / 20.0;
               Line_To (Cr, 300.0 * D, 150.0 + 50.0 * Sin (Two_Pi * D * 2.0));
            end loop;

            Set_Source_Rgb (Cr, 0.5, 0.0, 0.5);
            Set_Dash (Cr, (1 => 15.0, 2 => 10.0, 3 => 2.0, 4 => 10.0), 0.1);
            Stroke (Cr);

            --  Draw a sinusoid using thick round-capped lines
            Move_To (Cr, 2.0, 200.0);
            for J in 1 .. 40 loop
               D := Gdouble (J - 1) / 20.0;
               Line_To (Cr, 300.0 * D, 200.0 + 50.0 * Sin (Two_Pi * D * 2.0));
            end loop;

            declare
               Dashes : Dash_Array_Access;
               Offset : Gdouble;
            begin
               Get_Dash (Cr, Dashes, Offset);
               Set_Dash (Cr, Dashes (1 .. 4), Offset);
            end;

            Set_Line_Width (Cr, 7.0);
            Set_Line_Cap (Cr, Cairo_Line_Cap_Round);
            Set_Source_Rgb (Cr, 0.5, 0.5, 1.0);
            Stroke (Cr);

            --  Draw a sinusoid using a thin line and no dashes
            Move_To (Cr, 2.0, 250.0);
            for J in 2 .. 40 loop
               D := Gdouble (J - 1) / 20.0;
               Line_To (Cr, 300.0 * D, 250.0 + 50.0 * Sin (Two_Pi * D * 2.0));
            end loop;

            Set_Line_Width (Cr, 1.0);
            Set_Line_Cap (Cr, Cairo_Line_Cap_Butt);
            Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
            Set_Dash (Cr, No_Dashes, 0.0);
            Stroke (Cr);

            --  Draw a sinusoid without antialiasing
            Move_To (Cr, 2.0, 300.0);
            for J in 2 .. 40 loop
               D := Gdouble (J - 1) / 20.0;
               Line_To (Cr, 300.0 * D, 300.0 + 50.0 * Sin (Two_Pi * D * 2.0));
            end loop;

            Set_Antialias (Cr, Cairo_Antialias_None);
            Stroke (Cr);

         when Patterns =>
            --  A solid-filled rectangle
            P := Create_Rgb (1.0, 1.0, 0.0);
            Set_Source (Cr, P);
            Rectangle (Cr, 10.0, 10.0, 100.0, 100.0);
            Fill (Cr);
            Destroy (P);

            --  A rectangle with a transparent solid fill
            P := Create_Rgba (0.0, 0.0, 1.0, 0.3);
            Set_Source (Cr, P);
            Rectangle (Cr, 5.0, 30.0, 100.0, 100.0);
            Fill (Cr);
            Destroy (P);

            --  A rectangle with a linear gradient
            P := Create_Linear (120.0, 10.0, 170.0, 60.0);
            Add_Color_Stop_Rgb (P, 0.0, 1.0, 1.0, 0.0);
            Add_Color_Stop_Rgb (P, 1.0, 0.0, 0.0, 1.0);
            Set_Source (Cr, P);
            Rectangle (Cr, 120.0, 10.0, 100.0, 100.0);
            Fill (Cr);
            Destroy (P);

            --  A rectangle with a linear transparent gradient
            P := Create_Rgb (1.0, 1.0, 0.0);
            Set_Source (Cr, P);
            Rectangle (Cr, 230.0, 10.0, 100.0, 100.0);
            Fill (Cr);
            Destroy (P);
            P := Create_Linear (275.0, 30.0, 225.0, 80.0);
            Add_Color_Stop_Rgba (P, 0.0, 0.0, 1.0, 0.0, 0.0);
            Add_Color_Stop_Rgba (P, 1.0, 0.0, 0.0, 1.0, 1.0);
            Set_Source (Cr, P);
            Rectangle (Cr, 225.0, 30.0, 100.0, 100.0);
            Fill (Cr);
            Destroy (P);

            --  A rectangle with a radial transparent gradient
            Set_Source_Rgb (Cr, 0.5, 0.0, 0.5);
            P := Create_Radial (365.0, 35.0, 10.0, 365.0, 35.0, 30.0);
            Add_Color_Stop_Rgba (P, 0.0, 0.0, 1.0, 0.0, 0.0);
            Add_Color_Stop_Rgba (P, 1.0, 0.0, 0.0, 1.0, 1.0);
            Set_Source (Cr, P);
            Rectangle (Cr, 340.0, 10.0, 100.0, 100.0);
            Fill (Cr);
            Destroy (P);

         when Toy_Text =>
            --  "Hello world" using two calls to Show_Text, taking advantage
            --  of the fact that one call to Show_Text places the current point
            --  after the first string
            Set_Source_Rgb (Cr, 0.0, 0.0, 1.0);
            Select_Font_Face
              (Cr, "courier",
               Cairo_Font_Slant_Normal,
               Cairo_Font_Weight_Normal);
            Set_Font_Size (Cr, 10.0);
            Move_To (Cr, 10.0, 10.0);
            Show_Text (Cr, "Hello");
            Show_Text (Cr, " World!");

            --  Bold and a bigger font
            Move_To (Cr, 20.0, 30.0);
            Select_Font_Face
              (Cr, "courier",
               Cairo_Font_Slant_Normal,
               Cairo_Font_Weight_Bold);
            Set_Font_Size (Cr, 20.0);
            Show_Text (Cr, "Bigger");

            --  Modify font options to remove anti-aliasing
            Move_To (Cr, 10.0, 100.0);
            Opt := Create;
            Get_Font_Options (Cr, Opt);
            Set_Antialias (Opt, Cairo_Antialias_None);
            Set_Font_Options (Cr, Opt);
            Show_Text (Cr, "No antialias");
            Set_Antialias (Opt, Cairo_Antialias_Default);
            Set_Font_Options (Cr, Opt);
            Destroy (Opt);

            Fill (Cr);

            --  Draw along the path of the text
            Set_Source_Rgb (Cr, 0.3, 0.0, 0.1);
            Set_Font_Size (Cr, 80.0);
            Move_To (Cr, 150.0, 200.0);
            Set_Dash (Cr, (1 => 2.0, 2 => 2.0), 0.0);
            Text_Path (Cr, "Text path");

            --  Print the stroke extents for this text
            declare
               X1, Y1, X2, Y2 : aliased Gdouble;
            begin
               Stroke_Extents (Cr, X1'Access, Y1'Access, X2'Access, Y2'Access);
               Put_Line ("Stroke extents:" &
                         Integer (X1)'Img & "," &
                         Integer (Y1)'Img & " - " &
                         Integer (X2)'Img & "," &
                         Integer (Y2)'Img);
            end;

            Stroke (Cr);

            --  Use matrix transforms on the text
            Move_To (Cr, 200.0, 100.0);
            Set_Source_Rgb (Cr, 0.5, 0.0, 1.0);

            M := new Cairo_Matrix;
            Init_Scale (M, 10.0, 40.0);
            Rotate (M, -0.1);
            Set_Font_Matrix (Cr, M);
            Show_Text (Cr, "text with matrix transforms");
            Unchecked_Free (M);

         when Pango_Text =>
            declare
               Layout : Pango_Layout;
               Desc   : Pango_Font_Description;
            begin
               Desc := From_String ("Verdana Medium Italic 15");
               Layout := Create_Pango_Layout
                 (Win, "Verdana Medium Italic 15");
               Set_Font_Description (Layout, Desc);
               Show_Layout (Cr, Layout);
               Unref (Layout);
               Free (Desc);

               Desc := From_String ("Helvetica 20");
               Move_To (Cr, 20.0, 20.0);
               Layout := Create_Pango_Layout (Win);
               Layout.Set_Markup
                 ("<b>bold</b> <i>italic</i> " &
                  "<span foreground=""#FF9900"">orange</span>");
               Set_Font_Description (Layout, Desc);
               Show_Layout (Cr, Layout);
               Unref (Layout);
               Free (Desc);
            end;

         when Clip_And_Paint =>
            --  Paint the background pink
            Set_Source_Rgb (Cr, 1.0, 0.9, 0.9);
            Paint (Cr);

            --  Draw a green rectangle
            Rectangle (Cr, 50.0, 50.0, 150.0, 150.0);
            Set_Source_Rgb (Cr, 0.0, 1.0, 0.0);
            Fill (Cr);

            --  Create a path
            Move_To (Cr, 10.0, 10.0);
            Rel_Line_To (Cr, 0.0, 100.0);
            Rel_Line_To (Cr, 100.0, 60.0);
            Rel_Line_To (Cr, 50.0, 0.0);
            Close_Path (Cr);

            --  Clip
            Clip (Cr);

            --  Paint the clipped region with a transparent blue.
            Set_Source_Rgb (Cr, 0.0, 0.0, 1.0);
            Paint_With_Alpha (Cr, 0.6);

         when Surface_And_Png =>
            Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
            Rectangle (Cr, 40.0, 40.0, 300.0, 200.0);
            Fill (Cr);

            declare
               Width  : constant := 60;
               Height : constant := 60;
               Data   : constant ARGB32_Array_Access := new ARGB32_Array
                 (1 .. Width * Height);

               Data2  : constant RGB24_Array_Access := new RGB24_Array
                 (1 .. Width * Height);

               Data3  : constant Byte_Array_Access := new Byte_Array
                 (1 .. Width * Height);

            begin
               --  Initialize some data
               for Line in 1 .. Height loop
                  for Col in 1 .. Width loop
                     Data ((Line - 1) * Width + Col) :=
                       (Alpha => 200,
                        Red   => Byte (Line * 4),
                        Green => Byte (Col * 4),
                        Blue  => 0);
                     Data2 ((Line - 1) * Width + Col) :=
                       (Red   => Byte (Line * 4),
                        Green => Byte (Col * 4),
                        Blue  => 0);

                     Data3 ((Line - 1) * Width + Col) := Byte (Line);
                  end loop;
               end loop;

               --  Manual "video inverse" in the middle of the surface

               for L in 10 .. 30 loop
                  for C in 30 .. 50 loop
                     Data (L * Width + C).Red := 255
                       - Data (L * Width + C).Red;
                     Data (L * Width + C).Green := 255
                       - Data (L * Width + C).Green;
                     Data (L * Width + C).Blue := 255
                       - Data (L * Width + C).Blue;
                  end loop;
               end loop;

               Image_Surface := Create_For_Data_ARGB32 (Data, Width, Height);
               Set_Source_Surface (Cr, Image_Surface, 10.0, 10.0);
               Paint (Cr);

               Image_Surface := Create_For_Data_RGB24 (Data2, Width, Height);
               Set_Source_Surface (Cr, Image_Surface, 75.0, 10.0);
               Paint (Cr);

               Image_Surface := Create_For_Data_A8 (Data3, Width, Height);
               Set_Source_Surface (Cr, Image_Surface, 140.0, 10.0);
               Paint (Cr);

               Put ("Writing to PNG ... ");
               Status := Write_To_Png (Image_Surface, "try.png");
               Put_Line (Status'Img);
            end;
      end case;

      return False;
   end On_Redraw;

   ---------------
   -- Draw_Page --
   ---------------

   procedure Draw_Page
     (Op          : access Testcairo_Print_Operation_Record;
      Context     : Gtk_Print_Context;
      Page_Number : Gint)
   is
      Cr  : Cairo_Context;
      Tmp : Boolean;
      pragma Unreferenced (Tmp, Page_Number);
   begin
      Cr := Get_Cairo_Context (Context);
      Tmp := On_Redraw (Op.Widget, Cr);
   end Draw_Page;

   -----------------
   -- On_Print_Cb --
   -----------------

   procedure On_Print_Cb (Demo : access GObject_Record'Class) is
      D : constant Cairo_Demo := Cairo_Demo (Demo);
      Print_Op : Testcairo_Print_Operation;
      Result   : Gtk_Print_Operation_Result;
      pragma Unreferenced (Result);
   begin
      Print_Op := new Testcairo_Print_Operation_Record;
      Print_Op.Widget := D;
      Gtkada.Printing.Initialize (Print_Op);

      Print_Op.Set_N_Pages (1);

      --  Call up the print operation dialog
      Result := Connect_And_Run
         (Print_Op, Action_Print_Dialog, Gtk_Window (Get_Toplevel (D)));
   end On_Print_Cb;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test
      (Frame : access Gtk_Frame_Record'Class;
       Test  : Test_Type)
   is
      Box    : Cairo_Demo;
      Area   : Gtk_Drawing_Area;
      Button : Gtk_Button;
   begin
      Frame.Set_Label ("Cairo demo -- " & Test'Img);

      Box := new Cairo_Demo_Record;
      Box.Test := Test;
      Initialize_Vbox (Box, Homogeneous => False);
      Frame.Add (Box);

      Gtk_New (Area);
      Box.Pack_Start (Area, Expand => True, Fill => True);
      Area.On_Draw (On_Redraw'Access, Box);

      Gtk_New (Button, "Print");
      Box.Pack_Start (Button, Expand => False);
      Button.On_Clicked (On_Print_Cb'Access, Box);

      Frame.Show_All;
   end Run_Test;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk_Frame_Record'Class) is
   begin
      Run_Test (Frame, Test);
   end Run;
end Testcairo_Drawing;
