-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

pragma Ada_2005;

with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

with Glib; use Glib;

with Cairo;         use Cairo;
with Cairo.Matrix;  use Cairo.Matrix;
with Cairo.Pattern; use Cairo.Pattern;
with Cairo.Font_Options; use Cairo.Font_Options;

with Gdk.Cairo;    use Gdk.Cairo;

with Gdk.Event;    use Gdk.Event;
with Gtk.Main;
with Gtk.Window;   use Gtk.Window;
with Gtk.Handlers; use Gtk.Handlers;

procedure Testcairo is

   --  The tests implemented in this example program

   type Test_Type is (Rectangles, Transparency, Matrix, Transformations,
                      Paths, Patterns, Toy_Text, Clip_And_Paint);

   package Gdouble_Numerics is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);
   use Gdouble_Numerics;

   --  Pi : constant Gdouble := Gdouble (Ada.Numerics.Pi);
   Two_Pi : constant Gdouble := Gdouble (2.0 * Ada.Numerics.Pi);

   Win : Gtk_Window;

   package Event_Cb is new Gtk.Handlers.Return_Callback
     (Gtk_Window_Record, Boolean);

   function Expose_Cb (Win : access Gtk_Window_Record'Class;
                       Event : Gdk_Event) return Boolean;

   ---------------
   -- Expose_Cb --
   ---------------

   function Expose_Cb (Win : access Gtk_Window_Record'Class;
                       Event : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);
      Cr : Cairo_Context;
      D, D2, D3 : Gdouble;
      M, M2, M3 : Cairo_Matrix_Access;

      P   : Cairo_Pattern;
      Opt : access Cairo_Font_Options;

      Test : constant Test_Type := Test_Type'Last;
   begin
      Cr := Create (Get_Window (Win));

      case Test is
         when Rectangles =>
            for J in reverse 1 .. 10 loop
               D := Gdouble (J);
               --  Create a color
               Set_Source_Rgb (Cr, D / 10.0, 0.5 - D / 20.0, 0.0);

               --  Draw a rectangle
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
            Rectangle (Cr, 10.0, 10.0, 50.0, 50.0);
            Fill (Cr);
            Destroy (P);

            --  A rectangle with a transparent solid fill
            P := Create_Rgba (0.0, 0.0, 1.0, 0.3);
            Set_Source (Cr, P);
            Rectangle (Cr, 5.0, 30.0, 50.0, 50.0);
            Fill (Cr);
            Destroy (P);

            --  A rectangle with a linear gradient
            P := Create_Linear (70.0, 10.0, 120.0, 60.0);
            Add_Color_Stop_Rgb (P, 0.0, 1.0, 1.0, 0.0);
            Add_Color_Stop_Rgb (P, 1.0, 0.0, 0.0, 1.0);
            Set_Source (Cr, P);
            Rectangle (Cr, 70.0, 10.0, 50.0, 50.0);
            Fill (Cr);
            Destroy (P);

            --  A rectangle with a linear transparent gradient
            P := Create_Rgb (1.0, 1.0, 0.0);
            Set_Source (Cr, P);
            Rectangle (Cr, 130.0, 10.0, 50.0, 50.0);
            Fill (Cr);
            Destroy (P);
            P := Create_Linear (175.0, 30.0, 125.0, 80.0);
            Add_Color_Stop_Rgba (P, 0.0, 0.0, 1.0, 0.0, 0.0);
            Add_Color_Stop_Rgba (P, 1.0, 0.0, 0.0, 1.0, 1.0);
            Set_Source (Cr, P);
            Rectangle (Cr, 125.0, 30.0, 50.0, 50.0);
            Fill (Cr);
            Destroy (P);

            --  A rectangle with a radial transparent gradient
            Set_Source_Rgb (Cr, 0.5, 0.0, 0.5);
            P := Create_Radial (215.0, 35.0, 10.0, 215.0, 35.0, 30.0);
            Add_Color_Stop_Rgba (P, 0.0, 0.0, 1.0, 0.0, 0.0);
            Add_Color_Stop_Rgba (P, 1.0, 0.0, 0.0, 1.0, 1.0);
            Set_Source (Cr, P);
            Rectangle (Cr, 190.0, 10.0, 50.0, 50.0);
            Fill (Cr);
            Destroy (P);

         when Toy_Text =>
            Set_Source_Rgb (Cr, 0.0, 0.0, 1.0);
            Select_Font_Face
              (Cr, "courier",
               Cairo_Font_Slant_Normal,
               Cairo_Font_Weight_Normal);
            Set_Font_Size (Cr, 10.0);
            Move_To (Cr, 10.0, 10.0);
            Show_Text (Cr, "Hello");
            Show_Text (Cr, " World!");

            Move_To (Cr, 20.0, 30.0);
            Select_Font_Face
              (Cr, "courier",
               Cairo_Font_Slant_Normal,
               Cairo_Font_Weight_Bold);
            Set_Font_Size (Cr, 20.0);
            Show_Text (Cr, "Bigger");

            Move_To (Cr, 10.0, 100.0);

            Opt := new Cairo_Font_Options;
            Get_Font_Options (Cr, Opt);
            Set_Antialias (Opt, Cairo_Antialias_None);
            Set_Font_Options (Cr, Opt);
            Show_Text (Cr, "No antialias");

            Set_Antialias (Opt, Cairo_Antialias_Default);
            Set_Font_Options (Cr, Opt);

            Fill (Cr);

            Set_Source_Rgb (Cr, 0.3, 0.0, 0.1);
            Set_Font_Size (Cr, 80.0);
            Move_To (Cr, 150.0, 200.0);
            Set_Dash (Cr, (1 => 2.0, 2 => 2.0), 0.0);
            Text_Path (Cr, "Text path");
            Stroke (Cr);

            Move_To (Cr, 200.0, 100.0);
            Set_Source_Rgb (Cr, 0.5, 0.0, 1.0);

            M := new Cairo_Matrix;
            Init_Scale (M, 10.0, 40.0);
            Rotate (M, -0.1);
            Set_Font_Matrix (Cr, M);
            Show_Text (Cr, "text with matrix transforms");
            Unchecked_Free (M);

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
            Line_To (Cr, 10.0, 100.0);
            Line_To (Cr, 100.0, 160.0);
            Line_To (Cr, 100.0, 10.0);
            Close_Path (Cr);

            --  Clip
            Clip (Cr);

            --  Paint the clipped region with a transparent blue.
            Set_Source_Rgb (Cr, 0.0, 0.0, 1.0);
            Paint_With_Alpha (Cr, 0.6);
      end case;

      Destroy (Cr);
      return True;
   end Expose_Cb;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Win);
   Set_Default_Size (Win, 600, 400);

   --  Connect to the "expose" event.

   Event_Cb.Connect (Win, "expose_event",
                     Event_Cb.To_Marshaller (Expose_Cb'Access));

   Show_All (Win);
   Gtk.Main.Main;
end Testcairo;
