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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

with Ada.Text_IO; use Ada.Text_IO;

with Glib; use Glib;
with Glib.Object; use Glib.Object;

with Cairo;         use Cairo;
with Cairo.Matrix;  use Cairo.Matrix;
with Cairo.Pattern; use Cairo.Pattern;
with Cairo.Image_Surface; use Cairo.Image_Surface;
with Cairo.Font_Options; use Cairo.Font_Options;
with Cairo.Png; use Cairo.Png;

with Pango.Cairo;  use Pango.Cairo;
with Pango.Layout; use Pango.Layout;
with Pango.Font;   use Pango.Font;

with Gdk.Cairo;    use Gdk.Cairo;
with Gdk.Event;    use Gdk.Event;
with Gdk.Window;   use Gdk.Window;

with Gtk.Main;
with Gtk.Box;           use Gtk.Box;
with Gtk.Drawing_Area;  use Gtk.Drawing_Area;
with Gtk.Label;         use Gtk.Label;

with Gtk.Window;   use Gtk.Window;
with Gtk.Handlers; use Gtk.Handlers;

with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Tree_View;          use Gtk.Tree_View;
with Gtk.Tree_Selection;     use Gtk.Tree_Selection;
with Gtk.Tree_Store;         use Gtk.Tree_Store;
with Gtk.Tree_View_Column;   use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Widget; use Gtk.Widget;

procedure Testcairo is

   --  The tests implemented in this example program

   type Test_Type is (Rectangles, Transparency, Matrix, Transformations,
                      Paths, Patterns, Clip_And_Paint,
                      Surface_And_Png, Toy_Text, Pango_Text);

   function Pretty (T : Test_Type) return String;
   --  Pretty print T

   package Gdouble_Numerics is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);
   use Gdouble_Numerics;

   --  Pi : constant Gdouble := Gdouble (Ada.Numerics.Pi);
   Two_Pi : constant Gdouble := Gdouble (2.0 * Ada.Numerics.Pi);

   Win  : Gtk_Window;
   Area : Gtk_Drawing_Area;
   Test : Test_Type := Test_Type'First;

   package Event_Cb is new Gtk.Handlers.Return_Callback
     (Gtk_Drawing_Area_Record, Boolean);

   package Selection_Cb is new Gtk.Handlers.Callback
     (Gtk_Tree_Selection_Record);

   function Expose_Cb (Area  : access Gtk_Drawing_Area_Record'Class;
                       Event : Gdk_Event) return Boolean;
   --  Callback on an expose event on Win

   type Doc_Array is array (Test_Type) of Unbounded_String;

   function "-"
     (S : String) return Unbounded_String
      renames To_Unbounded_String;

   Docs : constant Doc_Array :=
     (Rectangles      => -"Simple rectangles",
      Transparency    => -"Transparency",
        Matrix        =>
      -"Translating, rotating and scaling using matrix transformations",
      Transformations => -"Direct transformations",
      Paths           => -"Paths",
      Patterns        => -"Patterns",
      Toy_Text        => -"The Cairo 'toy' text API",
      Pango_Text      => -"Rendering sophisticated text using Pango",
      Clip_And_Paint  => -"Painting and clipping",
      Surface_And_Png => -"Using surfaces and saving to PNG");

   ---------------
   -- Expose_Cb --
   ---------------

   function Expose_Cb (Area  : access Gtk_Drawing_Area_Record'Class;
                       Event : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);
      Cr : Cairo_Context;
      D, D2, D3 : Gdouble;
      M, M2, M3 : Cairo_Matrix_Access;

      P   : Cairo_Pattern;
      Opt : Cairo_Font_Options;

      Image_Surface : Cairo_Surface;
      Status : Cairo_Status;

   begin
      Cr := Create (Get_Window (Area));

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
               Status := Cairo_Surface_Write_To_Png (Image_Surface, "try.png");
               Put_Line (Status'Img);
            end;
      end case;

      Destroy (Cr);
      return False;
   end Expose_Cb;

   Tree    : Gtk_Tree_View;
   Model   : Gtk_Tree_Store;
   Col     : Gtk_Tree_View_Column;
   Rend    : Gtk_Cell_Renderer_Text;
   Col_Num : Gint;
   Iter    : Gtk_Tree_Iter;
   Label   : Gtk_Label;

   Box     : Gtk_Box;
   Vbox    : Gtk_Box;

   pragma Unreferenced (Col_Num);

   ------------
   -- Pretty --
   ------------

   function Pretty (T : Test_Type) return String is
      Capitalize : Boolean := True;
      Img        : String := Test_Type'Image (T);
   begin
      for J in Img'Range loop
         if not Capitalize then
            if Img (J) = '_' then
               Img (J) := ' ';
               Capitalize := True;
            else
               Img (J) := To_Lower (Img (J));
            end if;
         else
            Capitalize := False;
         end if;
      end loop;
      return Img;
   end Pretty;

   procedure S_Changed
     (Widget : access Gtk_Tree_Selection_Record'Class);

   ---------------
   -- S_Changed --
   ---------------

   procedure S_Changed
     (Widget : access Gtk_Tree_Selection_Record'Class)
   is
      pragma Unreferenced (Widget);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path;
      Ind   : Gint_Array (0 .. 0);
      W, H  : Gint;
   begin
      Get_Selected (Get_Selection (Tree), Model, Iter);
      Path := Get_Path (Model, Iter);
      Ind := Get_Indices (Path);
      Test := Test_Type'Val (Ind (0));
      Path_Free (Path);

      W := Area.Get_Allocation_Width;
      H := Area.Get_Allocation_Height;

      Invalidate_Rect (Get_Window (Area),
                       (0, 0, W, H), Invalidate_Children => False);

      Set_Text (Label, To_String (Docs (Test)));
   end S_Changed;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Win);
   Set_Default_Size (Win, 960, 400);

   --  Create the tree and model
   Gtk_New (Model, (0 => GType_String));
   Gtk_New (Tree, Model);
   Tree.Set_Headers_Visible (False);
   Tree.Set_Show_Expanders (False);
   Gtk_New (Rend);

   Gtk_New (Col);
   Pack_Start (Col, Rend, False);
   Add_Attribute (Col, Rend, "text", 0);
   Col_Num := Tree.Append_Column (Col);

   for T in Test_Type loop
      Append (Model, Iter, Null_Iter);
      Set (Model, Iter, 0, Pretty (T));
   end loop;

   Selection_Cb.Connect
     (Get_Selection (Tree),
      Gtk.Tree_Selection.Signal_Changed,
      S_Changed'Access,
      After => True);

   Gtk_New_Hbox (Box);
   Win.Add (Box);
   Box.Pack_Start (Tree, False, False, 3);

   Gtk_New (Area);

   Gtk_New_Vbox (Vbox);
   Vbox.Pack_Start (Area, True, True, 3);

   Gtk_New (Label);
   Vbox.Pack_Start (Label, False, False, 3);

   Box.Pack_Start (Vbox, True, True, 3);

   --  Connect to the "expose" event.

   Event_Cb.Connect (Area, "expose_event",
                     Event_Cb.To_Marshaller (Expose_Cb'Access));

   Show_All (Win);
   Gtk.Main.Main;
end Testcairo;
