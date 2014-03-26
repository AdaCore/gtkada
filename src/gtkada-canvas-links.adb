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

with Ada.Numerics.Generic_Elementary_Functions; use Ada.Numerics;
with Ada.Unchecked_Deallocation;
with Cairo.Region;                  use Cairo.Region;
with Gdk.Cairo;                     use Gdk.Cairo;
with Glib.Graphs;                   use Glib.Graphs;
with Gtkada.Canvas.Guides;          use Gtkada.Canvas.Guides;
with Gtkada.Style;                  use Gtkada.Style;
with Pango.Cairo;                   use Pango.Cairo;
with Pango.Layout;                  use Pango.Layout;

package body Gtkada.Canvas.Links is

   package Gdouble_Elementary_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Gdouble);
   use Gdouble_Elementary_Functions;

   procedure Compute_Anchors
     (Self  : not null access Canvas_Link_Record'Class);
   --  Compute the start and end point of the link, depending on the two
   --  objects it links. This makes sure that the link does not start in the
   --  middle of the box.

   function Compute_Line_Pos
     (Canvas : access Interactive_Canvas_Record'Class) return Gdouble_Array;
   --  ???

   procedure Draw_Annotation
     (Canvas   : access Interactive_Canvas_Record'Class;
      Cr       : Cairo_Context;
      X_Canvas : Gdouble;
      Y_Canvas : Gdouble;
      Link     : access Canvas_Link_Record'Class);
   --  Print an annotation on the canvas.
   --  The annotation is centered around (X, Y), in pixels. These coordinates
   --  should already include zoom processing.

   ---------------------
   -- Compute_Anchors --
   ---------------------

   procedure Compute_Anchors
     (Self  : not null access Canvas_Link_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      null;
   end Compute_Anchors;

   ---------------------
   -- Draw_Annotation --
   ---------------------

   procedure Draw_Annotation
     (Canvas   : access Interactive_Canvas_Record'Class;
      Cr       : Cairo_Context;
      X_Canvas : Gdouble;
      Y_Canvas : Gdouble;
      Link     : access Canvas_Link_Record'Class)
   is
      W, H : Gint;
   begin
      if Link.Descr /= null
        and then Link.Descr.all /= ""
        and then Canvas.Annotation_Layout /= null
      then
         Set_Text (Canvas.Annotation_Layout, Link.Descr.all);
         Get_Pixel_Size (Canvas.Annotation_Layout, W, H);

         Cairo.Save (Cr);
         Gdk.Cairo.Set_Source_RGBA (Cr, (0.0, 0.0, 0.0, 0.0));
         Cairo.Set_Line_Width (Cr, 1.0);
         Cairo.Rectangle
           (Cr,
            X_Canvas - 0.5,
            Y_Canvas - 0.5,
            Gdouble (W) + 1.0,
            Gdouble (H) + 1.0);
         Cairo.Fill (Cr);
         Cairo.Restore (Cr);

         Cairo.Move_To (Cr, X_Canvas, Y_Canvas);
         Pango.Cairo.Show_Layout (Cr, Canvas.Annotation_Layout);
      end if;
   end Draw_Annotation;

   ----------------------
   -- Compute_Line_Pos --
   ----------------------

   function Compute_Line_Pos
     (Canvas : access Interactive_Canvas_Record'Class) return Gdouble_Array
   is
      type Graph_Range is record
         From, To : Gdouble;
      end record;

      type Range_Array is array (Positive range <>) of Graph_Range;
      type Range_Array_Access is access all Range_Array;

      procedure Free is new Ada.Unchecked_Deallocation
        (Range_Array, Range_Array_Access);

      Free_Ranges : Range_Array_Access := new Range_Array (1 .. 1000);
      Tmp         : Range_Array_Access;
      Last_Range  : Positive := Free_Ranges'First;
      Iter        : Vertex_Iterator := First (Canvas.Children);
      E           : Canvas_Item;
      Right       : Gdouble;
   begin
      Free_Ranges (Free_Ranges'First) :=
        (From => Gdouble'First, To => Gdouble'Last);

      while not At_End (Iter) loop
         E := Canvas_Item (Get (Iter));
         Right := E.Coord.X + E.Coord.Width;

         for R in Free_Ranges'First .. Last_Range loop
            if Free_Ranges (R).From <= E.Coord.X
              and then Free_Ranges (R).To >= E.Coord.X
              and then Free_Ranges (R).To <= Right
            then
               Free_Ranges (R) :=
                 (From => Free_Ranges (R).From, To => E.Coord.X - 1.0);

            elsif Free_Ranges (R).From <= E.Coord.X
              and then Free_Ranges (R).To >= Right
            then
               if Last_Range >= Free_Ranges'Last then
                  Tmp := new Range_Array (1 .. Free_Ranges'Last * 2);
                  Tmp (1 .. Free_Ranges'Last) := Free_Ranges.all;
                  Free (Free_Ranges);
                  Free_Ranges := Tmp;
               end if;

               Free_Ranges (R + 1 .. Last_Range + 1) :=
                 Free_Ranges (R .. Last_Range);
               Free_Ranges (R + 1) :=
                 (From => Right + 1.0, To => Free_Ranges (R).To);
               Free_Ranges (R) :=
                 (From => Free_Ranges (R).From, To => E.Coord.X - 1.0);
               Last_Range := Last_Range + 1;

            elsif Free_Ranges (R).From >= E.Coord.X
              and then Free_Ranges (R).From <= Right
              and then Free_Ranges (R).To >= Right
            then
               Free_Ranges (R) :=
                 (From => Right + 1.0, To => Free_Ranges (R).To);
            end if;

            exit when Free_Ranges (R).From > Right;
         end loop;

         Next (Iter);
      end loop;

      declare
         Result : Gdouble_Array (1 .. Last_Range);
      begin
         for R in Result'Range loop
            --  ??? Should handle vertical layout and horizontal layout
            Result (R) :=
              (Free_Ranges (R).From + Free_Ranges (R).To) / 2.0;
         end loop;

         Free (Free_Ranges);
         return Result;
      end;
   end Compute_Line_Pos;

   --------------------------
   -- Draw_Orthogonal_Link --
   --------------------------

   procedure Draw_Orthogonal_Link
     (Canvas          : access Interactive_Canvas_Record'Class;
      Cr              : Cairo_Context;
      Link            : access Canvas_Link_Record'Class;
      Show_Annotation : Boolean)
   is
      X1, Y1, Xp1, Yp1, X2, Y2, Xp2, Yp2, X3, Y3 : Gdouble;
      X1_Canvas, Xc1_Canvas, Xc2_Canvas : Gdouble;
      X3_Canvas, Yp1_Canvas : Gdouble;
      Xp1_Canvas, Y2_Canvas, Y3_Canvas, Y1_Canvas : Gdouble;
      Yc1_Canvas, Yc2_Canvas, Yp2_Canvas : Gdouble;
      X2_Canvas, Xp2_Canvas : Gdouble;
      Xc1, Xc2, Yc1, Yc2 : Gdouble;
      Src      : constant Canvas_Item := Canvas_Item (Get_Src (Link));
      Dest     : constant Canvas_Item := Canvas_Item (Get_Dest (Link));
      Line_Pos : constant Gdouble_Array := Compute_Line_Pos (Canvas);

      Src_World : constant Cairo_Rectangle :=
        Get_Actual_Coordinates (Canvas, Src);
      Dest_World : constant Cairo_Rectangle :=
        Get_Actual_Coordinates (Canvas, Dest);

   begin
      X1 := Src_World.X;
      Y1 := Src_World.Y;
      X2 := Dest_World.X;
      Y2 := Dest_World.Y;

      Xp1 := X1 + Src.Coord.Width;
      Yp1 := Y1 + Src.Coord.Height;
      Xp2 := X2 + Dest_World.Width;
      Yp2 := Y2 + Dest_World.Height;

      Xc1 := (X1 + Xp1) / 2.0;

      if Canvas.Grid_Size > 0 then
         Xc1 := Do_Snap_Grid (Canvas, Xc1, 0.0);
      end if;

      Xc2 := (X2 + Xp2) / 2.0;
      if Canvas.Grid_Size > 0 then
         Xc2 := Do_Snap_Grid (Canvas, Xc2, 0.0);
      end if;

      Yc1 := (Y1 + Yp1) / 2.0;
      Yc2 := (Y2 + Yp2) / 2.0;

      --  The preferred case will be
      --     A ---
      --         |____ B
      --  The separation line should be at equal distance of the center of A
      --  and the center of B, so that multiple items lined up in a column
      --  above B all have the vertical line at the same location.
      --
      --  If the vertical line can be drawn at exact distance of the centers,
      --  then we try and display the vertical line at equal distance of the
      --  adjacent edges of A and B

      X3 := Gdouble'First;

      for L in Line_Pos'Range loop
         if Line_Pos (L) >= Xp1
           and then Line_Pos (L) <= X2
         then
            X3 := Line_Pos (L);
            exit;

         elsif Line_Pos (L) >= Xp2
           and then Line_Pos (L) <= X1
         then
            X3 := Line_Pos (L);
            exit;
         end if;
      end loop;

      X1_Canvas := World_To_Canvas_X (Canvas, X1);
      X2_Canvas := World_To_Canvas_X (Canvas, X2);
      Xc1_Canvas := World_To_Canvas_X (Canvas, Xc1);
      Xp1_Canvas := World_To_Canvas_X (Canvas, Xp1);
      Xc2_Canvas := World_To_Canvas_X (Canvas, Xc2);
      Xp2_Canvas := World_To_Canvas_X (Canvas, Xp2);

      Y1_Canvas  := World_To_Canvas_Y (Canvas, Y1);
      Y2_Canvas  := World_To_Canvas_Y (Canvas, Y2);
      Yp1_Canvas := World_To_Canvas_Y (Canvas, Yp1);
      Yp2_Canvas := World_To_Canvas_Y (Canvas, Yp2);
      Yc1_Canvas := World_To_Canvas_Y (Canvas, Yc1);
      Yc2_Canvas := World_To_Canvas_Y (Canvas, Yc2);

      if X3 /= Gdouble'First then
         X3_Canvas := World_To_Canvas_X (Canvas, X3);

         if X3 >= Xp1 then
            Link.Style.Draw_Polyline
              (Cr,
               ((Xp1_Canvas, Yc1_Canvas + 0.5),
                (X3_Canvas + 0.5,  Yc1_Canvas + 0.5),
                (X3_Canvas + 0.5,  Yc2_Canvas + 0.5),
                (X2_Canvas,        Yc2_Canvas + 0.5)));
         else
            Link.Style.Draw_Polyline
              (Cr,
               ((X1_Canvas,       Yc1_Canvas + 0.5),
                (X3_Canvas + 0.5, Yc1_Canvas + 0.5),
                (X3_Canvas + 0.5, Yc2_Canvas + 0.5),
                (Xp2_Canvas,      Yc2_Canvas + 0.5)));
         end if;

      --  Third case is when we didn't have enough space to draw the
      --  intermediate line. In that case, the layout is similar to
      --      A ----
      --           |
      --           B
      --  with the vertical line drawn at the same location as in the first
      --  algorithm.

      --  Second case is when one of the item is below the other one. In that
      --  case, the layout should look like
      --       AAA
      --       |_
      --         |
      --        BB
      --  ie the link connects the top side of one item and the bottom side of
      --  the other item.

      else
         Y3 := (Y1 + Yp1 + Y2 + Yp2) / 4.0;
         if Canvas.Grid_Size > 0 then
            Y3 := Do_Snap_Grid (Canvas, Y3, 0.0);
         end if;

         Y3_Canvas := World_To_Canvas_Y (Canvas, Y3);
         X3_Canvas := (Xc1_Canvas + Xc2_Canvas) / 2.0;

         if Y2 > Y3 then
            Link.Style.Draw_Polyline
              (Cr,
               ((Xc1_Canvas, Yp1_Canvas),
                (Xc1_Canvas, Y3_Canvas),
                (Xc2_Canvas, Y3_Canvas),
                (Xc2_Canvas, Y2_Canvas)));
         else
            Link.Style.Draw_Polyline
              (Cr,
               ((Xc1_Canvas, Y1_Canvas),
                (Xc1_Canvas, Y3_Canvas),
                (Xc2_Canvas, Y3_Canvas),
                (Xc2_Canvas, Yp2_Canvas)));
         end if;
      end if;

      --  Draw the text if any

      if Link.Descr /= null and then Show_Annotation then
         Set_Source_Color (Cr, Link.Style.Get_Font.Color);
         Draw_Annotation
           (Canvas, Cr, X3_Canvas, (Y1_Canvas + Y2_Canvas) / 2.0, Link);
      end if;
   end Draw_Orthogonal_Link;

   ------------------------
   -- Draw_Straight_Link --
   ------------------------

   procedure Draw_Straight_Link
     (Canvas          : access Interactive_Canvas_Record'Class;
      Cr              : Cairo_Context;
      Link            : access Canvas_Link_Record'Class;
      Show_Annotation : Boolean)
   is
      X1, Y1, X2, Y2, Xs, Ys, Xd, Yd : Gdouble;
      X1_Canvas, Y1_Canvas, X2_Canvas, Y2_Canvas : Gdouble;
      Src   : constant Canvas_Item := Canvas_Item (Get_Src (Link));
      Dest  : constant Canvas_Item := Canvas_Item (Get_Dest (Link));
      Src_Side, Dest_Side : Item_Side;

      Src_Coord : constant Cairo_Rectangle :=
        Get_Actual_Coordinates (Canvas, Src);
      Dest_Coord : constant Cairo_Rectangle :=
        Get_Actual_Coordinates (Canvas, Dest);

   begin
      Xs := Src_Coord.X;
      Ys := Src_Coord.Y;
      Xd := Dest_Coord.X;
      Yd := Dest_Coord.Y;

      Clip_Line
        (Src, Canvas,
         Xd + Dest_Coord.Width * Link.Dest_X_Pos,
         Yd + Dest_Coord.Height * Link.Dest_Y_Pos,
         X_Pos => Link.Src_X_Pos,
         Y_Pos => Link.Src_Y_Pos,
         Side  => Src_Side,
         X_Out => X1,
         Y_Out => Y1);
      Clip_Line
        (Dest, Canvas,
         Xs + Src_Coord.Width * Link.Src_X_Pos,
         Ys + Src_Coord.Height * Link.Src_Y_Pos,
         X_Pos => Link.Dest_X_Pos, Y_Pos => Link.Dest_Y_Pos,
         Side => Dest_Side, X_Out => X2, Y_Out => Y2);

      X1_Canvas := World_To_Canvas_X (Canvas, X1);
      Y1_Canvas := World_To_Canvas_Y (Canvas, Y1);
      X2_Canvas := World_To_Canvas_X (Canvas, X2);
      Y2_Canvas := World_To_Canvas_Y (Canvas, Y2);

      Link.Style.Draw_Polyline
        (Cr,
         ((X1_Canvas, Y1_Canvas), (X2_Canvas, Y2_Canvas)),
         Show_Arrows => True);

      --  Draw the text if any

      if Link.Descr /= null and then Show_Annotation then
         Draw_Annotation
           (Canvas, Cr,
            (X1_Canvas + X2_Canvas) / 2.0,
            (Y1_Canvas + Y2_Canvas) / 2.0, Link);
      end if;
   end Draw_Straight_Link;

   --------------------
   -- Draw_Self_Link --
   --------------------

   procedure Draw_Self_Link
     (Canvas          : access Interactive_Canvas_Record'Class;
      Cr              : Cairo_Context;
      Link            : access Canvas_Link_Record'Class;
      Offset          : Gint;
      Show_Annotation : Boolean)
   is
      Src       : constant Canvas_Item := Canvas_Item (Get_Src (Link));
      Xc, Yc    : Gdouble;
      X1, Y1, X3, Y3, Xc_Canvas, Yc_Canvas, Radius : Gdouble;
      Src_World : constant Cairo_Rectangle :=
        Get_Actual_Coordinates (Canvas, Src);

   begin
      pragma Assert (Src = Canvas_Item (Get_Dest (Link)));

      Xc := Src_World.X + Src_World.Width;
      Yc := Src_World.Y;

      Radius := World_To_Canvas_Length
        (Canvas, Gdouble (Canvas.Arc_Link_Offset / 2 * Offset));

      --  Location of the arrow and the annotation
      Xc_Canvas := World_To_Canvas_X (Canvas, Xc);
      Yc_Canvas := World_To_Canvas_Y (Canvas, Yc);
      X3 := Xc_Canvas - Radius;
      Y3 := Yc_Canvas;
      X1 := Xc_Canvas;
      Y1 := Yc_Canvas + Radius;

      Set_Source_Color (Cr, Link.Style.Get_Stroke);
      Cairo.Move_To (Cr, X3, Y3);
      Cairo.Arc (Cr, Xc_Canvas, Yc_Canvas, Radius, Pi, Pi * 0.5);
      Cairo.Stroke (Cr);

      Link.Style.Draw_Arrows_And_Symbols
        (Cr,
         ((X3, Y3), (X3, Y3 - 20.0),
          (X1 + 20.0, Y1), (X1, Y1)));

      --  Draw the annotations
      if Link.Descr /= null and then Show_Annotation then
         Draw_Annotation
           (Canvas, Cr,
            Xc_Canvas + Radius / 2.0, Yc_Canvas + Radius / 2.0, Link);
      end if;
   end Draw_Self_Link;

   -------------------
   -- Draw_Arc_Link --
   -------------------

   procedure Draw_Arc_Link
     (Canvas          : access Interactive_Canvas_Record'Class;
      Cr              : Cairo_Context;
      Link            : access Canvas_Link_Record'Class;
      Offset          : Gdouble;
      Show_Annotation : Boolean)
   is
      Angle       : Gdouble;
      X1, Y1, X2, Y2, X3, Y3 : Gdouble;
      X1_Canvas, Y1_Canvas, X2_Canvas, Y2_Canvas : Gdouble;
      X3_Canvas, Y3_Canvas : Gdouble;
      Right_Angle : constant Gdouble := Pi / 2.0;
      Arc_Offset  : constant Gdouble := Gdouble (Canvas.Arc_Link_Offset);
      Src         : constant Canvas_Item := Canvas_Item (Get_Src (Link));
      Dest        : constant Canvas_Item := Canvas_Item (Get_Dest (Link));
      Src_Side, Dest_Side : Item_Side;

      Src_World : constant Cairo_Rectangle :=
        Get_Actual_Coordinates (Canvas, Src);
      Dest_World : constant Cairo_Rectangle :=
        Get_Actual_Coordinates (Canvas, Dest);

   begin
      X1 := Src_World.X;
      Y1 := Src_World.Y;
      X3 := Dest_World.X;
      Y3 := Dest_World.Y;

      --  We will first compute the extra intermediate point between the
      --  center of the two items. Once we have this intermediate point, we
      --  will be able to use the intersection point between the two items
      --  and the two lines from the centers to the middle point. This extra
      --  point is used as a control point for the Bezier curve.

      X1 := X1 + Src.Coord.Width * Link.Src_X_Pos;
      Y1 := Y1 + Src.Coord.Height * Link.Src_Y_Pos;
      X3 := X3 + Dest.Coord.Width * Link.Dest_X_Pos;
      Y3 := Y3 + Dest.Coord.Height * Link.Dest_Y_Pos;

      --  Compute the middle point for the arc, and create a dummy item for it
      --  that the user can move.

      if X1 /= X3 then
         Angle := Arctan (Gdouble (Y3 - Y1), Gdouble (X3 - X1));
      elsif Y3 > Y1 then
         Angle := Right_Angle;
      else
         Angle := -Right_Angle;
      end if;

      if Offset < 0.0 then
         Angle := Angle - Right_Angle;
      else
         Angle := Angle + Right_Angle;
      end if;

      X2 := (X1 + X3) / 2.0 + abs (Offset) * Arc_Offset * Cos (Angle);
      Y2 := (Y1 + Y3) / 2.0 + abs (Offset) * Arc_Offset * Sin (Angle);

      --  Clip to the border of the boxes

      Clip_Line
        (Src, Canvas,
         X2, Y2, Link.Src_X_Pos, Link.Src_Y_Pos, Src_Side, X1, Y1);
      Clip_Line
        (Dest, Canvas, X2, Y2, Link.Dest_X_Pos, Link.Dest_Y_Pos,
         Dest_Side, X3, Y3);

      X1_Canvas := World_To_Canvas_X (Canvas, X1);
      Y1_Canvas := World_To_Canvas_Y (Canvas, Y1);
      X2_Canvas := World_To_Canvas_X (Canvas, X2);
      Y2_Canvas := World_To_Canvas_Y (Canvas, Y2);
      X3_Canvas := World_To_Canvas_X (Canvas, X3);
      Y3_Canvas := World_To_Canvas_Y (Canvas, Y3);

      Cairo.Move_To (Cr, X1_Canvas, Y1_Canvas);
      Cairo.Curve_To
        (Cr, X1_Canvas, Y1_Canvas,
         X2_Canvas, Y2_Canvas,
         X3_Canvas, Y3_Canvas);
      Cairo.Stroke (Cr);

      Link.Style.Draw_Arrows_And_Symbols
        (Cr,
         ((X1_Canvas, Y1_Canvas),
          (X2_Canvas, Y2_Canvas),
          (X3_Canvas, Y3_Canvas)));

      --  Draw the annotations, if any, in the middle of the link
      if Link.Descr /= null and then Show_Annotation then
         X2_Canvas := 0.25 * X1_Canvas + 0.5 * X2_Canvas + 0.25 * X3_Canvas;
         Y2_Canvas := 0.25 * Y1_Canvas + 0.5 * Y2_Canvas + 0.25 * Y3_Canvas;
         Draw_Annotation (Canvas, Cr, X2_Canvas, Y2_Canvas, Link);
      end if;
   end Draw_Arc_Link;

   ---------------
   -- Clip_Line --
   ---------------

   procedure Clip_Line
     (Src    : access Canvas_Item_Record'Class;
      Canvas : access Interactive_Canvas_Record'Class;
      To_X   : Gdouble;
      To_Y   : Gdouble;
      X_Pos  : Gdouble;
      Y_Pos  : Gdouble;
      Side   : out Item_Side;
      X_Out  : out Gdouble;
      Y_Out  : out Gdouble)
   is
      Rect    : constant Cairo_Rectangle :=
        Get_Actual_Coordinates (Canvas, Src);
      Src_X   : Gdouble;
      Src_Y   : Gdouble;
      Delta_X : Gdouble;
      Delta_Y : Gdouble;
      Offset  : Gdouble;
   begin
      Src_X    := Rect.X + Rect.Width * X_Pos;
      Src_Y    := Rect.Y + Rect.Height * Y_Pos;
      Delta_X  := To_X - Src_X;
      Delta_Y  := To_Y - Src_Y;

      --  Intersection with horizontal sides

      if Delta_Y /= 0.0 then
         Offset := (Src_X * To_Y - To_X * Src_Y);

         if Delta_Y < 0.0 then
            Side := North;
            Y_Out := Rect.Y;
         else
            Side := South;
            Y_Out := Rect.Y + Rect.Height;
         end if;

         X_Out := (Delta_X * Y_Out + Offset) / Delta_Y;

         if Rect.X <= X_Out
           and then X_Out <= Rect.X + Rect.Width
         then
            return;
         end if;
      end if;

      --  Intersection with vertical sides

      if Delta_X /= 0.0 then
         Offset := (To_X * Src_Y - Src_X * To_Y);

         if Delta_X < 0.0 then
            Side := West;
            X_Out := Rect.X;
         else
            Side := East;
            X_Out := Rect.X + Rect.Width;
         end if;

         Y_Out := (Delta_Y * X_Out + Offset) / Delta_X;

         if Rect.Y <= Y_Out
           and then Y_Out <= Rect.Y + Rect.Height
         then
            return;
         end if;
      end if;

      X_Out := 0.0;
      Y_Out := 0.0;
      Side := East;
   end Clip_Line;

end Gtkada.Canvas.Links;
