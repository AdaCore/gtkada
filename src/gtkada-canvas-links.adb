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
with Gdk.Cairo;                     use Gdk.Cairo;
with Glib.Graphs;                   use Glib.Graphs;
with Gtkada.Canvas.Objects;         use Gtkada.Canvas.Objects;
with Gtkada.Style;                  use Gtkada.Style;
with Pango.Cairo;                   use Pango.Cairo;
with Pango.Layout;                  use Pango.Layout;

package body Gtkada.Canvas.Links is

   package Gdouble_Elementary_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Gdouble);
   use Gdouble_Elementary_Functions;

   type Anchors is record
      From_Box          : Cairo_Rectangle;
      --  The bounding box for the source element of the link

      From_Toplevel_Box : Cairo_Rectangle;
      --  The bounding box for the toplevel element that contains the source
      --  element.

      To_Box            : Cairo_Rectangle;
      --  The bounding box for the target element of the link

      To_Toplevel_Box   : Cairo_Rectangle;
      --  The bounding box for the toplevel element that contains the target
      --  element.

      F, T              : Point;
      --  Anchor points for each end of the link, on the toplevel boxes
   end record;
   --  Various dimensions computed for the layout of links.
   --  It contains the start and end points for the link, as well as the
   --  bounding boxes.

   function Compute_Anchors
     (Self : not null access Canvas_Link_Record'Class) return Anchors;
   --  Compute the start and end point of the link, depending on the two
   --  objects it links. This makes sure that the link does not start in the
   --  middle of the box.

   procedure Compute_Labels
     (Self   : not null access Canvas_Link_Record'Class;
      Canvas : not null access Interactive_Canvas_Record'Class;
      Dim    : Anchors);
   --  Compute the position for the end labels and middle label for the link

   procedure Compute_Bounding_Box
     (Self : not null access Canvas_Link_Record'Class);
   --  Compute the bounding box for the link

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

   function Compute_Anchors
     (Self : not null access Canvas_Link_Record'Class) return Anchors
   is
      S : constant Canvas_Item := Canvas_Item (Self.Get_Src);
      D : constant Canvas_Item := Canvas_Item (Self.Get_Dest);

      Result : Anchors :=
        (F                 => Link_Anchor_Point (S, Self.Src_Anchor),
         From_Box          => S.Get_Coord,
         From_Toplevel_Box => S.Get_Toplevel.Get_Coord,
         T                 => Link_Anchor_Point (D, Self.Dest_Anchor),
         To_Box            => D.Get_Coord,
         To_Toplevel_Box   => D.Get_Toplevel.Get_Coord);

      P : Point;
   begin
      --  Clip the line to the side of the toplevel boxes.

      if Self.Src_Anchor.Side = Auto then
         if Self.Waypoints = null then
            P := Result.T;
         else
            P := Self.Waypoints (Self.Waypoints'First);
         end if;
         Result.F := S.Get_Toplevel.Clip_Line (Result.F, P);
      end if;

      if Self.Dest_Anchor.Side = Auto then
         if Self.Waypoints = null then
            P := Result.F;
         else
            P := Self.Waypoints (Self.Waypoints'Last);
         end if;
         Result.T := D.Get_Toplevel.Clip_Line (Result.T, P);
      end if;

      return Result;
   end Compute_Anchors;

   --------------------------
   -- Compute_Bounding_Box --
   --------------------------

   procedure Compute_Bounding_Box
     (Self : not null access Canvas_Link_Record'Class)
   is
      P     : constant Point_Array_Access := Self.Points;
      Max_X : Gdouble := P (P'First).X;
      Max_Y : Gdouble := P (P'First).Y;
      X, Y  : Gdouble;
   begin
      X := Max_X;
      Y := Max_Y;

      for P1 in P'First + 1 .. P'Last loop
         X := Gdouble'Min (X, P (P1).X);
         Y := Gdouble'Min (Y, P (P1).Y);
         Max_X := Gdouble'Max (Max_X, P (P1).X);
         Max_Y := Gdouble'Max (Max_Y, P (P1).Y);
      end loop;

      Self.Bounding_Box :=
        (X => X, Y => Y, Width => Max_X - X, Height => Max_Y - Y);
   end Compute_Bounding_Box;

   --------------------
   -- Compute_Labels --
   --------------------

   procedure Compute_Labels
     (Self   : not null access Canvas_Link_Record'Class;
      Canvas : not null access Interactive_Canvas_Record'Class;
      Dim    : Anchors)
   is
      pragma Unreferenced (Self, Canvas, Dim);
   begin
      null;
   end Compute_Labels;

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

   ----------------------------------------
   -- Compute_Layout_For_Orthogonal_Link --
   ----------------------------------------

   procedure Compute_Layout_For_Orthogonal_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      Link   : access Canvas_Link_Record'Class)
   is
      procedure Compute_Sides
        (Anchor : Anchor_Attachment;
         F : Point;
         Box, TBox : Cairo_Rectangle;
         Effective_Side : out Side_Attachment;
         P : out Point);
      --  Compute whether a link should start horizontal or vertical, and
      --  which side it should start on.

      function Clamp (Value, Min, Max : Gdouble) return Gdouble;
      --  Takes a number and clamps it to within the provided bounds.

      -----------
      -- Clamp --
      -----------

      function Clamp (Value, Min, Max : Gdouble) return Gdouble is
      begin
         return Gdouble'Min (Gdouble'Max (Value, Min), Max);
      end Clamp;

      -------------------
      -- Compute_Sides --
      -------------------

      procedure Compute_Sides
        (Anchor : Anchor_Attachment;
         F : Point;
         Box, TBox : Cairo_Rectangle;
         Effective_Side : out Side_Attachment;
         P : out Point)
      is
      begin
         case Anchor.Side is
            when Top | Bottom | Right | Left =>
               Effective_Side := Anchor.Side;
            when Auto =>
               if F.X = TBox.X then
                  Effective_Side := Left;
               elsif F.Y = TBox.Y then
                  Effective_Side := Top;
               elsif F.X = TBox.X + TBox.Width then
                  Effective_Side := Right;
               else
                  Effective_Side := Bottom;
               end if;
         end case;

         case Effective_Side is
            when Top =>
               P := (Box.X + Box.Width * Anchor.X, TBox.Y);
            when Bottom =>
               P := (Box.X + Box.Width * Anchor.X, TBox.Y + TBox.Height);
            when Right =>
               P := (TBox.X + TBox.Width, Box.Y + Box.Height * Anchor.Y);
            when Left =>
               P := (TBox.X, Box.Y + Box.Height * Anchor.Y);
            when others =>
               null;
         end case;
      end Compute_Sides;

      B : constant Gdouble := 10.0;
      --  Border around each box in which the links should not be displayed.

      Dim : constant Anchors := Compute_Anchors (Link);

      FTX1 : constant Gdouble := Dim.From_Toplevel_Box.X;  --  from-top-x1
      FTX2 : constant Gdouble := FTX1 + Dim.From_Toplevel_Box.Width;
      FTY1 : constant Gdouble := Dim.From_Toplevel_Box.Y;  --  from-top-y1
      FTY2 : constant Gdouble := FTY1 + Dim.From_Toplevel_Box.Height;

      TTX1 : constant Gdouble := Dim.To_Toplevel_Box.X;  --  from-top-x1
      TTX2 : constant Gdouble := TTX1 + Dim.To_Toplevel_Box.Width;
      TTY1 : constant Gdouble := Dim.To_Toplevel_Box.Y;  --  from-top-y1
      TTY2 : constant Gdouble := TTY1 + Dim.To_Toplevel_Box.Height;

      Min_Space : constant Gdouble := Link.Style.Get_Line_Width * 3.0;
      --  Minimal space between two boxes to pass a link between them

      From_Side, To_Side : Side_Attachment;
      From, To : Point;
      Middle : Point;
      M, Tmp : Gdouble;
      P1     : Point;   --  extending from the From box
      P2     : Point;   --  extending from the To box
   begin
      Compute_Sides
        (Link.Src_Anchor, Dim.F, Dim.From_Box, Dim.From_Toplevel_Box,
         From_Side, From);
      Compute_Sides
        (Link.Dest_Anchor, Dim.T, Dim.To_Box, Dim.To_Toplevel_Box,
         To_Side, To);

      case



      --  The position for the middle lines, when there is any. It is chosen
      --  relative to the source, but so that it doesn't overlap the box if
      --  possible. That ensures that all links existing from the source will
      --  use the same middle line, and thus will nicely overlap.

      if To.X > From.X then
         Middle.X := Clamp (From.X + 30.0, FTX2 + B, TTX1 - B);
      else
         Middle.X := Clamp (From.X - 30.0, TTX2 + B, FTX1 - B);
      end if;

      if To.Y > From.Y then
         Middle.Y := Clamp (From.Y + 30.0, FTY2 + B, TTY1 - B);
      else
         Middle.Y := Clamp (From.Y - 30.0, TTY2 + B, FTY1 - B);
      end if;

      Unchecked_Free (Link.Points);

      case From_Side is
         when Auto =>
            null;

         when Right =>
            case To_Side is
               when Auto =>
                  null;
               when Right =>
                  --  3:  A --
                  --          |
                  --      B --

                  M := Gdouble'Max (From.X + B, To.X + B);
                  Link.Points := new Point_Array'
                    ((From, (M, From.Y), (M, To.Y), To));

               when Left =>
                  --  1:  A --
                  --          |
                  --           --- B

                  if abs (From.Y - To.Y) < 5.0 then
                     --  can we straighten the link altogether ?
                     Link.Points := new Point_Array'((From, To));
                  else
                     Link.Points := new Point_Array'
                       ((From, (Middle.X, From.Y), (Middle.X, To.Y), To));
                  end if;

               when Top | Bottom =>
                  --  7:       B
                  --           |
                  --       A___|

                  Link.Points := new Point_Array'((From, (To.X, From.Y), To));

            end case;

         when Left =>
            case To_Side is
               when Auto =>
                  null;
               when Right =>
                  --  2:       -- A
                  --          |
                  --      B --

                  if abs (From.Y - To.Y) < 5.0 then
                     --  can we straighten the link altogether ?
                     Link.Points := new Point_Array'((From, To));
                  else
                     Link.Points := new Point_Array'
                       ((From, (Middle.X, From.Y), (Middle.X, To.Y), To));
                  end if;

               when Left =>
                  --  4:   -- A
                  --      |
                  --       -- B

                  M := Gdouble'Min (From.X - B, To.X - B);
                  Link.Points := new Point_Array'
                    ((From, (M, From.Y), (M, To.Y), To));

               when Top | Bottom =>
                  --  9:  B
                  --      |
                  --      -- A

                  Link.Points := new Point_Array'((From, (To.X, From.Y), To));

            end case;

         when Top =>
            case To_Side is
               when Auto =>
                  null;

               when Right =>
                  --   1: B--    2: B__  3: __   4: ___
                  --        |       __|    | |     |  |
                  --        A      |       A |     |  A
                  --               A       B_|   B_|

                  if FTY2 <= TTY1 - Min_Space then
                     Tmp := (FTY2 + TTY1) / 2.0;
                  elsif TTY2 <= FTY1 - Min_Space then
                     Tmp := (TTY2 + FTY1) / 2.0;
                  else
                     --  not enough space between the two
                     Tmp := Gdouble'Min (From.Y - B, To.Y - B);
                  end if;

                  if From.X > To.X + B and then From.Y - B > To.Y then
                     --  case 1
                     Link.Points := new Point_Array'
                       ((From, (From.X, To.Y), To));
                  elsif To.X >= FTX1 - B and then To.X <= FTX2 + B then
                     Link.Points := new Point_Array'
                       ((From,
                        (From.X, Tmp),
                        (To.X + B, Tmp),
                        (To.X + B, To.Y),
                        To));
                  else
                     Link.Points := new Point_Array'
                       ((From,
                        (From.X, Tmp),
                        (To.X + B, Tmp),
                        (To.X + B, To.Y),
                        To));
                  end if;

               when Left =>  --  Similar to Right
                  if FTY2 <= TTY1 - Min_Space then
                     Tmp := (FTY2 + TTY1) / 2.0;
                  elsif TTY2 <= FTY1 - Min_Space then
                     Tmp := (TTY2 + FTY1) / 2.0;
                  else
                     --  not enough space between the two
                     Tmp := Gdouble'Min (From.Y - B, To.Y - B);
                  end if;

                  if From.X < To.X - B and then From.Y - B > To.Y then
                     Link.Points := new Point_Array'
                       ((From, (From.X, To.Y), To));
                  elsif To.X - B >= FTX1 - B and then To.X - B <= FTX2 + B then
                     Link.Points := new Point_Array'
                       ((From,
                        (From.X, FTY1 - B),
                        (FTX1 - B, FTY1 - B),
                        (FTX1 - B, To.Y),
                        To));
                  else
                     Link.Points := new Point_Array'
                       ((From,
                        (From.X, FTY1 - B),
                        (To.X - B, FTY1 - B),
                        (To.X - B, To.Y),
                        To));
                  end if;

               when Top =>
                  --   18:   ---     ---       ---
                  --        |  |     |  |     |  |
                  --        A  B     A  |     B  |
                  --                    |        |
                  --                  --       --
                  --                 |        |
                  --                 B        A

                  M := Gdouble'Min (From.Y - B, To.Y - B);

                  if From.X < To.X then
                     Tmp := FTX2 + B;
                  else
                     Tmp := TTX2 + B;
                  end if;

                  if To.X >= FTX1 and then To.X <= FTX2 then
                     if From.Y < To.Y then
                        Link.Points := new Point_Array'
                          ((From,
                           (From.X, M),
                           (Tmp, M),
                           (Tmp, Middle.Y),
                           (To.X, Middle.Y),
                           To));
                     else
                        Link.Points := new Point_Array'
                          ((From,
                           (From.X, Middle.Y),
                           (Tmp, Middle.Y),
                           (Tmp, M),
                           (To.X, M),
                           To));
                     end if;
                  else
                     Link.Points := new Point_Array'
                       ((From, (From.X, M), (To.X, M), To));
                  end if;

               when Bottom =>
                  --  16:  B
                  --       |_
                  --         |
                  --         A

                  if abs (From.X - To.X) < 5.0 then
                     --  can we straighten the link altogether ?
                     Link.Points := new Point_Array'((From, To));
                  else
                     Link.Points := new Point_Array'
                       ((From, (From.X, Middle.Y), (To.X, Middle.Y), To));
                  end if;
            end case;

         when Bottom =>
            case To_Side is
               when Auto =>
                  null;
               when Left | Right =>
                  --  11:  A
                  --       |
                  --       |__B

                  Link.Points := new Point_Array'
                    ((From, (From.X, To.Y), To));

               when Top =>
                  --  15:   A
                  --       |_
                  --         |
                  --         B

                  if abs (From.X - To.X) < 5.0 then
                     --  can we straighten the link altogether ?
                     Link.Points := new Point_Array'((From, To));
                  else
                     Link.Points := new Point_Array'
                       ((From, (From.X, Middle.Y), (To.X, Middle.Y), To));
                  end if;

               when Bottom =>
                  --  17:  A  B
                  --       |  |
                  --       ----

                  M := Gdouble'Max (From.Y + B, To.Y + B);
                  Link.Points := new Point_Array'
                    ((From, (From.X, M), (To.X, M), To));
            end case;
      end case;

      Compute_Bounding_Box (Link);
      Compute_Labels (Link, Canvas, Dim);
   end Compute_Layout_For_Orthogonal_Link;

   --------------------------------------
   -- Compute_Layout_For_Straight_Link --
   --------------------------------------

   procedure Compute_Layout_For_Straight_Link
     (Canvas          : access Interactive_Canvas_Record'Class;
      Link            : access Canvas_Link_Record'Class)
   is
      P   : constant Point_Array_Access := Link.Points;
      Dim : constant Anchors := Compute_Anchors (Link);

      function Get_Wp return Point_Array;
      --  Return the waypoints to use

      function Get_Wp return Point_Array is
      begin
         --  Support for self-referencing straight links

         if Get_Src (Link) = Get_Dest (Link) then
            return No_Waypoints;
         elsif Link.Waypoints /= null then
            return Link.Waypoints.all;
         else
            return No_Waypoints;
         end if;
      end Get_Wp;

      Waypoints : Point_Array := Get_Wp;
      Tmp       : Point;
   begin
      if Get_Src (Link) = Get_Dest (Link) then
         Tmp := (Dim.From_Toplevel_Box.X + Dim.From_Toplevel_Box.Width,
                 Dim.From_Toplevel_Box.Y);
         Unchecked_Free (Link.Points);
         Link.Points := new Point_Array'
           ((Tmp.X - 10.0, Tmp.Y),
            (Tmp.X - 10.0, Tmp.Y - 10.0),
            (Tmp.X + 10.0, Tmp.Y - 10.0),
            (Tmp.X + 10.0, Tmp.Y + 10.0),
            (Tmp.X, Tmp.Y + 10.0));

      --  If a link had waypoints so that the first outbound segment from a
      --  box is horizontal or vertical, we move the waypoint to keep the
      --  segment that way. This provides a more natural feel.

      elsif P /= null and then Waypoints'Length /= 0 then
         --  Is the first segment horizontal or vertical ?
         if abs (P (P'First).Y - Waypoints (Waypoints'First).Y) < 0.8 then
            Waypoints (Waypoints'First).Y := Dim.F.Y;
         elsif abs (P (P'First).X - Waypoints (Waypoints'First).Y) < 0.8 then
            Waypoints (Waypoints'First).X := Dim.F.X;
         end if;

         --  Is the last segment horizontal or vertical ?

         --  If this horizontal ?
         if abs (P (P'Last).Y - Waypoints (Waypoints'Last).Y) < 0.8 then
            Waypoints (Waypoints'Last).Y := Dim.T.Y;
            --  is this vertical ?
         elsif abs (P (P'Last).X - Waypoints (Waypoints'Last).Y) < 0.8 then
            Waypoints (Waypoints'Last).X := Dim.T.X;
         end if;

         Unchecked_Free (Link.Points);
         Link.Points := new Point_Array'(Dim.F & Waypoints & Dim.T);

      else
         Unchecked_Free (Link.Points);
         Link.Points := new Point_Array'(Dim.F & Waypoints & Dim.T);
      end if;

      Compute_Bounding_Box (Link);
      Compute_Labels (Link, Canvas, Dim);
   end Compute_Layout_For_Straight_Link;

   ---------------------------------
   -- Compute_Layout_For_Arc_Link --
   ---------------------------------

   procedure Compute_Layout_For_Arc_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      Link   : access Canvas_Link_Record'Class;
      Offset : Gint := 1)
   is
      Dim : constant Anchors := Compute_Anchors (Link);

      function Get_Wp return Point_Array;
      --  Return the waypoints to use

      procedure Control_Points (From, To : Point; Ctrl1, Ctrl2 : out Point);
      --  Compute the control points on the curve from From to To

      function Get_Wp return Point_Array is
      begin
         if Get_Src (Link) = Get_Dest (Link) then
            return No_Waypoints;
         elsif Link.Waypoints /= null then
            return Link.Waypoints.all;
         else
            return No_Waypoints;
         end if;
      end Get_Wp;

      procedure Control_Points (From, To : Point; Ctrl1, Ctrl2 : out Point) is
         Dx : Gdouble := To.X - From.X;
         Dy : Gdouble := To.Y - From.Y;
         D : constant Gdouble := Sqrt (Dx * Dx + Dy * Dy);

         Pixels : constant Gdouble := 10.0;
         --  Height of the rectangle, i.e. maximum distance between the arc
         --  link and the straight link joining the same two ends.

         E : constant Gdouble := Pixels * Gdouble (abs (Offset)) / D;

      begin
         Dx := Dx * E;
         Dy := Dy * E;

         if Offset > 0 then
            Ctrl1 := (From.X - Dy, From.Y + Dx);
            Ctrl2 := (To.X - Dy,   To.Y + Dx);
         else
            Ctrl1 := (From.X + Dy, From.Y - Dx);
            Ctrl2 := (To.X + Dy,   To.Y - Dx);
         end if;
      end Control_Points;

      Waypoints : constant Point_Array := Get_Wp;
      P1, P2, P3, P4 : Point;
   begin
      Unchecked_Free (Link.Points);

      --  Support for self-referencing links

      if Get_Src (Link) = Get_Dest (Link) then
         Link.Points := new Point_Array'
           (Circle_From_Bezier
              (Center =>
                   (Dim.From_Toplevel_Box.X + Dim.From_Toplevel_Box.Width,
                    Dim.From_Toplevel_Box.Y),
               Radius => 10.0 + 4.0 * Gdouble (abs (Offset) - 1)));

      elsif Waypoints'Length = 0 then
         Control_Points (Dim.F, Dim.T, P1, P2);
         Link.Points := new Point_Array'((Dim.F, P1, P2, Dim.T));

      else
         Control_Points (Dim.F, Waypoints (Waypoints'First), P1, P2);
         Control_Points (Waypoints (Waypoints'Last), Dim.T, P3, P4);
         Link.Points := new Point_Array'
           (Point_Array'(Dim.F, P1, P2)
            & Waypoints
            & Point_Array'(P3, P4, Dim.T));
      end if;

      Compute_Bounding_Box (Link);
      Compute_Labels (Link, Canvas, Dim);
   end Compute_Layout_For_Arc_Link;

   ----------------------------------------
   -- Compute_Layout_For_Orthocurve_Link --
   ----------------------------------------

   procedure Compute_Layout_For_Orthocurve_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      Link   : access Canvas_Link_Record'Class)
   is
   begin
      Compute_Layout_For_Orthogonal_Link (Canvas, Link);
   end Compute_Layout_For_Orthocurve_Link;

   ---------------
   -- Draw_Link --
   ---------------

   procedure Draw_Link
     (Canvas           : access Interactive_Canvas_Record'Class;
      Link             : access Canvas_Link_Record'Class;
      Cr               : Cairo.Cairo_Context;
      Show_Annotations : Boolean := True)
   is
      P      : constant Point_Array_Access := Link.Points;
   begin
      pragma Assert (P /= null, "waypoints must be computed first");
      pragma Assert (P'Length >= 2, "no enough waypoints");

      case Link.Routing is
         when Straight | Orthogonal =>
            Link.Style.Draw_Polyline (Cr, P.all);

         when Orthocurve =>
            if P'Length = 2 then
               Link.Style.Draw_Polyline (Cr, P.all);
            else
               Link.Style.Draw_Polycurve (Cr, P.all);
            end if;

         when Curve =>
            Link.Style.Draw_Polycurve (Cr, P.all);
      end case;

      if False and then Show_Annotations then
         Draw_Annotation
           (Canvas, Cr, P (P'First).X, P (P'First).Y, Link);
      end if;
   end Draw_Link;

end Gtkada.Canvas.Links;
