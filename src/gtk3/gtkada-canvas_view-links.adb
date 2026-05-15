------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2018, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with Cairo;                         use Cairo;
with Gtkada.Canvas_View.Astar;      use Gtkada.Canvas_View.Astar;
with Gtkada.Canvas_View.Objects;    use Gtkada.Canvas_View.Objects;
with Gtkada.Style;                  use Gtkada.Style;

package body Gtkada.Canvas_View.Links is

   use Gdouble_Elementary_Functions;

   type End_Info is record
      Box      : Model_Rectangle;
      --  The bounding box for the end element of the link

      Toplevel : Model_Rectangle;
      --  The bounding box for the toplevel element that contains the source
      --  element.

      P        : Model_Point;
      --  Anchor points for each end of the link, on the toplevel box
   end record;

   type Anchors is record
      From, To : End_Info;
   end record;
   --  Various dimensions computed for the layout of links.
   --  It contains the start and end points for the link, as well as the
   --  bounding boxes.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Item_Point_Array, Item_Point_Array_Access);

   procedure Sort (Arr : in out Gdouble_Array);
   --  Sort the array

   function Compute_Anchors
     (Self : not null access Canvas_Link_Record'Class) return Anchors;
   --  Compute the start and end point of the link, depending on the two
   --  objects it links. This makes sure that the link does not start in the
   --  middle of the box.

   procedure Compute_Labels
     (Self    : not null access Canvas_Link_Record'Class;
      Context : Draw_Context;
      Dim     : Anchors);
   --  Compute the position for the end labels and middle label for the link

   type Layout_Matrix is record
      X, Y : Gdouble_Array (1 .. 7);
      Dim  : Anchors;
   end record;
   --  Mapping from integer to double: the double values indicate positions
   --  in the canvas itself. The integer coordinates are used for the A*
   --  algorithm, since we are working on a non-uniform grid.

   function Manhattan_Dist
      (Self : Layout_Matrix; From, To : Coordinate) return Integer;
   function Manhattan_Cost
      (Self : Layout_Matrix; Parent, From, To : Coordinate) return Integer;
   function Next_Point is new Manhattan_Next_Point (Layout_Matrix);
   --  Functions for the A* algorithm.

   function Relative_To_Item
     (From : Model_Point; Wp : Item_Point_Array) return Model_Point;
   function Relative_To_Array
     (From : Item_Point; Wp : Item_Point_Array) return Item_Point_Array;
   --  Compute the last point after applying all the relative coordinates
   --  from Wp

   procedure Compute_Sides
     (Link   : not null access Canvas_Link_Record'Class;
      Anchor : Anchor_Attachment;
      Info   : End_Info;
      Margin : Margins;
      P, P2  : out Item_Point);
   --  Compute where a link should point to (for one of its ends).
   --  (P, P2) is the first segment of an orthogonal link exiting from the
   --  box. P2 is aligned on a grid of size B, at a distance Margin from P,
   --  so that we can use Astar

   procedure Orthogonal_Waypoints
     (Link       : not null access Canvas_Link_Record'Class;
      Context    : Draw_Context;
      Min_Margin : Gdouble;
      Max_Margin : Gdouble);
   --  Min_Margin: we do not want lines to be displayed too close to an item,
   --  this is the minimal distance.
   --
   --  Max_Margin: On the other hand, we do not want the link to be too far
   --  either, so that for instance if Item1 is linked to both Item2 and Item3,
   --  the two link will share the middle line even when the items are not
   --  aligned:
   --
   --           Item1
   --             |
   --       ------+--------
   --       |             |
   --      Item2          |
   --                   Item3
   --
   --  If the position of the horizontal line only depends on the distance
   --  between Item1 and either Item2 or Item3, it would only be shared
   --  if Item2 and Item3 are at the same y coordinate.

   --------------------
   -- Manhattan_Dist --
   --------------------

   function Manhattan_Dist
     (Self : Layout_Matrix; From, To : Coordinate) return Integer
   is
      pragma Unreferenced (Self);
   begin
      return abs (From.X - To.X) + abs (From.Y - To.Y);
   end Manhattan_Dist;

   --------------------
   -- Manhattan_Cost --
   --------------------

   function Manhattan_Cost
      (Self : Layout_Matrix; Parent, From, To : Coordinate)
      return Integer is
   begin
      if To.X not in Self.X'Range
        or else To.Y not in Self.Y'Range

        --  If the target is not a valid destination
        or else abs (Self.X (To.X) - Gdouble'Last) < 0.01
        or else abs (Self.Y (To.Y) - Gdouble'Last) < 0.01
      then
         return Not_Traversable;

      elsif Point_In_Rect
          (Self.Dim.From.Toplevel, (Self.X (To.X), Self.Y (To.Y)))
        or else Point_In_Rect
          (Self.Dim.To.Toplevel, (Self.X (To.X), Self.Y (To.Y)))
      then
         return 1_000_000; --  very expensive, but not impossible if we have to

      else
         --  A bend is costly

         if (Parent.X = From.X and then From.X /= To.X)
           or else (Parent.Y = From.Y and then From.Y /= To.Y)
         then
            return 100_000;
         else
            return 1;
         end if;
      end if;
   end Manhattan_Cost;

   function Astar_Find is new Find_Path
     (User_Data      => Layout_Matrix,
      Heuristic_Cost => Manhattan_Cost,
      Next_Point     => Next_Point,
      Heuristic_Dist => Manhattan_Dist);
   --  An A* algorithm to find the shortest path between two points,
   --  while avoiding overlapping with two boxes specified in the
   --  Layout_Matrix.

   ----------------------
   -- Relative_To_Item --
   ----------------------

   function Relative_To_Item
     (From : Model_Point; Wp : Item_Point_Array) return Model_Point
   is
      C : Model_Point := From;
   begin
      for W in Wp'Range loop
         C := (C.X + Wp (W).X, C.Y + Wp (W).Y);
      end loop;
      return C;
   end Relative_To_Item;

   -----------------------
   -- Relative_To_Array --
   -----------------------

   function Relative_To_Array
     (From : Item_Point; Wp : Item_Point_Array) return Item_Point_Array
   is
      Result : Item_Point_Array (Wp'Range);
   begin
      Result (Result'First) :=
        (From.X + Wp (Wp'First).X, From.Y + Wp (Wp'First).Y);
      for P in Wp'First + 1 .. Wp'Last loop
         Result (P) :=
           (Result (P - 1).X + Wp (P).X,
            Result (P - 1).Y + Wp (P).Y);
      end loop;
      return Result;
   end Relative_To_Array;

   ---------------------
   -- Compute_Anchors --
   ---------------------

   function Compute_Anchors
     (Self : not null access Canvas_Link_Record'Class) return Anchors
   is
      S  : constant Abstract_Item := Self.From;
      ST : Abstract_Item := Gtkada.Canvas_View.Objects.Toplevel (S);
      D  : constant Abstract_Item := Self.To;
      DT : Abstract_Item := Gtkada.Canvas_View.Objects.Toplevel (D);
      P  : Model_Point;
      Item : Abstract_Item;
      Result : Anchors;
      L  : Gdouble;
   begin
      if ST = null then
         ST := S;
      end if;

      if DT = null then
         DT := D;
      end if;

      Result :=
        (From  =>
           (P        => S.Item_To_Model
              (S.Link_Anchor_Point (Self.Anchor_From)),
            Box      => S.Model_Bounding_Box,
            Toplevel => ST.Model_Bounding_Box),
         To    =>
           (P        => D.Item_To_Model
              (D.Link_Anchor_Point (Self.Anchor_To)),
            Box      => D.Model_Bounding_Box,
            Toplevel => DT.Model_Bounding_Box));

      --  Clip the line to the side of the toplevel boxes.

      if Self.Anchor_From.Toplevel_Side = Auto then

         --  Find the topmost non-invisible parent
         Item := S;
         ST := Item;
         while Item /= null loop
            if not Item.Is_Invisible then
               ST := Item;
            end if;
            Item := Item.Parent;
         end loop;
         Result.From.Toplevel := ST.Model_Bounding_Box;

         if Self.Waypoints = null then
            P := Result.To.P;
         elsif Self.Relative_Waypoints then
            P := Result.From.P;
            P := Self.Item_To_Model
              ((P.X + Self.Waypoints (Self.Waypoints'First).X,
                P.Y + Self.Waypoints (Self.Waypoints'First).Y));
         else
            P := Self.Item_To_Model (Self.Waypoints (Self.Waypoints'First));
         end if;

         Result.From.P := ST.Item_To_Model
           (ST.Clip_Line
              (ST.Model_To_Item (Result.From.P), ST.Model_To_Item (P)));

         if Self.Anchor_From.Distance > 0.0 then
            L := Sqrt
              ((Result.From.P.X - P.X) * (Result.From.P.X - P.X)
               + (Result.From.P.Y - P.Y) * (Result.From.P.Y - P.Y));

            L := Self.Anchor_From.Distance / L;

            Result.From.P :=
              (Result.From.P.X + L * (P.X - Result.From.P.X),
               Result.From.P.Y + L * (P.Y - Result.From.P.Y));
         end if;
      end if;

      if Self.Anchor_To.Toplevel_Side = Auto then

         --  Find the topmost non-invisible parent
         Item := D;
         DT := Item;
         while Item /= null loop
            if not Item.Is_Invisible then
               DT := Item;
            end if;
            Item := Item.Parent;
         end loop;
         Result.To.Toplevel := DT.Model_Bounding_Box;

         if Self.Waypoints = null then
            P := Result.From.P;
         elsif Self.Relative_Waypoints then
            P := Relative_To_Item (Result.From.P, Self.Waypoints.all);
         else
            P := Self.Item_To_Model (Self.Waypoints (Self.Waypoints'Last));
         end if;

         Result.To.P := DT.Item_To_Model
           (DT.Clip_Line
              (DT.Model_To_Item (Result.To.P), DT.Model_To_Item (P)));

         if Self.Anchor_To.Distance > 0.0 then
            L := Sqrt
              ((Result.To.P.X - P.X) * (Result.To.P.X - P.X)
               + (Result.To.P.Y - P.Y) * (Result.To.P.Y - P.Y));

            L := Self.Anchor_To.Distance / L;

            Result.To.P :=
              (Result.To.P.X + L * (P.X - Result.To.P.X),
               Result.To.P.Y + L * (P.Y - Result.To.P.Y));
         end if;
      end if;

      return Result;
   end Compute_Anchors;

   --------------------------
   -- Compute_Bounding_Box --
   --------------------------

   function Compute_Bounding_Box
     (Points   : Item_Point_Array;
      Relative : Boolean := False) return Item_Rectangle
   is
      Max_X : Gdouble := Points (Points'First).X;
      Max_Y : Gdouble := Points (Points'First).Y;
      X     : Gdouble := Max_X;
      Y     : Gdouble := Max_Y;
      Current : Item_Point := Points (Points'First);
   begin
      for P1 in Points'First + 1 .. Points'Last loop
         if Relative then
            Current := (Current.X + Points (P1).X,
                        Current.Y + Points (P1).Y);
         else
            Current := Points (P1);
         end if;

         X := Gdouble'Min (X, Current.X);
         Y := Gdouble'Min (Y, Current.Y);
         Max_X := Gdouble'Max (Max_X, Current.X);
         Max_Y := Gdouble'Max (Max_Y, Current.Y);
      end loop;

      return (X => X, Y => Y, Width => Max_X - X, Height => Max_Y - Y);
   end Compute_Bounding_Box;

   --------------------
   -- Compute_Labels --
   --------------------

   procedure Compute_Labels
     (Self    : not null access Canvas_Link_Record'Class;
      Context : Draw_Context;
      Dim     : Anchors)
   is
      P : constant Item_Point_Array_Access := Self.Points;

      procedure Place_End_Label
        (Label    : Container_Item;
         P1, P2   : Item_Point;
         Toplevel : Model_Rectangle);
      procedure Place_End_Label
        (Label    : Container_Item;
         P1, P2   : Item_Point;
         Toplevel : Model_Rectangle)
      is
         Margin : constant Gdouble := 3.0;   --  extra margin around the box
         Deltax : constant Gdouble := P2.X - P1.X;
         Deltay : constant Gdouble := P2.Y - P1.Y;
         D      : Gdouble := 1.0;
         X1, X2, Y1, Y2, Tmp : Gdouble;
      begin
         Label.Size_Request (Context);

         X1 := (Toplevel.X + Margin + Toplevel.Width - P1.X) / Deltax;
         X2 := (Toplevel.X - Margin - P1.X - Label.Width) / Deltax;
         Y1 := (Toplevel.Y + Margin + Toplevel.Height - P1.Y) / Deltay;
         Y2 := (Toplevel.Y - Margin - P1.Y - Label.Height) / Deltay;

         if Deltax < 0.0 then
            Tmp := X1;
            X1 := X2;
            X2 := Tmp;
         end if;

         if Deltay < 0.0 then
            Tmp := Y1;
            Y1 := Y2;
            Y2 := Y1;
         end if;

         if 0.0 <= X1 and then X1 <= 1.0 then
            D := X1;
         end if;

         if 0.0 <= X2 and then X2 <= 1.0 then
            D := Gdouble'Min (X2, D);
         end if;

         if 0.0 <= Y1 and then Y1 <= 1.0 then
            D := Gdouble'Min (Y1, D);
         end if;

         if 0.0 <= Y2 and then Y2 <= 1.0 then
            D := Gdouble'Min (Y2, D);
         end if;

         Label.Computed_Position :=
           (X => P1.X + D * Deltax, Y => P1.Y + D * Deltay);

         --  A small additional offset: the text is now outside of the box, we
         --  also want it outside of the link.

         if abs (Deltax) < abs (Deltay) then
            --  link is mostly vertical, so offset is horizontal
            Label.Computed_Position.X := Label.Computed_Position.X + 4.0;
         else
            Label.Computed_Position.Y := Label.Computed_Position.Y + 4.0;
         end if;

         Label.Size_Allocate;
      end Place_End_Label;

      Deltax, Deltay : Gdouble;
      Idx : Integer;
   begin
      --  Sets text.x and text.y to a position appropriate to show the label
      --  so that it doesn't intersect the from and to boxes. We are moving
      --  the text box along the line deltax, deltay. Basically, we are looking
      --  for d such that the two bounding boxes for the text and "from" do no
      --  intersect.
      --
      --     0 <= d <= 1    (position along the line)
      --     x + d * deltax > from.x + from.w
      --     || from.x > x + d * deltax + label.w
      --     || y + d * deltay > from.y + from.h
      --     || from.y > y + d * deltay + label.h
      --
      --  Which provides:
      --     0 <= d <= 1
      --     && (d * deltax < from.x - x - w
      --         || d * deltax > from.x + from.w - x
      --         || d * deltay < from.y - y -h
      --         || d * deltay > from.y + from.h - y)
      --
      --  We take the lowest d that satisfies any of the criteria, to keep the
      --  text closer to the box.

      if Self.Label_From /= null then
         Place_End_Label
           (Self.Label_From, P (P'First), P (P'First + 1), Dim.From.Toplevel);
      end if;

      if Self.Label_To /= null then
         Place_End_Label
           (Self.Label_To, P (P'Last), P (P'Last - 1), Dim.To.Toplevel);
      end if;

      if Self.Label /= null then
         if Self.Label.all in Text_Item_Record'Class
           and then Text_Item (Self.Label).Directed /= No_Text_Arrow
         then
            Deltax := P (P'First).X - P (P'Last).X;
            Deltay := P (P'First).Y - P (P'Last).Y;

            if abs (Deltax) > abs (Deltay) then
               if Deltax > 0.0 then
                  Text_Item (Self.Label).Directed := Left_Text_Arrow;
               else
                  Text_Item (Self.Label).Directed := Right_Text_Arrow;
               end if;

            else
               if Deltay > 0.0 then
                  Text_Item (Self.Label).Directed := Up_Text_Arrow;
               else
                  Text_Item (Self.Label).Directed := Down_Text_Arrow;
               end if;
            end if;
         end if;

         Self.Label.Size_Request (Context);

         --  Place the label in the middle of the middle segment.

         Idx := P'First + P'Length / 2 - 1;
         Self.Label.Computed_Position :=
           (X => (P (Idx).X + P (Idx + 1).X) / 2.0 - Self.Label.Width / 2.0,
            Y => (P (Idx).Y + P (Idx + 1).Y) / 2.0 - Self.Label.Height / 2.0);
         Self.Label.Size_Allocate;
      end if;
   end Compute_Labels;

   ----------
   -- Sort --
   ----------

   procedure Sort (Arr : in out Gdouble_Array) is
      --  A simple bubble sort
      Switched : Boolean;
      Tmp      : Gdouble;
   begin
      loop
         Switched := False;

         for J in Arr'First .. Arr'Last - 1 loop
            if Arr (J + 1) < Arr (J) then
               Tmp := Arr (J);
               Arr (J) := Arr (J + 1);
               Arr (J + 1) := Tmp;
               Switched := True;
            end if;
         end loop;

         exit when not Switched;
      end loop;
   end Sort;

   ----------------------------------------
   -- Compute_Layout_For_Orthogonal_Link --
   ----------------------------------------

   procedure Compute_Layout_For_Orthogonal_Link
     (Link    : not null access Canvas_Link_Record'Class;
      Context : Draw_Context)
   is
   begin
      Orthogonal_Waypoints
        (Link, Context, Min_Margin => 6.0, Max_Margin => 25.0);
   end Compute_Layout_For_Orthogonal_Link;

   -------------------
   -- Compute_Sides --
   -------------------

   procedure Compute_Sides
     (Link   : not null access Canvas_Link_Record'Class;
      Anchor : Anchor_Attachment;
      Info   : End_Info;
      Margin : Margins;
      P, P2  : out Item_Point)
   is
      Effective_Side : Side_Attachment;
   begin
      case Anchor.Toplevel_Side is
         when No_Clipping =>
            P := Link.Model_To_Item (Info.P);
            return;
         when Top | Bottom | Right | Left =>
            Effective_Side := Anchor.Toplevel_Side;
         when Auto =>
            if abs (Info.P.X - Info.Toplevel.X) < 0.001 then
               Effective_Side := Left;
            elsif abs (Info.P.Y - Info.Toplevel.Y) < 0.001 then
               Effective_Side := Top;
            elsif abs (Info.P.X - Info.Toplevel.X - Info.Toplevel.Width)
              < 0.001
            then
               Effective_Side := Right;
            else
               Effective_Side := Bottom;
            end if;
      end case;

      case Effective_Side is
         when Top =>
            P := (Info.Box.X + Info.Box.Width * abs (Anchor.X),
                  Info.Toplevel.Y);
            P2 := (P.X, P.Y - Margin.Top);
         when Bottom =>
            P := (Info.Box.X + Info.Box.Width * abs (Anchor.X),
                  Info.Toplevel.Y + Info.Toplevel.Height);
            P2 := (P.X, P.Y + Margin.Bottom);
         when Right =>
            P := (Info.Toplevel.X + Info.Toplevel.Width,
                  Info.Box.Y + Info.Box.Height * abs (Anchor.Y));
            P2 := (P.X + Margin.Right, P.Y);
         when Left =>
            P := (Info.Toplevel.X,
                  Info.Box.Y + Info.Box.Height * abs (Anchor.Y));
            P2 := (P.X - Margin.Left, P.Y);
         when others =>
            null;
      end case;
   end Compute_Sides;

   --------------------------
   -- Orthogonal_Waypoints --
   --------------------------

   procedure Orthogonal_Waypoints
     (Link       : not null access Canvas_Link_Record'Class;
      Context    : Draw_Context;
      Min_Margin : Gdouble;
      Max_Margin : Gdouble)
   is
      Min_Space : constant Gdouble := Link.Style.Get_Line_Width * 3.0;
      --  Minimal space between two boxes to pass a link between them

      Dim : constant Anchors := Compute_Anchors (Link);

      FTX1 : constant Gdouble := Dim.From.Toplevel.X;  --  from-top-x1
      FTX2 : constant Gdouble := FTX1 + Dim.From.Toplevel.Width;
      FTY1 : constant Gdouble := Dim.From.Toplevel.Y;  --  from-top-y1
      FTY2 : constant Gdouble := FTY1 + Dim.From.Toplevel.Height;

      TTX1 : constant Gdouble := Dim.To.Toplevel.X;  --  from-top-x1
      TTX2 : constant Gdouble := TTX1 + Dim.To.Toplevel.Width;
      TTY1 : constant Gdouble := Dim.To.Toplevel.Y;  --  from-top-y1
      TTY2 : constant Gdouble := TTY1 + Dim.To.Toplevel.Height;

      procedure Margins_Between_Items
        (Item1, Item2     : Model_Rectangle;
         Margin1, Margin2 : out Margins);
      --  When computing the layout for orthogonal links, we add some borders
      --  around the items, through which the link might go. These borders are
      --  larger between the two items.

      ---------------------------
      -- Margins_Between_Items --
      ---------------------------

      procedure Margins_Between_Items
        (Item1, Item2     : Model_Rectangle;
         Margin1, Margin2 : out Margins)
      is
         Dist, Tmp : Gdouble;
      begin
         if Item1.X + Item1.Width < Item2.X then
            Dist := Item2.X - Item1.X - Item1.Width;
            Margin1.Left := Min_Margin;
            Tmp := Gdouble'Max (Dist / 2.0, Min_Margin);
            Margin1.Right := Gdouble'Min (Max_Margin, Tmp);
            Margin2.Left := Dist - Margin1.Right;
            Margin2.Right := Min_Margin;

         elsif Item2.X + Item2.Width < Item1.X then
            Dist := Item1.X - Item2.X - Item2.Width;
            Margin1.Right := Min_Margin;
            Tmp := Gdouble'Max (Dist / 2.0, Min_Margin);
            Margin1.Left := Gdouble'Min (Max_Margin, Tmp);
            Margin2.Right := Dist - Margin1.Left;
            Margin2.Left := Min_Margin;

         else
            Margin1.Left := Min_Margin;
            Margin1.Right := Min_Margin;
            Margin2.Left := Min_Margin;
            Margin2.Right := Min_Margin;
         end if;

         if Item1.Y + Item1.Height < Item2.Y then
            Dist := Item2.Y - Item1.Y - Item1.Height;
            Margin1.Top := Min_Margin;
            Tmp := Gdouble'Max (Dist / 2.0, Min_Margin);
            Margin1.Bottom := Gdouble'Min (Max_Margin, Tmp);
            Margin2.Top := Dist - Margin1.Bottom;
            Margin2.Bottom := Min_Margin;

         elsif Item2.Y + Item2.Height < Item1.Y then
            Dist := Item1.Y - Item2.Y - Item2.Height;
            Margin1.Bottom := Min_Margin;
            Tmp := Gdouble'Max (Dist / 2.0, Min_Margin);
            Margin1.Top := Gdouble'Min (Max_Margin, Tmp);
            Margin2.Bottom := Dist - Margin1.Top;
            Margin2.Top := Min_Margin;

         else
            Margin1.Top := Min_Margin;
            Margin1.Bottom := Min_Margin;
            Margin2.Top := Min_Margin;
            Margin2.Bottom := Min_Margin;
         end if;
      end Margins_Between_Items;

      From, To : Item_Point;
      M      : Item_Point;
      P_From : Item_Point;   --  extending from the From box
      P_To   : Item_Point;   --  extending from the To box
      C1, C2, C3 : Coordinate;
      Matrix : Layout_Matrix;
      Margin_From, Margin_To : Margins;

   begin
      Margins_Between_Items
        (Dim.From.Toplevel, Dim.To.Toplevel, Margin_From, Margin_To);
      Compute_Sides
        (Link, Link.Anchor_From, Dim.From, Margin_From, From, P_From);
      Compute_Sides
        (Link, Link.Anchor_To, Dim.To, Margin_To, To, P_To);

      Unchecked_Free (Link.Points);

      if Link.Waypoints /= null then
         if Link.Relative_Waypoints then
            Link.Points := new Item_Point_Array'
              (From & Relative_To_Array (From, Link.Waypoints.all) & To);
         else
            Link.Points := new Item_Point_Array'
              (From & Link.Waypoints.all & To);
         end if;

         --  Is the final link supposed to be vertical ?
         --  And can we make it fully vertical, if the user requested it ?

         if abs (To.X - P_To.X) < 0.001
           and then Link.Anchor_To.X < 0.0
           and then Link.Points (Link.Points'Last - 1).X >= Dim.To.Box.X
           and then Link.Points (Link.Points'Last - 1).X <=
              Dim.To.Box.X + Dim.To.Box.Width
         then
            Link.Points (Link.Points'Last).X :=
              Link.Points (Link.Points'Last - 1).X;

         --  Is the final link supposed to be horizontal ?
         --  And can we make it fully vertical, if the user requested it ?

         elsif abs (To.Y - P_To.Y) < 0.001
           and then Link.Anchor_To.Y < 0.0
           and then Link.Points (Link.Points'Last - 1).Y >= Dim.To.Box.Y
           and then Link.Points (Link.Points'Last - 1).Y <=
              Dim.To.Box.Y + Dim.To.Box.Height
         then
            Link.Points (Link.Points'Last).Y :=
              Link.Points (Link.Points'Last - 1).Y;
         end if;

         Link.Bounding_Box := Compute_Bounding_Box (Link.Points.all);
         Compute_Labels (Link, Context, Dim);

         return;
      end if;

      --  We want to find a line that joins P_From to P_To, with the minimum
      --  number of bends. For this, there are in fact three possible X
      --  coordinates for each box (and same for Y):
      --          FTX1 - B, F.X, FTX2 + B
      --                TTX1 - B, T.X, TTX2 + B
      --
      --  When there is enough space between the two boxes, we can replace
      --  the two lines between the items with a single one in the middle.
      --  This gives use 5 or 6 possible X coordinates for vertical segments.
      --  Some of these cannot be used to go all the way down because they
      --  cross one of the two boxes.
      --  Also, P_From and P_To are aligned on that grid, so basically we need
      --  to find a path between two vertices of the grid.

      if FTX2 + Min_Space <= TTX1 then
         M.X := (FTX2 + TTX1) / 2.0;
      elsif TTX2 + Min_Space <= FTX1 then
         M.X := (FTX1 + TTX2) / 2.0;
      else
         M.X := Gdouble'Last;  --  no valid vertical line in between
      end if;

      if FTY2 + Min_Space <= TTY1 then
         M.Y := (FTY2 + TTY1) / 2.0;
      elsif TTY2 + Min_Space <= FTY1 then
         M.Y := (FTY1 + TTY2) / 2.0;
      else
         M.Y := Gdouble'Last;  --  no valid horizontal line in between
      end if;

      Matrix :=
        (Dim => Dim,
         X => ((FTX1 - Margin_From.Left,  From.X, FTX2 + Margin_From.Right,
                TTX1 - Margin_To.Left,    To.X,   TTX2 + Margin_To.Right,
                M.X)),
         Y => ((FTY1 - Margin_From.Top,   From.Y, FTY2 + Margin_From.Bottom,
                TTY1 - Margin_To.Top,     To.Y,   TTY2 + Margin_To.Bottom,
                M.Y)));
      Sort (Matrix.X);
      Sort (Matrix.Y);

      for J in Matrix.X'Range loop
         if abs (Matrix.X (J) - P_From.X) < 0.01 then
            C1.X := J;
         end if;
         if abs (Matrix.X (J) - P_To.X) < 0.01 then
            C2.X := J;
         end if;
         if abs (Matrix.X (J) - From.X) < 0.01 then
            C3.X := J;
         end if;

         if abs (Matrix.Y (J) - P_From.Y) < 0.01 then
            C1.Y := J;
         end if;
         if abs (Matrix.Y (J) - P_To.Y) < 0.01 then
            C2.Y := J;
         end if;
         if abs (Matrix.Y (J) - From.Y) < 0.01 then
            C3.Y := J;
         end if;
      end loop;

      --  Compute the shortest path between the two points C1 and C2. We also
      --  do some post-processing to limit the number of segments (which in
      --  turn speeds up drawing and click

      declare
         Path   : constant Coordinate_Array :=
           Astar_Find (Matrix, C1, C2, Parent => C3);
         Points : Item_Point_Array (1 .. 4 + Path'Length);
         P      : Integer;
         Along_X : Boolean;

         procedure Add_Point (New_P : Item_Point);
         --  Add a new point to the final path, or merge with the previous
         --  point if possible

         procedure Add_Point (New_P : Item_Point) is
            Tmp : Boolean;
         begin
            Tmp := abs (New_P.X - Points (P - 1).X) < 0.01;
            if Tmp = Along_X then
               --  both along the X axis => merge points
               Points (P - 1) := New_P;
            else
               --  Validate the new point
               Along_X := Tmp;
               Points (P) := New_P;
               P := P + 1;
            end if;
         end Add_Point;

      begin
         Points (Points'First) := From;
         P := Points'First + 1;
         Points (P) := P_From;
         Along_X := abs (Points (P).X - Points (P - 1).X) < 0.01;
         P := P + 1;

         for C in Path'First + 1 .. Path'Last loop
            Add_Point ((Matrix.X (Path (C).X), Matrix.Y (Path (C).Y)));
         end loop;

         Add_Point (To);

         Link.Points := new Item_Point_Array'(Points (Points'First .. P - 1));
      end;

      Link.Bounding_Box := Compute_Bounding_Box (Link.Points.all);
      Compute_Labels (Link, Context, Dim);
   end Orthogonal_Waypoints;

   --------------------------------------
   -- Compute_Layout_For_Straight_Link --
   --------------------------------------

   procedure Compute_Layout_For_Straight_Link
     (Link    : not null access Canvas_Link_Record'Class;
      Context : Draw_Context)
   is
      Dim : constant Anchors := Compute_Anchors (Link);
      Tmp, Tmp2 : Item_Point;
      Pos : Item_Coordinate;
   begin
      if Link.From = Link.To then
         Tmp := (Dim.From.Toplevel.X + Dim.From.Toplevel.Width,
                 Dim.From.Toplevel.Y);
         Unchecked_Free (Link.Points);
         Link.Points := new Item_Point_Array'
           ((Tmp.X - 10.0, Tmp.Y),
            (Tmp.X - 10.0, Tmp.Y - 10.0),
            (Tmp.X + 10.0, Tmp.Y - 10.0),
            (Tmp.X + 10.0, Tmp.Y + 10.0),
            (Tmp.X, Tmp.Y + 10.0));

      elsif Link.Waypoints /= null then
         Unchecked_Free (Link.Points);
         Tmp := Link.Model_To_Item (Dim.To.P);

         if Link.Relative_Waypoints then
            Tmp2 := Link.Model_To_Item (Dim.From.P);
            Link.Points := new Item_Point_Array'
              (Tmp2 & Relative_To_Array (Tmp2, Link.Waypoints.all) & Tmp);
         else
            Link.Points := new Item_Point_Array'
              (Link.Model_To_Item (Dim.From.P) & Link.Waypoints.all & Tmp);
         end if;

      else
         Unchecked_Free (Link.Points);
         Link.Points := new Item_Point_Array'
           (Link.Model_To_Item (Dim.From.P)
            & Link.Model_To_Item (Dim.To.P));
      end if;

      --  Is the user asking to get the link fully vertical when possible.
      --  This only applies to the last segment.

      if Link.Anchor_To.X < 0.0
        or else Link.Anchor_To.Y < 0.0
      then
         Tmp2 := Link.Points (Link.Points'Last);
         Tmp := Link.Points (Link.Points'Last - 1);

         if Link.Anchor_To.X < 0.0
           and then Tmp.X >= Dim.To.Box.X
           and then Tmp.X <= Dim.To.Box.X + Dim.To.Box.Width
         then
            Pos := (Tmp.X + Tmp2.X) / 2.0;
            Link.Points (Link.Points'Last - 1).X := Pos;
            Link.Points (Link.Points'Last).X := Pos;

         elsif Link.Anchor_To.Y < 0.0
           and then Tmp.Y >= Dim.To.Box.Y
           and then Tmp.Y <= Dim.To.Box.Y + Dim.To.Box.Height
         then
            Pos := (Tmp.Y + Tmp2.Y) / 2.0;
            Link.Points (Link.Points'Last - 1).Y := Pos;
            Link.Points (Link.Points'Last).Y := Pos;
         end if;
      end if;

      Link.Bounding_Box := Compute_Bounding_Box (Link.Points.all);
      Compute_Labels (Link, Context, Dim);
   end Compute_Layout_For_Straight_Link;

   ---------------------------------
   -- Compute_Layout_For_Arc_Link --
   ---------------------------------

   procedure Compute_Layout_For_Arc_Link
     (Link    : not null access Canvas_Link_Record'Class;
      Context : Draw_Context;
      Offset  : Gdouble := 10.0)
   is
      Dim : constant Anchors := Compute_Anchors (Link);

      function Get_Wp return Item_Point_Array;
      --  Return the waypoints to use

      procedure Control_Points
        (From, To : Item_Point; Ctrl1, Ctrl2 : out Item_Point);
      --  Compute the control points on the curve from From to To

      ------------
      -- Get_Wp --
      ------------

      function Get_Wp return Item_Point_Array is
      begin
         if Link.From = Link.To then
            return No_Waypoints;
         elsif Link.Waypoints /= null then
            return Link.Waypoints.all;
         else
            return No_Waypoints;
         end if;
      end Get_Wp;

      --------------------
      -- Control_Points --
      --------------------

      procedure Control_Points
        (From, To : Item_Point; Ctrl1, Ctrl2 : out Item_Point)
      is
         Dx : Gdouble := To.X - From.X;
         Dy : Gdouble := To.Y - From.Y;
         D : constant Gdouble := Sqrt (Dx * Dx + Dy * Dy);
         E : constant Gdouble := abs (Offset) / D;
      begin
         Dx := Dx * E;
         Dy := Dy * E;

         if Offset > 0.0 then
            Ctrl1 := (From.X - Dy, From.Y + Dx);
            Ctrl2 := (To.X - Dy,   To.Y + Dx);
         else
            Ctrl1 := (From.X + Dy, From.Y - Dx);
            Ctrl2 := (To.X + Dy,   To.Y - Dx);
         end if;
      end Control_Points;

      Waypoints : constant Item_Point_Array := Get_Wp;
      P1, P2, P3, P4 : Item_Point;
      FP, TP, Dummy : Item_Point;
      Radius, Tmp : Gdouble;
   begin
      Unchecked_Free (Link.Points);

      if Link.From = Link.To then
         Tmp := Gdouble'Min (abs (Offset), Dim.From.Toplevel.Width / 2.0);
         Radius := Gdouble'Min (Tmp, Dim.From.Toplevel.Height / 2.0);

         Link.Points := new Item_Point_Array'
           (Circle_From_Bezier
              (Center => Link.Model_To_Item
                   ((Dim.From.Toplevel.X + Dim.From.Toplevel.Width,
                    Dim.From.Toplevel.Y)),
               Radius => Radius));

      else
         Compute_Sides
           (Link, Link.Anchor_From, Dim.From, No_Margins, FP, Dummy);
         Compute_Sides
           (Link, Link.Anchor_To, Dim.To, No_Margins, TP, Dummy);

         if Waypoints'Length = 0 then
            Control_Points (FP, TP, P1, P2);
            Link.Points := new Item_Point_Array'((FP, P1, P2, TP));
         else
            Control_Points (FP, Waypoints (Waypoints'First), P1, P2);
            Control_Points (Waypoints (Waypoints'Last), TP, P3, P4);
            Link.Points := new Item_Point_Array'
              (Item_Point_Array'(FP, P1, P2)
               & Waypoints
               & Item_Point_Array'(P3, P4, TP));
         end if;
      end if;

      Link.Bounding_Box := Compute_Bounding_Box
        (Link.Points.all, Relative => False);
      Compute_Labels (Link, Context, Dim);
   end Compute_Layout_For_Arc_Link;

   -----------------------------------
   -- Compute_Layout_For_Curve_Link --
   -----------------------------------

   procedure Compute_Layout_For_Curve_Link
     (Link    : not null access Canvas_Link_Record'Class;
      Context : Draw_Context)
   is
      P : Item_Point_Array_Access;
      L : Integer;
   begin
      Orthogonal_Waypoints
        (Link, Context, Min_Margin => 6.0, Max_Margin => Gdouble'Last);

      P := Link.Points;

      if P'Length <= 2 then
         null;  --  degenerate case, handled in Draw_Link
      elsif P'Length = 3 then
         Link.Points := new Item_Point_Array'
           ((P (P'First), P (P'First + 1), P (P'First + 1), P (P'Last)));
         Unchecked_Free (P);
      elsif P'Length = 4 then
         null;
      else
         --  Create intermediate points in the middle of segments

         Link.Points := new Item_Point_Array (1 .. 3 * P'Length - 5);
         Link.Points (Link.Points'First) := P (P'First);
         L := Link.Points'First + 1;

         for P2 in P'First + 1 .. P'Last - 1 loop
            if P2 = P'Last - 1 then
               Link.Points (L + 2) := P (P2 + 1);
            else
               Link.Points (L + 2) :=          --  a new intermediate point
                 ((P (P2).X + P (P2 + 1).X) / 2.0,
                  (P (P2).Y + P (P2 + 1).Y) / 2.0);
            end if;

            --  The control points are 1/3 of the way.

            Link.Points (L) :=
              (P (P2).X * 2.0 / 3.0 + Link.Points (L - 1).X * 1.0 / 3.0,
               P (P2).Y * 2.0 / 3.0 + Link.Points (L - 1).Y * 1.0 / 3.0);
            Link.Points (L + 1) :=
              (P (P2).X * 1.0 / 3.0 + Link.Points (L + 2).X * 2.0 / 3.0,
               P (P2).Y * 1.0 / 3.0 + Link.Points (L + 2).Y * 2.0 / 3.0);

            L := L + 3;
         end loop;

         Unchecked_Free (P);
      end if;
   end Compute_Layout_For_Curve_Link;

   ------------------
   -- Prepare_Path --
   ------------------

   function Prepare_Path
     (Link    : not null access Canvas_Link_Record'Class;
      Context : Draw_Context) return Boolean
   is
      P    : constant Item_Point_Array_Access := Link.Points;
   begin
      case Link.Routing is
         when Straight | Orthogonal =>
            return Link.Style.Path_Polyline (Context.Cr, P.all);

         when Curve =>
            if P'Length = 2 then
               return Link.Style.Path_Polyline (Context.Cr, P.all);
            else
               return Link.Style.Path_Polycurve (Context.Cr, P.all);
            end if;

         when Arc =>
            return Link.Style.Path_Polycurve (Context.Cr, P.all);
      end case;
   end Prepare_Path;

   ---------------
   -- Draw_Link --
   ---------------

   procedure Draw_Link
     (Link     : not null access Canvas_Link_Record'Class;
      Context  : Draw_Context;
      Selected : Boolean)
   is
      P    : constant Item_Point_Array_Access := Link.Points;
      Fill : Cairo_Pattern;
      S    : Drawing_Style;
   begin
      pragma Assert (P /= null, "waypoints must be computed first");
      pragma Assert (P'Length >= 2, "no enough waypoints");

      if Prepare_Path (Link, Context) then
         --  never fill a link
         Fill := Link.Style.Get_Fill;
         Link.Style.Set_Fill (Null_Pattern);
         Link.Style.Finish_Path (Context.Cr);

         case Link.Routing is
            when Arc =>
               if P'Length >= 4 then
                  Link.Style.Draw_Arrows_And_Symbols
                    (Context.Cr,
                     (P (P'First),
                      --  middle of the two control points (approximate)
                      ((P (P'First + 1).X + P (P'First + 2).X) / 2.0,
                       (P (P'First + 1).Y + P (P'First + 2).Y) / 2.0),

                      ((P (P'Last - 1).X + P (P'Last - 2).X) / 2.0,
                       (P (P'Last - 1).Y + P (P'Last - 2).Y) / 2.0),
                      P (P'Last)));
               else
                  Link.Style.Draw_Arrows_And_Symbols (Context.Cr, P.all);
               end if;

            when others =>
               Link.Style.Draw_Arrows_And_Symbols (Context.Cr, P.all);
         end case;

         Link.Style.Set_Fill (Fill);

         if Selected
           and then Context.View /= null
         then
            S := Link.Style;
            Link.Style := Context.View.Selection_Style;

            if Prepare_Path (Link, Context) then
               Fill := Link.Style.Get_Fill;
               Link.Style.Set_Fill (Null_Pattern);
               Link.Style.Finish_Path (Context.Cr);
               Link.Style.Draw_Arrows_And_Symbols (Context.Cr, P.all);
               Link.Style.Set_Fill (Fill);
            end if;

            Link.Style := S;
         end if;
      end if;

      if Link.Label /= null then
         Link.Label.Translate_And_Draw_Item (Context);
      end if;

      if Link.Label_From /= null then
         Link.Label_From.Translate_And_Draw_Item (Context);
      end if;

      if Link.Label_To /= null then
         Link.Label_To.Translate_And_Draw_Item (Context);
      end if;
   end Draw_Link;

   ------------------------
   -- Circle_From_Bezier --
   ------------------------

   function Circle_From_Bezier
     (Center   : Item_Point;
      Radius   : Glib.Gdouble) return Item_Point_Array
   is
      --  Magic number comes from several articles on the web, including
      --    http://www.charlespetzold.com/blog/2012/12/
      --       Bezier-Circles-and-Bezier-Ellipses.html

      P : Item_Point_Array (1 .. 13);
      R : constant Gdouble := Radius * 0.55;
   begin
      P (P'First + 0)  := (Center.X,          Center.Y - Radius);

      P (P'First + 1)  := (Center.X + R,      Center.Y - Radius);
      P (P'First + 2)  := (Center.X + Radius, Center.Y - R);

      P (P'First + 3)  := (Center.X + Radius, Center.Y);

      P (P'First + 4)  := (Center.X + Radius, Center.Y + R);
      P (P'First + 5)  := (Center.X + R,      Center.Y + Radius);

      P (P'First + 6)  := (Center.X,          Center.Y + Radius);

      P (P'First + 7)  := (Center.X - R,      Center.Y + Radius);
      P (P'First + 8)  := (Center.X - Radius, Center.Y + R);

      P (P'First + 9)  := (Center.X - Radius, Center.Y);

      P (P'First + 10) := (Center.X - Radius, Center.Y - R);
      P (P'First + 11) := (Center.X - R,      Center.Y - Radius);

      P (P'First + 12) := (Center.X, Center.Y - Radius);

      return P;

      --  For quadratic bezier curves, we could have used:
      --  See http://texdoc.net/texmf-dist/doc/latex/lapdf/rcircle.pdf
   end Circle_From_Bezier;

end Gtkada.Canvas_View.Links;
