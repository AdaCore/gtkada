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

with Ada.Numerics.Generic_Elementary_Functions;
with Cairo;                    use Cairo;
with Gdk.RGBA;                 use Gdk.RGBA;
with Glib;                     use Glib;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtkada.Canvas_View;       use Gtkada.Canvas_View;
with Gtkada.Canvas_View.Views; use Gtkada.Canvas_View.Views;
with Gtkada.Style;             use Gtkada.Style;
with Pango.Font;               use Pango.Font;

package body Create_Canvas_View_Links is

   package Gdouble_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (Gdouble);
   use Gdouble_Functions;

   type Spring_Link_Record is new Canvas_Link_Record with null record;
   type Spring_Link is access all Spring_Link_Record'Class;
   --  A special type of link which draws itself as a spring

   function Gtk_New_Spring
     (From, To  : not null access Abstract_Item_Record'Class;
      Style     : Drawing_Style;
      Anchor_From : Anchor_Attachment := Middle_Attachment;
      Anchor_To   : Anchor_Attachment := Middle_Attachment)
      return Spring_Link;
   overriding procedure Draw
     (Self    : not null access Spring_Link_Record;
      Context : Draw_Context);

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
        & " the examples here."
        & ASCII.LF
        & "Custom links can be implemented relatively easily. This demo shows"
        & " for instance that displays a @bspring@B instead of a line."
        & ASCII.LF
        & "It is also possible to force specific @bwaypoints@B for links";
   end Help;

   --------------------
   -- Gtk_New_Spring --
   --------------------

   function Gtk_New_Spring
     (From, To  : not null access Abstract_Item_Record'Class;
      Style     : Drawing_Style;
      Anchor_From : Anchor_Attachment := Middle_Attachment;
      Anchor_To   : Anchor_Attachment := Middle_Attachment)
      return Spring_Link
   is
      L : constant Spring_Link := new Spring_Link_Record;
   begin
      L.Initialize
        (From, To, Style, Straight, null, Anchor_From, null, Anchor_To, null);
      return L;
   end Gtk_New_Spring;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self    : not null access Spring_Link_Record;
      Context : Draw_Context)
   is
      L : constant Gdouble := 12.0;    --  length of single segment
      N : constant Integer := 20;      --  number of segments
      Pad : constant Gdouble := 15.0;  --  each ends of the link

      Style  : constant Drawing_Style := Self.Get_Style;
      Fill   : constant Cairo_Pattern := Style.Get_Fill;
      Points : constant Item_Point_Array_Access := Self.Get_Points;
      P1     : constant Item_Point := Points (Points'First);
      P2     : constant Item_Point := Points (Points'Last);
      Deltax : constant Item_Coordinate := P2.X - P1.X;
      Deltay : constant Item_Coordinate := P2.Y - P1.Y;
      Angle  : Gdouble;
      Length : constant Gdouble := Sqrt (Deltax * Deltax + Deltay * Deltay);
      P      : Item_Point_Array (1 .. N + 4);
      X_Inc  : Gdouble;
      Y_Inc  : Gdouble;
      DY     : Gdouble;

   begin
      --  never fill a link
      Style.Set_Fill (Null_Pattern);

      if Length <= L * Gdouble (N)
        or else (Deltax = 0.0 and then Deltay = 0.0)
      then
         Angle := Arctan (Y => Deltay, X => Deltax);
         Translate
           (Context.Cr, Points (Points'First).X,
            Points (Points'First).Y);
         Rotate (Context.Cr, Angle);

         X_Inc := (Length - 2.0 * Pad) / Gdouble (N);
         Y_Inc := Sqrt (L * L - X_Inc * X_Inc);

         P (P'First)     := (0.0, 0.0);
         P (P'First + 1) := (Pad, 0.0);
         P (P'First + 2) := (X_Inc / 2.0, Y_Inc / 2.0);
         DY := -Y_Inc;

         for J in 3 .. N + 1 loop
            P (P'First + J) := (X_Inc, DY);
            DY := -DY;
         end loop;

         P (P'Last - 1) := (X_Inc / 2.0, DY / 2.0);
         P (P'Last) := (Pad, 0.0);

         Style.Draw_Polyline (Context.Cr, P, Relative => True);
      else
         Style.Draw_Polyline (Context.Cr, Points.all);
      end if;

      Style.Set_Fill (Fill);
   end Draw;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Canvas        : Canvas_View;
      Model         : List_Canvas_Model;
      Scrolled      : Gtk_Scrolled_Window;
      Black, Font   : Drawing_Style;

      procedure Do_Example (Routing : Route_Style; X, Y : Model_Coordinate);
      procedure Do_Example (Routing : Route_Style; X, Y : Model_Coordinate) is
         L1, L2        : Canvas_Link;
         It1, It2, It3 : Rect_Item;
         S             : Drawing_Style;
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
            Anchor_From => (1.0, 0.5, Auto, Distance => 0.0),
            Anchor_To   => (0.0, 0.5, Auto, Distance => 0.0));
         Model.Add (L1);

         S := Gtk_New
           (Stroke => (1.0, 0.0, 0.0, 1.0),
            Fill   => Create_Rgba_Pattern ((1.0, 0.0, 0.0, 1.0)),
            Arrow_From => (Head => Circle, Length => 2.0,
                           Fill => Black_RGBA, others => <>));

         L2 := Gtk_New
           (From => L1, To => It3, Style => S, Routing => Routing,
            Anchor_From => (0.5, 0.5, No_Clipping, Distance => 0.0));
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

      procedure Spring_Example
        (X, Y : Model_Coordinate; Style : Drawing_Style);
      procedure Spring_Example
        (X, Y : Model_Coordinate; Style : Drawing_Style)
      is
         It1, It2 : Rect_Item;
         L        : Spring_Link;
      begin
         It1 := Gtk_New_Rect (Black, 20.0, 20.0);
         It1.Set_Position ((X, Y));
         Model.Add (It1);

         It2 := Gtk_New_Rect (Black, 20.0, 20.0);
         It2.Set_Position ((X + 100.0, Y + 50.0));
         Model.Add (It2);

         L := Gtk_New_Spring (From => It1, To => It2, Style => Style);
         Model.Add (L);

         It1 := Gtk_New_Rect (Black, 20.0, 20.0);
         It1.Set_Position ((X + 300.0, Y));
         Model.Add (It1);

         L := Gtk_New_Spring (From => It2, To => It1, Style => Style);
         Model.Add (L);
      end Spring_Example;

      procedure Waypoints_Example
        (X, Y      : Model_Coordinate;
         Style     : Drawing_Style;
         Waypoints : Item_Point_Array;
         Relative_Waypoints : Boolean);
      procedure Waypoints_Example
        (X, Y      : Model_Coordinate;
         Style     : Drawing_Style;
         Waypoints : Item_Point_Array;
         Relative_Waypoints : Boolean)
      is
         It1, It2 : Rect_Item;
         L        : Canvas_Link;
      begin
         It1 := Gtk_New_Rect (Black, 20.0, 20.0);
         It1.Set_Position ((X, Y));
         Model.Add (It1);

         It2 := Gtk_New_Rect (Black, 20.0, 20.0);
         It2.Set_Position ((X + 200.0, Y + 50.0));
         Model.Add (It2);

         L := Gtk_New (It1, It2, Style, Straight);
         L.Set_Waypoints (Waypoints, Relative_Waypoints);
         Model.Add (L);
      end Waypoints_Example;

      procedure Label_Example
        (X, Y : Model_Coordinate; Style : Drawing_Style;
         Hide     : Boolean := False;
         Directed : Text_Arrow_Direction := No_Text_Arrow);
      procedure Label_Example
        (X, Y : Model_Coordinate; Style : Drawing_Style;
         Hide     : Boolean := False;
         Directed : Text_Arrow_Direction := No_Text_Arrow)
      is
         It1, It2 : Rect_Item;
         L        : Canvas_Link;
         Label, Label_From, Label_To : Text_Item;
      begin
         It1 := Gtk_New_Rect (Black, 20.0, 20.0);
         It1.Set_Position ((X, Y));
         Model.Add (It1);

         It2 := Gtk_New_Rect (Black, 20.0, 20.0);
         It2.Set_Position ((X + 200.0, Y + 50.0));
         Model.Add (It2);

         Label      := Gtk_New_Text (Style, "label", Directed);
         Label_From := Gtk_New_Text (Style, "labelFrom");
         Label_To   := Gtk_New_Text (Style, "labelTo");
         if Hide then
            Label.Hide;
            Label_From.Hide;
            Label_To.Hide;
         end if;
         L := Gtk_New (It1, It2, Black, Straight,
                       Label      => Label,
                       Label_From => Label_From,
                       Label_To   => Label_To);
         Model.Add (L);
      end Label_Example;

      procedure Distance_Example
        (X, Y : Model_Coordinate; Distance : Model_Coordinate);
      procedure Distance_Example
        (X, Y : Model_Coordinate; Distance : Model_Coordinate)
      is
         It1, It2, It3 : Rect_Item;
         L : Canvas_Link;
         S : Drawing_Style;
      begin
         It1 := Gtk_New_Rect (Black, 20.0, 20.0);
         It1.Set_Position ((X, Y));
         Model.Add (It1);

         It2 := Gtk_New_Rect (Black, 20.0, 20.0);
         It2.Set_Position ((X + 150.0, Y));
         Model.Add (It2);

         It3 := Gtk_New_Rect (Black, 20.0, 20.0);
         It3.Set_Position ((X + 75.0, Y + 75.0));
         Model.Add (It3);

         S := Gtk_New
           (Stroke => Black_RGBA,
            Arrow_From => (Head => Open, others => <>),
            Arrow_To   => (Head => Solid, others => <>));

         L := Gtk_New (It1, It2, S, Straight,
                       Anchor_From => (Distance => Distance, others => <>),
                       Anchor_To   => (Distance => Distance, others => <>));
         Model.Add (L);

         L := Gtk_New (It2, It3, S, Straight,
                       Anchor_From => (Distance => Distance, others => <>),
                       Anchor_To   => (Distance => Distance, others => <>));
         Model.Add (L);

         L := Gtk_New (It3, It1, S, Straight,
                       Anchor_From => (Distance => Distance, others => <>),
                       Anchor_To   => (Distance => Distance, others => <>));
         Model.Add (L);
      end Distance_Example;

      Text : Text_Item;
      Y : Gdouble;
   begin
      Gtk_New (Model);

      Black := Gtk_New;
      Font := Gtk_New (Stroke => Null_RGBA,
                       Font   => (Name => From_String ("sans 8"),
                                  others => <>));

      Text := Gtk_New_Text (Font, "Links attached to other links");
      Text.Set_Position ((0.0, -20.0));
      Model.Add (Text);

      Do_Example (Straight,   0.0, 0.0);
      Do_Example (Orthogonal, 150.0, 0.0);
      Do_Example (Arc,        300.0, 0.0);
      Do_Example (Curve,      450.0, 0.0);

      Text := Gtk_New_Text (Font, "Link arrows and symbols");
      Text.Set_Position ((0.0, 160.0));
      Model.Add (Text);

      Link_Example (0.0, 180.0,
                    Gtk_New (Arrow_To   => (Head   => Open,
                                            Fill   => Black_RGBA,
                                            others => <>),
                             Arrow_From => (Head   => Open,
                                            Length => 16.0,
                                            Angle  => 0.8,
                                            Stroke => (1.0, 0.0, 0.0, 1.0),
                                            Fill   => (1.0, 0.0, 0.0, 1.0),
                                            others => <>)));

      Link_Example (0.0, 220.0,
                    Gtk_New (Arrow_To   => (Head   => Solid,
                                            Fill   => Black_RGBA,
                                            others => <>),
                             Arrow_From => (Head   => Solid,
                                            Length => 16.0,
                                            Angle  => 0.8,
                                            Stroke => (1.0, 0.0, 0.0, 1.0),
                                            Fill   => (1.0, 0.0, 0.0, 1.0),
                                            others => <>)));

      Link_Example (0.0, 260.0,
                    Gtk_New (Arrow_To   => (Head   => Diamond,
                                            Fill   => Black_RGBA,
                                            others => <>),
                             Arrow_From => (Head   => Diamond,
                                            Length => 16.0,
                                            Angle  => 0.5,
                                            Stroke => (1.0, 0.0, 0.0, 1.0),
                                            Fill   => (1.0, 0.0, 0.0, 1.0),
                                            others => <>)));

      Link_Example (0.0, 300.0,
                    Gtk_New (Arrow_To   => (Head   => Circle,
                                            Length => 1.0,
                                            Fill   => Black_RGBA,
                                            others => <>),
                             Arrow_From => (Head   => Circle,
                                            Length => 2.0,
                                            Stroke => (1.0, 0.0, 0.0, 1.0),
                                            Fill   => (1.0, 0.0, 0.0, 1.0),
                                            others => <>)));

      Link_Example (0.0, 360.0,
                    Gtk_New (Line_Width => 2.0,
                             Symbol_To  => (Name   => Cross,
                                            others => <>),
                             Symbol_From => (Name => Strike,
                                             others => <>)));

      Link_Example (0.0, 400.0,
                    Gtk_New (Line_Width => 2.0,
                             Symbol_To  => (Name   => Double_Strike,
                                            others => <>)));

      Link_Example (300.0, 180.0,
                    Gtk_New (Dashes     => (2.0, 2.0)));
      Link_Example (300.0, 220.0,
                    Gtk_New (Dashes     => (2.0, 4.0, 4.0, 6.0)));
      Link_Example (300.0, 260.0,
                    Gtk_New (Dashes     => (2.0, 4.0, 6.0, 4.0)));

      Link_Example (300.0, 320.0,
                    Gtk_New (Line_Width => 2.0));
      Link_Example (300.0, 360.0,
                    Gtk_New (Line_Width => 4.0));
      Link_Example (300.0, 400.0,
                    Gtk_New (Line_Width => 6.0));

      Text := Gtk_New_Text (Font, "Custom drawing for links");
      Text.Set_Position ((0.0, 450.0));
      Model.Add (Text);

      Spring_Example (0.0, 470.0, Gtk_New (Stroke => Black_RGBA));

      Text := Gtk_New_Text (Font, "Custom waypoints for links");
      Text.Set_Position ((0.0, 580.0));
      Model.Add (Text);

      Waypoints_Example
        (0.0, 600.0,
         Gtk_New (Stroke => Black_RGBA),
         Waypoints          => ((80.0, 30.0), (0.0, 20.0)),
         Relative_Waypoints => True);

      ---------------------
      -- Labels on links --
      ---------------------

      Y := 680.0;

      Text := Gtk_New_Text (Font, "Labels on links");
      Text.Set_Position ((0.0, Y));
      Model.Add (Text);

      Label_Example (0.0, Y + 20.0, Gtk_New
                     (Font => (Name   => From_String ("sans 7"),
                               Color  => Black_RGBA,
                               others => <>),
                      Fill => Create_Rgba_Pattern ((1.0, 0.0, 0.0, 0.2))));
      Label_Example (300.0, Y + 20.0, Gtk_New
                     (Stroke => Null_RGBA,
                      Fill => Create_Rgba_Pattern ((1.0, 1.0, 1.0, 0.7)),
                      Font   => (Name   => From_String ("sans 8"),
                                 Color  => (1.0, 0.0, 0.0, 1.0),
                                 others => <>)));
      Label_Example (0.0, Y + 120.0, Gtk_New
                     (Stroke => Null_RGBA,
                      Fill => Create_Rgba_Pattern ((1.0, 1.0, 1.0, 0.7)),
                      Font   => (Name   => From_String ("sans 8"),
                                 Color  => (1.0, 0.0, 0.0, 1.0),
                                 others => <>)),
                     Directed => Left_Text_Arrow);

      -------------------
      -- Hidden labels --
      -------------------

      Y := Y + 200.0;

      Text := Gtk_New_Text (Font, "Hidden labels on links");
      Text.Set_Position ((0.0, Y));
      Model.Add (Text);

      Label_Example (0.0, Y + 20.0, Gtk_New
                     (Font => (Name   => From_String ("sans 7"),
                               Color  => Black_RGBA,
                               others => <>),
                      Fill => Create_Rgba_Pattern ((1.0, 0.0, 0.0, 0.2))),
                     Hide => True);
      Label_Example (300.0, Y + 20.0, Gtk_New
                     (Stroke => Null_RGBA,
                      Fill => Create_Rgba_Pattern ((1.0, 1.0, 1.0, 0.7)),
                      Font   => (Name   => From_String ("sans 8"),
                                 Color  => (1.0, 0.0, 0.0, 1.0),
                                 others => <>)),
                     Hide => True);
      Label_Example (0.0, Y + 120.0, Gtk_New
                     (Stroke => Null_RGBA,
                      Fill => Create_Rgba_Pattern ((1.0, 1.0, 1.0, 0.7)),
                      Font   => (Name   => From_String ("sans 8"),
                                 Color  => (1.0, 0.0, 0.0, 1.0),
                                 others => <>)),
                     Hide     => True,
                     Directed => Left_Text_Arrow);

      -------------------------------------
      -- Distance between link and label --
      -------------------------------------

      Y := Y + 200.0;

      Text := Gtk_New_Text (Font, "Links can stop a distance from the items");
      Text.Set_Position ((0.0, Y));
      Model.Add (Text);

      Distance_Example (0.0,   Y + 20.0, Distance => 0.0);
      Distance_Example (190.0, Y + 20.0, Distance => 10.0);
      Distance_Example (380.0, Y + 20.0, Distance => 30.0);

      --  Create the view once the model is populated, to avoid a refresh
      --  every time a new item is added.

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Always, Policy_Always);
      Frame.Add (Scrolled);

      Gtk_New (Canvas, Model);
      Canvas.On_Item_Event (On_Item_Event_Move_Item'Access);
      Unref (Model);
      Scrolled.Add (Canvas);

      Frame.Show_All;
   end Run;

end Create_Canvas_View_Links;
