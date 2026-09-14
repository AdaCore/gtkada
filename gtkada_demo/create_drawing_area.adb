------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2026, AdaCore                          --
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

--  This demo shows how to draw on a Gtk_Drawing_Area with Cairo. The draw
--  function installed with Set_Draw_Func is handed a Cairo context and the
--  area's current size, and is called again whenever the area has to be
--  repainted.
--
--  The picture it paints is a knockout group: a black disc has three discs
--  cut out of it, and the same three discs are then added back at half
--  intensity through a separate group so that they join without seams. It is
--  drawn over a checkerboard, the conventional backdrop for showing where a
--  surface is transparent.

with Ada.Numerics.Generic_Elementary_Functions;

with Glib;               use Glib;
with Cairo;              use Cairo;
with Cairo.Surface;
with Gtk.Box;            use Gtk.Box;
with Gtk.Drawing_Area;   use Gtk.Drawing_Area;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Frame;          use Gtk.Frame;
with Gtk.Label;          use Gtk.Label;
with Gtk.Widget;         use Gtk.Widget;

package body Create_Drawing_Area is

   package Gdouble_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (Gdouble);
   use Gdouble_Functions;

   Pi : constant := Ada.Numerics.Pi;

   Check_Size : constant Gint := 16;
   --  Side of one square of the checkerboard backdrop.

   procedure Oval_Path
     (Cr : Cairo_Context; Xc, Yc, Xr, Yr : Gdouble);
   --  Append an axis-aligned ellipse centred on (Xc, Yc) with radii Xr and
   --  Yr. Cairo has no ellipse primitive, so this scales the coordinate
   --  system and draws a circle in it; the transform is undone before
   --  returning, leaving only the path behind.

   procedure Fill_Checks
     (Cr : Cairo_Context; Width, Height : Gint);
   --  Paint the two-tone checkerboard the group is composited over.

   procedure Draw_3circles
     (Cr : Cairo_Context; Xc, Yc, Radius, Alpha : Gdouble);
   --  Draw the red, green and blue discs, equally spaced around the centre
   --  of the larger disc of radius Radius.

   procedure Groups_Draw
     (Area   : not null access Gtk_Drawing_Area_Record'Class;
      Cr     : Cairo_Context;
      Width  : Gint;
      Height : Gint);
   --  The draw function itself.

   ---------------
   -- Oval_Path --
   ---------------

   procedure Oval_Path
     (Cr : Cairo_Context; Xc, Yc, Xr, Yr : Gdouble) is
   begin
      Save (Cr);

      Translate (Cr, Xc, Yc);
      Scale (Cr, 1.0, Yr / Xr);
      Move_To (Cr, Xr, 0.0);
      Arc (Cr, 0.0, 0.0, Xr, 0.0, 2.0 * Pi);
      Close_Path (Cr);

      Restore (Cr);
   end Oval_Path;

   -----------------
   -- Fill_Checks --
   -----------------

   procedure Fill_Checks
     (Cr : Cairo_Context; Width, Height : Gint) is
   begin
      Rectangle (Cr, 0.0, 0.0, Gdouble (Width), Gdouble (Height));
      Set_Source_Rgb (Cr, 0.4, 0.4, 0.4);
      Fill (Cr);

      --  Collect every light square into a single path, then fill once.
      declare
         J : Gint := 0;
      begin
         while J < Height loop
            declare
               I : Gint := 0;
            begin
               while I < Width loop
                  if (I / Check_Size + J / Check_Size) mod 2 = 0 then
                     Rectangle
                       (Cr,
                        Gdouble (I), Gdouble (J),
                        Gdouble (Check_Size), Gdouble (Check_Size));
                  end if;
                  I := I + Check_Size;
               end loop;
            end;
            J := J + Check_Size;
         end loop;
      end;

      Set_Source_Rgb (Cr, 0.7, 0.7, 0.7);
      Fill (Cr);
   end Fill_Checks;

   -------------------
   -- Draw_3circles --
   -------------------

   procedure Draw_3circles
     (Cr : Cairo_Context; Xc, Yc, Radius, Alpha : Gdouble)
   is
      Subradius : constant Gdouble := Radius * (2.0 / 3.0 - 0.1);

      type Disc is record
         Angle      : Gdouble;
         R, G, B    : Gdouble;
      end record;

      --  The three discs sit a third of the way out from the centre, evenly
      --  spaced around it.
      Discs : constant array (1 .. 3) of Disc :=
        ((Angle => 0.5,           R => 1.0, G => 0.0, B => 0.0),
         (Angle => 0.5 + 2.0 / 3.0, R => 0.0, G => 1.0, B => 0.0),
         (Angle => 0.5 + 4.0 / 3.0, R => 0.0, G => 0.0, B => 1.0));
   begin
      for D of Discs loop
         Set_Source_Rgba (Cr, D.R, D.G, D.B, Alpha);
         Oval_Path
           (Cr,
            Xc + Radius / 3.0 * Cos (Pi * D.Angle),
            Yc - Radius / 3.0 * Sin (Pi * D.Angle),
            Subradius, Subradius);
         Fill (Cr);
      end loop;
   end Draw_3circles;

   -----------------
   -- Groups_Draw --
   -----------------

   procedure Groups_Draw
     (Area   : not null access Gtk_Drawing_Area_Record'Class;
      Cr     : Cairo_Context;
      Width  : Gint;
      Height : Gint)
   is
      pragma Unreferenced (Area);

      Radius : constant Gdouble :=
        0.5 * Gdouble (Gint'Min (Width, Height)) - 10.0;
      Xc     : constant Gdouble := Gdouble (Width) / 2.0;
      Yc     : constant Gdouble := Gdouble (Height) / 2.0;

      Target : constant Cairo_Surface := Get_Target (Cr);

      --  Three scratch surfaces: the overlay the group is assembled in, the
      --  alpha-only mask whose shape is knocked out of it, and the half
      --  intensity discs that are added back afterwards.
      Overlay : constant Cairo_Surface :=
        Cairo.Surface.Create_Similar
          (Target, Cairo_Content_Color_Alpha, Width, Height);
      Punch   : constant Cairo_Surface :=
        Cairo.Surface.Create_Similar
          (Target, Cairo_Content_Alpha, Width, Height);
      Circles : constant Cairo_Surface :=
        Cairo.Surface.Create_Similar
          (Target, Cairo_Content_Color_Alpha, Width, Height);

      Overlay_Cr : constant Cairo_Context := Create (Overlay);
      Punch_Cr   : Cairo_Context;
      Circles_Cr : Cairo_Context;
   begin
      Fill_Checks (Cr, Width, Height);

      --  A solid black disc, the body of the group.
      Set_Source_Rgb (Overlay_Cr, 0.0, 0.0, 0.0);
      Oval_Path (Overlay_Cr, Xc, Yc, Radius, Radius);
      Fill (Overlay_Cr);

      --  Knock the three discs out of it. Dest_Out keeps the destination
      --  only where the source is transparent.
      Punch_Cr := Create (Punch);
      Draw_3circles (Punch_Cr, Xc, Yc, Radius, 1.0);
      Destroy (Punch_Cr);

      Set_Operator (Overlay_Cr, Cairo_Operator_Dest_Out);
      Set_Source_Surface (Overlay_Cr, Punch, 0.0, 0.0);
      Paint (Overlay_Cr);

      --  Draw them again at half intensity in a group of their own, so the
      --  three overlap once among themselves rather than once per disc, and
      --  add that in. Compositing them straight onto the overlay would show
      --  seams where they meet.
      Circles_Cr := Create (Circles);
      Set_Operator (Circles_Cr, Cairo_Operator_Over);
      Draw_3circles (Circles_Cr, Xc, Yc, Radius, 0.5);
      Destroy (Circles_Cr);

      Set_Operator (Overlay_Cr, Cairo_Operator_Add);
      Set_Source_Surface (Overlay_Cr, Circles, 0.0, 0.0);
      Paint (Overlay_Cr);

      Destroy (Overlay_Cr);

      --  Finally composite the assembled group over the checkerboard.
      Set_Source_Surface (Cr, Overlay, 0.0, 0.0);
      Paint (Cr);

      Cairo.Surface.Destroy (Overlay);
      Cairo.Surface.Destroy (Punch);
      Cairo.Surface.Destroy (Circles);
   end Groups_Draw;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "A @bGtk_Drawing_Area@B is a blank widget you draw on with"
        & " @bCairo@B."
        & ASCII.LF
        & "Install a drawing function with @bSet_Draw_Func@B: it receives a"
        & " Cairo context and the area's current width and height, and is"
        & " called again every time the area has to be repainted. Resize the"
        & " window and the picture is redrawn at the new size."
        & ASCII.LF
        & "The picture is a @bknockout group@B. A black disc is drawn into an"
        & " offscreen surface, three discs are cut out of it with"
        & " @bCairo_Operator_Dest_Out@B, and the same three are added back at"
        & " half intensity with @bCairo_Operator_Add@B -- from a group of"
        & " their own, so that they join without seams. The checkerboard"
        & " behind it shows where the result is transparent.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box    : Gtk_Box;
      Info   : Gtk_Label;
      Group  : Gtk_Frame;
      Groups : Gtk_Drawing_Area;
   begin
      Gtk_New (Box, Orientation_Vertical, 10);
      Box.Set_Margin_Start (10);
      Box.Set_Margin_End (10);
      Box.Set_Margin_Top (10);
      Box.Set_Margin_Bottom (10);

      Gtk_New (Info, "Knockout groups");
      Info.Set_Halign (Align_Start);
      Box.Append (Info);

      Gtk_New (Group);
      Group.Set_Hexpand (True);
      Group.Set_Vexpand (True);

      Gtk_New (Groups);
      Groups.Set_Content_Width (100);
      Groups.Set_Content_Height (100);
      Groups.Set_Draw_Func (Groups_Draw'Access);
      Group.Set_Child (Groups);

      Box.Append (Group);

      Frame.Set_Child (Box);
   end Run;

end Create_Drawing_Area;
