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
--  There are two areas. The first paints a knockout group: a black disc has
--  three discs cut out of it, and the same three discs are then added back at
--  half intensity through a separate group so that they join without seams.
--  It is drawn over a checkerboard, the conventional backdrop for showing
--  where a surface is transparent.
--
--  The second is a scribble pad, and shows the other half of the job:
--  reacting to the pointer. A draw function may only draw, so the drawing
--  itself is kept in a Cairo image surface that the strokes are painted into
--  and that the draw function merely copies to the screen. The pointer is
--  read through event controllers -- a Gtk_Gesture_Drag to scribble with,
--  and a Gtk_Gesture_Click on the right button to clear.

with Ada.Numerics.Generic_Elementary_Functions;

with Glib;               use Glib;
with Cairo;              use Cairo;
with Cairo.Image_Surface;
with Cairo.Surface;
with Gtk.Box;            use Gtk.Box;
with Gtk.Drawing_Area;   use Gtk.Drawing_Area;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Frame;          use Gtk.Frame;
with Gtk.Gesture_Click;  use Gtk.Gesture_Click;
with Gtk.Gesture_Drag;   use Gtk.Gesture_Drag;
with Gtk.Gesture_Single; use Gtk.Gesture_Single;
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

   --------------------------
   -- The scribble pad     --
   --------------------------

   Button_Secondary : constant Guint := 3;
   --  GDK_BUTTON_SECONDARY. The GDK_BUTTON_* constants are not bound, so
   --  spell the value out, as the testsuite does for the GDK keyvals.

   Surface : Cairo_Surface := Null_Surface;
   --  Where the scribbles are kept. A draw function is not allowed to hold
   --  state, and is called afresh whenever any part of the area has to be
   --  repainted, so the strokes have to live somewhere of their own.

   Start_X, Start_Y : Gdouble := 0.0;
   --  Where the drag being followed began. Drag_Update and Drag_End report
   --  an offset from that point rather than an absolute position.

   procedure Create_Surface (Widget : not null access Gtk_Widget_Record'Class);
   --  Make a fresh, white surface the size of Widget, discarding any
   --  previous one. Resizing the area therefore clears it.

   procedure Scribble_Draw
     (Area   : not null access Gtk_Drawing_Area_Record'Class;
      Cr     : Cairo_Context;
      Width  : Gint;
      Height : Gint);
   --  Copy the stored scribbles to the screen.

   procedure Scribble_Resize
     (Area   : access Gtk_Drawing_Area_Record'Class;
      Width  : Gint;
      Height : Gint);

   procedure Draw_Brush
     (Widget : not null access Gtk_Widget_Record'Class; X, Y : Gdouble);
   --  Paint one round dab into the stored surface at (X, Y) and ask for a
   --  redraw.

   procedure Drag_Begin
     (Gesture : access Gtk_Gesture_Drag_Record'Class; X, Y : Gdouble);
   procedure Drag_Update
     (Gesture : access Gtk_Gesture_Drag_Record'Class; X, Y : Gdouble);
   procedure Drag_End
     (Gesture : access Gtk_Gesture_Drag_Record'Class; X, Y : Gdouble);

   procedure Pressed
     (Gesture : access Gtk_Gesture_Click_Record'Class;
      N_Press : Gint;
      X, Y    : Gdouble);
   --  Right button: clear the pad.

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


   --------------------
   -- Create_Surface --
   --------------------

   procedure Create_Surface (Widget : not null access Gtk_Widget_Record'Class)
   is
      Cr : Cairo_Context;
   begin
      if Surface /= Null_Surface then
         Cairo.Surface.Destroy (Surface);
      end if;

      Surface :=
        Cairo.Image_Surface.Create
          (Cairo.Image_Surface.Cairo_Format_ARGB32,
           Widget.Get_Width, Widget.Get_Height);

      Cr := Create (Surface);
      Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
      Paint (Cr);
      Destroy (Cr);
   end Create_Surface;

   -------------------
   -- Scribble_Draw --
   -------------------

   procedure Scribble_Draw
     (Area   : not null access Gtk_Drawing_Area_Record'Class;
      Cr     : Cairo_Context;
      Width  : Gint;
      Height : Gint)
   is
      pragma Unreferenced (Width, Height);
   begin
      --  The resize signal normally gets here first, but the area can be
      --  drawn before it has ever been resized.
      if Surface = Null_Surface then
         Create_Surface (Area);
      end if;

      Set_Source_Surface (Cr, Surface, 0.0, 0.0);
      Paint (Cr);
   end Scribble_Draw;

   ---------------------
   -- Scribble_Resize --
   ---------------------

   procedure Scribble_Resize
     (Area   : access Gtk_Drawing_Area_Record'Class;
      Width  : Gint;
      Height : Gint)
   is
      pragma Unreferenced (Width, Height);
   begin
      Create_Surface (Area);
   end Scribble_Resize;

   ----------------
   -- Draw_Brush --
   ----------------

   procedure Draw_Brush
     (Widget : not null access Gtk_Widget_Record'Class; X, Y : Gdouble)
   is
      Cr : Cairo_Context;
   begin
      if Surface = Null_Surface
        or else Cairo.Image_Surface.Get_Width (Surface) /= Widget.Get_Width
        or else Cairo.Image_Surface.Get_Height (Surface) /= Widget.Get_Height
      then
         Create_Surface (Widget);
      end if;

      Cr := Create (Surface);

      --  A zero-length segment with a round cap is a dot. Dragging emits
      --  these often enough that they run together into a line.
      Move_To (Cr, X, Y);
      Line_To (Cr, X, Y);
      Set_Line_Width (Cr, 6.0);
      Set_Line_Cap (Cr, Cairo_Line_Cap_Round);
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Stroke (Cr);

      Destroy (Cr);

      Widget.Queue_Draw;
   end Draw_Brush;

   ----------------
   -- Drag_Begin --
   ----------------

   procedure Drag_Begin
     (Gesture : access Gtk_Gesture_Drag_Record'Class; X, Y : Gdouble) is
   begin
      Start_X := X;
      Start_Y := Y;
      Draw_Brush (Gesture.Get_Widget, X, Y);
   end Drag_Begin;

   -----------------
   -- Drag_Update --
   -----------------

   procedure Drag_Update
     (Gesture : access Gtk_Gesture_Drag_Record'Class; X, Y : Gdouble) is
   begin
      Draw_Brush (Gesture.Get_Widget, Start_X + X, Start_Y + Y);
   end Drag_Update;

   --------------
   -- Drag_End --
   --------------

   procedure Drag_End
     (Gesture : access Gtk_Gesture_Drag_Record'Class; X, Y : Gdouble) is
   begin
      Draw_Brush (Gesture.Get_Widget, Start_X + X, Start_Y + Y);
   end Drag_End;

   -------------
   -- Pressed --
   -------------

   procedure Pressed
     (Gesture : access Gtk_Gesture_Click_Record'Class;
      N_Press : Gint;
      X, Y    : Gdouble)
   is
      pragma Unreferenced (N_Press, X, Y);
      Widget : constant Gtk_Widget := Gesture.Get_Widget;
   begin
      Create_Surface (Widget);
      Widget.Queue_Draw;
   end Pressed;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "A @bGtk_Drawing_Area@B is a blank widget you draw on with"
        & " @bCairo@B. Install a drawing function with @bSet_Draw_Func@B;"
        & " it is called again whenever the area needs to be repainted,"
        & " including on resize."
        & ASCII.LF
        & "The top area is a @bknockout group@B: a black disc is drawn"
        & " offscreen, three discs are cut out of it with"
        & " @bCairo_Operator_Dest_Out@B, then added back at half intensity"
        & " with @bCairo_Operator_Add@B so they blend without seams. The"
        & " checkerboard behind it shows where the result is transparent."
        & ASCII.LF
        & "The bottom area is a @bscribble pad@B: drag with any button to"
        & " draw, right-click to clear. Since a draw function may only"
        & " draw, the strokes are kept in a Cairo image surface of their"
        & " own, read through @bevent controllers@B (@bGtk_Gesture_Drag@B"
        & " and @bGtk_Gesture_Click@B) attached with @bAdd_Controller@B.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box      : Gtk_Box;
      Label    : Gtk_Label;
      Group    : Gtk_Frame;
      Groups   : Gtk_Drawing_Area;
      Scribble : Gtk_Drawing_Area;
      Drag     : Gtk_Gesture_Drag;
      Click    : Gtk_Gesture_Click;
   begin
      Gtk_New (Box, Orientation_Vertical, 10);
      Box.Set_Margin_Start (10);
      Box.Set_Margin_End (10);
      Box.Set_Margin_Top (10);
      Box.Set_Margin_Bottom (10);

      --  The knockout group: drawing and nothing else.

      Gtk_New (Label, "Knockout groups");
      Label.Set_Halign (Align_Start);
      Box.Append (Label);

      Gtk_New (Group);
      Group.Set_Hexpand (True);
      Group.Set_Vexpand (True);

      Gtk_New (Groups);
      Groups.Set_Content_Width (100);
      Groups.Set_Content_Height (100);
      Groups.Set_Draw_Func (Groups_Draw'Access);
      Group.Set_Child (Groups);

      Box.Append (Group);

      --  The scribble pad: drawing driven by the pointer.

      Gtk_New (Label, "Scribble area");
      Label.Set_Halign (Align_Start);
      Box.Append (Label);

      Gtk_New (Group);
      Group.Set_Hexpand (True);
      Group.Set_Vexpand (True);

      Gtk_New (Scribble);
      Scribble.Set_Content_Width (100);
      Scribble.Set_Content_Height (100);
      Scribble.Set_Draw_Func (Scribble_Draw'Access);
      Scribble.On_Resize (Scribble_Resize'Access);
      Group.Set_Child (Scribble);

      --  Drag with any button to scribble. Add_Controller takes ownership
      --  of the gesture, so there is nothing to free here.
      Gtk_New (Drag);
      Gtk_Gesture_Single (Drag).Set_Button (0);
      Drag.On_Drag_Begin (Drag_Begin'Access);
      Drag.On_Drag_Update (Drag_Update'Access);
      Drag.On_Drag_End (Drag_End'Access);
      Scribble.Add_Controller (Drag);

      --  Right button clears it.
      Gtk_New (Click);
      Gtk_Gesture_Single (Click).Set_Button (Button_Secondary);
      Click.On_Pressed (Pressed'Access);
      Scribble.Add_Controller (Click);

      Box.Append (Group);

      Frame.Set_Child (Box);
   end Run;

end Create_Drawing_Area;
