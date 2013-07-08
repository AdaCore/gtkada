------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2011-2013, AdaCore                     --
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

with Ada.Numerics;    use Ada.Numerics;
with Cairo.Pattern;   use Cairo, Cairo.Pattern;
with Glib;            use Glib;

separate (Gtkada.MDI)
package body Close_Button is

   type Color_HSV is record
      Hue        : Gdouble;
      Saturation : Gdouble;
      Value      : Gdouble;
      Alpha      : Gdouble;
   end record;

   function To_HSV (Color : Gdk_RGBA) return Color_HSV;
   function To_RGB (Color : Color_HSV) return Gdk_RGBA;
   --  Convertion between the Red-Green-Blue color space and
   --  the Hue-Saturation-Value one.

   procedure Clip_Luminance (Color : in out Gdk_RGBA);
   --  Clips the color's luminance so that we can highligh/darken it.
   --  This prevents too light or too dark colors as base color for the button.

   function Shade
     (Color       : Gdk_RGBA;
      Delta_Value : Gdouble) return Gdk_RGBA;
   --  Resulting color's luminance is old one + Delta_Value, cliped to the
   --  range 0 - 100% (e.g. 0.0 - 1.0)

   function On_Draw
     (Widget  : access Gtk_Widget_Record'Class;
      Cr      : Cairo.Cairo_Context)
      return Boolean;
   --  draws the close button upon "draw" event

   procedure Rounded_Rectangle
     (Cr         : Cairo_Context;
      X, Y, W, H : Gdouble;
      Radius     : Gdouble);
   --  Draws a rounded rectangle at coordinate X, Y with W and H size.

   procedure Cross
     (Cr            : Cairo_Context;
      W, Size, Thin : Gdouble);
   --  Draws a cross centered on W / 2.0 of current size and thin.

   function On_Tab_Enter
     (Widget : access Gtk_Widget_Record'Class) return Boolean;

   function On_Tab_Leave
     (Widget : access Gtk_Widget_Record'Class) return Boolean;

   function On_Enter (Widget : access Gtk_Widget_Record'Class) return Boolean;
   function On_Leave (Widget : access Gtk_Widget_Record'Class) return Boolean;

   function On_Mouse_Pressed
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event)
      return Boolean;

   function On_Mouse_Released
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event)
      return Boolean;

   procedure Invalidate (Widget : access Gtk_Widget_Record'Class);
   --  Invalidates the whole widget for queing a redraw

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Button      : out Gtkada_MDI_Close_Button;
      Tab         : access Gtk_Widget_Record'Class;
      Child       : access MDI_Child_Record'Class;
      Horizontal  : Boolean;
      In_Titlebar : Boolean)
   is
   begin
      Button := new Gtkada_MDI_Close_Button_Record;
      Gtk.Event_Box.Initialize (Button);
      Get_Style_Context (Button).Add_Class ("mdiCloseButton");
      Set_Visible_Window (Button, False);

      Button.Child       := MDI_Child (Child);
      Button.Pressed     := False;
      Button.Over        := False;
      Button.Tab_Over    := False;
      Button.In_Titlebar := In_Titlebar;
      Button.Horizontal  := Horizontal;

      --  In the titlebar, we can go up to 16px as this is the size of the
      --  pixmaps, but we lower this size to 14px to be able to draw the extra
      --  border for the hilight.

      --  In the tab, we keep it smaller, as space is limited there.
      if In_Titlebar then
         Button.Default_Size := 12;
      else
         Button.Default_Size := 11;
      end if;

      if Horizontal then
         Button.Set_Size_Request
           (Button.Default_Size, Button.Default_Size + 4);
      else
         Button.Set_Size_Request (Button.Default_Size, Button.Default_Size);
      end if;

      Set_Events
        (Button,
         Get_Events (Button) or Pointer_Motion_Mask or
           Button_Press_Mask or Button_Release_Mask or
           Enter_Notify_Mask or Leave_Notify_Mask);
      Return_Callback.Connect
        (Button, Signal_Draw,
         Return_Callback.To_Marshaller (On_Draw'Access));
      Return_Callback.Connect
        (Button, Signal_Enter_Notify_Event,
         Return_Callback.To_Marshaller (On_Enter'Access));
      Return_Callback.Connect
        (Button, Signal_Leave_Notify_Event,
         Return_Callback.To_Marshaller (On_Leave'Access));
      Return_Callback.Object_Connect
        (Tab, Signal_Enter_Notify_Event,
         Return_Callback.To_Marshaller (On_Tab_Enter'Access),
         Slot_Object => Button);
      Return_Callback.Object_Connect
        (Tab, Signal_Leave_Notify_Event,
         Return_Callback.To_Marshaller (On_Tab_Leave'Access),
         Slot_Object => Button);
      Return_Callback.Connect
        (Button, Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (On_Mouse_Pressed'Access));
      Return_Callback.Connect
        (Button, Signal_Button_Release_Event,
         Return_Callback.To_Marshaller (On_Mouse_Released'Access));
   end Gtk_New;

   -------------
   -- On_Draw --
   -------------

   function On_Draw
     (Widget : access Gtk_Widget_Record'Class;
      Cr     : Cairo_Context)
      return Boolean
   is
      Button  : constant Gtkada_MDI_Close_Button :=
                  Gtkada_MDI_Close_Button (Widget);
      Width   : Gint;
      Height  : Gint;
      dW      : Gdouble;
      dX, dY  : Gdouble;
      Cross_W : Gdouble;
      Base    : Gdk_RGBA;
      Lo, Hi  : Gdk_RGBA;
      Ptrn    : Cairo_Pattern;
      Alloc   : Gtk_Allocation;
      Ctx     : Gtk_Style_Context;

   begin
      if not Button.In_Titlebar
        and then not Button.Tab_Over
        and then not Button.Over
      then
         return False;
      end if;

      if Button.Get_Realized then
         Button.Get_Allocation (Alloc);
         Width  := Alloc.Width;
         Height := Alloc.Height;

         dW := Gdouble (Button.Default_Size);

         --  Make sure the button fits in the allocated space
         if dW > Gdouble (Width) then
            dW := Gdouble (Width);
         end if;

         --  Height - 2 : we want 1px for the thin highlight effect at the
         --  bottom of the button. We add another px to center the button
         --  (compensate the hilight size).
         if dW > Gdouble (Height - 2) then
            dW := Gdouble (Height - 2);
         end if;

         Cairo.Save (Cr);

         if Button.Horizontal then
            --  Align to right and center vertically
            dX := Gdouble (Width - Gint (dW));
            dY := Gdouble ((Height - Gint (dW)) / 2);
         else
            --  Align to bottom and center horizontally
            dX := Gdouble ((Width - Gint (dW)) / 2);
            dY := Gdouble (Height - Gint (dW));
         end if;

         Cairo.Translate (Cr, dX, dY);

         Cairo.Set_Line_Width (Cr, 1.0);
         Cross_W := dW * 0.7;

         --  Retrieve the parent's actual background color for a nice
         --  transparency effect
         Ctx := Get_Style_Context (Button.Child);
         Ctx.Get_Color (Gtk_State_Flag_Normal, Base);
         Clip_Luminance (Base);

         --  Shade the color according to the button's state
         if Button.Pressed
           or else Button.Over
         then
            Base.Alpha := 0.8;
         else
            Base.Alpha := 0.4;
         end if;

         Lo := Shade (Base, -0.3);
         Hi := Shade (Base, 0.8);

         --  Clip the cross
         Cairo.Set_Fill_Rule (Cr, Cairo_Fill_Rule_Even_Odd);
         Cairo.Rectangle
           (Cr, -1.0, -1.0, dW + 2.0, dW + 2.0);
         Cross (Cr, dW, Cross_W, dW / 5.0);
         Cairo.Clip (Cr);
         Cairo.Set_Fill_Rule (Cr, Cairo_Fill_Rule_Winding);

         --  Now actually draw the button

         --  Fill the base color
         Cairo.Set_Source_Rgba
           (Cr, Base.Red, Base.Green, Base.Blue, Base.Alpha);
         --  And draw inside a rounded rectangle
         Rounded_Rectangle (Cr, 0.0, 0.0, dW, dW, 2.5);
         Cairo.Fill (Cr);

         --  Add some radial shadow to simulate shadow under the cross
         Ptrn := Cairo.Pattern.Create_Radial
           (dW * 0.5, dW * 0.5, 2.0, dW * 0.5, dW * 0.5, Cross_W / 2.0);
         Cairo.Pattern.Add_Color_Stop_Rgba
           (Ptrn, 0.0, Lo.Red, Lo.Green, Lo.Blue, Lo.Alpha);
         Cairo.Pattern.Add_Color_Stop_Rgba
           (Ptrn, 1.0, Lo.Red, Lo.Green, Lo.Blue, 0.0);
         Rounded_Rectangle (Cr, 0.0, 0.0, dW, dW, 2.5);
         Cairo.Set_Source (Cr, Ptrn);
         Cairo.Pattern.Destroy (Ptrn);
         Cairo.Fill (Cr);

         --  Add a hilighted border with height bigger than shadowed border
         --  to just display a thin hilighted border under the button
         Cairo.Set_Source_Rgba (Cr, Hi.Red, Hi.Green, Hi.Blue, Hi.Alpha);

         if Button.Pressed then
            --  Keep the highlight under the button
            Rounded_Rectangle (Cr, 0.5, 0.5, dW - 1.0, dW, 2.5);
            --  Add a highlight inside the button, opposite direction as usual
            Rounded_Rectangle (Cr, 0.5, 0.5, dW - 1.0, dW - 2.0, 2.5);
         else
            Rounded_Rectangle (Cr, 0.5, 1.5, dW - 1.0, dW - 1.0, 2.5);
         end if;

         Cairo.Stroke (Cr);

         --  Now add the shadowed border
         Cairo.Set_Source_Rgba (Cr, Lo.Red, Lo.Green, Lo.Blue, Lo.Alpha);
         Rounded_Rectangle (Cr, 0.5, 0.5, dW - 1.0, dW - 1.0, 2.5);
         Cairo.Stroke (Cr);

         Cairo.Restore (Cr);
      end if;

      return False;
   end On_Draw;

   ------------
   -- To_HSV --
   ------------

   function To_HSV (Color : Gdk_RGBA) return Color_HSV is
      Ret            : Color_HSV;
      Min, Max, Delt : Gdouble;
   begin
      Ret.Alpha := Color.Alpha;
      Min := Gdouble'Min (Gdouble'Min (Color.Red, Color.Green), Color.Blue);
      Max := Gdouble'Max (Gdouble'Max (Color.Red, Color.Green), Color.Blue);

      Ret.Value := Max;
      Delt := Max - Min;

      if Max > 0.0 then
         Ret.Saturation := Delt / Max;
      else
         Ret.Saturation := 0.0;
         Ret.Hue := 0.0;

         return Ret;
      end if;

      if Color.Red >= Max then
         Ret.Hue := (Color.Green - Color.Blue) / Delt;
      elsif Color.Green >= Max then
         Ret.Hue := 2.0 + (Color.Blue - Color.Red) / Delt;
      else
         Ret.Hue := 4.0 + (Color.Red - Color.Green) / Delt;
      end if;

      Ret.Hue := Ret.Hue * 60.0;

      if Ret.Hue < 0.0 then
         Ret.Hue := Ret.Hue + 360.0;
      end if;

      return Ret;
   end To_HSV;

   ------------
   -- To_RGB --
   ------------

   function To_RGB (Color : Color_HSV) return Gdk_RGBA
   is
      Hh, P, Q, T, Ff : Gdouble;
      J               : Integer;
      Ret             : Gdk_RGBA;
   begin
      if Color.Saturation <= 0.0 then
         Ret := (Color.Value, Color.Value, Color.Value, Color.Alpha);

         return Ret;
      end if;

      Hh := Color.Hue;

      while Hh >= 360.0 loop
         Hh := Hh - 360.0;
      end loop;

      while Hh < 0.0 loop
         Hh := Hh + 360.0;
      end loop;

      Hh := Hh / 60.0;
      J := Integer (Gdouble'Floor (Hh));
      Ff := Hh - Gdouble (J);
      P := Color.Value * (1.0 - Color.Saturation);
      Q := Color.Value * (1.0 - (Color.Saturation * Ff));
      T := Color.Value * (1.0 - (Color.Saturation * (1.0 - Ff)));

      case J is
         when 0 =>
            Ret :=
              (Red   => Color.Value,
               Green => T,
               Blue  => P,
               Alpha => Color.Alpha);
         when 1 =>
            Ret :=
              (Red   => Q,
               Green => Color.Value,
               Blue  => P,
               Alpha => Color.Alpha);
         when 2 =>
            Ret :=
              (Red   => P,
               Green => Color.Value,
               Blue  => T,
               Alpha => Color.Alpha);
         when 3 =>
            Ret :=
              (Red   => P,
               Green => Q,
               Blue  => Color.Value,
               Alpha => Color.Alpha);
         when 4 =>
            Ret :=
              (Red   => T,
               Green => P,
               Blue  => Color.Value,
               Alpha => Color.Alpha);
         when others =>
            Ret :=
              (Red   => Color.Value,
               Green => P,
               Blue  => Q,
               Alpha => Color.Alpha);
      end case;

      return Ret;
   end To_RGB;

   --------------------
   -- Clip_Luminance --
   --------------------

   procedure Clip_Luminance (Color : in out Gdk_RGBA)
   is
      HSV : Color_HSV;

   begin
      HSV := To_HSV (Color);

      if HSV.Value > 0.85 then
         HSV.Value := 0.85;
      elsif HSV.Value < 0.15 then
         HSV.Value := 0.15;
      else
         return;
      end if;

      Color := To_RGB (HSV);
   end Clip_Luminance;

   -----------
   -- Shade --
   -----------

   function Shade
     (Color       : Gdk_RGBA;
      Delta_Value : Gdouble) return Gdk_RGBA
   is
      HSV : Color_HSV := To_HSV (Color);
   begin
      HSV.Value := HSV.Value + Delta_Value;

      if HSV.Value > 1.0 then
         HSV.Value := 1.0;
      elsif HSV.Value < 0.0 then
         HSV.Value := 0.0;
      end if;

      return To_RGB (HSV);
   end Shade;

   -----------------------
   -- Rounded_Rectangle --
   -----------------------

   procedure Rounded_Rectangle
     (Cr         : Cairo_Context;
      X, Y, W, H : Gdouble;
      Radius     : Gdouble) is
   begin
      Cairo.Move_To (Cr, X + Radius, Y);
      Cairo.Arc
        (Cr, X + W - Radius, Y + Radius, Radius, Pi * 1.5, Pi * 2.0);
      Cairo.Arc
        (Cr, X + W - Radius, Y + H - Radius, Radius, 0.0, Pi * 0.5);
      Cairo.Arc
        (Cr, X + Radius, Y + H - Radius, Radius, Pi * 0.5, Pi);
      Cairo.Arc
        (Cr, X + Radius, Y + Radius, Radius, Pi, Pi * 1.5);
   end Rounded_Rectangle;

   -----------
   -- Cross --
   -----------

   procedure Cross
     (Cr            : Cairo_Context;
      W, Size, Thin : Gdouble)
   is
      Matrix : aliased Cairo_Matrix;
   begin
      Cairo.Get_Matrix (Cr, Matrix'Access);

      --  We follow the cross's dots in the following order:
      --
      --      10+--+9
      --      11|  |8
      --  12 +--+  +--+ 7
      --     |        |
      --   1 +--+  +--+ 6
      --       2|  |5
      --       3+--+4
      --
      --        <-->
      --        Thin
      --
      --     <-------->
      --        Size

      Cairo.Translate (Cr, W / 2.0, W / 2.0);
      Cairo.Rotate (Cr, Pi * 0.25);
      Cairo.Move_To (Cr, -Size / 2.0, -Thin / 2.0); --  1
      Cairo.Line_To (Cr, -Thin / 2.0, -Thin / 2.0); --  2
      Cairo.Line_To (Cr, -Thin / 2.0, -Size / 2.0); --  3
      Cairo.Line_To (Cr, Thin / 2.0, -Size / 2.0);  --  4
      Cairo.Line_To (Cr, Thin / 2.0, -Thin / 2.0);  --  5
      Cairo.Line_To (Cr, Size / 2.0, -Thin / 2.0);  --  6
      Cairo.Line_To (Cr, Size / 2.0, Thin / 2.0);   --  7
      Cairo.Line_To (Cr, Thin / 2.0, Thin / 2.0);   --  8
      Cairo.Line_To (Cr, Thin / 2.0, Size / 2.0);   --  9
      Cairo.Line_To (Cr, -Thin / 2.0, Size / 2.0);  --  10
      Cairo.Line_To (Cr, -Thin / 2.0, Thin / 2.0);  --  11
      Cairo.Line_To (Cr, -Size / 2.0, Thin / 2.0);  --  12
      Cairo.Close_Path (Cr);
      --  Restore the transformation matrix
      Cairo.Set_Matrix (Cr, Matrix'Access);
   end Cross;

   ----------------
   -- Invalidate --
   ----------------

   procedure Invalidate (Widget : access Gtk_Widget_Record'Class) is
      Alloc : Gtk_Allocation;
   begin
      if Widget.Get_Realized then
         Widget.Get_Allocation (Alloc);
         Invalidate_Rect (Gtk.Widget.Get_Window (Widget), Alloc, False);
         Queue_Draw (Widget);
      end if;
   end Invalidate;

   ------------------
   -- On_Tab_Enter --
   ------------------

   function On_Tab_Enter
     (Widget : access Gtk_Widget_Record'Class)
      return Boolean is
   begin
      Gtkada_MDI_Close_Button (Widget).Tab_Over := True;
      Invalidate (Widget);

      return False;
   end On_Tab_Enter;

   ------------------
   -- On_Tab_Leave --
   ------------------

   function On_Tab_Leave
     (Widget : access Gtk_Widget_Record'Class)
      return Boolean
   is
   begin
      Gtkada_MDI_Close_Button (Widget).Tab_Over := False;
      return On_Leave (Widget);
   end On_Tab_Leave;

   --------------
   -- On_Enter --
   --------------

   function On_Enter
     (Widget : access Gtk_Widget_Record'Class)
      return Boolean is
   begin
      Gtkada_MDI_Close_Button (Widget).Over := True;
      Invalidate (Widget);

      return False;
   end On_Enter;

   --------------
   -- On_Leave --
   --------------

   function On_Leave
     (Widget : access Gtk_Widget_Record'Class)
      return Boolean is
   begin
      Gtkada_MDI_Close_Button (Widget).Over := False;
      Gtkada_MDI_Close_Button (Widget).Pressed := False;
      Invalidate (Widget);

      return False;
   end On_Leave;

   ----------------------
   -- On_Mouse_Pressed --
   ----------------------

   function On_Mouse_Pressed
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event)
      return Boolean
   is
      Button : constant Gtkada_MDI_Close_Button :=
                 Gtkada_MDI_Close_Button (Widget);

   begin
      if Event.Button.Button = 1 and then Button.Over then
         Button.Pressed := True;
         Invalidate (Widget);
         return True;
      end if;

      return False;
   end On_Mouse_Pressed;

   -----------------------
   -- On_Mouse_Released --
   -----------------------

   function On_Mouse_Released
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event)
      return Boolean
   is
      Button : constant Gtkada_MDI_Close_Button :=
                 Gtkada_MDI_Close_Button (Widget);

   begin
      if Button.Pressed and then Event.Button.Button = 1 then
         Close_Child (Button.Child);
         return True;
      end if;

      return False;
   end On_Mouse_Released;

end Close_Button;
