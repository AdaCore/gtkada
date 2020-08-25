------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2011-2020, AdaCore                     --
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
with Cairo;           use Cairo;
with Glib;            use Glib;

separate (Gtkada.MDI)
package body Close_Button is

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
      Position    : Gtk.Enums.Gtk_Position_Type;
      In_Titlebar : Boolean)
   is
   begin
      Button := new Gtkada_MDI_Close_Button_Record;
      Gtk.Event_Box.Initialize (Button);
      Get_Style_Context (Button).Add_Class ("mdiCloseButton");
      Set_Visible_Window (Button, False);

      Button.Child       := (if Child /= null then
                                MDI_Child_Record (Child.all)'Unchecked_Access
                             else
                                null);
      Button.Pressed     := False;
      Button.Over        := False;
      Button.Tab_Over    := False;
      Button.In_Titlebar := In_Titlebar;
      Button.Position    := Position;

      --  In the titlebar, we can go up to 16px as this is the size of the
      --  pixmaps, but we lower this size to 14px to be able to draw the extra
      --  border for the hilight.

      --  In the tab, we keep it smaller, as space is limited there.
      if In_Titlebar then
         Button.Default_Size := 12;
      else
         Button.Default_Size := 11;
      end if;

      Button.Set_Size_Request (Button.Default_Size, Button.Default_Size);

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
      Base    : Gdk_RGBA;
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
            dW := Gdouble (Width) - 2.0;
         end if;

         if dW > Gdouble (Height) then
            dW := Gdouble (Height) - 2.0;
         end if;

         Cairo.Save (Cr);

         case Button.Position is
            when Pos_Top | Pos_Bottom =>
               --  Align to right and center vertically + 1px offset to
               --  align with text baseline
               dX := Gdouble (Width - Gint (dW));
               dY := Gdouble ((Height - Gint (dW)) / 2) + 1.0;

            when Pos_Left =>
               --  Align to top and center horizontally + 1px offset to
               --  align with text baseline
               dX := Gdouble ((Width - Gint (dW)) / 2) + 1.0;
               dY := 0.0;
            when Pos_Right =>
               --  Align to bottom and center horizontally - 1px offset to
               --  align with text baseline
               dX := Gdouble ((Width - Gint (dW)) / 2) - 1.0;
               dY := Gdouble (Height - Gint (dW));
         end case;

         Cairo.Translate (Cr, dX, dY);

         Cairo.Set_Line_Width (Cr, 1.0);

         --  Retrieve the parent's actual background color for a nice
         --  transparency effect
         Ctx := Get_Style_Context (Button.Child);
         Ctx.Get_Color (Gtk_State_Flag_Normal, Base);

         --  Shade the color according to the button's state
         if Button.Pressed
           or else Button.Over
         then
            Base.Alpha := 1.0;
         else
            Base.Alpha := 0.4;
         end if;

         --  Now actually draw the button

         --  Fill the base color
         Cairo.Set_Source_Rgba
           (Cr, Base.Red, Base.Green, Base.Blue, Base.Alpha);

         --  And draw the cross
         Cross (Cr, dW, dW - 4.0, 1.0);
         Cairo.Fill (Cr);

         --  Draw a very thin border around the cross
         Cairo.Set_Source_Rgba
           (Cr, Base.Red, Base.Green, Base.Blue, 0.2);
         Rounded_Rectangle (Cr, 0.5, 0.5, dW - 1.0, dW - 1.0, 2.5);
         Cairo.Stroke (Cr);

         Cairo.Restore (Cr);
      end if;

      return False;
   end On_Draw;

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
