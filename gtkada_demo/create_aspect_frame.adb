------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2026, AdaCore                     --
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
with Interfaces.C; use Interfaces.C;

with Cairo;             use Cairo;
with Glib;              use Glib;
with Gtk.Aspect_Frame;  use Gtk.Aspect_Frame;
with Gtk.Box;           use Gtk.Box;
with Gtk.Check_Button;  use Gtk.Check_Button;
with Gtk.Drawing_Area;  use Gtk.Drawing_Area;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Grid;          use Gtk.Grid;
with Gtk.Label;         use Gtk.Label;
with Gtk.Spin_Button;   use Gtk.Spin_Button;
with Gtk.Widget;        use Gtk.Widget;

package body Create_Aspect_Frame is

   Aspect : Gtk_Aspect_Frame;
   --  The frame being driven by the controls below it.

   X_Spin, Y_Spin, Ratio_Spin : Gtk_Spin_Button;
   Obey                       : Gtk_Check_Button;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Aspect_Frame@B keeps its child at a fixed aspect"
        & " ratio (width / height) however the frame is resized."
        & ASCII.LF
        & "@bXalign@B and @bYalign@B (0.0 .. 1.0) say where the child sits"
        & " in the space left over. @bRatio@B gives the aspect ratio; when"
        & " @bObey_Child@B is set it is ignored and the ratio of the"
        & " child's own size request is used instead."
        & ASCII.LF
        & "Resize the window to see the drawing keep its shape.";
   end Help;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Area   : not null access Gtk_Drawing_Area_Record'Class;
      Cr     : Cairo_Context;
      Width  : Gint;
      Height : Gint)
   is
      pragma Unreferenced (Area);
      W : constant Gdouble := Gdouble (Width);
      H : constant Gdouble := Gdouble (Height);
   begin
      Set_Source_Rgb (Cr, 0.95, 0.85, 0.4);
      Rectangle (Cr, 0.0, 0.0, W, H);
      Fill (Cr);

      Set_Source_Rgb (Cr, 0.3, 0.2, 0.0);
      Set_Line_Width (Cr, 2.0);
      Move_To (Cr, 0.0, 0.0);
      Line_To (Cr, W, H);
      Move_To (Cr, W, 0.0);
      Line_To (Cr, 0.0, H);
      Stroke (Cr);
   end Draw;

   -------------
   -- Changed --
   -------------

   procedure Changed (Self : access Gtk_Spin_Button_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Aspect.Set_Xalign (C_float (X_Spin.Get_Value));
      Aspect.Set_Yalign (C_float (Y_Spin.Get_Value));
      Aspect.Set_Ratio (C_float (Ratio_Spin.Get_Value));
   end Changed;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (Self : access Gtk_Check_Button_Record'Class) is
   begin
      Aspect.Set_Obey_Child (Self.Get_Active);
      Ratio_Spin.Set_Sensitive (not Self.Get_Active);
   end Toggle;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box   : Gtk_Box;
      Table : Gtk_Grid;
      Area  : Gtk_Drawing_Area;

      procedure Add_Row
        (Row : Gint; Title : String; Spin : out Gtk_Spin_Button;
         Min, Max, Step, Initial : Gdouble);

      -------------
      -- Add_Row --
      -------------

      procedure Add_Row
        (Row : Gint; Title : String; Spin : out Gtk_Spin_Button;
         Min, Max, Step, Initial : Gdouble)
      is
         Label : Gtk_Label;
      begin
         Gtk_New (Label, Title);
         Label.Set_Halign (Align_Start);
         Table.Attach (Label, 0, Row);
         Gtk_New_With_Range (Spin, Min, Max, Step);
         Spin.Set_Value (Initial);
         Spin.On_Value_Changed (Changed'Access);
         Table.Attach (Spin, 1, Row);
      end Add_Row;

   begin
      Gtk.Frame.Set_Label (Frame, "Aspect Frame");

      Gtk_New (Box, Orientation_Vertical, 10);
      Box.Set_Margin_Start (10);
      Box.Set_Margin_End (10);
      Box.Set_Margin_Top (10);
      Box.Set_Margin_Bottom (10);
      Frame.Set_Child (Box);

      Gtk_New (Table);
      Table.Set_Row_Spacing (5);
      Table.Set_Column_Spacing (10);
      Box.Append (Table);

      Gtk_New (Aspect, 0.5, 0.5, 2.0, False);
      Aspect.Set_Hexpand (True);
      Aspect.Set_Vexpand (True);

      Gtk_New (Area);
      Area.Set_Content_Width (100);
      Area.Set_Content_Height (100);
      Area.Set_Draw_Func (Draw'Access);
      Aspect.Set_Child (Area);

      Add_Row (0, "Xalign", X_Spin, 0.0, 1.0, 0.1, 0.5);
      Add_Row (1, "Yalign", Y_Spin, 0.0, 1.0, 0.1, 0.5);
      Add_Row (2, "Ratio", Ratio_Spin, 0.1, 5.0, 0.1, 2.0);
      X_Spin.Set_Digits (1);
      Y_Spin.Set_Digits (1);
      Ratio_Spin.Set_Digits (1);

      Gtk_New_With_Label (Obey, "Obey child");
      Obey.On_Toggled (Toggle'Access);
      Table.Attach (Obey, 0, 3, 2, 1);

      Box.Append (Aspect);
   end Run;

end Create_Aspect_Frame;
