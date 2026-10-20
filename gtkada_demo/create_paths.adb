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

with Interfaces.C;     use Interfaces.C;
with System;
with Glib;             use Glib;
with Glib.Main;
with Glib.Object;      use Glib.Object;
with Gdk.RGBA;         use Gdk.RGBA;
with Graphene.Point;   use Graphene.Point;
with Graphene.Rect;    use Graphene.Rect;
with Gsk.Enums;        use Gsk.Enums;
with Gsk.Path;         use Gsk.Path;
with Gsk.Path_Builder; use Gsk.Path_Builder;
with Gsk.Path_Measure; use Gsk.Path_Measure;
with Gsk.Path_Point;   use Gsk.Path_Point;
with Gsk.Stroke;       use Gsk.Stroke;
with Gtk.Box;          use Gtk.Box;
with Gtk.Button;       use Gtk.Button;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Editable;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.GEntry;       use Gtk.GEntry;
with Gtk.Label;        use Gtk.Label;
with Gtk.Scale;        use Gtk.Scale;
with Gtk.Snapshot;     use Gtk.Snapshot;
with Gtk.Widget;       use Gtk.Widget;
with Pango.Font;
with Pango.Layout;     use Pango.Layout;

package body Create_Paths is
   type Demo_Kind is
     (Fill_And_Stroke, Maze, Spinner, Sweep, Text, Walk, Explorer);
   type Canvas_Record is new Gtk_Widget_Record with record
      Kind     : Demo_Kind;
      Width    : Gtk_Scale;
      Position : Gtk_Scale;
      Option   : Gtk_Check_Button;
      Animate  : Gtk_Check_Button;
      Input    : Gtk_Entry;
      Info     : Gtk_Label;
      Seed     : Natural               := 1;
      Timer    : Glib.Main.G_Source_Id := 0;
   end record;
   type Canvas is access all Canvas_Record'Class;
   Canvas_Class : aliased Ada_GObject_Class := Uninitialized_Class;
   package Timers is new Glib.Main.Generic_Sources (Canvas);
   use type Glib.Main.G_Source_Id;
   use type System.Address;

   Blue       : constant Gdk_RGBA := (0.12, 0.42, 0.82, 1.0);
   Orange     : constant Gdk_RGBA := (0.95, 0.35, 0.10, 1.0);
   Grey       : constant Gdk_RGBA := (0.60, 0.65, 0.72, 1.0);
   Background : constant Gdk_RGBA := (0.97, 0.97, 0.98, 1.0);
   Curve      : constant String   :=
     "M 60 240 C 80 30 240 30 300 180 S 520 330 540 100";

   function Build_Path (Self : Canvas) return Gsk_Path;

   --  A deterministic depth-first maze. Every cell is visited exactly once;
   --  removed walls form a spanning tree, hence there is one route between
   --  any two cells. The button changes the traversal order.
   procedure Add_Maze (Builder : Gsk_Path_Builder; Seed : Natural) is
      Columns : constant := 14;
      Rows    : constant := 8;
      type Cell is record
         Seen                    : Boolean := False;
         Right_Wall, Bottom_Wall : Boolean := True;
      end record;
      Cells : array (1 .. Columns, 1 .. Rows) of Cell;
      State : Guint32 := Guint32 (Seed);
      procedure Visit (X, Y : Integer) is
         DX : constant array (0 .. 3) of Integer := (1, 0, -1, 0);
         DY : constant array (0 .. 3) of Integer := (0, 1, 0, -1);
         First, D, NX, NY : Integer;
      begin
         Cells (X, Y).Seen := True;
         State             := State * 1_664_525 + 1_013_904_223;
         First             := Integer ((State / 65_536) mod 4);
         for I in 0 .. 3 loop
            D  := (First + I) mod 4;
            NX := X + DX (D);
            NY := Y + DY (D);
            if NX in 1 .. Columns and then NY in 1 .. Rows
              and then not Cells (NX, NY).Seen
            then
               case D is
                  when 0 =>
                     Cells (X, Y).Right_Wall := False;
                  when 1 =>
                     Cells (X, Y).Bottom_Wall := False;
                  when 2 =>
                     Cells (NX, NY).Right_Wall := False;
                  when 3 =>
                     Cells (NX, NY).Bottom_Wall := False;
                  when others =>
                     null;
               end case;
               Visit (NX, NY);
            end if;
         end loop;
      end Visit;
      procedure Line (X1, Y1, X2, Y2 : C_float) is
      begin
         Builder.Move_To (X1, Y1);
         Builder.Line_To (X2, Y2);
      end Line;
   begin
      Visit (1, 1);
      Line (48.0, 36.0, 552.0, 36.0);
      Line (48.0, 72.0, 48.0, 324.0);
      for X in 1 .. Columns loop
         for Y in 1 .. Rows loop
            if Cells (X, Y).Right_Wall and then (X /= Columns or Y /= Rows)
            then
               Line
                 (48.0 + C_float (X) * 36.0, C_float (Y) * 36.0,
                  48.0 + C_float (X) * 36.0, C_float (Y + 1) * 36.0);
            end if;
            if Cells (X, Y).Bottom_Wall then
               Line
                 (48.0 + C_float (X - 1) * 36.0, C_float (Y + 1) * 36.0,
                  48.0 + C_float (X) * 36.0, C_float (Y + 1) * 36.0);
            end if;
         end loop;
      end loop;
   end Add_Maze;

   function Build_Path (Self : Canvas) return Gsk_Path is
      Builder : Gsk_Path_Builder;
      Result  : Gsk_Path;
      Center  : aliased Graphene_Point_T := (300.0, 180.0);
   begin
      if Self.Kind in Sweep | Walk then
         return Parse (Curve);
      elsif Self.Kind = Explorer then
         return Parse (Self.Input.Get_Text);
      end if;
      Gtk_New (Builder);
      case Self.Kind is
         when Fill_And_Stroke =>
            Center.X := 245.0;
            Builder.Add_Circle (Center'Access, 115.0);
            Center.X := 355.0;
            Builder.Add_Circle (Center'Access, 115.0);
         when Maze =>
            Add_Maze (Builder, Self.Seed);
         when Spinner =>
            Builder.Add_Circle (Center'Access, 105.0);
         when Text =>
            declare
               Layout : constant Pango_Layout             :=
                 Self.Create_Pango_Layout (Self.Input.Get_Text);
               Font   : Pango.Font.Pango_Font_Description :=
                 Pango.Font.From_String ("Sans Bold 80");
            begin
               Layout.Set_Font_Description (Font);
               Builder.Move_To (30.0, 100.0);
               Builder.Add_Layout (Layout);
               Layout.Unref;
               Pango.Font.Free (Font);
            end;
         when others =>
            null;
      end case;
      Result := Builder.To_Path;
      Builder.Unref;
      return Result;
   end Build_Path;

   procedure Changed (Object : access GObject_Record'Class) is
      Self   : constant Canvas   := Canvas (Object);
      Path   : constant Gsk_Path := Build_Path (Self);
      Bounds : aliased Graphene_Rect_T;
   begin
      if Get_Object (Path) = System.Null_Address then
         Self.Info.Set_Text
           ("Invalid SVG path — enter M, L, C, Q, A or Z commands.");
      else
         declare
            Measure : constant Gsk_Path_Measure := Gsk_Path_Measure_New (Path);
         begin
            if Path.Get_Bounds (Bounds'Access) then
               Self.Info.Set_Text
                 ("Length:" & Measure.Get_Length'Image & "    Bounds:" &
                  Bounds.Size.Width'Image & " ×" & Bounds.Size.Height'Image);
            else
               Self.Info.Set_Text ("Empty path");
            end if;
            Measure.Unref;
         end;
         Path.Unref;
      end if;
      Self.Queue_Draw;
   end Changed;

   procedure Regenerate (Object : access GObject_Record'Class) is
      Self : constant Canvas := Canvas (Object);
   begin
      Self.Seed := Self.Seed + 1;
      Changed (Self);
   end Regenerate;

   function Advance (Self : Canvas) return Boolean is
      Value : Gdouble;
   begin
      if Self.Animate.Get_Active then
         Value := Self.Position.Get_Value + 0.006;
         if Value >= 1.0 then
            Value := Value - 1.0;
         end if;
         Self.Position.Set_Value (Value);
      end if;
      return True;
   end Advance;

   procedure Mapped (Object : access GObject_Record'Class) is
      Self : constant Canvas := Canvas (Object);
   begin
      if Self.Timer = 0 and then Self.Kind in Spinner | Sweep | Walk then
         Self.Timer := Timers.Timeout_Add (16, Advance'Access, Self);
      end if;
   end Mapped;

   procedure Unmapped (Object : access GObject_Record'Class) is
      Self : constant Canvas := Canvas (Object);
   begin
      if Self.Timer /= 0 then
         Glib.Main.Remove (Self.Timer);
         Self.Timer := 0;
      end if;
   end Unmapped;

   procedure Render (Widget, Snapshot : System.Address) with
     Convention => C;
   procedure Render (Widget, Snapshot : System.Address) is
      Stub      : Canvas_Record;
      Self      : constant Canvas := Canvas (Get_User_Data (Widget, Stub));
      Snap_Stub : Gtk_Snapshot_Record;
      Snap      : constant Gtk_Snapshot   :=
        Gtk_Snapshot (Get_User_Data (Snapshot, Snap_Stub));
      Bounds    : aliased Graphene_Rect_T :=
        ((0.0, 0.0), (C_float (Self.Get_Width), C_float (Self.Get_Height)));
      Origin    : aliased Graphene_Point_T;
      Scale     : C_float                 :=
        C_float'Min (Bounds.Size.Width / 600.0, Bounds.Size.Height / 360.0);
      Path      : constant Gsk_Path       := Build_Path (Self);
      Stroke    : constant Gsk_Stroke     :=
        Gsk_Stroke_New (C_float (Self.Width.Get_Value));
      Fraction  : constant C_float        := C_float (Self.Position.Get_Value);
      Rule      : Gsk_Fill_Rule           := Gsk_Fill_Rule_Winding;
   begin
      Snap.Append_Color (Background, Bounds'Access);
      if Get_Object (Path) = System.Null_Address then
         Stroke.Free;
         return;
      end if;
      Snap.Save;
      Origin :=
        ((Bounds.Size.Width - 600.0 * Scale) / 2.0,
         (Bounds.Size.Height - 360.0 * Scale) / 2.0);
      if Self.Kind in Text | Explorer then
         declare
            Path_Bounds : aliased Graphene_Rect_T;
            Padding     : constant C_float := 40.0;
         begin
            if Path.Get_Bounds (Path_Bounds'Access) then
               Scale  :=
                 C_float'Min
                   (Bounds.Size.Width / (Path_Bounds.Size.Width + Padding),
                    Bounds.Size.Height / (Path_Bounds.Size.Height + Padding));
               Origin :=
                 ((Bounds.Size.Width - Path_Bounds.Size.Width * Scale) / 2.0 -
                  Path_Bounds.Origin.X * Scale,
                  (Bounds.Size.Height - Path_Bounds.Size.Height * Scale) /
                  2.0 -
                  Path_Bounds.Origin.Y * Scale);
            end if;
         end;
      end if;
      Snap.Translate (Origin'Access);
      Snap.Scale (Scale, Scale);
      Stroke.Set_Line_Cap (Gsk_Line_Cap_Round);
      Stroke.Set_Line_Join (Gsk_Line_Join_Round);
      if Self.Option.Get_Active then
         if Self.Kind = Fill_And_Stroke then
            Rule := Gsk_Fill_Rule_Even_Odd;
         else
            Stroke.Set_Dash ((12.0, 8.0));
         end if;
      end if;
      if Self.Kind in Fill_And_Stroke | Text then
         Snap.Append_Fill (Path, Rule, Blue);
         Snap.Append_Stroke (Path, Stroke, Orange);
      elsif Self.Kind in Spinner | Sweep | Walk | Explorer then
         declare
            Measure : constant Gsk_Path_Measure := Gsk_Path_Measure_New (Path);
            Length        : constant C_float          := Measure.Get_Length;
            Start, Finish : aliased Gsk_Path_Point;
            Position      : Graphene_Point_T;
            Builder       : Gsk_Path_Builder;
            Segment       : Gsk_Path;
         begin
            Snap.Append_Stroke (Path, Stroke, Grey);
            if Length > 0.0
              and then Measure.Get_Point (Length * Fraction, Finish'Access)
            then
               if Self.Kind in Spinner | Sweep then
                  if (Self.Kind = Spinner or else Fraction > 0.0)
                    and then Measure.Get_Point
                      (Length *
                       (if Self.Kind = Spinner then
                          (if Fraction < 0.25 then Fraction + 0.75
                           else Fraction - 0.25)
                        else 0.0),
                       Start'Access)
                  then
                     Gtk_New (Builder);
                     Builder.Add_Segment (Path, Start'Access, Finish'Access);
                     Segment := Builder.To_Path;
                     Snap.Append_Stroke (Segment, Stroke, Blue);
                     Segment.Unref;
                     Builder.Unref;
                  end if;
               else
                  Get_Position (Finish'Access, Path, Position);
                  Origin := Position;
                  Snap.Save;
                  Snap.Translate (Origin'Access);
                  Snap.Rotate
                    (Get_Rotation (Finish'Access, Path, Gsk_Path_To_End));
                  Segment := Parse ("M 16 0 L -10 -8 L -5 0 L -10 8 Z");
                  Snap.Append_Fill (Segment, Gsk_Fill_Rule_Winding, Orange);
                  Segment.Unref;
                  Snap.Restore;
               end if;
            end if;
            Measure.Unref;
         end;
      else
         Snap.Append_Stroke (Path, Stroke, Blue);
      end if;
      Snap.Restore;
      Stroke.Free;
      Path.Unref;
   end Render;

   procedure Class_Init (Self : GObject_Class) with
     Convention => C;
   procedure Class_Init (Self : GObject_Class) is
   begin
      Set_Default_Snapshot_Handler (Self, Render'Access);
   end Class_Init;

   procedure Run
     (Frame : access Gtk.Frame.Gtk_Frame_Record'Class; Kind : Demo_Kind)
   is
      Box, Controls : Gtk_Box;
      Self          : constant Canvas := new Canvas_Record;
      Button        : Gtk_Button;
   begin
      Initialize_Class_Record
        (Ancestor  => Gtk.Widget.Get_Type, Class_Record => Canvas_Class,
         Type_Name => "GtkAdaPathCanvas", Class_Init => Class_Init'Access);
      G_New (Self, Canvas_Class.The_Type);
      Self.Kind := Kind;
      Gtk_New (Box, Orientation_Vertical, 8);
      Box.Set_Margin_Start (12);
      Box.Set_Margin_End (12);
      Box.Set_Margin_Top (12);
      Box.Set_Margin_Bottom (12);
      Frame.Set_Child (Box);
      Gtk_New (Controls, Orientation_Horizontal, 8);
      Controls.Append (Gtk_Label_New ("Stroke width"));
      Gtk_New_With_Range (Self.Width, Orientation_Horizontal, 1.0, 24.0, 0.5);
      Self.Width.Set_Value (if Kind = Maze then 3.0 else 8.0);
      Self.Width.Set_Hexpand (True);
      Controls.Append (Self.Width);
      Gtk_New_With_Label
        (Self.Option,
         (if Kind = Fill_And_Stroke then "Even-odd fill"
          else "Dashed stroke"));
      Controls.Append (Self.Option);
      Box.Append (Controls);
      Gtk_New_With_Label (Self.Animate, "Animate");
      Self.Animate.Set_Active (Kind in Spinner | Sweep | Walk);
      Box.Append (Self.Animate);
      Self.Animate.Set_Visible (Kind in Spinner | Sweep | Walk);
      Gtk_New_With_Range
        (Self.Position, Orientation_Horizontal, 0.0, 1.0, 0.001);
      Self.Position.Set_Value (0.40);
      if Kind in Spinner | Sweep | Walk | Explorer then
         Box.Append (Gtk_Label_New ("Position along the path"));
      end if;
      Box.Append (Self.Position);
      Self.Position.Set_Visible (Kind in Spinner | Sweep | Walk | Explorer);
      Gtk_New (Self.Input);
      Self.Input.Set_Text ((if Kind = Text then "GtkAda" else Curve));
      Box.Append (Self.Input);
      Self.Input.Set_Visible (Kind in Text | Explorer);
      if Kind = Maze then
         Gtk_New (Button, "New maze");
         Button.On_Clicked (Regenerate'Access, Slot => Self);
         Box.Append (Button);
      end if;
      Self.Set_Size_Request (300, 200);
      Self.Set_Hexpand (True);
      Self.Set_Vexpand (True);
      Box.Append (Self);
      Gtk_New (Self.Info);
      Box.Append (Self.Info);
      Self.Width.On_Value_Changed (Changed'Access, Slot => Self);
      Self.Position.On_Value_Changed (Changed'Access, Slot => Self);
      Self.Option.On_Toggled (Changed'Access, Slot => Self);
      Gtk.Editable.On_Changed (+Self.Input, Changed'Access, Slot => Self);
      Self.On_Map (Mapped'Access, Slot => Self);
      Self.On_Unmap (Unmapped'Access, Slot => Self);
      Self.On_Destroy (Unmapped'Access, Slot => Self);
      Changed (Self);
   end Run;

   procedure Run_Fill_And_Stroke
     (Frame : access Gtk.Frame.Gtk_Frame_Record'Class)
   is
   begin
      Run (Frame, Fill_And_Stroke);
   end Run_Fill_And_Stroke;

   function Help_Fill_And_Stroke return String is
   begin
      return
        "Overlapping contours show winding and even-odd fill rules. "
        & "Change the stroke width to compare filled geometry with its "
        & "outline.";
   end Help_Fill_And_Stroke;

   procedure Run_Maze (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
   begin
      Run (Frame, Maze);
   end Run_Maze;

   function Help_Maze return String is
   begin
      return
        "A depth-first search builds a perfect maze from independent line "
        & "contours. Generate another maze and experiment with stroke "
        & "widths, round joins and dashes.";
   end Help_Maze;

   procedure Run_Spinner (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
   begin
      Run (Frame, Spinner);
   end Run_Spinner;

   function Help_Spinner return String is
   begin
      return
        "A rotating segment of a circular path. PathMeasure locates its "
        & "endpoints and PathBuilder extracts the segment. Pause to inspect "
        & "any position.";
   end Help_Spinner;

   procedure Run_Sweep (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
   begin
      Run (Frame, Sweep);
   end Run_Sweep;

   function Help_Sweep return String is
   begin
      return
        "A measured Bezier path is progressively revealed with "
        & "Add_Segment. Pause and move the position slider to inspect the "
        & "sweep.";
   end Help_Sweep;

   procedure Run_Text (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
   begin
      Run (Frame, Text);
   end Run_Text;

   function Help_Text return String is
   begin
      return
        "Edit the text to turn a Pango layout into filled and stroked "
        & "glyph outlines using PathBuilder.Add_Layout.";
   end Help_Text;

   procedure Run_Walk (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
   begin
      Run (Frame, Walk);
   end Run_Walk;

   function Help_Walk return String is
   begin
      return
        "An arrow follows a Bezier path at constant distance increments. "
        & "PathPoint supplies both its position and tangent rotation.";
   end Help_Walk;

   procedure Run_Explorer (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
   begin
      Run (Frame, Explorer);
   end Run_Explorer;

   function Help_Explorer return String is
   begin
      return
        "Enter SVG path commands. The explorer reports length and bounds, "
        & "rejects invalid input, and lets you inspect the position and "
        & "tangent along the path.";
   end Help_Explorer;
end Create_Paths;
