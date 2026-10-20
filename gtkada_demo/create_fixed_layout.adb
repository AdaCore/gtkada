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

with Ada.Real_Time;     use Ada.Real_Time;
with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
with Glib;              use Glib;
with Glib.Main;
with Glib.Object;       use Glib.Object;
with Graphene.Point;    use Graphene.Point;
with Graphene.Point3d;  use Graphene.Point3d;
with Graphene.Vec3;     use Graphene.Vec3;
with Gsk.Transform;     use Gsk.Transform;
with Gtk.Box;           use Gtk.Box;
with Gtk.Button;        use Gtk.Button;
with Gtk.Check_Button;  use Gtk.Check_Button;
with Gtk.Css_Provider;  use Gtk.Css_Provider;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Fixed;         use Gtk.Fixed;
with Gtk.Label;         use Gtk.Label;
with Gtk.Scale;         use Gtk.Scale;
with Gtk.Style_Context; use Gtk.Style_Context;
with Gtk.Widget;        use Gtk.Widget;
with Interfaces.C;      use Interfaces.C;

package body Create_Fixed_Layout is
   type Demo_Kind is (Cube, Transformations);
   type Control_Kind is (Horizontal, Vertical, Angle, Size, Slant);
   type Controls_Array is array (Control_Kind) of Gtk_Scale;
   type Faces_Array is array (1 .. 6) of Gtk_Widget;
   type Stage_Record is new Gtk_Fixed_Record with record
      Kind                    : Demo_Kind;
      Controls                : Controls_Array;
      Faces                   : Faces_Array           := (others => null);
      Animate                 : Gtk_Check_Button;
      Info                    : Gtk_Label;
      Timer                   : Glib.Main.G_Source_Id := 0;
      Last_Width, Last_Height : Gint                  := -1;
      Last_Time               : Time                  := Time_First;
   end record;
   type Stage is access all Stage_Record'Class;
   Stage_Class : aliased Ada_GObject_Class := Uninitialized_Class;
   package Timers is new Glib.Main.Generic_Sources (Stage);
   use type Glib.Main.G_Source_Id;

   procedure Changed (Object : access GObject_Record'Class) is
      use Ada.Numerics.Elementary_Functions;
      Self           : constant Stage   := Stage (Object);
      T, Root        : Gsk_Transform;
      P              : aliased Graphene_Point_T;
      P3             : aliased Graphene_Point3D_T;
      X_Axis, Y_Axis : aliased Graphene_Vec3_T;
      Ignored        : access Graphene_Vec3_T;
      Width : constant C_float := C_float (Gint'Max (Self.Get_Width, 480));
      Height : constant C_float := C_float (Gint'Max (Self.Get_Height, 360));
      procedure Shift (X, Y : C_float) is
      begin
         P := (X, Y);
         T := T.Translate (P'Access);
      end Shift;
      procedure Shift_3D (X, Y, Z : C_float) is
      begin
         P3 := (X, Y, Z);
         T  := T.Translate_3D (P3'Access);
      end Shift_3D;
   begin
      Self.Last_Width  := Self.Get_Width;
      Self.Last_Height := Self.Get_Height;
      T                := Gsk_Transform_New;
      Shift (Width / 2.0, Height / 2.0);
      if Self.Kind = Transformations then
         Shift
           (C_float (Self.Controls (Horizontal).Get_Value),
            C_float (Self.Controls (Vertical).Get_Value));
         T := T.Rotate (C_float (Self.Controls (Angle).Get_Value));
         T :=
           T.Scale
             (C_float (Self.Controls (Size).Get_Value),
              C_float (Self.Controls (Size).Get_Value));
         T := T.Skew (C_float (Self.Controls (Slant).Get_Value), 0.0);
         Shift (-80.0, -80.0);
         Self.Set_Child_Transform (Self.Faces (1), T);
         Self.Info.Set_Text (T.To_String);
         T.Unref;
      else
         Ignored := Init (X_Axis'Access, 1.0, 0.0, 0.0);
         Ignored := Init (Y_Axis'Access, 0.0, 1.0, 0.0);
         T       := T.Perspective (C_float (Self.Controls (Size).Get_Value));
         T       :=
           T.Rotate_3D
             (C_float (Self.Controls (Vertical).Get_Value), X_Axis'Access);
         T       :=
           T.Rotate_3D
             (C_float (Self.Controls (Horizontal).Get_Value), Y_Axis'Access);
         Root    := T;
         declare
            type Depth_Array is array (1 .. 6) of Float;
            Depths   : Depth_Array;
            Order    : array (1 .. 6) of Positive := (1, 2, 3, 4, 5, 6);
            AX       : constant Float             :=
              Float (Self.Controls (Vertical).Get_Value) * Ada.Numerics.Pi /
              180.0;
            AY       : constant Float             :=
              Float (Self.Controls (Horizontal).Get_Value) * Ada.Numerics.Pi /
              180.0;
            Previous : Gtk_Widget                 := null;
            Temp     : Positive;
         begin
            --  Sort the face centres back to front: Gtk.Fixed snapshots in
            --  sibling order, rather than providing a depth buffer.
            Depths (1) := Cos (AX) * Cos (AY);
            Depths (2) := -Depths (1);
            Depths (3) := -Cos (AX) * Sin (AY);
            Depths (4) := -Depths (3);
            Depths (5) := -Sin (AX);
            Depths (6) := -Depths (5);
            for I in Order'Range loop
               T := Root.Ref;
               case I is
                  when 2 =>
                     T := T.Rotate_3D (180.0, Y_Axis'Access);
                  when 3 =>
                     T := T.Rotate_3D (90.0, Y_Axis'Access);
                  when 4 =>
                     T := T.Rotate_3D (-90.0, Y_Axis'Access);
                  when 5 =>
                     T := T.Rotate_3D (90.0, X_Axis'Access);
                  when 6 =>
                     T := T.Rotate_3D (-90.0, X_Axis'Access);
                  when others =>
                     null;
               end case;
               Shift_3D (0.0, 0.0, 80.0);
               Shift_3D (-80.0, -80.0, 0.0);
               Self.Set_Child_Transform (Self.Faces (I), T);
               T.Unref;
            end loop;
            for I in 1 .. 5 loop
               for J in I + 1 .. 6 loop
                  if Depths (Order (I)) > Depths (Order (J)) then
                     Temp      := Order (I);
                     Order (I) := Order (J);
                     Order (J) := Temp;
                  end if;
               end loop;
            end loop;
            for I of Order loop
               Self.Faces (I).Insert_After (Self, Previous);
               Previous := Self.Faces (I);
            end loop;
         end;
         Root.Unref;
      end if;
   end Changed;

   function Advance (Self : Stage) return Boolean is
      Now   : constant Time := Clock;
      Value : Gdouble;
   begin
      if Self.Kind = Cube and then Self.Animate.Get_Active then
         Value :=
           Self.Controls (Horizontal).Get_Value +
           Gdouble (To_Duration (Now - Self.Last_Time)) * 25.0;
         Self.Controls (Horizontal).Set_Value
           (Value - 360.0 * Gdouble'Floor (Value / 360.0));
      elsif Self.Last_Width /= Self.Get_Width
        or else Self.Last_Height /= Self.Get_Height
      then
         Changed (Self);
      end if;
      Self.Last_Time := Now;
      return True;
   end Advance;

   procedure Mapped (Object : access GObject_Record'Class) is
      Self : constant Stage := Stage (Object);
   begin
      Self.Last_Time := Clock;
      if Self.Timer = 0 then
         Self.Timer := Timers.Timeout_Add (16, Advance'Access, Self);
      end if;
   end Mapped;

   procedure Unmapped (Object : access GObject_Record'Class) is
      Self : constant Stage := Stage (Object);
   begin
      if Self.Timer /= 0 then
         Glib.Main.Remove (Self.Timer);
         Self.Timer := 0;
      end if;
   end Unmapped;

   procedure Reset (Object : access GObject_Record'Class) is
      Self : constant Stage := Stage (Object);
   begin
      Self.Controls (Horizontal).Set_Value
        ((if Self.Kind = Cube then 30.0 else 0.0));
      Self.Controls (Vertical).Set_Value
        ((if Self.Kind = Cube then -20.0 else 0.0));
      Self.Controls (Angle).Set_Value (0.0);
      Self.Controls (Size).Set_Value
        ((if Self.Kind = Cube then 600.0 else 1.0));
      Self.Controls (Slant).Set_Value (0.0);
      Changed (Self);
   end Reset;

   procedure Run
     (Frame : access Gtk.Frame.Gtk_Frame_Record'Class; Kind : Demo_Kind)
   is
      Self     : constant Stage := new Stage_Record;
      Box, Row : Gtk_Box;
      Button   : Gtk_Button;
      Provider : Gtk_Css_Provider;
      Face     : Gtk_Label;
      function Name (Control : Control_Kind) return String is
      begin
         case Control is
            when Horizontal =>
               return (if Kind = Cube then "Y rotation" else "X offset");
            when Vertical =>
               return (if Kind = Cube then "X rotation" else "Y offset");
            when Angle =>
               return "Rotation";
            when Size =>
               return (if Kind = Cube then "Perspective depth" else "Scale");
            when Slant =>
               return "X skew";
         end case;
      end Name;
   begin
      Initialize_Class_Record
        (Ancestor  => Gtk.Fixed.Get_Type, Class_Record => Stage_Class,
         Type_Name => "GtkAdaTransformStage");
      G_New (Self, Stage_Class.The_Type);
      Self.Kind := Kind;
      Gtk_New (Box, Orientation_Vertical, 8);
      Box.Set_Margin_Start (12);
      Box.Set_Margin_End (12);
      Box.Set_Margin_Top (12);
      Box.Set_Margin_Bottom (12);
      Frame.Set_Child (Box);
      for Control in Control_Kind loop
         Gtk_New (Row, Orientation_Horizontal, 8);
         Row.Append (Gtk_Label_New (Name (Control)));
         case Control is
            when Horizontal | Vertical =>
               Gtk_New_With_Range
                 (Self.Controls (Control), Orientation_Horizontal,
                  (if Kind = Cube and Control = Horizontal then 0.0
                   elsif Kind = Cube then -180.0 else -140.0),
                  (if Kind = Cube and Control = Horizontal then 360.0
                   elsif Kind = Cube then 180.0 else 140.0),
                  1.0);
            when Angle =>
               Gtk_New_With_Range
                 (Self.Controls (Control), Orientation_Horizontal, -180.0,
                  180.0, 1.0);
            when Size =>
               Gtk_New_With_Range
                 (Self.Controls (Control), Orientation_Horizontal,
                  (if Kind = Cube then 250.0 else 0.25),
                  (if Kind = Cube then 1_200.0 else 1.75),
                  (if Kind = Cube then 10.0 else 0.05));
            when Slant =>
               Gtk_New_With_Range
                 (Self.Controls (Control), Orientation_Horizontal, -45.0, 45.0,
                  1.0);
         end case;
         Self.Controls (Control).Set_Draw_Value (True);
         Self.Controls (Control).Set_Digits
           ((if Kind = Transformations and Control = Size then 2 else 0));
         Self.Controls (Control).Set_Hexpand (True);
         Row.Append (Self.Controls (Control));
         Box.Append (Row);
         Row.Set_Visible
           (Kind /= Cube or Control in Horizontal | Vertical | Size);
      end loop;
      Gtk_New (Row, Orientation_Horizontal, 8);
      Gtk_New_With_Label (Self.Animate, "Animate");
      Self.Animate.Set_Active (Kind = Cube);
      Row.Append (Self.Animate);
      Self.Animate.Set_Visible (Kind = Cube);
      Gtk_New (Button, "Reset");
      Button.On_Clicked (Reset'Access, Slot => Self);
      Row.Append (Button);
      Box.Append (Row);
      Self.Set_Size_Request (480, 360);
      Self.Set_Hexpand (True);
      Self.Set_Vexpand (True);
      Box.Append (Self);
      Gtk_New (Self.Info);
      Self.Info.Set_Wrap (True);
      Self.Info.Set_Selectable (True);
      Box.Append (Self.Info);
      Self.Info.Set_Visible (Kind = Transformations);
      Gtk_New (Provider);
      Provider.Load_From_String
        ("label { color: white; font-size: 24px; font-weight: bold; " &
         "border: 2px solid #243347; } " &
         ".face1 { background: #3584e4; } .face2 { background: #9141ac; } " &
         ".face3 { background: #e66100; } .face4 { background: #26a269; } " &
         ".face5 { background: #c01c28; } .face6 { background: #986a00; }");
      for I in 1 .. (if Kind = Cube then 6 else 1) loop
         Gtk_New
           (Face,
            (if Kind = Cube then
               (case I is when 1 => "Front", when 2 => "Back",
                  when 3 => "Right", when 4 => "Left", when 5 => "Top",
                  when others => "Bottom")
             else "GtkAda"));
         Face.Set_Size_Request (160, 160);
         Face.Add_Css_Class ("face" & Character'Val (Character'Pos ('0') + I));
         Get_Style_Context (Face).Add_Provider (+Provider, 800);
         Self.Faces (I) := Gtk_Widget (Face);
         Self.Put (Face, 0.0, 0.0);
      end loop;
      Provider.Unref;
      Reset (Self);
      for Control in Control_Kind loop
         Self.Controls (Control).On_Value_Changed
           (Changed'Access, Slot => Self);
      end loop;
      Self.On_Map (Mapped'Access, Slot => Self);
      Self.On_Unmap (Unmapped'Access, Slot => Self);
      Self.On_Destroy (Unmapped'Access, Slot => Self);
   end Run;

   procedure Run_Cube (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
   begin
      Run (Frame, Cube);
   end Run_Cube;

   function Help_Cube return String is
   begin
      return
        "Six widgets form a cube using Gsk.Transform.Translate_3D, " &
        "Rotate_3D and Perspective. Pause the animation to inspect the " &
        "rotation and perspective depth. Faces are ordered back to front. " &
        "A renderer supporting 3D transforms is required (GL or Vulkan).";
   end Help_Cube;

   procedure Run_Transformations
     (Frame : access Gtk.Frame.Gtk_Frame_Record'Class)
   is
   begin
      Run (Frame, Transformations);
   end Run_Transformations;

   function Help_Transformations return String is
   begin
      return
        "Compose translation, rotation, scale and skew on a Gtk.Fixed " &
        "child. Rotation and scaling use the centre of the widget as their " &
        "origin. The displayed transform lists the operations in order; " &
        "they act on points from right to left. Reset restores the defaults.";
   end Help_Transformations;
end Create_Fixed_Layout;
