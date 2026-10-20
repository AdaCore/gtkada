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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Glib;                  use Glib;
with Glib.Error;            use Glib.Error;
with Glib.Main;
with Glib.Object;           use Glib.Object;
with GNAT.Strings;
with Gtk.Box;               use Gtk.Box;
with Gtk.Builder;           use Gtk.Builder;
with Gtk.Button;            use Gtk.Button;
with Gtk.Constraint;        use Gtk.Constraint;
with Gtk.Constraint_Guide;  use Gtk.Constraint_Guide;
with Gtk.Constraint_Layout; use Gtk.Constraint_Layout;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Gesture_Drag;      use Gtk.Gesture_Drag;
with Gtk.Label;             use Gtk.Label;
with Gtk.Scale;             use Gtk.Scale;
with Gtk.Scrolled_Window;   use Gtk.Scrolled_Window;
with Gtk.Text_Buffer;       use Gtk.Text_Buffer;
with Gtk.Text_Iter;         use Gtk.Text_Iter;
with Gtk.Text_View;         use Gtk.Text_View;
with Gtk.Widget;            use Gtk.Widget;

package body Create_Constraints is
   Required : constant Gint :=
     Gtk_Constraint_Strength'Enum_Rep (Constraint_Strength_Required);
   Strong   : constant Gint :=
     Gtk_Constraint_Strength'Enum_Rep (Constraint_Strength_Strong);
   type Demo_Kind is (Simple, Interactive, VFL);
   type Stage_Record is new Gtk_Box_Record with record
      Kind       : Demo_Kind;
      Layout     : Gtk_Constraint_Layout;
      Guide      : Gtk_Constraint_Guide;
      Position   : Gtk_Constraint        := null;
      A, B, C    : Gtk_Button;
      Control    : Gtk_Scale;
      Input      : Gtk_Text_Buffer;
      Info       : Gtk_Label;
      Drag_Start : Gdouble               := 0.0;
      Last_Width : Gint                  := 0;
      Timer      : Glib.Main.G_Source_Id := 0;
   end record;
   type Stage is access all Stage_Record'Class;
   package Timers is new Glib.Main.Generic_Sources (Stage);
   use type Glib.Main.G_Source_Id;
   Stage_Class : aliased Ada_GObject_Class := Uninitialized_Class;

   procedure Link
     (Layout      : Gtk_Constraint_Layout; Target : Gtk_Constraint_Target;
      Attr        : Gtk_Constraint_Attribute; Source : Gtk_Constraint_Target;
      Source_Attr : Gtk_Constraint_Attribute; Offset : Gdouble := 0.0;
      Multiplier  : Gdouble := 1.0; Strength : Gint := Required)
   is
   begin
      Layout.Add_Constraint
        (Gtk_Constraint_New
           (Target, Attr, Constraint_Relation_Eq, Source, Source_Attr,
            Multiplier, Offset, Strength));
   end Link;

   procedure Changed (Object : access GObject_Record'Class) is
      Self          : constant Stage := Stage (Object);
      Width, Height : Gint;
      Value         : Gint;
   begin
      if Self.Kind = Simple then
         Value := Gint (Self.Control.Get_Value);
         Self.Guide.Get_Min_Size (Width, Height);
         --  Update the bounds in an order that keeps min <= max while live.
         if Value > Width then
            Self.Guide.Set_Max_Size (Value, 0);
            Self.Guide.Set_Min_Size (Value, 0);
         else
            Self.Guide.Set_Min_Size (Value, 0);
            Self.Guide.Set_Max_Size (Value, 0);
         end if;
         Self.Guide.Set_Nat_Size (Value, 0);
         Self.Info.Set_Text
           ("Equal widths above; full width below. The invisible guide " &
            "sets the horizontal gap.");
      else
         if Self.Position /= null then
            Self.Layout.Remove_Constraint (Self.Position);
         end if;
         --  Resolve the proportion to a pixel offset after allocation.
         --  A relation to the parent's width would also feed the requested
         --  position back into the solver's size measurement.
         Self.Last_Width := Gint'Max (360, Self.Get_Width);
         Self.Position   :=
           Gtk_Constraint_New
             (+Self.Guide, Constraint_Attribute_Center_X,
              Constraint_Relation_Eq, Null_Gtk_Constraint_Target,
              Constraint_Attribute_Left, 1.0,
              Gdouble'Max
                (112.0,
                 Gdouble'Min
                   (Gdouble (Self.Last_Width) - 112.0,
                    Gdouble (Self.Last_Width) * Self.Control.Get_Value /
                    100.0)),
              Strong);
         Self.Layout.Add_Constraint (Self.Position);
         Self.Info.Set_Text
           ("Drag the divider or move the slider. Required minimum widths " &
            "take priority over the requested divider position.");
      end if;
   end Changed;

   function Check_Size (Self : Stage) return Boolean is
   begin
      if Self.Get_Width /= Self.Last_Width then
         Changed (Self);
      end if;
      return True;
   end Check_Size;

   procedure Mapped (Object : access GObject_Record'Class) is
      Self : constant Stage := Stage (Object);
   begin
      Changed (Self);
      if Self.Timer = 0 then
         Self.Timer := Timers.Timeout_Add (50, Check_Size'Access, Self);
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

   procedure Drag_Begin (Object : access GObject_Record'Class; X, Y : Gdouble)
   is
      pragma Unreferenced (X, Y);
      Self : constant Stage := Stage (Object);
   begin
      Self.Drag_Start := Self.Control.Get_Value;
   end Drag_Begin;

   procedure Drag_Update (Object : access GObject_Record'Class; X, Y : Gdouble)
   is
      pragma Unreferenced (Y);
      Self : constant Stage := Stage (Object);
   begin
      Self.Control.Set_Value
        (Self.Drag_Start + X * 100.0 / Gdouble (Gint'Max (1, Self.Get_Width)));
   end Drag_Update;

   procedure Reset (Object : access GObject_Record'Class) is
      Self : constant Stage := Stage (Object);
   begin
      Self.Control.Set_Value ((if Self.Kind = Simple then 24.0 else 50.0));
   end Reset;

   procedure Build_Layout (Self : Stage) is
      Super   : constant Gtk_Constraint_Target := Null_Gtk_Constraint_Target;
      Divider : Gtk_Label;
      Drag    : Gtk_Gesture_Drag;
   begin
      Gtk_New (Self.Guide);
      Self.Guide.Set_Name ("horizontal-gap");
      Self.Guide.Set_Min_Size ((if Self.Kind = Interactive then 32 else 8), 0);
      Self.Guide.Set_Nat_Size
        ((if Self.Kind = Interactive then 32 else 24), 0);
      Self.Guide.Set_Max_Size
        ((if Self.Kind = Interactive then 32 else 120), 0);
      Self.Guide.Set_Strength (Constraint_Strength_Strong);
      Self.Layout.Add_Guide (Self.Guide);
      Link
        (Self.Layout, +Self.A, Constraint_Attribute_Start, Super,
         Constraint_Attribute_Start, 16.0);
      Link
        (Self.Layout, +Self.A, Constraint_Attribute_End, +Self.Guide,
         Constraint_Attribute_Start);
      Link
        (Self.Layout, +Self.B, Constraint_Attribute_Start, +Self.Guide,
         Constraint_Attribute_End);
      Link
        (Self.Layout, +Self.B, Constraint_Attribute_End, Super,
         Constraint_Attribute_End, -16.0);
      Link
        (Self.Layout, +Self.A, Constraint_Attribute_Top, Super,
         Constraint_Attribute_Top, 16.0);
      Link
        (Self.Layout, +Self.B, Constraint_Attribute_Top, +Self.A,
         Constraint_Attribute_Top);
      Link
        (Self.Layout, +Self.B, Constraint_Attribute_Bottom, +Self.A,
         Constraint_Attribute_Bottom);
      Link
        (Self.Layout, +Self.C, Constraint_Attribute_Start, +Self.A,
         Constraint_Attribute_Start);
      Link
        (Self.Layout, +Self.C, Constraint_Attribute_End, +Self.B,
         Constraint_Attribute_End);
      Link
        (Self.Layout, +Self.C, Constraint_Attribute_Top, +Self.A,
         Constraint_Attribute_Bottom, 16.0);
      Link
        (Self.Layout, +Self.C, Constraint_Attribute_Bottom, Super,
         Constraint_Attribute_Bottom, -16.0);
      Link
        (Self.Layout, +Self.C, Constraint_Attribute_Height, +Self.A,
         Constraint_Attribute_Height);
      if Self.Kind = Simple then
         Link
           (Self.Layout, +Self.A, Constraint_Attribute_Width, +Self.B,
            Constraint_Attribute_Width);
      else
         Self.Layout.Add_Constraint
           (Gtk_Constraint_New_Constant
              (+Self.A, Constraint_Attribute_Width, Constraint_Relation_Ge,
               80.0, Required));
         Self.Layout.Add_Constraint
           (Gtk_Constraint_New_Constant
              (+Self.B, Constraint_Attribute_Width, Constraint_Relation_Ge,
               80.0, Required));
         Gtk_New (Divider, "↔");
         Divider.Add_Css_Class ("accent");
         Divider.Set_Can_Target (True);
         Divider.Set_Cursor_From_Name ("col-resize");
         Self.Append (Divider);
         Link
           (Self.Layout, +Divider, Constraint_Attribute_Start, +Self.Guide,
            Constraint_Attribute_Start);
         Link
           (Self.Layout, +Divider, Constraint_Attribute_End, +Self.Guide,
            Constraint_Attribute_End);
         Link
           (Self.Layout, +Divider, Constraint_Attribute_Top, +Self.A,
            Constraint_Attribute_Top);
         Link
           (Self.Layout, +Divider, Constraint_Attribute_Bottom, +Self.A,
            Constraint_Attribute_Bottom);
         Gtk_New (Drag);
         Drag.On_Drag_Begin (Drag_Begin'Access, Slot => Self);
         Drag.On_Drag_Update (Drag_Update'Access, Slot => Self);
         --  Keep drag coordinates in the stationary container: attaching
         --  to the moving divider makes its own motion alter the offsets.
         Drag.Set_Propagation_Phase (Phase_Capture);
         Self.Add_Controller (Drag);
      end if;
   end Build_Layout;

   VFL_Source : constant String :=
     "H:|-[a(==b)]-[b]-|" & ASCII.LF & "H:|-[c]-|" & ASCII.LF &
     "V:|-[a(==c)]-[c]-|" & ASCII.LF & "V:|-[b(==c)]-[c]-|";

   procedure Apply_VFL (Object : access GObject_Record'Class) is
      Self        : constant Stage := Stage (Object);
      First, Last : Gtk_Text_Iter;
      Layout      : Gtk_Constraint_Layout;
      Error       : GError;
      List        : Object_Simple_List.Glist;
   begin
      Self.Input.Get_Bounds (First, Last);
      declare
         Source : constant String := Self.Input.Get_Text (First, Last);
         Count  : Positive        := 1;
      begin
         for Ch of Source loop
            if Ch = ASCII.LF then
               Count := Count + 1;
            end if;
         end loop;
         declare
            Lines : GNAT.Strings.String_List (1 .. Count);
            Start : Positive                  := Source'First;
            Index : Positive                  := 1;
            Views : constant Named_View_Array :=
              ((To_Unbounded_String ("a"), +Self.A),
               (To_Unbounded_String ("b"), +Self.B),
               (To_Unbounded_String ("c"), +Self.C));
         begin
            for I in Source'Range loop
               if Source (I) = ASCII.LF then
                  Lines (Index) := new String'(Source (Start .. I - 1));
                  Start         := I + 1;
                  Index         := Index + 1;
               end if;
            end loop;
            Lines (Index) := new String'(Source (Start .. Source'Last));
            Gtk_New (Layout);
            List :=
              Layout.Add_Constraints_From_Description
                (Lines, Views, Error, Hspacing => 16, Vspacing => 16);
            for Line of Lines loop
               GNAT.Strings.Free (Line);
            end loop;
            Object_Simple_List.Free (List);
            if Error /= null then
               Self.Info.Set_Text (Get_Message (Error));
               Error_Free (Error);
               Layout.Unref;
            else
               Self.Set_Layout_Manager (Layout);
               Self.Layout := Layout;
               Self.Info.Set_Text
                 ("VFL applied. Resize the window to see the constraints adapt.");
            end if;
         end;
      end;
   end Apply_VFL;

   procedure Run
     (Frame : access Gtk.Frame.Gtk_Frame_Record'Class; Kind : Demo_Kind)
   is
      Self     : constant Stage := new Stage_Record;
      Box, Row : Gtk_Box;
      Button   : Gtk_Button;
      View     : Gtk_Text_View;
      Scroll   : Gtk_Scrolled_Window;
   begin
      Initialize_Class_Record
        (Ancestor  => Gtk.Box.Get_Type, Class_Record => Stage_Class,
         Type_Name => "GtkAdaConstraintStage");
      G_New (Self, Stage_Class.The_Type);
      Self.Kind := Kind;
      Gtk_New (Self.Layout);
      Self.Set_Layout_Manager (Self.Layout);
      Gtk_New (Box, Orientation_Vertical, 8);
      Box.Set_Margin_Start (12);
      Box.Set_Margin_End (12);
      Box.Set_Margin_Top (12);
      Box.Set_Margin_Bottom (12);
      Frame.Set_Child (Box);
      Gtk_New (Self.Info);
      Self.Info.Set_Wrap (True);
      if Kind = VFL then
         Gtk_New (View);
         Self.Input := View.Get_Buffer;
         View.Set_Monospace (True);
         Self.Input.Set_Text (VFL_Source);
         Gtk_New (Scroll);
         Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
         Scroll.Set_Min_Content_Height (120);
         Scroll.Set_Child (View);
         Box.Append (Scroll);
         Gtk_New (Button, "Apply VFL");
         Button.On_Clicked (Apply_VFL'Access, Slot => Self);
         Box.Append (Button);
      else
         Gtk_New (Row, Orientation_Horizontal, 8);
         Row.Append
           (Gtk_Label_New
              ((if Kind = Simple then "Guide gap"
                else "Divider position (%)")));
         Gtk_New_With_Range
           (Self.Control, Orientation_Horizontal,
            (if Kind = Simple then 8.0 else 15.0),
            (if Kind = Simple then 120.0 else 85.0), 1.0);
         Self.Control.Set_Hexpand (True);
         Self.Control.Set_Draw_Value (True);
         Self.Control.Set_Digits (0);
         Row.Append (Self.Control);
         Gtk_New (Button, "Reset");
         Button.On_Clicked (Reset'Access, Slot => Self);
         Row.Append (Button);
         Box.Append (Row);
      end if;
      Gtk_New (Self.A, "Child 1");
      Gtk_New (Self.B, "Child 2");
      Gtk_New (Self.C, "Child 3");
      Self.A.Add_Css_Class ("suggested-action");
      Self.B.Add_Css_Class ("suggested-action");
      Self.Append (Self.A);
      Self.Append (Self.B);
      Self.Append (Self.C);
      Self.Set_Size_Request (360, 240);
      Self.Set_Hexpand (True);
      Self.Set_Vexpand (True);
      Box.Append (Self);
      Box.Append (Self.Info);
      if Kind = VFL then
         Apply_VFL (Self);
      else
         Build_Layout (Self);
         Reset (Self);
         Changed (Self);
         Self.Control.On_Value_Changed (Changed'Access, Slot => Self);
         if Kind = Interactive then
            Self.On_Map (Mapped'Access, Slot => Self);
            Self.On_Unmap (Unmapped'Access, Slot => Self);
            Self.On_Destroy (Unmapped'Access, Slot => Self);
         end if;
      end if;
   end Run;

   procedure Run_Simple (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
   begin
      Run (Frame, Simple);
   end Run_Simple;

   function Help_Simple return String is
   begin
      return
        "Required constraints give the top buttons equal widths and " &
        "align the bottom button with the outer edges. An invisible " &
        "Gtk.Constraint_Guide controls the gap. Resize the window or " &
        "change the guide's width.";
   end Help_Simple;

   procedure Run_Interactive (Frame : access Gtk.Frame.Gtk_Frame_Record'Class)
   is
   begin
      Run (Frame, Interactive);
   end Run_Interactive;

   function Help_Interactive return String is
   begin
      return
        "Drag the divider between the blue buttons, or use the slider. " &
        "The demo replaces an immutable position constraint as you move. " &
        "Required minimum widths take priority over its strong position " &
        "constraint. Resizing preserves the requested proportion.";
   end Help_Interactive;

   procedure Run_VFL (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
   begin
      Run (Frame, VFL);
   end Run_VFL;

   function Help_VFL return String is
   begin
      return
        "Edit the Visual Format Language describing children a, b and c, " &
        "then apply it. H and V select the axis; bars refer to the container. " &
        "Default spacing is 16 pixels. Invalid syntax keeps the previous " &
        "layout and displays the parser error.";
   end Help_VFL;

   procedure Run_Builder (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Builder : Gtk_Builder;
      Error   : aliased GError;
      Box     : Gtk_Box;
   begin
      Gtk_New (Builder);
      if not Builder.Add_From_File ("constraints.ui", Error) then
         Frame.Set_Child (Gtk_Label_New (Get_Message (Error)));
         Error_Free (Error);
      else
         Box := Gtk_Box (Builder.Get_Object ("constraints"));
         Frame.Set_Child (Box);
      end if;
      Builder.Unref;
   end Run_Builder;

   function Help_Builder return String is
   begin
      return
        "Gtk.Builder reads constraints.ui, including its constraints " &
        "and an invisible spacer guide. No Ada code positions the children. " &
        "Resize the window to see equal widths and aligned edges remain " &
        "satisfied. Run from the demo directory to load the UI file.";
   end Help_Builder;
end Create_Constraints;
