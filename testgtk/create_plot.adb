-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gdk.Color;             use Gdk.Color;
with Gdk.Event;             use Gdk.Event;
with Glib;                  use Glib;
with Gtk.Adjustment;        use Gtk.Adjustment;
with Gtk.Box;               use Gtk.Box;
with Gtk.Button;            use Gtk.Button;
with Gtk.Toggle_Button;     use Gtk.Toggle_Button;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Extra.Plot;        use Gtk.Extra.Plot;
with Gtk.Extra.Plot_Canvas; use Gtk.Extra.Plot_Canvas;
with Gtk.Extra.Plot_Ps;     use Gtk.Extra.Plot_Ps;
with Gtk.Frame;             use Gtk.Frame;
with Gtk.Scrolled_Window;   use Gtk.Scrolled_Window;
with Gtk.Style;             use Gtk.Style;
with Gtk.Handlers;          use Gtk.Handlers;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;

package body Create_Plot is

   Dataset : array (0 .. 4) of Gtk_Plot_Data;
   Px1  : aliased Gdouble_Array := (0.0, 0.2, 0.4, 0.6, 0.8, 1.0);
   Py1  : aliased Gdouble_Array := (0.2, 0.4, 0.5, 0.35, 0.3, 0.4);
   Pdx1 : aliased Gdouble_Array := (0.2, 0.2, 0.2, 0.2, 0.2, 0.2);
   Pdy1 : aliased Gdouble_Array := (0.1, 0.1, 0.1, 0.1, 0.1, 0.1);

   Px2  : aliased Gdouble_Array := (0.0, -0.2, -0.4, -0.6, -0.8, -1.0);
   Py2  : aliased Gdouble_Array := (0.2, 0.56, 0.9, 0.74, 0.3, 0.4);
   Pdx2 : aliased Gdouble_Array := (0.2, 0.2, 0.2, 0.2, 0.2, 0.2);
   Pdy2 : aliased Gdouble_Array := (0.1, 0.1, 0.1, 0.1, 0.1, 0.1);

   Px3  : aliased Gdouble_Array := (0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8);
   Py3  : aliased Gdouble_Array := (0.012, 0.067, 0.24, 0.5, 0.65, 0.5,
                                    0.24, 0.067);
   Pdx3 : aliased Gdouble_Array := (0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1);
   Pdy3 : aliased Gdouble_Array := (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);

   Num_Layers : Integer := 0;
   Buttons    : array (1 .. 10) of Gtk_Toggle_Button;
   Plots      : array (1 .. 10) of Gtk_Plot;

   Canvas : Gtk_Plot_Canvas;

   package Double_Numerics is new
     Ada.Numerics.Generic_Elementary_Functions (Gdouble);
   use Double_Numerics;

   package Event_Cb is new Gtk.Handlers.Return_Callback
     (Gtk_Plot_Canvas_Record, Boolean);
   package Button_Cb is new Gtk.Handlers.Callback
     (Gtk_Toggle_Button_Record);
   package Layout_Cb is new Gtk.Handlers.Callback
     (Gtk_Plot_Canvas_Record);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Plot@B widget can help when you want to display complex"
        & " data sets, that you dynamically generate through a function or"
        & " that you already have in an array."
        & ASCII.LF
        & "In this demo, the @bGtk_Plot@B is put in a @bGtk_Plot_Canvas@B"
        & " container, so as to provide drag-and-drop capabilities for its"
        & " legends, points, graphs,...";
   end Help;

   -------------------
   -- Print_Dataset --
   -------------------

   function Print_Dataset (Canvas : access Gtk_Plot_Canvas_Record'Class;
                           Event  : Gdk.Event.Gdk_Event_Button)
                          return Boolean
   is
      pragma Warnings (Off, Event);
      Arr : Points_Array := Dataset_Get_Y (Get_Active_Dataset (Canvas));
   begin
      Ada.Text_IO.Put_Line ("Current values of points in the dataset:");
      for Num in 0 .. Integer (Arr.Num_Points) - 1 loop
         Ada.Text_IO.Put (Gdouble'Image (Arr.Points (Num)) & "  ");
      end loop;
      Ada.Text_IO.New_Line;
      return True;
   end Print_Dataset;

   -----------------
   -- My_Function --
   -----------------

   function My_Function  (Plot  : access Gtk_Plot_Record'Class;
                          Set   : in     Gtk_Plot_Data;
                          X     : in     Gdouble;
                          Error : access Boolean)
                         return Gdouble
   is
      pragma Warnings (Off, Plot);
      pragma Warnings (Off, Set);
   begin
      Error.all := False;
      return (-0.5 + 0.3 * Sin (3.0 * X) * Sin (50.0 * X));
   end My_Function;

   function My_Func is new Generic_Plot_Function (My_Function);

   --------------
   -- Gaussian --
   --------------

   function Gaussian  (Plot  : access Gtk_Plot_Record'Class;
                       Set   : in     Gtk_Plot_Data;
                       X     : in     Gdouble;
                       Error : access Boolean)
                      return Gdouble
   is
      pragma Warnings (Off, Plot);
      pragma Warnings (Off, Set);
   begin
      Error.all := False;
      return 0.65 * Exp (-0.5 * (X - 0.5) * (X - 0.5) / 0.02);
   end Gaussian;

   function Gauss is new Generic_Plot_Function (Gaussian);

   -------------------
   -- Activate_Plot --
   -------------------

   function Activate_Plot (C : access Gtk_Plot_Canvas_Record'Class)
                          return Boolean
   is
      pragma Warnings (Off, C);
      P : Gtk_Plot;
   begin
      P := Get_Active_Plot (Canvas);
      for N in 1 .. Num_Layers loop
         if Plots (N) = P then
            Set_Active_Plot (Canvas, Plots (N));
            Set_Active (Buttons (N), True);
         else
            Set_Active (Buttons (N), False);
         end if;
         Queue_Draw (Buttons (N));
      end loop;
      return False;
   end Activate_Plot;

   -----------
   -- Print --
   -----------

   procedure Print (Canvas : access Gtk_Plot_Canvas_Record'Class) is
   begin
      Plot_Layout_Export_Ps (Canvas, "plotdemo.ps",
                             Plot_Portrait,
                             False,
                             Plot_Letter);

   end Print;

   -----------------------------
   -- Active_Plot_With_Button --
   -----------------------------

   procedure Active_Plot_With_Button
     (Button : access Gtk_Toggle_Button_Record'Class)
   is
   begin
      if Is_Active (Button) then
         for N in 1 .. Num_Layers loop
            if Buttons (N) = Gtk_Toggle_Button (Button) then
               Set_Active_Plot (Canvas, Plots (N));
            else
               Set_Active (Buttons (N), False);
            end if;
         end loop;
      end if;
   end Active_Plot_With_Button;

   ---------------
   -- New_Layer --
   ---------------

   function New_Layer (Canvas : access Gtk_Plot_Canvas_Record'Class)
                      return Gtk_Plot
   is
      Dummy : Boolean;
   begin
      Num_Layers := Num_Layers + 1;
      Gtk_New (Buttons (Num_Layers), Integer'Image (Num_Layers));
      Set_Usize (Buttons (Num_Layers), 20, 20);
      Put (Canvas, Buttons (Num_Layers), Gint (Num_Layers - 1) * 20, 0);

      Button_Cb.Connect
        (Buttons (Num_Layers), "toggled",
         Button_Cb.To_Marshaller (Active_Plot_With_Button'Access));

      Gtk_New (Plots (Num_Layers), Width => 0.5, Height => 0.25);

      Event_Cb.Connect (Canvas, "click_on_plot",
                        Event_Cb.To_Marshaller (Activate_Plot'Access));

      Dummy := Activate_Plot (Canvas);
      return Plots (Num_Layers);
   end New_Layer;

   --------------------
   -- Build_Example1 --
   --------------------

   procedure Build_Example1 (Active_Plot : access Gtk_Plot_Record'Class) is
      Color : Gdk.Color.Gdk_Color;
   begin
      --  Create the first set of data

      Dataset (0) := Gtk_Dataset_New;
      Add_Dataset (Active_Plot, Dataset (0));
      Dataset_Set_Points (Dataset (0), Px1'Access, Py1'Access,
                          Pdx1'Access, Pdy1'Access);
      Color := Parse ("red");
      Alloc (Gdk.Color.Get_System, Color);

      Dataset_Set_Symbol (Dataset (0),
                          Symbol_Diamond,
                          Symbol_Opaque,
                          10, 2, Color);
      Dataset_Set_Line_Attributes (Dataset (0),
                                   Line_Solid,
                                   1, Color);
      Dataset_Set_Connector (Dataset (0), Connect_Spline);
      Dataset_Show_Yerrbars (Dataset (0));
      Dataset_Set_Legend (Dataset (0), "Spline + EY");


      --  Create the second set of data
      Dataset (3) := Gtk_Dataset_New;
      Add_Dataset (Active_Plot, Dataset (3));
      Dataset_Set_Points (Dataset (3), Px2'Access, Py2'Access,
                          Pdx2'Access, Pdy2'Access);
      Dataset_Set_Symbol (Dataset (3),
                          Symbol_Square,
                          Symbol_Opaque,
                          8, 2, Get_Black (Get_Style (Active_Plot)));
      Dataset_Set_Line_Attributes (Dataset (3),
                                   Line_Solid,
                                   4, Color);
      Dataset_Set_Connector (Dataset (3), Connect_Straight);
      Dataset_Set_X_Attributes (Dataset (3),
                                Line_Solid,
                                0, Get_Black (Get_Style (Active_Plot)));
      Dataset_Set_Y_Attributes (Dataset (3),
                                Line_Solid,
                                0, Get_Black (Get_Style (Active_Plot)));
      Dataset_Set_Legend (Dataset (3), "Line + Symbol");


      --  Create the third set of data
      Color := Parse ("blue");
      Alloc (Gdk.Color.Get_System, Color);

      Dataset (1) := Add_Function (Active_Plot, My_Func'Access);
      Dataset_Set_Line_Attributes (Dataset (1), Line_Solid, 0, Color);
      Dataset_Set_Legend (Dataset (1), "Function Plot");
   end Build_Example1;

   --------------------
   -- Build_Example2 --
   --------------------

   procedure Build_Example2 (Active_Plot : access Gtk_Plot_Record'Class) is
      Color : Gdk.Color.Gdk_Color;
   begin
      Dataset (4) := Add_Function (Active_Plot, Gauss'Access);
      Color := Parse ("dark green");
      Alloc (Gdk.Color.Get_System, Color);
      Dataset_Set_Line_Attributes (Dataset (4),
                                   Line_Dashed,
                                   2, Color);
      Dataset_Set_Legend (Dataset (4), "Gaussian");

      Color := Parse ("blue");
      Alloc (Gdk.Color.Get_System, Color);
      Dataset (2) := Gtk_Dataset_New;
      Add_Dataset (Active_Plot, Dataset (2));
      Dataset_Set_Points (Dataset (2), Px3'Access, Py3'Access,
                          Pdx3'Access, Pdy3'Access);
      Dataset_Set_Symbol (Dataset (2),
                          Symbol_Impulse,
                          Symbol_Filled,
                          10, 5, Color);
      Dataset_Set_Line_Attributes (Dataset (2),
                                   Line_Solid,
                                   5, Color);
      Dataset_Set_Connector (Dataset (2), Connect_None);
      Dataset_Set_Legend (Dataset (2), "Impulses");
   end Build_Example2;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Vbox1       : Gtk_Box;
      Scrollw1    : Gtk_Scrolled_Window;
      Active_Plot : Gtk_Plot;
      Color       : Gdk_Color;
      Button      : Gtk_Button;
   begin
      Num_Layers := 0;
      Gtk_New_Vbox (Vbox1, False, 0);
      Add (Frame, Vbox1);

      --  Put the Plot in a scrolled window, in case it is too big for
      --  the screen
      Gtk_New (Scrollw1);
      Set_Border_Width (Scrollw1, 0);
      Set_Policy (Scrollw1, Policy_Always, Policy_Always);
      Pack_Start (Vbox1, Scrollw1, True, True, 0);

      --  Create the canvas in which the plot will be drawn
      Gtk_New (Canvas, 600, 700);
      Set_Usize (Canvas, 600, 700);
      Plot_Canvas_Set_Flags (Canvas, Dnd_Flags);
      Plot_Canvas_Set_Flags (Canvas, Allocate_Titles);
      Add (Scrollw1, Canvas);
      Set_Step_Increment (Get_Hadjustment (Canvas), 5.0);
      Set_Step_Increment (Get_Vadjustment (Canvas), 5.0);

      Event_Cb.Connect (Canvas, "click_on_point",
                        Event_Cb.To_Marshaller (Print_Dataset'Access));

      Active_Plot := New_Layer (Canvas);
      Set_Range (Active_Plot, -1.0, 1.0, -1.0, 1.4);
      Legends_Move (Active_Plot, 0.5, 0.05);
      Set_Legends_Border (Active_Plot, Border_None, 0);
      Axis_Hide_Title (Active_Plot, Axis_Top);
      Axis_Show_Ticks (Active_Plot, Axis_Top, Ticks_All, Ticks_All);
      Axis_Set_Ticks (Active_Plot,  Orientation_Horizontal, 1.0, 0.5);
      Axis_Set_Ticks (Active_Plot,  Orientation_Vertical, 1.0, 0.5);
      Axis_Set_Visible (Active_Plot, Axis_Top, True);
      Axis_Set_Visible (Active_Plot, Axis_Right, True);
      X0_Set_Visible (Active_Plot, True);
      Y0_Set_Visible (Active_Plot, True);
      Add_Plot (Canvas, Active_Plot, 0.15, 0.06);

      Build_Example1 (Active_Plot);

      --  Second plot

      Active_Plot := New_Layer (Canvas);
      Color := Parse ("light yellow");
      Alloc (Gdk.Color.Get_System, Color);
      Set_Background (Active_Plot, Color);

      Color := Parse ("light blue");
      Alloc (Gdk.Color.Get_System, Color);
      Legends_Set_Attributes (Active_Plot, "", 0, Null_Color, Color);
      Set_Range (Active_Plot, 0.0, 1.0, 0.0, 0.85);
      Axis_Set_Visible (Active_Plot, Axis_Top, True);
      Axis_Set_Visible (Active_Plot, Axis_Right, True);
      Grids_Set_Visible (Active_Plot, True, True, True, True);
      Add_Plot (Canvas, Active_Plot, 0.15, 0.4);
      Axis_Hide_Title (Active_Plot, Axis_Top);
      Axis_Hide_Title (Active_Plot, Axis_Right);
      Set_Legends_Border (Active_Plot, Border_Shadow, 2);
      Legends_Move (Active_Plot, 0.58, 0.05);

      Build_Example2 (Active_Plot);

      Put_Text (Canvas, 0.40, 0.005, 0,
                "Times-BoldItalic", 16, Null_Color, Null_Color,
                Justify_Center, "Dnd titles, legends and plots");

      Gtk_New (Button, "Print");
      Put (Canvas, Button, 0, 40);
      Layout_Cb.Object_Connect (Button,
                                "clicked",
                                Layout_Cb.To_Marshaller (Print'Access),
                                Slot_Object => Canvas);
      Show_All (Frame);
   end Run;

end Create_Plot;
