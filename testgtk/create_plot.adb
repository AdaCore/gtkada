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
with Gdk.Drawable;          use Gdk.Drawable;
with Gdk.Pixmap;            use Gdk.Pixmap;
with Gdk.Bitmap;            use Gdk.Bitmap;
with Glib;                  use Glib;
with Gtk.Arguments;         use Gtk.Arguments;
with Gtk.Box;               use Gtk.Box;
with Gtk.Button;            use Gtk.Button;
with Gtk.Toggle_Button;     use Gtk.Toggle_Button;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Extra.Plot;        use Gtk.Extra.Plot;
with Gtk.Extra.Plot_Bar;    use Gtk.Extra.Plot_Bar;
with Gtk.Extra.Plot_Box;    use Gtk.Extra.Plot_Box;
with Gtk.Extra.Plot_Polar;  use Gtk.Extra.Plot_Polar;
with Gtk.Extra.Plot_Data;   use Gtk.Extra.Plot_Data;
with Gtk.Extra.Plot_Canvas; use Gtk.Extra.Plot_Canvas;
with Gtk.Extra.Plot_Ps;     use Gtk.Extra.Plot_Ps;
with Gtk.Extra.PsFont;      use Gtk.Extra.PsFont;
with Gtk.Frame;             use Gtk.Frame;
with Gtk.Scrolled_Window;   use Gtk.Scrolled_Window;
with Gtk.Style;             use Gtk.Style;
with Gtk.Handlers;          use Gtk.Handlers;
with Gtk.Arguments;         use Gtk.Arguments;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with System;
with Gtkada.Types;          use Gtkada.Types;
with Interfaces.C.Strings;
with Unchecked_Conversion;

package body Create_Plot is

   Px1  : aliased Gdouble_Array := (0.0, 0.2, 0.4, 0.6, 0.8, 1.0);
   Py1  : aliased Gdouble_Array := (0.2, 0.4, 0.5, 0.35, 0.3, 0.4);
   Pz1  : aliased Gdouble_Array := (0.12, 0.22, 0.27, 0.12, 0.052, 0.42);
   Pdx1 : aliased Gdouble_Array := (0.2, 0.2, 0.2, 0.2, 0.2, 0.2);
   Pdy1 : aliased Gdouble_Array := (0.1, 0.1, 0.1, 0.1, 0.1, 0.1);
   Pdz1 : aliased Gdouble_Array :=
     (0.0243, 0.045, 0.075, 0.0213, 0.05, 0.0324);

   Px2  : aliased Gdouble_Array := (0.0, -0.2, -0.4, -0.6, -0.8, -1.0);
   Py2  : aliased Gdouble_Array := (0.2, 0.56, 0.9, 0.74, 0.3, 0.4);
   Pdx2 : aliased Gdouble_Array := (0.2, 0.2, 0.2, 0.2, 0.2, 0.2);
   Pdy2 : aliased Gdouble_Array := (0.1, 0.1, 0.1, 0.1, 0.1, 0.1);

   Px3  : aliased Gdouble_Array := (0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8);
   Py3  : aliased Gdouble_Array := (0.012, 0.067, 0.24, 0.5, 0.65, 0.5,
                                    0.24, 0.067);
   Pdx3 : aliased Gdouble_Array := (0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1);
   Pdy3 : aliased Gdouble_Array := (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);

   R    : aliased Gdouble_Array := (0.2, 0.34, 0.45, 0.6, 0.75, 0.81);
   Angle : aliased Gdouble_Array := (15.0, 20.0, 43.0, 67.0, 84.0, 106.0);

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
   package Axis_Cb is new Gtk.Handlers.Return_Callback
     (Gtk_Plot_Axis_Record, Boolean);

   Plot_Icons2 : Chars_Ptr_Array :=
     "48 48 5 1" +
     "       c #FFFFFFFFFFFF" +
     ".      c #000000000000" +
     "X      c #00000000FFFF" +
     "o      c #FFFF00000000" +
     "O      c #0000FFFFFFFF" +
     "                                                " +
     "     ..                                         " +
     "     .  .................................       " +
     "     .. .                  ..           .       " +
     "        .               . ...           .       " +
     "        ..               ...            .       " +
     "        .               ...             .       " +
     "     .. .       ....   ...              .       " +
     "     .. ...   ..X   .....               .       " +
     "      . .    .   X    ..           XX   .       " +
     " . .    .   X.    X   ...         X  X o.       " +
     " . .    .. X.     X    .         X   oo .       " +
     "  .     .  X. O    X   ..        X  o X .       " +
     "  .  .. . X . OO   X   .        X  o   X.       " +
     "      . ..X .  O   X   ..       X o    X.       " +
     "      . . X .  OO   X  .        oo     X.       " +
     "        .X   .  OO  X . .      o        .       " +
     "        .X   .      X .       oX        .       " +
     "     .. .X    ..    ..  .    o X        .       " +
     "     .. X       ....       oo X         .       " +
     "      . ... . . . .X. . . o . X . . . . .       " +
     "        .          X     o    X         .       " +
     "        .           X  oo    X          .       " +
     "        ..          X o      X          .       " +
     "        .            o  .    X          .       " +
     "     .. .          oo       X           .       " +
     "      . ..        o  X  .   X.........  .       " +
     "     .. .        o   X      X.       .  .       " +
     "        .      oo     X .  X . X ... .. .       " +
     "        ..    o       X    X .       .. .       " +
     "        .    o         X. X  .       .. .       " +
     "     .. .  oo           XX   . o ... .. .       " +
     "      . ..o             .    .       .. .       " +
     "      . .o                   .......... .       " +
     "        .               .      ........ .       " +
     "        ..                              .       " +
     "        .   .   .   .   .   .   .   .   .       " +
     "     .  . . . . . . . . . . . . . . . . .       " +
     "    . . .................................       " +
     "     .                                          " +
     "        .   .   ..  ..  ..  .   .   .   .       " +
     "       . .  .   ..  ..  .   .  ..   .  ..       " +
     "        .   .   .    .   .  .  ...  .   .       " +
     "                                                " +
     "                       . .                      " +
     "                        .                       " +
     "                       . .                      " +
     "                                                ";


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
        & " legends, points, graphs,..."
        & ASCII.LF
        & "The ""Print"" button at the top-left corner generates a"
        & " postscript file, called plotdemo.ps, that contains the exact"
        & " same content that you can see in this demo."
        & ASCII.LF
        & "The two buttons 1 and 2 indicate which plot is considered the"
        & " active one, as returned by the subprogram Get_Active_Plot."
        & ASCII.LF
        & "It is possible to specify customer labels for the ticks on each"
        & " axis.";
   end Help;

   -------------------
   -- Print_Dataset --
   -------------------

   function Print_Dataset (Canvas : access Gtk_Plot_Canvas_Record'Class;
                           Event  : Gdk.Event.Gdk_Event_Button)
                          return Boolean
   is
      pragma Warnings (Off, Event);
      Arr : Points_Array := Get_Y (Get_Active_Data (Canvas));
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
   pragma Convention (C, My_Func);

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
   pragma Convention (C, Gauss);

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
      return True;
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

   -----------------
   -- Select_Item --
   -----------------

   function Select_Item (Canvas : access Gtk_Plot_Canvas_Record'Class;
                         Args   : Gtk_Args)
                        return Boolean
   is
      Item  : Gtk_Plot_Canvas_Child
        := Gtk_Plot_Canvas_Child (To_C_Proxy (Args, 2));
      Tmp : Boolean;
   begin
      Ada.Text_IO.Put_Line (Plot_Canvas_Type'Image (Get_Item_Type (Item)));
      Tmp := Activate_Plot (Canvas);
      return True;
   end Select_Item;

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

   procedure New_Layer
     (Canvas : access Gtk_Plot_Canvas_Record'Class;
      Plot   : access Gtk_Plot_Record'Class)
   is
      Dummy : Boolean;
   begin
      Num_Layers := Num_Layers + 1;
      Gtk_New (Buttons (Num_Layers), Integer'Image (Num_Layers));
      Set_Usize (Buttons (Num_Layers), 20, 20);
      Put (Canvas, Buttons (Num_Layers), Gint (Num_Layers - 1) * 20, 0);
      Show (Buttons (Num_Layers));

      Button_Cb.Connect
        (Buttons (Num_Layers), "toggled",
         Button_Cb.To_Marshaller (Active_Plot_With_Button'Access));

      Plots (Num_Layers) := Gtk_Plot (Plot);
      Dummy := Activate_Plot (Canvas);
   end New_Layer;

   -------------------
   -- My_Tick_Label --
   -------------------

   function My_Tick_Label
     (Axis : access Gtk_Plot_Axis_Record'Class;
      Args : Gtk_Args) return Boolean
   is
      pragma Warnings (Off, Axis);
      use Interfaces.C.Strings;
      type Gdouble_Access is access Gdouble;
      function Convert is new Unchecked_Conversion
        (System.Address, Interfaces.C.Strings.chars_ptr);
      function Convert is new Unchecked_Conversion
        (System.Address, Gdouble_Access);

      Tick_Value : Gdouble_Access := Convert (To_Address (Args, 1));
      Label : Interfaces.C.Strings.chars_ptr := Convert (To_Address (Args, 2));
   begin
      if Tick_Value.all = 0.0 then
         Update (Label, 0, String' ("custom" & ASCII.NUL), False);
         return True;
      end if;
      return False;
   end My_Tick_Label;

   --------------------
   -- Build_Example1 --
   --------------------

   procedure Build_Example1 is
      Red, Blue : Gdk.Color.Gdk_Color;
      Plot      : Gtk_Plot;
      Data      : Gtk_Plot_Data;
   begin
      --  Create the plot itself, its axis and the legend box

      Gtk_New (Plot, Width => 0.4, Height => 0.3);
      New_Layer (Canvas, Plot);
      Set_Range (Plot, -1.0, 1.0, -1.0, 1.4);
      Legends_Move (Plot, 0.5, 0.05);
      Set_Legends_Border (Plot, Border_None, 0);
      Axis_Hide_Title (Plot, Axis_Top);
      Axis_Show_Ticks (Plot, Axis_Top, Ticks_In, Ticks_In);
      Axis_Set_Ticks (Plot,  Axis_X, 1.0, 1);
      Axis_Set_Ticks (Plot,  Axis_Y, 1.0, 1);
      Axis_Set_Visible (Plot, Axis_Top, True);
      Axis_Set_Visible (Plot, Axis_Right, True);
      X0_Set_Visible (Plot, True);
      Y0_Set_Visible (Plot, True);

      Add_Plot (Canvas, Plot, 0.05, 0.05);
      Show (Plot);

      --  Specify a custom tick label for the right axis

      Axis_Use_Custom_Tick_Labels (Plot, Axis_Bottom, True);
      Axis_Cb.Connect
        (Get_Axis (Plot, Axis_Bottom), "tick_label", My_Tick_Label'Access);

      --  Common initialization

      Red := Parse ("red");
      Alloc (Gdk.Color.Get_System, Red);
      Blue := Parse ("blue");
      Alloc (Gdk.Color.Get_System, Blue);

      --  Create the first set of data

      Gtk_New (Data);
      Add_Data (Plot, Data);
      Show (Data);
      Set_Points (Data, Px1'Access, Py1'Access, Pdx1'Access, Pdy1'Access);
      Set_Symbol (Data, Symbol_Diamond, Symbol_Opaque, 10, 2.0, Red, Red);
      Set_Line_Attributes (Data, Line_Solid, 1.0, Red);
      Set_Connector (Data, Connect_Spline);
      Show_Yerrbars (Data);
      Set_Legend (Data, "Spline + EY");

      --  Create the second set of data

      Gtk_New (Data);
      Add_Data (Plot, Data);
      Show (Data);
      Set_Points (Data, Px2'Access, Py2'Access, Pdx2'Access, Pdy2'Access);
      Set_Symbol (Data, Symbol_Square, Symbol_Opaque, 8, 2.0,
                  Get_Black (Get_Style (Plot)), Get_Black (Get_Style (Plot)));
      Set_Line_Attributes (Data, Line_Solid, 4.0, Red);
      Set_Connector (Data, Connect_Straight);
      Set_X_Attributes (Data, Line_Solid, 0.0, Get_Black (Get_Style (Plot)));
      Set_Y_Attributes (Data, Line_Solid, 0.0, Get_Black (Get_Style (Plot)));
      Set_Legend (Data, "Line + Symbol");

      --  Create the third set of data

      Data := Add_Function (Plot, My_Func'Access);
      Show (Data);
      Set_Line_Attributes (Data, Line_Solid, 0.0, Blue);
      Set_Legend (Data, "Function Plot");
   end Build_Example1;

   --------------------
   -- Build_Example3 --
   --------------------

   procedure Build_Example3 is
      Red, Yellow : Gdk.Color.Gdk_Color;
      Plot        : Gtk_Plot;
      Data        : Gtk_Plot_Box;
   begin
      --  Create the plot itself, its axis and the legend box

      Gtk_New (Plot, Width => 0.4, Height => 0.3);
      New_Layer (Canvas, Plot);
      Set_Range (Plot, -1.0, 1.0, -1.0, 1.4);
      Legends_Move (Plot, 0.5, 0.05);
      Set_Legends_Border (Plot, Border_None, 0);
      Axis_Hide_Title (Plot, Axis_Top);
      Axis_Show_Ticks (Plot, Axis_Top, Ticks_In, Ticks_In);
      Axis_Set_Ticks (Plot,  Axis_X, 1.0, 1);
      Axis_Set_Ticks (Plot,  Axis_Y, 1.0, 1);
      Axis_Set_Visible (Plot, Axis_Top, True);
      Axis_Set_Visible (Plot, Axis_Right, True);
      X0_Set_Visible (Plot, True);
      Y0_Set_Visible (Plot, True);

      Add_Plot (Canvas, Plot, 0.55, 0.05);
      Show (Plot);

      --  Common initializations

      Red := Parse ("red");
      Alloc (Gdk.Color.Get_System, Red);
      Yellow := Parse ("yellow");
      Alloc (Gdk.Color.Get_System, Yellow);

      --  Create the data

      Gtk_New (Data, Orientation_Vertical);
      Add_Data (Plot, Data);
      Show (Data);
      Set_Points (Data, Px1'Access, Py1'Access, null, null);
      Set_Z (Data, Pz1'Access);
      Set_Dz (Data, Pdz1'Access);
      Show_Zerrbars (Data);
      Set_Symbol (Data, Symbol_None, Symbol_Filled, 10, 2.0, Yellow, Red);
      Set_Line_Attributes (Data, Line_None, 1.0, red);
      Set_Legend (Data, "Boxes");
   end Build_Example3;

   --------------------
   -- Build_Example4 --
   --------------------
   --  Polar coordinates

   procedure Build_Example4 is
      Red   : Gdk_Color;
      Plot  : Gtk_Plot_Polar;
      Data  : Gtk_Plot_Data;

   begin
      Gtk_New (Plot, Width => 0.4, Height => 0.3);
      New_Layer (Canvas, Plot);
      Add_Plot (Canvas, Plot, 0.55, 0.45);
      Show (Plot);

      Red := Parse ("red");
      Alloc (Gdk.Color.Get_System, Red);

      Gtk_New (Data);
      Add_Data (Plot, Data);
      Show (Data);
      Set_Points (Data, R'Access, Angle'Access, null, null);
      Set_Symbol (Data, Symbol_Diamond, Symbol_Opaque, 10, 2.0, Red, Red);
      Set_Line_Attributes (Data, Line_Solid, 1.0, Red);
      Set_Legend (Data, "Polar");
   end Build_Example4;

   --------------------
   -- Build_Example2 --
   --------------------

   procedure Build_Example2 is
      Blue, Yellow, Green : Gdk.Color.Gdk_Color;
      Plot                : Gtk_Plot;
      Data                : Gtk_Plot_Data;
      Data2               : Gtk_Plot_Bar;
   begin
      Yellow := Parse ("light yellow");
      Alloc (Gdk.Color.Get_System, Yellow);
      Blue := Parse ("light blue");
      Alloc (Gdk.Color.Get_System, Blue);
      Green := Parse ("dark green");
      Alloc (Gdk.Color.Get_System, Green);

      Gtk_New (Plot, Width => 0.4, Height => 0.3);
      New_Layer (Canvas, Plot);
      Set_Background (Plot, Yellow);
      Legends_Set_Attributes (Plot, "", 0, Null_Color, Blue);
      Set_Range (Plot, 0.0, 1.0, 0.0, 0.85);
      Axis_Set_Visible (Plot, Axis_Top, True);
      Axis_Set_Visible (Plot, Axis_Right, True);
      Grids_Set_Visible (Plot, True, True, True, True);
      Axis_Hide_Title (Plot, Axis_Top);
      Axis_Hide_Title (Plot, Axis_Right);
      Set_Legends_Border (Plot, Border_Shadow, 2);
      Legends_Move (Plot, 0.58, 0.05);

      Add_Plot (Canvas, Plot, 0.05, 0.45);
      Show (Plot);

      Data := Add_Function (Plot, Gauss'Access);
      Show (Data);
      Set_Line_Attributes (Data, Line_Dashed, 2.0, Green);
      Set_Legend (Data, "Gaussian");

      Blue := Parse ("blue");
      Alloc (Gdk.Color.Get_System, Blue);
      Gtk_New (Data2, Orientation_Vertical);
      Show (Data2);
      Add_Data (Plot, Data2);
      Set_Points (Data2, Px3'Access, Py3'Access, Pdx3'Access, Pdy3'Access);
      Set_Symbol (Data2, Symbol_None, Symbol_Opaque, 10, 2.0, Blue, Blue);
      Set_Line_Attributes (Data2, Line_None, 1.0, Blue);
      Set_Legend (Data2, "V Bar");
--    Set_Symbol (Data2, Symbol_Impulse, Symbol_Filled, 10, 5.0, Blue, Blue);
--    Set_Line_Attributes (Data2, Line_Solid, 5.0, Blue);
--    Set_Connector (Data2, Connect_None);
--    Set_Legend (Data2, "Impulses");
   end Build_Example2;

   ----------------
   -- Draw_Child --
   ----------------

   procedure Draw_Child (C_Canvas : System.Address;
                         Child    : Gtk_Plot_Canvas_Child)
   is
      Canvas : Gtk_Plot_Canvas := Convert (C_Canvas);
      Pixmap : Gdk_Pixmap;
      Mask   : Gdk_Bitmap;
   begin
      Create_From_Xpm_D (Pixmap,
                         Get_Window (Canvas),
                         Get_System,
                         Mask,
                         Null_Color,
                         Plot_Icons2);
      Draw_Pixmap (Get_Pixmap (Canvas),
                   Get_Fg_Gc (Get_Style (Canvas), State_Normal),
                   Pixmap,
                   0, 0,
                   Get_Allocation_X (Child),
                   Get_Allocation_Y (Child),
                   Gint (Get_Allocation_Width (Child)),
                   Gint (Get_Allocation_Height (Child)));
      Gdk.Pixmap.Unref (Pixmap);
      Gdk.Bitmap.Unref (Mask);
   end Draw_Child;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Vbox1       : Gtk_Box;
      Scrollw1    : Gtk_Scrolled_Window;
      Button      : Gtk_Button;
      Text        : Gtk_Plot_Canvas_Child;
      Child       : Gtk_Plot_Canvas_Child;
      Tmp         : Gint;
   begin
      Set_Label (Frame, "Gtk.Extra.Plot_3D demo");

      Tmp := Gtk.Extra.PsFont.Init;

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
      --
      Gtk_New (Canvas, Gint (Get_Allocation_Width (Frame) - 10),
               Gint (Get_Allocation_Height (Frame) - 10), 1.0);
      Plot_Canvas_Set_Flags (Canvas, Dnd_Flags);
      Add_With_Viewport (Scrollw1, Canvas);

      Build_Example1;
      Build_Example2;
      Build_Example3;
      Build_Example4;

      Event_Cb.Connect (Canvas, "select_item", Select_Item'Access);
      Show_All (Frame);

      Text := Put_Text
        (Canvas, 0.40, 0.02,
         "Times-BoldItalic", 16, 0, Null_Color, Null_Color, True,
         Justify_Center, "Dnd titles, legends and plots");

      Gtk_New (Button, "Print");
      Put (Canvas, Button, 0, 40);
      Layout_Cb.Object_Connect (Button,
                                "clicked",
                                Layout_Cb.To_Marshaller (Print'Access),
                                Slot_Object => Canvas);

      Text := Put_Text
        (Canvas, 0.4, 0.84,
         "Times-Roman", 16, 0, Null_Color, Null_Color, False,
         Justify_Center,
         "You can use \ssubscripts\b\b\b\b\b\b\b\b"
         & "\b\b\N\Ssuperscripts");
      Text := Put_Text
        (Canvas, 0.4, 0.88,
         "Times-Roman", 16, 0, Null_Color, Null_Color, True,
         Justify_Center,
         "Format text mixing \Bbold \N\i, italics, \ggreek \4\N "
         & "and \+different fonts");


      -- Insert a customer item --
      Child := Child_New (Custom);
      Set_Draw_Func (Child, Draw_Child'Access);
      Put_Child (Canvas, Child, 0.6, 0.2, 0.58, 0.56);

      Show_All (Frame);
   end Run;

end Create_Plot;
