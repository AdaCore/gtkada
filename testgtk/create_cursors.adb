-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

with Unchecked_Conversion;

with Glib; use Glib;
with Gdk.Cursor;     use Gdk.Cursor;
with Gdk.Drawable;   use Gdk.Drawable;
with Gdk.Event;      use Gdk.Event;
with Gdk.GC;         use Gdk.GC;
with Gdk.Types;      use Gdk.Types;
with Gdk.Window;     use Gdk.Window;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Box; use Gtk.Box;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Label;  use Gtk.Label;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Spin_Button; use Gtk.Spin_Button;
with Gtk.Style;  use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;

with Ada.Text_IO; use Ada.Text_IO;

package body Create_Cursors is

   type My_Spin_Button_Record is new Gtk_Spin_Button_Record with record
      Label : Gtk_Label;
   end record;
   type My_Spin_Button is access all My_Spin_Button_Record;

   package Spin_Cb is new Signal.Object_Callback (My_Spin_Button_Record);
   package Spin2_Cb is new Signal.Callback (My_Spin_Button_Record,
                                            Gtk_Drawing_Area);
   package Spin3_Cb is new Signal.Two_Callback (Gtk_Widget_Record,
                                                My_Spin_Button,
                                                Gdk_Event_Button);
   package Da_Cb is new Signal.Object_Callback (Gtk_Drawing_Area_Record);

   procedure Cursor_Expose_Event (Darea : access Gtk_Drawing_Area_Record) is
      Style      : Gtk_Style := Get_Style (Darea);
      Draw       : Gdk_Drawable := Gdk_Drawable (Get_Window (Darea));
      White_GC   : Gdk_GC := Get_White_GC (Style);
      Black_GC   : Gdk_GC := Get_Black_GC (Style);
      Gray_GC    : Gdk_GC := Get_Bg_GC (Style, State_Normal);
      Max_Width  : Guint  := Get_Allocation_Width (Darea);
      Max_Height : Guint  := Get_Allocation_Height (Darea);

   begin
      Draw_Rectangle (Draw, White_GC, True, 0, 0,
                      Gint (Max_Width), Gint (Max_Height / 2));
      Draw_Rectangle (Draw, Black_GC, True, 0, Gint (Max_Height / 2),
                      Gint (Max_Width), Gint (Max_Height / 2));
      Draw_Rectangle (Draw, Gray_GC, True, Gint (Max_Width / 3),
                      Gint (Max_Height / 3), Gint (Max_Width / 3),
                      Gint (Max_Height / 3));
   end Cursor_Expose_Event;


   procedure Set_Cursor (Spinner : access My_Spin_Button_Record;
                         Widget  : in Gtk_Drawing_Area)
   is
      pragma Warnings (Off);
      function To_Cursor is new Unchecked_Conversion (Gint,
                                                      Gdk_Cursor_Type);
      pragma Warnings (On);

      C      : Gint := Get_Value_As_Int (Spinner);
      Cursor : Gdk_Cursor;
      Window : Gdk_Window := Get_Window (Widget);
   begin
      C := C mod 154;
      Gdk_New (Cursor, To_Cursor (C));
      Set_Cursor (Window, Cursor);
      Set_Text (Spinner.Label, Gdk_Cursor_Type'Image (To_Cursor (C)));
      Destroy (Cursor);
   end Set_Cursor;

   procedure Cursor_Event (Darea   : access Gtk_Widget_Record;
                           Event   : in Gdk_Event_Button;
                           Spinner : in My_Spin_Button) is
      pragma Warnings (Off, Darea);
   begin
      if Get_Button (Event) = 1 then
         Spin (Spinner, Arrow_Up, Get_Step_Increment (Get_Adjustment (Spinner)));
      elsif Get_Button (Event) = 3 then
         Spin (Spinner, Arrow_Down, Get_Step_Increment (Get_Adjustment (Spinner)));
      else
         Put_Line ("Unknown button : " & Guint'Image (Get_Button (Event)));
      end if;
   end Cursor_Event;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Id      : Guint;
      Vbox,
        Hbox  : Gtk_Box;
      Label   : Gtk_Label;
      Adj     : Gtk_Adjustment;
      Spinner : My_Spin_Button;
      Frame2  : Gtk_Frame;
      Darea   : Gtk_Drawing_Area;
   begin
      Set_Label (Frame, "Cursors");

      Gtk_New_Vbox (Vbox, False, 5);
      Set_Border_Width (Vbox, 10);
      Add (Frame, Vbox);

      Gtk_New_Hbox (Hbox, False, 0);
      Set_Border_Width (Hbox, 5);
      Pack_Start (Vbox, Hbox, False, False, 0);

      Gtk_New (Label, "Cursor Value:");
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Hbox, Label, False, True, 0);

      Gtk_New (Adj, 0.0, 0.0, 152.0, 2.0, 10.0, 0.0);

      Spinner := new My_Spin_Button_Record;
      Initialize (Spinner, Adj, 0.0, 0);
      Pack_Start (Hbox, Spinner, True, True, 0);

      Gtk_New (Frame2, "Cursor Area");
      Set_Shadow_Type (Frame2, Shadow_Etched_In);
      Set_Label_Align (Frame2, 0.5, 0.0);
      Set_Border_Width (Frame2, 10);
      Pack_Start (Vbox, Frame2);

      Gtk_New (Darea);
      Set_Usize (Darea, 80, 80);
      Add (Frame2, Darea);
      Id := Da_Cb.Connect (Darea, "expose_event",
                           Cursor_Expose_Event'Access, Darea);

      Unrealize (Darea); --  Required for the call to Set_Events
      Set_Events (Darea, Exposure_Mask or Button_Press_Mask);

      Id := Spin3_Cb.Connect (Darea, "button_press_event",
                              Cursor_Event'Access, Spinner);

      Id := Spin2_Cb.Connect (Spinner, "changed", Set_Cursor'Access, Darea);

      Gtk_New (Spinner.Label, "XXX");
      Pack_Start (Vbox, Spinner.Label, False, False, 0);

      Show_All (Frame);
   end Run;

end Create_Cursors;


