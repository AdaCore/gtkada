------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2018, AdaCore                     --
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

with Unchecked_Conversion;

with Cairo;            use Cairo;
with Glib;             use Glib;
with Gdk;              use Gdk;
with Gdk.Cursor;       use Gdk.Cursor;

with Gdk.Device_Manager; use Gdk.Device_Manager;
with Gdk.Display;

with Gdk.Event;        use Gdk.Event;
with Gdk.Window;       use Gdk.Window;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Box;          use Gtk.Box;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Label;        use Gtk.Label;
with Gtk.Handlers;     use Gtk.Handlers;
with Gtk.Spin_Button;  use Gtk.Spin_Button;
with Gtk.Widget;       use Gtk.Widget;
with Gtk;              use Gtk;

with Ada.Text_IO; use Ada.Text_IO;

package body Create_Cursors is

   type My_Spin_Button_Record is new Gtk_Spin_Button_Record with record
      Label : Gtk_Label;
   end record;
   type My_Spin_Button is access all My_Spin_Button_Record;

   package Spin2_Cb is new Handlers.User_Callback
     (My_Spin_Button_Record, Gtk_Drawing_Area);
   package Spin3_Cb is new Handlers.User_Return_Callback
     (Gtk_Widget_Record, Gint, My_Spin_Button);
   package Da_Cb is new Handlers.Return_Callback
     (Gtk_Drawing_Area_Record, Boolean);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "Multiple kind of cursors can be used in your application. Even"
        & " though you can define your own pixmaps for cursors, a number of"
        & " cursors are predefined."
        & ASCII.LF
        & "This demo also shows a basic example on how to draw into a"
        & " @bGtk_Drawing_Area@B.";
   end Help;

   -------------
   -- On_Draw --
   -------------

   function On_Draw
      (Darea : access Gtk_Drawing_Area_Record'Class;
       Cr    : Cairo_Context) return Boolean
   is
      W       : constant Gdouble  := Gdouble (Get_Allocated_Width (Darea));
      H       : constant Gdouble  := Gdouble (Get_Allocated_Height (Darea));

   begin
      Rectangle (Cr, 0.0, 0.0, W, H / 2.0);
      Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
      Cairo.Fill (Cr);

      Rectangle (Cr, 0.0, H / 2.0, W, H / 2.0);
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Cairo.Fill (Cr);

      Rectangle (Cr, W / 3.0, H / 3.0, W / 3.0, H / 3.0);
      Set_Source_Rgb (Cr, 0.5, 0.5, 0.5);
      Cairo.Fill (Cr);
      return False;
   end On_Draw;

   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor (Spinner : access My_Spin_Button_Record'Class;
                         Widget  : Gtk_Drawing_Area)
   is
      pragma Warnings (Off);
      function To_Cursor is new Unchecked_Conversion
        (Gint, Gdk_Cursor_Type);
      pragma Warnings (On);

      C      : Gint := Get_Value_As_Int (Spinner);
      Window : constant Gdk_Window := Get_Window (Widget);
      Cursor : Gdk_Cursor := null;

      Device_Manager : constant Gdk_Device_Manager :=
        Get_Device_Manager (Gdk.Display.Get_Default);

   begin
      C := C mod 154;
      Gdk_New (Cursor, To_Cursor (C));
      Set_Text (Spinner.Label, Gdk_Cursor_Type'Image (To_Cursor (C)));

      Set_Device_Cursor (Self   => Window,
                         Device => Device_Manager.Get_Client_Pointer,
                         Cursor => Cursor);

      --  The cursor change is asynchronous: if you plan to do a blocking
      --  operation right after setting this, it is useful to call
      --  Process_All_Updates in order for impacted windows to have the new
      --  cursor.
      Process_All_Updates;

      --  Note: the cursor pixmap is copied to the server, which keeps it as
      --  long at it needs. On the client side, it is possible to delete the
      --  cursor right now.
      Unref (Cursor);
   end Set_Cursor;

   ------------------
   -- Cursor_Event --
   ------------------

   function Cursor_Event
     (Darea   : access Gtk_Widget_Record'Class;
      Event   : Gdk_Event;
      Spinner : My_Spin_Button) return Gint
   is
      pragma Warnings (Off, Darea);
   begin
      if Get_Button (Event) = 1 then
         Spin
           (Spinner, Spin_Step_Forward,
            Get_Step_Increment (Get_Adjustment (Spinner)));

      elsif Get_Button (Event) = 3 then
         Spin
           (Spinner, Spin_Step_Backward,
            Get_Step_Increment (Get_Adjustment (Spinner)));

      else
         Put_Line ("Unknown button : " & Guint'Image (Get_Button (Event)));
      end if;

      return 0;
   end Cursor_Event;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
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
      Set_Size_Request (Darea, 80, 80);
      Add (Frame2, Darea);
      Da_Cb.Connect (Darea, Signal_Draw, Da_Cb.To_Marshaller (On_Draw'Access));

      Unrealize (Darea); --  Required for the call to Set_Events
      Set_Events (Darea, Exposure_Mask or Button_Press_Mask);

      Spin3_Cb.Connect (Darea, "button_press_event",
                        Spin3_Cb.To_Marshaller (Cursor_Event'Access), Spinner);
      Spin2_Cb.Connect (Spinner, "changed", Set_Cursor'Access, Darea);

      Gtk_New (Spinner.Label, "XXX");
      Pack_Start (Vbox, Spinner.Label, False, False, 0);

      Show_All (Frame);
   end Run;

end Create_Cursors;
