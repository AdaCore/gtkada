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

with Glib; use Glib;

with Gdk;           use Gdk;
with Gdk.Drawable;  use Gdk.Drawable;
with Gdk.Event;     use Gdk.Event;
with Gdk.GC;        use Gdk.GC;
with Gdk.Rectangle; use Gdk.Rectangle;
with Gdk.Types;
with Gdk.Window;    use Gdk.Window;

with Gtk;              use Gtk;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Box;          use Gtk.Box;
with Gtk.Button;       use Gtk.Button;
with Gtk.Dialog;       use Gtk.Dialog;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Scrollbar;    use Gtk.Scrollbar;
with Gtk.Signal;
with Gtk.Style;
with Gtk.Widget;       use Gtk.Widget;
with Common;           use Common;

package body Create_Scroll_Test is

   package Adjustment_Cb is new Signal.Callback
     (Base_Type => Adjustment.Gtk_Adjustment_Record,
      Data_Type => Drawing_Area.Gtk_Drawing_Area);

   package Event_Configure_Cb is new Signal.Two_Callback
     (Base_Type => Drawing_Area.Gtk_Drawing_Area_Record,
      Data_Type => Adjustment.Gtk_Adjustment,
      Cb_Type => Gdk.Event.Gdk_Event_Configure);

   package Event_Expose_Cb is new Signal.Two_Callback
     (Base_Type => Drawing_Area.Gtk_Drawing_Area_Record,
      Data_Type => Adjustment.Gtk_Adjustment,
      Cb_Type => Gdk.Event.Gdk_Event_Expose);

   Scroll_Test_Pos : Gint := 0;
   Scroll_Test_GC : Gdk.GC.Gdk_GC;
   Dialog : aliased Gtk.Dialog.Gtk_Dialog;


   -------------------------
   --  Adjustment_Change  --
   -------------------------

   procedure Adjustment_Change
     (Adj : access Adjustment.Gtk_Adjustment_Record;
      Widget   : in Drawing_Area.Gtk_Drawing_Area)
   is
      Source_Min : Gint := Gint (Get_Value (Adj)) - Scroll_Test_Pos;
      Source_Max : Gint := Source_Min + Gint (Get_Allocation_Height (Widget));
      Dest_Min   : Gint := 0;
      Dest_Max   : Gint := Gint (Get_Allocation_Height (Widget));
      Rect       : Gdk_Rectangle;
      Event      : Gdk_Event_Expose;

   begin
      Scroll_Test_Pos := Gint (Get_Value (Adj));

      if not Drawable_Is_Set (Widget) then
         return;
      end if;

      Rect := (X => 0, Y => 0, Width => 0, Height => 0);
      --  This is actually different from C, since we can not
      --  create a C's GdkRectangle, only a C's GdkRectangle*

      if Source_Min < 0 then
         Rect.Width := Get_Allocation_Width (Widget);
         Rect.Height := Guint'Min (Guint (-Source_Min),
                                   Get_Allocation_Height (Widget));
         Source_Min := 0;
         Dest_Min   := Gint (Rect.Height);
      else
         Rect.Y :=
                Gint'Max (0, 2 * Gint (Get_Allocation_Height (Widget))
                          - Source_Max);
         Rect.Width := Get_Allocation_Width (Widget);
         Rect.Height :=
                  Get_Allocation_Height (Widget) - Guint (Rect.Y);
         Source_Max := Gint (Get_Allocation_Height (Widget));
         Dest_Max := Rect.Y;
      end if;

      if Source_Min /= Source_Max then
         if not Is_Created (Scroll_Test_Gc) then
            Gdk_New (Scroll_Test_GC, Get_Window (Widget));
            Set_Exposures (Scroll_Test_GC, True);
         end if;

         Draw_Pixmap (Get_Window (Widget), Scroll_Test_GC,
                      Get_Window (Widget),
                      0, Source_Min,
                      0, Dest_Min,
                      Gint (Get_Allocation_Width (Widget)),
                      Source_Max - Source_Min);

         --  Make sure graphics expose events are processed before
         --  scrolling again

         loop
            Get_Graphics_Expose (Event, Get_Window (Widget));
            exit when not Is_Created (Event);
            Gtk.Widget.Event (Widget, Gdk_Event (Event));
            if Get_Count (Event) = 0 then
               Free (Event);
               exit;
            end if;
            Free (Event);
         end loop;
      end if;

      if Rect.Height /= 0 then
         Draw (Widget, Rect);
      end if;

   end Adjustment_Change;

   -----------------
   --  Configure  --
   -----------------

   procedure Configure (Widget  : access Drawing_Area.Gtk_Drawing_Area_Record;
                        Cb_Data : in Gdk.Event.Gdk_Event_Configure;
                        Adj     : in Adjustment.Gtk_Adjustment) is
      pragma Warnings (Off, Cb_Data);
   begin
      Set_Page_Increment (Adj, 0.9 * Gfloat (Get_Allocation_Height (Widget)));
      Set_Page_Size (Adj, Gfloat (Get_Allocation_Height (Widget)));
      --  FIXME Emit_By_Name (Adj, "changed");
   end Configure;

   --------------
   --  Expose  --
   --------------

   procedure Expose (Widget  : access Drawing_Area.Gtk_Drawing_Area_Record;
                     Event   : in Gdk.Event.Gdk_Event_Expose;
                     Adj     : in Adjustment.Gtk_Adjustment) is
      Area : Gdk.Rectangle.Gdk_Rectangle;
      Imin, Imax, Jmin, Jmax : Gint;
      Sty : aliased Gtk.Style.Gtk_Style_Record
        := Gtk.Style.Get_Style (Widget);
   begin
      Area := Gdk.Event.Get_Area (Event);

      Imin := Area.X / 10;
      Imax := (Area.X + Gint (Area.Width) + 9) / 10;

      Jmin := (Gint (Adjustment.Get_Value (Adj)) + Area.Y) / 10;
      Jmax := (Gint (Adjustment.Get_Value (Adj)) + Area.Y
               + Gint (Area.Height) + 9) / 10;

      Gdk.Window.Clear_Area (Window => Get_Window (Widget),
                             X => Area.X, Y => Area.Y,
                             Width => Gint (Area.Width),
                             Height => Gint (Area.Height));

      for I in Imin .. Imax loop
         for J in Jmin .. Jmax loop
            if ((I + J) mod 2 /= 0) then
               Gdk.Drawable.Draw_Rectangle
                 (Drawable => Get_Window (Widget),
                  GC => Gtk.Style.Get_Black_Gc (Sty'Access),
                  Filled => True,
                  X => 10 * I, Y => 10 * J - Gint (Adjustment.Get_Value (Adj)),
                  Width => 1 + I mod 10, Height => 1 + J mod 10);
            end if;
         end loop; --  J
      end loop;  --  I

   end Expose;


   -----------
   --  Run  --
   -----------

   procedure Run (Widget : access Gtk.Button.Gtk_Button_Record) is
      Id : Guint;
      Hbox : Box.Gtk_Box;
      Drawing_Area : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Adj : Gtk.Adjustment.Gtk_Adjustment;
      Scrollbar : Gtk.Scrollbar.Gtk_Scrollbar;
      Button : Gtk.Button.Gtk_Button;
   begin
      if Dialog = null then

         Gtk.Dialog.Gtk_New (Dialog);
         Id := Destroy_Dialog_Cb.Connect
           (Dialog, "destroy", Destroy_Dialog'Access, Dialog'Access);
         Set_Title (Window => Dialog, Title => "Scroll Test");
         Set_Border_Width (Dialog, 0);

         Box.Gtk_New_Hbox (Hbox, Homogeneous => False, Spacing => 0);
         Box.Pack_Start (In_Box => Gtk.Dialog.Get_Vbox (Dialog), Child => Hbox);
         Show (Hbox);

         Gtk.Drawing_Area.Gtk_New (Drawing_Area);
         Gtk.Drawing_Area.Size (Darea => Drawing_Area,
                                Width => 200, Height => 200);
         Box.Pack_Start (In_Box => Hbox, Child => Drawing_Area);
         Show (Drawing_Area);

         Set_Events (Widget => Drawing_Area,
                     Events => Gdk.Types.Exposure_Mask);

         Adjustment.Gtk_New (Adjustment => Adj, Value => 0.0, Lower => 0.0,
                             Upper => 1000.0, Step_Increment => 1.0,
                             Page_Increment => 180.0, Page_Size => 200.0);
         Scroll_Test_Pos := 0;

         Gtk.Scrollbar.Gtk_New_Vscrollbar (Widget => scrollbar,
                                           Adjustment => Adj);
         Box.Pack_Start (In_Box => Hbox, Child => Scrollbar,
                         Expand => False, Fill => False);
         Show (Scrollbar);

         Id := Event_Expose_Cb.Connect (Obj => Drawing_Area,
                                        Name => "expose_event",
                                        Func => Expose'Access,
                                        Func_Data => Adj);

         Id := Event_Configure_Cb.Connect (Obj => Drawing_Area,
                                           Name => "configure_event",
                                           Func => Configure'Access,
                                           Func_Data => Adj);

         Id := Adjustment_Cb.Connect (Obj => Adj,
                                      Name => "value_changed",
                                      Func => Adjustment_Change'Access,
                                      Func_Data => Drawing_Area);

         Gtk.Button.Gtk_New (Button, Label => "Quit");
         Box.Pack_Start (In_Box => Gtk.Dialog.Get_Action_Area (Dialog),
                         Child => Button);
         Id := Widget_Cb.Connect (Button, "clicked",
                                  Gtk.Widget.Destroy'Access,
                                  Dialog);
         Show (Button);
         Show (Dialog);
      else
         Destroy (Dialog);
      end if;

   end Run;

end Create_Scroll_Test;






