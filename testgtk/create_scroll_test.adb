-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
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

with Gdk.Drawable;
with Gdk.Event;
with Gdk.GC;
with Gdk.Rectangle; use Gdk.Rectangle;
with Gdk.Types;
with Gdk.Window;

with Gtk; use Gtk;
with Gtk.Adjustment;
with Gtk.Box;
with Gtk.Button;
with Gtk.Container;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Drawing_Area;
with Gtk.Scrollbar;
with Gtk.Signal;
with Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body Create_Scroll_Test is

   package Widget_Cb is new Signal.Object_Callback (Widget.Gtk_Widget);

   package Adjustment_Cb is new Signal.Callback
     (Widget_Type => Adjustment.Gtk_Adjustment,
      Data_Type => Drawing_Area.Gtk_Drawing_Area);

   package Event_Configure_Cb is new Signal.Two_Callback
     (Widget_Type => Drawing_Area.Gtk_Drawing_Area,
      Data_Type => Adjustment.Gtk_Adjustment,
      Cb_Type => Gdk.Event.Gdk_Event_Configure);

   package Event_Expose_Cb is new Signal.Two_Callback
     (Widget_Type => Drawing_Area.Gtk_Drawing_Area,
      Data_Type => Adjustment.Gtk_Adjustment,
      Cb_Type => Gdk.Event.Gdk_Event_Expose);

   package Widget2_Cb is new Signal.Callback (Gtk_Widget, Gtk_Widget_Access);


   Scroll_Test_Pos : Integer := 0;
   Scroll_Test_GC : Gdk.GC.Gdk_GC;
   Dialog : aliased Gtk.Dialog.Gtk_Dialog;


   -------------------------
   --  Adjustment_Change  --
   -------------------------

   procedure Adjustment_Change
     (Widget : in out Adjustment.Gtk_Adjustment'Class;
      Data   : in out Drawing_Area.Gtk_Drawing_Area) is
   begin
      null;
   end Adjustment_Change;


   -----------------
   --  Configure  --
   -----------------

   procedure Configure (Widget  : in out Drawing_Area.Gtk_Drawing_Area'Class;
                        Cb_Data : in out Gdk.Event.Gdk_Event_Configure;
                        Data    : in out Adjustment.Gtk_Adjustment) is
   begin
      null;
   end Configure;


   --------------
   --  Expose  --
   --------------

   procedure Expose (Widget  : in out Drawing_Area.Gtk_Drawing_Area'Class;
                     Event   : in out Gdk.Event.Gdk_Event_Expose;
                     Adj     : in out Adjustment.Gtk_Adjustment) is
      Area : Gdk.Rectangle.Gdk_Rectangle;
      Imin, Imax, Jmin, Jmax : Gint;
   begin
      Gdk.Event.Get_Area (Event => Event, Area => Area);

      Imin := Get_X (Area) / 10;
      Imax := (Get_X (Area) + Gint (Get_Width (Area)) + 9) / 10;

      Jmin := (Gint (Adjustment.Get_Value (Adj)) + Get_Y (Area)) / 10;
      Jmax := (Gint (Adjustment.Get_Value (Adj)) + Get_Y (Area)
               + Gint (Get_Height (Area)) + 9) / 10;

      Gdk.Window.Clear_Area (Window => Gdk.Window.Get_Window (Widget),
                             X => Get_X (Area), Y => Get_Y (Area),
                             Width => Gint (Get_Width (Area)),
                             Height => Gint (Get_Height (Area)));

      for I in Imin .. Imax loop
         for J in Jmin .. Jmax loop
            if ((I + J) mod 2 /= 0) then
               Gdk.Drawable.Draw_Rectangle
                 (Drawable => Gdk.Window.Get_Window (Widget),
                  GC => Style.Get_Black_Gc (Style.Get_Style (Widget)),
                  Filled => True,
                  X => 10 * I, Y => 10 * J - Gint (Adjustment.Get_Value (Adj)),
                  Width => 1 + 10 mod I, Height => 1 + 10 mod J);
            end if;
         end loop; --  J
      end loop;  --  I

   end Expose;


   -----------
   --  Run  --
   -----------

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id : Guint;
      Hbox : Box.Gtk_Box;
      Drawing_Area : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Adj : Gtk.Adjustment.Gtk_Adjustment;
      Scrollbar : Gtk.Scrollbar.Gtk_Scrollbar;
      Button : Gtk.Button.Gtk_Button;
   begin
      if not Is_Created (Dialog) then

         Gtk.Dialog.Gtk_New (Dialog);
         Id := Widget2_Cb.Connect (Dialog, "destroy",
                                   Destroyed'Access,
                                   Dialog'Access);
         Gtk.Dialog.Set_Title (Window => Dialog, Title => "Scroll Test");
         Container.Border_Width (Dialog, 0);

         Box.Gtk_New_Hbox (Widget => Hbox, Homogeneous => False, Spacing => 0);
         Box.Pack_Start (In_Box => Gtk.Dialog.Get_Vbox (Dialog), Child => Hbox);
         Gtk.Widget.Show (Hbox);

         Gtk.Drawing_Area.Gtk_New (Drawing_Area);
         Gtk.Drawing_Area.Size (Darea => Drawing_Area,
                                Width => 200, Height => 200);
         Box.Pack_Start (In_Box => Hbox, Child => Drawing_Area);
         Gtk.Widget.Show (Drawing_Area);

         Gtk.Widget.Set_Events (Widget => Drawing_Area,
                                Events => Gdk.Types.Exposure_Mask);

         Adjustment.Gtk_New (Adjustment => Adj, Value => 0.0, Lower => 0.0,
                             Upper => 1000.0, Step_Increment => 1.0,
                             Page_Increment => 180.0, Page_Size => 200.0);
         Scroll_Test_Pos := 0;

         Gtk.Scrollbar.Gtk_New_Vscrollbar (Widget => scrollbar,
                                           Adjustment => Adj);
         Box.Pack_Start (In_Box => Hbox, Child => Scrollbar,
                         Expand => False, Fill => False);
         Gtk.Widget.Show (Scrollbar);

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

         Gtk.Button.Gtk_New (Widget => Button, Label => "Quit");
         Box.Pack_Start (In_Box => Gtk.Dialog.Get_Action_Area (Dialog),
                         Child => Button);
         Id := Widget_Cb.Connect (Button, "clicked",
                                  Gtk.Widget.Destroy'Access,
                                  Dialog);
         Gtk.Widget.Show (Button);

      end if;

      if not Gtk.Widget.Visible_Is_Set (Dialog) then
         Gtk.Widget.Show (Dialog);
      else
         Gtk.Widget.Destroy (Dialog);
      end if;

   end Run;

end Create_Scroll_Test;
