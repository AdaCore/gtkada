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
-----------------------------------------------------------------------

with Glib; use Glib;
with Gdk.GC;
with Gdk.Types;
with Gtk; use Gtk;
with Gtk.Adjustment;
with Gtk.Box;
with Gtk.Button;
with Gtk.Container;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Drawing_Area;
with Gtk.Scrollbar;
with Gtk.Signal;
with Gtk.Widget;

package body Create_Scroll_Test is

   package Widget_Cb is new Signal.Object_Callback (Widget.Gtk_Widget);

   Scroll_Test_Pos : Integer := 0;
   Scroll_Test_GC : Gdk.GC.Gdk_GC;
   Dialog : Gtk.Dialog.Gtk_Dialog;


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
         Id := Widget_Cb.Connect (Dialog, "destroy",
                                  Gtk.Widget.Destroy'Access,
                                  Dialog);
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

         --
         --  FIXME : Connect the event related callbacks
         --

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
