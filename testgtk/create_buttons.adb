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
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Object; use Gtk.Object;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Table; use Gtk.Table;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;

package body Create_Buttons is

   package Button_Cb is new Signal.Object_Callback (Gtk_Button);
   package Window_Cb is new Signal.Object_Callback (Gtk_Widget);

   Window : Gtk.Window.Gtk_Window;

   procedure Button_Window (Widget : in out Gtk_Button'Class) is
   begin
      if Visible_Is_Set (Widget) then
         Hide (Widget);
      else
         Show (Widget);
      end if;
   end Button_Window;


   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id      : Guint;
      Box1,
        Box2  : Gtk_Box;
      Separator : Gtk_Separator;
      Table   : Gtk_Table;
      Button  : array (0 .. 8) of Gtk_Button;
      Left_A  : array (0 .. 8) of Gint := (0, 1, 2, 0, 2, 1, 1, 2, 0);
      Right_A : array (0 .. 8) of Gint := (1, 2, 3, 1, 3, 2, 2, 3, 1);
      Top_A   : array (0 .. 8) of Gint := (0, 1, 2, 2, 0, 2, 0, 1, 1);
      Bott_A  : array (0 .. 8) of Gint := (1, 2, 3, 3, 1, 3, 1, 2, 2);
      Close   : Gtk_Button;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Id := Window_Cb.Connect (Window, "destroy", Destroy'Access, Window);
         Set_Title (Window, "buttons");
         Border_Width (Window, Border_Width => 0);

         Gtk_New_Vbox (Box1, Homogeneous => False, Spacing => 0);
         Add (Window, Box1);
         Show (Box1);

         Gtk_New (Table, Rows => 3, Columns => 3, Homogeneous => False);
         Set_Row_Spacings (Table, Spacing => 5);
         Set_Col_Spacings (Table, Spacing => 5);
         Border_Width (Table, Border_Width => 10);
         Pack_Start (Box1, Table, Expand => True, Fill => True, Padding => 0);
         Show (Table);

         for J in Button'Range loop
            Gtk_New (Button (J), Label => "Button" & Integer'Image (J));
         end loop;

         for J in Button'Range loop
            Id :=Button_Cb.Connect (Button (J), "clicked", Button_Window'Access,
                                    Button ((J + 1) mod Button'Length));
            Attach (Table, Button (J),
                    Left_A (J), Right_A (J),
                    Top_A (J), Bott_A (J),
                    Expand + Fill,
                    Expand + Fill, Xpadding => 0, Ypadding => 0);
            Show (Button (J));
         end loop;

         Gtk_New_Hseparator (Separator);
         Pack_Start (Box1, Separator, Expand => False, Fill => True,
                     Padding => 0);
         Show (Separator);

         Gtk_New_Vbox (Box2, Homogeneous => False, Spacing => 0);
         Border_Width (Box2, Border_Width => 10);
         Pack_Start (Box1, Box2, Expand => False, Fill => True, Padding => 0);
         Show (Box2);

         Gtk_New (Close, Label => "Close");
         Id := Window_Cb.Connect (Close, "clicked", Destroy'Access, Window);
         Pack_Start (Box2, Close, Expand => True, Fill => True, Padding => 0);
         Set_Flags (Close, Can_Default);
         Grab_Default (Close);
         Show (Close);

      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Gtk.Widget.Show (Window);
      else
         Gtk.Widget.Destroy (Window);
      end if;

   end Run;

end Create_Buttons;

