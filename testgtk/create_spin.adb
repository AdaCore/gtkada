-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
--         General Public License for more details.                  --
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
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Label; use Gtk.Label;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Object; use Gtk.Object;
with Gtk.Spin_Button; use Gtk.Spin_Button;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;
with Common; use Common;

package body Create_Spin is

   package Label_User is new User_Data (Gtk_Label);
   package Spin_O_Cb is new Signal.Object_Callback (Gtk_Spin_Button);
   package Spin_Cb is new Signal.Callback (Gtk_Toggle_Button,
                                           Gtk_Spin_Button);
   package Button_Cb is new Signal.Callback (Gtk_Button, Gint);

   Window   : aliased Gtk.Window.Gtk_Window;
   Spinner1 : Gtk_Spin_Button;

   procedure Change_Digits (Spin : in out Gtk_Spin_Button) is
   begin
      Set_Digits (Spinner1, Get_Value_As_Int (Spin));
   end Change_Digits;

   procedure Toggle_Snap (Widget : in out Gtk_Toggle_Button;
                          Spin : in out Gtk_Spin_Button) is
   begin
      if Is_Active (Widget) then
         Set_Update_Policy (Spin, Update_Always + Update_Snap_To_Ticks);
      else
         Set_Update_Policy (Spin, Update_Always);
      end if;
   end Toggle_Snap;

   procedure Toggle_Numeric (Widget : in out Gtk_Toggle_Button;
                             Spin : in out Gtk_Spin_Button) is
   begin
      Set_Numeric (Spin, Is_Active (Widget));
   end Toggle_Numeric;

   procedure Get_Value (Widget : in out Gtk_Button;
                        Data   : in out Gint)
   is
      Label : Gtk_Label := Label_User.Get (Widget);
      Spin  : Gtk_Spin_Button := Spinner1;
   begin
      if Data = 1 then
         Set (Label, Gint'Image (Get_Value_As_Int (Spin)));
      else
         Set (Label, Gfloat'Image (Get_Value_As_Float (Spin)));
      end if;
   end Get_Value;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button) is
      Id       : Guint;
      Main_Box : Gtk_Box;
      VBox      : Gtk_Box;
      Hbox     : Gtk_Box;
      Vbox2    : Gtk_Box;
      Label    : Gtk_Label;
      Adj      : Gtk_Adjustment;
      Spinner  : Gtk_Spin_Button;
      Spinner2 : Gtk_Spin_Button;
      Frame    : Gtk_Frame;
      Check    : Gtk_Check_Button;
      Button   : Gtk_Button;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Id := Widget2_Cb.Connect (Window, "destroy", Destroyed'Access,
                                   Window'Access);
         Set_Title (Window, "spin buttons");
         Border_Width (Window, Border_Width => 0);

         Gtk_New_Vbox (Main_Box, False, 5);
         Border_Width (Main_Box, 10);
         Add (Window, Main_Box);

         Gtk_New (Frame, "Not accelerated");
         Pack_Start (Main_Box, Frame, True, True, 0);

         Gtk_New_Vbox (VBox, False, 0);
         Border_Width (VBox, 5);
         Add (Frame, VBox);

         --  Day, month, year spinners
         Gtk_New_Hbox (Hbox, False, 0);
         Pack_Start (VBox, Hbox, True, True, 5);

         Gtk_New_Vbox (Vbox2, False, 0);
         Pack_Start (Hbox, Vbox2, True, True, 5);
         Gtk_New (Label, "Day:");
         Set_Alignment (Label, 0.0, 0.5);
         Pack_Start (Vbox2, Label, False, True, 0);
         Gtk_New (Adj, 1.0, 1.0, 31.0, 1.0, 5.0, 0.0);
         Gtk_New (Spinner, Adj, 0.0, 0);
         Set_Wrap (Spinner, True);
         Pack_Start (Vbox2, Spinner, False, True, 0);

         Gtk_New_Vbox (Vbox2, False, 0);
         Pack_Start (Hbox, Vbox2, True, True, 5);
         Gtk_New (Label, "Month:");
         Set_Alignment (Label, 0.0, 0.5);
         Pack_Start (Vbox2, Label, False, True, 0);
         Gtk_New (Adj, 1.0, 1.0, 12.0, 1.0, 5.0, 0.0);
         Gtk_New (Spinner, Adj, 0.0, 0);
         Set_Wrap (Spinner, True);
         Pack_Start (Vbox2, Spinner, False, True, 0);

         Gtk_New_Vbox (Vbox2, False, 0);
         Pack_Start (Hbox, Vbox2, True, True, 5);
         Gtk_New (Label, "Year:");
         Set_Alignment (Label, 0.0, 0.5);
         Pack_Start (Vbox2, Label, False, True, 0);
         Gtk_New (Adj, 1998.0, 0.0, 2100.0, 1.0, 100.0, 0.0);
         Gtk_New (Spinner, Adj, 0.0, 0);
         Set_Wrap (Spinner, True);
         Set_Usize (Spinner, 55, 0);
         Pack_Start (Vbox2, Spinner, False, True, 0);

         Gtk_New (Frame, "Accelerated");
         Pack_Start (Main_Box, Frame, True, True, 0);

         Gtk_New_Vbox (Vbox, False, 0);
         Border_Width (Vbox, 5);
         Add (Frame, Vbox);

         Gtk_New_Hbox (Hbox, False, 0);
         Pack_Start (Vbox, Hbox, False, True, 5);

         Gtk_New_Vbox (Vbox2, False, 0);
         Pack_Start (Hbox, Vbox2, True, True, 5);
         Gtk_New (Label, "Value:");
         Set_Alignment (Label, 0.0, 0.5);
         Pack_Start (Vbox2, Label, False, True, 0);
         Gtk_New (Adj, 0.0, -10000.0, 10000.0, 0.5, 100.0, 0.0);
         Gtk_New (Spinner1, Adj, 1.0, 2);
         Set_Wrap (Spinner1, True);
         Set_Usize (Spinner1, 100, 0);
         Set_Update_Policy (Spinner1, Update_Always);
         Pack_Start (Vbox2, Spinner1, False, True, 0);

         Gtk_New_Vbox (Vbox2, False, 0);
         Pack_Start (Hbox, Vbox2, True, True, 5);
         Gtk_New (Label, "Digits:");
         Set_Alignment (Label, 0.0, 0.5);
         Pack_Start (Vbox2, Label, False, True, 0);
         Gtk_New (Adj, 2.0, 1.0, 5.0, 1.0, 1.0, 0.0);
         Gtk_New (Spinner2, Adj, 0.0, 0);
         Set_Wrap (Spinner2, True);
         Id := Spin_O_Cb.Connect (Adj, "value_changed", Change_Digits'Access,
                                  Spinner2);

         Pack_Start (Vbox2, Spinner2, False, True, 0);

         Gtk_New_Hbox (Hbox, False, 0);
         Pack_Start (Vbox, Hbox, False, True, 5);

         Gtk_New (Check, "Snap to 0.5-ticks");
         Id := Spin_Cb.Connect (Check, "clicked", Toggle_Snap'Access,
                                Spinner1);
         Pack_Start (Vbox, Check, True, True, 0);
         Set_State (Check, True);

         Gtk_New (Check, "Snap Numeric only input mode");
         Id := Spin_Cb.Connect (Check, "clicked", Toggle_Numeric'Access,
                                Spinner1);
         Pack_Start (Vbox, Check, True, True, 0);
         Set_State (Check, True);

         Gtk_New (Label, "");
         Gtk_New_Hbox (Hbox, False, 0);
         Pack_Start (Vbox, Hbox, False, True, 5);

         Gtk_New (Button, "Value as Int");
         Label_User.Set (Button, Label);
         Id := Button_Cb.Connect (Button, "clicked", Get_Value'Access, 1);
         Pack_Start (Hbox, Button, True, True, 5);

         Gtk_New (Button, "Value as Float");
         Label_User.Set (Button, Label);
         Id := Button_Cb.Connect (Button, "clicked", Get_Value'Access, 2);
         Pack_Start (Hbox, Button, True, True, 5);

         Pack_Start (Vbox, Label, True, True, 0);
         Set (Label, "0");

         Gtk_New_Hbox (Hbox, False, 0);
         Pack_Start (Main_Box, Hbox, False, True, 0);

         Gtk_New (Button, "Close");
         Id := Widget_Cb.Connect (Button, "clicked", Destroy'Access, Window);
         Pack_Start (Hbox, Button, True, True, 5);
      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Show_All (Window);
      else
         Destroy (Window);
      end if;

   end Run;

end Create_Spin;










