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
with Gdk.Types; use Gdk.Types;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Alignment;  use Gtk.Alignment;
with Gtk.Box; use Gtk.Box;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Frame;  use Gtk.Frame;
with Gtk.Gentry; use Gtk.Gentry;
with Gtk.Label; use Gtk.Label;
with Gtk.Main; use Gtk.Main;
with Gtk.Option_Menu; use Gtk.Option_Menu;
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtk.Spin_Button; use Gtk.Spin_Button;
with Gtk.Table;       use Gtk.Table;
with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;
with Gtkada.Types; use Gtkada.Types;

with Common; use Common;
with Interfaces.C.Strings;

package body Create_Progress is

   package ICS renames Interfaces.C.Strings;

   package Time_Cb   is new Gtk.Main.Timeout (Gtk_Progress_Bar);

   Items1 : constant Chars_Ptr_Array :=
     (ICS.New_String ("Left-Right"),
      ICS.New_String ("Right-Left"),
      ICS.New_String ("Bottom-Top"),
      ICS.New_String ("Top-Bottom"));

   Items2 : constant Chars_Ptr_Array :=
     (ICS.New_String ("Continuous"),
      ICS.New_String ("Discrete"));

   type Simple_Cb_Func is access procedure (Wiget : access Gtk_Widget_Record);

   type ProgressData is
      record
         Pbar            : Gtk_Progress_Bar;
         Block_Spin      : Gtk_Spin_Button;
         X_Align_Spin    : Gtk_Spin_Button;
         Y_Align_Spin    : Gtk_Spin_Button;
         Step_Spin       : Gtk_Spin_Button;
         Act_Blocks_Spin : Gtk_Spin_Button;
         Label           : Gtk_Label;
         Omenu1          : Gtk_Option_Menu;
         Omenu1_Group    : Widget_Slist.GSlist;
         Omenu2          : Gtk_Option_Menu;
         Omenu2_Group    : Widget_Slist.GSlist;
         Gentry          : Gtk_Entry;
         Timer           : Guint;
      end record;

   Pdata : ProgressData;

   function Progress_Timeout (Pbar : Gtk_Progress_Bar) return Boolean is
      pragma Warnings (Off, Pbar);
      New_Val : Gfloat;
      Adj     : Gtk_Adjustment := Get_Adjustment (Pdata.Pbar);
   begin
      New_Val := Get_Value (Adj) + 1.0;
      if New_Val > Get_Upper (Adj) then
         New_Val := Get_Lower (Adj);
      end if;
      Set_Value (Pdata.Pbar, New_Val);
      return True;
   end Progress_Timeout;


   procedure Destroy_Progress (Window : access Gtk_Widget_Record) is
      pragma Warnings (Off, Window);
   begin
      Timeout_Remove (Pdata.Timer);
      Pdata.Omenu1_Group := Widget_Slist.Null_List;
      Pdata.Omenu2_Group := Widget_Slist.Null_List;
      Pdata.Timer := 0;
      --  Note: we are in a callback for destroy, so the window will be
      --  destroyed elsewhere. No need to do that here.
   end Destroy_Progress;

   procedure Toggle_Orientation (Widget : access Gtk_Widget_Record) is
      pragma Warnings (Off, Widget);
      I : Natural := Selected_Button (Pdata.Omenu1_Group);
   begin
      Set_Orientation (Pdata.Pbar,
                       Gtk_Progress_Bar_Orientation'Val (3 - I));
   end Toggle_Orientation;


   procedure Toggle_Show_Text (Widget : access Gtk_Check_Button_Record) is
   begin
      Set_Show_Text (Progress  => Pdata.Pbar,
                     Show_Text => Is_Active (Widget));
      Set_Sensitive (Pdata.Gentry, Is_Active (Widget));
      Set_Sensitive (Pdata.X_Align_Spin, Is_Active (Widget));
      Set_Sensitive (Pdata.Y_Align_Spin, Is_Active (Widget));
   end Toggle_Show_Text;

   procedure Toggle_Bar_Style (Widget : access Gtk_Widget_Record) is
      pragma Warnings (Off, Widget);
      I : Natural := Selected_Button (Pdata.Omenu2_Group);
   begin
      Set_Sensitive (Pdata.Block_Spin, I /= 1);
      Set_Bar_Style (Pdata.Pbar, Gtk_Progress_Bar_Style'Val (1 - I));
   end Toggle_Bar_Style;


   procedure Value_Changed (Adj   : access Gtk_Adjustment_Record) is
      pragma Warnings (Off, Adj);
   begin
      if Get_Activity_Mode (Pdata.Pbar) then
         Set_Text (Pdata.Label, "???");
      else
         Set_Text (Pdata.Label,
                   GFloat'Image (Get_Current_Percentage (Pdata.Pbar))
                   & "%");
      end if;
   end Value_Changed;


   procedure Adjust_Blocks (Adj   : access Gtk_Adjustment_Record) is
      pragma Warnings (Off, Adj);
   begin
      Set_Percentage (Pdata.Pbar, 0.0);
      Set_Discrete_Blocks (Pdata.Pbar,
                           Guint (Get_Value_As_Int (Pdata.Block_Spin)));
   end Adjust_Blocks;


   procedure Adjust_Step (Adj   : access Gtk_Adjustment_Record) is
      pragma Warnings (Off, Adj);
   begin
      Set_Activity_Step (Pdata.Pbar,
                         Guint (Get_Value_As_Int (Pdata.Step_Spin)));
   end Adjust_Step;


   procedure Adjust_Act_Blocks (Adj   : access Gtk_Adjustment_Record) is
      pragma Warnings (Off, Adj);
   begin
      Set_Activity_Blocks (Pdata.Pbar,
                           Guint (Get_Value_As_Int (Pdata.Act_Blocks_Spin)));
   end Adjust_Act_Blocks;


   procedure Adjust_Align (Adj   : access Gtk_Adjustment_Record) is
      pragma Warnings (Off, Adj);
   begin
      Set_Text_Alignment (Pdata.Pbar,
                          Get_Value_As_Float (Pdata.X_Align_Spin),
                          Get_Value_As_Float (Pdata.Y_Align_Spin));
   end Adjust_Align;


   procedure Toggle_Activity_Mode (Widget : access Gtk_Check_Button_Record) is
   begin
      Set_Activity_Mode (Pdata.Pbar, Is_Active (Widget));
      Set_Sensitive (Pdata.Step_Spin, Is_Active (Widget));
      Set_Sensitive (Pdata.Act_Blocks_Spin, Is_Active (Widget));
   end Toggle_Activity_Mode;


   procedure Entry_Changed (Widget : access Gtk_Widget_Record) is
      pragma Warnings (Off, Widget);
   begin
      Set_Format_String (Pdata.Pbar, Get_Text (Pdata.Gentry));
   end Entry_Changed;


   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Id     : Guint;
      Vbox   : Gtk_Box;
      Vbox2  : Gtk_Box;
      Hbox   : Gtk_Box;
      Frame2 : Gtk_Frame;
      Align  : Gtk_Alignment;
      Adj    : Gtk_Adjustment;
      Label  : Gtk_Label;
      Tab    : Gtk_Table;
      Check  : Gtk_Check_Button;

   begin
      Set_Label (Frame, "Progress Bar");

      Pdata.Timer := 0;

      Gtk_New_Vbox (Vbox, False, 5);
      Set_Border_Width (Vbox, 10);
      Add (Frame, Vbox);

      Id := Widget3_Cb.Connect
        (Vbox, "destroy", Destroy_Progress'Access);

      Gtk_New (Frame2, "Progress");
      Pack_Start (Vbox, Frame2, False, True, 0);

      Gtk_New_Vbox (Vbox2, False, 5);
      Add (Frame2, Vbox2);

      Gtk_New (Align,
               Xalign => 0.5,
               Yalign => 0.5,
               Xscale => 0.0,
               Yscale => 0.0);
      Pack_Start (Vbox2, Align, False, False, 5);

      Gtk_New (Adj,
               Value          => 0.0,
               Lower          => 1.0,
               Upper          => 300.0,
               Step_Increment => 0.0,
               Page_Increment => 0.0,
               Page_Size      => 0.0);
      Id := Adj_Cb.Connect (Adj, "value_changed",
                            Value_Changed'Access);

      Gtk_New (Pdata.Pbar, Adj);
      Set_Format_String (Pdata.Pbar, "%v from [%l,%u] (=%p%%)");
      Add (Align, Pdata.Pbar);

      Pdata.Timer := Time_Cb.Add (100, Progress_Timeout'Access, Pdata.Pbar);

      Gtk_New (Align,
               Xalign => 0.5,
               Yalign => 0.5,
               Xscale => 0.0,
               Yscale => 0.0);
      Pack_Start (Vbox2, Align, False, False, 5);

      Gtk_New_Hbox (Hbox, False, 5);
      Add (Align, Hbox);

      Gtk_New (Label, "Label updated by user :");
      Pack_Start (Hbox, Label, False, True, 0);

      Gtk_New (Pdata.Label, "");
      Pack_Start (Hbox, Pdata.Label, False, True, 0);

      Gtk_New (Frame2, "Options");
      Pack_Start (Vbox, Frame2, False, True, 0);

      Gtk_New_Vbox (Vbox2, False, 5);
      Add (Frame2, Vbox2);

      Gtk_New (Tab,
               Rows        => 7,
               Columns     => 2,
               Homogeneous => False);
      Pack_Start (Vbox2, Tab, False, True, 0);

      Gtk_New (Label, "Orientation :");
      Attach (Tab, Label, 0, 1, 0, 1, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);
      Set_Alignment (Label, 0.0, 0.5);

      Build_Option_Menu (Pdata.Omenu1, Pdata.Omenu1_Group,
                         Items1, 0, Toggle_Orientation'Access);

      Gtk_New_Hbox (Hbox, False, 0);
      Attach (Tab, Hbox, 1, 2, 0, 1, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);
      Pack_Start (Hbox, Pdata.Omenu1, True, True, 0);

      Gtk_New (Check, "Show Text");
      Id := Check_Cb.Connect (Check, "clicked", Toggle_Show_Text'Access);
      Attach (Tab, Check, 0, 1, 1, 2, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);

      Gtk_New_Hbox (Hbox, False, 0);
      Attach (Tab, Hbox, 1, 2, 1, 2, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);

      Gtk_New (Label, "Format : ");
      Pack_Start (Hbox, Label, False, True, 0);

      Gtk_New (Pdata.Gentry);
      Id := Widget_Cb.Connect (Pdata.Gentry, "changed",
                               Entry_Changed'Access, Pdata.Gentry);
      Pack_Start (Hbox, Pdata.Gentry, True, True, 0);
      Set_Text (Pdata.Gentry, "%v from [%l,%u] (=%p%%)");
      Set_Usize (Pdata.Gentry, 100, -1);
      Set_Sensitive (Pdata.Gentry, False);

      Gtk_New (Label, "Text align :");
      Attach (Tab, label, 0, 1, 2, 3, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);
      Set_Alignment (Label, 0.0, 0.5);

      Gtk_New_Hbox (Hbox, False, 0);
      Attach (Tab, Hbox, 1, 2, 2, 3, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);

      Gtk_New (Label, "x :");
      Pack_Start (Hbox, Label, False, True, 5);

      Gtk_New (Adj,
               Value          => 0.5,
               Lower          => 0.0,
               Upper          => 1.0,
               Step_Increment => 0.1,
               Page_Increment => 0.1,
               Page_Size      => 0.0);
      Gtk_New (Pdata.X_Align_Spin, Adj, Climb_Rate => 0.0, The_Digits => 1);
      Pack_Start (Hbox, Pdata.X_Align_Spin, False, True, 0);
      Set_Sensitive (Pdata.X_Align_Spin, False);
      Id := Adj_Cb.Connect (Adj, "value_changed", Adjust_Align'Access);

      Gtk_New (Label, "y :");
      Pack_Start (Hbox, Label, False, True, 5);

      Gtk_New (Adj,
               Value          => 0.5,
               Lower          => 0.0,
               Upper          => 1.0,
               Step_Increment => 0.1,
               Page_Increment => 0.1,
               Page_Size      => 0.0);
      Gtk_New (Pdata.Y_Align_Spin, Adj, Climb_Rate => 0.0, The_Digits => 1);
      Pack_Start (Hbox, Pdata.Y_Align_Spin, False, True, 0);
      Set_Sensitive (Pdata.Y_Align_Spin, False);
      Id := Adj_Cb.Connect (Adj, "value_changed", Adjust_Align'Access);

      Gtk_New (Label, "Bar Style :");
      Attach (Tab, Label, 0, 1, 3, 4, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);
      Set_Alignment (Label, 0.0, 0.5);

      Build_Option_Menu (Pdata.Omenu2, Pdata.Omenu2_Group,
                         Items2, 0, Toggle_Bar_Style'Access);

      Gtk_New_Hbox (Hbox, False, 0);
      Attach (Tab, Hbox, 1, 2, 3, 4, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);
      Pack_Start (Hbox, Pdata.Omenu2, True, True, 0);

      Gtk_New (Label, "Block count :");
      Attach (Tab, Label, 0, 1, 4, 5, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);
      Set_Alignment (Label, 0.0, 0.5);

      Gtk_New_Hbox (Hbox, False, 0);
      Attach (Tab, Hbox, 1, 2, 4, 5, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);

      Gtk_New (Adj,
               Value          => 10.0,
               Lower          => 2.0,
               Upper          => 20.0,
               Step_Increment => 1.0,
               Page_Increment => 5.0,
               Page_Size      => 0.0);
      Gtk_New (Pdata.Block_Spin, Adj, Climb_Rate => 0.0, The_Digits => 0);
      Pack_Start (Hbox, Pdata.Block_Spin, False, True, 0);
      Set_Sensitive (Pdata.Block_Spin, False);
      Id := Adj_Cb.Connect (Adj, "value_changed", Adjust_Blocks'Access);

      Gtk_New (Check, "Activity mode");
      Id := Check_Cb.Connect (Check, "clicked",
                              Toggle_Activity_Mode'Access);
      Attach (Tab, Check, 0, 1, 5, 6, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);

      Gtk_New_Hbox (Hbox, False, 0);
      Attach (Tab, Hbox, 1, 2, 5, 6, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);

      Gtk_New (Label, "Step size :");
      Pack_Start (Hbox, Label, False, True, 0);

      Gtk_New (Adj,
               Value          => 3.0,
               Lower          => 1.0,
               Upper          => 20.0,
               Step_Increment => 1.0,
               Page_Increment => 5.0,
               Page_Size      => 0.0);
      Gtk_New (Pdata.Step_Spin, Adj, Climb_Rate => 0.0, The_Digits => 0);
      Pack_Start (Hbox, Pdata.Step_Spin, False, True, 0);
      Set_Sensitive (Pdata.Step_Spin, False);
      Id := Adj_Cb.Connect (Adj, "value_changed", Adjust_Step'Access);

      Gtk_New_Hbox (Hbox, False, 0);
      Attach (Tab, Hbox, 1, 2, 6, 7, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);

      Gtk_New (Label, "Blocks :");
      Pack_Start (Hbox, Label, False, True, 0);

      Gtk_New (Adj,
               Value          => 5.0,
               Lower          => 2.0,
               Upper          => 10.0,
               Step_Increment => 1.0,
               Page_Increment => 5.0,
               Page_Size      => 0.0);
      Gtk_New (Pdata.Act_Blocks_Spin, Adj, Climb_Rate => 0.0,
               The_Digits => 0);
      Pack_Start (Hbox, Pdata.Act_Blocks_Spin, False, True, 0);
      Set_Sensitive (Pdata.Act_Blocks_Spin, False);
      Id := Adj_Cb.Connect (Adj, "value_changed", Adjust_Act_Blocks'Access);

      Show_All (Vbox);
   end Run;

end Create_Progress;

