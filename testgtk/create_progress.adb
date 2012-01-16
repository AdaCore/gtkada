------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2012, AdaCore                     --
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

with Glib;                use Glib;
with Gtk.Adjustment;      use Gtk.Adjustment;
with Gtk.Alignment;       use Gtk.Alignment;
with Gtk.Box;             use Gtk.Box;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Label;           use Gtk.Label;
with Glib.Main;           use Glib.Main;
--  ??? replace Option_Menu with, what, Combo_Box?
with Gtk.Option_Menu;     use Gtk.Option_Menu;
with Gtk.Progress_Bar;    use Gtk.Progress_Bar;
with Gtk.Spin_Button;     use Gtk.Spin_Button;
with Gtk.Table;           use Gtk.Table;
with Gtk.Widget;          use Gtk.Widget;
with Gtk;                 use Gtk;
with Gtkada.Types;        use Gtkada.Types;

with Common; use Common;

package body Create_Progress is

   package Time_Cb  is new Glib.Main.Generic_Sources (Gtk_Progress_Bar);

   Items1 : constant Chars_Ptr_Array := "Horizontal" + "Vertical";
   Items2 : constant Chars_Ptr_Array := "Not Inverted" + "Inverted";

   type ProgressData is record
      Pbar            : Gtk_Progress_Bar;
      Block_Spin      : Gtk_Spin_Button;
      X_Align_Spin    : Gtk_Spin_Button;
      Y_Align_Spin    : Gtk_Spin_Button;
      Step_Spin       : Gtk_Spin_Button;
      Act_Blocks_Spin : Gtk_Spin_Button;
      Label           : Gtk_Label;
      Omenu1          : Gtk_Option_Menu;
      Omenu1_Group    : Widget_SList.GSlist;
      Omenu2          : Gtk_Option_Menu;
      Omenu2_Group    : Widget_SList.GSlist;
      Gentry          : Gtk_Entry;
      Timer           : G_Source_Id;
   end record;

   Pdata : ProgressData;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Progress_Bar@B is a widget that you can use to display"
        & " the status of an operation. In this example, the progress bar is"
        & " associated with a @bTimeout@B, and thus updates itself"
        & " periodically.";
   end Help;

   ----------------------
   -- Progress_Timeout --
   ----------------------

   function Progress_Timeout (Pbar : Gtk_Progress_Bar) return Boolean is
      pragma Warnings (Off, Pbar);
      New_Val : Gdouble;
   begin
      New_Val := Get_Fraction (Pdata.Pbar);

      New_Val := New_Val + 0.05;
      if New_Val > 1.0 then
         New_Val := 0.0;
      end if;

      Set_Fraction (Pdata.Pbar, New_Val);

      return True;
   end Progress_Timeout;

   ----------------------
   -- Destroy_Progress --
   ----------------------

   procedure Destroy_Progress (Window : access Gtk_Widget_Record'Class) is
      pragma Warnings (Off, Window);
   begin
      Remove (Pdata.Timer);
      Pdata.Omenu1_Group := Widget_SList.Null_List;
      Pdata.Timer := 0;
      --  Note: we are in a callback for destroy, so the window will be
      --  destroyed elsewhere. No need to do that here.
   end Destroy_Progress;

   ----------------------
   -- Toggle_Inversion --
   ----------------------

   procedure Toggle_Inversion (Widget : access Gtk_Widget_Record'Class) is
      pragma Warnings (Off, Widget);
      Is_Inverted : constant Boolean := Get_Inverted (Pdata.Pbar);
   begin
      Set_Inverted (Pdata.Pbar, not Is_Inverted);
   end Toggle_Inversion;

   ------------------------
   -- Toggle_Orientation --
   ------------------------

   procedure Toggle_Orientation (Widget : access Gtk_Widget_Record'Class) is
      pragma Warnings (Off, Widget);
      Orientation : constant Gtk_Orientation := Get_Orientation (Pdata.Pbar);
   begin
      if Orientation = Gtk_Orientation'Last then
         Set_Orientation (Pdata.Pbar, Gtk_Orientation'First);
      else
         Set_Orientation (Pdata.Pbar, Gtk_Orientation'Succ (Orientation));
      end if;
   end Toggle_Orientation;

   ----------------------
   -- Toggle_Show_Text --
   ----------------------

   procedure Toggle_Show_Text (Widget : access Gtk_Check_Button_Record'Class)
   is
   begin
      Set_Show_Text (Progress_Bar => Pdata.Pbar,
                     Show_Text    => Get_Active (Widget));
      Set_Sensitive (Pdata.Gentry, Get_Active (Widget));
      Set_Sensitive (Pdata.X_Align_Spin, Get_Active (Widget));
      Set_Sensitive (Pdata.Y_Align_Spin, Get_Active (Widget));
   end Toggle_Show_Text;

   --------------------------
   -- Toggle_Activity_Mode --
   --------------------------

   procedure Toggle_Activity_Mode
     (Widget : access Gtk_Check_Button_Record'Class)
   is
   begin
      if Get_Active (Widget) then
         --  Calling Pulse will cause the progress bar to enter activity
         --  mode.
         Pulse (Pdata.Pbar);
      else
         --  Calling Set_Fraction should cause the progress bar to enter
         --  normal "fill in" mode.
         Set_Fraction (Pdata.Pbar, 0.0);
      end if;
      Set_Sensitive (Pdata.Step_Spin, Get_Active (Widget));
      Set_Sensitive (Pdata.Act_Blocks_Spin, Get_Active (Widget));
   end Toggle_Activity_Mode;

   -------------------
   -- Entry_Changed --
   -------------------

   procedure Entry_Changed (Widget : access Gtk_Widget_Record'Class) is
      pragma Warnings (Off, Widget);
   begin
      --  ??? will text with control characters display as expected?
      Set_Text (Pdata.Pbar, Get_Text (Pdata.Gentry));
   end Entry_Changed;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
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

      Widget_Handler.Connect
        (Vbox, "destroy",
         Widget_Handler.To_Marshaller (Destroy_Progress'Access));

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

      Gtk_New (Pdata.Pbar);
      --  ??? will text with control characters display as expected?
      Set_Text (Pdata.Pbar, "%v from [%l,%u] (=%p%%)");
      Add (Align, Pdata.Pbar);

      Pdata.Timer := Time_Cb.Timeout_Add
        (100, Progress_Timeout'Access, Pdata.Pbar);

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

      --  Orientation

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

      --  Inversion

      Gtk_New (Label, "Inversion :");
      Attach (Tab, Label, 0, 1, 1, 2, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);
      Set_Alignment (Label, 0.0, 0.5);

      Build_Option_Menu (Pdata.Omenu2, Pdata.Omenu2_Group,
                         Items2, 0, Toggle_Inversion'Access);

      Gtk_New_Hbox (Hbox, False, 0);
      Attach (Tab, Hbox, 1, 2, 1, 2, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);
      Pack_Start (Hbox, Pdata.Omenu2, True, True, 0);

      --  Show Text

      Gtk_New (Check, "Show Text");
      Check_Handler.Connect
        (Check, "clicked",
         Check_Handler.To_Marshaller (Toggle_Show_Text'Access));
      Attach (Tab, Check, 0, 1, 2, 3, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);

      Gtk_New_Hbox (Hbox, False, 0);
      Attach (Tab, Hbox, 1, 2, 2, 3, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);

      --  Format

      Gtk_New (Label, "Format : ");
      Pack_Start (Hbox, Label, False, True, 0);

      Gtk_New (Pdata.Gentry);
      Widget_Handler.Object_Connect
        (Pdata.Gentry, "changed",
         Widget_Handler.To_Marshaller (Entry_Changed'Access),
         Slot_Object => Pdata.Gentry);
      Pack_Start (Hbox, Pdata.Gentry, True, True, 0);
      Set_Text (Pdata.Gentry, "%v from [%l,%u] (=%p%%)");
      Set_USize (Pdata.Gentry, 100, -1);
      Set_Sensitive (Pdata.Gentry, False);

      --  Block Count

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

      --  Activity Mode

      Gtk_New (Check, "Activity mode");
      Check_Handler.Connect
        (Check, "clicked",
         Check_Handler.To_Marshaller (Toggle_Activity_Mode'Access));
      Attach (Tab, Check, 0, 1, 5, 6, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);

      Gtk_New_Hbox (Hbox, False, 0);
      Attach (Tab, Hbox, 1, 2, 5, 6, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);

      --  Step Size

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

      Gtk_New_Hbox (Hbox, False, 0);
      Attach (Tab, Hbox, 1, 2, 6, 7, Enums.Expand or Enums.Fill,
              Enums.Expand or Enums.Fill, 5, 5);

      --  Blocks

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

      Show_All (Vbox);
   end Run;

end Create_Progress;
