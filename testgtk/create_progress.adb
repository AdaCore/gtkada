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

with Glib;                use Glib;
with Gtk.Adjustment;      use Gtk.Adjustment;
with Gtk.Alignment;       use Gtk.Alignment;
with Gtk.Box;             use Gtk.Box;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Label;           use Gtk.Label;
with Glib.Main;           use Glib.Main;
with Gtk.Combo_Box_Text;  use Gtk.Combo_Box_Text;
with Gtk.Progress_Bar;    use Gtk.Progress_Bar;
with Gtk.Spin_Button;     use Gtk.Spin_Button;
with Gtk.Grid;            use Gtk.Grid;
with Gtk.Widget;          use Gtk.Widget;
with Gtk;                 use Gtk;
with Gtkada.Handlers;     use Gtkada.Handlers;

with Common; use Common;

package body Create_Progress is

   package Time_Cb  is new Glib.Main.Generic_Sources (Gtk_Progress_Bar);

   type ProgressData is record
      Pbar            : Gtk_Progress_Bar;
      Block_Spin      : Gtk_Spin_Button;
      Label           : Gtk_Label;
      Omenu1          : Gtk_Combo_Box_Text;
      Omenu1_Group    : Widget_SList.GSlist;
      Omenu2          : Gtk_Combo_Box_Text;
      Omenu2_Group    : Widget_SList.GSlist;
      Gentry          : Gtk_Entry;
      Timer           : G_Source_Id;
      Activity        : Boolean := False;
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
      New_Val := New_Val + 0.01;
      if New_Val > 1.0 then
         New_Val := 0.0;
      end if;

      if Pdata.Activity then
         Pdata.Pbar.Pulse;
         Pdata.Label.Set_Text ("???");
      else
         Set_Fraction (Pdata.Pbar, New_Val);
         Pdata.Label.Set_Text (Integer'Image (Integer (New_Val * 100.0)));
      end if;

      return True;
   end Progress_Timeout;

   ----------------------
   -- Destroy_Progress --
   ----------------------

   procedure Destroy_Progress
      (Window : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Window);
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

   procedure Toggle_Inversion
      (Widget : access Gtk_Widget_Record'Class)
   is
      Combo : constant Gtk_Combo_Box_Text := Gtk_Combo_Box_Text (Widget);
      Current : constant UTF8_String := Combo.Get_Active_Text;
      Is_Inverted : constant Boolean := Current = "Inverted";
   begin
      Pdata.Pbar.Set_Inverted (Is_Inverted);
   end Toggle_Inversion;

   ------------------------
   -- Toggle_Orientation --
   ------------------------

   procedure Toggle_Orientation
      (Widget : access Gtk_Widget_Record'Class)
   is
      Combo : constant Gtk_Combo_Box_Text := Gtk_Combo_Box_Text (Widget);
      Current : constant UTF8_String := Combo.Get_Active_Text;
   begin
      if Current = "Horizontal" then
         Set_Orientation (Pdata.Pbar, Orientation_Horizontal);
      else
         Set_Orientation (Pdata.Pbar, Orientation_Vertical);
      end if;
   end Toggle_Orientation;

   ----------------------
   -- Toggle_Show_Text --
   ----------------------

   procedure Toggle_Show_Text
      (Widget : access Gtk_Check_Button_Record'Class)
   is
   begin
      Set_Show_Text (Progress_Bar => Pdata.Pbar,
                     Show_Text    => Get_Active (Widget));
      Set_Sensitive (Pdata.Gentry, Get_Active (Widget));
   end Toggle_Show_Text;

   --------------------------
   -- Toggle_Activity_Mode --
   --------------------------

   procedure Toggle_Activity_Mode
     (Widget : access Gtk_Check_Button_Record'Class)
   is
   begin
      Pdata.Activity := Get_Active (Widget);
   end Toggle_Activity_Mode;

   -------------------
   -- Entry_Changed --
   -------------------

   procedure Entry_Changed
      (Widget : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Widget);
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
      Tab    : Gtk_Grid;
      Check  : Gtk_Check_Button;

   begin
      Set_Label (Frame, "Progress Bar");

      Pdata.Timer := 0;

      Gtk_New_Vbox (Vbox, False, 5);
      Set_Border_Width (Vbox, 10);
      Add (Frame, Vbox);

      Widget_Handler.Connect (Vbox, "destroy", Destroy_Progress'Access);

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
      Set_Text (Pdata.Pbar, "");
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

      Gtk_New (Tab);
      Pack_Start (Vbox2, Tab, False, True, 0);

      --  Orientation

      Gtk_New (Label, "Orientation :");
      Tab.Attach (Label, 0, 0);
      Set_Alignment (Label, 0.0, 0.5);

      Gtk_New (Pdata.Omenu1);
      Pdata.Omenu1.Append_Text ("Horizontal");
      Pdata.Omenu1.Append_Text ("Vertical");
      Widget_Callback.Connect
         (Pdata.Omenu1, "changed", Toggle_Orientation'Access);

      Gtk_New_Hbox (Hbox, False, 0);
      Tab.Attach (Hbox, 1, 0);
      Pack_Start (Hbox, Pdata.Omenu1, True, True, 0);

      --  Inversion

      Gtk_New (Label, "Inversion :");
      Tab.Attach (Label, 0, 1);
      Set_Alignment (Label, 0.0, 0.5);

      Gtk_New (Pdata.Omenu2);
      Pdata.Omenu2.Append_Text ("Not Inverted");
      Pdata.Omenu2.Append_Text ("Inverted");
      Widget_Callback.Connect
         (Pdata.Omenu2, "changed", Toggle_Inversion'Access);

      Gtk_New_Hbox (Hbox, False, 0);
      Tab.Attach (Hbox, 1, 1);
      Pack_Start (Hbox, Pdata.Omenu2, True, True, 0);

      --  Show Text

      Gtk_New (Check, "Show Text");
      Check_Handler.Connect
        (Check, "clicked",
         Check_Handler.To_Marshaller (Toggle_Show_Text'Access));
      Tab.Attach (Check, 0, 2);

      Gtk_New_Hbox (Hbox, False, 0);
      Tab.Attach (Hbox, 1, 2);

      --  Format

      Gtk_New (Label, "Text (leave empty to display a percentage) : ");
      Pack_Start (Hbox, Label, False, True, 0);

      Gtk_New (Pdata.Gentry);
      Widget_Handler.Object_Connect
        (Pdata.Gentry, "changed", Entry_Changed'Access,
         Slot_Object => Pdata.Gentry);
      Pack_Start (Hbox, Pdata.Gentry, True, True, 0);
      Set_Text (Pdata.Gentry, "");
      Set_Size_Request (Pdata.Gentry, 100, -1);
      Set_Sensitive (Pdata.Gentry, False);

      --  Block Count

      Gtk_New (Label, "Block count :");
      Tab.Attach (Label, 0, 4);
      Set_Alignment (Label, 0.0, 0.5);

      Gtk_New_Hbox (Hbox, False, 0);
      Tab.Attach (Hbox, 1, 4);

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
      Check_Handler.Connect (Check, "clicked", Toggle_Activity_Mode'Access);
      Tab.Attach (Check, 0, 5);

      Show_All (Vbox);
   end Run;

end Create_Progress;
