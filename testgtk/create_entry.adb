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

with Glib;             use Glib;
with Glib.Main;        use Glib.Main;
with Gtk.Box;          use Gtk.Box;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.GEntry;       use Gtk.GEntry;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Level_Bar;    use Gtk.Level_Bar;
with Gtk.Handlers;     use Gtk.Handlers;
with Gtk.Search_Entry; use Gtk.Search_Entry;
with Gtk.Separator;    use Gtk.Separator;
with Gtk.Widget;       use Gtk.Widget;
with Gtk;              use Gtk;

with Common;           use Common;

package body Create_Entry is

   package Entry_Cb is new Handlers.User_Callback
     (Gtk_Check_Button_Record, Gtk_Entry);

   package Time_Cb is new Glib.Main.Generic_Sources (Gtk_Entry);

   Timer1, Timer2 : G_Source_Id;
   --  This is stored at the library level so that the On_Destroy callback
   --  can refer to the results from the calls to Time_Cb.Timeout_Add in the
   --  Run subprogram.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo demonstrates several types of widgets.  The first"
        & " couple of @bGtk_Entry@B widgets demonstrate different kinds of"
        & " progress bars that may be embedded.  Then, there's a simple"
        & " @bGtk_Entry@B with user-selectable options.  Finally, the last"
        & " widget is a @bGtk_Combo@B that adds a window to facilitate the"
        & " insertion of the text.";
   end Help;

   ---------------------
   -- Toggle_Editable --
   ---------------------

   procedure Toggle_Editable
     (Button    : access Gtk_Check_Button_Record'Class;
      The_Entry : Gtk_Entry) is
   begin
      The_Entry.Set_Editable (Button.Get_Active);
   end Toggle_Editable;

   ----------------------
   -- Toggle_Overwrite --
   ----------------------

   procedure Toggle_Overwrite
     (Button    : access Gtk_Check_Button_Record'Class;
      The_Entry : Gtk_Entry)
   is
   begin
      The_Entry.Set_Overwrite_Mode (Button.Get_Active);
   end Toggle_Overwrite;

   ----------------------
   -- Toggle_Sensitive --
   ----------------------

   procedure Toggle_Sensitive
     (Button    : access Gtk_Check_Button_Record'Class;
      The_Entry : Gtk_Entry)
   is
   begin
      The_Entry.Set_Sensitive (Button.Get_Active);
   end Toggle_Sensitive;

   -----------------------
   -- Toggle_Visibility --
   -----------------------

   procedure Toggle_Visibility
     (Button    : access Gtk_Check_Button_Record'Class;
      The_Entry : Gtk_Entry)
   is
   begin
      The_Entry.Set_Visibility (Button.Get_Active);
   end Toggle_Visibility;

   -------------------
   -- Pulse_Timeout --
   -------------------

   function Pulse_Timeout (The_Entry : Gtk_Entry) return Boolean is
   begin
      The_Entry.Progress_Pulse;
      return True;
   end Pulse_Timeout;

   ------------------------
   -- Fractional_Timeout --
   ------------------------

   function Fractional_Timeout (The_Entry : Gtk_Entry) return Boolean is
      Progress : Gdouble := Get_Progress_Fraction (The_Entry);
   begin
      Progress := Progress + 0.005;
      if Progress > 1.0 then
         Progress := 0.0;
      end if;

      The_Entry.Set_Progress_Fraction (Progress);
      return True;
   end Fractional_Timeout;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Window : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Window);
   begin
      Remove (Timer1);
      Remove (Timer2);
   end On_Destroy;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      use String_List;

      Box1, Box2 : Gtk_Box;
      The_Entry  : Gtk_Entry;
      Combo      : Gtk_Combo_Box_Text;
      Check      : Gtk_Check_Button;
      Hsep       : Gtk_Hseparator;
      Search     : Gtk_Search_Entry;
      Level      : Gtk_Level_Bar;

   begin
      Set_Label (Frame, "Entry");

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Widget_Handler.Connect (Box1, "destroy", On_Destroy'Access);

      Gtk_New_Vbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, False, False, 0);

      Gtk_New (The_Entry);
      Set_Text (The_Entry, "Pulsed Progress");
      Set_Sensitive (The_Entry, False);
      Set_Editable (The_Entry, False);
      Timer1 := Time_Cb.Timeout_Add
        (100, Pulse_Timeout'Access, The_Entry);
      Pack_Start (Box2, The_Entry, True, True, 0);

      Gtk_New (The_Entry);
      Set_Text (The_Entry, "Fractional Progress");
      Set_Sensitive (The_Entry, False);
      Set_Editable (The_Entry, False);
      Timer2 := Time_Cb.Timeout_Add
        (20, Fractional_Timeout'Access, The_Entry);
      Pack_Start (Box2, The_Entry, True, True, 0);

      Gtk_New_Hseparator (Hsep);
      Pack_Start (Box2, Hsep);

      Gtk_New (The_Entry);
      Set_Text (The_Entry, "Hello world");
      Pack_Start (Box2, The_Entry, True, True, 0);

      Gtk_New (Check, "Editable");
      Pack_Start (Box2, Check, False, True, 0);
      Entry_Cb.Connect (Check, "toggled", Toggle_Editable'Access, The_Entry);
      Set_Active (Check, True);

      Gtk_New (Check, "Overwrite");
      Pack_Start (Box2, Check, False, True, 0);
      Entry_Cb.Connect (Check, "toggled", Toggle_Overwrite'Access, The_Entry);
      Set_Active (Check, False);

      Gtk_New (Check, "Visible");
      Pack_Start (Box2, Check, False, True, 0);
      Entry_Cb.Connect (Check, "toggled", Toggle_Visibility'Access, The_Entry);
      Set_Active (Check, True);

      Gtk_New (Check, "Sensitive");
      Pack_Start (Box2, Check, False, True, 0);
      Entry_Cb.Connect (Check, "toggled", Toggle_Sensitive'Access, The_Entry);
      Set_Active (Check, True);

      Gtk_New_Hseparator (Hsep);
      Pack_Start (Box2, Hsep);

      Gtk_New_With_Entry (Combo);
      Combo.Append_Text ("item0");
      Combo.Append_Text ("item1 item1");
      Combo.Append_Text ("item2 item2 item2");
      Combo.Append_Text ("item3 item3 item3 item3");
      Combo.Append_Text ("item4 item4 item4 item4 item4");
      Combo.Append_Text ("item5 item5 item5 item5 item5 item5");
      Combo.Append_Text ("item6 item6 item6 item6 item6");
      Set_Text (Gtk_Entry (Combo.Get_Child), "hello world");
      Pack_Start (Box2, Combo, True, True, 0);

      Gtk_New (Search);
      Search.Set_Tooltip_Text ("A Gtk_Search_Entry");
      Box2.Pack_Start (Search, False, False, 0);

      Gtk_New (Level);
      Level.Set_Min_Value (0.0);
      Level.Set_Max_Value (10.0);
      Level.Set_Value (5.0);
      Box2.Pack_Start (Level, False, False, 0);
      Level.Set_Tooltip_Text ("A Gtk_Level_Bar");

      Show_All (Frame);
   end Run;

end Create_Entry;
