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

with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Gtk.Adjustment;    use Gtk.Adjustment;
with Gtk.Box;           use Gtk.Box;
with Gtk.Button;        use Gtk.Button;
with Gtk.Check_Button;  use Gtk.Check_Button;
with Gtk.Handlers;      use Gtk.Handlers;
with Gtk.Label;         use Gtk.Label;
with Gtk.Spin_Button;   use Gtk.Spin_Button;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk;               use Gtk;

package body Create_Spin is

   type My_Button_Record is new Gtk_Button_Record with record
      Label : Gtk_Label;
   end record;
   type My_Button is access all My_Button_Record;
   --  This is basic Gtk_Button, except that is has an extra internal
   --  data.

   package Spin_Cb is new Handlers.User_Callback
     (Gtk_Toggle_Button_Record, Gtk_Spin_Button);
   package Button_Cb is new Handlers.User_Callback (My_Button_Record, Gint);

   Spinner1 : Gtk_Spin_Button;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Spin_Button@B is a widget that allows the user to"
        & " choose a value from a specific range. You can only associate"
        & " the spin button with numeric values."
        & ASCII.LF
        & "This demo also creates a child of @bGtk_Button@B for the two"
        & " buttons at the bottom. These are basic buttons, but they also"
        & " contain a pointer to the label where to display the value.";
   end Help;

   -------------------
   -- Change_Digits --
   -------------------

   procedure Change_Digits (Spin : access GObject_Record'Class) is
      S : constant Gtk_Spin_Button := Gtk_Spin_Button (Spin);
   begin
      Set_Digits (Spinner1, Guint (S.Get_Value_As_Int));
   end Change_Digits;

   -----------------
   -- Toggle_Snap --
   -----------------

   procedure Toggle_Snap
      (Widget : access Gtk_Toggle_Button_Record'Class;
       Spin : Gtk_Spin_Button) is
   begin
      Set_Snap_To_Ticks (Spin, Get_Active (Widget));
   end Toggle_Snap;

   --------------------
   -- Toggle_Numeric --
   --------------------

   procedure Toggle_Numeric
      (Widget : access Gtk_Toggle_Button_Record'Class;
       Spin   : Gtk_Spin_Button) is
   begin
      Set_Numeric (Spin, Get_Active (Widget));
   end Toggle_Numeric;

   ---------------
   -- Get_Value --
   ---------------

   procedure Get_Value (Widget : access My_Button_Record'Class;
                        Data   : Gint)
   is
      Spin  : constant Gtk_Spin_Button := Spinner1;
   begin
      if Data = 1 then
         Set_Text (Widget.Label, Gint'Image (Get_Value_As_Int (Spin)));
      else
         Set_Text (Widget.Label, Gdouble'Image (Get_Value (Spin)));
      end if;
   end Get_Value;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Main_Box : Gtk_Box;
      VBox     : Gtk_Box;
      Hbox     : Gtk_Box;
      Vbox2    : Gtk_Box;
      Label    : Gtk_Label;
      Adj      : Gtk_Adjustment;
      Spinner  : Gtk_Spin_Button;
      Spinner2 : Gtk_Spin_Button;
      Frame2   : Gtk_Frame;
      Check    : Gtk_Check_Button;
      Myb      : My_Button;

   begin
      Set_Label (Frame, "Spin Buttons");

      Gtk_New_Vbox (Main_Box, False, 5);
      Set_Border_Width (Main_Box, 10);
      Add (Frame, Main_Box);

      Gtk_New (Frame2, "Not accelerated");
      Pack_Start (Main_Box, Frame2, False, False, 0);

      Gtk_New_Vbox (VBox, False, 0);
      Set_Border_Width (VBox, 5);
      Add (Frame2, VBox);

      --  Day, month, year spinners
      Gtk_New_Hbox (Hbox, False, 0);
      Pack_Start (VBox, Hbox, False, False, 5);

      Gtk_New_Vbox (Vbox2, False, 0);
      Pack_Start (Hbox, Vbox2, False, False, 5);
      Gtk_New (Label, "Day:");
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Vbox2, Label, False, False, 0);
      Gtk_New (Adj, 1.0, 1.0, 31.0, 1.0, 5.0, 0.0);
      Gtk_New (Spinner, Adj, 0.0, 0);
      Set_Wrap (Spinner, True);
      Pack_Start (Vbox2, Spinner, False, False, 0);

      Gtk_New_Vbox (Vbox2, False, 0);
      Pack_Start (Hbox, Vbox2, False, False, 5);
      Gtk_New (Label, "Month:");
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Vbox2, Label, False, False, 0);
      Gtk_New (Adj, 1.0, 1.0, 12.0, 1.0, 5.0, 0.0);
      Gtk_New (Spinner, Adj, 0.0, 0);
      Set_Wrap (Spinner, True);
      Pack_Start (Vbox2, Spinner, False, False, 0);

      Gtk_New_Vbox (Vbox2, False, 0);
      Pack_Start (Hbox, Vbox2, False, False, 5);
      Gtk_New (Label, "Year:");
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Vbox2, Label, False, False, 0);
      Gtk_New (Adj, 1998.0, 0.0, 2100.0, 1.0, 100.0, 0.0);
      Gtk_New (Spinner, Adj, 0.0, 0);
      Set_Wrap (Spinner, True);
      Set_Size_Request (Spinner, 55, 0);
      Pack_Start (Vbox2, Spinner, False, False, 0);

      Gtk_New (Frame2, "Accelerated");
      Pack_Start (Main_Box, Frame2, False, False, 0);

      Gtk_New_Vbox (VBox, False, 0);
      Set_Border_Width (VBox, 5);
      Add (Frame2, VBox);

      Gtk_New_Hbox (Hbox, False, 0);
      Pack_Start (VBox, Hbox, False, False, 5);

      Gtk_New_Vbox (Vbox2, False, 0);
      Pack_Start (Hbox, Vbox2, False, False, 5);
      Gtk_New (Label, "Value:");
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Vbox2, Label, False, False, 0);
      Gtk_New (Adj, 0.0, -10000.0, 10000.0, 0.5, 100.0, 0.0);
      Gtk_New (Spinner1, Adj, 1.0, 2);
      Set_Wrap (Spinner1, True);
      Set_Size_Request (Spinner1, 100, 0);
      Set_Update_Policy (Spinner1, Update_Always);
      Pack_Start (Vbox2, Spinner1, False, False, 0);

      Gtk_New_Vbox (Vbox2, False, 0);
      Pack_Start (Hbox, Vbox2, False, False, 5);
      Gtk_New (Label, "Digits:");
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Vbox2, Label, False, False, 0);
      Gtk_New (Adj, 2.0, 1.0, 5.0, 1.0, 1.0, 0.0);
      Gtk_New (Spinner2, Adj, 0.0, 0);
      Set_Wrap (Spinner2, True);
      Adj.On_Value_Changed (Change_Digits'Access, Spinner2);

      Pack_Start (Vbox2, Spinner2, False, False, 0);

      Gtk_New_Hbox (Hbox, False, 0);
      Pack_Start (VBox, Hbox, False, False, 5);

      Gtk_New (Check, "Snap to 0.5-ticks");
      Spin_Cb.Connect (Check, "clicked", Toggle_Snap'Access, Spinner1);
      Pack_Start (VBox, Check, False, False, 0);
      Set_Active (Check, True);

      Gtk_New (Check, "Snap Numeric only input mode");
      Spin_Cb.Connect (Check, "clicked", Toggle_Numeric'Access, Spinner1);
      Pack_Start (VBox, Check, False, False, 0);
      Set_Active (Check, True);

      Gtk_New (Label, "");
      Gtk_New_Hbox (Hbox, False, 0);
      Pack_Start (VBox, Hbox, False, False, 5);

      Myb := new My_Button_Record;
      Initialize (Myb, "Value as Int");
      Myb.Label := Label;
      Button_Cb.Connect (Myb, "clicked", Get_Value'Access, 1);
      Pack_Start (Hbox, Myb, False, False, 5);

      Myb := new My_Button_Record;
      Initialize (Myb, "Value as Float");
      Myb.Label := Label;
      Button_Cb.Connect (Myb, "clicked", Get_Value'Access, 2);
      Pack_Start (Hbox, Myb, False, False, 5);

      Pack_Start (VBox, Label, False, False, 0);
      Set_Text (Label, "0");

      Show_All (Frame);
   end Run;

end Create_Spin;
