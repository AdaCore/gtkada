------------------------------------------------------------------------------
--               GtkAda - Ada12 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2026, AdaCore                     --
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
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Label;         use Gtk.Label;
with Gtk.Spin_Button;   use Gtk.Spin_Button;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk;               use Gtk;

package body Create_Spin is

   type My_Button_Record is new Gtk_Check_Button_Record with record
      Label : Gtk_Label;
      Data : Glib.Gint;
   end record;
   type My_Button is access all My_Button_Record;
   --  This is a basic Gtk_Check_Button with extra internal data.

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
     (Toggle : access Gtk_Check_Button_Record'Class)
   is
   begin
      Set_Snap_To_Ticks (Spinner1, Toggle.Get_Active);
   end Toggle_Snap;

   --------------------
   -- Toggle_Numeric --
   --------------------

   procedure Toggle_Numeric
     (Toggle : access Gtk_Check_Button_Record'Class)
   is
   begin
      Set_Numeric (Spinner1, Toggle.Get_Active);
   end Toggle_Numeric;

   ---------------
   -- Get_Value --
   ---------------

   procedure Get_Value (Widget : access GObject_Record'Class)
   with Pre => Widget /= null and then Widget.all in My_Button_Record'Class;
   procedure Get_Value (Widget : access GObject_Record'Class)
   is
      Spin  : constant Gtk_Spin_Button := Spinner1;
      My_B  : My_Button_Record'Class := My_Button_Record (Widget.all);
   begin
      if My_B.Data = 1 then
         My_B.Label.Set_Text (Gint'Image (Get_Value_As_Int (Spin)));
      else
         My_B.Label.Set_Text (Gdouble'Image (Get_Value (Spin)));
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

      Gtk_New (Main_Box, Orientation_Vertical, 5);
      Main_Box.Set_Homogeneous (False);
      Main_Box.Set_Margin_Start (10);
      Main_Box.Set_Margin_End (10);
      Main_Box.Set_Margin_Top (10);
      Main_Box.Set_Margin_Bottom (10);
      Frame.Set_Child (Main_Box);

      Gtk_New (Frame2, "Not accelerated");
      Main_Box.Append (Frame2);

      Gtk_New (VBox, Orientation_Vertical, 0);
      VBox.Set_Margin_Start (5);
      VBox.Set_Margin_End (5);
      VBox.Set_Margin_Top (5);
      VBox.Set_Margin_Bottom (5);
      Frame2.Set_Child (VBox);

      --  Day, month, year spinners
      Gtk_New (Hbox, Orientation_Horizontal, 5);
      VBox.Append (Hbox);

      Gtk_New (Vbox2, Orientation_Vertical, 5);
      Hbox.Append (Vbox2);
      Gtk_New (Label, "Day:");
      Label.Set_Yalign (0.5);
      Vbox2.Append (Label);
      Gtk_New (Adj, 1.0, 1.0, 31.0, 1.0, 5.0, 0.0);
      Gtk_New (Spinner, Adj, 0.0, 0);
      Set_Wrap (Spinner, True);
      Vbox2.Append (Spinner);

      Gtk_New (Vbox2, Orientation_Vertical, 5);
      Hbox.Append (Vbox2);
      Gtk_New (Label, "Month:");
      Label.Set_Yalign (0.5);
      Vbox2.Append (Label);
      Gtk_New (Adj, 1.0, 1.0, 12.0, 1.0, 5.0, 0.0);
      Gtk_New (Spinner, Adj, 0.0, 0);
      Set_Wrap (Spinner, True);
      Vbox2.Append (Spinner);

      Gtk_New (Vbox2, Orientation_Vertical, 5);
      Hbox.Append (Vbox2);
      Gtk_New (Label, "Year:");
      Label.Set_Yalign (0.5);
      Vbox2.Append (Label);
      Gtk_New (Adj, 1998.0, 0.0, 2100.0, 1.0, 100.0, 0.0);
      Gtk_New (Spinner, Adj, 0.0, 0);
      Set_Wrap (Spinner, True);
      Set_Size_Request (Spinner, 55, 0);
      Vbox2.Append (Spinner);

      Gtk_New (Frame2, "Accelerated");
      Main_Box.Append (Frame2);

      Gtk_New (VBox, Orientation_Vertical, 0);
      Vbox.Set_Margin_Start (5);
      Vbox.Set_Margin_End (5);
      Vbox.Set_Margin_Top (5);
      Vbox.Set_Margin_Bottom (5);
      Frame2.Set_Child (VBox);

      Gtk_New (Hbox, Orientation_Horizontal, 5);
      VBox.Append (Hbox);

      Gtk_New (Vbox2, Orientation_Vertical, 5);
      Hbox.Append (Vbox2);
      Gtk_New (Label, "Value:");
      Label.Set_Yalign (0.5);
      Vbox2.Append (Label);
      Gtk_New (Adj, 0.0, -10000.0, 10000.0, 0.5, 100.0, 0.0);
      Gtk_New (Spinner1, Adj, 1.0, 2);
      Set_Wrap (Spinner1, True);
      Set_Size_Request (Spinner1, 100, 0);
      Set_Update_Policy (Spinner1, Update_Always);
      Vbox2.Append (Spinner1);

      Gtk_New (Vbox2, Orientation_Vertical, 5);
      Hbox.Append (Vbox2);
      Gtk_New (Label, "Digits:");
      Label.Set_Yalign (0.5);
      Vbox2.Append (Label);
      Gtk_New (Adj, 2.0, 1.0, 5.0, 1.0, 1.0, 0.0);
      Gtk_New (Spinner2, Adj, 0.0, 0);
      Set_Wrap (Spinner2, True);
      Adj.On_Value_Changed (Change_Digits'Access, Spinner2);

      Vbox2.Append (Spinner2);

      Gtk_New (Hbox, Orientation_Horizontal, 5);
      VBox.Append (Hbox);

      Gtk_New_With_Label (Check, "Snap to 0.5-ticks");
      Check.On_Toggled (Toggle_Snap'Access);
      VBox.Append (Check);
      Set_Active (Check, True);

      Gtk_New_With_Label (Check, "Snap Numeric only input mode");
      Check.On_Toggled (Toggle_Numeric'Access);
      VBox.Append (Check);
      Set_Active (Check, True);

      Gtk_New (Label, "");
      Gtk_New (Hbox, Orientation_Horizontal, 5);
      VBox.Append (Hbox);

      Myb := new My_Button_Record;
      Gtk.Check_Button.Initialize_With_Label (Myb, "Value as Int");
      Myb.Label := Label;
      Myb.Data := 1;
      Myb.On_Toggled (Get_Value'Access, Slot => Myb);
      Hbox.Append (Myb);

      Myb := new My_Button_Record;
      Gtk.Check_Button.Initialize_With_Label (Myb, "Value as Float");
      Myb.Label := Label;
      Myb.Data := 2;
      Myb.On_Toggled (Get_Value'Access, Slot => Myb);
      Hbox.Append (Myb);

      VBox.Append (Label);
      Label.Set_Text ("0");
   end Run;

end Create_Spin;
