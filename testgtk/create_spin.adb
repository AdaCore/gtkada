with Glib; use Glib;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Label; use Gtk.Label;
with Gtk.Misc; use Gtk.Misc;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Object; use Gtk.Object;
with Gtk.Spin_Button; use Gtk.Spin_Button;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;

package body Create_Spin is

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);
   package Label_User is new User_Data (Gtk_Label);
   package Spin_O_Cb is new Signal.Object_Callback (Gtk_Spin_Button);
   package Spin_Cb is new Signal.Callback (Gtk_Toggle_Button,
                                           Gtk_Spin_Button);
   package Button_Cb is new Signal.Callback (Gtk_Button, Gint);

   Window   : Gtk.Window.Gtk_Window;
   Spinner1 : Gtk_Spin_Button;

   procedure Change_Digits (Spin : in out Gtk_Spin_Button'Class) is
   begin
      Set_Digits (Spinner1, Get_Value_As_Int (Spin));
   end Change_Digits;

   procedure Toggle_Snap (Widget : in out Gtk_Toggle_Button'Class;
                          Spin : in out Gtk_Spin_Button) is
   begin
      if Is_Active (Widget) then
         Set_Update_Policy (Spin, Update_Always + Update_Snap_To_Ticks);
      else
         Set_Update_Policy (Spin, Update_Always);
      end if;
   end Toggle_Snap;

   procedure Toggle_Numeric (Widget : in out Gtk_Toggle_Button'Class;
                             Spin : in out Gtk_Spin_Button) is
   begin
      Set_Numeric (Spin, Is_Active (Widget));
   end Toggle_Numeric;

   procedure Get_Value (Widget : in out Gtk_Button'Class;
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

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
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
         Id := Widget_Cb.Connect (Window, "destroy", Destroy'Access, Window);
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

--          Gtk_New_Vbox (Vbox2, False, 0);
--          Pack_Start (Hbox, Vbox2, True, True, 5);
--          Gtk_New (Label, "Day:");
--          Set_Alignment (Label, 0.0, 0.5);
--          Pack_Start (Vbox2, Label, False, True, 0);
--          Gtk_New (Adj, 1.0, 1.0, 31.0, 1.0, 5.0, 0.0);
--          Gtk_New (Spinner, Adj, 0.0, 0);
--          Set_Wrap (Spinner, True);
--          Pack_Start (Vbox2, Spinner, False, True, 0);

--          Gtk_New_Vbox (Vbox2, False, 0);
--          Pack_Start (Hbox, Vbox2, True, True, 5);
--          Gtk_New (Label, "Month:");
--          Set_Alignment (Label, 0.0, 0.5);
--          Pack_Start (Vbox2, Label, False, True, 0);
--          Gtk_New (Adj, 1.0, 1.0, 12.0, 1.0, 5.0, 0.0);
--          Gtk_New (Spinner, Adj, 0.0, 0);
--          Set_Wrap (Spinner, True);
--          Pack_Start (Vbox2, Spinner, False, True, 0);

--          Gtk_New_Vbox (Vbox2, False, 0);
--          Pack_Start (Hbox, Vbox2, True, True, 5);
--          Gtk_New (Label, "Year:");
--          Set_Alignment (Label, 0.0, 0.5);
--          Pack_Start (Vbox2, Label, False, True, 0);
--          Gtk_New (Adj, 1998.0, 0.0, 2100.0, 1.0, 100.0, 0.0);
--          Gtk_New (Spinner, Adj, 0.0, 0);
--          Set_Wrap (Spinner, True);
--          Set_Usize (Spinner, 55, 0);
--          Pack_Start (Vbox2, Spinner, False, True, 0);

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
--         Unref (Spinner2);
         --  FIXME : this one is a little bit tricky : Adj will not be
         --  destroyed till Spinner2 is destroyed.
         --  And the previous callback will not be removed till Adj is
         --  destroyed.
         --  Finally, SPinner2 will not be destroyed while the callback
         --  exists, as there is a reference from the callback to Spinner2.
         --  So we have a circular depency here, that we can only break with
         --  the 'Unref' statement above.
         --  Actually, this problem has been solved directly in gtk-signal.adb,
         --  so that we do not keep a reference on Slot_Object

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
         Gtk.Widget.Show_All (Window);
      else
         Gtk.Widget.Destroy (Window);
      end if;

   end Run;

end Create_Spin;










