with Ada.Command_Line;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Test; use Glib.Test;
with Gtk.Button; use Gtk.Button;
with Gtk.Info_Bar; use Gtk.Info_Bar;
with Gtk.Label; use Gtk.Label;
with Gtk.Main;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Interfaces.C.Strings;
with System;

procedure Info_Bar is
   Seen : Gint := 0;
   Responses, Closes : Natural := 0;

   procedure Emit
     (Object : System.Address; Name : Interfaces.C.Strings.chars_ptr);
   pragma Import (C_Variadic_2, Emit, "g_signal_emit_by_name");

   procedure Click (Button : not null access Gtk_Widget_Record'Class) is
      Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("clicked");
   begin
      Emit (Get_Object (Button), Name);
      Interfaces.C.Strings.Free (Name);
   end Click;

   procedure On_Response
     (Self : access Gtk_Info_Bar_Record'Class; Response_Id : Gint)
   is
      pragma Unreferenced (Self);
   begin
      Seen := Response_Id;
      Responses := Responses + 1;
   end On_Response;

   procedure On_Close (Self : access Gtk_Info_Bar_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Closes := Closes + 1;
   end On_Close;

   procedure Test_Properties with Convention => C;
   procedure Test_Children with Convention => C;
   procedure Test_Actions with Convention => C;
   procedure Test_Signals with Convention => C;

   procedure Test_Properties is
      Bar : Gtk_Info_Bar;
   begin
      Gtk_New (Bar);
      Ref_Sink (Bar);
      Assert_True (Bar.Get_Message_Type = Message_Info);
      Assert_True (Bar.Get_Revealed);
      Assert_False (Bar.Get_Show_Close_Button);
      for Kind in Gtk_Message_Type loop
         Bar.Set_Message_Type (Kind);
         Assert_True (Get_Property (Bar, Gtk.Info_Bar.Message_Type_Property) = Kind);
         Set_Property (Bar, Gtk.Info_Bar.Message_Type_Property, Kind);
         Assert_True (Bar.Get_Message_Type = Kind);
      end loop;
      Bar.Set_Revealed (False);
      Assert_False (Get_Property (Bar, Revealed_Property));
      Set_Property (Bar, Revealed_Property, True);
      Assert_True (Bar.Get_Revealed);
      Bar.Set_Show_Close_Button (True);
      Assert_True (Get_Property (Bar, Show_Close_Button_Property));
      Set_Property (Bar, Show_Close_Button_Property, False);
      Assert_False (Bar.Get_Show_Close_Button);
      Unref (Bar);
   end Test_Properties;

   procedure Test_Children is
      Bar : constant Gtk_Info_Bar := Gtk_Info_Bar_New;
      Label : constant Gtk_Label := Gtk_Label_New ("Résumé — café");
   begin
      Ref_Sink (Bar);
      Ref_Sink (Label);
      Bar.Add_Child (Label);
      Assert_True (Label.Get_Parent /= null);
      Assert_True (Label.Is_Ancestor (Bar));
      Assert_Cmpstr_Eq (Label.Get_Text, "Résumé — café");
      Bar.Remove_Child (Label);
      Assert_True (Label.Get_Parent = null);
      Bar.Add_Child (Label);
      Unref (Label);
      Unref (Bar);
   end Test_Children;

   procedure Test_Actions is
      Win : constant Gtk_Window := Gtk_Window_New;
      Bar : constant Gtk_Info_Bar := Gtk_Info_Bar_New;
      First, Second : Gtk_Widget;
      Custom : constant Gtk_Button := Gtk_Button_New_With_Label ("Custom");
   begin
      Responses := 0;
      Win.Set_Child (Bar);
      First := Bar.Add_Button ("_Continue", 42);
      Second := Bar.Add_Button ("_Again", 42);
      Ref_Sink (Custom);
      Bar.Add_Action_Widget (Custom, -6);
      Bar.On_Response (On_Response'Unrestricted_Access);
      Bar.Set_Response_Sensitive (42, False);
      Assert_False (First.Get_Sensitive);
      Assert_False (Second.Get_Sensitive);
      Assert_True (Custom.Get_Sensitive);
      Bar.Set_Response_Sensitive (42, True);
      Assert_True (First.Get_Sensitive);
      Assert_True (Second.Get_Sensitive);
      Bar.Set_Default_Response (42);
      --  GTK selects the first action matching the response id.
      Assert_True (Win.Get_Default_Widget = First);
      Click (First);
      Assert_True (Seen = 42 and Responses = 1);
      Click (Custom);
      Assert_True (Seen = -6 and Responses = 2);
      Bar.Remove_Action_Widget (Custom);
      Assert_True (Custom.Get_Parent = null);
      --  Removing an action must also disconnect its response handler.
      Click (Custom);
      Assert_True (Responses = 2);
      Bar.Add_Action_Widget (Custom, 73);
      Click (Custom);
      Assert_True (Seen = 73 and Responses = 3);
      Unref (Custom);
      Win.Destroy;
   end Test_Actions;

   procedure Test_Signals is
      Bar : constant Gtk_Info_Bar := Gtk_Info_Bar_New;
      Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("close");
   begin
      Ref_Sink (Bar);
      Responses := 0;
      Closes := 0;
      Bar.On_Response (On_Response'Unrestricted_Access);
      Bar.On_Close (On_Close'Unrestricted_Access);
      Bar.Response (-7);
      Assert_True (Seen = -7 and Responses = 1);
      Bar.Response (123);
      Assert_True (Seen = 123 and Responses = 2);
      Bar.Set_Show_Close_Button (True);
      Emit (Get_Object (Bar), Name);
      Assert_True (Closes = 1);
      Assert_True (Seen = -6 and Responses = 3);
      Interfaces.C.Strings.Free (Name);
      Unref (Bar);
   end Test_Signals;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/infobar/properties", Test_Properties'Unrestricted_Access);
   Add_Func ("/infobar/children", Test_Children'Unrestricted_Access);
   Add_Func ("/infobar/actions", Test_Actions'Unrestricted_Access);
   Add_Func ("/infobar/signals", Test_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Info_Bar;
