
package body Gtk.Radio_Button is

   -----------------------
   --  New_From_Widget  --
   -----------------------

   procedure New_From_Widget
     (Widget : out Gtk_Radio_Button;
      Group : in Gtk_Radio_Button := Null_Gtk_Radio_Button) is
      function Internal (Group : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_radio_button_new_from_widget");
   begin
      Set_Object (Widget, Internal (Get_Object (Group)));
   end New_From_Widget;


   -----------------------
   --  New_From_Widget  --
   -----------------------

   procedure New_From_Widget
     (Widget : out Gtk_Radio_Button;
      With_Label : in String;
      Group : in Gtk_Radio_Button := Null_Gtk_Radio_Button) is
      function Internal (Group : in System.Address;
                         Label : in String) return System.Address;
      pragma Import (C, Internal,
                     "gtk_radio_button_new_with_label_from_widget");
   begin
      Set_Object (Widget, Internal (Get_Object (Group),
                                    With_Label & ASCII.NUL));
   end New_From_Widget;


   -----------------------------
   --  Null_Gtk_Radio_Button  --
   -----------------------------

   function Null_Gtk_Radio_Button return Gtk_Radio_Button is
      Result : Gtk_Radio_Button;
   begin
      Set_Object (Result, System.Null_Address);
      return Result;
   end Null_Gtk_Radio_Button;


end Gtk.Radio_Button;
