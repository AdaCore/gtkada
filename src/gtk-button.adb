
package body Gtk.Button is

   ---------------
   --  Clicked  --
   ---------------

   procedure Clicked (Widget : in Gtk_Button'Class) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_clicked");
   begin
      Internal (Get_Object (Widget));
   end Clicked;


   -------------
   --  Enter  --
   -------------

   procedure Enter (Widget : in Gtk_Button'Class) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_enter");
   begin
      Internal (Get_Object (Widget));
   end Enter;


   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Widget : out Gtk_Button) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_button_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Widget : out Gtk_Button;
                      With_Label  : in String) is
      function Internal (S : String) return System.Address;
      pragma Import (C, Internal, "gtk_button_new_with_label");
   begin
      Set_Object (Widget, Internal (With_Label & Ascii.NUL));
   end Gtk_New;

   -------------
   --  Leave  --
   -------------
   procedure Leave (Widget : in Gtk_Button'Class) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_enter");
   begin
      Internal (Get_Object (Widget));
   end Leave;


   ---------------
   --  Pressed  --
   ---------------

   procedure Pressed (Widget : in Gtk_Button'Class) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_pressed");
   begin
      Internal (Get_Object (Widget));
   end Pressed;


   ----------------
   --  Released  --
   ----------------

   procedure Released (Widget : in Gtk_Button'Class) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_released");
   begin
      Internal (Get_Object (Widget));
   end Released;


end Gtk.Button;







