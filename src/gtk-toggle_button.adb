
package body Gtk.Toggle_Button is

   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Widget : out Gtk_Toggle_Button) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_toggle_button_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;


   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Widget : out Gtk_Toggle_Button;
                         With_Label : in String) is
      function Internal (S : in String) return System.Address;
      pragma Import (C, Internal, "gtk_toggle_button_new_with_label");
   begin
      Set_Object (Widget, Internal (With_Label & ASCII.NUL));
   end Gtk_New;


   -------------------
   --  Is_Selected  --
   -------------------

   function Is_Selected (Widget : in Gtk_Toggle_Button'Class) return Boolean is
      function Internal (W : in System.Address) return Gint;
      pragma Import (C, Internal, "get_state");
   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Is_Selected;


   ----------------
   --  Set_Mode  --
   ----------------

   procedure Set_Mode (Widget : in out Gtk_Toggle_Button'Class;
                       New_Mode : in Toggle_Button_Mode) is
      procedure Internal (W : in System.Address;
                          M : in Gint);
      pragma Import (C, Internal, "gtk_toggle_button_set_mode");
   begin

      Internal (Get_Object (Widget), To_Gint (New_Mode = Radio_Style));
      --
      --  The mode inside gtk is equal to (New_Mode = Radio_Button) because :
      --    + To have a Radio_Button, we have to set the mode to True
      --    + To have a Classic_Button, we have to set the mode to False
   end Set_Mode;


   -----------------
   --  Set_state  --
   -----------------

   procedure Set_State (Widget : in out Gtk_Toggle_Button'Class;
                        To_Selected : in Boolean) is
      procedure Internal (W : in System.Address; S : in Gint);
      pragma Import (C, Internal, "gtk_toggle_button_set_state");
   begin
      Internal (Get_Object (Widget), To_Gint (To_Selected));
   end Set_State;


   ---------------
   --  Toggled  --
   ---------------

   procedure Toggled (Widget : in out Gtk_Toggle_Button'Class) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_toggle_button_toggled");
   begin
      Internal (Get_Object (Widget));
   end Toggled;

end Gtk.Toggle_Button;
