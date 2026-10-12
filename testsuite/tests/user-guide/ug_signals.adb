package body UG_Signals is

   --  START handler
   procedure Handler (Button : access Gtk_Button_Record'Class) is
   begin
      Clicks_On_Button := Clicks_On_Button + 1;
   end Handler;
   --  END handler

   --  START slot_handler
   procedure Window_Handler (Win : access GObject_Record'Class) is
   begin
      Clicks_On_Window := Clicks_On_Window + 1;
   end Window_Handler;
   --  END slot_handler

   ---------------------
   -- Connect_Handler --
   ---------------------

   procedure Connect_Handler (Button : Gtk_Button) is
   begin
      --  START connect
      Button.On_Clicked (Handler'Access);
      --  END connect
   end Connect_Handler;

   --------------------------
   -- Connect_Slot_Handler --
   --------------------------

   procedure Connect_Slot_Handler
     (Button : Gtk_Button; Main_Window : Gtk_Window) is
   begin
      --  START connect_slot
      Button.On_Clicked (Window_Handler'Access, Slot => Main_Window);
      --  END connect_slot
   end Connect_Slot_Handler;

end UG_Signals;
