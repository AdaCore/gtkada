package body Gdk.Event is


   ------------
   --  Copy  --
   ------------

   procedure Copy (Source : in Gdk_Event;
                   Destination : out Gdk_Event) is
      function Internal (Source : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_event_copy");
   begin
      Set_Object (Destination, Internal (Get_Object (Source)));
   end Copy;


   ------------
   --  Free  --
   ------------

   procedure Free (Event : in out Gdk_Event) is
      procedure Internal (Event : in System.Address);
      pragma Import (C, Internal, "gdk_event_free");
   begin
      Internal (Get_Object (Event));
      Set_Object (Event, System.Null_Address);
   end Free;


   -----------
   --  Get  --
   -----------

   procedure Get (Event : out Gdk_Event'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_event_get");
   begin
      Set_Object (Event, Internal);
   end Get;


   ---------------------------
   --  Get_Graphics_Expose  --
   ---------------------------

   procedure Get_Graphics_Expose (Event : out Gdk_Event'Class;
                                  Window : in Gdk_Window'Class) is
      function Internal (Window : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_event_get_graphics_expose");
   begin
      Set_Object (Event, Internal (Get_Object (Window)));
   end Get_Graphics_Expose;


   -----------------------
   --  Get_Show_Events  --
   -----------------------

   function Get_Show_Events return Boolean is
      function Internal return Gint;
      pragma Import (C, Internal, "gdk_get_show_events");
   begin
      return To_Boolean (Internal);
   end Get_Show_Events;


   -----------
   --  Put  --
   -----------

   procedure Put (Event : in Gdk_Event'Class) is
      procedure Internal (Event : in System.Address);
      pragma Import (C, Internal, "gdk_event_put");
   begin
      Internal (Get_Object (Event));
   end Put;


   -----------------------
   --  Set_Show_Events  --
   -----------------------

   procedure Set_Show_Events (Show_Events : in Boolean := True) is
      procedure Internal (Show_Events : in Gint);
      pragma Import (C, Internal, "gdk_set_show_events");
   begin
      Internal (To_Gint (Show_Events));
   end Set_Show_Events;

end Gdk.Event;
