package body Gdk.Event is


   ------------
   --  Copy  --
   ------------

   procedure Copy (Source : in Gdk_Event'Class;
                   Destination : out Gdk_Event'Class) is
      function Internal (Source : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_event_copy");
   begin
      Set_Object (Destination, Internal (Get_Object (Source)));
   end Copy;


   ------------
   --  Free  --
   ------------

   procedure Free (Event : in out Gdk_Event'Class) is
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


   ----------------------
   --  Get_Event_Type  --
   ----------------------

   function Get_Event_Type (Event : in Gdk_Event'Class)
                            return Types.Gdk_Event_Type is
      function Internal (Event : in System.Address)
                         return Types.Gdk_Event_Type;
      pragma Import (C, Internal, "ada_gdk_event_any_get_event_type");
   begin
      return Internal (Get_Object (Event));
   end Get_Event_Type;


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


   ----------------------
   --  Get_Send_Event  --
   ----------------------

   function Get_Send_Event (Event : in Gdk_Event'Class) return Boolean is
      function Internal (Event : in System.Address) return Gint8;
      pragma Import (C, Internal, "ada_gdk_event_any_get_send_event");
   begin
      return To_Boolean (Internal (Get_Object (Event)));
   end Get_Send_Event;


   -----------------------
   --  Get_Show_Events  --
   -----------------------

   function Get_Show_Events return Boolean is
      function Internal return Gint;
      pragma Import (C, Internal, "gdk_get_show_events");
   begin
      return To_Boolean (Internal);
   end Get_Show_Events;


   ------------------
   --  Get_Window  --
   ------------------

   procedure Get_Window (Event  : in     Gdk_Event'Class;
                         Window :    out Gdk_Window'Class) is
      function Internal (Event : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gdk_event_any_get_window");
   begin
      Set_Object (Window, Internal (Get_Object (Event)));
   end Get_Window;


   -----------
   --  Put  --
   -----------

   procedure Put (Event : in Gdk_Event'Class) is
      procedure Internal (Event : in System.Address);
      pragma Import (C, Internal, "gdk_event_put");
   begin
      Internal (Get_Object (Event));
   end Put;


   ----------------------
   --  Set_Event_Type  --
   ----------------------

   procedure Set_Event_Type (Event      : in out Gdk_Event'Class;
                             Event_Type : in     Types.Gdk_Event_Type) is
      procedure Internal (Event : in System.Address;
                          Event_Type : in Types.Gdk_Event_Type);
      pragma Import (C, Internal, "ada_gdk_event_any_set_event_type");
   begin
      Internal (Get_Object (Event), Event_Type);
   end Set_Event_Type;


   ----------------------
   --  Set_Send_Event  --
   ----------------------

   procedure Set_Send_Event (Event      : in out Gdk_Event'Class;
                             Send_Event : in     Boolean := True) is
      procedure Internal (Event : in System.Address;
                          Send_Event : in Gint8);
      pragma Import (C, Internal, "ada_gdk_event_any_set_send_event");
   begin
      Internal (Get_Object (Event), To_Gint (Send_Event));
   end Set_Send_Event;



   -----------------------
   --  Set_Show_Events  --
   -----------------------

   procedure Set_Show_Events (Show_Events : in Boolean := True) is
      procedure Internal (Show_Events : in Gint);
      pragma Import (C, Internal, "gdk_set_show_events");
   begin
      Internal (To_Gint (Show_Events));
   end Set_Show_Events;


   ------------------
   --  Set_Window  --
   ------------------

   procedure Set_Window (Event  : in out Gdk_Event'Class;
                         Window : in     Gdk.Window.Gdk_Window'Class) is
      procedure Internal (Event, Window : in System.Address);
      pragma Import (C, Internal, "ada_gdk_event_any_set_window");
   begin
      Internal (Get_Object (Event), Get_Object (Window));
   end Set_Window;

end Gdk.Event;
