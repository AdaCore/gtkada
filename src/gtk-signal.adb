with Unchecked_Conversion;

package body Gtk.Signal is

   ---------------
   --  Callback --
   ---------------

   package body Callback is

      type Data_Type_Access is access all Data_Type;
      D : Data_Type_Access;
      function Convert is new Unchecked_Conversion (Data_Type_Access,
                                                    System.Address);

      ------------------------
      -- Gtk_Signal_Connect --
      ------------------------

      function Gtk_Connect
        (Object    : in Gtk_Object'Class;
         Name      : in String;
         Func      : in Gtk_Callback;
         Func_Data : in Data_Type)
         return GUint
      is
         function Internal (Object    : System.Address;
                            Name      : String;
                            Func      : Gtk_Callback;
                            Func_Data : System.Address) return GUint;
         pragma Import (C, Internal, "gtk_signal_connect");
      begin
         D := new Data_Type'(Func_Data);
         return Internal (Get_Object (Object), Name & Ascii.NUL,
                          Func, Convert (D));
      end Gtk_Connect;

      -------------------------------
      --  Gtk_Signal_Connect_After --
      -------------------------------

      function Gtk_Connect_After
        (Object    : in Gtk_Object'Class;
         Name      : in String;
         Func      : in Gtk_Callback;
         Func_Data : in Data_Type)
         return GUint
      is
         function Internal (Object    : System.Address;
                            Name      : String;
                            Func      : Gtk_Callback;
                            Func_Data : System.Address) return GUint;
         pragma Import (C, Internal, "gtk_signal_connect_after");
      begin
         D := new Data_Type'(Func_Data);
         return Internal (Get_Object (Object), Name & Ascii.NUL,
                          Func, Convert (D));
      end Gtk_Connect_After;

   end Callback;

   ------------------------
   -- Gtk_Signal_Connect --
   ------------------------

   function Gtk_Connect
     (Object : in Gtk_Object'Class;
      Name   : in String;
      Func   : in Gtk_Void_Callback)
      return GUint
   is
      function Internal (Object : System.Address;
                         Name   : String;
                         Func   : Gtk_Void_Callback;
                         Data   : System.Address) return GUint;
      pragma Import (C, Internal, "gtk_signal_connect");
   begin
      return Internal (Get_Object (Object), Name & Ascii.NUL,
                       Func, System.Null_Address);
   end Gtk_Connect;

   ------------------------------
   -- Gtk_Signal_Connect_After --
   ------------------------------

   function Gtk_Connect_After
     (Object : in Gtk_Object'Class;
      Name   : in String;
      Func   : in Gtk_Void_Callback)
      return GUint
   is
      function Internal (Object : System.Address;
                         Name   : String;
                         Func   : Gtk_Void_Callback;
                         Data   : System.Address) return GUint;
      pragma Import (C, Internal, "gtk_signal_connect_after");
   begin
      return Internal (Get_Object (Object), Name & Ascii.NUL,
                       Func, System.Null_Address);
   end Gtk_Connect_After;

   -------------------------------
   -- Gtk_Signal_Connect_Object --
   -------------------------------

   function Gtk_Connect_Object
     (Object      : in Gtk_Object'Class;
      Name        : in String;
      Func        : in Gtk_Signal_Func;
      Slot_Object : access Gtk_Object'Class)
      return GUint
   is
      function Internal (Object : System.Address;
                         Name   : String;
                         Func   : Gtk_Signal_Func;
                         Slot   : System.Address) return GUint;
      pragma Import (C, Internal, "gtk_signal_connect_object");
   begin
      return Internal (Get_Object (Object), Name & Ascii.NUL,
                       Func, Slot_Object.all'Address);
   end Gtk_Connect_Object;

   -------------------------------------
   -- Gtk_Signal_Connect_Object_After --
   -------------------------------------

   function Gtk_Connect_Object_After
     (Object      : in Gtk_Object'Class;
      Name        : in String;
      Func        : in Gtk_Signal_Func;
      Slot_Object : access Gtk_Object'Class)
      return GUint
   is
      function Internal (Object : System.Address;
                         Name   : String;
                         Func   : Gtk_Signal_Func;
                         Slot   : System.Address) return GUint;
      pragma Import (C, Internal, "gtk_signal_connect_object_after");
   begin
      return Internal (Get_Object (Object), Name & Ascii.NUL,
                       Func, Slot_Object.all'Address);
   end Gtk_Connect_Object_After;

   ---------------------------
   -- Gtk_Signal_Disconnect --
   ---------------------------

   procedure Gtk_Disconnect (Object     : in Gtk_Object'Class;
                             Handler_Id : in GUint)
   is
      procedure Internal (Object     : System.Address;
                          Handler_Id : GUint);
      pragma Import (C, Internal, "gtk_signal_disconnect");
   begin
      Internal (Get_Object (Object), Handler_Id);
   end Gtk_Disconnect;

   ------------------------------
   -- Gtk_Signal_Handler_Block --
   ------------------------------
   procedure Gtk_Handler_Block (Object : in Gtk_Object'Class;
                                Handler_Id : in GUint)
   is
      procedure Internal (Object     : in System.Address;
                          Handler_Id : in GUint);
      pragma Import (C, Internal, "gtk_signal_handler_block");
   begin
      Internal (Get_Object (Object), Handler_Id);
   end Gtk_Handler_Block;

   ---------------------------------
   -- Gtk_Signal_Handlers_Destroy --
   ---------------------------------

   procedure Gtk_Handlers_Destroy (Object : in Gtk_Object'Class)
   is
      procedure Internal (Object : System.Address);
      pragma Import (C, Internal, "gtk_signal_handlers_destroy");
   begin
      Internal (Get_Object (Object));
   end Gtk_Handlers_Destroy;

   -------------------------------- -- Signal_Handler_Unblock --
   -- Gtk_Signal_Handler_Unblock --
   procedure Handler_Unblock (Obj        : in Object'Class;
   procedure Gtk_Handler_Unblock (Object     : in Gtk_Object'Class;
                                  Handler_Id : in GUint)
      procedure Internal (Obj     : in System.Address;
      procedure Internal (Object     : in System.Address;
      pragma Import (C, Internal, "gtk_signal_handler_unblock");
   begin
      Internal (Get_Object (Obj), Handler_Id);
      Internal (Get_Object (Object), Handler_Id);
   end Gtk_Handler_Unblock;
end Gtk.Signal;
