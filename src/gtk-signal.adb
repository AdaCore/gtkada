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
      -- Signal_Connect --
      ------------------------

      function Connect
        (Obj       : in Object'Class;
         Name      : in String;
         Func      : in Callback;
         Func_Data : in Data_Type)
         return GUint
      is
         function Internal (Obj       : System.Address;
                            Name      : String;
                            Func      : Callback;
                            Func_Data : System.Address) return GUint;
         pragma Import (C, Internal, "gtk_signal_connect");
      begin
         D := new Data_Type'(Func_Data);
         return Internal (Get_Object (Obj), Name & Ascii.NUL,
                          Func, Convert (D));
      end Connect;

      -------------------------------
      --  Signal_Connect_After --
      -------------------------------

      function Connect_After
        (Obj       : in Object'Class;
         Name      : in String;
         Func      : in Callback;
         Func_Data : in Data_Type)
         return GUint
      is
         function Internal (Obj       : System.Address;
                            Name      : String;
                            Func      : Callback;
                            Func_Data : System.Address) return GUint;
         pragma Import (C, Internal, "gtk_signal_connect_after");
      begin
         D := new Data_Type'(Func_Data);
         return Internal (Get_Object (Obj), Name & Ascii.NUL,
                          Func, Convert (D));
      end Connect_After;

   end Callback;

   ------------------------
   -- Signal_Connect --
   ------------------------

   function Connect
     (Obj  : in Object'Class;
      Name : in String;
      Func : in Void_Callback)
      return GUint
   is
      function Internal (Obj  : System.Address;
                         Name : String;
                         Func : Void_Callback;
                         Data : System.Address) return GUint;
      pragma Import (C, Internal, "gtk_signal_connect");
   begin
      return Internal (Get_Object (Obj), Name & Ascii.NUL,
                       Func, System.Null_Address);
   end Connect;

   ------------------------------
   -- Signal_Connect_After --
   ------------------------------

   function Connect_After
     (Obj  : in Object'Class;
      Name : in String;
      Func : in Void_Callback)
      return GUint
   is
      function Internal (Obj  : System.Address;
                         Name : String;
                         Func : Void_Callback;
                         Data : System.Address) return GUint;
      pragma Import (C, Internal, "gtk_signal_connect_after");
   begin
      return Internal (Get_Object (Obj), Name & Ascii.NUL,
                       Func, System.Null_Address);
   end Connect_After;

   -------------------------------
   -- Signal_Connect_Object --
   -------------------------------

   function Connect_Object
     (Obj         : in Object'Class;
      Name        : in String;
      Func        : in Signal_Func;
      Slot_Object : access Object'Class)
      return GUint
   is
      function Internal (Obj  : System.Address;
                         Name : String;
                         Func : Signal_Func;
                         Slot : System.Address) return GUint;
      pragma Import (C, Internal, "gtk_signal_connect_object");
   begin
      return Internal (Get_Object (Obj), Name & Ascii.NUL,
                       Func, Slot_Object.all'Address);
   end Connect_Object;

   -------------------------------------
   -- Signal_Connect_Object_After --
   -------------------------------------

   function Connect_Object_After
     (Obj         : in Object'Class;
      Name        : in String;
      Func        : in Signal_Func;
      Slot_Object : access Object'Class)
      return GUint
   is
      function Internal (Obj  : System.Address;
                         Name : String;
                         Func : Signal_Func;
                         Slot : System.Address) return GUint;
      pragma Import (C, Internal, "gtk_signal_connect_object_after");
   begin
      return Internal (Get_Object (Obj), Name & Ascii.NUL,
                       Func, Slot_Object.all'Address);
   end Connect_Object_After;

   ---------------------------
   -- Signal_Disconnect --
   ---------------------------

   procedure Disconnect (Obj        : in Object'Class;
                         Handler_Id : in GUint)
   is
      procedure Internal (Obj        : System.Address;
                          Handler_Id : GUint);
      pragma Import (C, Internal, "gtk_signal_disconnect");
   begin
      Internal (Get_Object (Obj), Handler_Id);
   end Disconnect;

   ------------------------------
   -- Signal_Handler_Block --
   ------------------------------
   procedure Handler_Block (Obj        : in Object'Class;
                            Handler_Id : in GUint)
   is
      procedure Internal (Obj        : in System.Address;
                          Handler_Id : in GUint);
      pragma Import (C, Internal, "gtk_signal_handler_block");
   begin
      Internal (Get_Object (Obj), Handler_Id);
   end Handler_Block;

   ---------------------------------
   -- Signal_Handlers_Destroy --
   ---------------------------------

   procedure Handlers_Destroy (Obj : in Object'Class)
   is
      procedure Internal (Obj : System.Address);
      pragma Import (C, Internal, "gtk_signal_handlers_destroy");
   begin
      Internal (Get_Object (Obj));
   end Handlers_Destroy;

   -------------------------------- -- Signal_Handler_Unblock --
   --------------------------------
   procedure Handler_Unblock (Obj        : in Object'Class;
                              Handler_Id : in GUint)
   is
      procedure Internal (Obj     : in System.Address;
                          Handler_Id : in GUint);
      pragma Import (C, Internal, "gtk_signal_handler_unblock");
   begin
      Internal (Get_Object (Obj), Handler_Id);
   end Handler_Unblock;

end Gtk.Signal;
