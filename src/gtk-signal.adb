with Unchecked_Conversion;
with Unchecked_Deallocation;
with Gtk.Object;

--  This package tries to handle three problems encountered with callbacks
--    1) the memory allocated internally by our Ada functions must be freed
--       when the callback is destroyed. For this, we used the 'destroy_func'
--       field in gtk
--    2) The object passed to the Ada callback must be the same one that was
--       passed to connect. For this, widgets are defined as pointers to a
--       a structure, the real type beeing know thanks to the generic packages
--    3) in gtk, callbacks may have different number of arguments, depending
--       on the signal and the widget. For this, we use a C function with a
--       variable number of argument as the real callback. This C function in
--       turns calls a General_Cb function in Ada with creates the correct
--       widget type. 'General_Cb' then calls the user callback.
--
--  Note that the instantiations of the generic packages below have to be
--  done at a library level, as the callbacks are in fact pointers to
--  functions in this package. Thus the functions have to be found when the
--  callback is found


package body Gtk.Signal is

   type DestroyFunc is access procedure (Data : in System.Address);
   type GtkArgArray is new System.Address;
   type MarshallerFunc is access procedure (Object    : in System.Address;
                                            User_Data : in System.Address;
                                            Nparams   : in Guint;
                                            Params    : in GtkArgArray);
   function Do_Signal_Connect (Object     : in Gtk.Object.Gtk_Object'Class;
                               Name       : in String;
                               Marshaller : in System.Address;
                               Func_Data  : in System.Address;
                               Destroy    : in System.Address;
                               After      : in Boolean)
                               return          Guint;

   -----------------------
   -- Do_Signal_Connect --
   -----------------------

   function Do_Signal_Connect (Object     : in Gtk.Object.Gtk_Object'Class;
                               Name       : in String;
                               Marshaller : in System.Address;
                               Func_Data  : in System.Address;
                               Destroy    : in System.Address;
                               After      : in Boolean)
                               return         Guint
   is
      function Internal (Object        : System.Address;
                         Name          : String;
                         Func          : System.Address;
                         Marshaller    : System.Address;
                         Func_Data     : System.Address;
                         Destroy       : System.Address;
                         Object_Signal : Gint;
                         After         : Gint)
                         return          Guint;
      pragma Import (C, Internal, "gtk_signal_connect_full");
   begin
      return Internal (Get_Object (Object),
                       Name & Ascii.NUL,
                       System.Null_Address,
                       Marshaller,
                       Func_Data,
                       Destroy,
                       Boolean'Pos (False),
                       Boolean'Pos (After));
   end Do_Signal_Connect;

   --------------
   -- Callback --
   --------------

   package body Callback is

      type Data_Access is access Data_Type;
      type Data_Type_Record is
         record
            Data   : Data_Access;
            Func   : Callback;
         end record;
      type Data_Type_Access is access all Data_Type_Record;
      pragma Convention (C, Data_Type_Access);

      function Convert is new Unchecked_Conversion (Data_Type_Access,
                                                    System.Address);
      function Convert is new Unchecked_Conversion (System.Address,
                                                    Data_Type_Access);

      procedure Free (Data : in System.Address);
      pragma Convention (C, Free);
      --  Free the memory associated with the callback's data

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray);
      pragma Convention (C, Marshaller);

      ----------
      -- Free --
      ----------

      procedure Free (Data : in System.Address) is
         procedure Internal is new Unchecked_Deallocation
           (Data_Type_Record, Data_Type_Access);
         procedure Internal_2 is new Unchecked_Deallocation
           (Data_Type, Data_Access);
         D : Data_Type_Access := Convert (Data);
      begin
         Internal_2 (D.Data);
         Internal (D);
      end Free;

      ----------------
      -- Marshaller --
      ----------------

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray)
      is
         Data   : Data_Type_Access := Convert (User_Data);
         Widget : Widget_Type;
      begin
         if Data.Func /= null then
            Set_Object (Widget, Object);
            Data.Func (Widget, Data.Data.all);
         end if;
      end Marshaller;

      -------------
      -- Connect --
      -------------

      function Connect
        (Obj       : in Widget_Type'Class;
         Name      : in String;
         Func      : in Callback;
         Func_Data : in Data_Type;
         After     : in Boolean := False)
         return Guint
      is
         D : Data_Type_Access :=
          new Data_Type_Record'(Data => new Data_Type'(Func_Data),
                                Func => Func);
      begin
         return Do_Signal_Connect (Obj,
                                   Name & Ascii.NUL,
                                   Marshaller'Address,
                                   Convert (D),
                                   Free'Address,
                                   After);
      end Connect;
   end Callback;

   ------------------
   -- Two_Callback --
   ------------------

   package body Two_Callback is

      type Data_Access is access Data_Type;
      type Data_Type_Record is
         record
            Data   : Data_Access;
            Func   : Callback;
         end record;
      type Data_Type_Access is access all Data_Type_Record;
      pragma Convention (C, Data_Type_Access);

      function Convert is new Unchecked_Conversion (Data_Type_Access,
                                                    System.Address);
      function Convert is new Unchecked_Conversion (System.Address,
                                                    Data_Type_Access);

      procedure Free (Data : in System.Address);
      pragma Convention (C, Free);
      --  Free the memory associated with the callback's data

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray);
      pragma Convention (C, Marshaller);

      ----------
      -- Free --
      ----------

      procedure Free (Data : in System.Address) is
         procedure Internal is new Unchecked_Deallocation
           (Data_Type_Record, Data_Type_Access);
         procedure Internal_2 is new Unchecked_Deallocation
           (Data_Type, Data_Access);
         D : Data_Type_Access := Convert (Data);
      begin
         Internal_2 (D.Data);
         Internal (D);
      end Free;

      ----------------
      -- Marshaller --
      ----------------

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray)
      is
         function Internal (Params : in GtkArgArray;
                            Num    : in Guint)
                            return System.Address;
         pragma Import (C, Internal, "ada_gtkarg_value_object");
         use type System.Address;
         Data   : Data_Type_Access := Convert (User_Data);
         Widget : Widget_Type;
         Widget2 : Cb_Type;
         Tmp    : System.Address := Internal (Params, 0);
      begin
         if Nparams = 0 then
            raise Constraint_Error;
         end if;
         if Data.Func /= null then
            Set_Object (Widget, Object);
            Set_Object (Widget2, Tmp);
            Data.Func (Widget, Widget2, Data.Data.all);
         end if;
      end Marshaller;

      -------------
      -- Connect --
      -------------

      function Connect
        (Obj       : in Widget_Type'Class;
         Name      : in String;
         Func      : in Callback;
         Func_Data : in Data_Type;
         After     : in Boolean := False)
         return Guint
      is
         D : Data_Type_Access :=
          new Data_Type_Record'(Data => new Data_Type'(Func_Data),
                                Func => Func);
      begin
         return Do_Signal_Connect (Obj,
                                   Name & Ascii.NUL,
                                   Marshaller'Address,
                                   Convert (D),
                                   Free'Address,
                                   After);
      end Connect;
   end Two_Callback;

   ------------------------------------------------------------
   -- Void_Callback                                          --
   ------------------------------------------------------------

   package body Void_Callback is

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray);
      pragma Convention (C, Marshaller);

      function Convert is new Unchecked_Conversion (System.Address,
                                                    Callback);
      function Convert is new Unchecked_Conversion (Callback,
                                                    System.Address);

      ----------------
      -- Marshaller --
      ----------------

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray)
      is
         Data   : Callback := Convert (User_Data);
         Widget : Widget_Type;
      begin
         Set_Object (Widget, Object);
         Data (Widget);
      end Marshaller;

      -------------
      -- Connect --
      -------------

      function Connect
        (Obj   : in Widget_Type'Class;
         Name  : in String;
         Func  : in Callback;
         After : in Boolean := False)
         return Guint
      is
      begin
         return Do_Signal_Connect (Obj,
                                   Name & Ascii.NUL,
                                   Marshaller'Address,
                                   Convert (Func),
                                   System.Null_Address,
                                   After);
      end Connect;
   end Void_Callback;


   ---------------------------------------------------------------
   -- Object_Callback                                           --
   ---------------------------------------------------------------

   package body Object_Callback is

      type Data_Type_Record is
         record
            Func : Callback;
            Data : Widget_Type;
         end record;
      type Data_Type_Access is access all Data_Type_Record;

      pragma Convention (C, Data_Type_Access);

      function Convert is new Unchecked_Conversion (Data_Type_Access,
                                                    System.Address);
      function Convert is new Unchecked_Conversion (System.Address,
                                                    Data_Type_Access);

      procedure Free (Data : in System.Address);
      pragma Convention (C, Free);

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray);
      pragma Convention (C, Marshaller);

      ----------
      -- Free --
      ----------

      procedure Free (Data : in System.Address) is
         procedure Internal is new Unchecked_Deallocation
           (Data_Type_Record, Data_Type_Access);
         D : Data_Type_Access := Convert (Data);
      begin
         Internal (D);
      end Free;

      ----------------
      -- Marshaller --
      ----------------

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray)
      is
         Data : Data_Type_Access := Convert (User_Data);
      begin
         if Data.Func /= null then
            Data.Func (Data.Data);
         end if;
      end Marshaller;

      -------------
      -- Connect --
      -------------

      function Connect
        (Obj         : in Object.Gtk_Object'Class;
         Name        : in String;
         Func        : in Callback;
         Slot_Object : in Widget_Type'Class;
         After       : in Boolean := False)
         return Guint
      is
         D : Data_Type_Access
           := new Data_Type_Record'(Data => Widget_Type (Slot_Object),
                                    Func => Func);
      begin
         return Do_Signal_Connect (Obj,
                                   Name & Ascii.NUL,
                                   Marshaller'Address,
                                   Convert (D),
                                   Free'Address,
                                   After);
      end Connect;
   end Object_Callback;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect
     (Object     : in Gtk.Object.Gtk_Object'Class;
      Handler_Id : in Guint)
   is
      procedure Internal (Obj : System.Address; Id  : Guint);
      pragma Import (C, Internal, "gtk_signal_disconnect");

   begin
      Internal (Obj => Get_Object (Object),
                Id  => Handler_Id);
   end Disconnect;

   -------------------
   -- Handler_Block --
   -------------------

   procedure Handler_Block
     (Obj        : in Gtk.Object.Gtk_Object'Class;
      Handler_Id : in Guint)
   is
      procedure Internal (Obj : in System.Address; Id  : in Guint);
      pragma Import (C, Internal, "gtk_signal_handler_block");
   begin
      Internal (Obj => Get_Object (Obj), Id  => Handler_Id);
   end Handler_Block;

   ----------------------
   -- Handlers_Destroy --
   ----------------------

   procedure Handlers_Destroy (Obj : in Object.Gtk_Object'Class)
   is
      procedure Internal (Obj : System.Address);
      pragma Import (C, Internal, "gtk_signal_handlers_destroy");
   begin
      Internal (Obj => Get_Object (Obj));
   end Handlers_Destroy;

   ---------------------
   -- Handler_Unblock --
   ---------------------

   procedure Handler_Unblock (Obj        : in Gtk.Object.Gtk_Object'Class;
                              Handler_Id : in Guint)
   is
      procedure Internal (Obj : in System.Address; Id  : in Guint);
      pragma Import (C, Internal, "gtk_signal_handler_unblock");
   begin
      Internal (Obj => Get_Object (Obj), Id  => Handler_Id);
   end Handler_Unblock;

end Gtk.Signal;
