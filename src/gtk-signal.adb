with Unchecked_Conversion;
with Unchecked_Deallocation;

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

   function C_GTK_Signal_Connect
     (Obj       : System.Address;
      Name      : String;
      Func_Data : System.Address;
      Destroy   : System.Address;
      After     : Gint)
      return      Guint;
   pragma Import (C, C_GTK_Signal_Connect, "ada_gtk_signal_connect");

   ---------------
   --  Callback --
   ---------------

   package body Callback is

      type Data_Access is access Data_Type;
      type Data_Type_Record is
         record
            Binding_Func : System.Address;
            Signal       : Gint;
            Data         : Data_Access;
            Func         : Callback;
         end record;
      type Data_Type_Access is access all Data_Type_Record;
      pragma Convention (C, Data_Type_Access);

      function Convert is new Unchecked_Conversion (Data_Type_Access,
                                                    System.Address);

      procedure Free (Data : in out Data_Type_Access);
      --  Free the memory associated with the callback's data

      procedure General_Cb (Widget : System.Address;
                            Data   : Data_Type_Access);
      --  This is the only real callback function which is called from
      --  C. It dispatches the call to the real callback, after converting
      --  the widget from a C pointer to an Ada Widget type
      pragma Convention (C, General_Cb);

      ----------
      -- Free --
      ----------

      procedure Free (Data : in out Data_Type_Access) is
         procedure Internal is new Unchecked_Deallocation
           (Data_Type_Record, Data_Type_Access);
         procedure Internal_2 is new Unchecked_Deallocation
           (Data_Type, Data_Access);
      begin
         Internal_2 (Data.Data);
         Internal (Data);
      end Free;

      ----------------
      -- General_Cb --
      ----------------

      procedure General_Cb (Widget : System.Address;
                            Data   : Data_Type_Access)
      is
         Object : Widget_Type;
      begin
         Set_Object (Object, Widget);
         if Data.Func /= null then
            Data.Func (Object, Data.Data.all);
         end if;
      end General_Cb;

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
          new Data_Type_Record'(Data         => new Data_Type'(Func_Data),
                                Func         => Func,
                                Binding_Func => General_Cb'Address,
                                Signal       => 1);
      begin
         return C_GTK_Signal_Connect
           (Obj       => Get_Object (Obj),
            Name      => Name & Ascii.NUL,
            Func_Data => Convert (D),
            Destroy   => Free'Address,
            After     => Boolean'Pos (After));
      end Connect;
   end Callback;

   ------------------------------------------------------------
   -- Void_Callback                                          --
   ------------------------------------------------------------

   package body Void_Callback is

      type Data_Type_Record is
         record
            Binding_Func : System.Address;
            Signal       : Gint;
            Func         : Callback;
         end record;
      type Data_Type_Access is access all Data_Type_Record;
      pragma Convention (C, Data_Type_Access);

      function Convert is new Unchecked_Conversion (Data_Type_Access,
                                                    System.Address);

      procedure Free (Data : in out Data_Type_Access);

      procedure General_Cb (Widget : System.Address;
                            Data   : Data_Type_Access);
      pragma Convention (C, General_Cb);

      ----------
      -- Free --
      ----------

      procedure Free (Data : in out Data_Type_Access) is
         procedure Internal is new Unchecked_Deallocation (Data_Type_Record,
                                                           Data_Type_Access);
      begin
         Internal (Data);
      end Free;

      ----------------
      -- General_Cb --
      ----------------

      procedure General_Cb (Widget : System.Address;
                            Data   : Data_Type_Access)
      is
         AWidget : Widget_Type;
      begin
         Set_Object (AWidget, Widget);
         if Data.Func /= null then
            Data.Func (AWidget);
         end if;
      end General_Cb;

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
         D : Data_Type_Access
           := new Data_Type_Record'(Func         => Func,
                                    Binding_Func => General_Cb'Address,
                                    Signal       => 1);
      begin
         return C_GTK_Signal_Connect
           (Obj       => Get_Object (Obj),
            Name      => Name & Ascii.NUL,
            Func_Data => Convert (D),
            Destroy   => Free'Address,
            After     => Boolean'Pos (After));
      end Connect;
   end Void_Callback;


   ---------------------------------------------------------------
   -- Object_Callback                                           --
   ---------------------------------------------------------------

   package body Object_Callback is

      type Widget_Type_Access is access all Widget_Type;
      type Data_Type_Record is
         record
            Binding_Func : System.Address;
            Signal       : Gint;
            Func         : Callback;
            Data         : Widget_Type_Access;
         end record;
      --  Data has to be a pointer otherwise the C functions can not access
      --  properly the Binding_Func field of the structure...

      type Data_Type_Access is access all Data_Type_Record;
      pragma Convention (C, Data_Type_Access);

      function Convert is new Unchecked_Conversion (Data_Type_Access,
                                                    System.Address);

      procedure Free (Data : in out Data_Type_Access);
      procedure General_Cb (Widget : System.Address;
                            Data   : Data_Type_Access);
      pragma Convention (C, General_Cb);

      ----------
      -- Free --
      ----------

      procedure Free (Data : in out Data_Type_Access) is
         procedure Internal is new Unchecked_Deallocation
           (Data_Type_Record, Data_Type_Access);
         procedure Internal2 is new Unchecked_Deallocation
           (Widget_Type, Widget_Type_Access);
      begin
         Internal2 (Data.Data);
         Internal (Data);
      end Free;

      ----------------
      -- General_Cb --
      ----------------

      procedure General_Cb (Widget : System.Address;
                            Data   : Data_Type_Access)
      is
      begin
         if Data.Func /= null then
            Data.Func (Data.Data.all);
         end if;
      end General_Cb;

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
           := new Data_Type_Record'(Data         =>
                                      new Widget_Type'(Slot_Object),
                                    Func         => Func,
                                    Binding_Func => General_Cb'Address,
                                    Signal       => 1);
      begin
         return C_GTK_Signal_Connect
           (Obj       => Get_Object (Obj),
            Name      => Name & Ascii.NUL,
            Func_Data => Convert (D),
            Destroy   => Free'Address,
            After     => Boolean'Pos (After));
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
