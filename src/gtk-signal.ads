with Gtk.Object;

package Gtk.Signal is

   --  FIXME some signal can have a different declaration for a callback
   --  FIXME function, ie having more parameters. We probably need to
   --  FIXME defined some new generic package...

   ---------------------------------------------------------------
   --  The following package is for callbacks requiring a data to
   --  be passed to the callback function
   ---------------------------------------------------------------

   generic
      type Data_Type is private;
      --  The type of the data for the callback
      --  This type need not be an access type (as opposed as what happens in
      --  C). A new access is created by the connect function.

      type Widget_Type is new Object.Gtk_Object with private;
      --  The type of the widget to which a callback is connected

   package Callback is

      type Callback is access procedure
        (Widget : in out Widget_Type'Class;
         Data   : in     Data_Type);
      --  Callback function for Signal_Connect below

      procedure Void_Callback_Procedure (Widget : in out Widget_Type'Class;
                                         Data   : in     Data_Type);
      --
      --  Null body procedure

      function Connect
        (Obj       : in Widget_Type'Class;
         Name      : in String;
         Func      : in Callback;
         Func_Data : in Data_Type)
         return Guint;
      --  mapping: Connect gtksignal.h gtk_signal_connect

      function Connect_After
        (Obj       : in Widget_Type'Class;
         Name      : in String;
         Func      : in Callback;
         Func_Data : in Data_Type)
         return Guint;
      --  mapping: Connect_After gtksignal.h gtk_signal_connect_after

      procedure Disconnect (Object     : in Widget_Type;
                            Handler_Id : in Guint);
      --  mapping: Signal_Disconnect gtksignal.h gtk_signal_disconnect

      procedure Handler_Block (Obj        : in Widget_Type'Class;
                               Handler_Id : in Guint);
      --  mapping: Handler_Block gtksignal.h gtk_signal_handler_block

      procedure Handler_Unblock (Obj        : in Widget_Type'Class;
                                 Handler_Id : in Guint);
      --  mapping: Handler_Unblock gtksignal.h gtk_signal_handler_unblock

   end Callback;

   -----------------------------------------------------------------
   --  The following functions are for callbacks requiring no data to be
   --  passed to the callback
   -----------------------------------------------------------------

   generic
      type Widget_Type is new Object.Gtk_Object with private;

   package Void_Callback is

      type Callback is access procedure
        (Widget : in out Widget_Type'Class);

      procedure Void_Callback_Procedure (Widget : in out Widget_Type'Class);
      --
      --  null body callback.

      function Connect
        (Obj    : in Widget_Type'Class;
         Name   : in String;
         Func   : in Callback)
         return Guint;
      --  mapping: Connect gtksignal.h gtk_signal_connect

      function Connect_After
        (Obj    : in Widget_Type'Class;
         Name   : in String;
         Func   : in Callback)
         return Guint;
      --  mapping: Connect_After gtksignal.h gtk_signal_connect_after

      procedure Disconnect (Object     : in Widget_Type;
                            Handler_Id : in Guint);
      --  mapping: Signal_Disconnect gtksignal.h gtk_signal_disconnect

      procedure Handler_Block (Obj        : in Widget_Type'Class;
                               Handler_Id : in Guint);
      --  mapping: Handler_Block gtksignal.h gtk_signal_handler_block

      procedure Handler_Unblock (Obj        : in Widget_Type'Class;
                                 Handler_Id : in Guint);
      --  mapping: Handler_Unblock gtksignal.h gtk_signal_handler_unblock

   end Void_Callback;

   ------------------------------------------------------------------
   --  The following functions are for callbacks send to another object
   ------------------------------------------------------------------

   generic
      type Widget_Type is new Object.Gtk_Object with private;

   package Object_Callback is
      type Callback is access procedure
        (Object : in out Widget_Type'Class);

      function Connect
        (Obj         : in Object.Gtk_Object'Class;
         Name        : in String;
         Func        : in Callback;
         Slot_Object : in Widget_Type'Class)
         return Guint;
      --  mapping: Connect_Object gtksignal.h gtk_signal_connect_object

      function Connect_After
        (Obj         : in Object.Gtk_Object'Class;
         Name        : in String;
         Func        : in Callback;
         Slot_Object : in Widget_Type'Class)
         return Guint;
      --  mapping: Connect_Object_After gtksignal.h \
      --  mapping: gtk_signal_connect_object_after

      procedure Disconnect (Object     : in Widget_Type;
                            Handler_Id : in Guint);
      --  mapping: Signal_Disconnect gtksignal.h gtk_signal_disconnect

      procedure Handler_Block (Obj        : in Object.Gtk_Object'Class;
                               Handler_Id : in Guint);
      --  mapping: Handler_Block gtksignal.h gtk_signal_handler_block

      procedure Handler_Unblock (Obj        : in Object.Gtk_Object'Class;
                                 Handler_Id : in Guint);
      --  mapping: Handler_Unblock gtksignal.h gtk_signal_handler_unblock

   end Object_Callback;

   ------------------------------------------------------------------
   --  More general functions
   ------------------------------------------------------------------

   procedure Handlers_Destroy (Obj : in Object.Gtk_Object'Class);
   --  mapping: Handlers_Destroy gtksignal.h gtk_signal_handlers_destroy


   --  Functions which are not implemented because they are probably not needed
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_init
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_new
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_newv
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_lookup
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_name
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_emit
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_emit_by_name
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_emitv
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_emitv_by_name
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_n_emissions
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_n_emissions_by_name
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_emit_stop
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_emit_stop_by_name
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_connect_full
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_connect_interp
   --  mapping: NOT_IMPLEMENTED gtksignal.h \
   --  mapping: gtk_signal_connect_object_while_alive
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_connect_while_alive
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_disconnect_by_func
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_disconnect_by_data
   --    Actually, gtk_signal_disconnect_by_data can not be implemented since
   --    we did a new memory allocation in the Connect function
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_handler_block_by_func
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_handler_block_by_data
   --    The two 'block' can not be implemented
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_handler_unblock_by_func
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_handler_unblock_by_data
   --    The two 'unblock' can not be implemented
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_handler_pending
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_default_marshaller
   --  mapping: NOT_IMPLEMENTED gtksignal.h gtk_signal_set_funcs

   --  mapping: INTERNAL gtksignal.h gtk_signal_query

end Gtk.Signal;
