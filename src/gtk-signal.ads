with Gtk; use Gtk;
with Gtk.Widget;

package Gtk.Signal is

   --  FIXME some signal can have a different declaration for a callback
   --  FIXME function, ie having more parameters. We probably need to
   --  FIXME defined some new generic package...

   --  FIXME free memory associated with the data in disconnect
   --  FIXME We have to keep track of the data type somehow
   --  FIXME A simple way could be that Connect returns a special type
   --  FIXME associated with the generic package, and the disconnect
   --  FIXME function is part of this generic package.

   ---------------------------------------------------------------
   --  The following package is for callbacks requiring a data to
   --  be passed to the callback function
   ---------------------------------------------------------------

   generic
      type Data_Type is private;
      --  The type of the data for the callback

   package Callback is

      type Callback is access procedure
        (Widget : in Gtk.Widget.Widget'Class;
         Data   : access Data_Type);
      --  Callback function for Signal_Connect below

      function Connect
        (Obj       : in Object'Class;
         Name      : in String;
         Func      : in Callback;
         Func_Data : in Data_Type)
         return GUint;
      --  mapping: Connect gtksignal.h gtk_signal_connect

      function Connect_After
        (Obj       : in Object'Class;
         Name      : in String;
         Func      : in Callback;
         Func_Data : in Data_Type)
         return GUint;
      --  mapping: Connect_After gtksignal.h gtk_signal_connect_after

   end Callback;

   -----------------------------------------------------------------
   --  The following functions are for callbacks requiring no data to be
   --  passed to the callback
   -----------------------------------------------------------------

   type Void_Callback is access procedure
     (Widget : in Gtk.Widget.Widget'Class);

   function Connect
     (Obj    : in Object'Class;
      Name   : in String;
      Func   : in Void_Callback)
      return GUint;
   --  mapping: Connect gtksignal.h gtk_signal_connect

   function Connect_After
     (Obj    : in Object'Class;
      Name   : in String;
      Func   : in Void_Callback)
      return GUint;
   --  mapping: Connect_After gtksignal.h gtk_signal_connect_after

   ------------------------------------------------------------------
   --  The following functions are for callbacks send to another object
   ------------------------------------------------------------------
   type Signal_Func is access procedure
     (Object : in Gtk.Widget.Widget'Class);

   function Connect_Object
     (Obj         : in Object'Class;
      Name        : in String;
      Func        : in Signal_Func;
      Slot_Object : access Object'Class)
      return GUint;
   --  mapping: Connect_Object gtksignal.h gtk_signal_connect_object

   function Connect_Object_After
     (Obj         : in Object'Class;
      Name        : in String;
      Func        : in Signal_Func;
      Slot_Object : access Object'Class)
      return GUint;
   --  mapping: Connect_Object_After gtksignal.h \
   --  mapping: gtk_signal_connect_object_after

   ------------------------------------------------------------------
   --  More general functions
   ------------------------------------------------------------------

   procedure Disconnect (Obj        : in Object'Class;
                         Handler_Id : in GUint);
   --  mapping: Disconnect gtksignal.h gtk_signal_disconnect

   procedure Handler_Block (Obj        : in Object'Class;
                            Handler_Id : in GUint);
   --  mapping: Handler_Block gtksignal.h gtk_signal_handler_block

   procedure Handlers_Destroy (Obj : in Object'Class);
   --  mapping: Handlers_Destroy gtksignal.h gtk_signal_handlers_destroy

   procedure Handler_Unblock (Obj        : in Object'Class;
                              Handler_Id : in GUint);
   --  mapping: Handler_Unblock gtksignal.h gtk_signal_handler_unblock


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
