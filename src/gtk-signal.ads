with Gtk; use Gtk;
with Gtk.Widget;

package Gtk.Signal is

   --  FIXME some signal can have a different declaration for a callback
   --  FIXME function, ie having more parameters. We probably need to
   --  FIXME defined some new generic package...

   --  FIXME free memory associated with the data in disconnect
   --  FIXME We have to keep track of the data type somehow
   --  FIXME A simple way could be that Gtk_Connect returns a special type
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

      type Gtk_Callback is access procedure
        (Widget : in Gtk.Widget.Gtk_Widget'Class;
         Data   : access Data_Type);
      --  Callback function for Gtk_Signal_Connect below

      function Gtk_Connect
        (Object    : in Gtk_Object'Class;
         Name      : in String;
         Func      : in Gtk_Callback;
         Func_Data : in Data_Type)
         return GUint;
      --  mapping: Gtk_Connect gtksignal.h gtk_signal_connect

      function Gtk_Connect_After
        (Object    : in Gtk_Object'Class;
         Name      : in String;
         Func      : in Gtk_Callback;
         Func_Data : in Data_Type)
         return GUint;
      --  mapping: Gtk_Connect_After gtksignal.h gtk_signal_connect_after

   end Callback;

   -----------------------------------------------------------------
   --  The following functions are for callbacks requiring no data to be
   --  passed to the callback
   -----------------------------------------------------------------

   type Gtk_Void_Callback is access procedure
     (Widget : in Gtk.Widget.Gtk_Widget'Class);

   function Gtk_Connect
     (Object : in Gtk_Object'Class;
      Name   : in String;
      Func   : in Gtk_Void_Callback)
      return GUint;
   --  mapping: Gtk_Connect gtksignal.h gtk_signal_connect

   function Gtk_Connect_After
     (Object : in Gtk_Object'Class;
      Name   : in String;
      Func   : in Gtk_Void_Callback)
      return GUint;
   --  mapping: Gtk_Connect_After gtksignal.h gtk_signal_connect_after

   ------------------------------------------------------------------
   --  The following functions are for callbacks send to another object
   ------------------------------------------------------------------
   type Gtk_Signal_Func is access procedure
     (Object : in Gtk.Widget.Gtk_Widget'Class);

   function Gtk_Connect_Object
     (Object      : in Gtk_Object'Class;
      Name        : in String;
      Func        : in Gtk_Signal_Func;
      Slot_Object : access Gtk_Object'Class)
      return GUint;
   --  mapping: Gtk_Connect_Object gtksignal.h gtk_signal_connect_object

   function Gtk_Connect_Object_After
     (Object      : in Gtk_Object'Class;
      Name        : in String;
      Func        : in Gtk_Signal_Func;
      Slot_Object : access Gtk_Object'Class)
      return GUint;
   --  mapping: Gtk_Connect_Object_After gtksignal.h \
   --  mapping: gtk_signal_connect_object_after

   ------------------------------------------------------------------
   --  More general functions
   ------------------------------------------------------------------

   procedure Gtk_Disconnect (Object     : in Gtk_Object'Class;
                             Handler_Id : in GUint);
   --  mapping: Gtk_Disconnect gtksignal.h gtk_signal_disconnect

   procedure Gtk_Handler_Block (Object     : in Gtk_Object'Class;
                                Handler_Id : in GUint);
   --  mapping: Gtk_Handler_Block gtksignal.h gtk_signal_handler_block

   procedure Gtk_Handlers_Destroy (Object : in Gtk_Object'Class);
   --  mapping: Gtk_Handlers_Destroy gtksignal.h gtk_signal_handlers_destroy

   procedure Gtk_Handler_Unblock (Object     : in Gtk_Object'Class;
                                  Handler_Id : in GUint);
   --  mapping: Gtk_Handler_Unblock gtksignal.h gtk_signal_handler_unblock


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
   --    we did a new memory allocation in the Gtk_Connect function
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
