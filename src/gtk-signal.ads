-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
-----------------------------------------------------------------------

with Gdk;
with Gtk.Object;
with Gtk.Widget;
with Gtk.Tips_Query;
with System;

package Gtk.Signal is

   --  Every Connect function accepts 'null' for the function to call. In
   --  this case, the callback behaves as if you were calling a function
   --  with a null body

   ---------------------------------------------------------------
   --  The following package is for callbacks requiring a data to
   --  be passed to the callback function
   ---------------------------------------------------------------

   generic
      type Widget_Type is new Gtk.Object.Gtk_Object with private;

      type Data_Type (<>) is private;
      --  The type of the data for the callback
      --  This type need not be an access type (as opposed as what happens in
      --  C). A new access is created by the connect function.

   package Callback is

      type Callback is access procedure
        (Widget : in out Widget_Type'Class;
         Data   : in out Data_Type);
      --  Callback function for Signal_Connect below

      function Connect
        (Obj       : in Widget_Type'Class;
         Name      : in String;
         Func      : in Callback;
         Func_Data : in Data_Type;
         After     : in Boolean := False)
         return Guint;
      --  mapping: Connect gtksignal.h gtk_signal_connect
      --  mapping: Connect gtksignal.h gtk_signal_connect_after
   end Callback;

   ---------------------------------------------------------------
   --  The following package is for callbacks requiring two data to
   --  be passed to the callback function
   --  The second data will be given by gtk itself
   ---------------------------------------------------------------

   generic
      type Widget_Type is new Gtk.Object.Gtk_Object with private;

      type Data_Type (<>) is private;

      type Cb_Type is new Gdk.Root_Type with private;

   package Two_Callback is

      type Callback is access procedure
        (Widget  : in out Widget_Type'Class;
         Cb_Data : in out Cb_Type;
         Data    : in out Data_Type);
      --  Callback function for Signal_Connect below

      function Connect
        (Obj       : in Widget_Type'Class;
         Name      : in String;
         Func      : in Callback;
         Func_Data : in Data_Type;
         After     : in Boolean := False)
         return Guint;
      --  mapping: Connect gtksignal.h gtk_signal_connect
      --  mapping: Connect gtksignal.h gtk_signal_connect_after
   end Two_Callback;

   -----------------------------------------------------------------
   --  The following functions are for callbacks requiring no data to be
   --  passed to the callback
   -----------------------------------------------------------------

   generic
      type Widget_Type is new Object.Gtk_Object with private;

   package Void_Callback is

      type Callback is access procedure
        (Widget : in out Widget_Type'Class);

      function Connect
        (Obj    : in Widget_Type'Class;
         Name   : in String;
         Func   : in Callback;
         After  : in Boolean := False)
         return Guint;
      --  mapping: Connect gtksignal.h gtk_signal_connect
      --  mapping: Connect gtksignal.h gtk_signal_connect_after
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
         Slot_Object : in Widget_Type'Class;
         After       : in Boolean := False)
         return Guint;
      --  mapping: Connect gtksignal.h gtk_signal_connect_object
      --  mapping: Connect gtksignal.h gtk_signal_connect_object_after
   end Object_Callback;

   ------------------------------------------------------------------
   --  The following functions are for callbacks for tips query
   ------------------------------------------------------------------

   generic
      type Data_Type is new Object.Gtk_Object with private;

   package Tips_Query_Callback is
      type Callback is access procedure
        (Tips_Query  : in out Gtk.Tips_Query.Gtk_Tips_Query'Class;
         Widget      : in out Gtk.Widget.Gtk_Widget;
         Tip_Text    : in String;
         Tip_Private : in String;
         Data        : in out Data_Type);

      function Connect
        (Obj         : in Gtk.Tips_Query.Gtk_Tips_Query'Class;
         Name        : in String;
         Func        : in Callback;
         Data        : in Data_Type;
         After       : in Boolean := False)
         return Guint;
   end Tips_Query_Callback;

   ----------------------------------------------------------------
   --  The following function for connecting a default C callback
   ----------------------------------------------------------------

   function C_Unsafe_Connect (Object      : in Gtk.Object.Gtk_Object'Class;
                              Name        : in String;
                              Func        : in System.Address;
                              Slot_Object : in Gtk.Object.Gtk_Object'Class)
                              return Guint;
   --  See testgtk/create_ruler for an example how to use this...
   --  You should avoid using this whenever possible

   ------------------------------------------------------------------
   --  More general functions
   ------------------------------------------------------------------

   procedure Disconnect (Object     : in Gtk.Object.Gtk_Object'Class;
                         Handler_Id : in Guint);
   --  mapping: Disconnect gtksignal.h gtk_signal_disconnect

   procedure Emit_Stop_By_Name (Object : in Gtk.Object.Gtk_Object'Class;
                                Name   : in String);
   --  mapping: Emit_Stop_By_Name gtksignal.h gtk_signal_emit_stop_by_name

   procedure Handler_Block (Obj        : in Gtk.Object.Gtk_Object'Class;
                            Handler_Id : in Guint);
   --  mapping: Handler_Block gtksignal.h gtk_signal_handler_block

   procedure Handlers_Destroy (Obj : in Object.Gtk_Object'Class);
   --  mapping: Handlers_Destroy gtksignal.h gtk_signal_handlers_destroy

   procedure Handler_Unblock (Obj        : in Gtk.Object.Gtk_Object'Class;
                              Handler_Id : in Guint);
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
