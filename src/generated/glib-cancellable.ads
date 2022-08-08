------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  <description>
--  GCancellable is a thread-safe operation cancellation stack used throughout
--  GIO to allow for cancellation of synchronous and asynchronous operations.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib.Main;   use Glib.Main;
with Glib.Object; use Glib.Object;

package Glib.Cancellable is

   type Gcancellable_Record is new GObject_Record with null record;
   type Gcancellable is access all Gcancellable_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gcallback is access procedure;
   --  The type used for callback functions in structure definitions and
   --  function signatures. This doesn't mean that all callback functions must
   --  take no parameters and return void. The required signature of a callback
   --  function is determined by the context in which is used (e.g. the signal
   --  to which it is connected). Use G_CALLBACK to cast the callback function
   --  to a Gcallback.

   ------------------
   -- Constructors --
   ------------------

   procedure G_New (Self : out Gcancellable);
   --  Creates a new Glib.Cancellable.Gcancellable object.
   --  Applications that want to start one or more operations that should be
   --  cancellable should create a Glib.Cancellable.Gcancellable and pass it to
   --  the operations.
   --  One Glib.Cancellable.Gcancellable can be used in multiple consecutive
   --  operations or in multiple concurrent operations.

   procedure Initialize (Self : not null access Gcancellable_Record'Class);
   --  Creates a new Glib.Cancellable.Gcancellable object.
   --  Applications that want to start one or more operations that should be
   --  cancellable should create a Glib.Cancellable.Gcancellable and pass it to
   --  the operations.
   --  One Glib.Cancellable.Gcancellable can be used in multiple consecutive
   --  operations or in multiple concurrent operations.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gcancellable_New return Gcancellable;
   --  Creates a new Glib.Cancellable.Gcancellable object.
   --  Applications that want to start one or more operations that should be
   --  cancellable should create a Glib.Cancellable.Gcancellable and pass it to
   --  the operations.
   --  One Glib.Cancellable.Gcancellable can be used in multiple consecutive
   --  operations or in multiple concurrent operations.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_cancellable_get_type");

   -------------
   -- Methods --
   -------------

   procedure Cancel (Self : not null access Gcancellable_Record);
   --  Will set Cancellable to cancelled, and will emit the
   --  Glib.Cancellable.Gcancellable::cancelled signal. (However, see the
   --  warning about race conditions in the documentation for that signal if
   --  you are planning to connect to it.)
   --  This function is thread-safe. In other words, you can safely call it
   --  from a thread other than the one running the operation that was passed
   --  the Cancellable.
   --  If Cancellable is null, this function returns immediately for
   --  convenience.
   --  The convention within GIO is that cancelling an asynchronous operation
   --  causes it to complete asynchronously. That is, if you cancel the
   --  operation from the same thread in which it is running, then the
   --  operation's Gasync_Ready_Callback will not be invoked until the
   --  application returns to the main loop.

   function Connect
      (Self              : not null access Gcancellable_Record;
       Callback          : Gcallback;
       Data_Destroy_Func : Glib.G_Destroy_Notify_Address) return Gulong;
   --  Convenience function to connect to the
   --  Glib.Cancellable.Gcancellable::cancelled signal. Also handles the race
   --  condition that may happen if the cancellable is cancelled right before
   --  connecting.
   --  Callback is called at most once, either directly at the time of the
   --  connect if Cancellable is already cancelled, or when Cancellable is
   --  cancelled in some thread.
   --  Data_Destroy_Func will be called when the handler is disconnected, or
   --  immediately if the cancellable is already cancelled.
   --  See Glib.Cancellable.Gcancellable::cancelled for details on how to use
   --  this.
   --  Since GLib 2.40, the lock protecting Cancellable is not held when
   --  Callback is invoked. This lifts a restriction in place for earlier GLib
   --  versions which now makes it easier to write cleanup code that
   --  unconditionally invokes e.g. Glib.Cancellable.Cancel.
   --  Since: gtk+ 2.22
   --  "callback": The Gcallback to connect.
   --  "data_destroy_func": Free function for Data or null.

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Connect_User_Data is

      type Gcallback is access procedure (Data : User_Data_Type);
      --  The type used for callback functions in structure definitions and
      --  function signatures. This doesn't mean that all callback functions must
      --  take no parameters and return void. The required signature of a callback
      --  function is determined by the context in which is used (e.g. the signal
      --  to which it is connected). Use G_CALLBACK to cast the callback function
      --  to a Gcallback.

      function Connect
         (Self              : not null access Glib.Cancellable.Gcancellable_Record'Class;
          Callback          : Gcallback;
          Data              : User_Data_Type;
          Data_Destroy_Func : Glib.G_Destroy_Notify_Address) return Gulong;
      --  Convenience function to connect to the
      --  Glib.Cancellable.Gcancellable::cancelled signal. Also handles the
      --  race condition that may happen if the cancellable is cancelled right
      --  before connecting.
      --  Callback is called at most once, either directly at the time of the
      --  connect if Cancellable is already cancelled, or when Cancellable is
      --  cancelled in some thread.
      --  Data_Destroy_Func will be called when the handler is disconnected,
      --  or immediately if the cancellable is already cancelled.
      --  See Glib.Cancellable.Gcancellable::cancelled for details on how to
      --  use this.
      --  Since GLib 2.40, the lock protecting Cancellable is not held when
      --  Callback is invoked. This lifts a restriction in place for earlier
      --  GLib versions which now makes it easier to write cleanup code that
      --  unconditionally invokes e.g. Glib.Cancellable.Cancel.
      --  Since: gtk+ 2.22
      --  "callback": The Gcallback to connect.
      --  "data": Data to pass to Callback.
      --  "data_destroy_func": Free function for Data or null.

   end Connect_User_Data;

   procedure Disconnect
      (Self       : not null access Gcancellable_Record;
       Handler_Id : Gulong);
   --  Disconnects a handler from a cancellable instance similar to
   --  g_signal_handler_disconnect. Additionally, in the event that a signal
   --  handler is currently running, this call will block until the handler has
   --  finished. Calling this function from a
   --  Glib.Cancellable.Gcancellable::cancelled signal handler will therefore
   --  result in a deadlock.
   --  This avoids a race condition where a thread cancels at the same time as
   --  the cancellable operation is finished and the signal handler is removed.
   --  See Glib.Cancellable.Gcancellable::cancelled for details on how to use
   --  this.
   --  If Cancellable is null or Handler_Id is `0` this function does nothing.
   --  Since: gtk+ 2.22
   --  "handler_id": Handler id of the handler to be disconnected, or `0`.

   function Get_Fd
      (Self : not null access Gcancellable_Record) return Glib.Gint;
   --  Gets the file descriptor for a cancellable job. This can be used to
   --  implement cancellable operations on Unix systems. The returned fd will
   --  turn readable when Cancellable is cancelled.
   --  You are not supposed to read from the fd yourself, just check for
   --  readable status. Reading to unset the readable status is done with
   --  Glib.Cancellable.Reset.
   --  After a successful return from this function, you should use
   --  Glib.Cancellable.Release_Fd to free up resources allocated for the
   --  returned file descriptor.
   --  See also g_cancellable_make_pollfd.

   function Is_Cancelled
      (Self : not null access Gcancellable_Record) return Boolean;
   --  Checks if a cancellable job has been cancelled.

   procedure Pop_Current (Self : not null access Gcancellable_Record);
   --  Pops Cancellable off the cancellable stack (verifying that Cancellable
   --  is on the top of the stack).

   procedure Push_Current (Self : not null access Gcancellable_Record);
   --  Pushes Cancellable onto the cancellable stack. The current cancellable
   --  can then be received using Glib.Cancellable.Get_Current.
   --  This is useful when implementing cancellable operations in code that
   --  does not allow you to pass down the cancellable object.
   --  This is typically called automatically by e.g. Gfile.Gfile operations,
   --  so you rarely have to call this yourself.

   procedure Release_Fd (Self : not null access Gcancellable_Record);
   --  Releases a resources previously allocated by Glib.Cancellable.Get_Fd or
   --  g_cancellable_make_pollfd.
   --  For compatibility reasons with older releases, calling this function is
   --  not strictly required, the resources will be automatically freed when
   --  the Cancellable is finalized. However, the Cancellable will block scarce
   --  file descriptors until it is finalized if this function is not called.
   --  This can cause the application to run out of file descriptors when many
   --  GCancellables are used at the same time.
   --  Since: gtk+ 2.22

   procedure Reset (Self : not null access Gcancellable_Record);
   --  Resets Cancellable to its uncancelled state.
   --  If cancellable is currently in use by any cancellable operation then
   --  the behavior of this function is undefined.
   --  Note that it is generally not a good idea to reuse an existing
   --  cancellable for more operations after it has been cancelled once, as
   --  this function might tempt you to do. The recommended practice is to drop
   --  the reference to a cancellable after cancelling it, and let it die with
   --  the outstanding async operations. You should create a fresh cancellable
   --  for further async operations.

   function Set_Error_If_Cancelled
      (Self : not null access Gcancellable_Record) return Boolean;
   --  If the Cancellable is cancelled, sets the error to notify that the
   --  operation was cancelled.

   function Source_New
      (Self : not null access Gcancellable_Record) return Glib.Main.G_Source;
   --  Creates a source that triggers if Cancellable is cancelled and calls
   --  its callback of type Gcancellable_Source_Func. This is primarily useful
   --  for attaching to another (non-cancellable) source with
   --  g_source_add_child_source to add cancellability to it.
   --  For convenience, you can call this with a null
   --  Glib.Cancellable.Gcancellable, in which case the source will never
   --  trigger.
   --  The new Glib.Main.G_Source will hold a reference to the
   --  Glib.Cancellable.Gcancellable.
   --  Since: gtk+ 2.28

   ---------------
   -- Functions --
   ---------------

   function Get_Current return Gcancellable;
   --  Gets the top cancellable from the stack.

   -------------
   -- Signals --
   -------------

   type Cb_Gcancellable_Void is not null access procedure (Self : access Gcancellable_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Cancelled : constant Glib.Signal_Name := "cancelled";
   procedure On_Cancelled
      (Self  : not null access Gcancellable_Record;
       Call  : Cb_Gcancellable_Void;
       After : Boolean := False);
   procedure On_Cancelled
      (Self  : not null access Gcancellable_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the operation has been cancelled.
   --
   --  Can be used by implementations of cancellable operations. If the
   --  operation is cancelled from another thread, the signal will be emitted
   --  in the thread that cancelled the operation, not the thread that is
   --  running the operation.
   --
   --  Note that disconnecting from this signal (or any signal) in a
   --  multi-threaded program is prone to race conditions. For instance it is
   --  possible that a signal handler may be invoked even after a call to
   --  g_signal_handler_disconnect for that handler has already returned.
   --
   --  There is also a problem when cancellation happens right before
   --  connecting to the signal. If this happens the signal will unexpectedly
   --  not be emitted, and checking before connecting to the signal leaves a
   --  race condition where this is still happening.
   --
   --  In order to make it safe and easy to connect handlers there are two
   --  helper functions: Glib.Cancellable.Connect and
   --  Glib.Cancellable.Disconnect which protect against problems like this.
   --
   --  An example of how to us this: |[<!-- language="C" --> // Make sure we
   --  don't do unnecessary work if already cancelled if
   --  (g_cancellable_set_error_if_cancelled (cancellable, error)) return;
   --
   --  // Set up all the data needed to be able to handle cancellation // of
   --  the operation my_data = my_data_new (...);
   --
   --  id = 0; if (cancellable) id = g_cancellable_connect (cancellable,
   --  G_CALLBACK (cancelled_handler) data, NULL);
   --
   --  // cancellable operation here...
   --
   --  g_cancellable_disconnect (cancellable, id);
   --
   --  // cancelled_handler is never called after this, it is now safe // to
   --  free the data my_data_free (my_data); ]|
   --
   --  Note that the cancelled signal is emitted in the thread that the user
   --  cancelled from, which may be the main thread. So, the cancellable signal
   --  should not do something that can block.

end Glib.Cancellable;
