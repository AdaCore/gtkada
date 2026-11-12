--  Library-level callbacks used by the cancellable tests. Keeping these
--  routines outside Main avoids passing nested Ada subprogram trampolines
--  through the binding's address-based callback adapters.

with Ada.Exceptions;
with Glib.Cancellable;
with Glib.Object;

package Callbacks is

   --  These counters record each callback route for Glib.Test assertions.
   Basic_Callback_Count : Natural := 0;
   Data_Callback_Count  : Natural := 0;
   Last_Callback_Data   : Integer := 0;
   Typed_Signal_Count   : Natural := 0;
   Slot_Signal_Count    : Natural := 0;
   Exception_Count      : Natural := 0;

   --  Observe the generic Destroy contract for glib-cancellable.closure-destroy.
   User_Data_Callback_Count : Natural := 0;
   User_Data_Destroy_Count  : Natural := 0;
   Destroyed_User_Data      : Integer := 0;

   procedure On_User_Data_Callback (Data : Integer);
   procedure Destroy_User_Data (Data : in out Integer);

   --  Expected objects let each marshaller verify the identity it passes.
   Expected_Cancellable : Glib.Cancellable.Gcancellable := null;
   Expected_Slot        : Glib.Cancellable.Gcancellable := null;

   procedure On_Basic_Callback;
   --  A library-level callback also lets Connect_User_Data store a plain
   --  Ada subprogram address without a nested subprogram trampoline.
   procedure On_Data_Callback (Data : Integer);
   procedure On_Typed_Cancelled
     (Self : access Glib.Cancellable.Gcancellable_Record'Class);
   procedure On_Typed_Raises
     (Self : access Glib.Cancellable.Gcancellable_Record'Class);
   procedure On_Slot_Cancelled
     (Self : access Glib.Object.GObject_Record'Class);
   procedure On_Slot_Raises
     (Self : access Glib.Object.GObject_Record'Class);
   procedure Capture_Signal_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

end Callbacks;
