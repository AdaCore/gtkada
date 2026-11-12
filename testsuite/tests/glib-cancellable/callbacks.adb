--  Callback bodies for the Glib.Cancellable test. These are library-level
--  Ada subprograms because the binding stores their addresses for later calls.

with Glib.Object;
with Glib.Test;
with System;

package body Callbacks is

   use type System.Address;

   ------------------------
   -- On_Basic_Callback --
   ------------------------

   procedure On_Basic_Callback is
   begin
      Basic_Callback_Count := Basic_Callback_Count + 1;
   end On_Basic_Callback;

   -------------------------
   -- On_Typed_Cancelled --
   -------------------------

   procedure On_Typed_Cancelled
     (Self : access Glib.Cancellable.Gcancellable_Record'Class)
   is
   begin
      --  The typed signal marshaller must return the object that emitted.
      Glib.Test.Assert_True
        (Glib.Object.Get_Object (Self) =
           Glib.Object.Get_Object (Expected_Cancellable));
      Typed_Signal_Count := Typed_Signal_Count + 1;
   end On_Typed_Cancelled;

   ----------------------
   -- On_Typed_Raises --
   ----------------------

   procedure On_Typed_Raises
     (Self : access Glib.Cancellable.Gcancellable_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "typed cancellable callback";
   end On_Typed_Raises;

   -----------------------
   -- On_Slot_Cancelled --
   -----------------------

   procedure On_Slot_Cancelled
     (Self : access Glib.Object.GObject_Record'Class)
   is
   begin
      --  A slot signal callback receives its designated GObject.
      Glib.Test.Assert_True
        (Glib.Object.Get_Object (Self) = Glib.Object.Get_Object (Expected_Slot));
      Slot_Signal_Count := Slot_Signal_Count + 1;
   end On_Slot_Cancelled;

   --------------------
   -- On_Slot_Raises --
   --------------------

   procedure On_Slot_Raises
     (Self : access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "slot cancellable callback";
   end On_Slot_Raises;

   -----------------------------
   -- Capture_Signal_Exception --
   -----------------------------

   procedure Capture_Signal_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
      pragma Unreferenced (Occurrence);
   begin
      Exception_Count := Exception_Count + 1;
   end Capture_Signal_Exception;

end Callbacks;
