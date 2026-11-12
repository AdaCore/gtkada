--  The callbacks record dispatch while leaving ownership of the sample alone.
with System;

package body Glib_Boxed_Callbacks is
   use type System.Address;

   --  Returning the pointer unchanged keeps this callback safe for stack data.
   function Copy_Boxed (Boxed : System.Address) return System.Address is
   begin
      Copy_Calls := Copy_Calls + 1;
      return Boxed;
   end Copy_Boxed;

   --  The sample value remains stack-owned, so free only records the call.
   procedure Free_Boxed (Boxed : System.Address) is
   begin
      if Boxed /= System.Null_Address then
         Free_Calls := Free_Calls + 1;
      end if;
   end Free_Boxed;
end Glib_Boxed_Callbacks;
