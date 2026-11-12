--  C-compatible callbacks used to test GLib boxed type registration.
with System;

package Glib_Boxed_Callbacks is
   --  These counters expose callback invocation to the Ada test assertions.
   Copy_Calls : Natural := 0;
   Free_Calls : Natural := 0;

   --  Library-level callbacks can be passed as C function pointers.
   function Copy_Boxed (Boxed : System.Address) return System.Address;
   pragma Convention (C, Copy_Boxed);

   procedure Free_Boxed (Boxed : System.Address);
   pragma Convention (C, Free_Boxed);
end Glib_Boxed_Callbacks;
