package body Glib is


   ------------------
   --  To_Boolean  --
   ------------------

   function To_Boolean (Value : in Gint) return Boolean is
   begin
      return Value /= 0;
   end To_Boolean;


   ---------------
   --  To_Gint  --
   ---------------

   function To_Gint (Bool : in Boolean) return Gint is
   begin
      if Bool then
         return 1;
      else
         return 0;
      end if;
   end To_Gint;

end Glib;
