with System;

package Gdk is

   type Root_Type is abstract tagged private;


   --  The following 2 services are for INTERNAL use only. They are not
   --  declared inside the private part for visibilty issues. Do NOT use them
   --  outside of the binding.
   --
   function Get_Object (Object : in Root_Type'Class) return System.Address;
   pragma Inline (Get_Object);

   procedure Set_Object (Object : in out Root_Type'Class;
                         Value  : in     System.Address);
   pragma Inline (Set_Object);

private

   type Root_Type is abstract tagged
     record
        Ptr : System.Address := System.Null_Address;
     end record;

end Gdk;
