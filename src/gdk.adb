package body Gdk is


   ------------------
   --  Get_Object  --
   ------------------

   function Get_Object (Object : in Root_Type'Class) return System.Address is
   begin
      return Object.Ptr;
   end Get_Object;


   ------------------
   --  Set_Object  --
   ------------------

   procedure Set_Object (Object : in out Root_Type'Class;
                         Value  : in     System.Address) is
   begin
      Object.Ptr := Value;
   end Set_Object;

end Gdk;
