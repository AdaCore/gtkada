
with Gtk.Signal;

package body Gtk is

   -----------------
   -- Destroy --
   -----------------

   procedure Destroy (Obj : in Gtk_Object'Class)
   is
      procedure Internal  (Obj : in System.Address);
      pragma Import (C, Internal, "gtk_object_destroy");
   begin
      --  When the widget is destroyed, the callbacks are automatically
      --  destroyed too.
      --  In this binding, we have to make sure the memory allocated for
      --  the data is freed. The simple way is to call
      Gtk.Signal.Handlers_Destroy (Obj);

      Internal (Get_Object (Obj));
   end Destroy;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (Obj : in Gtk_Object'Class)
                        return System.Address is
   begin
      return Obj.Ptr;
   end Get_Object;

   ----------------
   -- Set_Object --
   ----------------

   procedure Set_Object (Obj   : in out Gtk_Object'Class;
                         Value : in     System.Address) is
      use type System.Address;
   begin
      Obj.Ptr := Value;
   end Set_Object;

   ------------------
   --  To_Boolean  --
   ------------------

   function To_Boolean (Value : in Gint) return Boolean is
   begin
      return Value /= 0;
   end To_Boolean;

   ---------------
   --  To_GInt  --
   ---------------

   function To_Gint (Bool : in Boolean) return Gint is
   begin
      if Bool then
         return 1;
      else
         return 0;
      end if;
   end To_Gint;

end Gtk;
