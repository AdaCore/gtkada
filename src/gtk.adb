
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


   --------------
   -- Init --
   --------------

   procedure Init is
      procedure Internal;
      pragma Import (C, Internal, "ag_gtk_init");
   begin
      Internal;
   end Init;


   --------------
   -- Main --
   --------------

   procedure Main is
      procedure Internal;
      pragma Import (C, Internal, "gtk_main");
   begin
      Internal;
   end Main;


   -------------------
   -- Main_Quit --
   -------------------

   procedure Main_Quit is
      procedure Internal;
      pragma Import (C, Internal, "gtk_main_quit");
   begin
      Internal;
   end Main_Quit;


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

end Gtk;
