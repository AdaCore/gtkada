
with Gtk.Signal;

package body Gtk is

   type Object_Internal is new Integer;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (Obj : in Object'Class)
                        return System.Address is
   begin
      return Obj.Ptr;
   end Get_Object;

   -----------------
   -- Destroy --
   -----------------

   procedure Destroy (Obj : in Object'Class)
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

   --------------
   -- Init --
   --------------

   procedure Init is
      procedure Internal (Arg  : System.Address;
                          Argv : System.Address);
      pragma Import (C, Internal, "gtk_init");

      Argc : Integer := 0;
   begin
      --  FIXME we have to pass the command line arguments to gtk_init,
      --  FIXME so that -display,... are parsed.
      --  FIXME We then have to rewrite some of Ada.Command_Line
      --  FIXME functions, so that we skip already parsed arguments
      Internal (Argc'Address, System.Null_Address);
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

   procedure Set_Object (Obj   : in out Object'Class;
                         Value : in     System.Address) is
      use type System.Address;
   begin
      Obj.Ptr := Value;
   end Set_Object;

end Gtk;
