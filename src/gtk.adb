
with Gtk.Signal;

package body Gtk is

   type Gtk_Object_Internal is new Integer;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (Object : in Gtk_Object'Class)
                        return System.Address is
   begin
      return Object.Ptr;
   end Get_Object;

   -----------------
   -- Gtk_Destroy --
   -----------------

   procedure Gtk_Destroy (Object : in Gtk_Object'Class)
   is
      procedure Internal  (Object : in System.Address);
      pragma Import (C, Internal, "gtk_object_destroy");
   begin
      --  When the widget is destroyed, the callbacks are automatically
      --  destroyed too.
      --  In this binding, we have to make sure the memory allocated for
      --  the data is freed. The simple way is to call
      Gtk.Signal.Gtk_Handlers_Destroy (Object);

      Internal (Get_Object (Object));
   end Gtk_Destroy;

   --------------
   -- Gtk_Init --
   --------------

   procedure Gtk_Init is
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
   end Gtk_Init;

   --------------
   -- Gtk_Main --
   --------------

   procedure Gtk_Main is
      procedure Internal;
      pragma Import (C, Internal, "gtk_main");
   begin
      Internal;
   end Gtk_Main;

   -------------------
   -- Gtk_Main_Quit --
   -------------------

   procedure Gtk_Main_Quit is
      procedure Internal;
      pragma Import (C, Internal, "gtk_main_quit");
   begin
      Internal;
   end Gtk_Main_Quit;

   ----------------
   -- Set_Object --
   ----------------

   procedure Set_Object (Object : in out Gtk_Object'Class;
                         Value  : in     System.Address) is
      use type System.Address;
   begin
      Object.Ptr := Value;
   end Set_Object;

end Gtk;
