
with System;
with Ada.Command_Line;
with Gtk.Signal;
with Interfaces.C.Strings;

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
      use Ada.Command_Line;
      procedure Internal (Arg  : System.Address;
                          Argv : System.Address);
      pragma Import (C, Internal, "gtk_init");

      Argc : Integer := Ada.Command_Line.Argument_Count;
      type Argv_T is array (1 .. Argc) of Interfaces.C.Strings.chars_ptr;
      Argv : Argv_T;

   begin
      for I in 1 .. Argc loop
         Argv (I) := Interfaces.C.Strings.New_String (Argument (I));
      end loop;
      if Argc = 0 then
         Internal (Argc'Address, System.Null_Address);
      else
         Internal (Argc'Address, Argv (1)'Address);
      end if;

      for I in 1 .. Argc loop
         Interfaces.C.Strings.Free (Argv (I));
      end loop;
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
