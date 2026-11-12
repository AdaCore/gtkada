--  Test Gtk.Bin_Layout's constructors and its idempotent initialization.
--  There is no gtk_bin_layout_* test in the C testsuite to port.

with Ada.Command_Line;
with Glib;
with Glib.Object;
with Glib.Test;
with Gtk.Bin_Layout;
with Gtk.Main;
with System;

procedure Main is
   use type Glib.GType;
   use type System.Address;

   procedure Test_Constructors with Convention => C;

   --  Both constructor forms must produce a native GtkBinLayout object.
   procedure Test_Constructors is
      By_Function  : constant Gtk.Bin_Layout.Gtk_Bin_Layout :=
        Gtk.Bin_Layout.Gtk_Bin_Layout_New;
      By_Procedure : Gtk.Bin_Layout.Gtk_Bin_Layout;
      Function_Object  : constant System.Address :=
        Glib.Object.Get_Object (By_Function);
   begin
      Gtk.Bin_Layout.Gtk_New (By_Procedure);

      --  Check the object type and that the two constructors created objects.
      Glib.Test.Assert_True (Gtk.Bin_Layout.Get_Type /= 0);
      Glib.Test.Assert_True
        (By_Function.Get_Type = Gtk.Bin_Layout.Get_Type);
      Glib.Test.Assert_True
        (By_Procedure.Get_Type = Gtk.Bin_Layout.Get_Type);
      Glib.Test.Assert_True
        (Function_Object /= System.Null_Address);
      Glib.Test.Assert_True
        (Glib.Object.Get_Object (By_Procedure) /= System.Null_Address);
      Glib.Test.Assert_True
        (Function_Object /= Glib.Object.Get_Object (By_Procedure));

      --  Constructors initialize fresh objects; these calls cover the
      --  documented already-created path and must preserve object identity.
      Gtk.Bin_Layout.Initialize (By_Function);
      Gtk.Bin_Layout.Initialize (By_Procedure);
      Glib.Test.Assert_True
        (Glib.Object.Get_Object (By_Function) = Function_Object);
      Glib.Test.Assert_True
        (By_Procedure.Get_Type = Gtk.Bin_Layout.Get_Type);
   end Test_Constructors;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/bin-layout/constructors", Test_Constructors'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
