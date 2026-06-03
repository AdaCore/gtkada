with Glib.Test; use Glib.Test;
with Ada.Command_Line;
with Gtk.Main;

procedure Main is

   procedure Test_Version
   with Convention => C;

   procedure Test_Version is
   begin
      --  Check that the major version = 4
      Assert_Cmpuint_Eq (Gtk.Main.Get_Major_Version, 4);
      --  Check that the minor version is >= 22
      Assert_Cmpuint_Ge (Gtk.Main.Get_Minor_Version, 22);
   end Test_Version;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/main/version", Test_Version'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
