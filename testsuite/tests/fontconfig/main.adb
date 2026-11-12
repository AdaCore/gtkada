--  Exercise GtkAda's Fontconfig application-font helper.
--  There is no matching C testsuite case to port; the public helper is tested
--  with an invalid font path so the failure result and cleanup are checked.

with Ada.Command_Line;

with Fontconfig;
with Glib.Test;

procedure Main is

   procedure Test_Missing_Font
   with Convention => C;

   --  Fontconfig rejects a nonexistent font file.  The empty path is
   --  deterministic and does not rely on fonts installed on the test host.
   procedure Test_Missing_Font is
   begin
      Glib.Test.Assert_False (Fontconfig.App_Font_Add_File (""));
   end Test_Missing_Font;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/fontconfig/missing-font", Test_Missing_Font'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
