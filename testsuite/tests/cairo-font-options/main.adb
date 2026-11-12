--  Exercise Cairo.Font_Options.Equal, for which no matching C tests exist.
--  The cases below use only Cairo's in-memory font option objects, so they
--  need neither a printer nor a display.

with Ada.Command_Line;

with Cairo;
with Cairo.Font_Options;
with Glib.Test;

procedure Main is

   --  Equal must accept identical values, even when they live in distinct
   --  Cairo objects, and reject objects whose antialias setting differs.
   procedure Test_Equality
   with Convention => C;

   procedure Test_Equality is
      Defaults : constant Cairo.Cairo_Font_Options :=
        Cairo.Font_Options.Create;
      Another_Defaults : constant Cairo.Cairo_Font_Options :=
        Cairo.Font_Options.Create;
      Defaults_Copy : constant Cairo.Cairo_Font_Options :=
        Cairo.Font_Options.Copy (Defaults);
      Changed : constant Cairo.Cairo_Font_Options :=
        Cairo.Font_Options.Copy (Defaults);
   begin
      --  Fresh objects start with matching default values; copying preserves
      --  those values without requiring pointer identity.
      Glib.Test.Assert_True (Cairo.Font_Options.Equal (Defaults, Defaults));
      Glib.Test.Assert_True
        (Cairo.Font_Options.Equal (Defaults, Another_Defaults));
      Glib.Test.Assert_True
        (Cairo.Font_Options.Equal (Defaults, Defaults_Copy));

      --  Changing one option makes the objects unequal in both argument
      --  orders, exercising the false result from Cairo's equality routine.
      Cairo.Font_Options.Set_Antialias
        (Changed, Cairo.Cairo_Antialias_None);
      Glib.Test.Assert_True (not Cairo.Font_Options.Equal (Defaults, Changed));
      Glib.Test.Assert_True (not Cairo.Font_Options.Equal (Changed, Defaults));

      --  Destroy each owned Cairo object after all comparisons are complete.
      Cairo.Font_Options.Destroy (Changed);
      Cairo.Font_Options.Destroy (Defaults_Copy);
      Cairo.Font_Options.Destroy (Another_Defaults);
      Cairo.Font_Options.Destroy (Defaults);
   end Test_Equality;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/cairo/font-options/equality", Test_Equality'Unrestricted_Access);

   --  Return GLib's test result to the testsuite driver.
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
