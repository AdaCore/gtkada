--  Exercise Cairo region emptiness and point containment. No matching region
--  tests are present in testsuite/c_tests/.

with Ada.Command_Line;

with Cairo;
with Cairo.Region;
with Glib; use Glib;
with Glib.Test; use Glib.Test;

use type Cairo.Cairo_Status;

procedure Main is

   --  Empty and rectangle regions provide deterministic true and false cases
   --  for both region queries without requiring a display or a Cairo surface.
   procedure Test_Queries
   with Convention => C;

   procedure Test_Queries is
      Rectangle : aliased Cairo.Region.Cairo_Rectangle_Int :=
        (X => 10, Y => 20, Width => 30, Height => 40);
      Empty_Region : constant Cairo.Region.Cairo_Region :=
        Cairo.Region.Create;
      Filled_Region : constant Cairo.Region.Cairo_Region :=
        Cairo.Region.Create_Rectangle (Rectangle'Access);
   begin
      --  Cairo returns valid regions with success status for these inputs.
      Assert_True
        (Cairo.Region.Status (Empty_Region) = Cairo.Cairo_Status_Success);
      Assert_True
        (Cairo.Region.Status (Filled_Region) = Cairo.Cairo_Status_Success);

      --  A newly created region has no area; adding a rectangle makes it nonempty.
      Assert_True (Cairo.Region.Is_Empty (Empty_Region));
      Assert_True (not Cairo.Region.Is_Empty (Filled_Region));

      --  Containment includes points inside the rectangle and excludes points
      --  outside it, including a point on its right edge.
      Assert_True (Cairo.Region.Contains_Point (Filled_Region, 10, 20));
      Assert_True (Cairo.Region.Contains_Point (Filled_Region, 39, 59));
      Assert_True (not Cairo.Region.Contains_Point (Filled_Region, 40, 30));
      Assert_True (not Cairo.Region.Contains_Point (Empty_Region, 10, 20));

      --  Release both native regions after all queries have been checked.
      Cairo.Region.Destroy (Filled_Region);
      Cairo.Region.Destroy (Empty_Region);
   end Test_Queries;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/cairo/region/queries", Test_Queries'Unrestricted_Access);

   --  Return the GLib test runner's status to the testsuite driver.
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
