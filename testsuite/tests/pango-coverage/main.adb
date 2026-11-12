--  Exercise Pango coverage construction, lookup, modification, combination,
--  copying, and reference counting. There is no matching C test to port.

with Ada.Command_Line;

with Glib;      use Glib;
with Glib.Test; use Glib.Test;
with Pango.Coverage;
with Pango.Enums;

procedure Main is
   use type Pango.Coverage.Pango_Coverage;
   use type Pango.Enums.Coverage_Level;

   procedure Test_Coverage_Lifecycle
   with Convention => C;

   --  Use both public constructors and check each operation against concrete
   --  Unicode code points so incorrect levels or copy behavior are visible.
   procedure Test_Coverage_Lifecycle is
      Coverage : Pango.Coverage.Pango_Coverage;
      Other    : Pango.Coverage.Pango_Coverage;
      Copy     : Pango.Coverage.Pango_Coverage;
      Ref      : Pango.Coverage.Pango_Coverage;
   begin
      --  Gdk_New creates an empty coverage map. Reinitializing it is a no-op
      --  and checks the already-created branch of Initialize.
      Pango.Coverage.Gdk_New (Coverage);
      Assert_True (Coverage /= null);
      Assert_True
        (Coverage.Get (65) = Pango.Enums.Pango_Coverage_None);
      Pango.Coverage.Initialize (Coverage);

      --  Pango_Coverage_New provides the function-form constructor. Max keeps
      --  the better level at each index in Coverage. Keep the assertion on
      --  an index that both maps already contain.
      Coverage.Set (65, Pango.Enums.Pango_Coverage_Fallback);
      Other := Pango.Coverage.Pango_Coverage_New;
      Assert_True (Other /= null);
      Other.Set (65, Pango.Enums.Pango_Coverage_Exact);
      Coverage.Max (Other);
      Assert_True
        (Coverage.Get (65) = Pango.Enums.Pango_Coverage_Exact);

      --  Copy owns an independent map; later changes to Coverage must not
      --  alter the copied values.
      Copy := Coverage.Copy;
      Assert_True (Copy /= null);
      Assert_True (Copy /= Coverage);
      Coverage.Set (65, Pango.Enums.Pango_Coverage_None);
      Assert_True
        (Coverage.Get (65) = Pango.Enums.Pango_Coverage_None);
      Assert_True
        (Copy.Get (65) = Pango.Enums.Pango_Coverage_Exact);

      --  Ref returns the same coverage with one additional owned reference.
      Ref := Coverage.Ref;
      Assert_True (Ref = Coverage);

      --  Release each owned C reference exactly once. Ref and Coverage refer
      --  to the same Ada wrapper, but they represent two C references.
      Pango.Coverage.Unref (Copy);
      Pango.Coverage.Unref (Other);
      Pango.Coverage.Unref (Coverage);
      Pango.Coverage.Unref (Ref);
   end Test_Coverage_Lifecycle;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/pango-coverage/lifecycle",
      Test_Coverage_Lifecycle'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
