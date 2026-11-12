--  Exercise the standard Gdk.Color_State values and their ownership helpers.
--  There is no direct C testsuite case for these APIs, so this checks the
--  documented properties in Ada without requiring a display.

with Ada.Command_Line;

with Gdk.Cicp_Params;
with Gdk.Color_State;
with Glib.Object;
with Glib.Test;

procedure Main is
   use Glib.Test;
   use type Gdk.Cicp_Params.Gdk_Cicp_Params;
   use type Gdk.Color_State.Gdk_Color_State;

   procedure Test_Standard_States with Convention => C;
   procedure Test_Object_Free with Convention => C;

   --  From_Object_Free copies the wrapped state and frees its heap wrapper;
   --  this confirms that the returned state remains usable afterwards.
   procedure Test_Object_Free is
      Wrapped : constant access Gdk.Color_State.Gdk_Color_State :=
        new Gdk.Color_State.Gdk_Color_State'(Gdk.Color_State.Get_Srgb);
      State : constant Gdk.Color_State.Gdk_Color_State :=
        Gdk.Color_State.From_Object_Free (Wrapped);
      Expected : constant Gdk.Color_State.Gdk_Color_State :=
        Gdk.Color_State.Get_Srgb;
   begin
      Assert_True (Gdk.Color_State.Equal (State, Expected));
      Gdk.Color_State.Unref (Expected);
      Gdk.Color_State.Unref (State);
   end Test_Object_Free;

   --  Every named color space is available, and repeated requests describe
   --  equal and equivalent states. Distinct standard spaces remain distinct.
   procedure Test_Standard_States is
      Oklab : Gdk.Color_State.Gdk_Color_State;
      Oklch : Gdk.Color_State.Gdk_Color_State;
      Rec_Linear : Gdk.Color_State.Gdk_Color_State;
      Rec_Pq : Gdk.Color_State.Gdk_Color_State;
      Srgb : Gdk.Color_State.Gdk_Color_State;
      Srgb_Linear : Gdk.Color_State.Gdk_Color_State;
      Same_Srgb : Gdk.Color_State.Gdk_Color_State;
      Held_Srgb : Gdk.Color_State.Gdk_Color_State;
      CICP : Gdk.Cicp_Params.Gdk_Cicp_Params;
   begin
      Oklab := Gdk.Color_State.Get_Oklab;
      Oklch := Gdk.Color_State.Get_Oklch;
      Rec_Linear := Gdk.Color_State.Get_Rec2100_Linear;
      Rec_Pq := Gdk.Color_State.Get_Rec2100_Pq;
      Srgb := Gdk.Color_State.Get_Srgb;
      Srgb_Linear := Gdk.Color_State.Get_Srgb_Linear;
      Same_Srgb := Gdk.Color_State.Get_Srgb;
      Held_Srgb := Gdk.Color_State.Ref (Srgb);
      CICP := Srgb.Create_Cicp_Params;
      --  The getters return usable owned objects for all six documented spaces.
      Assert_True (Oklab /= Gdk.Color_State.Null_Gdk_Color_State);
      Assert_True (Oklch /= Gdk.Color_State.Null_Gdk_Color_State);
      Assert_True (Rec_Linear /= Gdk.Color_State.Null_Gdk_Color_State);
      Assert_True (Rec_Pq /= Gdk.Color_State.Null_Gdk_Color_State);
      Assert_True (Srgb /= Gdk.Color_State.Null_Gdk_Color_State);
      Assert_True (Srgb_Linear /= Gdk.Color_State.Null_Gdk_Color_State);

      --  The returned reference denotes the same state and has its own ref.
      Assert_True (Gdk.Color_State.Equal (Srgb, Same_Srgb));
      Assert_True (Gdk.Color_State.Equivalent (Srgb, Same_Srgb));
      Assert_True (Gdk.Color_State.Equal (Srgb, Held_Srgb));
      Assert_False (Gdk.Color_State.Equal (Srgb, Srgb_Linear));
      Assert_False (Gdk.Color_State.Equivalent (Srgb, Srgb_Linear));
      Assert_False (Gdk.Color_State.Equivalent (Rec_Linear, Rec_Pq));

      --  sRGB has a documented CICP representation; release the transferred
      --  GObject reference returned by Create_Cicp_Params.
      Assert_True (CICP /= null);
      Glib.Object.Unref (Glib.Object.GObject (CICP));

      --  Each getter and Ref result transfers one reference to this test.
      Gdk.Color_State.Unref (Held_Srgb);
      Gdk.Color_State.Unref (Same_Srgb);
      Gdk.Color_State.Unref (Srgb_Linear);
      Gdk.Color_State.Unref (Srgb);
      Gdk.Color_State.Unref (Rec_Pq);
      Gdk.Color_State.Unref (Rec_Linear);
      Gdk.Color_State.Unref (Oklch);
      Gdk.Color_State.Unref (Oklab);
   end Test_Standard_States;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gdk/color-state/object-free",
      Test_Object_Free'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk/color-state/standard-states",
      Test_Standard_States'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
