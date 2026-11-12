--  Exercise Gdk.Cicp_Params constructors, properties, and color-state building.
--  There is no corresponding C testsuite case to port; these checks cover the
--  documented Ada bindings without creating a display or any widgets.

with Ada.Command_Line;
with System;

with Gdk.Cicp_Params;
with Gdk.Color_State;
with Glib;
with Glib.Error;
with Glib.Object;
with Glib.Test;

procedure Main is
   use Glib.Test;
   use type Gdk.Cicp_Params.Gdk_Cicp_Params;
   use type Gdk.Cicp_Params.Gdk_Cicp_Range;
   use type Gdk.Color_State.Gdk_Color_State;
   use type Glib.Error.GError;
   use type System.Address;

   procedure Test_Constructors_And_Properties with Convention => C;
   procedure Test_Build_Color_State with Convention => C;

   --  Verify both constructor forms and round-trip every scalar property.
   procedure Test_Constructors_And_Properties is
      Params       : constant Gdk.Cicp_Params.Gdk_Cicp_Params :=
        Gdk.Cicp_Params.Gdk_Cicp_Params_New;
      By_Procedure : Gdk.Cicp_Params.Gdk_Cicp_Params;
      Original     : System.Address;
   begin
      Assert_True (Glib.Object.Is_Created (Params.all));

      --  Initialize is documented to leave an already-created object intact.
      Original := Params.Get_Object;
      Gdk.Cicp_Params.Initialize (Params);
      Assert_True (Params.Get_Object = Original);

      --  The out-parameter constructor is a separate generated code path.
      Gdk.Cicp_Params.Gdk_New (By_Procedure);
      Assert_True (Glib.Object.Is_Created (By_Procedure.all));

      --  The property wrappers preserve supported values in both directions.
      Params.Set_Color_Primaries (9);
      Params.Set_Matrix_Coefficients (0);
      Params.Set_Range (Gdk.Cicp_Params.Gdk_Cicp_Range_Full);
      Params.Set_Transfer_Function (8);
      Assert_Cmpuint_Eq (Params.Get_Color_Primaries, 9);
      Assert_Cmpuint_Eq (Params.Get_Matrix_Coefficients, 0);
      Assert_True
        (Params.Get_Range = Gdk.Cicp_Params.Gdk_Cicp_Range_Full);
      Assert_Cmpuint_Eq (Params.Get_Transfer_Function, 8);

      --  Each constructor returns an owned GObject reference.
      Glib.Object.Unref (Glib.Object.GObject (By_Procedure));
      Glib.Object.Unref (Glib.Object.GObject (Params));
   end Test_Constructors_And_Properties;

   --  Unspecified values exercise the error return; a supported tuple exercises
   --  the successful conversion of the returned C object to a color state.
   procedure Test_Build_Color_State is
      Params : constant Gdk.Cicp_Params.Gdk_Cicp_Params :=
        Gdk.Cicp_Params.Gdk_Cicp_Params_New;
      Error : Glib.Error.GError;
      State : Gdk.Color_State.Gdk_Color_State;
   begin
      State := Params.Build_Color_State (Error);
      Assert_True (State = Gdk.Color_State.Null_Gdk_Color_State);
      Assert_True (Error /= null);
      Glib.Error.Error_Free (Error);

      --  This tuple is Rec. 2100 linear (primaries / transfer / matrix / range).
      Params.Set_Color_Primaries (9);
      Params.Set_Transfer_Function (8);
      Params.Set_Matrix_Coefficients (0);
      Params.Set_Range (Gdk.Cicp_Params.Gdk_Cicp_Range_Full);
      State := Params.Build_Color_State (Error);
      Assert_True (Error = null);
      Assert_True (State /= Gdk.Color_State.Null_Gdk_Color_State);

      --  Build_Color_State transfers the new color state to the caller.
      Gdk.Color_State.Unref (State);
      Glib.Object.Unref (Glib.Object.GObject (Params));
   end Test_Build_Color_State;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gdk/cicp-params/constructors-and-properties",
      Test_Constructors_And_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk/cicp-params/build-color-state",
      Test_Build_Color_State'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
