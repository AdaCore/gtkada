--  Gdk.Content_Provider.Get_Value: the only GError-throwing method on a
--  content provider built directly from a GValue (Gdk_New_For_Value), so no
--  display/clipboard/DnD machinery is needed.
--
--  No widget is created, so this test needs no display.

with Ada.Command_Line;

with Glib;                use Glib;
with Glib.Error;          use Glib.Error;
with Glib.Values;         use Glib.Values;
with Glib.Test;           use Glib.Test;
with Gdk.Content_Provider; use Gdk.Content_Provider;

procedure Main is

   procedure Test_Get_Value_Success
   with Convention => C;

   procedure Test_Get_Value_Invalid_Target
   with Convention => C;

   -----------------------------
   -- Test_Get_Value_Success --
   -----------------------------

   procedure Test_Get_Value_Success is
      Source   : GValue;
      Target   : GValue;
      Provider : Gdk_Content_Provider;
      Error    : Glib.Error.GError;
      Success  : Boolean;
   begin
      Init_Set_Int (Source, 42);
      Gdk_New_For_Value (Provider, Source);

      Init (Target, GType_Int);
      Success := Get_Value (Provider, Target, Error);

      Assert_True (Error = null);
      Assert_True (Success);
      Assert_Cmpint_Eq (Get_Int (Target), 42);

      Unref (Provider);
   end Test_Get_Value_Success;

   --------------------------------------
   -- Test_Get_Value_Invalid_Target --
   --------------------------------------

   procedure Test_Get_Value_Invalid_Target is
      Source   : GValue;
      Target   : GValue;
      Provider : Gdk_Content_Provider;
      Error    : Glib.Error.GError;
      Success  : Boolean;
   begin
      Init_Set_Int (Source, 42);
      Gdk_New_For_Value (Provider, Source);

      --  A validly-initialised (G_IS_VALUE-passing) but incompatible
      --  target: an Int can never transform into a GObject reference.
      Init (Target, GType_Object);
      Success := Get_Value (Provider, Target, Error);

      Assert_False (Success);
      Assert_True (Error /= null);

      Unref (Provider);
   end Test_Get_Value_Invalid_Target;

begin
   Glib.Test.Init;

   Glib.Test.Add_Func
     ("/gdk-content-provider/get-value-success",
      Test_Get_Value_Success'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-content-provider/get-value-invalid-target",
      Test_Get_Value_Invalid_Target'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
