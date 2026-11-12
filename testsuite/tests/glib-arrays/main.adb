--  Exercise Glib.Arrays.From_Object_Free, a GtkAda helper with no matching
--  C test. Check that it copies the public GArray fields before freeing the
--  C-allocated wrapper, while leaving the separately owned data untouched.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with Interfaces.C;
with System;

with Glib.Arrays;
with Glib.Test;
with Gtkada.Types;

procedure Main is
   use Glib.Test;
   use type Gtkada.Types.Chars_Ptr;

   --  From_Object_Free expects a GArray-sized block compatible with g_free.
   function G_Malloc (Size : Interfaces.C.size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   --  Reinterpret only the address returned by g_malloc as the C record.
   function To_Garray_Access is new Ada.Unchecked_Conversion
     (System.Address, Glib.Arrays.Garray_Access);

   procedure Test_Copy_And_Free with Convention => C;

   --  The helper frees the record storage but the array's data stays owned by
   --  its caller, so the test checks and then releases that payload itself.
   procedure Test_Copy_And_Free is
      Data : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("payload");
      Raw  : constant System.Address := G_Malloc
        (Interfaces.C.size_t (Glib.Arrays.Garray'Size / System.Storage_Unit));
      Source : constant Glib.Arrays.Garray_Access := To_Garray_Access (Raw);
      Result : Glib.Arrays.Garray;
   begin
      Source.all := (Data => Data, Len => 3);
      Result := Glib.Arrays.From_Object_Free (Source);

      --  Check both the copied length and that the shallow copy retains data.
      Assert_Cmpuint_Eq (Result.Len, 3);
      Assert_True (Result.Data = Data);
      Assert_Cmpstr_Eq (Gtkada.Types.Value (Result.Data), "payload");

      Gtkada.Types.Free (Data);
   end Test_Copy_And_Free;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/glib-arrays/copy-and-free", Test_Copy_And_Free'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
