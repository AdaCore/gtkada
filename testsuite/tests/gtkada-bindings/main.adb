--  Gtkada.Bindings tests for pointer/string conversions, zero-terminated
--  arrays, generic null-address conversion and exception-handler fallback.

with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces.C;
with Interfaces.C.Strings;
with Glib;
with Glib.Test;
with Gtkada.Bindings;
with Gtkada.Types;
with System;

procedure Main is
   package C_Strings renames Interfaces.C.Strings;
   use Glib.Test;
   use type C_Strings.chars_ptr;
   use type Gtkada.Types.Chars_Ptr;
   use type Glib.Gint;
   use type System.Address;
   use type Interfaces.C.size_t;

   --  This instantiation checks both branches of the generic helper using
   --  actual aliased integers whose addresses match its documented input.
   function Address_Or_Null is new Gtkada.Bindings.Generic_To_Address_Or_Null
     (T => Glib.Gint, Null_T => 0);

   Handled : Boolean := False;

   procedure Test_String_Conversions
   with Convention => C;

   procedure Test_Nullable_Strings
   with Convention => C;

   procedure Test_Array_And_Address_Conversions
   with Convention => C;

   procedure Test_Exception_Fallback
   with Convention => C;

   procedure Record_Exception (Occurrence : Ada.Exceptions.Exception_Occurrence);
   procedure Raise_From_Handler
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   --  Empty Ada strings map to null pointers. Nonempty allocations are
   --  consumed by Value_And_Free, which uses the allocator paired with each
   --  pointer type.
   procedure Test_String_Conversions is
      C_Ptr : C_Strings.chars_ptr;
      G_Ptr : Gtkada.Types.Chars_Ptr;
   begin
      C_Ptr := Gtkada.Bindings.String_Or_Null ("");
      Assert_True (C_Ptr = C_Strings.Null_Ptr);
      Assert_Cmpstr_Eq
        (Gtkada.Bindings.Value_And_Free (C_Ptr), "");

      C_Ptr := Gtkada.Bindings.String_Or_Null ("C string");
      Assert_Cmpstr_Eq
        (Gtkada.Bindings.Value_And_Free (C_Ptr), "C string");

      G_Ptr := Gtkada.Bindings.String_Or_Null ("");
      Assert_True (G_Ptr = Gtkada.Types.Null_Ptr);
      Assert_Cmpstr_Eq
        (Gtkada.Bindings.Value_And_Free (G_Ptr), "");

      G_Ptr := Gtkada.Bindings.String_Or_Null ("GLib string");
      Assert_Cmpstr_Eq
        (Gtkada.Bindings.Value_And_Free (G_Ptr), "GLib string");
   end Test_String_Conversions;

   --  Each overload accepts its own null pointer type and returns an empty
   --  value; non-null strings pass through unchanged.
   procedure Test_Nullable_Strings is
      C_Ptr : C_Strings.chars_ptr := C_Strings.New_String ("signal");
      G_Ptr : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("signal");
      C_Text : constant String :=
        Gtkada.Bindings.Value_Allowing_Null (C_Strings.Null_Ptr);
      G_Text : constant String :=
        Gtkada.Bindings.Value_Allowing_Null (Gtkada.Types.Null_Ptr);
      C_Name : constant Glib.Signal_Name :=
        Gtkada.Bindings.Value_Allowing_Null (C_Strings.Null_Ptr);
      G_Name : constant Glib.Signal_Name :=
        Gtkada.Bindings.Value_Allowing_Null (Gtkada.Types.Null_Ptr);
      C_Nonnull_Name : constant Glib.Signal_Name :=
        Gtkada.Bindings.Value_Allowing_Null (C_Ptr);
      G_Nonnull_Name : constant Glib.Signal_Name :=
        Gtkada.Bindings.Value_Allowing_Null (G_Ptr);
   begin
      Assert_Cmpstr_Eq (C_Text, "");
      Assert_Cmpstr_Eq (G_Text, "");
      Assert_Cmpstr_Eq (String (C_Name), "");
      Assert_Cmpstr_Eq (String (G_Name), "");

      Assert_Cmpstr_Eq
        (Gtkada.Bindings.Value_Allowing_Null (C_Ptr), "signal");
      Assert_Cmpstr_Eq
        (Gtkada.Bindings.Value_Allowing_Null (G_Ptr), "signal");
      Assert_Cmpstr_Eq
        (String (C_Nonnull_Name), "signal");
      Assert_Cmpstr_Eq
        (String (G_Nonnull_Name), "signal");

      C_Strings.Free (C_Ptr);
      Gtkada.Types.g_free (G_Ptr);
   end Test_Nullable_Strings;

   --  The array helper stops at its zero terminator and returns the prefix
   --  through that terminator. The address helper maps only the null value.
   procedure Test_Array_And_Address_Conversions is
      function Allocate_Array
        (Size : Interfaces.C.size_t) return System.Address;
      pragma Import (C, Allocate_Array, "g_malloc");

      procedure Free_Array (Mem : System.Address);
      pragma Import (C, Free_Array, "g_free");

      Bytes_Per_Gint : constant Interfaces.C.size_t :=
        Interfaces.C.size_t (Glib.Gint'Size / System.Storage_Unit);
      Raw : System.Address :=
        Allocate_Array (Bytes_Per_Gint * Interfaces.C.size_t (3));
      Values : Gtkada.Bindings.Gint_Arrays.Unbounded_Array_Access :=
        Gtkada.Bindings.Gint_Arrays.Convert (Raw);
      Converted : Glib.Gint_Array (1 .. 3);
      Zero : aliased Glib.Gint := 0;
      Nonzero : aliased Glib.Gint := 7;
   begin
      Values (1) := 4;
      Values (2) := 9;
      Values (3) := 0;
      Converted := Gtkada.Bindings.To_Gint_Array_Zero_Terminated (Values);
      Assert_Cmpint_Eq (Converted (1), 4);
      Assert_Cmpint_Eq (Converted (2), 9);
      Assert_Cmpint_Eq (Converted (3), 0);
      Free_Array (Raw);
      Values := null;
      Raw := System.Null_Address;

      Assert_True
        (Address_Or_Null (Zero'Address) = System.Null_Address);
      Assert_True (Address_Or_Null (Nonzero'Address) = Nonzero'Address);
   end Test_Array_And_Address_Conversions;

   --  A user handler that raises must not unwind through the C callback
   --  boundary: Process_Exception reports that secondary exception itself.
   procedure Test_Exception_Fallback is
   begin
      Handled := False;
      Gtkada.Bindings.Set_On_Exception (Raise_From_Handler'Unrestricted_Access);
      begin
         raise Constraint_Error with "callback exception";
      exception
         when Occurrence : others =>
            Gtkada.Bindings.Process_Exception (Occurrence);
      end;
      Assert_True (Handled);

      --  Restore a non-raising handler so the test leaves the package in a
      --  usable state even if more cases are registered in the future.
      Gtkada.Bindings.Set_On_Exception (Record_Exception'Unrestricted_Access);
   end Test_Exception_Fallback;

   --  This handler confirms that the exception reached the configured hook.
   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Handled := True;
   end Record_Exception;

   --  Raising here deliberately drives Process_Exception's protective
   --  fallback path, which writes the handler error to standard error.
   procedure Raise_From_Handler
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Handled := True;
      raise Program_Error with "handler failure";
   end Raise_From_Handler;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gtkada-bindings/string-conversions",
      Test_String_Conversions'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gtkada-bindings/nullable-strings",
      Test_Nullable_Strings'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gtkada-bindings/array-address-conversions",
      Test_Array_And_Address_Conversions'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gtkada-bindings/exception-fallback",
      Test_Exception_Fallback'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
