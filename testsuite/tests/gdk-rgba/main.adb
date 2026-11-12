--  Exercise Gdk.RGBA parsing, predicates, null conversion, and the temporary
--  wrapper ownership helper. No C test directly covers these RGBA functions.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with Interfaces.C;
with System;

with Gdk.RGBA;
with Glib;
with Glib.Generic_Properties;
with Glib.Test;
with Glib.Values;

procedure Main is
   use Glib.Test;
   use type Glib.GType;
   use type Interfaces.C.C_float;
   use type System.Address;

   procedure Test_Predicates with Convention => C;
   procedure Test_Parse_And_String with Convention => C;
   procedure Test_Null_Conversion with Convention => C;
   procedure Test_Temporary_Wrapper with Convention => C;

   --  From_Object_Free calls g_free, so its Ada record must be allocated by
   --  GLib. The unchecked conversion gives that correctly aligned storage a
   --  typed view for the helper call.
   type RGBA_Access is access all Gdk.RGBA.Gdk_RGBA;
   function To_RGBA_Access is new Ada.Unchecked_Conversion
     (System.Address, RGBA_Access);

   ---------------------
   -- Test_Predicates --
   ---------------------

   --  Check both outcomes of each alpha predicate and that the boxed type is
   --  registered for use in GValues and object properties.
   procedure Test_Predicates is
      Clear_Color      : constant Gdk.RGBA.Gdk_RGBA := (0.2, 0.4, 0.6, 0.0);
      Translucent      : constant Gdk.RGBA.Gdk_RGBA := (0.2, 0.4, 0.6, 0.5);
      Opaque_Color     : constant Gdk.RGBA.Gdk_RGBA := (0.2, 0.4, 0.6, 1.0);
   begin
      Assert_True (Gdk.RGBA.Get_Type /= Glib.GType_Invalid);
      Assert_True (Gdk.RGBA.Is_Clear (Clear_Color));
      Assert_False (Gdk.RGBA.Is_Clear (Translucent));
      Assert_True (Gdk.RGBA.Is_Opaque (Opaque_Color));
      Assert_False (Gdk.RGBA.Is_Opaque (Translucent));
   end Test_Predicates;

   ----------------------------
   -- Test_Parse_And_String --
   ----------------------------

   --  Parse a CSS RGBA value, check the channels, format it, and parse the
   --  formatted value again. An invalid specification must report failure.
   procedure Test_Parse_And_String is
      Color       : Gdk.RGBA.Gdk_RGBA;
      Roundtrip   : Gdk.RGBA.Gdk_RGBA;
      Success     : Boolean;
      Text        : constant String := "rgba(51, 102, 153, 0.5)";
      Formatted   : String := Gdk.RGBA.To_String
        ((51.0 / 255.0, 102.0 / 255.0, 153.0 / 255.0, 0.5));
   begin
      Gdk.RGBA.Parse (Color, Text, Success);
      Assert_True (Success);
      Assert_True (abs (Color.Red - (51.0 / 255.0)) < 0.001);
      Assert_True (abs (Color.Green - (102.0 / 255.0)) < 0.001);
      Assert_True (abs (Color.Blue - (153.0 / 255.0)) < 0.001);
      Assert_True (abs (Color.Alpha - 0.5) < 0.001);

      --  The documented output form uses byte-valued RGB channels and a
      --  fractional alpha; the selected channels round-trip without loss.
      Assert_Cmpstr_Eq (Formatted, "rgba(51,102,153,0.5)");
      Gdk.RGBA.Parse (Roundtrip, Formatted, Success);
      Assert_True (Success);
      Assert_True (abs (Roundtrip.Red - Color.Red) < 0.001);
      Assert_True (abs (Roundtrip.Green - Color.Green) < 0.001);
      Assert_True (abs (Roundtrip.Blue - Color.Blue) < 0.001);
      Assert_True (abs (Roundtrip.Alpha - Color.Alpha) < 0.001);

      --  Do not inspect Color after a failed parse: only Success is defined
      --  when the input is invalid.
      Gdk.RGBA.Parse (Color, "not-a-color", Success);
      Assert_False (Success);
   end Test_Parse_And_String;

   --------------------------
   -- Test_Null_Conversion --
   --------------------------

   --  The generated address helper takes the address of a record, including
   --  for the null sentinel; it does not accept System.Null_Address itself.
   procedure Test_Null_Conversion is
      Null_Color : aliased Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Null_RGBA;
      Color      : aliased Gdk.RGBA.Gdk_RGBA := (0.1, 0.3, 0.7, 1.0);
      Value      : Glib.Values.GValue;
      Raised     : Boolean := False;
   begin
      Assert_True
        (Gdk.RGBA.Gdk_RGBA_Or_Null (Null_Color'Address) = System.Null_Address);
      Assert_True
        (Gdk.RGBA.Gdk_RGBA_Or_Null (Color'Address) = Color'Address);

      --  A zero RGBA sentinel is stored as a null boxed pointer; reading it
      --  raises the documented unset-value exception.
      Gdk.RGBA.Set_Value (Value, Gdk.RGBA.Null_RGBA);
      Assert_True (Glib.Values.Type_Of (Value) = Gdk.RGBA.Get_Type);
      begin
         declare
            Ignored : constant Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Get_Value (Value);
         begin
            pragma Unreferenced (Ignored);
         end;
      exception
         when Glib.Generic_Properties.Unset_Value =>
            Raised := True;
      end;
      Assert_True (Raised);
      Glib.Values.Unset (Value);
   end Test_Null_Conversion;

   ----------------------------
   -- Test_Temporary_Wrapper --
   ----------------------------

   --  Copy a record out of GLib-allocated storage and verify that conversion
   --  consumes the temporary allocation while preserving every channel.
   procedure Test_Temporary_Wrapper is
      function G_Malloc (Size : Glib.Gsize) return System.Address;
      pragma Import (C, G_Malloc, "g_malloc");

      Color : constant Gdk.RGBA.Gdk_RGBA := (0.15, 0.35, 0.55, 0.75);
      Bytes : constant Glib.Gsize :=
        Glib.Gsize
          ((Gdk.RGBA.Gdk_RGBA'Size + System.Storage_Unit - 1)
           / System.Storage_Unit);
      Wrapper   : constant RGBA_Access := To_RGBA_Access (G_Malloc (Bytes));
      Converted : Gdk.RGBA.Gdk_RGBA;
   begin
      Wrapper.all := Color;
      Converted := Gdk.RGBA.From_Object_Free (Wrapper);
      Assert_True (Converted.Red = Color.Red);
      Assert_True (Converted.Green = Color.Green);
      Assert_True (Converted.Blue = Color.Blue);
      Assert_True (Converted.Alpha = Color.Alpha);
   end Test_Temporary_Wrapper;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gdk/rgba/predicates", Test_Predicates'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk/rgba/parse-and-string", Test_Parse_And_String'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk/rgba/null-conversion", Test_Null_Conversion'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk/rgba/temporary-wrapper", Test_Temporary_Wrapper'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
