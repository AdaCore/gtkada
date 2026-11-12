--  Exercise Pango color parsing, alpha handling, formatting, and the
--  generated wrapper conversion. The formatting check mirrors the color
--  serialization asserted by test_label_markup in testsuite/c_tests/label.c.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib;      use Glib;
with Glib.Test; use Glib.Test;
with Pango.Color; use Pango.Color;

procedure Main is
   use type Glib.Guint16;

   --  From_Object_Free expects a GLib-allocated wrapper because it releases
   --  that allocation with g_free. The color record itself owns no resources.
   type Color_Access is access all Pango_Color;
   function To_Color_Access is new Ada.Unchecked_Conversion
     (System.Address, Color_Access);

   function G_Malloc (Size : Gsize) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Test_Parse_And_Format
   with Convention => C;

   procedure Test_Parse_With_Alpha
   with Convention => C;

   procedure Test_Wrapper_Conversion
   with Convention => C;

   ----------------------------
   -- Test_Parse_And_Format --
   ----------------------------

   --  Long hexadecimal input preserves each 16-bit channel; named colors
   --  and malformed input cover the other Parse return paths.
   procedure Test_Parse_And_Format is
      Color : aliased Pango_Color;
   begin
      Assert_True (Parse (Color'Access, "#123456789abc"));
      Assert_True (Color.Red = 16#1234#);
      Assert_True (Color.Green = 16#5678#);
      Assert_True (Color.Blue = 16#9ABC#);
      Assert_Cmpstr_Eq (To_String (Color), "#123456789abc");

      --  This is the exact foreground color string checked by the C label
      --  markup test when it prints Pango color attributes.
      Assert_True (Parse (Color'Access, "#1b1b6a6acbcb"));
      Assert_Cmpstr_Eq (To_String (Color), "#1b1b6a6acbcb");

      Assert_True (Parse (Color'Access, "red"));
      Assert_True (Color.Red = 16#FFFF#);
      Assert_True (Color.Green = 0);
      Assert_True (Color.Blue = 0);
      Assert_False (Parse (Color'Access, "not-a-color"));
   end Test_Parse_And_Format;

   -----------------------------
   -- Test_Parse_With_Alpha --
   -----------------------------

   --  An explicit alpha channel is decoded alongside RGB. When the input
   --  has no alpha component, Pango reports a fully opaque color.
   procedure Test_Parse_With_Alpha is
      Color : aliased Pango_Color;
      Alpha : aliased Guint16 := 0;
   begin
      Assert_True (Parse_With_Alpha (Color'Access, Alpha'Access, "#12345678"));
      Assert_True (Color.Red = 16#1212#);
      Assert_True (Color.Green = 16#3434#);
      Assert_True (Color.Blue = 16#5656#);
      Assert_True (Alpha = 16#7878#);

      Assert_True (Parse_With_Alpha (Color'Access, Alpha'Access, "blue"));
      Assert_True (Color.Red = 0);
      Assert_True (Color.Green = 0);
      Assert_True (Color.Blue = 16#FFFF#);
      Assert_True (Alpha = 16#FFFF#);
      Assert_False
        (Parse_With_Alpha (Color'Access, Alpha'Access, "invalid-color"));
   end Test_Parse_With_Alpha;

   -------------------------------
   -- Test_Wrapper_Conversion --
   -------------------------------

   --  Exercise the internal conversion with a GLib allocation so the helper
   --  can copy the record and release its wrapper using the matching allocator.
   procedure Test_Wrapper_Conversion is
      Wrapper_Bytes : constant Gsize :=
        Gsize ((Pango_Color'Size + System.Storage_Unit - 1)
               / System.Storage_Unit);
      Wrapper : constant Color_Access :=
        To_Color_Access (G_Malloc (Wrapper_Bytes));
      Converted : Pango_Color;
   begin
      Wrapper.all :=
        (Red => 16#1020#, Green => 16#3040#, Blue => 16#5060#);
      Converted := From_Object_Free (Wrapper);
      Assert_True (Converted.Red = 16#1020#);
      Assert_True (Converted.Green = 16#3040#);
      Assert_True (Converted.Blue = 16#5060#);
   end Test_Wrapper_Conversion;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/pango-color/parse-and-format",
      Test_Parse_And_Format'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/pango-color/parse-with-alpha",
      Test_Parse_With_Alpha'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/pango-color/wrapper-conversion",
      Test_Wrapper_Conversion'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
