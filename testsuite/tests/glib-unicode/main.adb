--  Exercise the public Glib.Unicode bindings for character classes, case
--  mapping, UTF-8 validation, traversal, encoding, decoding, and length.
--  There is no focused C test for these helpers to port; the C tests only use
--  a few helpers incidentally while testing text widgets.
with Ada.Command_Line;
with Glib.Test;
with Glib.Unicode;

procedure Main is
   use Glib;
   use Glib.Test;
   use Glib.Unicode;

   procedure Test_Character_Classes with Convention => C;
   procedure Test_Character_Classes is
   begin
      --  Representative ASCII and Unicode properties check each classifier.
      Assert_True (Is_Space (Character'Pos (' ')));
      Assert_True (Is_Alnum (Character'Pos ('A')));
      Assert_True (Is_Alpha (Character'Pos ('A')));
      Assert_True (Is_Digit (Character'Pos ('7')));
      Assert_True (Is_Lower (Character'Pos ('a')));
      Assert_True (Is_Upper (Character'Pos ('Z')));
      Assert_True (Is_Punct (Character'Pos ('!')));
      Assert_True (Unichar_Type (Character'Pos ('A')) = Unicode_Uppercase_Letter);
      Assert_True (To_Lower (Character'Pos ('A')) = Character'Pos ('a'));
      Assert_True (To_Upper (Character'Pos ('z')) = Character'Pos ('Z'));
   end Test_Character_Classes;

   procedure Test_Validation with Convention => C;
   procedure Test_Validation is
      Valid       : Boolean;
      Invalid_Pos : Natural;
      Bad_Text    : constant UTF8_String := "ok" & Character'Val (16#FF#);
   begin
      --  A well-formed multibyte sequence reports success and position zero.
      UTF8_Validate ("Aλ", Valid, Invalid_Pos);
      Assert_True (Valid);
      Assert_True (Invalid_Pos = 0);

      --  An invalid trailing byte reports its one-based position.
      UTF8_Validate (Bad_Text, Valid, Invalid_Pos);
      Assert_True (not Valid);
      Assert_True (Invalid_Pos = 3);
   end Test_Validation;

   procedure Test_String_Case_And_Encoding with Convention => C;
   procedure Test_String_Case_And_Encoding is
      Buffer : UTF8_String (4 .. 9) := (others => ASCII.NUL);
      Last   : Natural;
   begin
      --  String case conversion preserves UTF-8 and allocates the result.
      Assert_Cmpstr_Eq (UTF8_Strdown ("GtkAda"), "gtkada");
      Assert_Cmpstr_Eq (UTF8_Strup ("GtkAda"), "GTKADA");

      --  Encoding a Greek character writes its two UTF-8 bytes into a
      --  non-one-based buffer and returns the last written index.
      Unichar_To_UTF8 (16#03BB#, Buffer, Last);
      Assert_True (Last = 5);
      Assert_Cmpstr_Eq (Buffer (4 .. Last), "λ");
   end Test_String_Case_And_Encoding;

   procedure Test_UTF8_Traversal with Convention => C;
   procedure Test_UTF8_Traversal is
      Text : constant UTF8_String := "AλB";
   begin
      --  UTF8_Strlen counts code points, while the traversal helpers return
      --  byte indexes; λ occupies two bytes in this string.
      Assert_True (UTF8_Strlen (Text) = 3);
      Assert_True (UTF8_Next_Char (Text, 2) = 4);
      Assert_True (UTF8_Find_Next_Char (Text, 1) = 2);
      Assert_True (UTF8_Find_Next_Char (Text, 4) = Text'Last + 1);
      Assert_True (UTF8_Find_Prev_Char (Text, 4) = 2);
      Assert_True (UTF8_Find_Prev_Char (Text, 1) = Text'First - 1);

      --  Decoders return the first code point, with the validated variant
      --  used on an otherwise valid complete sequence.
      Assert_True (UTF8_Get_Char ("λ") = 16#03BB#);
      Assert_True (UTF8_Get_Char_Validated ("λ") = 16#03BB#);
   end Test_UTF8_Traversal;

begin
   Glib.Test.Init;
   Add_Func ("/glib-unicode/character-classes", Test_Character_Classes'Unrestricted_Access);
   Add_Func ("/glib-unicode/validation", Test_Validation'Unrestricted_Access);
   Add_Func ("/glib-unicode/string-case-encoding", Test_String_Case_And_Encoding'Unrestricted_Access);
   Add_Func ("/glib-unicode/utf8-traversal", Test_UTF8_Traversal'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
