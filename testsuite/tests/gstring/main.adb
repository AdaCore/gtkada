--  Glib.String (GString) coverage, ported from GLib's own
--  glib/tests/string.c.

--  Out of scope: g_string_chunk_* (a different C type, GStringChunk, not
--  bound at all) and g_string_printf/append_printf/vprintf (variadic C,
--  not bound).
--
--  Not chased down (separate, pre-existing, unrelated to the ABI fix):
--  Append_Len with an embedded NUL byte mid-string loses the trailing
--  bytes on readback (Len comes back right, content doesn't). Not
--  reinvestigated here - flagged in gstring_abi_bug.md.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;

with Glib;          use Glib;
with Glib.Test;     use Glib.Test;
with Glib.String;   use Glib.String;
with Gtkada.Types;  use Gtkada.Types;

procedure Main is

   function Str_Of (G : Gstring) return String;
   --  Reads G's contents via Str/Len directly (nul-safe).

   ------------
   -- Str_Of --
   ------------

   function Str_Of (G : Gstring) return String is
      --  Both Interfaces.C.Strings.Value overloads (String and char_array)
      --  stop at the first embedded NUL in practice, regardless of the
      --  Length passed in -- confirmed by direct testing, not just the
      --  overload's name. Overlaying a fixed-length String directly onto
      --  the pointer's target reads exactly G.Len bytes with no C-string
      --  interpretation at all.
      subtype Fixed_String is String (1 .. Natural (G.Len));
      type Fixed_String_Access is access all Fixed_String;
      function Convert is new Ada.Unchecked_Conversion
        (Chars_Ptr, Fixed_String_Access);
   begin
      return Convert (G.Str).all;
   end Str_Of;

   procedure Test_New with Convention => C;
   procedure Test_New_Len with Convention => C;
   procedure Test_New_Take with Convention => C;
   procedure Test_Sized_New with Convention => C;
   procedure Test_Assign with Convention => C;
   procedure Test_Append with Convention => C;
   procedure Test_Prepend with Convention => C;
   procedure Test_Insert with Convention => C;
   procedure Test_Equal with Convention => C;
   procedure Test_Truncate with Convention => C;
   procedure Test_Overwrite with Convention => C;
   procedure Test_Up_Down with Convention => C;
   procedure Test_Set_Size with Convention => C;
   procedure Test_Steal with Convention => C;
   procedure Test_Erase with Convention => C;
   procedure Test_Hash with Convention => C;
   procedure Test_Append_Uri_Escaped with Convention => C;

   --------------
   -- Test_New --
   --------------

   procedure Test_New is
      S : constant UTF8_String := "hello";
      G : constant Gstring := Gstring_New (S);
   begin
      Assert_Cmpint_Eq (Gint (G.Len), 5);
      Assert_Cmpstr_Eq (Str_Of (G), S);
      Assert_Cmpstr_Eq (Free (G, Free_Segment => True), "");
   end Test_New;

   ------------------
   -- Test_New_Len --
   ------------------

   procedure Test_New_Len is
      S : constant UTF8_String := "hello";
      G : constant Gstring := Gstring_New_Len ("hello world", 5);
   begin
      Assert_Cmpint_Eq (Gint (G.Len), 5);
      Assert_Cmpstr_Eq (Str_Of (G), "hello");
      Assert_Cmpstr_Eq (Free (G, Free_Segment => True), "");
   end Test_New_Len;

   -------------------
   -- Test_New_Take --
   -------------------

   procedure Test_New_Take is
      S : constant UTF8_String := "taken";
      G : constant Gstring := Gstring_New_Take (S);
   begin
      Assert_Cmpint_Eq (Gint (G.Len), 5);
      Assert_Cmpstr_Eq (Str_Of (G), S);
      Assert_Cmpstr_Eq (Free (G, Free_Segment => True), "");
   end Test_New_Take;

   ---------------------
   -- Test_Sized_New --
   ---------------------

   procedure Test_Sized_New is
      G : constant Gstring := Gstring_Sized_New (64);
   begin
      Assert_Cmpint_Eq (Gint (G.Len), 0);
      Assert_Cmpstr_Eq (Free (G, Free_Segment => True), "");
   end Test_Sized_New;

   -----------------
   -- Test_Assign --
   -----------------

   procedure Test_Assign is
      G : Gstring := Gstring_New ("hello");
   begin
      G := Assign (G, "goodbye");
      Assert_Cmpstr_Eq (Str_Of (G), "goodbye");
   end Test_Assign;

   -----------------
   -- Test_Append --
   -----------------

   procedure Test_Append is
      G : Gstring := Gstring_New ("abc");
   begin
      G := Append (G, "def");
      Assert_Cmpstr_Eq (Str_Of (G), "abcdef");
      G := Append_Char (G, Gchar (ASCII.LF));
      Assert_Cmpstr_Eq (Str_Of (G), "abcdef" & ASCII.LF);
      G := Append_Len (G, "ghijk", 3);
      Assert_Cmpstr_Eq (Str_Of (G), "abcdef" & ASCII.LF & "ghi");
      G := Append_Unichar (G, Gunichar (Character'Pos ('!')));
      Assert_Cmpstr_Eq (Str_Of (G), "abcdef" & ASCII.LF & "ghi!");
   end Test_Append;

   ------------------
   -- Test_Prepend --
   ------------------

   procedure Test_Prepend is
      G : Gstring := Gstring_New ("world");
   begin
      G := Prepend (G, "hello ");
      Assert_Cmpstr_Eq (Str_Of (G), "hello world");
      G := Prepend_Char (G, Gchar (Character'('!')));
      Assert_Cmpstr_Eq (Str_Of (G), "!hello world");
      G := Prepend_Len (G, "xyzzy", 2);
      Assert_Cmpstr_Eq (Str_Of (G), "xy!hello world");
      G := Prepend_Unichar (G, Gunichar (Character'Pos ('?')));
      Assert_Cmpstr_Eq (Str_Of (G), "?xy!hello world");
   end Test_Prepend;

   -----------------
   -- Test_Insert --
   -----------------

   procedure Test_Insert is
      G : Gstring := Gstring_New ("ac");
   begin
      G := Insert (G, 1, "b");
      Assert_Cmpstr_Eq (Str_Of (G), "abc");
      G := Insert_C (G, 0, Gchar (Character'('_')));
      Assert_Cmpstr_Eq (Str_Of (G), "_abc");
      G := Insert_Len (G, -1, "defg", 3);
      Assert_Cmpstr_Eq (Str_Of (G), "_abcdef");
      G := Insert_Unichar (G, -1, Gunichar (Character'Pos ('.')));
      Assert_Cmpstr_Eq (Str_Of (G), "_abcdef.");
   end Test_Insert;

   ----------------
   -- Test_Equal --
   ----------------

   procedure Test_Equal is
      A : constant Gstring := Gstring_New ("same");
      B : constant Gstring := Gstring_New ("same");
      C : constant Gstring := Gstring_New ("different");
   begin
      Assert_True (Equal (A, B));
      Assert_True (not Equal (A, C));
   end Test_Equal;

   -------------------
   -- Test_Truncate --
   -------------------

   procedure Test_Truncate is
      G : Gstring := Gstring_New ("hello world");
   begin
      G := Truncate (G, 5);
      Assert_Cmpstr_Eq (Str_Of (G), "hello");
   end Test_Truncate;

   --------------------
   -- Test_Overwrite --
   --------------------

   procedure Test_Overwrite is
      G : Gstring := Gstring_New ("hello world");
   begin
      G := Overwrite (G, 6, "there");
      Assert_Cmpstr_Eq (Str_Of (G), "hello there");
      G := Overwrite_Len (G, 0, "HELLOX", 5);
      Assert_Cmpstr_Eq (Str_Of (G), "HELLO there");
   end Test_Overwrite;

   ------------------
   -- Test_Up_Down --
   ------------------

   procedure Test_Up_Down is
      G : Gstring := Gstring_New ("Hello World");
   begin
      G := Down (G);
      Assert_Cmpstr_Eq (Str_Of (G), "hello world");
      G := Up (G);
      Assert_Cmpstr_Eq (Str_Of (G), "HELLO WORLD");
      G := Ascii_Down (G);
      Assert_Cmpstr_Eq (Str_Of (G), "hello world");
      G := Ascii_Up (G);
      Assert_Cmpstr_Eq (Str_Of (G), "HELLO WORLD");
   end Test_Up_Down;

   -------------------
   -- Test_Set_Size --
   -------------------

   procedure Test_Set_Size is
      G : Gstring := Gstring_New ("hello world");
   begin
      G := Set_Size (G, 5);
      Assert_Cmpint_Eq (Gint (G.Len), 5);
      Assert_Cmpstr_Eq (Str_Of (G), "hello");
   end Test_Set_Size;

   -----------------
   -- Test_Steal --
   -----------------

   procedure Test_Steal is
      G      : constant Gstring := Gstring_New ("stolen");
      Stolen : constant UTF8_String := Free_And_Steal (G);
   begin
      Assert_Cmpstr_Eq (Stolen, "stolen");
   end Test_Steal;

   -----------------
   -- Test_Erase --
   -----------------

   procedure Test_Erase is
      G : Gstring := Gstring_New ("hello world");
   begin
      G := Erase (G, 5, 1);
      Assert_Cmpstr_Eq (Str_Of (G), "helloworld");
   end Test_Erase;

   ----------------
   -- Test_Hash --
   ----------------

   procedure Test_Hash is
      A : constant Gstring := Gstring_New ("same content");
      B : constant Gstring := Gstring_New ("same content");
   begin
      Assert_True (Hash (A) = Hash (B));
   end Test_Hash;

   ------------------------------
   -- Test_Append_Uri_Escaped --
   ------------------------------

   procedure Test_Append_Uri_Escaped is
      G : Gstring := Gstring_New;
   begin
      G := Append_Uri_Escaped (G, "a b", "", True);
      Assert_Cmpstr_Eq (Str_Of (G), "a%20b");
   end Test_Append_Uri_Escaped;

begin
   Glib.Test.Init;

   Glib.Test.Add_Func ("/gstring/new", Test_New'Unrestricted_Access);
   Glib.Test.Add_Func ("/gstring/new-len", Test_New_Len'Unrestricted_Access);
   Glib.Test.Add_Func ("/gstring/new-take", Test_New_Take'Unrestricted_Access);
   Glib.Test.Add_Func ("/gstring/sized-new", Test_Sized_New'Unrestricted_Access);
   Glib.Test.Add_Func ("/gstring/assign", Test_Assign'Unrestricted_Access);
   Glib.Test.Add_Func ("/gstring/append", Test_Append'Unrestricted_Access);
   Glib.Test.Add_Func ("/gstring/prepend", Test_Prepend'Unrestricted_Access);
   Glib.Test.Add_Func ("/gstring/insert", Test_Insert'Unrestricted_Access);
   Glib.Test.Add_Func ("/gstring/equal", Test_Equal'Unrestricted_Access);
   Glib.Test.Add_Func ("/gstring/truncate", Test_Truncate'Unrestricted_Access);
   Glib.Test.Add_Func ("/gstring/overwrite", Test_Overwrite'Unrestricted_Access);
   Glib.Test.Add_Func ("/gstring/up-down", Test_Up_Down'Unrestricted_Access);
   Glib.Test.Add_Func ("/gstring/set-size", Test_Set_Size'Unrestricted_Access);
   Glib.Test.Add_Func ("/gstring/steal", Test_Steal'Unrestricted_Access);
   Glib.Test.Add_Func ("/gstring/erase", Test_Erase'Unrestricted_Access);
   Glib.Test.Add_Func ("/gstring/hash", Test_Hash'Unrestricted_Access);
   Glib.Test.Add_Func ("/gstring/append-uri-escaped", Test_Append_Uri_Escaped'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
