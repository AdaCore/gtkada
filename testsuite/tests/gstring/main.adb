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
with System;

with Glib;          use Glib;
with Glib.Bytes;    use Glib.Bytes;
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
   procedure Test_Out_Constructors with Convention => C;
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
   procedure Test_Replace with Convention => C;
   procedure Test_Free_To_Bytes with Convention => C;
   procedure Test_Wrapper_Free with Convention => C;

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

   --------------------------
   -- Test_Out_Constructors --
   --------------------------

   --  Exercise the procedure forms separately from the returning constructors.
   --  Empty and nonempty values cover both input-allocation paths.
   procedure Test_Out_Constructors is
      G : Gstring;
   begin
      G_New (G, "procedure");
      Assert_Cmpstr_Eq (Str_Of (G), "procedure");
      Assert_Cmpstr_Eq (Free (G, Free_Segment => True), "");

      G_New (G);
      Assert_Cmpint_Eq (Gint (G.Len), 0);
      Assert_Cmpstr_Eq (Free (G, Free_Segment => True), "");

      --  The length constructor copies precisely the requested prefix.
      G_New_Len (G, "length suffix", 6);
      Assert_Cmpstr_Eq (Str_Of (G), "length");
      Assert_Cmpstr_Eq (Free (G, Free_Segment => True), "");

      --  The take constructor transfers the copied buffer to GLib.
      G_New_Take (G, "owned");
      Assert_Cmpstr_Eq (Str_Of (G), "owned");
      Assert_Cmpstr_Eq (Free (G, Free_Segment => True), "");

      G_New_Take (G);
      Assert_Cmpint_Eq (Gint (G.Len), 0);
      Assert_Cmpstr_Eq (Free (G, Free_Segment => True), "");

      --  The sized form reserves capacity while starting with an empty value.
      G_Sized_New (G, 64);
      Assert_Cmpint_Eq (Gint (G.Len), 0);
      Assert_Cmpstr_Eq (Free (G, Free_Segment => True), "");
   end Test_Out_Constructors;

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

   ------------------
   -- Test_Replace --
   ------------------

   --  Check both a bounded replacement and the zero-means-unlimited form.
   procedure Test_Replace is
      G : Gstring := Gstring_New ("one fish two fish");
   begin
      Assert_Cmpuint_Eq (Replace (G, "fish", "cat", 1), 1);
      Assert_Cmpstr_Eq (Str_Of (G), "one cat two fish");
      Assert_Cmpuint_Eq (Replace (G, "fish", "cat", 0), 1);
      Assert_Cmpstr_Eq (Str_Of (G), "one cat two cat");
      Assert_Cmpstr_Eq (Free (G, Free_Segment => True), "");
   end Test_Replace;

   -----------------------
   -- Test_Free_To_Bytes --
   -----------------------

   --  Free_To_Bytes consumes the GString and keeps its exact bytes in GBytes.
   procedure Test_Free_To_Bytes is
      subtype Five_Chars is String (1 .. 5);
      type Five_Chars_Access is access all Five_Chars;
      function To_Five_Chars is new Ada.Unchecked_Conversion
        (System.Address, Five_Chars_Access);

      G      : constant Gstring := Gstring_New ("bytes");
      Bytes  : constant Gbytes := Free_To_Bytes (G);
      Region : constant System.Address :=
        Get_Region (Bytes, Element_Size => 1, Offset => 0, N_Elements => 5);
   begin
      Assert_Cmpuint_Eq (Guint (Get_Size (Bytes)), 5);
      Assert_Cmpstr_Eq (To_Five_Chars (Region).all, "bytes");
      Unref (Bytes);
   end Test_Free_To_Bytes;

   -----------------------
   -- Test_Wrapper_Free --
   -----------------------

   --  From_Object_Free expects its temporary record wrapper to use g_free.
   procedure Test_Wrapper_Free is
      type Record_Access is access all Gstring_Record;
      function To_Record_Access is new Ada.Unchecked_Conversion
        (System.Address, Record_Access);
      function G_Malloc (Size : Gsize) return System.Address;
      pragma Import (C, G_Malloc, "g_malloc");

      Original : constant Gstring := Gstring_New ("wrapper");
      Wrapper_Bytes : constant Gsize :=
        Gsize ((Gstring_Record'Size + System.Storage_Unit - 1)
               / System.Storage_Unit);
      Wrapper : constant Record_Access :=
        To_Record_Access (G_Malloc (Wrapper_Bytes));
      Result : Gstring_Record;
   begin
      --  Copy only the record fields into GLib-allocated wrapper storage.
      Wrapper.all := Original.all;
      Result := From_Object_Free (Wrapper);
      Assert_Cmpuint_Eq (Guint (Result.Len), 7);
      Assert_Cmpuint_Eq (Guint (Result.Allocated_Len),
                         Guint (Original.Allocated_Len));
      Assert_Cmpstr_Eq (Free (Original, Free_Segment => True), "");
   end Test_Wrapper_Free;

begin
   Glib.Test.Init;

   Glib.Test.Add_Func ("/gstring/new", Test_New'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gstring/out-constructors", Test_Out_Constructors'Unrestricted_Access);
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
   Glib.Test.Add_Func ("/gstring/replace", Test_Replace'Unrestricted_Access);
   Glib.Test.Add_Func ("/gstring/free-to-bytes", Test_Free_To_Bytes'Unrestricted_Access);
   Glib.Test.Add_Func ("/gstring/wrapper-free", Test_Wrapper_Free'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
