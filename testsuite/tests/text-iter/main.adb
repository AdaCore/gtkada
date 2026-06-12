--  Ada port of GTK's testsuite/gtk/textiter.c (GTK 4.22.2).
--
--  Gtk.Text_Iter operates on a plain Gtk.Text_Buffer model, so the whole
--  suite runs without a realized window: it exercises forward and
--  backward search (with and without flags), tag toggles, line ends,
--  word / cursor-position / sentence boundaries, backward_line with a
--  range insertion, and invisible text.
--
--  The C tests test_visible_word_boundaries and
--  test_visible_cursor_positions are intentionally omitted: they assert
--  behaviour the C source itself marks with FIXME comments, which makes
--  them poor anchors for a binding test.

with Ada.Command_Line;

with Glib;            use Glib;
with Glib.Properties;
with Glib.Test;       use Glib.Test;

with Gtk.Main;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_Iter;   use Gtk.Text_Iter;
with Gtk.Text_Tag;    use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;

procedure Main is

   LF : constant Character := ASCII.LF;

   --  "À" (U+00C0) and "à" (U+00E0), 2 bytes each in UTF-8, and the
   --  decomposed form "a" + combining grave accent (U+0300).
   A_Grave : constant UTF8_String :=
     Character'Val (16#C3#) & Character'Val (16#80#);
   A_Grave_Lower : constant UTF8_String :=
     Character'Val (16#C3#) & Character'Val (16#A0#);
   A_Combining_Grave : constant UTF8_String :=
     'a' & Character'Val (16#CC#) & Character'Val (16#80#);

   procedure Check_Found_Forward
     (Haystack        : UTF8_String;
      Needle          : UTF8_String;
      Flags           : Gtk_Text_Search_Flags;
      Expected_Start  : Gint;
      Expected_End    : Gint;
      Expected_String : UTF8_String);

   procedure Check_Found_Backward
     (Haystack        : UTF8_String;
      Needle          : UTF8_String;
      Flags           : Gtk_Text_Search_Flags;
      Expected_Start  : Gint;
      Expected_End    : Gint;
      Expected_String : UTF8_String);

   procedure Check_Not_Found
     (Haystack : UTF8_String;
      Needle   : UTF8_String;
      Flags    : Gtk_Text_Search_Flags);

   -------------------------
   -- Check_Found_Forward --
   -------------------------

   procedure Check_Found_Forward
     (Haystack        : UTF8_String;
      Needle          : UTF8_String;
      Flags           : Gtk_Text_Search_Flags;
      Expected_Start  : Gint;
      Expected_End    : Gint;
      Expected_String : UTF8_String)
   is
      Buffer      : Gtk_Text_Buffer;
      Iter        : Gtk_Text_Iter;
      Match_Start : Gtk_Text_Iter;
      Match_End   : Gtk_Text_Iter;
      Found       : Boolean;
   begin
      Gtk_New (Buffer);
      Buffer.Set_Text (Haystack);

      Buffer.Get_Start_Iter (Iter);
      Forward_Search
        (Iter, Needle, Flags, Match_Start, Match_End, Result => Found);
      Assert_True (Found);
      Assert_Cmpint_Eq (Expected_Start, Get_Offset (Match_Start));
      Assert_Cmpint_Eq (Expected_End, Get_Offset (Match_End));
      Assert_Cmpstr_Eq (Expected_String, Get_Text (Match_Start, Match_End));
   end Check_Found_Forward;

   --------------------------
   -- Check_Found_Backward --
   --------------------------

   procedure Check_Found_Backward
     (Haystack        : UTF8_String;
      Needle          : UTF8_String;
      Flags           : Gtk_Text_Search_Flags;
      Expected_Start  : Gint;
      Expected_End    : Gint;
      Expected_String : UTF8_String)
   is
      Buffer      : Gtk_Text_Buffer;
      Iter        : Gtk_Text_Iter;
      Match_Start : Gtk_Text_Iter;
      Match_End   : Gtk_Text_Iter;
      Found       : Boolean;
   begin
      Gtk_New (Buffer);
      Buffer.Set_Text (Haystack);

      Buffer.Get_End_Iter (Iter);
      Backward_Search
        (Iter, Needle, Flags, Match_Start, Match_End, Result => Found);
      Assert_True (Found);
      Assert_Cmpint_Eq (Expected_Start, Get_Offset (Match_Start));
      Assert_Cmpint_Eq (Expected_End, Get_Offset (Match_End));
      Assert_Cmpstr_Eq (Expected_String, Get_Text (Match_Start, Match_End));
   end Check_Found_Backward;

   ---------------------
   -- Check_Not_Found --
   ---------------------

   procedure Check_Not_Found
     (Haystack : UTF8_String;
      Needle   : UTF8_String;
      Flags    : Gtk_Text_Search_Flags)
   is
      Buffer      : Gtk_Text_Buffer;
      Iter        : Gtk_Text_Iter;
      Match_Start : Gtk_Text_Iter;
      Match_End   : Gtk_Text_Iter;
      Found       : Boolean;
   begin
      Gtk_New (Buffer);
      Buffer.Set_Text (Haystack);

      Buffer.Get_Start_Iter (Iter);
      Forward_Search
        (Iter, Needle, Flags, Match_Start, Match_End, Result => Found);
      Assert_False (Found);

      Buffer.Get_End_Iter (Iter);
      Backward_Search
        (Iter, Needle, Flags, Match_Start, Match_End, Result => Found);
      Assert_False (Found);
   end Check_Not_Found;

   procedure Test_Empty_Search
   with Convention => C;

   procedure Test_Search_Full_Buffer
   with Convention => C;

   procedure Test_Search
   with Convention => C;

   procedure Test_Search_Caseless
   with Convention => C;

   procedure Test_Forward_To_Tag_Toggle
   with Convention => C;

   procedure Test_Forward_To_Line_End
   with Convention => C;

   procedure Test_Word_Boundaries
   with Convention => C;

   procedure Test_Cursor_Positions
   with Convention => C;

   procedure Test_Sentence_Boundaries
   with Convention => C;

   procedure Test_Backward_Line
   with Convention => C;

   procedure Test_Invisible_Text
   with Convention => C;

   -----------------------
   -- Test_Empty_Search --
   -----------------------

   procedure Test_Empty_Search is
      Buffer      : Gtk_Text_Buffer;
      Iter        : Gtk_Text_Iter;
      Match_Start : Gtk_Text_Iter;
      Match_End   : Gtk_Text_Iter;
      Found       : Boolean;
   begin
      Gtk_New (Buffer);
      Buffer.Set_Text ("This is some foo text");

      --  Search for the empty string from the start, forward.
      Buffer.Get_Start_Iter (Iter);
      Forward_Search
        (Iter, "", 0, Match_Start, Match_End, Result => Found);
      Assert_True (Found);
      Assert_Cmpint_Eq (Get_Offset (Match_Start), Get_Offset (Match_End));
      Assert_Cmpint_Eq (Get_Offset (Match_Start), 1);

      --  Search for the empty string from the end, backward.
      Buffer.Get_End_Iter (Iter);
      Backward_Search
        (Iter, "", 0, Match_Start, Match_End, Result => Found);
      Assert_True (Found);
      Assert_Cmpint_Eq (Get_Offset (Match_Start), Get_Offset (Match_End));
      Assert_Cmpint_Eq (Get_Offset (Match_Start), 20);
   end Test_Empty_Search;

   -----------------------------
   -- Test_Search_Full_Buffer --
   -----------------------------

   procedure Test_Search_Full_Buffer is
   begin
      Check_Found_Forward ("foo", "foo", 0, 0, 3, "foo");
      Check_Found_Backward ("foo", "foo", 0, 0, 3, "foo");
      Check_Found_Forward ("foo", "foo", Case_Insensitive, 0, 3, "foo");
      Check_Found_Backward ("foo", "foo", Case_Insensitive, 0, 3, "foo");
      Check_Found_Forward ("foo", "Foo", Case_Insensitive, 0, 3, "foo");
      Check_Found_Backward ("foo", "Foo", Case_Insensitive, 0, 3, "foo");
   end Test_Search_Full_Buffer;

   -----------------
   -- Test_Search --
   -----------------

   procedure Test_Search is
   begin
      --  Simple match
      Check_Found_Forward ("This is some foo text", "foo", 0, 13, 16, "foo");
      Check_Found_Backward ("This is some foo text", "foo", 0, 13, 16, "foo");
      Check_Not_Found ("This is some foo text", "Foo", 0);

      --  Different matches for forward and backward
      Check_Found_Forward
        ("This is some foo foo text", "foo", 0, 13, 16, "foo");
      Check_Found_Backward
        ("This is some foo foo text", "foo", 0, 17, 20, "foo");

      --  New lines in the haystack
      Check_Found_Forward
        ("This is some" & LF & "foo text", "foo", 0, 13, 16, "foo");
      Check_Found_Backward
        ("This is some" & LF & "foo text", "foo", 0, 13, 16, "foo");
      Check_Found_Forward
        ("This is some foo" & LF & "foo text", "foo", 0, 13, 16, "foo");
      Check_Found_Backward
        ("This is some foo" & LF & "foo text", "foo", 0, 17, 20, "foo");
      Check_Not_Found ("This is some" & LF & "foo text", "Foo", 0);

      --  End of buffer
      Check_Found_Forward
        ("This is some" & LF & "text foo", "foo", 0, 18, 21, "foo");
      Check_Found_Backward
        ("This is some" & LF & "text foo", "foo", 0, 18, 21, "foo");
      Check_Not_Found ("This is some" & LF & "text foo", "Foo", 0);

      --  Multiple lines in the needle
      Check_Found_Forward
        ("This is some foo" & LF & "foo text", "foo" & LF & "foo", 0,
         13, 20, "foo" & LF & "foo");
      Check_Found_Backward
        ("This is some foo" & LF & "foo text", "foo" & LF & "foo", 0,
         13, 20, "foo" & LF & "foo");
      Check_Not_Found
        ("This is some foo" & LF & "foo text", "Foo" & LF & "foo", 0);

      --  Multi-byte characters in the needle
      Check_Found_Forward
        ("This is some " & A_Grave & " text", A_Grave, 0, 13, 14, A_Grave);
      Check_Found_Forward
        ("This is some " & A_Grave & " text", "some " & A_Grave, 0,
         8, 14, "some " & A_Grave);
      Check_Found_Forward
        ("This is some " & A_Grave & " text", A_Grave & " text", 0,
         13, 19, A_Grave & " text");
      Check_Found_Backward
        ("This is some " & A_Grave_Lower & " text", A_Grave_Lower, 0,
         13, 14, A_Grave_Lower);
      Check_Found_Backward
        ("This is some " & A_Grave_Lower & " text",
         "some " & A_Grave_Lower, 0, 8, 14, "some " & A_Grave_Lower);

      --  Multi-byte characters outside the needle
      Check_Found_Forward (A_Grave & " aa", "aa", 0, 2, 4, "aa");
      Check_Found_Forward ("aa " & A_Grave, "aa", 0, 0, 2, "aa");
      Check_Found_Backward (A_Grave & " aa", "aa", 0, 2, 4, "aa");
      Check_Found_Backward ("aa " & A_Grave, "aa", 0, 0, 2, "aa");
   end Test_Search;

   --------------------------
   -- Test_Search_Caseless --
   --------------------------

   procedure Test_Search_Caseless is
      Flags : constant Gtk_Text_Search_Flags := Case_Insensitive;
   begin
      --  Simple match
      Check_Found_Forward
        ("This is some foo text", "foo", Flags, 13, 16, "foo");
      Check_Found_Forward
        ("This is some foo text", "Foo", Flags, 13, 16, "foo");
      Check_Found_Forward
        ("This is some Foo text", "foo", Flags, 13, 16, "Foo");
      Check_Found_Backward
        ("This is some foo text", "foo", Flags, 13, 16, "foo");
      Check_Found_Backward
        ("This is some foo text", "Foo", Flags, 13, 16, "foo");
      Check_Found_Backward
        ("This is some Foo text", "foo", Flags, 13, 16, "Foo");

      --  Different matches for forward and backward, including
      --  case-insensitive matches across different compositions of
      --  accented characters.
      Check_Found_Forward
        ("This is some foo foo text", "foo", Flags, 13, 16, "foo");
      Check_Found_Forward
        ("This is some foo foo text", "Foo", Flags, 13, 16, "foo");
      Check_Found_Forward
        ("This is some Foo foo text", "foo", Flags, 13, 16, "Foo");
      Check_Found_Forward
        ("This is some " & A_Grave & " " & A_Grave_Lower & " text",
         A_Grave_Lower, Flags, 13, 14, A_Grave);
      Check_Found_Forward
        ("This is some " & A_Grave & " " & A_Grave_Lower & " text",
         A_Grave, Flags, 13, 14, A_Grave);
      Check_Found_Forward
        ("This is some " & A_Grave & " " & A_Grave_Lower & " text",
         A_Combining_Grave, Flags, 13, 14, A_Grave);
      Check_Found_Backward
        ("This is some foo foo text", "foo", Flags, 17, 20, "foo");
      Check_Found_Backward
        ("This is some foo foo text", "Foo", Flags, 17, 20, "foo");
      Check_Found_Backward
        ("This is some foo Foo text", "foo", Flags, 17, 20, "Foo");
      Check_Found_Backward
        ("This is some " & A_Grave & " " & A_Grave_Lower & " text",
         A_Grave_Lower, Flags, 15, 16, A_Grave_Lower);
      Check_Found_Backward
        ("This is some " & A_Grave & " " & A_Grave_Lower & " text",
         A_Grave, Flags, 15, 16, A_Grave_Lower);
      Check_Found_Backward
        ("This is some " & A_Grave & " " & A_Grave_Lower & " text",
         A_Combining_Grave, Flags, 15, 16, A_Grave_Lower);

      --  New lines in the haystack
      Check_Found_Forward
        ("This is some" & LF & "Foo text", "foo", Flags, 13, 16, "Foo");
      Check_Found_Forward
        ("This is some" & LF & A_Grave & " text", A_Grave_Lower, Flags,
         13, 14, A_Grave);
      Check_Found_Backward
        ("This is some" & LF & "foo text", "Foo", Flags, 13, 16, "foo");

      --  End of buffer
      Check_Found_Forward
        ("This is some" & LF & "text Foo", "foo", Flags, 18, 21, "Foo");
      Check_Found_Backward
        ("This is some" & LF & "text " & A_Grave, A_Combining_Grave,
         Flags, 18, 19, A_Grave);

      --  Multiple lines in the needle
      Check_Found_Forward
        ("This is some Foo" & LF & "Foo text", "foo" & LF & "foo", Flags,
         13, 20, "Foo" & LF & "Foo");
      Check_Found_Backward
        ("This is some " & A_Grave & LF & A_Grave & " text",
         A_Grave_Lower & LF & A_Grave_Lower, Flags,
         13, 16, A_Grave & LF & A_Grave);

      --  Multi-byte characters outside the needle
      Check_Found_Forward (A_Grave & " aa", "aa", Flags, 2, 4, "aa");
      Check_Found_Backward ("aa " & A_Grave, "aa", Flags, 0, 2, "aa");
   end Test_Search_Caseless;

   --------------------------------
   -- Test_Forward_To_Tag_Toggle --
   --------------------------------

   procedure Test_Forward_To_Tag_Toggle is
      Buffer       : Gtk_Text_Buffer;
      Bold_Tag     : Gtk_Text_Tag;
      Editable_Tag : Gtk_Text_Tag;
      Iter         : Gtk_Text_Iter;
      Found        : Boolean;
   begin
      Gtk_New (Buffer);

      Bold_Tag := Buffer.Create_Tag ("bold");
      Editable_Tag := Buffer.Create_Tag ("not-editable");

      Buffer.Get_Start_Iter (Iter);
      Buffer.Insert (Iter, "a");
      Buffer.Insert_With_Tags (Iter, "b", Bold_Tag);
      Buffer.Insert_With_Tags (Iter, "c", Editable_Tag);

      --  Go to the first "on" toggle
      Buffer.Get_Start_Iter (Iter);
      Forward_To_Tag_Toggle (Iter, Result => Found);
      Assert_True (Found);
      Assert_Cmpint_Eq (Get_Offset (Iter), 1);

      --  Go to the last "off" toggle for the bold tag
      Forward_To_Tag_Toggle (Iter, Bold_Tag, Found);
      Assert_True (Found);
      Assert_Cmpint_Eq (Get_Offset (Iter), 2);

      Forward_To_Tag_Toggle (Iter, Bold_Tag, Found);
      Assert_False (Found);

      --  Go to the first "on" toggle for the editable tag
      Buffer.Get_Start_Iter (Iter);
      Forward_To_Tag_Toggle (Iter, Editable_Tag, Found);
      Assert_True (Found);
      Assert_Cmpint_Eq (Get_Offset (Iter), 2);

      --  Test with the end iter
      Buffer.Get_End_Iter (Iter);
      Forward_To_Tag_Toggle (Iter, Editable_Tag, Found);
      Assert_False (Found);
   end Test_Forward_To_Tag_Toggle;

   ------------------------------
   -- Test_Forward_To_Line_End --
   ------------------------------

   procedure Test_Forward_To_Line_End is

      procedure Check_Forward_Line_End
        (Buffer_Text    : UTF8_String;
         Initial_Offset : Gint;
         Result_Offset  : Gint;
         Expected_Ret   : Boolean);

      ----------------------------
      -- Check_Forward_Line_End --
      ----------------------------

      procedure Check_Forward_Line_End
        (Buffer_Text    : UTF8_String;
         Initial_Offset : Gint;
         Result_Offset  : Gint;
         Expected_Ret   : Boolean)
      is
         Buffer : Gtk_Text_Buffer;
         Iter   : Gtk_Text_Iter;
         Moved  : Boolean;
      begin
         Gtk_New (Buffer);
         Buffer.Set_Text (Buffer_Text);

         Buffer.Get_Iter_At_Offset (Iter, Initial_Offset);
         Forward_To_Line_End (Iter, Moved);
         Assert_True (Expected_Ret = Moved);
         Assert_Cmpint_Eq (Result_Offset, Get_Offset (Iter));
      end Check_Forward_Line_End;

      CR : constant Character := ASCII.CR;
   begin
      Check_Forward_Line_End ("a", 0, 1, False);
      Check_Forward_Line_End ("a" & LF, 0, 1, True);
      Check_Forward_Line_End ("a" & CR & LF, 0, 1, True);
      Check_Forward_Line_End ("a" & LF & "a" & LF, 1, 3, True);
      Check_Forward_Line_End ("a" & LF & "a" & LF & LF, 1, 3, True);
      Check_Forward_Line_End ("a" & CR & LF & "a" & LF, 1, 4, True);
      Check_Forward_Line_End
        ("a" & CR & LF & "a" & CR & LF & CR & LF, 1, 4, True);
   end Test_Forward_To_Line_End;

   --------------------------
   -- Test_Word_Boundaries --
   --------------------------

   procedure Test_Word_Boundaries is

      procedure Check_Word_Boundaries
        (Buffer_Text : UTF8_String;
         Offset      : Gint;
         Starts      : Boolean;
         Ends        : Boolean;
         Inside      : Boolean);

      procedure Check_Forward_Word_End
        (Buffer_Text    : UTF8_String;
         Initial_Offset : Gint;
         Result_Offset  : Gint;
         Expected_Ret   : Boolean);

      procedure Check_Backward_Word_Start
        (Buffer_Text    : UTF8_String;
         Initial_Offset : Gint;
         Result_Offset  : Gint;
         Expected_Ret   : Boolean);

      ---------------------------
      -- Check_Word_Boundaries --
      ---------------------------

      procedure Check_Word_Boundaries
        (Buffer_Text : UTF8_String;
         Offset      : Gint;
         Starts      : Boolean;
         Ends        : Boolean;
         Inside      : Boolean)
      is
         Buffer : Gtk_Text_Buffer;
         Iter   : Gtk_Text_Iter;
      begin
         Gtk_New (Buffer);
         Buffer.Set_Text (Buffer_Text);

         Buffer.Get_Iter_At_Offset (Iter, Offset);
         Assert_True (Starts = Starts_Word (Iter));
         Assert_True (Ends = Ends_Word (Iter));
         Assert_True (Inside = Inside_Word (Iter));
      end Check_Word_Boundaries;

      ----------------------------
      -- Check_Forward_Word_End --
      ----------------------------

      procedure Check_Forward_Word_End
        (Buffer_Text    : UTF8_String;
         Initial_Offset : Gint;
         Result_Offset  : Gint;
         Expected_Ret   : Boolean)
      is
         Buffer : Gtk_Text_Buffer;
         Iter   : Gtk_Text_Iter;
         Moved  : Boolean;
      begin
         Gtk_New (Buffer);
         Buffer.Set_Text (Buffer_Text);

         Buffer.Get_Iter_At_Offset (Iter, Initial_Offset);
         Forward_Word_End (Iter, Moved);
         Assert_True (Expected_Ret = Moved);
         Assert_Cmpint_Eq (Result_Offset, Get_Offset (Iter));
      end Check_Forward_Word_End;

      -------------------------------
      -- Check_Backward_Word_Start --
      -------------------------------

      procedure Check_Backward_Word_Start
        (Buffer_Text    : UTF8_String;
         Initial_Offset : Gint;
         Result_Offset  : Gint;
         Expected_Ret   : Boolean)
      is
         Buffer : Gtk_Text_Buffer;
         Iter   : Gtk_Text_Iter;
         Moved  : Boolean;
      begin
         Gtk_New (Buffer);
         Buffer.Set_Text (Buffer_Text);

         Buffer.Get_Iter_At_Offset (Iter, Initial_Offset);
         Backward_Word_Start (Iter, Moved);
         Assert_True (Expected_Ret = Moved);
         Assert_Cmpint_Eq (Result_Offset, Get_Offset (Iter));
      end Check_Backward_Word_Start;

   begin
      --  Test with trivial content; the word boundaries are determined
      --  by Pango and could change for corner cases.
      Check_Word_Boundaries ("ab ", 0, True, False, True);
      Check_Word_Boundaries ("ab ", 1, False, False, True);
      Check_Word_Boundaries ("ab ", 2, False, True, False);
      Check_Word_Boundaries ("ab ", 3, False, False, False);
      Check_Word_Boundaries ("", 0, False, False, False);

      Check_Forward_Word_End ("ab ", 0, 2, True);
      Check_Forward_Word_End ("ab ", 1, 2, True);
      Check_Forward_Word_End ("ab ", 2, 2, False);
      Check_Forward_Word_End ("ab ", 3, 3, False);
      Check_Forward_Word_End ("ab", 0, 2, False);
      Check_Forward_Word_End ("ab" & LF, 2, 2, False);

      Check_Backward_Word_Start (" ab", 3, 1, True);
      Check_Backward_Word_Start (" ab", 2, 1, True);
      Check_Backward_Word_Start (" ab", 1, 1, False);
      Check_Backward_Word_Start (" ab", 0, 0, False);
      Check_Backward_Word_Start ("ab", 2, 0, True);
   end Test_Word_Boundaries;

   ---------------------------
   -- Test_Cursor_Positions --
   ---------------------------

   procedure Test_Cursor_Positions is

      procedure Check_Is_Cursor_Position
        (Buffer_Text  : UTF8_String;
         Offset       : Gint;
         Expected_Ret : Boolean);

      procedure Check_Cursor_Position
        (Buffer_Text    : UTF8_String;
         Forward        : Boolean;
         Initial_Offset : Gint;
         Result_Offset  : Gint;
         Expected_Ret   : Boolean);

      ------------------------------
      -- Check_Is_Cursor_Position --
      ------------------------------

      procedure Check_Is_Cursor_Position
        (Buffer_Text  : UTF8_String;
         Offset       : Gint;
         Expected_Ret : Boolean)
      is
         Buffer : Gtk_Text_Buffer;
         Iter   : Gtk_Text_Iter;
      begin
         Gtk_New (Buffer);
         Buffer.Set_Text (Buffer_Text);

         Buffer.Get_Iter_At_Offset (Iter, Offset);
         Assert_True (Expected_Ret = Is_Cursor_Position (Iter));
      end Check_Is_Cursor_Position;

      ---------------------------
      -- Check_Cursor_Position --
      ---------------------------

      procedure Check_Cursor_Position
        (Buffer_Text    : UTF8_String;
         Forward        : Boolean;
         Initial_Offset : Gint;
         Result_Offset  : Gint;
         Expected_Ret   : Boolean)
      is
         Buffer : Gtk_Text_Buffer;
         Iter   : Gtk_Text_Iter;
         Moved  : Boolean;
      begin
         Gtk_New (Buffer);
         Buffer.Set_Text (Buffer_Text);

         Buffer.Get_Iter_At_Offset (Iter, Initial_Offset);
         if Forward then
            Forward_Cursor_Position (Iter, Moved);
         else
            Backward_Cursor_Position (Iter, Moved);
         end if;
         Assert_True (Expected_Ret = Moved);
         Assert_Cmpint_Eq (Result_Offset, Get_Offset (Iter));
      end Check_Cursor_Position;

      CR : constant Character := ASCII.CR;
   begin
      --  A CR/LF pair is a single cursor position.
      Check_Is_Cursor_Position ("a" & CR & LF, 0, True);
      Check_Is_Cursor_Position ("a" & CR & LF, 1, True);
      Check_Is_Cursor_Position ("a" & CR & LF, 2, False);
      Check_Is_Cursor_Position ("a" & CR & LF, 3, True);
      Check_Is_Cursor_Position ("", 0, True);

      --  Forward
      Check_Cursor_Position ("a" & CR & LF & "b", True, 0, 1, True);
      Check_Cursor_Position ("a" & CR & LF & "b", True, 1, 3, True);
      Check_Cursor_Position ("a" & CR & LF & "b", True, 2, 3, True);
      Check_Cursor_Position ("a" & CR & LF & "b", True, 3, 4, False);
      Check_Cursor_Position ("a" & CR & LF & "b", True, 4, 4, False);
      Check_Cursor_Position ("a" & LF, True, 1, 2, False);

      --  Backward
      Check_Cursor_Position ("a" & CR & LF & "b", False, 4, 3, True);
      Check_Cursor_Position ("a" & CR & LF & "b", False, 3, 1, True);
      Check_Cursor_Position ("a" & CR & LF & "b", False, 2, 1, True);
      Check_Cursor_Position ("a" & CR & LF & "b", False, 1, 0, True);
      Check_Cursor_Position ("a" & CR & LF & "b", False, 0, 0, False);
   end Test_Cursor_Positions;

   ------------------------------
   -- Test_Sentence_Boundaries --
   ------------------------------

   procedure Test_Sentence_Boundaries is

      procedure Check_Sentence_Boundaries
        (Buffer_Text : UTF8_String;
         Offset      : Gint;
         Starts      : Boolean;
         Ends        : Boolean;
         Inside      : Boolean);

      procedure Check_Forward_Sentence_End
        (Buffer_Text    : UTF8_String;
         Initial_Offset : Gint;
         Result_Offset  : Gint;
         Expected_Ret   : Boolean);

      procedure Check_Backward_Sentence_Start
        (Buffer_Text    : UTF8_String;
         Initial_Offset : Gint;
         Result_Offset  : Gint;
         Expected_Ret   : Boolean);

      -------------------------------
      -- Check_Sentence_Boundaries --
      -------------------------------

      procedure Check_Sentence_Boundaries
        (Buffer_Text : UTF8_String;
         Offset      : Gint;
         Starts      : Boolean;
         Ends        : Boolean;
         Inside      : Boolean)
      is
         Buffer : Gtk_Text_Buffer;
         Iter   : Gtk_Text_Iter;
      begin
         Gtk_New (Buffer);
         Buffer.Set_Text (Buffer_Text);

         Buffer.Get_Iter_At_Offset (Iter, Offset);
         Assert_True (Starts = Starts_Sentence (Iter));
         Assert_True (Ends = Ends_Sentence (Iter));
         Assert_True (Inside = Inside_Sentence (Iter));
      end Check_Sentence_Boundaries;

      --------------------------------
      -- Check_Forward_Sentence_End --
      --------------------------------

      procedure Check_Forward_Sentence_End
        (Buffer_Text    : UTF8_String;
         Initial_Offset : Gint;
         Result_Offset  : Gint;
         Expected_Ret   : Boolean)
      is
         Buffer : Gtk_Text_Buffer;
         Iter   : Gtk_Text_Iter;
         Moved  : Boolean;
      begin
         Gtk_New (Buffer);
         Buffer.Set_Text (Buffer_Text);

         Buffer.Get_Iter_At_Offset (Iter, Initial_Offset);
         Forward_Sentence_End (Iter, Moved);
         Assert_True (Expected_Ret = Moved);
         Assert_Cmpint_Eq (Result_Offset, Get_Offset (Iter));
      end Check_Forward_Sentence_End;

      -----------------------------------
      -- Check_Backward_Sentence_Start --
      -----------------------------------

      procedure Check_Backward_Sentence_Start
        (Buffer_Text    : UTF8_String;
         Initial_Offset : Gint;
         Result_Offset  : Gint;
         Expected_Ret   : Boolean)
      is
         Buffer : Gtk_Text_Buffer;
         Iter   : Gtk_Text_Iter;
         Moved  : Boolean;
      begin
         Gtk_New (Buffer);
         Buffer.Set_Text (Buffer_Text);

         Buffer.Get_Iter_At_Offset (Iter, Initial_Offset);
         Backward_Sentence_Start (Iter, Moved);
         Assert_True (Expected_Ret = Moved);
         Assert_Cmpint_Eq (Result_Offset, Get_Offset (Iter));
      end Check_Backward_Sentence_Start;

   begin
      Check_Sentence_Boundaries ("Hi. ", 0, True, False, True);
      Check_Sentence_Boundaries ("Hi. ", 1, False, False, True);
      Check_Sentence_Boundaries ("Hi. ", 2, False, False, True);
      Check_Sentence_Boundaries ("Hi. ", 3, False, True, False);
      Check_Sentence_Boundaries ("Hi. ", 4, False, False, False);
      Check_Sentence_Boundaries ("", 0, False, False, False);

      Check_Forward_Sentence_End ("Hi. ", 0, 3, True);
      Check_Forward_Sentence_End ("Hi. ", 1, 3, True);
      Check_Forward_Sentence_End ("Hi. ", 2, 3, True);
      Check_Forward_Sentence_End ("Hi. ", 3, 3, False);
      Check_Forward_Sentence_End ("Hi. ", 4, 4, False);
      Check_Forward_Sentence_End ("Hi.", 0, 3, False);
      Check_Forward_Sentence_End ("Hi." & LF, 3, 3, False);

      Check_Backward_Sentence_Start (" Hi.", 4, 1, True);
      Check_Backward_Sentence_Start (" Hi.", 3, 1, True);
      Check_Backward_Sentence_Start (" Hi.", 2, 1, True);
      Check_Backward_Sentence_Start (" Hi.", 1, 1, False);
      Check_Backward_Sentence_Start (" Hi.", 0, 0, False);
   end Test_Sentence_Boundaries;

   ------------------------
   -- Test_Backward_Line --
   ------------------------

   procedure Test_Backward_Line is
      Buffer  : Gtk_Text_Buffer;
      Iter    : Gtk_Text_Iter;
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter;
      Moved   : Boolean;
   begin
      Gtk_New (Buffer);
      Buffer.Get_Start_Iter (Iter);
      Buffer.Insert (Iter, "Hi line 1" & LF & "Hi line 2");

      --  Go to the middle of the first line
      Backward_Line (Iter, Moved);
      Set_Line_Offset (Iter, 4);

      --  Now insert some chars with Insert_Range
      Buffer.Get_End_Iter (The_End);
      Copy (Source => The_End, Dest => Start);
      Backward_Cursor_Positions (Start, 5, Moved);
      Buffer.Insert_Range (Iter, Start, The_End);

      --  Check that we are still on the first line
      Assert_Cmpint_Eq (Get_Line (Iter), 0);

      --  Backward_Line moves to the start of the line (or stays there,
      --  returning False, when already at the start); either way the
      --  iter must end up at line offset 0.
      Backward_Line (Iter, Moved);
      Assert_True (Moved);
      Assert_Cmpint_Eq (Get_Line_Offset (Iter), 0);
   end Test_Backward_Line;

   -------------------------
   -- Test_Invisible_Text --
   -------------------------

   procedure Test_Invisible_Text is
      Buffer  : Gtk_Text_Buffer;
      Iter    : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter;
      Tag     : Gtk_Text_Tag;
      Tags    : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table;
      Moved   : Boolean;
      Added   : Boolean;
   begin
      Gtk_New (Buffer);

      Gtk_New (Tag, "invisible");
      Glib.Properties.Set_Property
        (Tag, Gtk.Text_Tag.Invisible_Property, True);

      Tags := Buffer.Get_Tag_Table;
      Added := Tags.Add (Tag);
      Assert_True (Added);

      Buffer.Get_Start_Iter (Iter);
      Buffer.Insert (Iter, "one ");
      Buffer.Insert_With_Tags (Iter, "two three", Tag);
      Buffer.Insert (Iter, " four");

      Buffer.Get_Start_Iter (Iter);
      Buffer.Get_End_Iter (The_End);

      Forward_Visible_Word_End (Iter, Moved);
      Assert_Cmpstr_Eq
        (Buffer.Get_Text (Iter, The_End, Include_Hidden_Chars => True),
         " two three four");

      Forward_Word_End (Iter, Moved);
      Assert_Cmpstr_Eq
        (Buffer.Get_Text (Iter, The_End, Include_Hidden_Chars => True),
         " three four");

      Forward_Visible_Word_End (Iter, Moved);
      Assert_Cmpstr_Eq
        (Buffer.Get_Text (Iter, The_End, Include_Hidden_Chars => True),
         " four");

      Forward_Visible_Word_End (Iter, Moved);
      Assert_True (Equal (Iter, The_End));

      --  Get_Visible_Text skips the tagged-invisible segment.
      Buffer.Get_Start_Iter (Iter);
      Assert_Cmpstr_Eq (Get_Visible_Text (Iter, The_End), "one  four");
   end Test_Invisible_Text;

begin
   Glib.Test.Init;

   --  The C test uses gtk_test_init, which also initializes GTK; buffers
   --  cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/text-iter/search-empty", Test_Empty_Search'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-iter/search-full-buffer",
      Test_Search_Full_Buffer'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-iter/search", Test_Search'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-iter/search-caseless",
      Test_Search_Caseless'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-iter/forward-to-tag-toggle",
      Test_Forward_To_Tag_Toggle'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-iter/forward-to-line-end",
      Test_Forward_To_Line_End'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-iter/word-boundaries",
      Test_Word_Boundaries'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-iter/cursor-positions",
      Test_Cursor_Positions'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-iter/sentence-boundaries",
      Test_Sentence_Boundaries'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-iter/backward-line", Test_Backward_Line'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-iter/invisible-text",
      Test_Invisible_Text'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
