--  Ada port of a representative subset of GTK's
--  testsuite/gtk/textbuffer.c (GTK 4.22.2), as agreed on the work item.
--
--  Gtk.Text_Buffer is a pure data model, so the whole suite runs without
--  a realized window: it exercises creation, get/set of the contents,
--  iterator lookup, insertion and deletion, marks, tags, child anchors,
--  selection handling, undo, and signal emission.
--
--  The C tests test_iter_with_anchor / test_get_text_with_anchor attach
--  child widgets through a GtkTextView; the view is not bound yet (it is
--  a follow-up work item), so the child-anchor test below keeps the
--  buffer-side assertions only, which do not need a widget. The
--  clipboard, line-separator, logical-motion and serialize tests are
--  omitted from this representative subset.

with Ada.Command_Line;
with System;                use type System.Address;

with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Test;             use Glib.Test;

with Gtk.Main;
with Gtk.Text_Buffer;       use Gtk.Text_Buffer;
with Gtk.Text_Child_Anchor; use Gtk.Text_Child_Anchor;
with Gtk.Text_Iter;         use Gtk.Text_Iter;
with Gtk.Text_Mark;         use Gtk.Text_Mark;
with Gtk.Text_Tag;          use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;    use Gtk.Text_Tag_Table;

procedure Main is

   --  "ß" (U+00DF), which takes 2 bytes in UTF-8
   Sharp_S : constant UTF8_String :=
     Character'Val (16#C3#) & Character'Val (16#9F#);

   Unknown_Char : constant Gunichar := 16#FFFC#;
   --  The Unicode "object replacement character" used for child anchors

   function UTF8_Strlen (Text : UTF8_String) return Gint;
   --  Number of characters (not bytes) in Text.

   procedure Check_Buffer_Contents
     (Buffer   : Gtk_Text_Buffer;
      Contents : UTF8_String);
   --  Analogue of C check_buffer_contents.

   -----------------
   -- UTF8_Strlen --
   -----------------

   function UTF8_Strlen (Text : UTF8_String) return Gint is
      N : Gint := 0;
   begin
      for C of Text loop
         --  Count all bytes except UTF-8 continuation bytes (10xxxxxx)
         if Character'Pos (C) not in 16#80# .. 16#BF# then
            N := N + 1;
         end if;
      end loop;
      return N;
   end UTF8_Strlen;

   ---------------------------
   -- Check_Buffer_Contents --
   ---------------------------

   procedure Check_Buffer_Contents
     (Buffer   : Gtk_Text_Buffer;
      Contents : UTF8_String)
   is
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter;
   begin
      Buffer.Get_Bounds (Start, The_End);
      Assert_Cmpstr_Eq (Buffer.Get_Text (Start, The_End), Contents);
   end Check_Buffer_Contents;

   procedure Test_Empty_Buffer
   with Convention => C;

   procedure Test_Get_Set
   with Convention => C;

   procedure Test_Get_Iter
   with Convention => C;

   procedure Test_Insert_Delete
   with Convention => C;

   procedure Test_Marks
   with Convention => C;

   procedure Test_Tag
   with Convention => C;

   procedure Test_Child_Anchor
   with Convention => C;

   procedure Test_Selection
   with Convention => C;

   procedure Test_Undo
   with Convention => C;

   procedure Test_Signals
   with Convention => C;

   -----------------------
   -- Test_Empty_Buffer --
   -----------------------

   --  Check that a buffer starts with one empty line and zero chars.

   procedure Test_Empty_Buffer is
      Buffer : Gtk_Text_Buffer;
      Start  : Gtk_Text_Iter;
   begin
      Gtk_New (Buffer);

      Assert_Cmpint_Eq (Buffer.Get_Line_Count, 1);
      Assert_Cmpint_Eq (Buffer.Get_Char_Count, 0);

      --  The empty first line contains 0 chars and 0 bytes.
      Buffer.Get_Start_Iter (Start);
      Assert_Cmpint_Eq (Get_Chars_In_Line (Start), 0);
      Assert_Cmpint_Eq (Get_Bytes_In_Line (Start), 0);
   end Test_Empty_Buffer;

   ------------------
   -- Test_Get_Set --
   ------------------

   --  Analogue of C check_get_set_text, applied to the same strings.

   procedure Test_Get_Set is

      procedure Check_Get_Set_Text
        (Buffer : Gtk_Text_Buffer;
         Str    : UTF8_String);

      ------------------------
      -- Check_Get_Set_Text --
      ------------------------

      procedure Check_Get_Set_Text
        (Buffer : Gtk_Text_Buffer;
         Str    : UTF8_String)
      is
         Start   : Gtk_Text_Iter;
         The_End : Gtk_Text_Iter;
         Iter    : Gtk_Text_Iter;
         N       : Gint;
         Moved   : Boolean;
      begin
         Buffer.Set_Text (Str);
         Assert_Cmpint_Eq (Buffer.Get_Char_Count, UTF8_Strlen (Str));

         Buffer.Get_Bounds (Start, The_End);
         Assert_Cmpstr_Eq
           (Buffer.Get_Text (Start, The_End, Include_Hidden_Chars => True),
            Str);

         --  Line char counts must sum to the buffer char count.
         Copy (Source => Start, Dest => Iter);
         N := 0;
         loop
            N := N + Get_Chars_In_Line (Iter);
            Forward_Line (Iter, Moved);
            exit when not Moved;
         end loop;
         Assert_Cmpint_Eq (N, Buffer.Get_Char_Count);

         --  Line byte counts must sum to the buffer byte count.
         Copy (Source => Start, Dest => Iter);
         N := 0;
         loop
            N := N + Get_Bytes_In_Line (Iter);
            Forward_Line (Iter, Moved);
            exit when not Moved;
         end loop;
         Assert_Cmpint_Eq (N, Str'Length);

         Buffer.Set_Text ("");
         Assert_Cmpint_Eq (Buffer.Get_Line_Count, 1);
         Assert_Cmpint_Eq (Buffer.Get_Char_Count, 0);
      end Check_Get_Set_Text;

      Buffer : Gtk_Text_Buffer;
   begin
      Gtk_New (Buffer);

      Check_Get_Set_Text (Buffer, "Hello");
      Check_Get_Set_Text (Buffer, "Hello" & ASCII.LF);
      Check_Get_Set_Text (Buffer, "Hello" & ASCII.CR & ASCII.LF);
      Check_Get_Set_Text (Buffer, "Hello" & ASCII.CR);
      Check_Get_Set_Text (Buffer, "Hello" & ASCII.LF & "Bar" & ASCII.LF
                          & "Foo");
      Check_Get_Set_Text (Buffer, "Hello" & ASCII.LF & "Bar" & ASCII.LF
                          & "Foo" & ASCII.LF);
   end Test_Get_Set;

   -------------------
   -- Test_Get_Iter --
   -------------------

   --  Port of C test_get_iter: Get_Iter_At_Line, Get_Iter_At_Line_Offset,
   --  Get_Iter_At_Line_Index and Get_Iter_At_Offset on a buffer holding
   --  multi-byte characters and a CR/LF line end.

   procedure Test_Get_Iter is
      Buffer : Gtk_Text_Buffer;
      Iter   : aliased Gtk_Text_Iter;
   begin
      Gtk_New (Buffer);

      Buffer.Set_Text ("ab" & ASCII.LF & Sharp_S & "d" & ASCII.CR & ASCII.LF
                       & "ef");

      --  Get_Iter_At_Line
      Assert_True (Buffer.Get_Iter_At_Line (Iter'Access, 0));
      Assert_True (Is_Start (Iter));

      Assert_True (Buffer.Get_Iter_At_Line (Iter'Access, 1));
      Assert_Cmpint_Eq (Get_Offset (Iter), 3);

      Assert_True (Buffer.Get_Iter_At_Line (Iter'Access, 2));
      Assert_Cmpint_Eq (Get_Offset (Iter), 7);

      Assert_False (Buffer.Get_Iter_At_Line (Iter'Access, 3));
      Assert_True (Is_End (Iter));

      --  Get_Iter_At_Line_Offset
      Assert_True (Buffer.Get_Iter_At_Line_Offset (Iter'Access, 0, 0));
      Assert_True (Is_Start (Iter));

      Assert_True (Buffer.Get_Iter_At_Line_Offset (Iter'Access, 0, 1));
      Assert_Cmpint_Eq (Get_Offset (Iter), 1);

      Assert_True (Buffer.Get_Iter_At_Line_Offset (Iter'Access, 0, 2));
      Assert_Cmpint_Eq (Get_Offset (Iter), 2);

      Assert_False (Buffer.Get_Iter_At_Line_Offset (Iter'Access, 0, 3));
      Assert_Cmpint_Eq (Get_Offset (Iter), 2);

      Assert_True (Buffer.Get_Iter_At_Line_Offset (Iter'Access, 1, 1));
      Assert_Cmpint_Eq (Get_Offset (Iter), 4);

      Assert_True (Buffer.Get_Iter_At_Line_Offset (Iter'Access, 2, 1));
      Assert_Cmpint_Eq (Get_Offset (Iter), 8);

      Assert_True (Buffer.Get_Iter_At_Line_Offset (Iter'Access, 2, 2));
      Assert_True (Is_End (Iter));

      Assert_False (Buffer.Get_Iter_At_Line_Offset (Iter'Access, 2, 3));
      Assert_True (Is_End (Iter));

      Assert_False (Buffer.Get_Iter_At_Line_Offset (Iter'Access, 3, 1));
      Assert_True (Is_End (Iter));

      --  Get_Iter_At_Line_Index ("ß" takes 2 bytes in UTF-8)
      Assert_True (Buffer.Get_Iter_At_Line_Index (Iter'Access, 0, 0));
      Assert_True (Is_Start (Iter));

      Assert_True (Buffer.Get_Iter_At_Line_Index (Iter'Access, 0, 1));
      Assert_Cmpint_Eq (Get_Offset (Iter), 1);

      Assert_True (Buffer.Get_Iter_At_Line_Index (Iter'Access, 0, 2));
      Assert_Cmpint_Eq (Get_Offset (Iter), 2);

      Assert_False (Buffer.Get_Iter_At_Line_Index (Iter'Access, 0, 3));
      Assert_Cmpint_Eq (Get_Offset (Iter), 2);

      Assert_True (Buffer.Get_Iter_At_Line_Index (Iter'Access, 1, 0));
      Assert_Cmpint_Eq (Get_Offset (Iter), 3);

      Assert_True (Buffer.Get_Iter_At_Line_Index (Iter'Access, 1, 2));
      Assert_Cmpint_Eq (Get_Offset (Iter), 4);

      Assert_True (Buffer.Get_Iter_At_Line_Index (Iter'Access, 1, 3));
      Assert_Cmpint_Eq (Get_Offset (Iter), 5);

      Assert_True (Buffer.Get_Iter_At_Line_Index (Iter'Access, 2, 2));
      Assert_True (Is_End (Iter));

      Assert_False (Buffer.Get_Iter_At_Line_Index (Iter'Access, 2, 3));
      Assert_True (Is_End (Iter));

      Assert_False (Buffer.Get_Iter_At_Line_Index (Iter'Access, 3, 1));
      Assert_True (Is_End (Iter));

      --  Get_Iter_At_Offset
      Buffer.Get_Iter_At_Offset (Iter, 0);
      Assert_True (Is_Start (Iter));

      Buffer.Get_Iter_At_Offset (Iter, 1);
      Assert_Cmpint_Eq (Get_Offset (Iter), 1);

      Buffer.Get_Iter_At_Offset (Iter, 8);
      Assert_Cmpint_Eq (Get_Offset (Iter), 8);
      Assert_False (Is_End (Iter));

      Buffer.Get_Iter_At_Offset (Iter, 9);
      Assert_True (Is_End (Iter));

      Buffer.Get_Iter_At_Offset (Iter, 100);
      Assert_True (Is_End (Iter));

      Buffer.Get_Iter_At_Offset (Iter, -1);
      Assert_True (Is_End (Iter));
   end Test_Get_Iter;

   ------------------------
   -- Test_Insert_Delete --
   ------------------------

   --  Insertion at an iterator, range deletion and backspace.

   procedure Test_Insert_Delete is
      Buffer  : Gtk_Text_Buffer;
      Iter    : Gtk_Text_Iter;
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter;
      Result  : Boolean;
   begin
      Gtk_New (Buffer);

      Buffer.Set_Text ("Hello World");

      --  Insert revalidates the iter to the end of the inserted text.
      Buffer.Get_Iter_At_Offset (Iter, 5);
      Buffer.Insert (Iter, ",");
      Assert_Cmpint_Eq (Get_Offset (Iter), 6);
      Check_Buffer_Contents (Buffer, "Hello, World");

      --  Delete the inserted range again.
      Buffer.Get_Iter_At_Offset (Start, 5);
      Buffer.Get_Iter_At_Offset (The_End, 6);
      Buffer.Delete (Start, The_End);
      Check_Buffer_Contents (Buffer, "Hello World");
      Assert_Cmpint_Eq (Get_Offset (Start), 5);

      --  Backspace deletes the character before the iter.
      Buffer.Get_Iter_At_Offset (Iter, 5);
      Buffer.Backspace (Iter, Interactive => False, Result => Result);
      Assert_True (Result);
      Check_Buffer_Contents (Buffer, "Hell World");

      --  Get_Slice and Get_Text agree on plain text.
      Buffer.Get_Bounds (Start, The_End);
      Assert_Cmpstr_Eq (Buffer.Get_Slice (Start, The_End), "Hell World");
   end Test_Insert_Delete;

   ----------------
   -- Test_Marks --
   ----------------

   --  Port of C test_marks: marks surviving deletion, Add_Mark moving a
   --  mark between buffers, and standalone Gtk_New marks.

   procedure Test_Marks is
      Buf1 : Gtk_Text_Buffer;
      Buf2 : Gtk_Text_Buffer;
      Mark : Gtk_Text_Mark;
      Iter : Gtk_Text_Iter;
   begin
      Gtk_New (Buf1);
      Gtk_New (Buf2);

      Buf1.Get_Start_Iter (Iter);
      Mark := Buf1.Create_Mark ("foo", Iter, Left_Gravity => True);
      Mark.Ref;
      Mark.Set_Visible (True);
      Buf1.Delete_Mark (Mark);

      Assert_True (Mark.Get_Visible);
      Assert_True (Mark.Get_Left_Gravity);
      Assert_Cmpstr_Eq ("foo", Mark.Get_Name);
      Assert_True (Get_Buffer (Mark) = null);
      Assert_True (Mark.Get_Deleted);
      Assert_True (Buf1.Get_Mark ("foo") = null);

      Buf2.Get_Start_Iter (Iter);
      Buf2.Add_Mark (Mark, Iter);
      Buf2.Insert (Iter, "ewfwefwefwe");
      Buf2.Get_Iter_At_Mark (Iter, Mark);

      Assert_True (Mark.Get_Visible);
      Assert_True (Is_Start (Iter));
      Assert_True (Mark.Get_Left_Gravity);
      Assert_Cmpstr_Eq ("foo", Mark.Get_Name);
      Assert_True (Get_Buffer (Mark).Get_Object = Buf2.Get_Object);
      Assert_False (Mark.Get_Deleted);
      Assert_True (Buf2.Get_Mark ("foo").Get_Object = Mark.Get_Object);

      Buf2.Delete_Mark (Mark);
      Mark.Set_Visible (False);
      Mark.Unref;

      Gtk_New (Mark, "blah", Left_Gravity => True);
      Buf1.Get_Start_Iter (Iter);
      Mark.Set_Visible (True);
      Buf1.Add_Mark (Mark, Iter);

      Assert_True (Mark.Get_Visible);
      Assert_True (Get_Buffer (Mark).Get_Object = Buf1.Get_Object);
      Assert_False (Mark.Get_Deleted);
      Assert_True (Buf1.Get_Mark ("blah").Get_Object = Mark.Get_Object);
      Assert_Cmpstr_Eq ("blah", Mark.Get_Name);

      Mark.Set_Visible (False);
      Buf1.Delete_Mark (Mark);
      Assert_False (Mark.Get_Visible);
      Assert_True (Buf1.Get_Mark ("blah") = null);
      Assert_True (Get_Buffer (Mark) = null);
      Assert_True (Mark.Get_Deleted);

      Buf2.Get_Start_Iter (Iter);
      Buf2.Add_Mark (Mark, Iter);
      Assert_True (Get_Buffer (Mark).Get_Object = Buf2.Get_Object);
      Assert_False (Mark.Get_Deleted);
      Assert_True (Buf2.Get_Mark ("blah").Get_Object = Mark.Get_Object);
      Assert_Cmpstr_Eq ("blah", Mark.Get_Name);
   end Test_Marks;

   --------------
   -- Test_Tag --
   --------------

   --  Tag creation, apply/remove (by object and by name), tag predicates
   --  on iterators, and the buffer's tag table.

   procedure Test_Tag is
      Buffer  : Gtk_Text_Buffer;
      Tag     : Gtk_Text_Tag;
      Other   : Gtk_Text_Tag;
      Table   : Gtk_Text_Tag_Table;
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter;
      Iter    : Gtk_Text_Iter;
   begin
      Gtk_New (Buffer);

      Tag := Buffer.Create_Tag ("fg_blue");
      Other := Buffer.Create_Tag ("fg_red");

      Table := Buffer.Get_Tag_Table;
      Assert_Cmpint_Eq (Table.Get_Size, 2);
      Assert_True (Table.Lookup ("fg_blue").Get_Object = Tag.Get_Object);
      Assert_True (Table.Lookup ("nosuchtag") = null);

      --  Tag priorities follow the order of addition to the table.
      Assert_Cmpint_Eq (Tag.Get_Priority, 0);
      Assert_Cmpint_Eq (Other.Get_Priority, 1);

      Buffer.Set_Text ("adcdef");
      Buffer.Get_Iter_At_Offset (Start, 1);
      Buffer.Get_Iter_At_Offset (The_End, 3);
      Buffer.Apply_Tag (Tag, Start, The_End);

      --  The tag toggles on at 1, applies to [1, 3) and toggles off at 3.
      Buffer.Get_Iter_At_Offset (Iter, 1);
      Assert_True (Starts_Tag (Iter, Tag));
      Assert_True (Toggles_Tag (Iter, Tag));
      Assert_True (Has_Tag (Iter, Tag));

      Buffer.Get_Iter_At_Offset (Iter, 2);
      Assert_False (Starts_Tag (Iter, Tag));
      Assert_False (Ends_Tag (Iter, Tag));
      Assert_True (Has_Tag (Iter, Tag));

      Buffer.Get_Iter_At_Offset (Iter, 3);
      Assert_True (Ends_Tag (Iter, Tag));
      Assert_True (Toggles_Tag (Iter, Tag));
      Assert_False (Has_Tag (Iter, Tag));

      --  Get_Tags returns the applied tag inside the range.
      Buffer.Get_Iter_At_Offset (Iter, 2);
      declare
         use Gtk.Text_Tag.Text_Tag_List;
         Tags : GSlist := Get_Tags (Iter);
      begin
         Assert_Cmpuint_Eq (Length (Tags), 1);
         Free (Tags);
      end;

      --  Remove the tag from the range again.
      Buffer.Get_Iter_At_Offset (Start, 1);
      Buffer.Get_Iter_At_Offset (The_End, 3);
      Buffer.Remove_Tag (Tag, Start, The_End);
      Buffer.Get_Iter_At_Offset (Iter, 2);
      Assert_False (Has_Tag (Iter, Tag));

      --  Apply by name, remove all.
      Buffer.Apply_Tag_By_Name ("fg_blue", Start, The_End);
      Buffer.Apply_Tag_By_Name ("fg_red", Start, The_End);
      Buffer.Get_Iter_At_Offset (Iter, 2);
      Assert_True (Has_Tag (Iter, Tag));
      Assert_True (Has_Tag (Iter, Other));
      Buffer.Remove_All_Tags (Start, The_End);
      Assert_False (Has_Tag (Iter, Tag));
      Assert_False (Has_Tag (Iter, Other));

      --  A standalone tag table.
      declare
         Lone_Table : Gtk_Text_Tag_Table;
         Lone_Tag   : Gtk_Text_Tag;
      begin
         Gtk_New (Lone_Table);
         Gtk_New (Lone_Tag, "lonesome");
         Assert_Cmpint_Eq (Lone_Table.Get_Size, 0);
         Assert_True (Lone_Table.Add (Lone_Tag));
         Assert_Cmpint_Eq (Lone_Table.Get_Size, 1);
         Assert_True
           (Lone_Table.Lookup ("lonesome").Get_Object = Lone_Tag.Get_Object);
         Lone_Table.Remove (Lone_Tag);
         Assert_Cmpint_Eq (Lone_Table.Get_Size, 0);
         Assert_True (Lone_Table.Lookup ("lonesome") = null);
      end;
   end Test_Tag;

   -----------------------
   -- Test_Child_Anchor --
   -----------------------

   --  Buffer-side port of C test_iter_with_anchor /
   --  test_get_text_with_anchor (the child widgets, which need the
   --  not-yet-bound GtkTextView, are omitted).

   procedure Test_Child_Anchor is
      Buffer  : Gtk_Text_Buffer;
      Iter    : aliased Gtk_Text_Iter;
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter;
      Anchor  : Gtk_Text_Child_Anchor;
      Repl    : Gtk_Text_Child_Anchor;
      Chars   : Gint;
   begin
      Gtk_New (Buffer);

      Buffer.Set_Text ("ab" & ASCII.LF & "cd" & ASCII.CR & ASCII.LF & "ef");
      Chars := Buffer.Get_Char_Count;

      --  An anchor counts as one character.
      Assert_True (Buffer.Get_Iter_At_Line_Offset (Iter'Access, 0, 1));
      Anchor := Buffer.Create_Child_Anchor (Iter);
      Assert_Cmpint_Eq (Buffer.Get_Char_Count, Chars + 1);

      --  The anchor reads back as the unknown char at its location.
      Buffer.Get_Iter_At_Child_Anchor (Iter, Anchor);
      Assert_True (Get_Char (Iter) = Unknown_Char);
      Assert_False (Anchor.Get_Deleted);

      --  An anchor with a replacement character ("ß" takes 2 bytes).
      Assert_True (Buffer.Get_Iter_At_Line_Offset (Iter'Access, 1, 1));
      Gtk_New_With_Replacement (Repl, Sharp_S);
      Buffer.Insert_Child_Anchor (Iter, Repl);

      Buffer.Get_Iter_At_Child_Anchor (Iter, Repl);
      Assert_True (Get_Char (Iter) = 16#DF#);

      --  Get_Text shows the replacement character but skips the plain
      --  anchor; Get_Slice represents every anchor as one character.
      Buffer.Get_Bounds (Start, The_End);
      Assert_Cmpstr_Eq
        (Buffer.Get_Text (Start, The_End),
         "ab" & ASCII.LF & "c" & Sharp_S & "d" & ASCII.CR & ASCII.LF & "ef");
      Assert_Cmpint_Eq
        (UTF8_Strlen (Buffer.Get_Slice (Start, The_End)),
         Buffer.Get_Char_Count);

      --  Deleting the range around an anchor marks it deleted.
      declare
         Moved : Boolean;
      begin
         Repl.Ref;
         Buffer.Get_Iter_At_Child_Anchor (Start, Repl);
         Copy (Source => Start, Dest => The_End);
         Forward_Char (The_End, Moved);
         Assert_True (Moved);
         Buffer.Delete (Start, The_End);
         Assert_True (Repl.Get_Deleted);
         Repl.Unref;
      end;
   end Test_Child_Anchor;

   --------------------
   -- Test_Selection --
   --------------------

   --  Select_Range / Get_Selection_Bounds / Delete_Selection, plus the
   --  insert and selection_bound marks.

   procedure Test_Selection is
      Buffer  : Gtk_Text_Buffer;
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter;
      Iter    : Gtk_Text_Iter;
      Result  : Boolean;
   begin
      Gtk_New (Buffer);

      Buffer.Set_Text ("Hello World");

      --  No selection initially.
      Assert_False (Buffer.Get_Has_Selection);
      Buffer.Get_Selection_Bounds (Start, The_End, Result);
      Assert_False (Result);
      Assert_Cmpint_Eq (Get_Offset (Start), Get_Offset (The_End));

      --  Select "World".
      Buffer.Get_Iter_At_Offset (Start, 6);
      Buffer.Get_Iter_At_Offset (The_End, 11);
      Buffer.Select_Range (Start, The_End);
      Assert_True (Buffer.Get_Has_Selection);
      Assert_True (Buffer.Selection_Exists);

      Buffer.Get_Selection_Bounds (Start, The_End, Result);
      Assert_True (Result);
      Assert_Cmpint_Eq (Get_Offset (Start), 6);
      Assert_Cmpint_Eq (Get_Offset (The_End), 11);
      Assert_Cmpstr_Eq (Buffer.Get_Text (Start, The_End), "World");

      --  The selection is the region between the insert and
      --  selection_bound marks.
      Buffer.Get_Iter_At_Mark (Iter, Buffer.Get_Insert);
      Assert_Cmpint_Eq (Get_Offset (Iter), 6);
      Buffer.Get_Iter_At_Mark (Iter, Buffer.Get_Selection_Bound);
      Assert_Cmpint_Eq (Get_Offset (Iter), 11);

      --  Delete the selection.
      Result := Buffer.Delete_Selection (Interactive => False);
      Assert_True (Result);
      Check_Buffer_Contents (Buffer, "Hello ");
      Assert_False (Buffer.Get_Has_Selection);

      --  Place_Cursor collapses the selection.
      Buffer.Get_Bounds (Start, The_End);
      Buffer.Select_Range (Start, The_End);
      Assert_True (Buffer.Get_Has_Selection);
      Buffer.Get_Start_Iter (Iter);
      Buffer.Place_Cursor (Iter);
      Assert_False (Buffer.Get_Has_Selection);
   end Test_Selection;

   ---------------
   -- Test_Undo --
   ---------------

   --  Port of C test_undo0: basic undo of Insert_At_Cursor actions.

   procedure Test_Undo is
      Buffer : Gtk_Text_Buffer;
      Text_1 : constant UTF8_String :=
        "The quick brown fox jumps over the lazy dog.";
      Text_2 : constant UTF8_String := "Portez ce vieux whisky" & Sharp_S;
   begin
      Gtk_New (Buffer);

      Assert_True (Buffer.Get_Enable_Undo);
      Assert_False (Buffer.Get_Can_Undo);

      --  Set_Text is irreversible, so it does not enable undo.
      Buffer.Set_Text ("text before");
      Check_Buffer_Contents (Buffer, "text before");
      Assert_False (Buffer.Get_Can_Undo);

      Buffer.Insert_At_Cursor (Text_1);
      Check_Buffer_Contents (Buffer, "text before" & Text_1);
      Assert_True (Buffer.Get_Can_Undo);

      Buffer.Insert_At_Cursor (Text_2);
      Check_Buffer_Contents (Buffer, "text before" & Text_1 & Text_2);
      Assert_True (Buffer.Get_Can_Undo);

      Buffer.Undo;
      Check_Buffer_Contents (Buffer, "text before" & Text_1);
      Assert_True (Buffer.Get_Can_Undo);
      Assert_True (Buffer.Get_Can_Redo);

      Buffer.Undo;
      Check_Buffer_Contents (Buffer, "text before");
      Assert_False (Buffer.Get_Can_Undo);

      Buffer.Redo;
      Check_Buffer_Contents (Buffer, "text before" & Text_1);
   end Test_Undo;

   ------------------
   -- Test_Signals --
   ------------------

   --  Buffer modifications emit the matching signals, marshalled through
   --  the re-enabled Unchecked_To_Gtk_Text_Iter machinery.

   Insert_Count    : Natural := 0;
   Delete_Count    : Natural := 0;
   Changed_Count   : Natural := 0;
   Mark_Set_Count  : Natural := 0;
   Apply_Tag_Count : Natural := 0;
   Last_Inserted   : Gint    := -1;

   procedure On_Insert_Text_Cb
     (Self     : access Gtk_Text_Buffer_Record'Class;
      Location : Gtk_Text_Iter;
      Text     : UTF8_String;
      Len      : Gint);

   procedure On_Delete_Range_Cb
     (Self    : access Gtk_Text_Buffer_Record'Class;
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter);

   procedure On_Changed_Cb (Self : access Gtk_Text_Buffer_Record'Class);

   procedure On_Mark_Set_Cb
     (Self     : access Gtk_Text_Buffer_Record'Class;
      Location : Gtk_Text_Iter;
      Mark     : not null access Gtk_Text_Mark_Record'Class);

   procedure On_Apply_Tag_Cb
     (Self    : access Gtk_Text_Buffer_Record'Class;
      Tag     : not null access Gtk_Text_Tag_Record'Class;
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter);

   -----------------------
   -- On_Insert_Text_Cb --
   -----------------------

   procedure On_Insert_Text_Cb
     (Self     : access Gtk_Text_Buffer_Record'Class;
      Location : Gtk_Text_Iter;
      Text     : UTF8_String;
      Len      : Gint)
   is
      pragma Unreferenced (Self, Text, Len);
   begin
      Insert_Count := Insert_Count + 1;
      Last_Inserted := Get_Offset (Location);
   end On_Insert_Text_Cb;

   ------------------------
   -- On_Delete_Range_Cb --
   ------------------------

   procedure On_Delete_Range_Cb
     (Self    : access Gtk_Text_Buffer_Record'Class;
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter)
   is
      pragma Unreferenced (Self, Start, The_End);
   begin
      Delete_Count := Delete_Count + 1;
   end On_Delete_Range_Cb;

   -------------------
   -- On_Changed_Cb --
   -------------------

   procedure On_Changed_Cb (Self : access Gtk_Text_Buffer_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Changed_Count := Changed_Count + 1;
   end On_Changed_Cb;

   --------------------
   -- On_Mark_Set_Cb --
   --------------------

   procedure On_Mark_Set_Cb
     (Self     : access Gtk_Text_Buffer_Record'Class;
      Location : Gtk_Text_Iter;
      Mark     : not null access Gtk_Text_Mark_Record'Class)
   is
      pragma Unreferenced (Self, Location, Mark);
   begin
      Mark_Set_Count := Mark_Set_Count + 1;
   end On_Mark_Set_Cb;

   ---------------------
   -- On_Apply_Tag_Cb --
   ---------------------

   procedure On_Apply_Tag_Cb
     (Self    : access Gtk_Text_Buffer_Record'Class;
      Tag     : not null access Gtk_Text_Tag_Record'Class;
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter)
   is
      pragma Unreferenced (Self, Tag);
   begin
      Apply_Tag_Count := Apply_Tag_Count + 1;
      Assert_Cmpint_Eq (Get_Offset (Start), 0);
      Assert_Cmpint_Eq (Get_Offset (The_End), 5);
   end On_Apply_Tag_Cb;

   procedure Test_Signals is
      Buffer  : Gtk_Text_Buffer;
      Tag     : Gtk_Text_Tag;
      Mark    : Gtk_Text_Mark;
      Iter    : Gtk_Text_Iter;
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter;
   begin
      Gtk_New (Buffer);
      Tag := Buffer.Create_Tag ("bold");

      Buffer.On_Insert_Text (On_Insert_Text_Cb'Unrestricted_Access);
      Buffer.On_Delete_Range (On_Delete_Range_Cb'Unrestricted_Access);
      Buffer.On_Changed (On_Changed_Cb'Unrestricted_Access);
      Buffer.On_Mark_Set (On_Mark_Set_Cb'Unrestricted_Access);
      Buffer.On_Apply_Tag (On_Apply_Tag_Cb'Unrestricted_Access);

      --  insert-text and changed fire on insertion; our handler runs
      --  before the default one, so it sees the original insertion
      --  location.
      Buffer.Get_Start_Iter (Iter);
      Buffer.Insert (Iter, "Hello");
      Assert_Cmpint_Eq (Gint (Insert_Count), 1);
      Assert_Cmpint_Eq (Gint (Changed_Count), 1);
      Assert_Cmpint_Eq (Last_Inserted, 0);

      --  mark-set fires when a mark is created or moved.
      Buffer.Get_Start_Iter (Iter);
      Mark := Buffer.Create_Mark ("here", Iter);
      Assert_Cmpint_Eq (Gint (Mark_Set_Count), 1);
      Buffer.Get_End_Iter (Iter);
      Buffer.Move_Mark (Mark, Iter);
      Assert_Cmpint_Eq (Gint (Mark_Set_Count), 2);

      --  apply-tag fires with the tagged range.
      Buffer.Get_Bounds (Start, The_End);
      Buffer.Apply_Tag (Tag, Start, The_End);
      Assert_Cmpint_Eq (Gint (Apply_Tag_Count), 1);

      --  delete-range and changed fire on deletion.
      Buffer.Get_Bounds (Start, The_End);
      Buffer.Delete (Start, The_End);
      Assert_Cmpint_Eq (Gint (Delete_Count), 1);
      Assert_Cmpint_Eq (Gint (Changed_Count), 2);
   end Test_Signals;

begin
   Glib.Test.Init;

   --  The C test uses gtk_test_init, which also initializes GTK; buffers
   --  cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/text-buffer/empty-buffer", Test_Empty_Buffer'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-buffer/get-set", Test_Get_Set'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-buffer/get-iter", Test_Get_Iter'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-buffer/insert-delete", Test_Insert_Delete'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-buffer/marks", Test_Marks'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-buffer/tag", Test_Tag'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-buffer/child-anchor", Test_Child_Anchor'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-buffer/selection", Test_Selection'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-buffer/undo", Test_Undo'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-buffer/signals", Test_Signals'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
