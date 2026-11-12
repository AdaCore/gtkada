--  Ada port of a representative subset of GTK's
--  testsuite/gtk/textbuffer.c (GTK 4.22.2), as agreed on the work item.
--
--  Gtk.Text_Buffer is a pure data model, so the whole suite runs without
--  a realized window: it exercises creation, get/set of the contents,
--  iterator lookup, insertion and deletion, marks, tags, child anchors,
--  selection handling, undo, and signal emission.
--
--  The C tests test_iter_with_anchor / test_get_text_with_anchor attach
--  child widgets through a GtkTextView; now that the view is bound, the
--  child-anchor test below attaches a Gtk_Label at each anchor through
--  the view, as the C tests do, in addition to the buffer-side
--  assertions. Clipboard coverage uses only the in-process display clipboard;
--  line-separator, logical-motion and serialize cases are omitted.
--  Tag checks also cover GtkAda's return-value constructor, priority changes,
--  and typed tag-list conversions, which have no direct C test counterpart.

with Ada.Command_Line;
with System;                use type System.Address;

with Glib;                  use Glib;
with Glib.Main;
with Glib.Object;           use Glib.Object;
with Glib.Test;             use Glib.Test;
with Glib.Values;

with Gdk.Clipboard;
with Gdk.Content_Provider;
with Gdk.Display;
with Gdk.Paintable;
with Gdk.Pixbuf;
with Gdk.Texture;
use Gdk.Texture;
with Gtk.Main;
with Gtk.Text_Buffer;       use Gtk.Text_Buffer;
with Gtk.Text_Child_Anchor; use Gtk.Text_Child_Anchor;
with Gtk.Text_Iter;         use Gtk.Text_Iter;
with Gtk.Text_Mark;         use Gtk.Text_Mark;
with Gtk.Text_Tag;          use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;    use Gtk.Text_Tag_Table;
with Gtk.Text_View;         use Gtk.Text_View;
with Gtk.Label;             use Gtk.Label;
with Gtkada.Types;

use type Gdk.Content_Provider.Gdk_Content_Provider;

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

   procedure Test_Tag_Table
   with Convention => C;

   procedure Test_Child_Anchor
   with Convention => C;

   procedure Test_Selection
   with Convention => C;

   procedure Test_Undo
   with Convention => C;

   procedure Test_Signals
   with Convention => C;

   procedure Test_Additional_Methods
   with Convention => C;

   --  State shared by the table callbacks below lets the test verify callback
   --  arguments without capturing a nested Ada subprogram environment.
   Expected_Tag_Table : Gtk_Text_Tag_Table;
   Expected_Signal_Tag : Gtk_Text_Tag;
   Expected_Foreach_Tag_One : Gtk_Text_Tag;
   Expected_Foreach_Tag_Two : Gtk_Text_Tag;

   Tag_Table_Foreach_Count : Gint := 0;
   Tag_Table_Foreach_Data_Count : Gint := 0;
   Tag_Table_Foreach_Data_Total : Gint := 0;
   Tag_Table_Data_Destroy_Count : Gint := 0;

   Tag_Added_Direct_Count : Gint := 0;
   Tag_Added_Slot_Count : Gint := 0;
   Tag_Changed_Direct_True_Count : Gint := 0;
   Tag_Changed_Direct_False_Count : Gint := 0;
   Tag_Changed_Slot_True_Count : Gint := 0;
   Tag_Changed_Slot_False_Count : Gint := 0;
   Tag_Removed_Direct_Count : Gint := 0;
   Tag_Removed_Slot_Count : Gint := 0;

   --  A shared expected table also serves as the explicitly supplied slot.
   --  The tag handles remain live until each synchronous signal is delivered.

   --  Foreach_User_Data owns one copy of its user data for the callback run.
   procedure Destroy_Tag_Table_Data (Data : in out Gint);

   package Tag_Table_Foreach_With_Data is new Gtk.Text_Tag_Table.Foreach_User_Data
     (User_Data_Type => Gint,
      Destroy        => Destroy_Tag_Table_Data);

   procedure On_Tag_Table_Foreach
     (Tag : not null access Gtk_Text_Tag_Record'Class);
   procedure On_Tag_Table_Foreach_With_Data
     (Tag  : not null access Gtk_Text_Tag_Record'Class;
      Data : Gint);

   --  Both native GtkAda and GObject slot callback forms are exercised for
   --  every table signal, so each generated marshaller receives real payloads.
   procedure On_Table_Tag_Added
     (Self : access Gtk_Text_Tag_Table_Record'Class;
      Tag  : not null access Gtk_Text_Tag_Record'Class);
   procedure On_Table_Tag_Added_Slot
     (Self : access GObject_Record'Class;
      Tag  : not null access Gtk_Text_Tag_Record'Class);
   procedure On_Table_Tag_Changed
     (Self         : access Gtk_Text_Tag_Table_Record'Class;
      Tag          : not null access Gtk_Text_Tag_Record'Class;
      Size_Changed : Boolean);
   procedure On_Table_Tag_Changed_Slot
     (Self         : access GObject_Record'Class;
      Tag          : not null access Gtk_Text_Tag_Record'Class;
      Size_Changed : Boolean);
   procedure On_Table_Tag_Removed
     (Self : access Gtk_Text_Tag_Table_Record'Class;
      Tag  : not null access Gtk_Text_Tag_Record'Class);
   procedure On_Table_Tag_Removed_Slot
     (Self : access GObject_Record'Class;
      Tag  : not null access Gtk_Text_Tag_Record'Class);

   --  The callback bodies validate both object identity and the changed flag.
   procedure Destroy_Tag_Table_Data (Data : in out Gint) is
   begin
      Tag_Table_Data_Destroy_Count := Tag_Table_Data_Destroy_Count + 1;
      Data := 0;
   end Destroy_Tag_Table_Data;

   procedure On_Tag_Table_Foreach
     (Tag : not null access Gtk_Text_Tag_Record'Class)
   is
   begin
      Assert_True
        (Tag.Get_Object = Expected_Foreach_Tag_One.Get_Object
         or else Tag.Get_Object = Expected_Foreach_Tag_Two.Get_Object);
      Tag_Table_Foreach_Count := Tag_Table_Foreach_Count + 1;
   end On_Tag_Table_Foreach;

   procedure On_Tag_Table_Foreach_With_Data
     (Tag  : not null access Gtk_Text_Tag_Record'Class;
      Data : Gint)
   is
   begin
      Assert_True
        (Tag.Get_Object = Expected_Foreach_Tag_One.Get_Object
         or else Tag.Get_Object = Expected_Foreach_Tag_Two.Get_Object);
      Assert_Cmpint_Eq (Data, 9);
      Tag_Table_Foreach_Data_Count := Tag_Table_Foreach_Data_Count + 1;
      Tag_Table_Foreach_Data_Total := Tag_Table_Foreach_Data_Total + Data;
   end On_Tag_Table_Foreach_With_Data;

   procedure On_Table_Tag_Added
     (Self : access Gtk_Text_Tag_Table_Record'Class;
      Tag  : not null access Gtk_Text_Tag_Record'Class)
   is
   begin
      --  The direct form receives the emitting table as Self.
      Assert_True (Self.Get_Object = Expected_Tag_Table.Get_Object);
      Assert_True (Tag.Get_Object = Expected_Signal_Tag.Get_Object);
      Tag_Added_Direct_Count := Tag_Added_Direct_Count + 1;
   end On_Table_Tag_Added;

   procedure On_Table_Tag_Added_Slot
     (Self : access GObject_Record'Class;
      Tag  : not null access Gtk_Text_Tag_Record'Class)
   is
   begin
      --  The slot form receives the connected table as its first argument.
      Assert_True (Self.Get_Object = Expected_Tag_Table.Get_Object);
      Assert_True (Tag.Get_Object = Expected_Signal_Tag.Get_Object);
      Tag_Added_Slot_Count := Tag_Added_Slot_Count + 1;
   end On_Table_Tag_Added_Slot;

   procedure On_Table_Tag_Changed
     (Self         : access Gtk_Text_Tag_Table_Record'Class;
      Tag          : not null access Gtk_Text_Tag_Record'Class;
      Size_Changed : Boolean)
   is
   begin
      --  Check the tag and receiver before recording the boolean payload.
      Assert_True (Self.Get_Object = Expected_Tag_Table.Get_Object);
      Assert_True (Tag.Get_Object = Expected_Signal_Tag.Get_Object);
      if Size_Changed then
         Tag_Changed_Direct_True_Count := Tag_Changed_Direct_True_Count + 1;
      else
         Tag_Changed_Direct_False_Count := Tag_Changed_Direct_False_Count + 1;
      end if;
   end On_Table_Tag_Changed;

   procedure On_Table_Tag_Changed_Slot
     (Self         : access GObject_Record'Class;
      Tag          : not null access Gtk_Text_Tag_Record'Class;
      Size_Changed : Boolean)
   is
   begin
      --  The slot variant must preserve Size_Changed as well as the tag.
      Assert_True (Self.Get_Object = Expected_Tag_Table.Get_Object);
      Assert_True (Tag.Get_Object = Expected_Signal_Tag.Get_Object);
      if Size_Changed then
         Tag_Changed_Slot_True_Count := Tag_Changed_Slot_True_Count + 1;
      else
         Tag_Changed_Slot_False_Count := Tag_Changed_Slot_False_Count + 1;
      end if;
   end On_Table_Tag_Changed_Slot;

   procedure On_Table_Tag_Removed
     (Self : access Gtk_Text_Tag_Table_Record'Class;
      Tag  : not null access Gtk_Text_Tag_Record'Class)
   is
   begin
      --  The removed tag is still valid while its signal handlers run.
      Assert_True (Self.Get_Object = Expected_Tag_Table.Get_Object);
      Assert_True (Tag.Get_Object = Expected_Signal_Tag.Get_Object);
      Tag_Removed_Direct_Count := Tag_Removed_Direct_Count + 1;
   end On_Table_Tag_Removed;

   procedure On_Table_Tag_Removed_Slot
     (Self : access GObject_Record'Class;
      Tag  : not null access Gtk_Text_Tag_Record'Class)
   is
   begin
      --  Slot callbacks receive the supplied table object, not the tag.
      Assert_True (Self.Get_Object = Expected_Tag_Table.Get_Object);
      Assert_True (Tag.Get_Object = Expected_Signal_Tag.Get_Object);
      Tag_Removed_Slot_Count := Tag_Removed_Slot_Count + 1;
   end On_Table_Tag_Removed_Slot;

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
      Iter   : Gtk_Text_Iter;
   begin
      Gtk_New (Buffer);

      Buffer.Set_Text ("ab" & ASCII.LF & Sharp_S & "d" & ASCII.CR & ASCII.LF
                       & "ef");

      --  Get_Iter_At_Line
      Assert_True (Buffer.Get_Iter_At_Line (Iter, 0));
      Assert_True (Is_Start (Iter));

      Assert_True (Buffer.Get_Iter_At_Line (Iter, 1));
      Assert_Cmpint_Eq (Get_Offset (Iter), 3);

      Assert_True (Buffer.Get_Iter_At_Line (Iter, 2));
      Assert_Cmpint_Eq (Get_Offset (Iter), 7);

      Assert_False (Buffer.Get_Iter_At_Line (Iter, 3));
      Assert_True (Is_End (Iter));

      --  Get_Iter_At_Line_Offset
      Assert_True (Buffer.Get_Iter_At_Line_Offset (Iter, 0, 0));
      Assert_True (Is_Start (Iter));

      Assert_True (Buffer.Get_Iter_At_Line_Offset (Iter, 0, 1));
      Assert_Cmpint_Eq (Get_Offset (Iter), 1);

      Assert_True (Buffer.Get_Iter_At_Line_Offset (Iter, 0, 2));
      Assert_Cmpint_Eq (Get_Offset (Iter), 2);

      Assert_False (Buffer.Get_Iter_At_Line_Offset (Iter, 0, 3));
      Assert_Cmpint_Eq (Get_Offset (Iter), 2);

      Assert_True (Buffer.Get_Iter_At_Line_Offset (Iter, 1, 1));
      Assert_Cmpint_Eq (Get_Offset (Iter), 4);

      Assert_True (Buffer.Get_Iter_At_Line_Offset (Iter, 2, 1));
      Assert_Cmpint_Eq (Get_Offset (Iter), 8);

      Assert_True (Buffer.Get_Iter_At_Line_Offset (Iter, 2, 2));
      Assert_True (Is_End (Iter));

      Assert_False (Buffer.Get_Iter_At_Line_Offset (Iter, 2, 3));
      Assert_True (Is_End (Iter));

      Assert_False (Buffer.Get_Iter_At_Line_Offset (Iter, 3, 1));
      Assert_True (Is_End (Iter));

      --  Get_Iter_At_Line_Index ("ß" takes 2 bytes in UTF-8)
      Assert_True (Buffer.Get_Iter_At_Line_Index (Iter, 0, 0));
      Assert_True (Is_Start (Iter));

      Assert_True (Buffer.Get_Iter_At_Line_Index (Iter, 0, 1));
      Assert_Cmpint_Eq (Get_Offset (Iter), 1);

      Assert_True (Buffer.Get_Iter_At_Line_Index (Iter, 0, 2));
      Assert_Cmpint_Eq (Get_Offset (Iter), 2);

      Assert_False (Buffer.Get_Iter_At_Line_Index (Iter, 0, 3));
      Assert_Cmpint_Eq (Get_Offset (Iter), 2);

      Assert_True (Buffer.Get_Iter_At_Line_Index (Iter, 1, 0));
      Assert_Cmpint_Eq (Get_Offset (Iter), 3);

      Assert_True (Buffer.Get_Iter_At_Line_Index (Iter, 1, 2));
      Assert_Cmpint_Eq (Get_Offset (Iter), 4);

      Assert_True (Buffer.Get_Iter_At_Line_Index (Iter, 1, 3));
      Assert_Cmpint_Eq (Get_Offset (Iter), 5);

      Assert_True (Buffer.Get_Iter_At_Line_Index (Iter, 2, 2));
      Assert_True (Is_End (Iter));

      Assert_False (Buffer.Get_Iter_At_Line_Index (Iter, 2, 3));
      Assert_True (Is_End (Iter));

      Assert_False (Buffer.Get_Iter_At_Line_Index (Iter, 3, 1));
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
      Read_Back : Gtk_Text_Mark;
      Value : Glib.Values.GValue;
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

      --  Gtk_Text_Mark_New is the return-value constructor. Check its
      --  name and gravity, then release its standalone reference below.
      Mark := Gtk_Text_Mark_New ("returned", Left_Gravity => False);
      Assert_Cmpstr_Eq ("returned", Mark.Get_Name);
      Assert_False (Mark.Get_Left_Gravity);

      --  These GtkAda helpers pass a mark through a pointer-valued GValue.
      --  The mark stays alive while the non-owning pointer is read back.
      Glib.Values.Init (Value, GType_Pointer);
      Set_Text_Mark (Value, Mark);
      Read_Back := Get_Text_Mark (Value);
      Assert_True (Read_Back.Get_Object = Mark.Get_Object);
      Glib.Values.Unset (Value);
      Mark.Unref;
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
         --  Reading and appending elements exercises both typed list
         --  conversions and checks that they preserve the tag object.
         Assert_True (Nth_Data (Tags, 0).Get_Object = Tag.Get_Object);
         Append (Tags, Tag);
         Assert_Cmpuint_Eq (Length (Tags), 2);
         Assert_True (Nth_Data (Tags, 1).Get_Object = Tag.Get_Object);
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
         Returned_Tag : Gtk_Text_Tag;
      begin
         Gtk_New (Lone_Table);
         Gtk_New (Lone_Tag, "lonesome");
         Returned_Tag := Gtk_Text_Tag_New ("returned-tag");
         Assert_Cmpint_Eq (Lone_Table.Get_Size, 0);
         Assert_True (Lone_Table.Add (Lone_Tag));
         Assert_True (Lone_Table.Add (Returned_Tag));
         Assert_Cmpint_Eq (Lone_Table.Get_Size, 2);
         Assert_True
           (Lone_Table.Lookup ("lonesome").Get_Object = Lone_Tag.Get_Object);
         Assert_True
           (Lone_Table.Lookup ("returned-tag").Get_Object =
              Returned_Tag.Get_Object);

         --  Moving the newly constructed tag to priority zero shifts the
         --  existing tag while preserving the table's unique priorities.
         Assert_Cmpint_Eq (Lone_Tag.Get_Priority, 0);
         Assert_Cmpint_Eq (Returned_Tag.Get_Priority, 1);
         Returned_Tag.Set_Priority (0);
         Assert_Cmpint_Eq (Returned_Tag.Get_Priority, 0);
         Assert_Cmpint_Eq (Lone_Tag.Get_Priority, 1);

         Lone_Table.Remove (Lone_Tag);
         Lone_Table.Remove (Returned_Tag);
         Assert_Cmpint_Eq (Lone_Table.Get_Size, 0);
         Assert_True (Lone_Table.Lookup ("lonesome") = null);
         Assert_True (Lone_Table.Lookup ("returned-tag") = null);
      end;
   end Test_Tag;

   ---------------------
   -- Test_Tag_Table --
   ---------------------

   --  Check standalone table construction and iteration, then exercise every
   --  table signal overload with both a native callback and an explicit slot.

   procedure Test_Tag_Table is
      Table     : Gtk_Text_Tag_Table;
      First     : Gtk_Text_Tag;
      Second    : Gtk_Text_Tag;
   begin
      --  This constructor returns an initialized table with no tags.
      Table := Gtk_Text_Tag_Table_New;
      Assert_Cmpint_Eq (Table.Get_Size, 0);

      Gtk_New (First, "table-first");
      Gtk_New (Second, "table-second");
      Expected_Tag_Table := Table;
      Expected_Foreach_Tag_One := First;
      Expected_Foreach_Tag_Two := Second;

      --  Connect all six public signal overloads before table mutations.
      Tag_Added_Direct_Count := 0;
      Tag_Added_Slot_Count := 0;
      Tag_Changed_Direct_True_Count := 0;
      Tag_Changed_Direct_False_Count := 0;
      Tag_Changed_Slot_True_Count := 0;
      Tag_Changed_Slot_False_Count := 0;
      Tag_Removed_Direct_Count := 0;
      Tag_Removed_Slot_Count := 0;
      Table.On_Tag_Added (On_Table_Tag_Added'Unrestricted_Access);
      Table.On_Tag_Added
        (On_Table_Tag_Added_Slot'Unrestricted_Access, Slot => Table);
      Table.On_Tag_Changed (On_Table_Tag_Changed'Unrestricted_Access);
      Table.On_Tag_Changed
        (On_Table_Tag_Changed_Slot'Unrestricted_Access, Slot => Table);
      Table.On_Tag_Removed (On_Table_Tag_Removed'Unrestricted_Access);
      Table.On_Tag_Removed
        (On_Table_Tag_Removed_Slot'Unrestricted_Access, Slot => Table);

      --  Add emits tag-added with the table and exact tag as its payload.
      Expected_Signal_Tag := First;
      Assert_True (Table.Add (First));
      Assert_Cmpint_Eq (Tag_Added_Direct_Count, 1);
      Assert_Cmpint_Eq (Tag_Added_Slot_Count, 1);

      Expected_Signal_Tag := Second;
      Assert_True (Table.Add (Second));
      Assert_Cmpint_Eq (Table.Get_Size, 2);
      Assert_Cmpint_Eq (Tag_Added_Direct_Count, 2);
      Assert_Cmpint_Eq (Tag_Added_Slot_Count, 2);

      --  Both iteration APIs visit each tag and forward their user data.
      Tag_Table_Foreach_Count := 0;
      Tag_Table_Foreach_Data_Count := 0;
      Tag_Table_Foreach_Data_Total := 0;
      Tag_Table_Data_Destroy_Count := 0;
      Table.Foreach (On_Tag_Table_Foreach'Unrestricted_Access);
      Assert_Cmpint_Eq (Tag_Table_Foreach_Count, 2);
      Tag_Table_Foreach_With_Data.Foreach
        (Table, On_Tag_Table_Foreach_With_Data'Unrestricted_Access, 9);
      Assert_Cmpint_Eq (Tag_Table_Foreach_Data_Count, 2);
      Assert_Cmpint_Eq (Tag_Table_Foreach_Data_Total, 18);
      --  The closure destroys its owned data once after iteration completes.
      Assert_Cmpint_Eq (Tag_Table_Data_Destroy_Count, 1);

      --  Changed reports both values of Size_Changed to both callback forms.
      Expected_Signal_Tag := First;
      First.Changed (Size_Changed => True);
      First.Changed (Size_Changed => False);
      Assert_Cmpint_Eq (Tag_Changed_Direct_True_Count, 1);
      Assert_Cmpint_Eq (Tag_Changed_Direct_False_Count, 1);
      Assert_Cmpint_Eq (Tag_Changed_Slot_True_Count, 1);
      Assert_Cmpint_Eq (Tag_Changed_Slot_False_Count, 1);

      --  Removed keeps the tag alive through its callback and leaves the
      --  second tag in the table until its own removal.
      Expected_Signal_Tag := Second;
      Table.Remove (Second);
      Assert_Cmpint_Eq (Tag_Removed_Direct_Count, 1);
      Assert_Cmpint_Eq (Tag_Removed_Slot_Count, 1);
      Assert_Cmpint_Eq (Table.Get_Size, 1);
      Expected_Signal_Tag := First;
      Table.Remove (First);
      Assert_Cmpint_Eq (Tag_Removed_Direct_Count, 2);
      Assert_Cmpint_Eq (Tag_Removed_Slot_Count, 2);
      Assert_Cmpint_Eq (Table.Get_Size, 0);
      Assert_True (Table.Lookup ("table-first") = null);
   end Test_Tag_Table;

   -----------------------
   -- Test_Child_Anchor --
   -----------------------

   --  Port of C test_iter_with_anchor / test_get_text_with_anchor: a
   --  Gtk_Label is attached at each anchor through a Gtk_Text_View, as
   --  the C tests do, alongside the buffer-side assertions.

   procedure Test_Child_Anchor is
      View    : Gtk_Text_View;
      Buffer  : Gtk_Text_Buffer;
      Iter    : Gtk_Text_Iter;
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter;
      Anchor  : Gtk_Text_Child_Anchor;
      Repl    : Gtk_Text_Child_Anchor;
      Child   : Gtk_Label;
      Chars   : Gint;
   begin
      -- Exercise both ordinary constructor forms and the replacement
      -- function form; the procedure replacement form is used below.
      declare
         Fresh      : Gtk_Text_Child_Anchor;
         Fresh_Fn   : Gtk_Text_Child_Anchor;
         Repl_Fn    : Gtk_Text_Child_Anchor;
      begin
         Gtk_New (Fresh);
         Assert_True (Is_Created (Fresh.all));
         Gtk.Text_Child_Anchor.Initialize (Fresh);
         Assert_True (Is_Created (Fresh.all));

         Fresh_Fn := Gtk_Text_Child_Anchor_New;
         Assert_True (Is_Created (Fresh_Fn.all));

         Repl_Fn := Gtk_Text_Child_Anchor_New_With_Replacement (Sharp_S);
         Assert_True (Is_Created (Repl_Fn.all));

         -- These detached anchors are only used for constructor checks.
         Fresh.Unref;
         Fresh_Fn.Unref;
         Repl_Fn.Unref;
      end;

      --  The view owns the buffer, as in the C test.
      Gtk_New (View);
      Buffer := View.Get_Buffer;

      Buffer.Set_Text ("ab" & ASCII.LF & "cd" & ASCII.CR & ASCII.LF & "ef");
      Chars := Buffer.Get_Char_Count;

      --  An anchor counts as one character.
      Assert_True (Buffer.Get_Iter_At_Line_Offset (Iter, 0, 1));
      Anchor := Buffer.Create_Child_Anchor (Iter);
      Assert_Cmpint_Eq (Buffer.Get_Char_Count, Chars + 1);

      --  Attach a child widget at the anchor through the view.
      Gtk_New (Child, "text");
      View.Add_Child_At_Anchor (Child, Anchor);
      declare
         Widgets : constant Gtk_Widget_Array := Get_Widgets (Anchor);
      begin
         -- The view reports its one child through the anchor helper.
         Assert_Cmpuint_Eq (Widgets'Length, 1);
      end;

      --  The anchor reads back as the unknown char at its location.
      Buffer.Get_Iter_At_Child_Anchor (Iter, Anchor);
      -- The Gtk.Text.Iter helper returns the anchor at this iterator.
      Assert_True (Get_Child_Anchor (Iter) = Anchor);
      Assert_True (Get_Char (Iter) = Unknown_Char);
      Assert_False (Anchor.Get_Deleted);

      --  An anchor with a replacement character ("ß" takes 2 bytes).
      Assert_True (Buffer.Get_Iter_At_Line_Offset (Iter, 1, 1));
      Gtk_New_With_Replacement (Repl, Sharp_S);
      Buffer.Insert_Child_Anchor (Iter, Repl);

      Gtk_New (Child, "text");
      View.Add_Child_At_Anchor (Child, Repl);

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

      --  Consume the view's floating reference (as g_object_ref_sink in
      --  the C test) now that we are done with it.
      View.Ref_Sink;
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

   ------------------------------
   -- Test_Additional_Methods --
   ------------------------------

   --  Exercise the remaining synchronous GtkTextBuffer methods: alternate
   --  constructors, pointer-string overloads, interactive edits, range and
   --  markup insertion, paintables, modification state, marks and undo limits.
   --  Each assertion checks resulting contents, iterator position, or exposed
   --  state so the calls also serve as behavioral tests.

   procedure Test_Additional_Methods is
      Table    : Gtk_Text_Tag_Table;
      Buffer   : Gtk_Text_Buffer;
      Source   : Gtk_Text_Buffer;
      Tag      : Gtk_Text_Tag;
      Mark     : Gtk_Text_Mark;
      Iter     : Gtk_Text_Iter;
      Start    : Gtk_Text_Iter;
      The_End  : Gtk_Text_Iter;
      Result   : Boolean;
      Raw      : Gtkada.Types.Chars_Ptr;
      Pixbuf   : Gdk.Pixbuf.Gdk_Pixbuf;
      Texture  : Gdk.Texture.Gdk_Texture;
      Provider : Gdk.Content_Provider.Gdk_Content_Provider;
   begin
      --  Constructing with an explicit table preserves the table object.
      Gtk_New (Table);
      Buffer := Gtk_Text_Buffer_New (Table);
      Assert_True (Buffer.Get_Tag_Table.Get_Object = Table.Get_Object);
      Buffer.Initialize (Table);
      Assert_True (Buffer.Get_Tag_Table.Get_Object = Table.Get_Object);

      --  The GtkAda C-string overloads accept and return owned strings.
      --  The caller frees both returned text and the input passed to Insert.
      Buffer.Set_Text ("plain text");
      Buffer.Get_Bounds (Start, The_End);
      Raw := Buffer.Get_Text (Start, The_End);
      Assert_Cmpstr_Eq (Gtkada.Types.Value (Raw), "plain text");
      Gtkada.Types.Free (Raw);
      Raw := Gtkada.Types.New_String ("!");
      Buffer.Insert (The_End, Raw);
      Gtkada.Types.Free (Raw);
      Assert_Cmpint_Eq (Get_Offset (The_End), 11);
      Raw := Gtkada.Types.New_String ("?");
      Buffer.Insert_At_Cursor (Raw);
      Gtkada.Types.Free (Raw);
      Check_Buffer_Contents (Buffer, "plain text!?");

      --  The iterator overload also reports its owning buffer.
      Buffer.Get_Start_Iter (Iter);
      Assert_True (Get_Buffer (Iter).Get_Object = Buffer.Get_Object);

      --  Interactive insertion honors default editability and updates its
      --  iterator to the end of the inserted run.
      --  With no editability tag, Default_Editable=False blocks the edit.
      Buffer.Set_Text ("abc");
      Buffer.Get_Iter_At_Offset (Iter, 1);
      Assert_True (Buffer.Insert_Interactive (Iter, "X"));
      Assert_Cmpint_Eq (Get_Offset (Iter), 2);
      Assert_False (Buffer.Insert_Interactive (Iter, "blocked", False));
      Assert_True (Buffer.Insert_Interactive_At_Cursor ("Y"));
      Check_Buffer_Contents (Buffer, "aXbcY");

      --  Interactive deletion succeeds for editable text and is declined
      --  when the default editability is false.
      Buffer.Get_Iter_At_Offset (Start, 1);
      Buffer.Get_Iter_At_Offset (The_End, 2);
      Buffer.Delete_Interactive (Start, The_End, Result => Result);
      Assert_True (Result);
      Check_Buffer_Contents (Buffer, "abcY");
      Buffer.Get_Iter_At_Offset (Start, 1);
      Buffer.Get_Iter_At_Offset (The_End, 2);
      Buffer.Delete_Interactive
        (Start, The_End, Default_Editable => False, Result => Result);
      Assert_False (Result);

      --  Range insertion preserves tags, while Pango markup inserts its
      --  visible text and applies attributes to that text.
      --  Both buffers share Table, as required for cross-buffer tag copying.
      Gtk_New (Source, Table);
      Tag := Source.Create_Tag ("range-tag");
      Source.Set_Text ("source");
      Source.Get_Bounds (Start, The_End);
      Source.Apply_Tag (Tag, Start, The_End);
      Buffer.Set_Text ("target:");
      Buffer.Get_End_Iter (Iter);
      Buffer.Insert_Range (Iter, Start, The_End);
      Check_Buffer_Contents (Buffer, "target:source");
      Buffer.Insert_Markup (Iter, "<b> bold</b>");
      Check_Buffer_Contents (Buffer, "target:source bold");
      Buffer.Get_Iter_At_Offset (Iter, 0);
      Buffer.Insert_With_Tags (Iter, "tagged", Tag);
      Check_Buffer_Contents (Buffer, "taggedtarget:source bold");
      --  Also exercise the pointer overload, which uses a NUL-terminated
      --  string and leaves the tag owned by the shared tag table.
      Buffer.Get_End_Iter (Iter);
      Raw := Gtkada.Types.New_String ("pointer-tagged");
      Buffer.Insert_With_Tags (Iter, Raw, Tag);
      Gtkada.Types.Free (Raw);
      Check_Buffer_Contents
        (Buffer, "taggedtarget:source boldpointer-tagged");
      Buffer.Get_Bounds (Start, The_End);
      Buffer.Remove_Tag_By_Name ("range-tag", Start, The_End);
      Buffer.Get_Iter_At_Offset (Iter, 8);
      Assert_False (Has_Tag (Iter, Tag));

      --  The interactive range variant succeeds only at editable locations.
      --  A second request at the same location checks the rejected path.
      Buffer.Set_Text ("destination");
      Source.Get_Bounds (Start, The_End);
      Buffer.Get_End_Iter (Iter);
      Buffer.Insert_Range_Interactive
        (Iter, Start, The_End, Result => Result);
      Assert_True (Result);
      Check_Buffer_Contents (Buffer, "destinationsource");
      Buffer.Get_End_Iter (Iter);
      Buffer.Insert_Range_Interactive
        (Iter, Start, The_End, Default_Editable => False, Result => Result);
      Assert_False (Result);

      --  Use a one-pixel in-memory texture to exercise image insertion
      --  without reading a file or opening a window.
      Pixbuf := Gdk.Pixbuf.Gdk_Pixbuf_New
        (Gdk.Pixbuf.Gdk_Colorspace_Rgb, True, 8, 1, 1);
      Pixbuf.Fill (16#3366CCFF#);
      Texture := Gdk.Texture.Gdk_Texture_New_For_Pixbuf (Pixbuf);
      Buffer.Get_End_Iter (Iter);
      Buffer.Insert_Paintable
        (Iter, +Texture);
      Assert_Cmpint_Eq (Buffer.Get_Char_Count, 18);
      Buffer.Get_Iter_At_Offset (Iter, 17);
      Assert_True (Get_Char (Iter) = Unknown_Char);
      Texture.Unref;
      Pixbuf.Unref;

      --  Named mark helpers and the modified flag have observable state.
      --  Deleting by name removes lookup access while keeping other content.
      Buffer.Get_Start_Iter (Iter);
      Mark := Buffer.Create_Mark ("named", Iter);
      Buffer.Get_End_Iter (The_End);
      Buffer.Move_Mark_By_Name ("named", The_End);
      Buffer.Get_Iter_At_Mark (Iter, Mark);
      Assert_True (Is_End (Iter));
      Buffer.Delete_Mark_By_Name ("named");
      Assert_True (Buffer.Get_Mark ("named") = null);
      Buffer.Set_Modified (False);
      Assert_False (Buffer.Get_Modified);
      Buffer.Set_Modified (True);
      Assert_True (Buffer.Get_Modified);

      --  Undo configuration and action boundaries update the exposed state.
      Buffer.Set_Max_Undo_Levels (3);
      Assert_Cmpuint_Eq (Buffer.Get_Max_Undo_Levels, 3);
      Buffer.Set_Enable_Undo (False);
      Assert_False (Buffer.Get_Enable_Undo);
      Buffer.Set_Enable_Undo (True);
      Assert_True (Buffer.Get_Enable_Undo);
      Buffer.Set_Text ("before");
      Buffer.Begin_Irreversible_Action;
      Buffer.Insert_At_Cursor (" permanent");
      Buffer.End_Irreversible_Action;
      Assert_False (Buffer.Get_Can_Undo);
      Buffer.Begin_User_Action;
      Buffer.Insert_At_Cursor (" grouped");
      Buffer.End_User_Action;
      Assert_True (Buffer.Get_Can_Undo);
      Buffer.Undo;
      Check_Buffer_Contents (Buffer, "before permanent");

      --  Selection content is exposed as a Gdk content provider.
      --  The returned provider has a full reference, released after checking.
      Buffer.Get_Iter_At_Offset (Start, 0);
      Buffer.Get_Iter_At_Offset (The_End, 6);
      Buffer.Select_Range (The_End, Start);
      Provider := Buffer.Get_Selection_Content;
      Assert_True (Provider /= null);
      Provider.Unref;
   end Test_Additional_Methods;

   ---------------------
   -- Test_Clipboard --
   ---------------------

   --  Clipboard methods use the display's in-process clipboard, so this test
   --  stays deterministic and never accesses the user's clipboard or files.
   --  The same clipboard instance is used for copy, cut, and paste.

   procedure Test_Clipboard
   with Convention => C;

   procedure Test_Clipboard is
      Buffer    : Gtk_Text_Buffer;
      Display   : Gdk.Display.Gdk_Display;
      Clipboard : Gdk.Clipboard.Gdk_Clipboard;
      Start     : Gtk_Text_Iter;
      The_End   : Gtk_Text_Iter;
      Iter      : Gtk_Text_Iter;
      Done      : Boolean := False;
      Slot_Paste_Done_Count : Natural := 0;

      procedure On_Paste_Done_Cb
        (Self      : access Gtk_Text_Buffer_Record'Class;
         Clipboard_Arg : not null access Gdk.Clipboard.Gdk_Clipboard_Record'Class);

      procedure On_Clipboard_Slot
        (Self      : access GObject_Record'Class;
         Clipboard : not null access Gdk.Clipboard.Gdk_Clipboard_Record'Class);

      --  The paste completion signal carries the same clipboard object.
      procedure On_Paste_Done_Cb
        (Self      : access Gtk_Text_Buffer_Record'Class;
         Clipboard_Arg : not null access Gdk.Clipboard.Gdk_Clipboard_Record'Class)
      is
         pragma Unreferenced (Self);
      begin
         Assert_True (Clipboard_Arg.Get_Object = Clipboard.Get_Object);
         Done := True;
      end On_Paste_Done_Cb;

      procedure On_Clipboard_Slot
        (Self      : access GObject_Record'Class;
         Clipboard : not null access Gdk.Clipboard.Gdk_Clipboard_Record'Class)
      is
         pragma Unreferenced (Self, Clipboard);
      begin
         Slot_Paste_Done_Count := Slot_Paste_Done_Count + 1;
      end On_Clipboard_Slot;
   begin
      Gtk_New (Buffer);
      Display := Gdk.Display.Get_Default;
      Clipboard := Display.Get_Clipboard;

      --  The registered selection clipboard can be removed again cleanly.
      Buffer.Add_Selection_Clipboard (Clipboard);
      Buffer.Remove_Selection_Clipboard (Clipboard);

      --  Copy and cut operate on the selected range immediately.
      --  The selection is reversed to exercise GtkTextBuffer's normalized
      --  bounds when removing its contents.
      Buffer.Set_Text ("abcdef");
      Buffer.Get_Iter_At_Offset (Start, 0);
      Buffer.Get_Iter_At_Offset (The_End, 3);
      Buffer.Select_Range (The_End, Start);
      Buffer.Copy_Clipboard (Clipboard);
      Assert_True (Clipboard.Get_Content /= null);
      Buffer.Cut_Clipboard (Clipboard);
      Check_Buffer_Contents (Buffer, "def");

      --  Clipboard paste is asynchronous, so iterate the main context until
      --  paste-done confirms that the request was handled.
      --  Place_Cursor chooses the insertion point because Paste_Clipboard
      --  uses the buffer's insert mark rather than accepting an iterator.
      Clipboard.Set_Text ("paste");
      Buffer.On_Paste_Done (On_Paste_Done_Cb'Unrestricted_Access);
      Buffer.On_Paste_Done
        (On_Clipboard_Slot'Unrestricted_Access, Slot => Buffer);
      Buffer.Get_End_Iter (Iter);
      Buffer.Place_Cursor (Iter);
      Buffer.Paste_Clipboard (Clipboard);
      for Attempt in 1 .. 100 loop
         exit when Done;
         if Glib.Main.Main_Context_Iteration (May_Block => False) then
            null;
         end if;
         delay 0.01;
      end loop;
      Assert_True (Done);
      Assert_True (Slot_Paste_Done_Count > 0);
      Check_Buffer_Contents (Buffer, "defpaste");
   end Test_Clipboard;

   ------------------
   -- Test_Signals --
   ------------------

   --  Buffer modifications emit the matching signals, marshalled through
   --  the re-enabled Unchecked_To_Gtk_Text_Iter machinery.
   --  Assertions in each callback validate its payload or record its call.

   Insert_Count    : Natural := 0;
   Delete_Count    : Natural := 0;
   Changed_Count   : Natural := 0;
   Mark_Set_Count  : Natural := 0;
   Apply_Tag_Count : Natural := 0;
   Remove_Tag_Count : Natural := 0;
   Begin_Action_Count : Natural := 0;
   End_Action_Count : Natural := 0;
   Mark_Deleted_Count : Natural := 0;
   Modified_Count : Natural := 0;
   Undo_Count : Natural := 0;
   Redo_Count : Natural := 0;
   Child_Anchor_Count : Natural := 0;
   Paintable_Count : Natural := 0;
   Slot_Void_Count : Natural := 0;
   Slot_Range_Count : Natural := 0;
   Slot_Tag_Count : Natural := 0;
   Slot_Anchor_Count : Natural := 0;
   Slot_Paintable_Count : Natural := 0;
   Slot_Text_Count : Natural := 0;
   Slot_Mark_Count : Natural := 0;
   Slot_Iter_Mark_Count : Natural := 0;
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

   procedure On_Begin_Action_Cb
     (Self : access Gtk_Text_Buffer_Record'Class);
   procedure On_End_Action_Cb
     (Self : access Gtk_Text_Buffer_Record'Class);
   procedure On_Modified_Cb
     (Self : access Gtk_Text_Buffer_Record'Class);
   procedure On_Undo_Cb
     (Self : access Gtk_Text_Buffer_Record'Class);
   procedure On_Redo_Cb
     (Self : access Gtk_Text_Buffer_Record'Class);
   procedure On_Remove_Tag_Cb
     (Self    : access Gtk_Text_Buffer_Record'Class;
      Tag     : not null access Gtk_Text_Tag_Record'Class;
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter);
   procedure On_Insert_Child_Anchor_Cb
     (Self     : access Gtk_Text_Buffer_Record'Class;
      Location : Gtk_Text_Iter;
      Anchor   : not null access Gtk_Text_Child_Anchor_Record'Class);
   procedure On_Insert_Paintable_Cb
     (Self      : access Gtk_Text_Buffer_Record'Class;
      Location  : Gtk_Text_Iter;
      Paintable : Gdk.Paintable.Gdk_Paintable);
   procedure On_Mark_Deleted_Cb
     (Self : access Gtk_Text_Buffer_Record'Class;
      Mark : not null access Gtk_Text_Mark_Record'Class);

   --  These GObject-first callbacks exercise each generated Connect_Slot
   --  signature and its argument marshaller.
   --  Separate counters let the test confirm every payload type was invoked.
   procedure On_Void_Slot
     (Self : access GObject_Record'Class);
   procedure On_Range_Slot
     (Self    : access GObject_Record'Class;
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter);
   procedure On_Tag_Slot
     (Self    : access GObject_Record'Class;
      Tag     : not null access Gtk_Text_Tag_Record'Class;
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter);
   procedure On_Anchor_Slot
     (Self     : access GObject_Record'Class;
      Location : Gtk_Text_Iter;
      Anchor   : not null access Gtk_Text_Child_Anchor_Record'Class);
   procedure On_Paintable_Slot
     (Self      : access GObject_Record'Class;
      Location  : Gtk_Text_Iter;
      Paintable : Gdk.Paintable.Gdk_Paintable);
   procedure On_Text_Slot
     (Self     : access GObject_Record'Class;
      Location : Gtk_Text_Iter;
      Text     : UTF8_String;
      Len      : Gint);
   procedure On_Mark_Slot
     (Self : access GObject_Record'Class;
      Mark : not null access Gtk_Text_Mark_Record'Class);
   procedure On_Iter_Mark_Slot
     (Self     : access GObject_Record'Class;
      Location : Gtk_Text_Iter;
      Mark     : not null access Gtk_Text_Mark_Record'Class);

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

   procedure On_Begin_Action_Cb
     (Self : access Gtk_Text_Buffer_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Begin_Action_Count := Begin_Action_Count + 1;
   end On_Begin_Action_Cb;

   procedure On_End_Action_Cb
     (Self : access Gtk_Text_Buffer_Record'Class) is
      pragma Unreferenced (Self);
   begin
      End_Action_Count := End_Action_Count + 1;
   end On_End_Action_Cb;

   procedure On_Modified_Cb
     (Self : access Gtk_Text_Buffer_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Modified_Count := Modified_Count + 1;
   end On_Modified_Cb;

   procedure On_Undo_Cb
     (Self : access Gtk_Text_Buffer_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Undo_Count := Undo_Count + 1;
   end On_Undo_Cb;

   procedure On_Redo_Cb
     (Self : access Gtk_Text_Buffer_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Redo_Count := Redo_Count + 1;
   end On_Redo_Cb;

   procedure On_Remove_Tag_Cb
     (Self    : access Gtk_Text_Buffer_Record'Class;
      Tag     : not null access Gtk_Text_Tag_Record'Class;
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter)
   is
      pragma Unreferenced (Self, Tag);
   begin
      Remove_Tag_Count := Remove_Tag_Count + 1;
      Assert_Cmpint_Eq (Get_Offset (Start), 0);
      Assert_Cmpint_Eq (Get_Offset (The_End), 5);
   end On_Remove_Tag_Cb;

   procedure On_Insert_Child_Anchor_Cb
     (Self     : access Gtk_Text_Buffer_Record'Class;
      Location : Gtk_Text_Iter;
      Anchor   : not null access Gtk_Text_Child_Anchor_Record'Class)
   is
      pragma Unreferenced (Self, Location, Anchor);
   begin
      Child_Anchor_Count := Child_Anchor_Count + 1;
   end On_Insert_Child_Anchor_Cb;

   procedure On_Insert_Paintable_Cb
     (Self      : access Gtk_Text_Buffer_Record'Class;
      Location  : Gtk_Text_Iter;
      Paintable : Gdk.Paintable.Gdk_Paintable)
   is
      pragma Unreferenced (Self, Location, Paintable);
   begin
      Paintable_Count := Paintable_Count + 1;
   end On_Insert_Paintable_Cb;

   procedure On_Mark_Deleted_Cb
     (Self : access Gtk_Text_Buffer_Record'Class;
      Mark : not null access Gtk_Text_Mark_Record'Class)
   is
      pragma Unreferenced (Self, Mark);
   begin
      Mark_Deleted_Count := Mark_Deleted_Count + 1;
   end On_Mark_Deleted_Cb;

   procedure On_Void_Slot (Self : access GObject_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Slot_Void_Count := Slot_Void_Count + 1;
   end On_Void_Slot;

   procedure On_Range_Slot
     (Self    : access GObject_Record'Class;
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter)
   is
      pragma Unreferenced (Self, Start, The_End);
   begin
      Slot_Range_Count := Slot_Range_Count + 1;
   end On_Range_Slot;

   procedure On_Tag_Slot
     (Self    : access GObject_Record'Class;
      Tag     : not null access Gtk_Text_Tag_Record'Class;
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter)
   is
      pragma Unreferenced (Self, Tag, Start, The_End);
   begin
      Slot_Tag_Count := Slot_Tag_Count + 1;
   end On_Tag_Slot;

   procedure On_Anchor_Slot
     (Self     : access GObject_Record'Class;
      Location : Gtk_Text_Iter;
      Anchor   : not null access Gtk_Text_Child_Anchor_Record'Class)
   is
      pragma Unreferenced (Self, Location, Anchor);
   begin
      Slot_Anchor_Count := Slot_Anchor_Count + 1;
   end On_Anchor_Slot;

   procedure On_Paintable_Slot
     (Self      : access GObject_Record'Class;
      Location  : Gtk_Text_Iter;
      Paintable : Gdk.Paintable.Gdk_Paintable)
   is
      pragma Unreferenced (Self, Location, Paintable);
   begin
      Slot_Paintable_Count := Slot_Paintable_Count + 1;
   end On_Paintable_Slot;

   procedure On_Text_Slot
     (Self     : access GObject_Record'Class;
      Location : Gtk_Text_Iter;
      Text     : UTF8_String;
      Len      : Gint)
   is
      pragma Unreferenced (Self, Location, Text, Len);
   begin
      Slot_Text_Count := Slot_Text_Count + 1;
   end On_Text_Slot;

   procedure On_Mark_Slot
     (Self : access GObject_Record'Class;
      Mark : not null access Gtk_Text_Mark_Record'Class)
   is
      pragma Unreferenced (Self, Mark);
   begin
      Slot_Mark_Count := Slot_Mark_Count + 1;
   end On_Mark_Slot;

   procedure On_Iter_Mark_Slot
     (Self     : access GObject_Record'Class;
      Location : Gtk_Text_Iter;
      Mark     : not null access Gtk_Text_Mark_Record'Class)
   is
      pragma Unreferenced (Self, Location, Mark);
   begin
      Slot_Iter_Mark_Count := Slot_Iter_Mark_Count + 1;
   end On_Iter_Mark_Slot;

   procedure Test_Signals is
      Buffer  : Gtk_Text_Buffer;
      Tag     : Gtk_Text_Tag;
      Mark    : Gtk_Text_Mark;
      Anchor  : Gtk_Text_Child_Anchor;
      Iter    : Gtk_Text_Iter;
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter;
      Pixbuf  : Gdk.Pixbuf.Gdk_Pixbuf;
      Texture : Gdk.Texture.Gdk_Texture;
   begin
      Gtk_New (Buffer);
      Tag := Buffer.Create_Tag ("bold");

      Buffer.On_Insert_Text (On_Insert_Text_Cb'Unrestricted_Access);
      Buffer.On_Delete_Range (On_Delete_Range_Cb'Unrestricted_Access);
      Buffer.On_Changed (On_Changed_Cb'Unrestricted_Access);
      Buffer.On_Mark_Set (On_Mark_Set_Cb'Unrestricted_Access);
      Buffer.On_Apply_Tag (On_Apply_Tag_Cb'Unrestricted_Access);
      Buffer.On_Begin_User_Action (On_Begin_Action_Cb'Unrestricted_Access);
      Buffer.On_End_User_Action (On_End_Action_Cb'Unrestricted_Access);
      Buffer.On_Modified_Changed (On_Modified_Cb'Unrestricted_Access);
      Buffer.On_Undo (On_Undo_Cb'Unrestricted_Access);
      Buffer.On_Redo (On_Redo_Cb'Unrestricted_Access);
      Buffer.On_Remove_Tag (On_Remove_Tag_Cb'Unrestricted_Access);
      Buffer.On_Insert_Child_Anchor
        (On_Insert_Child_Anchor_Cb'Unrestricted_Access);
      Buffer.On_Insert_Paintable
        (On_Insert_Paintable_Cb'Unrestricted_Access);
      Buffer.On_Mark_Deleted (On_Mark_Deleted_Cb'Unrestricted_Access);

      --  A slot callback receives the supplied GObject as its first argument.
      --  Connect both native GtkAda callbacks and GObject slot callbacks so
      --  both marshaller families handle the same signal emissions.
      Buffer.On_Apply_Tag (On_Tag_Slot'Unrestricted_Access, Slot => Buffer);
      Buffer.On_Remove_Tag (On_Tag_Slot'Unrestricted_Access, Slot => Buffer);
      Buffer.On_Begin_User_Action
        (On_Void_Slot'Unrestricted_Access, Slot => Buffer);
      Buffer.On_Changed (On_Void_Slot'Unrestricted_Access, Slot => Buffer);
      Buffer.On_Delete_Range
        (On_Range_Slot'Unrestricted_Access, Slot => Buffer);
      Buffer.On_End_User_Action
        (On_Void_Slot'Unrestricted_Access, Slot => Buffer);
      Buffer.On_Insert_Child_Anchor
        (On_Anchor_Slot'Unrestricted_Access, Slot => Buffer);
      Buffer.On_Insert_Paintable
        (On_Paintable_Slot'Unrestricted_Access, Slot => Buffer);
      Buffer.On_Insert_Text (On_Text_Slot'Unrestricted_Access, Slot => Buffer);
      Buffer.On_Mark_Deleted (On_Mark_Slot'Unrestricted_Access, Slot => Buffer);
      Buffer.On_Mark_Set
        (On_Iter_Mark_Slot'Unrestricted_Access, Slot => Buffer);
      Buffer.On_Modified_Changed
        (On_Void_Slot'Unrestricted_Access, Slot => Buffer);
      Buffer.On_Redo (On_Void_Slot'Unrestricted_Access, Slot => Buffer);
      Buffer.On_Undo (On_Void_Slot'Unrestricted_Access, Slot => Buffer);

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

      --  The remove-tag signal uses the same tag/range marshaller.
      --  Its callback checks the exact half-open range before default removal.
      Buffer.Remove_Tag (Tag, Start, The_End);
      Assert_Cmpint_Eq (Gint (Remove_Tag_Count), 1);

      --  delete-range and changed fire on deletion.
      Buffer.Get_Bounds (Start, The_End);
      Buffer.Delete (Start, The_End);
      Assert_Cmpint_Eq (Gint (Delete_Count), 1);
      Assert_Cmpint_Eq (Gint (Changed_Count), 2);

      --  Cover the remaining signal payloads with real buffer events.
      --  Flip the modified bit, wrap an edit in a user action, then undo and
      --  redo it to emit their respective notifications.
      Buffer.Set_Modified (False);
      Buffer.Set_Modified (True);
      Buffer.Begin_User_Action;
      Buffer.Insert_At_Cursor ("undoable");
      Buffer.End_User_Action;
      Buffer.Undo;
      Buffer.Redo;
      Buffer.Get_Start_Iter (Iter);
      Anchor := Buffer.Create_Child_Anchor (Iter);
      --  Insert the anchor and a tiny paintable to exercise object payloads.
      Pixbuf := Gdk.Pixbuf.Gdk_Pixbuf_New
        (Gdk.Pixbuf.Gdk_Colorspace_Rgb, True, 8, 1, 1);
      Pixbuf.Fill (16#3366CCFF#);
      Texture := Gdk.Texture.Gdk_Texture_New_For_Pixbuf (Pixbuf);
      Buffer.Get_End_Iter (Iter);
      Buffer.Insert_Paintable (Iter, +Texture);
      Buffer.Get_Start_Iter (Iter);
      Mark := Buffer.Create_Mark ("delete-me", Iter);
      Buffer.Delete_Mark (Mark);

      --  Each signal family has at least one observable callback invocation.
      Assert_Cmpint_Eq (Gint (Begin_Action_Count), 1);
      Assert_Cmpint_Eq (Gint (End_Action_Count), 1);
      Assert_True (Modified_Count >= 2);
      Assert_Cmpint_Eq (Gint (Undo_Count), 1);
      Assert_Cmpint_Eq (Gint (Redo_Count), 1);
      Assert_Cmpint_Eq (Gint (Child_Anchor_Count), 1);
      Assert_Cmpint_Eq (Gint (Paintable_Count), 1);
      Assert_Cmpint_Eq (Gint (Mark_Deleted_Count), 1);
      Assert_True (Slot_Void_Count > 0);
      Assert_True (Slot_Range_Count > 0);
      Assert_True (Slot_Tag_Count > 0);
      Assert_True (Slot_Anchor_Count > 0);
      Assert_True (Slot_Paintable_Count > 0);
      Assert_True (Slot_Text_Count > 0);
      Assert_True (Slot_Mark_Count > 0);
      Assert_True (Slot_Iter_Mark_Count > 0);
      Texture.Unref;
      Pixbuf.Unref;
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
     ("/text-buffer/tag-table", Test_Tag_Table'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-buffer/child-anchor", Test_Child_Anchor'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-buffer/selection", Test_Selection'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-buffer/undo", Test_Undo'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-buffer/additional-methods",
      Test_Additional_Methods'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-buffer/clipboard", Test_Clipboard'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-buffer/signals", Test_Signals'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
