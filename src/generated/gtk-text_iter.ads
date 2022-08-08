------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  <description>
--  You may wish to begin by reading the [text widget conceptual
--  overview][TextWidget] which gives an overview of all the objects and data
--  types related to the text widget and how they work together.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Pixbuf;              use Gdk.Pixbuf;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Values;             use Glib.Values;
with Gtk.Text_Attributes;     use Gtk.Text_Attributes;
with Gtk.Text_Tag;            use Gtk.Text_Tag;
with Gtkada.Types;            use Gtkada.Types;
with Pango.Language;          use Pango.Language;

package Gtk.Text_Iter is

   type Gtk_Text_Search_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Text_Search_Flags);
   --  Flags affecting how a search is done.
   --
   --  If neither GTK_TEXT_SEARCH_VISIBLE_ONLY nor GTK_TEXT_SEARCH_TEXT_ONLY
   --  are enabled, the match must be exact; the special 0xFFFC character will
   --  match embedded pixbufs or child widgets.

   Visible_Only : constant Gtk_Text_Search_Flags := 1;
   Text_Only : constant Gtk_Text_Search_Flags := 2;
   Case_Insensitive : constant Gtk_Text_Search_Flags := 4;

   type Gtk_Text_Iter is private;
   function From_Object_Free (B : access Gtk_Text_Iter) return Gtk_Text_Iter;
   pragma Inline (From_Object_Free);
   --  You may wish to begin by reading the [text widget conceptual
   --  overview][TextWidget] which gives an overview of all the objects and
   --  data types related to the text widget and how they work together.

   Null_Text_Iter : constant Gtk_Text_Iter;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Text_Search_Flags_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Text_Search_Flags);
   type Property_Gtk_Text_Search_Flags is new Gtk_Text_Search_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_text_iter_get_type");

   -------------
   -- Methods --
   -------------

   procedure Assign (Iter : Gtk_Text_Iter; Other : Gtk_Text_Iter);
   pragma Import (C, Assign, "gtk_text_iter_assign");
   --  Assigns the value of Other to Iter. This function is not useful in
   --  applications, because iterators can be assigned with `GtkTextIter i =
   --  j;`. The function is used by language bindings.
   --  Since: gtk+ 3.2
   --  "other": another Gtk.Text_Iter.Gtk_Text_Iter

   procedure Backward_Char
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean);
   --  Moves backward by one character offset. Returns True if movement was
   --  possible; if Iter was the first in the buffer (character offset 0),
   --  Gtk.Text_Iter.Backward_Char returns False for convenience when writing
   --  loops.

   procedure Backward_Chars
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean);
   --  Moves Count characters backward, if possible (if Count would move past
   --  the start or end of the buffer, moves to the start or end of the
   --  buffer). The return value indicates whether the iterator moved onto a
   --  dereferenceable position; if the iterator didn't move, or moved onto the
   --  end iterator, then False is returned. If Count is 0, the function does
   --  nothing and returns False.
   --  "count": number of characters to move

   procedure Backward_Cursor_Position
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean);
   --  Like Gtk.Text_Iter.Forward_Cursor_Position, but moves backward.

   procedure Backward_Cursor_Positions
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean);
   --  Moves up to Count cursor positions. See
   --  Gtk.Text_Iter.Forward_Cursor_Position for details.
   --  "count": number of positions to move

   procedure Backward_Line
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean);
   --  Moves Iter to the start of the previous line. Returns True if Iter
   --  could be moved; i.e. if Iter was at character offset 0, this function
   --  returns False. Therefore if Iter was already on line 0, but not at the
   --  start of the line, Iter is snapped to the start of the line and the
   --  function returns True. (Note that this implies that in a loop calling
   --  this function, the line number may not change on every iteration, if
   --  your first iteration is on line 0.)

   procedure Backward_Lines
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean);
   --  Moves Count lines backward, if possible (if Count would move past the
   --  start or end of the buffer, moves to the start or end of the buffer).
   --  The return value indicates whether the iterator moved onto a
   --  dereferenceable position; if the iterator didn't move, or moved onto the
   --  end iterator, then False is returned. If Count is 0, the function does
   --  nothing and returns False. If Count is negative, moves forward by 0 -
   --  Count lines.
   --  "count": number of lines to move backward

   procedure Backward_Search
      (Iter        : in out Gtk_Text_Iter;
       Str         : UTF8_String;
       Flags       : Gtk_Text_Search_Flags;
       Match_Start : out Gtk_Text_Iter;
       Match_End   : out Gtk_Text_Iter;
       Limit       : Gtk_Text_Iter := Null_Text_Iter;
       Result      : out Boolean);
   --  Same as Gtk.Text_Iter.Forward_Search, but moves backward.
   --  Match_End will never be set to a Gtk.Text_Iter.Gtk_Text_Iter located
   --  after Iter, even if there is a possible Match_Start before or at Iter.
   --  "str": search string
   --  "flags": bitmask of flags affecting the search
   --  "match_start": return location for start of match, or null
   --  "match_end": return location for end of match, or null
   --  "limit": location of last possible Match_Start, or null for start of
   --  buffer

   procedure Backward_Sentence_Start
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean);
   --  Moves backward to the previous sentence start; if Iter is already at
   --  the start of a sentence, moves backward to the next one. Sentence
   --  boundaries are determined by Pango and should be correct for nearly any
   --  language (if not, the correct fix would be to the Pango text boundary
   --  algorithms).

   procedure Backward_Sentence_Starts
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean);
   --  Calls Gtk.Text_Iter.Backward_Sentence_Start up to Count times, or until
   --  it returns False. If Count is negative, moves forward instead of
   --  backward.
   --  "count": number of sentences to move

   procedure Backward_To_Tag_Toggle
      (Iter   : in out Gtk_Text_Iter;
       Tag    : Gtk.Text_Tag.Gtk_Text_Tag := null;
       Result : out Boolean);
   --  Moves backward to the next toggle (on or off) of the
   --  Gtk.Text_Tag.Gtk_Text_Tag Tag, or to the next toggle of any tag if Tag
   --  is null. If no matching tag toggles are found, returns False, otherwise
   --  True. Does not return toggles located at Iter, only toggles before Iter.
   --  Sets Iter to the location of the toggle, or the start of the buffer if
   --  no toggle is found.
   --  "tag": a Gtk.Text_Tag.Gtk_Text_Tag, or null

   procedure Backward_Visible_Cursor_Position
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean);
   --  Moves Iter forward to the previous visible cursor position. See
   --  Gtk.Text_Iter.Backward_Cursor_Position for details.
   --  Since: gtk+ 2.4

   procedure Backward_Visible_Cursor_Positions
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean);
   --  Moves up to Count visible cursor positions. See
   --  Gtk.Text_Iter.Backward_Cursor_Position for details.
   --  Since: gtk+ 2.4
   --  "count": number of positions to move

   procedure Backward_Visible_Line
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean);
   --  Moves Iter to the start of the previous visible line. Returns True if
   --  Iter could be moved; i.e. if Iter was at character offset 0, this
   --  function returns False. Therefore if Iter was already on line 0, but not
   --  at the start of the line, Iter is snapped to the start of the line and
   --  the function returns True. (Note that this implies that in a loop
   --  calling this function, the line number may not change on every
   --  iteration, if your first iteration is on line 0.)
   --  Since: gtk+ 2.8

   procedure Backward_Visible_Lines
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean);
   --  Moves Count visible lines backward, if possible (if Count would move
   --  past the start or end of the buffer, moves to the start or end of the
   --  buffer). The return value indicates whether the iterator moved onto a
   --  dereferenceable position; if the iterator didn't move, or moved onto the
   --  end iterator, then False is returned. If Count is 0, the function does
   --  nothing and returns False. If Count is negative, moves forward by 0 -
   --  Count lines.
   --  Since: gtk+ 2.8
   --  "count": number of lines to move backward

   procedure Backward_Visible_Word_Start
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean);
   --  Moves backward to the previous visible word start. (If Iter is
   --  currently on a word start, moves backward to the next one after that.)
   --  Word breaks are determined by Pango and should be correct for nearly any
   --  language (if not, the correct fix would be to the Pango word break
   --  algorithms).
   --  Since: gtk+ 2.4

   procedure Backward_Visible_Word_Starts
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean);
   --  Calls Gtk.Text_Iter.Backward_Visible_Word_Start up to Count times.
   --  Since: gtk+ 2.4
   --  "count": number of times to move

   procedure Backward_Word_Start
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean);
   --  Moves backward to the previous word start. (If Iter is currently on a
   --  word start, moves backward to the next one after that.) Word breaks are
   --  determined by Pango and should be correct for nearly any language (if
   --  not, the correct fix would be to the Pango word break algorithms).

   procedure Backward_Word_Starts
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean);
   --  Calls Gtk.Text_Iter.Backward_Word_Start up to Count times.
   --  "count": number of times to move

   function Begins_Tag
      (Iter : Gtk_Text_Iter;
       Tag  : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class) return Boolean;
   pragma Obsolescent (Begins_Tag);
   --  Returns True if Tag is toggled on at exactly this point. If Tag is
   --  null, returns True if any tag is toggled on at this point.
   --  Note that if Gtk.Text_Iter.Begins_Tag returns True, it means that Iter
   --  is at the beginning of the tagged range, and that the character at Iter
   --  is inside the tagged range. In other words, unlike
   --  Gtk.Text_Iter.Ends_Tag, if Gtk.Text_Iter.Begins_Tag returns True,
   --  Gtk.Text_Iter.Has_Tag will also return True for the same parameters.
   --  Deprecated since 3.20, 1
   --  "tag": a Gtk.Text_Tag.Gtk_Text_Tag, or null

   function Can_Insert
      (Iter                : Gtk_Text_Iter;
       Default_Editability : Boolean) return Boolean;
   --  Considering the default editability of the buffer, and tags that affect
   --  editability, determines whether text inserted at Iter would be editable.
   --  If text inserted at Iter would be editable then the user should be
   --  allowed to insert text at Iter. Gtk.Text_Buffer.Insert_Interactive uses
   --  this function to decide whether insertions are allowed at a given
   --  position.
   --  "default_editability": True if text is editable by default

   function Compare
      (Iter : Gtk_Text_Iter;
       Rhs  : Gtk_Text_Iter) return Glib.Gint;
   pragma Import (C, Compare, "gtk_text_iter_compare");
   --  A qsort-style function that returns negative if Lhs is less than Rhs,
   --  positive if Lhs is greater than Rhs, and 0 if they're equal. Ordering is
   --  in character offset order, i.e. the first character in the buffer is
   --  less than the second character in the buffer.
   --  "rhs": another Gtk.Text_Iter.Gtk_Text_Iter

   function Editable
      (Iter            : Gtk_Text_Iter;
       Default_Setting : Boolean) return Boolean;
   --  Returns whether the character at Iter is within an editable region of
   --  text. Non-editable text is "locked" and can't be changed by the user via
   --  Gtk.Text_View.Gtk_Text_View. This function is simply a convenience
   --  wrapper around Gtk.Text_Iter.Get_Attributes. If no tags applied to this
   --  text affect editability, Default_Setting will be returned.
   --  You don't want to use this function to decide whether text can be
   --  inserted at Iter, because for insertion you don't want to know whether
   --  the char at Iter is inside an editable range, you want to know whether a
   --  new character inserted at Iter would be inside an editable range. Use
   --  Gtk.Text_Iter.Can_Insert to handle this case.
   --  "default_setting": True if text is editable by default

   function Ends_Line (Iter : Gtk_Text_Iter) return Boolean;
   --  Returns True if Iter points to the start of the paragraph delimiter
   --  characters for a line (delimiters will be either a newline, a carriage
   --  return, a carriage return followed by a newline, or a Unicode paragraph
   --  separator character). Note that an iterator pointing to the \n of a \r\n
   --  pair will not be counted as the end of a line, the line ends before the
   --  \r. The end iterator is considered to be at the end of a line, even
   --  though there are no paragraph delimiter chars there.

   function Ends_Sentence (Iter : Gtk_Text_Iter) return Boolean;
   --  Determines whether Iter ends a sentence. Sentence boundaries are
   --  determined by Pango and should be correct for nearly any language (if
   --  not, the correct fix would be to the Pango text boundary algorithms).

   function Ends_Tag
      (Iter : Gtk_Text_Iter;
       Tag  : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class) return Boolean;
   --  Returns True if Tag is toggled off at exactly this point. If Tag is
   --  null, returns True if any tag is toggled off at this point.
   --  Note that if Gtk.Text_Iter.Ends_Tag returns True, it means that Iter is
   --  at the end of the tagged range, but that the character at Iter is
   --  outside the tagged range. In other words, unlike
   --  Gtk.Text_Iter.Starts_Tag, if Gtk.Text_Iter.Ends_Tag returns True,
   --  Gtk.Text_Iter.Has_Tag will return False for the same parameters.
   --  "tag": a Gtk.Text_Tag.Gtk_Text_Tag, or null

   function Ends_Word (Iter : Gtk_Text_Iter) return Boolean;
   --  Determines whether Iter ends a natural-language word. Word breaks are
   --  determined by Pango and should be correct for nearly any language (if
   --  not, the correct fix would be to the Pango word break algorithms).

   function Equal (Iter : Gtk_Text_Iter; Rhs : Gtk_Text_Iter) return Boolean;
   --  Tests whether two iterators are equal, using the fastest possible
   --  mechanism. This function is very fast; you can expect it to perform
   --  better than e.g. getting the character offset for each iterator and
   --  comparing the offsets yourself. Also, it's a bit faster than
   --  Gtk.Text_Iter.Compare.
   --  "rhs": another Gtk.Text_Iter.Gtk_Text_Iter

   procedure Forward_Char
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean);
   --  Moves Iter forward by one character offset. Note that images embedded
   --  in the buffer occupy 1 character slot, so Gtk.Text_Iter.Forward_Char may
   --  actually move onto an image instead of a character, if you have images
   --  in your buffer. If Iter is the end iterator or one character before it,
   --  Iter will now point at the end iterator, and Gtk.Text_Iter.Forward_Char
   --  returns False for convenience when writing loops.

   procedure Forward_Chars
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean);
   --  Moves Count characters if possible (if Count would move past the start
   --  or end of the buffer, moves to the start or end of the buffer). The
   --  return value indicates whether the new position of Iter is different
   --  from its original position, and dereferenceable (the last iterator in
   --  the buffer is not dereferenceable). If Count is 0, the function does
   --  nothing and returns False.
   --  "count": number of characters to move, may be negative

   procedure Forward_Cursor_Position
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean);
   --  Moves Iter forward by a single cursor position. Cursor positions are
   --  (unsurprisingly) positions where the cursor can appear. Perhaps
   --  surprisingly, there may not be a cursor position between all characters.
   --  The most common example for European languages would be a carriage
   --  return/newline sequence. For some Unicode characters, the equivalent of
   --  say the letter "a" with an accent mark will be represented as two
   --  characters, first the letter then a "combining mark" that causes the
   --  accent to be rendered; so the cursor can't go between those two
   --  characters. See also the Pango_Log_Attr-struct and pango_break function.

   procedure Forward_Cursor_Positions
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean);
   --  Moves up to Count cursor positions. See
   --  Gtk.Text_Iter.Forward_Cursor_Position for details.
   --  "count": number of positions to move

   procedure Forward_Line
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean);
   --  Moves Iter to the start of the next line. If the iter is already on the
   --  last line of the buffer, moves the iter to the end of the current line.
   --  If after the operation, the iter is at the end of the buffer and not
   --  dereferencable, returns False. Otherwise, returns True.

   procedure Forward_Lines
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean);
   --  Moves Count lines forward, if possible (if Count would move past the
   --  start or end of the buffer, moves to the start or end of the buffer).
   --  The return value indicates whether the iterator moved onto a
   --  dereferenceable position; if the iterator didn't move, or moved onto the
   --  end iterator, then False is returned. If Count is 0, the function does
   --  nothing and returns False. If Count is negative, moves backward by 0 -
   --  Count lines.
   --  "count": number of lines to move forward

   procedure Forward_Search
      (Iter        : in out Gtk_Text_Iter;
       Str         : UTF8_String;
       Flags       : Gtk_Text_Search_Flags;
       Match_Start : out Gtk_Text_Iter;
       Match_End   : out Gtk_Text_Iter;
       Limit       : Gtk_Text_Iter := Null_Text_Iter;
       Result      : out Boolean);
   --  Searches forward for Str. Any match is returned by setting Match_Start
   --  to the first character of the match and Match_End to the first character
   --  after the match. The search will not continue past Limit. Note that a
   --  search is a linear or O(n) operation, so you may wish to use Limit to
   --  avoid locking up your UI on large buffers.
   --  Match_Start will never be set to a Gtk.Text_Iter.Gtk_Text_Iter located
   --  before Iter, even if there is a possible Match_End after or at Iter.
   --  "str": a search string
   --  "flags": flags affecting how the search is done
   --  "match_start": return location for start of match, or null
   --  "match_end": return location for end of match, or null
   --  "limit": location of last possible Match_End, or null for the end of
   --  the buffer

   procedure Forward_Sentence_End
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean);
   --  Moves forward to the next sentence end. (If Iter is at the end of a
   --  sentence, moves to the next end of sentence.) Sentence boundaries are
   --  determined by Pango and should be correct for nearly any language (if
   --  not, the correct fix would be to the Pango text boundary algorithms).

   procedure Forward_Sentence_Ends
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean);
   --  Calls Gtk.Text_Iter.Forward_Sentence_End Count times (or until
   --  Gtk.Text_Iter.Forward_Sentence_End returns False). If Count is negative,
   --  moves backward instead of forward.
   --  "count": number of sentences to move

   procedure Forward_To_End (Iter : in out Gtk_Text_Iter);
   pragma Import (C, Forward_To_End, "gtk_text_iter_forward_to_end");
   --  Moves Iter forward to the "end iterator," which points one past the
   --  last valid character in the buffer. Gtk.Text_Iter.Get_Char called on the
   --  end iterator returns 0, which is convenient for writing loops.

   procedure Forward_To_Line_End
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean);
   --  Moves the iterator to point to the paragraph delimiter characters,
   --  which will be either a newline, a carriage return, a carriage
   --  return/newline in sequence, or the Unicode paragraph separator
   --  character. If the iterator is already at the paragraph delimiter
   --  characters, moves to the paragraph delimiter characters for the next
   --  line. If Iter is on the last line in the buffer, which does not end in
   --  paragraph delimiters, moves to the end iterator (end of the last line),
   --  and returns False.

   procedure Forward_To_Tag_Toggle
      (Iter   : in out Gtk_Text_Iter;
       Tag    : Gtk.Text_Tag.Gtk_Text_Tag := null;
       Result : out Boolean);
   --  Moves forward to the next toggle (on or off) of the
   --  Gtk.Text_Tag.Gtk_Text_Tag Tag, or to the next toggle of any tag if Tag
   --  is null. If no matching tag toggles are found, returns False, otherwise
   --  True. Does not return toggles located at Iter, only toggles after Iter.
   --  Sets Iter to the location of the toggle, or to the end of the buffer if
   --  no toggle is found.
   --  "tag": a Gtk.Text_Tag.Gtk_Text_Tag, or null

   procedure Forward_Visible_Cursor_Position
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean);
   --  Moves Iter forward to the next visible cursor position. See
   --  Gtk.Text_Iter.Forward_Cursor_Position for details.
   --  Since: gtk+ 2.4

   procedure Forward_Visible_Cursor_Positions
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean);
   --  Moves up to Count visible cursor positions. See
   --  Gtk.Text_Iter.Forward_Cursor_Position for details.
   --  Since: gtk+ 2.4
   --  "count": number of positions to move

   procedure Forward_Visible_Line
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean);
   --  Moves Iter to the start of the next visible line. Returns True if there
   --  was a next line to move to, and False if Iter was simply moved to the
   --  end of the buffer and is now not dereferenceable, or if Iter was already
   --  at the end of the buffer.
   --  Since: gtk+ 2.8

   procedure Forward_Visible_Lines
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean);
   --  Moves Count visible lines forward, if possible (if Count would move
   --  past the start or end of the buffer, moves to the start or end of the
   --  buffer). The return value indicates whether the iterator moved onto a
   --  dereferenceable position; if the iterator didn't move, or moved onto the
   --  end iterator, then False is returned. If Count is 0, the function does
   --  nothing and returns False. If Count is negative, moves backward by 0 -
   --  Count lines.
   --  Since: gtk+ 2.8
   --  "count": number of lines to move forward

   procedure Forward_Visible_Word_End
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean);
   --  Moves forward to the next visible word end. (If Iter is currently on a
   --  word end, moves forward to the next one after that.) Word breaks are
   --  determined by Pango and should be correct for nearly any language (if
   --  not, the correct fix would be to the Pango word break algorithms).
   --  Since: gtk+ 2.4

   procedure Forward_Visible_Word_Ends
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean);
   --  Calls Gtk.Text_Iter.Forward_Visible_Word_End up to Count times.
   --  Since: gtk+ 2.4
   --  "count": number of times to move

   procedure Forward_Word_End
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean);
   --  Moves forward to the next word end. (If Iter is currently on a word
   --  end, moves forward to the next one after that.) Word breaks are
   --  determined by Pango and should be correct for nearly any language (if
   --  not, the correct fix would be to the Pango word break algorithms).

   procedure Forward_Word_Ends
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean);
   --  Calls Gtk.Text_Iter.Forward_Word_End up to Count times.
   --  "count": number of times to move

   procedure Free (Iter : Gtk_Text_Iter);
   pragma Import (C, Free, "gtk_text_iter_free");
   --  Free an iterator allocated on the heap. This function is intended for
   --  use in language bindings, and is not especially useful for applications,
   --  because iterators can simply be allocated on the stack.

   function Get_Attributes
      (Iter   : Gtk_Text_Iter;
       Values : access Gtk.Text_Attributes.Gtk_Text_Attributes)
       return Boolean;
   --  Computes the effect of any tags applied to this spot in the text. The
   --  Values parameter should be initialized to the default settings you wish
   --  to use if no tags are in effect. You'd typically obtain the defaults
   --  from Gtk.Text_View.Get_Default_Attributes.
   --  Gtk.Text_Iter.Get_Attributes will modify Values, applying the effects
   --  of any tags present at Iter. If any tags affected Values, the function
   --  returns True.
   --  "values": a Gtk.Text_Attributes.Gtk_Text_Attributes to be filled in

   function Get_Bytes_In_Line (Iter : Gtk_Text_Iter) return Glib.Gint;
   pragma Import (C, Get_Bytes_In_Line, "gtk_text_iter_get_bytes_in_line");
   --  Returns the number of bytes in the line containing Iter, including the
   --  paragraph delimiters.

   function Get_Char (Iter : Gtk_Text_Iter) return Gunichar;
   pragma Import (C, Get_Char, "gtk_text_iter_get_char");
   --  The Unicode character at this iterator is returned. (Equivalent to
   --  operator* on a C++ iterator.) If the element at this iterator is a
   --  non-character element, such as an image embedded in the buffer, the
   --  Unicode "unknown" character 0xFFFC is returned. If invoked on the end
   --  iterator, zero is returned; zero is not a valid Unicode character. So
   --  you can write a loop which ends when Gtk.Text_Iter.Get_Char returns 0.

   function Get_Chars_In_Line (Iter : Gtk_Text_Iter) return Glib.Gint;
   pragma Import (C, Get_Chars_In_Line, "gtk_text_iter_get_chars_in_line");
   --  Returns the number of characters in the line containing Iter, including
   --  the paragraph delimiters.

   function Get_Language
      (Iter : Gtk_Text_Iter) return Pango.Language.Pango_Language;
   --  A convenience wrapper around Gtk.Text_Iter.Get_Attributes, which
   --  returns the language in effect at Iter. If no tags affecting language
   --  apply to Iter, the return value is identical to that of
   --  Gtk.Main.Get_Default_Language.

   function Get_Line (Iter : Gtk_Text_Iter) return Glib.Gint;
   pragma Import (C, Get_Line, "gtk_text_iter_get_line");
   --  Returns the line number containing the iterator. Lines in a
   --  Gtk.Text_Buffer.Gtk_Text_Buffer are numbered beginning with 0 for the
   --  first line in the buffer.

   procedure Set_Line (Iter : in out Gtk_Text_Iter; Line_Number : Glib.Gint);
   pragma Import (C, Set_Line, "gtk_text_iter_set_line");
   --  Moves iterator Iter to the start of the line Line_Number. If
   --  Line_Number is negative or larger than the number of lines in the
   --  buffer, moves Iter to the start of the last line in the buffer.
   --  "line_number": line number (counted from 0)

   function Get_Line_Index (Iter : Gtk_Text_Iter) return Glib.Gint;
   pragma Import (C, Get_Line_Index, "gtk_text_iter_get_line_index");
   --  Returns the byte index of the iterator, counting from the start of a
   --  newline-terminated line. Remember that Gtk.Text_Buffer.Gtk_Text_Buffer
   --  encodes text in UTF-8, and that characters can require a variable number
   --  of bytes to represent.

   procedure Set_Line_Index
      (Iter         : in out Gtk_Text_Iter;
       Byte_On_Line : Glib.Gint);
   pragma Import (C, Set_Line_Index, "gtk_text_iter_set_line_index");
   --  Same as Gtk.Text_Iter.Set_Line_Offset, but works with a byte index. The
   --  given byte index must be at the start of a character, it can't be in the
   --  middle of a UTF-8 encoded character.
   --  "byte_on_line": a byte index relative to the start of Iter's current
   --  line

   function Get_Line_Offset (Iter : Gtk_Text_Iter) return Glib.Gint;
   pragma Import (C, Get_Line_Offset, "gtk_text_iter_get_line_offset");
   --  Returns the character offset of the iterator, counting from the start
   --  of a newline-terminated line. The first character on the line has offset
   --  0.

   procedure Set_Line_Offset
      (Iter         : in out Gtk_Text_Iter;
       Char_On_Line : Glib.Gint);
   pragma Import (C, Set_Line_Offset, "gtk_text_iter_set_line_offset");
   --  Moves Iter within a line, to a new character (not byte) offset. The
   --  given character offset must be less than or equal to the number of
   --  characters in the line; if equal, Iter moves to the start of the next
   --  line. See Gtk.Text_Iter.Set_Line_Index if you have a byte index rather
   --  than a character offset.
   --  "char_on_line": a character offset relative to the start of Iter's
   --  current line

   function Get_Marks
      (Iter : Gtk_Text_Iter) return Glib.Object.Object_List.GSlist;
   --  Returns a list of all Gtk.Text_Mark.Gtk_Text_Mark at this location.
   --  Because marks are not iterable (they don't take up any "space" in the
   --  buffer, they are just marks in between iterable locations), multiple
   --  marks can exist in the same place. The returned list is not in any
   --  meaningful order.

   function Get_Offset (Iter : Gtk_Text_Iter) return Glib.Gint;
   pragma Import (C, Get_Offset, "gtk_text_iter_get_offset");
   --  Returns the character offset of an iterator. Each character in a
   --  Gtk.Text_Buffer.Gtk_Text_Buffer has an offset, starting with 0 for the
   --  first character in the buffer. Use Gtk.Text_Buffer.Get_Iter_At_Offset to
   --  convert an offset back into an iterator.

   procedure Set_Offset
      (Iter        : in out Gtk_Text_Iter;
       Char_Offset : Glib.Gint);
   pragma Import (C, Set_Offset, "gtk_text_iter_set_offset");
   --  Sets Iter to point to Char_Offset. Char_Offset counts from the start of
   --  the entire text buffer, starting with 0.
   --  "char_offset": a character number

   function Get_Pixbuf (Iter : Gtk_Text_Iter) return Gdk.Pixbuf.Gdk_Pixbuf;
   --  If the element at Iter is a pixbuf, the pixbuf is returned (with no new
   --  reference count added). Otherwise, null is returned.

   function Get_Slice
      (Iter    : Gtk_Text_Iter;
       The_End : Gtk_Text_Iter) return UTF8_String;
   --  Returns the text in the given range. A "slice" is an array of
   --  characters encoded in UTF-8 format, including the Unicode "unknown"
   --  character 0xFFFC for iterable non-character elements in the buffer, such
   --  as images. Because images are encoded in the slice, byte and character
   --  offsets in the returned array will correspond to byte offsets in the
   --  text buffer. Note that 0xFFFC can occur in normal text as well, so it is
   --  not a reliable indicator that a pixbuf or widget is in the buffer.
   --  "end": iterator at end of a range

   function Get_Tags
      (Iter : Gtk_Text_Iter) return Gtk.Text_Tag.Text_Tag_List.GSlist;
   --  Returns a list of tags that apply to Iter, in ascending order of
   --  priority (highest-priority tags are last). The Gtk.Text_Tag.Gtk_Text_Tag
   --  in the list don't have a reference added, but you have to free the list
   --  itself.

   function Get_Text
      (Iter    : Gtk_Text_Iter;
       The_End : Gtk_Text_Iter) return UTF8_String;
   --  Returns text in the given range. If the range contains non-text
   --  elements such as images, the character and byte offsets in the returned
   --  string will not correspond to character and byte offsets in the buffer.
   --  If you want offsets to correspond, see Gtk.Text_Iter.Get_Slice.
   --  "end": iterator at end of a range

   function Get_Toggled_Tags
      (Iter       : Gtk_Text_Iter;
       Toggled_On : Boolean) return Gtk.Text_Tag.Text_Tag_List.GSlist;
   --  Returns a list of Gtk.Text_Tag.Gtk_Text_Tag that are toggled on or off
   --  at this point. (If Toggled_On is True, the list contains tags that are
   --  toggled on.) If a tag is toggled on at Iter, then some non-empty range
   --  of characters following Iter has that tag applied to it. If a tag is
   --  toggled off, then some non-empty range following Iter does not have the
   --  tag applied to it.
   --  "toggled_on": True to get toggled-on tags

   function Get_Visible_Line_Index (Iter : Gtk_Text_Iter) return Glib.Gint;
   pragma Import (C, Get_Visible_Line_Index, "gtk_text_iter_get_visible_line_index");
   --  Returns the number of bytes from the start of the line to the given
   --  Iter, not counting bytes that are invisible due to tags with the
   --  "invisible" flag toggled on.

   procedure Set_Visible_Line_Index
      (Iter         : in out Gtk_Text_Iter;
       Byte_On_Line : Glib.Gint);
   pragma Import (C, Set_Visible_Line_Index, "gtk_text_iter_set_visible_line_index");
   --  Like Gtk.Text_Iter.Set_Line_Index, but the index is in visible bytes,
   --  i.e. text with a tag making it invisible is not counted in the index.
   --  "byte_on_line": a byte index

   function Get_Visible_Line_Offset (Iter : Gtk_Text_Iter) return Glib.Gint;
   pragma Import (C, Get_Visible_Line_Offset, "gtk_text_iter_get_visible_line_offset");
   --  Returns the offset in characters from the start of the line to the
   --  given Iter, not counting characters that are invisible due to tags with
   --  the "invisible" flag toggled on.

   procedure Set_Visible_Line_Offset
      (Iter         : in out Gtk_Text_Iter;
       Char_On_Line : Glib.Gint);
   pragma Import (C, Set_Visible_Line_Offset, "gtk_text_iter_set_visible_line_offset");
   --  Like Gtk.Text_Iter.Set_Line_Offset, but the offset is in visible
   --  characters, i.e. text with a tag making it invisible is not counted in
   --  the offset.
   --  "char_on_line": a character offset

   function Get_Visible_Slice
      (Iter    : Gtk_Text_Iter;
       The_End : Gtk_Text_Iter) return UTF8_String;
   --  Like Gtk.Text_Iter.Get_Slice, but invisible text is not included.
   --  Invisible text is usually invisible because a Gtk.Text_Tag.Gtk_Text_Tag
   --  with the "invisible" attribute turned on has been applied to it.
   --  "end": iterator at end of range

   function Get_Visible_Text
      (Iter    : Gtk_Text_Iter;
       The_End : Gtk_Text_Iter) return UTF8_String;
   --  Like Gtk.Text_Iter.Get_Text, but invisible text is not included.
   --  Invisible text is usually invisible because a Gtk.Text_Tag.Gtk_Text_Tag
   --  with the "invisible" attribute turned on has been applied to it.
   --  "end": iterator at end of range

   function Has_Tag
      (Iter : Gtk_Text_Iter;
       Tag  : not null access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class)
       return Boolean;
   --  Returns True if Iter points to a character that is part of a range
   --  tagged with Tag. See also Gtk.Text_Iter.Starts_Tag and
   --  Gtk.Text_Iter.Ends_Tag.
   --  "tag": a Gtk.Text_Tag.Gtk_Text_Tag

   function In_Range
      (Iter    : Gtk_Text_Iter;
       Start   : Gtk_Text_Iter;
       The_End : Gtk_Text_Iter) return Boolean;
   --  Checks whether Iter falls in the range [Start, End). Start and End must
   --  be in ascending order.
   --  "start": start of range
   --  "end": end of range

   function Inside_Sentence (Iter : Gtk_Text_Iter) return Boolean;
   --  Determines whether Iter is inside a sentence (as opposed to in between
   --  two sentences, e.g. after a period and before the first letter of the
   --  next sentence). Sentence boundaries are determined by Pango and should
   --  be correct for nearly any language (if not, the correct fix would be to
   --  the Pango text boundary algorithms).

   function Inside_Word (Iter : Gtk_Text_Iter) return Boolean;
   --  Determines whether the character pointed by Iter is part of a
   --  natural-language word (as opposed to say inside some whitespace). Word
   --  breaks are determined by Pango and should be correct for nearly any
   --  language (if not, the correct fix would be to the Pango word break
   --  algorithms).
   --  Note that if Gtk.Text_Iter.Starts_Word returns True, then this function
   --  returns True too, since Iter points to the first character of the word.

   function Is_Cursor_Position (Iter : Gtk_Text_Iter) return Boolean;
   --  See Gtk.Text_Iter.Forward_Cursor_Position or Pango_Log_Attr or
   --  pango_break for details on what a cursor position is.

   function Is_End (Iter : Gtk_Text_Iter) return Boolean;
   --  Returns True if Iter is the end iterator, i.e. one past the last
   --  dereferenceable iterator in the buffer. Gtk.Text_Iter.Is_End is the most
   --  efficient way to check whether an iterator is the end iterator.

   function Is_Start (Iter : Gtk_Text_Iter) return Boolean;
   --  Returns True if Iter is the first iterator in the buffer, that is if
   --  Iter has a character offset of 0.

   procedure Order (Iter : Gtk_Text_Iter; Second : Gtk_Text_Iter);
   pragma Import (C, Order, "gtk_text_iter_order");
   --  Swaps the value of First and Second if Second comes before First in the
   --  buffer. That is, ensures that First and Second are in sequence. Most
   --  text buffer functions that take a range call this automatically on your
   --  behalf, so there's no real reason to call it yourself in those cases.
   --  There are some exceptions, such as Gtk.Text_Iter.In_Range, that expect a
   --  pre-sorted range.
   --  "second": another Gtk.Text_Iter.Gtk_Text_Iter

   function Starts_Line (Iter : Gtk_Text_Iter) return Boolean;
   --  Returns True if Iter begins a paragraph, i.e. if
   --  Gtk.Text_Iter.Get_Line_Offset would return 0. However this function is
   --  potentially more efficient than Gtk.Text_Iter.Get_Line_Offset because it
   --  doesn't have to compute the offset, it just has to see whether it's 0.

   function Starts_Sentence (Iter : Gtk_Text_Iter) return Boolean;
   --  Determines whether Iter begins a sentence. Sentence boundaries are
   --  determined by Pango and should be correct for nearly any language (if
   --  not, the correct fix would be to the Pango text boundary algorithms).

   function Starts_Tag
      (Iter : Gtk_Text_Iter;
       Tag  : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class) return Boolean;
   --  Returns True if Tag is toggled on at exactly this point. If Tag is
   --  null, returns True if any tag is toggled on at this point.
   --  Note that if Gtk.Text_Iter.Starts_Tag returns True, it means that Iter
   --  is at the beginning of the tagged range, and that the character at Iter
   --  is inside the tagged range. In other words, unlike
   --  Gtk.Text_Iter.Ends_Tag, if Gtk.Text_Iter.Starts_Tag returns True,
   --  Gtk.Text_Iter.Has_Tag will also return True for the same parameters.
   --  Since: gtk+ 3.20
   --  "tag": a Gtk.Text_Tag.Gtk_Text_Tag, or null

   function Starts_Word (Iter : Gtk_Text_Iter) return Boolean;
   --  Determines whether Iter begins a natural-language word. Word breaks are
   --  determined by Pango and should be correct for nearly any language (if
   --  not, the correct fix would be to the Pango word break algorithms).

   function Toggles_Tag
      (Iter : Gtk_Text_Iter;
       Tag  : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class) return Boolean;
   --  This is equivalent to (gtk_text_iter_starts_tag ||
   --  Gtk.Text_Iter.Ends_Tag), i.e. it tells you whether a range with Tag
   --  applied to it begins or ends at Iter.
   --  "tag": a Gtk.Text_Tag.Gtk_Text_Tag, or null

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Copy (Source : Gtk_Text_Iter; Dest : out Gtk_Text_Iter);
   pragma Inline (Copy);
   --  Create a copy of Source.

   function Get_Char (Iter : Gtk_Text_Iter) return Character;
   --  Return the character immediately following Iter. If Iter is at the
   --  end of the buffer, then return ASCII.NUL.
   --  Note that this function assumes that the text is encoded in ASCII
   --  format. If this is not the case, use the Get_Char function that
   --  returns a Gunichar instead.

   function Iter_Or_Null (Iter : System.Address) return System.Address;
   --  Internal function for GtkAda

   -------------------------------
   -- Converting to/from GValue --
   -------------------------------

   procedure Set_Text_Iter
     (Val  : in out Glib.Values.GValue;
      Iter : Gtk_Text_Iter);
   pragma Import (C, Set_Text_Iter, "g_value_set_pointer");
   --  Set the value of the given GValue to Iter.
   --  Note that Iter is stored by reference, which means no copy of Iter
   --  is made. Iter should remain allocated as long as Val is being used.

   procedure Get_Text_Iter
     (Val  : Glib.Values.GValue;
      Iter : out Gtk_Text_Iter);
   --  Extract the iterator from the given GValue.
   --  Note that the iterator returned is a copy of the iterator referenced
   --  by the give GValue. Modifying the iterator returned does not modify
   --  the iterator referenced by the GValue.

   function Get_Slice
     (Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter) return Gtkada.Types.Chars_Ptr;
   --  Same as Get_Slice above, but returns the raw C string.
   --  The caller is responsible for freeing the string returned,
   --  using Gtkada.Types.g_free.

private
type Gtk_Text_Iter is record
   Dummy1 : System.Address := System.Null_Address;
   Dummy2 : System.Address := System.Null_Address;
   Dummy3 : Glib.Gint := 0;
   Dummy4 : Glib.Gint := 0;
   Dummy5 : Glib.Gint := 0;
   Dummy6 : Glib.Gint := 0;
   Dummy7 : Glib.Gint := 0;
   Dummy8 : Glib.Gint := 0;
   Dummy9 : System.Address := System.Null_Address;
   Dummy10 : System.Address := System.Null_Address;
   Dummy11 : Glib.Gint := 0;
   Dummy12 : Glib.Gint := 0;
   Dummy13 : Glib.Gint := 0;
   Dummy14 : System.Address := System.Null_Address;
end record;
pragma Convention (C, Gtk_Text_Iter);


   Null_Text_Iter : constant Gtk_Text_Iter :=
     (System.Null_Address, System.Null_Address, 0, 0, 0, 0, 0, 0,
      System.Null_Address, System.Null_Address, 0, 0, 0,
      System.Null_Address);
        
end Gtk.Text_Iter;
