-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  This is the public representation of a text buffer to be used in
--  cunjunction with Gtk.Text_View.
--  </description>
--  <c_version>1.3.4</c_version>

with Gdk.Pixbuf;
with Gtk.Text_Child;
with Gtk.Text_Iter;
with Gtk.Text_Mark;
with Gtk.Text_Tag;
with Gtk.Text_Tag_Table;

package Gtk.Text_Buffer is

   type Gtk_Text_Buffer_Record is new GObject_Record with private;
   type Gtk_Text_Buffer is access all Gtk_Text_Buffer_Record'Class;

   procedure Gtk_New
     (Buffer : out Gtk_Text_Buffer;
      Table  : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table := null);
   --  Create a new text buffer.
   --  Create a new table if Table is null.

   procedure Initialize
     (Buffer : access Gtk_Text_Buffer_Record'Class;
      Table  : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Text_Buffer.

   function Get_Line_Count
     (Buffer : access Gtk_Text_Buffer_Record) return Gint;
   --  Return the number of lines in the buffer.
   --  This value is cached, so the function is very fast.

   function Get_Char_Count
     (Buffer : access Gtk_Text_Buffer_Record) return Gint;
   --  Return the number of characters in the buffer.
   --  Note that characters and bytes are not the same, you can't e.g. expect
   --  the contents of the buffer in string form to be this many bytes long.
   --  The character count is cached, so this function is very fast.

   function Get_Tag_Table
     (Buffer : access Gtk_Text_Buffer_Record)
      return Gtk.Text_Tag_Table.Gtk_Text_Tag_Table;
   --  Get the Gtk_Text_Tag_Table associated with this buffer.

   procedure Set_Text
     (Buffer : access Gtk_Text_Buffer_Record;
      Text   : String);
   --  Delete current contents of Buffer, and insert Text instead.
   --  If Text doesn't end with a newline, a newline is added;
   --  Gtk_Text_Buffer contents must always end with a newline. If Text
   --  ends with a newline, the new buffer contents will be exactly Text.
   --  Text: UTF-8 format text to insert.

   procedure Insert
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Text   : String);
   --  Insert Text at position Iter.
   --  Emit the "insert_text" signal; insertion actually occurs
   --  in the default handler for the signal. Iter is invalidated when
   --  insertion occurs (because the buffer contents change), but the
   --  default signal handler revalidates it to point to the end of the
   --  inserted text.
   --  Text: UTF-8 format text to insert.

   procedure Insert_At_Cursor
     (Buffer : access Gtk_Text_Buffer_Record;
      Text   : String);
   --  Call Buffer_Insert, using the current cursor position
   --  as the insertion point.
   --  Text: UTF-8 format text to insert.

   procedure Insert_Interactive
     (Buffer           : access Gtk_Text_Buffer_Record;
      Iter             : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Text             : String;
      Default_Editable : Boolean;
      Result           : out Boolean);
   --  Like Insert, but the insertion will not occur if Iter is at a
   --  non-editable location in the buffer. Usually you
   --  want to prevent insertions at ineditable locations if the insertion
   --  results from a user action (is interactive).
   --
   --  Default_Editable indicates the editability of text that doesn't
   --  have a tag affecting editability applied to it. Typically the
   --  result of Gtk.Text_View.Get_Editable is appropriate here.
   --  Text: UTF-8 format text to insert.
   --  Result: whether text was actually inserted.

   function Insert_Interactive_At_Cursor
     (Buffer           : access Gtk_Text_Buffer_Record;
      Text             : String;
      Default_Editable : Boolean) return Boolean;
   --  Call Insert_Interactive at the cursor position.
   --  Text: UTF-8 format text to insert.
   --  Return value: whether text was actually inserted.

   procedure Insert_Range
     (Buffer  : access Gtk_Text_Buffer_Record;
      Iter    : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Start   : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Copy text, tags, and pixbufs between Start and End.
   --  The order of Start and End doesn't matter.
   --  Also insert the copy at Iter.
   --  Used instead of simply getting/inserting text because it preserves
   --  images and tags. If Start and End are in a different buffer from
   --  Buffer, the two buffers must share the same tag table.
   --  Implemented via emissions of the insert_text and apply_tag signals,
   --  so expect those.

   procedure Insert_Range_Interactive
     (Buffer           : access Gtk_Text_Buffer_Record;
      Iter             : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Start            : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End          : Gtk.Text_Iter.Gtk_Text_Iter;
      Default_Editable : Boolean;
      Result           : out Boolean);
   --  Like Insert_Range, does nothing if the insertion point isn't editable.
   --  The Default_Editable parameter indicates whether the text is editable at
   --  Iter if no tags enclosing Iter affect editability. Typically the result
   --  of Gtk.Text_View.Get_Editable is appropriate here.
   --  Result: whether an insertion was possible at @iter

   --  gtk_text_buffer_insert_with_tags not bound: variable number of arguments
   --  could write the equivalent of this convenience function using arrays.

   --  gtk_text_buffer_insert_with_tags_by_name not bound: variable number
   --  of arguments. ditto.

   procedure Delete
     (Buffer  : access Gtk_Text_Buffer_Record;
      Start   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : in out Gtk.Text_Iter.Gtk_Text_Iter);
   --  Delete text between Start and End.
   --  The order of Start and End is not actually relevant;
   --  Delete will reorder them. This function actually emits the
   --  "delete_range" signal, and the default handler of that signal deletes
   --  the text. Because the buffer is modified, all outstanding iterators
   --  become invalid after calling this function; however, the Start and End
   --  will be re-initialized to point to the location where text was deleted.
   --
   --  Note that the final newline in the buffer may not be deleted; a
   --  Gtk_Text_Buffer always contains at least one newline. You can
   --  safely include the final newline in the range [Start,End) but it
   --  won't be affected by the deletion.

   procedure Delete_Interactive
     (Buffer           : access Gtk_Text_Buffer_Record;
      Start_Iter       : in out Gtk.Text_Iter.Gtk_Text_Iter;
      End_Iter         : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Default_Editable : Boolean;
      Result           : out Boolean);
   --  Delete all editable text in the given range.
   --  Call Delete for each editable sub-range of [Start,End). Start and End
   --  are revalidated to point to the location of the last deleted range, or
   --  left untouched if no text was deleted.
   --  Result: whether some text was actually deleted

   function Get_Text
     (Buffer               : access Gtk_Text_Buffer_Record;
      Start                : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
      Include_Hidden_Chars : Boolean := False) return String;
   --  Return the text in the range [Start,End).
   --  Exclude undisplayed text (text marked with tags that set the
   --  invisibility attribute) if Include_Hidden_Chars is False. Does not
   --  include characters representing embedded images, so byte and character
   --  indexes into the returned string do not correspond to byte and character
   --  indexes into the buffer. Contrast with Get_Slice.
   --  Return value: an allocated UTF-8 string

   function Get_Slice
     (Buffer               : access Gtk_Text_Buffer_Record;
      Start                : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
      Include_Hidden_Chars : Boolean := False) return String;
   --  Return the text in the range [Start,End).
   --  Exclude undisplayed text (text marked with tags that set the
   --  invisibility attribute) if Include_Hidden_Chars is False. The returned
   --  string includes a 16#FFFC# character whenever the buffer contains
   --  embedded images, so byte and character indexes into
   --  the returned string do correspond to byte and character indexes into
   --  the buffer. Contrast with Get_Text. Note that 16#FFFC# can occur in
   --  normal text as well, so it is not a reliable indicator that a pixbuf or
   --  widget is in the buffer.
   --  Return value: an allocated UTF-8 string

   procedure Insert_Pixbuf
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf);
   --  Insert an image into the text buffer at Iter.
   --  The image will be counted as one character in character counts, and when
   --  obtaining the buffer contents as a string, will be represented by the
   --  Unicode "object replacement character" 16#FFFC#. Note that the "slice"
   --  variants for obtaining portions of the buffer as a string include
   --  this character for pixbufs, but the "text" variants do not. e.g. see
   --  Get_Slice and Get_Text.

   procedure Insert_Child_Anchor
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Anchor : access Gtk.Text_Child.Gtk_Text_Child_Anchor_Record'Class);
   --  Insert a child widget anchor into the text buffer at Iter.
   --  The anchor will be counted as one character in character counts, and
   --  when obtaining the buffer contents as a string, will be represented
   --  by the Unicode "object replacement character" 16#FFFC#. Note that the
   --  "slice" variants for obtaining portions of the buffer as a string
   --  include this character for pixbufs, but the "text" variants do
   --  not. e.g. see Get_Slice and Get_Text. Consider Create_Child_Anchor as a
   --  more convenient alternative to this function. The buffer will add a
   --  reference to the anchor, so you can unref it after insertion.

   procedure Create_Child_Anchor
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Result : out Gtk.Text_Child.Gtk_Text_Child_Anchor);
   --  Convenience function which simply creates a child anchor with
   --  Gtk.Text_Child.Gtk_New and inserts it into the buffer with
   --  Insert_Child_Anchor.
   --  Result: the created child anchor.

   function Create_Mark
     (Buffer       : access Gtk_Text_Buffer_Record;
      Mark_Name    : String := "";
      Where        : Gtk.Text_Iter.Gtk_Text_Iter;
      Left_Gravity : Boolean := True) return Gtk.Text_Mark.Gtk_Text_Mark;
   --  Create a mark at position Where.
   --  If Mark_Name is null, the mark is anonymous; otherwise, the mark can be
   --  retrieved by name using Get_Mark. If a mark has left gravity, and text
   --  is inserted at the mark's current location, the mark will be moved to
   --  the left of the newly-inserted text. If the mark has right gravity
   --  (Left_Gravity = False), the mark will end up on the right of
   --  newly-inserted text. The standard left-to-right cursor is a mark
   --  with right gravity (when you type, the cursor stays on the right
   --  side of the text you're typing).
   --
   --  The caller of this function does not own a reference to the returned
   --  Gtk_Text_Mark, so you can ignore the return value if you like. Marks are
   --  owned by the buffer and go away when the buffer does.
   --  Emit the "mark_set" signal as notification of the mark's initial
   --  placement.
   --
   --  Return value: the new Gtk_Text_Mark object.

   procedure Move_Mark
     (Buffer : access Gtk_Text_Buffer_Record;
      Mark   : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class;
      Where  : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Move Mark to the new location Where.
   --  Emit the "mark_set" signal as notification of the move.

   procedure Delete_Mark
     (Buffer : access Gtk_Text_Buffer_Record;
      Mark   : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);
   --  Delete Mark, so that it's no longer located anywhere in the
   --  buffer. Remove the reference the buffer holds to the mark, so if
   --  you haven't called Ref on the mark, it will be freed. Even
   --  if the mark isn't freed, most operations on Mark become
   --  invalid. There is no way to undelete a mark.
   --  Gtk.Text_Mark.Get_Deleted will return True after this
   --  function has been called on a mark; Gtk.Text_Mark.Get_Deleted
   --  indicates that a mark no longer belongs to a buffer. The "mark_deleted"
   --  signal will be emitted as notification after the mark is deleted.

   function Get_Mark
     (Buffer : access Gtk_Text_Buffer_Record;
      Name   : String) return Gtk.Text_Mark.Gtk_Text_Mark;
   --  Return the mark named Name in Buffer
   --  or null if no such mark exists in the buffer.

   procedure Move_Mark_By_Name
     (Buffer : access Gtk_Text_Buffer_Record;
      Name   : String;
      Where  : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Move the mark named Name (which must exist) to location Where.
   --  See Move_Mark for details.

   procedure Delete_Mark_By_Name
     (Buffer : access Gtk_Text_Buffer_Record;
      Name   : String);
   --  Delete the mark named Name
   --  The mark must exist. See Delete_Mark for details.

   function Get_Insert
     (Buffer : access Gtk_Text_Buffer_Record)
      return Gtk.Text_Mark.Gtk_Text_Mark;
   --  Return the mark that represents the cursor (insertion point).
   --  Equivalent to calling Get_Mark to get the mark name "insert", but
   --  slightly more efficient, and involves less typing.

   function Get_Selection_Bound
     (Buffer : access Gtk_Text_Buffer_Record)
      return Gtk.Text_Mark.Gtk_Text_Mark;
   --  Return the mark that represents the selection bound.
   --  Equivalent to calling Get_Mark to get the mark name "selection_bound",
   --  but very slightly more efficient, and involves less typing.
   --
   --  The currently-selected text in Buffer is the region between the
   --  "selection_bound" and "insert" marks. If "selection_bound" and
   --  "insert" are in the same place, then there is no current selection.
   --  Get_Selection_Bounds is another convenient function for handling the
   --  selection, if you just want to know whether there's a selection and what
   --  its bounds are.

   procedure Place_Cursor
     (Buffer : access Gtk_Text_Buffer_Record;
      Where  : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Move the "insert" and "selection_bound" marks simultaneously.
   --  If you move them to the same place in two steps with Move_Mark, you will
   --  temporarily select a region in between their old and new locations,
   --  which can be pretty inefficient since the temporarily-selected region
   --  will force stuff to be recalculated. This function moves them as a unit,
   --  which can be optimized.

   --  gtk_text_buffer_create_tag not bound: variable number of arguments
   --  ??? Discuss this with the Gtk+ team.
   --  equivalent to Gtk_New + Gtk.Text_Tag.Table_Add

   procedure Apply_Tag
     (Buffer  : access Gtk_Text_Buffer_Record;
      Tag     : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
      Start   : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Emit the "apply_tag" signal on Buffer.
   --  The default handler for the signal applies Tag to the given range.
   --  Start and End do not have to be in order.

   procedure Remove_Tag
     (Buffer  : access Gtk_Text_Buffer_Record;
      Tag     : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
      Start   : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Emit the "remove_tag" signal.
   --  The default handler for the signal removes all occurrences of Tag from
   --  the given range. Start and End don't have to be in order.

   procedure Apply_Tag_By_Name
     (Buffer  : access Gtk_Text_Buffer_Record;
      Name    : String;
      Start   : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Call Gtk.Text_Tag_Table.Lookup on the buffer's tag table to
   --  get a Gtk_Text_Tag, then call Apply_Tag.

   procedure Remove_Tag_By_Name
     (Buffer  : access Gtk_Text_Buffer_Record;
      Name    : String;
      Start   : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Call Gtk.Text_Tag_Table.Lookup on the buffer's tag table to
   --  get a Gtk_Text_Tag, then call Remove_Tag.

   procedure Get_Iter_At_Line_Offset
     (Buffer      : access Gtk_Text_Buffer_Record;
      Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
      Line_Number : Gint;
      Char_Offset : Gint := 0);
   --  Obtain an iterator pointing to Char_Offset within the given
   --  line. The Char_Offset must exist, offsets off the end of the line
   --  are not allowed. Note characters, not bytes;
   --  UTF-8 may encode one character as multiple bytes.

   procedure Get_Iter_At_Line_Index
     (Buffer      : access Gtk_Text_Buffer_Record;
      Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
      Line_Number : Gint;
      Byte_Index  : Gint := 0);
   --  Obtain an iterator pointing to Byte_Index within the given line.
   --  Byte_Index must be the start of a UTF-8 character, and must not be
   --  beyond the end of the line. Note bytes, not characters; UTF-8 may encode
   --  one character as multiple bytes.

   procedure Get_Iter_At_Offset
     (Buffer      : access Gtk_Text_Buffer_Record;
      Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
      Char_Offset : Gint);
   --  Initialize Iter to a position Char_Offset chars from the start of the
   --  entire buffer.
   --  Char_Offset: char offset from start of buffer, counting from 0.

   procedure Get_Iter_At_Line
     (Buffer      : access Gtk_Text_Buffer_Record;
      Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
      Line_Number : Gint);
   --  Initialize Iter to the start of the given line.
   --  Line_Number: line number counting from 0.

   procedure Get_End_Iter
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : out Gtk.Text_Iter.Gtk_Text_Iter);
   --  Initialize Iter with the "end iterator", one past the last valid
   --  character in the text buffer. If dereferenced with
   --  Gtk.Text_Iter.Get_Char, the end iterator has a character value of 0.
   --  The entire buffer lies in the range from the first position in the
   --  buffer (call Get_Iter_At_Offset to get character position 0) to the end
   --  iterator.

   procedure Get_Bounds
     (Buffer  : access Gtk_Text_Buffer_Record;
      Start   : out Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : out Gtk.Text_Iter.Gtk_Text_Iter);
   --  Retrieve the first and last iterators in the buffer.
   --  The entire buffer lies within the range [Start,End).

   procedure Get_Iter_At_Mark
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : out Gtk.Text_Iter.Gtk_Text_Iter;
      Mark   : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);
   --  Initialize Iter with the current position of Mark.

   procedure Get_Iter_At_Child_Anchor
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : out Gtk.Text_Iter.Gtk_Text_Iter;
      Anchor : access Gtk.Text_Child.Gtk_Text_Child_Anchor_Record'Class);
   --  ???

   function Get_Modified
     (Buffer : access Gtk_Text_Buffer_Record) return Boolean;
   --  Whether the buffer has been modified since the last call
   --  to Set_Modified set the modification flag to False. Used for example to
   --  enable a "save" function in a text editor.

   procedure Set_Modified
     (Buffer  : access Gtk_Text_Buffer_Record;
      Setting : Boolean := True);
   --  Used to keep track of whether the buffer has been modified since the
   --  last time it was saved. Whenever the buffer is saved to disk, call
   --  Set_Modified (Buffer, False). When the buffer is modified,
   --  it will automatically toggled on the modified bit again. When the
   --  modified bit flips, the buffer emits a "modified_changed" signal.

   procedure Paste_Primary
     (Buffer            : access Gtk_Text_Buffer_Record;
      Override_Location : Gtk.Text_Iter.Gtk_Text_Iter;
      Default_Editable  : Boolean);
   --  Paste the primary selection at the insertion point, or at
   --  Override_Location.
   --  (Note: pasting is asynchronous, that is, we'll ask for the paste data
   --  and return, and at some point later after the main loop runs, the paste
   --  data will be inserted.)

   procedure Cut_Clipboard
     (Buffer           : access Gtk_Text_Buffer_Record;
      Default_Editable : Boolean);
   --  Copy the currently-selected text to the clipboard, then delete
   --  said text if it's editable.

   procedure Copy_Clipboard (Buffer : access Gtk_Text_Buffer_Record);
   --  Copy the currently-selected text to the clipboard.

   procedure Paste_Clipboard
     (Buffer           : access Gtk_Text_Buffer_Record;
      Default_Editable : Boolean);
   --  Paste the clipboard contents at the insertion point.
   --  (Note: pasting is asynchronous, that is, we'll ask for the paste data
   --  and return, and at some point later after the main loop runs, the paste
   --  data will be inserted.)

   procedure Get_Selection_Bounds
     (Buffer  : access Gtk_Text_Buffer_Record;
      Start   : out Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : out Gtk.Text_Iter.Gtk_Text_Iter;
      Result  : out Boolean);
   --  Place the bounds of the selection in Start and End.
   --  If the selection has length 0, then Start and End are filled in with the
   --  same value. Start and End will be in ascending order. If Start and End
   --  are null, then they are not filled in, but the return value still
   --  indicates whether text is selected.
   --  Result: whether the selection has nonzero length.

   function Delete_Selection
     (Buffer           : access Gtk_Text_Buffer_Record;
      Interactive      : Boolean;
      Default_Editable : Boolean) return Boolean;
   --  Delete the range between the "insert" and "selection_bound" marks,
   --  that is, the currently-selected text. If Interactive is True,
   --  the editability of the selection will be considered (users can't delete
   --  uneditable text).
   --  Return value: whether there was a non-empty selection to delete.

   procedure Begin_User_Action (Buffer : access Gtk_Text_Buffer_Record);
   --  Called to indicate that the buffer operations between here and a
   --  call to End_User_Action are part of a single user-visible operation.
   --  The operations between Begin_User_Action and End_User_Action can then be
   --  grouped when creating an undo stack. Gtk_Text_Buffer maintains a count
   --  of calls to Begin_User_Action that have not been closed with a call to
   --  End_User_Action, and emits the "begin_user_action" and "end_user_action"
   --  signals only for the outermost pair of calls.
   --  This allows you to build user actions from other user actions.
   --
   --  The "interactive" buffer mutation functions, such as Insert_Interactive,
   --  automatically call begin/end user action around the buffer operations
   --  they perform, so there's no need to add extra calls if your user action
   --  consists solely of a single call to one of those functions.

   procedure End_User_Action (Buffer : access Gtk_Text_Buffer_Record);
   --  Should be paired with a call to Begin_User_Action.
   --  See that function for a full explanation.

   function Get_Buffer (Iter : Gtk.Text_Iter.Gtk_Text_Iter)
                        return Gtk_Text_Buffer;
   --  Return the buffer associated to the given Gtk_Text_Iterator.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "insert_text"
   --    procedure Handler
   --      (Widget : access Gtk_Text_Buffer_Record'Class;
   --       Pos    : Gtk.Text_Iter.Gtk_Text_Iter;
   --       Text   : String;
   --       Length : Gint);
   --
   --  - "insert_pixbuf"
   --    procedure Handler
   --      (Widget : access Gtk_Text_Buffer_Record'Class;
   --       Pos    : Gtk.Text_Iter.Gtk_Text_Iter;
   --       Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf);
   --
   --  - "insert_child_anchor"
   --    procedure Handler
   --      (Widget : access Gtk_Text_Buffer_Record'Class;
   --       Pos    : Gtk.Text_Iter.Gtk_Text_Iter;
   --       Anchor : access Gtk.Text_Child.Gtk_Text_Child_Anchor_Record'Class);
   --
   --  - "delete_range"
   --    procedure Handler
   --      (Widget  : access Gtk_Text_Buffer_Record'Class;
   --       Start   : Gtk.Text_Iter.Gtk_Text_Iter;
   --       The_End : Gtk.Text_Iter.Gtk_Text_Iter);
   --
   --  - "changed"
   --    procedure Handler (Widget : access Gtk_Text_Buffer_Record'Class);
   --
   --  - "modified_changed"
   --    procedure Handler (Widget : access Gtk_Text_Buffer_Record'Class);
   --
   --  - "mark_set"
   --    procedure Handler
   --      (Widget   : access Gtk_Text_Buffer_Record'Class;
   --       Location : Gtk.Text_Iter.Gtk_Text_Iter;
   --       Mark     : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);
   --
   --  - "mark_deleted"
   --    procedure Handler
   --      (Widget : access Gtk_Text_Buffer_Record'Class;
   --       Mark   : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);
   --
   --  - "apply_tag"
   --    procedure Handler
   --      (Widget     : access Gtk_Text_Buffer_Record'Class;
   --       Tag        : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
   --       Start_Char : Gtk.Text_Iter.Gtk_Text_Iter;
   --       End_Char   : Gtk.Text_Iter.Gtk_Text_Iter);
   --
   --  - "remove_tag"
   --    procedure Handler
   --      (Widget     : access Gtk_Text_Buffer_Record'Class;
   --       Tag        : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
   --       Start_Char : Gtk.Text_Iter.Gtk_Text_Iter;
   --       End_Char   : Gtk.Text_Iter.Gtk_Text_Iter);
   --
   --  - "begin_user_action"
   --    procedure Handler (Widget : access Gtk_Text_Buffer_Record'Class);
   --
   --  - "end_user_action"
   --    procedure Handler (Widget : access Gtk_Text_Buffer_Record'Class);
   --
   --  </signals>

private

   type Gtk_Text_Buffer_Record is new GObject_Record with null record;

   pragma Import (C, Get_Type, "gtk_text_buffer_get_type");

end Gtk.Text_Buffer;
