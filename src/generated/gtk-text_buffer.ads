------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Stores text and attributes for display in a `GtkTextView`.
--
--  You may wish to begin by reading the [text widget conceptual
--  overview](section-text-widget.html), which gives an overview of all the
--  objects and data types related to the text widget and how they work
--  together.
--
--  GtkTextBuffer can support undoing changes to the buffer content, see
--  [methodGtk.TextBuffer.set_enable_undo].

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Clipboard;         use Gdk.Clipboard;
with Gdk.Content_Provider;  use Gdk.Content_Provider;
with Gdk.Paintable;         use Gdk.Paintable;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Gtk.Text_Child_Anchor; use Gtk.Text_Child_Anchor;
with Gtk.Text_Iter;         use Gtk.Text_Iter;
with Gtk.Text_Mark;         use Gtk.Text_Mark;
with Gtk.Text_Tag;          use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;    use Gtk.Text_Tag_Table;
with Gtkada.Types;          use Gtkada.Types;

package Gtk.Text_Buffer is

   type Gtk_Text_Buffer_Record is new GObject_Record with null record;
   type Gtk_Text_Buffer is access all Gtk_Text_Buffer_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Buffer : out Gtk_Text_Buffer;
       Table  : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table := null);
   procedure Initialize
      (Buffer : not null access Gtk_Text_Buffer_Record'Class;
       Table  : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table := null);
   --  Creates a new text buffer.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Table a tag table, or null to create a new one

   function Gtk_Text_Buffer_New
      (Table : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table := null)
       return Gtk_Text_Buffer;
   --  Creates a new text buffer.
   --  @param Table a tag table, or null to create a new one

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_text_buffer_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Mark
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Mark   : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class;
       Where  : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Adds the mark at position Where.
   --  The mark must not be added to another buffer, and if its name is not
   --  null then there must not be another mark in the buffer with the same
   --  name.
   --  Emits the [signalGtk.TextBuffer::mark-set] signal as notification of
   --  the mark's initial placement.
   --  @param Mark the mark to add
   --  @param Where location to place mark

   procedure Add_Selection_Clipboard
      (Buffer    : not null access Gtk_Text_Buffer_Record;
       Clipboard : not null access Gdk.Clipboard.Gdk_Clipboard_Record'Class);
   --  Adds Clipboard to the list of clipboards in which the selection
   --  contents of Buffer are available.
   --  In most cases, Clipboard will be the `GdkClipboard` returned by
   --  [methodGtk.Widget.get_primary_clipboard] for a view of Buffer.
   --  @param Clipboard a `GdkClipboard`

   procedure Apply_Tag
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Tag     : not null access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
       Start   : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Emits the "apply-tag" signal on Buffer.
   --  The default handler for the signal applies Tag to the given range.
   --  Start and End do not have to be in order.
   --  @param Tag a `GtkTextTag`
   --  @param Start one bound of range to be tagged
   --  @param The_End other bound of range to be tagged

   procedure Apply_Tag_By_Name
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Name    : UTF8_String;
       Start   : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Emits the "apply-tag" signal on Buffer.
   --  Calls [methodGtk.TextTagTable.lookup] on the buffer's tag table to get
   --  a `GtkTextTag`, then calls [methodGtk.TextBuffer.apply_tag].
   --  @param Name name of a named `GtkTextTag`
   --  @param Start one bound of range to be tagged
   --  @param The_End other bound of range to be tagged

   procedure Backspace
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Iter             : in out Gtk.Text_Iter.Gtk_Text_Iter;
       Interactive      : Boolean;
       Default_Editable : Boolean := True;
       Result           : out Boolean);
   --  Performs the appropriate action as if the user hit the delete key with
   --  the cursor at the position specified by Iter.
   --  In the normal case a single character will be deleted, but when
   --  combining accents are involved, more than one character can be deleted,
   --  and when precomposed character and accent combinations are involved,
   --  less than one character will be deleted.
   --  Because the buffer is modified, all outstanding iterators become
   --  invalid after calling this function; however, the Iter will be
   --  re-initialized to point to the location where text was deleted.
   --  @param Iter a position in Buffer
   --  @param Interactive whether the deletion is caused by user interaction
   --  @param Default_Editable whether the buffer is editable by default
   --  @return True if the buffer was modified

   procedure Begin_Irreversible_Action
      (Buffer : not null access Gtk_Text_Buffer_Record);
   --  Denotes the beginning of an action that may not be undone.
   --  This will cause any previous operations in the undo/redo queue to be
   --  cleared.
   --  This should be paired with a call to
   --  [methodGtk.TextBuffer.end_irreversible_action] after the irreversible
   --  action has completed.
   --  You may nest calls to Gtk.Text_Buffer.Begin_Irreversible_Action and
   --  Gtk.Text_Buffer.End_Irreversible_Action pairs.

   procedure Begin_User_Action
      (Buffer : not null access Gtk_Text_Buffer_Record);
   --  Called to indicate that the buffer operations between here and a call
   --  to Gtk.Text_Buffer.End_User_Action are part of a single user-visible
   --  operation.
   --  The operations between Gtk.Text_Buffer.Begin_User_Action and
   --  Gtk.Text_Buffer.End_User_Action can then be grouped when creating an
   --  undo stack. `GtkTextBuffer` maintains a count of calls to
   --  Gtk.Text_Buffer.Begin_User_Action that have not been closed with a call
   --  to Gtk.Text_Buffer.End_User_Action, and emits the "begin-user-action"
   --  and "end-user-action" signals only for the outermost pair of calls. This
   --  allows you to build user actions from other user actions.
   --  The "interactive" buffer mutation functions, such as
   --  [methodGtk.TextBuffer.insert_interactive], automatically call begin/end
   --  user action around the buffer operations they perform, so there's no
   --  need to add extra calls if you user action consists solely of a single
   --  call to one of those functions.

   procedure Copy_Clipboard
      (Buffer    : not null access Gtk_Text_Buffer_Record;
       Clipboard : not null access Gdk.Clipboard.Gdk_Clipboard_Record'Class);
   --  Copies the currently-selected text to a clipboard.
   --  @param Clipboard the `GdkClipboard` object to copy to

   function Create_Child_Anchor
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : Gtk.Text_Iter.Gtk_Text_Iter)
       return Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor;
   --  Creates and inserts a child anchor.
   --  This is a convenience function which simply creates a child anchor with
   --  [ctorGtk.TextChildAnchor.new] and inserts it into the buffer with
   --  [methodGtk.TextBuffer.insert_child_anchor].
   --  The new anchor is owned by the buffer; no reference count is returned
   --  to the caller of this function.
   --  @param Iter location in the buffer
   --  @return the created child anchor

   function Create_Mark
      (Buffer       : not null access Gtk_Text_Buffer_Record;
       Mark_Name    : UTF8_String := "";
       Where        : Gtk.Text_Iter.Gtk_Text_Iter;
       Left_Gravity : Boolean := True) return Gtk.Text_Mark.Gtk_Text_Mark;
   --  Creates a mark at position Where.
   --  If Mark_Name is null, the mark is anonymous; otherwise, the mark can be
   --  retrieved by name using [methodGtk.TextBuffer.get_mark]. If a mark has
   --  left gravity, and text is inserted at the mark's current location, the
   --  mark will be moved to the left of the newly-inserted text. If the mark
   --  has right gravity (Left_Gravity = False), the mark will end up on the
   --  right of newly-inserted text. The standard left-to-right cursor is a
   --  mark with right gravity (when you type, the cursor stays on the right
   --  side of the text you're typing).
   --  The caller of this function does not own a reference to the returned
   --  `GtkTextMark`, so you can ignore the return value if you like. Marks are
   --  owned by the buffer and go away when the buffer does.
   --  Emits the [signalGtk.TextBuffer::mark-set] signal as notification of
   --  the mark's initial placement.
   --  @param Mark_Name name for mark
   --  @param Where location to place mark
   --  @param Left_Gravity whether the mark has left gravity
   --  @return the new `GtkTextMark` object

   procedure Cut_Clipboard
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Clipboard        : not null access Gdk.Clipboard.Gdk_Clipboard_Record'Class;
       Default_Editable : Boolean := True);
   --  Copies the currently-selected text to a clipboard, then deletes said
   --  text if it's editable.
   --  @param Clipboard the `GdkClipboard` object to cut to
   --  @param Default_Editable default editability of the buffer

   procedure Delete
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Start   : in out Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : in out Gtk.Text_Iter.Gtk_Text_Iter);
   --  Deletes text between Start and End.
   --  The order of Start and End is not actually relevant;
   --  Gtk.Text_Buffer.Delete will reorder them.
   --  This function actually emits the "delete-range" signal, and the default
   --  handler of that signal deletes the text. Because the buffer is modified,
   --  all outstanding iterators become invalid after calling this function;
   --  however, the Start and End will be re-initialized to point to the
   --  location where text was deleted.
   --  @param Start a position in Buffer
   --  @param The_End another position in Buffer

   procedure Delete_Interactive
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Start_Iter       : in out Gtk.Text_Iter.Gtk_Text_Iter;
       End_Iter         : in out Gtk.Text_Iter.Gtk_Text_Iter;
       Default_Editable : Boolean := True;
       Result           : out Boolean);
   --  Deletes all editable text in the given range.
   --  Calls [methodGtk.TextBuffer.delete] for each editable sub-range of
   --  [Start,End). Start and End are revalidated to point to the location of
   --  the last deleted range, or left untouched if no text was deleted.
   --  @param Start_Iter start of range to delete
   --  @param End_Iter end of range
   --  @param Default_Editable whether the buffer is editable by default
   --  @return whether some text was actually deleted

   procedure Delete_Mark
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Mark   : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);
   --  Deletes Mark, so that it's no longer located anywhere in the buffer.
   --  Removes the reference the buffer holds to the mark, so if you haven't
   --  called g_object_ref on the mark, it will be freed. Even if the mark
   --  isn't freed, most operations on Mark become invalid, until it gets added
   --  to a buffer again with [methodGtk.TextBuffer.add_mark]. Use
   --  [methodGtk.TextMark.get_deleted] to find out if a mark has been removed
   --  from its buffer.
   --  The [signalGtk.TextBuffer::mark-deleted] signal will be emitted as
   --  notification after the mark is deleted.
   --  @param Mark a `GtkTextMark` in Buffer

   procedure Delete_Mark_By_Name
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Name   : UTF8_String);
   --  Deletes the mark named Name; the mark must exist.
   --  See [methodGtk.TextBuffer.delete_mark] for details.
   --  @param Name name of a mark in Buffer

   function Delete_Selection
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Interactive      : Boolean;
       Default_Editable : Boolean := True) return Boolean;
   --  Deletes the range between the "insert" and "selection_bound" marks,
   --  that is, the currently-selected text.
   --  If Interactive is True, the editability of the selection will be
   --  considered (users can't delete uneditable text).
   --  @param Interactive whether the deletion is caused by user interaction
   --  @param Default_Editable whether the buffer is editable by default
   --  @return whether there was a non-empty selection to delete

   procedure End_Irreversible_Action
      (Buffer : not null access Gtk_Text_Buffer_Record);
   --  Denotes the end of an action that may not be undone.
   --  This will cause any previous operations in the undo/redo queue to be
   --  cleared.
   --  This should be called after completing modifications to the text buffer
   --  after [methodGtk.TextBuffer.begin_irreversible_action] was called.
   --  You may nest calls to Gtk.Text_Buffer.Begin_Irreversible_Action and
   --  Gtk.Text_Buffer.End_Irreversible_Action pairs.

   procedure End_User_Action
      (Buffer : not null access Gtk_Text_Buffer_Record);
   --  Ends a user-visible operation.
   --  Should be paired with a call to
   --  [methodGtk.TextBuffer.begin_user_action]. See that function for a full
   --  explanation.

   procedure Get_Bounds
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Start   : out Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : out Gtk.Text_Iter.Gtk_Text_Iter);
   --  Retrieves the first and last iterators in the buffer, i.e. the entire
   --  buffer lies within the range [Start,End).
   --  @param Start iterator to initialize with first position in the buffer
   --  @param The_End iterator to initialize with the end iterator

   function Get_Can_Redo
      (Buffer : not null access Gtk_Text_Buffer_Record) return Boolean;
   --  Gets whether there is a redoable action in the history.
   --  @return True if there is a redoable action

   function Get_Can_Undo
      (Buffer : not null access Gtk_Text_Buffer_Record) return Boolean;
   --  Gets whether there is an undoable action in the history.
   --  @return True if there is an undoable action

   function Get_Char_Count
      (Buffer : not null access Gtk_Text_Buffer_Record) return Glib.Gint;
   --  Gets the number of characters in the buffer.
   --  Note that characters and bytes are not the same, you can't e.g. expect
   --  the contents of the buffer in string form to be this many bytes long.
   --  The character count is cached, so this function is very fast.
   --  @return number of characters in the buffer

   function Get_Enable_Undo
      (Buffer : not null access Gtk_Text_Buffer_Record) return Boolean;
   --  Gets whether the buffer is saving modifications to the buffer to allow
   --  for undo and redo actions.
   --  See [methodGtk.TextBuffer.begin_irreversible_action] and
   --  [methodGtk.TextBuffer.end_irreversible_action] to create changes to the
   --  buffer that cannot be undone.
   --  @return True if undoing and redoing changes to the buffer is allowed.

   procedure Set_Enable_Undo
      (Buffer      : not null access Gtk_Text_Buffer_Record;
       Enable_Undo : Boolean);
   --  Sets whether or not to enable undoable actions in the text buffer.
   --  Undoable actions in this context are changes to the text content of the
   --  buffer. Changes to tags and marks are not tracked.
   --  If enabled, the user will be able to undo the last number of actions up
   --  to [methodGtk.TextBuffer.get_max_undo_levels].
   --  See [methodGtk.TextBuffer.begin_irreversible_action] and
   --  [methodGtk.TextBuffer.end_irreversible_action] to create changes to the
   --  buffer that cannot be undone.
   --  @param Enable_Undo True to enable undo

   procedure Get_End_Iter
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : out Gtk.Text_Iter.Gtk_Text_Iter);
   --  Initializes Iter with the "end iterator," one past the last valid
   --  character in the text buffer.
   --  If dereferenced with [methodGtk.TextIter.get_char], the end iterator
   --  has a character value of 0. The entire buffer lies in the range from the
   --  first position in the buffer (call [methodGtk.TextBuffer.get_start_iter]
   --  to get character position 0) to the end iterator.
   --  @param Iter iterator to initialize

   function Get_Has_Selection
      (Buffer : not null access Gtk_Text_Buffer_Record) return Boolean;
   --  Indicates whether the buffer has some text currently selected.
   --  @return True if the there is text selected

   function Get_Insert
      (Buffer : not null access Gtk_Text_Buffer_Record)
       return Gtk.Text_Mark.Gtk_Text_Mark;
   --  Returns the mark that represents the cursor (insertion point).
   --  Equivalent to calling [methodGtk.TextBuffer.get_mark] to get the mark
   --  named "insert", but very slightly more efficient, and involves less
   --  typing.
   --  @return insertion point mark

   procedure Insert
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
       Text   : UTF8_String);
   --  Inserts Len bytes of Text at position Iter.
   --  If Len is -1, Text must be nul-terminated and will be inserted in its
   --  entirety. Emits the "insert-text" signal; insertion actually occurs in
   --  the default handler for the signal. Iter is invalidated when insertion
   --  occurs (because the buffer contents change), but the default signal
   --  handler revalidates it to point to the end of the inserted text.
   --  @param Iter a position in the buffer
   --  @param Text text in UTF-8 format

   procedure Get_Iter_At_Child_Anchor
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : out Gtk.Text_Iter.Gtk_Text_Iter;
       Anchor : not null access Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor_Record'Class);
   --  Obtains the location of Anchor within Buffer.
   --  @param Iter an iterator to be initialized
   --  @param Anchor a child anchor that appears in Buffer

   function Get_Iter_At_Line
      (Buffer      : not null access Gtk_Text_Buffer_Record;
       Iter        : access Gtk.Text_Iter.Gtk_Text_Iter;
       Line_Number : Glib.Gint) return Boolean;
   --  Initializes Iter to the start of the given line.
   --  If Line_Number is greater than or equal to the number of lines in the
   --  Buffer, the end iterator is returned.
   --  @param Iter iterator to initialize
   --  @param Line_Number line number counting from 0
   --  @return whether the exact position has been found

   function Get_Iter_At_Line_Index
      (Buffer      : not null access Gtk_Text_Buffer_Record;
       Iter        : access Gtk.Text_Iter.Gtk_Text_Iter;
       Line_Number : Glib.Gint;
       Byte_Index  : Glib.Gint) return Boolean;
   --  Obtains an iterator pointing to Byte_Index within the given line.
   --  Byte_Index must be the start of a UTF-8 character. Note bytes, not
   --  characters; UTF-8 may encode one character as multiple bytes.
   --  If Line_Number is greater than or equal to the number of lines in the
   --  Buffer, the end iterator is returned. And if Byte_Index is off the end
   --  of the line, the iterator at the end of the line is returned.
   --  @param Iter iterator to initialize
   --  @param Line_Number line number counting from 0
   --  @param Byte_Index byte index from start of line
   --  @return whether the exact position has been found

   function Get_Iter_At_Line_Offset
      (Buffer      : not null access Gtk_Text_Buffer_Record;
       Iter        : access Gtk.Text_Iter.Gtk_Text_Iter;
       Line_Number : Glib.Gint;
       Char_Offset : Glib.Gint) return Boolean;
   --  Obtains an iterator pointing to Char_Offset within the given line.
   --  Note characters, not bytes; UTF-8 may encode one character as multiple
   --  bytes.
   --  If Line_Number is greater than or equal to the number of lines in the
   --  Buffer, the end iterator is returned. And if Char_Offset is off the end
   --  of the line, the iterator at the end of the line is returned.
   --  @param Iter iterator to initialize
   --  @param Line_Number line number counting from 0
   --  @param Char_Offset char offset from start of line
   --  @return whether the exact position has been found

   procedure Get_Iter_At_Mark
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : out Gtk.Text_Iter.Gtk_Text_Iter;
       Mark   : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);
   --  Initializes Iter with the current position of Mark.
   --  @param Iter iterator to initialize
   --  @param Mark a `GtkTextMark` in Buffer

   procedure Get_Iter_At_Offset
      (Buffer      : not null access Gtk_Text_Buffer_Record;
       Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
       Char_Offset : Glib.Gint);
   --  Initializes Iter to a position Char_Offset chars from the start of the
   --  entire buffer.
   --  If Char_Offset is -1 or greater than the number of characters in the
   --  buffer, Iter is initialized to the end iterator, the iterator one past
   --  the last valid character in the buffer.
   --  @param Iter iterator to initialize
   --  @param Char_Offset char offset from start of buffer, counting from 0,
   --  or -1

   function Get_Line_Count
      (Buffer : not null access Gtk_Text_Buffer_Record) return Glib.Gint;
   --  Obtains the number of lines in the buffer.
   --  This value is cached, so the function is very fast.
   --  @return number of lines in the buffer

   function Get_Mark
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Name   : UTF8_String) return Gtk.Text_Mark.Gtk_Text_Mark;
   --  Returns the mark named Name in buffer Buffer, or null if no such mark
   --  exists in the buffer.
   --  @param Name a mark name
   --  @return a `GtkTextMark`

   function Get_Max_Undo_Levels
      (Buffer : not null access Gtk_Text_Buffer_Record) return Guint;
   --  Gets the maximum number of undo levels to perform.
   --  If 0, unlimited undo actions may be performed. Note that this may have
   --  a memory usage impact as it requires storing an additional copy of the
   --  inserted or removed text within the text buffer.
   --  @return The max number of undo levels allowed (0 indicates unlimited).

   procedure Set_Max_Undo_Levels
      (Buffer          : not null access Gtk_Text_Buffer_Record;
       Max_Undo_Levels : Guint);
   --  Sets the maximum number of undo levels to perform.
   --  If 0, unlimited undo actions may be performed. Note that this may have
   --  a memory usage impact as it requires storing an additional copy of the
   --  inserted or removed text within the text buffer.
   --  @param Max_Undo_Levels the maximum number of undo actions to perform

   function Get_Modified
      (Buffer : not null access Gtk_Text_Buffer_Record) return Boolean;
   --  Indicates whether the buffer has been modified since the last call to
   --  [methodGtk.TextBuffer.set_modified] set the modification flag to False.
   --  Used for example to enable a "save" function in a text editor.
   --  @return True if the buffer has been modified

   procedure Set_Modified
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Setting : Boolean);
   --  Used to keep track of whether the buffer has been modified since the
   --  last time it was saved.
   --  Whenever the buffer is saved to disk, call
   --  `gtk_text_buffer_set_modified (Buffer, FALSE)`. When the buffer is
   --  modified, it will automatically toggle on the modified bit again. When
   --  the modified bit flips, the buffer emits the
   --  [signalGtk.TextBuffer::modified-changed] signal.
   --  @param Setting modification flag setting

   function Get_Selection_Bound
      (Buffer : not null access Gtk_Text_Buffer_Record)
       return Gtk.Text_Mark.Gtk_Text_Mark;
   --  Returns the mark that represents the selection bound.
   --  Equivalent to calling [methodGtk.TextBuffer.get_mark] to get the mark
   --  named "selection_bound", but very slightly more efficient, and involves
   --  less typing.
   --  The currently-selected text in Buffer is the region between the
   --  "selection_bound" and "insert" marks. If "selection_bound" and "insert"
   --  are in the same place, then there is no current selection.
   --  [methodGtk.TextBuffer.get_selection_bounds] is another convenient
   --  function for handling the selection, if you just want to know whether
   --  there's a selection and what its bounds are.
   --  @return selection bound mark

   procedure Get_Selection_Bounds
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Start   : out Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : out Gtk.Text_Iter.Gtk_Text_Iter;
       Result  : out Boolean);
   --  Returns True if some text is selected; places the bounds of the
   --  selection in Start and End.
   --  If the selection has length 0, then Start and End are filled in with
   --  the same value. Start and End will be in ascending order. If Start and
   --  End are null, then they are not filled in, but the return value still
   --  indicates whether text is selected.
   --  @param Start iterator to initialize with selection start
   --  @param The_End iterator to initialize with selection end
   --  @return whether the selection has nonzero length

   function Get_Selection_Content
      (Buffer : not null access Gtk_Text_Buffer_Record)
       return Gdk.Content_Provider.Gdk_Content_Provider;
   --  Get a content provider for this buffer.
   --  It can be used to make the content of Buffer available in a
   --  `GdkClipboard`, see [methodGdk.Clipboard.set_content].
   --  @return a new `GdkContentProvider`.

   function Get_Slice
      (Buffer               : not null access Gtk_Text_Buffer_Record;
       Start                : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
       Include_Hidden_Chars : Boolean := False) return UTF8_String;
   --  Returns the text in the range [Start,End).
   --  Excludes undisplayed text (text marked with tags that set the
   --  invisibility attribute) if Include_Hidden_Chars is False. The returned
   --  string includes a 0xFFFC character whenever the buffer contains embedded
   --  images, so byte and character indexes into the returned string do
   --  correspond to byte and character indexes into the buffer. Contrast with
   --  [methodGtk.TextBuffer.get_text]. Note that 0xFFFC can occur in normal
   --  text as well, so it is not a reliable indicator that a paintable or
   --  widget is in the buffer.
   --  @param Start start of a range
   --  @param The_End end of a range
   --  @param Include_Hidden_Chars whether to include invisible text
   --  @return an allocated UTF-8 string

   procedure Get_Start_Iter
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : out Gtk.Text_Iter.Gtk_Text_Iter);
   --  Initialized Iter with the first position in the text buffer.
   --  This is the same as using [methodGtk.TextBuffer.get_iter_at_offset] to
   --  get the iter at character offset 0.
   --  @param Iter iterator to initialize

   function Get_Tag_Table
      (Buffer : not null access Gtk_Text_Buffer_Record)
       return Gtk.Text_Tag_Table.Gtk_Text_Tag_Table;
   --  Get the `GtkTextTagTable` associated with this buffer.
   --  @return the buffer's tag table

   function Get_Text
      (Buffer               : not null access Gtk_Text_Buffer_Record;
       Start                : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
       Include_Hidden_Chars : Boolean := False) return UTF8_String;
   --  Returns the text in the range [Start,End).
   --  Excludes undisplayed text (text marked with tags that set the
   --  invisibility attribute) if Include_Hidden_Chars is False. Does not
   --  include characters representing embedded images, so byte and character
   --  indexes into the returned string do not correspond to byte and character
   --  indexes into the buffer. Contrast with [methodGtk.TextBuffer.get_slice].
   --  @param Start start of a range
   --  @param The_End end of a range
   --  @param Include_Hidden_Chars whether to include invisible text
   --  @return an allocated UTF-8 string

   procedure Set_Text
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Text   : UTF8_String);
   --  Deletes current contents of Buffer, and inserts Text instead. This is
   --  automatically marked as an irreversible action in the undo stack. If you
   --  wish to mark this action as part of a larger undo operation, call
   --  [methodTextbuffer.delete] and [methodTextbuffer.insert] directly
   --  instead.
   --  If Len is -1, Text must be nul-terminated. Text must be valid UTF-8.
   --  @param Text UTF-8 text to insert

   procedure Insert_At_Cursor
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Text   : UTF8_String);
   --  Inserts Text in Buffer.
   --  Simply calls [methodGtk.TextBuffer.insert], using the current cursor
   --  position as the insertion point.
   --  @param Text text in UTF-8 format

   procedure Insert_Child_Anchor
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
       Anchor : not null access Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor_Record'Class);
   --  Inserts a child widget anchor into the text buffer at Iter.
   --  The anchor will be counted as one character in character counts, and
   --  when obtaining the buffer contents as a string, will be represented by
   --  the Unicode "object replacement character" 0xFFFC. Note that the "slice"
   --  variants for obtaining portions of the buffer as a string include this
   --  character for child anchors, but the "text" variants do not. E.g. see
   --  [methodGtk.TextBuffer.get_slice] and [methodGtk.TextBuffer.get_text].
   --  Consider [methodGtk.TextBuffer.create_child_anchor] as a more
   --  convenient alternative to this function. The buffer will add a reference
   --  to the anchor, so you can unref it after insertion.
   --  @param Iter location to insert the anchor
   --  @param Anchor a `GtkTextChildAnchor`

   function Insert_Interactive
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Iter             : access Gtk.Text_Iter.Gtk_Text_Iter;
       Text             : UTF8_String;
       Default_Editable : Boolean := True) return Boolean;
   --  Inserts Text in Buffer.
   --  Like [methodGtk.TextBuffer.insert], but the insertion will not occur if
   --  Iter is at a non-editable location in the buffer. Usually you want to
   --  prevent insertions at ineditable locations if the insertion results from
   --  a user action (is interactive).
   --  Default_Editable indicates the editability of text that doesn't have a
   --  tag affecting editability applied to it. Typically the result of
   --  [methodGtk.TextView.get_editable] is appropriate here.
   --  @param Iter a position in Buffer
   --  @param Text some UTF-8 text
   --  @param Default_Editable default editability of buffer
   --  @return whether text was actually inserted

   function Insert_Interactive_At_Cursor
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Text             : UTF8_String;
       Default_Editable : Boolean := True) return Boolean;
   --  Inserts Text in Buffer.
   --  Calls [methodGtk.TextBuffer.insert_interactive] at the cursor position.
   --  Default_Editable indicates the editability of text that doesn't have a
   --  tag affecting editability applied to it. Typically the result of
   --  [methodGtk.TextView.get_editable] is appropriate here.
   --  @param Text text in UTF-8 format
   --  @param Default_Editable default editability of buffer
   --  @return whether text was actually inserted

   procedure Insert_Markup
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
       Markup : UTF8_String);
   --  Inserts the text in Markup at position Iter.
   --  Markup will be inserted in its entirety and must be nul-terminated and
   --  valid UTF-8. Emits the [signalGtk.TextBuffer::insert-text] signal,
   --  possibly multiple times; insertion actually occurs in the default
   --  handler for the signal. Iter will point to the end of the inserted text
   --  on return.
   --  @param Iter location to insert the markup
   --  @param Markup a nul-terminated UTF-8 string containing Pango markup

   procedure Insert_Paintable
      (Buffer    : not null access Gtk_Text_Buffer_Record;
       Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
       Paintable : Gdk.Paintable.Gdk_Paintable);
   --  Inserts an image into the text buffer at Iter.
   --  The image will be counted as one character in character counts, and
   --  when obtaining the buffer contents as a string, will be represented by
   --  the Unicode "object replacement character" 0xFFFC. Note that the "slice"
   --  variants for obtaining portions of the buffer as a string include this
   --  character for paintable, but the "text" variants do not. e.g. see
   --  [methodGtk.TextBuffer.get_slice] and [methodGtk.TextBuffer.get_text].
   --  @param Iter location to insert the paintable
   --  @param Paintable a `GdkPaintable`

   procedure Insert_Range
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Iter    : in out Gtk.Text_Iter.Gtk_Text_Iter;
       Start   : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Copies text, tags, and paintables between Start and End and inserts the
   --  copy at Iter.
   --  The order of Start and End doesn't matter.
   --  Used instead of simply getting/inserting text because it preserves
   --  images and tags. If Start and End are in a different buffer from Buffer,
   --  the two buffers must share the same tag table.
   --  Implemented via emissions of the ::insert-text and ::apply-tag signals,
   --  so expect those.
   --  @param Iter a position in Buffer
   --  @param Start a position in a `GtkTextBuffer`
   --  @param The_End another position in the same buffer as Start

   procedure Insert_Range_Interactive
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Iter             : in out Gtk.Text_Iter.Gtk_Text_Iter;
       Start            : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End          : Gtk.Text_Iter.Gtk_Text_Iter;
       Default_Editable : Boolean := True;
       Result           : out Boolean);
   --  Copies text, tags, and paintables between Start and End and inserts the
   --  copy at Iter.
   --  Same as [methodGtk.TextBuffer.insert_range], but does nothing if the
   --  insertion point isn't editable. The Default_Editable parameter indicates
   --  whether the text is editable at Iter if no tags enclosing Iter affect
   --  editability. Typically the result of [methodGtk.TextView.get_editable]
   --  is appropriate here.
   --  @param Iter a position in Buffer
   --  @param Start a position in a `GtkTextBuffer`
   --  @param The_End another position in the same buffer as Start
   --  @param Default_Editable default editability of the buffer
   --  @return whether an insertion was possible at Iter

   procedure Move_Mark
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Mark   : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class;
       Where  : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Moves Mark to the new location Where.
   --  Emits the [signalGtk.TextBuffer::mark-set] signal as notification of
   --  the move.
   --  @param Mark a `GtkTextMark`
   --  @param Where new location for Mark in Buffer

   procedure Move_Mark_By_Name
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Name   : UTF8_String;
       Where  : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Moves the mark named Name (which must exist) to location Where.
   --  See [methodGtk.TextBuffer.move_mark] for details.
   --  @param Name name of a mark
   --  @param Where new location for mark

   procedure Paste_Clipboard
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Clipboard        : not null access Gdk.Clipboard.Gdk_Clipboard_Record'Class;
       Default_Editable : Boolean := True);
   --  Pastes the contents of a clipboard.
   --  If Override_Location is null, the pasted text will be inserted at the
   --  cursor position, or the buffer selection will be replaced if the
   --  selection is non-empty.
   --  Note: pasting is asynchronous, that is, we'll ask for the paste data
   --  and return, and at some point later after the main loop runs, the paste
   --  data will be inserted.
   --  @param Clipboard the `GdkClipboard` to paste from
   --  @param Default_Editable whether the buffer is editable by default

   procedure Place_Cursor
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Where  : Gtk.Text_Iter.Gtk_Text_Iter);
   --  This function moves the "insert" and "selection_bound" marks
   --  simultaneously.
   --  If you move them to the same place in two steps with
   --  [methodGtk.TextBuffer.move_mark], you will temporarily select a region
   --  in between their old and new locations, which can be pretty inefficient
   --  since the temporarily-selected region will force stuff to be
   --  recalculated. This function moves them as a unit, which can be
   --  optimized.
   --  @param Where where to put the cursor

   procedure Redo (Buffer : not null access Gtk_Text_Buffer_Record);
   --  Redoes the next redoable action on the buffer, if there is one.

   procedure Remove_All_Tags
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Start   : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Removes all tags in the range between Start and End.
   --  Be careful with this function; it could remove tags added in code
   --  unrelated to the code you're currently writing. That is, using this
   --  function is probably a bad idea if you have two or more unrelated code
   --  sections that add tags.
   --  @param Start one bound of range to be untagged
   --  @param The_End other bound of range to be untagged

   procedure Remove_Selection_Clipboard
      (Buffer    : not null access Gtk_Text_Buffer_Record;
       Clipboard : not null access Gdk.Clipboard.Gdk_Clipboard_Record'Class);
   --  Removes a `GdkClipboard` added with
   --  [methodGtk.TextBuffer.add_selection_clipboard]
   --  @param Clipboard a `GdkClipboard` added to Buffer by
   --  [methodGtk.TextBuffer.add_selection_clipboard]

   procedure Remove_Tag
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Tag     : not null access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
       Start   : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Emits the "remove-tag" signal.
   --  The default handler for the signal removes all occurrences of Tag from
   --  the given range. Start and End don't have to be in order.
   --  @param Tag a `GtkTextTag`
   --  @param Start one bound of range to be untagged
   --  @param The_End other bound of range to be untagged

   procedure Remove_Tag_By_Name
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Name    : UTF8_String;
       Start   : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Emits the "remove-tag" signal.
   --  Calls [methodGtk.TextTagTable.lookup] on the buffer's tag table to get
   --  a `GtkTextTag`, then calls [methodGtk.TextBuffer.remove_tag].
   --  @param Name name of a `GtkTextTag`
   --  @param Start one bound of range to be untagged
   --  @param The_End other bound of range to be untagged

   procedure Select_Range
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Ins    : Gtk.Text_Iter.Gtk_Text_Iter;
       Bound  : Gtk.Text_Iter.Gtk_Text_Iter);
   --  This function moves the "insert" and "selection_bound" marks
   --  simultaneously.
   --  If you move them in two steps with [methodGtk.TextBuffer.move_mark],
   --  you will temporarily select a region in between their old and new
   --  locations, which can be pretty inefficient since the
   --  temporarily-selected region will force stuff to be recalculated. This
   --  function moves them as a unit, which can be optimized.
   --  @param Ins where to put the "insert" mark
   --  @param Bound where to put the "selection_bound" mark

   procedure Undo (Buffer : not null access Gtk_Text_Buffer_Record);
   --  Undoes the last undoable action on the buffer, if there is one.

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Insert
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Text   : Gtkada.Types.Chars_Ptr);
   --  More efficient version of Insert, which doesn't require a string copy.

   procedure Insert_At_Cursor
     (Buffer : access Gtk_Text_Buffer_Record;
      Text   : Gtkada.Types.Chars_Ptr;
      Len    : Gint := -1);
   --  Call Buffer_Insert, using the current cursor position
   --  as the insertion point.
   --  Text: UTF-8 format C string to insert.

   function Get_Text
     (Buffer               : access Gtk_Text_Buffer_Record;
      Start                : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
      Include_Hidden_Chars : Boolean := False) return Gtkada.Types.Chars_Ptr;
   --  Same as Get_Text above, but return a pointer to a C string, for
   --  efficiency.
   --  The caller is responsible for freeing (using Gtkada.Types.g_free) the
   --  returned pointer.

   function Selection_Exists
     (Buffer : access Gtk_Text_Buffer_Record) return Boolean;
   --  Return True if some text in the buffer is currently selected.

   procedure Insert_With_Tags
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Text   : UTF8_String;
      Tag    : Gtk.Text_Tag.Gtk_Text_Tag);
   procedure Insert_With_Tags
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Text   : Gtkada.Types.Chars_Ptr;
      Tag    : Gtk.Text_Tag.Gtk_Text_Tag);
   --  Same as Insert, but specifies the tag to apply to the range.

   function Get_Buffer
     (Iter : Gtk.Text_Iter.Gtk_Text_Iter)
   return Gtk.Text_Buffer.Gtk_Text_Buffer;
   --  Returns the Gtk.Text_Buffer.Gtk_Text_Buffer this iterator is associated
   --  with.

   function Get_Buffer
     (Mark : Gtk.Text_Mark.Gtk_Text_Mark)
   return Gtk.Text_Buffer.Gtk_Text_Buffer;
   --  Gets the buffer this mark is located inside, or null if the mark is
   --  deleted.

   function Create_Tag
     (Buffer              : access Gtk_Text_Buffer_Record;
      Tag_Name            : String := "")
   return Gtk.Text_Tag.Gtk_Text_Tag;
   --  Creates a tag and adds it to the tag table for Buffer. Equivalent to
   --  calling gtk.text_tag.gtk_new and then adding the tag to the buffer's tag
   --  table. The returned tag is owned by the buffer's tag table, so the ref
   --  count will be equal to one.
   --
   --  If Tag_Name is empty, the tag is anonymous, otherwise a tag called
   --  Tag_Name must not already exist in the tag table for this buffer.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Can_Redo_Property : constant Glib.Properties.Property_Boolean;
   --  Denotes that the buffer can reapply the last undone action.

   Can_Undo_Property : constant Glib.Properties.Property_Boolean;
   --  Denotes that the buffer can undo the last applied action.

   Cursor_Position_Property : constant Glib.Properties.Property_Int;
   --  The position of the insert mark.
   --
   --  This is an offset from the beginning of the buffer. It is useful for
   --  getting notified when the cursor moves.

   Enable_Undo_Property : constant Glib.Properties.Property_Boolean;
   --  Denotes if support for undoing and redoing changes to the buffer is
   --  allowed.

   Has_Selection_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the buffer has some text currently selected.

   Tag_Table_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Text_Tag_Table.Gtk_Text_Tag_Table
   --  The GtkTextTagTable for the buffer.

   Text_Property : constant Glib.Properties.Property_String;
   --  The text content of the buffer.
   --
   --  Without child widgets and images, see [methodGtk.TextBuffer.get_text]
   --  for more information.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Text_Buffer_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void is not null access procedure
     (Self    : access Gtk_Text_Buffer_Record'Class;
      Tag     : not null access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
      Start   : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter);

   type Cb_GObject_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      Tag     : not null access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
      Start   : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter);

   Signal_Apply_Tag : constant Glib.Signal_Name := "apply-tag";
   procedure On_Apply_Tag
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       After : Boolean := False);
   procedure On_Apply_Tag
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to apply a tag to a range of text in a `GtkTextBuffer`.
   --
   --  Applying actually occurs in the default handler.
   --
   --  Note that if your handler runs before the default handler it must not
   --  invalidate the Start and End iters (or has to revalidate them).
   --
   --  See also: [methodGtk.TextBuffer.apply_tag],
   --  [methodGtk.TextBuffer.insert_with_tags],
   --  [methodGtk.TextBuffer.insert_range].
   -- 
   --  Callback parameters:
   --    --  @param Tag the applied tag
   --    --  @param Start the start of the range the tag is applied to
   --    --  @param The_End the end of the range the tag is applied to

   type Cb_Gtk_Text_Buffer_Void is not null access procedure
     (Self : access Gtk_Text_Buffer_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Begin_User_Action : constant Glib.Signal_Name := "begin-user-action";
   procedure On_Begin_User_Action
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Void;
       After : Boolean := False);
   procedure On_Begin_User_Action
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted at the beginning of a single user-visible operation on a
   --  `GtkTextBuffer`.
   --
   --  See also: [methodGtk.TextBuffer.begin_user_action],
   --  [methodGtk.TextBuffer.insert_interactive],
   --  [methodGtk.TextBuffer.insert_range_interactive],
   --  [methodGtk.TextBuffer.delete_interactive],
   --  [methodGtk.TextBuffer.backspace],
   --  [methodGtk.TextBuffer.delete_selection].

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the content of a `GtkTextBuffer` has changed.

   type Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Iter_Void is not null access procedure
     (Self    : access Gtk_Text_Buffer_Record'Class;
      Start   : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter);

   type Cb_GObject_Gtk_Text_Iter_Gtk_Text_Iter_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      Start   : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter);

   Signal_Delete_Range : constant Glib.Signal_Name := "delete-range";
   procedure On_Delete_Range
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       After : Boolean := False);
   procedure On_Delete_Range
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to delete a range from a `GtkTextBuffer`.
   --
   --  Note that if your handler runs before the default handler it must not
   --  invalidate the Start and End iters (or has to revalidate them). The
   --  default signal handler revalidates the Start and End iters to both point
   --  to the location where text was deleted. Handlers which run after the
   --  default handler (see g_signal_connect_after) do not have access to the
   --  deleted text.
   --
   --  See also: [methodGtk.TextBuffer.delete].
   -- 
   --  Callback parameters:
   --    --  @param Start the start of the range to be deleted
   --    --  @param The_End the end of the range to be deleted

   Signal_End_User_Action : constant Glib.Signal_Name := "end-user-action";
   procedure On_End_User_Action
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Void;
       After : Boolean := False);
   procedure On_End_User_Action
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted at the end of a single user-visible operation on the
   --  `GtkTextBuffer`.
   --
   --  See also: [methodGtk.TextBuffer.end_user_action],
   --  [methodGtk.TextBuffer.insert_interactive],
   --  [methodGtk.TextBuffer.insert_range_interactive],
   --  [methodGtk.TextBuffer.delete_interactive],
   --  [methodGtk.TextBuffer.backspace],
   --  [methodGtk.TextBuffer.delete_selection],
   --  [methodGtk.TextBuffer.backspace].

   type Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void is not null access procedure
     (Self     : access Gtk_Text_Buffer_Record'Class;
      Location : Gtk.Text_Iter.Gtk_Text_Iter;
      Anchor   : not null access Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor_Record'Class);

   type Cb_GObject_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Location : Gtk.Text_Iter.Gtk_Text_Iter;
      Anchor   : not null access Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor_Record'Class);

   Signal_Insert_Child_Anchor : constant Glib.Signal_Name := "insert-child-anchor";
   procedure On_Insert_Child_Anchor
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void;
       After : Boolean := False);
   procedure On_Insert_Child_Anchor
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to insert a `GtkTextChildAnchor` in a `GtkTextBuffer`.
   --
   --  Insertion actually occurs in the default handler.
   --
   --  Note that if your handler runs before the default handler it must not
   --  invalidate the Location iter (or has to revalidate it). The default
   --  signal handler revalidates it to be placed after the inserted Anchor.
   --
   --  See also: [methodGtk.TextBuffer.insert_child_anchor].
   -- 
   --  Callback parameters:
   --    --  @param Location position to insert Anchor in Textbuffer
   --    --  @param Anchor the `GtkTextChildAnchor` to be inserted

   type Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gdk_Paintable_Void is not null access procedure
     (Self      : access Gtk_Text_Buffer_Record'Class;
      Location  : Gtk.Text_Iter.Gtk_Text_Iter;
      Paintable : Gdk.Paintable.Gdk_Paintable);

   type Cb_GObject_Gtk_Text_Iter_Gdk_Paintable_Void is not null access procedure
     (Self      : access Glib.Object.GObject_Record'Class;
      Location  : Gtk.Text_Iter.Gtk_Text_Iter;
      Paintable : Gdk.Paintable.Gdk_Paintable);

   Signal_Insert_Paintable : constant Glib.Signal_Name := "insert-paintable";
   procedure On_Insert_Paintable
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gdk_Paintable_Void;
       After : Boolean := False);
   procedure On_Insert_Paintable
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gtk_Text_Iter_Gdk_Paintable_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to insert a `GdkPaintable` in a `GtkTextBuffer`.
   --
   --  Insertion actually occurs in the default handler.
   --
   --  Note that if your handler runs before the default handler it must not
   --  invalidate the Location iter (or has to revalidate it). The default
   --  signal handler revalidates it to be placed after the inserted Paintable.
   --
   --  See also: [methodGtk.TextBuffer.insert_paintable].
   -- 
   --  Callback parameters:
   --    --  @param Location position to insert Paintable in Textbuffer
   --    --  @param Paintable the `GdkPaintable` to be inserted

   type Cb_Gtk_Text_Buffer_Gtk_Text_Iter_UTF8_String_Gint_Void is not null access procedure
     (Self     : access Gtk_Text_Buffer_Record'Class;
      Location : Gtk.Text_Iter.Gtk_Text_Iter;
      Text     : UTF8_String;
      Len      : Glib.Gint);

   type Cb_GObject_Gtk_Text_Iter_UTF8_String_Gint_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Location : Gtk.Text_Iter.Gtk_Text_Iter;
      Text     : UTF8_String;
      Len      : Glib.Gint);

   Signal_Insert_Text : constant Glib.Signal_Name := "insert-text";
   procedure On_Insert_Text
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_UTF8_String_Gint_Void;
       After : Boolean := False);
   procedure On_Insert_Text
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gtk_Text_Iter_UTF8_String_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to insert text in a `GtkTextBuffer`.
   --
   --  Insertion actually occurs in the default handler.
   --
   --  Note that if your handler runs before the default handler it must not
   --  invalidate the Location iter (or has to revalidate it). The default
   --  signal handler revalidates it to point to the end of the inserted text.
   --
   --  See also: [methodGtk.TextBuffer.insert],
   --  [methodGtk.TextBuffer.insert_range].
   -- 
   --  Callback parameters:
   --    --  @param Location position to insert Text in Textbuffer
   --    --  @param Text the UTF-8 text to be inserted
   --    --  @param Len length of the inserted text in bytes

   type Cb_Gtk_Text_Buffer_Gtk_Text_Mark_Void is not null access procedure
     (Self : access Gtk_Text_Buffer_Record'Class;
      Mark : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);

   type Cb_GObject_Gtk_Text_Mark_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Mark : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);

   Signal_Mark_Deleted : constant Glib.Signal_Name := "mark-deleted";
   procedure On_Mark_Deleted
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gtk_Text_Mark_Void;
       After : Boolean := False);
   procedure On_Mark_Deleted
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gtk_Text_Mark_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted as notification after a `GtkTextMark` is deleted.
   --
   --  See also: [methodGtk.TextBuffer.delete_mark].

   type Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Mark_Void is not null access procedure
     (Self     : access Gtk_Text_Buffer_Record'Class;
      Location : Gtk.Text_Iter.Gtk_Text_Iter;
      Mark     : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);

   type Cb_GObject_Gtk_Text_Iter_Gtk_Text_Mark_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Location : Gtk.Text_Iter.Gtk_Text_Iter;
      Mark     : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);

   Signal_Mark_Set : constant Glib.Signal_Name := "mark-set";
   procedure On_Mark_Set
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Mark_Void;
       After : Boolean := False);
   procedure On_Mark_Set
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gtk_Text_Iter_Gtk_Text_Mark_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted as notification after a `GtkTextMark` is set.
   --
   --  See also: [methodGtk.TextBuffer.create_mark],
   --  [methodGtk.TextBuffer.move_mark].
   -- 
   --  Callback parameters:
   --    --  @param Location The location of Mark in Textbuffer
   --    --  @param Mark The mark that is set

   Signal_Modified_Changed : constant Glib.Signal_Name := "modified-changed";
   procedure On_Modified_Changed
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Void;
       After : Boolean := False);
   procedure On_Modified_Changed
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the modified bit of a `GtkTextBuffer` flips.
   --
   --  See also: [methodGtk.TextBuffer.set_modified].

   type Cb_Gtk_Text_Buffer_Gdk_Clipboard_Void is not null access procedure
     (Self      : access Gtk_Text_Buffer_Record'Class;
      Clipboard : not null access Gdk.Clipboard.Gdk_Clipboard_Record'Class);

   type Cb_GObject_Gdk_Clipboard_Void is not null access procedure
     (Self      : access Glib.Object.GObject_Record'Class;
      Clipboard : not null access Gdk.Clipboard.Gdk_Clipboard_Record'Class);

   Signal_Paste_Done : constant Glib.Signal_Name := "paste-done";
   procedure On_Paste_Done
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gdk_Clipboard_Void;
       After : Boolean := False);
   procedure On_Paste_Done
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gdk_Clipboard_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted after paste operation has been completed.
   --
   --  This is useful to properly scroll the view to the end of the pasted
   --  text. See [methodGtk.TextBuffer.paste_clipboard] for more details.

   Signal_Redo : constant Glib.Signal_Name := "redo";
   procedure On_Redo
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Void;
       After : Boolean := False);
   procedure On_Redo
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a request has been made to redo the previously undone
   --  operation.

   Signal_Remove_Tag : constant Glib.Signal_Name := "remove-tag";
   procedure On_Remove_Tag
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       After : Boolean := False);
   procedure On_Remove_Tag
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to remove all occurrences of Tag from a range of text in a
   --  `GtkTextBuffer`.
   --
   --  Removal actually occurs in the default handler.
   --
   --  Note that if your handler runs before the default handler it must not
   --  invalidate the Start and End iters (or has to revalidate them).
   --
   --  See also: [methodGtk.TextBuffer.remove_tag].
   -- 
   --  Callback parameters:
   --    --  @param Tag the tag to be removed
   --    --  @param Start the start of the range the tag is removed from
   --    --  @param The_End the end of the range the tag is removed from

   Signal_Undo : constant Glib.Signal_Name := "undo";
   procedure On_Undo
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Void;
       After : Boolean := False);
   procedure On_Undo
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a request has been made to undo the previous operation or
   --  set of operations that have been grouped together.

private
   Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("text");
   Tag_Table_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("tag-table");
   Has_Selection_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-selection");
   Enable_Undo_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enable-undo");
   Cursor_Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("cursor-position");
   Can_Undo_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("can-undo");
   Can_Redo_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("can-redo");
end Gtk.Text_Buffer;
