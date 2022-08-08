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
with Gdk.Pixbuf;            use Gdk.Pixbuf;
with Gdk.Types;             use Gdk.Types;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Gtk.Clipboard;         use Gtk.Clipboard;
with Gtk.Target_List;       use Gtk.Target_List;
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
   --  "table": a tag table, or null to create a new one

   function Gtk_Text_Buffer_New
      (Table : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table := null)
       return Gtk_Text_Buffer;
   --  Creates a new text buffer.
   --  "table": a tag table, or null to create a new one

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_text_buffer_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Mark
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Mark   : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class;
       Where  : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Adds the mark at position Where. The mark must not be added to another
   --  buffer, and if its name is not null then there must not be another mark
   --  in the buffer with the same name.
   --  Emits the Gtk.Text_Buffer.Gtk_Text_Buffer::mark-set signal as
   --  notification of the mark's initial placement.
   --  Since: gtk+ 2.12
   --  "mark": the mark to add
   --  "where": location to place mark

   procedure Add_Selection_Clipboard
      (Buffer    : not null access Gtk_Text_Buffer_Record;
       Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class);
   --  Adds Clipboard to the list of clipboards in which the selection
   --  contents of Buffer are available. In most cases, Clipboard will be the
   --  Gtk.Clipboard.Gtk_Clipboard of type GDK_SELECTION_PRIMARY for a view of
   --  Buffer.
   --  "clipboard": a Gtk.Clipboard.Gtk_Clipboard

   procedure Apply_Tag
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Tag     : not null access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
       Start   : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Emits the "apply-tag" signal on Buffer. The default handler for the
   --  signal applies Tag to the given range. Start and End do not have to be
   --  in order.
   --  "tag": a Gtk.Text_Tag.Gtk_Text_Tag
   --  "start": one bound of range to be tagged
   --  "end": other bound of range to be tagged

   procedure Apply_Tag_By_Name
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Name    : UTF8_String;
       Start   : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Calls Gtk.Text_Tag_Table.Lookup on the buffer's tag table to get a
   --  Gtk.Text_Tag.Gtk_Text_Tag, then calls Gtk.Text_Buffer.Apply_Tag.
   --  "name": name of a named Gtk.Text_Tag.Gtk_Text_Tag
   --  "start": one bound of range to be tagged
   --  "end": other bound of range to be tagged

   function Backspace
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Iter             : Gtk.Text_Iter.Gtk_Text_Iter;
       Interactive      : Boolean;
       Default_Editable : Boolean) return Boolean;
   --  Performs the appropriate action as if the user hit the delete key with
   --  the cursor at the position specified by Iter. In the normal case a
   --  single character will be deleted, but when combining accents are
   --  involved, more than one character can be deleted, and when precomposed
   --  character and accent combinations are involved, less than one character
   --  will be deleted.
   --  Because the buffer is modified, all outstanding iterators become
   --  invalid after calling this function; however, the Iter will be
   --  re-initialized to point to the location where text was deleted.
   --  Since: gtk+ 2.6
   --  "iter": a position in Buffer
   --  "interactive": whether the deletion is caused by user interaction
   --  "default_editable": whether the buffer is editable by default

   procedure Begin_User_Action
      (Buffer : not null access Gtk_Text_Buffer_Record);
   --  Called to indicate that the buffer operations between here and a call
   --  to Gtk.Text_Buffer.End_User_Action are part of a single user-visible
   --  operation. The operations between Gtk.Text_Buffer.Begin_User_Action and
   --  Gtk.Text_Buffer.End_User_Action can then be grouped when creating an
   --  undo stack. Gtk.Text_Buffer.Gtk_Text_Buffer maintains a count of calls
   --  to Gtk.Text_Buffer.Begin_User_Action that have not been closed with a
   --  call to Gtk.Text_Buffer.End_User_Action, and emits the
   --  "begin-user-action" and "end-user-action" signals only for the outermost
   --  pair of calls. This allows you to build user actions from other user
   --  actions.
   --  The "interactive" buffer mutation functions, such as
   --  Gtk.Text_Buffer.Insert_Interactive, automatically call begin/end user
   --  action around the buffer operations they perform, so there's no need to
   --  add extra calls if you user action consists solely of a single call to
   --  one of those functions.

   procedure Copy_Clipboard
      (Buffer    : not null access Gtk_Text_Buffer_Record;
       Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class);
   --  Copies the currently-selected text to a clipboard.
   --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard object to copy to

   function Create_Child_Anchor
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : Gtk.Text_Iter.Gtk_Text_Iter)
       return Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor;
   --  This is a convenience function which simply creates a child anchor with
   --  Gtk.Text_Child_Anchor.Gtk_New and inserts it into the buffer with
   --  Gtk.Text_Buffer.Insert_Child_Anchor. The new anchor is owned by the
   --  buffer; no reference count is returned to the caller of
   --  Gtk.Text_Buffer.Create_Child_Anchor.
   --  "iter": location in the buffer

   function Create_Mark
      (Buffer       : not null access Gtk_Text_Buffer_Record;
       Mark_Name    : UTF8_String := "";
       Where        : Gtk.Text_Iter.Gtk_Text_Iter;
       Left_Gravity : Boolean := True) return Gtk.Text_Mark.Gtk_Text_Mark;
   --  Creates a mark at position Where. If Mark_Name is null, the mark is
   --  anonymous; otherwise, the mark can be retrieved by name using
   --  Gtk.Text_Buffer.Get_Mark. If a mark has left gravity, and text is
   --  inserted at the mark's current location, the mark will be moved to the
   --  left of the newly-inserted text. If the mark has right gravity
   --  (Left_Gravity = False), the mark will end up on the right of
   --  newly-inserted text. The standard left-to-right cursor is a mark with
   --  right gravity (when you type, the cursor stays on the right side of the
   --  text you're typing).
   --  The caller of this function does not own a reference to the returned
   --  Gtk.Text_Mark.Gtk_Text_Mark, so you can ignore the return value if you
   --  like. Marks are owned by the buffer and go away when the buffer does.
   --  Emits the Gtk.Text_Buffer.Gtk_Text_Buffer::mark-set signal as
   --  notification of the mark's initial placement.
   --  "mark_name": name for mark, or null
   --  "where": location to place mark
   --  "left_gravity": whether the mark has left gravity

   procedure Cut_Clipboard
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Clipboard        : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
       Default_Editable : Boolean);
   --  Copies the currently-selected text to a clipboard, then deletes said
   --  text if it's editable.
   --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard object to cut to
   --  "default_editable": default editability of the buffer

   procedure Delete
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Start   : in out Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : in out Gtk.Text_Iter.Gtk_Text_Iter);
   --  Deletes text between Start and End. The order of Start and End is not
   --  actually relevant; Gtk.Text_Buffer.Delete will reorder them. This
   --  function actually emits the "delete-range" signal, and the default
   --  handler of that signal deletes the text. Because the buffer is modified,
   --  all outstanding iterators become invalid after calling this function;
   --  however, the Start and End will be re-initialized to point to the
   --  location where text was deleted.
   --  "start": a position in Buffer
   --  "end": another position in Buffer

   procedure Delete_Interactive
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Start_Iter       : Gtk.Text_Iter.Gtk_Text_Iter;
       End_Iter         : Gtk.Text_Iter.Gtk_Text_Iter;
       Default_Editable : Boolean;
       Result           : out Boolean);
   --  Deletes all editable text in the given range. Calls
   --  Gtk.Text_Buffer.Delete for each editable sub-range of [Start,End). Start
   --  and End are revalidated to point to the location of the last deleted
   --  range, or left untouched if no text was deleted.
   --  "start_iter": start of range to delete
   --  "end_iter": end of range
   --  "default_editable": whether the buffer is editable by default

   procedure Delete_Mark
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Mark   : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);
   --  Deletes Mark, so that it's no longer located anywhere in the buffer.
   --  Removes the reference the buffer holds to the mark, so if you haven't
   --  called g_object_ref on the mark, it will be freed. Even if the mark
   --  isn't freed, most operations on Mark become invalid, until it gets added
   --  to a buffer again with Gtk.Text_Buffer.Add_Mark. Use
   --  Gtk.Text_Mark.Get_Deleted to find out if a mark has been removed from
   --  its buffer. The Gtk.Text_Buffer.Gtk_Text_Buffer::mark-deleted signal
   --  will be emitted as notification after the mark is deleted.
   --  "mark": a Gtk.Text_Mark.Gtk_Text_Mark in Buffer

   procedure Delete_Mark_By_Name
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Name   : UTF8_String);
   --  Deletes the mark named Name; the mark must exist. See
   --  Gtk.Text_Buffer.Delete_Mark for details.
   --  "name": name of a mark in Buffer

   function Delete_Selection
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Interactive      : Boolean;
       Default_Editable : Boolean) return Boolean;
   --  Deletes the range between the "insert" and "selection_bound" marks,
   --  that is, the currently-selected text. If Interactive is True, the
   --  editability of the selection will be considered (users can't delete
   --  uneditable text).
   --  "interactive": whether the deletion is caused by user interaction
   --  "default_editable": whether the buffer is editable by default

   function Deserialize_Get_Can_Create_Tags
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Format : Gdk.Types.Gdk_Atom) return Boolean;
   --  This functions returns the value set with
   --  Gtk.Text_Buffer.Deserialize_Set_Can_Create_Tags
   --  Since: gtk+ 2.10
   --  "format": a Gdk.Types.Gdk_Atom representing a registered rich text
   --  format

   procedure Deserialize_Set_Can_Create_Tags
      (Buffer          : not null access Gtk_Text_Buffer_Record;
       Format          : Gdk.Types.Gdk_Atom;
       Can_Create_Tags : Boolean);
   --  Use this function to allow a rich text deserialization function to
   --  create new tags in the receiving buffer. Note that using this function
   --  is almost always a bad idea, because the rich text functions you
   --  register should know how to map the rich text format they handler to
   --  your text buffers set of tags.
   --  The ability of creating new (arbitrary!) tags in the receiving buffer
   --  is meant for special rich text formats like the internal one that is
   --  registered using Gtk.Text_Buffer.Register_Deserialize_Tagset, because
   --  that format is essentially a dump of the internal structure of the
   --  source buffer, including its tag names.
   --  You should allow creation of tags only if you know what you are doing,
   --  e.g. if you defined a tagset name for your application suite's text
   --  buffers and you know that it's fine to receive new tags from these
   --  buffers, because you know that your application can handle the newly
   --  created tags.
   --  Since: gtk+ 2.10
   --  "format": a Gdk.Types.Gdk_Atom representing a registered rich text
   --  format
   --  "can_create_tags": whether deserializing this format may create tags

   procedure End_User_Action
      (Buffer : not null access Gtk_Text_Buffer_Record);
   --  Should be paired with a call to Gtk.Text_Buffer.Begin_User_Action. See
   --  that function for a full explanation.

   procedure Get_Bounds
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Start   : out Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : out Gtk.Text_Iter.Gtk_Text_Iter);
   --  Retrieves the first and last iterators in the buffer, i.e. the entire
   --  buffer lies within the range [Start,End).
   --  "start": iterator to initialize with first position in the buffer
   --  "end": iterator to initialize with the end iterator

   function Get_Char_Count
      (Buffer : not null access Gtk_Text_Buffer_Record) return Glib.Gint;
   --  Gets the number of characters in the buffer; note that characters and
   --  bytes are not the same, you can't e.g. expect the contents of the buffer
   --  in string form to be this many bytes long. The character count is
   --  cached, so this function is very fast.

   function Get_Copy_Target_List
      (Buffer : not null access Gtk_Text_Buffer_Record)
       return Gtk.Target_List.Gtk_Target_List;
   --  This function returns the list of targets this text buffer can provide
   --  for copying and as DND source. The targets in the list are added with
   --  Info values from the Gtk_Text_Buffer_Target_Info enum, using
   --  gtk_target_list_add_rich_text_targets and
   --  Gtk.Target_List.Add_Text_Targets.
   --  Since: gtk+ 2.10

   procedure Get_End_Iter
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : out Gtk.Text_Iter.Gtk_Text_Iter);
   --  Initializes Iter with the "end iterator," one past the last valid
   --  character in the text buffer. If dereferenced with
   --  Gtk.Text_Iter.Get_Char, the end iterator has a character value of 0. The
   --  entire buffer lies in the range from the first position in the buffer
   --  (call Gtk.Text_Buffer.Get_Start_Iter to get character position 0) to the
   --  end iterator.
   --  "iter": iterator to initialize

   function Get_Has_Selection
      (Buffer : not null access Gtk_Text_Buffer_Record) return Boolean;
   --  Indicates whether the buffer has some text currently selected.
   --  Since: gtk+ 2.10

   function Get_Insert
      (Buffer : not null access Gtk_Text_Buffer_Record)
       return Gtk.Text_Mark.Gtk_Text_Mark;
   --  Returns the mark that represents the cursor (insertion point).
   --  Equivalent to calling Gtk.Text_Buffer.Get_Mark to get the mark named
   --  "insert", but very slightly more efficient, and involves less typing.

   procedure Insert
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
       Text   : UTF8_String);
   --  Inserts Len bytes of Text at position Iter. If Len is -1, Text must be
   --  nul-terminated and will be inserted in its entirety. Emits the
   --  "insert-text" signal; insertion actually occurs in the default handler
   --  for the signal. Iter is invalidated when insertion occurs (because the
   --  buffer contents change), but the default signal handler revalidates it
   --  to point to the end of the inserted text.
   --  "iter": a position in the buffer
   --  "text": text in UTF-8 format

   procedure Get_Iter_At_Child_Anchor
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : out Gtk.Text_Iter.Gtk_Text_Iter;
       Anchor : not null access Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor_Record'Class);
   --  Obtains the location of Anchor within Buffer.
   --  "iter": an iterator to be initialized
   --  "anchor": a child anchor that appears in Buffer

   procedure Get_Iter_At_Line
      (Buffer      : not null access Gtk_Text_Buffer_Record;
       Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
       Line_Number : Glib.Gint);
   --  Initializes Iter to the start of the given line. If Line_Number is
   --  greater than the number of lines in the Buffer, the end iterator is
   --  returned.
   --  "iter": iterator to initialize
   --  "line_number": line number counting from 0

   procedure Get_Iter_At_Line_Index
      (Buffer      : not null access Gtk_Text_Buffer_Record;
       Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
       Line_Number : Glib.Gint;
       Byte_Index  : Glib.Gint);
   --  Obtains an iterator pointing to Byte_Index within the given line.
   --  Byte_Index must be the start of a UTF-8 character. Note bytes, not
   --  characters; UTF-8 may encode one character as multiple bytes.
   --  Before the 3.20 version, it was not allowed to pass an invalid
   --  location.
   --  Since the 3.20 version, if Line_Number is greater than the number of
   --  lines in the Buffer, the end iterator is returned. And if Byte_Index is
   --  off the end of the line, the iterator at the end of the line is
   --  returned.
   --  "iter": iterator to initialize
   --  "line_number": line number counting from 0
   --  "byte_index": byte index from start of line

   procedure Get_Iter_At_Line_Offset
      (Buffer      : not null access Gtk_Text_Buffer_Record;
       Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
       Line_Number : Glib.Gint;
       Char_Offset : Glib.Gint);
   --  Obtains an iterator pointing to Char_Offset within the given line. Note
   --  characters, not bytes; UTF-8 may encode one character as multiple bytes.
   --  Before the 3.20 version, it was not allowed to pass an invalid
   --  location.
   --  Since the 3.20 version, if Line_Number is greater than the number of
   --  lines in the Buffer, the end iterator is returned. And if Char_Offset is
   --  off the end of the line, the iterator at the end of the line is
   --  returned.
   --  "iter": iterator to initialize
   --  "line_number": line number counting from 0
   --  "char_offset": char offset from start of line

   procedure Get_Iter_At_Mark
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : out Gtk.Text_Iter.Gtk_Text_Iter;
       Mark   : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);
   --  Initializes Iter with the current position of Mark.
   --  "iter": iterator to initialize
   --  "mark": a Gtk.Text_Mark.Gtk_Text_Mark in Buffer

   procedure Get_Iter_At_Offset
      (Buffer      : not null access Gtk_Text_Buffer_Record;
       Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
       Char_Offset : Glib.Gint);
   --  Initializes Iter to a position Char_Offset chars from the start of the
   --  entire buffer. If Char_Offset is -1 or greater than the number of
   --  characters in the buffer, Iter is initialized to the end iterator, the
   --  iterator one past the last valid character in the buffer.
   --  "iter": iterator to initialize
   --  "char_offset": char offset from start of buffer, counting from 0, or -1

   function Get_Line_Count
      (Buffer : not null access Gtk_Text_Buffer_Record) return Glib.Gint;
   --  Obtains the number of lines in the buffer. This value is cached, so the
   --  function is very fast.

   function Get_Mark
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Name   : UTF8_String) return Gtk.Text_Mark.Gtk_Text_Mark;
   --  Returns the mark named Name in buffer Buffer, or null if no such mark
   --  exists in the buffer.
   --  "name": a mark name

   function Get_Modified
      (Buffer : not null access Gtk_Text_Buffer_Record) return Boolean;
   --  Indicates whether the buffer has been modified since the last call to
   --  Gtk.Text_Buffer.Set_Modified set the modification flag to False. Used
   --  for example to enable a "save" function in a text editor.

   procedure Set_Modified
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Setting : Boolean);
   --  Used to keep track of whether the buffer has been modified since the
   --  last time it was saved. Whenever the buffer is saved to disk, call
   --  gtk_text_buffer_set_modified (Buffer, FALSE). When the buffer is
   --  modified, it will automatically toggled on the modified bit again. When
   --  the modified bit flips, the buffer emits the
   --  Gtk.Text_Buffer.Gtk_Text_Buffer::modified-changed signal.
   --  "setting": modification flag setting

   function Get_Paste_Target_List
      (Buffer : not null access Gtk_Text_Buffer_Record)
       return Gtk.Target_List.Gtk_Target_List;
   --  This function returns the list of targets this text buffer supports for
   --  pasting and as DND destination. The targets in the list are added with
   --  Info values from the Gtk_Text_Buffer_Target_Info enum, using
   --  gtk_target_list_add_rich_text_targets and
   --  Gtk.Target_List.Add_Text_Targets.
   --  Since: gtk+ 2.10

   function Get_Selection_Bound
      (Buffer : not null access Gtk_Text_Buffer_Record)
       return Gtk.Text_Mark.Gtk_Text_Mark;
   --  Returns the mark that represents the selection bound. Equivalent to
   --  calling Gtk.Text_Buffer.Get_Mark to get the mark named
   --  "selection_bound", but very slightly more efficient, and involves less
   --  typing.
   --  The currently-selected text in Buffer is the region between the
   --  "selection_bound" and "insert" marks. If "selection_bound" and "insert"
   --  are in the same place, then there is no current selection.
   --  Gtk.Text_Buffer.Get_Selection_Bounds is another convenient function for
   --  handling the selection, if you just want to know whether there's a
   --  selection and what its bounds are.

   procedure Get_Selection_Bounds
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Start   : out Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : out Gtk.Text_Iter.Gtk_Text_Iter;
       Result  : out Boolean);
   --  Returns True if some text is selected; places the bounds of the
   --  selection in Start and End (if the selection has length 0, then Start
   --  and End are filled in with the same value). Start and End will be in
   --  ascending order. If Start and End are NULL, then they are not filled in,
   --  but the return value still indicates whether text is selected.
   --  "start": iterator to initialize with selection start
   --  "end": iterator to initialize with selection end

   function Get_Slice
      (Buffer               : not null access Gtk_Text_Buffer_Record;
       Start                : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
       Include_Hidden_Chars : Boolean := False) return UTF8_String;
   --  Returns the text in the range [Start,End). Excludes undisplayed text
   --  (text marked with tags that set the invisibility attribute) if
   --  Include_Hidden_Chars is False. The returned string includes a 0xFFFC
   --  character whenever the buffer contains embedded images, so byte and
   --  character indexes into the returned string do correspond to byte and
   --  character indexes into the buffer. Contrast with
   --  Gtk.Text_Buffer.Get_Text. Note that 0xFFFC can occur in normal text as
   --  well, so it is not a reliable indicator that a pixbuf or widget is in
   --  the buffer.
   --  "start": start of a range
   --  "end": end of a range
   --  "include_hidden_chars": whether to include invisible text

   procedure Get_Start_Iter
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : out Gtk.Text_Iter.Gtk_Text_Iter);
   --  Initialized Iter with the first position in the text buffer. This is
   --  the same as using Gtk.Text_Buffer.Get_Iter_At_Offset to get the iter at
   --  character offset 0.
   --  "iter": iterator to initialize

   function Get_Tag_Table
      (Buffer : not null access Gtk_Text_Buffer_Record)
       return Gtk.Text_Tag_Table.Gtk_Text_Tag_Table;
   --  Get the Gtk.Text_Tag_Table.Gtk_Text_Tag_Table associated with this
   --  buffer.

   function Get_Text
      (Buffer               : not null access Gtk_Text_Buffer_Record;
       Start                : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
       Include_Hidden_Chars : Boolean := False) return UTF8_String;
   --  Returns the text in the range [Start,End). Excludes undisplayed text
   --  (text marked with tags that set the invisibility attribute) if
   --  Include_Hidden_Chars is False. Does not include characters representing
   --  embedded images, so byte and character indexes into the returned string
   --  do not correspond to byte and character indexes into the buffer.
   --  Contrast with Gtk.Text_Buffer.Get_Slice.
   --  "start": start of a range
   --  "end": end of a range
   --  "include_hidden_chars": whether to include invisible text

   procedure Set_Text
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Text   : UTF8_String);
   --  Deletes current contents of Buffer, and inserts Text instead. If Len is
   --  -1, Text must be nul-terminated. Text must be valid UTF-8.
   --  "text": UTF-8 text to insert

   procedure Insert_At_Cursor
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Text   : UTF8_String);
   --  Simply calls Gtk.Text_Buffer.Insert, using the current cursor position
   --  as the insertion point.
   --  "text": text in UTF-8 format

   procedure Insert_Child_Anchor
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
       Anchor : not null access Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor_Record'Class);
   --  Inserts a child widget anchor into the text buffer at Iter. The anchor
   --  will be counted as one character in character counts, and when obtaining
   --  the buffer contents as a string, will be represented by the Unicode
   --  "object replacement character" 0xFFFC. Note that the "slice" variants
   --  for obtaining portions of the buffer as a string include this character
   --  for child anchors, but the "text" variants do not. E.g. see
   --  Gtk.Text_Buffer.Get_Slice and Gtk.Text_Buffer.Get_Text. Consider
   --  Gtk.Text_Buffer.Create_Child_Anchor as a more convenient alternative to
   --  this function. The buffer will add a reference to the anchor, so you can
   --  unref it after insertion.
   --  "iter": location to insert the anchor
   --  "anchor": a Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor

   function Insert_Interactive
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Iter             : access Gtk.Text_Iter.Gtk_Text_Iter;
       Text             : UTF8_String;
       Default_Editable : Boolean) return Boolean;
   --  Like Gtk.Text_Buffer.Insert, but the insertion will not occur if Iter
   --  is at a non-editable location in the buffer. Usually you want to prevent
   --  insertions at ineditable locations if the insertion results from a user
   --  action (is interactive).
   --  Default_Editable indicates the editability of text that doesn't have a
   --  tag affecting editability applied to it. Typically the result of
   --  Gtk.Text_View.Get_Editable is appropriate here.
   --  "iter": a position in Buffer
   --  "text": some UTF-8 text
   --  "default_editable": default editability of buffer

   function Insert_Interactive_At_Cursor
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Text             : UTF8_String;
       Default_Editable : Boolean) return Boolean;
   --  Calls Gtk.Text_Buffer.Insert_Interactive at the cursor position.
   --  Default_Editable indicates the editability of text that doesn't have a
   --  tag affecting editability applied to it. Typically the result of
   --  Gtk.Text_View.Get_Editable is appropriate here.
   --  "text": text in UTF-8 format
   --  "default_editable": default editability of buffer

   procedure Insert_Markup
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
       Markup : UTF8_String;
       Len    : Glib.Gint);
   --  Inserts the text in Markup at position Iter. Markup will be inserted in
   --  its entirety and must be nul-terminated and valid UTF-8. Emits the
   --  Gtk.Text_Buffer.Gtk_Text_Buffer::insert-text signal, possibly multiple
   --  times; insertion actually occurs in the default handler for the signal.
   --  Iter will point to the end of the inserted text on return.
   --  Since: gtk+ 3.16
   --  "iter": location to insert the markup
   --  "markup": a nul-terminated UTF-8 string containing [Pango
   --  markup][PangoMarkupFormat]
   --  "len": length of Markup in bytes, or -1

   procedure Insert_Pixbuf
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
       Pixbuf : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Inserts an image into the text buffer at Iter. The image will be
   --  counted as one character in character counts, and when obtaining the
   --  buffer contents as a string, will be represented by the Unicode "object
   --  replacement character" 0xFFFC. Note that the "slice" variants for
   --  obtaining portions of the buffer as a string include this character for
   --  pixbufs, but the "text" variants do not. e.g. see
   --  Gtk.Text_Buffer.Get_Slice and Gtk.Text_Buffer.Get_Text.
   --  "iter": location to insert the pixbuf
   --  "pixbuf": a Gdk.Pixbuf.Gdk_Pixbuf

   procedure Insert_Range
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Iter    : Gtk.Text_Iter.Gtk_Text_Iter;
       Start   : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Copies text, tags, and pixbufs between Start and End (the order of
   --  Start and End doesn't matter) and inserts the copy at Iter. Used instead
   --  of simply getting/inserting text because it preserves images and tags.
   --  If Start and End are in a different buffer from Buffer, the two buffers
   --  must share the same tag table.
   --  Implemented via emissions of the insert_text and apply_tag signals, so
   --  expect those.
   --  "iter": a position in Buffer
   --  "start": a position in a Gtk.Text_Buffer.Gtk_Text_Buffer
   --  "end": another position in the same buffer as Start

   function Insert_Range_Interactive
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Iter             : Gtk.Text_Iter.Gtk_Text_Iter;
       Start            : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End          : Gtk.Text_Iter.Gtk_Text_Iter;
       Default_Editable : Boolean) return Boolean;
   --  Same as Gtk.Text_Buffer.Insert_Range, but does nothing if the insertion
   --  point isn't editable. The Default_Editable parameter indicates whether
   --  the text is editable at Iter if no tags enclosing Iter affect
   --  editability. Typically the result of Gtk.Text_View.Get_Editable is
   --  appropriate here.
   --  "iter": a position in Buffer
   --  "start": a position in a Gtk.Text_Buffer.Gtk_Text_Buffer
   --  "end": another position in the same buffer as Start
   --  "default_editable": default editability of the buffer

   procedure Move_Mark
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Mark   : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class;
       Where  : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Moves Mark to the new location Where. Emits the
   --  Gtk.Text_Buffer.Gtk_Text_Buffer::mark-set signal as notification of the
   --  move.
   --  "mark": a Gtk.Text_Mark.Gtk_Text_Mark
   --  "where": new location for Mark in Buffer

   procedure Move_Mark_By_Name
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Name   : UTF8_String;
       Where  : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Moves the mark named Name (which must exist) to location Where. See
   --  Gtk.Text_Buffer.Move_Mark for details.
   --  "name": name of a mark
   --  "where": new location for mark

   procedure Paste_Clipboard
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Clipboard        : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
       Default_Editable : Boolean := True);
   --  Pastes the contents of a clipboard. If Override_Location is null, the
   --  pasted text will be inserted at the cursor position, or the buffer
   --  selection will be replaced if the selection is non-empty.
   --  Note: pasting is asynchronous, that is, we'll ask for the paste data
   --  and return, and at some point later after the main loop runs, the paste
   --  data will be inserted.
   --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard to paste from
   --  "default_editable": whether the buffer is editable by default

   procedure Place_Cursor
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Where  : Gtk.Text_Iter.Gtk_Text_Iter);
   --  This function moves the "insert" and "selection_bound" marks
   --  simultaneously. If you move them to the same place in two steps with
   --  Gtk.Text_Buffer.Move_Mark, you will temporarily select a region in
   --  between their old and new locations, which can be pretty inefficient
   --  since the temporarily-selected region will force stuff to be
   --  recalculated. This function moves them as a unit, which can be
   --  optimized.
   --  "where": where to put the cursor

   function Register_Deserialize_Tagset
      (Buffer      : not null access Gtk_Text_Buffer_Record;
       Tagset_Name : UTF8_String := "") return Gdk.Types.Gdk_Atom;
   --  This function registers GTK+'s internal rich text serialization format
   --  with the passed Buffer. See Gtk.Text_Buffer.Register_Serialize_Tagset
   --  for details.
   --  Since: gtk+ 2.10
   --  "tagset_name": an optional tagset name, on null

   function Register_Serialize_Tagset
      (Buffer      : not null access Gtk_Text_Buffer_Record;
       Tagset_Name : UTF8_String := "") return Gdk.Types.Gdk_Atom;
   --  This function registers GTK+'s internal rich text serialization format
   --  with the passed Buffer. The internal format does not comply to any
   --  standard rich text format and only works between
   --  Gtk.Text_Buffer.Gtk_Text_Buffer instances. It is capable of serializing
   --  all of a text buffer's tags and embedded pixbufs.
   --  This function is just a wrapper around
   --  gtk_text_buffer_register_serialize_format. The mime type used for
   --  registering is "application/x-gtk-text-buffer-rich-text", or
   --  "application/x-gtk-text-buffer-rich-text;format=Tagset_Name" if a
   --  Tagset_Name was passed.
   --  The Tagset_Name can be used to restrict the transfer of rich text to
   --  buffers with compatible sets of tags, in order to avoid unknown tags
   --  from being pasted. It is probably the common case to pass an identifier
   --  != null here, since the null tagset requires the receiving buffer to
   --  deal with with pasting of arbitrary tags.
   --  Since: gtk+ 2.10
   --  "tagset_name": an optional tagset name, on null

   procedure Remove_All_Tags
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Start   : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Removes all tags in the range between Start and End. Be careful with
   --  this function; it could remove tags added in code unrelated to the code
   --  you're currently writing. That is, using this function is probably a bad
   --  idea if you have two or more unrelated code sections that add tags.
   --  "start": one bound of range to be untagged
   --  "end": other bound of range to be untagged

   procedure Remove_Selection_Clipboard
      (Buffer    : not null access Gtk_Text_Buffer_Record;
       Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class);
   --  Removes a Gtk.Clipboard.Gtk_Clipboard added with
   --  Gtk.Text_Buffer.Add_Selection_Clipboard.
   --  "clipboard": a Gtk.Clipboard.Gtk_Clipboard added to Buffer by
   --  Gtk.Text_Buffer.Add_Selection_Clipboard

   procedure Remove_Tag
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Tag     : not null access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
       Start   : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Emits the "remove-tag" signal. The default handler for the signal
   --  removes all occurrences of Tag from the given range. Start and End don't
   --  have to be in order.
   --  "tag": a Gtk.Text_Tag.Gtk_Text_Tag
   --  "start": one bound of range to be untagged
   --  "end": other bound of range to be untagged

   procedure Remove_Tag_By_Name
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Name    : UTF8_String;
       Start   : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Calls Gtk.Text_Tag_Table.Lookup on the buffer's tag table to get a
   --  Gtk.Text_Tag.Gtk_Text_Tag, then calls Gtk.Text_Buffer.Remove_Tag.
   --  "name": name of a Gtk.Text_Tag.Gtk_Text_Tag
   --  "start": one bound of range to be untagged
   --  "end": other bound of range to be untagged

   procedure Select_Range
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Ins    : Gtk.Text_Iter.Gtk_Text_Iter;
       Bound  : Gtk.Text_Iter.Gtk_Text_Iter);
   --  This function moves the "insert" and "selection_bound" marks
   --  simultaneously. If you move them in two steps with
   --  Gtk.Text_Buffer.Move_Mark, you will temporarily select a region in
   --  between their old and new locations, which can be pretty inefficient
   --  since the temporarily-selected region will force stuff to be
   --  recalculated. This function moves them as a unit, which can be
   --  optimized.
   --  Since: gtk+ 2.4
   --  "ins": where to put the "insert" mark
   --  "bound": where to put the "selection_bound" mark

   procedure Unregister_Deserialize_Format
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Format : Gdk.Types.Gdk_Atom);
   --  This function unregisters a rich text format that was previously
   --  registered using gtk_text_buffer_register_deserialize_format or
   --  Gtk.Text_Buffer.Register_Deserialize_Tagset.
   --  Since: gtk+ 2.10
   --  "format": a Gdk.Types.Gdk_Atom representing a registered rich text
   --  format.

   procedure Unregister_Serialize_Format
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Format : Gdk.Types.Gdk_Atom);
   --  This function unregisters a rich text format that was previously
   --  registered using gtk_text_buffer_register_serialize_format or
   --  Gtk.Text_Buffer.Register_Serialize_Tagset
   --  Since: gtk+ 2.10
   --  "format": a Gdk.Types.Gdk_Atom representing a registered rich text
   --  format.

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
      Tag    : Gtk_Text_Tag);
   procedure Insert_With_Tags
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Text   : Gtkada.Types.Chars_Ptr;
      Tag    : Gtk.Text_Tag.Gtk_Text_Tag);
   --  Same as Insert, but specifies the tag to apply to the range.

   function Get_Buffer
     (Iter : Gtk_Text_Iter) return Gtk.Text_Buffer.Gtk_Text_Buffer;
   --  Returns the Gtk.Text_Buffer.Gtk_Text_Buffer this iterator is associated
   --  with.

   function Get_Buffer
     (Mark : Gtk_Text_Mark)
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
   --  If Tag_Name is NULL, the tag is anonymous, otherwise a tag called
   --  Tag_Name must not already exist in the tag table for this buffer.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Copy_Target_List_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Target_List.Gtk_Target_List
   --  The list of targets this buffer supports for clipboard copying and as
   --  DND source.

   Cursor_Position_Property : constant Glib.Properties.Property_Int;
   --  The position of the insert mark (as offset from the beginning of the
   --  buffer). It is useful for getting notified when the cursor moves.

   Has_Selection_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the buffer has some text currently selected.

   Paste_Target_List_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Target_List.Gtk_Target_List
   --  The list of targets this buffer supports for clipboard pasting and as
   --  DND destination.

   Tag_Table_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Text_Tag_Table.Gtk_Text_Tag_Table

   Text_Property : constant Glib.Properties.Property_String;
   --  The text content of the buffer. Without child widgets and images, see
   --  Gtk.Text_Buffer.Get_Text for more information.

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
   --  The ::apply-tag signal is emitted to apply a tag to a range of text in
   --  a Gtk.Text_Buffer.Gtk_Text_Buffer. Applying actually occurs in the
   --  default handler.
   --
   --  Note that if your handler runs before the default handler it must not
   --  invalidate the Start and End iters (or has to revalidate them).
   --
   --  See also: Gtk.Text_Buffer.Apply_Tag, gtk_text_buffer_insert_with_tags,
   --  Gtk.Text_Buffer.Insert_Range.
   -- 
   --  Callback parameters:
   --    --  "tag": the applied tag
   --    --  "start": the start of the range the tag is applied to
   --    --  "end": the end of the range the tag is applied to

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
   --  The ::begin-user-action signal is emitted at the beginning of a single
   --  user-visible operation on a Gtk.Text_Buffer.Gtk_Text_Buffer.
   --
   --  See also: Gtk.Text_Buffer.Begin_User_Action,
   --  Gtk.Text_Buffer.Insert_Interactive,
   --  Gtk.Text_Buffer.Insert_Range_Interactive,
   --  Gtk.Text_Buffer.Delete_Interactive, Gtk.Text_Buffer.Backspace,
   --  Gtk.Text_Buffer.Delete_Selection.

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
   --  The ::changed signal is emitted when the content of a
   --  Gtk.Text_Buffer.Gtk_Text_Buffer has changed.

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
   --  The ::delete-range signal is emitted to delete a range from a
   --  Gtk.Text_Buffer.Gtk_Text_Buffer.
   --
   --  Note that if your handler runs before the default handler it must not
   --  invalidate the Start and End iters (or has to revalidate them). The
   --  default signal handler revalidates the Start and End iters to both point
   --  to the location where text was deleted. Handlers which run after the
   --  default handler (see g_signal_connect_after) do not have access to the
   --  deleted text.
   --
   --  See also: Gtk.Text_Buffer.Delete.
   -- 
   --  Callback parameters:
   --    --  "start": the start of the range to be deleted
   --    --  "end": the end of the range to be deleted

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
   --  The ::end-user-action signal is emitted at the end of a single
   --  user-visible operation on the Gtk.Text_Buffer.Gtk_Text_Buffer.
   --
   --  See also: Gtk.Text_Buffer.End_User_Action,
   --  Gtk.Text_Buffer.Insert_Interactive,
   --  Gtk.Text_Buffer.Insert_Range_Interactive,
   --  Gtk.Text_Buffer.Delete_Interactive, Gtk.Text_Buffer.Backspace,
   --  Gtk.Text_Buffer.Delete_Selection, Gtk.Text_Buffer.Backspace.

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
   --  The ::insert-child-anchor signal is emitted to insert a
   --  Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor in a
   --  Gtk.Text_Buffer.Gtk_Text_Buffer. Insertion actually occurs in the
   --  default handler.
   --
   --  Note that if your handler runs before the default handler it must not
   --  invalidate the Location iter (or has to revalidate it). The default
   --  signal handler revalidates it to be placed after the inserted Anchor.
   --
   --  See also: Gtk.Text_Buffer.Insert_Child_Anchor.
   -- 
   --  Callback parameters:
   --    --  "location": position to insert Anchor in Textbuffer
   --    --  "anchor": the Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor to be
   --    --  inserted

   type Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gdk_Pixbuf_Void is not null access procedure
     (Self     : access Gtk_Text_Buffer_Record'Class;
      Location : Gtk.Text_Iter.Gtk_Text_Iter;
      Pixbuf   : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);

   type Cb_GObject_Gtk_Text_Iter_Gdk_Pixbuf_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Location : Gtk.Text_Iter.Gtk_Text_Iter;
      Pixbuf   : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);

   Signal_Insert_Pixbuf : constant Glib.Signal_Name := "insert-pixbuf";
   procedure On_Insert_Pixbuf
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gdk_Pixbuf_Void;
       After : Boolean := False);
   procedure On_Insert_Pixbuf
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gtk_Text_Iter_Gdk_Pixbuf_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::insert-pixbuf signal is emitted to insert a Gdk.Pixbuf.Gdk_Pixbuf
   --  in a Gtk.Text_Buffer.Gtk_Text_Buffer. Insertion actually occurs in the
   --  default handler.
   --
   --  Note that if your handler runs before the default handler it must not
   --  invalidate the Location iter (or has to revalidate it). The default
   --  signal handler revalidates it to be placed after the inserted Pixbuf.
   --
   --  See also: Gtk.Text_Buffer.Insert_Pixbuf.
   -- 
   --  Callback parameters:
   --    --  "location": position to insert Pixbuf in Textbuffer
   --    --  "pixbuf": the Gdk.Pixbuf.Gdk_Pixbuf to be inserted

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
   --  The ::insert-text signal is emitted to insert text in a
   --  Gtk.Text_Buffer.Gtk_Text_Buffer. Insertion actually occurs in the
   --  default handler.
   --
   --  Note that if your handler runs before the default handler it must not
   --  invalidate the Location iter (or has to revalidate it). The default
   --  signal handler revalidates it to point to the end of the inserted text.
   --
   --  See also: Gtk.Text_Buffer.Insert, Gtk.Text_Buffer.Insert_Range.
   -- 
   --  Callback parameters:
   --    --  "location": position to insert Text in Textbuffer
   --    --  "text": the UTF-8 text to be inserted
   --    --  "len": length of the inserted text in bytes

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
   --  The ::mark-deleted signal is emitted as notification after a
   --  Gtk.Text_Mark.Gtk_Text_Mark is deleted.
   --
   --  See also: Gtk.Text_Buffer.Delete_Mark.

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
   --  The ::mark-set signal is emitted as notification after a
   --  Gtk.Text_Mark.Gtk_Text_Mark is set.
   --
   --  See also: Gtk.Text_Buffer.Create_Mark, Gtk.Text_Buffer.Move_Mark.
   -- 
   --  Callback parameters:
   --    --  "location": The location of Mark in Textbuffer
   --    --  "mark": The mark that is set

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
   --  The ::modified-changed signal is emitted when the modified bit of a
   --  Gtk.Text_Buffer.Gtk_Text_Buffer flips.
   --
   --  See also: Gtk.Text_Buffer.Set_Modified.

   type Cb_Gtk_Text_Buffer_Gtk_Clipboard_Void is not null access procedure
     (Self      : access Gtk_Text_Buffer_Record'Class;
      Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class);

   type Cb_GObject_Gtk_Clipboard_Void is not null access procedure
     (Self      : access Glib.Object.GObject_Record'Class;
      Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class);

   Signal_Paste_Done : constant Glib.Signal_Name := "paste-done";
   procedure On_Paste_Done
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gtk_Clipboard_Void;
       After : Boolean := False);
   procedure On_Paste_Done
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gtk_Clipboard_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The paste-done signal is emitted after paste operation has been
   --  completed. This is useful to properly scroll the view to the end of the
   --  pasted text. See Gtk.Text_Buffer.Paste_Clipboard for more details.

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
   --  The ::remove-tag signal is emitted to remove all occurrences of Tag
   --  from a range of text in a Gtk.Text_Buffer.Gtk_Text_Buffer. Removal
   --  actually occurs in the default handler.
   --
   --  Note that if your handler runs before the default handler it must not
   --  invalidate the Start and End iters (or has to revalidate them).
   --
   --  See also: Gtk.Text_Buffer.Remove_Tag.
   -- 
   --  Callback parameters:
   --    --  "tag": the tag to be removed
   --    --  "start": the start of the range the tag is removed from
   --    --  "end": the end of the range the tag is removed from

private
   Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("text");
   Tag_Table_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("tag-table");
   Paste_Target_List_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("paste-target-list");
   Has_Selection_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-selection");
   Cursor_Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("cursor-position");
   Copy_Target_List_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("copy-target-list");
end Gtk.Text_Buffer;
