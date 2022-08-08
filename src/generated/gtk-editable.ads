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
--  The Gtk.Editable.Gtk_Editable interface is an interface which should be
--  implemented by text editing widgets, such as Gtk.GEntry.Gtk_Entry and
--  Gtk.Spin_Button.Gtk_Spin_Button. It contains functions for generically
--  manipulating an editable widget, a large number of action signals used for
--  key bindings, and several signals that an application can connect to to
--  modify the behavior of a widget.
--
--  As an example of the latter usage, by connecting the following handler to
--  Gtk.Editable.Gtk_Editable::insert-text, an application can convert all
--  entry into a widget into uppercase.
--
--  ## Forcing entry to uppercase.
--
--  |[<!-- language="C" --> include <ctype.h>;
--
--  void insert_text_handler (GtkEditable *editable, const gchar *text, gint
--  length, gint *position, gpointer data) { gchar *result = g_utf8_strup
--  (text, length);
--
--  g_signal_handlers_block_by_func (editable, (gpointer) insert_text_handler,
--  data); gtk_editable_insert_text (editable, result, length, position);
--  g_signal_handlers_unblock_by_func (editable, (gpointer)
--  insert_text_handler, data);
--
--  g_signal_stop_emission_by_name (editable, "insert_text");
--
--  g_free (result); } ]|
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;         use Glib;
with Glib.Object;  use Glib.Object;
with Glib.Types;   use Glib.Types;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types; use Gtkada.Types;
pragma Warnings(On);

package Gtk.Editable is

   type Gtk_Editable is new Glib.Types.GType_Interface;
   Null_Gtk_Editable : constant Gtk_Editable;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_editable_get_type");

   -------------
   -- Methods --
   -------------

   procedure Copy_Clipboard (Editable : Gtk_Editable);
   pragma Import (C, Copy_Clipboard, "gtk_editable_copy_clipboard");
   --  Copies the contents of the currently selected content in the editable
   --  and puts it on the clipboard.

   procedure Cut_Clipboard (Editable : Gtk_Editable);
   pragma Import (C, Cut_Clipboard, "gtk_editable_cut_clipboard");
   --  Removes the contents of the currently selected content in the editable
   --  and puts it on the clipboard.

   procedure Delete_Selection (Editable : Gtk_Editable);
   pragma Import (C, Delete_Selection, "gtk_editable_delete_selection");
   --  Deletes the currently selected text of the editable. This call doesn't
   --  do anything if there is no selected text.

   procedure Delete_Text
      (Editable  : Gtk_Editable;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1);
   pragma Import (C, Delete_Text, "gtk_editable_delete_text");
   --  Deletes a sequence of characters. The characters that are deleted are
   --  those characters at positions from Start_Pos up to, but not including
   --  End_Pos. If End_Pos is negative, then the characters deleted are those
   --  from Start_Pos to the end of the text.
   --  Note that the positions are specified in characters, not bytes.
   --  "start_pos": start position
   --  "end_pos": end position

   function Get_Chars
      (Editable  : Gtk_Editable;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1) return UTF8_String;
   --  Retrieves a sequence of characters. The characters that are retrieved
   --  are those characters at positions from Start_Pos up to, but not
   --  including End_Pos. If End_Pos is negative, then the characters retrieved
   --  are those characters from Start_Pos to the end of the text.
   --  Note that positions are specified in characters, not bytes.
   --  "start_pos": start of text
   --  "end_pos": end of text

   function Get_Editable (Editable : Gtk_Editable) return Boolean;
   --  Retrieves whether Editable is editable. See Gtk.Editable.Set_Editable.

   procedure Set_Editable (Editable : Gtk_Editable; Is_Editable : Boolean);
   --  Determines if the user can edit the text in the editable widget or not.
   --  "is_editable": True if the user is allowed to edit the text in the
   --  widget

   function Get_Position (Editable : Gtk_Editable) return Glib.Gint;
   pragma Import (C, Get_Position, "gtk_editable_get_position");
   --  Retrieves the current position of the cursor relative to the start of
   --  the content of the editable.
   --  Note that this position is in characters, not in bytes.

   procedure Set_Position (Editable : Gtk_Editable; Position : Glib.Gint);
   pragma Import (C, Set_Position, "gtk_editable_set_position");
   --  Sets the cursor position in the editable to the given value.
   --  The cursor is displayed before the character with the given (base 0)
   --  index in the contents of the editable. The value must be less than or
   --  equal to the number of characters in the editable. A value of -1
   --  indicates that the position should be set after the last character of
   --  the editable. Note that Position is in characters, not in bytes.
   --  "position": the position of the cursor

   procedure Get_Selection_Bounds
      (Editable      : Gtk_Editable;
       Start_Pos     : out Glib.Gint;
       End_Pos       : out Glib.Gint;
       Has_Selection : out Boolean);
   --  Retrieves the selection bound of the editable. start_pos will be filled
   --  with the start of the selection and End_Pos with end. If no text was
   --  selected both will be identical and False will be returned.
   --  Note that positions are specified in characters, not bytes.
   --  "start_pos": location to store the starting position, or null
   --  "end_pos": location to store the end position, or null

   procedure Insert_Text
      (Editable        : Gtk_Editable;
       New_Text        : UTF8_String;
       New_Text_Length : Glib.Gint;
       Position        : in out Glib.Gint);
   --  Inserts New_Text_Length bytes of New_Text into the contents of the
   --  widget, at position Position.
   --  Note that the position is in characters, not in bytes. The function
   --  updates Position to point after the newly inserted text.
   --  "new_text": the text to append
   --  "new_text_length": the length of the text in bytes, or -1
   --  "position": location of the position text will be inserted at

   procedure Paste_Clipboard (Editable : Gtk_Editable);
   pragma Import (C, Paste_Clipboard, "gtk_editable_paste_clipboard");
   --  Pastes the content of the clipboard to the current position of the
   --  cursor in the editable.

   procedure Select_Region
      (Editable  : Gtk_Editable;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1);
   pragma Import (C, Select_Region, "gtk_editable_select_region");
   --  Selects a region of text. The characters that are selected are those
   --  characters at positions from Start_Pos up to, but not including End_Pos.
   --  If End_Pos is negative, then the characters selected are those
   --  characters from Start_Pos to the end of the text.
   --  Note that positions are specified in characters, not bytes.
   --  "start_pos": start of region
   --  "end_pos": end of region

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Insert_Text
     (Editable : Gtk_Editable;
      New_Text : UTF8_String;
      Position : in out Gint);
   --  Convenience subprogram, identical to Insert_Text above without
   --  the requirement to supply the New_Text_Length argument.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Editable_Void is not null access procedure (Self : Gtk_Editable);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : Gtk_Editable;
       Call  : Cb_Gtk_Editable_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : Gtk_Editable;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::changed signal is emitted at the end of a single user-visible
   --  operation on the contents of the Gtk.Editable.Gtk_Editable.
   --
   --  E.g., a paste operation that replaces the contents of the selection
   --  will cause only one signal emission (even though it is implemented by
   --  first deleting the selection, then inserting the new content, and may
   --  cause multiple ::notify::text signals to be emitted).

   type Cb_Gtk_Editable_Gint_Gint_Void is not null access procedure
     (Self      : Gtk_Editable;
      Start_Pos : Glib.Gint;
      End_Pos   : Glib.Gint);

   type Cb_GObject_Gint_Gint_Void is not null access procedure
     (Self      : access Glib.Object.GObject_Record'Class;
      Start_Pos : Glib.Gint;
      End_Pos   : Glib.Gint);

   Signal_Delete_Text : constant Glib.Signal_Name := "delete-text";
   procedure On_Delete_Text
      (Self  : Gtk_Editable;
       Call  : Cb_Gtk_Editable_Gint_Gint_Void;
       After : Boolean := False);
   procedure On_Delete_Text
      (Self  : Gtk_Editable;
       Call  : Cb_GObject_Gint_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when text is deleted from the widget by the
   --  user. The default handler for this signal will normally be responsible
   --  for deleting the text, so by connecting to this signal and then stopping
   --  the signal with g_signal_stop_emission, it is possible to modify the
   --  range of deleted text, or prevent it from being deleted entirely. The
   --  Start_Pos and End_Pos parameters are interpreted as for
   --  Gtk.Editable.Delete_Text.
   -- 
   --  Callback parameters:
   --    --  "start_pos": the starting position
   --    --  "end_pos": the end position

   type Cb_Gtk_Editable_UTF8_String_Gint_Gint_Void is not null access procedure
     (Self            : Gtk_Editable;
      New_Text        : UTF8_String;
      New_Text_Length : Glib.Gint;
      Position        : access Glib.Gint);

   type Cb_GObject_UTF8_String_Gint_Gint_Void is not null access procedure
     (Self            : access Glib.Object.GObject_Record'Class;
      New_Text        : UTF8_String;
      New_Text_Length : Glib.Gint;
      Position        : access Glib.Gint);

   Signal_Insert_Text : constant Glib.Signal_Name := "insert-text";
   procedure On_Insert_Text
      (Self  : Gtk_Editable;
       Call  : Cb_Gtk_Editable_UTF8_String_Gint_Gint_Void;
       After : Boolean := False);
   procedure On_Insert_Text
      (Self  : Gtk_Editable;
       Call  : Cb_GObject_UTF8_String_Gint_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when text is inserted into the widget by the
   --  user. The default handler for this signal will normally be responsible
   --  for inserting the text, so by connecting to this signal and then
   --  stopping the signal with g_signal_stop_emission, it is possible to
   --  modify the inserted text, or prevent it from being inserted entirely.
   -- 
   --  Callback parameters:
   --    --  "new_text": the new text to insert
   --    --  "new_text_length": the length of the new text, in bytes, or -1 if
   --    --  new_text is nul-terminated
   --    --  "position": the position, in characters, at which to insert the new
   --    --  text. this is an in-out parameter. After the signal emission is
   --    --  finished, it should point after the newly inserted text.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Editable"

   function "+" (W : Gtk_Editable) return Gtk_Editable;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Changed is access procedure (Editable : Gtk_Editable);
   pragma Convention (C, Virtual_Changed);

   type Virtual_Delete_Text is access procedure
     (Editable  : Gtk_Editable;
      Start_Pos : Glib.Gint;
      End_Pos   : Glib.Gint);
   pragma Convention (C, Virtual_Delete_Text);
   --  Deletes a sequence of characters. The characters that are deleted are
   --  those characters at positions from Start_Pos up to, but not including
   --  End_Pos. If End_Pos is negative, then the characters deleted are those
   --  from Start_Pos to the end of the text.
   --  Note that the positions are specified in characters, not bytes.
   --  "start_pos": start position
   --  "end_pos": end position

   type Virtual_Do_Delete_Text is access procedure
     (Editable  : Gtk_Editable;
      Start_Pos : Glib.Gint;
      End_Pos   : Glib.Gint);
   pragma Convention (C, Virtual_Do_Delete_Text);
   --  Deletes a sequence of characters. The characters that are deleted are
   --  those characters at positions from Start_Pos up to, but not including
   --  End_Pos. If End_Pos is negative, then the characters deleted are those
   --  from Start_Pos to the end of the text.
   --  Note that the positions are specified in characters, not bytes.
   --  "start_pos": start position
   --  "end_pos": end position

   type Virtual_Do_Insert_Text is access procedure
     (Editable        : Gtk_Editable;
      New_Text        : Gtkada.Types.Chars_Ptr;
      New_Text_Length : Glib.Gint;
      Position        : in out Glib.Gint);
   pragma Convention (C, Virtual_Do_Insert_Text);
   --  Inserts New_Text_Length bytes of New_Text into the contents of the
   --  widget, at position Position.
   --  Note that the position is in characters, not in bytes. The function
   --  updates Position to point after the newly inserted text.
   --  "new_text": the text to append
   --  "new_text_length": the length of the text in bytes, or -1
   --  "position": location of the position text will be inserted at

   type Virtual_Get_Chars is access function
     (Editable  : Gtk_Editable;
      Start_Pos : Glib.Gint;
      End_Pos   : Glib.Gint) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Chars);
   --  Retrieves a sequence of characters. The characters that are retrieved
   --  are those characters at positions from Start_Pos up to, but not
   --  including End_Pos. If End_Pos is negative, then the characters retrieved
   --  are those characters from Start_Pos to the end of the text.
   --  Note that positions are specified in characters, not bytes.
   --  "start_pos": start of text
   --  "end_pos": end of text

   type Virtual_Get_Position is access function (Editable : Gtk_Editable) return Glib.Gint;
   pragma Convention (C, Virtual_Get_Position);
   --  Retrieves the current position of the cursor relative to the start of
   --  the content of the editable.
   --  Note that this position is in characters, not in bytes.

   type Virtual_Get_Selection_Bounds is access function
     (Editable  : Gtk_Editable;
      Start_Pos : access Glib.Gint;
      End_Pos   : access Glib.Gint) return Glib.Gboolean;
   pragma Convention (C, Virtual_Get_Selection_Bounds);
   --  Retrieves the selection bound of the editable. start_pos will be filled
   --  with the start of the selection and End_Pos with end. If no text was
   --  selected both will be identical and False will be returned.
   --  Note that positions are specified in characters, not bytes.
   --  "start_pos": location to store the starting position, or null
   --  "end_pos": location to store the end position, or null

   type Virtual_Insert_Text is access procedure
     (Editable        : Gtk_Editable;
      New_Text        : Gtkada.Types.Chars_Ptr;
      New_Text_Length : Glib.Gint;
      Position        : in out Glib.Gint);
   pragma Convention (C, Virtual_Insert_Text);
   --  Inserts New_Text_Length bytes of New_Text into the contents of the
   --  widget, at position Position.
   --  Note that the position is in characters, not in bytes. The function
   --  updates Position to point after the newly inserted text.
   --  "new_text": the text to append
   --  "new_text_length": the length of the text in bytes, or -1
   --  "position": location of the position text will be inserted at

   type Virtual_Set_Position is access procedure (Editable : Gtk_Editable; Position : Glib.Gint);
   pragma Convention (C, Virtual_Set_Position);
   --  Sets the cursor position in the editable to the given value.
   --  The cursor is displayed before the character with the given (base 0)
   --  index in the contents of the editable. The value must be less than or
   --  equal to the number of characters in the editable. A value of -1
   --  indicates that the position should be set after the last character of
   --  the editable. Note that Position is in characters, not in bytes.
   --  "position": the position of the cursor

   type Virtual_Set_Selection_Bounds is access procedure
     (Editable  : Gtk_Editable;
      Start_Pos : Glib.Gint;
      End_Pos   : Glib.Gint);
   pragma Convention (C, Virtual_Set_Selection_Bounds);
   --  Selects a region of text. The characters that are selected are those
   --  characters at positions from Start_Pos up to, but not including End_Pos.
   --  If End_Pos is negative, then the characters selected are those
   --  characters from Start_Pos to the end of the text.
   --  Note that positions are specified in characters, not bytes.
   --  "start_pos": start of region
   --  "end_pos": end of region

   subtype Editable_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Changed
     (Self    : Editable_Interface_Descr;
      Handler : Virtual_Changed);
   pragma Import (C, Set_Changed, "gtkada_Editable_set_changed");

   procedure Set_Delete_Text
     (Self    : Editable_Interface_Descr;
      Handler : Virtual_Delete_Text);
   pragma Import (C, Set_Delete_Text, "gtkada_Editable_set_delete_text");

   procedure Set_Do_Delete_Text
     (Self    : Editable_Interface_Descr;
      Handler : Virtual_Do_Delete_Text);
   pragma Import (C, Set_Do_Delete_Text, "gtkada_Editable_set_do_delete_text");

   procedure Set_Do_Insert_Text
     (Self    : Editable_Interface_Descr;
      Handler : Virtual_Do_Insert_Text);
   pragma Import (C, Set_Do_Insert_Text, "gtkada_Editable_set_do_insert_text");

   procedure Set_Get_Chars
     (Self    : Editable_Interface_Descr;
      Handler : Virtual_Get_Chars);
   pragma Import (C, Set_Get_Chars, "gtkada_Editable_set_get_chars");

   procedure Set_Get_Position
     (Self    : Editable_Interface_Descr;
      Handler : Virtual_Get_Position);
   pragma Import (C, Set_Get_Position, "gtkada_Editable_set_get_position");

   procedure Set_Get_Selection_Bounds
     (Self    : Editable_Interface_Descr;
      Handler : Virtual_Get_Selection_Bounds);
   pragma Import (C, Set_Get_Selection_Bounds, "gtkada_Editable_set_get_selection_bounds");

   procedure Set_Insert_Text
     (Self    : Editable_Interface_Descr;
      Handler : Virtual_Insert_Text);
   pragma Import (C, Set_Insert_Text, "gtkada_Editable_set_insert_text");

   procedure Set_Set_Position
     (Self    : Editable_Interface_Descr;
      Handler : Virtual_Set_Position);
   pragma Import (C, Set_Set_Position, "gtkada_Editable_set_set_position");

   procedure Set_Set_Selection_Bounds
     (Self    : Editable_Interface_Descr;
      Handler : Virtual_Set_Selection_Bounds);
   pragma Import (C, Set_Set_Selection_Bounds, "gtkada_Editable_set_set_selection_bounds");
   --  See Glib.Object.Add_Interface

private

Null_Gtk_Editable : constant Gtk_Editable :=
   Gtk_Editable (Glib.Types.Null_Interface);
end Gtk.Editable;
