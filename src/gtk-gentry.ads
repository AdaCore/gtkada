-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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
--
--  A Gtk_Entry is a single line text editing widget.
--  The text is automatically scrolled if it is longer than can be displayed
--  on the screen, so that the cursor position is visible at all times.
--
--  See also Gtk_Text for a multiple-line text editing widget.
--
--  Note that this widget does not currently support wide-character, or
--  character sets that require multiple-byte encoding.
--
--  </description>
--  <c_version>1.3.4</c_version>

with Gtk.Widget;

package Gtk.GEntry is

   --  <doc_ignore>
   type Gtk_Entry_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Entry is access all Gtk_Entry_Record'Class;
   subtype Gtk_GEntry is Gtk_Entry;
   --  </doc_ignore>

   procedure Gtk_New (Widget : out Gtk_Entry; Max : Guint16);
   --  Create a new entry with a maximum length for the text.
   --  The text can never be longer than Max characters.
   --  pragma Deprecated (Gtk_New);

   procedure Gtk_New (Widget : out Gtk_Entry);
   --  Create a new entry with no maximum length for the text

   procedure Initialize
     (Widget : access Gtk_Entry_Record'Class; Max : Guint16);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Initialize (Widget : access Gtk_Entry_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Entry.

   procedure Set_Editable
     (The_Entry : access Gtk_Entry_Record; Editable : Boolean);

   procedure Set_Visibility
     (The_Entry : access Gtk_Entry_Record; Visible : Boolean);
   --  Set the visibility of the characters in the entry.
   --  If Visible is set to False, the characters will be replaced with
   --  starts ('*') in the display, and when the text is copied elsewhere.

   procedure Set_Max_Length
     (The_Entry : access Gtk_Entry_Record; Max : Guint16);
   --  Set the maximum length for the text.
   --  The current text is truncated if needed.

   procedure Set_Text (The_Entry : access Gtk_Entry_Record; Text : String);
   --  Modify the text in the entry.
   --  The text is cut at the maximum length that was set when the entry was
   --  created.
   --  The text replaces the current contents.

   function Get_Text (The_Entry : access Gtk_Entry_Record) return String;
   --  Return the current text written in the entry.

   procedure Append_Text
     (The_Entry : access Gtk_Entry_Record; Text : String);
   --  Append a new string at the end of the existing one.
   --  pragma Deprecated (Append_Text);

   procedure Prepend_Text
     (The_Entry : access Gtk_Entry_Record; Text : String);
   --  Insert some text at the beginning of the entry.
   --  pragma Deprecated (Prepend_Text);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  - "insert_text"
   --  - "delete_text"
   --  - "changed"
   --  - "populate_popup"
   --  - "activate"
   --  - "move_cursor"
   --  - "insert_at_cursor"
   --  - "delete_from_cursor"
   --  - "cut_clipboard"
   --  - "copy_clipboard"
   --  - "paste_clipboard"
   --  - "toggle_overwrite"
   --  </signals>

private
   type Gtk_Entry_Record is new
     Gtk.Widget.Gtk_Widget_Record with null record;
   pragma Import (C, Get_Type, "gtk_entry_get_type");
end Gtk.GEntry;

--  missing:
--  procedure Set_Invisible_Char
--    (GtkEntry *entry, gunichar ch);

--  procedure Set_Has_Frame
--    (GtkEntry *entry, gboolean setting);

--  function Get_Has_Frame (GtkEntry *entry) return Boolean;

--  procedure Set_Activates_Default
--    (GtkEntry *entry, gboolean setting);

--  function Get_Activates_Default (GtkEntry *entry) return Boolean;

--  procedure Set_Width_Chars
--    (GtkEntry *entry, gint n_chars);

--  function Get_Width_Chars (GtkEntry *entry) return Gint;

