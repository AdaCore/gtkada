-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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
--  See Gtk_Text_View for a multiple-line text editing widget.
--
--  </description>
--  <c_version>1.3.11</c_version>

with Glib.Properties;
with Gtk.Editable;
with Pango.Layout;

package Gtk.GEntry is

   --  <doc_ignore>
   type Gtk_Entry_Record is new Gtk.Editable.Gtk_Editable_Record with private;
   --  Gtk_Entry is actually a child of Gtk_Widget, and implements the
   --  Gtk_Editable interface, but GtkAda does not support yet interfaces,
   --  so use direct inheritance for now ???

   type Gtk_Entry is access all Gtk_Entry_Record'Class;
   subtype Gtk_GEntry is Gtk_Entry;

   procedure Gtk_New (Widget : out Gtk_Entry; Max : Gint);
   --  Create a new entry with a maximum length for the text.
   --  The text can never be longer than Max characters.
   --  pragma Deprecated (Gtk_New);
   --  </doc_ignore>

   procedure Gtk_New (Widget : out Gtk_Entry);
   --  Create a new entry with no maximum length for the text

   procedure Initialize
     (Widget : access Gtk_Entry_Record'Class; Max : Gint);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Initialize (Widget : access Gtk_Entry_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Entry.

   procedure Set_Visibility
     (The_Entry : access Gtk_Entry_Record; Visible : Boolean);
   --  Set the visibility of the characters in the entry.
   --  If Visible is set to False, the characters will be replaced with
   --  the invisible character ('*' by default) in the display, and when the
   --  text is copied elsewhere.

   function Get_Visibility
     (The_Entry : access Gtk_Entry_Record) return Boolean;
   --  Return the visibility of the characters in the entry.

   procedure Set_Invisible_Char
     (The_Entry : access Gtk_Entry_Record; Char : Gunichar);
   --  Set the character to use in place of the actual text when
   --  Set_Visibility has been called to set text visibility to False.
   --  i.e. this is the character used in "password mode" to
   --  show the user how many characters have been typed. The default
   --  invisible char is an asterisk ('*'). If you set the invisible char
   --  to 0, then the user will get no feedback at all; there will be
   --  no text on the screen as they type.

   function Get_Invisible_Char
     (The_Entry : access Gtk_Entry_Record) return Gunichar;
   --  Retrieve the character displayed in place of the real characters
   --  for entries with visisbility set to false. See Set_Invisible_Char.

   procedure Set_Has_Frame
     (The_Entry : access Gtk_Entry_Record; Setting : Boolean := True);
   --  Set whether the entry has a beveled frame around it.

   function Get_Has_Frame
     (The_Entry : access Gtk_Entry_Record) return Boolean;
   --  Return whether the entry has a beveled frame.

   procedure Set_Max_Length
     (The_Entry : access Gtk_Entry_Record; Max : Gint);
   --  Set the maximum length for the text.
   --  The current text is truncated if needed.

   function Get_Max_Length (The_Entry : access Gtk_Entry_Record) return Gint;
   --  Return the maximum length for the text.

   procedure Set_Activates_Default
     (The_Entry : access Gtk_Entry_Record; Setting : Boolean);
   --  If Setting is True, pressing Enter in the Entry will activate the
   --  default widget for the window containing the entry. This usually means
   --  that the dialog box containing the entry will be closed, since the
   --  default widget is usually one of the dialog buttons.
   --
   --  (For experts: if Setting is True, the entry calls
   --  Gtk.Window.Activate_Default on the window containing the entry, in
   --  the default handler for the "activate" signal.)

   function Get_Activates_Default
     (The_Entry : access Gtk_Entry_Record) return Boolean;
   --  Return whether the entry will activate the default widget.

   procedure Set_Width_Chars
     (The_Entry : access Gtk_Entry_Record'Class; Width : Gint);
   --  Number of characters to leave space for in the entry, on the screen.
   --  This is the number of visible characters, not the maximal number of
   --  characters the entry can contain

   function Get_Width_Chars
     (The_Entry : access Gtk_Entry_Record'Class) return Gint;
   --  Return number of characters to leave space for in the entry.

   procedure Set_Text (The_Entry : access Gtk_Entry_Record; Text : String);
   --  Modify the text in the entry.
   --  The text is cut at the maximum length that was set when the entry was
   --  created.
   --  The text replaces the current contents.

   function Get_Text (The_Entry : access Gtk_Entry_Record) return String;
   --  Return the current text written in the entry.

   procedure Get_Layout_Offsets
     (The_Entry : access Gtk_Entry_Record;
      X         : out Gint;
      Y         : out Gint);
   --  Obtain the position of the Pango_Layout used to render text
   --  in the entry, in widget coordinates. Useful if you want to line
   --  up the text in an entry with some other text, e.g. when using the
   --  entry to implement editable cells in a sheet widget.
   --
   --  Also useful to convert mouse events into coordinates inside the
   --  Pango_Layout, e.g. to take some action if some part of the entry text
   --  is clicked.
   --
   --  Note that as the user scrolls around in the entry the offsets will
   --  change; you'll need to connect to the "notify::scroll_offset"
   --  signal to track this. Remember when using the Pango_Layout
   --  functions you need to convert to and from pixels using
   --  Pango_Pixels or Pango_Scale.

   function Get_Layout (The_Entry : access Gtk_Entry_Record)
      return Pango.Layout.Pango_Layout;
   --  Return the widget that manages all the layout of text (left-to-right,
   --  right-to-left, fonts,...). Changing the font used for the entry should
   --  be done by changing the font using for this layout.
   --
   --  The layout is useful to e.g. convert text positions to pixel positions,
   --  in combination with Get_Layout_Offsets.  The returned layout is owned by
   --  the entry so need not be freed by the caller.

   --  <doc_ignore>
   function Get_Chars (The_Entry : access Gtk_Entry_Record) return String
     renames Get_Text;
   --  Convenience function provided for compatibility with GtkAda 1.2

   procedure Append_Text
     (The_Entry : access Gtk_Entry_Record; Text : String);
   --  Append a new string at the end of the existing one.
   --  pragma Deprecated (Append_Text);

   procedure Prepend_Text
     (The_Entry : access Gtk_Entry_Record; Text : String);
   --  Insert some text at the beginning of the entry.
   --  pragma Deprecated (Prepend_Text);

   procedure Set_Editable
     (The_Entry : access Gtk_Entry_Record; Editable : Boolean);
   --  pragma Deprecated (Set_Editable);
   --  </doc_ignore>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  Text_Position_Property
   --    Type:  Gint
   --    Flags: read-write
   --    Descr: The current position of the insertion point
   --    See also:  ???
   --
   --  - Name:  Editable_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether the entry contents can be edited
   --    See also:  Set_Editable
   --
   --  - Name:  Max_Length_Property
   --    Type:  Gint
   --    Flags: read-write
   --    Descr: Maximum number of characters for this entry
   --    See also:  Set_Max_Length
   --
   --  - Name:  Visibility_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: FALSE displays the "invisible char" instead of the actual
   --           text (password mode)
   --    See also:  Set_Visibility
   --
   --  - Name:  Invisible_Char_Property
   --    Type:  Gunichar
   --    Flags: read-write
   --    Descr: The character to use when masking entry contents
   --           (in "password mode")
   --    See also: <none>
   --
   --  - Name:  Activates_Default_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether to activate the default widget (such as the default
   --           button in a dialog) when Enter is pressed.)
   --    See also: <none>
   --
   --  - Name:  Width_Chars_Property
   --    Type:  Gint
   --    Flags: read-write
   --    Descr: Number of characters to leave space for in the entry.
   --    See also: Set_Width_Chars
   --
   --  - Name:  Scroll_Offset_Property
   --    Type:  Gint
   --    Flags: read only
   --    Descr: Number of pixels of the entry scrolled off the screen to the
   --           left
   --    See also: <none>
   --
   --  </properties>

   Text_Position_Property : constant Glib.Properties.Property_Int;
   Editable_Property : constant Glib.Properties.Property_Boolean;
   Max_Length_Property : constant Glib.Properties.Property_Int;
   Visibility_Property : constant Glib.Properties.Property_Boolean;
   Invisible_Char_Property : constant Glib.Properties.Property_Unichar;
   Activates_Default_Property : constant Glib.Properties.Property_Boolean;
   Width_Chars_Property : constant Glib.Properties.Property_Int;
   Scroll_Offset_Property : constant Glib.Properties.Property_Int;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  - "activate"
   --  - "populate_popup"
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
     Gtk.Editable.Gtk_Editable_Record with null record;

   Text_Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("text_position");
   Editable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editable");
   Max_Length_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("max_length");
   Visibility_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visibility");
   Invisible_Char_Property : constant Glib.Properties.Property_Unichar :=
     Glib.Properties.Build ("invisible_char");
   Activates_Default_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("activates_default");
   Width_Chars_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width_chars");
   Scroll_Offset_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("scroll_offset");

   pragma Import (C, Get_Type, "gtk_entry_get_type");
end Gtk.GEntry;

