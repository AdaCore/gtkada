-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                  Copyright (C) 2001 ACT-Europe                    --
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
--  This widget emulates the Gtk_Text widget provided in the GtkAda-1.2
--  series.  It is not 100% compatible with the genuine GtkText, but is
--  provided here in order to facilitate the transition from GtkAda-1.2 to
--  GtkAda-2.0.
--
--  This widget displays any given text that can be manipulated by both the
--  user and the programmer. The text can optionally be interactively
--  modified by the user. Different colors can be used for any given part
--  of the text.
--
--  </description>

with Gdk.Color;
with Gdk.Font;
with Gdk.Window;
with Gtk.Adjustment;
with Gtk.Text_View;

package Gtk.Text is

   type Gtk_Text_Record is new Gtk.Text_View.Gtk_Text_View_Record with private;
   type Gtk_Text is access all Gtk_Text_Record'Class;

   procedure Gtk_New
     (Text : out Gtk_Text;
      Hadj : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment;
      Vadj : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment);
   --  Create a new text widget.
   --  You need to insert the Gtk_Text in a Gtk_Scrolled_Window to make
   --  the scrollbars visible.

   procedure Initialize
     (Text : access Gtk_Text_Record'Class;
      Hadj : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment;
      Vadj : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   ---------------------------
   -- Gtk_Editable services --
   ---------------------------

   procedure Changed (Editable : access Gtk_Text_Record);
   --  Cause the "changed" signal to be emitted.

   --  procedure Claim_Selection
   --    (Editable : access Gtk_Text_Record;
   --     Claim    : in Boolean := True;
   --     Time     : in Guint32);
   --  Not implemented.

   procedure Copy_Clipboard
     (Editable : access Gtk_Text_Record;
      Time     : Guint32);
   --  Copy the characters in the current selection to the clipboard.
   --  Time is ignored.

   procedure Cut_Clipboard
     (Editable : access Gtk_Text_Record;
      Time     : Guint32);
   --  Copy the characters in the current selection to the clipboard.
   --  The selection is then deleted.
   --  Time is ignored.

   procedure Delete_Selection (Editable : access Gtk_Text_Record);
   --  Disclaim and delete the current selection.

   procedure Delete_Text
     (Editable  : access Gtk_Text_Record;
      Start_Pos : Gint := 0;
      End_Pos   : Gint := -1);
   --  Delete the characters from Start_Pos to End_Pos.
   --  If End_Pos is negative, the characters are deleted from Start_Pos to the
   --  end of the text.

   function Get_Chars
      (Editable  : access Gtk_Text_Record;
       Start_Pos : Gint := 0;
       End_Pos   : Gint := -1) return String;
   --  Get the text from Start_Pos to End_Pos.
   --  If End_Pos is negative, the text from Start_Pos to the end is returned.

   --  function Get_Clipboard_Text
   --    (Widget : access Gtk_Text_Record) return String;
   --  Not implemented.

   --  function Get_Has_Selection
   --    (Widget : access Gtk_Text_Record) return Boolean;
   --  Not implemented.

   function Get_Selection_End_Pos
     (Widget : access Gtk_Text_Record) return Guint;
   --  Return the position of the end of the current selection.

   function Get_Selection_Start_Pos
     (Widget : access Gtk_Text_Record) return Guint;
   --  Return the position of the beginning of the current selection.

   procedure Insert_Text
     (Editable : access Gtk_Text_Record;
      New_Text : String;
      Position : in out Gint);
   --  Insert the given string at the given position.
   --  Position is set to the new cursor position.

   procedure Paste_Clipboard
     (Editable : access Gtk_Text_Record;
      Time     : Guint32);
   --  The contents of the clipboard is pasted into the given widget at
   --  the current cursor position.
   --  Time is ignored.

   procedure Select_Region
     (Editable : access Gtk_Text_Record;
      Start    : Gint;
      The_End  : Gint := -1);
   --  Select the region of text from Start to The_End.
   --  The characters that are selected are those characters at positions
   --  from Start up to, but not including The_End. If The_End_Pos is
   --  negative, then the characters selected will be those characters
   --  from Start to the end of the text.

   procedure Set_Position
     (Editable : access Gtk_Text_Record;
      Position : Gint);
   --  Change the position of the cursor in the entry.
   --  The cursor is displayed before the character with the given
   --  index in the widget (the first character has index 0). The
   --  value must be less than or equal to the number of characters in the
   --  widget. A value of -1 indicates that the position
   --  should be set after the last character in the entry.
   --  Note that this position is in characters, not in bytes.

   function Get_Position (Editable : access Gtk_Text_Record) return Gint;
   --  Return the position of the cursor.

   -----------------------
   -- Gtk_Text services --
   -----------------------

   function Get_Text_Area (Text : access Gtk_Text_Record)
     return Gdk.Window.Gdk_Window;
   --  Return the specific window into which the text is displayed.
   --  Note that a Gtk_Text is in fact a complex widget, which includes borders
   --  on the sides. Thus, whenever you want to convert the mouse coordinates
   --  to a position in the text, you should use the Gdk.Window.Get_Pointer
   --  function, passing it this text area as the origin window, rather than
   --  directly Get_Window (Text).
   --  Note that null will be returned while Text hasn't been realized.

   function Backward_Delete (Text : access Gtk_Text_Record;
                             Nchars : Guint)
                            return Boolean;
   --  Backward delete Nchars characters from the current cursor position.
   --  There must be at least Nchars characters to delete before the
   --  pointer, or the operation will not be performed.
   --  Return True if the operation was successful, False otherwise.

   function Forward_Delete (Text : access Gtk_Text_Record;
                            Nchars : Guint)
                           return Boolean;
   --  Forward delete Nchars characters from the current point position.
   --  There must be at least Nchars characters to delete after the
   --  pointer, or the operation will not be performed.
   --  Return True if the operation was successful, False otherwise.

   procedure Freeze (Text : access Gtk_Text_Record);
   --  ??? Can this procedure be easily implemented?
   --  ??? Does nothing at the moment.
   --  Freeze the Gtk_Text widget.
   --  In other words, stop any redrawing of the widget until the Thaw
   --  operation is called. This operation is useful when
   --  a large number of changes need to be made within the widget.
   --  Freezing it during the updates will avoid some flicker seen by
   --  the user.
   --  Note also that an internal counter is incremented. The updates will
   --  be performed only when the same numbers of calls to Thaw has been
   --  performed.
   --
   --  Note that you can not call Set_Position while the widget is frozen.
   --  This will create a Storage_Error otherwise.

   procedure Thaw (Text : access Gtk_Text_Record);
   --  ??? Can this procedure be easily implemented?
   --  ??? Does nothing at the moment.
   --  Cancel the previous call to Freeze.
   --  Allow the widget to be redrawn again, when Thaw has been called as
   --  many times as Freeze.

   function Get_Hadj (Text : access Gtk_Text_Record)
                     return Gtk.Adjustment.Gtk_Adjustment;
   --  Return the horizontal scrollbar saved inside Text.

   function Get_Vadj (Text : access Gtk_Text_Record)
                     return Gtk.Adjustment.Gtk_Adjustment;
   --  Return the vertical scrollbar saved inside Text.

   function Get_Length (Text : access Gtk_Text_Record) return Guint;
   --  Return the total length of the text (the number of characters)
   --  contained within the text widget.

   procedure Set_Point
     (Text  : access Gtk_Text_Record;
      Index : Guint);

   function Get_Point (Text : access Gtk_Text_Record) return Guint;
   --  Get the current position of the insertion point (cursor).
   --  Return the number of characters from the upper left corner of the
   --  widget.

   --  procedure Set_Point (Text  : access Gtk_Text_Record;
   --                       Index : in Guint);
   --  Not implemented (not supported).

   procedure Insert
     (Text   : access Gtk_Text_Record;
      Font   : Gdk.Font.Gdk_Font := Gdk.Font.Null_Font;
      Fore   : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Back   : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Chars  : String := "";
      Length : Gint := -1);
   --  Insert the given string (Chars) inside the text of the text widget.
   --  Use the specified Font, foreground (Fore) and background
   --  (Back) colors. Only the first "Length" characters are inserted,
   --  unless Length is set to -1, in which case the complete string is
   --  inserted.
   --  Note that the colors must be allocated first, and the font loaded.
   --  If the default parameters are passed for font and colors, the text
   --  widget will use the ones defined in the style for Text (see Gtk.Style
   --  for more information about styles).
   --
   --  Note that Font is ignored (not supported as is).

   procedure Set_Adjustments (Text : access Gtk_Text_Record;
                              Hadj : Gtk.Adjustment.Gtk_Adjustment;
                              Vadj : Gtk.Adjustment.Gtk_Adjustment);
   --  Set the horizontal and vertical adjustments associated with Text.

   --  procedure Set_Editable (Text     : access Gtk_Text_Record;
   --                          Editable : in Boolean := True);
   --  Inherited from Gtk_Text_View.

   procedure Set_Line_Wrap (Text      : access Gtk_Text_Record;
                            Line_Wrap : Boolean := True);
   --  Set the Line_Wrap state of the given text widget.
   --  If set to True, the line is broken when it reaches the extent of the
   --  widget viewing area and the rest is displayed on the next line. If set
   --  to false, the line continues regardless of the size of current
   --  viewing area.

   procedure Set_Word_Wrap (Text      : access Gtk_Text_Record;
                            Word_Wrap : Boolean := True);
   --  Set the Word_Wrap state of the given text widget.
   --  If set to True, words are wrapped down to the next line if they can't
   --  be completed on the current line.
   --
   --  Note that calling Set_Word_Wrap implies a call to Set_Line_Wrap.
   --  Line wrapping will still be active after calling Set_Word_Wrap
   --  with Word_Wrap set to false. To completely deactivate wrapping,
   --  use Set_Line_Wrap.

   --  procedure Claim_Selection
   --    (Text  : access Gtk_Text_Record;
   --     Claim : Boolean := True;
   --     Time  : Guint32);
   --  Not implemented.

private

   type Gtk_Text_Record is
     new Gtk.Text_View.Gtk_Text_View_Record with null record;

end Gtk.Text;
