-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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
--  This widget displays any given text that can be manipulated by
--  both the user and the programmer.
--  The text can optionaly be interactively modified by the user.
--  Different colors and fonts can be used for any given part of the
--  text. The background can have any color, or even be a pixmap.
--
--  </description>
--  <c_version>1.2.6</c_version>

with Gdk.Color;
with Gdk.Font;
with Gtk.Object; use Gtk.Object;
with Gtk.Adjustment;
with Gtk.Editable;

package Gtk.Text is

   type Gtk_Text_Record is new Gtk.Editable.Gtk_Editable_Record with private;
   type Gtk_Text is access all Gtk_Text_Record'Class;

   procedure Gtk_New
     (Text : out Gtk_Text;
      Hadj : in Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment;
      Vadj : in Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment);
   --  Create a new text widget with the given adjustments.
   --  If either or both scrollbars is not provided, the text widget will
   --  create its own.
   --  You need to insert the Gtk_Text in a Gtk_Scrolled_Window to make
   --  the scrollbars visible. Not also that this widget does not currently
   --  support horizontal scrollbars.

   procedure Initialize
     (Text : access Gtk_Text_Record'Class;
      Hadj : in Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment;
      Vadj : in Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Text.

   function Backward_Delete (Text : access Gtk_Text_Record;
                             Nchars : in Guint)
                            return Boolean;
   --  Backward delete Nchars characters from the current cursor position.
   --  There must be at least Nchars characters to delete before the
   --  pointer, or the operation will not be performed.
   --  Return True if the operation was succesful, False otherwise.

   function Forward_Delete (Text : access Gtk_Text_Record;
                            Nchars : in Guint)
                           return Boolean;
   --  Forward delete Nchars characters from the current point position.
   --  There must be at least Nchars characters to delete after the
   --  pointer, or the operation will not be performed.
   --  Return True if the operation was succesful, False otherwise.

   procedure Freeze (Text : access Gtk_Text_Record);
   --  Freeze the Gtk_Text widget.
   --  In other words, stop any redrawing of the widget until the Thaw
   --  operation is called. This operation is useful when
   --  a large number of changes need to be made within the widget.
   --  Freezing it during the updates will avoid some flicker seen by
   --  the user.
   --  Note also that an internal counter is incremented. The updates will
   --  be performed only when the same numbers of calls to Thaw has been
   --  performed.

   procedure Thaw (Text : access Gtk_Text_Record);
   --  Cancel the previous call to Freeze.
   --  Allow the widget to be redrawn again, when Thaw has been called as
   --  many times as Freeze.

   --  <doc_ignore>
   function Get_Gap_Position (Text : access Gtk_Text_Record) return Guint;
   function Get_Gap_Size (Text : access Gtk_Text_Record) return Guint;
   function Get_Text_End (Text : access Gtk_Text_Record) return Guint;
   --  Those 2 functions should probably be deleted.
   --  </doc_ignore>

   function Get_Hadj (Text : access Gtk_Text_Record)
                     return Gtk.Adjustment.Gtk_Adjustment;
   --  Return the horizontal scrollbar associated with Text.

   function Get_Vadj (Text : access Gtk_Text_Record)
                     return Gtk.Adjustment.Gtk_Adjustment;
   --  Return the vertical scrollbar associated to the given text widget.

   function Get_Length (Text : access Gtk_Text_Record) return Guint;
   --  Return the total length of the text contained within the text widget.

   function Get_Point (Text : access Gtk_Text_Record) return Guint;
   --  Get the current position of the cursor.
   --  Return the number of characters from the upper left corner of the
   --  widget.

   procedure Set_Point (Text  : access Gtk_Text_Record;
                        Index : in Guint);
   --  Set the cursor position.

   --  <doc_ignore>
   function Get_Text (Text : access Gtk_Text_Record) return String;
   --  Should probably be deleted (does not work, fails to capture
   --  user changes). Use Gtk.Editable.Get_Chars instead.
   --  </doc_ignore>

   procedure Insert
     (Text   : access Gtk_Text_Record;
      Font   : in Gdk.Font.Gdk_Font'Class;
      Fore   : in Gdk.Color.Gdk_Color;
      Back   : in Gdk.Color.Gdk_Color;
      Chars  : in String;
      Length : in Gint := -1);
   --  Insert the given string (Chars) inside the text of the text widget.
   --  Use the specified Font, foreground (Fore) and background
   --  (Back) colors. Only the first "Length" characters are inserted,
   --  unless Length is set to -1, in which case the complete string is
   --  inserted.
   --  Note that the colors must be allocated first, and the font loaded.

   procedure Set_Adjustments (Text : access Gtk_Text_Record;
                              Hadj : Gtk.Adjustment.Gtk_Adjustment;
                              Vadj : Gtk.Adjustment.Gtk_Adjustment);
   --  Set the horizontal and vertical adjustments associated with Text.

   procedure Set_Editable (Text     : access Gtk_Text_Record;
                           Editable : in Boolean := True);
   --  Toggle the editable state of the given text widget.
   --  This determines whether the user can edit the text or not. Note that
   --  the programmer can still perform any update.

   procedure Set_Line_Wrap (Text      : access Gtk_Text_Record;
                            Line_Wrap : in Boolean := True);
   --  Set the Line_Wrap state of the given text widget.
   --  If set to True, the line is broken when it reaches the extent of the
   --  widget viewing area and the rest is displayed on the next line. If set
   --  to false, the line continues regardless of the size of current
   --  viewing area.

   procedure Set_Word_Wrap (Text      : access Gtk_Text_Record;
                            Word_Wrap : in Boolean := True);
   --  Set the Word_Wrap state of the given text widget.
   --  If set to True, words are wrapped down to the next line if they can't
   --  be completed on the current line.

   ----------------------------
   -- Support for Gate/Dgate --
   ----------------------------

   procedure Generate (N : in Node_Ptr; File : in File_Type);
   --  Gate internal function

   procedure Generate (Text : in out Gtk_Object; N : in Node_Ptr);
   --  Dgate internal function

private
   type Gtk_Text_Record is new Gtk.Editable.Gtk_Editable_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_text_get_type");
end Gtk.Text;
