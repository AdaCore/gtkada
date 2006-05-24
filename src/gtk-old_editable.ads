-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-1999 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
--  This widget is an abstract widget designed to support the common
--  functionalities of all widgets for editing text. It provides general
--  services to manipulate an editable widget, a large number of action
--  signals used for key bindings, and several signals that an
--  application can connect to to modify the behavior of a widget.
--  </description>
--  <c_version>2.8.17</c_version>

with Gtk.Widget;

package Gtk.Old_Editable is

   type Gtk_Old_Editable_Record is new
     Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Old_Editable is access all Gtk_Old_Editable_Record'Class;

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   procedure Changed (Editable : access Gtk_Old_Editable_Record);
   pragma Obsolescent; --  Changed
   --  Cause the "changed" signal to be emitted.

   function Get_Type return Gtk.Gtk_Type;
   pragma Obsolescent; --  Get_Type
   --  Return the internal value associated with a Gtk_Old_Editable.

   procedure Claim_Selection
     (Editable : access Gtk_Old_Editable_Record;
      Claim    : in Boolean := True;
      Time     : in Guint32);
   pragma Obsolescent;  --  Claim_Selection
   --  If Claim is set to True, claim the ownership of the primary X selection.
   --  Otherwise, release it. "Time" should be set to the
   --  time of the last-change time for the specified selection. It is
   --  discarded if it is earlier than the current last-change time, or
   --  later than the current X server time.

   --  </doc_ignore>

   ---------------
   --  Signals  --
   ---------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "changed"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class);
   --
   --    emitted when the user has changed the text of the widget.
   --
   --  - "insert_text"
   --    procedure Handler (Widget   : access Gtk_Old_Editable_Record'Class;
   --                       Text     : in UTF8_String;
   --                       Length   : in Gint;
   --                       Position : in Gint_Access);
   --
   --    Emitted when some text is inserted inside the widget by the
   --    user. The default handler inserts the text into the widget.
   --    By connecting a handler to this signal, and then by stopping
   --    the signal with Gtk.Handlers.Emit_Stop_By_Name, it is possible
   --    to modify the inserted text, or even prevent it from being
   --    inserted.
   --    Position.all should be modified by the callback, and indicates
   --    the new position of the cursor after the text has been inserted.
   --
   --  - "delete_text"
   --    procedure Handler (Widget    : access Gtk_Old_Editable_Record'Class;
   --                       Start_Pos : in Gint;
   --                       End_Pos   : in Gint);
   --
   --    Emitted when some text is deleted by the user. As for the
   --    "insert-text" handler, it is possible to override the default
   --    behavior by connecting a handler to this signal, and then
   --    stopping the signal.
   --
   --  - "activate"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class);
   --
   --    Emitted when the user has activated the widget in some fashion.
   --
   --  - "set-editable"
   --    procedure Handler (Widget     : access Gtk_Old_Editable_Record'Class;
   --                       Is_Editable: in Boolean);
   --
   --    Emitting this signal is equivalent to calling Set_Old_Editable.
   --
   --  - "move_cursor"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class;
   --                       X, Y   : in Gint);
   --
   --    Emitting this signal will move the cursor position for X
   --    characters horizontally, and Y characters vertically.
   --
   --  - "move_word"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class;
   --                       N      : in Gint);
   --
   --    Emitting this signal will move the cursor by N words (N can be
   --    negative).
   --
   --  - "move_page"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class;
   --                       X, Y   : in Gint);
   --
   --    Emitting this signal will move the cursor for X pages
   --    horizontally, and Y pages vertically.
   --
   --  - "move_to_row"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class;
   --                       Row    : in Gint);
   --
   --    Emitting this signal will move the cursor to the given row.
   --
   --  - "move_to_column"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class;
   --                       Column : in Gint);
   --
   --    Emitting this signal will move the cursor to the given column.
   --
   --  - "kill_char"
   --    procedure Handler (Widget    : access Gtk_Old_Editable_Record'Class;
   --                       Direction : in Gint);
   --
   --    Emitting this signal deletes a single character. If Direction
   --    is positive, delete forward, else delete backward.
   --
   --  - "kill_word"
   --    procedure Handler (Widget    : access Gtk_Old_Editable_Record'Class;
   --                       Direction : in Gint);
   --
   --    Emitting this signal deletes a single word. If Direction is
   --    positive, delete forward, otherwise delete backward.
   --
   --  - "kill_line"
   --    procedure Handler (Widget    : access Gtk_Old_Editable_Record'Class;
   --                       Direction : in Gint);
   --
   --    Emitting this signal deletes a single line. If Direction is
   --    positive, delete forward, otherwise delete backward.
   --
   --  - "cut_clipboard"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class);
   --
   --    Emitting this signal will cut the current selection to the
   --    clipboard.
   --
   --  - "copy_clipboard"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class);
   --
   --    Emitting this signal will copy the current selection to the
   --    clipboard.
   --
   --  - "paste_clipboard"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class);
   --
   --    Emitting this signal will paste the clipboard into the text
   --    of the widget at the current cursor position.
   --
   --  </signals>

   Signal_Activate : constant String := "activate";
   Signal_Copy_Clipboard : constant String := "copy_clipboard";
   Signal_Cut_Clipboard : constant String := "cut_clipboard";
   Signal_Kill_Char : constant String := "kill_char";
   Signal_Kill_Line : constant String := "kill_line";
   Signal_Kill_Word : constant String := "kill_word";
   Signal_Move_Cursor : constant String := "move_cursor";
   Signal_Move_Page : constant String := "move_page";
   Signal_Move_To_Column : constant String := "move_to_column";
   Signal_Move_To_Row : constant String := "move_to_row";
   Signal_Move_Word : constant String := "move_word";
   Signal_Paste_Clipboard : constant String := "paste_clipboard";
   Signal_Set_Editable : constant String := "set-editable";

private
   type Gtk_Old_Editable_Record is new Gtk.Widget.Gtk_Widget_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_old_editable_get_type");
end Gtk.Old_Editable;
