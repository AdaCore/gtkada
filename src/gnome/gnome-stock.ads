-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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
--  These functions provide an applications programmer with default
--  icons for toolbars, menu pixmaps, etc. One such `icon' should have
--  at least three pixmaps to reflect it's state. There is a `regular'
--  pixmap, a `disabled' pixmap and a `focused' pixmap. You can get
--  either each of these pixmaps by calling Gnome.Stock.Pixmap or you
--  can get a widget by calling Gnome.Stock.Pixmap_Widget. This widget
--  is a container which shows the pixmap, that is
--  reflecting the current state of the widget. If for example you
--  Gtk.Container.Add this widget to a button, which is currently not
--  sensitive, the widget will just show the `disabled' pixmap. If the
--  state of the button changes to sensitive, the widget will change to
--  the `regular' pixmap. The `focused' pixmap will be shown, when the
--  mouse pointer enters the widget.
--
--  We now have stock buttons too. To use them, just replace any
--  Gtk.Button.Gtk_New with Gnome.Stock.Button (Button_...).
--  This function returns a Gtk_Button with a gettexted default text and an
--  icon.
--  </description>

with Gtk.Button;
with Gtk.Menu_Item;

package Gnome.Stock is

   --  The names of `well known' icons. I define these strings mainly to
   --  prevent errors due to typos.

   Pixmap_New         : constant String := "New" & ASCII.NUL;
   Pixmap_Open        : constant String := "Open" & ASCII.NUL;
   Pixmap_Close       : constant String := "Close" & ASCII.NUL;
   Pixmap_Revert      : constant String := "Revert" & ASCII.NUL;
   Pixmap_Save        : constant String := "Save" & ASCII.NUL;
   Pixmap_Save_As     : constant String := "Save As" & ASCII.NUL;
   Pixmap_Cut         : constant String := "Cut" & ASCII.NUL;
   Pixmap_Copy        : constant String := "Copy" & ASCII.NUL;
   Pixmap_Paste       : constant String := "Paste" & ASCII.NUL;
   Pixmap_Clear       : constant String := "Clear" & ASCII.NUL;
   Pixmap_Properties  : constant String := "Properties" & ASCII.NUL;
   Pixmap_Preferences : constant String := "Preferences" & ASCII.NUL;
   Pixmap_Help        : constant String := "Help" & ASCII.NUL;
   Pixmap_Scores      : constant String := "Scores" & ASCII.NUL;
   Pixmap_Print       : constant String := "Print" & ASCII.NUL;
   Pixmap_Search      : constant String := "Search" & ASCII.NUL;
   Pixmap_Srchrpl     : constant String := "Search/Replace" & ASCII.NUL;
   Pixmap_Back        : constant String := "Back" & ASCII.NUL;
   Pixmap_Forward     : constant String := "Forward" & ASCII.NUL;
   Pixmap_First       : constant String := "First" & ASCII.NUL;
   Pixmap_Last        : constant String := "Last" & ASCII.NUL;
   Pixmap_Home        : constant String := "Home" & ASCII.NUL;
   Pixmap_Stop        : constant String := "Stop" & ASCII.NUL;
   Pixmap_Refresh     : constant String := "Refresh" & ASCII.NUL;
   Pixmap_Undo        : constant String := "Undo" & ASCII.NUL;
   Pixmap_Redo        : constant String := "Redo" & ASCII.NUL;
   Pixmap_Timer       : constant String := "Timer" & ASCII.NUL;
   Pixmap_Timer_Stop  : constant String := "Timer Stopped" & ASCII.NUL;
   Pixmap_Mail        : constant String := "Mail" & ASCII.NUL;
   Pixmap_Mail_Rcv    : constant String := "Receive Mail" & ASCII.NUL;
   Pixmap_Mail_Snd    : constant String := "Send Mail" & ASCII.NUL;
   Pixmap_Mail_Rpl    : constant String := "Reply to Mail" & ASCII.NUL;
   Pixmap_Mail_Fwd    : constant String := "Forward Mail" & ASCII.NUL;
   Pixmap_Mail_New    : constant String := "New Mail" & ASCII.NUL;
   Pixmap_Trash       : constant String := "Trash" & ASCII.NUL;
   Pixmap_Trash_Full  : constant String := "Trash Full" & ASCII.NUL;
   Pixmap_Undelete    : constant String := "Undelete" & ASCII.NUL;
   Pixmap_Spellcheck  : constant String := "Spellchecker" & ASCII.NUL;
   Pixmap_Mic         : constant String := "Microphone" & ASCII.NUL;
   Pixmap_Line_In     : constant String := "Line In" & ASCII.NUL;
   Pixmap_Cdrom       : constant String := "Cdrom" & ASCII.NUL;
   Pixmap_Volume      : constant String := "Volume" & ASCII.NUL;
   Pixmap_Midi        : constant String := "Midi" & ASCII.NUL;
   Pixmap_Book_Red    : constant String := "Book Red" & ASCII.NUL;
   Pixmap_Book_Green  : constant String := "Book Green" & ASCII.NUL;
   Pixmap_Book_Blue   : constant String := "Book Blue" & ASCII.NUL;
   Pixmap_Book_Yellow : constant String := "Book Yellow" & ASCII.NUL;
   Pixmap_Book_Open   : constant String := "Book Open" & ASCII.NUL;
   Pixmap_About       : constant String := "About" & ASCII.NUL;
   Pixmap_Quit        : constant String := "Quit" & ASCII.NUL;
   Pixmap_Multiple    : constant String := "Multiple" & ASCII.NUL;
   Pixmap_Not         : constant String := "Not" & ASCII.NUL;
   Pixmap_Convert     : constant String := "Convert" & ASCII.NUL;
   Pixmap_Jump_To     : constant String := "Jump To" & ASCII.NUL;
   Pixmap_Up          : constant String := "Up" & ASCII.NUL;
   Pixmap_Down        : constant String := "Down" & ASCII.NUL;
   Pixmap_Top         : constant String := "Top" & ASCII.NUL;
   Pixmap_Bottom      : constant String := "Bottom" & ASCII.NUL;
   Pixmap_Attach      : constant String := "Attach" & ASCII.NUL;
   Pixmap_Index       : constant String := "Index" & ASCII.NUL;
   Pixmap_Font        : constant String := "Font" & ASCII.NUL;
   Pixmap_Exec        : constant String := "Exec" & ASCII.NUL;

   Pixmap_Align_Left     : constant String := "Left" & ASCII.NUL;
   Pixmap_Align_Right    : constant String := "Right" & ASCII.NUL;
   Pixmap_Align_Center   : constant String := "Center" & ASCII.NUL;
   Pixmap_AlIgn_Justify  : constant String := "Justify" & ASCII.NUL;

   Pixmap_Text_Bold      : constant String := "Bold" & ASCII.NUL;
   Pixmap_Text_Italic    : constant String := "Italic" & ASCII.NUL;
   Pixmap_Text_Underline : constant String := "Underline" & ASCII.NUL;
   Pixmap_Text_Strikeout : constant String := "Strikeout" & ASCII.NUL;

   Pixmap_Text_Indent    : constant String := "Text Indent" & ASCII.NUL;
   Pixmap_Text_Unindent  : constant String := "Text Unindent" & ASCII.NUL;

   Pixmap_Exit           : constant String := Pixmap_Quit;

   Pixmap_Colorselector  : constant String := "Color Select" & ASCII.NUL;

   Pixmap_Add            : constant String := "Add" & ASCII.NUL;
   Pixmap_Remove         : constant String := "Remove" & ASCII.NUL;

   Pixmap_Table_Borders  : constant String := "Table Borders" & ASCII.NUL;
   Pixmap_Table_Fill     : constant String := "Table Fill" & ASCII.NUL;

   Pixmap_Text_Bulleted_List : constant String :=
     "Text Bulleted List" & ASCII.NUL;
   Pixmap_Text_Numbered_List : constant String :=
     "Text Numbered List" & ASCII.NUL;

   --  The basic pixmap version of an icon.

   Pixmap_Regular  : constant String := "regular" & ASCII.NUL;
   Pixmap_Disabled : constant String := "disabled" & ASCII.NUL;
   Pixmap_Focused  : constant String := "focused" & ASCII.NUL;

   --  Buttons

   Button_Ok     : constant String := "Button_Ok" & ASCII.NUL;
   Button_Cancel : constant String := "Button_Cancel" & ASCII.NUL;
   Button_Yes    : constant String := "Button_Yes" & ASCII.NUL;
   Button_No     : constant String := "Button_No" & ASCII.NUL;
   Button_Close  : constant String := "Button_Close" & ASCII.NUL;
   Button_Apply  : constant String := "Button_Apply" & ASCII.NUL;
   Button_Help   : constant String := "Button_Help" & ASCII.NUL;
   Button_Next   : constant String := "Button_Next" & ASCII.NUL;
   Button_Prev   : constant String := "Button_Prev" & ASCII.NUL;
   Button_Up     : constant String := "Button_Up" & ASCII.NUL;
   Button_Down   : constant String := "Button_Down" & ASCII.NUL;
   Button_Font   : constant String := "Button_Font" & ASCII.NUL;

   function Button
     (Button_Type : String;
      Ordinary    : Boolean := False) return Gtk.Button.Gtk_Button;
   --  Return a default button widget for dialogs
   --  If the Button_Type argument matches a Button_* define, then a stock
   --  button is created.
   --- Otherwise, if Ordinary is True, an ordinary button is created, and
   --  Button_Type is given as the label. If Ordinary is False, null will
   --  be returned.

   --  Menus

   Menu_Blank        : constant String := "Menu_" & ASCII.NUL;
   Menu_New          : constant String := "Menu_New" & ASCII.NUL;
   Menu_Save         : constant String := "Menu_Save" & ASCII.NUL;
   Menu_Save_As      : constant String := "Menu_Save As" & ASCII.NUL;
   Menu_Revert       : constant String := "Menu_Revert" & ASCII.NUL;
   Menu_Open         : constant String := "Menu_Open" & ASCII.NUL;
   Menu_Close        : constant String := "Menu_Close" & ASCII.NUL;
   Menu_Quit         : constant String := "Menu_Quit" & ASCII.NUL;
   Menu_Cut          : constant String := "Menu_Cut" & ASCII.NUL;
   Menu_Copy         : constant String := "Menu_Copy" & ASCII.NUL;
   Menu_Paste        : constant String := "Menu_Paste" & ASCII.NUL;
   Menu_Prop         : constant String := "Menu_Properties" & ASCII.NUL;
   Menu_Pref         : constant String := "Menu_Preferences" & ASCII.NUL;
   Menu_About        : constant String := "Menu_About" & ASCII.NUL;
   Menu_Scores       : constant String := "Menu_Scores" & ASCII.NUL;
   Menu_Undo         : constant String := "Menu_Undo" & ASCII.NUL;
   Menu_Redo         : constant String := "Menu_Redo" & ASCII.NUL;
   Menu_Print        : constant String := "Menu_Print" & ASCII.NUL;
   Menu_Search       : constant String := "Menu_Search" & ASCII.NUL;
   Menu_Srchrpl      : constant String := "Menu_Search/Replace" & ASCII.NUL;
   Menu_Back         : constant String := "Menu_Back" & ASCII.NUL;
   Menu_Forward      : constant String := "Menu_Forward" & ASCII.NUL;
   Menu_First        : constant String := "Menu_First" & ASCII.NUL;
   Menu_Last         : constant String := "Menu_Last" & ASCII.NUL;
   Menu_Home         : constant String := "Menu_Home" & ASCII.NUL;
   Menu_Stop         : constant String := "Menu_Stop" & ASCII.NUL;
   Menu_Refresh      : constant String := "Menu_Refresh" & ASCII.NUL;
   Menu_Mail         : constant String := "Menu_Mail" & ASCII.NUL;
   Menu_Mail_Rcv     : constant String := "Menu_Receive Mail" & ASCII.NUL;
   Menu_Mail_Snd     : constant String := "Menu_Send Mail" & ASCII.NUL;
   Menu_Mail_Rpl     : constant String := "Menu_Reply to Mail" & ASCII.NUL;
   Menu_Mail_Fwd     : constant String := "Menu_Forward Mail" & ASCII.NUL;
   Menu_Mail_New     : constant String := "Menu_New Mail" & ASCII.NUL;
   Menu_Trash        : constant String := "Menu_Trash" & ASCII.NUL;
   Menu_Trash_Full   : constant String := "Menu_Trash Full" & ASCII.NUL;
   Menu_Undelete     : constant String := "Menu_Undelete" & ASCII.NUL;
   Menu_Timer        : constant String := "Menu_Timer" & ASCII.NUL;
   Menu_Timer_Stop   : constant String := "Menu_Timer Stopped" & ASCII.NUL;
   Menu_Spellcheck   : constant String := "Menu_Spellchecker" & ASCII.NUL;
   Menu_Mic          : constant String := "Menu_Microphone" & ASCII.NUL;
   Menu_Line_In      : constant String := "Menu_Line In" & ASCII.NUL;
   Menu_Cdrom        : constant String := "Menu_Cdrom" & ASCII.NUL;
   Menu_Volume       : constant String := "Menu_Volume" & ASCII.NUL;
   Menu_Midi         : constant String := "Menu_Midi" & ASCII.NUL;
   Menu_Book_Red     : constant String := "Menu_Book Red" & ASCII.NUL;
   Menu_Book_Green   : constant String := "Menu_Book Green" & ASCII.NUL;
   Menu_Book_Blue    : constant String := "Menu_Book Blue" & ASCII.NUL;
   Menu_Book_Yellow  : constant String := "Menu_Book Yellow" & ASCII.NUL;
   Menu_Book_Open    : constant String := "Menu_Book Open" & ASCII.NUL;
   Menu_Convert      : constant String := "Menu_Convert" & ASCII.NUL;
   Menu_Jump_To      : constant String := "Menu_Jump To" & ASCII.NUL;
   Menu_Up           : constant String := "Menu_Up" & ASCII.NUL;
   Menu_Down         : constant String := "Menu_Down" & ASCII.NUL;
   Menu_Top          : constant String := "Menu_Top" & ASCII.NUL;
   Menu_Bottom       : constant String := "Menu_Bottom" & ASCII.NUL;
   Menu_Attach       : constant String := "Menu_Attach" & ASCII.NUL;
   Menu_Index        : constant String := "Menu_Index" & ASCII.NUL;
   Menu_Font         : constant String := "Menu_Font" & ASCII.NUL;
   Menu_Exec         : constant String := "Menu_Exec" & ASCII.NUL;

   Menu_Align_Left     : constant String := "Menu_Left" & ASCII.NUL;
   Menu_Align_Right    : constant String := "Menu_Right" & ASCII.NUL;
   Menu_Align_Center   : constant String := "Menu_Center" & ASCII.NUL;
   Menu_Align_Justify  : constant String := "Menu_Justify" & ASCII.NUL;

   Menu_Text_Bold      : constant String := "Menu_Bold" & ASCII.NUL;
   Menu_Text_Italic    : constant String := "Menu_Italic" & ASCII.NUL;
   Menu_Text_Underline : constant String := "Menu_Underline" & ASCII.NUL;
   Menu_Text_Strikeout : constant String := "Menu_Strikeout" & ASCII.NUL;

   Menu_Exit           : constant String := Menu_Quit;

   function Menu_Item
     (Icon : String;
      Text : String) return Gtk.Menu_Item.Gtk_Menu_Item;
   --  Return a Gtk_Menu_Item with a stock icon and text

end Gnome.Stock;
