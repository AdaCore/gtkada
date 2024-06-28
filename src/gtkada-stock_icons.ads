------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2024, AdaCore                          --
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
--  Utility package that replaces the deprecated Gtk.Stock package.
--  Lists all the named icons that replace the old GTK_STOCK_* macros
--  for icon names.
--  See https://docs.gtk.org/gtk3/index.html#constants for more information.
--  </description>
--  <group>Configuration and Themes</group>

with Glib; use Glib;

package Gtkada.Stock_Icons is

   Stock_About : constant UTF8_String := "help-about";

   Stock_Add : constant UTF8_String := "list-add";

   Stock_Bold : constant UTF8_String := "format-text-bold";

   Stock_Caps_Lock_Warning : constant UTF8_String := "dialog-warning-symbolic";

   Stock_Cdrom : constant UTF8_String := "media-optical";

   Stock_Clear : constant UTF8_String := "edit-clear";

   Stock_Close : constant UTF8_String := "window-close";

   Stock_Copy : constant UTF8_String := "edit-copy";

   Stock_Cut : constant UTF8_String := "edit-cut";

   Stock_Delete : constant UTF8_String := "edit-delete";

   Stock_Dialog_Authentication : constant UTF8_String := "dialog-password";

   Stock_Dialog_Error : constant UTF8_String := "dialog-error";

   Stock_Dialog_Info : constant UTF8_String := "dialog-information";

   Stock_Dialog_Question : constant UTF8_String := "dialog-question";

   Stock_Dialog_Warning : constant UTF8_String := "dialog-warning";

   Stock_Directory : constant UTF8_String := "folder";

   Stock_Execute : constant UTF8_String := "system-run";

   Stock_File : constant UTF8_String := "text-x-generic";

   Stock_Find : constant UTF8_String := "edit-find";

   Stock_Find_And_Replace : constant UTF8_String := "edit-find-replace";

   Stock_Fullscreen : constant UTF8_String := "view-fullscreen";

   Stock_Go_Back : constant UTF8_String := "go-previous";

   Stock_Go_Down : constant UTF8_String := "go-down";

   Stock_Go_Forward : constant UTF8_String := "go-forward";

   Stock_Go_Up : constant UTF8_String := "go-up";

   Stock_Goto_Bottom : constant UTF8_String := "go-bottom";

   Stock_Goto_First : constant UTF8_String := "go-first";

   Stock_Goto_Last : constant UTF8_String := "go-last";

   Stock_Goto_Top : constant UTF8_String := "go-top";

   Stock_Harddisk : constant UTF8_String := "drive-harddisk";

   Stock_Help : constant UTF8_String := "help-browse";

   Stock_Home : constant UTF8_String := "go-home";

   Stock_Indent : constant UTF8_String := "format-indent-more";

   Stock_Info : constant UTF8_String := "dialog-information";

   Stock_Italic : constant UTF8_String := "format-text-italic";

   Stock_Jump_To : constant UTF8_String := "go-jump";

   Stock_Justify_Center : constant UTF8_String := "format-justify-center";

   Stock_Justify_Fill : constant UTF8_String := "format-justify-fill";

   Stock_Justify_Left : constant UTF8_String := "format-justify-left";

   Stock_Justify_Right : constant UTF8_String := "format-justify-right";

   Stock_Leave_Fullscreen : constant UTF8_String := "view-restore";

   Stock_Media_Forward : constant UTF8_String := "media-seek-forward";

   Stock_Media_Next : constant UTF8_String := "media-skip-forward";

   Stock_Media_Pause : constant UTF8_String := "media-playback-pause";

   Stock_Media_Play : constant UTF8_String := "media-playback-start";

   Stock_Media_Previous : constant UTF8_String := "media-skip-backward";

   Stock_Media_Record : constant UTF8_String := "media-record";

   Stock_Media_Rewind : constant UTF8_String := "media-seek-backward";

   Stock_Media_Stop : constant UTF8_String := "media-playback-sto";

   Stock_Missing_Image : constant UTF8_String := "image-missing";

   Stock_Network : constant UTF8_String := "network-workgroup";

   Stock_New : constant UTF8_String := "document-new";

   Stock_Open : constant UTF8_String := "document-open";

   Stock_Page_Setup : constant UTF8_String := "document-page-setup";

   Stock_Paste : constant UTF8_String := "edit-paste";

   Stock_Preferences : constant UTF8_String := "preferences-system";

   Stock_Print : constant UTF8_String := "document-print";

   Stock_Print_Error : constant UTF8_String := "printer-error";

   Stock_Print_Paused : constant UTF8_String := "_Print Paused";

   Stock_Print_Warning : constant UTF8_String := "_Print Warning";

   Stock_Properties : constant UTF8_String := "document-properties";

   Stock_Quit : constant UTF8_String := "application-exit";

   Stock_Redo : constant UTF8_String := "edit-redo";

   Stock_Refresh : constant UTF8_String := "view-refresh";

   Stock_Remove : constant UTF8_String := "list-remove";

   Stock_Revert_To_Saved : constant UTF8_String := "document-revert";

   Stock_Save : constant UTF8_String := "document-save";

   Stock_Save_As : constant UTF8_String := "document-save-as";

   Stock_Select_All : constant UTF8_String := "edit-select-all";

   Stock_Sort_Ascending : constant UTF8_String := "view-sort-ascending";

   Stock_Sort_Descending : constant UTF8_String := "view-sort-descending";

   Stock_Spell_Check : constant UTF8_String := "tools-check-spelling";

   Stock_Stop : constant UTF8_String := "process-stop";

   Stock_Strikethrough : constant UTF8_String := "format-text-strikethrough";

   Stock_Underline : constant UTF8_String := "format-text-underline";

   Stock_Undo : constant UTF8_String := "edit-undo";

   Stock_Unindent : constant UTF8_String := "format-indent-less";

   Stock_Zoom_100 : constant UTF8_String := "zoom-original";

   Stock_Zoom_Fit : constant UTF8_String := "zoom-fit-best";

   Stock_Zoom_In : constant UTF8_String := "zoom-in";

   Stock_Zoom_Out : constant UTF8_String := "zoom-out";

end Gtkada.Stock_Icons;
