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
--  Lists all the predefined labels that replace the old GTK_STOCK_* macros
--  for stock labels.
--  See https://docs.gtk.org/gtk3/index.html#constants for more information.
--  </description>
--  <group>Configuration and Themes</group>

with Glib; use Glib;

package Gtkada.Stock_Labels is

   Stock_About : constant UTF8_String := "_About";

   Stock_Add : constant UTF8_String := "_Add";

   Stock_Apply : constant UTF8_String := "_Apply";

   Stock_Cancel : constant UTF8_String := "_Cancel";

   Stock_Close : constant UTF8_String := "_Close";

   Stock_Copy : constant UTF8_String := "_Copy";

   Stock_Cut : constant UTF8_String := "Cu_t";

   Stock_Delete : constant UTF8_String := "_Delete";

   Stock_Media_Forward : constant UTF8_String := "_Forward";

   Stock_Media_Next : constant UTF8_String := "_Next";

   Stock_Media_Pause : constant UTF8_String := "P_ause";

   Stock_Media_Play : constant UTF8_String := "_Play";

   Stock_Media_Previous : constant UTF8_String := "Pre_vious";

   Stock_Media_Record : constant UTF8_String := "_Record";

   Stock_Media_Rewind : constant UTF8_String := "R_ewind";

   Stock_Media_Stop : constant UTF8_String := "_Stop";

   Stock_New : constant UTF8_String := "_New";

   Stock_No : constant UTF8_String := "_No";

   Stock_Ok : constant UTF8_String := "_OK";

   Stock_Open : constant UTF8_String := "_Open";

   Stock_Page_Setup : constant UTF8_String := "Page Set_up";

   Stock_Paste : constant UTF8_String := "_Paste";

   Stock_Preferences : constant UTF8_String := "_Preferences";

   Stock_Print : constant UTF8_String := "_Print";

   Stock_Print_Preview : constant UTF8_String := "Pre_view";

   Stock_Properties : constant UTF8_String := "_Properties";

   Stock_Quit : constant UTF8_String := "_Quit";

   Stock_Redo : constant UTF8_String := "_Redo";

   Stock_Refresh : constant UTF8_String := "_Refresh";

   Stock_Remove : constant UTF8_String := "_Remove";

   Stock_Revert_To_Saved : constant UTF8_String := "_Revert";

   Stock_Save : constant UTF8_String := "_Save";

   Stock_Save_As : constant UTF8_String := "Save _As";

   Stock_Select_All : constant UTF8_String := "Select _All";

   Stock_Stop : constant UTF8_String := "_Stop";

   Stock_Strikethrough : constant UTF8_String := "_Strikethrough";

   Stock_Underline : constant UTF8_String := "_Underline";

   Stock_Undo : constant UTF8_String := "_Undo";

   Stock_Yes : constant UTF8_String := "_Yes";

   Stock_Zoom_100 : constant UTF8_String := "_Normal Size";

   Stock_Zoom_Fit : constant UTF8_String := "Best _Fit";

   Stock_Zoom_In : constant UTF8_String := "Zoom _In";

   Stock_Zoom_Out : constant UTF8_String := "Zoom _Out";

end Gtkada.Stock_Labels;
