------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
--  This file defines standard sizes, spacings, and whatever
--  else seems standardizable via simple definitions.
--  </description>

with Gdk.Types; use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;

package Gnome.UI_Defs is

   Pad       : constant := 8;
   Pad_Small : constant := 4;
   Pad_Big   : constant := 12;
   --  All-purpose padding. If you always use these instead of making up
   --  some arbitrary padding number that looks good on your screen,
   --  people can change the "spaciousness" of the GUI globally.

   --  These are keybindings, in UI_Info format. USE THEM OR DIE!
   --  Add to the list as well..

   Key_Name_Exit         : constant Gdk_Key_Type := GDK_Q;
   Key_Mod_Exit          : constant Gdk_Modifier_Type := Control_Mask;
   Key_Name_Close        : constant Gdk_Key_Type := GDK_W;
   Key_Mod_Close         : constant Gdk_Modifier_Type := Control_Mask;

   Key_Name_Cut          : constant Gdk_Key_Type := GDK_X;
   Key_Mod_Cut           : constant Gdk_Modifier_Type := Control_Mask;
   Key_Name_Copy         : constant Gdk_Key_Type := GDK_C;
   Key_Mod_Copy          : constant Gdk_Modifier_Type := Control_Mask;
   Key_Name_Paste        : constant Gdk_Key_Type := GDK_V;
   Key_Mod_Paste         : constant Gdk_Modifier_Type := Control_Mask;
   Key_Name_Select_All   : constant Gdk_Key_Type := 0;
   Key_Mod_Select_All    : constant Gdk_Modifier_Type := 0;
   Key_Name_Clear        : constant Gdk_Key_Type := 0;
   Key_Mod_Clear         : constant Gdk_Modifier_Type := 0;

   Key_Name_Undo         : constant Gdk_Key_Type := GDK_Z;
   Key_Mod_Undo          : constant Gdk_Modifier_Type := Control_Mask;
   Key_Name_Redo         : constant Gdk_Key_Type := GDK_R;
   Key_Mod_Redo          : constant Gdk_Modifier_Type := Control_Mask;

   Key_Name_Save         : constant Gdk_Key_Type := GDK_S;
   Key_Mod_Save          : constant Gdk_Modifier_Type := Control_Mask;
   Key_Name_OPEN         : constant Gdk_Key_Type := GDK_F3;
   Key_Mod_OPEN          : constant Gdk_Modifier_Type := 0;
   Key_Name_Save_As      : constant Gdk_Key_Type := 0;
   Key_Mod_Save_As       : constant Gdk_Modifier_Type := 0;
   Key_Name_New          : constant Gdk_Key_Type := 0;
   Key_Mod_New           : constant Gdk_Modifier_Type := 0;

   Key_Name_Print        : constant Gdk_Key_Type := 0;
   Key_Mod_Print         : constant Gdk_Modifier_Type := 0;

   Key_Name_Print_Setup  : constant Gdk_Key_Type := 0;
   Key_Mod_Print_Setup   : constant Gdk_Modifier_Type := 0;

   Key_Name_Find         : constant Gdk_Key_Type := GDK_F6;
   Key_Mod_Find          : constant Gdk_Modifier_Type := 0;
   Key_Name_Find_Again   : constant Gdk_Key_Type := GDK_F6;
   Key_Mod_Find_Again    : constant Gdk_Modifier_Type := Shift_Mask;
   Key_Name_Replace      : constant Gdk_Key_Type := GDK_F7;
   Key_Mod_Replace       : constant Gdk_Modifier_Type := 0;

   Key_Name_New_Window   : constant Gdk_Key_Type := 0;
   Key_Mod_New_Window    : constant Gdk_Modifier_Type := 0;
   Key_Name_Close_Window : constant Gdk_Key_Type := 0;
   Key_Mod_Close_Window  : constant Gdk_Modifier_Type := 0;

   Key_Name_Redo_Move    : constant Gdk_Key_Type := GDK_R;
   Key_Mod_Redo_Move     : constant Gdk_Modifier_Type := Control_Mask;
   Key_Name_Undo_Move    : constant Gdk_Key_Type := GDK_Z;
   Key_Mod_Undo_Move     : constant Gdk_Modifier_Type := Control_Mask;

   Key_Name_Pause_Game   : constant Gdk_Key_Type := 0;
   Key_Mod_Pause_Game    : constant Gdk_Modifier_Type := 0;
   Key_Name_New_Game     : constant Gdk_Key_Type := GDK_N;
   Key_Mod_New_Game      : constant Gdk_Modifier_Type := Control_Mask;

end Gnome.UI_Defs;
