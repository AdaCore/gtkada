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

with Glib;
with Glib.Glist;
pragma Elaborate_All (Glib.Glist);

with System;

package Gtk.Enums is

   type Gtk_Arrow_Type is (Arrow_Up, Arrow_Down, Arrow_Left, Arrow_Right);
   for Gtk_Arrow_Type'Size use Gint'Size;

   type Gtk_Attach_Options is new Glib.Guint32;
   Expand : constant Gtk_Attach_Options := 1;
   Shrink : constant Gtk_Attach_Options := 2;
   Fill   : constant Gtk_Attach_Options := 4;

   type Gtk_Button_Action is new Guint;
   Button_Ignored : constant Gtk_Button_Action := 0;
   Button_Selects : constant Gtk_Button_Action := 1 ** 0;
   Button_Drags   : constant Gtk_Button_Action := 1 ** 1;
   Button_Expands : constant Gtk_Button_Action := 1 ** 2;

   type Gtk_Button_Box_Style is
     (Buttonbox_Default_Style,
      Buttonbox_Spread,
      Buttonbox_Edge,
      Buttonbox_Start,
      Buttonbox_End);
   for Gtk_Button_Box_Style'Size use Gint'Size;

   type Gtk_Cell_Type is
     (Cell_Empty,
      Cell_Text,
      Cell_Pixmap,
      Cell_Pixtext,
      Cell_Widget);
   for Gtk_Cell_Type'Size use Gint'Size;

   type Gtk_Ctree_Line_Style is
     (Ctree_Lines_None,
      --  No line will be drawn in the Ctree

      Ctree_Lines_Solid,
      --  Solid lines will be drawn

      Ctree_Lines_Dotted,
      --  Dotted lines will be drawn

      Ctree_Lines_Tabbed
      --  The tree won't be highlighted by lines but by tabs surrounding nodes
     );
   for Gtk_Ctree_Line_Style'Size use Gint'Size;
   --  See Gtk.Ctree.Set_Line_Style for more details.

   type Gtk_Ctree_Expander_Style is
     (Ctree_Expander_None,
      --  No pixmap will be drawn, you will have to double-click on the node to
      --  expand it.

      Ctree_Expander_Square,
      --  The pixmap will be a square

      Ctree_Expander_Triangle,
      --  The pixmap will be a triangle

      Ctree_Expander_Circular
      --  The pixmap will be a circle
     );
   --  See Gtk.Ctree.Set_Expander_Style for more details.
   for Gtk_Ctree_Expander_Style'Size use Gint'Size;

   type Gtk_Delete_Type is
     (Delete_Chars,
      Delete_Word_Ends,
      --  Delete only the portion of the word to the left/right of the cursor
      --  if we are in the middle of a word.
      Delete_Words,
      Delete_Display_Lines,
      Delete_Display_Line_Ends,
      Delete_Paragraph_Ends,
      --  Delete until end of line ???
      Delete_Paragraphs,
      --  Delete current line ???
      Delete_Whitespace);
   for Gtk_Delete_Type'Size use Gint'Size;

   type Gtk_Direction_Type is
     (Dir_Tab_Forward,
      Dir_Tab_Backward,
      Dir_Up,
      Dir_Down,
      Dir_Left,
      Dir_Right);
   for Gtk_Direction_Type'Size use Gint'Size;

   type Gtk_Icon_Size is
     (Icon_Size_Invalid,
      Icon_Size_Menu,
      Icon_Size_Small_Toolbar,
      Icon_Size_Large_Toolbar,
      Icon_Size_Button,
      Icon_Size_Dialog);
   for Gtk_Icon_Size'Size use Gint'Size;

   type Gtk_Justification is
     (Justify_Left,
      Justify_Right,
      Justify_Center,
      Justify_Fill);
   for Gtk_Justification'Size use Gint'Size;

   type Gtk_Metric_Type is (Pixels, Inches, Centimeters);
   for Gtk_Metric_Type'Size use Gint'Size;

   type Gtk_Orientation is (Orientation_Horizontal, Orientation_Vertical);
   for Gtk_Orientation'Size use Gint'Size;

   type Gtk_Pack_Type is (Pack_Start, Pack_End);
   for Gtk_Pack_Type'Size use Gint'Size;

   type Gtk_Policy_Type is (Policy_Always, Policy_Automatic, Policy_Never);
   for Gtk_Policy_Type'Size use Gint'Size;

   type Gtk_Position_Type is
     (Pos_Left,
      Pos_Right,
      Pos_Top,
      Pos_Bottom);
   for Gtk_Position_Type'Size use Gint'Size;

   type Gtk_Preview_Type is (Preview_Color, Preview_Grayscale);
   for Gtk_Preview_Type'Size use Gint'Size;

   type Gtk_Progress_Bar_Orientation is
     (Progress_Left_To_Right,
      Progress_Right_To_Left,
      Progress_Bottom_To_Top,
      Progress_Top_To_Bottom);
   for Gtk_Progress_Bar_Orientation'Size use Gint'Size;

   type Gtk_Relief_Style is (Relief_Normal, Relief_Half, Relief_None);
   for Gtk_Relief_Style'Size use Gint'Size;

   type Gtk_Resize_Mode is
     (Resize_Parent,     --  Pass request to the parent
      Resize_Queue,      --  Queue resizes on this widget
      Resize_Immediate); --  Perform the resizes now
   for Gtk_Resize_Mode'Size use Gint'Size;

   type Gtk_Scroll_Type is
     (Scroll_None,
      Scroll_Step_Backward,
      Scroll_Step_Forward,
      Scroll_Page_Backward,
      Scroll_Page_Forward,
      Scroll_Jump,
      Scroll_Step_Up,
      Scroll_Step_Down,
      Scroll_Page_Up,
      Scroll_Page_Down,
      Scroll_Step_Left,
      Scroll_Step_Right,
      Scroll_Page_Left,
      Scroll_Page_Right);
   for Gtk_Scroll_Type'Size use Gint'Size;

   type Gtk_Selection_Mode is
     (Selection_Single,
      Selection_Browse,
      Selection_Multiple,
      Selection_Extended);
   for Gtk_Selection_Mode'Size use Gint'Size;

   type Gtk_Shadow_Type is
     (Shadow_None,
      Shadow_In,
      Shadow_Out,
      Shadow_Etched_In,
      Shadow_Etched_Out);
   for Gtk_Shadow_Type'Size use Gint'Size;

   type Gtk_State_Type is
     (State_Normal,
      State_Active,
      State_Prelight,
      State_Selected,
      State_Insensitive);
   for Gtk_State_Type'Size use Gint'Size;

   type Gtk_Submenu_Direction is (Direction_Left, Direction_Right);
   for Gtk_Submenu_Direction'Size use Gint'Size;

   type Gtk_Submenu_Placement is (Top_Bottom, Left_Right);
   for Gtk_Submenu_Placement'Size use Gint'Size;

   type Gtk_Text_Direction is
     (Text_Dir_None,
      Text_Dir_Ltr,
      Text_Dir_Rtl);
   for Gtk_Text_Direction'Size use Gint'Size;

   type Gtk_Text_Window_Type is
     (Text_Window_Private,
      Text_Window_Widget,
      Text_Window_Text,
      Text_Window_Left,
      Text_Window_Right,
      Text_Window_Top,
      Text_Window_Bottom);
   for Gtk_Text_Window_Type'Size use Gint'Size;

   type Gtk_Toolbar_Child_Type is
     (Toolbar_Child_Space,
      Toolbar_Child_Button,
      Toolbar_Child_Togglebutton,
      Toolbar_Child_Radiobutton,
      Toolbar_Child_Widget);
   for Gtk_Toolbar_Child_Type'Size use Gint'Size;

   type Gtk_Toolbar_Style is (Toolbar_Icons, Toolbar_Text, Toolbar_Both);
   for Gtk_Toolbar_Style'Size use Gint'Size;

   type Gtk_Toolbar_Space_Style is (Toolbar_Space_Empty, Toolbar_Space_Line);
   for Gtk_Toolbar_Space_Style'Size use Gint'Size;

   type Gtk_Tree_View_Mode is (Tree_View_Line, Tree_View_Item);
   for Gtk_Tree_View_Mode'Size use Gint'Size;

   type Gtk_Trough_Type is
     (Trough_None,
      Trough_Start,
      Trough_End,
      Trough_Jump);
   for Gtk_Trough_Type'Size use Gint'Size;

   type Gtk_Update_Type is
     (Update_Continuous,
      Update_Discontinuous,
      Update_Delayed);
   for Gtk_Update_Type'Size use Gint'Size;

   type Gtk_Visibility is
     (Visibility_None, Visibility_Partial, Visibility_Full);
   for Gtk_Visibility'Size use Gint'Size;

   type Gtk_Window_Position is
     (Win_Pos_None,
      Win_Pos_Center,
      Win_Pos_Mouse,
      Win_Pos_Center_Always,
      Win_Pos_Center_On_Parent);
   for Gtk_Window_Position'Size use Gint'Size;

   type Gtk_Window_Type is (Window_Toplevel, Window_Popup);
   for Gtk_Window_Type'Size use Gint'Size;

   type Gtk_Wrap_Mode is
     (Wrapmode_None,
      Wrapmode_Char,
      Wrapmode_Word);
   for Gtk_Wrap_Mode'Size use Gint'Size;

   --  Some Glib instantiations

   function Convert (S : String) return System.Address;
   function Convert (S : System.Address) return String;
   function Convert_I (I : Gint) return System.Address;
   function Convert_A (S : System.Address) return Gint;
   pragma Import (C, Convert_I, "convert_i");
   pragma Import (C, Convert_A, "convert_a");

   package String_List is new Glib.Glist.Generic_List (String);
   --  Warning: when you create this list, new memory gets allocated for
   --  all the strings. You should use the function Free_String_List
   --  instead of Glib.Glist.Free to be sure to free this memory.

   procedure Free_String_List (List : in out String_List.Glist);
   --  Free the memory occupied by all the strings in the list, as well
   --  as the memory occupied by the list itself.

   package Gint_List is new
     Glib.Glist.Generic_List (Gint, Convert_I, Convert_A);

   function Convert_UI (I : Guint) return System.Address;
   function Convert_UA (S : System.Address) return Guint;
   pragma Import (C, Convert_UI, "convert_ui");
   pragma Import (C, Convert_UA, "convert_ua");
   package Guint_List is new
     Glib.Glist.Generic_List (Guint, Convert_UI, Convert_UA);

end Gtk.Enums;
