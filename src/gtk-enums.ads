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

--  <c_version>1.3.11</c_version>

with Glib;
with Glib.Generic_Properties;  use Glib.Generic_Properties;
pragma Elaborate_All (Glib.Generic_Properties);
with Glib.Glist;
pragma Elaborate_All (Glib.Glist);

with System;

package Gtk.Enums is

   type Gtk_Anchor_Type is
     (Anchor_Center,
      Anchor_North,
      Anchor_North_West,
      Anchor_North_East,
      Anchor_South,
      Anchor_South_East,
      Anchor_South_West,
      Anchor_West,
      Anchor_East);
   pragma Convention (C, Gtk_Anchor_Type);
   --  Gtk_Anchor_Type indicates the exact location of the widget on its
   --  side. Note that not all anchors are relevant for each side.
   --
   --  For instance, if you put a widget on Side_Right, with an anchor of
   --  Anchor_North, Anchor_North_West or Anchor_North_East, the widget will
   --  in fact appear on the upper right side of the remaining space in the
   --  container.
   --
   --  Thus, if a previous child was added on Side_North, then the new child
   --  will only appear on the second line in the container. The order the
   --  children are inserted into the container is important.

   Anchor_N  : Gtk_Anchor_Type renames Anchor_North;
   Anchor_NW : Gtk_Anchor_Type renames Anchor_North_West;
   Anchor_NE : Gtk_Anchor_Type renames Anchor_North_East;
   Anchor_S  : Gtk_Anchor_Type renames Anchor_South;
   Anchor_SW : Gtk_Anchor_Type renames Anchor_South_West;
   Anchor_SE : Gtk_Anchor_Type renames Anchor_South_East;
   Anchor_W  : Gtk_Anchor_Type renames Anchor_West;
   Anchor_E  : Gtk_Anchor_Type renames Anchor_East;

   type Gtk_Arrow_Type is (Arrow_Up, Arrow_Down, Arrow_Left, Arrow_Right);
   pragma Convention (C, Gtk_Arrow_Type);

   type Gtk_Attach_Options is new Glib.Guint32;
   Expand : constant Gtk_Attach_Options := 1;
   Shrink : constant Gtk_Attach_Options := 2;
   Fill   : constant Gtk_Attach_Options := 4;

   type Gtk_Button_Box_Style is
     (Buttonbox_Default_Style,
      Buttonbox_Spread,
      Buttonbox_Edge,
      Buttonbox_Start,
      Buttonbox_End);
   pragma Convention (C, Gtk_Button_Box_Style);

   type Gtk_Curve_Type is
     (Curve_Type_Linear,
      --  Linear interpolation

      Curve_Type_Spline,
      --  Spline interpolation

      Curve_Type_Free
      --  Free form curve
     );
   pragma Convention (C, Gtk_Curve_Type);

   type Gtk_Delete_Type is
     (Delete_Chars,
      Delete_Word_Ends,
      --  Delete only the portion of the word to the left/right of the cursor
      --  if we are in the middle of a word.

      Delete_Words,
      Delete_Display_Lines,
      Delete_Display_Line_Ends,
      Delete_Paragraph_Ends,
      --  Like C-k in Emacs: delete until end of line

      Delete_Paragraphs,
      --  C-k in pico: delete while line

      Delete_Whitespace
      --  M-\ in Emacs
     );
   pragma Convention (C, Gtk_Delete_Type);

   type Gtk_Direction_Type is
     (Dir_Tab_Forward,
      Dir_Tab_Backward,
      Dir_Up,
      Dir_Down,
      Dir_Left,
      Dir_Right);
   pragma Convention (C, Gtk_Direction_Type);
   --  Focus movement types

   type Gtk_Expander_Style is
     (Expander_Collapsed,
      Expander_Semi_Collapsed,
      Expander_Semi_Expanded,
      Expander_Expanded);
   pragma Convention (C, Gtk_Expander_Style);
   --  Expander styles

   type Gtk_Icon_Size is
     (Icon_Size_Invalid,
      Icon_Size_Menu,
      Icon_Size_Small_Toolbar,
      Icon_Size_Large_Toolbar,
      Icon_Size_Button,
      Icon_Size_Dnd,
      Icon_Size_Dialog);
   pragma Convention (C, Gtk_Icon_Size);
   --  Built-in stock icon sizes

   type Gtk_Side_Type is
     (Side_Top,
      Side_Bottom,
      Side_Left,
      Side_Right);
   pragma Convention (C, Gtk_Side_Type);
   --  Gtk_Side_Type indicates on which the widget should be inserted.
   --  The children are displayed in the order they were inserted into the
   --  container.
   --  Each time a child is displayed, the available space for the remaining
   --  child is restrained. For instance, every time you put a child on the
   --  Side_Top or Side_Bottom, the available space is decreased so that no
   --  other widget is inserted in the same line.
   --
   --  For instance, if you put two widgets on Side_Top, the second one will
   --  appear below the first one. If you add two widgets on Side_Right, the
   --  second one will be placed on the left of the first.

   type Gtk_Text_Direction is
     (Text_Dir_None,
      Text_Dir_Ltr,
      Text_Dir_Rtl);
   pragma Convention (C, Gtk_Text_Direction);

   type Gtk_Justification is
     (Justify_Left,
      Justify_Right,
      Justify_Center,
      Justify_Fill);
   pragma Convention (C, Gtk_Justification);

   type Gtk_Match_Type is
     (Match_All,
      Match_All_Tail,
      Match_Head,
      Match_Tail,
      Match_Exact,
      Match_Last);
   pragma Convention (C, Gtk_Match_Type);

   type Gtk_Menu_Direction_Type is
     (Menu_Dir_Parent,
      Menu_Dir_Child,
      Menu_Dir_Next,
      Menu_Dir_Prev);
   pragma Convention (C, Gtk_Menu_Direction_Type);
   --  Direction where to move the selection.

   type Gtk_Metric_Type is (Pixels, Inches, Centimeters);
   pragma Convention (C, Gtk_Metric_Type);

   type Gtk_Movement_Step is
     (Movement_Logical_Positions, --  move by forw/back graphemes
      Movement_Visual_Positions,  --  move by left/right graphemes
      Movement_Words,             --  move by forward/back words
      Movement_Display_Lines,     --  move up/down lines (wrapped lines)
      Movement_Display_Line_Ends, --  move up/down lines (wrapped lines)
      Movement_Paragraphs,        --  move up/down paragraphs
      Movement_Paragraph_Ends,    --  move to either end of a paragraph
      Movement_Pages,             --  move by pages
      Movement_Buffer_Ends);      --  move to ends of the buffer
   pragma Convention (C, Gtk_Movement_Step);
   --  Note that a paragraph is defined as a new-line ended line.

   type Gtk_Orientation is (Orientation_Horizontal, Orientation_Vertical);
   pragma Convention (C, Gtk_Orientation);

   type Gtk_Corner_Type is
     (Corner_Top_Left,
      Corner_Bottom_Left,
      Corner_Top_Right,
      Corner_Bottom_Right);
   pragma Convention (C, Gtk_Corner_Type);
   --  Type used by Set_Placement below to determine the location of the
   --  child widget with respect to the scrollbars.
   --  Corner_Top_Left means the child is in the top left, with the scrollbars
   --  underneath and to the right.

   type Gtk_Pack_Type is (Pack_Start, Pack_End);
   pragma Convention (C, Gtk_Pack_Type);

   type Gtk_Path_Priority_Type is mod 2 ** 32;

   Path_Prio_Lowest      : constant Gtk_Path_Priority_Type := 0;
   Path_Prio_Gtk         : constant Gtk_Path_Priority_Type := 4;
   Path_Prio_Application : constant Gtk_Path_Priority_Type := 8;
   Path_Prio_Theme       : constant Gtk_Path_Priority_Type := 10;
   Path_Prio_RC          : constant Gtk_Path_Priority_Type := 12;
   Path_Prio_Highest     : constant Gtk_Path_Priority_Type := 15;
   --  Priorities for path lookups

   Path_Prio_Mask        : constant Gtk_Path_Priority_Type := 16#0f#;

   type Gtk_Path_Type is (Path_Widget, Path_Widget_Class, Path_Class);
   pragma Convention (C, Gtk_Path_Type);
   --  Widget path types

   type Gtk_Policy_Type is (Policy_Always, Policy_Automatic, Policy_Never);
   pragma Convention (C, Gtk_Policy_Type);

   type Gtk_Position_Type is
     (Pos_Left,
      Pos_Right,
      Pos_Top,
      Pos_Bottom);
   pragma Convention (C, Gtk_Position_Type);

   type Gtk_Preview_Type is (Preview_Color, Preview_Grayscale);
   pragma Convention (C, Gtk_Preview_Type);

   type Gtk_Relief_Style is (Relief_Normal, Relief_Half, Relief_None);
   pragma Convention (C, Gtk_Relief_Style);

   type Gtk_Resize_Mode is
     (Resize_Parent,     --  Pass request to the parent
      Resize_Queue,      --  Queue resizes on this widget
      Resize_Immediate); --  Perform the resizes now
   pragma Convention (C, Gtk_Resize_Mode);

   type Gtk_Scroll_Type is
     (Scroll_None,
      Scroll_Jump,
      Scroll_Step_Backward,
      Scroll_Step_Forward,
      Scroll_Page_Backward,
      Scroll_Page_Forward,
      Scroll_Step_Up,
      Scroll_Step_Down,
      Scroll_Page_Up,
      Scroll_Page_Down,
      Scroll_Step_Left,
      Scroll_Step_Right,
      Scroll_Page_Left,
      Scroll_Page_Right,
      Scroll_Start,
      Scroll_End);
   pragma Convention (C, Gtk_Scroll_Type);

   type Gtk_Selection_Mode is
     (Selection_None,
      Selection_Single,
      Selection_Browse,
      Selection_Multiple);
   pragma Convention (C, Gtk_Selection_Mode);
   --  The old Selection_Extended is now deprected, and replaced by
   --  Selection_Multiple.

   type Gtk_Shadow_Type is
     (Shadow_None,
      Shadow_In,
      Shadow_Out,
      Shadow_Etched_In,
      Shadow_Etched_Out);
   pragma Convention (C, Gtk_Shadow_Type);

   type Gtk_State_Type is
     (State_Normal,
      State_Active,
      State_Prelight,
      State_Selected,
      State_Insensitive);
   pragma Convention (C, Gtk_State_Type);

   type Gtk_Submenu_Direction is (Direction_Left, Direction_Right);
   pragma Convention (C, Gtk_Submenu_Direction);

   type Gtk_Submenu_Placement is (Top_Bottom, Left_Right);
   pragma Convention (C, Gtk_Submenu_Placement);

   type Gtk_Text_Window_Type is
     (Text_Window_Private,
      Text_Window_Widget,
      Text_Window_Text,
      Text_Window_Left,
      Text_Window_Right,
      Text_Window_Top,
      Text_Window_Bottom);
   pragma Convention (C, Gtk_Text_Window_Type);

   type Gtk_Toolbar_Child_Type is
     (Toolbar_Child_Space,
      Toolbar_Child_Button,
      Toolbar_Child_Togglebutton,
      Toolbar_Child_Radiobutton,
      Toolbar_Child_Widget);
   pragma Convention (C, Gtk_Toolbar_Child_Type);

   type Gtk_Toolbar_Style is (Toolbar_Icons, Toolbar_Text, Toolbar_Both);
   pragma Convention (C, Gtk_Toolbar_Style);

   type Gtk_Toolbar_Space_Style is (Toolbar_Space_Empty, Toolbar_Space_Line);
   pragma Convention (C, Gtk_Toolbar_Space_Style);

   type Gtk_Tree_View_Mode is (Tree_View_Line, Tree_View_Item);
   pragma Convention (C, Gtk_Tree_View_Mode);

   type Gtk_Update_Type is
     (Update_Continuous,
      Update_Discontinuous,
      Update_Delayed);
   pragma Convention (C, Gtk_Update_Type);

   type Gtk_Visibility is
     (Visibility_None, Visibility_Partial, Visibility_Full);
   pragma Convention (C, Gtk_Visibility);

   type Gtk_Window_Position is
     (Win_Pos_None,
      Win_Pos_Center,
      Win_Pos_Mouse,
      Win_Pos_Center_Always,
      Win_Pos_Center_On_Parent);
   pragma Convention (C, Gtk_Window_Position);

   type Gtk_Window_Type is (Window_Toplevel, Window_Popup);
   pragma Convention (C, Gtk_Window_Type);

   type Gtk_Wrap_Mode is
     (Wrap_None,
      Wrap_Char,
      Wrap_Word);
   pragma Convention (C, Gtk_Wrap_Mode);

   type Gtk_Sort_Type is
     (Sort_Ascending,
      Sort_Descending);
   pragma Convention (C, Gtk_Sort_Type);
   --  How to sort

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

   ----------------
   -- Properties --
   ----------------
   --  The following packages and types are used to represent properties of
   --  the given type. They are used in the packages that use these properties

   package Relief_Style_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Relief_Style);
   package Resize_Mode_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Resize_Mode);
   package Arrow_Type_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Arrow_Type);
   package Shadow_Type_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Shadow_Type);
   package Update_Type_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Update_Type);
   package Position_Type_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Position_Type);
   package Button_Box_Style_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Button_Box_Style);
   package Justification_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Justification);
   package Orientation_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Orientation);
   package Window_Type_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Window_Type);
   package Window_Position_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Window_Position);
   package Text_Direction_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Text_Direction);
   package Wrap_Mode_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Text_Direction);

   type Property_Gtk_Relief_Style  is new Relief_Style_Properties.Property;
   type Property_Gtk_Resize_Mode   is new Resize_Mode_Properties.Property;
   type Property_Gtk_Arrow_Type    is new Arrow_Type_Properties.Property;
   type Property_Gtk_Shadow_Type   is new Shadow_Type_Properties.Property;
   type Property_Gtk_Update_Type   is new Update_Type_Properties.Property;
   type Property_Gtk_Position_Type is new Position_Type_Properties.Property;
   type Property_Gtk_Button_Box_Style is new
     Button_Box_Style_Properties.Property;
   type Property_Gtk_Justification is new Justification_Properties.Property;
   type Property_Gtk_Orientation   is new Orientation_Properties.Property;
   type Property_Gtk_Window_Type   is new Window_Type_Properties.Property;
   type Property_Gtk_Window_Position is new
     Window_Position_Properties.Property;
   type Property_Gtk_Text_Direction is new Text_Direction_Properties.Property;
   type Property_Gtk_Wrap_Mode     is new Wrap_Mode_Properties.Property;

end Gtk.Enums;
