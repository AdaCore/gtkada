------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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


pragma Warnings (Off, "*is already use-visible*");
with Glib.GSlist;             use Glib.GSlist;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Glist;              use Glib.Glist;

package Gtk.Enums is

   type Gtk_Arrow_Type is (
      Arrow_Up,
      Arrow_Down,
      Arrow_Left,
      Arrow_Right,
      Arrow_None);
   pragma Convention (C, Gtk_Arrow_Type);
   --  Indicates the direction in which an arrow should point.

   type Gtk_Accessible_Text_Content_Change is (
      Accessible_Text_Content_Change_Insert,
      Accessible_Text_Content_Change_Remove);
   pragma Convention (C, Gtk_Accessible_Text_Content_Change);
   --  The type of contents change operation.

   type Gtk_Builder_Closure_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Builder_Closure_Flags);
   --  The list of flags that can be passed to Gtk.Builder.Create_Closure.
   --
   --  New values may be added in the future for new features, so external
   --  implementations of [ifaceGtk.BuilderScope] should test the flags for
   --  unknown values and raise a Gtk.Builder.Builder_Error_Invalid_Attribute
   --  error when they encounter one.

   Builder_Closure_Swapped : constant Gtk_Builder_Closure_Flags := 1;

   type Gtk_Corner_Type is (
      Corner_Top_Left,
      Corner_Bottom_Left,
      Corner_Top_Right,
      Corner_Bottom_Right);
   pragma Convention (C, Gtk_Corner_Type);
   --  Specifies which corner a child widget should be placed in when packed
   --  into a `GtkScrolledWindow.`
   --
   --  This is effectively the opposite of where the scroll bars are placed.

   type Gtk_Delete_Type is (
      Delete_Chars,
      Delete_Word_Ends,
      Delete_Words,
      Delete_Display_Lines,
      Delete_Display_Line_Ends,
      Delete_Paragraph_Ends,
      Delete_Paragraphs,
      Delete_Whitespace);
   pragma Convention (C, Gtk_Delete_Type);
   --  Passed to various keybinding signals for deleting text.

   type Gtk_Direction_Type is (
      Dir_Tab_Forward,
      Dir_Tab_Backward,
      Dir_Up,
      Dir_Down,
      Dir_Left,
      Dir_Right);
   pragma Convention (C, Gtk_Direction_Type);
   --  Focus movement types.

   type Gtk_Icon_Size is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Icon_Size);
   --  Built-in icon sizes.
   --
   --  Icon sizes default to being inherited. Where they cannot be inherited,
   --  text size is the default.
   --
   --  All widgets which use `GtkIconSize` set the normal-icons or large-icons
   --  style classes correspondingly, and let themes determine the actual size
   --  to be used with the `-gtk-icon-size` CSS property.

   Icon_Size_Inherit : constant Gtk_Icon_Size := 0;
   Icon_Size_Normal : constant Gtk_Icon_Size := 1;
   Icon_Size_Large : constant Gtk_Icon_Size := 2;

   type Gtk_Image_Type is (
      Image_Empty,
      Image_Icon_Name,
      Image_Gicon,
      Image_Paintable);
   pragma Convention (C, Gtk_Image_Type);
   --  Describes the image data representation used by a [classGtk.Image].
   --
   --  If you want to get the image from the widget, you can only get the
   --  currently-stored representation; for instance, if the
   --  gtk_image_get_storage_type returns Gtk.Enums.Image_Paintable, then you
   --  can call gtk_image_get_paintable.
   --
   --  For empty images, you can request any storage type (call any of the
   --  "get" functions), but they will all return null values.

   type Gtk_Baseline_Position is (
      Baseline_Position_Top,
      Baseline_Position_Center,
      Baseline_Position_Bottom);
   pragma Convention (C, Gtk_Baseline_Position);
   --  Baseline position in a row of widgets.
   --
   --  Whenever a container has some form of natural row it may align children
   --  in that row along a common typographical baseline. If the amount of
   --  vertical space in the row is taller than the total requested height of
   --  the baseline-aligned children then it can use a `GtkBaselinePosition` to
   --  select where to put the baseline inside the extra available space.

   type Gtk_Justification is (
      Justify_Left,
      Justify_Right,
      Justify_Center,
      Justify_Fill);
   pragma Convention (C, Gtk_Justification);
   --  Used for justifying the text inside a [classLabel] widget.

   type Gtk_Movement_Step is (
      Movement_Logical_Positions,
      Movement_Visual_Positions,
      Movement_Words,
      Movement_Display_Lines,
      Movement_Display_Line_Ends,
      Movement_Paragraphs,
      Movement_Paragraph_Ends,
      Movement_Pages,
      Movement_Buffer_Ends,
      Movement_Horizontal_Pages);
   pragma Convention (C, Gtk_Movement_Step);
   --  Passed as argument to various keybinding signals for moving the cursor
   --  position.

   type Gtk_Input_Hints is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Input_Hints);
   --  Describes hints that might be taken into account by input methods or
   --  applications.
   --
   --  Note that input methods may already tailor their behaviour according to
   --  the [enumInputpurpose] of the entry.
   --
   --  Some common sense is expected when using these flags - mixing
   --  Gtk.Enums.Input_Hint_Lowercase with any of the uppercase hints makes no
   --  sense.
   --
   --  This enumeration may be extended in the future; input methods should
   --  ignore unknown values.

   Input_Hint_None : constant Gtk_Input_Hints := 0;
   Input_Hint_Spellcheck : constant Gtk_Input_Hints := 1;
   Input_Hint_No_Spellcheck : constant Gtk_Input_Hints := 2;
   Input_Hint_Word_Completion : constant Gtk_Input_Hints := 4;
   Input_Hint_Lowercase : constant Gtk_Input_Hints := 8;
   Input_Hint_Uppercase_Chars : constant Gtk_Input_Hints := 16;
   Input_Hint_Uppercase_Words : constant Gtk_Input_Hints := 32;
   Input_Hint_Uppercase_Sentences : constant Gtk_Input_Hints := 64;
   Input_Hint_Inhibit_Osk : constant Gtk_Input_Hints := 128;
   Input_Hint_Vertical_Writing : constant Gtk_Input_Hints := 256;
   Input_Hint_Emoji : constant Gtk_Input_Hints := 512;
   Input_Hint_No_Emoji : constant Gtk_Input_Hints := 1024;
   Input_Hint_Private : constant Gtk_Input_Hints := 2048;

   type Gtk_Input_Purpose is (
      Input_Purpose_Free_Form,
      Input_Purpose_Alpha,
      Input_Purpose_Digits,
      Input_Purpose_Number,
      Input_Purpose_Phone,
      Input_Purpose_Url,
      Input_Purpose_Email,
      Input_Purpose_Name,
      Input_Purpose_Password,
      Input_Purpose_Pin,
      Input_Purpose_Terminal);
   pragma Convention (C, Gtk_Input_Purpose);
   --  Describes primary purpose of the input widget.
   --
   --  This information is useful for on-screen keyboards and similar input
   --  methods to decide which keys should be presented to the user.
   --
   --  Note that the purpose is not meant to impose a totally strict rule
   --  about allowed characters, and does not replace input validation. It is
   --  fine for an on-screen keyboard to let the user override the character
   --  set restriction that is expressed by the purpose. The application is
   --  expected to validate the entry contents, even if it specified a purpose.
   --
   --  The difference between Gtk.Enums.Input_Purpose_Digits and
   --  Gtk.Enums.Input_Purpose_Number is that the former accepts only digits
   --  while the latter also some punctuation (like commas or points, plus,
   --  minus) and "e" or "E" as in 3.14E+000.
   --
   --  This enumeration may be extended in the future; input methods should
   --  interpret unknown values as "free form".

   type Gtk_List_Tab_Behavior is (
      List_Tab_All,
      List_Tab_Item,
      List_Tab_Cell);
   pragma Convention (C, Gtk_List_Tab_Behavior);
   --  Used to configure the focus behavior in the `GTK_DIR_TAB_FORWARD` and
   --  `GTK_DIR_TAB_BACKWARD` direction, like the <kbd>Tab</kbd> key in a
   --  [classGtk.ListView].

   type Gtk_List_Scroll_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_List_Scroll_Flags);
   --  List of actions to perform when scrolling to items in a list widget.

   List_Scroll_None : constant Gtk_List_Scroll_Flags := 0;
   List_Scroll_Focus : constant Gtk_List_Scroll_Flags := 1;
   List_Scroll_Select : constant Gtk_List_Scroll_Flags := 2;

   type Gtk_Natural_Wrap_Mode is (
      Natural_Wrap_Inherit,
      Natural_Wrap_None,
      Natural_Wrap_Word);
   pragma Convention (C, Gtk_Natural_Wrap_Mode);
   --  Options for selecting a different wrap mode for natural size requests.
   --
   --  See for example the [propertyGtk.Label:natural-wrap-mode] property.

   type Gtk_Number_Up_Layout is (
      Left_To_Right_Top_To_Bottom,
      Left_To_Right_Bottom_To_Top,
      Right_To_Left_Top_To_Bottom,
      Right_To_Left_Bottom_To_Top,
      Top_To_Bottom_Left_To_Right,
      Top_To_Bottom_Right_To_Left,
      Bottom_To_Top_Left_To_Right,
      Bottom_To_Top_Right_To_Left);
   pragma Convention (C, Gtk_Number_Up_Layout);
   --  Used to determine the layout of pages on a sheet when printing multiple
   --  pages per sheet.

   type Gtk_Orientation is (
      Orientation_Horizontal,
      Orientation_Vertical);
   pragma Convention (C, Gtk_Orientation);
   --  Represents the orientation of widgets and other objects.
   --
   --  Typical examples are [classBox] or [classGesturepan].

   type Gtk_Pack_Type is (
      Pack_Start,
      Pack_End);
   pragma Convention (C, Gtk_Pack_Type);
   --  Represents the packing location of a children in its parent.
   --
   --  See [classWindowcontrols] for example.

   type Gtk_Page_Orientation is (
      Page_Orientation_Portrait,
      Page_Orientation_Landscape,
      Page_Orientation_Reverse_Portrait,
      Page_Orientation_Reverse_Landscape);
   pragma Convention (C, Gtk_Page_Orientation);
   --  See also gtk_print_settings_set_orientation.

   type Gtk_Page_Set is (
      Page_Set_All,
      Page_Set_Even,
      Page_Set_Odd);
   pragma Convention (C, Gtk_Page_Set);
   --  See also gtk_print_job_set_page_set.

   type Gtk_Pan_Direction is (
      Pan_Direction_Left,
      Pan_Direction_Right,
      Pan_Direction_Up,
      Pan_Direction_Down);
   pragma Convention (C, Gtk_Pan_Direction);
   --  Describes the panning direction of a [classGesturepan].

   type Gtk_Propagation_Phase is (
      Phase_None,
      Phase_Capture,
      Phase_Bubble,
      Phase_Target);
   pragma Convention (C, Gtk_Propagation_Phase);
   --  Describes the stage at which events are fed into a
   --  [classEventcontroller].

   type Gtk_Event_Sequence_State is (
      Event_Sequence_None,
      Event_Sequence_Claimed,
      Event_Sequence_Denied);
   pragma Convention (C, Gtk_Event_Sequence_State);
   --  Describes the state of a [structGdk.EventSequence] in a [classGesture].

   type Gtk_Level_Bar_Mode is (
      Level_Bar_Mode_Continuous,
      Level_Bar_Mode_Discrete);
   pragma Convention (C, Gtk_Level_Bar_Mode);
   --  Describes how [classLevelbar] contents should be rendered.
   --
   --  Note that this enumeration could be extended with additional modes in
   --  the future.

   type Gtk_Policy_Type is (
      Policy_Always,
      Policy_Automatic,
      Policy_Never,
      Policy_External);
   pragma Convention (C, Gtk_Policy_Type);
   --  Determines how the size should be computed to achieve the one of the
   --  visibility mode for the scrollbars.

   type Gtk_Notebook_Tab is (
      Notebook_Tab_First,
      Notebook_Tab_Last);
   pragma Convention (C, Gtk_Notebook_Tab);
   --  The parameter used in the action signals of `GtkNotebook`.

   type Gtk_Position_Type is (
      Pos_Left,
      Pos_Right,
      Pos_Top,
      Pos_Bottom);
   pragma Convention (C, Gtk_Position_Type);
   --  Describes which edge of a widget a certain feature is positioned at.
   --
   --  For examples, see the tabs of a [classNotebook], or the label of a
   --  [classScale].

   type Gtk_Print_Duplex is (
      Print_Duplex_Simplex,
      Print_Duplex_Horizontal,
      Print_Duplex_Vertical);
   pragma Convention (C, Gtk_Print_Duplex);
   --  See also gtk_print_settings_set_duplex.

   type Gtk_Print_Pages is (
      Print_Pages_All,
      Print_Pages_Current,
      Print_Pages_Ranges,
      Print_Pages_Selection);
   pragma Convention (C, Gtk_Print_Pages);
   --  See also gtk_print_job_set_pages

   type Gtk_Print_Quality is (
      Print_Quality_Low,
      Print_Quality_Normal,
      Print_Quality_High,
      Print_Quality_Draft);
   pragma Convention (C, Gtk_Print_Quality);
   --  See also gtk_print_settings_set_quality.

   type Gtk_Scroll_Step is (
      Scroll_Steps,
      Scroll_Pages,
      Scroll_Ends,
      Scroll_Horizontal_Steps,
      Scroll_Horizontal_Pages,
      Scroll_Horizontal_Ends);
   pragma Convention (C, Gtk_Scroll_Step);
   --  Passed as argument to various keybinding signals.

   type Gtk_Scroll_Type is (
      Scroll_None,
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
   --  Scrolling types.

   type Gtk_Scrollable_Policy is (
      Scroll_Minimum,
      Scroll_Natural);
   pragma Convention (C, Gtk_Scrollable_Policy);
   --  Defines the policy to be used in a scrollable widget when updating the
   --  scrolled window adjustments in a given orientation.

   type Gtk_Selection_Mode is (
      Selection_None,
      Selection_Single,
      Selection_Browse,
      Selection_Multiple);
   pragma Convention (C, Gtk_Selection_Mode);
   --  Used to control what selections users are allowed to make.

   type Gtk_Sensitivity_Type is (
      Sensitivity_Auto,
      Sensitivity_On,
      Sensitivity_Off);
   pragma Convention (C, Gtk_Sensitivity_Type);
   --  Determines how GTK handles the sensitivity of various controls, such as
   --  combo box buttons.

   type Gtk_Size_Request_Mode is (
      Height_For_Width,
      Width_For_Height,
      Constant_Size);
   pragma Convention (C, Gtk_Size_Request_Mode);
   --  Specifies a preference for height-for-width or width-for-height
   --  geometry management.

   type Gtk_Sort_Type is (
      Sort_Ascending,
      Sort_Descending);
   pragma Convention (C, Gtk_Sort_Type);
   --  Determines the direction of a sort.

   type Gtk_State_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_State_Flags);
   --  Describes a widget state.
   --
   --  Widget states are used to match the widget against CSS pseudo-classes.
   --  Note that GTK extends the regular CSS classes and sometimes uses
   --  different names.

   Gtk_State_Flag_Normal : constant Gtk_State_Flags := 0;
   Gtk_State_Flag_Active : constant Gtk_State_Flags := 1;
   Gtk_State_Flag_Prelight : constant Gtk_State_Flags := 2;
   Gtk_State_Flag_Selected : constant Gtk_State_Flags := 4;
   Gtk_State_Flag_Insensitive : constant Gtk_State_Flags := 8;
   Gtk_State_Flag_Inconsistent : constant Gtk_State_Flags := 16;
   Gtk_State_Flag_Focused : constant Gtk_State_Flags := 32;
   Gtk_State_Flag_Backdrop : constant Gtk_State_Flags := 64;
   Gtk_State_Flag_Dir_Ltr : constant Gtk_State_Flags := 128;
   Gtk_State_Flag_Dir_Rtl : constant Gtk_State_Flags := 256;
   Gtk_State_Flag_Link : constant Gtk_State_Flags := 512;
   Gtk_State_Flag_Visited : constant Gtk_State_Flags := 1024;
   Gtk_State_Flag_Checked : constant Gtk_State_Flags := 2048;
   Gtk_State_Flag_Drop_Active : constant Gtk_State_Flags := 4096;
   Gtk_State_Flag_Focus_Visible : constant Gtk_State_Flags := 8192;
   Gtk_State_Flag_Focus_Within : constant Gtk_State_Flags := 16384;

   type Gtk_Text_Direction is (
      Text_Dir_None,
      Text_Dir_Ltr,
      Text_Dir_Rtl);
   pragma Convention (C, Gtk_Text_Direction);
   --  Reading directions for text.

   type Gtk_Text_Window_Type is (
      Text_Window_Widget,
      Text_Window_Text,
      Text_Window_Left,
      Text_Window_Right,
      Text_Window_Top,
      Text_Window_Bottom);
   pragma Convention (C, Gtk_Text_Window_Type);
   --  Used to reference the parts of `GtkTextView`.

   for Gtk_Text_Window_Type use (
      Text_Window_Widget => 1,
      Text_Window_Text => 2,
      Text_Window_Left => 3,
      Text_Window_Right => 4,
      Text_Window_Top => 5,
      Text_Window_Bottom => 6);

   type Gtk_Tree_View_Grid_Lines is (
      Grid_Lines_None,
      Grid_Lines_Horizontal,
      Grid_Lines_Vertical,
      Grid_Lines_Both);
   pragma Convention (C, Gtk_Tree_View_Grid_Lines);
   --  Used to indicate which grid lines to draw in a tree view.

   type Gtk_Unit is (
      None,
      Points,
      Inch,
      Mm);
   pragma Convention (C, Gtk_Unit);
   --  See also gtk_print_settings_set_paper_width.

   type Gtk_Window_Gravity is (
      Window_Gravity_Top_Left,
      Window_Gravity_Top,
      Window_Gravity_Top_Right,
      Window_Gravity_Left,
      Window_Gravity_Center,
      Window_Gravity_Right,
      Window_Gravity_Bottom_Left,
      Window_Gravity_Bottom,
      Window_Gravity_Bottom_Right,
      Window_Gravity_Top_Start,
      Window_Gravity_Top_End,
      Window_Gravity_Start,
      Window_Gravity_End,
      Window_Gravity_Bottom_Start,
      Window_Gravity_Bottom_End);
   pragma Convention (C, Gtk_Window_Gravity);
   --  Determines which point or edge of a window is meant to remain fixed
   --  when a window changes size.

   type Gtk_Wrap_Mode is (
      Wrap_None,
      Wrap_Char,
      Wrap_Word,
      Wrap_Word_Char);
   pragma Convention (C, Gtk_Wrap_Mode);
   --  Describes a type of line wrapping.

   type Gtk_Ordering is (
      Smaller,
      Equal,
      Larger);
   pragma Convention (C, Gtk_Ordering);
   --  Describes the way two values can be compared.
   --
   --  These values can be used with a [callbackGlib.CompareFunc]. However, a
   --  `GCompareFunc` is allowed to return any integer values. For converting
   --  such a value to a `GtkOrdering` value, use
   --  [funcGtk.Ordering.from_cmpfunc].

   for Gtk_Ordering use (
      Smaller => -1,
      Equal => 0,
      Larger => 1);

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Arrow_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Arrow_Type);
   type Property_Gtk_Arrow_Type is new Gtk_Arrow_Type_Properties.Property;

   package Gtk_Accessible_Text_Content_Change_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Accessible_Text_Content_Change);
   type Property_Gtk_Accessible_Text_Content_Change is new Gtk_Accessible_Text_Content_Change_Properties.Property;

   package Gtk_Builder_Closure_Flags_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Builder_Closure_Flags);
   type Property_Gtk_Builder_Closure_Flags is new Gtk_Builder_Closure_Flags_Properties.Property;

   package Gtk_Corner_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Corner_Type);
   type Property_Gtk_Corner_Type is new Gtk_Corner_Type_Properties.Property;

   package Gtk_Delete_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Delete_Type);
   type Property_Gtk_Delete_Type is new Gtk_Delete_Type_Properties.Property;

   package Gtk_Direction_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Direction_Type);
   type Property_Gtk_Direction_Type is new Gtk_Direction_Type_Properties.Property;

   package Gtk_Icon_Size_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Icon_Size);
   type Property_Gtk_Icon_Size is new Gtk_Icon_Size_Properties.Property;

   package Gtk_Image_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Image_Type);
   type Property_Gtk_Image_Type is new Gtk_Image_Type_Properties.Property;

   package Gtk_Baseline_Position_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Baseline_Position);
   type Property_Gtk_Baseline_Position is new Gtk_Baseline_Position_Properties.Property;

   package Gtk_Justification_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Justification);
   type Property_Gtk_Justification is new Gtk_Justification_Properties.Property;

   package Gtk_Movement_Step_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Movement_Step);
   type Property_Gtk_Movement_Step is new Gtk_Movement_Step_Properties.Property;

   package Gtk_Input_Hints_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Input_Hints);
   type Property_Gtk_Input_Hints is new Gtk_Input_Hints_Properties.Property;

   package Gtk_Input_Purpose_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Input_Purpose);
   type Property_Gtk_Input_Purpose is new Gtk_Input_Purpose_Properties.Property;

   package Gtk_List_Tab_Behavior_Properties is
      new Generic_Internal_Discrete_Property (Gtk_List_Tab_Behavior);
   type Property_Gtk_List_Tab_Behavior is new Gtk_List_Tab_Behavior_Properties.Property;

   package Gtk_List_Scroll_Flags_Properties is
      new Generic_Internal_Discrete_Property (Gtk_List_Scroll_Flags);
   type Property_Gtk_List_Scroll_Flags is new Gtk_List_Scroll_Flags_Properties.Property;

   package Gtk_Natural_Wrap_Mode_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Natural_Wrap_Mode);
   type Property_Gtk_Natural_Wrap_Mode is new Gtk_Natural_Wrap_Mode_Properties.Property;

   package Gtk_Number_Up_Layout_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Number_Up_Layout);
   type Property_Gtk_Number_Up_Layout is new Gtk_Number_Up_Layout_Properties.Property;

   package Gtk_Orientation_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Orientation);
   type Property_Gtk_Orientation is new Gtk_Orientation_Properties.Property;

   package Gtk_Pack_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Pack_Type);
   type Property_Gtk_Pack_Type is new Gtk_Pack_Type_Properties.Property;

   package Gtk_Page_Orientation_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Page_Orientation);
   type Property_Gtk_Page_Orientation is new Gtk_Page_Orientation_Properties.Property;

   package Gtk_Page_Set_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Page_Set);
   type Property_Gtk_Page_Set is new Gtk_Page_Set_Properties.Property;

   package Gtk_Pan_Direction_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Pan_Direction);
   type Property_Gtk_Pan_Direction is new Gtk_Pan_Direction_Properties.Property;

   package Gtk_Propagation_Phase_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Propagation_Phase);
   type Property_Gtk_Propagation_Phase is new Gtk_Propagation_Phase_Properties.Property;

   package Gtk_Event_Sequence_State_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Event_Sequence_State);
   type Property_Gtk_Event_Sequence_State is new Gtk_Event_Sequence_State_Properties.Property;

   package Gtk_Level_Bar_Mode_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Level_Bar_Mode);
   type Property_Gtk_Level_Bar_Mode is new Gtk_Level_Bar_Mode_Properties.Property;

   package Gtk_Policy_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Policy_Type);
   type Property_Gtk_Policy_Type is new Gtk_Policy_Type_Properties.Property;

   package Gtk_Notebook_Tab_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Notebook_Tab);
   type Property_Gtk_Notebook_Tab is new Gtk_Notebook_Tab_Properties.Property;

   package Gtk_Position_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Position_Type);
   type Property_Gtk_Position_Type is new Gtk_Position_Type_Properties.Property;

   package Gtk_Print_Duplex_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Print_Duplex);
   type Property_Gtk_Print_Duplex is new Gtk_Print_Duplex_Properties.Property;

   package Gtk_Print_Pages_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Print_Pages);
   type Property_Gtk_Print_Pages is new Gtk_Print_Pages_Properties.Property;

   package Gtk_Print_Quality_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Print_Quality);
   type Property_Gtk_Print_Quality is new Gtk_Print_Quality_Properties.Property;

   package Gtk_Scroll_Step_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Scroll_Step);
   type Property_Gtk_Scroll_Step is new Gtk_Scroll_Step_Properties.Property;

   package Gtk_Scroll_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Scroll_Type);
   type Property_Gtk_Scroll_Type is new Gtk_Scroll_Type_Properties.Property;

   package Gtk_Scrollable_Policy_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Scrollable_Policy);
   type Property_Gtk_Scrollable_Policy is new Gtk_Scrollable_Policy_Properties.Property;

   package Gtk_Selection_Mode_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Selection_Mode);
   type Property_Gtk_Selection_Mode is new Gtk_Selection_Mode_Properties.Property;

   package Gtk_Sensitivity_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Sensitivity_Type);
   type Property_Gtk_Sensitivity_Type is new Gtk_Sensitivity_Type_Properties.Property;

   package Gtk_Size_Request_Mode_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Size_Request_Mode);
   type Property_Gtk_Size_Request_Mode is new Gtk_Size_Request_Mode_Properties.Property;

   package Gtk_Sort_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Sort_Type);
   type Property_Gtk_Sort_Type is new Gtk_Sort_Type_Properties.Property;

   package Gtk_State_Flags_Properties is
      new Generic_Internal_Discrete_Property (Gtk_State_Flags);
   type Property_Gtk_State_Flags is new Gtk_State_Flags_Properties.Property;

   package Gtk_Text_Direction_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Text_Direction);
   type Property_Gtk_Text_Direction is new Gtk_Text_Direction_Properties.Property;

   package Gtk_Text_Window_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Text_Window_Type);
   type Property_Gtk_Text_Window_Type is new Gtk_Text_Window_Type_Properties.Property;

   package Gtk_Tree_View_Grid_Lines_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Tree_View_Grid_Lines);
   type Property_Gtk_Tree_View_Grid_Lines is new Gtk_Tree_View_Grid_Lines_Properties.Property;

   package Gtk_Unit_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Unit);
   type Property_Gtk_Unit is new Gtk_Unit_Properties.Property;

   package Gtk_Window_Gravity_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Window_Gravity);
   type Property_Gtk_Window_Gravity is new Gtk_Window_Gravity_Properties.Property;

   package Gtk_Wrap_Mode_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Wrap_Mode);
   type Property_Gtk_Wrap_Mode is new Gtk_Wrap_Mode_Properties.Property;

   package Gtk_Ordering_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Ordering);
   type Property_Gtk_Ordering is new Gtk_Ordering_Properties.Property;

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Convert (S : String) return System.Address;
   function Convert (S : System.Address) return String;
   package String_List is new Glib.Glist.Generic_List (UTF8_String);
   package String_SList is new Glib.GSlist.Generic_SList (UTF8_String);
   --  Warning: when you create this list, new memory gets allocated for
   --  all the strings. You should use the function Free_String_List
   --  instead of Glib.Glist.Free to be sure to free this memory.

   procedure Free_String_List (List : in out String_List.Glist);
   procedure Free_String_List (List : in out String_SList.GSlist);
   --  Free the memory occupied by all the strings in the list, as well
   --  as the memory occupied by the list itself.

end Gtk.Enums;
