------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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
   --  Used to indicate the direction in which an arrow should point.

   type Gtk_Attach_Options is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Attach_Options);
   --  Denotes the expansion properties that a widget will have when it (or
   --  its parent) is resized.

   Expand : constant Gtk_Attach_Options := 1;
   Shrink : constant Gtk_Attach_Options := 2;
   Fill : constant Gtk_Attach_Options := 4;

   type Gtk_Button_Box_Style is (
      Buttonbox_Spread,
      Buttonbox_Edge,
      Buttonbox_Start,
      Buttonbox_End,
      Buttonbox_Center,
      Buttonbox_Expand);
   pragma Convention (C, Gtk_Button_Box_Style);
   --  Used to dictate the style that a Gtk.Button_Box.Gtk_Button_Box uses to
   --  layout the buttons it contains.

   for Gtk_Button_Box_Style use (
      Buttonbox_Spread => 1,
      Buttonbox_Edge => 2,
      Buttonbox_Start => 3,
      Buttonbox_End => 4,
      Buttonbox_Center => 5,
      Buttonbox_Expand => 6);

   type Gtk_Corner_Type is (
      Corner_Top_Left,
      Corner_Bottom_Left,
      Corner_Top_Right,
      Corner_Bottom_Right);
   pragma Convention (C, Gtk_Corner_Type);
   --  Specifies which corner a child widget should be placed in when packed
   --  into a Gtk.Scrolled_Window.Gtk_Scrolled_Window. This is effectively the
   --  opposite of where the scroll bars are placed.

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
   --  See also: Gtk.GEntry.Gtk_Entry::delete-from-cursor.

   type Gtk_Direction_Type is (
      Dir_Tab_Forward,
      Dir_Tab_Backward,
      Dir_Up,
      Dir_Down,
      Dir_Left,
      Dir_Right);
   pragma Convention (C, Gtk_Direction_Type);
   --  Focus movement types.

   type Gtk_Drag_Result is (
      Drag_Result_Success,
      Drag_Result_No_Target,
      Drag_Result_User_Cancelled,
      Drag_Result_Timeout_Expired,
      Drag_Result_Grab_Broken,
      Drag_Result_Error);
   pragma Convention (C, Gtk_Drag_Result);
   --  Gives an indication why a drag operation failed. The value can by
   --  obtained by connecting to the Gtk.Widget.Gtk_Widget::drag-failed signal.

   type Gtk_Expander_Style is (
      Expander_Collapsed,
      Expander_Semi_Collapsed,
      Expander_Semi_Expanded,
      Expander_Expanded);
   pragma Convention (C, Gtk_Expander_Style);
   --  Used to specify the style of the expanders drawn by a
   --  Gtk.Tree_View.Gtk_Tree_View.

   type Gtk_Icon_Size is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Icon_Size);
   --  Built-in stock icon sizes.

   Icon_Size_Invalid : constant Gtk_Icon_Size := 0;
   Icon_Size_Menu : constant Gtk_Icon_Size := 1;
   Icon_Size_Small_Toolbar : constant Gtk_Icon_Size := 2;
   Icon_Size_Large_Toolbar : constant Gtk_Icon_Size := 3;
   Icon_Size_Button : constant Gtk_Icon_Size := 4;
   Icon_Size_Dnd : constant Gtk_Icon_Size := 5;
   Icon_Size_Dialog : constant Gtk_Icon_Size := 6;

   type Gtk_Junction_Sides is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Junction_Sides);
   --  Describes how a rendered element connects to adjacent elements.

   Junction_None : constant Gtk_Junction_Sides := 0;
   Junction_Corner_Topleft : constant Gtk_Junction_Sides := 1;
   Junction_Corner_Topright : constant Gtk_Junction_Sides := 2;
   Junction_Corner_Bottomleft : constant Gtk_Junction_Sides := 4;
   Junction_Corner_Bottomright : constant Gtk_Junction_Sides := 8;
   Junction_Top : constant Gtk_Junction_Sides := 3;
   Junction_Bottom : constant Gtk_Junction_Sides := 12;
   Junction_Left : constant Gtk_Junction_Sides := 5;
   Junction_Right : constant Gtk_Junction_Sides := 10;

   type Gtk_Baseline_Position is (
      Baseline_Position_Top,
      Baseline_Position_Center,
      Baseline_Position_Bottom);
   pragma Convention (C, Gtk_Baseline_Position);
   --  Whenever a container has some form of natural row it may align children
   --  in that row along a common typographical baseline. If the amount of
   --  verical space in the row is taller than the total requested height of
   --  the baseline-aligned children then it can use a
   --  Gtk.Enums.Gtk_Baseline_Position to select where to put the baseline
   --  inside the extra availible space.

   type Gtk_Justification is (
      Justify_Left,
      Justify_Right,
      Justify_Center,
      Justify_Fill);
   pragma Convention (C, Gtk_Justification);
   --  Used for justifying the text inside a Gtk.Label.Gtk_Label widget. (See
   --  also Gtk.Alignment.Gtk_Alignment).

   type Gtk_Menu_Direction_Type is (
      Menu_Dir_Parent,
      Menu_Dir_Child,
      Menu_Dir_Next,
      Menu_Dir_Prev);
   pragma Convention (C, Gtk_Menu_Direction_Type);
   --  An enumeration representing directional movements within a menu.

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


   type Gtk_Input_Hints is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Input_Hints);
   --  Describes hints that might be taken into account by input methods or
   --  applications. Note that input methods may already tailor their behaviour
   --  according to the Gtk.Enums.Gtk_Input_Purpose of the entry.
   --
   --  Some common sense is expected when using these flags - mixing
   --  Gtk_Input_Hint_Lowercase with any of the uppercase hints makes no sense.
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
   --  Describes primary purpose of the input widget. This information is
   --  useful for on-screen keyboards and similar input methods to decide which
   --  keys should be presented to the user.
   --
   --  Note that the purpose is not meant to impose a totally strict rule
   --  about allowed characters, and does not replace input validation. It is
   --  fine for an on-screen keyboard to let the user override the character
   --  set restriction that is expressed by the purpose. The application is
   --  expected to validate the entry contents, even if it specified a purpose.
   --
   --  The difference between Gtk_Input_Purpose_Digits and
   --  Gtk_Input_Purpose_Number is that the former accepts only digits while
   --  the latter also some punctuation (like commas or points, plus, minus)
   --  and "e" or "E" as in 3.14E+000.
   --
   --  This enumeration may be extended in the future; input methods should
   --  interpret unknown values as "free form".

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
   --  Represents the orientation of widgets and other objects which can be
   --  switched between horizontal and vertical orientation on the fly, like
   --  Gtk.Toolbar.Gtk_Toolbar or Gtk.Gesture_Pan.Gtk_Gesture_Pan.

   type Gtk_Pack_Direction is (
      Pack_Direction_Ltr,
      Pack_Direction_Rtl,
      Pack_Direction_Ttb,
      Pack_Direction_Btt);
   pragma Convention (C, Gtk_Pack_Direction);
   --  Determines how widgets should be packed inside menubars and menuitems
   --  contained in menubars.

   type Gtk_Pack_Type is (
      Pack_Start,
      Pack_End);
   pragma Convention (C, Gtk_Pack_Type);
   --  Represents the packing location Gtk.Box.Gtk_Box children. (See:
   --  Gtk.Box.Gtk_Vbox, Gtk.Box.Gtk_Hbox, and Gtk.Button_Box.Gtk_Button_Box).

   type Gtk_Page_Orientation is (
      Page_Orientation_Portrait,
      Page_Orientation_Landscape,
      Page_Orientation_Reverse_Portrait,
      Page_Orientation_Reverse_Landscape);
   pragma Convention (C, Gtk_Page_Orientation);
   --  See also Gtk.Print_Settings.Set_Orientation.

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
   --  Describes the panning direction of a Gtk.Gesture_Pan.Gtk_Gesture_Pan

   type Gtk_Popover_Constraint is (
      Popover_Constraint_None,
      Popover_Constraint_Window);
   pragma Convention (C, Gtk_Popover_Constraint);
   --  Describes constraints to positioning of popovers. More values may be
   --  added to this enumeration in the future.

   type Gtk_Propagation_Phase is (
      Phase_None,
      Phase_Capture,
      Phase_Bubble,
      Phase_Target);
   pragma Convention (C, Gtk_Propagation_Phase);
   --  Describes the stage at which events are fed into a
   --  Gtk.Event_Controller.Gtk_Event_Controller.

   type Gtk_Event_Sequence_State is (
      Event_Sequence_None,
      Event_Sequence_Claimed,
      Event_Sequence_Denied);
   pragma Convention (C, Gtk_Event_Sequence_State);
   --  Describes the state of a Gdk.Event.Gdk_Event_Sequence in a
   --  Gtk.Gesture.Gtk_Gesture.

   type Gtk_Level_Bar_Mode is (
      Level_Bar_Mode_Continuous,
      Level_Bar_Mode_Discrete);
   pragma Convention (C, Gtk_Level_Bar_Mode);
   --  Describes how Gtk.Level_Bar.Gtk_Level_Bar contents should be rendered.
   --  Note that this enumeration could be extended with additional modes in
   --  the future.

   type Gtk_Path_Priority_Type is (
      Path_Prio_Lowest,
      Path_Prio_Gtk,
      Path_Prio_Application,
      Path_Prio_Theme,
      Path_Prio_Rc,
      Path_Prio_Highest);
   pragma Convention (C, Gtk_Path_Priority_Type);
   --  Priorities for path lookups. See also Gtk.Binding_Set.Add_Path.

   for Gtk_Path_Priority_Type use (
      Path_Prio_Lowest => 0,
      Path_Prio_Gtk => 4,
      Path_Prio_Application => 8,
      Path_Prio_Theme => 10,
      Path_Prio_Rc => 12,
      Path_Prio_Highest => 15);

   type Gtk_Path_Type is (
      Path_Widget,
      Path_Widget_Class,
      Path_Class);
   pragma Convention (C, Gtk_Path_Type);
   --  Widget path types. See also Gtk.Binding_Set.Add_Path.

   type Gtk_Policy_Type is (
      Policy_Always,
      Policy_Automatic,
      Policy_Never,
      Policy_External);
   pragma Convention (C, Gtk_Policy_Type);
   --  Determines how the size should be computed to achieve the one of the
   --  visibility mode for the scrollbars.

   type Gtk_Position_Type is (
      Pos_Left,
      Pos_Right,
      Pos_Top,
      Pos_Bottom);
   pragma Convention (C, Gtk_Position_Type);
   --  Describes which edge of a widget a certain feature is positioned at,
   --  e.g. the tabs of a Gtk.Notebook.Gtk_Notebook, the handle of a
   --  Gtk.Handle_Box.Gtk_Handle_Box or the label of a Gtk.Scale.Gtk_Scale.

   type Gtk_Print_Duplex is (
      Print_Duplex_Simplex,
      Print_Duplex_Horizontal,
      Print_Duplex_Vertical);
   pragma Convention (C, Gtk_Print_Duplex);
   --  See also Gtk.Print_Settings.Set_Duplex.

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
   --  See also Gtk.Print_Settings.Set_Quality.

   type Gtk_Region_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Region_Flags);
   --  Describes a region within a widget.

   Region_Even : constant Gtk_Region_Flags := 1;
   Region_Odd : constant Gtk_Region_Flags := 2;
   Region_First : constant Gtk_Region_Flags := 4;
   Region_Last : constant Gtk_Region_Flags := 8;
   Region_Only : constant Gtk_Region_Flags := 16;
   Region_Sorted : constant Gtk_Region_Flags := 32;

   type Gtk_Relief_Style is (
      Relief_Normal,
      Relief_Half,
      Relief_None);
   pragma Convention (C, Gtk_Relief_Style);
   --  Indicated the relief to be drawn around a Gtk.Button.Gtk_Button.

   type Gtk_Resize_Mode is (
      Resize_Parent,
      Resize_Queue,
      Resize_Immediate);
   pragma Convention (C, Gtk_Resize_Mode);


   type Gtk_Scroll_Step is (
      Scroll_Steps,
      Scroll_Pages,
      Scroll_Ends,
      Scroll_Horizontal_Steps,
      Scroll_Horizontal_Pages,
      Scroll_Horizontal_Ends);
   pragma Convention (C, Gtk_Scroll_Step);


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
   --  Determines how GTK+ handles the sensitivity of stepper arrows at the
   --  end of range widgets.

   type Gtk_Shadow_Type is (
      Shadow_None,
      Shadow_In,
      Shadow_Out,
      Shadow_Etched_In,
      Shadow_Etched_Out);
   pragma Convention (C, Gtk_Shadow_Type);
   --  Used to change the appearance of an outline typically provided by a
   --  Gtk.Frame.Gtk_Frame.
   --
   --  Note that many themes do not differentiate the appearance of the
   --  various shadow types: Either their is no visible shadow
   --  (Gtk_Shadow_None), or there is (any other value).

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
   --  Describes a widget state. Widget states are used to match the widget
   --  against CSS pseudo-classes. Note that GTK extends the regular CSS
   --  classes and sometimes uses different names.

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

   type Gtk_State_Type is (
      State_Normal,
      State_Active,
      State_Prelight,
      State_Selected,
      State_Insensitive,
      State_Inconsistent,
      State_Focused);
   pragma Convention (C, Gtk_State_Type);
   --  This type indicates the current state of a widget; the state determines
   --  how the widget is drawn. The Gtk.Enums.Gtk_State_Type enumeration is
   --  also used to identify different colors in a Gtk.Style.Gtk_Style for
   --  drawing, so states can be used for subparts of a widget as well as
   --  entire widgets.

   type Gtk_Target_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Target_Flags);
   --  The Gtk.Enums.Gtk_Target_Flags enumeration is used to specify
   --  constraints on a Gtk.Target_Entry.Gtk_Target_Entry.

   Gtk_Target_Same_App : constant Gtk_Target_Flags := 1;
   Gtk_Target_Same_Widget : constant Gtk_Target_Flags := 2;
   Gtk_Target_Other_App : constant Gtk_Target_Flags := 4;
   Gtk_Target_Other_Widget : constant Gtk_Target_Flags := 8;

   type Gtk_Text_Direction is (
      Text_Dir_None,
      Text_Dir_Ltr,
      Text_Dir_Rtl);
   pragma Convention (C, Gtk_Text_Direction);
   --  Reading directions for text.

   type Gtk_Text_Window_Type is (
      Text_Window_Private,
      Text_Window_Widget,
      Text_Window_Text,
      Text_Window_Left,
      Text_Window_Right,
      Text_Window_Top,
      Text_Window_Bottom);
   pragma Convention (C, Gtk_Text_Window_Type);
   --  Used to reference the parts of Gtk.Text_View.Gtk_Text_View.

   type Gtk_Tree_View_Grid_Lines is (
      Grid_Lines_None,
      Grid_Lines_Horizontal,
      Grid_Lines_Vertical,
      Grid_Lines_Both);
   pragma Convention (C, Gtk_Tree_View_Grid_Lines);
   --  Used to indicate which grid lines to draw in a tree view.

   type Gtk_Toolbar_Space_Style is (
      Toolbar_Space_Empty,
      Toolbar_Space_Line);
   pragma Convention (C, Gtk_Toolbar_Space_Style);
   --  Whether spacers are vertical lines or just blank.

   type Gtk_Toolbar_Style is (
      Toolbar_Icons,
      Toolbar_Text,
      Toolbar_Both,
      Toolbar_Both_Horiz);
   pragma Convention (C, Gtk_Toolbar_Style);
   --  Used to customize the appearance of a Gtk.Toolbar.Gtk_Toolbar. Note
   --  that setting the toolbar style overrides the user's preferences for the
   --  default toolbar style. Note that if the button has only a label set and
   --  GTK_TOOLBAR_ICONS is used, the label will be visible, and vice versa.

   type Gtk_Unit is (
      None,
      Points,
      Inch,
      Mm);
   pragma Convention (C, Gtk_Unit);
   --  See also Gtk.Print_Settings.Set_Paper_Width.

   type Gtk_Window_Position is (
      Win_Pos_None,
      Win_Pos_Center,
      Win_Pos_Mouse,
      Win_Pos_Center_Always,
      Win_Pos_Center_On_Parent);
   pragma Convention (C, Gtk_Window_Position);
   --  Window placement can be influenced using this enumeration. Note that
   --  using GTK_WIN_POS_CENTER_ALWAYS is almost always a bad idea. It won't
   --  necessarily work well with all window managers or on all windowing
   --  systems.

   type Gtk_Window_Type is (
      Window_Toplevel,
      Window_Popup);
   pragma Convention (C, Gtk_Window_Type);
   --  A Gtk.Window.Gtk_Window can be one of these types. Most things you'd
   --  consider a "window" should have type GTK_WINDOW_TOPLEVEL; windows with
   --  this type are managed by the window manager and have a frame by default
   --  (call Gtk.Window.Set_Decorated to toggle the frame). Windows with type
   --  GTK_WINDOW_POPUP are ignored by the window manager; window manager
   --  keybindings won't work on them, the window manager won't decorate the
   --  window with a frame, many GTK+ features that rely on the window manager
   --  will not work (e.g. resize grips and maximization/minimization).
   --  GTK_WINDOW_POPUP is used to implement widgets such as Gtk.Menu.Gtk_Menu
   --  or tooltips that you normally don't think of as windows per se. Nearly
   --  all windows should be GTK_WINDOW_TOPLEVEL. In particular, do not use
   --  GTK_WINDOW_POPUP just to turn off the window borders; use
   --  Gtk.Window.Set_Decorated for that.

   type Gtk_Wrap_Mode is (
      Wrap_None,
      Wrap_Char,
      Wrap_Word,
      Wrap_Word_Char);
   pragma Convention (C, Gtk_Wrap_Mode);
   --  Describes a type of line wrapping.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Arrow_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Arrow_Type);
   type Property_Gtk_Arrow_Type is new Gtk_Arrow_Type_Properties.Property;

   package Gtk_Attach_Options_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Attach_Options);
   type Property_Gtk_Attach_Options is new Gtk_Attach_Options_Properties.Property;

   package Gtk_Button_Box_Style_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Button_Box_Style);
   type Property_Gtk_Button_Box_Style is new Gtk_Button_Box_Style_Properties.Property;

   package Gtk_Corner_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Corner_Type);
   type Property_Gtk_Corner_Type is new Gtk_Corner_Type_Properties.Property;

   package Gtk_Delete_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Delete_Type);
   type Property_Gtk_Delete_Type is new Gtk_Delete_Type_Properties.Property;

   package Gtk_Direction_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Direction_Type);
   type Property_Gtk_Direction_Type is new Gtk_Direction_Type_Properties.Property;

   package Gtk_Drag_Result_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Drag_Result);
   type Property_Gtk_Drag_Result is new Gtk_Drag_Result_Properties.Property;

   package Gtk_Expander_Style_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Expander_Style);
   type Property_Gtk_Expander_Style is new Gtk_Expander_Style_Properties.Property;

   package Gtk_Icon_Size_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Icon_Size);
   type Property_Gtk_Icon_Size is new Gtk_Icon_Size_Properties.Property;

   package Gtk_Junction_Sides_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Junction_Sides);
   type Property_Gtk_Junction_Sides is new Gtk_Junction_Sides_Properties.Property;

   package Gtk_Baseline_Position_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Baseline_Position);
   type Property_Gtk_Baseline_Position is new Gtk_Baseline_Position_Properties.Property;

   package Gtk_Justification_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Justification);
   type Property_Gtk_Justification is new Gtk_Justification_Properties.Property;

   package Gtk_Menu_Direction_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Menu_Direction_Type);
   type Property_Gtk_Menu_Direction_Type is new Gtk_Menu_Direction_Type_Properties.Property;

   package Gtk_Movement_Step_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Movement_Step);
   type Property_Gtk_Movement_Step is new Gtk_Movement_Step_Properties.Property;

   package Gtk_Input_Hints_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Input_Hints);
   type Property_Gtk_Input_Hints is new Gtk_Input_Hints_Properties.Property;

   package Gtk_Input_Purpose_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Input_Purpose);
   type Property_Gtk_Input_Purpose is new Gtk_Input_Purpose_Properties.Property;

   package Gtk_Number_Up_Layout_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Number_Up_Layout);
   type Property_Gtk_Number_Up_Layout is new Gtk_Number_Up_Layout_Properties.Property;

   package Gtk_Orientation_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Orientation);
   type Property_Gtk_Orientation is new Gtk_Orientation_Properties.Property;

   package Gtk_Pack_Direction_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Pack_Direction);
   type Property_Gtk_Pack_Direction is new Gtk_Pack_Direction_Properties.Property;

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

   package Gtk_Popover_Constraint_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Popover_Constraint);
   type Property_Gtk_Popover_Constraint is new Gtk_Popover_Constraint_Properties.Property;

   package Gtk_Propagation_Phase_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Propagation_Phase);
   type Property_Gtk_Propagation_Phase is new Gtk_Propagation_Phase_Properties.Property;

   package Gtk_Event_Sequence_State_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Event_Sequence_State);
   type Property_Gtk_Event_Sequence_State is new Gtk_Event_Sequence_State_Properties.Property;

   package Gtk_Level_Bar_Mode_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Level_Bar_Mode);
   type Property_Gtk_Level_Bar_Mode is new Gtk_Level_Bar_Mode_Properties.Property;

   package Gtk_Path_Priority_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Path_Priority_Type);
   type Property_Gtk_Path_Priority_Type is new Gtk_Path_Priority_Type_Properties.Property;

   package Gtk_Path_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Path_Type);
   type Property_Gtk_Path_Type is new Gtk_Path_Type_Properties.Property;

   package Gtk_Policy_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Policy_Type);
   type Property_Gtk_Policy_Type is new Gtk_Policy_Type_Properties.Property;

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

   package Gtk_Region_Flags_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Region_Flags);
   type Property_Gtk_Region_Flags is new Gtk_Region_Flags_Properties.Property;

   package Gtk_Relief_Style_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Relief_Style);
   type Property_Gtk_Relief_Style is new Gtk_Relief_Style_Properties.Property;

   package Gtk_Resize_Mode_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Resize_Mode);
   type Property_Gtk_Resize_Mode is new Gtk_Resize_Mode_Properties.Property;

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

   package Gtk_Shadow_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Shadow_Type);
   type Property_Gtk_Shadow_Type is new Gtk_Shadow_Type_Properties.Property;

   package Gtk_Size_Request_Mode_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Size_Request_Mode);
   type Property_Gtk_Size_Request_Mode is new Gtk_Size_Request_Mode_Properties.Property;

   package Gtk_Sort_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Sort_Type);
   type Property_Gtk_Sort_Type is new Gtk_Sort_Type_Properties.Property;

   package Gtk_State_Flags_Properties is
      new Generic_Internal_Discrete_Property (Gtk_State_Flags);
   type Property_Gtk_State_Flags is new Gtk_State_Flags_Properties.Property;

   package Gtk_State_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_State_Type);
   type Property_Gtk_State_Type is new Gtk_State_Type_Properties.Property;

   package Gtk_Target_Flags_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Target_Flags);
   type Property_Gtk_Target_Flags is new Gtk_Target_Flags_Properties.Property;

   package Gtk_Text_Direction_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Text_Direction);
   type Property_Gtk_Text_Direction is new Gtk_Text_Direction_Properties.Property;

   package Gtk_Text_Window_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Text_Window_Type);
   type Property_Gtk_Text_Window_Type is new Gtk_Text_Window_Type_Properties.Property;

   package Gtk_Tree_View_Grid_Lines_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Tree_View_Grid_Lines);
   type Property_Gtk_Tree_View_Grid_Lines is new Gtk_Tree_View_Grid_Lines_Properties.Property;

   package Gtk_Toolbar_Space_Style_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Toolbar_Space_Style);
   type Property_Gtk_Toolbar_Space_Style is new Gtk_Toolbar_Space_Style_Properties.Property;

   package Gtk_Toolbar_Style_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Toolbar_Style);
   type Property_Gtk_Toolbar_Style is new Gtk_Toolbar_Style_Properties.Property;

   package Gtk_Unit_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Unit);
   type Property_Gtk_Unit is new Gtk_Unit_Properties.Property;

   package Gtk_Window_Position_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Window_Position);
   type Property_Gtk_Window_Position is new Gtk_Window_Position_Properties.Property;

   package Gtk_Window_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Window_Type);
   type Property_Gtk_Window_Type is new Gtk_Window_Type_Properties.Property;

   package Gtk_Wrap_Mode_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Wrap_Mode);
   type Property_Gtk_Wrap_Mode is new Gtk_Wrap_Mode_Properties.Property;

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
