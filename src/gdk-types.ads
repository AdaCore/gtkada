-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
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

with Glib; use Glib;

package Gdk.Types is

   type Gdk_Cap_Style is (Not_Last, Butt, Round, Projecting);

   type Gdk_Cursor_Type is (X_Cursor,
                            Arrow,
                            Based_Arrow_Down,
                            Based_Arrow_Up,
                            Boat,
                            Bogosity,
                            Bottom_Left_Corner,
                            Bottom_Right_Corner,
                            Bottom_Side,
                            Bottom_Tee,
                            Box_Spiral,
                            Center_Ptr,
                            Circle,
                            Clock,
                            Coffee_Mug,
                            Cross,
                            Cross_Reverse,
                            Crosshair,
                            Diamond_Cross,
                            Dot,
                            Dotbox,
                            Double_Arrow,
                            Draft_Large,
                            Draft_Small,
                            Draped_Box,
                            Exchange,
                            Fleur,
                            Gobbler,
                            Gumby,
                            Hand1,
                            Hand2,
                            Heart,
                            Icon,
                            Iron_Cross,
                            Left_Ptr,
                            Left_Side,
                            Left_Tee,
                            Leftbutton,
                            Ll_Angle,
                            Lr_Angle,
                            Man,
                            Middlebutton,
                            Mouse,
                            Pencil,
                            Pirate,
                            Plus,
                            Question_Arrow,
                            Right_Ptr,
                            Right_Side,
                            Right_Tee,
                            Rightbutton,
                            Rtl_Logo,
                            Sailboat,
                            Sb_Down_Arrow,
                            Sb_H_Double_Arrow,
                            Sb_Left_Arrow,
                            Sb_Right_Arrow,
                            Sb_Up_Arrow,
                            Sb_V_Double_Arrow,
                            Shuttle,
                            Sizing,
                            Spider,
                            Spraycan,
                            Star,
                            Target,
                            Tcross,
                            Top_Left_Arrow,
                            Top_Left_Corner,
                            Top_Right_Corner,
                            Top_Side,
                            Top_Tee,
                            Trek,
                            Ul_Angle,
                            Umbrella,
                            Ur_Angle,
                            Watch,
                            Xterm);

   type Gdk_Event_Mask is new Guint;
   Null_Event_Mask          : constant Gdk_Event_Mask;
   Exposure_Mask            : constant Gdk_Event_Mask;
   Pointer_Motion_Mask      : constant Gdk_Event_Mask;
   Pointer_Motion_Hint_Mask : constant Gdk_Event_Mask;
   Button_Motion_Mask       : constant Gdk_Event_Mask;
   Button_1_Motion_Mask     : constant Gdk_Event_Mask;
   Button_2_Motion_Mask     : constant Gdk_Event_Mask;
   Button_3_Motion_Mask     : constant Gdk_Event_Mask;
   Button_Press_Mask        : constant Gdk_Event_Mask;
   Button_Release_Mask      : constant Gdk_Event_Mask;
   Key_Press_Mask           : constant Gdk_Event_Mask;
   Key_Release_Mask         : constant Gdk_Event_Mask;
   Enter_Notify_Mask        : constant Gdk_Event_Mask;
   Leave_Notify_Mask        : constant Gdk_Event_Mask;
   Focus_Change_Mask        : constant Gdk_Event_Mask;
   Structure_Mask           : constant Gdk_Event_Mask;
   Property_Change_Mask     : constant Gdk_Event_Mask;
   Visibility_Notify_Mask   : constant Gdk_Event_Mask;
   Proximity_In_Mask        : constant Gdk_Event_Mask;
   Proximity_Out_Mask       : constant Gdk_Event_Mask;
   All_Events_Mask          : constant Gdk_Event_Mask;

   type Gdk_Event_Type is (Nothing,
                           Delete,
                           Destroy,
                           Expose,
                           Motion_Notify,
                           Button_Press,
                           Gdk_2button_Press,
                           Gdk_3button_Press,
                           Button_Release,
                           Key_Press,
                           Key_Release,
                           Enter_Notify,
                           Leave_Notify,
                           Focus_Change,
                           Configure,
                           Map,
                           Unmap,
                           Property_Notify,
                           Selection_Clear,
                           Selection_Request,
                           Selection_Notify,
                           Proximity_In,
                           Proximity_Out,
                           Drag_Begin,
                           Drag_Request,
                           Drop_Enter,
                           Drop_Leave,
                           Drop_Data_Avail,
                           Client_Event,
                           Visibility_Notify,
                           No_Expose,
                           Other_Event);

   type Gdk_Fill is (Solid, Tiled, Stippled, Opaque_Stippled);

   type Gdk_Fill_Rule is (Even_Odd_Rule, Winding_Rule);

   type Gdk_Function is (Copy, Invert, Gdk_Xor);

   type Gdk_Join_Style is (Miter, Round, Bevel);

   type Gdk_Input_Condition is (Read, Write, Input_Exception);

   type Gdk_Line_Style is (Solid, On_Off_Dash, Double_Dash);

   type Gdk_Modifier_Type is new Guint;
   Shift_Mask   : constant Gdk_Modifier_Type;
   Lock_Mask    : constant Gdk_Modifier_Type;
   Control_Mask : constant Gdk_Modifier_Type;
   Mod1_Mask    : constant Gdk_Modifier_Type;
   Mod2_Mask    : constant Gdk_Modifier_Type;
   Mod3_Mask    : constant Gdk_Modifier_Type;
   Mod4_Mask    : constant Gdk_Modifier_Type;
   Mod5_Mask    : constant Gdk_Modifier_Type;
   Button1_Mask : constant Gdk_Modifier_Type;
   Button2_Mask : constant Gdk_Modifier_Type;
   Button3_Mask : constant Gdk_Modifier_Type;
   Button4_Mask : constant Gdk_Modifier_Type;
   Button5_Mask : constant Gdk_Modifier_Type;

   type Gdk_Overlap_Type is (Rectangle_In, Rectangle_Out, Rectangle_Part);

   type Gdk_Window_Attributes_Type is new Guint;
   Wa_Title    : constant Gdk_Window_Attributes_Type;
   Wa_X        : constant Gdk_Window_Attributes_Type;
   Wa_Y        : constant Gdk_Window_Attributes_Type;
   Wa_Cursor   : constant Gdk_Window_Attributes_Type;
   Wa_Colormap : constant Gdk_Window_Attributes_Type;
   Wa_Visual   : constant Gdk_Window_Attributes_Type;
   Wa_Wmclass  : constant Gdk_Window_Attributes_Type;
   Wa_Noredir  : constant Gdk_Window_Attributes_Type;

   type Gdk_Subwindow_Mode is (Clip_By_Children, Include_Inferiors);

   type Gdk_Values_Mask is new Guint;
   Foreground    : constant Gdk_Values_Mask;
   Background    : constant Gdk_Values_Mask;
   Font          : constant Gdk_Values_Mask;
   GC_Function   : constant Gdk_Values_Mask;
   Fill          : constant Gdk_Values_Mask;
   Tile          : constant Gdk_Values_Mask;
   Stipple       : constant Gdk_Values_Mask;
   Clip_Mask     : constant Gdk_Values_Mask;
   Subwindow     : constant Gdk_Values_Mask;
   Ts_X_Origin   : constant Gdk_Values_Mask;
   Tx_Y_Origin   : constant Gdk_Values_Mask;
   Clip_X_Origin : constant Gdk_Values_Mask;
   Clip_Y_Origin : constant Gdk_Values_Mask;
   Exposures     : constant Gdk_Values_Mask;
   Line_Width    : constant Gdk_Values_Mask;
   Line_Style    : constant Gdk_Values_Mask;
   Cap_Style     : constant Gdk_Values_Mask;
   Join_Style    : constant Gdk_Values_Mask;

   type Gdk_Visual_Type is (Static_Gray,
                            Grayscale,
                            Static_Color,
                            Pseudo_Color,
                            True_Color,
                            Direct_Color);

   type Gdk_Window_Class is (Input_Output,
                             Input_Only);

   type Gdk_Window_Hint is new Guint;
   Gdk_Hint_Pos      : constant Gdk_Window_Hint;
   Gdk_Hint_Min_Size : constant Gdk_Window_Hint;
   Gdk_Hint_Max_Size : constant Gdk_Window_Hint;

   type Gdk_Window_Type is (Root,
                            Toplevel,
                            Child,
                            Dialog,
                            Temp,
                            Pixmap,
                            Foreign);

   type Gdk_Wm_Decoration is new Guint;
   Decor_All      : constant Gdk_Wm_Decoration;
   Decor_Border   : constant Gdk_Wm_Decoration;
   Decor_Resize_H : constant Gdk_Wm_Decoration;
   Decor_Title    : constant Gdk_Wm_Decoration;
   Decor_Menu     : constant Gdk_Wm_Decoration;
   Decor_Minimize : constant Gdk_Wm_Decoration;
   Decor_Maximize : constant Gdk_Wm_Decoration;

   type Gdk_Wm_Function is new Guint;
   Func_All      : constant Gdk_Wm_Function;
   Func_Resize   : constant Gdk_Wm_Function;
   Func_Move     : constant Gdk_Wm_Function;
   Func_Minimize : constant Gdk_Wm_Function;
   Func_Maximize : constant Gdk_Wm_Function;
   Func_Close    : constant Gdk_Wm_Function;

private

   -------------------------
   --  Private constants  --
   -------------------------

   Null_Event_Mask          : constant Gdk_Event_Mask := 0;
   Exposure_Mask            : constant Gdk_Event_Mask := 2 ** 1;
   Pointer_Motion_Mask      : constant Gdk_Event_Mask := 2 ** 2;
   Pointer_Motion_Hint_Mask : constant Gdk_Event_Mask := 2 ** 3;
   Button_Motion_Mask       : constant Gdk_Event_Mask := 2 ** 4;
   Button_1_Motion_Mask     : constant Gdk_Event_Mask := 2 ** 5;
   Button_2_Motion_Mask     : constant Gdk_Event_Mask := 2 ** 6;
   Button_3_Motion_Mask     : constant Gdk_Event_Mask := 2 ** 7;
   Button_Press_Mask        : constant Gdk_Event_Mask := 2 ** 8;
   Button_Release_Mask      : constant Gdk_Event_Mask := 2 ** 9;
   Key_Press_Mask           : constant Gdk_Event_Mask := 2 ** 10;
   Key_Release_Mask         : constant Gdk_Event_Mask := 2 ** 11;
   Enter_Notify_Mask        : constant Gdk_Event_Mask := 2 ** 12;
   Leave_Notify_Mask        : constant Gdk_Event_Mask := 2 ** 13;
   Focus_Change_Mask        : constant Gdk_Event_Mask := 2 ** 14;
   Structure_Mask           : constant Gdk_Event_Mask := 2 ** 15;
   Property_Change_Mask     : constant Gdk_Event_Mask := 2 ** 16;
   Visibility_Notify_Mask   : constant Gdk_Event_Mask := 2 ** 17;
   Proximity_In_Mask        : constant Gdk_Event_Mask := 2 ** 18;
   Proximity_Out_Mask       : constant Gdk_Event_Mask := 2 ** 19;
   All_Events_Mask          : constant Gdk_Event_Mask := 16#7FFFF#;

   Shift_Mask   : constant Gdk_Modifier_Type := 2 ** 0;
   Lock_Mask    : constant Gdk_Modifier_Type := 2 ** 1;
   Control_Mask : constant Gdk_Modifier_Type := 2 ** 2;
   Mod1_Mask    : constant Gdk_Modifier_Type := 2 ** 3;
   Mod2_Mask    : constant Gdk_Modifier_Type := 2 ** 4;
   Mod3_Mask    : constant Gdk_Modifier_Type := 2 ** 5;
   Mod4_Mask    : constant Gdk_Modifier_Type := 2 ** 6;
   Mod5_Mask    : constant Gdk_Modifier_Type := 2 ** 7;
   Button1_Mask : constant Gdk_Modifier_Type := 2 ** 8;
   Button2_Mask : constant Gdk_Modifier_Type := 2 ** 9;
   Button3_Mask : constant Gdk_Modifier_Type := 2 ** 10;
   Button4_Mask : constant Gdk_Modifier_Type := 2 ** 11;
   Button5_Mask : constant Gdk_Modifier_Type := 2 ** 12;

   Wa_Title    : constant Gdk_Window_Attributes_Type := 2 ** 1;
   Wa_X        : constant Gdk_Window_Attributes_Type := 2 ** 2;
   Wa_Y        : constant Gdk_Window_Attributes_Type := 2 ** 3;
   Wa_Cursor   : constant Gdk_Window_Attributes_Type := 2 ** 4;
   Wa_Colormap : constant Gdk_Window_Attributes_Type := 2 ** 5;
   Wa_Visual   : constant Gdk_Window_Attributes_Type := 2 ** 6;
   Wa_Wmclass  : constant Gdk_Window_Attributes_Type := 2 ** 7;
   Wa_Noredir  : constant Gdk_Window_Attributes_Type := 2 ** 8;

   Foreground    : constant Gdk_Values_Mask := 2 ** 0;
   Background    : constant Gdk_Values_Mask := 2 ** 1;
   Font          : constant Gdk_Values_Mask := 2 ** 2;
   GC_Function   : constant Gdk_Values_Mask := 2 ** 3;
   Fill          : constant Gdk_Values_Mask := 2 ** 4;
   Tile          : constant Gdk_Values_Mask := 2 ** 5;
   Stipple       : constant Gdk_Values_Mask := 2 ** 6;
   Clip_Mask     : constant Gdk_Values_Mask := 2 ** 7;
   Subwindow     : constant Gdk_Values_Mask := 2 ** 8;
   Ts_X_Origin   : constant Gdk_Values_Mask := 2 ** 9;
   Tx_Y_Origin   : constant Gdk_Values_Mask := 2 ** 10;
   Clip_X_Origin : constant Gdk_Values_Mask := 2 ** 11;
   Clip_Y_Origin : constant Gdk_Values_Mask := 2 ** 12;
   Exposures     : constant Gdk_Values_Mask := 2 ** 13;
   Line_Width    : constant Gdk_Values_Mask := 2 ** 14;
   Line_Style    : constant Gdk_Values_Mask := 2 ** 15;
   Cap_Style     : constant Gdk_Values_Mask := 2 ** 16;
   Join_Style    : constant Gdk_Values_Mask := 2 ** 17;

   Gdk_Hint_Pos      : constant Gdk_Window_Hint := 2 ** 0;
   Gdk_Hint_Min_Size : constant Gdk_Window_Hint := 2 ** 1;
   Gdk_Hint_Max_Size : constant Gdk_Window_Hint := 2 ** 2;

   ------------------------------
   --  Representation clauses  --
   ------------------------------

   for Gdk_Event_Type use (Nothing => -1,
                           Delete => 0,
                           Destroy => 1,
                           Expose => 2,
                           Motion_Notify => 3,
                           Button_Press => 4,
                           Gdk_2button_Press => 5,
                           Gdk_3button_Press => 6,
                           Button_Release => 7,
                           Key_Press => 8,
                           Key_Release => 9,
                           Enter_Notify => 10,
                           Leave_Notify => 11,
                           Focus_Change => 12,
                           Configure => 13,
                           Map => 14,
                           Unmap => 15,
                           Property_Notify => 16,
                           Selection_Clear => 17,
                           Selection_Request => 18,
                           Selection_Notify => 19,
                           Proximity_In => 20,
                           Proximity_Out => 21,
                           Drag_Begin => 22,
                           Drag_Request => 23,
                           Drop_Enter => 24,
                           Drop_Leave => 25,
                           Drop_Data_Avail => 26,
                           Client_Event => 27,
                           Visibility_Notify => 28,
                           No_Expose => 29,
                           Other_Event => 9999);

   for Gdk_Input_Condition use (Read            => 2 ** 0,
                                Write           => 2 ** 1,
                                Input_Exception => 2 ** 2);

   for Gdk_Subwindow_Mode use (Clip_By_Children  => 0,
                               Include_Inferiors => 1);

   --  FIXME: Warning : these values should be kept synchronized with
   --  FIXME: gdk/gdcursors.h
   for Gdk_Cursor_Type use (X_Cursor => 0,
                            Arrow    => 2,
                            Based_Arrow_Down => 4,
                            Based_Arrow_Up => 6,
                            Boat => 8,
                            Bogosity => 10,
                            Bottom_Left_Corner => 12,
                            Bottom_Right_Corner => 14,
                            Bottom_Side => 16,
                            Bottom_Tee => 18,
                            Box_Spiral => 20,
                            Center_Ptr => 22,
                            Circle => 24,
                            Clock => 26,
                            Coffee_Mug => 28,
                            Cross => 30,
                            Cross_Reverse => 32,
                            Crosshair => 34,
                            Diamond_Cross => 36,
                            Dot => 38,
                            Dotbox => 40,
                            Double_Arrow => 42,
                            Draft_Large => 44,
                            Draft_Small => 46,
                            Draped_Box => 48,
                            Exchange => 50,
                            Fleur => 52,
                            Gobbler => 54,
                            Gumby => 56,
                            Hand1 => 58,
                            Hand2 => 60,
                            Heart => 62,
                            Icon => 64,
                            Iron_Cross => 66,
                            Left_Ptr => 68,
                            Left_Side => 70,
                            Left_Tee => 72,
                            Leftbutton => 74,
                            Ll_Angle => 76,
                            Lr_Angle => 78,
                            Man => 80,
                            Middlebutton => 82,
                            Mouse => 84,
                            Pencil => 86,
                            Pirate => 88,
                            Plus => 90,
                            Question_Arrow => 92,
                            Right_Ptr => 94,
                            Right_Side => 96,
                            Right_Tee => 98,
                            Rightbutton => 100,
                            Rtl_Logo => 102,
                            Sailboat => 104,
                            Sb_Down_Arrow => 106,
                            Sb_H_Double_Arrow => 108,
                            Sb_Left_Arrow => 110,
                            Sb_Right_Arrow => 112,
                            Sb_Up_Arrow => 114,
                            Sb_V_Double_Arrow => 116,
                            Shuttle => 118,
                            Sizing => 120,
                            Spider => 122,
                            Spraycan => 124,
                            Star => 126,
                            Target => 128,
                            Tcross => 130,
                            Top_Left_Arrow => 132,
                            Top_Left_Corner => 134,
                            Top_Right_Corner => 136,
                            Top_Side => 138,
                            Top_Tee => 140,
                            Trek => 142,
                            Ul_Angle => 144,
                            Umbrella => 146,
                            Ur_Angle => 148,
                            Watch => 150,
                            Xterm => 152);

   Decor_All      : constant Gdk_Wm_Decoration := 2 ** 0;
   Decor_Border   : constant Gdk_Wm_Decoration := 2 ** 1;
   Decor_Resize_H : constant Gdk_Wm_Decoration := 2 ** 2;
   Decor_Title    : constant Gdk_Wm_Decoration := 2 ** 3;
   Decor_Menu     : constant Gdk_Wm_Decoration := 2 ** 4;
   Decor_Minimize : constant Gdk_Wm_Decoration := 2 ** 5;
   Decor_Maximize : constant Gdk_Wm_Decoration := 2 ** 6;

   Func_All      : constant Gdk_Wm_Function := 2 ** 0;
   Func_Resize   : constant Gdk_Wm_Function := 2 ** 1;
   Func_Move     : constant Gdk_Wm_Function := 2 ** 2;
   Func_Minimize : constant Gdk_Wm_Function := 2 ** 3;
   Func_Maximize : constant Gdk_Wm_Function := 2 ** 4;
   Func_Close    : constant Gdk_Wm_Function := 2 ** 5;

end Gdk.Types;
