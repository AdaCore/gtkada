-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

with Glib; use Glib;

package Gdk.Types is

   type Gdk_Geometry is record
      Min_Width   : Gint;
      Min_Height  : Gint;
      Max_Width   : Gint;
      Max_Height  : Gint;
      Base_Width  : Gint;
      Base_Height : Gint;
      Width_Inc   : Gint;
      Height_Inc  : Gint;
      Min_Aspect  : Gdouble;
      Max_Aspect  : Gdouble;
   end record;
   pragma Pack (Gdk_Geometry);

   type Gdk_Point is record
      X : Gint16;
      Y : Gint16;
   end record;
   pragma Pack (Gdk_Point);
   for Gdk_Point'Size use 32;

   type Gdk_Points_Array is array (Positive range <>) of Gdk_Point;
   pragma Pack (Gdk_Points_Array);

   type Gdk_Segment is record
      X1 : Gint16;
      Y1 : Gint16;
      X2 : Gint16;
      Y2 : Gint16;
   end record;
   pragma Pack (Gdk_Segment);
   for Gdk_Segment'Size use 64;

   type Gdk_Segments_Array is array (Positive range <>) of Gdk_Segment;
   pragma Pack (Gdk_Segments_Array);

   --
   --  See at the end of the package a list of all the types that
   --  have not been "bound".
   --

   type Gdk_Atom is new Gulong;

   type Gdk_Axis_Use is (Axis_Ignore,
                         Axis_X,
                         Axis_Y,
                         Axis_Pressure,
                         Axis_X_Tilt,
                         Axis_Y_Tilt,
                         Axis_Last);

   type Gdk_Byte_Order is (Lsb_First, Msb_First);

   type Gdk_Cap_Style is (Cap_Not_Last, Cap_Butt, Cap_Round, Cap_Projecting);

   type Gdk_Crossing_Mode is (Crossing_Normal, Crossing_Grab, Crossing_Ungrab);

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

   type Gdk_Device_Id is new Guint32;
   --
   --  This type is specific to GtkAda. In gdk, they use guint32.

   type Gdk_Event_Mask is new Guint;
   --  Note that you need to change the event mask of a widget if you want
   --  to be able to get certain events. To change this mask, the widget
   --  must first be Unrealized.

   Null_Event_Mask          : constant Gdk_Event_Mask;
   Exposure_Mask            : constant Gdk_Event_Mask;

   Pointer_Motion_Mask      : constant Gdk_Event_Mask;
   --  Every time the mouse moves, GtkAda will send a Motion_Notify event.
   --  These events will be sent as fast as possible, and your application
   --  needs to be able to respond as fast as possible (generally about
   --  200 events per second).

   Pointer_Motion_Hint_Mask : constant Gdk_Event_Mask;
   --  GtkAda will only send one Motion_Notify event when the mouse moves.
   --  The handler should call Gdk.Window.Get_Pointer, to get the current
   --  position and signals GtkAda that it is ready to get another
   --  Motion_Notify signal. No new Motion_Notify will be sent until
   --  Get_Pointer has been called.

   Button_Motion_Mask       : constant Gdk_Event_Mask;
   Button1_Motion_Mask      : constant Gdk_Event_Mask;
   Button2_Motion_Mask      : constant Gdk_Event_Mask;
   Button3_Motion_Mask      : constant Gdk_Event_Mask;
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
                           Gdk_2button_Press,  --  Double-click
                           Gdk_3button_Press,  --  Triple-click
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
                           Drag_Enter,
                           Drag_Leave,
                           Drag_Motion,
                           Drag_Status,
                           Drop_Start,
                           Drop_Finished,
                           Client_Event,
                           Visibility_Notify,
                           No_Expose);

   type Gdk_Extension_Mode is (Extension_Events_None,
                               Extension_Events_All,
                               Extension_Events_Cursor);

   type Gdk_Fill is (Solid, Tiled, Stippled, Opaque_Stippled);

   type Gdk_Fill_Rule is (Even_Odd_Rule, Winding_Rule);

   type Gdk_Function is (Copy,
                         Invert,
                         Gdk_Xor,
                         Clear,
                         Gdk_And,
                         And_Reverse,
                         And_Invert,
                         Noop,
                         Gdk_Or,
                         Equiv,
                         Or_Reverse,
                         Copy_Invert,
                         Or_Invert,
                         Nand,
                         Set);

   type Gdk_Join_Style is (Join_Miter, Join_Round, Join_Bevel);

   type Gdk_IC_Attributes_Type is new Guint;
   Ic_Style                : constant Gdk_IC_Attributes_Type;
   Ic_Client_Window        : constant Gdk_IC_Attributes_Type;
   Ic_Focus_Window         : constant Gdk_IC_Attributes_Type;
   Ic_Filter_Events        : constant Gdk_IC_Attributes_Type;
   Ic_Spot_Location        : constant Gdk_IC_Attributes_Type;
   Ic_Line_Spacing         : constant Gdk_IC_Attributes_Type;
   Ic_Cursor               : constant Gdk_IC_Attributes_Type;
   Ic_Preedit_Fontset      : constant Gdk_IC_Attributes_Type;
   Ic_Preedit_Area         : constant Gdk_IC_Attributes_Type;
   Ic_Preedit_Area_Needed  : constant Gdk_IC_Attributes_Type;
   Ic_Preedit_Foreground   : constant Gdk_IC_Attributes_Type;
   Ic_Preedit_Background   : constant Gdk_IC_Attributes_Type;
   Ic_Preedit_Pixmap       : constant Gdk_IC_Attributes_Type;
   Ic_Preedit_Colormap     : constant Gdk_IC_Attributes_Type;
   Ic_Status_Fontset       : constant Gdk_IC_Attributes_Type;
   Ic_Status_Area          : constant Gdk_IC_Attributes_Type;
   Ic_Status_Area_Needed   : constant Gdk_IC_Attributes_Type;
   Ic_Status_Foreground    : constant Gdk_IC_Attributes_Type;
   Ic_Status_Background    : constant Gdk_IC_Attributes_Type;
   Ic_Status_Pixmap        : constant Gdk_IC_Attributes_Type;
   Ic_Status_Colormap      : constant Gdk_IC_Attributes_Type;
   Ic_All_Req              : constant Gdk_IC_Attributes_Type;
   Ic_Preedit_Area_Req     : constant Gdk_IC_Attributes_Type;
   Ic_Preedit_Position_Req : constant Gdk_IC_Attributes_Type;
   Ic_Status_Area_Req      : constant Gdk_IC_Attributes_Type;

   type Gdk_IM_Style is new Guint;
   Im_Preedit_Area      : constant Gdk_IM_Style;
   Im_Preedit_Callbacks : constant Gdk_IM_Style;
   Im_Preedit_Position  : constant Gdk_IM_Style;
   Im_Preedit_Nothing   : constant Gdk_IM_Style;
   Im_Preedit_None      : constant Gdk_IM_Style;
   Im_Preedit_Mask      : constant Gdk_IM_Style;
   Im_Status_Area       : constant Gdk_IM_Style;
   Im_Status_Callbacks  : constant Gdk_IM_Style;
   Im_Status_Nothing    : constant Gdk_IM_Style;
   Im_Status_None       : constant Gdk_IM_Style;
   Im_Status_Mask       : constant Gdk_IM_Style;

   type Gdk_Input_Condition is (Input_Read, Input_Write, Input_Exception);

   type Gdk_Input_Mode is (Mode_Disabled,
                           Mode_Screen,
                           Mode_Window);

   type Gdk_Input_Source is (Source_Mouse,
                             Source_Pen,
                             Source_Eraser,
                             Source_Cursor);

   type Gdk_Key_Type is new Guint;
   --  see Gdk.Types.Keysyms for key type constants

   type Gdk_Line_Style is (Line_Solid, Line_On_Off_Dash, Line_Double_Dash);

   type Gdk_Modifier_Type is new Guint;
   Shift_Mask    : constant Gdk_Modifier_Type;
   Lock_Mask     : constant Gdk_Modifier_Type;
   Control_Mask  : constant Gdk_Modifier_Type;
   Mod1_Mask     : constant Gdk_Modifier_Type;
   Mod2_Mask     : constant Gdk_Modifier_Type;
   Mod3_Mask     : constant Gdk_Modifier_Type;
   Mod4_Mask     : constant Gdk_Modifier_Type;
   Mod5_Mask     : constant Gdk_Modifier_Type;
   Button1_Mask  : constant Gdk_Modifier_Type;
   Button2_Mask  : constant Gdk_Modifier_Type;
   Button3_Mask  : constant Gdk_Modifier_Type;
   Button4_Mask  : constant Gdk_Modifier_Type;
   Button5_Mask  : constant Gdk_Modifier_Type;
   Release_Mask  : constant Gdk_Modifier_Type;
   Modifier_Mask : constant Gdk_Modifier_Type;

   type Gdk_Notify_Type is
     (Notify_Ancestor,
      Notify_Virtual,
      Notify_Inferior,
      Notify_Non_Linear,
      Notify_Non_Linear_Virtual,
      Notify_Unknown);

   type Gdk_Overlap_Type is
     (Overlap_Rectangle_In,
      Overlap_Rectangle_Out,
      Overlap_Rectangle_Part);

   type Gdk_Prop_Mode is (Prop_Mode_Replace,
                          Prop_Mode_Prepend,
                          Prop_Mode_Append);

   type Gdk_Subwindow_Mode is (Clip_By_Children, Include_Inferiors);

   type Gdk_GC_Values_Mask is new Guint;
   GC_Foreground    : constant Gdk_GC_Values_Mask;
   GC_Background    : constant Gdk_GC_Values_Mask;
   GC_Font          : constant Gdk_GC_Values_Mask;
   GC_Function      : constant Gdk_GC_Values_Mask;
   GC_Fill          : constant Gdk_GC_Values_Mask;
   GC_Tile          : constant Gdk_GC_Values_Mask;
   GC_Stipple       : constant Gdk_GC_Values_Mask;
   GC_Clip_Mask     : constant Gdk_GC_Values_Mask;
   GC_Subwindow     : constant Gdk_GC_Values_Mask;
   GC_Ts_X_Origin   : constant Gdk_GC_Values_Mask;
   GC_Tx_Y_Origin   : constant Gdk_GC_Values_Mask;
   GC_Clip_X_Origin : constant Gdk_GC_Values_Mask;
   GC_Clip_Y_Origin : constant Gdk_GC_Values_Mask;
   GC_Exposures     : constant Gdk_GC_Values_Mask;
   GC_Line_Width    : constant Gdk_GC_Values_Mask;
   GC_Line_Style    : constant Gdk_GC_Values_Mask;
   GC_Cap_Style     : constant Gdk_GC_Values_Mask;
   GC_Join_Style    : constant Gdk_GC_Values_Mask;

   type Gdk_Visibility_State is (Visibility_Unobscured,
                                 Visibility_Partial,
                                 Visibility_Fully_Obscured);

   type Gdk_Visual_Type is (Visual_Static_Gray,
                            Visual_Grayscale,
                            Visual_Static_Color,
                            Visual_Pseudo_Color,
                            Visual_True_Color,
                            Visual_Direct_Color);

   type Gdk_Window_Attributes_Type is new Guint;
   Wa_Title    : constant Gdk_Window_Attributes_Type;
   Wa_X        : constant Gdk_Window_Attributes_Type;
   Wa_Y        : constant Gdk_Window_Attributes_Type;
   Wa_Cursor   : constant Gdk_Window_Attributes_Type;
   Wa_Colormap : constant Gdk_Window_Attributes_Type;
   Wa_Visual   : constant Gdk_Window_Attributes_Type;
   Wa_Wmclass  : constant Gdk_Window_Attributes_Type;
   Wa_Noredir  : constant Gdk_Window_Attributes_Type;

   type Gdk_Window_Class is (Input_Output,
                             Input_Only);

   type Gdk_Window_Hints is new Guint;
   Gdk_Hint_Pos        : constant Gdk_Window_Hints;
   Gdk_Hint_Min_Size   : constant Gdk_Window_Hints;
   Gdk_Hint_Max_Size   : constant Gdk_Window_Hints;
   Gdk_Hint_Base_Size  : constant Gdk_Window_Hints;
   Gdk_Hint_Aspect     : constant Gdk_Window_Hints;
   Gdk_Hint_Resize_Inc : constant Gdk_Window_Hints;

   type Gdk_Window_Type is (Window_Root,
                            Window_Toplevel,
                            Window_Child,
                            Window_Dialog,
                            Window_Temp,
                            Window_Pixmap,
                            Window_Foreign);

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

   subtype Gdk_WChar is Standard.Wide_Character;
   subtype Gdk_WString is Standard.Wide_String;
   --
   --  Gdk does not define a Gdk_WString type, but uses pointers
   --  to Gdk_WChar instead.

private

   -------------------------
   --  Private constants  --
   -------------------------

   Null_Event_Mask          : constant Gdk_Event_Mask := 0;
   Exposure_Mask            : constant Gdk_Event_Mask := 2 ** 1;
   Pointer_Motion_Mask      : constant Gdk_Event_Mask := 2 ** 2;
   Pointer_Motion_Hint_Mask : constant Gdk_Event_Mask := 2 ** 3;
   Button_Motion_Mask       : constant Gdk_Event_Mask := 2 ** 4;
   Button1_Motion_Mask      : constant Gdk_Event_Mask := 2 ** 5;
   Button2_Motion_Mask      : constant Gdk_Event_Mask := 2 ** 6;
   Button3_Motion_Mask      : constant Gdk_Event_Mask := 2 ** 7;
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
   Substructure_Mask        : constant Gdk_Event_Mask := 2 ** 20;
   All_Events_Mask          : constant Gdk_Event_Mask := 16#0FFFFF#;

   Ic_Style                : constant Gdk_IC_Attributes_Type := 2 ** 0;
   Ic_Client_Window        : constant Gdk_IC_Attributes_Type := 2 ** 1;
   Ic_Focus_Window         : constant Gdk_IC_Attributes_Type := 2 ** 2;
   Ic_Filter_Events        : constant Gdk_IC_Attributes_Type := 2 ** 3;
   Ic_Spot_Location        : constant Gdk_IC_Attributes_Type := 2 ** 4;
   Ic_Line_Spacing         : constant Gdk_IC_Attributes_Type := 2 ** 5;
   Ic_Cursor               : constant Gdk_IC_Attributes_Type := 2 ** 6;
   Ic_Preedit_Fontset      : constant Gdk_IC_Attributes_Type := 2 ** 10;
   Ic_Preedit_Area         : constant Gdk_IC_Attributes_Type := 2 ** 11;
   Ic_Preedit_Area_Needed  : constant Gdk_IC_Attributes_Type := 2 ** 12;
   Ic_Preedit_Foreground   : constant Gdk_IC_Attributes_Type := 2 ** 13;
   Ic_Preedit_Background   : constant Gdk_IC_Attributes_Type := 2 ** 14;
   Ic_Preedit_Pixmap       : constant Gdk_IC_Attributes_Type := 2 ** 15;
   Ic_Preedit_Colormap     : constant Gdk_IC_Attributes_Type := 2 ** 16;
   Ic_Status_Fontset       : constant Gdk_IC_Attributes_Type := 2 ** 21;
   Ic_Status_Area          : constant Gdk_IC_Attributes_Type := 2 ** 22;
   Ic_Status_Area_Needed   : constant Gdk_IC_Attributes_Type := 2 ** 23;
   Ic_Status_Foreground    : constant Gdk_IC_Attributes_Type := 2 ** 24;
   Ic_Status_Background    : constant Gdk_IC_Attributes_Type := 2 ** 25;
   Ic_Status_Pixmap        : constant Gdk_IC_Attributes_Type := 2 ** 26;
   Ic_Status_Colormap      : constant Gdk_IC_Attributes_Type := 2 ** 27;
   Ic_All_Req              : constant Gdk_IC_Attributes_Type
     := Ic_Style or Ic_Client_Window;
   Ic_Preedit_Area_Req     : constant Gdk_IC_Attributes_Type
     := Ic_Preedit_Area or Ic_Preedit_Fontset;
   Ic_Preedit_Position_Req : constant Gdk_IC_Attributes_Type
     := Ic_Preedit_Area or Ic_Spot_Location or Ic_Preedit_Fontset;
   Ic_Status_Area_Req      : constant Gdk_IC_Attributes_Type
     := Ic_Status_Area or Ic_Status_Fontset;

   Im_Preedit_Area      : constant Gdk_IM_Style := 16#0001#;
   Im_Preedit_Callbacks : constant Gdk_IM_Style := 16#0002#;
   Im_Preedit_Position  : constant Gdk_IM_Style := 16#0004#;
   Im_Preedit_Nothing   : constant Gdk_IM_Style := 16#0008#;
   Im_Preedit_None      : constant Gdk_IM_Style := 16#0010#;
   Im_Preedit_Mask      : constant Gdk_IM_Style := 16#001F#;
   Im_Status_Area       : constant Gdk_IM_Style := 16#0100#;
   Im_Status_Callbacks  : constant Gdk_IM_Style := 16#0200#;
   Im_Status_Nothing    : constant Gdk_IM_Style := 16#0400#;
   Im_Status_None       : constant Gdk_IM_Style := 16#0800#;
   Im_Status_Mask       : constant Gdk_IM_Style := 16#0F00#;

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
   Release_Mask  : constant Gdk_Modifier_Type := 2 ** 13;
   Modifier_Mask : constant Gdk_Modifier_Type := 16#3FFF#;

   Wa_Title    : constant Gdk_Window_Attributes_Type := 2 ** 1;
   Wa_X        : constant Gdk_Window_Attributes_Type := 2 ** 2;
   Wa_Y        : constant Gdk_Window_Attributes_Type := 2 ** 3;
   Wa_Cursor   : constant Gdk_Window_Attributes_Type := 2 ** 4;
   Wa_Colormap : constant Gdk_Window_Attributes_Type := 2 ** 5;
   Wa_Visual   : constant Gdk_Window_Attributes_Type := 2 ** 6;
   Wa_Wmclass  : constant Gdk_Window_Attributes_Type := 2 ** 7;
   Wa_Noredir  : constant Gdk_Window_Attributes_Type := 2 ** 8;

   GC_Foreground    : constant Gdk_GC_Values_Mask := 2 ** 0;
   GC_Background    : constant Gdk_GC_Values_Mask := 2 ** 1;
   GC_Font          : constant Gdk_GC_Values_Mask := 2 ** 2;
   GC_Function      : constant Gdk_GC_Values_Mask := 2 ** 3;
   GC_Fill          : constant Gdk_GC_Values_Mask := 2 ** 4;
   GC_Tile          : constant Gdk_GC_Values_Mask := 2 ** 5;
   GC_Stipple       : constant Gdk_GC_Values_Mask := 2 ** 6;
   GC_Clip_Mask     : constant Gdk_GC_Values_Mask := 2 ** 7;
   GC_Subwindow     : constant Gdk_GC_Values_Mask := 2 ** 8;
   GC_Ts_X_Origin   : constant Gdk_GC_Values_Mask := 2 ** 9;
   GC_Tx_Y_Origin   : constant Gdk_GC_Values_Mask := 2 ** 10;
   GC_Clip_X_Origin : constant Gdk_GC_Values_Mask := 2 ** 11;
   GC_Clip_Y_Origin : constant Gdk_GC_Values_Mask := 2 ** 12;
   GC_Exposures     : constant Gdk_GC_Values_Mask := 2 ** 13;
   GC_Line_Width    : constant Gdk_GC_Values_Mask := 2 ** 14;
   GC_Line_Style    : constant Gdk_GC_Values_Mask := 2 ** 15;
   GC_Cap_Style     : constant Gdk_GC_Values_Mask := 2 ** 16;
   GC_Join_Style    : constant Gdk_GC_Values_Mask := 2 ** 17;

   Gdk_Hint_Pos        : constant Gdk_Window_Hints := 2 ** 0;
   Gdk_Hint_Min_Size   : constant Gdk_Window_Hints := 2 ** 1;
   Gdk_Hint_Max_Size   : constant Gdk_Window_Hints := 2 ** 2;
   Gdk_Hint_Base_Size  : constant Gdk_Window_Hints := 2 ** 3;
   Gdk_Hint_Aspect     : constant Gdk_Window_Hints := 2 ** 4;
   Gdk_Hint_Resize_Inc : constant Gdk_Window_Hints := 2 ** 5;


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
                           Drag_Enter => 22,
                           Drag_Leave => 23,
                           Drag_Motion => 24,
                           Drag_Status => 25,
                           Drop_Start => 26,
                           Drop_Finished => 27,
                           Client_Event => 28,
                           Visibility_Notify => 29,
                           No_Expose => 30);

   for Gdk_Input_Condition use (Input_Read      => 2 ** 0,
                                Input_Write     => 2 ** 1,
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

--
--  The following types were not bound because it did not seem
--  to be necessary (yet).
--
--  + GdkColorContextMode
--  + GdkCrossingMode
--  + GdkFilterReturn
--  + GdkFontType
--  + GdkNotifyType
--  + GdkPropertyState
--  + GdkSelection
--  + GdkSelectionType
--  + GdkStatus
--  + GdkTarget
--
--  The following types will probably be bound later (DnD).
--
--  + GdkDragAction
