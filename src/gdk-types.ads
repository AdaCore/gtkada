with Glib; use Glib;

package Gdk.Types is

   type Gdk_Cap_Style is (Not_Last, Butt, Round, Projecting);

   type Gdk_Cursor_Type is (Num_Glyphs,
                            X_Cursor,
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
                            Xterm,
                            Last_Cursor,
                            Cursor_Is_Pixmap);

   type Gdk_Event_Mask is new Gint;
   Exposure_Mask            : constant Gdk_Event_Mask := 2 ** 0;
   Pointer_Motion_Mask      : constant Gdk_Event_Mask := 2 ** 1;
   Pointer_Motion_Hint_Mask : constant Gdk_Event_Mask := 2 ** 2;
   Button_Motion_Mask       : constant Gdk_Event_Mask := 2 ** 3;
   Button_1_Motion_Mask     : constant Gdk_Event_Mask := 2 ** 4;
   Button_2_Motion_Mask     : constant Gdk_Event_Mask := 2 ** 5;
   Button_3_Motion_Mask     : constant Gdk_Event_Mask := 2 ** 6;
   Button_Press_Mask        : constant Gdk_Event_Mask := 2 ** 7;
   Button_Release_Mask      : constant Gdk_Event_Mask := 2 ** 8;
   Key_Press_Mask           : constant Gdk_Event_Mask := 2 ** 9;
   Key_Release_Mask         : constant Gdk_Event_Mask := 2 ** 10;
   Enter_Notify_Mask        : constant Gdk_Event_Mask := 2 ** 11;
   Leave_Notify_Mask        : constant Gdk_Event_Mask := 2 ** 12;
   Focus_Change_Mask        : constant Gdk_Event_Mask := 2 ** 13;
   Structure_Mask           : constant Gdk_Event_Mask := 2 ** 14;
   Property_Change_Mask     : constant Gdk_Event_Mask := 2 ** 15;
   Visibility_Notify_Mask   : constant Gdk_Event_Mask := 2 ** 16;
   Proximity_In_Mask        : constant Gdk_Event_Mask := 2 ** 17;
   All_Events_Mask          : constant Gdk_Event_Mask := 2 ** 18;
   Proximity_Out_Mask       : constant Gdk_Event_Mask := 2 ** 19;

   type Gdk_Fill is (Solid, Tiled, Stippled, Opaque_Stippled);

   type Gdk_Fill_Rule is (Even_Odd_Rule, Winding_Rule);

   type Gdk_Function is (Copy, Invert, Gdk_Xor);

   type Gdk_Join_Style is (Miter, Round, Bevel);

   type Gdk_Input_Condition is (Read, Write, Input_Exception);

   type Gdk_Line_Style is (Solid, On_Off_Dash, Double_Dash);

   type Gdk_Overlap_Type is (Rectangle_In, Rectangle_Out, Rectangle_Part);

   type Gdk_Subwindow_Mode is (Clip_By_Children, Include_Inferiors);

   type Gdk_Values_Mask is (Foreground,
                            Background,
                            Font,
                            GC_Function,
                            Fill,
                            Tile,
                            Stipple,
                            Clip_Mask,
                            Subwindow,
                            Ts_X_Origin,
                            Tx_Y_Origin,
                            Clip_X_Origin,
                            Clip_Y_Origin,
                            Exposures,
                            Line_Width,
                            Line_Style,
                            Cap_Style,
                            Join_Style);


   --  FIXME: These values are meant to be a mask, which suppose that
   --  FIXME: they can be combined together to form a value. It is
   --  FIXME: possible to do so in C but the current implementation in
   --  FIXME: Ada does not allow that for the moment....
   --  FIXME: This should be re-written.

   type Gdk_Visual_Type is (Static_Gray,
                            Grayscale,
                            Static_Color,
                            Pseudo_Color,
                            True_Color,
                            Direct_Color);


   ------------------------------
   --  Representation clauses  --
   ------------------------------

   for Gdk_Input_Condition use (Read            => 2 ** 0,
                                Write           => 2 ** 1,
                                Input_Exception => 2 ** 2);

   for Gdk_Subwindow_Mode use (Clip_By_Children  => 0,
                               Include_Inferiors => 1);

   for Gdk_Values_Mask use (Foreground    => 2 ** 0,
                            Background    => 2 ** 1,
                            Font          => 2 ** 2,
                            GC_Function   => 2 ** 3,
                            Fill          => 2 ** 4,
                            Tile          => 2 ** 5,
                            Stipple       => 2 ** 6,
                            Clip_Mask     => 2 ** 7,
                            Subwindow     => 2 ** 8,
                            Ts_X_Origin   => 2 ** 9,
                            Tx_Y_Origin   => 2 ** 10,
                            Clip_X_Origin => 2 ** 11,
                            Clip_Y_Origin => 2 ** 12,
                            Exposures     => 2 ** 13,
                            Line_Width    => 2 ** 14,
                            Line_Style    => 2 ** 15,
                            Cap_Style     => 2 ** 16,
                            Join_Style    => 2 ** 17);
   --
   --  FIXME: See Gdk_Event_Mask. Same remark.

end Gdk.Types;
