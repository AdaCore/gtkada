
package Gtk.Enums is

   type State_Type is (State_Normal,
                       State_Active,
                       State_Prelight,
                       State_Selected,
                       State_Insensitive);

   --  FIXME : Move the Window_Type back here.

   type Direction_Type is (Dir_Tab_Forward,
                           Dir_Tab_Backward,
                           Dir_Up,
                           Dir_Down,
                           Dir_Left,
                           Dir_Right);

   type Shadow_Type is (Shadow_None,
                        Shadow_In,
                        Shadow_Out,
                        Shadow_Etched_In,
                        Shadow_Etched_Out);

   type Arrow_Type is (Arrow_Up,
                       Arrow_Down,
                       Arrow_Left,
                       Arrow_Right);

   --  FIXME  move the Pack_Type back here.

   type Policy_Type is (Policy_Always,
                        Policy_Automatic);

   type Update_Type is (Update_Continuous,
                        Update_Discontinuous,
                        Update_Delayed);

   --  FIXME  How do we bind this ?
   --
   --  /* Attach options (for tables) */
   --  typedef enum
   --  {
   --    GTK_EXPAND = 1 << 0,
   --    GTK_SHRINK = 1 << 1,
   --    GTK_FILL   = 1 << 2
   --  } GtkAttachOptions;
   --
   --     type Attach_Options is (Expand,
   --                      Shrink,
   --                      Fill);

   --  FIXME  use some rep clauses here?
   --
   --  typedef enum
   --  {
   --    GTK_RUN_FIRST      = 0x1,
   --    GTK_RUN_LAST       = 0x2,
   --    GTK_RUN_BOTH       = 0x3,
   --    GTK_RUN_MASK       = 0xF,
   --    GTK_RUN_NO_RECURSE = 0x10
   --  } GtkSignalRunType;

   type Window_Position is (Win_Pos_None,
                            Win_Pos_Center,
                            Win_Pos_Mouse);

   type Submenu_Direction is (Direction_Left,
                              Direction_Right);

   type Submenu_Placement is (Top_Bottom,
                              Left_Right);

   type Menu_Factory_Type is (Factory_Menu,
                              Factory_Menu_Bar,
                              Factory_Option_Menu);

   type Metric_Type is (Pixels,
                        Inches,
                        Centimeters);

   type Scroll_Type is (Scroll_None,
                        Scroll_Step_Backward,
                        Scroll_Step_Forward,
                        Scroll_Page_Backward,
                        Scroll_Page_Forward,
                        Scroll_Jump);

   type Trough_Type is (Trough_None,
                        Trough_Start,
                        Trough_End,
                        Trough_Jump);

   type Position_Type is (Pos_Left,
                          Pos_Right,
                          Pos_Top,
                          Pos_Bottom);

   type Preview_Type is (Preview_Color,
                         Preview_Grayscale);

   type Justification is (Justify_Left,
                          Justify_Right,
                          Justify_Center,
                          Justify_Fill);

   type Selection_Mode is (Selection_Single,
                           Selection_Browse,
                           Selection_Multiple,
                           Selection_Extended);

   type Orientation is (Orientation_Horizontal,
                        Orientation_Vertical);

   type Toolbar_Style is (Toolbar_Icons,
                          Toolbar_Text,
                          Toolbar_Both);

   type Visibility is (Visibility_None,
                       Visibility_Partial,
                       Visibility_Full);

end Gtk.Enums;
