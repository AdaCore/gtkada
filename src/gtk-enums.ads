with Glib;

package Gtk.Enums is

   type Gtk_State_Type is (State_Normal,
                           State_Active,
                           State_Prelight,
                           State_Selected,
                           State_Insensitive);

   type Gtk_Window_Type is (Window_Toplevel,
                            Window_Dialog,
                            Window_Popup);

   type Gtk_Direction_Type is (Dir_Tab_Forward,
                               Dir_Tab_Backward,
                               Dir_Up,
                               Dir_Down,
                               Dir_Left,
                               Dir_Right);

   type Gtk_Shadow_Type is (Shadow_None,
                            Shadow_In,
                            Shadow_Out,
                            Shadow_Etched_In,
                            Shadow_Etched_Out);

   type Gtk_Arrow_Type is (Arrow_Up,
                           Arrow_Down,
                           Arrow_Left,
                           Arrow_Right);

   --  FIXME  move the Pack_Type back here.

   type Gtk_Policy_Type is (Policy_Always,
                            Policy_Automatic);

   type Gtk_Update_Type is (Update_Continuous,
                            Update_Discontinuous,
                            Update_Delayed);

   type Gtk_Attach_Options is new Glib.Gint;
   Expand : constant Gtk_Attach_Options := 1;
   Shrink : constant Gtk_Attach_Options := 2;
   Fill   : constant Gtk_Attach_Options := 4;

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

   type Gtk_Window_Position is (Win_Pos_None,
                                Win_Pos_Center,
                                Win_Pos_Mouse);

   type Gtk_Submenu_Direction is (Direction_Left,
                                  Direction_Right);

   type Gtk_Submenu_Placement is (Top_Bottom,
                                  Left_Right);

   type Gtk_Menu_Factory_Type is (Factory_Menu,
                                  Factory_Menu_Bar,
                                  Factory_Option_Menu);

   type Gtk_Metric_Type is (Pixels,
                            Inches,
                            Centimeters);

   type Gtk_Scroll_Type is (Scroll_None,
                            Scroll_Step_Backward,
                            Scroll_Step_Forward,
                            Scroll_Page_Backward,
                            Scroll_Page_Forward,
                            Scroll_Jump);

   type Gtk_Trough_Type is (Trough_None,
                            Trough_Start,
                            Trough_End,
                            Trough_Jump);

   type Gtk_Position_Type is (Pos_Left,
                              Pos_Right,
                              Pos_Top,
                              Pos_Bottom);

   type Gtk_Preview_Type is (Preview_Color,
                             Preview_Grayscale);

   type Gtk_Justification is (Justify_Left,
                              Justify_Right,
                              Justify_Center,
                              Justify_Fill);

   type Gtk_Selection_Mode is (Selection_Single,
                               Selection_Browse,
                               Selection_Multiple,
                               Selection_Extended);

   type Gtk_Orientation is (Orientation_Horizontal,
                            Orientation_Vertical);

   type Gtk_Toolbar_Style is (Toolbar_Icons,
                              Toolbar_Text,
                              Toolbar_Both);

   type Gtk_Toolbar_Child_Type is (Gtk_Toolbar_Child_Space,
                                   Gtk_Toolbar_Child_Button,
                                   Gtk_Toolbar_Child_Togglebutton,
                                   Gtk_Toolbar_Child_Radiobutton,
                                   Gtk_Toolbar_Child_Widget);

   type Gtk_Tree_View_Mode is (Gtk_Tree_View_Line,
                               Gtk_Tree_View_Item);

   type Gtk_Visibility is (Visibility_None,
                           Visibility_Partial,
                           Visibility_Full);

end Gtk.Enums;
