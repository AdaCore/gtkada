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

with Glib;
with Glib.Glist;
with System;

package Gtk.Enums is

   type Gtk_State_Type is (State_Normal,
                           State_Active,
                           State_Prelight,
                           State_Selected,
                           State_Insensitive);

   type Gtk_Window_Type is (Window_Toplevel,
                            Window_Dialog,
                            Window_Popup);

   type Gtk_Button_Box_Style is (Default_Style, Spread,
                                 Edge, Start, Style_End);

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

   type Gtk_Cell_Type is (Cell_Empty,
                          Cell_Text,
                          Cell_Pixmap,
                          Cell_Pixtext,
                          Cell_Widget);

   type Gtk_Pack_Type is (Pack_Start, Pack_End);

   type Gtk_Policy_Type is (Policy_Always,
                            Policy_Automatic,
                            Policy_Never);

   type Gtk_Update_Type is (Update_Continuous,
                            Update_Discontinuous,
                            Update_Delayed);

   type Gtk_Relief_Style is (Normal, Half, None);

   type Gtk_Attach_Options is new Glib.Guint32;
   Expand : constant Gtk_Attach_Options := 1;
   Shrink : constant Gtk_Attach_Options := 2;
   Fill   : constant Gtk_Attach_Options := 4;

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

   type Gtk_Resize_Mode is (Resize_Parent,     --  Pass request to the parent
                            Resize_Queue,      --  Queue resizes on this widget
                            Resize_Immediate); --  Perform the resizes now

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

   type Gtk_Spin_Button_Update_Policy is new Glib.Guint32;
   Update_Always        : constant Gtk_Spin_Button_Update_Policy := 1;
   Update_If_Valid      : constant Gtk_Spin_Button_Update_Policy := 2;
   Update_Snap_To_Ticks : constant Gtk_Spin_Button_Update_Policy := 4;


   type Gtk_Toolbar_Style is (Toolbar_Icons,
                              Toolbar_Text,
                              Toolbar_Both);

   type Gtk_Toolbar_Child_Type is (Child_Space,
                                   Child_Button,
                                   Child_Togglebutton,
                                   Child_Radiobutton,
                                   Child_Widget);

   type Gtk_Tree_View_Mode is (View_Line,
                               View_Item);

   type Gtk_Visibility is (Visibility_None,
                           Visibility_Partial,
                           Visibility_Full);

   type Gtk_Progress_Bar_Style is (Continuous,
                                   Discrete);
   --  enum type introduced in gtk1.1

   type Gtk_Progress_Bar_Orientation is (Left_To_Right,
                                         Right_To_Left,
                                         Bottom_To_Top,
                                         Top_To_Bottom);
   --  enum type introduced in gtk1.1

   --  Some Glib instanciations

   function Convert (S : String) return System.Address;
   function Convert (S : System.Address) return String;
   function Convert_I (I : Gint) return System.Address;
   function Convert_A (S : System.Address) return Gint;
   pragma Import (C, Convert_I, "convert_i");
   pragma Import (C, Convert_A, "convert_a");

   package String_List is new Glib.Glist.Generic_List (String);
   package Gint_List is new Glib.Glist.Generic_List (Gint,
                                                     Convert_I,
                                                     Convert_A);

end Gtk.Enums;
