-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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

--  <description>
--
--  A Gtk_Button_Box is a special type of Gtk_Box specially tailored to contain
--  buttons.
--
--  This is only a base class for Gtk_Hbutton_Box and Gtk_Vbutton_Box which
--  provide a way to arrange their children horizontally (resp. vertically).
--  You can not instanciate a Gtk_Button_Box directly, and have to use one the
--  above two instead.
--
--  </description>
--  <c_version>1.2.6</c_version>

with Gtk.Box;
with Gtk.Enums;
with Gtk.Object;

package Gtk.Button_Box is

   type Gtk_Button_Box_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Gtk_Button_Box is access all Gtk_Button_Box_Record'Class;

   function Get_Type return Gtk.Gtk_Type;
   --  Returns the internal value associated with a Gtk_Button_Box internally.

   procedure Set_Child_Size_Default (Min_Width  : in Gint;
                                     Min_Height : in Gint);
   --  Sets the default size for the children of the box.
   --  This is the minimal size that the children will have (in pixels).
   --  These default values apply to all the Button_Boxes created in your
   --  application, unless for boxes where Set_Child_Size has been called.

   procedure Get_Child_Size_Default (Min_Width  : out Gint;
                                     Min_Height : out Gint);
   --  Returns the default size for the children of the button_boxes in your
   --  application.

   procedure Set_Child_Ipadding_Default (Ipad_X : in Gint;
                                         Ipad_Y : in Gint);
   --  Sets the default padding (the empty space left around all children) for
   --  all the button_box in your application, except those for which
   --  Set_Child_Ipadding has been used.

   procedure Get_Child_Ipadding_Default (Ipad_X : out Gint;
                                         Ipad_Y : out Gint);
   --  Returns the default padding for all the button_boxes in your
   --  application.

   procedure Set_Spacing (Button_Box : access Gtk_Button_Box_Record;
                          Spacing    : in Gint);
   --  Sets the spacing (ie the space left between two adjacent children) for
   --  the button box. Note that there is a default spacing set for
   --  Gtk_Hbutton_boxes and Gtk_Vbutton_boxes.

   function Get_Spacing (Button_Box : access Gtk_Button_Box_Record)
                        return Gint;
   --  Returns the spacing use for the button box.

   procedure Set_Layout (Button_Box   : access Gtk_Button_Box_Record;
                         Layout_Style : in Enums.Gtk_Button_Box_Style);
   --  Sets the layout to use for the box.
   --  There are four such styles:
   --    Buttonbox_Spread: The children are spread regularly across the box
   --    Buttonbox_Edge  : Same as Spread, except that the first and last
   --                      children are aligned on the border of the box.
   --    Buttonbox_Start : The children are put as much to the left (resp. top)
   --                      as possible in the box.
   --    Buttonbox_End   : The children are put as much to the right
   --                      (resp. bottom) as possible in the box.

   function Get_Layout (Button_Box : access Gtk_Button_Box_Record)
                       return Enums.Gtk_Button_Box_Style;
   --  Returns the layout used in the box.

   procedure Set_Child_Size (Button_Box : access Gtk_Button_Box_Record;
                             Min_Width  : in Gint;
                             Min_Height : in Gint);
   --  Sets the size to use for children of this specific box. You can modify
   --  the size for all the boxes at once by using Set_Child_Size_Default.

   procedure Get_Child_Size (Button_Box : access Gtk_Button_Box_Record;
                             Min_Width  : out Gint;
                             Min_Height : out Gint);
   --  Returns the size to use for children of this specific box.
   --  MIN_WIDTH and MIN_HEIGHT are set to -1 if this widget uses the default
   --  size set by Set_Child_Size_Default.

   procedure Set_Child_Ipadding (Button_Box : access Gtk_Button_Box_Record;
                                 Ipad_X     : in Gint;
                                 Ipad_Y     : in Gint);
   --  Sets the padding to use for the children of this specific box. You can
   --  modify the default padding to use for all the boxes by using
   --  Set_Child_Ipadding_Default.

   procedure Get_Child_Ipadding (Button_Box : access Gtk_Button_Box_Record;
                                 Ipad_X     : out Gint;
                                 Ipad_Y     : out Gint);
   --  Returns the padding to use for children of this specific box.
   --  IPAD_X and IPAD_Y are set to -1 if this widget uses the default
   --  values set by Set_Child_Ipadding_Default.

   ----------------------------
   -- Support for GATE/DGATE --
   ----------------------------

   procedure Generate (N : in Node_Ptr; File : in File_Type);
   --  Gate internal function

   procedure Generate (Button_Box : in out Object.Gtk_Object; N : in Node_Ptr);
   --  Dgate internal function

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Button_Box_Record is new Gtk.Box.Gtk_Box_Record with null record;
   pragma Import (C, Get_Type, "gtk_button_box_get_type");
   pragma Import (C, Set_Child_Ipadding_Default,
                  "gtk_button_box_set_child_ipadding_default");
   pragma Import (C, Set_Child_Size_Default,
                  "gtk_button_box_set_child_size_default");
   pragma Import (C, Get_Child_Size_Default,
                  "gtk_button_box_get_child_size_default");
   pragma Import (C, Get_Child_Ipadding_Default,
                  "gtk_button_box_get_child_ipadding_default");
end Gtk.Button_Box;
