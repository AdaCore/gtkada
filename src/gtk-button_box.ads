
with Gtk.Box;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package Gtk.Button_Box is

   type Gtk_Button_Box is new Gtk.Box.Gtk_Box with private;

   procedure Child_Requisition
      (Widget        : in Gtk.Widget.Gtk_Widget'Class;
       Nvis_Children : in out Integer;
       Width         : in out Integer;
       Height        : in out Integer);
   procedure Get_Child_Ipadding
      (Widget : in Gtk_Button_Box'Class;
       Ipad_X : in out Gint;
       Ipad_Y : in out Gint);
   procedure Get_Child_Ipadding_Default
      (Ipad_X : in out Gint;
       Ipad_Y : in out Gint);
   procedure Get_Child_Size
      (Widget     : in Gtk_Button_Box'Class;
       Min_Width  : in out Gint;
       Min_Height : in out Gint);
   procedure Get_Child_Size_Default
      (Min_Width  : in out Gint;
       Min_Height : in out Gint);
   function Get_Layout (Widget : in Gtk_Button_Box'Class)
                        return      Gtk_Button_Box_Style;
   function Get_Spacing (Widget : in Gtk_Button_Box'Class)
                         return      Gint;
   procedure Set_Child_Ipadding
      (Widget : in Gtk_Button_Box'Class;
       Ipad_X : in Gint;
       Ipad_Y : in Gint);
   procedure Set_Child_Ipadding_Default
      (Ipad_X : in Gint;
       Ipad_Y : in Gint);
   procedure Set_Child_Size
      (Widget     : in Gtk_Button_Box'Class;
       Min_Width  : in Gint;
       Min_Height : in Gint);
   procedure Set_Child_Size_Default
      (Min_Width  : in Gint;
       Min_Height : in Gint);
   procedure Set_Layout
      (Widget       : in Gtk_Button_Box'Class;
       Layout_Style : in Gtk_Button_Box_Style);
   procedure Set_Spacing
      (Widget  : in Gtk_Button_Box'Class;
       Spacing : in Gint);

private
   type Gtk_Button_Box is new Gtk.Box.Gtk_Box with null record;

   --  mapping: Child_Requisition gtkbbox.h gtk_button_box_child_requisition
   --  mapping: Get_Child_Ipadding gtkbbox.h gtk_button_box_get_child_ipadding
   --  mapping: Get_Child_Ipadding_Default gtkbbox.h \
   --  mapping:      gtk_button_box_get_child_ipadding_default
   --  mapping: Get_Child_Size gtkbbox.h gtk_button_box_get_child_size
   --  mapping: Get_Child_Size_Default gtkbbox.h \
   --  mapping:      gtk_button_box_get_child_size_default
   --  mapping: Get_Layout gtkbbox.h gtk_button_box_get_layout
   --  mapping: Get_Spacing gtkbbox.h gtk_button_box_get_spacing
   --  mapping: NOT_IMPLEMENTED gtkbbox.h gtk_button_box_get_type
   --  mapping: Set_Child_Ipadding gtkbbox.h gtk_button_box_set_child_ipadding
   --  mapping: Set_Child_Ipadding_Default gtkbbox.h \
   --  mapping:      gtk_button_box_set_child_ipadding_default
   --  mapping: Set_Child_Size gtkbbox.h gtk_button_box_set_child_size
   --  mapping: Set_Child_Size_Default gtkbbox.h \
   --  mapping:      gtk_button_box_set_child_size_default
   --  mapping: Set_Layout gtkbbox.h gtk_button_box_set_layout
   --  mapping: Set_Spacing gtkbbox.h gtk_button_box_set_spacing
end Gtk.Button_Box;
