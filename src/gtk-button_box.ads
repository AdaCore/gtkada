
with Gtk.Box;

package Gtk.Button_Box is

   type Gtk_Button_Box is abstract new Box.Gtk_Box with private;

   type Style is (Default_Style, Spread, Edge, Start, Style_End);
   --  mapping: Style gtkbbox.h GtkButtonBoxStyle

   procedure Get_Child_Ipadding (Widget : in Gtk_Button_Box'Class;
                                 Ipad_X : out Gint;
                                 Ipad_Y : out Gint);
   --  mapping: Get_Child_Ipadding gtkbbox.h gtk_button_box_get_child_ipadding

   procedure Get_Child_Ipadding_Default (Ipad_X : out Gint;
                                         Ipad_Y : out Gint);
   --  mapping: Get_Child_Ipadding_Default gtkbbox.h \
   --  mapping: gtk_button_box_get_child_ipadding_default

   procedure Get_Child_Size (Widget     : in Gtk_Button_Box'Class;
                             Min_Width  : out Gint;
                             Min_Height : out Gint);
   --  mapping: Get_Child_Size gtkbbox.h gtk_button_box_get_child_size

   procedure Get_Child_Size_Default (Min_Width  : out Gint;
                                     Min_Height : out Gint);
   --  mapping: Get_Child_Size_Default gtkbbox.h \
   --  mapping: gtk_button_box_get_child_size_default

   function Get_Layout (Widget : in Gtk_Button_Box'Class)
                        return Style;
   --  mapping: Get_Layout gtkbbox.h gtk_button_box_get_layout

   function Get_Spacing (Widget : in Gtk_Button_Box'Class)
                         return Gint;
   --  mapping: Get_Spacing gtkbbox.h gtk_button_box_get_spacing

   procedure Set_Child_Ipadding (Widget : in Gtk_Button_Box'Class;
                                 Ipad_X : in Gint;
                                 Ipad_Y : in Gint);
   --  mapping: Set_Child_Ipadding gtkbbox.h gtk_button_box_set_child_ipadding

   procedure Set_Child_Ipadding_Default (Ipad_X : in Gint;
                                         Ipad_Y : in Gint);
   --  mapping: Set_Child_Ipadding_Default gtkbbox.h \
   --  mapping: gtk_button_box_set_child_ipadding_default

   procedure Set_Child_Size (Widget     : in Gtk_Button_Box'Class;
                             Min_Width  : in Gint;
                             Min_Height : in Gint);
   --  mapping: Set_Child_Size gtkbbox.h gtk_button_box_set_child_size

   procedure Set_Child_Size_Default (Min_Width  : in Gint;
                                     Min_Height : in Gint);
   --  mapping: Set_Child_Size_Default gtkbbox.h \
   --  mapping: gtk_button_box_set_child_size_default

   procedure Set_Layout (Widget       : in Gtk_Button_Box'Class;
                         Layout_Style : in Style);
   --  mapping: Set_Layout gtkbbox.h gtk_button_box_set_layout

   procedure Set_Spacing (Widget  : in Gtk_Button_Box'Class;
                          Spacing : in Gint);
   --  mapping: Set_Spacing gtkbbox.h gtk_button_box_set_spacing

   --  mapping: NOT_IMPLEMENTED gtkbbox.h gtk_button_box_get_type
   --  mapping: NOT_IMPLEMENTED gtkbbox.h gtk_button_box_child_requisition


private

   type Gtk_Button_Box is new Box.Gtk_Box with null record;

end Gtk.Button_Box;
