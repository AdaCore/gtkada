
with Gtk.Box;

package Gtk.Box.Button_Box is

   type Button_Box is abstract new Gtk.Box.Box with private;

   type Style is (Default_Style, Spread, Edge, Start, Style_End);
   --  mapping: Style gtkbbox.h GtkButtonBoxStyle

   procedure Get_Child_Ipadding (Widget : in Button_Box'Class;
                                 Ipad_X : out Gint;
                                 Ipad_Y : out GInt);
   --  mapping: Get_Child_Ipadding gtkbbox.h gtk_button_box_get_child_ipadding

   procedure Get_Child_Ipadding_Default (Ipad_X : out Gint;
                                         Ipad_Y : out Gint);
   --  mapping: Get_Child_Ipadding_Default gtkbbox.h \
   --  mapping: gtk_button_box_get_child_ipadding_default

   procedure Get_Child_Size (Widget     : in Button_Box'Class;
                             Min_Width  : out GInt;
                             Min_Height : out Gint);
   --  mapping: Get_Child_Size gtkbbox.h gtk_button_box_get_child_size

   procedure Get_Child_Size_Default (Min_Width  : out Gint;
                                     Min_Height : out Gint);
   --  mapping: Get_Child_Size_Default gtkbbox.h \
   --  mapping: gtk_button_box_get_child_size_default

   function Get_Layout (Widget : in Button_Box'Class)
                        return Style;
   --  mapping: Get_Layout gtkbbox.h gtk_button_box_get_layout

   function Get_Spacing (Widget : in Button_Box'Class)
                         return GInt;
   --  mapping: Get_Spacing gtkbbox.h gtk_button_box_get_spacing

   procedure Set_Child_Ipadding (Widget : in Button_Box'Class;
                                 Ipad_X : in GInt;
                                 Ipad_Y : in GInt);
   --  mapping: Set_Child_Ipadding gtkbbox.h gtk_button_box_set_child_ipadding

   procedure Set_Child_Ipadding_Default (Ipad_X : in GInt;
                                         Ipad_Y : in GInt);
   --  mapping: Set_Child_Ipadding_Default gtkbbox.h \
   --  mapping: gtk_button_box_set_child_ipadding_default

   procedure Set_Child_Size (Widget     : in Button_Box'Class;
                             Min_Width  : in GInt;
                             Min_Height : in GInt);
   --  mapping: Set_Child_Size gtkbbox.h gtk_button_box_set_child_size

   procedure Set_Child_Size_Default (Min_Width  : in GInt;
                                     Min_Height : in GInt);
   --  mapping: Set_Child_Size_Default gtkbbox.h \
   --  mapping: gtk_button_box_set_child_size_default

   procedure Set_Layout (Widget       : in Button_Box'Class;
                         Layout_Style : in Style);
   --  mapping: Set_Layout gtkbbox.h gtk_button_box_set_layout

   procedure Set_Spacing (Widget  : in Button_Box'Class;
                          Spacing : in GInt);
   --  mapping: Set_Spacing gtkbbox.h gtk_button_box_set_spacing

   --  mapping: NOT_IMPLEMENTED gtkbbox.h gtk_button_box_get_type
   --  mapping: NOT_IMPLEMENTED gtkbbox.h gtk_button_box_child_requisition


private

   type Button_Box is new Gtk.Box.Box with null record;

end Gtk.Box.Button_Box;
