
with Gtk.Button_Box;

package Gtk.VButton_Box is

   type Gtk_VButton_Box is new Button_Box.Gtk_Button_Box with private;

   function Get_Spacing_Default return Gint;
   --  mapping: Get_Spacing_Default gtkvbbox.h \
   --  mapping: gtk_vbutton_box_get_spacing_default

   function Get_Layout_Default return Button_Box.Style;
   --  mapping: Get_Layout_Default gtkvbbox.h \
   --  mapping: gtk_vbutton_box_get_layout_default

   procedure Set_Spacing_Default (Spacing : in Gint);
   --  mapping: Set_Spacing_Default gtkvbbox.h \
   --  mapping: gtk_vbutton_box_set_spacing_default

   procedure Set_Layout_Default (Layout : in Button_Box.Style);
   --  mapping: Set_Layout_Default gtkvbbox.h \
   --  mapping: gtk_vbutton_box_set_layout_default

private

   type Gtk_VButton_Box is new Button_Box.Gtk_Button_Box with null record;

   --  mapping: NOT_IMPLEMENTED gtkvbbox.h gtk_vbutton_box_get_type

end Gtk.VButton_Box;
