
with Gtk.Button_Box;

package Gtk.VButton_Box is

   type VButton_Box is new Gtk.Button_Box.Button_Box with private;

   function Get_Spacing_Default return GInt;
   --  mapping: Get_Spacing_Default gtkvbbox.h \
   --  mapping: gtk_vbutton_box_get_spacing_default

   function Get_Layout_Default return Gtk.Button_Box.Style;
   --  mapping: Get_Layout_Default gtkvbbox.h \
   --  mapping: gtk_vbutton_box_get_layout_default

   procedure Set_Spacing_Default (Spacing : in GInt);
   --  mapping: Set_Spacing_Default gtkvbbox.h \
   --  mapping: gtk_vbutton_box_set_spacing_default

   procedure Set_Layout_Default (Layout : in Gtk.Button_Box.Style);
   --  mapping: Set_Layout_Default gtkvbbox.h \
   --  mapping: gtk_vbutton_box_set_layout_default

   --  mapping: NOT_IMPLEMENTED gtkvbbox.h gtk_vbutton_box_get_type

private

   type VButton_Box is new Gtk.Button_Box.Button_Box with null record;


end Gtk.VButton_Box;
