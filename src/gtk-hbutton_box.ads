
with Gtk.Button_Box;

package Gtk.HButton_Box is

   type HButton_Box is new Gtk.Button_Box.Button_Box with private;

   function Get_Spacing_Default return GInt;
   --  mapping: Get_Spacing_Default gtkhbbox.h \
   --  mapping: gtk_hbutton_box_get_spacing_default

   function Get_Layout_Default return Gtk.Button_Box.Style;
   --  mapping: Get_Layout_Default gtkhbbox.h \
   --  mapping: gtk_hbutton_box_get_layout_default

   procedure Set_Spacing_Default (Spacing : in GInt);
   --  mapping: Set_Spacing_Default gtkhbbox.h \
   --  mapping: gtk_hbutton_box_set_spacing_default

   procedure Set_Layout_Default (Layout : in Gtk.Button_Box.Style);
   --  mapping: Set_Layout_Default gtkhbbox.h \
   --  mapping: gtk_hbutton_box_set_layout_default

   --  mapping: NOT_IMPLEMENTED gtkhbbox.h gtk_hbutton_box_get_type

private

   type HButton_Box is new Gtk.Button_Box.Button_Box with null record;


end Gtk.HButton_Box;
