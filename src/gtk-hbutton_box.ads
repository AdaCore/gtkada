
with Gtk.Button_Box;

package Gtk.HButton_Box is

   type Gtk_HButton_Box is new Button_Box.Gtk_Button_Box with private;

   function Get_Spacing_Default return Gint;
   --  mapping: Get_Spacing_Default gtkhbbox.h \
   --  mapping: gtk_hbutton_box_get_spacing_default

   function Get_Layout_Default return Button_Box.Style;
   --  mapping: Get_Layout_Default gtkhbbox.h \
   --  mapping: gtk_hbutton_box_get_layout_default

   procedure Set_Spacing_Default (Spacing : in Gint);
   --  mapping: Set_Spacing_Default gtkhbbox.h \
   --  mapping: gtk_hbutton_box_set_spacing_default

   procedure Set_Layout_Default (Layout : in Button_Box.Style);
   --  mapping: Set_Layout_Default gtkhbbox.h \
   --  mapping: gtk_hbutton_box_set_layout_default


private

   type Gtk_HButton_Box is new Button_Box.Gtk_Button_Box with null record;

   --  mapping: NOT_IMPLEMENTED gtkhbbox.h gtk_hbutton_box_get_type

end Gtk.HButton_Box;
