
with Gtk.Button_Box;
with Gtk.Enums; use Gtk.Enums;

package Gtk.HButton_Box is

   type Gtk_HButton_Box is new Gtk.Button_Box.Gtk_Button_Box with private;

   function Get_Layout_Default return Gtk_Button_Box_Style;
   function Get_Spacing_Default return Gint;
   procedure Gtk_New (Widget : out Gtk_HButton_Box);
   procedure Set_Layout_Default (Layout : in Gtk_Button_Box_Style);
   procedure Set_Spacing_Default (Spacing : in Gint);

private
   type Gtk_HButton_Box is new Gtk.Button_Box.Gtk_Button_Box with null record;

   --  mapping: Get_Layout_Default gtkhbbox.h \
   --  mapping:      gtk_hbutton_box_get_layout_default
   --  mapping: Get_Spacing_Default gtkhbbox.h \
   --  mapping:      gtk_hbutton_box_get_spacing_default
   --  mapping: NOT_IMPLEMENTED gtkhbbox.h gtk_hbutton_box_get_type
   --  mapping: Gtk_New gtkhbbox.h gtk_hbutton_box_new
   --  mapping: Set_Layout_Default gtkhbbox.h \
   --  mapping:      gtk_hbutton_box_set_layout_default
   --  mapping: Set_Spacing_Default gtkhbbox.h \
   --  mapping:      gtk_hbutton_box_set_spacing_default
end Gtk.HButton_Box;
