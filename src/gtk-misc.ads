with Gtk.Widget;

package Gtk.Misc is

   type Gtk_Misc is new Widget.Gtk_Widget with private;

   procedure Set_Alignment (Misc   : in out Gtk_Misc'Class;
                            Xalign : in     Gfloat;
                            Yalign : in     Gfloat);
   --  mapping: Set_Alignment gtkmisc.h gtk_misc_set_alignment

   procedure Set_Padding (Misc : in out Gtk_Misc'Class;
                          Xpad : in     Gint;
                          Ypad : in     Gint);
   --  mapping: Set_Padding gtkmisc.h gtk_misc_set_padding

private

   type Gtk_Misc is new Widget.Gtk_Widget with null record;

   --  mapping: USE_OBJECT_ORIENTED gtkmisc.h gtk_misc_get_type
end Gtk.Misc;
