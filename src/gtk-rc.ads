with Gtk.Style;
with Gtk.Widget;

package Gtk.Rc is


   procedure Init;
   --  mapping: Init gtkrc.h gtk_rc_init

   procedure Parse (Filename : in String);
   --  mapping: Parse gtkrc.h gtk_rc_parse

   procedure Parse_String (Rc_String : in String);
   --  mapping: Parse_String gtkrc.h gtk_rc_parse_string

   procedure Get_Style (Widget : in     Gtk.Widget.Gtk_Widget'Class;
                        Style  :    out Gtk.Style.Gtk_Style);
   --  mapping: Get_Style gtkrc.h gtk_rc_get_style

   procedure Add_Widget_Name_Style (Style   : in out Gtk.Style.Gtk_Style;
                                    Pattern : in     String);
   --  mapping: Add_Widget_Name_Style gtkrc.h gtk_rc_add_widget_name_style

   procedure Add_Widget_Class_Style (Style   : in out Gtk.Style.Gtk_Style;
                                     Pattern : in     String);
   --  mapping: Add_Widget_Class_Style gtkrc.h gtk_rc_add_widget_class_style


   pragma Import (C, Init, "gtk_rc_init");

end Gtk.Rc;
