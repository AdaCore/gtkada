with Gtk.Check_Button;
with Glib.GSlist; use Glib.GSlist;
with Gtk.Widget;

package Gtk.Radio_Button is

   type Gtk_Radio_Button is new Check_Button.Gtk_Check_Button with private;
   package Widget_SList is new Glib.GSlist.Generic_SList
     (Gtk.Widget.Gtk_Widget'Class);

   function Group (Button : in Gtk_Radio_Button) return Widget_SList.GSlist;
   --  mapping: Set_Group gtkradiobutton.h gtk_radio_button_group

   procedure Gtk_New (Button : out Gtk_Radio_Button;
                      Group  : in Widget_SList.GSlist);
   --  mapping: Gtk_New gtkradiobutton.h gtk_radio_button_new

   procedure Gtk_New (Button : out Gtk_Radio_Button;
                      Group  : in Widget_SList.GSlist;
                      Label  : in String);
   --  mapping: Gtk_New gtkradiobutton.h gtk_radio_button_new_with_label

   procedure Gtk_New
     (Button : out Gtk_Radio_Button;
      Group  : in Gtk_Radio_Button);
   --  mapping: New_From_Widget gtkradiobutton.h \
   --  mapping:                 gtk_radio_button_new_from_widget

   procedure Gtk_New
     (Button : out Gtk_Radio_Button;
      Group  : in Gtk_Radio_Button;
      Label  : in String);
   --  mapping: New_With_Label_From_Widget  gtkradiobutton.h \
   --  mapping: gtk_radio_button_new_with_label_from_widget

   procedure Set_Group (Button : in Gtk_Radio_Button;
                        Group  : in Widget_SList.GSlist);
   --  mapping: Set_Group gtkradiobutton.h gtk_radio_button_set_group

private

   type Gtk_Radio_Button is new Check_Button.Gtk_Check_Button with null record;

   --  mapping: NOT_IMPLEMENTED gtkradiobutton.h gtk_radio_button_get_type

end Gtk.Radio_Button;

