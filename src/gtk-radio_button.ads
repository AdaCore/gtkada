with Gtk.Check_Button;

package Gtk.Radio_Button is

   type Gtk_Radio_Button is new Check_Button.Gtk_Check_Button with private;

   function Null_Gtk_Radio_Button return Gtk_Radio_Button;

   procedure New_From_Widget
     (Widget : out Gtk_Radio_Button;
      Group : in Gtk_Radio_Button := Null_Gtk_Radio_Button);
   --  mapping: New_From_Widget gtkradiobutton.h \
   --  mapping:                 gtk_radio_button_new_from_widget

   procedure New_From_Widget
     (Widget : out Gtk_Radio_Button;
      With_Label : in String;
      Group : in Gtk_Radio_Button := Null_Gtk_Radio_Button);
   --  mapping: New_With_Label_From_Widget  gtkradiobutton.h \
   --  mapping: gtk_radio_button_new_with_label_from_widget


private

   type Gtk_Radio_Button is new Check_Button.Gtk_Check_Button with null record;

   --  Services not mapped...
   --
   --  mapping: OBJECT_ORIENTED gtkradiobutton.h gtk_radio_button_get_type

   --  Services not mapped because they are not needed.
   --
   --  mapping: NOT_IMPLEMENTED gtkradiobutton.h gtk_radio_button_new
   --  mapping: NOT_IMPLEMENTED gtkradiobutton.h \
   --  mapping:                 gtk_radio_button_new_with_label
   --  mapping: NOT_IMPLEMENTED gtkradiobutton.h gtk_radio_button_group
   --  mapping: NOT_IMPLEMENTED gtkradiobutton.h gtk_radio_button_set_group


end Gtk.Radio_Button;
