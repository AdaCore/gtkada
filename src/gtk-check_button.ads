with Gtk.Toggle_Button;

package Gtk.Check_Button is

   type Gtk_Check_Button is new Toggle_Button.Gtk_Toggle_Button with private;


   procedure Gtk_New (Widget : out Gtk_Check_Button);
   --  mapping: Create_New gtkcheckbutton.h gtk_check_button_new

   procedure Gtk_New (Widget : out Gtk_Check_Button;
                      With_Label : in String);
   --  mapping: Create_New gtkcheckbutton.h gtk_check_button_new_with_label

private

   type Gtk_Check_Button is new Toggle_Button.Gtk_Toggle_Button
     with null record;

   --  Services not mappend ...
   --
   --  mapping: USE_OBJECT_ORIENTED gtkcheckbutton.h gtk_check_button_get_type

end Gtk.Check_Button;

