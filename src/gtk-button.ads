with Gtk.Container;

package Gtk.Button is

   type Gtk_Button is new Container.Gtk_Container with private;


   procedure Gtk_New (Widget : out Gtk_Button);
   --  mapping: Create_New gtkbutton.h gtk_button_new

   procedure Gtk_New (Widget      : out Gtk_Button;
                      With_Label  : in String);
   --  mapping: Create_New gtkbutton.h gtk_button_new_with_label

   ---------------
   --  Signals  --
   ---------------

   procedure Pressed (Widget : in Gtk_Button'Class);
   --  mapping: Pressed gtkbutton.h gtk_button_pressed

   procedure Released (Widget : in Gtk_Button'Class);
   --  mapping: Released gtkbutton.h gtk_button_released

   procedure Clicked (Widget : in Gtk_Button'Class);
   --  mapping: Clicked gtkbutton.h gtk_button_clicked

   procedure Enter (Widget : in Gtk_Button'Class);
   --  mapping: Enter gtkbutton.h gtk_button_enter

   procedure Leave (Widget : in Gtk_Button'Class);
   --  mapping: Leave gtkbutton.h gtk_button_leave

private

   type Gtk_Button is new Container.Gtk_Container with null record;

   --  services NOT mapped...
   --

   --  mapping: USE_OBJECT_ORIENTED gtkbutton.h gtk_button_get_type

end Gtk.Button;
