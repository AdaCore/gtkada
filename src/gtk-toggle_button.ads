with Gtk.Button;

package Gtk.Toggle_Button is

   type Gtk_Toggle_Button is new Button.Gtk_Button with private;

   type Toggle_Button_Mode is (Button_Style, Radio_Style);


   procedure Gtk_New (Widget : out Gtk_Toggle_Button);
   --  mapping: Create_New gtktogglebutton.h gtk_toggle_button_new

   procedure Gtk_New (Widget : out Gtk_Toggle_Button;
                         With_Label : in String);
   --  mapping: Create_New gtktogglebutton.h gtk_toggle_button_new_with_label

   procedure Set_Mode (Widget : in out Gtk_Toggle_Button'Class;
                       New_Mode : in Toggle_Button_Mode);
   --  mapping: Set_Mode gtktogglebutton.h gtk_toggle_button_set_mode

   procedure Set_State (Widget : in out Gtk_Toggle_Button'Class;
                        To_Selected : in Boolean);
   --  mapping: Set_State gtktogglebutton.h gtk_toggle_button_set_state


   procedure Toggled (Widget : in out Gtk_Toggle_Button'Class);
   --  mapping: Toggled gtktogglebutton.h gtk_toggle_button_toggled


   function Is_Selected (Widget : in Gtk_Toggle_Button'Class) return Boolean;


private

   type Gtk_Toggle_Button is new Button.Gtk_Button with null record;

   --  Services not mapped ...
   --
   --  mapping: OBJECT_ORIENTED gtktogglebutton.h gtk_toggle_button_get_type

end Gtk.Toggle_Button;
