
with Gtk.Button;

package Gtk.Toggle_Button is

   type Gtk_Toggle_Button is new Gtk.Button.Gtk_Button with private;

   function Is_Active (Widget : in Gtk_Toggle_Button'Class)
                        return      Boolean;
   procedure Gtk_New (Widget : out Gtk_Toggle_Button);
   procedure Gtk_New (Widget : out Gtk_Toggle_Button;
                      Label  : in String);
   procedure Set_Mode
      (Toggle_Button  : in Gtk_Toggle_Button'Class;
       Draw_Indicator : in Gint);
   procedure Set_State
      (Toggle_Button : in Gtk_Toggle_Button'Class;
       Active        : in Boolean);
   procedure Toggled (Toggle_Button : in Gtk_Toggle_Button'Class);

private
   type Gtk_Toggle_Button is new Gtk.Button.Gtk_Button with null record;

   --  mapping: Is_Active gtktogglebutton.h GtkToggleButton->active
   --  mapping: NOT_IMPLEMENTED gtktogglebutton.h gtk_toggle_button_get_type
   --  mapping: Gtk_New gtktogglebutton.h gtk_toggle_button_new
   --  mapping: Gtk_New gtktogglebutton.h gtk_toggle_button_new_with_label
   --  mapping: Set_Mode gtktogglebutton.h gtk_toggle_button_set_mode
   --  mapping: Set_State gtktogglebutton.h gtk_toggle_button_set_state
   --  mapping: Toggled gtktogglebutton.h gtk_toggle_button_toggled
end Gtk.Toggle_Button;
