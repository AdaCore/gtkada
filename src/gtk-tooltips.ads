
with Gtk.Data;
with Gtk.Widget;

package Gtk.Tooltips is

   type Gtk_Tooltips is new Gtk.Data.Gtk_Data with private;

   procedure Enable (Tooltips : in Gtk_Tooltips'Class);
   procedure Disable (Tooltips : in Gtk_Tooltips'Class);
   procedure Gtk_New (Widget : out Gtk_Tooltips);
   procedure Set_Delay
     (Tooltips : in Gtk_Tooltips'Class;
      Duration : in Guint);
   procedure Set_Tip
     (Tooltips    : in Gtk_Tooltips'Class;
      Widget      : in Gtk.Widget.Gtk_Widget'Class;
      Tip_Text    : in String;
      Tip_Private : in String);


private

   type Gtk_Tooltips is new Gtk.Data.Gtk_Data with null record;

   --  mapping: NOT_IMPLEMENTED gtktooltips.h gtk_tooltips_get_type
   --  mapping: Gtk_New gtktooltips.h gtk_tooltips_new
   --  mapping: Enable gtktooltips.h gtk_tooltips_enable
   --  mapping: Disable gtktooltips.h gtk_tooltips_disable
   --  mapping: Set_Delay gtktooltips.h gtk_tooltips_set_delay
   --  mapping: Set_Tip gtktooltips.h gtk_tooltips_set_tip
   --  mapping: NOT_IMPLEMENTED gtktooltips.h gtk_tooltips_set_colors
   --  mapping: NOT_IMPLEMENTED gtktooltips.h gtk_tooltips_data_get

end Gtk.Tooltips;
