with Gtk.Data;

package Gtk.Adjustment is

   type Gtk_Adjustment is new Data.Gtk_Data with private;

   procedure Gtk_New (Adjustment : out Gtk_Adjustment;
                      Value          : in Gfloat;
                      Lower          : in Gfloat;
                      Upper          : in Gfloat;
                      Step_Increment : in Gfloat;
                      Page_Increment : in Gfloat;
                      Page_Size      : in Gfloat);
   --  mapping: Gtk_New gtkadjustment.h gtk_adjustment_new

   procedure Set_Value (Adjustment : in out Gtk_Adjustment;
                        Value      : in     Gfloat);
   --  mapping: Set_Value gtkadjustment.h gtk_adjustment_set_value

   procedure Clamp_Page (Adjustment : in out Gtk_Adjustment;
                         Lower      : in     Gfloat;
                         Upper      : in     Gfloat);
   --  mapping: Clamp_Page gtkadjustment.h gtk_adjustment_clamp_page

private

   type Gtk_Adjustment is new Data.Gtk_Data with null record;

   --  mapping: NOT_IMPLEMENTED gtkadjustment.h gtk_adjustment_get_type

end Gtk.Adjustment;
