
with Gtk.Enums; use Gtk.Enums;
with Gtk.Gtk_Range;

package Gtk.Scale is

   type Gtk_Scale is abstract new Gtk.Gtk_Range.Gtk_Range with private;

   procedure Draw_Value (Scale : in Gtk_Scale'Class);
   procedure Set_Digits
      (Scale      : in Gtk_Scale'Class;
       The_Digits : in Gint);
   procedure Set_Draw_Value
      (Scale      : in Gtk_Scale'Class;
       Draw_Value : in Gint);
   procedure Set_Value_Pos
      (Scale : in Gtk_Scale'Class;
       Pos   : in Gtk_Position_Type);
   function Value_Width (Scale  : in Gtk_Scale'Class)
                         return      Gint;

private
   type Gtk_Scale is abstract new Gtk.Gtk_Range.Gtk_Range with null record;

   --  mapping: Draw_Value gtkscale.h gtk_scale_draw_value
   --  mapping: NOT_IMPLEMENTED gtkscale.h gtk_scale_get_type
   --  mapping: Set_Digits gtkscale.h gtk_scale_set_digits
   --  mapping: Set_Draw_Value gtkscale.h gtk_scale_set_draw_value
   --  mapping: Set_Value_Pos gtkscale.h gtk_scale_set_value_pos
   --  mapping: Value_Width gtkscale.h gtk_scale_value_width
end Gtk.Scale;
