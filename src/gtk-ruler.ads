
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package Gtk.Ruler is

   type Gtk_Ruler is new Gtk.Widget.Gtk_Widget with private;

   procedure Draw_Pos (Ruler : in Gtk_Ruler'Class);
   procedure Draw_Ticks (Ruler : in Gtk_Ruler'Class);
   function Get_Lower (Widget : in Gtk_Ruler'Class)
                       return      Gfloat;
   function Get_Max_Size (Widget : in Gtk_Ruler'Class)
                          return      Gfloat;
   function Get_Position (Widget : in Gtk_Ruler'Class)
                          return      Gfloat;
   function Get_Upper (Widget : in Gtk_Ruler'Class)
                       return      Gfloat;
   procedure Gtk_New_Hruler (Widget : out Gtk_Ruler);
   procedure Gtk_New_Vruler (Widget : out Gtk_Ruler);
   procedure Set_Metric
      (Ruler  : in Gtk_Ruler'Class;
       Metric : in Gtk_Metric_Type);
   procedure Set_Range
      (Ruler    : in Gtk_Ruler'Class;
       Lower    : in Gfloat;
       Upper    : in Gfloat;
       Position : in Gfloat;
       Max_Size : in Gfloat);

private
   type Gtk_Ruler is new Gtk.Widget.Gtk_Widget with null record;

   --  mapping: Draw_Pos gtkruler.h gtk_ruler_draw_pos
   --  mapping: Draw_Ticks gtkruler.h gtk_ruler_draw_ticks
   --  mapping: Get_Lower gtkruler.h GtkRuler->lower
   --  mapping: Get_Max_Size gtkruler.h GtkRuler->max_size
   --  mapping: Get_Position gtkruler.h GtkRuler->position
   --  mapping: NOT_IMPLEMENTED gtkruler.h gtk_ruler_get_type
   --  mapping: Get_Upper gtkruler.h GtkRuler->upper
   --  mapping: Set_Metric gtkruler.h gtk_ruler_set_metric
   --  mapping: Set_Range gtkruler.h gtk_ruler_set_range
   --  mapping: Gtk_New_Hruler gtkhruler.h gtk_hruler_new
   --  mapping: Gtk_New_Vruler gtkvruler.h gtk_vruler_new
end Gtk.Ruler;
