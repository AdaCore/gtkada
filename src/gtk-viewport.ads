
with Gtk.Adjustment;
with Gtk.Bin;
with Gtk.Enums; use Gtk.Enums;

package Gtk.Viewport is

   type Gtk_Viewport is new Gtk.Bin.Gtk_Bin with private;

   function Get_Hadjustment (Viewport : in Gtk_Viewport'Class)
                             return        Gtk.Adjustment.Gtk_Adjustment'Class;
   function Get_Vadjustment (Viewport : in Gtk_Viewport'Class)
                             return        Gtk.Adjustment.Gtk_Adjustment'Class;
   procedure Gtk_New
      (Widget      : out Gtk_Viewport;
       Hadjustment : in Gtk.Adjustment.Gtk_Adjustment'Class;
       Vadjustment : in Gtk.Adjustment.Gtk_Adjustment'Class);
   procedure Set_Hadjustment
      (Viewport   : in Gtk_Viewport'Class;
       Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class);
   procedure Set_Shadow_Type
      (Viewport : in Gtk_Viewport'Class;
       The_Type : in Gtk_Shadow_Type);
   procedure Set_Vadjustment
      (Viewport   : in Gtk_Viewport'Class;
       Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class);

private
   type Gtk_Viewport is new Gtk.Bin.Gtk_Bin with null record;

   --  mapping: Get_Hadjustment gtkviewport.h gtk_viewport_get_hadjustment
   --  mapping: NOT_IMPLEMENTED gtkviewport.h gtk_viewport_get_type
   --  mapping: Get_Vadjustment gtkviewport.h gtk_viewport_get_vadjustment
   --  mapping: Gtk_New gtkviewport.h gtk_viewport_new
   --  mapping: Set_Hadjustment gtkviewport.h gtk_viewport_set_hadjustment
   --  mapping: Set_Shadow_Type gtkviewport.h gtk_viewport_set_shadow_type
   --  mapping: Set_Vadjustment gtkviewport.h gtk_viewport_set_vadjustment
end Gtk.Viewport;
