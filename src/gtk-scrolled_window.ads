with Gtk.Adjustment;
with Gtk.Container;
with Gtk.Enums;

package Gtk.Scrolled_Window is

   type Gtk_Scrolled_Window is new Container.Gtk_Container with private;

   procedure Gtk_New
     (Scrolled_Window :    out Gtk_Scrolled_Window'Class;
      Hadjustment     : in     Adjustment.Gtk_Adjustment'Class :=
        Adjustment.Null_Adjustment;
      Vadjustment     : in     Adjustment.Gtk_Adjustment'Class :=
        Adjustment.Null_Adjustment);
   --  mapping: Gtk_New gtkscrolledwindow.h gtk_scrolled_window_new

   procedure Construct
     (Scrolled_Window : in out Gtk_Scrolled_Window'Class;
      Hadjustment     : in     Adjustment.Gtk_Adjustment'Class :=
        Adjustment.Null_Adjustment;
      Vadjustment     : in     Adjustment.Gtk_Adjustment'Class :=
        Adjustment.Null_Adjustment);
   --  mapping: Construct gtkscrolledwindow.h gtk_scrolled_window_construct

   procedure Get_Hadjustement
     (Scrolled_Window : in     Gtk_Scrolled_Window'Class;
      Hadjustment     :    out Adjustment.Gtk_adjustment'Class);
   --  mapping: Get_Hadjustement gtkscrolledwindow.h \
   --  mapping:                  gtk_scrolled_window_get_hadjustment

   procedure Get_Vadjustement
     (Scrolled_Window : in     Gtk_Scrolled_Window'Class;
      Vadjustment     :    out Adjustment.Gtk_adjustment'Class);
   --  mapping: Get_Vadjustement gtkscrolledwindow.h \
   --  mapping:                  gtk_scrolled_window_get_vadjustment

   procedure Set_Policy (Scrolled_Window    : in out Gtk_Scrolled_Window'Class;
                         H_Scrollbar_Policy : in     Enums.Policy_Type;
                         V_Scrollbar_Policy : in     Enums.Policy_Type);
   --  mapping: Set_Policy gtkscrolledwindow.h gtk_scrolled_window_set_policy

private

   type Gtk_Scrolled_Window is new Container.Gtk_Container with null record;

   --  mapping: USE_OBJECT_ORIENTED gtkscrolledwindow.h \
   --  mapping:                     gtk_scrolled_window_get_type

end Gtk.Scrolled_Window;
