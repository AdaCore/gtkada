
with Gtk.Enums; use Gtk.Enums;
with Gtk.Misc;

package Gtk.Arrow is

   type Gtk_Arrow is new Gtk.Misc.Gtk_Misc with private;

   procedure Gtk_New
      (Widget      : out Gtk_Arrow;
       Arrow_Type  : in Gtk_Arrow_Type;
       Shadow_Type : in Gtk_Shadow_Type);
   procedure Set
      (Arrow       : in Gtk_Arrow'Class;
       Arrow_Type  : in Gtk_Arrow_Type;
       Shadow_Type : in Gtk_Shadow_Type);

private
   type Gtk_Arrow is new Gtk.Misc.Gtk_Misc with null record;

   --  mapping: NOT_IMPLEMENTED gtkarrow.h gtk_arrow_get_type
   --  mapping: Gtk_New gtkarrow.h gtk_arrow_new
   --  mapping: Set gtkarrow.h gtk_arrow_set
end Gtk.Arrow;
