with Gtk.Enums;
with Gtk.Misc;

package Gtk.Label is

   type Gtk_Label is new Misc.Gtk_Misc with private;

   procedure Gtk_New (Label :    out Gtk_Label;
                      Str   : in     String);
   --  mapping: Gtk_New gtklabel.h gtk_label_new

   procedure Set (Label : in Gtk_Label'Class;
                  Str   : in String);
   --  mapping: Set gtklabel.h gtk_label_set

   procedure Set_Justify (Label : in Gtk_Label'Class;
                          Jtype : in Enums.Gtk_Justification);
   --  mapping: NOT_IMPLEMENTED gtklabel.h gtk_label_set_justify

   function Get (Label : in Gtk_Label'Class) return String;
   --  mapping: NOT_IMPLEMENTED gtklabel.h gtk_label_get

private

   type Gtk_Label is new Misc.Gtk_Misc with null record;

   --  mapping: USE_OBJECT_ORIENTED gtklabel.h gtk_label_get_type

end Gtk.Label;
