
with Gtk.Bin;

package Gtk.Alignment is

   type Gtk_Alignment is new Gtk.Bin.Gtk_Bin with private;

   function Get_Xalign (Widget : in Gtk_Alignment'Class)
                        return      Gfloat;
   function Get_Xscale (Widget : in Gtk_Alignment'Class)
                        return      Gfloat;
   function Get_Yalign (Widget : in Gtk_Alignment'Class)
                        return      Gfloat;
   function Get_Yscale (Widget : in Gtk_Alignment'Class)
                        return      Gfloat;
   procedure Gtk_New
      (Widget : out Gtk_Alignment;
       Xalign : in Gfloat;
       Yalign : in Gfloat;
       Xscale : in Gfloat;
       Yscale : in Gfloat);
   procedure Set
      (Alignment : in Gtk_Alignment'Class;
       Xalign    : in Gfloat;
       Yalign    : in Gfloat;
       Xscale    : in Gfloat;
       Yscale    : in Gfloat);

private
   type Gtk_Alignment is new Gtk.Bin.Gtk_Bin with null record;

   --  mapping: NOT_IMPLEMENTED gtkalignment.h gtk_alignment_get_type
   --  mapping: Get_Xalign gtkalignment.h GtkAlignment->xalign
   --  mapping: Get_Xscale gtkalignment.h GtkAlignment->xscale
   --  mapping: Get_Yalign gtkalignment.h GtkAlignment->yalign
   --  mapping: Get_Yscale gtkalignment.h GtkAlignment->yscale
   --  mapping: Gtk_New gtkalignment.h gtk_alignment_new
   --  mapping: Set gtkalignment.h gtk_alignment_set
end Gtk.Alignment;
