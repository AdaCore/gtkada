
with Gtk.Container;
with Gtk.Widget;

package Gtk.Box is

   type Box is abstract new Gtk.Container.Container with private;

   type Pack_Type is (Pack_Start, Pack_End);
   --  mapping: Pack_Type gtkenums.h GtkPackType

   procedure Pack_Start
     (In_Box  : in Box'Class;
      Child   : in Gtk.Widget.Widget'Class;
      Expand  : in Boolean := True;
      Fill    : in Boolean := True;
      Padding : in GInt    := 0);
   --  mapping: Pack_Start gtkbox.h gtk_box_pack_start
   --  mapping: Pack_Start gtkbox.h gtk_box_pack_start_defaults

   procedure Pack_End
     (In_Box  : in Box'Class;
      Child   : in Gtk.Widget.Widget'Class;
      Expand  : in Boolean := True;
      Fill    : in Boolean := True;
      Padding : in GInt    := 0);
   --  mapping: Pack_End gtkbox.h gtk_box_pack_end
   --  mapping: Pack_End gtkbox.h gtk_box_pack_end_defaults

   procedure Set_Homogeneous
     (In_Box      : in Box'Class;
      Homogeneous : in Boolean);
   --  mapping: Set_Homogeneous gtkbox.h gtk_box_set_homogeneous

   procedure Set_Spacing
     (In_Box  : in Box'Class;
      Spacing : in GInt);
   --  mapping: Set_Spacing gtkbox.h gtk_box_set_spacing

   procedure Reorder_Child
     (In_Box : in Box'Class;
      Child  : in Gtk.Widget.Widget'Class;
      Pos    : in GUint);
   --  mapping: Box_Reorder_Child gtkbox.h gtk_box_reorder_child

   procedure Query_Child_Packing
     (In_Box   : in Box'Class;
      Child    : in Gtk.Widget.Widget'Class;
      Expand   : out Boolean;
      Fill     : out Boolean;
      Padding  : out GInt;
      PackType : out Pack_Type);
   --  mapping: Query_Child_Packing gtkbox.h gtk_box_query_child_packing

   procedure Set_Child_Packing
     (In_Box    : in Box'Class;
      Child     : in Gtk.Widget.Widget'Class;
      Expand    : in Boolean;
      Fill      : in Boolean;
      Padding   : in GInt;
      PackType  : in Pack_Type);
   --  mapping: Set_Child_Packing gtkbox.h gtk_box_set_child_packing


   --  mapping: NOT_IMPLEMENTED gtkbox.h gtk_box_get_type

private

   type Box is new Gtk.Container.Container with null record;

end Gtk.Box;
