
with Gtk.Container;
with Gtk.Widget;

package Gtk.Box is

   type Gtk_Box is new Gtk.Container.Gtk_Container with private;

   type Pack_Type is (Pack_Start, Pack_End);
   --  mapping: Pack_Type gtkenums.h GtkPackType

   function Get_Child
     (Box : in Gtk_Box;
      Num : in Gint)
      return   Gtk.Widget.Gtk_Widget;

   procedure Gtk_New_Vbox (Widget      : out Gtk_Box;
                           Homogeneous : in  Boolean;
                           Spacing     : in  Gint);
   --  mapping: Gtk_New_Vbox gtkvbox.h gtk_vbox_new

   procedure Gtk_New_Hbox (Widget      : out Gtk_Box;
                           Homogeneous : in  Boolean;
                           Spacing     : in  Gint);
   --  mapping: Gtk_New_Hbox gtkhbox.h gtk_hbox_new

   procedure Pack_Start
     (In_Box  : in Gtk_Box'Class;
      Child   : in Gtk.Widget.Gtk_Widget'Class;
      Expand  : in Boolean := True;
      Fill    : in Boolean := True;
      Padding : in Gint    := 0);
   --  mapping: Pack_Start gtkbox.h gtk_box_pack_start
   --  mapping: Pack_Start gtkbox.h gtk_box_pack_start_defaults

   procedure Pack_End
     (In_Box  : in Gtk_Box'Class;
      Child   : in Gtk.Widget.Gtk_Widget'Class;
      Expand  : in Boolean := True;
      Fill    : in Boolean := True;
      Padding : in Gint    := 0);
   --  mapping: Pack_End gtkbox.h gtk_box_pack_end
   --  mapping: Pack_End gtkbox.h gtk_box_pack_end_defaults

   procedure Set_Homogeneous
     (In_Box      : in Gtk_Box'Class;
      Homogeneous : in Boolean);
   --  mapping: Set_Homogeneous gtkbox.h gtk_box_set_homogeneous

   procedure Set_Spacing
     (In_Box  : in Gtk_Box'Class;
      Spacing : in Gint);
   --  mapping: Set_Spacing gtkbox.h gtk_box_set_spacing

   procedure Reorder_Child
     (In_Box : in Gtk_Box'Class;
      Child  : in Gtk.Widget.Gtk_Widget'Class;
      Pos    : in Guint);
   --  mapping: Box_Reorder_Child gtkbox.h gtk_box_reorder_child

   procedure Query_Child_Packing
     (In_Box   : in Gtk_Box'Class;
      Child    : in Gtk.Widget.Gtk_Widget'Class;
      Expand   : out Boolean;
      Fill     : out Boolean;
      Padding  : out Gint;
      PackType : out Pack_Type);
   --  mapping: Query_Child_Packing gtkbox.h gtk_box_query_child_packing

   procedure Set_Child_Packing
     (In_Box    : in Gtk_Box'Class;
      Child     : in Gtk.Widget.Gtk_Widget'Class;
      Expand    : in Boolean;
      Fill      : in Boolean;
      Padding   : in Gint;
      PackType  : in Pack_Type);
   --  mapping: Set_Child_Packing gtkbox.h gtk_box_set_child_packing


   --  mapping: NOT_IMPLEMENTED gtkbox.h gtk_box_get_type

private

   type Gtk_Box is new Gtk.Container.Gtk_Container with null record;

end Gtk.Box;
