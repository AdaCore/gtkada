
package body Gtk.Box is

   ----------------
   -- Pack_Start --
   ----------------

   procedure Pack_Start
     (In_Box  : in Gtk_Box'Class;
      Child   : in Gtk.Widget.Gtk_Widget'Class;
      Expand  : in Boolean := True;
      Fill    : in Boolean := True;
      Padding : in Gint    := 0)
   is
      procedure Internal (In_Box  : System.Address;
                          Child   : System.Address;
                          Expand  : Gint;
                          Fill    : Gint;
                          Padding : Gint);
      pragma Import (C, Internal, "gtk_box_pack_start");
   begin
      Internal (Get_Object (In_Box), Get_Object (Child),
                Boolean'Pos (Expand), Boolean'Pos (Fill), Padding);
   end Pack_Start;

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
     (In_Box  : in Gtk_Box'Class;
      Child   : in Gtk.Widget.Gtk_Widget'Class;
      Expand  : in Boolean := True;
      Fill    : in Boolean := True;
      Padding : in Gint    := 0)
   is
      procedure Internal (In_Box  : in System.Address;
                          Child   : in System.Address;
                          Expand  : in Gint;
                          Fill    : in Gint;
                          Padding : in Gint);
      pragma Import (C, Internal, "gtk_box_pack_end");
   begin
      Internal (Get_Object (In_Box), Get_Object (Child),
                Boolean'Pos (Expand), Boolean'Pos (Fill), Padding);
   end Pack_End;

   ---------------------
   -- Set_Homogeneous --
   ---------------------

   procedure Set_Homogeneous (In_Box      : in Gtk_Box'Class;
                              Homogeneous : in Boolean)
   is
      procedure Internal (In_Box      : in System.Address;
                          Homogeneous : in Gint);
      pragma Import (C, Internal, "gtk_box_set_homogeneous");
   begin
      Internal (Get_Object (In_Box), Boolean'Pos (Homogeneous));
   end Set_Homogeneous;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing (In_Box  : in Gtk_Box'Class;
                          Spacing : in Gint)
   is
      procedure Internal (In_Box  : in System.Address;
                          Spacing : in Gint);
      pragma Import (C, Internal, "gtk_box_set_spacing");
   begin
      Internal (Get_Object (In_Box), Spacing);
   end Set_Spacing;

   -------------------
   -- Reorder_Child --
   -------------------

   procedure Reorder_Child
     (In_Box : in Gtk_Box'Class;
      Child  : in Gtk.Widget.Gtk_Widget'Class;
      Pos    : in Guint)
   is
      procedure Internal (In_Box : in System.Address;
                          Child  : in System.Address;
                          Pos    : in Guint);
      pragma Import (C, Internal, "gtk_box_reorder_child");
   begin
      Internal (Get_Object (In_Box), Get_Object (Child), Pos);
   end Reorder_Child;

   -------------------------
   -- Query_Child_Packing --
   -------------------------

   procedure Query_Child_Packing
     (In_Box   : in  Gtk_Box'Class;
      Child    : in  Gtk.Widget.Gtk_Widget'Class;
      Expand   : out Boolean;
      Fill     : out Boolean;
      Padding  : out Gint;
      PackType : out Pack_Type)
   is
      procedure Internal (In_Box   : in System.Address;
                          Child    : in System.Address;
                          Expand   : out Gint;
                          Fill     : out Gint;
                          Padding  : out Gint;
                          PackType : out Gint);
      pragma Import (C, Internal, "gtk_box_query_child_packing");

      Expand_Ptr  : Gint;
      Fill_Ptr    : Gint;
      PackT_Ptr   : Gint;
   begin
      Internal (Get_Object (In_Box), Get_Object (Child),
                Expand_Ptr, Fill_Ptr, Padding, PackT_Ptr);
      Expand   := Expand_Ptr /= 0;
      Fill     := Fill_Ptr /= 0;
      PackType := Pack_Type'Val (PackT_Ptr);
   end Query_Child_Packing;

   -----------------------
   -- Set_Child_Packing --
   -----------------------

   procedure Set_Child_Packing
     (In_Box    : in Gtk_Box'Class;
      Child     : in Gtk.Widget.Gtk_Widget'Class;
      Expand    : in Boolean;
      Fill      : in Boolean;
      Padding   : in Gint;
      PackType  : in Pack_Type)
   is
      procedure Internal (In_Box   : in System.Address;
                          Child    : in System.Address;
                          Expand   : in Gint;
                          Fill     : in Gint;
                          Padding  : in Gint;
                          PackType : in Gint);
      pragma Import (C, Internal, "gtk_box_set_child_packing");
   begin
      Internal (Get_Object (In_Box), Get_Object (Child),
                Boolean'Pos (Expand), Boolean'Pos (Fill), Padding,
                Pack_Type'Pos (PackType));
   end Set_Child_Packing;


end Gtk.Box;
