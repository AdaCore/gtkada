
package body Gtk.Box is

   ----------------
   -- Pack_Start --
   ----------------

   procedure Pack_Start
     (In_Box  : in Box'Class;
      Child   : in Gtk.Widget.Widget'Class;
      Expand  : in Boolean := True;
      Fill    : in Boolean := True;
      Padding : in GInt    := 0)
   is
      procedure Internal (In_Box  : System.Address;
                          Child   : System.Address;
                          Expand  : GInt;
                          Fill    : GInt;
                          Padding : GInt);
      pragma Import (C, Internal, "gtk_box_pack_start");
   begin
      Internal (Get_Object (In_Box), Get_Object (Child),
                Boolean'Pos (Expand), Boolean'Pos (Fill), Padding);
   end Pack_Start;

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
     (In_Box  : in Box'Class;
      Child   : in Gtk.Widget.Widget'Class;
      Expand  : in Boolean := True;
      Fill    : in Boolean := True;
      Padding : in GInt    := 0)
   is
      procedure Internal (In_Box  : in System.Address;
                          Child   : in System.Address;
                          Expand  : in GInt;
                          Fill    : in GInt;
                          Padding : in GInt);
      pragma Import (C, Internal, "gtk_box_pack_end");
   begin
      Internal (Get_Object (In_Box), Get_Object (Child),
                Boolean'Pos (Expand), Boolean'Pos (Fill), Padding);
   end Pack_End;

   ---------------------
   -- Set_Homogeneous --
   ---------------------

   procedure Set_Homogeneous (In_Box      : in Box'Class;
                              Homogeneous : in Boolean)
   is
      procedure Internal (In_Box      : in System.Address;
                          Homogeneous : in GInt);
      pragma Import (C, Internal, "gtk_box_set_homogeneous");
   begin
      Internal (Get_Object (In_Box), Boolean'Pos (Homogeneous));
   end Set_Homogeneous;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing (In_Box  : in Box'Class;
                          Spacing : in GInt)
   is
      procedure Internal (In_Box  : in System.Address;
                          Spacing : in GInt);
      pragma Import (C, Internal, "gtk_box_set_spacing");
   begin
      Internal (Get_Object (In_Box), Spacing);
   end Set_Spacing;

   -------------------
   -- Reorder_Child --
   -------------------

   procedure Reorder_Child
     (In_Box : in Box'Class;
      Child  : in Gtk.Widget.Widget'Class;
      Pos    : in GUint)
   is
      procedure Internal (In_Box : in System.Address;
                          Child  : in System.Address;
                          Pos    : in GUint);
      pragma Import (C, Internal, "gtk_box_reorder_child");
   begin
      Internal (Get_Object (In_Box), Get_Object (Child), Pos);
   end Reorder_Child;

   -------------------------
   -- Query_Child_Packing --
   -------------------------

   procedure Query_Child_Packing
     (In_Box   : in Box'Class;
      Child    : in Gtk.Widget.Widget'Class;
      Expand   : out Boolean;
      Fill     : out Boolean;
      Padding  : out GInt;
      PackType : out Pack_Type)
   is
      procedure Internal (In_Box   : in System.Address;
                          Child    : in System.Address;
                          Expand   : out GInt;
                          Fill     : out GInt;
                          Padding  : out GInt;
                          PackType : out GInt);
      pragma Import (C, Internal, "gtk_box_query_child_packing");

      Expand_Ptr  : GInt;
      Fill_Ptr    : GInt;
      PackT_Ptr   : GInt;
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
     (In_Box    : in Box'Class;
      Child     : in Gtk.Widget.Widget'Class;
      Expand    : in Boolean;
      Fill      : in Boolean;
      Padding   : in GInt;
      PackType  : in Pack_Type)
   is
      procedure Internal (In_Box   : in System.Address;
                          Child    : in System.Address;
                          Expand   : in GInt;
                          Fill     : in GInt;
                          Padding  : in GInt;
                          PackType : in GInt);
      pragma Import (C, Internal, "gtk_box_set_child_packing");
   begin
      Internal (Get_Object (In_Box), Get_Object (Child),
                Boolean'Pos (Expand), Boolean'Pos (Fill), Padding,
                Pack_Type'Pos (PackType));
   end Set_Child_Packing;


end Gtk.Box;
