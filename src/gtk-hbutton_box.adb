package body Gtk.HButton_Box is

   -------------------------
   -- Get_Spacing_Default --
   -------------------------

   function Get_Spacing_Default return GInt is
      function Internal return GInt;
      pragma Import (C, Internal, "gtk_hbutton_box_get_spacing_default");
   begin
      return Internal;
   end Get_Spacing_Default;

   ------------------------
   -- Get_Layout_Default --
   ------------------------

   function Get_Layout_Default return Gtk.Box.Button_Box.Style is
      function Internal return GInt;
      pragma Import (C, Internal, "gtk_hbutton_box_get_layout_default");
   begin
      return Gtk.Box.Button_Box.Style'Val (Internal);
   end Get_Layout_Default;

   -------------------------
   -- Set_Spacing_Default --
   -------------------------

   procedure Set_Spacing_Default (Spacing : in GInt) is
      procedure Internal (Spacing : in GInt);
      pragma Import (C, Internal, "gtk_hbutton_box_set_spacing_default");
   begin
      Internal (Spacing);
   end Set_Spacing_Default;

   ------------------------
   -- Set_Layout_Default --
   ------------------------

   procedure Set_Layout_Default (Layout : in Gtk.Box.Button_Box.Style) is
      procedure Internal (Layout : in GInt);
      pragma Import (C, Internal, "gtk_hbutton_box_set_layout_default");
   begin
      Internal (Gtk.Box.Button_Box.Style'Pos (Layout));
   end Set_Layout_Default;

end Gtk.HButton_Box;

