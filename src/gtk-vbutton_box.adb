
with Gtk.Enums; use Gtk.Enums;

package body Gtk.VButton_Box is

   ------------------------
   -- Get_Layout_Default --
   ------------------------

   function Get_Layout_Default return Gtk_Button_Box_Style
   is
      function Internal return Gint;
      pragma Import (C, Internal, "gtk_vbutton_box_get_layout_default");
   begin
      return Gtk_Button_Box_Style'Val (Internal);
   end Get_Layout_Default;

   -------------------------
   -- Get_Spacing_Default --
   -------------------------

   function Get_Spacing_Default return Gint
   is
      function Internal return Gint;
      pragma Import (C, Internal, "gtk_vbutton_box_get_spacing_default");
   begin
      return Internal;
   end Get_Spacing_Default;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_VButton_Box)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_vbutton_box_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ------------------------
   -- Set_Layout_Default --
   ------------------------

   procedure Set_Layout_Default (Layout : in Gtk_Button_Box_Style)
   is
      procedure Internal (Layout : in Gint);
      pragma Import (C, Internal, "gtk_vbutton_box_set_layout_default");
   begin
      Internal (Gtk_Button_Box_Style'Pos (Layout));
   end Set_Layout_Default;

   -------------------------
   -- Set_Spacing_Default --
   -------------------------

   procedure Set_Spacing_Default (Spacing : in Gint)
   is
      procedure Internal (Spacing : in Gint);
      pragma Import (C, Internal, "gtk_vbutton_box_set_spacing_default");
   begin
      Internal (Spacing);
   end Set_Spacing_Default;

end Gtk.VButton_Box;
