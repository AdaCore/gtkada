
package body Gtk.Drawing_Area is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Drawing_Area) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_drawing_area_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ----------
   -- Size --
   ----------

   procedure Size
     (Darea  : in Gtk_Drawing_Area'Class;
      Width  : in Gint;
      Height : in Gint)
   is
      procedure Internal (Darea  : System.Address;
                          Width  : Gint;
                          Height : Gint);
      pragma Import (C, Internal, "gtk_drawing_area_size");
   begin
      Internal (Get_Object (Darea), Width, Height);
   end Size;

end Gtk.Drawing_Area;
