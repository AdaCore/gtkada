

package body Gtk.Container is

   -----------
   --  Add  --
   -----------

   procedure Add (In_Container : in Gtk_Container'Class;
                  Widget       : in Gtk.Widget.Gtk_Widget'Class) is
      procedure Internal (In_Container : System.Address;
                          Widget       : System.Address);
      pragma Import (C, Internal, "gtk_container_add");
   begin
      Internal (Get_Object (In_Container), Get_Object (Widget));
   end Add;


   --------------------
   --  Border_Width  --
   --------------------

   procedure Border_Width (Of_Container : in Gtk_Container'Class;
                           Border_Width : in Gint) is
      procedure Internal (Of_Container  : System.Address;
                          Border_Widget : Gint);
      pragma Import (C, Internal, "gtk_container_border_width");
   begin
      Internal (Get_Object (Of_Container), Border_Width);
   end Border_Width;

end Gtk.Container;
