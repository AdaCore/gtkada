

package body Gtk.Container is

   -------------
   -- Add --
   -------------

   procedure Add (In_Container : in Container'Class;
                  Widget       : in Gtk.Widget.Widget'Class) is
      procedure Internal (In_Container : System.Address;
                          Widget       : System.Address);
      pragma Import (C, Internal, "gtk_container_add");
   begin
      Internal (Get_Object (In_Container), Get_Object (Widget));
   end Add;

   ----------------------
   -- Border_Width --
   ----------------------

   procedure Border_Width (Of_Container : in Container'Class;
                           Border_Width : in GInt) is
      procedure Internal (Of_Container  : System.Address;
                          Border_Widget : GInt);
      pragma Import (C, Internal, "gtk_container_border_width");
   begin
      Internal (Get_Object (Of_Container), Border_Width);
   end Border_Width;

end Gtk.Container;
