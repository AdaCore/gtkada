

package body Gtk.Container is

   -------------
   -- Gtk_Add --
   -------------

   procedure Gtk_Add (Container : in Gtk_Container'Class;
                      Widget    : in Gtk.Widget.Gtk_Widget'Class) is
      procedure Internal (Container : System.Address;
                          Widget    : System.Address);
      pragma Import (C, Internal, "gtk_container_add");
   begin
      Internal (Get_Object (Container), Get_Object (Widget));
   end Gtk_Add;

   ----------------------
   -- Gtk_Border_Width --
   ----------------------

   procedure Gtk_Border_Width (Container    : in Gtk_Container'Class;
                               Border_Width : in GInt) is
      procedure Internal (Container     : System.Address;
                          Border_Widget : GInt);
      pragma Import (C, Internal, "gtk_container_border_width");
   begin
      Internal (Get_Object (Container), Border_Width);
   end Gtk_Border_Width;

end Gtk.Container;
