
package body Gtk.Box is

   procedure Gtk_Pack_Start (Box     : in Gtk_Box'Class;
                             Widget  : in Gtk.Widget.Gtk_Widget'Class;
                             Expand  : in Boolean;
                             Fill    : in Boolean;
                             Padding : in GInt) is
      procedure Internal (Box     : System.Address;
                          Widget  : System.Address;
                          Expand  : GInt;
                          Fill    : GInt;
                          Padding : GInt);
      pragma Import (C, Internal, "gtk_box_pack_start");
   begin
      Internal (Get_Object (Box), Get_Object (Widget),
                Boolean'Pos (Expand), Boolean'Pos (Fill), Padding);
   end Gtk_Pack_Start;

end Gtk.Box;
