
package body Gtk.Box is

   procedure Pack_Start (In_Box  : in Box'Class;
                         Widget  : in Gtk.Widget.Widget'Class;
                         Expand  : in Boolean;
                         Fill    : in Boolean;
                         Padding : in GInt) is
      procedure Internal (In_Box  : System.Address;
                          Widget  : System.Address;
                          Expand  : GInt;
                          Fill    : GInt;
                          Padding : GInt);
      pragma Import (C, Internal, "gtk_box_pack_start");
   begin
      Internal (Get_Object (In_Box), Get_Object (Widget),
                Boolean'Pos (Expand), Boolean'Pos (Fill), Padding);
   end Pack_Start;

end Gtk.Box;
