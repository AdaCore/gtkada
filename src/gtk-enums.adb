with Unchecked_Conversion;
with Interfaces.C.Strings;
with Gtk;

package body Gtk.Enums is

   -------------
   -- Convert --
   -------------

   function Convert (S : String) return System.Address is
      function Internal is new Unchecked_Conversion
        (Interfaces.C.Strings.chars_ptr, System.Address);
   begin
      return Internal (Interfaces.C.Strings.New_String (S));
   end Convert;

   function Convert (S : System.Address) return String is
      function Internal is new Unchecked_Conversion
        (System.Address, Interfaces.C.Strings.chars_ptr);
   begin
      return Interfaces.C.Strings.Value (Internal (S));
   end Convert;

   function Convert (W : Gtk.Widget.Gtk_Widget'Class) return System.Address is
   begin
      return Gtk.Get_Object (W);
   end Convert;

   function Convert (W : System.Address) return Gtk.Widget.Gtk_Widget'Class is
      Widget : Gtk.Widget.Gtk_Widget;
   begin
      Gtk.Set_Object (Widget, W);
      return Widget;
   end Convert;

   function Convert (I : Gint) return System.Address is
      function Internal is new Unchecked_Conversion
        (Gint, System.Address);
   begin
      return Internal (I);
   end Convert;

   function Convert (S : System.Address) return Gint is
      function Internal is new Unchecked_Conversion
        (System.Address, Gint);
   begin
      return Internal (S);
   end Convert;

end Gtk.Enums;
