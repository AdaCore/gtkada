with Gtk;

package body Gdk.Window is

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window (Widget : in Gtk.Widget.Gtk_Widget'Class)
                        return       Gdk.Window.Gdk_Window'Class
   is
      function Internal (Widget : System.Address)
                        return    System.Address;
      pragma Import (C, Internal, "ada_widget_get_window");
      Window : Gdk.Window.Gdk_Window;
   begin
      Set_Object (Window, Internal (Gtk.Get_Object (Widget)));
      return Window;
   end Get_Window;

end Gdk.Window;
