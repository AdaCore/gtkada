with Gtk.Widget;

package Gdk.Window is

   type Gdk_Window is new Root_Type with private;

   function Get_Window (Widget : in Gtk.Widget.Gtk_Widget'Class)
                        return Gdk.Window.Gdk_Window'Class;

private

   type Gdk_Window is new Root_Type with null record;

end Gdk.Window;
