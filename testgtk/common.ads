with Gtk; use Gtk;
with Gtk.Signal;
with Gtk.Label; use Gtk.Label;
with Gtk.Widget; use Gtk.Widget;

Package Common is

    --  This package is created to avoid the instanciation of the
    --  generic packages for callbacks. This provides a much smaller
    --  executable

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);
   package Widget2_Cb is new Signal.Callback (Gtk_Widget, Gtk_Widget_Access);
   package Label_Cb is new Signal.Object_Callback (Gtk_Label);
end Common;
