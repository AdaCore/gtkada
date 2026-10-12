--  Code samples quoted by the "Signal handling" chapter of the User's Guide
--  (docs/gtkada_ug/signals.rst). The text between the START and END markers
--  is included verbatim in the guide.

with Glib.Object; use Glib.Object;
with Gtk.Button;  use Gtk.Button;
with Gtk.Window;  use Gtk.Window;

package UG_Signals is

   Clicks_On_Button : Natural := 0;
   Clicks_On_Window : Natural := 0;
   --  Incremented by the handlers, so that the test can check that they ran

   procedure Connect_Handler (Button : Gtk_Button);
   --  Connect a handler that receives the button itself

   procedure Connect_Slot_Handler
     (Button : Gtk_Button; Main_Window : Gtk_Window);
   --  Connect a handler that receives Main_Window instead of the button

end UG_Signals;
