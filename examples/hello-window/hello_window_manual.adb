with Glib.Main;  use Glib.Main;
with Gtk.Main;
with Gtk.Window; use Gtk.Window;

procedure Hello_Window_Manual is
   Window     : Gtk_Window;
   Running    : Boolean := True;
   pragma Volatile (Running);  --  Modified by the callback during iteration.
   Dispatched : Boolean;
   pragma Unreferenced (Dispatched);

   function Close_Request
     (Self : access Gtk_Window_Record'Class) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      Running := False;
      return False;  --  Let GTK's default handler destroy the window.
   end Close_Request;

begin
   --  Without GtkApplication, initialize GTK before creating any widgets.
   Gtk.Main.Init;
   Gtk_New (Window);
   Window.Set_Title ("Hello, world! (manual loop)");
   Window.Set_Default_Size (320, 200);
   Window.On_Close_Request (Close_Request'Unrestricted_Access);
   Window.Present;

   while Running loop
      --  The default context handles GTK events. Block while there is no work.
      Dispatched := Main_Context_Iteration (May_Block => True);
   end loop;
end Hello_Window_Manual;
