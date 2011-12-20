------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Hello_Package; use Hello_Package;
with Gtk.Main;
with Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk.Button; use Gtk.Button;

procedure Hello is
   Window : Gtk_Window;
   Button : Gtk_Button;

begin
   --  This is called in all GtkAda applications. Arguments are parsed
   --  from the command line and are returned to the application.
   Gtk.Main.Init;

   --  Creates a new window
   Gtk.Window.Gtk_New (Window);

   --  When the window is given the "delete_event" signal (this is given
   --  by the window manager, usually by the "close" option, or on the
   --  titlebar), we ask it to call the Delete_Event function.
   Return_Handlers.Connect
     (Window, "delete_event",
      Return_Handlers.To_Marshaller (Delete_Event'Access));

   --  Here we connect the "destroy" event to a signal handler.
   --  This event occurs when we call Gtk.Widget.Destroy on the window,
   --  or if we return False in the "delete_event" callback.
   Handlers.Connect
     (Window, "destroy", Handlers.To_Marshaller (Destroy'Access));

   --  Sets the border width of the window.
   Gtk.Window.Set_Border_Width (Window, 10);

   --  Creates a new button with the label "Hello World".
   Gtk_New (Button, "Hello World");

   --  When the button receives the "clicked" signal, it will call the
   --  procedure Hello_Callback.
   Handlers.Connect
     (Button, "clicked", Handlers.To_Marshaller (Hello_Callback'Access));

   --  This will cause the window to be destroyed by calling
   --  Gtk.Widget.Destroy (Window) when "clicked".  Again, the destroy
   --  signal could come from here, or the window manager.
   Handlers.Object_Connect
     (Button,
      "clicked",
      Handlers.To_Marshaller (Gtk.Widget.Destroy_Cb'Access),
      Window);

   --  This packs the button into the window (a Gtk_Container).
   Gtk.Window.Add (Window, Button);

   --  The final step is to display this newly created widget.
   Show (Button);

   --  and the window
   Show (Window);

   --  All GtkAda applications must have a Main. Control ends here
   --  and waits for an event to occur (like a key press or
   --  mouse event).
   Gtk.Main.Main;
end Hello;
