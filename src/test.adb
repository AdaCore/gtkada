with Gtk; use Gtk;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk.Container; use Gtk.Container;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Button; use Gtk.Button;
with Gtk.Box;    use Gtk.Box;
with Gtk.Hbox;   use Gtk.Hbox;
with Ada.Text_IO;

package body Test is

   subtype String7 is String (1 .. 7);

   procedure Hello (Widget : in Gtk.Widget.Widget'Class;
                    S      : access String7);
   procedure Destroy (Object : in Gtk.Widget.Widget'Class);

   -----------
   -- Hello --
   -----------

   procedure Hello (Widget : in Gtk.Widget.Widget'Class;
                    S      : access String7) is
   begin
      Ada.Text_IO.Put_Line ("Hello World  => String was=" & S.all);
   end Hello;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Object : in Gtk.Widget.Widget'Class) is
   begin
      Ada.Text_IO.Put_Line ("Destroy");
      Main_Quit;
   end Destroy;

   ----------
   -- Main --
   ----------

   procedure Main is

      package String_Cb is new Callback (String7);

      A_Window : aliased Gtk.Window.Window;
      A_Button : Gtk.Button.Button;
      A_Box    : Gtk.Hbox.Hbox;
      Cb_Id    : GInt;
   begin
      Init;
      --  Initialize the library (how can we pass the command line arguments ?)

      --  Create the window  (window = gtk_window_new (GTK_WINDOW_TOPLEVEL))
      Gtk_New (A_Window, Window_Toplevel);
      Set_Title (A_Window, "Hello buttons");
      Border_Width (A_Window, 10);
      Cb_Id := Connect (A_Window, "destroy", Destroy'Access);

      --  Create the box to store the buttons
      Gtk_New (A_Box, False, 0);
      Add (A_Window, A_Box);

      --  Create the first button
      Gtk_New (A_Button, Label => "Button1");
      Cb_Id := String_Cb.Connect (A_Button, "clicked",
                                  Hello'Access, "Button1");
      Pack_Start (A_Box, A_Button, True, True, 0);
      Show (A_Button);

      --  Create the second button
      Gtk_New (A_Button, Label => "Button2");
      Cb_Id := String_Cb.Connect (A_Button, "clicked",
                                  Hello'Access, "Button2");
      Pack_Start (A_Box, A_Button, True, True, 0);
      Show (A_Button);

      --  Show the box
      Show (A_Box);

      Show (A_Window);

      Gtk.Main;
      --  Call the main loop   (gtk_main)
   end Main;

end Test;
