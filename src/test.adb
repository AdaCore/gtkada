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

   procedure Hello (Widget : in Gtk.Widget.Gtk_Widget'Class;
                    S      : access String7);
   procedure Destroy (Object : in Gtk.Widget.Gtk_Widget'Class);

   -----------
   -- Hello --
   -----------

   procedure Hello (Widget : in Gtk.Widget.Gtk_Widget'Class;
                    S      : access String7) is
   begin
      Ada.Text_IO.Put_Line ("Hello World  => String was=" & S.all);
   end Hello;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Object : in Gtk.Widget.Gtk_Widget'Class) is
   begin
      Ada.Text_IO.Put_Line ("Destroy");
      Gtk_Main_Quit;
   end Destroy;

   ----------
   -- Main --
   ----------

   procedure Main is

      package String_Cb is new Callback (String7);

      A_Window : aliased Gtk.Window.Gtk_Window;
      A_Button : Gtk.Button.Gtk_Button;
      A_Box    : Gtk.Hbox.Gtk_Hbox;
      Cb_Id    : GInt;
   begin
      Gtk_Init;
      --  Initialize the library (how can we pass the command line arguments ?)

      --  Create the window  (window = gtk_window_new (GTK_WINDOW_TOPLEVEL))
      Gtk_New (A_Window, Gtk_Window_Toplevel);
      Gtk_Set_Title (A_Window, "Hello buttons");
      Gtk_Border_Width (A_Window, 10);
      Cb_Id := Gtk_Connect (A_Window, "destroy", Destroy'Access);

      --  Create the box to store the buttons
      Gtk_New (A_Box, False, 0);
      Gtk_Add (A_Window, A_Box);

      --  Create the first button
      Gtk_New (A_Button, Label => "Button1");
      Cb_Id := String_Cb.Gtk_Connect (A_Button, "clicked",
                                      Hello'Access, "Button1");
      Gtk_Pack_Start (A_Box, A_Button, True, True, 0);
      Gtk_Show (A_Button);

      --  Create the second button
      Gtk_New (A_Button, Label => "Button2");
      Cb_Id := String_Cb.Gtk_Connect (A_Button, "clicked",
                                      Hello'Access, "Button2");
      Gtk_Pack_Start (A_Box, A_Button, True, True, 0);
      Gtk_Show (A_Button);

      --  Show the box
      Gtk_Show (A_Box);

      Gtk_Show (A_Window);

      Gtk_Main;
      --  Call the main loop   (gtk_main)
   end Main;

end Test;
