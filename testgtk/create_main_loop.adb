with Glib; use Glib;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Container; use Gtk.Container;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Label; use Gtk.Label;
with Gtk.Main; use Gtk.Main;
with Gtk.Misc; use Gtk.Misc;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Object; use Gtk.Object;
with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;

with Ada.Text_IO;

package body Create_Main_Loop is

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);

   Window : Gtk.Dialog.Gtk_Dialog;

   procedure Loop_Destroy (Window : in out Gtk_Widget'Class) is
   begin
      Gtk.Widget.Destroy (Window);
      Main_Quit;
   end Loop_Destroy;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id     : Guint;
      Label  : Gtk_Label;
      Button : Gtk_Button;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window);
         Id := Widget_Cb.Connect (Window, "destroy", Loop_Destroy'Access,
                                  Window);
         Set_Title (Window, "test_main_loop");
         Border_Width (Window, Border_Width => 0);

         Gtk_New (Label, "In recursive main loop...");
         Set_Padding (Label, 20, 20);
         Pack_Start (Get_Vbox (Window), Label, True, True, 0);
         Show (Label);

         Gtk_New (Button, "Leave");
         Pack_Start (Get_Action_Area (Window), Button, False, True, 0);
         Id := Widget_Cb.Connect (Button, "clicked", Loop_Destroy'Access, Window);
         Set_Flags (Button, Can_Default);
         Grab_Default (Button);
         Show (Button);
      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Gtk.Widget.Show (Window);
         Ada.Text_IO.Put_Line ("Create_Mainloop: start");
         Gtk.Main.Main;
         Ada.Text_IO.Put_Line ("Create_Mainloop: done");
      else
         Gtk.Widget.Destroy (Window);
      end if;
   end Run;

end Create_Main_Loop;

