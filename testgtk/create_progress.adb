with Glib; use Glib;
with Gdk.Types; use Gdk.Types;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Container; use Gtk.Container;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Label; use Gtk.Label;
with Gtk.Main; use Gtk.Main;
with Gtk.Misc; use Gtk.Misc;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Object; use Gtk.Object;
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Gtk.Tooltips; use Gtk.Tooltips;
with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;

package body Create_Progress is

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);
   package Time_Cb   is new Gtk.Main.Timeout (Gtk_Progress_Bar);

   Window     : Gtk.Dialog.Gtk_Dialog;
   Timeout_Id : Guint := 0;

   procedure Destroy_Progress (Window : in out Gtk_Widget'Class) is
   begin
      Timeout_Remove (Timeout_Id);
      Timeout_Id := 0;
      Gtk.Widget.Destroy (Window);
   end Destroy_Progress;

   function Progress_Timeout (Pbar : in Gtk_Progress_Bar) return Boolean is
      New_Val : Gfloat;
   begin
      New_Val := Get_Percentage (Pbar);
      if New_Val >= 1.0 then
         New_Val := 0.0;
      end if;
      New_Val := New_Val + 0.02;
      Update (Pbar, New_Val);
      return True;
   end Progress_Timeout;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id       : Guint;
      Tooltips : Gtk_Tooltips;
      Vbox     : Gtk_Box;
      Pbar     : Gtk_Progress_Bar;
      Button   : Gtk_Button;
      Label    : Gtk_Label;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window);
         Id := Widget_Cb.Connect (Window, "destroy", Destroy_Progress'Access,
                                  Window);
         Set_Title (Window, "progress bar");
         Border_Width (Window, Border_Width => 0);

         Gtk_New (Tooltips);

         Gtk_New_Vbox (Vbox, False, 5);
         Border_Width (Vbox, 10);
         Pack_Start (Get_Vbox (Window), Vbox, True, True, 0);
         Show (Vbox);

         Gtk_New (Label, "progress...");
         Set_Alignment (Label, 0.0, 0.5);
         Pack_Start (Vbox, Label, False, True, 0);
         Show (Label);

         Gtk_New (Pbar);
         Set_Events (Pbar, Enter_Notify_Mask + Leave_Notify_Mask);
         Set_Usize (Pbar, 200, 20);
         Pack_Start (Vbox, Pbar, True, True, 0);
         Show (Pbar);

         Set_Tip (Tooltips, Pbar, "Countdown is progressing yet!", "Secret");
         Set_Delay (Tooltips, 0);

         Timeout_Id := Time_Cb.Add (100, Progress_Timeout'Access, Pbar);

         Gtk_New (Button, "close");
         Id := Widget_Cb.Connect (Button, "clicked", Destroy_Progress'Access,
                                  Window);
         Set_Flags (Button, Can_Default);
         Pack_Start (Get_Action_Area (Window), Button, True, True, 0);
         Grab_Default (Button);
         Show (Button);
      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Gtk.Widget.Show (Window);
      else
         Gtk.Widget.Destroy (Window);
      end if;

   end Run;

end Create_Progress;

