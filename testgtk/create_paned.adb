with Glib; use Glib;
with Gtk.Button; use Gtk.Button;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with Gtk.HPaned; use Gtk.HPaned;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Paned; use Gtk.Paned;
with Gtk.VPaned; use Gtk.VPaned;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;

package body Create_Paned is

   package Button_Cb is new Signal.Object_Callback (Gtk_Button);
   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);

   Window : Gtk.Window.Gtk_Window;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      VPaned : Gtk_VPaned;
      HPaned : Gtk_HPaned;
      Frame  : Gtk_Frame;
      Button : Gtk_Button;
      Id     : Guint;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Id := Widget_Cb.Connect (Window, "destroy", Destroy'Access, Window);
         Set_Title (Window, "Panes");
         Border_Width (Window, Border_Width => 0);

         Gtk_New (VPaned);
         Add (Window, VPaned);
         Border_Width (VPaned, 5);
         Show (VPaned);

         Gtk_New (HPaned);
         Add1 (Vpaned, HPaned);

         Gtk_New (Frame);
         Set_Shadow_Type (Frame, Shadow_In);
         Set_Usize (Frame, 60, 60);
         Add1 (HPaned, Frame);
         Show (Frame);

         Gtk_New (Button, "Hi There");
         Add (Frame, Button);
         Show (Button);

         Gtk_New (Frame);
         Set_Shadow_Type (Frame, Shadow_In);
         Set_Usize (Frame, 80, 60);
         Add2 (HPaned, Frame);
         Show (Frame);

         Show (HPaned);

         Gtk_New (Frame);
         Set_Shadow_Type (Frame, Shadow_In);
         Set_Usize (Frame, 60, 80);
         Add2 (VPaned, Frame);
         Show (Frame);
      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Gtk.Widget.Show (Window);
      else
         Gtk.Widget.Destroy (Window);
      end if;

   end Run;

end Create_Paned;
