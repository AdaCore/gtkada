with Glib; use Glib;
with Gdk.Types; use Gdk.Types;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with Gtk.GRange; use Gtk.GRange;
with Gtk.Hbox; use Gtk.Hbox;
with Gtk.HRuler; use Gtk.HRuler;
with Gtk.Hscale; use Gtk.Hscale;
with Gtk.Hscrollbar; use Gtk.Hscrollbar;
with Gtk.Hseparator; use Gtk.Hseparator;
with Gtk.Label; use Gtk.Label;
with Gtk.Object; use Gtk.Object;
with Gtk.Ruler; use Gtk.Ruler;
with Gtk.Scale; use Gtk.Scale;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Table; use Gtk.Table;
with Gtk.Vbox; use Gtk.Vbox;
with Gtk.Vruler; use Gtk.VRuler;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;

with Ada.Text_IO;

package body Create_Rulers is

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);
   package Ruler_Cb is new Signal.Object_Callback (Gtk_Ruler);

   Window : Gtk.Window.Gtk_Window;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id        : Guint;
      Box1      : Gtk_Vbox;
      Box2      : Gtk_Hbox;
      Box3      : Gtk_Vbox;
      Label     : Gtk_Label;
      Frame     : Gtk_Frame;
      Button    : Gtk_Button;
      Separator : Gtk_HSeparator;
      Ruler     : GTk_HRuler;
      VRuler    : Gtk_VRuler;
      Table     : Gtk_Table;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Id := Widget_Cb.Connect (Window, "destroy", Destroy'Access, Window);
         Set_Title (Window, "Ruler");
         Border_Width (Window, Border_Width => 0);
         Set_Usize (Window, 300, 300);
         Set_Events (Window, Pointer_Motion_Mask + Pointer_Motion_Hint_Mask);

         Gtk_New (Table, 2, 2, False);
         Add (Window, Table);
         Show (Table);

         Gtk_New (Ruler);
         Set_Range (Ruler, 5.0, 15.0, 0.0, 20.0);
         Id := C_Unsafe_Connect (Window, "motion_notify_event",
                                 Get_Default_Motion_Notify_Event (Ruler),
                                 Ruler);
         Attach (Table, Ruler, 1, 2, 0, 1, Expand + Enums.Fill, Enums.Fill, 0, 0);
         Show (Ruler);

         Gtk_New (VRuler);
         Set_Range (VRuler, 5.0, 15.0, 0.0, 20.0);
         Id := C_Unsafe_Connect (Window, "motion_notify_event",
                                 Get_Default_Motion_Notify_Event (VRuler),
                                 VRuler);
         Attach (Table, VRuler, 0, 1, 1, 2, Enums.Fill, Expand + Enums.Fill, 0, 0);
         Show (VRuler);


      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Gtk.Widget.Show (Window);
      else
         Gtk.Widget.Destroy (Window);
      end if;

   end Run;

end Create_Rulers;

