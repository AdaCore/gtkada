with Glib; use Glib;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.GRange; use Gtk.GRange;
with Gtk.Hscale; use Gtk.Hscale;
with Gtk.Hscrollbar; use Gtk.Hscrollbar;
with Gtk.Hseparator; use Gtk.Hseparator;
with Gtk.Object; use Gtk.Object;
with Gtk.Scale; use Gtk.Scale;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Table; use Gtk.Table;
with Gtk.Vbox; use Gtk.Vbox;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;

package body Create_Range is

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);

   Window : Gtk.Window.Gtk_Window;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id         : Guint;
      Box1       : Gtk_Vbox;
      Box2       : Gtk_Vbox;
      Adjustment : Gtk_Adjustment;
      Scale      : Gtk_Hscale;
      Scrollbar  : Gtk_Hscrollbar;
      Separator  : Gtk_Hseparator;
      Button     : Gtk_Button;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Id := Widget_Cb.Connect (Window, "destroy", Destroy'Access, Window);
         Set_Title (Window, "range");
         Border_Width (Window, Border_Width => 0);

         Gtk_New (Box1, False, 0);
         Add (Window, Box1);
         Show (Box1);

         Gtk_New (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, True, True, 0);
         Show (Box2);

         Gtk_New (Adjustment, 0.0, 0.0, 101.0, 0.1, 1.0, 1.0);
         Gtk_New (Scale, Adjustment);
         Set_Usize (Scale, 150, 30);
         Set_Update_Policy (Scale, Update_Delayed);
         Set_Digits (Scale, 1);
         Set_Draw_Value (Scale, True);
         Pack_Start (Box2, Scale, True, True, 0);
         Show (Scale);

         Gtk_New (Scrollbar, Adjustment);
         Set_Update_Policy (Scrollbar, Update_Continuous);
         Pack_Start (Box2, Scrollbar, True, True, 0);
         Show (Scrollbar);

         Gtk_New (Separator);
         Pack_Start (Box1, Separator, False, True, 0);
         Show (Separator);

         Gtk_New (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, False, True, 0);
         Show (Box2);

         Gtk_New (Button, "close");
         Id := Widget_Cb.Connect (Button, "clicked", Destroy'Access, Window);
         Pack_Start (Box2, Button, True, True, 0);
         Set_Flags (Button, Can_Default);
         Grab_Default (Button);
         Show (Button);
      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Gtk.Widget.Show (Window);
      else
         Gtk.Widget.Destroy (Window);
      end if;

   end Run;

end Create_Range;

