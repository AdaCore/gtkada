
with Glib; use Glib;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Button_Box; use Gtk.Button_Box;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Vbox; use Gtk.Vbox;
with Gtk.Vbutton_Box; use Gtk.Vbutton_Box;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;

package body Create_Button_Box is

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);
   package Void_Cb is new Signal.Void_Callback (Gtk_Button);

   Window : Gtk_Window;

   procedure Create_Bbox_Window (Horizontal : in Boolean;
                                 Title      : in String;
                                 Pos        : in Gint;
                                 Spacing    : in Gint;
                                 Child_W    : in Gint;
                                 Child_H    : in Gint;
                                 Layout     : in Gtk_Button_Box_Style)
   is
      Window : Gtk_Window;
      Id     : Guint;
      Box1   : Gtk_Vbox;
      Bbox   : Gtk_Button_Box;
      Button : Gtk_Button;
   begin
      Gtk_New (Window, Window_Toplevel);
      Set_Title (Window, Title);
      Id := Widget_Cb.Connect (Window, "destroy", Destroy'Access, Window);

      if Horizontal then
         Set_Usize (Window, 550, 60);
         Set_Uposition (Window, 150, Pos);
         Gtk_New (Box1, False, 0);
      else
         Set_Usize (Window, 150, 400);
         Set_Uposition (Window, Pos, 200);
         Gtk_New (Box1, False, 0);
      end if;

      Add (Window, Box1);
      Show (Box1);

      if Horizontal then
         declare
            Tmp : Gtk_Hbutton_Box;
         begin
            Gtk_New (Tmp);
            Bbox := Gtk_Button_Box (Tmp);
         end;
      else
         declare
            Tmp : Gtk_Vbutton_Box;
         begin
            Gtk_New (Tmp);
            Bbox := Gtk_Button_Box (Tmp);
         end;
      end if;

      Set_Layout (Bbox, Layout);
      Gtk.Button_Box.Set_Spacing (Bbox, Spacing);
      Set_Child_Size (Bbox, Child_W, Child_H);
      Show (Bbox);

      Border_Width (Box1, 25);
      Pack_Start (Box1, Bbox, True, True, 0);

      Gtk_New (Button, Label => "OK");
      Add (Bbox, Button);
      Id := Widget_Cb.Connect (Button, "clicked", Destroy'Access, Window);
      Show (Button);

      Gtk_New (Button, Label => "Cancel");
      Add (Bbox, Button);
      Show (Button);

      Gtk_New (Button, Label => "Help");
      Add (Bbox, Button);
      Show (Button);

      Show (Window);
   end Create_Bbox_Window;

   procedure Test_Hbbox (Widget : in out Gtk_Button'Class) is
      pragma Warnings (Off, Widget);
   begin
      Create_Bbox_Window (True, "Spread", 50,40, 85, 28, Spread);
      Create_Bbox_Window (True, "Edge", 200, 40, 85, 25, Edge);
      Create_Bbox_Window (True, "Start", 350, 40, 85, 25, Start);
      Create_Bbox_Window (True, "End", 500, 15, 30, 25, Style_End);
   end Test_Hbbox;

   procedure Test_Vbbox (Widget : in out Gtk_Button'Class) is
      pragma Warnings (Off, Widget);
   begin
      Create_Bbox_Window (False, "Spread", 50, 40, 85, 25, Spread);
      Create_Bbox_Window (False, "Edge", 250, 40, 85, 28, Edge);
      Create_Bbox_Window (False, "Start", 450, 40, 85, 25, Start);
      Create_Bbox_Window (False, "End", 650, 15, 30, 25, Style_end);
   end Test_Vbbox;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id     : Guint;
      Bbox   : Gtk_Hbutton_Box;
      Button : Gtk_Button;
   begin
      if not Is_Created (Window) then

         Gtk_New (Window, Window_Toplevel);
         Set_Title (Window, "Button Box Test");
         Id := Widget_Cb.Connect (Window, "destroy", Destroy'Access, Window);
         Border_Width (Window, Border_Width => 20);

         Gtk_New (Bbox);
         Add (Window, Bbox);
         Show (Bbox);

         Gtk_New (Button, Label => "Horizontal");
         Id := Void_Cb.Connect (Button, "clicked", Test_Hbbox'Access);
         Add (Bbox, Button);
         Show (Button);

         Gtk_New (Button, Label => "Vertical");
         Id := Void_Cb.Connect (Button, "clicked", Test_Vbbox'Access);
         Add (Bbox, Button);
         Show (Button);
      end if;

      if not Visible_Is_Set (Window) then
         Show (Window);
      else
         Gtk.Widget.Destroy (Window);
      end if;
   end Run;

end Create_Button_Box;

