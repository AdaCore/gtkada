with Glib; use Glib;
with Gtk.Button; use Gtk.Button;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Main; use Gtk.Main;
with Gtk.Preview; use Gtk.Preview;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;

package body Create_Preview_Color is

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);
   package Preview_Idle is new Gtk.Main.Idle_Func (Gtk_Preview);

   Window : Gtk.Window.Gtk_Window;

   Color_Idle : Guint  := 0;
   Count      : Guchar := 1;

   function Color_Idle_Func (Preview : Gtk_Preview) return Boolean is
      Buf : Guchar_Array (0 .. 767);
      K   : Natural;
   begin
      for I in 0 .. Guchar'(255) loop
         K := 0;
         for J in 0 .. Guchar'(255) loop
            Buf (K + 0) := I + Count;
            Buf (K + 1) := 0;
            Buf (K + 2) := J + Count;
            K := K + 3;
         end loop;
         Draw_Row (Preview, Buf, 0, Gint (I), 256);
      end loop;
      Count := Count + 1;
      Draw (Preview);
      return True;
   end Color_Idle_Func;

   procedure Preview_Destroy (Widget : in out Gtk_Widget'Class) is
   begin
      Idle_Remove (Color_Idle);
      Color_Idle := 0;
      Gtk.Widget.Destroy (Widget);
   end Preview_Destroy;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id      : Guint;
      Preview : Gtk_Preview;
      Buf     : Guchar_Array (0 .. 767);
      K       : Natural;
   begin
      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Id := Widget_Cb.Connect (Window, "destroy", Preview_Destroy'Access,
                                  Window);
         Set_Title (Window, "test");
         Border_Width (Window, Border_Width => 10);

         Gtk_New (Preview, Preview_Color);
         Size (Preview, 256, 256);
         Add (Window, Preview);
         Show (Preview);

         for I in 0 .. Guchar'(255) loop
            K := 0;
            for J in 0 .. Guchar'(255) loop
               Buf (K + 0) := I;
               Buf (K + 1) := 0;
               Buf (K + 2) := J;
               K := K + 3;
            end loop;
            Draw_Row (Preview, Buf, 0, Gint (I), 256);
         end loop;

         Color_Idle := Preview_Idle.Add (Color_Idle_Func'Access, Preview);
      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Gtk.Widget.Show (Window);
      else
         Gtk.Widget.Destroy (Window);
      end if;

   end Run;

end Create_Preview_Color;

