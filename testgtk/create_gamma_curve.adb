with Glib; use Glib;
with Gtk.Button; use Gtk.Button;
with Gtk.Container; use Gtk.Container;
with Gtk.Curve; use Gtk.Curve;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Gamma_Curve; use Gtk.Gamma_Curve;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;

package body Create_Gamma_Curve is

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);
   package Float_P is new Ada.Numerics.Generic_Elementary_Functions (Gfloat);

   Window : Gtk_Window;
   Count  : Gint := 0;
   Curve : Gtk_Gamma_Curve;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id    : Guint;
      Max   : Gint := 127 + (Count mod 4) * 128;
      Vec   : Curve_Vector (Positive (Max));
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, WIndow_Toplevel);
         Set_Title (Window, "test");
         Border_Width (Window, 10);
         Id := Widget_Cb.Connect (Window, "destroy", Destroy'Access, Window);

         Gtk_New (Curve);
         Add (Window, Curve);
         Show (Curve);
      end if;

      if (Count mod 4 /= 3) then
         Ada.Text_IO.Put_Line ("Redrawing the window with "
                               & Gint'Image (Max)
                               & " points");
      end if;
      Set_Range (Get_Curve (Curve), 0.0, Gfloat (Max), 0.0, Gfloat (Max));
      for J in Vec.Vector'Range loop
         Vec.Vector (J) := (127.0 / Float_P.Sqrt (Gfloat (Max)))
           * Float_P.Sqrt (Gfloat (J));
      end loop;
      Set_Vector (Get_Curve (Curve), Vec);


      if not Visible_Is_Set (Window) then
         Gtk.Widget.Show (Window);
      elsif (Count mod 4 = 3) then
         Gtk.Widget.Destroy (Window);
      end if;

      Count := Count + 1;
   end Run;

end Create_Gamma_Curve;

