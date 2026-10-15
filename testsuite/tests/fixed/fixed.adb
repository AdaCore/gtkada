--  Check the double-coordinate ABI and child ownership of Gtk.Fixed.
with Ada.Command_Line;
with Glib;               use Glib;
with Glib.Test;          use Glib.Test;
with Gtk.Fixed;          use Gtk.Fixed;
with Gtk.Label;          use Gtk.Label;
with Gtk.Layout_Manager; use Gtk.Layout_Manager;
with Gtk.Main;
with Gtk.Widget;         use Gtk.Widget;

procedure Fixed is
   procedure Test_Position with Convention => C;

   procedure Test_Position is
      Container : Gtk_Fixed;
      Child     : Gtk_Label;
      X, Y      : Gdouble;
   begin
      Gtk_New (Container);
      Container.Ref_Sink;
      Gtk_New (Child, "positioned child");
      Child.Ref_Sink;

      Container.Put (Child, 10.5, 20.25);
      Assert_True (Child.Get_Parent = Gtk_Widget (Container));
      --  GTK queries the allocated origin, not the pending transform.
      Container.Get_Layout_Manager.Allocate (Container, 400, 300, -1);
      Container.Get_Child_Position (Child, X, Y);
      Assert_Cmpfloat_Eq (X, 10.5);
      Assert_Cmpfloat_Eq (Y, 20.25);

      Container.Move (Child, -3.5, 123.75);
      Container.Get_Layout_Manager.Allocate (Container, 400, 300, -1);
      Container.Get_Child_Position (Child, X, Y);
      Assert_Cmpfloat_Eq (X, -3.5);
      Assert_Cmpfloat_Eq (Y, 123.75);

      Container.Remove (Child);
      Assert_True (Child.Get_Parent = null);
      Child.Unref;
      Container.Unref;
   end Test_Position;
begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/fixed/position", Test_Position'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Fixed;
