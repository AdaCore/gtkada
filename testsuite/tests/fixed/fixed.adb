--  Check Gtk.Fixed positioning, child ownership, and inherited accessibility.
--  GTK's C testsuite has no dedicated GtkFixed test to port, so these cases
--  exercise the documented GtkFixed API directly.
with Ada.Command_Line;
with Glib;               use Glib;
with Glib.Test;          use Glib.Test;
with Gtk.Accessible;     use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Fixed;          use Gtk.Fixed;
with Gtk.Label;          use Gtk.Label;
with Gtk.Layout_Manager; use Gtk.Layout_Manager;
with Gtk.Main;
with Gtk.Widget;         use Gtk.Widget;

procedure Fixed is
   procedure Test_Position with Convention => C;
   procedure Test_Accessibility with Convention => C;

   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Atcontext.Gtk_Atcontext;

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

   --  An unrooted fixed container exposes its accessible child and role, but
   --  has neither a display bounds rectangle nor an AT context yet.
   procedure Test_Accessibility is
      Container : constant Gtk_Fixed := Gtk_Fixed_New;
      Child     : constant Gtk_Label := Gtk_Label_New ("accessible child");
      Context   : Gtk.Atcontext.Gtk_Atcontext;
      Parent    : Gtk_Accessible;
      Next      : Gtk_Accessible;
      X, Y, Width, Height : Gint;
      Has_Bounds : Boolean;
   begin
      Container.Ref_Sink;
      Child.Ref_Sink;
      Container.Put (Child, 0.0, 0.0);

      Assert_True (Container.Get_Accessible_Role = Accessible_Role_Generic);
      Assert_Cmpstr_Eq (Container.Get_Accessible_Id, "");
      Assert_True
        (Container.Get_First_Accessible_Child /= Null_Gtk_Accessible);

      Context := Container.Get_At_Context;
      Assert_True (Context = null);
      Has_Bounds := Container.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);

      --  A detached, unrooted container has no accessible parent or sibling;
      --  its focused platform state is false until GTK reports otherwise.
      Parent := Container.Get_Accessible_Parent;
      Next := Container.Get_Next_Accessible_Sibling;
      Assert_True (Parent = Null_Gtk_Accessible);
      Assert_True (Next = Null_Gtk_Accessible);
      Assert_False
        (Container.Get_Platform_State (Accessible_Platform_State_Focused));

      --  Notifications and reset requests are valid without an assistive
      --  technology. Null links leave this detached widget's tree unchanged.
      Container.Announce
        ("fixed accessibility test", Accessible_Announcement_Priority_Low);
      Container.Reset_Property (Accessible_Property_Label);
      Container.Reset_Relation (Accessible_Relation_Labelled_By);
      Container.Reset_State (Accessible_State_Busy);
      Container.Update_Platform_State (Accessible_Platform_State_Focusable);
      Container.Set_Accessible_Parent
        (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True
        (Container.Get_Accessible_Parent = Null_Gtk_Accessible);
      Container.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True
        (Container.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  Reset and notification calls do not change the container's role.
      Assert_True (Container.Get_Accessible_Role = Accessible_Role_Generic);
      Container.Unref;
      Child.Unref;
   end Test_Accessibility;
begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/fixed/position", Test_Position'Unrestricted_Access);
   Add_Func
     ("/fixed/accessibility", Test_Accessibility'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Fixed;
