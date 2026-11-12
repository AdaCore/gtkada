--  Unit test for Gtk.Aspect_Frame.
--
--  GTK's own testsuite has no aspectframe.c to port, so this checks the
--  constructor, the accessors, the property mirrors and the child.

with Ada.Command_Line;
with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Glib.Properties;
with Glib.Test;        use Glib.Test;
with Gtk.Accessible;   use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Aspect_Frame; use Gtk.Aspect_Frame;
with Gtk.Label;        use Gtk.Label;
with Gtk.Main;
with Gtk.Widget;       use Gtk.Widget;
with Interfaces.C;     use Interfaces.C;

procedure Aspect_Frame is

   use type Gtk.Atcontext.Gtk_Atcontext;

   procedure Test_New with Convention => C;
   procedure Test_Accessors with Convention => C;
   procedure Test_Child with Convention => C;
   procedure Test_Accessibility with Convention => C;

   procedure Test_New is
      F : constant Gtk_Aspect_Frame :=
        Gtk_Aspect_Frame_New (0.25, 0.75, 1.5, True);
   begin
      Assert_Cmpfloat_Eq (Gdouble (F.Get_Xalign), 0.25);
      Assert_Cmpfloat_Eq (Gdouble (F.Get_Yalign), 0.75);
      Assert_Cmpfloat_Eq (Gdouble (F.Get_Ratio), 1.5);
      Assert_True (F.Get_Obey_Child);
   end Test_New;

   procedure Test_Accessors is
      F : Gtk_Aspect_Frame;
   begin
      Gtk_New (F, 0.5, 0.5, 1.0, False);
      Assert_False (F.Get_Obey_Child);

      F.Set_Xalign (0.0);
      F.Set_Yalign (1.0);
      F.Set_Ratio (2.0);
      F.Set_Obey_Child (True);
      Assert_Cmpfloat_Eq (Gdouble (F.Get_Xalign), 0.0);
      Assert_Cmpfloat_Eq (Gdouble (F.Get_Yalign), 1.0);
      Assert_Cmpfloat_Eq (Gdouble (F.Get_Ratio), 2.0);
      Assert_True (F.Get_Obey_Child);

      --  The properties mirror the accessors
      Assert_Cmpfloat_Eq
        (Gdouble (Glib.Properties.Get_Property
           (F, Gtk.Aspect_Frame.Ratio_Property)), 2.0);
      Glib.Properties.Set_Property (F, Gtk.Aspect_Frame.Xalign_Property, 0.75);
      Assert_Cmpfloat_Eq (Gdouble (F.Get_Xalign), 0.75);
   end Test_Accessors;

   procedure Test_Child is
      F : constant Gtk_Aspect_Frame :=
        Gtk_Aspect_Frame_New (0.5, 0.5, 1.0, False);
      L : Gtk_Label;
   begin
      Assert_True (F.Get_Child = null);
      Gtk_New (L, "child");
      F.Set_Child (L);
      Assert_True (F.Get_Child = Gtk_Widget (L));
      F.Set_Child (null);
      Assert_True (F.Get_Child = null);
   end Test_Child;

   --  Check inherited accessibility getters and mutations on an unrooted
   --  aspect frame, where no screen coordinates or AT context are available.
   procedure Test_Accessibility is
      F : constant Gtk_Aspect_Frame :=
        Gtk_Aspect_Frame_New (0.5, 0.5, 1.0, False);
      Child : constant Gtk_Label := Gtk_Label_New ("accessible child");
      X, Y, Width, Height : Gint;
   begin
      Ref_Sink (F);
      F.Set_Child (Child);

      --  GtkAspectFrame uses the generic accessible role and exposes its
      --  child in the accessible tree; a detached widget has no parent.
      Assert_True (F.Get_Accessible_Role = Accessible_Role_Generic);
      Assert_Cmpstr_Eq (F.Get_Accessible_Id, "");
      Assert_True (F.Get_First_Accessible_Child /= Null_Gtk_Accessible);
      Assert_True (F.Get_Accessible_Parent = Null_Gtk_Accessible);
      Assert_True (F.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      Assert_True (F.Get_At_Context = null);
      Assert_False (F.Get_Bounds (X, Y, Width, Height));
      Assert_False
        (F.Get_Platform_State (Accessible_Platform_State_Focused));

      --  These accessibility notifications are safe without a screen reader.
      --  Null links keep the detached widget out of a synthetic accessible tree.
      F.Announce ("aspect frame test", Accessible_Announcement_Priority_Low);
      F.Reset_Property (Accessible_Property_Label);
      F.Reset_Relation (Accessible_Relation_Labelled_By);
      F.Reset_State (Accessible_State_Busy);
      F.Update_Platform_State (Accessible_Platform_State_Focusable);
      F.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (F.Get_Accessible_Parent = Null_Gtk_Accessible);
      F.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (F.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      Assert_True (F.Get_Accessible_Role = Accessible_Role_Generic);

      Unref (F);
   end Test_Accessibility;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/aspectframe/new", Test_New'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/aspectframe/accessors", Test_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func ("/aspectframe/child", Test_Child'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/aspectframe/accessibility", Test_Accessibility'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Aspect_Frame;
