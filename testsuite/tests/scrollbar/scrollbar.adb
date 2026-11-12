--  Tests GtkScrollbar constructors, its shared adjustment and orientation,
--  and the accessibility methods exposed by the generated Gtk.Scrollbar API.
--  There is no scrollbar-specific C test in testsuite/c_tests/ to port.

with Ada.Command_Line;
with Glib;                   use Glib;
with Glib.Object;            use Glib.Object;
with Glib.Test;              use Glib.Test;
with Gtk.Accessible;         use Gtk.Accessible;
with Gtk.Adjustment;         use Gtk.Adjustment;
with Gtk.Atcontext;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Main;
with Gtk.Scrollbar;          use Gtk.Scrollbar;
with Gtk.Widget;             use Gtk.Widget;

procedure Scrollbar is
   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Atcontext.Gtk_Atcontext;

   procedure Test_Constructors_And_Adjustment with Convention => C;
   procedure Test_Orientation_And_Accessibility with Convention => C;

   --  Both constructor forms must retain the adjustment supplied by the
   --  caller. Reinitializing a live object must leave its original state in
   --  place, as documented for Initialize.
   procedure Test_Constructors_And_Adjustment is
      A1 : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (10.0, 0.0, 100.0, 1.0, 10.0, 0.0);
      A2 : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (25.0, 0.0, 200.0, 2.0, 20.0, 0.0);
      By_Procedure : Gtk_Scrollbar;
      By_Function  : constant Gtk_Scrollbar :=
        Gtk_Scrollbar_New (Orientation_Horizontal, A2);
      Raw_Record   : constant Gtk_Scrollbar := new Gtk_Scrollbar_Record;
   begin
      --  Sink the floating widget references before keeping them in locals.
      Gtk_New (By_Procedure, Orientation_Vertical, A1);
      Ref_Sink (By_Procedure);
      Ref_Sink (By_Function);
      Assert_True (By_Procedure.Is_Created);
      Assert_True (By_Function.Is_Created);
      Assert_True (By_Procedure.Get_Adjustment = A1);
      Assert_True (By_Function.Get_Adjustment = A2);

      --  The public Initialize operation can also initialize an allocated
      --  Ada record, and becomes a no-op once the GTK object exists.
      Gtk.Scrollbar.Initialize
        (Raw_Record, Orientation_Horizontal, A1);
      Ref_Sink (Raw_Record);
      Assert_True (Raw_Record.Is_Created);
      Gtk.Scrollbar.Initialize
        (By_Procedure, Orientation_Horizontal, A2);
      Assert_True (By_Procedure.Get_Orientation = Orientation_Vertical);
      Assert_True (By_Procedure.Get_Adjustment = A1);

      --  Replacing the adjustment updates the scrollbar's model immediately.
      By_Procedure.Set_Adjustment (A2);
      Assert_True (By_Procedure.Get_Adjustment = A2);

      --  Release the owned GTK objects after all observations are complete.
      Unref (Raw_Record);
      Unref (By_Function);
      Unref (By_Procedure);
   end Test_Constructors_And_Adjustment;

   --  A detached scrollbar has no screen bounds, accessibility context,
   --  parent, or following sibling. Its role and orientation are fixed by
   --  the widget class unless explicitly changed through GtkOrientable.
   procedure Test_Orientation_And_Accessibility is
      Bar : constant Gtk_Scrollbar :=
        Gtk_Scrollbar_New (Orientation_Horizontal, null);
      Other : constant Gtk_Scrollbar :=
        Gtk_Scrollbar_New (Orientation_Vertical, null);
      Child  : Gtk_Accessible;
      Next   : Gtk_Accessible;
      X, Y, Width, Height : Gint;
   begin
      Ref_Sink (Bar);
      Ref_Sink (Other);

      --  Verify the orientable interface writes and reads the orientation.
      Assert_True (Bar.Get_Orientation = Orientation_Horizontal);
      Bar.Set_Orientation (Orientation_Vertical);
      Assert_True (Bar.Get_Orientation = Orientation_Vertical);

      --  These values describe an unrooted scrollbar and do not need a
      --  realized display window.
      Assert_True (Bar.Get_Accessible_Role = Accessible_Role_Scrollbar);
      Assert_Cmpstr_Eq (Bar.Get_Accessible_Id, "");
      Assert_True (Bar.Get_At_Context = null);
      Assert_False (Bar.Get_Bounds (X, Y, Width, Height));
      Assert_True (Bar.Get_Accessible_Parent = Null_Gtk_Accessible);
      Child := Bar.Get_First_Accessible_Child;
      --  GtkRange exposes its slider as an accessible child.
      Assert_True (Child /= Null_Gtk_Accessible);
      Next := Bar.Get_Next_Accessible_Sibling;
      Assert_True (Next = Null_Gtk_Accessible);
      Assert_False
        (Bar.Get_Platform_State (Accessible_Platform_State_Focusable));

      --  Widget focusability supplies the platform state that the accessible
      --  notification reports; querying it verifies the update path.
      Bar.Set_Focusable (True);
      Bar.Update_Platform_State (Accessible_Platform_State_Focusable);
      Assert_True
        (Bar.Get_Platform_State (Accessible_Platform_State_Focusable));

      --  Reset operations accept valid property, relation and state values.
      --  The fixed role remains an observable invariant after those resets.
      Bar.Reset_Property (Accessible_Property_Label);
      Bar.Reset_Relation (Accessible_Relation_Labelled_By);
      Bar.Reset_State (Accessible_State_Busy);
      Assert_True (Bar.Get_Accessible_Role = Accessible_Role_Scrollbar);

      --  These tree updates use null endpoints because a standalone widget
      --  cannot install a synthetic accessible parent or sibling.
      Bar.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Bar.Get_Accessible_Parent = Null_Gtk_Accessible);
      Bar.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (Bar.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  Announcements are allowed to degrade to a no-op without an AT.
      Bar.Announce
        ("scrollbar accessibility test", Accessible_Announcement_Priority_Low);

      Unref (Other);
      Unref (Bar);
   end Test_Orientation_And_Accessibility;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/scrollbar/constructors_adjustment",
      Test_Constructors_And_Adjustment'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scrollbar/orientation_accessibility",
      Test_Orientation_And_Accessibility'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Scrollbar;
