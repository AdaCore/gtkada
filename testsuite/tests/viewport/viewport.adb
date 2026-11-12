--  Headless tests for Gtk.Viewport. There is no direct GTK C viewport test
--  in this checkout, so these cases check the binding's documented API.

with Ada.Command_Line;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Test;             use Glib.Test;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Adjustment;        use Gtk.Adjustment;
with Gtk.Atcontext;
with Gtk.Border;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Label;             use Gtk.Label;
with Gtk.Main;
with Gtk.Scroll_Info;
with Gtk.Scrollable;
with Gtk.Viewport;           use Gtk.Viewport;
with Gtk.Widget;             use Gtk.Widget;
with System;

procedure Viewport is

   --  Pointer comparisons verify that getters return the same GTK objects
   --  supplied to constructors and setters.
   use type System.Address;
   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gtk.Accessible.Gtk_Accessible_Role;
   use type Gtk.Enums.Gtk_Scrollable_Policy;
   use type Gtk.Scrollable.Gtk_Scrollable;
   use type Gtk.Viewport.Gtk_Viewport;
   use type Gtk.Widget.Gtk_Widget;

   procedure Test_Constructors with Convention => C;
   procedure Test_Child_Focus_And_Scroll with Convention => C;
   procedure Test_Scrollable_And_Accessibility with Convention => C;
   procedure Test_Scrollable_Interface_Wrappers with Convention => C;

   --  Exercise both constructor forms and Initialize's already-created path.
   procedure Test_Constructors is
      H_Adjustment : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (10.0, 0.0, 100.0, 1.0, 10.0, 20.0);
      V_Adjustment : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (5.0, 0.0, 80.0, 1.0, 8.0, 15.0);
      Via_Procedure : Gtk_Viewport;
      Via_Function : constant Gtk_Viewport :=
        Gtk_Viewport_New (H_Adjustment, V_Adjustment);
      Raw_Record : constant Gtk_Viewport := new Gtk_Viewport_Record;
   begin
      Gtk_New (Via_Procedure, H_Adjustment, V_Adjustment);
      Ref_Sink (Via_Procedure);
      Ref_Sink (Via_Function);

      --  Construction retains the provided horizontal and vertical objects.
      Assert_True (Via_Procedure.Is_Created);
      Assert_True (Via_Function.Is_Created);
      Assert_True
        (Get_Object (Via_Procedure.Get_Hadjustment) = Get_Object (H_Adjustment));
      Assert_True
        (Get_Object (Via_Function.Get_Vadjustment) = Get_Object (V_Adjustment));

      --  The first Initialize creates the raw record; the second must leave it
      --  unchanged, even when different (null) adjustments are requested.
      Gtk.Viewport.Initialize (Raw_Record, H_Adjustment, V_Adjustment);
      Gtk.Viewport.Initialize (Raw_Record, null, null);
      Ref_Sink (Raw_Record);
      Assert_True (Raw_Record.Is_Created);
      Assert_True
        (Get_Object (Raw_Record.Get_Hadjustment) = Get_Object (H_Adjustment));
      Assert_Nonnull (Get_Object (Raw_Record));
      Assert_True (Gtk.Viewport.Get_Type /= 0);

      --  The test process owns these objects through shutdown; keeping their
      --  references avoids finalizing GTK's lazily linked adjustment signals.
   end Test_Constructors;

   --  Check the child and focus property round trips; Scroll_To is safe before
   --  mapping and must leave the viewport's child relationship intact.
   procedure Test_Child_Focus_And_Scroll is
      View : constant Gtk_Viewport := Gtk_Viewport_New (null, null);
      Child : constant Gtk_Label := Gtk_Label_New ("viewport content");
   begin
      Ref_Sink (View);
      Ref_Sink (Child);

      Assert_True (View.Get_Child = null);
      View.Set_Scroll_To_Focus (False);
      Assert_False (View.Get_Scroll_To_Focus);
      View.Set_Scroll_To_Focus (True);
      Assert_True (View.Get_Scroll_To_Focus);

      View.Set_Child (Child);
      Assert_True (Gtk_Widget (View.Get_Child) = Gtk_Widget (Child));
      View.Scroll_To (Child, Gtk.Scroll_Info.Null_Gtk_Scroll_Info);
      Assert_True (Gtk_Widget (View.Get_Child) = Gtk_Widget (Child));

      --  Passing null is the documented way to remove the current child.
      View.Set_Child (null);
      Assert_True (View.Get_Child = null);
      --  Leave the widgets alive until test-process shutdown.
   end Test_Child_Focus_And_Scroll;

   --  Exercise GtkScrollable's adjustment and policy methods plus inherited
   --  GtkAccessible calls while the viewport is deliberately unmapped.
   procedure Test_Scrollable_And_Accessibility is
      View : constant Gtk_Viewport := Gtk_Viewport_New (null, null);
      Child : constant Gtk_Label := Gtk_Label_New ("accessible content");
      H_Adjustment : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (2.0, 0.0, 100.0, 1.0, 10.0, 15.0);
      V_Adjustment : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (4.0, 0.0, 80.0, 1.0, 8.0, 12.0);
      Border : Gtk.Border.Gtk_Border;
      Has_Border : Boolean;
      X, Y, Width, Height : Gint;
   begin
      Ref_Sink (View);
      Ref_Sink (Child);

      --  Both adjustment accessors must return the objects installed by the
      --  GtkScrollable setters, and the policies retain separate axis values.
      View.Set_Hadjustment (H_Adjustment);
      View.Set_Vadjustment (V_Adjustment);
      Assert_True
        (Get_Object (View.Get_Hadjustment) = Get_Object (H_Adjustment));
      Assert_True
        (Get_Object (View.Get_Vadjustment) = Get_Object (V_Adjustment));
      View.Set_Hscroll_Policy (Scroll_Natural);
      View.Set_Vscroll_Policy (Scroll_Minimum);
      Assert_True (View.Get_Hscroll_Policy = Scroll_Natural);
      Assert_True (View.Get_Vscroll_Policy = Scroll_Minimum);

      --  An unmapped widget has no accessible bounds or platform focus state.
      Assert_True (View.Get_Accessible_Role = Accessible_Role_Generic);
      Assert_Cmpstr_Eq (View.Get_Accessible_Id, "");
      Assert_True (View.Get_At_Context = null);
      Assert_False (View.Get_Bounds (X, Y, Width, Height));
      Assert_False
        (View.Get_Platform_State (Accessible_Platform_State_Focused));
      Has_Border := View.Get_Border (Border);
      --  Themes may supply a border; when present, its insets are nonnegative.
      Assert_True
        (not Has_Border or else
           (Border.Left >= 0 and Border.Right >= 0
            and Border.Top >= 0 and Border.Bottom >= 0));

      --  The child appears in the accessible tree, while absent parent and
      --  sibling relationships are represented by the null interface value.
      Assert_True (View.Get_Accessible_Parent = Null_Gtk_Accessible);
      Assert_True (View.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      View.Set_Child (Child);
      Assert_True (View.Get_First_Accessible_Child /= Null_Gtk_Accessible);
      View.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (View.Get_Accessible_Parent = Null_Gtk_Accessible);
      View.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (View.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  These operations use valid public enum values and a short UTF-8
      --  message; GTK_A11Y=none keeps the test independent of an AT process.
      View.Announce ("viewport test", Accessible_Announcement_Priority_Low);
      View.Reset_Property (Accessible_Property_Label);
      View.Reset_Relation (Accessible_Relation_Labelled_By);
      View.Reset_State (Accessible_State_Busy);
      View.Update_Platform_State (Accessible_Platform_State_Focusable);
      Assert_True (View.Get_Accessible_Role = Accessible_Role_Generic);

      View.Set_Child (null);
      --  Keep GTK-owned adjustment and child links alive until shutdown.
   end Test_Scrollable_And_Accessibility;

   --  Call Gtk.Scrollable's own wrappers through the interface value. The
   --  viewport also exposes generated forwarding methods, so exercising those
   --  alone would leave the shared Gtk.Scrollable body untouched.
   procedure Test_Scrollable_Interface_Wrappers is
      View : constant Gtk_Viewport := Gtk_Viewport_New (null, null);
      H_Adjustment : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (3.0, 0.0, 50.0, 1.0, 5.0, 10.0);
      V_Adjustment : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (6.0, 0.0, 60.0, 1.0, 6.0, 12.0);
      Scrollable : constant Gtk.Scrollable.Gtk_Scrollable :=
        Gtk.Viewport.Implements_Gtk_Scrollable.To_Interface (View);
      Border : Gtk.Border.Gtk_Border;
      Has_Border : Boolean;
   begin
      Ref_Sink (View);

      --  The interface identity operator must preserve the value and still
      --  convert back to the same viewport object.
      Assert_True
        (Gtk.Viewport.Implements_Gtk_Scrollable.To_Object
           (Gtk.Scrollable."+" (Scrollable)) = View);

      --  These interface setters and getters exercise the shared Ada wrappers
      --  and must return the exact adjustment objects that GTK stores.
      Gtk.Scrollable.Set_Hadjustment (Scrollable, H_Adjustment);
      Gtk.Scrollable.Set_Vadjustment (Scrollable, V_Adjustment);
      Assert_True
        (Get_Object (Gtk.Scrollable.Get_Hadjustment (Scrollable)) =
           Get_Object (H_Adjustment));
      Assert_True
        (Get_Object (Gtk.Scrollable.Get_Vadjustment (Scrollable)) =
           Get_Object (V_Adjustment));

      --  Get_Border may report no border; if a theme supplies one, all four
      --  reported insets must be valid nonnegative sizes.
      Has_Border := Gtk.Scrollable.Get_Border (Scrollable, Border);
      Assert_True
        (not Has_Border or else
           (Border.Left >= 0 and Border.Right >= 0
            and Border.Top >= 0 and Border.Bottom >= 0));
   end Test_Scrollable_Interface_Wrappers;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/viewport/constructors", Test_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/viewport/child-focus-scroll", Test_Child_Focus_And_Scroll'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/viewport/scrollable-accessibility",
      Test_Scrollable_And_Accessibility'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/viewport/scrollable-interface-wrappers",
      Test_Scrollable_Interface_Wrappers'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Viewport;
