--  Exercise Gtk.Box construction, layout, child ordering, and inherited
--  accessibility methods. The child-order checks adapt simple/two from
--  testsuite/c_tests/widgetorder.c to the Gtk.Box child APIs.

with Ada.Command_Line;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties;
with Glib.Test;       use Glib.Test;
with Gtk.Accessible;  use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Box;         use Gtk.Box;
with Gtk.Box_Layout;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Label;       use Gtk.Label;
with Gtk.Main;
with Gtk.Orientable;
with Gtk.Widget;

use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Accessible.Gtk_Accessible_Role;
use type Gtk.Box_Layout.Gtk_Box_Layout;
use type Gtk.Atcontext.Gtk_Atcontext;
use type Gtk.Box.Gtk_Box;
use type Gtk.Orientable.Gtk_Orientable;
use type Gtk.Widget.Gtk_Widget;

procedure Box is

   procedure Test_Layout
   with Convention => C;

   procedure Test_Box_Layout
   with Convention => C;

   procedure Test_Child_Order
   with Convention => C;

   procedure Test_Accessibility
   with Convention => C;

   --  Both constructor forms and the setter/getter pairs preserve their
   --  documented values. Reinitializing an existing object must be a no-op.
   procedure Test_Layout is
      Box_Function  : constant Gtk_Box := Gtk_Box_New (Orientation_Horizontal, 3);
      Box_Procedure : Gtk_Box;
   begin
      Ref_Sink (Box_Function);
      Gtk_New (Box_Procedure, Orientation_Vertical, 5);
      Ref_Sink (Box_Procedure);

      Assert_True (Box_Function.Get_Orientation = Orientation_Horizontal);
      Assert_Cmpint_Eq (Box_Function.Get_Spacing, 3);
      Assert_True (Box_Procedure.Get_Orientation = Orientation_Vertical);
      Assert_Cmpint_Eq (Box_Procedure.Get_Spacing, 5);

      --  Initialize must keep the original GObject and its configured values.
      Box_Function.Initialize (Orientation_Vertical, 99);
      Assert_True (Box_Function.Get_Orientation = Orientation_Horizontal);
      Assert_Cmpint_Eq (Box_Function.Get_Spacing, 3);

      Box_Function.Set_Orientation (Orientation_Vertical);
      Assert_True (Box_Function.Get_Orientation = Orientation_Vertical);
      Box_Function.Set_Orientation (Orientation_Horizontal);
      Assert_True (Box_Function.Get_Orientation = Orientation_Horizontal);

      Box_Function.Set_Spacing (7);
      Assert_Cmpint_Eq (Box_Function.Get_Spacing, 7);
      Box_Function.Set_Spacing (0);
      Assert_Cmpint_Eq (Box_Function.Get_Spacing, 0);

      Assert_False (Box_Function.Get_Homogeneous);
      Box_Function.Set_Homogeneous (True);
      Assert_True (Box_Function.Get_Homogeneous);
      Box_Function.Set_Homogeneous (False);
      Assert_False (Box_Function.Get_Homogeneous);

      --  Baseline controls round-trip the selected child index and alignment.
      Assert_Cmpint_Eq (Box_Function.Get_Baseline_Child, -1);
      Box_Function.Set_Baseline_Child (2);
      Assert_Cmpint_Eq (Box_Function.Get_Baseline_Child, 2);
      Box_Function.Set_Baseline_Child (-1);
      Assert_Cmpint_Eq (Box_Function.Get_Baseline_Child, -1);

      Box_Function.Set_Baseline_Position (Baseline_Position_Top);
      Assert_True
        (Box_Function.Get_Baseline_Position = Baseline_Position_Top);
      Box_Function.Set_Baseline_Position (Baseline_Position_Center);
      Assert_True
        (Box_Function.Get_Baseline_Position = Baseline_Position_Center);
      Box_Function.Set_Baseline_Position (Baseline_Position_Bottom);
      Assert_True
        (Box_Function.Get_Baseline_Position = Baseline_Position_Bottom);

      Unref (Box_Procedure);
      Unref (Box_Function);
   end Test_Layout;

   --  Exercise Gtk.Box_Layout directly; Gtk.Box's internal manager does not
   --  call these Ada bindings. The two constructors and Initialize share the
   --  same object lifecycle as the corresponding C API.
   procedure Test_Box_Layout is
      By_Function : constant Gtk.Box_Layout.Gtk_Box_Layout :=
        Gtk.Box_Layout.Gtk_Box_Layout_New (Orientation_Vertical);
      By_Procedure : Gtk.Box_Layout.Gtk_Box_Layout;
   begin
      --  Layout managers are GObjects, so retain each constructor's reference
      --  while checking its properties and release it at the end of the case.
      Glib.Object.Ref_Sink (Glib.Object.GObject (By_Function));
      Gtk.Box_Layout.Gtk_New (By_Procedure, Orientation_Horizontal);
      Glib.Object.Ref_Sink (Glib.Object.GObject (By_Procedure));

      Assert_True (By_Function.Get_Orientation = Orientation_Vertical);
      Assert_True (By_Procedure.Get_Orientation = Orientation_Horizontal);
      Assert_Cmpuint_Eq (By_Function.Get_Spacing, 0);
      Assert_False (By_Function.Get_Homogeneous);
      Assert_Cmpint_Eq (By_Function.Get_Baseline_Child, -1);

      --  Initialize is idempotent once the object has a GObject. It must not
      --  replace the original horizontal manager with a vertical one.
      By_Procedure.Initialize (Orientation_Vertical);
      Assert_True (By_Procedure.Get_Orientation = Orientation_Horizontal);

      --  Exercise each setter/getter pair with values that distinguish the
      --  properties, including the documented -1 sentinel for no baseline.
      By_Function.Set_Orientation (Orientation_Horizontal);
      Assert_True (By_Function.Get_Orientation = Orientation_Horizontal);
      By_Function.Set_Orientation (Orientation_Vertical);
      Assert_True (By_Function.Get_Orientation = Orientation_Vertical);
      By_Function.Set_Spacing (9);
      Assert_Cmpuint_Eq (By_Function.Get_Spacing, 9);
      By_Function.Set_Spacing (0);
      Assert_Cmpuint_Eq (By_Function.Get_Spacing, 0);
      By_Function.Set_Homogeneous (True);
      Assert_True (By_Function.Get_Homogeneous);
      By_Function.Set_Homogeneous (False);
      Assert_False (By_Function.Get_Homogeneous);
      By_Function.Set_Baseline_Child (3);
      Assert_Cmpint_Eq (By_Function.Get_Baseline_Child, 3);
      By_Function.Set_Baseline_Child (-1);
      Assert_Cmpint_Eq (By_Function.Get_Baseline_Child, -1);
      By_Function.Set_Baseline_Position (Baseline_Position_Top);
      Assert_True
        (By_Function.Get_Baseline_Position = Baseline_Position_Top);

      --  The GObject properties provide an alternate typed path to the same
      --  values; these checks also exercise the public property constants.
      Glib.Properties.Set_Property
        (By_Function, Gtk.Box_Layout.Homogeneous_Property, True);
      Assert_True
        (Glib.Properties.Get_Property
           (By_Function, Gtk.Box_Layout.Homogeneous_Property));
      Glib.Properties.Int_Properties.Set_Property
        (By_Function,
         Glib.Properties.Int_Properties.Property
           (Gtk.Box_Layout.Spacing_Property), 5);
      Assert_Cmpuint_Eq (By_Function.Get_Spacing, 5);
      Glib.Properties.Int_Properties.Set_Property
        (By_Function,
         Glib.Properties.Int_Properties.Property
           (Gtk.Box_Layout.Baseline_Child_Property), 1);
      Assert_Cmpint_Eq (By_Function.Get_Baseline_Child, 1);
      Gtk.Enums.Gtk_Baseline_Position_Properties.Set_Property
        (By_Function,
         Gtk.Enums.Gtk_Baseline_Position_Properties.Property
           (Gtk.Box_Layout.Baseline_Position_Property),
         Baseline_Position_Bottom);
      Assert_True
        (By_Function.Get_Baseline_Position = Baseline_Position_Bottom);

      --  Gtk.Box_Layout implements Gtk.Orientable. Round-trip its interface
      --  view and call through the interface to verify dispatch and typing.
      declare
         Orientable : constant Gtk.Orientable.Gtk_Orientable :=
           Gtk.Box_Layout."+" (By_Function);
         Back : constant Gtk.Box_Layout.Gtk_Box_Layout :=
           Gtk.Box_Layout."-" (Orientable);
      begin
         Assert_True (Back = By_Function);
         Gtk.Orientable.Set_Orientation (Orientable, Orientation_Horizontal);
         Assert_True
           (Gtk.Orientable.Get_Orientation (Orientable) =
              Orientation_Horizontal);
         Assert_True (By_Function.Get_Orientation = Orientation_Horizontal);
      end;

      Glib.Object.Unref (Glib.Object.GObject (By_Procedure));
      Glib.Object.Unref (Glib.Object.GObject (By_Function));
   end Test_Box_Layout;

   --  Check append, prepend, insertion, removal, and reordering through
   --  Gtk.Box, asserting the child at every affected position.
   procedure Test_Child_Order is
      Container : constant Gtk_Box := Gtk_Box_New (Orientation_Horizontal, 0);
      A : constant Gtk_Label := Gtk_Label_New ("a");
      B : constant Gtk_Label := Gtk_Label_New ("b");
      C : constant Gtk_Label := Gtk_Label_New ("c");
      D : constant Gtk_Label := Gtk_Label_New ("d");
   begin
      Ref_Sink (Container);
      Ref_Sink (A);
      Ref_Sink (B);
      Ref_Sink (C);
      Ref_Sink (D);

      --  An empty box has no zero-th child; appending two labels matches the
      --  simple/two ordering checks in widgetorder.c.
      Assert_True (Container.Get_Child (0) = null);
      Container.Append (A);
      Container.Append (B);
      Assert_True (Container.Get_Child (0) = Gtk.Widget.Gtk_Widget (A));
      Assert_True (Container.Get_Child (1) = Gtk.Widget.Gtk_Widget (B));
      Assert_True (Container.Get_Child (2) = null);

      --  Prepend and insert-after place the new child at the requested slot.
      Container.Prepend (C);
      Container.Insert_Child_After (D, A);
      Assert_True (Container.Get_Child (0) = Gtk.Widget.Gtk_Widget (C));
      Assert_True (Container.Get_Child (1) = Gtk.Widget.Gtk_Widget (A));
      Assert_True (Container.Get_Child (2) = Gtk.Widget.Gtk_Widget (D));
      Assert_True (Container.Get_Child (3) = Gtk.Widget.Gtk_Widget (B));
      Assert_True (Container.Get_Child (4) = null);

      --  A null sibling means the beginning for reorder and insert-after.
      Container.Reorder_Child_After (B, null);
      Assert_True (Container.Get_Child (0) = Gtk.Widget.Gtk_Widget (B));
      Container.Reorder_Child_After (C, D);
      Assert_True (Container.Get_Child (1) = Gtk.Widget.Gtk_Widget (A));
      Assert_True (Container.Get_Child (2) = Gtk.Widget.Gtk_Widget (D));
      Assert_True (Container.Get_Child (3) = Gtk.Widget.Gtk_Widget (C));

      Container.Remove (D);
      Assert_True (Container.Get_Child (0) = Gtk.Widget.Gtk_Widget (B));
      Assert_True (Container.Get_Child (1) = Gtk.Widget.Gtk_Widget (A));
      Assert_True (Container.Get_Child (2) = Gtk.Widget.Gtk_Widget (C));
      Assert_True (Container.Get_Child (3) = null);

      Unref (D);
      Unref (C);
      Unref (B);
      Unref (A);
      Unref (Container);
   end Test_Child_Order;

   --  Detached boxes have deterministic accessible metadata and no screen
   --  bounds or AT context, so these checks need no realized window.
   procedure Test_Accessibility is
      Container : constant Gtk_Box := Gtk_Box_New (Orientation_Vertical, 0);
      Child     : constant Gtk_Label := Gtk_Label_New ("accessible child");
      Parent    : Gtk_Accessible;
      Next      : Gtk_Accessible;
      First     : Gtk_Accessible;
      Context   : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
   begin
      Ref_Sink (Container);
      Ref_Sink (Child);
      Container.Append (Child);

      Assert_True
        (Container.Get_Accessible_Role = Accessible_Role_Generic);
      Assert_Cmpstr_Eq (Container.Get_Accessible_Id, "");
      Parent := Container.Get_Accessible_Parent;
      Assert_True (Parent = Null_Gtk_Accessible);
      Next := Container.Get_Next_Accessible_Sibling;
      Assert_True (Next = Null_Gtk_Accessible);
      First := Container.Get_First_Accessible_Child;
      Assert_True (First /= Null_Gtk_Accessible);

      Context := Container.Get_At_Context;
      Assert_True (Context = null);
      Assert_False (Container.Get_Bounds (X, Y, Width, Height));
      Assert_False
        (Container.Get_Platform_State (Accessible_Platform_State_Focused));

      --  Exercise inherited accessible mutators with valid enum values. Null
      --  parent and sibling updates keep the detached accessible tree empty.
      Container.Announce
        ("box test", Accessible_Announcement_Priority_Low);
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

      Container.Remove (Child);
      Unref (Child);
      Unref (Container);
   end Test_Accessibility;

begin
   Glib.Test.Init;
   --  GTK must be initialized before creating widgets in the test cases.
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/box/layout", Test_Layout'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/box/box-layout", Test_Box_Layout'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/box/child-order", Test_Child_Order'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/box/accessibility", Test_Accessibility'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Box;
