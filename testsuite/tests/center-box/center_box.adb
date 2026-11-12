--  Exercise Gtk.Center_Box constructors, layout methods, inherited
--  accessibility methods, and the interfaces implemented by the widget.
--  There is no corresponding GtkCenterBox test in testsuite/c_tests/ to port.

with Ada.Command_Line;
with Glib;
with Glib.Object; use Glib.Object;
with Glib.Test;   use Glib.Test;
with Gtk.Accessible; use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Center_Box; use Gtk.Center_Box;
with Gtk.Constraint_Target;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Label; use Gtk.Label;
with Gtk.Main;
with Gtk.Orientable;
with Gtk.Widget;
with System;

use type Glib.GType;
use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Atcontext.Gtk_Atcontext;
use type Gtk.Buildable.Gtk_Buildable;
use type Gtk.Center_Box.Gtk_Center_Box;
use type Gtk.Constraint_Target.Gtk_Constraint_Target;
use type Gtk.Orientable.Gtk_Orientable;
use type Gtk.Widget.Gtk_Widget;
use type System.Address;

procedure Center_Box is

   procedure Test_Layout
   with Convention => C;

   procedure Test_Accessibility_And_Interfaces
   with Convention => C;

   --  Both constructor forms create a horizontal box. The position setters
   --  and child accessors are checked after changing each value.
   procedure Test_Layout is
      Box       : Gtk_Center_Box;
      From_Func : Gtk_Center_Box;
      Start     : Gtk_Label;
      Center    : Gtk_Label;
      Finish    : Gtk_Label;
      Object    : System.Address;
   begin
      Gtk_New (Box);
      From_Func := Gtk_Center_Box_New;
      Ref_Sink (Box);
      Ref_Sink (From_Func);
      Assert_True (Gtk.Center_Box.Get_Type /= Glib.GType_Invalid);

      --  Initialize is intentionally called again to cover its documented
      --  already-created path and verify it keeps the same GObject.
      Object := Box.Get_Object;
      Gtk.Center_Box.Initialize (Box);
      Assert_True (Box.Get_Object = Object);
      Assert_True (From_Func.Get_Orientation = Orientation_Horizontal);

      --  The default baseline position is changed through every enum choice.
      Box.Set_Baseline_Position (Baseline_Position_Top);
      Assert_True (Box.Get_Baseline_Position = Baseline_Position_Top);
      Box.Set_Baseline_Position (Baseline_Position_Center);
      Assert_True (Box.Get_Baseline_Position = Baseline_Position_Center);
      Box.Set_Baseline_Position (Baseline_Position_Bottom);
      Assert_True (Box.Get_Baseline_Position = Baseline_Position_Bottom);

      --  Shrink policy is a boolean property with a round-trip getter.
      Box.Set_Shrink_Center_Last (False);
      Assert_False (Box.Get_Shrink_Center_Last);
      Box.Set_Shrink_Center_Last (True);
      Assert_True (Box.Get_Shrink_Center_Last);

      --  Hold references to each label while the center box also parents it.
      Start := Gtk_Label_New ("start");
      Center := Gtk_Label_New ("center");
      Finish := Gtk_Label_New ("finish");
      Ref_Sink (Start);
      Ref_Sink (Center);
      Ref_Sink (Finish);
      Box.Set_Start_Widget (Start);
      Box.Set_Center_Widget (Center);
      Box.Set_End_Widget (Finish);
      Assert_True (Box.Get_Start_Widget = Gtk.Widget.Gtk_Widget (Start));
      Assert_True (Box.Get_Center_Widget = Gtk.Widget.Gtk_Widget (Center));
      Assert_True (Box.Get_End_Widget = Gtk.Widget.Gtk_Widget (Finish));

      --  GtkOrientable controls the same orientation as the direct methods.
      Box.Set_Orientation (Orientation_Vertical);
      Assert_True (Box.Get_Orientation = Orientation_Vertical);
      Box.Set_Orientation (Orientation_Horizontal);
      Assert_True (Box.Get_Orientation = Orientation_Horizontal);

      --  Passing null removes a child from its slot.
      Box.Set_Start_Widget (null);
      Box.Set_Center_Widget (null);
      Box.Set_End_Widget (null);
      Assert_True (Box.Get_Start_Widget = null);
      Assert_True (Box.Get_Center_Widget = null);
      Assert_True (Box.Get_End_Widget = null);

      Unref (Finish);
      Unref (Center);
      Unref (Start);
      Unref (From_Func);
      Unref (Box);
   end Test_Layout;

   --  Accessibility state on an unrooted widget is deterministic: it has no
   --  parent, AT context, or screen bounds, while its role is generic.
   procedure Test_Accessibility_And_Interfaces is
      Box       : constant Gtk_Center_Box := Gtk_Center_Box_New;
      Other     : constant Gtk_Center_Box := Gtk_Center_Box_New;
      Child     : constant Gtk_Label := Gtk_Label_New ("accessible child");
      Parent    : Gtk_Accessible;
      Next      : Gtk_Accessible;
      First     : Gtk_Accessible;
      Context   : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Glib.Gint;
      Has_Bounds : Boolean;

      --  Each conversion pair is exercised to verify that it preserves the
      --  identity of the original center box.
      Accessible : constant Gtk_Accessible := +Box;
      Buildable  : constant Gtk.Buildable.Gtk_Buildable := +Box;
      Constraint : constant Gtk.Constraint_Target.Gtk_Constraint_Target := +Box;
      Orientable : constant Gtk.Orientable.Gtk_Orientable := +Box;
   begin
      Ref_Sink (Box);
      Ref_Sink (Other);
      Ref_Sink (Child);
      Box.Set_Center_Widget (Child);

      Assert_True (Box.Get_Accessible_Role = Accessible_Role_Generic);
      Assert_Cmpstr_Eq (Box.Get_Accessible_Id, "");
      Parent := Box.Get_Accessible_Parent;
      Assert_True (Parent = Null_Gtk_Accessible);
      Next := Box.Get_Next_Accessible_Sibling;
      Assert_True (Next = Null_Gtk_Accessible);
      First := Box.Get_First_Accessible_Child;
      Assert_True (First /= Null_Gtk_Accessible);

      --  An unrooted widget does not yet have platform geometry or an AT
      --  context, so these checks do not depend on a real display surface.
      Context := Box.Get_At_Context;
      Assert_True (Context = null);
      Has_Bounds := Box.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);
      Assert_False (Box.Get_Platform_State (Accessible_Platform_State_Focused));

      --  These inherited operations accept valid values without requiring an
      --  accessibility backend. The null tree updates leave the box detached.
      Box.Announce ("center box test", Accessible_Announcement_Priority_Low);
      Box.Reset_Property (Accessible_Property_Label);
      Box.Reset_Relation (Accessible_Relation_Labelled_By);
      Box.Reset_State (Accessible_State_Busy);
      Box.Update_Platform_State (Accessible_Platform_State_Focusable);
      Box.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Box.Get_Accessible_Parent = Null_Gtk_Accessible);
      Box.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (Box.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      Assert_True (-Accessible = Box);
      Assert_True (-Buildable = Box);
      Assert_True (-Constraint = Box);
      Assert_True (-Orientable = Box);

      --  Clear the child before releasing the caller's references.
      Box.Set_Center_Widget (null);
      Unref (Child);
      Unref (Other);
      Unref (Box);
   end Test_Accessibility_And_Interfaces;

begin
   Glib.Test.Init;
   --  GTK must be initialized before creating any widgets in these cases.
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/center-box/layout", Test_Layout'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/center-box/accessibility-and-interfaces",
      Test_Accessibility_And_Interfaces'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Center_Box;
