--  Exercise Gtk.Separator construction, orientation, accessibility wrappers,
--  and the interface conversions declared by the Ada binding. There is no
--  direct GtkSeparator test in testsuite/c_tests/ to port.

with Ada.Command_Line;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Test;             use Glib.Test;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Main;
with Gtk.Orientable;
with Gtk.Separator;         use Gtk.Separator;
with System;

use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Atcontext.Gtk_Atcontext;
use type Gtk.Buildable.Gtk_Buildable;
use type Gtk.Constraint_Target.Gtk_Constraint_Target;
use type Gtk.Orientable.Gtk_Orientable;
use type Gtk.Separator.Gtk_Separator;
use type System.Address;

procedure Separator is

   --  Check both constructors and the inherited GtkOrientable property. Calling
   --  Initialize again verifies its documented no-op behavior after creation.
   procedure Test_Constructors_And_Orientation with Convention => C;

   --  Cover every accessibility wrapper on a detached separator, where GTK
   --  reports no accessible parent, child, sibling, context, or bounds.
   procedure Test_Accessibility_And_Interfaces with Convention => C;

   procedure Test_Constructors_And_Orientation is
      By_Procedure : Gtk_Separator;
      By_Function  : Gtk_Separator :=
        Gtk_Separator_New (Orientation_Horizontal);
   begin
      Gtk_New (By_Procedure, Orientation_Vertical);
      Ref_Sink (By_Procedure);
      Ref_Sink (By_Function);

      --  The first Initialize call set the requested orientation. A later call
      --  must leave the already-created object's orientation unchanged.
      Assert_True (By_Procedure.Get_Orientation = Orientation_Vertical);
      By_Procedure.Initialize (Orientation_Horizontal);
      Assert_True (By_Procedure.Get_Orientation = Orientation_Vertical);
      Assert_True (By_Function.Get_Orientation = Orientation_Horizontal);

      --  The constructors return live native objects and the widget type is
      --  registered with GLib.
      Assert_True (By_Procedure.Get_Object /= System.Null_Address);
      Assert_True (By_Function.Get_Object /= System.Null_Address);
      Assert_True (Gtk.Separator.Get_Type /= GType_Invalid);

      By_Function.Set_Orientation (Orientation_Vertical);
      Assert_True (By_Function.Get_Orientation = Orientation_Vertical);
      By_Function.Set_Orientation (Orientation_Horizontal);
      Assert_True (By_Function.Get_Orientation = Orientation_Horizontal);

      Unref (By_Procedure);
      Unref (By_Function);
   end Test_Constructors_And_Orientation;

   procedure Test_Accessibility_And_Interfaces is
      Separator : constant Gtk_Separator :=
        Gtk_Separator_New (Orientation_Horizontal);
      Parent  : Gtk_Accessible;
      Next    : Gtk_Accessible;
      X, Y, Width, Height : Gint;
   begin
      Ref_Sink (Separator);

      --  The fixed role identifies this widget; detached widgets do not yet
      --  have an AT context or screen-space bounds.
      Assert_True (Separator.Get_Accessible_Role = Accessible_Role_Separator);
      Assert_Cmpstr_Eq (Separator.Get_Accessible_Id, "");
      Assert_True (Separator.Get_At_Context = null);
      Assert_False (Separator.Get_Bounds (X, Y, Width, Height));
      Assert_False
        (Separator.Get_Platform_State (Accessible_Platform_State_Focused));

      --  A separator has no accessible children, parent, or next sibling.
      Assert_True
        (Separator.Get_First_Accessible_Child = Null_Gtk_Accessible);
      Parent := Separator.Get_Accessible_Parent;
      Assert_True (Parent = Null_Gtk_Accessible);
      Next := Separator.Get_Next_Accessible_Sibling;
      Assert_True (Next = Null_Gtk_Accessible);

      --  Exercise the public accessibility update and reset calls with valid
      --  enum values. Null relationships preserve this detached widget state.
      Separator.Announce ("separator test", Accessible_Announcement_Priority_Low);
      Separator.Reset_Property (Accessible_Property_Label);
      Separator.Reset_Relation (Accessible_Relation_Labelled_By);
      Separator.Reset_State (Accessible_State_Busy);
      Separator.Update_Platform_State (Accessible_Platform_State_Focusable);
      Separator.Set_Accessible_Parent
        (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True
        (Separator.Get_Accessible_Parent = Null_Gtk_Accessible);
      Separator.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True
        (Separator.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  Round-trip each interface implemented by Gtk.Separator. This checks
      --  the binding's conversions preserve the identity of the Ada object.
      declare
         Accessible : constant Gtk_Accessible := +Separator;
         Buildable  : constant Gtk.Buildable.Gtk_Buildable := +Separator;
         Constraint : constant Gtk.Constraint_Target.Gtk_Constraint_Target :=
           +Separator;
         Orientable : constant Gtk.Orientable.Gtk_Orientable := +Separator;
      begin
         Assert_True (-Accessible = Separator);
         Assert_True (-Buildable = Separator);
         Assert_True (-Constraint = Separator);
         Assert_True (-Orientable = Separator);
      end;

      Unref (Separator);
   end Test_Accessibility_And_Interfaces;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/separator/constructors-orientation",
      Test_Constructors_And_Orientation'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/separator/accessibility-interfaces",
      Test_Accessibility_And_Interfaces'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Separator;
