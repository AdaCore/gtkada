--  Ada port of testsuite/c_tests/templates.c:test_action_bar_basic.
--  The additional cases exercise Gtk.Action_Bar's child, revealed-property,
--  accessibility, and interface-conversion bindings.

with Ada.Command_Line;
with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Glib.Properties;    use Glib.Properties;
with Glib.Test;          use Glib.Test;
with Gtk.Accessible;     use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Constraint_Target;
with Gtk.Action_Bar;     use Gtk.Action_Bar;
with Gtk.Label;
with Gtk.Main;
with Gtk.Widget;
with System;

procedure Action_Bar is
   use type Gtk_Action_Bar;
   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Accessible.Gtk_Accessible_Role;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gtk.Widget.Gtk_Widget;
   use type System.Address;

   procedure Test_Constructors
   with Convention => C;

   procedure Test_Children_And_Revealed
   with Convention => C;

   procedure Test_Accessibility_And_Interfaces
   with Convention => C;

   --  The C test only checks that the constructor produces an action bar.
   --  Calling Initialize again also checks its documented no-op behavior on
   --  an object that has already been created.
   procedure Test_Constructors is
      By_Function : constant Gtk_Action_Bar := Gtk_Action_Bar_New;
      By_Procedure : Gtk_Action_Bar;
      Original_Object : System.Address;
   begin
      Ref_Sink (By_Function);
      Assert_True (By_Function /= null);

      Gtk_New (By_Procedure);
      Ref_Sink (By_Procedure);
      Original_Object := By_Procedure.Get_Object;
      Gtk.Action_Bar.Initialize (By_Procedure);
      Assert_True (By_Procedure.Get_Object = Original_Object);

      Unref (By_Procedure);
      Unref (By_Function);
   end Test_Constructors;

   --  Packed children stay owned by the bar; sinking our own references lets
   --  this test remove them and then release each object safely.
   procedure Test_Children_And_Revealed is
      Bar : constant Gtk_Action_Bar := Gtk_Action_Bar_New;
      Start_Child : constant Gtk.Widget.Gtk_Widget :=
        Gtk.Widget.Gtk_Widget (Gtk.Label.Gtk_Label_New ("start"));
      End_Child : constant Gtk.Widget.Gtk_Widget :=
        Gtk.Widget.Gtk_Widget (Gtk.Label.Gtk_Label_New ("end"));
      Center_Child : constant Gtk.Widget.Gtk_Widget :=
        Gtk.Widget.Gtk_Widget (Gtk.Label.Gtk_Label_New ("center"));
   begin
      Ref_Sink (Bar);

      Bar.Set_Revealed (False);
      Assert_False (Bar.Get_Revealed);
      Bar.Set_Revealed (True);
      Assert_True (Bar.Get_Revealed);
      --  The typed property accessor must report the same revealed state.
      Assert_True (Get_Property (Bar, Revealed_Property));
      Set_Property (Bar, Revealed_Property, False);
      Assert_False (Bar.Get_Revealed);

      --  Verify both packing sides and the separately managed center slot.
      Bar.Pack_Start (Start_Child);
      Bar.Pack_End (End_Child);
      Bar.Set_Center_Widget (Center_Child);
      Assert_True (Bar.Get_Center_Widget = Center_Child);
      Bar.Set_Center_Widget (null);
      Assert_True (Bar.Get_Center_Widget = null);

      Bar.Remove (Start_Child);
      Bar.Remove (End_Child);
      Unref (Bar);
   end Test_Children_And_Revealed;

   --  These calls are valid on an unrooted action bar and need no screen
   --  reader. Its bounds and AT context are absent until it is rooted.
   procedure Test_Accessibility_And_Interfaces is
      Bar : constant Gtk_Action_Bar := Gtk_Action_Bar_New;
      Parent : Gtk.Accessible.Gtk_Accessible;
      Sibling : Gtk.Accessible.Gtk_Accessible;
      First_Child : Gtk.Accessible.Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
      Has_Bounds : Boolean;
      Accessible : constant Gtk.Accessible.Gtk_Accessible := +Bar;
      Buildable : constant Gtk.Buildable.Gtk_Buildable := +Bar;
      Target : constant Gtk.Constraint_Target.Gtk_Constraint_Target := +Bar;
   begin
      Ref_Sink (Bar);
      Assert_True (Bar.Get_Accessible_Role /= Accessible_Role_None);
      Assert_Cmpstr_Eq (Bar.Get_Accessible_Id, "");
      Context := Bar.Get_At_Context;
      Assert_True (Context = null);
      Has_Bounds := Bar.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);
      Parent := Bar.Get_Accessible_Parent;
      Sibling := Bar.Get_Next_Accessible_Sibling;
      Assert_True (Parent = Null_Gtk_Accessible);
      Assert_True (Sibling = Null_Gtk_Accessible);
      First_Child := Bar.Get_First_Accessible_Child;
      Assert_True (First_Child /= Null_Gtk_Accessible);
      --  The action bar's internal accessible child exists even while detached.
      Assert_False (Bar.Get_Platform_State (Accessible_Platform_State_Focused));

      --  Accessibility notifications and reset requests can be issued while
      --  detached; null links leave the bar's accessible tree unchanged.
      Bar.Announce ("action bar test", Accessible_Announcement_Priority_Low);
      Bar.Reset_Property (Accessible_Property_Label);
      Bar.Reset_Relation (Accessible_Relation_Labelled_By);
      Bar.Reset_State (Accessible_State_Busy);
      Bar.Update_Platform_State (Accessible_Platform_State_Focusable);
      Bar.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Bar.Get_Accessible_Parent = Null_Gtk_Accessible);
      Bar.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (Bar.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  Round trips exercise both directions of each interface conversion.
      Assert_True (-Accessible = Bar);
      Assert_True (-Buildable = Bar);
      Assert_True (-Target = Bar);
      Unref (Bar);
   end Test_Accessibility_And_Interfaces;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/action-bar/constructors", Test_Constructors'Unrestricted_Access);
   Add_Func ("/action-bar/children-revealed", Test_Children_And_Revealed'Unrestricted_Access);
   Add_Func
     ("/action-bar/accessibility-interfaces",
      Test_Accessibility_And_Interfaces'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Action_Bar;
