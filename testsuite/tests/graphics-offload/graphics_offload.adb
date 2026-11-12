--  Unit tests for Gtk.Graphics_Offload constructors, properties, accessibility
--  methods, and interface conversions. No matching C test exists in the
--  checked-in GTK testsuite, so these cases check the documented Ada API.

with Ada.Command_Line;
with Glib.Object;       use Glib.Object;
with Glib.Properties;
with Glib.Test;         use Glib.Test;
with Glib;
with Gtk.Accessible;    use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Constraint_Target;
with Gtk.Graphics_Offload; use Gtk.Graphics_Offload;
with Gtk.Label;         use Gtk.Label;
with Gtk.Main;
with Gtk.Widget;        use Gtk.Widget;
with System;

procedure Graphics_Offload is

   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gtk.Buildable.Gtk_Buildable;
   use type Gtk.Constraint_Target.Gtk_Constraint_Target;
   use type Gtk.Graphics_Offload.Gtk_Graphics_Offload;
   use type Gtk.Widget.Gtk_Widget;
   use type System.Address;
   use type Glib.GType;

   procedure Test_Constructors_And_Properties with Convention => C;
   procedure Test_Accessibility_And_Interfaces with Convention => C;

   --  Both constructor forms attach their child. Reinitializing a created
   --  object is documented as a no-op, so its original child must remain.
   procedure Test_Constructors_And_Properties is
      Function_Child : constant Gtk_Label := Gtk_Label_New ("function child");
      Procedure_Child : constant Gtk_Label := Gtk_Label_New ("procedure child");
      Replacement : constant Gtk_Label := Gtk_Label_New ("replacement");
      By_Function : constant Gtk_Graphics_Offload :=
        Gtk_Graphics_Offload_New (Function_Child);
      By_Procedure : Gtk_Graphics_Offload;
   begin
      Ref_Sink (By_Function);
      Gtk_New (By_Procedure, Procedure_Child);
      Ref_Sink (By_Procedure);
      --  Keep explicit references while either label is temporarily detached
      --  by replacing the graphics-offload child.
      Ref_Sink (Function_Child);
      Ref_Sink (Procedure_Child);
      Ref_Sink (Replacement);

      Assert_True (By_Function.Get_Type = Gtk.Graphics_Offload.Get_Type);
      Assert_True (By_Procedure.Get_Type = Gtk.Graphics_Offload.Get_Type);
      Assert_True (By_Function.Get_Child = Gtk_Widget (Function_Child));
      Assert_True (By_Procedure.Get_Child = Gtk_Widget (Procedure_Child));

      By_Procedure.Initialize (Function_Child);
      Assert_True (By_Procedure.Get_Child = Gtk_Widget (Procedure_Child));

      --  The method and GObject property paths expose the same mutable values.
      Assert_True (By_Function.Get_Enabled = Enabled);
      By_Function.Set_Enabled (Disabled);
      Assert_True (By_Function.Get_Enabled = Disabled);
      Gtk.Graphics_Offload.Gtk_Graphics_Offload_Enabled_Properties.Set_Property
        (By_Function,
         Gtk.Graphics_Offload.Gtk_Graphics_Offload_Enabled_Properties.Property
           (Gtk.Graphics_Offload.Enabled_Property),
         Enabled);
      Assert_True
        (Gtk.Graphics_Offload.Gtk_Graphics_Offload_Enabled_Properties.Get_Property
           (By_Function,
            Gtk.Graphics_Offload.Gtk_Graphics_Offload_Enabled_Properties.Property
              (Gtk.Graphics_Offload.Enabled_Property)) = Enabled);

      Assert_False (By_Function.Get_Black_Background);
      Glib.Properties.Set_Property
        (By_Function, Gtk.Graphics_Offload.Black_Background_Property, True);
      Assert_True (By_Function.Get_Black_Background);
      By_Function.Set_Black_Background (False);
      Assert_False
        (Glib.Properties.Get_Property
           (By_Function, Gtk.Graphics_Offload.Black_Background_Property));

      --  The object-valued property reports the child held by the container;
      --  replacing it exercises the ordinary setter path as well.
      Assert_True
        (Glib.Properties.Get_Property
           (By_Procedure, Gtk.Graphics_Offload.Child_Property).Get_Object =
         Get_Object (Procedure_Child));
      By_Procedure.Set_Child (Replacement);
      Assert_True (By_Procedure.Get_Child = Gtk_Widget (Replacement));
      Glib.Properties.Set_Property
        (By_Procedure, Gtk.Graphics_Offload.Child_Property, Procedure_Child);
      Assert_True (By_Procedure.Get_Child = Gtk_Widget (Procedure_Child));
      By_Procedure.Set_Child (null);
      Assert_True (By_Procedure.Get_Child = null);

      Unref (By_Procedure);
      Unref (By_Function);
      Unref (Replacement);
      Unref (Procedure_Child);
      Unref (Function_Child);
   end Test_Constructors_And_Properties;

   --  Unrooted widgets have no AT context or screen-coordinate bounds.
   --  Accessibility updates are still valid, and interface round-trips retain
   --  the original GObject for every interface declared by this binding.
   procedure Test_Accessibility_And_Interfaces is
      Child : constant Gtk_Label := Gtk_Label_New ("accessible child");
      Other : constant Gtk_Label := Gtk_Label_New ("other child");
      Offload : constant Gtk_Graphics_Offload := Gtk_Graphics_Offload_New (Child);
      Parent : Gtk_Accessible;
      Next : Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Glib.Gint;
      Has_Bounds : Boolean;
      Accessible : constant Gtk_Accessible := +Offload;
      Buildable : constant Gtk.Buildable.Gtk_Buildable := +Offload;
      Target : constant Gtk.Constraint_Target.Gtk_Constraint_Target := +Offload;
   begin
      Ref_Sink (Offload);
      --  Hold the label independently of the container for safe teardown.
      Ref_Sink (Child);
      Ref_Sink (Other);

      Assert_Cmpstr_Eq (Offload.Get_Accessible_Id, "");
      Assert_True (Offload.Get_Accessible_Role = Accessible_Role_Generic);
      Assert_True (Offload.Get_First_Accessible_Child /= Null_Gtk_Accessible);
      Context := Offload.Get_At_Context;
      Assert_True (Context = null);
      Has_Bounds := Offload.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);
      Parent := Offload.Get_Accessible_Parent;
      Next := Offload.Get_Next_Accessible_Sibling;
      Assert_True (Parent = Null_Gtk_Accessible);
      Assert_True (Next = Null_Gtk_Accessible);
      Assert_False
        (Offload.Get_Platform_State (Accessible_Platform_State_Focused));

      --  These requests are safe for an unattached widget and need no screen
      --  reader. The null parent and sibling keep the accessible tree detached.
      Offload.Announce ("graphics offload test", Accessible_Announcement_Priority_Low);
      Offload.Reset_Property (Accessible_Property_Label);
      Offload.Reset_Relation (Accessible_Relation_Labelled_By);
      Offload.Reset_State (Accessible_State_Busy);
      Offload.Update_Platform_State (Accessible_Platform_State_Focusable);
      Offload.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Offload.Get_Accessible_Parent = Null_Gtk_Accessible);
      Offload.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True
        (Offload.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  Each conversion pair must round-trip to the same widget object.
      Assert_True (-Accessible = Offload);
      Assert_True (-Buildable = Offload);
      Assert_True (-Target = Offload);

      --  The child remains the original accessible child after unrelated
      --  interface conversions; a second widget confirms pointer identity.
      Assert_True (Offload.Get_Child = Gtk_Widget (Child));
      Assert_True (Get_Object (Other) /= Get_Object (Child));
      Unref (Offload);
      Unref (Child);
      Unref (Other);
   end Test_Accessibility_And_Interfaces;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func
     ("/graphics-offload/constructors-properties",
      Test_Constructors_And_Properties'Unrestricted_Access);
   Add_Func
     ("/graphics-offload/accessibility-interfaces",
      Test_Accessibility_And_Interfaces'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Graphics_Offload;
