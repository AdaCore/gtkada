--  Unit tests for Gtk.Frame's constructors, child and label API, interfaces,
--  properties, and inherited accessibility operations. GTK's C testsuite has
--  no gtk_frame_* test to port, so these cases check the documented API.

with Ada.Command_Line;
with Glib;                    use Glib;
with Glib.Object;             use Glib.Object;
with Glib.Properties;
with Glib.Test;               use Glib.Test;
with Gtk.Accessible;          use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Constraint_Target;
with Gtk.Frame;               use Gtk.Frame;
with Gtk.Label;               use Gtk.Label;
with Gtk.Main;
with Gtk.Widget;              use Gtk.Widget;
with Interfaces.C;            use Interfaces.C;
with System;

procedure Frame is

   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gtk.Buildable.Gtk_Buildable;
   use type Gtk.Constraint_Target.Gtk_Constraint_Target;
   use type Gtk.Frame.Gtk_Frame;
   use type Gtk.Widget.Gtk_Widget;
   use type System.Address;

   procedure Test_Constructors with Convention => C;
   procedure Test_Labels_And_Children with Convention => C;
   procedure Test_Accessibility_And_Interfaces with Convention => C;

   --  Both public constructor forms create the same GtkFrame type and keep
   --  the supplied label. Initialize is also idempotent after construction.
   procedure Test_Constructors is
      By_Function  : constant Gtk_Frame := Gtk_Frame_New ("function label");
      By_Procedure : Gtk_Frame;
   begin
      Ref_Sink (By_Function);
      Gtk_New (By_Procedure, "procedure label");
      Ref_Sink (By_Procedure);
      Assert_True (By_Function.Get_Type = Gtk.Frame.Get_Type);
      Assert_Cmpstr_Eq (By_Function.Get_Label, "function label");
      Assert_Cmpstr_Eq (By_Procedure.Get_Label, "procedure label");
      Assert_Cmpfloat_Eq (Gdouble (By_Function.Get_Label_Align), 0.0);

      --  A second initialize request must leave the existing native object
      --  and its label alone, as documented by Gtk.Frame.Initialize.
      By_Procedure.Initialize ("ignored after creation");
      Assert_Cmpstr_Eq (By_Procedure.Get_Label, "procedure label");
      Unref (By_Function);
      Unref (By_Procedure);
   end Test_Constructors;

   --  The frame owns at most one child. Replacing it with null detaches it;
   --  the label widget can be an ordinary GtkLabel or another widget type.
   procedure Test_Labels_And_Children is
      F       : constant Gtk_Frame := Gtk_Frame_New;
      Child   : constant Gtk_Label := Gtk_Label_New ("inside");
      Caption : constant Gtk_Label := Gtk_Label_New ("custom caption");
   begin
      Ref_Sink (F);
      F.Set_Child (Child);
      Assert_True (F.Get_Child = Gtk_Widget (Child));
      Assert_True
        (Get_Object
           (Glib.Properties.Get_Property (F, Gtk.Frame.Child_Property)) =
         Get_Object (Child));

      F.Set_Label ("text label");
      Assert_Cmpstr_Eq (F.Get_Label, "text label");
      F.Set_Label_Align (0.75);
      Assert_Cmpfloat_Eq (Gdouble (F.Get_Label_Align), 0.75);
      Glib.Properties.Set_Property (F, Gtk.Frame.Label_Xalign_Property, 0.25);
      Assert_Cmpfloat_Eq (Gdouble (F.Get_Label_Align), 0.25);

      F.Set_Label_Widget (Caption);
      Assert_True (F.Get_Label_Widget = Gtk_Widget (Caption));
      Assert_True
        (Get_Object
           (Glib.Properties.Get_Property
              (F, Gtk.Frame.Label_Widget_Property)) = Get_Object (Caption));
      Assert_Cmpstr_Eq (F.Get_Label, "custom caption");
      F.Set_Label_Widget (null);
      Assert_True (F.Get_Label_Widget = null);

      --  An empty label removes the implicit label widget and returns an
      --  empty string; resetting the child proves the nullable setter path.
      F.Set_Label ("");
      Assert_Cmpstr_Eq (F.Get_Label, "");
      F.Set_Child (null);
      Assert_True (F.Get_Child = null);
      Unref (F);
   end Test_Labels_And_Children;

   --  The accessibility methods are safe on an unrooted frame: GTK reports
   --  no AT context or screen bounds until it belongs to a displayed tree.
   --  Conversion round-trips check all interfaces declared by Gtk.Frame.
   procedure Test_Accessibility_And_Interfaces is
      F       : constant Gtk_Frame := Gtk_Frame_New ("accessible frame");
      Child   : constant Gtk_Label := Gtk_Label_New ("child");
      Parent  : Gtk_Accessible;
      Next    : Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
      Has_Bounds : Boolean;
      Accessible : constant Gtk_Accessible := +F;
      Buildable : constant Gtk.Buildable.Gtk_Buildable := +F;
      Target : constant Gtk.Constraint_Target.Gtk_Constraint_Target := +F;
   begin
      Ref_Sink (F);
      F.Set_Child (Child);
      Assert_True (F.Get_Accessible_Role = Accessible_Role_Group);
      Assert_Cmpstr_Eq (F.Get_Accessible_Id, "");
      Assert_True (F.Get_First_Accessible_Child /= Null_Gtk_Accessible);

      Context := F.Get_At_Context;
      Assert_True (Context = null);
      Has_Bounds := F.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);
      Parent := F.Get_Accessible_Parent;
      Next := F.Get_Next_Accessible_Sibling;
      Assert_True (Parent = Null_Gtk_Accessible);
      Assert_True (Next = Null_Gtk_Accessible);
      Assert_False (F.Get_Platform_State (Accessible_Platform_State_Focused));

      --  Valid accessibility notifications and reset requests need no
      --  platform screen reader. Null tree links keep this frame detached.
      F.Announce ("frame test", Accessible_Announcement_Priority_Low);
      F.Reset_Property (Accessible_Property_Label);
      F.Reset_Relation (Accessible_Relation_Labelled_By);
      F.Reset_State (Accessible_State_Busy);
      F.Update_Platform_State (Accessible_Platform_State_Focusable);
      F.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (F.Get_Accessible_Parent = Null_Gtk_Accessible);
      F.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (F.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  Converting back from each interface must preserve the native object.
      Assert_True (-Accessible = F);
      Assert_Cmpstr_Eq (Gtk.Buildable.Get_Buildable_Id (Buildable), "");
      Assert_True (-Buildable = F);
      Assert_True (-Target = F);
      Unref (F);
   end Test_Accessibility_And_Interfaces;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func ("/frame/constructors", Test_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func ("/frame/labels-children", Test_Labels_And_Children'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/frame/accessibility-interfaces",
      Test_Accessibility_And_Interfaces'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Frame;
