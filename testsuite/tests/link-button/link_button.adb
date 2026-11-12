--  GTK has no testsuite/gtk/linkbutton.c to port, so these cases are ours:
--  they cover the surface the Gtk.Link_Button binding adds, and no more.

with Ada.Command_Line;
with System;
with Glib;            use Glib;
with Glib.Test;       use Glib.Test;
with Glib.Object;     use Glib.Object;
with Glib.Variant;    use Glib.Variant;
with Gtk.Accessible;  use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Button;      use Gtk.Button;
with Gtk.Link_Button; use Gtk.Link_Button;
with Gtk.Main;
with Gtkada.Types;

procedure Link_Button is

   Home : constant String := "http://www.example.com/";
   Away : constant String := "http://www.adacore.com/";

   --  The counters let the signal test verify that GTK called both handlers.
   Typed_Count : Natural := 0;
   Slot_Count  : Natural := 0;
   Typed_Object : System.Address := System.Null_Address;
   Slot_Object  : System.Address := System.Null_Address;

   use type System.Address;
   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Glib.Variant.Gvariant;
   use type Glib.Signal_Name;

   procedure Test_Uri
   with Convention => C;

   procedure Test_Visited
   with Convention => C;

   procedure Test_Label
   with Convention => C;

   procedure Test_Procedure_Constructors
   with Convention => C;

   procedure Test_Accessibility
   with Convention => C;

   procedure Test_Actionable
   with Convention => C;

   procedure Test_Activate_Link
   with Convention => C;

   --  Emit the boolean signal synchronously so its callback runs without
   --  opening a browser or depending on GtkButton's press animation timer.
   procedure Emit_Activate_Link
     (Object       : System.Address;
      Name         : Gtkada.Types.Chars_Ptr;
      Return_Value : access Glib.Gboolean);
   pragma Import (C, Emit_Activate_Link, "g_signal_emit_by_name");

   function On_Activate_Link
     (Self : access Gtk_Link_Button_Record'Class) return Boolean;

   function On_Activate_Link_Slot
     (Self : access Glib.Object.GObject_Record'Class) return Boolean;

   --  There is deliberately no activate-link case. Nothing in the gtk4
   --  binding emits the signal synchronously: gtk_button_clicked is gone,
   --  and Gtk.Widget.Activate only starts GtkButton's press animation, which
   --  emits "clicked" from a 250ms timeout. Driving it would mean spinning
   --  the main loop on a timer, which buys less than it risks in flakiness.

   --------------
   -- Test_Uri --
   --------------

   procedure Test_Uri is
      B : constant Gtk_Link_Button := Gtk_Link_Button_New (Home);
   begin
      Assert_Cmpstr_Eq (B.Get_Uri, Home);

      B.Set_Uri (Away);
      Assert_Cmpstr_Eq (B.Get_Uri, Away);
   end Test_Uri;

   ------------------
   -- Test_Visited --
   ------------------

   procedure Test_Visited is
      B : constant Gtk_Link_Button := Gtk_Link_Button_New (Home);
   begin
      Assert_False (B.Get_Visited);

      B.Set_Visited (True);
      Assert_True (B.Get_Visited);

      --  Re-pointing the button is documented to unset "visited".
      B.Set_Uri (Away);
      Assert_False (B.Get_Visited);
   end Test_Visited;

   ----------------
   -- Test_Label --
   ----------------

   procedure Test_Label is
      Labelled : constant Gtk_Link_Button :=
        Gtk_Link_Button_New_With_Label (Home, "Click me");

      --  The label is nullable, and the binding maps the default "" onto
      --  NULL, which is what makes GTK fall back on the URI.
      Bare     : constant Gtk_Link_Button :=
        Gtk_Link_Button_New_With_Label (Home);
   begin
      Assert_Cmpstr_Eq (Labelled.Get_Uri, Home);
      Assert_Cmpstr_Eq (Labelled.Get_Label, "Click me");

      Assert_Cmpstr_Eq (Bare.Get_Uri, Home);
      Assert_Cmpstr_Eq (Bare.Get_Label, Home);
   end Test_Label;

   --------------------------------
   -- Test_Procedure_Constructors --
   --------------------------------

   procedure Test_Procedure_Constructors is
      By_Procedure : Gtk_Link_Button;
      By_Procedure_With_Label : Gtk_Link_Button;
   begin
      --  Exercise both Gtk_New overloads, which complement the function
      --  constructors covered by the URI and label tests above.
      Gtk.Link_Button.Gtk_New (By_Procedure, Home);
      Ref_Sink (By_Procedure);
      Assert_Cmpstr_Eq (By_Procedure.Get_Uri, Home);
      Gtk.Link_Button.Gtk_New_With_Label
        (By_Procedure_With_Label, Home, "procedure label");
      Ref_Sink (By_Procedure_With_Label);
      Assert_Cmpstr_Eq (By_Procedure_With_Label.Get_Uri, Home);
      Assert_Cmpstr_Eq
        (By_Procedure_With_Label.Get_Label, "procedure label");

      --  Initialize is documented as a no-op after the object already exists.
      Gtk.Link_Button.Initialize (By_Procedure, Away);
      Gtk.Link_Button.Initialize_With_Label
        (By_Procedure_With_Label, Away, "changed");
      Assert_Cmpstr_Eq (By_Procedure.Get_Uri, Home);
      Assert_Cmpstr_Eq (By_Procedure_With_Label.Get_Uri, Home);
      Assert_Cmpstr_Eq
        (By_Procedure_With_Label.Get_Label, "procedure label");
      Unref (By_Procedure);
      Unref (By_Procedure_With_Label);
   end Test_Procedure_Constructors;

   --------------------------
   -- Test_Accessibility --
   --------------------------

   procedure Test_Accessibility is
      Button : constant Gtk_Link_Button := Gtk_Link_Button_New (Home);
      First_Child : Gtk_Accessible;
      Parent : Gtk_Accessible;
      Next : Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
   begin
      Ref_Sink (Button);

      --  The detached button has link semantics and its URI label is exposed
      --  as an accessible child, while it has no parent, sibling, or AT context.
      Assert_True (Button.Get_Accessible_Role = Accessible_Role_Link);
      Assert_Cmpstr_Eq (Button.Get_Accessible_Id, "");
      First_Child := Button.Get_First_Accessible_Child;
      Assert_True (First_Child /= Null_Gtk_Accessible);
      Parent := Button.Get_Accessible_Parent;
      Next := Button.Get_Next_Accessible_Sibling;
      Assert_True (Parent = Null_Gtk_Accessible);
      Assert_True (Next = Null_Gtk_Accessible);
      Context := Button.Get_At_Context;
      Assert_True (Context = null);

      --  An unmapped widget has no screen bounds or focused platform state.
      Assert_False (Button.Get_Bounds (X, Y, Width, Height));
      Assert_False
        (Button.Get_Platform_State (Accessible_Platform_State_Focused));

      --  These notifications and resets are valid without a screen reader.
      Button.Announce ("link button test", Accessible_Announcement_Priority_Low);
      Button.Reset_Property (Accessible_Property_Label);
      Button.Reset_Relation (Accessible_Relation_Labelled_By);
      Button.Reset_State (Accessible_State_Busy);
      Button.Update_Platform_State (Accessible_Platform_State_Focusable);
      Button.Set_Accessible_Parent
        (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Button.Get_Accessible_Parent = Null_Gtk_Accessible);
      Button.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (Button.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      Assert_True (Button.Get_Accessible_Role = Accessible_Role_Link);
      Unref (Button);
   end Test_Accessibility;

   -----------------------
   -- Test_Actionable --
   -----------------------

   procedure Test_Actionable is
      Button : constant Gtk_Link_Button := Gtk_Link_Button_New (Home);
      Target : constant Gvariant := Gvariant_New_String ("open");
      Read_Back : Gvariant;
   begin
      Ref_Sink (Button);

      --  Actionable starts with neither an action name nor a target value.
      Assert_Cmpstr_Eq (Button.Get_Action_Name, "");
      Read_Back := Button.Get_Action_Target_Value;
      Assert_True (Read_Back = Null_Gvariant);

      --  Setters round-trip names and a string target through GtkActionable.
      Button.Set_Action_Name ("app.open");
      Assert_Cmpstr_Eq (Button.Get_Action_Name, "app.open");
      Button.Set_Action_Target_Value (Target);
      Read_Back := Button.Get_Action_Target_Value;
      Assert_True (Read_Back /= Null_Gvariant);
      Assert_Cmpstr_Eq (Get_String (Read_Back), "open");

      --  The detailed name replaces the name, and empty/null inputs clear it.
      Button.Set_Detailed_Action_Name ("app.follow");
      Assert_Cmpstr_Eq (Button.Get_Action_Name, "app.follow");
      Button.Set_Action_Name;
      Assert_Cmpstr_Eq (Button.Get_Action_Name, "");
      Button.Set_Action_Target_Value (Null_Gvariant);
      Assert_True (Button.Get_Action_Target_Value = Null_Gvariant);
      Unref (Button);
   end Test_Actionable;

   ---------------------------
   -- On_Activate_Link --
   ---------------------------

   function On_Activate_Link
     (Self : access Gtk_Link_Button_Record'Class) return Boolean is
   begin
      Typed_Count := Typed_Count + 1;
      Typed_Object := Self.Get_Object;
      return True;
   end On_Activate_Link;

   -------------------------------
   -- On_Activate_Link_Slot --
   -------------------------------

   function On_Activate_Link_Slot
     (Self : access Glib.Object.GObject_Record'Class) return Boolean is
   begin
      Slot_Count := Slot_Count + 1;
      Slot_Object := Self.Get_Object;
      return True;
   end On_Activate_Link_Slot;

   --------------------------
   -- Test_Activate_Link --
   --------------------------

   procedure Test_Activate_Link is
      Typed : constant Gtk_Link_Button := Gtk_Link_Button_New (Home);
      Slot_Button : constant Gtk_Link_Button := Gtk_Link_Button_New (Away);
      Name : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("activate-link");
      Result : aliased Glib.Gboolean := 0;
   begin
      Ref_Sink (Typed);
      Ref_Sink (Slot_Button);

      --  Returning True handles the activation locally, avoiding URI launch.
      Assert_True (Signal_Activate_Link = "activate-link");
      Typed.On_Activate_Link (On_Activate_Link'Unrestricted_Access);
      Emit_Activate_Link (Typed.Get_Object, Name, Result'Access);
      Assert_True (Result /= 0);
      Assert_True (Typed_Count = 1);
      Assert_True (Typed_Object = Typed.Get_Object);

      --  The slot overload receives its supplied GObject instead of the
      --  emitting button; direct signal emission also covers its marshaller.
      Slot_Button.On_Activate_Link
        (On_Activate_Link_Slot'Unrestricted_Access, Typed);
      Result := 0;
      Emit_Activate_Link (Slot_Button.Get_Object, Name, Result'Access);
      Assert_True (Result /= 0);
      Assert_True (Slot_Count = 1);
      Assert_True (Slot_Object = Typed.Get_Object);

      Gtkada.Types.Free (Name);
      Unref (Typed);
      Unref (Slot_Button);
   end Test_Activate_Link;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/link-button/uri", Test_Uri'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/link-button/visited", Test_Visited'Unrestricted_Access);
   Glib.Test.Add_Func ("/link-button/label", Test_Label'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/link-button/procedure-constructors",
      Test_Procedure_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/link-button/accessibility", Test_Accessibility'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/link-button/actionable", Test_Actionable'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/link-button/activate-link", Test_Activate_Link'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Link_Button;
