--  Exercise Gtk.Expander constructors, child and label properties, its
--  inherited accessibility API, interface conversions, and activate signals.
--  GTK 4.22.2 has no GtkExpander C test to port, so these cases check the Ada
--  binding directly against Gtk's documented widget behavior.

with Ada.Command_Line;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Test;             use Glib.Test;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Constraint_Target;
with Gtk.Label;             use Gtk.Label;
with Gtk.Main;
with Gtk.Expander;          use Gtk.Expander;
with Gtk.Widget;
with Gtkada.Types;
with System;

use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Atcontext.Gtk_Atcontext;
use type Gtk.Buildable.Gtk_Buildable;
use type Gtk.Constraint_Target.Gtk_Constraint_Target;
use type Gtk.Expander.Gtk_Expander;
use type Gtk.Widget.Gtk_Widget;
use type System.Address;

procedure Main is
   Typed_Activations : Natural := 0;
   Slot_Activations  : Natural := 0;
   Slot_Address      : System.Address := System.Null_Address;

   --  g_signal_emit_by_name is variadic; this prefix emits the no-argument
   --  activate signal without depending on a user pressing the expander.
   procedure Emit_Activate
     (Object : System.Address; Name : Gtkada.Types.Chars_Ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Typed_Activate (Self : access Gtk_Expander_Record'Class);
   procedure Slot_Activate (Self : access GObject_Record'Class);
   procedure Test_Constructors_And_Properties with Convention => C;
   procedure Test_Accessibility_And_Interfaces with Convention => C;
   procedure Test_Activate_Signal with Convention => C;

   --  The typed callback confirms GtkAda passes the expander instance.
   procedure Typed_Activate (Self : access Gtk_Expander_Record'Class) is
   begin
      Assert_True (Self /= null);
      Typed_Activations := Typed_Activations + 1;
   end Typed_Activate;

   --  The slot overload receives a general GObject and keeps its own closure
   --  data; checking the object identity exercises the generated marshaller.
   procedure Slot_Activate (Self : access GObject_Record'Class) is
   begin
      Assert_True (Self /= null);
      Slot_Address := Self.Get_Object;
      Slot_Activations := Slot_Activations + 1;
   end Slot_Activate;

   --  Each constructor overload is checked, including the empty-label path.
   --  Calling Initialize again confirms its documented already-created no-op.
   procedure Test_Constructors_And_Properties is
      By_Function : constant Gtk_Expander := Gtk_Expander_New ("Details");
      By_Mnemonic : constant Gtk_Expander :=
        Gtk_Expander_New_With_Mnemonic ("_Options");
      Empty : constant Gtk_Expander := Gtk_Expander_New;
      Empty_Mnemonic : constant Gtk_Expander :=
        Gtk_Expander_New_With_Mnemonic;
      By_Procedure : Gtk_Expander;
      By_Mnemonic_Procedure : Gtk_Expander;
      Child : constant Gtk_Label := Gtk_Label_New ("expanded content");
      Label_Widget : constant Gtk_Label := Gtk_Label_New ("custom title");
   begin
      Ref_Sink (By_Function);
      Ref_Sink (By_Mnemonic);
      Ref_Sink (Empty);
      Ref_Sink (Empty_Mnemonic);
      Assert_Cmpstr_Eq (By_Function.Get_Label, "Details");
      Assert_Cmpstr_Eq (By_Mnemonic.Get_Label, "_Options");
      Assert_Cmpstr_Eq (Empty.Get_Label, "");
      Assert_Cmpstr_Eq (Empty_Mnemonic.Get_Label, "");

      --  The out-parameter constructor variants create equivalent widgets.
      Gtk_New (By_Procedure, "Procedure");
      Gtk_New_With_Mnemonic (By_Mnemonic_Procedure, "_Procedure");
      Ref_Sink (By_Procedure);
      Ref_Sink (By_Mnemonic_Procedure);
      Assert_Cmpstr_Eq (By_Procedure.Get_Label, "Procedure");
      Assert_Cmpstr_Eq (By_Mnemonic_Procedure.Get_Label, "_Procedure");
      By_Procedure.Initialize ("ignored after creation");
      By_Mnemonic_Procedure.Initialize_With_Mnemonic ("_ignored");
      Assert_Cmpstr_Eq (By_Procedure.Get_Label, "Procedure");
      Assert_Cmpstr_Eq (By_Mnemonic_Procedure.Get_Label, "_Procedure");

      --  Widget children are borrowed by getters; compare their object handles.
      By_Function.Set_Child (Child);
      Assert_True (By_Function.Get_Child.Get_Object = Child.Get_Object);
      Assert_True (Get_Property (By_Function, Child_Property).Get_Object
                   = Child.Get_Object);
      By_Function.Set_Expanded (True);
      Assert_True (By_Function.Get_Expanded);
      Assert_True (Get_Property (By_Function, Expanded_Property));
      --  Both collapsed and expanded states exercise GTK's Boolean mapping.
      By_Function.Set_Expanded (False);
      Assert_False (By_Function.Get_Expanded);
      Set_Property (By_Function, Expanded_Property, True);
      Assert_True (By_Function.Get_Expanded);
      By_Function.Set_Child (null);
      Assert_True (By_Function.Get_Child = null);

      --  The separate label widget replaces the ordinary text label.
      By_Function.Set_Label_Widget (Label_Widget);
      Assert_True (By_Function.Get_Label_Widget.Get_Object
                   = Label_Widget.Get_Object);
      Assert_True (Get_Property (By_Function, Label_Widget_Property).Get_Object
                   = Label_Widget.Get_Object);
      --  Setting text after a custom widget restores the text label path.
      By_Function.Set_Label ("Replacement");
      Assert_Cmpstr_Eq (By_Function.Get_Label, "Replacement");
      Assert_Cmpstr_Eq
        (Get_Property (By_Function, Gtk.Expander.Label_Property), "Replacement");
      By_Function.Set_Label ("");
      Assert_Cmpstr_Eq (By_Function.Get_Label, "");

      By_Function.Set_Resize_Toplevel (True);
      Assert_True (By_Function.Get_Resize_Toplevel);
      Assert_True (Get_Property (By_Function, Resize_Toplevel_Property));
      By_Function.Set_Resize_Toplevel (False);
      Assert_False (By_Function.Get_Resize_Toplevel);
      By_Function.Set_Use_Markup (True);
      Assert_True (By_Function.Get_Use_Markup);
      Assert_True (Get_Property (By_Function, Gtk.Expander.Use_Markup_Property));
      By_Function.Set_Use_Markup (False);
      Assert_False (By_Function.Get_Use_Markup);
      By_Function.Set_Use_Underline (True);
      Assert_True (By_Function.Get_Use_Underline);
      Assert_True
        (Get_Property (By_Function, Gtk.Expander.Use_Underline_Property));
      By_Function.Set_Use_Underline (False);
      Assert_False (By_Function.Get_Use_Underline);

      Unref (By_Function);
      Unref (By_Mnemonic);
      Unref (Empty);
      Unref (Empty_Mnemonic);
      Unref (By_Procedure);
      Unref (By_Mnemonic_Procedure);
   end Test_Constructors_And_Properties;

   --  Accessibility getters return the documented role and null screen
   --  context/bounds for an unrooted widget, without requiring a real display.
   --  The expander implements three interfaces; every conversion pair keeps
   --  the same object identity when converted back.
   procedure Test_Accessibility_And_Interfaces is
      Expander : constant Gtk_Expander := Gtk_Expander_New ("Accessible");
      Child    : constant Gtk_Label := Gtk_Label_New ("child");
      Accessible : constant Gtk_Accessible := +Expander;
      Buildable : constant Gtk.Buildable.Gtk_Buildable := +Expander;
      Target : constant Gtk.Constraint_Target.Gtk_Constraint_Target := +Expander;
      Parent  : Gtk_Accessible;
      Next    : Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
      Has_Bounds : Boolean;
   begin
      Ref_Sink (Expander);
      Expander.Set_Child (Child);
      Expander.Set_Expanded (True);
      Assert_True (Expander.Get_Accessible_Role = Accessible_Role_Button);
      Assert_Cmpstr_Eq (Expander.Get_Accessible_Id, "");
      Assert_True (Expander.Get_First_Accessible_Child /= Null_Gtk_Accessible);
      Context := Expander.Get_At_Context;
      Assert_True (Context = null);
      Has_Bounds := Expander.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);
      Parent := Expander.Get_Accessible_Parent;
      Next := Expander.Get_Next_Accessible_Sibling;
      Assert_True (Parent = Null_Gtk_Accessible);
      Assert_True (Next = Null_Gtk_Accessible);
      Assert_False (Expander.Get_Platform_State (Accessible_Platform_State_Focused));

      --  These updates are valid on a detached widget and are safe in Xvfb.
      Expander.Announce ("Expander ready", Accessible_Announcement_Priority_Low);
      Expander.Reset_Property (Accessible_Property_Label);
      Expander.Reset_Relation (Accessible_Relation_Labelled_By);
      Expander.Reset_State (Accessible_State_Busy);
      Expander.Update_Platform_State (Accessible_Platform_State_Focusable);
      Expander.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Expander.Get_Accessible_Parent = Null_Gtk_Accessible);
      Expander.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (Expander.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      Assert_True (-Accessible = Expander);
      --  A fresh object has no ID assigned by a GtkBuilder document.
      Assert_Cmpstr_Eq (Gtk.Buildable.Get_Buildable_Id (Buildable), "");
      Assert_True (-Buildable = Expander);
      --  Exercise Gtk.Buildable's identity operator on an existing interface
      --  value; the widget-to-interface conversion above uses another one.
      Assert_True (Gtk.Buildable."+" (Buildable) = Buildable);
      Assert_True (-Target = Expander);
      Unref (Expander);
   end Test_Accessibility_And_Interfaces;

   --  Emit the signal on separate instances so both generated callback
   --  marshallers run once, one with a typed callback and one with a slot.
   procedure Test_Activate_Signal is
      Typed : constant Gtk_Expander := Gtk_Expander_New ("typed");
      Slotted : constant Gtk_Expander := Gtk_Expander_New ("slotted");
      Name : Gtkada.Types.Chars_Ptr;
   begin
      Ref_Sink (Typed);
      Ref_Sink (Slotted);
      Typed_Activations := 0;
      Slot_Activations := 0;
      Slot_Address := System.Null_Address;
      Assert_Cmpstr_Eq (UTF8_String (Signal_Activate), "activate");
      Typed.On_Activate (Typed_Activate'Unrestricted_Access);
      Slotted.On_Activate (Slot_Activate'Unrestricted_Access, Slotted);
      --  Keep the signal name allocation alive until both emissions finish.
      Name := Gtkada.Types.New_String ("activate");
      Emit_Activate (Typed.Get_Object, Name);
      Emit_Activate (Slotted.Get_Object, Name);
      Gtkada.Types.Free (Name);
      Assert_Cmpuint_Eq (Guint (Typed_Activations), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Activations), 1);
      Assert_True (Slot_Address = Slotted.Get_Object);

      Unref (Typed);
      Unref (Slotted);
   end Test_Activate_Signal;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/expander/constructors-properties",
             Test_Constructors_And_Properties'Unrestricted_Access);
   Add_Func ("/expander/accessibility-interfaces",
             Test_Accessibility_And_Interfaces'Unrestricted_Access);
   Add_Func ("/expander/activate-signal",
             Test_Activate_Signal'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
