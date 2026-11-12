--  Headless tests for Gtk.Font_Dialog_Button constructors, properties,
--  accessible methods, interface conversions, and signal marshallers.
--  The matching C test is misc.c:test_font_dialog_button_new, although that
--  C function currently calls the color dialog button constructor by mistake.

with Ada.Command_Line;
with Glib;                    use Glib;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Test;               use Glib.Test;
with Gtk.Accessible;          use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Constraint_Target;
with Gtk.Font_Dialog;         use Gtk.Font_Dialog;
with Gtk.Font_Dialog_Button;  use Gtk.Font_Dialog_Button;
with Gtk.Main;
with Gtk.Widget;
with Gtkada.Types;            use Gtkada.Types;
with Pango.Font;
with Pango.Language;
with System;

use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Atcontext.Gtk_Atcontext;
use type Gtk.Buildable.Gtk_Buildable;
use type Gtk.Constraint_Target.Gtk_Constraint_Target;

procedure Font_Dialog_Button is
   Typed_Activations : Natural := 0;
   Slot_Activations  : Natural := 0;
   --  The slot form receives a separate object supplied to On_Activate.
   Slot_Object       : Gtk_Font_Dialog_Button;

   --  GtkAda uses a small C helper to emit a void signal by name.
   procedure Emit_Void (Object : System.Address; Name : Chars_Ptr);
   pragma Import (C, Emit_Void, "ada_g_signal_emit_by_name");

   procedure Test_Constructors_And_Settings with Convention => C;
   procedure Test_Accessibility_And_Interfaces with Convention => C;
   procedure Test_Activate_Signal with Convention => C;
   procedure On_Activate_Typed
     (Self : access Gtk_Font_Dialog_Button_Record'Class);
   procedure On_Activate_Slot
     (Self : access GObject_Record'Class);

   --  Verify the typed marshaller passes the font button that emitted activate.
   procedure On_Activate_Typed
     (Self : access Gtk_Font_Dialog_Button_Record'Class) is
   begin
      Assert_Nonnull (Self.all'Address);
      Typed_Activations := Typed_Activations + 1;
   end On_Activate_Typed;

   --  Verify the GObject slot marshaller passes the registered slot object.
   procedure On_Activate_Slot
     (Self : access GObject_Record'Class) is
   begin
      Assert_True (Self = GObject (Slot_Object));
      Slot_Activations := Slot_Activations + 1;
   end On_Activate_Slot;

   --  Both constructor overloads accept a null dialog; setters then install
   --  a dialog and round-trip the font-specific values through GTK.
   procedure Test_Constructors_And_Settings is
      By_Procedure : Gtk_Font_Dialog_Button;
      By_Function  : constant Gtk_Font_Dialog_Button :=
        Gtk_Font_Dialog_Button_New (null);
      Dialog       : constant Gtk_Font_Dialog := Gtk_Font_Dialog_New;
      Dialog2      : constant Gtk_Font_Dialog := Gtk_Font_Dialog_New;
      Font         : Pango.Font.Pango_Font_Description :=
        Pango.Font.From_String ("Monospace Bold 13");
      Read_Back    : Pango.Font.Pango_Font_Description;
      Language     : constant Pango.Language.Pango_Language :=
        Pango.Language.From_String ("en_US");
   begin
      Gtk_New (By_Procedure, null);
      Ref_Sink (By_Procedure);
      Ref_Sink (By_Function);

      Assert_Nonnull (By_Procedure.all'Address);
      Assert_Nonnull (By_Function.all'Address);
      Assert_True (By_Procedure.Get_Dialog = null);
      Assert_True (By_Function.Get_Dialog = null);

      --  Initialize is idempotent once the constructor has created the object.
      By_Procedure.Initialize (null);
      By_Procedure.Set_Dialog (Dialog);
      Assert_True (By_Procedure.Get_Dialog = Dialog);
      By_Procedure.Set_Dialog (Dialog2);
      Assert_True (By_Procedure.Get_Dialog = Dialog2);

      By_Procedure.Set_Font_Desc (Font);
      Read_Back := By_Procedure.Get_Font_Desc;
      Assert_Cmpstr_Eq (Pango.Font.Get_Family (Read_Back), "Monospace");
      --  Get_Font_Desc returns GTK's borrowed boxed value; only the parsed
      --  input description below belongs to this test and is freed here.
      Pango.Font.Free (Font);

      --  The default empty value exercises the binding's null-pointer branch;
      --  the nonempty value checks allocation, forwarding, and cleanup.
      By_Procedure.Set_Font_Features;
      Assert_Cmpstr_Eq (By_Procedure.Get_Font_Features, "");
      By_Procedure.Set_Font_Features ("kern=1");
      Assert_Cmpstr_Eq (By_Procedure.Get_Font_Features, "kern=1");
      Assert_Cmpstr_Eq
        (Get_Property (By_Procedure, Font_Features_Property), "kern=1");

      By_Procedure.Set_Language (Language);
      Assert_Cmpstr_Eq
        (Pango.Language.To_String (By_Procedure.Get_Language), "en-us");

      --  Check each legal enum value so the C enum conversion is exercised.
      for Level in Gtk_Font_Level loop
         By_Procedure.Set_Level (Level);
         Assert_True (By_Procedure.Get_Level = Level);
         Assert_True (Get_Property (By_Procedure, Level_Property) = Level);
      end loop;

      By_Procedure.Set_Use_Font (True);
      Assert_True (By_Procedure.Get_Use_Font);
      Assert_True (Get_Property (By_Procedure, Use_Font_Property));
      By_Procedure.Set_Use_Font (False);
      Assert_False (By_Procedure.Get_Use_Font);
      Assert_False (Get_Property (By_Procedure, Use_Font_Property));

      By_Procedure.Set_Use_Size (True);
      Assert_True (By_Procedure.Get_Use_Size);
      Assert_True (Get_Property (By_Procedure, Use_Size_Property));
      By_Procedure.Set_Use_Size (False);
      Assert_False (By_Procedure.Get_Use_Size);
      Assert_False (Get_Property (By_Procedure, Use_Size_Property));

      Unref (By_Procedure);
      Unref (By_Function);
      Unref (Dialog);
      Unref (Dialog2);
   end Test_Constructors_And_Settings;

   --  Accessibility queries on an unrooted widget have deterministic empty
   --  parent/sibling, no layout bounds, and no platform accessibility state.
   procedure Test_Accessibility_And_Interfaces is
      Button : constant Gtk_Font_Dialog_Button :=
        Gtk_Font_Dialog_Button_New (null);
      Parent  : Gtk_Accessible;
      Next    : Gtk_Accessible;
      First   : Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
      Has_Bounds : Boolean;
      Accessible : constant Gtk_Accessible := +Button;
      Buildable : constant Gtk.Buildable.Gtk_Buildable := +Button;
      Constraint : constant Gtk.Constraint_Target.Gtk_Constraint_Target := +Button;
   begin
      Ref_Sink (Button);
      Assert_True (Button.Get_Accessible_Role /= Accessible_Role_None);
      Assert_Cmpstr_Eq (Button.Get_Accessible_Id, "");
      Assert_True (Button.Get_Accessible_Parent = Null_Gtk_Accessible);
      Assert_True (Button.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      First := Button.Get_First_Accessible_Child;
      Assert_True (First /= Null_Gtk_Accessible);
      Context := Button.Get_At_Context;
      Assert_True (Context = null);
      Has_Bounds := Button.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);
      Assert_False
        (Button.Get_Platform_State (Accessible_Platform_State_Focused));

      --  Null accessible relationships keep the test independent of a parent
      --  widget tree or an accessibility backend.
      Button.Announce ("font button test", Accessible_Announcement_Priority_Low);
      Button.Reset_Property (Accessible_Property_Label);
      Button.Reset_Relation (Accessible_Relation_Labelled_By);
      Button.Reset_State (Accessible_State_Busy);
      Button.Update_Platform_State (Accessible_Platform_State_Focusable);
      Button.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Parent := Button.Get_Accessible_Parent;
      Assert_True (Parent = Null_Gtk_Accessible);
      Button.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Next := Button.Get_Next_Accessible_Sibling;
      Assert_True (Next = Null_Gtk_Accessible);

      --  Round-trip all interface conversions declared by this generated type.
      Assert_True (-Accessible = Button);
      Assert_True (-Buildable = Button);
      Assert_True (-Constraint = Button);
      Unref (Button);
   end Test_Accessibility_And_Interfaces;

   --  Emitting activate with no dialog attached exercises both callback
   --  marshallers without presenting a chooser or requiring user interaction.
   procedure Test_Activate_Signal is
      Button : constant Gtk_Font_Dialog_Button :=
        Gtk_Font_Dialog_Button_New (null);
      Signal_Name : Chars_Ptr;
   begin
      Ref_Sink (Button);
      Slot_Object := Button;
      Typed_Activations := 0;
      Slot_Activations := 0;
      Button.On_Activate (On_Activate_Typed'Unrestricted_Access);
      Button.On_Activate (On_Activate_Slot'Unrestricted_Access, Button);

      Signal_Name := New_String ("activate");
      Emit_Void (Button.Get_Object, Signal_Name);
      Free (Signal_Name);
      Assert_Cmpuint_Eq (Guint (Typed_Activations), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Activations), 1);
      Unref (Button);
   end Test_Activate_Signal;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/font-dialog-button/constructors-settings",
      Test_Constructors_And_Settings'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/font-dialog-button/accessibility-interfaces",
      Test_Accessibility_And_Interfaces'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/font-dialog-button/activate-signal",
      Test_Activate_Signal'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Font_Dialog_Button;
