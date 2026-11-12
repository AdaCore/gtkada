--  Exercise GtkScaleButton constructors, value and adjustment behavior,
--  accessibility wrappers, and typed plus slot-based signal callbacks.
with Ada.Command_Line;
with GNAT.Strings;
with Gtk.Widget;        use Gtk.Widget;
with Glib;              use Glib;
with Glib.Main;
with Glib.Object;       use Glib.Object;
with Glib.Properties;   use Glib.Properties;
with Glib.Test;         use Glib.Test;
with Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Adjustment;    use Gtk.Adjustment;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Main;
with Gtk.Scale;         use Gtk.Scale;
with Gtk.Scale_Button;  use Gtk.Scale_Button;
with Gtk.Scrollbar;     use Gtk.Scrollbar;
with Gtk.Window;        use Gtk.Window;
with Gtkada.Types;
with Gtk.Volume_Button; use Gtk.Volume_Button;
with System;

procedure Scale_Button is
   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Accessible.Gtk_Accessible_Role;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type System.Address;

   Calls : Gint := 0;
   Seen  : Gdouble := 0.0;
   Popup_Calls         : Natural := 0;
   Popdown_Calls       : Natural := 0;
   Slot_Popup_Calls    : Natural := 0;
   Slot_Popdown_Calls  : Natural := 0;
   Slot_Changed_Calls  : Natural := 0;
   Expected_Object     : System.Address := System.Null_Address;

   --  Emit action signals directly so the test needs no pointer or key input.
   procedure Emit_Void_Signal
     (Object : System.Address;
      Name   : Gtkada.Types.Chars_Ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Changed
     (Self : access Gtk_Scale_Button_Record'Class; Value : Gdouble) is
   begin
      Assert_Cmpfloat_Eq (Self.Get_Value, Value);
      Calls := Calls + 1;
      Seen := Value;
   end Changed;

   procedure Test_Shared_Adjustment with Convention => C;
   procedure Test_Volume with Convention => C;
   procedure Test_Volume_Accessibility with Convention => C;
   procedure Test_Constructors_And_Accessibility with Convention => C;
   procedure Test_Signals with Convention => C;

   procedure Typed_Popup (Self : access Gtk_Scale_Button_Record'Class);
   procedure Typed_Popdown (Self : access Gtk_Scale_Button_Record'Class);
   procedure Slot_Popup (Self : access Glib.Object.GObject_Record'Class);
   procedure Slot_Popdown (Self : access Glib.Object.GObject_Record'Class);
   procedure Slot_Changed
     (Self : access Glib.Object.GObject_Record'Class; Value : Gdouble);

   --  Typed callbacks receive the emitting scale button; slot callbacks
   --  receive the object supplied as the connection's slot.
   procedure Typed_Popup (Self : access Gtk_Scale_Button_Record'Class) is
   begin
      Assert_True (Get_Object (Self) = Expected_Object);
      Popup_Calls := Popup_Calls + 1;
   end Typed_Popup;

   procedure Typed_Popdown (Self : access Gtk_Scale_Button_Record'Class) is
   begin
      Assert_True (Get_Object (Self) = Expected_Object);
      Popdown_Calls := Popdown_Calls + 1;
   end Typed_Popdown;

   procedure Slot_Popup (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Assert_True (Get_Object (Self) = Expected_Object);
      Slot_Popup_Calls := Slot_Popup_Calls + 1;
   end Slot_Popup;

   procedure Slot_Popdown (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Assert_True (Get_Object (Self) = Expected_Object);
      Slot_Popdown_Calls := Slot_Popdown_Calls + 1;
   end Slot_Popdown;

   procedure Slot_Changed
     (Self : access Glib.Object.GObject_Record'Class; Value : Gdouble) is
   begin
      Assert_True (Get_Object (Self) = Expected_Object);
      Assert_Cmpfloat_Eq (Value, 37.0);
      Slot_Changed_Calls := Slot_Changed_Calls + 1;
   end Slot_Changed;

   procedure Test_Shared_Adjustment is
      B : Gtk_Scale_Button;
      S : Gtk_Scale;
      R : Gtk_Scrollbar;
      A : Gtk_Adjustment;
      Icons : GNAT.Strings.String_List :=
        (new String'("audio-volume-muted-symbolic"),
         new String'("audio-volume-high-symbolic"),
         new String'("audio-volume-low-symbolic"));
   begin
      Gtk_New (B, 0.0, 100.0, 2.0, Icons);
      B.Ref_Sink;
      A := B.Get_Adjustment;
      Assert_Cmpfloat_Eq (A.Get_Upper, 100.0);
      Assert_Cmpfloat_Eq (A.Get_Step_Increment, 2.0);
      B.Set_Icons (Icons (2 .. 2));
      for Icon of Icons loop
         GNAT.Strings.Free (Icon);
      end loop;
      B.Set_Icons ((1 .. 0 => null));
      Gtk_New (A, 20.0, 0.0, 100.0, 1.0, 10.0, 0.0);
      B.Set_Adjustment (A);
      Assert_True (B.Get_Adjustment = A);
      Gtk_New (S, Orientation_Horizontal, A);
      S.Ref_Sink;
      Gtk_New (R, Orientation_Horizontal, A);
      R.Ref_Sink;
      Assert_True (R.Get_Adjustment = A);
      B.On_Value_Changed (Changed'Unrestricted_Access);
      Calls := 0;
      S.Set_Value (40.0);
      Assert_Cmpfloat_Eq (B.Get_Value, 40.0);
      Assert_Cmpfloat_Eq (Seen, 40.0);
      Assert_Cmpint_Eq (Calls, 1);
      B.Set_Value (40.0);
      Assert_Cmpint_Eq (Calls, 1);
      B.Set_Value (200.0);
      Assert_Cmpfloat_Eq (Seen, 100.0);
      Assert_Cmpfloat_Eq (R.Get_Adjustment.Get_Value, 100.0);
      Assert_Cmpfloat_Eq (S.Get_Value, 100.0);
      Assert_Cmpint_Eq (Calls, 2);
      B.Set_Orientation (Orientation_Horizontal);
      Assert_True (B.Get_Orientation = Orientation_Horizontal);
      Assert_True (B.Get_Plus_Button /= null);
      Assert_True (B.Get_Minus_Button /= null);
      Assert_True (B.Get_Popup /= null);
      B.Unref;
      S.Unref;
      R.Unref;
   end Test_Shared_Adjustment;

   procedure Test_Volume is
      V : Gtk_Volume_Button;
      B : Gtk_Scale_Button;
   begin
      Gtk_New (V);
      V.Ref_Sink;
      Assert_Cmpfloat_Eq (V.Get_Adjustment.Get_Lower, 0.0);
      Assert_Cmpfloat_Eq (V.Get_Adjustment.Get_Upper, 1.0);
      Assert_Cmpfloat_Eq (V.Get_Adjustment.Get_Step_Increment, 0.02);
      V.Set_Value (0.5);
      Assert_Cmpfloat_Eq (V.Get_Value, 0.5);
      Set_Property (V, Use_Symbolic_Property, False);
      Assert_False (Get_Property (V, Use_Symbolic_Property));
      V.Unref;
      --  Nullable icons are also accepted by the scale button constructor.
      Gtk_New (B, 0.0, 1.0, 0.1, (1 .. 0 => null));
      B.Ref_Sink;
      B.Set_Value (0.75);
      Assert_Cmpfloat_Eq (B.Get_Value, 0.75);
      B.Unref;
   end Test_Volume;

   --------------------------------
   -- Test_Volume_Accessibility --
   --------------------------------

   procedure Test_Volume_Accessibility is
      By_Function : constant Gtk_Volume_Button := Gtk_Volume_Button_New;
      By_Initialize : constant Gtk_Volume_Button :=
        new Gtk_Volume_Button_Record;
      Parent  : constant Gtk_Window := Gtk_Window_New;
      Sibling : constant Gtk_Window := Gtk_Window_New;
      X, Y, Width, Height : Gint;
      Role : Gtk.Accessible.Gtk_Accessible_Role;
   begin
      --  Exercise all three public construction paths and the Initialize
      --  no-op contract after the Ada record already owns a GTK object.
      By_Function.Ref_Sink;
      Gtk.Volume_Button.Initialize (By_Initialize);
      By_Initialize.Ref_Sink;
      Gtk.Volume_Button.Initialize (By_Initialize);
      Parent.Ref_Sink;
      Sibling.Ref_Sink;

      --  GtkVolumeButton uses the documented 0.0 .. 1.0 volume range and
      --  exposes a nonempty accessible role and stable class identifier.
      Assert_Cmpfloat_Eq (By_Function.Get_Adjustment.Get_Lower, 0.0);
      Assert_Cmpfloat_Eq (By_Function.Get_Adjustment.Get_Upper, 1.0);
      Assert_Cmpfloat_Eq
        (By_Function.Get_Adjustment.Get_Step_Increment, 0.02);
      Role := By_Function.Get_Accessible_Role;
      Assert_True (Role /= Gtk.Accessible.Accessible_Role_None);
      Assert_Cmpstr_Eq (By_Function.Get_Accessible_Id, "GtkVolumeButton");

      --  A detached volume button is its own accessible-tree root. Its
      --  internal scale controls are exposed as accessible children.
      Assert_True
        (By_Function.Get_Accessible_Parent =
           Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (By_Function.Get_First_Accessible_Child /=
           Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (By_Function.Get_Next_Accessible_Sibling =
           Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (By_Function.Get_At_Context = null);
      Assert_False (By_Function.Get_Bounds (X, Y, Width, Height));

      --  The interface wrappers accept accessible peers, while a detached
      --  GtkWidget continues to report the tree determined by its hierarchy.
      By_Function.Set_Accessible_Parent
        (Gtk.Window.Implements_Gtk_Accessible.To_Interface (Parent),
         Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (By_Function.Get_Accessible_Parent =
           Gtk.Accessible.Null_Gtk_Accessible);
      By_Function.Update_Next_Accessible_Sibling
        (Gtk.Window.Implements_Gtk_Accessible.To_Interface (Sibling));
      Assert_True
        (By_Function.Get_Next_Accessible_Sibling =
           Gtk.Accessible.Null_Gtk_Accessible);
      By_Function.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      By_Function.Update_Next_Accessible_Sibling
        (Gtk.Accessible.Null_Gtk_Accessible);

      --  Reset and platform-state calls are valid without an active
      --  assistive technology; the fixed widget role remains unchanged.
      By_Function.Reset_Property
        (Gtk.Accessible.Accessible_Property_Label);
      By_Function.Reset_Relation
        (Gtk.Accessible.Accessible_Relation_Labelled_By);
      By_Function.Reset_State (Gtk.Accessible.Accessible_State_Selected);
      By_Function.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focusable);
      Assert_True (By_Function.Get_Accessible_Role = Role);
      Assert_False
        (By_Function.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focused));

      --  With GTK_A11Y=none this announcement may be a no-op, but it still
      --  accepts a UTF-8 message and priority without changing the widget.
      By_Function.Announce
        ("Volume level", Gtk.Accessible.Accessible_Announcement_Priority_Low);
      Assert_Cmpstr_Eq (By_Function.Get_Accessible_Id, "GtkVolumeButton");

      --  GtkVolumeButton also implements GtkOrientable through GtkScaleButton.
      By_Initialize.Set_Orientation (Orientation_Horizontal);
      Assert_True
        (By_Initialize.Get_Orientation = Orientation_Horizontal);

      By_Function.Unref;
      By_Initialize.Unref;
      Parent.Unref;
      Sibling.Unref;
   end Test_Volume_Accessibility;

   ------------------------------------------
   -- Test_Constructors_And_Accessibility --
   ------------------------------------------

   procedure Test_Constructors_And_Accessibility is
      By_Function : constant Gtk_Scale_Button :=
        Gtk_Scale_Button_New (0.0, 10.0, 1.0, (1 .. 0 => null));
      By_Initialize : constant Gtk_Scale_Button :=
        new Gtk_Scale_Button_Record;
      X, Y, Width, Height : Gint;
   begin
      By_Function.Ref_Sink;
      Gtk.Scale_Button.Initialize
        (By_Initialize, 0.0, 20.0, 2.0, (1 .. 0 => null));
      By_Initialize.Ref_Sink;
      --  Initialize is a no-op after the object already owns a GTK instance.
      Gtk.Scale_Button.Initialize
        (By_Initialize, 0.0, 99.0, 9.0, (1 .. 0 => null));

      Assert_Cmpfloat_Eq (By_Function.Get_Adjustment.Get_Upper, 10.0);
      Assert_Cmpfloat_Eq (By_Initialize.Get_Adjustment.Get_Upper, 20.0);
      Assert_False (By_Function.Get_Active);
      By_Function.Set_Has_Frame (False);
      Assert_False (By_Function.Get_Has_Frame);
      By_Function.Set_Has_Frame (True);
      Assert_True (By_Function.Get_Has_Frame);

      --  A scale button is exposed as an accessible range and has a private
      --  accessible child for its control buttons.
      Assert_Cmpfloat_Eq (By_Function.Get_Value, 0.0);
      Assert_True
        (By_Function.Get_Accessible_Role /= Gtk.Accessible.Accessible_Role_None);
      Assert_True
        (By_Function.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (By_Function.Get_First_Accessible_Child /= Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (By_Function.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_Cmpstr_Eq (By_Function.Get_Accessible_Id, "GtkScaleButton");
      Assert_True (By_Function.Get_At_Context = null);
      Assert_False (By_Function.Get_Bounds (X, Y, Width, Height));

      --  Set and reset operations accept nullable relationships on a
      --  standalone widget and do not require an assistive technology.
      By_Initialize.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      By_Initialize.Update_Next_Accessible_Sibling
        (Gtk.Accessible.Null_Gtk_Accessible);
      By_Initialize.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      By_Initialize.Reset_Relation
        (Gtk.Accessible.Accessible_Relation_Described_By);
      By_Initialize.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      Assert_False
        (By_Initialize.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focusable));
      Assert_False
        (By_Initialize.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focused));
      By_Initialize.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focusable);

      --  GTK_A11Y=none is used by the unattended driver, so announcements
      --  are unavailable there and are intentionally not emitted.
      By_Function.Unref;
      By_Initialize.Unref;
   end Test_Constructors_And_Accessibility;

   -------------------
   -- Test_Signals --
   -------------------

   procedure Test_Signals is
      B : constant Gtk_Scale_Button :=
        Gtk_Scale_Button_New (0.0, 100.0, 1.0, (1 .. 0 => null));
      Window : Gtk_Window := Gtk_Window_New;
      Popup_Name   : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("popup");
      Popdown_Name : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("popdown");
   begin
      B.Ref_Sink;
      --  Popup and popdown have GTK default handlers, so attach the button to
      --  a window before emitting those action signals.
      Window.Set_Child (B);
      Window.Present;
      for Step in 1 .. 10 loop
         if Glib.Main.Main_Context_Iteration (May_Block => False) then
            null;
         end if;
      end loop;
      Expected_Object := Get_Object (B);
      Popup_Calls := 0;
      Popdown_Calls := 0;
      Slot_Popup_Calls := 0;
      Slot_Popdown_Calls := 0;
      Slot_Changed_Calls := 0;

      B.On_Popup (Typed_Popup'Unrestricted_Access);
      B.On_Popup (Slot_Popup'Unrestricted_Access, B, After => True);
      B.On_Popdown (Typed_Popdown'Unrestricted_Access, After => True);
      B.On_Popdown (Slot_Popdown'Unrestricted_Access, B);
      B.On_Value_Changed (Changed'Unrestricted_Access);
      B.On_Value_Changed (Slot_Changed'Unrestricted_Access, B);

      Emit_Void_Signal (Expected_Object, Popup_Name);
      Emit_Void_Signal (Expected_Object, Popdown_Name);
      Assert_Cmpint_Eq (Gint (Popup_Calls), 1);
      Assert_Cmpint_Eq (Gint (Slot_Popup_Calls), 1);
      Assert_Cmpint_Eq (Gint (Popdown_Calls), 1);
      Assert_Cmpint_Eq (Gint (Slot_Popdown_Calls), 1);

      --  Set_Value emits the double-valued signal through both marshallers.
      Calls := 0;
      Seen := 0.0;
      B.Set_Value (37.0);
      Assert_Cmpint_Eq (Calls, 1);
      Assert_Cmpfloat_Eq (Seen, 37.0);
      Assert_Cmpint_Eq (Gint (Slot_Changed_Calls), 1);

      Gtkada.Types.Free (Popup_Name);
      Gtkada.Types.Free (Popdown_Name);
      Expected_Object := System.Null_Address;
      Window.Destroy;
      B.Unref;
   end Test_Signals;

begin
   Gtk.Main.Init;
   Glib.Test.Init;
   Add_Func ("/scale-button/shared-adjustment",
             Test_Shared_Adjustment'Unrestricted_Access);
   Add_Func ("/scale-button/volume", Test_Volume'Unrestricted_Access);
   Add_Func ("/scale-button/volume-accessibility",
             Test_Volume_Accessibility'Unrestricted_Access);
   Add_Func ("/scale-button/constructors-accessibility",
             Test_Constructors_And_Accessibility'Unrestricted_Access);
   Add_Func ("/scale-button/signals", Test_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Scale_Button;
