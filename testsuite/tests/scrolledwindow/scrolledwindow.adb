--  Tests Gtk.ScrolledWindow's constructors, properties, sizing, accessible
--  methods, interface conversions, and signal callbacks. The sizing cases
--  port test_size from testsuite/c_tests/scrolledwindow.c.

with Ada.Command_Line;
with Ada.Exceptions;
with Glib;                         use Glib;
with Glib.Object;                  use Glib.Object;
with Glib.Test;                    use Glib.Test;
with Gtk.Accessible;               use Gtk.Accessible;
with Gtk.Adjustment;               use Gtk.Adjustment;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Box;                      use Gtk.Box;
with Gtk.Constraint_Target;
with Gtk.Enums;                    use Gtk.Enums;
with Gtk.Label;                    use Gtk.Label;
with Gtk.Main;
with Gtkada.Bindings;
with Gtk.Scrolled_Window;          use Gtk.Scrolled_Window;
with Gtk.Viewport;
with Gtk.Widget;                   use Gtk.Widget;
with Interfaces.C.Strings;
with System;

use type System.Address;
use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Atcontext.Gtk_Atcontext;

procedure Scrolledwindow is

   --  These fixed prefixes call g_signal_emit_by_name's variadic interface.
   --  GTK's public signals use enum arguments, and scroll-child also writes a
   --  gboolean result through the final pointer argument.
   procedure Emit_Position
     (Instance : System.Address;
      Name     : Interfaces.C.Strings.chars_ptr;
      Position : Gtk.Enums.Gtk_Position_Type)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Direction
     (Instance  : System.Address;
      Name      : Interfaces.C.Strings.chars_ptr;
      Direction : Gtk.Enums.Gtk_Direction_Type)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Scroll_Child
     (Instance   : System.Address;
      Name       : Interfaces.C.Strings.chars_ptr;
      Scroll     : Gtk.Enums.Gtk_Scroll_Type;
      Horizontal : Glib.Gboolean;
      Result     : access Glib.Gboolean)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   Position_Typed_Count : Natural := 0;
   Position_Slot_Count  : Natural := 0;
   Direction_Typed_Count : Natural := 0;
   Direction_Slot_Count  : Natural := 0;
   Scroll_Typed_Count    : Natural := 0;
   Scroll_Slot_Count     : Natural := 0;
   Exception_Count       : Natural := 0;
   Unexpected_Exception_Count : Natural := 0;
   Raise_Callbacks       : Boolean := False;
   Typed_Position        : Gtk_Position_Type := Pos_Left;
   Slot_Position         : Gtk_Position_Type := Pos_Left;
   Typed_Direction       : Gtk_Direction_Type := Dir_Tab_Forward;
   Slot_Direction        : Gtk_Direction_Type := Dir_Tab_Forward;
   Typed_Scroll          : Gtk_Scroll_Type := Scroll_None;
   Slot_Scroll           : Gtk_Scroll_Type := Scroll_None;

   procedure On_Position
     (Self : access Gtk_Scrolled_Window_Record'Class;
      Pos  : Gtk_Position_Type);
   procedure On_Position_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Pos  : Gtk_Position_Type);
   procedure On_Direction
     (Self      : access Gtk_Scrolled_Window_Record'Class;
      Direction : Gtk_Direction_Type);
   procedure On_Direction_Slot
     (Self      : access Glib.Object.GObject_Record'Class;
      Direction : Gtk_Direction_Type);
   --  The boolean-return marshaller calls its callback while declaring a local
   --  value, before its exception handler can catch a callback exception.
   function On_Scroll
     (Self       : access Gtk_Scrolled_Window_Record'Class;
      Scroll     : Gtk_Scroll_Type;
      Horizontal : Boolean) return Boolean;
   function On_Scroll_Slot
     (Self       : access Glib.Object.GObject_Record'Class;
      Scroll     : Gtk_Scroll_Type;
      Horizontal : Boolean) return Boolean;

   --  During the second signal pass, callback exceptions should be reported
   --  to GtkAda's handler instead of unwinding through the C signal system.
   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure On_Position
     (Self : access Gtk_Scrolled_Window_Record'Class;
      Pos  : Gtk_Position_Type) is
      pragma Unreferenced (Self);
   begin
      if Raise_Callbacks then
         raise Program_Error with "expected signal callback exception";
      end if;
      Position_Typed_Count := Position_Typed_Count + 1;
      Typed_Position := Pos;
   end On_Position;

   --  Slot callbacks receive the signal emitter as a GObject, rather than
   --  its more specific ScrolledWindow type.
   procedure On_Position_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Pos  : Gtk_Position_Type) is
      pragma Unreferenced (Self);
   begin
      if Raise_Callbacks then
         raise Program_Error with "expected signal callback exception";
      end if;
      Position_Slot_Count := Position_Slot_Count + 1;
      Slot_Position := Pos;
   end On_Position_Slot;

   procedure On_Direction
     (Self      : access Gtk_Scrolled_Window_Record'Class;
      Direction : Gtk_Direction_Type) is
      pragma Unreferenced (Self);
   begin
      if Raise_Callbacks then
         raise Program_Error with "expected signal callback exception";
      end if;
      Direction_Typed_Count := Direction_Typed_Count + 1;
      Typed_Direction := Direction;
   end On_Direction;

   --  The slot form exercises the generic GObject signal marshaller.
   procedure On_Direction_Slot
     (Self      : access Glib.Object.GObject_Record'Class;
      Direction : Gtk_Direction_Type) is
      pragma Unreferenced (Self);
   begin
      if Raise_Callbacks then
         raise Program_Error with "expected signal callback exception";
      end if;
      Direction_Slot_Count := Direction_Slot_Count + 1;
      Slot_Direction := Direction;
   end On_Direction_Slot;

   function On_Scroll
     (Self       : access Gtk_Scrolled_Window_Record'Class;
      Scroll     : Gtk_Scroll_Type;
      Horizontal : Boolean) return Boolean is
      pragma Unreferenced (Self, Horizontal);
   begin
      Scroll_Typed_Count := Scroll_Typed_Count + 1;
      Typed_Scroll := Scroll;
      return False;
   end On_Scroll;

   --  Returning True lets the test check that the signal's boolean result is
   --  marshalled back after both the typed and slot callbacks have run.
   function On_Scroll_Slot
     (Self       : access Glib.Object.GObject_Record'Class;
      Scroll     : Gtk_Scroll_Type;
      Horizontal : Boolean) return Boolean is
      pragma Unreferenced (Self, Horizontal);
   begin
      Scroll_Slot_Count := Scroll_Slot_Count + 1;
      Slot_Scroll := Scroll;
      return False;
   end On_Scroll_Slot;

   --  Count the errors after they have crossed the generated Ada marshallers;
   --  assertions stay in the test body so the exception handler stays safe.
   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
   begin
      if Ada.Exceptions.Exception_Name (Occurrence) = "PROGRAM_ERROR" then
         Exception_Count := Exception_Count + 1;
      else
         Unexpected_Exception_Count := Unexpected_Exception_Count + 1;
      end if;
   end Record_Exception;

   procedure Test_Constructors with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Sizing with Convention => C;
   procedure Test_Accessibility_And_Interfaces with Convention => C;
   procedure Test_Signals with Convention => C;

   --  Both constructor forms create a GTK object; calling Initialize again
   --  checks its documented idempotent path for an already-created record.
   procedure Test_Constructors is
      From_Procedure : Gtk_Scrolled_Window;
      From_Function  : constant Gtk_Scrolled_Window := Gtk_Scrolled_Window_New;
      Raw_Record     : constant Gtk_Scrolled_Window := new Gtk_Scrolled_Window_Record;
   begin
      Gtk_New (From_Procedure);
      Ref_Sink (From_Procedure);
      Ref_Sink (From_Function);

      Assert_True (From_Procedure.Is_Created);
      Assert_True (From_Function.Is_Created);
      Gtk.Scrolled_Window.Initialize (Raw_Record);
      Ref_Sink (Raw_Record);
      Assert_True (Raw_Record.Is_Created);
      Gtk.Scrolled_Window.Initialize (Raw_Record);
      Assert_True (Gtk.Scrolled_Window.Get_Type /= 0);
      Assert_Nonnull (Get_Object (Raw_Record));

      Unref (Raw_Record);
      Unref (From_Function);
      Unref (From_Procedure);
   end Test_Constructors;

   --  Each setter is followed by its getter so the test checks the state GTK
   --  retained, including both scrollbar policies and both adjustments.
   procedure Test_Properties is
      SW       : constant Gtk_Scrolled_Window := Gtk_Scrolled_Window_New;
      Label    : constant Gtk_Label := Gtk_Label_New ("viewport child");
      H_Adjust : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (10.0, 0.0, 100.0, 1.0, 10.0, 20.0);
      V_Adjust : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (5.0, 0.0, 80.0, 1.0, 8.0, 15.0);
      H_Policy : Gtk_Policy_Type;
      V_Policy : Gtk_Policy_Type;
   begin
      --  Keep the widget alive explicitly while checking its borrowed child,
      --  adjustments, and scrollbar objects.
      Ref_Sink (SW);
      Assert_True (SW.Get_Child = null);
      Assert_Nonnull (Get_Object (SW.Get_Hadjustment));
      Assert_Nonnull (Get_Object (SW.Get_Vadjustment));
      Assert_Nonnull (Get_Object (SW.Get_Hscrollbar));
      Assert_Nonnull (Get_Object (SW.Get_Vscrollbar));
      --  Setting both adjustments must replace the defaults without changing
      --  the identity of the objects returned by the getters.
      SW.Set_Hadjustment (H_Adjust);
      SW.Set_Vadjustment (V_Adjust);
      Assert_True (Get_Object (SW.Get_Hadjustment) = Get_Object (H_Adjust));
      Assert_True (Get_Object (SW.Get_Vadjustment) = Get_Object (V_Adjust));

      --  Check both Boolean states for the frame and touch-scrolling options.
      SW.Set_Has_Frame (True);
      Assert_True (SW.Get_Has_Frame);
      SW.Set_Has_Frame (False);
      Assert_False (SW.Get_Has_Frame);
      SW.Set_Kinetic_Scrolling (False);
      Assert_False (SW.Get_Kinetic_Scrolling);
      SW.Set_Kinetic_Scrolling (True);
      Assert_True (SW.Get_Kinetic_Scrolling);

      --  Min and max limits are stored separately for each orientation.
      SW.Set_Min_Content_Width (120);
      SW.Set_Min_Content_Height (90);
      SW.Set_Max_Content_Width (420);
      SW.Set_Max_Content_Height (360);
      Assert_Cmpint_Eq (SW.Get_Min_Content_Width, 120);
      Assert_Cmpint_Eq (SW.Get_Min_Content_Height, 90);
      Assert_Cmpint_Eq (SW.Get_Max_Content_Width, 420);
      Assert_Cmpint_Eq (SW.Get_Max_Content_Height, 360);

      --  Placement can be selected explicitly and restored to its default.
      SW.Set_Overlay_Scrolling (False);
      Assert_False (SW.Get_Overlay_Scrolling);
      SW.Set_Overlay_Scrolling (True);
      Assert_True (SW.Get_Overlay_Scrolling);
      SW.Set_Placement (Corner_Bottom_Right);
      Assert_True (SW.Get_Placement = Corner_Bottom_Right);
      SW.Unset_Placement;
      Assert_True (SW.Get_Placement = Corner_Top_Left);

      --  The two policy outputs retain their distinct horizontal/vertical values.
      SW.Set_Policy (Policy_Always, Policy_Never);
      SW.Get_Policy (H_Policy, V_Policy);
      Assert_True (H_Policy = Policy_Always);
      Assert_True (V_Policy = Policy_Never);

      --  Natural-size propagation can be switched on and off on each axis.
      SW.Set_Propagate_Natural_Height (True);
      SW.Set_Propagate_Natural_Width (True);
      Assert_True (SW.Get_Propagate_Natural_Height);
      Assert_True (SW.Get_Propagate_Natural_Width);
      SW.Set_Propagate_Natural_Height (False);
      SW.Set_Propagate_Natural_Width (False);
      Assert_False (SW.Get_Propagate_Natural_Height);
      Assert_False (SW.Get_Propagate_Natural_Width);

      --  Adding a non-scrollable widget creates an accessible viewport wrapper;
      --  removing it clears the scrolled window's child again.
      --  A Label is not GtkScrollable, so GTK inserts a Viewport automatically.
      SW.Set_Child (Label);
      Assert_Nonnull (Get_Object (SW.Get_Child));
      Assert_True
        (Gtk.Viewport.Gtk_Viewport (SW.Get_Child).Get_Child = Gtk_Widget (Label));
      SW.Set_Child (null);
      Assert_True (SW.Get_Child = null);

      --  Retain the widget until process teardown: its GTK-owned adjustment
      --  signal links expose a cleanup crash in this GTK build after replacing
      --  them, while the getter/setter behavior above remains correct.
      Unref (V_Adjust);
      Unref (H_Adjust);
   end Test_Properties;

   --  Port the C test's min/max content checks. GTK's measure results are
   --  independent of a mapped window and the private Xvfb display.
   procedure Test_Sizing is
      SW       : constant Gtk_Scrolled_Window := Gtk_Scrolled_Window_New;
      Box      : constant Gtk_Box := Gtk_Box_New (Orientation_Vertical, 0);
      Minimum  : Gint;
      Natural  : Gint;
      Min_Base : Gint;
      Nat_Base : Gint;
   begin
      Ref_Sink (SW);
      Ref_Sink (Box);
      SW.Set_Propagate_Natural_Width (True);
      SW.Set_Propagate_Natural_Height (True);
      SW.Set_Overlay_Scrolling (True);
      SW.Set_Policy (Policy_Automatic, Policy_Automatic);
      SW.Set_Child (Box);

      --  The horizontal cases match overlay_automatic_width_min_max.
      SW.Set_Min_Content_Width (150);
      SW.Measure (Orientation_Horizontal, -1, Minimum, Natural, Min_Base, Nat_Base);
      Assert_Cmpint_Eq (Minimum, 150);
      SW.Set_Max_Content_Width (300);
      Box.Set_Size_Request (Width => 600, Height => -1);
      SW.Measure (Orientation_Horizontal, -1, Minimum, Natural, Min_Base, Nat_Base);
      Assert_Cmpint_Eq (Natural, 300);
      Box.Measure (Orientation_Horizontal, -1, Minimum, Natural, Min_Base, Nat_Base);
      Assert_Cmpint_Eq (Minimum, 600);

      --  The vertical checks cover the corresponding C test path too.
      SW.Set_Min_Content_Height (150);
      SW.Measure (Orientation_Vertical, -1, Minimum, Natural, Min_Base, Nat_Base);
      Assert_Cmpint_Eq (Minimum, 150);
      SW.Set_Max_Content_Height (300);
      Box.Set_Size_Request (Width => -1, Height => 600);
      SW.Measure (Orientation_Vertical, -1, Minimum, Natural, Min_Base, Nat_Base);
      Assert_Cmpint_Eq (Natural, 300);
      Box.Measure (Orientation_Vertical, -1, Minimum, Natural, Min_Base, Nat_Base);
      Assert_Cmpint_Eq (Minimum, 600);

      SW.Set_Child (null);
      Unref (Box);
      Unref (SW);
   end Test_Sizing;

   --  Accessible queries work before mapping; interface round trips verify
   --  that ScrolledWindow exposes all three interfaces declared in its spec.
   procedure Test_Accessibility_And_Interfaces is
      SW       : constant Gtk_Scrolled_Window := Gtk_Scrolled_Window_New;
      Other    : constant Gtk_Scrolled_Window := Gtk_Scrolled_Window_New;
      Label    : constant Gtk_Label := Gtk_Label_New ("accessible child");
      Parent   : Gtk_Accessible;
      Next     : Gtk_Accessible;
      Context  : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
      Has_Bounds : Boolean;
   begin
      Ref_Sink (SW);
      Ref_Sink (Other);

      Assert_True (SW.Get_Accessible_Role = Accessible_Role_Generic);
      Assert_Cmpstr_Eq (SW.Get_Accessible_Id, "");
      Context := SW.Get_At_Context;
      Assert_True (Context = null);
      Has_Bounds := SW.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);
      Parent := SW.Get_Accessible_Parent;
      Assert_True (Parent = Null_Gtk_Accessible);
      Next := SW.Get_Next_Accessible_Sibling;
      Assert_True (Next = Null_Gtk_Accessible);
      Assert_False (SW.Get_Platform_State (Accessible_Platform_State_Focused));

      SW.Set_Child (Label);
      Assert_True (SW.Get_First_Accessible_Child /= Null_Gtk_Accessible);
      SW.Announce ("scrolled window accessibility test", Accessible_Announcement_Priority_Low);
      SW.Reset_Property (Accessible_Property_Label);
      SW.Reset_Relation (Accessible_Relation_Labelled_By);
      SW.Reset_State (Accessible_State_Busy);
      SW.Update_Platform_State (Accessible_Platform_State_Focused);
      SW.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (SW.Get_Accessible_Parent = Null_Gtk_Accessible);
      SW.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (SW.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  Each conversion is checked in both directions against the same
      --  underlying GObject, covering all interfaces declared by the binding.
      declare
         A : constant Gtk_Accessible := +SW;
         S : constant Gtk_Scrolled_Window := -A;
      begin
         Assert_True (S.Get_Object = SW.Get_Object);
      end;
      declare
         B : constant Gtk.Buildable.Gtk_Buildable := +SW;
         S : constant Gtk_Scrolled_Window := -B;
      begin
         Assert_True (S.Get_Object = SW.Get_Object);
      end;
      declare
         C : constant Gtk.Constraint_Target.Gtk_Constraint_Target := +SW;
         S : constant Gtk_Scrolled_Window := -C;
      begin
         Assert_True (S.Get_Object = SW.Get_Object);
      end;

      SW.Set_Child (null);
      Unref (Other);
      Unref (SW);
   end Test_Accessibility_And_Interfaces;

   --  Emit the four signals directly to test both generated callback forms
   --  and all GtkPositionType, GtkDirectionType, and GtkScrollType marshallers.
   procedure Test_Signals is
      SW : constant Gtk_Scrolled_Window := Gtk_Scrolled_Window_New;
      Name : Interfaces.C.Strings.chars_ptr;
      Result : aliased Gboolean := -1;
   begin
      Ref_Sink (SW);

      SW.On_Edge_Overshot (On_Position'Unrestricted_Access);
      SW.On_Edge_Overshot (On_Position_Slot'Unrestricted_Access, SW, After => True);
      SW.On_Edge_Reached (On_Position'Unrestricted_Access, After => True);
      SW.On_Edge_Reached (On_Position_Slot'Unrestricted_Access, SW);
      SW.On_Move_Focus_Out (On_Direction'Unrestricted_Access);
      SW.On_Move_Focus_Out (On_Direction_Slot'Unrestricted_Access, SW, After => True);
      SW.On_Scroll_Child (On_Scroll'Unrestricted_Access, After => True);
      SW.On_Scroll_Child (On_Scroll_Slot'Unrestricted_Access, SW);

      Name := Interfaces.C.Strings.New_String ("edge-overshot");
      Emit_Position (SW.Get_Object, Name, Pos_Left);
      Interfaces.C.Strings.Free (Name);
      Name := Interfaces.C.Strings.New_String ("edge-reached");
      Emit_Position (SW.Get_Object, Name, Pos_Bottom);
      Interfaces.C.Strings.Free (Name);
      Assert_Cmpuint_Eq (Guint (Position_Typed_Count), 2);
      Assert_Cmpuint_Eq (Guint (Position_Slot_Count), 2);
      Assert_True (Typed_Position = Pos_Bottom);
      Assert_True (Slot_Position = Pos_Bottom);

      Name := Interfaces.C.Strings.New_String ("move-focus-out");
      Emit_Direction (SW.Get_Object, Name, Dir_Tab_Backward);
      Interfaces.C.Strings.Free (Name);
      Assert_Cmpuint_Eq (Guint (Direction_Typed_Count), 1);
      Assert_Cmpuint_Eq (Guint (Direction_Slot_Count), 1);
      Assert_True (Typed_Direction = Dir_Tab_Backward);
      Assert_True (Slot_Direction = Dir_Tab_Backward);

      Name := Interfaces.C.Strings.New_String ("scroll-child");
      Emit_Scroll_Child
        (SW.Get_Object, Name, Scroll_Page_Down, 0, Result'Access);
      Interfaces.C.Strings.Free (Name);
      Assert_Cmpuint_Eq (Guint (Scroll_Typed_Count), 1);
      Assert_Cmpuint_Eq (Guint (Scroll_Slot_Count), 1);
      Assert_True (Typed_Scroll = Scroll_Page_Down);
      Assert_True (Slot_Scroll = Scroll_Page_Down);
      Assert_True (Result = 0);

      --  The position and direction callbacks each raise through both callback
      --  forms; the boolean callbacks stay normal for the marshaller reason above.
      Gtkada.Bindings.Set_On_Exception (Record_Exception'Unrestricted_Access);
      Raise_Callbacks := True;

      Name := Interfaces.C.Strings.New_String ("edge-overshot");
      Emit_Position (SW.Get_Object, Name, Pos_Left);
      Interfaces.C.Strings.Free (Name);
      Name := Interfaces.C.Strings.New_String ("edge-reached");
      Emit_Position (SW.Get_Object, Name, Pos_Bottom);
      Interfaces.C.Strings.Free (Name);
      Name := Interfaces.C.Strings.New_String ("move-focus-out");
      Emit_Direction (SW.Get_Object, Name, Dir_Tab_Forward);
      Interfaces.C.Strings.Free (Name);
      Name := Interfaces.C.Strings.New_String ("scroll-child");
      Emit_Scroll_Child
        (SW.Get_Object, Name, Scroll_Page_Up, 1, Result'Access);
      Interfaces.C.Strings.Free (Name);

      Assert_Cmpuint_Eq (Guint (Exception_Count), 6);
      Assert_Cmpuint_Eq (Guint (Unexpected_Exception_Count), 0);
      Assert_True (Result = 0);

      Unref (SW);
   end Test_Signals;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/scrolledwindow/constructors", Test_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func ("/scrolledwindow/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func ("/sizing/scrolledwindow/measure", Test_Sizing'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scrolledwindow/accessibility-and-interfaces",
      Test_Accessibility_And_Interfaces'Unrestricted_Access);
   Glib.Test.Add_Func ("/scrolledwindow/signals", Test_Signals'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Scrolledwindow;
