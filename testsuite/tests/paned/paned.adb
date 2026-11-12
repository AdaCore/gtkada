--  Exercises Gtk.Paned's child, divider, orientation, accessibility,
--  interface-conversion, property and action-signal bindings. No dedicated
--  GtkPaned C test exists in testsuite/c_tests/ to port.

with Ada.Command_Line;
with Interfaces.C.Strings;
with System;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Main;
with Glib.Properties;       use Glib.Properties;
with Glib.Test;             use Glib.Test;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Accessible_Range;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Button;            use Gtk.Button;
with Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Label;             use Gtk.Label;
with Gtk.Main;
with Gtk.Orientable;
with Gtk.Paned;              use Gtk.Paned;
with Gtk.Widget;
with Gtk.Window;             use Gtk.Window;

use type System.Address;
use type Glib.C_Proxy;
use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Atcontext.Gtk_Atcontext;
use type Gtk.Widget.Gtk_Widget;

procedure Paned is
   use Interfaces.C.Strings;

   Accept_Typed, Accept_Slot : Natural := 0;
   Cancel_Typed, Cancel_Slot : Natural := 0;
   Child_Typed, Child_Slot   : Natural := 0;
   Handle_Typed, Handle_Slot : Natural := 0;
   Move_Typed, Move_Slot     : Natural := 0;
   Toggle_Typed, Toggle_Slot : Natural := 0;
   Seen_Reversed             : Boolean := False;
   Seen_Scroll               : Gtk_Scroll_Type := Scroll_None;

   --  g_signal_emit_by_name is used below for all variadic signal signatures.
   --  g_signal_emit_by_name is variadic. The result pointer follows the
   --  ordinary parameters for each non-void Paned action signal.
   procedure Emit_Boolean_Result
     (Instance : System.Address; Name : chars_ptr; Result : access Gboolean)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Reversed_Result
     (Instance : System.Address; Name : chars_ptr; Reversed : Gint;
      Result : access Gboolean)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Scroll_Result
     (Instance : System.Address; Name : chars_ptr; Scroll : Gtk_Scroll_Type;
      Result : access Gboolean)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Test_Children_And_Properties with Convention => C;
   procedure Test_Accessibility_And_Interfaces with Convention => C;
   procedure Test_Signals with Convention => C;

   --  Each callback records its invocation so the signal wrappers and their
   --  conversion of Ada arguments to the generated C marshallers are checked.
   function Accept_Position
     (Self : access Gtk_Paned_Record'Class) return Boolean is
   begin
      Assert_True (Self /= null);
      Accept_Typed := Accept_Typed + 1;
      return True;
   end Accept_Position;

   function Accept_Position_Slot
     (Self : access GObject_Record'Class) return Boolean is
   begin
      Assert_True (Self /= null);
      Accept_Slot := Accept_Slot + 1;
      return True;
   end Accept_Position_Slot;

   --  Cancel-position uses the same zero-argument callback signature.
   function Cancel_Position
     (Self : access Gtk_Paned_Record'Class) return Boolean is
   begin
      Assert_True (Self /= null);
      Cancel_Typed := Cancel_Typed + 1;
      return True;
   end Cancel_Position;

   function Cancel_Position_Slot
     (Self : access GObject_Record'Class) return Boolean is
   begin
      Assert_True (Self /= null);
      Cancel_Slot := Cancel_Slot + 1;
      return True;
   end Cancel_Position_Slot;

   --  The focus-cycle callbacks check conversion of their Boolean parameter.
   function Cycle_Child
     (Self : access Gtk_Paned_Record'Class;
      Reversed : Boolean) return Boolean is
   begin
      Assert_True (Self /= null);
      Seen_Reversed := Reversed;
      Child_Typed := Child_Typed + 1;
      return True;
   end Cycle_Child;

   function Cycle_Child_Slot
     (Self : access GObject_Record'Class;
      Reversed : Boolean) return Boolean is
   begin
      Assert_True (Self /= null);
      Seen_Reversed := Reversed;
      Child_Slot := Child_Slot + 1;
      return True;
   end Cycle_Child_Slot;

   --  The second focus-cycle signal uses the same callback types.
   function Cycle_Handle
     (Self : access Gtk_Paned_Record'Class;
      Reversed : Boolean) return Boolean is
   begin
      Assert_True (Self /= null);
      Seen_Reversed := Reversed;
      Handle_Typed := Handle_Typed + 1;
      return True;
   end Cycle_Handle;

   function Cycle_Handle_Slot
     (Self : access GObject_Record'Class;
      Reversed : Boolean) return Boolean is
   begin
      Assert_True (Self /= null);
      Seen_Reversed := Reversed;
      Handle_Slot := Handle_Slot + 1;
      return True;
   end Cycle_Handle_Slot;

   --  Move-handle checks conversion of the GtkScrollType enumeration.
   function Move_Handle
     (Self : access Gtk_Paned_Record'Class;
      Scroll_Type : Gtk_Scroll_Type) return Boolean is
   begin
      Assert_True (Self /= null);
      Seen_Scroll := Scroll_Type;
      Move_Typed := Move_Typed + 1;
      return True;
   end Move_Handle;

   function Move_Handle_Slot
     (Self : access GObject_Record'Class;
      Scroll_Type : Gtk_Scroll_Type) return Boolean is
   begin
      Assert_True (Self /= null);
      Seen_Scroll := Scroll_Type;
      Move_Slot := Move_Slot + 1;
      return True;
   end Move_Handle_Slot;

   --  Toggle-handle-focus completes the zero-argument callback overloads.
   function Toggle_Handle
     (Self : access Gtk_Paned_Record'Class) return Boolean is
   begin
      Assert_True (Self /= null);
      Toggle_Typed := Toggle_Typed + 1;
      return True;
   end Toggle_Handle;

   function Toggle_Handle_Slot
     (Self : access GObject_Record'Class) return Boolean is
   begin
      Assert_True (Self /= null);
      Toggle_Slot := Toggle_Slot + 1;
      return True;
   end Toggle_Handle_Slot;

   --  Constructors and setters are checked through their public getters and
   --  GObject properties, including nullable children and both Boolean values.
   procedure Test_Children_And_Properties is
      Paned : Gtk_Paned;
      Other : constant Gtk_Paned := Gtk_Paned_New (Orientation_Vertical);
      Start : constant Gtk_Label := Gtk_Label_New ("start");
      Finish : constant Gtk_Button := Gtk_Button_New_With_Label ("end");
      Position : Gint;
   begin
      Gtk_New (Paned, Orientation_Horizontal);
      Ref_Sink (Paned);
      Ref_Sink (Other);
      Assert_True (Paned.Is_Created and Other.Is_Created);
      Assert_True (Paned.Get_Orientation = Orientation_Horizontal);
      Assert_True (Other.Get_Orientation = Orientation_Vertical);

      --  Initialize on an already-created object is documented as a no-op.
      Paned.Initialize (Orientation_Vertical);
      Assert_True (Paned.Get_Orientation = Orientation_Horizontal);
      Paned.Set_Orientation (Orientation_Vertical);
      Assert_True (Paned.Get_Orientation = Orientation_Vertical);
      Paned.Set_Orientation (Orientation_Horizontal);
      Assert_True (Get_Property (Paned, Gtk.Orientable.Orientation_Property)
                   = Orientation_Horizontal);

      Paned.Set_Start_Child (Start);
      Paned.Set_End_Child (Finish);
      --  Child getters return borrowed widgets, so compare their identities.
      Assert_True (Paned.Get_Start_Child.Get_Object = Start.Get_Object);
      Assert_True (Paned.Get_End_Child.Get_Object = Finish.Get_Object);

      --  The write-only object-property overload takes a GObject access value;
      --  use the same child property to verify that it stores the child.
      Set_Property
        (Paned, Property_Object_WO (Start_Child_Property), GObject (Start));
      Assert_True
        (Get_Property (Paned, Start_Child_Property).Get_Object
         = Start.Get_Object);

      --  The opaque C_Proxy overload is another public route to an object-valued
      --  property. Compare the round-tripped proxy with the child's C address.
      Set_Property
        (Paned, Property_C_Proxy (Start_Child_Property),
         Glib.To_Proxy (Start.Get_Object));
      Assert_True
        (Get_Property (Paned, Property_C_Proxy (Start_Child_Property))
         = Glib.To_Proxy (Start.Get_Object));

      Assert_True (Get_Property (Paned, Start_Child_Property).Get_Object
                   = Start.Get_Object);
      Assert_True (Get_Property (Paned, End_Child_Property).Get_Object
                   = Finish.Get_Object);

      --  A requested divider position is retained before the widget is laid
      --  out, so the getter and property can be compared deterministically.
      Paned.Set_Position (37);
      Position := Paned.Get_Position;
      Assert_Cmpint_Eq (Position, 37);
      Assert_Cmpint_Eq (Get_Property (Paned, Position_Property), Position);
      Assert_True (Get_Property (Paned, Position_Set_Property));
      --  These bounds are computed by GTK from the children and flags.
      Assert_True (Get_Property (Paned, Min_Position_Property)
                   <= Get_Property (Paned, Max_Position_Property));

      --  The resize options can be changed independently for either child.
      Paned.Set_Resize_Start_Child (False);
      Assert_False (Paned.Get_Resize_Start_Child);
      Paned.Set_Resize_Start_Child (True);
      Assert_True (Paned.Get_Resize_Start_Child);
      Paned.Set_Resize_End_Child (False);
      Assert_False (Paned.Get_Resize_End_Child);
      Paned.Set_Resize_End_Child (True);
      Assert_True (Paned.Get_Resize_End_Child);
      Assert_True (Get_Property (Paned, Resize_Start_Child_Property));
      Assert_True (Get_Property (Paned, Resize_End_Child_Property));

      --  Shrinkability is likewise checked in both states for both children.
      Paned.Set_Shrink_Start_Child (False);
      Assert_False (Paned.Get_Shrink_Start_Child);
      Paned.Set_Shrink_Start_Child (True);
      Assert_True (Paned.Get_Shrink_Start_Child);
      Paned.Set_Shrink_End_Child (False);
      Assert_False (Paned.Get_Shrink_End_Child);
      Paned.Set_Shrink_End_Child (True);
      Assert_True (Paned.Get_Shrink_End_Child);
      Assert_True (Get_Property (Paned, Shrink_Start_Child_Property));
      Assert_True (Get_Property (Paned, Shrink_End_Child_Property));

      --  Wide handles are a visual property and support both Boolean states.
      Paned.Set_Wide_Handle (True);
      Assert_True (Paned.Get_Wide_Handle);
      Assert_True (Get_Property (Paned, Wide_Handle_Property));
      Paned.Set_Wide_Handle (False);
      Assert_False (Paned.Get_Wide_Handle);

      --  A null child removes the existing widget from that side.
      Paned.Set_Start_Child (null);
      Paned.Set_End_Child (null);
      Assert_True (Paned.Get_Start_Child = null);
      Assert_True (Paned.Get_End_Child = null);
      Unref (Other);
      Unref (Paned);
   end Test_Children_And_Properties;

   --  Detached widgets have no screen bounds or AT context. The remaining
   --  accessibility calls are safe on this tree and exercise the inherited
   --  GtkAccessible wrappers without relying on a screen reader or display.
   procedure Test_Accessibility_And_Interfaces is
      Paned : constant Gtk_Paned := Gtk_Paned_New (Orientation_Horizontal);
      Parent : Gtk_Accessible;
      Sibling : Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
   begin
      Ref_Sink (Paned);
      --  GTK assigns Paned a concrete accessible role, which may vary with
      --  the backend's accessibility implementation.
      Assert_True (Paned.Get_Accessible_Role /= Accessible_Role_None);
      Assert_Cmpstr_Eq (Paned.Get_Accessible_Id, "");
      Context := Paned.Get_At_Context;
      Assert_True (Context = null);
      --  This detached widget has no location in display coordinates.
      Assert_False (Paned.Get_Bounds (X, Y, Width, Height));
      Parent := Paned.Get_Accessible_Parent;
      Sibling := Paned.Get_Next_Accessible_Sibling;
      Assert_True (Parent = Null_Gtk_Accessible);
      Assert_True (Sibling = Null_Gtk_Accessible);
      --  GTK exposes its internal accessible child through this method.
      Assert_True (Paned.Get_First_Accessible_Child /= Null_Gtk_Accessible);
      Assert_False
        (Paned.Get_Platform_State (Accessible_Platform_State_Focused));

      --  Accessibility state changes and announcements need no AT service.
      Paned.Announce ("paned test", Accessible_Announcement_Priority_Low);
      Paned.Reset_Property (Accessible_Property_Label);
      Paned.Reset_Relation (Accessible_Relation_Labelled_By);
      Paned.Reset_State (Accessible_State_Busy);
      Paned.Update_Platform_State (Accessible_Platform_State_Focused);
      Paned.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Paned.Get_Accessible_Parent = Null_Gtk_Accessible);
      Paned.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (Paned.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  Each generated interface pair must round-trip to the same GObject.
      declare
         --  GtkAccessible represents the widget's accessible tree interface.
         A : constant Gtk_Accessible := +Paned;
         P : constant Gtk_Paned := -A;
      begin
         Assert_True (P.Get_Object = Paned.Get_Object);
      end;
      declare
         --  GtkAccessibleRange is an implemented interface even without value.
         A : constant Gtk.Accessible_Range.Gtk_Accessible_Range := +Paned;
         P : constant Gtk_Paned := -A;
      begin
         Assert_True (P.Get_Object = Paned.Get_Object);
      end;
      declare
         --  The GtkBuildable conversion does not need a builder instance.
         A : constant Gtk.Buildable.Gtk_Buildable := +Paned;
         P : constant Gtk_Paned := -A;
      begin
         Assert_True (P.Get_Object = Paned.Get_Object);
      end;
      declare
         --  ConstraintTarget conversion is safe without a constraint layout.
         A : constant Gtk.Constraint_Target.Gtk_Constraint_Target := +Paned;
         P : constant Gtk_Paned := -A;
      begin
         Assert_True (P.Get_Object = Paned.Get_Object);
      end;
      declare
         --  GtkOrientable conversion is paired with the orientation checks.
         A : constant Gtk.Orientable.Gtk_Orientable := +Paned;
         P : constant Gtk_Paned := -A;
      begin
         Assert_True (P.Get_Object = Paned.Get_Object);
      end;
      Unref (Paned);
   end Test_Accessibility_And_Interfaces;

   --  Explicitly emit keyboard action signals so callback marshallers are
   --  exercised without synthesizing key events or depending on user input.
   procedure Test_Signals is
      Paned : constant Gtk_Paned := Gtk_Paned_New (Orientation_Horizontal);
      Slot : constant Gtk_Button := Gtk_Button_New_With_Label ("callback slot");
      Window : constant Gtk_Window := Gtk_Window_New;
      Name : chars_ptr;
      Result : aliased Gboolean := 0;
      Dispatched : Boolean;

      --  Re-emit all six signal signatures for the normal and exceptional
      --  callback paths; each emission is synchronous.
      procedure Emit_Action_Signals is
      begin
         Result := 0;
         Name := New_String ("accept-position");
         Emit_Boolean_Result (Paned.Get_Object, Name, Result'Access);
         Free (Name);
         Assert_True (Result /= 0);

         Result := 0;
         Name := New_String ("cancel-position");
         Emit_Boolean_Result (Paned.Get_Object, Name, Result'Access);
         Free (Name);
         Assert_True (Result /= 0);

         Result := 0;
         Name := New_String ("cycle-child-focus");
         Emit_Reversed_Result (Paned.Get_Object, Name, 1, Result'Access);
         Free (Name);
         Assert_True (Result /= 0 and Seen_Reversed);

         Result := 0;
         Name := New_String ("cycle-handle-focus");
         Emit_Reversed_Result (Paned.Get_Object, Name, 1, Result'Access);
         Free (Name);
         Assert_True (Result /= 0 and Seen_Reversed);

         Result := 0;
         Name := New_String ("move-handle");
         Emit_Scroll_Result
           (Paned.Get_Object, Name, Scroll_Page_Forward, Result'Access);
         Free (Name);
         Assert_True (Result /= 0);
         Assert_True (Seen_Scroll = Scroll_Page_Forward);

         Result := 0;
         Name := New_String ("toggle-handle-focus");
         Emit_Boolean_Result (Paned.Get_Object, Name, Result'Access);
         Free (Name);
         Assert_True (Result /= 0);
      end Emit_Action_Signals;
   begin
      Ref_Sink (Paned);
      Ref_Sink (Slot);
      Ref_Sink (Window);
      Accept_Typed := 0; Accept_Slot := 0;
      Cancel_Typed := 0; Cancel_Slot := 0;
      Child_Typed := 0; Child_Slot := 0;
      Handle_Typed := 0; Handle_Slot := 0;
      Move_Typed := 0; Move_Slot := 0;
      Toggle_Typed := 0; Toggle_Slot := 0;

      Paned.On_Accept_Position (Accept_Position'Unrestricted_Access);
      --  The slot overload exercises a generic GObject callback signature.
      Paned.On_Accept_Position
        (Accept_Position_Slot'Unrestricted_Access, Slot, After => True);
      Paned.On_Cancel_Position (Cancel_Position'Unrestricted_Access);
      --  Connect both callback forms for each emitted action signal.
      Paned.On_Cancel_Position
        (Cancel_Position_Slot'Unrestricted_Access, Slot, After => True);
      Paned.On_Cycle_Child_Focus (Cycle_Child'Unrestricted_Access);
      --  Reversed is checked through the typed and generic callbacks.
      Paned.On_Cycle_Child_Focus
        (Cycle_Child_Slot'Unrestricted_Access, Slot, After => True);
      Paned.On_Cycle_Handle_Focus (Cycle_Handle'Unrestricted_Access);
      --  After=True exercises the alternate signal connection option.
      Paned.On_Cycle_Handle_Focus
        (Cycle_Handle_Slot'Unrestricted_Access, Slot, After => True);
      Paned.On_Move_Handle (Move_Handle'Unrestricted_Access);
      --  The forwarded enum and Boolean return are checked after emission.
      Paned.On_Move_Handle
        (Move_Handle_Slot'Unrestricted_Access, Slot, After => True);
      Paned.On_Toggle_Handle_Focus (Toggle_Handle'Unrestricted_Access);
      --  The final signal checks both toggle callback overloads.
      Paned.On_Toggle_Handle_Focus
        (Toggle_Handle_Slot'Unrestricted_Access, Slot, After => True);

      --  GtkPaned's cycle-focus class handlers enumerate realized Paned
      --  widgets. Rooting and presenting it supplies that valid widget tree.
      Window.Set_Child (Paned);
      Window.Present;
      for I in 1 .. 10 loop
         Dispatched := Glib.Main.Main_Context_Iteration
           (null, May_Block => False);
      end loop;
      Assert_True (Paned.Get_Realized);

      Emit_Action_Signals;
      Assert_Cmpuint_Eq (Guint (Accept_Typed), 1);
      Assert_Cmpuint_Eq (Guint (Accept_Slot), 1);
      Assert_Cmpuint_Eq (Guint (Cancel_Typed), 1);
      Assert_Cmpuint_Eq (Guint (Cancel_Slot), 1);
      Assert_Cmpuint_Eq (Guint (Child_Typed), 1);
      Assert_Cmpuint_Eq (Guint (Child_Slot), 1);
      Assert_Cmpuint_Eq (Guint (Handle_Typed), 1);
      Assert_Cmpuint_Eq (Guint (Handle_Slot), 1);
      Assert_Cmpuint_Eq (Guint (Move_Typed), 1);
      Assert_Cmpuint_Eq (Guint (Move_Slot), 1);
      Assert_Cmpuint_Eq (Guint (Toggle_Typed), 1);
      Assert_Cmpuint_Eq (Guint (Toggle_Slot), 1);

      Window.Destroy;
      Unref (Window);
      Unref (Slot);
      Unref (Paned);
   end Test_Signals;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func
     ("/paned/children-properties",
      Test_Children_And_Properties'Unrestricted_Access);
   Add_Func
     ("/paned/accessibility-interfaces",
      Test_Accessibility_And_Interfaces'Unrestricted_Access);
   Add_Func ("/paned/signals", Test_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Paned;
