--  GTK has no testsuite/gtk/overlay.c to port, so these cases are ours:
--  they cover the surface the Gtk.Overlay binding adds, and no more.

with Ada.Command_Line;
with Gdk.Rectangle;    use Gdk.Rectangle;
with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Glib.Properties;
with Glib.Test;        use Glib.Test;
with Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Button;       use Gtk.Button;
with Gtk.Label;        use Gtk.Label;
with Gtk.Layout_Manager; use Gtk.Layout_Manager;
with Gtk.Main;
with Gtk.Overlay;      use Gtk.Overlay;
with Gtk.Overlay_Layout;
with Gtk.Widget;       use Gtk.Widget;
with Interfaces.C.Strings;
with System;

procedure Overlay is

   procedure Emit_Child_Position
     (Instance : System.Address;
      Name     : Interfaces.C.Strings.chars_ptr;
      Child    : System.Address;
      Rect     : access Gdk_Rectangle;
      Result   : access Gboolean)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";
   --  Emit "get-child-position", whose GdkRectangle is an out parameter and
   --  whose return value is a gboolean.

   procedure Test_Defaults with Convention => C;
   procedure Test_Constructors with Convention => C;
   procedure Test_Child with Convention => C;
   procedure Test_Overlays with Convention => C;
   procedure Test_Flags with Convention => C;
   procedure Test_Layout_Constructors with Convention => C;
   procedure Test_Layout_Properties with Convention => C;
   procedure Test_Accessibility with Convention => C;
   procedure Test_Position with Convention => C;
   procedure Test_Position_Slot with Convention => C;

   Handler_Calls : Natural := 0;
   Slot_Handler_Calls : Natural := 0;
   Slot_Object_Address : System.Address := System.Null_Address;

   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Accessible.Gtk_Accessible_Role;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Glib.GType;
   use type System.Address;

   function On_Child_Position
     (Self       : access Gtk_Overlay_Record'Class;
      Widget     : not null access Gtk_Widget_Record'Class;
      Allocation : out Gdk_Rectangle) return Boolean;

   -----------------------
   -- On_Child_Position --
   -----------------------

   function On_Child_Position
     (Self       : access Gtk_Overlay_Record'Class;
      Widget     : not null access Gtk_Widget_Record'Class;
      Allocation : out Gdk_Rectangle) return Boolean
   is
      pragma Unreferenced (Self, Widget);
   begin
      Handler_Calls := Handler_Calls + 1;
      Allocation := (X => 10, Y => 20, Width => 30, Height => 40);
      return True;
   end On_Child_Position;

   function On_Child_Position_Slot
     (Self       : access Glib.Object.GObject_Record'Class;
      Widget     : not null access Gtk_Widget_Record'Class;
      Allocation : out Gdk_Rectangle) return Boolean;

   -----------------------------
   -- On_Child_Position_Slot --
   -----------------------------

   function On_Child_Position_Slot
     (Self       : access Glib.Object.GObject_Record'Class;
      Widget     : not null access Gtk_Widget_Record'Class;
      Allocation : out Gdk_Rectangle) return Boolean
   is
      pragma Unreferenced (Widget);
   begin
      --  A slotted signal passes the supplied object as Self, rather than
      --  the GtkOverlay that emitted the signal.
      Assert_True (Get_Object (Self) = Slot_Object_Address);
      Slot_Handler_Calls := Slot_Handler_Calls + 1;
      Allocation := (X => 50, Y => 60, Width => 70, Height => 80);
      return True;
   end On_Child_Position_Slot;

   -------------------
   -- Test_Defaults --
   -------------------

   procedure Test_Defaults is
      O : constant Gtk_Overlay := Gtk_Overlay_New;
   begin
      Ref_Sink (O);
      Assert_True (O.Get_Child = null);
      Unref (O);
   end Test_Defaults;

   -----------------------
   -- Test_Constructors --
   -----------------------

   procedure Test_Constructors is
      By_Procedure : Gtk_Overlay;
      By_Function  : constant Gtk_Overlay := Gtk_Overlay_New;
   begin
      --  Exercise the out-parameter constructor and the already-created
      --  Initialize path while checking that both objects have GTK instances.
      Gtk_New (By_Procedure);
      Gtk.Overlay.Initialize (By_Procedure);
      Assert_True (Gtk.Overlay.Get_Type /= 0);
      Assert_True (Get_Object (By_Procedure) /= System.Null_Address);
      Assert_True (Get_Object (By_Function) /= System.Null_Address);

      --  These standalone layout objects remain alive for this short-lived
      --  test process; no widget takes ownership of them.
   end Test_Constructors;

   ----------------
   -- Test_Child --
   ----------------

   procedure Test_Child is
      O     : constant Gtk_Overlay := Gtk_Overlay_New;
      Label : constant Gtk_Label := Gtk_Label_New ("child");
   begin
      Ref_Sink (O);

      O.Set_Child (Label);
      Assert_True (O.Get_Child = Gtk_Widget (Label));

      O.Set_Child (null);
      Assert_True (O.Get_Child = null);

      Unref (O);
   end Test_Child;

   ---------------------
   -- Test_Overlays --
   ---------------------

   procedure Test_Overlays is
      O      : constant Gtk_Overlay := Gtk_Overlay_New;
      Button : constant Gtk_Button := Gtk_Button_New_With_Label ("over");
   begin
      Ref_Sink (O);
      --  Keep the button alive across Remove_Overlay, which drops the
      --  overlay's reference.
      Ref_Sink (Button);

      Assert_True (Button.Get_Parent = null);

      O.Add_Overlay (Button);
      Assert_True (Button.Get_Parent = Gtk_Widget (O));
      --  An overlay is not the main child
      Assert_True (O.Get_Child = null);

      O.Remove_Overlay (Button);
      Assert_True (Button.Get_Parent = null);

      Unref (Button);
      Unref (O);
   end Test_Overlays;

   ----------------
   -- Test_Flags --
   ----------------

   procedure Test_Flags is
      O      : constant Gtk_Overlay := Gtk_Overlay_New;
      Button : constant Gtk_Button := Gtk_Button_New_With_Label ("b");
   begin
      Ref_Sink (O);
      O.Add_Overlay (Button);

      Assert_False (O.Get_Clip_Overlay (Button));
      O.Set_Clip_Overlay (Button, True);
      Assert_True (O.Get_Clip_Overlay (Button));
      O.Set_Clip_Overlay (Button, False);
      Assert_False (O.Get_Clip_Overlay (Button));

      Assert_False (O.Get_Measure_Overlay (Button));
      O.Set_Measure_Overlay (Button, True);
      Assert_True (O.Get_Measure_Overlay (Button));
      O.Set_Measure_Overlay (Button, False);
      Assert_False (O.Get_Measure_Overlay (Button));

      Unref (O);
   end Test_Flags;

   -------------------------------
   -- Test_Layout_Constructors --
   -------------------------------

   procedure Test_Layout_Constructors is
      By_Procedure : Gtk.Overlay_Layout.Gtk_Overlay_Layout;
      By_Function  : constant Gtk.Overlay_Layout.Gtk_Overlay_Layout :=
        Gtk.Overlay_Layout.Gtk_Overlay_Layout_New;
   begin
      --  Exercise both constructors and Initialize's already-created guard.
      Gtk.Overlay_Layout.Gtk_New (By_Procedure);
      Gtk.Overlay_Layout.Initialize (By_Procedure);
      Assert_True (Gtk.Overlay_Layout.Get_Type /= 0);
      Assert_True (Gtk.Overlay_Layout.Overlay_Layout_Child_Get_Type /= 0);
      Assert_True (Get_Object (By_Procedure) /= System.Null_Address);
      Assert_True (Get_Object (By_Function) /= System.Null_Address);

      --  These standalone layout objects remain alive for this short-lived
      --  test process; no widget takes ownership of them.
   end Test_Layout_Constructors;

   ------------------------------
   -- Test_Layout_Properties --
   ------------------------------

   procedure Test_Layout_Properties is
      O       : constant Gtk_Overlay := Gtk_Overlay_New;
      Button  : constant Gtk_Button := Gtk_Button_New_With_Label ("layout child");
      Manager : Gtk_Layout_Manager;
      Child   : Gtk.Overlay_Layout.Gtk_Overlay_Layout_Child;
   begin
      Ref_Sink (O);
      O.Add_Overlay (Button);

      --  GtkOverlay creates GtkOverlayLayoutChild objects for its overlays;
      --  view-convert the generic layout child to reach its typed properties.
      Manager := O.Get_Layout_Manager;
      Child := Gtk.Overlay_Layout.Gtk_Overlay_Layout_Child
        (Manager.Get_Layout_Child (Button));

      Assert_False (Gtk.Overlay_Layout.Get_Clip_Overlay (Child));
      Gtk.Overlay_Layout.Set_Clip_Overlay (Child, True);
      Assert_True (Gtk.Overlay_Layout.Get_Clip_Overlay (Child));
      Glib.Properties.Set_Property
        (Child, Gtk.Overlay_Layout.Clip_Overlay_Property, False);
      Assert_False (Gtk.Overlay_Layout.Get_Clip_Overlay (Child));

      Assert_False (Gtk.Overlay_Layout.Get_Measure (Child));
      Gtk.Overlay_Layout.Set_Measure (Child, True);
      Assert_True (Gtk.Overlay_Layout.Get_Measure (Child));
      Glib.Properties.Set_Property
        (Child, Gtk.Overlay_Layout.Measure_Property, False);
      Assert_False (Gtk.Overlay_Layout.Get_Measure (Child));

      Unref (O);
   end Test_Layout_Properties;

   -------------------------
   -- Test_Accessibility --
   -------------------------

   procedure Test_Accessibility is
      O       : constant Gtk_Overlay := Gtk_Overlay_New;
      Child   : constant Gtk_Label := Gtk_Label_New ("accessible child");
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
   begin
      Ref_Sink (O);
      O.Set_Child (Child);

      --  A detached overlay has no accessible parent, sibling, or mapped
      --  bounds; its child is still represented in the accessible tree.
      Assert_True
        (O.Get_Accessible_Role /= Gtk.Accessible.Accessible_Role_None);
      Assert_Cmpstr_Eq (O.Get_Accessible_Id, "");
      Assert_True
        (O.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_False (O.Get_Bounds (X, Y, Width, Height));
      Assert_True
        (O.Get_First_Accessible_Child /= Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (O.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_False
        (O.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focused));

      --  These interface methods accept valid values without requiring an
      --  accessibility service in the headless test environment.
      O.Announce
        ("overlay test",
         Gtk.Accessible.Accessible_Announcement_Priority_Low);
      O.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      O.Reset_Relation
        (Gtk.Accessible.Accessible_Relation_Labelled_By);
      O.Reset_State (Gtk.Accessible.Accessible_State_Selected);
      O.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focusable);
      O.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (O.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      O.Update_Next_Accessible_Sibling
        (Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (O.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);

      Context := O.Get_At_Context;
      Assert_True (Context = null);
      Unref (O);
   end Test_Accessibility;

   -------------------
   -- Test_Position --
   -------------------

   procedure Test_Position is
      O      : constant Gtk_Overlay := Gtk_Overlay_New;
      Button : constant Gtk_Button := Gtk_Button_New_With_Label ("b");
      Rect   : aliased Gdk_Rectangle := (0, 0, 0, 0);
      Result : aliased Gboolean := 0;
      Name   : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("get-child-position");
   begin
      Ref_Sink (O);
      O.Add_Overlay (Button);
      O.On_Get_Child_Position (On_Child_Position'Unrestricted_Access);

      Emit_Child_Position
        (Get_Object (O), Name, Get_Object (Button), Rect'Access,
         Result'Access);
      Interfaces.C.Strings.Free (Name);

      Assert_Cmpuint_Eq (Guint (Handler_Calls), 1);
      Assert_True (Result /= 0);
      Assert_Cmpint_Eq (Rect.X, 10);
      Assert_Cmpint_Eq (Rect.Y, 20);
      Assert_Cmpint_Eq (Rect.Width, 30);
      Assert_Cmpint_Eq (Rect.Height, 40);

      Unref (O);
   end Test_Position;

   ------------------------
   -- Test_Position_Slot --
   ------------------------

   procedure Test_Position_Slot is
      O      : constant Gtk_Overlay := Gtk_Overlay_New;
      Button : constant Gtk_Button := Gtk_Button_New_With_Label ("b");
      Slot   : constant Gtk_Label := Gtk_Label_New ("signal slot");
      Rect   : aliased Gdk_Rectangle := (0, 0, 0, 0);
      Result : aliased Gboolean := 0;
      Name   : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("get-child-position");
   begin
      Ref_Sink (O);
      Ref_Sink (Slot);
      O.Add_Overlay (Button);
      Slot_Object_Address := Get_Object (Slot);
      O.On_Get_Child_Position
        (On_Child_Position_Slot'Unrestricted_Access, Slot);

      --  Emitting directly keeps this test deterministic and exercises
      --  GtkAda's GObject-slot marshaller without pointer or keyboard input.
      Emit_Child_Position
        (Get_Object (O), Name, Get_Object (Button), Rect'Access,
         Result'Access);
      Interfaces.C.Strings.Free (Name);

      Assert_Cmpuint_Eq (Guint (Slot_Handler_Calls), 1);
      Assert_True (Result /= 0);
      Assert_Cmpint_Eq (Rect.X, 50);
      Assert_Cmpint_Eq (Rect.Y, 60);
      Assert_Cmpint_Eq (Rect.Width, 70);
      Assert_Cmpint_Eq (Rect.Height, 80);

      Unref (Slot);
      Unref (O);
   end Test_Position_Slot;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/overlay/defaults", Test_Defaults'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/overlay/constructors", Test_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func ("/overlay/child", Test_Child'Unrestricted_Access);
   Glib.Test.Add_Func ("/overlay/overlays", Test_Overlays'Unrestricted_Access);
   Glib.Test.Add_Func ("/overlay/flags", Test_Flags'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/overlay/layout-constructors",
      Test_Layout_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/overlay/layout-properties",
      Test_Layout_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/overlay/accessibility", Test_Accessibility'Unrestricted_Access);
   Glib.Test.Add_Func ("/overlay/position", Test_Position'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/overlay/position-slot", Test_Position_Slot'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Overlay;
