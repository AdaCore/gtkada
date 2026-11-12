--  Exercises the shared Gtk.Widget methods with real GTK widgets.
--  The hierarchy checks follow the cases in testsuite/c_tests/widgetorder.c;
--  state and geometry checks cover the GtkWidget API used by GTK containers.

with Ada.Command_Line;
with System;
with Interfaces.C;
with Glib;                 use Glib;
with Glib.Main;            use Glib.Main;
with Glib.Test;            use Glib.Test;
with Glib.List_Model;      use Glib.List_Model;
with Glib.Object;          use Glib.Object;
with Glib.Action_Group;
with Glib.Variant;
with GNAT.Strings;
with Cairo;                 use Cairo;
with Gdk.Rectangle;        use Gdk.Rectangle;
with Gdk.Frame_Clock;
with Gdk.RGBA;
with Gtk.Box;              use Gtk.Box;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Fixed;            use Gtk.Fixed;
with Gtk.Gesture_Click;    use Gtk.Gesture_Click;
with Gtk.Label;            use Gtk.Label;
with Gtk.Main;
with Gtk.Native;
with Gtk.Root;
with Gtk.Widget;           use Gtk.Widget;
with Gtk.Window;           use Gtk.Window;
with Gtk.Settings;
with Gdk.Display;
with Gdk.Clipboard;
with Graphene.Point;
with Graphene.Rect;
with Graphene.Matrix;
with Pango.Font_Map;
with Pango.Layout;

procedure Gtk_Widget is

   use type System.Address;
   use type Interfaces.C.C_float;
   use type Pango.Font_Map.Pango_Font_Map;
   use type Cairo.Cairo_Font_Options;
   use type Gtk.Native.Gtk_Native;
   use type Gtk.Root.Gtk_Root;

   Tick_Called      : Boolean := False;
   Data_Tick_Called : Boolean := False;
   Wait_Done        : Boolean := False;
   Void_Signal_Calls : Natural := 0;
   Slot_Void_Calls   : Natural := 0;
   Direction_Calls   : Natural := 0;
   Slot_Direction_Calls : Natural := 0;
   Keynav_Calls      : Natural := 0;
   Slot_Keynav_Calls : Natural := 0;
   Mnemonic_Calls    : Natural := 0;
   Slot_Mnemonic_Calls : Natural := 0;
   pragma Volatile (Tick_Called);
   pragma Volatile (Data_Tick_Called);
   pragma Volatile (Wait_Done);

   --  This minimal subclass sends each relevant vfunc through GtkAda's
   --  inherited-handler bridge while GTK measures, allocates, realizes, and
   --  snapshots it as a normal window child.
   type Test_Widget_Record is new Gtk_Widget_Record with null record;
   type Test_Widget is access all Test_Widget_Record'Class;
   Test_Widget_Class : aliased Ada_GObject_Class := Uninitialized_Class;
   Measure_Calls, Allocation_Calls : Natural := 0;
   Realize_Calls, Snapshot_Calls : Natural := 0;

   function Stop_Waiting return Boolean;
   procedure Test_Properties with Convention => C;
   procedure Test_Hierarchy with Convention => C;
   procedure Test_Mapped_Widget with Convention => C;
   procedure Test_Inherited_Vfuncs with Convention => C;

   procedure Test_Widget_Class_Init (Self : GObject_Class);
   pragma Convention (C, Test_Widget_Class_Init);
   procedure Test_Widget_Measure
     (Widget                             : System.Address;
      Orientation                        : Gtk_Orientation;
      For_Size                           : Gint;
      Minimum, Natural                   : out Gint;
      Minimum_Baseline, Natural_Baseline : out Gint);
   pragma Convention (C, Test_Widget_Measure);
   procedure Test_Widget_Size_Allocate
     (Widget : System.Address; Width, Height, Baseline : Gint);
   pragma Convention (C, Test_Widget_Size_Allocate);
   procedure Test_Widget_Realize (Widget : System.Address);
   pragma Convention (C, Test_Widget_Realize);
   procedure Test_Widget_Snapshot
     (Widget : System.Address; Snapshot : System.Address);
   pragma Convention (C, Test_Widget_Snapshot);

   function Test_Widget_Get_Type return GType;
   procedure Initialize_Test_Widget
     (Self : not null access Test_Widget_Record'Class);

   function Test_Widget_Get_Type return GType is
   begin
      Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtk.Widget.Get_Type,
         Class_Record => Test_Widget_Class,
         Type_Name    => "GtkAdaCoverageWidget",
         Class_Init   => Test_Widget_Class_Init'Unrestricted_Access);
      return Test_Widget_Class.The_Type;
   end Test_Widget_Get_Type;

   procedure Initialize_Test_Widget
     (Self : not null access Test_Widget_Record'Class) is
   begin
      G_New (Self, Test_Widget_Get_Type);
   end Initialize_Test_Widget;

   --  Install the handlers on the Ada GObject class. Each callback chains
   --  through the corresponding parent implementation below.
   procedure Test_Widget_Class_Init (Self : GObject_Class) is
   begin
      Set_Default_Measure_Handler
        (Self, Test_Widget_Measure'Unrestricted_Access);
      Set_Default_Size_Allocate_Handler
        (Self, Test_Widget_Size_Allocate'Unrestricted_Access);
      Set_Default_Realize_Handler
        (Self, Test_Widget_Realize'Unrestricted_Access);
      Set_Default_Snapshot_Handler
        (Self, Test_Widget_Snapshot'Unrestricted_Access);
   end Test_Widget_Class_Init;

   procedure Test_Widget_Measure
     (Widget                             : System.Address;
      Orientation                        : Gtk_Orientation;
      For_Size                           : Gint;
      Minimum, Natural                   : out Gint;
      Minimum_Baseline, Natural_Baseline : out Gint)
   is
      Stub : Test_Widget_Record;
      Self : constant Test_Widget := Test_Widget (Get_User_Data (Widget, Stub));
   begin
      Measure_Calls := Measure_Calls + 1;
      Inherited_Measure
        (Test_Widget_Class, Self, Orientation, For_Size,
         Minimum, Natural, Minimum_Baseline, Natural_Baseline);
      Minimum_Baseline := -1;
      Natural_Baseline := -1;
   end Test_Widget_Measure;

   procedure Test_Widget_Size_Allocate
     (Widget : System.Address; Width, Height, Baseline : Gint)
   is
      Stub : Test_Widget_Record;
      Self : constant Test_Widget := Test_Widget (Get_User_Data (Widget, Stub));
   begin
      Allocation_Calls := Allocation_Calls + 1;
      Inherited_Size_Allocate
        (Test_Widget_Class, Self, Width, Height, Baseline);
   end Test_Widget_Size_Allocate;

   procedure Test_Widget_Realize (Widget : System.Address) is
      Stub : Test_Widget_Record;
      Self : constant Test_Widget := Test_Widget (Get_User_Data (Widget, Stub));
   begin
      Realize_Calls := Realize_Calls + 1;
      Inherited_Realize (Test_Widget_Class, Self);
   end Test_Widget_Realize;

   procedure Test_Widget_Snapshot
     (Widget : System.Address; Snapshot : System.Address)
   is
      Stub : Test_Widget_Record;
      Self : constant Test_Widget := Test_Widget (Get_User_Data (Widget, Stub));
   begin
      Snapshot_Calls := Snapshot_Calls + 1;
      Inherited_Snapshot (Test_Widget_Class, Self, Snapshot);
   end Test_Widget_Snapshot;

   function Stop_Waiting return Boolean is
   begin
      Wait_Done := True;
      return False;
   end Stop_Waiting;

   --  Returning False removes each callback after its first frame.
   function Tick
     (Widget      : not null access Gtk_Widget_Record'Class;
      Frame_Clock : not null access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class)
      return Boolean
   is
      pragma Unreferenced (Widget, Frame_Clock);
   begin
      Tick_Called := True;
      return False;
   end Tick;

   package Tick_With_Data is new Add_Tick_Callback_User_Data (Natural);

   function Data_Tick
     (Widget      : not null access Gtk_Widget_Record'Class;
      Frame_Clock : not null access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class;
      User_Data   : Natural) return Boolean;

   function Data_Tick
     (Widget      : not null access Gtk_Widget_Record'Class;
      Frame_Clock : not null access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class;
      User_Data   : Natural) return Boolean
   is
      pragma Unreferenced (Widget, Frame_Clock);
   begin
      Data_Tick_Called := User_Data = 17;
      return False;
   end Data_Tick;

   --  The common void callbacks let the test connect both typed and GObject
   --  slot handlers to each widget lifecycle signal.
   procedure On_Widget_Void (Self : access Gtk_Widget_Record'Class);
   procedure On_GObject_Void (Self : access GObject_Record'Class);
   procedure On_Widget_Text_Direction
     (Self : access Gtk_Widget_Record'Class; Direction : Gtk_Text_Direction);
   procedure On_GObject_Text_Direction
     (Self : access GObject_Record'Class; Direction : Gtk_Text_Direction);
   function On_Widget_Keynav
     (Self : access Gtk_Widget_Record'Class; Direction : Gtk_Direction_Type)
      return Boolean;
   function On_GObject_Keynav
     (Self : access GObject_Record'Class; Direction : Gtk_Direction_Type)
      return Boolean;
   function On_Widget_Mnemonic
     (Self : access Gtk_Widget_Record'Class; Group_Cycling : Boolean)
      return Boolean;
   function On_GObject_Mnemonic
     (Self : access GObject_Record'Class; Group_Cycling : Boolean)
      return Boolean;

   --  Lifecycle callbacks assert that GTK passed the live sender object.
   procedure On_Widget_Void (Self : access Gtk_Widget_Record'Class) is
   begin
      Assert_Nonnull (Get_Object (Self));
      Void_Signal_Calls := Void_Signal_Calls + 1;
   end On_Widget_Void;

   --  The slot form receives the same sender through its broader GObject view.
   procedure On_GObject_Void (Self : access GObject_Record'Class) is
   begin
      Assert_Nonnull (Get_Object (Self));
      Slot_Void_Calls := Slot_Void_Calls + 1;
   end On_GObject_Void;

   procedure On_Widget_Text_Direction
     (Self : access Gtk_Widget_Record'Class; Direction : Gtk_Text_Direction) is
   begin
      Assert_Nonnull (Get_Object (Self));
      Assert_True (Direction /= Text_Dir_Rtl);
      Direction_Calls := Direction_Calls + 1;
   end On_Widget_Text_Direction;

   procedure On_GObject_Text_Direction
     (Self : access GObject_Record'Class; Direction : Gtk_Text_Direction) is
   begin
      Assert_Nonnull (Get_Object (Self));
      Assert_True (Direction /= Text_Dir_Rtl);
      Slot_Direction_Calls := Slot_Direction_Calls + 1;
   end On_GObject_Text_Direction;

   --  Returning False from the first key-navigation handler lets the slot
   --  handler decide the signal result and exercises both enum marshallers.
   function On_Widget_Keynav
     (Self : access Gtk_Widget_Record'Class; Direction : Gtk_Direction_Type)
      return Boolean is
   begin
      Assert_Nonnull (Get_Object (Self));
      Assert_True (Direction = Dir_Up);
      Keynav_Calls := Keynav_Calls + 1;
      return False;
   end On_Widget_Keynav;

   function On_GObject_Keynav
     (Self : access GObject_Record'Class; Direction : Gtk_Direction_Type)
      return Boolean is
   begin
      Assert_Nonnull (Get_Object (Self));
      Assert_True (Direction = Dir_Up);
      Slot_Keynav_Calls := Slot_Keynav_Calls + 1;
      return True;
   end On_GObject_Keynav;

   function On_Widget_Mnemonic
     (Self : access Gtk_Widget_Record'Class; Group_Cycling : Boolean)
      return Boolean is
   begin
      Assert_Nonnull (Get_Object (Self));
      Assert_False (Group_Cycling);
      Mnemonic_Calls := Mnemonic_Calls + 1;
      return False;
   end On_Widget_Mnemonic;

   function On_GObject_Mnemonic
     (Self : access GObject_Record'Class; Group_Cycling : Boolean)
      return Boolean is
   begin
      Assert_Nonnull (Get_Object (Self));
      Assert_False (Group_Cycling);
      Slot_Mnemonic_Calls := Slot_Mnemonic_Calls + 1;
      return True;
   end On_GObject_Mnemonic;

   ---------------------
   -- Test_Properties --
   ---------------------

   procedure Test_Properties is
      Label : constant Gtk_Label := Gtk_Label_New ("widget properties");
      Width, Height : Gint;
   begin
      --  Check that the ordinary boolean and enum properties round-trip.
      Set_Can_Focus (Label, False);
      Assert_False (Get_Can_Focus (Label));
      Set_Can_Focus (Label, True);
      Assert_True (Get_Can_Focus (Label));
      Set_Can_Target (Label, False);
      Assert_False (Get_Can_Target (Label));
      Set_Can_Target (Label, True);
      Assert_True (Get_Can_Target (Label));
      Set_Focusable (Label, True);
      Assert_True (Get_Focusable (Label));
      Set_Focus_On_Click (Label, False);
      Assert_False (Get_Focus_On_Click (Label));
      Set_Halign (Label, Align_End);
      Assert_True (Get_Halign (Label) = Align_End);
      Set_Valign (Label, Align_Center);
      Assert_True (Get_Valign (Label) = Align_Center);

      --  Margins, name, opacity and overflow are independent widget state.
      Set_Margin_Bottom (Label, 7);
      Set_Margin_End (Label, 8);
      Set_Margin_Start (Label, 9);
      Set_Margin_Top (Label, 10);
      Assert_Cmpint_Eq (Get_Margin_Bottom (Label), 7);
      Assert_Cmpint_Eq (Get_Margin_End (Label), 8);
      Assert_Cmpint_Eq (Get_Margin_Start (Label), 9);
      Assert_Cmpint_Eq (Get_Margin_Top (Label), 10);
      Set_Name (Label, "widget-test-name");
      Assert_Cmpstr_Eq (Get_Name (Label), "widget-test-name");
      Set_Opacity (Label, 0.5);
      Assert_Cmpfloat_With_Epsilon (Get_Opacity (Label), 0.5, 0.01);
      Set_Overflow (Label, Overflow_Hidden);
      Assert_True (Get_Overflow (Label) = Overflow_Hidden);

      --  Expand, sensitivity, focus and default-action flags have paired APIs.
      Set_Hexpand (Label, True);
      Assert_True (Get_Hexpand (Label));
      Assert_True (Get_Hexpand_Set (Label));
      Set_Hexpand_Set (Label, False);
      Assert_False (Get_Hexpand_Set (Label));
      Set_Vexpand (Label, True);
      Assert_True (Get_Vexpand (Label));
      Assert_True (Get_Vexpand_Set (Label));
      Set_Vexpand_Set (Label, False);
      Assert_False (Get_Vexpand_Set (Label));
      Set_Receives_Default (Label, True);
      Assert_True (Get_Receives_Default (Label));
      Set_Sensitive (Label, False);
      Assert_False (Get_Sensitive (Label));
      Assert_False (Is_Sensitive (Label));
      Set_Sensitive (Label, True);
      Assert_True (Is_Sensitive (Label));

      --  Tooltip convenience setters also maintain has-tooltip.
      Set_Tooltip_Text (Label, "plain tooltip");
      Assert_Cmpstr_Eq (Get_Tooltip_Text (Label), "plain tooltip");
      Assert_True (Get_Has_Tooltip (Label));
      Set_Tooltip_Markup (Label, "<b>marked tooltip</b>");
      Assert_Cmpstr_Eq (Get_Tooltip_Markup (Label), "<b>marked tooltip</b>");
      Set_Has_Tooltip (Label, False);
      Assert_False (Get_Has_Tooltip (Label));

      --  CSS class changes are observable through both membership APIs.
      Add_Css_Class (Label, "coverage-class");
      Assert_True (Has_Css_Class (Label, "coverage-class"));
      declare
         Classes : GNAT.Strings.String_List := Get_Css_Classes (Label);
      begin
         Assert_True (Classes'Length >= 1);
         Assert_True (Classes (Classes'First).all = "coverage-class");
         --  The returned Ada strings are owned by this list and are freed below.
         Set_Css_Classes (Label, Classes);
         Assert_True (Has_Css_Class (Label, "coverage-class"));
         for Index in Classes'Range loop
            GNAT.Strings.Free (Classes (Index));
         end loop;
      end;
      Remove_Css_Class (Label, "coverage-class");
      Assert_False (Has_Css_Class (Label, "coverage-class"));

      --  GtkStateFlags is a bitset; changing one bit preserves the other bits.
      Set_State_Flags (Label, Gtk_State_Flag_Selected, Clear => True);
      Assert_True ((Get_State_Flags (Label) and Gtk_State_Flag_Selected) /= 0);
      Unset_State_Flags (Label, Gtk_State_Flag_Selected);
      Assert_False ((Get_State_Flags (Label) and Gtk_State_Flag_Selected) /= 0);

      --  Missing action names return false without activating anything.
      Assert_False (Activate (Label));
      Action_Set_Enabled (Label, "missing.action", False);
      Assert_False
        (Label.Activate_Action_Variant
           ("missing.action", Glib.Variant.Null_Gvariant));
      Activate_Default (Label);
      Label.Insert_Action_Group
        ("coverage", Glib.Action_Group.Null_Gaction_Group);

      --  Template lookups on an ordinary widget are empty and harmless.
      Assert_True
        (Get_Template_Child (Label, Gtk.Label.Get_Type, "missing-child") = null);

      --  Newer GTK releases expose a per-widget event propagation limit.
      Set_Limit_Events (Label, True);
      Assert_True (Get_Limit_Events (Label));
      Set_Limit_Events (Label, False);
      Assert_False (Get_Limit_Events (Label));

      --  Request helpers agree on the minimum and natural extents.
      Set_Size_Request (Label, 80, 24);
      Get_Size_Request (Label, Width, Height);
      Assert_Cmpint_Eq (Width, 80);
      Assert_Cmpint_Eq (Height, 24);
      declare
         Minimum, Natural : Gtk_Requisition;
      begin
         Get_Preferred_Size (Label, Minimum, Natural);
         Assert_Cmpint_Ge (Natural.Width, Minimum.Width);
         Assert_Cmpint_Ge (Natural.Height, Minimum.Height);
      end;
      declare
         Minimum, Natural : Gint;
         Minimum_Baseline, Natural_Baseline : Gint;
      begin
         Measure
           (Label, Orientation_Horizontal, -1,
            Minimum, Natural, Minimum_Baseline, Natural_Baseline);
         Assert_Cmpint_Ge (Natural, Minimum);
      end;

      --  Direction overrides are local; the process default is restored.
      Set_Direction (Label, Text_Dir_Rtl);
      Assert_True (Get_Direction (Label) = Text_Dir_Rtl);
      declare
         Default_Direction : constant Gtk_Text_Direction := Get_Default_Direction;
      begin
         Set_Default_Direction (Text_Dir_Rtl);
         Assert_True (Get_Default_Direction = Text_Dir_Rtl);
         Set_Default_Direction (Default_Direction);
      end;

      --  Visibility helpers report both the local flag and effective state.
      Set_Visible (Label, False);
      Assert_False (Get_Visible (Label));
      Set_Visible (Label, True);
      Assert_True (Get_Visible (Label));
      Assert_True (Is_Visible (Label));
      Hide (Label);
      Assert_False (Is_Visible (Label));
      Show (Label);
      Assert_True (Is_Visible (Label));

      --  Unmapped widgets still have a CSS name, context and useful requests.
      Assert_Cmpstr_Eq (Get_Css_Name (Label), "label");
      Assert_Nonnull (Get_Object (Create_Pango_Context (Label)));
      Assert_Cmpstr_Eq (Pango.Layout.Get_Text (Create_Pango_Layout (Label, "layout")), "layout");
      Assert_True (Get_Baseline (Label) >= -1);
      Assert_True (Get_Allocated_Baseline (Label) >= -1);
      Assert_True (Get_Allocated_Width (Label) >= 0);
      Assert_True (Get_Allocated_Height (Label) >= 0);
      Assert_True (Get_Width (Label) >= 0);
      Assert_True (Get_Height (Label) >= 0);
      Assert_True
        (Gtk_Size_Request_Mode'Pos (Get_Request_Mode (Label)) in 0 .. 2);
      Assert_False (Get_Mapped (Label));
      Assert_False (Get_Realized (Label));
      Assert_False (Is_Drawable (Label));
      Assert_False (Has_Focus (Label));
      Assert_False (Has_Default (Label));
      Assert_False (Has_Visible_Focus (Label));
      Assert_False (Is_Focus (Label));
      Assert_False (In_Destruction (Label));
   end Test_Properties;

   --------------------
   -- Test_Hierarchy --
   --------------------

   procedure Test_Hierarchy is
      Parent : constant Gtk_Label := Gtk_Label_New ("parent");
      First  : constant Gtk_Label := Gtk_Label_New ("first");
      Middle : constant Gtk_Label := Gtk_Label_New ("middle");
      Last   : constant Gtk_Label := Gtk_Label_New ("last");
      Model  : Glib.List_Model.Glist_Model;
      Click  : constant Gtk_Gesture_Click := Gtk_Gesture_Click_New;
      Controller_Count : Guint;
   begin
      --  These raw child operations mirror the widgetorder C test and are
      --  intended for widget implementations rather than application layout.
      --  The initial links establish a three-child order for both sibling APIs.
      Set_Parent (First, Parent);
      Insert_After (Last, Parent, First);
      Insert_Before (Middle, Parent, Last);

      Assert_True (Get_Object (Get_Parent (First)) = Get_Object (Parent));
      Assert_True (Get_Object (Get_Parent (Middle)) = Get_Object (Parent));
      Assert_True (Get_Object (Get_Parent (Last)) = Get_Object (Parent));
      Assert_True (Get_Object (Get_First_Child (Parent)) = Get_Object (First));
      Assert_True (Get_Object (Get_Last_Child (Parent)) = Get_Object (Last));
      Assert_True (Get_Object (Get_Next_Sibling (First)) = Get_Object (Middle));
      Assert_True (Get_Object (Get_Prev_Sibling (Middle)) = Get_Object (First));
      Assert_True (Get_Object (Get_Next_Sibling (Middle)) = Get_Object (Last));
      Assert_True (Get_Object (Get_Prev_Sibling (Last)) = Get_Object (Middle));
      Assert_True (Get_Prev_Sibling (First) = null);
      Assert_True (Get_Next_Sibling (Last) = null);
      --  A widget's child and controller observations are list models.
      Model := Observe_Children (Parent);
      Assert_Cmpuint_Eq (Get_N_Items (Model), 3);
      Assert_Nonnull (Get_Object (Get_Item (Model, 1)));
      --  Adding a controller changes the observed model's item count.
      Model := Observe_Controllers (Parent);
      Controller_Count := Get_N_Items (Model);
      Add_Controller (Parent, Click);
      Model := Observe_Controllers (Parent);
      Assert_Cmpuint_Eq (Get_N_Items (Model), Controller_Count + 1);

      --  Child visibility is state on the child and is reset by unparenting.
      Set_Child_Visible (Middle, False);
      Assert_False (Get_Child_Visible (Middle));
      Set_Child_Visible (Middle, True);
      Assert_True (Get_Child_Visible (Middle));

      --  Focus-child bookkeeping and mnemonic label lists are inspectable.
      Set_Focus_Child (Parent, Middle);
      Assert_True (Get_Object (Get_Focus_Child (Parent)) = Get_Object (Middle));
      Add_Mnemonic_Label (First, Last);
      Assert_Cmpuint_Eq (Widget_List.Length (List_Mnemonic_Labels (First)), 1);
      Remove_Mnemonic_Label (First, Last);
      Assert_Cmpuint_Eq (Widget_List.Length (List_Mnemonic_Labels (First)), 0);

      --  Removing one child leaves the order and parent links consistent.
      Unparent (Middle);
      Assert_True (Get_Parent (Middle) = null);
      Assert_True (Get_Object (Get_Next_Sibling (First)) = Get_Object (Last));
      Unparent (Last);
      Unparent (First);
   end Test_Hierarchy;

   ------------------------
   -- Test_Mapped_Widget --
   ------------------------

   procedure Test_Mapped_Widget is
      Window : constant Gtk_Window := Gtk_Window_New;
      Fixed  : constant Gtk_Fixed := Gtk_Fixed_New;
      Label  : constant Gtk_Label := Gtk_Label_New ("mapped widget");
      Tick_Id, Data_Tick_Id : Guint;
      Allocation : aliased Gdk_Rectangle;
      Point, Out_Point : aliased Graphene.Point.Graphene_Point_T;
      Bounds : aliased Graphene.Rect.Graphene_Rect_T;
      Transform : aliased Graphene.Matrix.Graphene_Matrix_T;
      X, Y : Gdouble;
      Coordinates_Ok : Boolean;
      Width, Height : Gint;
      Color : Gdk.RGBA.Gdk_RGBA;

   begin
      --  The window roots the tree so display, native and frame-clock getters
      --  have valid backing objects under the testsuite's Xvfb display.
      Void_Signal_Calls := 0;
      Slot_Void_Calls := 0;
      Direction_Calls := 0;
      Slot_Direction_Calls := 0;
      Keynav_Calls := 0;
      Slot_Keynav_Calls := 0;
      Mnemonic_Calls := 0;
      Slot_Mnemonic_Calls := 0;

      --  Typed and GObject-slot handlers exercise both marshaller paths.
      On_Destroy (Label, On_Widget_Void'Unrestricted_Access);
      On_Destroy (Label, On_GObject_Void'Unrestricted_Access, Slot => Window);
      On_Direction_Changed
        (Label, On_Widget_Text_Direction'Unrestricted_Access);
      On_Direction_Changed
        (Label, On_GObject_Text_Direction'Unrestricted_Access, Slot => Window);
      On_Hide (Label, On_Widget_Void'Unrestricted_Access);
      On_Hide (Label, On_GObject_Void'Unrestricted_Access, Slot => Window);
      On_Keynav_Failed (Label, On_Widget_Keynav'Unrestricted_Access);
      On_Keynav_Failed
        (Label, On_GObject_Keynav'Unrestricted_Access, Slot => Window);
      On_Map (Label, On_Widget_Void'Unrestricted_Access);
      On_Map (Label, On_GObject_Void'Unrestricted_Access, Slot => Window);
      On_Mnemonic_Activate (Label, On_Widget_Mnemonic'Unrestricted_Access);
      On_Mnemonic_Activate
        (Label, On_GObject_Mnemonic'Unrestricted_Access, Slot => Window);
      On_Realize (Label, On_Widget_Void'Unrestricted_Access);
      On_Realize (Label, On_GObject_Void'Unrestricted_Access, Slot => Window);
      On_Show (Label, On_Widget_Void'Unrestricted_Access);
      On_Show (Label, On_GObject_Void'Unrestricted_Access, Slot => Window);
      On_Unmap (Label, On_Widget_Void'Unrestricted_Access);
      On_Unmap (Label, On_GObject_Void'Unrestricted_Access, Slot => Window);
      On_Unrealize (Label, On_Widget_Void'Unrestricted_Access);
      On_Unrealize (Label, On_GObject_Void'Unrestricted_Access, Slot => Window);

      --  These direct widget operations emit the non-lifecycle signals.
      Hide (Label);
      Show (Label);
      --  Direction changes reach both typed and slot callbacks.
      Set_Direction (Label, Text_Dir_Rtl);
      Set_State_Flags (Label, Gtk_State_Flag_Selected, Clear => True);
      Unset_State_Flags (Label, Gtk_State_Flag_Selected);
      --  The slot handler reports the keynav signal as handled.
      --  The result is true because the slot callback marks navigation handled.
      Assert_True (Keynav_Failed (Label, Dir_Up));
      --  The mnemonic callback handles activation before GtkLabel's default.
      Assert_True (Mnemonic_Activate (Label, False));
      Set_Has_Tooltip (Label, True);

      Set_Size_Request (Label, 100, 40);
      Fixed.Put (Label, 12.0, 15.0);
      Window.Set_Child (Fixed);
      Window.Set_Default_Size (240, 120);
      Window.Present;

      --  Pump GTK until the label is mapped; the timeout bounds a broken test.
      Tick_Called := False;
      Data_Tick_Called := False;
      Wait_Done := False;
      declare
         Timeout : constant G_Source_Id := Timeout_Add
           (5000, Stop_Waiting'Unrestricted_Access);
         pragma Unreferenced (Timeout);
      begin
         while (not Get_Mapped (Label) or else Get_Allocated_Width (Label) = 0)
           and not Wait_Done
         loop
            declare
               Dispatched : constant Boolean := Main_Context_Iteration (null, True);
               pragma Unreferenced (Dispatched);
            begin
               null;
            end;
         end loop;
      end;
      Assert_True (Get_Mapped (Label));
      Assert_Cmpint_Eq (Gint (Direction_Calls), 1);
      Assert_Cmpint_Eq (Gint (Slot_Direction_Calls), 1);
      Assert_Cmpint_Eq (Gint (Keynav_Calls), 1);
      Assert_Cmpint_Eq (Gint (Slot_Keynav_Calls), 1);
      Assert_Cmpint_Eq (Gint (Mnemonic_Calls), 1);
      Assert_Cmpint_Eq (Gint (Slot_Mnemonic_Calls), 1);
      Assert_Cmpint_Gt (Gint (Void_Signal_Calls), 0);
      Assert_Cmpint_Gt (Gint (Slot_Void_Calls), 0);

      --  These context getters require a mapped widget with a real root.
      Assert_Nonnull (Get_Object (Get_Display (Label)));
      declare
         --  Gtk.Native's interface methods must be called explicitly here:
         --  Gtk.Window also has same-named class methods with separate Ada
         --  bodies, so those calls alone do not cover the interface binding.
         Native : constant Gtk.Native.Gtk_Native := Get_Native (Label);
         Surface : constant Gdk.Gdk_Surface := Gtk.Native.Get_Surface (Native);
         Native_From_Surface : constant Gtk.Native.Gtk_Native :=
           Gtk.Native.Get_For_Surface (Surface);
      begin
         Assert_Nonnull (Get_Object (Surface));
         Assert_Nonnull (Get_Object (Gtk.Native.Get_Renderer (Native)));
         --  The reverse lookup returns the native widget owning this surface.
         Assert_True (Native_From_Surface = Native);
         --  The interface's unary plus conversion preserves its value.
         Assert_True (Gtk.Native."+" (Native) = Native);
      end;
      declare
         Root : constant Gtk.Root.Gtk_Root := Get_Root (Label);
      begin
         --  Gtk.Root has interface methods separate from Gtk.Window's methods.
         Assert_Nonnull (Get_Object (Gtk.Root.Get_Display (Root)));
         --  The unary plus conversion preserves the interface value.
         Assert_True (Gtk.Root."+" (Root) = Root);
         --  Root focus can be assigned and cleared without activating a window.
         Set_Focusable (Label, True);
         Gtk.Root.Set_Focus (Root, Label);
         Assert_True
           (Gtk.Root.Get_Focus (Root) = Gtk.Widget.Gtk_Widget (Label));
         Gtk.Root.Set_Focus (Root, null);
         Assert_True (Gtk.Root.Get_Focus (Root) = null);
      end;
      Assert_Nonnull (Get_Object (Get_Settings (Label)));
      Assert_Nonnull (Get_Object (Get_Primary_Clipboard (Label)));
      Assert_Nonnull (Get_Object (Get_Pango_Context (Label)));
      --  A widget with no custom map returns null; its Pango context still
      --  supplies the inherited map used to render the label.
      Assert_True (Get_Font_Map (Label) = null);
      Set_Font_Map (Label, null);
      Assert_True (Get_Font_Map (Label) = null);
      --  No font options override is installed on this widget.
      Assert_True (Get_Font_Options (Label) = Cairo.Null_Font_Options);
      Assert_Nonnull (Get_Object (Get_Frame_Clock (Label)));
      Assert_Cmpint_Gt (Get_Scale_Factor (Label), 0);
      Assert_True (Get_Realized (Label));
      Assert_True (Grab_Focus (Label) = Has_Focus (Label));
      Assert_True (Is_Drawable (Label));
      Assert_Cmpint_Gt (Get_Allocated_Width (Label), 0);
      Assert_Cmpint_Gt (Get_Allocated_Height (Label), 0);
      Assert_Cmpint_Eq
        (Get_Size (Label, Orientation_Horizontal), Get_Width (Label));
      Get_Allocation (Label, Allocation);
      Assert_Cmpint_Eq (Allocation.Width, Get_Width (Label));
      Assert_Cmpint_Eq (Allocation.Height, Get_Height (Label));
      Get_Size_Request (Label, Width, Height);
      Assert_Cmpint_Eq (Width, 100);
      Assert_Cmpint_Eq (Height, 40);

      --  Identity-local coordinates avoid platform-specific screen positions.
      Point.X := 0.0;
      Point.Y := 0.0;
      Assert_True
        (Gtk.Widget.Compute_Point
           (Gtk_Widget_Record (Label.all)'Access,
            Label, Point'Access, Out_Point'Access));
      Assert_Cmpfloat_With_Epsilon (Gdouble (Out_Point.X), Gdouble (Point.X), 0.001);
      Assert_Cmpfloat_With_Epsilon (Gdouble (Out_Point.Y), Gdouble (Point.Y), 0.001);
      Assert_True
        (Gtk.Widget.Compute_Bounds
           (Gtk_Widget_Record (Label.all)'Access, Label, Bounds'Access));
      Assert_Cmpfloat_With_Epsilon
        (Gdouble (Bounds.Size.Width), Gdouble (Get_Width (Label)), 0.001);
      Assert_True
        (Gtk.Widget.Compute_Transform
           (Gtk_Widget_Record (Label.all)'Access, Label, Transform'Access));
      Translate_Coordinates
        (Label, Label, 4.0, 6.0, X, Y, Coordinates_Ok);
      Assert_True (Coordinates_Ok);
      Assert_Cmpfloat_With_Epsilon (X, 4.0, 0.001);
      Assert_Cmpfloat_With_Epsilon (Y, 6.0, 0.001);
      Assert_True (Contains (Label, 0.0, 0.0));
      Assert_True
        (Get_Object (Pick (Fixed, 15.0, 18.0, Pick_Default)) = Get_Object (Label));

      --  Measurement and sizing use the allocated widget, not the window.
      declare
         Minimum, Natural : Gint;
         Minimum_Baseline, Natural_Baseline : Gint;
      begin
         Measure
           (Label, Orientation_Horizontal, -1,
            Minimum, Natural, Minimum_Baseline, Natural_Baseline);
         Assert_Cmpint_Ge (Natural, Minimum);
      end;
      Get_Color (Label, Color);
      Assert_True (Color.Alpha >= 0.0 and Color.Alpha <= 1.0);
      Error_Bell (Label);
      Trigger_Tooltip_Query (Label);
      Assert_True (Get_Has_Tooltip (Label));

      --  Tick callbacks exercise both the plain and user-data marshallers.
      --  Both callback registrations return ids that must be removed later.
      Tick_Id := Add_Tick_Callback (Label, Tick'Unrestricted_Access);
      Data_Tick_Id := Tick_With_Data.Add_Tick_Callback
        (Label, Data_Tick'Unrestricted_Access, 17);
      Assert_Cmpuint_Gt (Tick_Id, 0);
      Assert_Cmpuint_Gt (Data_Tick_Id, 0);
      Wait_Done := False;
      declare
         Timeout : constant G_Source_Id := Timeout_Add
           (5000, Stop_Waiting'Unrestricted_Access);
         pragma Unreferenced (Timeout);
      begin
         while not (Tick_Called and Data_Tick_Called) and not Wait_Done loop
            declare
               Dispatched : constant Boolean := Main_Context_Iteration (null, True);
               pragma Unreferenced (Dispatched);
            begin
               null;
            end;
         end loop;
      end;
      Assert_True (Tick_Called);
      Assert_True (Data_Tick_Called);
      Remove_Tick_Callback (Label, Tick_Id);
      Remove_Tick_Callback (Label, Data_Tick_Id);

      --  Cursor names are resolved against the widget's realized display.
      Set_Cursor_From_Name (Label, "default");
      Assert_Nonnull (Get_Object (Get_Cursor (Label)));
      --  A null cursor and an empty name both clear the widget override.
      Set_Cursor (Label, null);
      Set_Cursor_From_Name (Label, "");

      --  Queue helpers are valid after mapping and request future layout work.
      Queue_Allocate (Label);
      Queue_Draw (Label);
      Queue_Resize (Label);
      Assert_True (Should_Layout (Fixed));
      Assert_False (Compute_Expand (Fixed, Orientation_Horizontal));
      Assert_False (Drag_Check_Threshold (Label, 10, 10, 11, 11));
      Assert_True (Drag_Check_Threshold (Label, 10, 10, 80, 80));
      Assert_False (Child_Focus (Fixed, Dir_Tab_Forward));

      --  Hiding the root unmaps its child and makes the state query observable.
      Window.Hide;
      Assert_False (Get_Mapped (Label));
      Assert_False (Is_Drawable (Label));
      Window.Destroy;
   end Test_Mapped_Widget;

   --------------------------
   -- Test_Inherited_Vfuncs --
   --------------------------

   procedure Test_Inherited_Vfuncs is
      Window : constant Gtk_Window := Gtk_Window_New;
      Widget : constant Test_Widget := new Test_Widget_Record;
      Allocation : Gdk_Rectangle;
   begin
      --  The parent wrappers are invoked from the subclass callbacks, not by
      --  calling GTK's protected lifecycle methods on an ordinary widget.
      Measure_Calls := 0;
      Allocation_Calls := 0;
      Realize_Calls := 0;
      Snapshot_Calls := 0;
      Initialize_Test_Widget (Widget);
      Window.Set_Child (Widget.all'Unrestricted_Access);
      Window.Set_Default_Size (180, 90);
      Window.Present;

      --  Processing a frame makes GTK run layout, realization and snapshot.
      Wait_Done := False;
      declare
         Timeout : constant G_Source_Id := Timeout_Add
           (5000, Stop_Waiting'Unrestricted_Access);
         pragma Unreferenced (Timeout);
      begin
         while (not Get_Mapped (Widget) or else Snapshot_Calls = 0)
           and not Wait_Done
         loop
            declare
               Dispatched : constant Boolean := Main_Context_Iteration (null, True);
               pragma Unreferenced (Dispatched);
            begin
               null;
            end;
         end loop;
      end;
      Assert_True (Get_Mapped (Widget));
      --  Explicitly allocating an already realized custom widget is the
      --  supported container-side path for exercising these wrappers.
      Get_Allocation (Widget, Allocation);
      Size_Allocate (Widget, Allocation, -1);
      Realize (Widget);
      Assert_Cmpint_Gt (Get_Allocated_Width (Widget), 0);
      Assert_Cmpint_Gt (Gint (Measure_Calls), 0);
      Assert_Cmpint_Gt (Gint (Allocation_Calls), 0);
      Assert_Cmpint_Gt (Gint (Realize_Calls), 0);
      --  Each counter is incremented inside the corresponding C vfunc callback.
      Assert_Cmpint_Gt (Gint (Snapshot_Calls), 0);
      --  These lifecycle entry points are intended for widget implementations;
      --  the custom child is realized and parented exactly as those callers
      --  require.
      Unmap (Widget);
      Assert_False (Get_Mapped (Widget));
      Unrealize (Widget);
      Assert_False (Get_Realized (Widget));
      Realize (Widget);
      Map (Widget);
      Assert_True (Get_Mapped (Widget));
      Window.Destroy;
   end Test_Inherited_Vfuncs;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/gtk-widget/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func ("/gtk-widget/hierarchy", Test_Hierarchy'Unrestricted_Access);
   Glib.Test.Add_Func ("/gtk-widget/mapped", Test_Mapped_Widget'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gtk-widget/inherited-vfuncs", Test_Inherited_Vfuncs'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Gtk_Widget;
