--  Tests for Gtk.Scale and its parent Gtk.GRange (GtkRange).
--
--  GTK has no C test of its own for GtkScale, so this is written directly
--  against the Ada API. Only the formatting tests use a window: the rest
--  runs on scales that are never shown. It covers the constructors (with
--  a range, with a shared adjustment), the property
--  accessors of both classes, the adjustment being shared between the range
--  and its owner, the value-changed signal, marks and Set_Format_Value_Func
--  (both flavours: GTK calls the formatter and shows what it returns, on a
--  scale in a window as well, and the user data of the second one is handed
--  back and destroyed).

with Glib;             use Glib;
with Glib.Main;        use Glib.Main;
with Glib.Object;
with Glib.Properties;  use Glib.Properties;
with Glib.Test;        use Glib.Test;
with Ada.Command_Line;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Accessible;
with Gtk.Accessible_Range;
with Gtk.Atcontext;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.GRange;       use Gtk.GRange;
with Gtk.Main;
with Gtk.Scale;        use Gtk.Scale;
with Gtk.Window;       use Gtk.Window;
with Gtkada.Types;
with Gdk.Rectangle;
with System;

procedure Scale is

   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Accessible.Gtk_Accessible_Role;
   use type Gtk.Accessible_Range.Gtk_Accessible_Range;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gtk.GRange.Gtk_Range;
   use type System.Address;

   Value_Changed_Count : Gint := 0;

   Bounds_Typed_Count : Natural := 0;
   Bounds_Slot_Count  : Natural := 0;
   Change_Typed_Count : Natural := 0;
   Change_Slot_Count  : Natural := 0;
   Move_Typed_Count   : Natural := 0;
   Move_Slot_Count    : Natural := 0;
   Slot_Value_Count   : Natural := 0;
   --  Counters make both signal overloads and their parameters observable.

   Last_Typed_Object : System.Address := System.Null_Address;
   Last_Slot_Object  : System.Address := System.Null_Address;
   Last_Signal_Value : Gdouble := 0.0;
   Last_Scroll       : Gtk_Scroll_Type := Scroll_None;

   Format_Calls   : Natural := 0;
   --  How many times GTK called a formatter

   Destroyed      : Natural := 0;
   Last_Destroyed : Unbounded_String;
   --  How many times, and for which data, the user data of the formatter
   --  was destroyed

   procedure Test_New_With_Range
   with Convention => C;

   procedure Test_New_With_Adjustment
   with Convention => C;

   procedure Test_Scale_Accessors
   with Convention => C;

   procedure Test_Range_Accessors
   with Convention => C;

   procedure Test_Range_Accessibility
   with Convention => C;

   procedure Test_Scale_Interface_Wrappers
   with Convention => C;

   procedure Test_Range_Layout
   with Convention => C;

   procedure Test_Range_Signals
   with Convention => C;

   procedure Test_Set_Range
   with Convention => C;

   procedure Test_Value_Changed
   with Convention => C;

   procedure Test_Marks
   with Convention => C;

   procedure Test_Format_Value_Func
   with Convention => C;

   procedure Test_Format_Drawn
   with Convention => C;

   procedure Test_Format_User_Data
   with Convention => C;

   procedure Value_Changed_Cb (Self : access Gtk_Range_Record'Class);

   --  Emit the range's action signals directly, avoiding user input and
   --  allowing the generated Ada marshallers to receive known arguments.
   procedure Emit_Double_Signal
     (Object : System.Address;
      Name   : Gtkada.Types.Chars_Ptr;
      Value  : Gdouble)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Scroll_Value_Signal
     (Object       : System.Address;
      Name         : Gtkada.Types.Chars_Ptr;
      Scroll       : Gtk_Scroll_Type;
      Value        : Gdouble;
      Return_Value : access Glib.Gboolean)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Scroll_Signal
     (Object : System.Address;
      Name   : Gtkada.Types.Chars_Ptr;
      Scroll : Gtk_Scroll_Type)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Bounds_Typed
     (Self : access Gtk_Range_Record'Class; Value : Gdouble);
   procedure Bounds_Slot
     (Self : access Glib.Object.GObject_Record'Class; Value : Gdouble);
   function Change_Typed
     (Self : access Gtk_Range_Record'Class;
      Scroll : Gtk_Scroll_Type; Value : Gdouble) return Boolean;
   function Change_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Scroll : Gtk_Scroll_Type; Value : Gdouble) return Boolean;
   procedure Move_Typed
     (Self : access Gtk_Range_Record'Class; Scroll : Gtk_Scroll_Type);
   procedure Move_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Scroll : Gtk_Scroll_Type);
   procedure Value_Slot
     (Self : access Glib.Object.GObject_Record'Class);

   function Format (Scale : not null access Gtk_Scale_Record'Class;
                    Value : Gdouble) return UTF8_String;

   function Format_With_Data
     (Scale : not null access Gtk_Scale_Record'Class;
      Value : Gdouble;
      Data  : String) return UTF8_String;

   procedure Note_Destroyed (Data : in out String);

   package Formatter is new Set_Format_Value_Func_User_Data
     (String, Note_Destroyed);

   function Image (Value : Gdouble) return String
   is (Trim (Gint'Image (Gint (Value)), Left));

   Timed_Out : Boolean := False;
   pragma Volatile (Timed_Out);
   --  Set by Stop, from a timeout dispatched by Main_Context_Iteration

   function Stop return Boolean;

   procedure Pump;
   --  Run the main loop for a moment, to let GTK realize, lay out and draw
   --  what it has to: the frame clock works from its own timer.

   function Shown_Text (S : not null access Gtk_Scale_Record'Class)
     return String;
   --  The text GTK draws for the value of S

   procedure Value_Changed_Cb (Self : access Gtk_Range_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Value_Changed_Count := Value_Changed_Count + 1;
   end Value_Changed_Cb;

   --  Record each signal's emitter or user-data object and the values passed
   --  through the generated typed and GObject-slot marshallers.
   procedure Bounds_Typed
     (Self : access Gtk_Range_Record'Class; Value : Gdouble) is
   begin
      Bounds_Typed_Count := Bounds_Typed_Count + 1;
      Last_Typed_Object := Self.Get_Object;
      Last_Signal_Value := Value;
   end Bounds_Typed;

   procedure Bounds_Slot
     (Self : access Glib.Object.GObject_Record'Class; Value : Gdouble) is
   begin
      Bounds_Slot_Count := Bounds_Slot_Count + 1;
      Last_Slot_Object := Self.Get_Object;
      Last_Signal_Value := Value;
   end Bounds_Slot;

   function Change_Typed
     (Self : access Gtk_Range_Record'Class;
      Scroll : Gtk_Scroll_Type; Value : Gdouble) return Boolean is
   begin
      Change_Typed_Count := Change_Typed_Count + 1;
      Last_Typed_Object := Self.Get_Object;
      Last_Scroll := Scroll;
      Last_Signal_Value := Value;
      return True;
   end Change_Typed;

   function Change_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Scroll : Gtk_Scroll_Type; Value : Gdouble) return Boolean is
   begin
      Change_Slot_Count := Change_Slot_Count + 1;
      Last_Slot_Object := Self.Get_Object;
      Last_Scroll := Scroll;
      Last_Signal_Value := Value;
      return True;
   end Change_Slot;

   procedure Move_Typed
     (Self : access Gtk_Range_Record'Class; Scroll : Gtk_Scroll_Type) is
   begin
      Move_Typed_Count := Move_Typed_Count + 1;
      Last_Typed_Object := Self.Get_Object;
      Last_Scroll := Scroll;
   end Move_Typed;

   procedure Move_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Scroll : Gtk_Scroll_Type) is
   begin
      Move_Slot_Count := Move_Slot_Count + 1;
      Last_Slot_Object := Self.Get_Object;
      Last_Scroll := Scroll;
   end Move_Slot;

   procedure Value_Slot
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Slot_Value_Count := Slot_Value_Count + 1;
      Last_Slot_Object := Self.Get_Object;
   end Value_Slot;

   function Format (Scale : not null access Gtk_Scale_Record'Class;
                    Value : Gdouble) return UTF8_String
   is
      pragma Unreferenced (Scale);
   begin
      Format_Calls := Format_Calls + 1;
      return "<" & Image (Value) & ">";
   end Format;

   function Format_With_Data
     (Scale : not null access Gtk_Scale_Record'Class;
      Value : Gdouble;
      Data  : String) return UTF8_String
   is
      pragma Unreferenced (Scale);
   begin
      Format_Calls := Format_Calls + 1;
      return Data & ":" & Image (Value);
   end Format_With_Data;

   procedure Note_Destroyed (Data : in out String) is
   begin
      Destroyed := Destroyed + 1;
      Last_Destroyed := To_Unbounded_String (Data);
   end Note_Destroyed;

   function Stop return Boolean is
   begin
      Timed_Out := True;
      Wakeup (null);
      return False;
   end Stop;

   procedure Pump is
      Id : G_Source_Id;
      pragma Unreferenced (Id);
   begin
      Timed_Out := False;
      Id := Timeout_Add (300, Stop'Unrestricted_Access);
      while not Timed_Out loop
         if Main_Context_Iteration (null, May_Block => True) then
            null;
         end if;
      end loop;
   end Pump;

   function Shown_Text (S : not null access Gtk_Scale_Record'Class)
     return String is
   begin
      return S.Get_Layout.Get_Text;
   end Shown_Text;

   procedure Test_New_With_Range is
      S : Gtk_Scale;
      By_Function : Gtk_Scale;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 100.0, 1.0);

      Assert_True (S /= null);
      Assert_Cmpfloat_Eq (S.Get_Adjustment.Get_Lower, 0.0);
      Assert_Cmpfloat_Eq (S.Get_Adjustment.Get_Upper, 100.0);
      Assert_Cmpfloat_Eq (S.Get_Adjustment.Get_Step_Increment, 1.0);
      Assert_True (S.Get_Orientation = Orientation_Horizontal);
      Assert_Cmpfloat_Eq (S.Get_Value, 0.0);

      --  The number of digits is derived from the step: 1.0 needs none,
      --  0.1 needs one.
      Assert_Cmpint_Eq (S.Get_Digits, 0);
      Gtk_New_With_Range (S, Orientation_Vertical, 0.0, 1.0, 0.1);
      Assert_Cmpint_Eq (S.Get_Digits, 1);
      Assert_True (S.Get_Orientation = Orientation_Vertical);

      --  The function constructors return the same configured widget types
      --  as their Gtk_New counterparts.
      By_Function := Gtk_Scale_New_With_Range
        (Orientation_Horizontal, -2.0, 2.0, 0.5);
      Assert_True (By_Function.Get_Orientation = Orientation_Horizontal);
      Assert_Cmpfloat_Eq (By_Function.Get_Adjustment.Get_Lower, -2.0);
      Assert_Cmpfloat_Eq (By_Function.Get_Adjustment.Get_Upper, 2.0);
      Assert_Cmpfloat_Eq
        (By_Function.Get_Adjustment.Get_Step_Increment, 0.5);
   end Test_New_With_Range;

   procedure Test_New_With_Adjustment is
      Adj : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (5.0, 0.0, 20.0, 1.0, 5.0, 0.0);
      S   : Gtk_Scale;
      By_Function : Gtk_Scale;
   begin
      Gtk_New (S, Orientation_Horizontal, Adj);
      Assert_True (S.Get_Adjustment = Adj);
      Assert_Cmpfloat_Eq (S.Get_Value, 5.0);

      --  The adjustment is shared, whichever side changes the value.
      S.Set_Value (7.0);
      Assert_Cmpfloat_Eq (Adj.Get_Value, 7.0);
      Adj.Set_Value (9.0);
      Assert_Cmpfloat_Eq (S.Get_Value, 9.0);

      --  A null adjustment makes the scale create its own.
      Gtk_New (S, Orientation_Vertical, null);
      Assert_True (S.Get_Adjustment /= null);
      Assert_True (S.Get_Adjustment /= Adj);

      S.Set_Adjustment (Adj);
      Assert_True (S.Get_Adjustment = Adj);

      --  The function form must retain the supplied adjustment as well.
      By_Function := Gtk_Scale_New (Orientation_Vertical, Adj);
      Assert_True (By_Function.Get_Adjustment = Adj);
      Assert_True (By_Function.Get_Orientation = Orientation_Vertical);
   end Test_New_With_Adjustment;

   procedure Test_Scale_Accessors is
      S : Gtk_Scale;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 10.0, 1.0);

      --  Defaults of the GtkScale properties.
      Assert_False (S.Get_Draw_Value);
      Assert_True (S.Get_Has_Origin);
      Assert_True (S.Get_Value_Pos = Pos_Top);

      S.Set_Digits (3);
      Assert_Cmpint_Eq (S.Get_Digits, 3);
      Assert_Cmpint_Eq (Get_Property (S, The_Digits_Property), 3);

      S.Set_Draw_Value (True);
      Assert_True (S.Get_Draw_Value);
      S.Set_Draw_Value (False);
      Assert_False (S.Get_Draw_Value);

      S.Set_Has_Origin (False);
      Assert_False (S.Get_Has_Origin);
      S.Set_Has_Origin (True);
      Assert_True (S.Get_Has_Origin);

      for Pos in Gtk_Position_Type loop
         S.Set_Value_Pos (Pos);
         Assert_True (S.Get_Value_Pos = Pos);
      end loop;
   end Test_Scale_Accessors;

   procedure Test_Range_Accessors is
      S : Gtk_Scale;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 100.0, 1.0);

      S.Set_Inverted (True);
      Assert_True (S.Get_Inverted);
      S.Set_Inverted (False);
      Assert_False (S.Get_Inverted);

      S.Set_Flippable (False);
      Assert_False (S.Get_Flippable);
      S.Set_Flippable (True);
      Assert_True (S.Get_Flippable);

      S.Set_Round_Digits (2);
      Assert_Cmpint_Eq (S.Get_Round_Digits, 2);

      S.Set_Fill_Level (40.0);
      Assert_Cmpfloat_Eq (S.Get_Fill_Level, 40.0);

      S.Set_Show_Fill_Level (True);
      Assert_True (S.Get_Show_Fill_Level);
      S.Set_Show_Fill_Level (False);
      Assert_False (S.Get_Show_Fill_Level);

      S.Set_Restrict_To_Fill_Level (False);
      Assert_False (S.Get_Restrict_To_Fill_Level);

      S.Set_Slider_Size_Fixed (True);
      Assert_True (S.Get_Slider_Size_Fixed);
      S.Set_Slider_Size_Fixed (False);
      Assert_False (S.Get_Slider_Size_Fixed);

      S.Set_Increments (2.0, 25.0);
      Assert_Cmpfloat_Eq (S.Get_Adjustment.Get_Step_Increment, 2.0);
      Assert_Cmpfloat_Eq (S.Get_Adjustment.Get_Page_Increment, 25.0);
   end Test_Range_Accessors;

   --  The GtkRange-inherited accessibility API is exercised through a scale,
   --  the concrete public subclass available to applications.
   procedure Test_Range_Accessibility is
      S : Gtk_Scale;
      R : Gtk_Range;
      Base : access Gtk_Range_Record;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      As_Accessible : Gtk.Accessible.Gtk_Accessible;
      As_Accessible_Range : Gtk.Accessible_Range.Gtk_Accessible_Range;
      Back : Gtk_Range;
      X, Y, Width, Height : Gint;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 100.0, 1.0);
      R := Gtk_Range (S);
      --  The explicit parent view selects Gtk.GRange's own interface wrappers;
      --  Gtk.Scale declares parallel wrappers for its inherited interfaces.
      Base := Gtk_Range_Record (S.all)'Access;

      --  A detached scale has a slider role and no accessible tree neighbors.
      --  The role is nonempty even when GTK chooses it through the backend.
      Assert_True
        (Gtk.GRange.Get_Accessible_Role (Base) /=
           Gtk.Accessible.Accessible_Role_None);
      Assert_Cmpstr_Eq (Gtk.GRange.Get_Accessible_Id (Base), "");
      Assert_True
        (Gtk.GRange.Get_Accessible_Parent (Base) =
           Gtk.Accessible.Null_Gtk_Accessible);
      --  GTK exposes the internal slider parts as accessible descendants.
      Assert_True
        (Gtk.GRange.Get_First_Accessible_Child (Base) /=
           Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Gtk.GRange.Get_Next_Accessible_Sibling (Base) =
           Gtk.Accessible.Null_Gtk_Accessible);

      --  No AT context or screen bounds exist before the scale is rooted.
      --  The false bounds result is the documented detached-widget state.
      Context := Gtk.GRange.Get_At_Context (Base);
      Assert_True (Context = null);
      Assert_False (Gtk.GRange.Get_Bounds (Base, X, Y, Width, Height));

      --  Focus state queries reflect the widget's focusable setting.
      --  Focusable is true after enabling it; a newly detached widget is not focused.
      S.Set_Focusable (True);
      Assert_True
        (Gtk.GRange.Get_Platform_State
           (Base, Gtk.Accessible.Accessible_Platform_State_Focusable));
      Assert_False
        (Gtk.GRange.Get_Platform_State
           (Base, Gtk.Accessible.Accessible_Platform_State_Focused));

      --  These updates are public fire-and-forget operations; verify the
      --  standalone parent and sibling stay unset after their null updates.
      --  No assistive technology service is required by these operations.
      Gtk.GRange.Announce
        (Base, "range accessibility test",
         Gtk.Accessible.Accessible_Announcement_Priority_Low);
      Gtk.GRange.Reset_Property
        (Base, Gtk.Accessible.Accessible_Property_Label);
      Gtk.GRange.Reset_Relation
        (Base, Gtk.Accessible.Accessible_Relation_Described_By);
      Gtk.GRange.Reset_State
        (Base, Gtk.Accessible.Accessible_State_Busy);
      Gtk.GRange.Set_Accessible_Parent
        (Base, Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Gtk.GRange.Get_Accessible_Parent (Base) =
           Gtk.Accessible.Null_Gtk_Accessible);
      Gtk.GRange.Update_Next_Accessible_Sibling
        (Base, Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Gtk.GRange.Get_Next_Accessible_Sibling (Base) =
           Gtk.Accessible.Null_Gtk_Accessible);
      Gtk.GRange.Update_Platform_State
        (Base, Gtk.Accessible.Accessible_Platform_State_Focusable);

      --  GtkRange implements AccessibleRange and Orientable; both generated
      --  conversions and the inherited orientation accessors round-trip.
      --  Comparing the returned objects verifies that the views refer to S.
      As_Accessible := Gtk.GRange."+" (R);
      Back := Gtk.GRange."-" (As_Accessible);
      Assert_True (Back = R);
      As_Accessible_Range := Gtk.GRange."+" (R);
      Back := Gtk.GRange."-" (As_Accessible_Range);
      Assert_True (Back = R);
      Assert_True
        (Gtk.GRange.Get_Orientation (Base) = Orientation_Horizontal);
      Gtk.GRange.Set_Orientation (Base, Orientation_Vertical);
      Assert_True
        (Gtk.GRange.Get_Orientation (Base) = Orientation_Vertical);
   end Test_Range_Accessibility;

   --  Gtk.Scale repeats its implemented-interface wrappers in its own
   --  package, so call these through the concrete scale type as well.
   procedure Test_Scale_Interface_Wrappers is
      S : Gtk_Scale;
      X, Y, Width, Height : Gint;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      Parent, Child, Sibling : Gtk.Accessible.Gtk_Accessible;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 100.0, 1.0);

      --  The scale exposes its accessible role, ID, internal child, and
      --  detached tree relationships through Gtk.Scale's wrappers.
      Assert_True
        (S.Get_Accessible_Role /= Gtk.Accessible.Accessible_Role_None);
      Assert_Cmpstr_Eq (S.Get_Accessible_Id, "");
      Parent := S.Get_Accessible_Parent;
      Child := S.Get_First_Accessible_Child;
      Sibling := S.Get_Next_Accessible_Sibling;
      Assert_True (Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (Child /= Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (Sibling = Gtk.Accessible.Null_Gtk_Accessible);

      --  Before rooting a widget, GTK has no AT context or screen bounds.
      Context := S.Get_At_Context;
      Assert_True (Context = null);
      Assert_False (S.Get_Bounds (X, Y, Width, Height));

      --  Widget focusability is reflected by its platform-state query.
      S.Set_Focusable (True);
      Assert_True
        (S.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focusable));
      Assert_False
        (S.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focused));

      --  Null parent/sibling updates preserve the detached relationships;
      --  reset and announcement calls are safe on a standalone widget.
      S.Announce
        ("scale accessibility wrapper", 
         Gtk.Accessible.Accessible_Announcement_Priority_Low);
      S.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      S.Reset_Relation (Gtk.Accessible.Accessible_Relation_Described_By);
      S.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      S.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (S.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      S.Update_Next_Accessible_Sibling (Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (S.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      S.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focusable);

      --  The orientable interface's concrete wrappers round-trip the change.
      Assert_True (S.Get_Orientation = Orientation_Horizontal);
      S.Set_Orientation (Orientation_Vertical);
      Assert_True (S.Get_Orientation = Orientation_Vertical);
   end Test_Scale_Interface_Wrappers;

   --  The trough and thumb geometry is only available after GTK has allocated
   --  the range inside a presented window.
   procedure Test_Range_Layout is
      Window : constant Gtk_Window := Gtk_Window_New;
      S : Gtk_Scale;
      R : Gtk_Range;
      Base : access Gtk_Range_Record;
      Rect : Gdk.Rectangle.Gdk_Rectangle;
      Slider_Start, Slider_End : Gint;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 100.0, 1.0);
      R := Gtk_Range (S);
      Base := Gtk_Range_Record (S.all)'Access;
      Window.Set_Child (S);
      Window.Present;
      --  Pump lets the frame clock allocate the scale before geometry queries.
      Pump;

      --  The trough has a nonempty allocation and the slider range is ordered.
      Gtk.GRange.Get_Range_Rect (Base, Rect);
      Assert_True (Rect.Width > 0 and then Rect.Height > 0);
      Gtk.GRange.Get_Slider_Range (Base, Slider_Start, Slider_End);
      Assert_True (Slider_Start <= Slider_End);

      --  The scale's displayed value layout reports a position after layout.
      declare
         X, Y : Gint;
      begin
         S.Get_Layout_Offsets (X, Y);
         Assert_True (X >= 0 and then Y >= 0);
      end;
      Window.Destroy;
   end Test_Range_Layout;

   --  Cover every GtkRange signal overload by attaching both callback forms
   --  and emitting deterministic arguments directly through GObject.
   procedure Test_Range_Signals is
      Bounds_Typed_Range, Bounds_Slot_Range : Gtk_Scale;
      Change_Typed_Range, Change_Slot_Range : Gtk_Scale;
      Move_Typed_Range, Move_Slot_Range : Gtk_Scale;
      Value_Typed_Range, Value_Slot_Range : Gtk_Scale;
      Slot : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (0.0, 0.0, 100.0, 1.0, 10.0, 0.0);
      Result : aliased Glib.Gboolean := 0;

      --  C strings are scoped to one emission and released immediately after.
      --  This keeps repeated signal emissions independent of each other.
      procedure Emit_Double
        (Object : Gtk_Scale; Signal : String; Value : Gdouble) is
         Name : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String (Signal);
      begin
         Emit_Double_Signal (Object.Get_Object, Name, Value);
         Gtkada.Types.Free (Name);
      end Emit_Double;

      procedure Emit_Scroll_Value
        (Object : Gtk_Scale; Signal : String;
         Scroll : Gtk_Scroll_Type; Value : Gdouble;
         Return_Value : access Glib.Gboolean) is
         Name : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String (Signal);
      begin
         Emit_Scroll_Value_Signal
           (Object.Get_Object, Name, Scroll, Value, Return_Value);
         Gtkada.Types.Free (Name);
      end Emit_Scroll_Value;

      procedure Emit_Scroll
        (Object : Gtk_Scale; Signal : String; Scroll : Gtk_Scroll_Type) is
         Name : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String (Signal);
      begin
         Emit_Scroll_Signal (Object.Get_Object, Name, Scroll);
         Gtkada.Types.Free (Name);
      end Emit_Scroll;
   begin
      Gtk_New_With_Range
        (Bounds_Typed_Range, Orientation_Horizontal, 0.0, 100.0, 1.0);
      Gtk_New_With_Range
        (Bounds_Slot_Range, Orientation_Horizontal, 0.0, 100.0, 1.0);
      Gtk_New_With_Range
        (Change_Typed_Range, Orientation_Horizontal, 0.0, 100.0, 1.0);
      Gtk_New_With_Range
        (Change_Slot_Range, Orientation_Horizontal, 0.0, 100.0, 1.0);
      Gtk_New_With_Range
        (Move_Typed_Range, Orientation_Horizontal, 0.0, 100.0, 1.0);
      Gtk_New_With_Range
        (Move_Slot_Range, Orientation_Horizontal, 0.0, 100.0, 1.0);
      Gtk_New_With_Range
        (Value_Typed_Range, Orientation_Horizontal, 0.0, 100.0, 1.0);
      Gtk_New_With_Range
        (Value_Slot_Range, Orientation_Horizontal, 0.0, 100.0, 1.0);
      --  The typed adjust-bounds overload receives the emitting range/value.
      --  The expected payload also guards the Gdouble conversion.
      Bounds_Typed_Count := 0;
      Bounds_Typed_Range.On_Adjust_Bounds
        (Bounds_Typed'Unrestricted_Access);
      Emit_Double (Bounds_Typed_Range, "adjust-bounds", 21.5);
      Assert_Cmpuint_Eq (Guint (Bounds_Typed_Count), 1);
      Assert_True
        (Last_Typed_Object = Bounds_Typed_Range.Get_Object);
      Assert_Cmpfloat_Eq (Last_Signal_Value, 21.5);

      --  The slot overload receives its explicit GObject, not the emitter.
      --  A GtkAdjustment is a separate GObject suitable for this user-data slot.
      Bounds_Slot_Count := 0;
      Bounds_Slot_Range.On_Adjust_Bounds
        (Bounds_Slot'Unrestricted_Access, Slot);
      Emit_Double (Bounds_Slot_Range, "adjust-bounds", 22.5);
      Assert_Cmpuint_Eq (Guint (Bounds_Slot_Count), 1);
      Assert_True (Last_Slot_Object = Slot.Get_Object);
      Assert_Cmpfloat_Eq (Last_Signal_Value, 22.5);

      --  The boolean signal returns the callback's handled result.
      Change_Typed_Count := 0;
      Change_Typed_Range.On_Change_Value
        (Change_Typed'Unrestricted_Access);
      Result := 0;
      Emit_Scroll_Value
        (Change_Typed_Range, "change-value", Scroll_Step_Forward,
         31.5, Result'Access);
      Assert_True (Result /= 0);
      Assert_Cmpuint_Eq (Guint (Change_Typed_Count), 1);
      Assert_True (Last_Typed_Object = Change_Typed_Range.Get_Object);
      Assert_True (Last_Scroll = Scroll_Step_Forward);
      Assert_Cmpfloat_Eq (Last_Signal_Value, 31.5);

      --  Exercise the GObject-slot boolean marshaller in its own emission.
      Change_Slot_Count := 0;
      Change_Slot_Range.On_Change_Value
        (Change_Slot'Unrestricted_Access, Slot);
      Result := 0;
      Emit_Scroll_Value
        (Change_Slot_Range, "change-value", Scroll_Page_Backward,
         42.5, Result'Access);
      Assert_True (Result /= 0);
      Assert_Cmpuint_Eq (Guint (Change_Slot_Count), 1);
      Assert_True (Last_Slot_Object = Slot.Get_Object);
      Assert_True (Last_Scroll = Scroll_Page_Backward);
      Assert_Cmpfloat_Eq (Last_Signal_Value, 42.5);

      --  The move-slider callbacks receive the selected scroll action.
      Move_Typed_Count := 0;
      Move_Typed_Range.On_Move_Slider (Move_Typed'Unrestricted_Access);
      Emit_Scroll (Move_Typed_Range, "move-slider", Scroll_Page_Forward);
      Assert_Cmpuint_Eq (Guint (Move_Typed_Count), 1);
      Assert_True (Last_Typed_Object = Move_Typed_Range.Get_Object);
      Assert_True (Last_Scroll = Scroll_Page_Forward);

      --  Repeat move-slider through the explicit slot-object overload.
      Move_Slot_Count := 0;
      Move_Slot_Range.On_Move_Slider
        (Move_Slot'Unrestricted_Access, Slot);
      Emit_Scroll (Move_Slot_Range, "move-slider", Scroll_Step_Backward);
      Assert_Cmpuint_Eq (Guint (Move_Slot_Count), 1);
      Assert_True (Last_Slot_Object = Slot.Get_Object);
      Assert_True (Last_Scroll = Scroll_Step_Backward);

      --  A value change drives the typed and slot forms of the void signal.
      --  Separate scales keep the two registered handlers from sharing state.
      Value_Changed_Count := 0;
      Slot_Value_Count := 0;
      Value_Typed_Range.On_Value_Changed
        (Value_Changed_Cb'Unrestricted_Access);
      Value_Slot_Range.On_Value_Changed
        (Value_Slot'Unrestricted_Access, Slot);
      Value_Typed_Range.Set_Value (7.0);
      Value_Slot_Range.Set_Value (8.0);
      Assert_Cmpint_Eq (Value_Changed_Count, 1);
      Assert_Cmpuint_Eq (Guint (Slot_Value_Count), 1);
      Assert_True (Last_Slot_Object = Slot.Get_Object);

   end Test_Range_Signals;

   procedure Test_Set_Range is
      S : Gtk_Scale;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 100.0, 1.0);
      S.Set_Value (50.0);

      --  Narrowing the range clamps the value into it.
      S.Set_Range (60.0, 80.0);
      Assert_Cmpfloat_Eq (S.Get_Adjustment.Get_Lower, 60.0);
      Assert_Cmpfloat_Eq (S.Get_Adjustment.Get_Upper, 80.0);
      Assert_Cmpfloat_Eq (S.Get_Value, 60.0);

      S.Set_Value (1000.0);
      Assert_Cmpfloat_Eq (S.Get_Value, 80.0);
      S.Set_Value (-1000.0);
      Assert_Cmpfloat_Eq (S.Get_Value, 60.0);
   end Test_Set_Range;

   procedure Test_Value_Changed is
      S : Gtk_Scale;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 100.0, 1.0);
      On_Value_Changed (S, Value_Changed_Cb'Unrestricted_Access);

      Value_Changed_Count := 0;
      S.Set_Value (10.0);
      Assert_Cmpint_Eq (Value_Changed_Count, 1);

      --  No change of value, no signal.
      S.Set_Value (10.0);
      Assert_Cmpint_Eq (Value_Changed_Count, 1);

      --  The adjustment is the source of the change, the range relays it.
      S.Get_Adjustment.Set_Value (20.0);
      Assert_Cmpint_Eq (Value_Changed_Count, 2);
   end Test_Value_Changed;

   procedure Test_Marks is
      S : Gtk_Scale;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 100.0, 1.0);

      S.Add_Mark (0.0, Pos_Bottom);
      S.Add_Mark (50.0, Pos_Bottom, "<b>half</b>");
      S.Add_Mark (100.0, Pos_Top, "full");
      S.Clear_Marks;
      S.Add_Mark (25.0, Pos_Top);
      S.Clear_Marks;
   end Test_Marks;

   procedure Test_Format_Value_Func is
      S : Gtk_Scale;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 100.0, 1.0);
      S.Set_Draw_Value (True);
      S.Set_Value (42.0);

      --  Without a function the value is rounded to the digits of the scale.
      Assert_Cmpstr_Eq (Shown_Text (S), "42");

      Format_Calls := 0;
      S.Set_Format_Value_Func (Format'Unrestricted_Access);
      Assert_Cmpstr_Eq (Shown_Text (S), "<42>");
      Assert_True (Format_Calls > 0);

      --  The value reaches the function, and the returned string is taken
      --  over by GTK, which frees it each time the text is replaced.
      for V in 0 .. 20 loop
         S.Set_Value (Gdouble (V));
         Assert_Cmpstr_Eq (Shown_Text (S), "<" & Image (Gdouble (V)) & ">");
      end loop;

      --  Clearing the function goes back to the default.
      S.Set_Format_Value_Func (null);
      Assert_Cmpstr_Eq (Shown_Text (S), "20");
   end Test_Format_Value_Func;

   procedure Test_Format_Drawn is
      Window : constant Gtk_Window := Gtk_Window_New;
      S      : Gtk_Scale;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 100.0, 1.0);
      S.Set_Draw_Value (True);
      S.Set_Value (63.0);
      S.Set_Format_Value_Func (Format'Unrestricted_Access);

      Window.Set_Child (S);
      Window.Present;
      Pump;
      Assert_Cmpstr_Eq (Shown_Text (S), "<63>");

      --  GTK4 draws the value with an internal label, which is given the
      --  text the function returns each time the value changes.
      Format_Calls := 0;
      S.Set_Value (80.0);
      Pump;
      Assert_True (Format_Calls > 0);
      Assert_Cmpstr_Eq (Shown_Text (S), "<80>");

      Window.Destroy;
   end Test_Format_Drawn;

   procedure Test_Format_User_Data is
      Window : constant Gtk_Window := Gtk_Window_New;
      S      : Gtk_Scale;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 100.0, 1.0);
      S.Set_Draw_Value (True);
      S.Set_Value (42.0);
      Window.Set_Child (S);

      Destroyed := 0;
      Last_Destroyed := Null_Unbounded_String;

      --  A null user-data formatter clears the callback without creating or
      --  destroying user data; it restores GTK's normal value text.
      Formatter.Set_Format_Value_Func (S, null, "unused");
      Assert_Cmpstr_Eq (Shown_Text (S), "42");
      Assert_Cmpint_Eq (Gint (Destroyed), 0);

      Formatter.Set_Format_Value_Func
        (S, Format_With_Data'Unrestricted_Access, "first");
      Assert_Cmpstr_Eq (Shown_Text (S), "first:42");
      Assert_Cmpint_Eq (Gint (Destroyed), 0);

      --  Replacing the function destroys the data of the old one, once.
      Formatter.Set_Format_Value_Func
        (S, Format_With_Data'Unrestricted_Access, "second");
      Assert_Cmpint_Eq (Gint (Destroyed), 1);
      Assert_Cmpstr_Eq (To_String (Last_Destroyed), "first");
      Assert_Cmpstr_Eq (Shown_Text (S), "second:42");

      --  So does clearing it.
      S.Set_Format_Value_Func (null);
      Assert_Cmpint_Eq (Gint (Destroyed), 2);
      Assert_Cmpstr_Eq (To_String (Last_Destroyed), "second");
      Assert_Cmpstr_Eq (Shown_Text (S), "42");

      --  And so does destroying the scale.
      Formatter.Set_Format_Value_Func
        (S, Format_With_Data'Unrestricted_Access, "third");
      Window.Present;
      Pump;
      Assert_Cmpint_Eq (Gint (Destroyed), 2);

      Window.Destroy;
      Pump;
      Assert_Cmpint_Eq (Gint (Destroyed), 3);
      Assert_Cmpstr_Eq (To_String (Last_Destroyed), "third");
   end Test_Format_User_Data;

begin
   Glib.Test.Init;

   --  The C tests use gtk_test_init, which also initializes GTK.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/scale/new_with_range", Test_New_With_Range'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/new_with_adjustment",
      Test_New_With_Adjustment'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/scale_accessors", Test_Scale_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/range_accessors", Test_Range_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/range_accessibility",
      Test_Range_Accessibility'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/scale_interface_wrappers",
      Test_Scale_Interface_Wrappers'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/range_layout", Test_Range_Layout'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/range_signals", Test_Range_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/set_range", Test_Set_Range'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/value_changed", Test_Value_Changed'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/marks", Test_Marks'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/format_value_func",
      Test_Format_Value_Func'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/format_drawn", Test_Format_Drawn'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/format_user_data", Test_Format_User_Data'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Scale;
