--  Port of testsuite/c_tests/calendar.c, with additional coverage for the
--  GtkAda calendar accessibility interface and its generated signal adapters.
with Ada.Command_Line;
with Ada.Exceptions;
with Glib;
with Glib.Date_Time;
with Glib.Object;
with Glib.Properties;
with Glib.Test;
with Glib.Time_Zone;
with Glib.Values;
with Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Calendar;
with Gtk.Constraint_Target;
with Gtk.Main;
with Gtkada.Bindings;
with Gtkada.Types;
with System;

procedure Calendar_Test is
   use Glib;
   use Glib.Date_Time;
   use Glib.Time_Zone;
   use Glib.Test;
   use Gtk.Calendar;

   use type Gtk.Calendar.Gtk_Calendar;
   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Accessible.Gtk_Accessible_Role;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gtk.Buildable.Gtk_Buildable;
   use type Gtk.Constraint_Target.Gtk_Constraint_Target;
   use type System.Address;

   Typed_Count : Natural := 0;
   Slot_Count  : Natural := 0;
   Typed_Object : System.Address := System.Null_Address;
   Slot_Object  : System.Address := System.Null_Address;
   Exception_Count : Natural := 0;
   Callback_Error : exception;

   --  Calendar's five signals have no parameters. This C variadic declaration
   --  emits them directly so the callback adapters are tested without input.
   procedure Emit_Void_Signal
     (Object : System.Address;
      Name   : Gtkada.Types.Chars_Ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Typed_Callback (Self : access Gtk_Calendar_Record'Class);
   procedure Slot_Callback
     (Self : access Glib.Object.GObject_Record'Class);
   procedure Raising_Typed_Callback
     (Self : access Gtk_Calendar_Record'Class);
   procedure Raising_Slot_Callback
     (Self : access Glib.Object.GObject_Record'Class);
   procedure Capture_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Test_Constructors_And_Date with Convention => C;
   procedure Test_Date_Property with Convention => C;
   procedure Test_Marks_And_Options with Convention => C;
   procedure Test_Accessibility with Convention => C;
   procedure Test_Signals with Convention => C;

   --  Port the C constructor and date tests, and cover Gtk_New plus the
   --  Initialize no-op branch for an already-created object.
   procedure Test_Constructors_And_Date is
      By_Function : constant Gtk_Calendar := Gtk_Calendar_New;
      By_Procedure : Gtk_Calendar;
      Selected : Gdate_Time;
      Year, Month, Day : Gint;
   begin
      By_Function.Ref_Sink;
      Gtk_New (By_Procedure);
      By_Procedure.Ref_Sink;
      --  A repeated Initialize must keep the existing GTK object alive.
      Gtk.Calendar.Initialize (By_Procedure);

      --  Use the tenth day before changing month or year to keep each date valid.
      By_Procedure.Set_Day (10);
      By_Procedure.Set_Year (2024);
      By_Procedure.Set_Month (1);
      Assert_Cmpint_Eq (By_Procedure.Get_Year, 2024);
      Assert_Cmpint_Eq (By_Procedure.Get_Month, 1);
      Assert_Cmpint_Eq (By_Procedure.Get_Day, 10);

      --  The returned GDateTime has transfer-full ownership and is unrefed.
      Selected := By_Procedure.Get_Date;
      Selected.Get_Ymd (Year, Month, Day);
      Assert_Cmpint_Eq (Year, 2024);
      Assert_Cmpint_Eq (Month, 2);
      Assert_Cmpint_Eq (Day, 10);
      Unref (Selected);

      Unref (By_Procedure);
      Unref (By_Function);
   end Test_Constructors_And_Date;

   --  Port the C GObject date-property round trip and method-based Set_Date.
   procedure Test_Date_Property is
      Cal : constant Gtk_Calendar := Gtk_Calendar_New;
      Input : Gdate_Time := Gdate_Time_New_From_Iso8601
        ("1970-01-01T00:00:00Z", Null_Gtime_Zone);
      Value : Glib.Values.GValue;
      Input_Method : Gdate_Time := Gdate_Time_New_From_Iso8601
        ("2110-11-03T20:20:05Z", Null_Gtime_Zone);
      Output : Gdate_Time;
      Year, Month, Day : Gint;
   begin
      Cal.Ref_Sink;
      Assert_True (Input /= Null_Gdate_Time);

      --  GDateTime is a boxed GValue. Setting and reading it through the
      --  generic GObject property API mirrors the original C test.
      Glib.Values.Init (Value, Glib.Date_Time.Get_Type);
      Glib.Values.Set_Boxed (Value, Input.Get_Object);
      Glib.Properties.Set_Property (Cal, "date", Value);
      --  Unsetting releases the GValue's reference to the boxed date.
      Glib.Values.Unset (Value);

      Glib.Values.Init (Value, Glib.Date_Time.Get_Type);
      Glib.Properties.Get_Property (Cal, "date", Value);
      Output := From_Object (Glib.Values.Get_Boxed (Value));
      Assert_True (Equal (Input, Output));
      --  Output is borrowed from Value here, so Unset owns its release.
      Glib.Values.Unset (Value);

      --  Set_Date selects a date's calendar day; the time of day is not part
      --  of GtkCalendar's selected-date state.
      Cal.Set_Date (Input_Method);
      Output := Cal.Get_Date;
      Output.Get_Ymd (Year, Month, Day);
      Assert_Cmpint_Eq (Year, 2110);
      Assert_Cmpint_Eq (Month, 11);
      Assert_Cmpint_Eq (Day, 3);
      Unref (Output);

      Unref (Input);
      Unref (Input_Method);
      Unref (Cal);
   end Test_Date_Property;

   --  Port the C day/month/year checks and exercise marks and each display
   --  option through both the methods and their read-back getters.
   procedure Test_Marks_And_Options is
      Cal : constant Gtk_Calendar := Gtk_Calendar_New;
   begin
      Cal.Ref_Sink;
      Cal.Set_Day (10);
      Cal.Set_Month (1);
      Assert_Cmpint_Eq (Cal.Get_Month, 1);
      --  Moving from February 10 to day 11 keeps the date valid.
      Cal.Set_Day (11);
      Assert_Cmpint_Eq (Cal.Get_Day, 11);
      Cal.Set_Year (2024);
      Assert_Cmpint_Eq (Cal.Get_Year, 2024);

      Cal.Mark_Day (11);
      Assert_True (Cal.Get_Day_Is_Marked (11));
      --  Check both removing one mark and clearing the remaining marks.
      Cal.Unmark_Day (11);
      Assert_False (Cal.Get_Day_Is_Marked (11));
      Cal.Mark_Day (12);
      Assert_True (Cal.Get_Day_Is_Marked (12));
      Cal.Clear_Marks;
      Assert_False (Cal.Get_Day_Is_Marked (12));

      Cal.Set_Show_Day_Names (False);
      Assert_False (Cal.Get_Show_Day_Names);
      Cal.Set_Show_Day_Names (True);
      Assert_True (Cal.Get_Show_Day_Names);
      Cal.Set_Show_Heading (False);
      Assert_False (Cal.Get_Show_Heading);
      Cal.Set_Show_Heading (True);
      Assert_True (Cal.Get_Show_Heading);
      Cal.Set_Show_Week_Numbers (True);
      Assert_True (Cal.Get_Show_Week_Numbers);
      Cal.Set_Show_Week_Numbers (False);
      Assert_False (Cal.Get_Show_Week_Numbers);
      Unref (Cal);
   end Test_Marks_And_Options;

   --  Exercise the inherited GtkAccessible wrappers on a fresh, unrooted
   --  calendar, where bounds and the accessibility context are unavailable.
   procedure Test_Accessibility is
      Cal : constant Gtk_Calendar := Gtk_Calendar_New;
      Parent : Gtk.Accessible.Gtk_Accessible;
      Child : Gtk.Accessible.Gtk_Accessible;
      Sibling : Gtk.Accessible.Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      As_Accessible : Gtk.Accessible.Gtk_Accessible;
      As_Buildable : Gtk.Buildable.Gtk_Buildable;
      As_Constraint_Target : Gtk.Constraint_Target.Gtk_Constraint_Target;
      X, Y, Width, Height : Gint;
   begin
      Cal.Ref_Sink;

      --  GtkCalendar uses the generic role in this GTK build; its days are
      --  exposed as accessible children beneath the widget.
      Assert_Cmpstr_Eq
        (Gtk.Accessible.Gtk_Accessible_Role'Image
           (Cal.Get_Accessible_Role),
         "ACCESSIBLE_ROLE_GENERIC");
      Parent := Cal.Get_Accessible_Parent;
      Assert_True (Parent = Gtk.Accessible.Null_Gtk_Accessible);
      --  GTK exposes the calendar's day cells as children even before mapping.
      Child := Cal.Get_First_Accessible_Child;
      Assert_True (Child /= Gtk.Accessible.Null_Gtk_Accessible);
      Sibling := Cal.Get_Next_Accessible_Sibling;
      Assert_True (Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_Cmpstr_Eq (Cal.Get_Accessible_Id, "");
      Context := Cal.Get_At_Context;
      Assert_True (Context = null);
      --  The four output coordinates are meaningful only when bounds exist.
      Assert_False (Cal.Get_Bounds (X, Y, Width, Height));

      --  A calendar is focusable by default, but does not start focused.
      Assert_True (Cal.Get_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focusable));
      Assert_False (Cal.Get_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focused));
      Cal.Announce ("calendar accessibility test",
        Gtk.Accessible.Accessible_Announcement_Priority_Low);
      Cal.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      Cal.Reset_Relation (Gtk.Accessible.Accessible_Relation_Described_By);
      Cal.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      --  Null parent/sibling values leave this standalone accessible unlinked.
      Cal.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Cal.Update_Next_Accessible_Sibling
        (Gtk.Accessible.Null_Gtk_Accessible);
      Cal.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focusable);

      --  Check the three interface conversions generated for GtkCalendar.
      As_Accessible := +Cal;
      Assert_True (-As_Accessible = Cal);
      As_Buildable := +Cal;
      Assert_True (-As_Buildable = Cal);
      As_Constraint_Target := +Cal;
      Assert_True (-As_Constraint_Target = Cal);

      Unref (Cal);
   end Test_Accessibility;

   --  Register both generated overloads for every calendar signal and emit
   --  each one, covering the typed and GObject-slot marshallers.
   procedure Test_Signals is
      Cal : constant Gtk_Calendar := Gtk_Calendar_New;
      Exceptional_Cal : constant Gtk_Calendar := Gtk_Calendar_New;
      Slot : constant Gtk_Calendar := Gtk_Calendar_New;
      Name : Gtkada.Types.Chars_Ptr;

      procedure Emit (Object : Gtk_Calendar; Signal : String) is
      begin
         Name := Gtkada.Types.New_String (Signal);
         Emit_Void_Signal (Object.Get_Object, Name);
         Gtkada.Types.Free (Name);
      end Emit;
   begin
      Cal.Ref_Sink;
      Exceptional_Cal.Ref_Sink;
      Slot.Ref_Sink;
      Typed_Count := 0;
      Slot_Count := 0;
      Typed_Object := System.Null_Address;
      Slot_Object := System.Null_Address;
      Exception_Count := 0;

      Cal.On_Day_Selected (Typed_Callback'Unrestricted_Access);
      Cal.On_Day_Selected (Slot_Callback'Unrestricted_Access, Slot);
      Cal.On_Next_Month (Typed_Callback'Unrestricted_Access);
      Cal.On_Next_Month (Slot_Callback'Unrestricted_Access, Slot);
      Cal.On_Next_Year (Typed_Callback'Unrestricted_Access);
      Cal.On_Next_Year (Slot_Callback'Unrestricted_Access, Slot);
      Cal.On_Prev_Month (Typed_Callback'Unrestricted_Access);
      Cal.On_Prev_Month (Slot_Callback'Unrestricted_Access, Slot);
      Cal.On_Prev_Year (Typed_Callback'Unrestricted_Access);
      Cal.On_Prev_Year (Slot_Callback'Unrestricted_Access, Slot);

      --  The names are copied into C strings and freed after each emission.
      Emit (Cal, "day-selected");
      Emit (Cal, "next-month");
      Emit (Cal, "next-year");
      Emit (Cal, "prev-month");
      Emit (Cal, "prev-year");

      Assert_Cmpuint_Eq (Guint (Typed_Count), 5);
      Assert_Cmpuint_Eq (Guint (Slot_Count), 5);
      Assert_True (Typed_Object = Cal.Get_Object);
      Assert_True (Slot_Object = Slot.Get_Object);

      --  Exceptions raised by Ada signal callbacks are caught at the C
      --  boundary and delivered to GtkAda's configured exception handler.
      Gtkada.Bindings.Set_On_Exception
        (Capture_Exception'Unrestricted_Access);
      --  Use a separate emitter to keep ordinary callback counts unchanged.
      Exceptional_Cal.On_Next_Year
        (Raising_Typed_Callback'Unrestricted_Access);
      Exceptional_Cal.On_Next_Year
        (Raising_Slot_Callback'Unrestricted_Access, Slot);
      Emit (Exceptional_Cal, "next-year");
      Assert_Cmpuint_Eq (Guint (Exception_Count), 2);

      Unref (Slot);
      Unref (Exceptional_Cal);
      Unref (Cal);
   end Test_Signals;

   --  Typed callbacks receive the signal emitter; slot callbacks receive the
   --  separately supplied GObject, as in GtkAda's ordinary signal API.
   procedure Typed_Callback (Self : access Gtk_Calendar_Record'Class) is
   begin
      Typed_Count := Typed_Count + 1;
      Typed_Object := Self.Get_Object;
   end Typed_Callback;

   procedure Slot_Callback
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Slot_Count := Slot_Count + 1;
      Slot_Object := Self.Get_Object;
   end Slot_Callback;

   --  These callbacks deliberately raise so both marshallers' exception
   --  handlers can be checked without letting an Ada exception cross C.
   procedure Raising_Typed_Callback
     (Self : access Gtk_Calendar_Record'Class) is
      pragma Unreferenced (Self);
   begin
      raise Callback_Error;
   end Raising_Typed_Callback;

   procedure Raising_Slot_Callback
     (Self : access Glib.Object.GObject_Record'Class) is
      pragma Unreferenced (Self);
   begin
      raise Callback_Error;
   end Raising_Slot_Callback;

   procedure Capture_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Exception_Count := Exception_Count + 1;
   end Capture_Exception;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/calendar/constructors-and-date",
      Test_Constructors_And_Date'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/calendar/date-property", Test_Date_Property'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/calendar/marks-and-options",
      Test_Marks_And_Options'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/calendar/accessibility", Test_Accessibility'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/calendar/signals", Test_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Calendar_Test;
