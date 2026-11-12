--  Gtk.Sorter tests, including string sorter cases ported from
--  testsuite/c_tests/sorter.c (/sorter/string and /sorter/change).
--  The focused checks also cover Gtk.Sorter's direct methods and both
--  generated callback adapters for its changed signal.

with Ada.Command_Line;
with Ada.Exceptions;

with Glib.Object;
with Glib.Test;
with Glib.Values;
with Gtk.Constant_Expression;
with Gtk.Enums;
with Gtk.Expression;
with Gtk.Main;
with Gtk.Numeric_Sorter;
with Gtk.Property_Expression;
with Gtk.Sorter;
with Gtk.String_Object;
with Gtk.String_Sorter;
with Gtkada.Bindings;
with System;

procedure Gtk_Sorter_Test is
   use Glib;
   use Glib.Test;
   use Gtk.Sorter;

   use type Glib.Object.GObject;
   use type Gtk.Enums.Gtk_Ordering;
   use type Gtk.Enums.Gtk_Sort_Type;
   use type Gtk.Expression.Gtk_Expression;
   use type Gtk.String_Sorter.Gtk_Collation;
   use type Gtk.String_Object.Gtk_String_Object;
   use type System.Address;

   Sorter : Gtk.String_Sorter.Gtk_String_Sorter;
   Low    : Gtk.String_Object.Gtk_String_Object;
   High   : Gtk.String_Object.Gtk_String_Object;

   Typed_Calls : Natural := 0;
   Slot_Calls  : Natural := 0;
   Exception_Callbacks : Natural := 0;
   Handled_Exceptions : Natural := 0;
   Typed_Object_Ok : Boolean := True;
   Slot_Object_Ok  : Boolean := True;
   Last_Change : Gtk_Sorter_Change := Different;

   procedure Test_Sorter with Convention => C;
   procedure Test_String_Sorter with Convention => C;
   procedure Test_Numeric_Sorter with Convention => C;
   procedure On_Changed
     (Self : access Gtk_Sorter_Record'Class;
      Change : Gtk_Sorter_Change);
   procedure On_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Change : Gtk_Sorter_Change);
   procedure Raise_On_Changed
     (Self : access Gtk_Sorter_Record'Class;
      Change : Gtk_Sorter_Change);
   procedure Raise_On_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Change : Gtk_Sorter_Change);
   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   --  Numeric sorters need an expression that returns a numeric GValue.
   --  Constant expressions keep this test independent of custom GObject types.
   function New_Integer_Expression
     (Value : Gint) return Gtk.Expression.Gtk_Expression;

   --  GtkExpression is boxed, so its Ada access value is not a stable identity.
   --  Compare the underlying GTK handles when checking the borrowed getter.
   function Expression_Handle
     (Value : Gtk.Expression.Gtk_Expression) return System.Address;

   --  The typed callback receives the sorter that emitted changed.
   procedure On_Changed
     (Self : access Gtk_Sorter_Record'Class;
      Change : Gtk_Sorter_Change) is
   begin
      Typed_Object_Ok := Typed_Object_Ok
        and then Glib.Object.Get_Object (Self) =
          Glib.Object.Get_Object (Sorter);
      Last_Change := Change;
      Typed_Calls := Typed_Calls + 1;
   end On_Changed;

   --  The slot callback receives the explicit slot object, not the sorter.
   procedure On_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Change : Gtk_Sorter_Change) is
   begin
      Slot_Object_Ok := Slot_Object_Ok
        and then Glib.Object.Get_Object (Self) =
          Glib.Object.Get_Object (Low);
      Last_Change := Change;
      Slot_Calls := Slot_Calls + 1;
   end On_Changed_Slot;

   --  Both generated marshallers must contain callback exceptions and report
   --  them through Gtkada's configured exception handler.
   procedure Raise_On_Changed
     (Self : access Gtk_Sorter_Record'Class;
      Change : Gtk_Sorter_Change) is
      pragma Unreferenced (Self, Change);
   begin
      Exception_Callbacks := Exception_Callbacks + 1;
      raise Program_Error with "expected typed sorter callback exception";
   end Raise_On_Changed;

   procedure Raise_On_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Change : Gtk_Sorter_Change) is
      pragma Unreferenced (Self, Change);
   begin
      Exception_Callbacks := Exception_Callbacks + 1;
      raise Program_Error with "expected sorter slot callback exception";
   end Raise_On_Changed_Slot;

   --  The marshaller catches callback exceptions before they can cross C.
   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Handled_Exceptions := Handled_Exceptions + 1;
   end Record_Exception;

   ---------------------------
   -- New_Integer_Expression --
   ---------------------------

   function New_Integer_Expression
     (Value : Gint) return Gtk.Expression.Gtk_Expression
   is
      Input : Glib.Values.GValue;
      Expr  : Gtk.Constant_Expression.Gtk_Constant_Expression;
   begin
      Glib.Values.Init_Set_Int (Input, Value);
      Expr := Gtk.Constant_Expression.Gtk_Constant_Expression_New_For_Value
        (Input);
      Glib.Values.Unset (Input);
      return Gtk.Expression.Gtk_Expression (Expr);
   end New_Integer_Expression;

   ----------------------
   -- Expression_Handle --
   ----------------------

   function Expression_Handle
     (Value : Gtk.Expression.Gtk_Expression) return System.Address is
   begin
      return Gtk.Expression.Get_Object (Value.all'Access);
   end Expression_Handle;

   --  This ports the C test's checks that changing an option emits once and
   --  setting it to its current value emits nothing further.
   procedure Test_Sorter is
      Order : Gtk_Sorter_Order;
      Calls_Before : Natural;
   begin
      Low := Gtk.String_Object.Gtk_String_Object_New ("alpha");
      High := Gtk.String_Object.Gtk_String_Object_New ("omega");
      Sorter := Gtk.String_Sorter.Gtk_String_Sorter_New (null);

      --  With no expression, the string sorter treats both values as invalid
      --  and therefore compares them as equal.
      Assert_True
        (Compare (Gtk_Sorter (Sorter), Low, High) = Gtk.Enums.Equal);
      Assert_True
        (Compare (Gtk_Sorter (Sorter), High, Low) = Gtk.Enums.Equal);

      --  The order query must return one of the declared GTK order values.
      Order := Get_Order (Gtk_Sorter (Sorter));
      Assert_True
        (Order = Partial or else Order = None or else Order = Total);

      --  The first callback uses the sorter type; the slot form checks that
      --  GtkAda forwards the separate GObject supplied by the caller.
      Gtk.Sorter.On_Changed
        (Gtk_Sorter (Sorter), On_Changed'Unrestricted_Access);
      Gtk.Sorter.On_Changed
        (Gtk_Sorter (Sorter), On_Changed_Slot'Unrestricted_Access,
         Slot => Low, After => True);

      Gtk.String_Sorter.Set_Ignore_Case (Sorter, False);
      Assert_False (Gtk.String_Sorter.Get_Ignore_Case (Sorter));
      Assert_Cmpuint_Eq (Guint (Typed_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Calls), 1);
      Calls_Before := Typed_Calls;

      --  As in /sorter/change, setting the same value is a no-op.
      Gtk.String_Sorter.Set_Ignore_Case (Sorter, False);
      Assert_Cmpuint_Eq (Guint (Typed_Calls), Guint (Calls_Before));
      Assert_Cmpuint_Eq (Guint (Slot_Calls), Guint (Calls_Before));

      --  Changed is public for GtkSorter implementers; exercise every enum
      --  value and verify both marshallers decode its GValue argument.
      for Change in Gtk_Sorter_Change loop
         Calls_Before := Typed_Calls;
         Gtk.Sorter.Changed (Gtk_Sorter (Sorter), Change);
         Assert_Cmpuint_Eq (Guint (Typed_Calls), Guint (Calls_Before + 1));
         Assert_Cmpuint_Eq (Guint (Slot_Calls), Guint (Calls_Before + 1));
         Assert_True (Last_Change = Change);
      end loop;

      Assert_True (Typed_Object_Ok);
      Assert_True (Slot_Object_Ok);

      --  Trigger each marshaller's exception branch after installing a
      --  handler; neither exception may unwind through GTK's signal dispatch.
      Gtk.Sorter.On_Changed
        (Gtk_Sorter (Sorter), Raise_On_Changed'Unrestricted_Access);
      Gtk.Sorter.On_Changed
        (Gtk_Sorter (Sorter), Raise_On_Changed_Slot'Unrestricted_Access,
         Slot => Low);
      Gtkada.Bindings.Set_On_Exception
        (Record_Exception'Unrestricted_Access);
      Gtk.Sorter.Changed (Gtk_Sorter (Sorter), Different);
      Assert_Cmpuint_Eq (Guint (Exception_Callbacks), 2);
      Assert_Cmpuint_Eq (Guint (Handled_Exceptions), 2);

      Glib.Object.Unref (Glib.Object.GObject (Sorter));
      Glib.Object.Unref (Glib.Object.GObject (High));
      Glib.Object.Unref (Glib.Object.GObject (Low));
   end Test_Sorter;

   ------------------------
   -- Test_String_Sorter --
   ------------------------

   procedure Test_String_Sorter is
      Expression : constant Gtk.Property_Expression.Gtk_Property_Expression :=
        Gtk.Property_Expression.Gtk_Property_Expression_New
          (Gtk.String_Object.Get_Type, null, "string");
      Replacement : constant Gtk.Property_Expression.Gtk_Property_Expression :=
        Gtk.Property_Expression.Gtk_Property_Expression_New
          (Gtk.String_Object.Get_Type, null, "string");
      Sorter : Gtk.String_Sorter.Gtk_String_Sorter;
      Alpha  : constant Gtk.String_Object.Gtk_String_Object :=
        Gtk.String_Object.Gtk_String_Object_New ("alpha");
      Omega  : constant Gtk.String_Object.Gtk_String_Object :=
        Gtk.String_Object.Gtk_String_Object_New ("omega");
   begin
      --  Port /sorter/string's expression getter check and exercise the
      --  procedure constructor, whose expression argument is transferred.
      Gtk.String_Sorter.Gtk_New
        (Sorter, Gtk.Expression.Gtk_Expression (Expression));
      Assert_True
        (Expression_Handle (Gtk.String_Sorter.Get_Expression (Sorter)) =
         Expression_Handle (Gtk.Expression.Gtk_Expression (Expression)));

      --  A property expression reads GtkStringObject.string, so comparing
      --  these objects verifies that the configured expression is evaluated.
      Assert_True
        (Gtk.Sorter.Compare
           (Gtk.Sorter.Gtk_Sorter (Sorter), Alpha, Omega) = Gtk.Enums.Smaller);

      --  The default is Unicode collation; the C /sorter/change test selects
      --  filename collation and checks the getter reports that selection.
      Assert_True
        (Gtk.String_Sorter.Get_Collation (Sorter) =
         Gtk.String_Sorter.Collation_Unicode);
      Gtk.String_Sorter.Set_Collation
        (Sorter, Gtk.String_Sorter.Collation_Filename);
      Assert_True
        (Gtk.String_Sorter.Get_Collation (Sorter) =
         Gtk.String_Sorter.Collation_Filename);

      --  Initialize is a no-op after construction, while Set_Expression
      --  replaces the expression and accepts null to clear it.
      Gtk.String_Sorter.Initialize
        (Sorter, Gtk.Expression.Gtk_Expression (Replacement));
      Assert_True
        (Expression_Handle (Gtk.String_Sorter.Get_Expression (Sorter)) =
         Expression_Handle (Gtk.Expression.Gtk_Expression (Expression)));
      Gtk.String_Sorter.Set_Expression
        (Sorter, Gtk.Expression.Gtk_Expression (Replacement));
      Assert_True
        (Expression_Handle (Gtk.String_Sorter.Get_Expression (Sorter)) =
         Expression_Handle (Gtk.Expression.Gtk_Expression (Replacement)));
      Gtk.String_Sorter.Set_Expression (Sorter, null);
      Assert_True (Gtk.String_Sorter.Get_Expression (Sorter) = null);

      Glib.Object.Unref (Glib.Object.GObject (Sorter));
      Glib.Object.Unref (Glib.Object.GObject (Omega));
      Glib.Object.Unref (Glib.Object.GObject (Alpha));
   end Test_String_Sorter;

   -------------------------
   -- Test_Numeric_Sorter --
   -------------------------

   procedure Test_Numeric_Sorter is
      Initial_Expression : constant Gtk.Expression.Gtk_Expression :=
        New_Integer_Expression (17);
      Replacement : constant Gtk.Expression.Gtk_Expression :=
        New_Integer_Expression (29);
      Function_Sorter : constant Gtk.Numeric_Sorter.Gtk_Numeric_Sorter :=
        Gtk.Numeric_Sorter.Gtk_Numeric_Sorter_New (Initial_Expression);
      Procedure_Sorter : Gtk.Numeric_Sorter.Gtk_Numeric_Sorter;
   begin
      --  The C /sorter/numeric_boolean case checks the default order and that
      --  the expression getter returns the same expression passed to GTK.
      Assert_True
        (Gtk.Numeric_Sorter.Get_Sort_Order (Function_Sorter) =
         Gtk.Enums.Sort_Ascending);
      Assert_True
        (Expression_Handle
           (Gtk.Numeric_Sorter.Get_Expression (Function_Sorter)) =
         Expression_Handle (Initial_Expression));

      --  Gtk_New covers the procedure constructor with a null expression;
      --  setting and getting the expression then exercises both API methods.
      Gtk.Numeric_Sorter.Gtk_New (Procedure_Sorter, null);
      Assert_True
        (Gtk.Numeric_Sorter.Get_Expression (Procedure_Sorter) = null);
      Gtk.Numeric_Sorter.Set_Expression (Procedure_Sorter, Replacement);
      Assert_True
        (Expression_Handle
           (Gtk.Numeric_Sorter.Get_Expression (Procedure_Sorter)) =
         Expression_Handle (Replacement));

      --  Initialize is intentionally a no-op on an already-created sorter.
      Gtk.Numeric_Sorter.Initialize (Procedure_Sorter, Initial_Expression);
      Assert_True
        (Expression_Handle
           (Gtk.Numeric_Sorter.Get_Expression (Procedure_Sorter)) =
         Expression_Handle (Replacement));

      --  The method and generated enum property both control the same order.
      Gtk.Numeric_Sorter.Set_Sort_Order
        (Function_Sorter, Gtk.Enums.Sort_Descending);
      Assert_True
        (Gtk.Numeric_Sorter.Get_Sort_Order (Function_Sorter) =
         Gtk.Enums.Sort_Descending);
      Gtk.Enums.Set_Property
        (Function_Sorter, Gtk.Numeric_Sorter.Sort_Order_Property,
         Gtk.Enums.Sort_Ascending);
      Assert_True
        (Gtk.Enums.Get_Property
           (Function_Sorter, Gtk.Numeric_Sorter.Sort_Order_Property) =
         Gtk.Enums.Sort_Ascending);

      --  Clearing an expression is supported and makes Get_Expression null.
      Gtk.Numeric_Sorter.Set_Expression (Function_Sorter, null);
      Assert_True
        (Gtk.Numeric_Sorter.Get_Expression (Function_Sorter) = null);

      Glib.Object.Unref (Glib.Object.GObject (Procedure_Sorter));
      Glib.Object.Unref (Glib.Object.GObject (Function_Sorter));
   end Test_Numeric_Sorter;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/sorter/string", Test_String_Sorter'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/sorter/change-and-methods", Test_Sorter'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/sorter/numeric", Test_Numeric_Sorter'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Gtk_Sorter_Test;
