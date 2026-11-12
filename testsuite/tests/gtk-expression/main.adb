--  Port of test_property, test_interface_property, test_constant, test_object,
--  test_try, test_bind, test_cclosure, and test_closure from testsuite/c_tests/expression.c, with
--  extra checks for GtkAda's watch callbacks and expression constructors.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib;
with Glib.Object;
with Glib.Properties.Creation;
with Glib.Test;
with Glib.Types;
with Glib.Values;
with Gtk.Cclosure_Expression;
with Gtk.Closure_Expression;
with Gtk.Constant_Expression;
with Gtk.Expression;
with Gtk.Expression_Watch;
with Gtk.Label;
with Gtk.Main;
with Gtk.Orientable;
with Gtk.Object_Expression;
with Gtk.Property_Expression;
with Gtk.String_Filter;
with Gtk.Try_Expression;
with Gtk.Window;
with Gtkada.Types;

procedure Main is

   use type Glib.GType;
   use type Glib.Gboolean;
   use type Glib.Gint;
   use type Glib.Param_Spec;
   use type Glib.Guint;
   use type Glib.Object.GObject;
   use type Gtk.Expression.Gtk_Expression;
   use type Gtk.Object_Expression.Gtk_Object_Expression;
   use type Gtk.Property_Expression.Gtk_Property_Expression;
   use type Gtk.String_Filter.Gtk_String_Filter;
   use type System.Address;

   Direct_Notifications : Glib.Guint := 0;
   Data_Notifications   : Glib.Guint := 0;
   Destroyed_Data       : Glib.Guint := 0;
   Last_Data            : Integer := 0;

   --  The C-closure test records each native argument so the Ada test can
   --  assert that Gtk passed the context object and expression values intact.
   Expected_Callback_Object : System.Address := System.Null_Address;
   Callback_Got_Object      : Boolean := False;
   Callback_Got_Search      : Boolean := False;
   Callback_Got_Ignore_Case : Boolean := False;
   Callback_Got_Match_Mode  : Boolean := False;
   Callback_Got_Closure_Parameter : Boolean := False;

   --  Gtk's generic GClosure marshaller calls this C-convention function with
   --  `this` followed by the three values supplied by the expressions.
   function Print_Filter_Info
     (Filter      : System.Address;
      Search      : Gtkada.Types.Chars_Ptr;
      Ignore_Case : Glib.Gboolean;
      Match_Mode  : Glib.Gint) return Gtkada.Types.Chars_Ptr
   with Convention => C;

   --  Gtk's API accepts an untyped GCallback procedure pointer. Convert the
   --  correctly typed C callback without changing its native calling convention.
   type Print_Filter_Info_Access is access function
     (Filter      : System.Address;
      Search      : Gtkada.Types.Chars_Ptr;
      Ignore_Case : Glib.Gboolean;
      Match_Mode  : Glib.Gint) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Print_Filter_Info_Access);

   function To_Gcallback is new Ada.Unchecked_Conversion
     (Print_Filter_Info_Access, Gtk.Cclosure_Expression.Gcallback);

   --  This callback receives the value of the parameter expression and
   --  returns a new string, as the C closure test does with g_strdup.
   function Echo_Closure_Parameter
     (This     : System.Address;
      Argument : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr
   with Convention => C;

   type Echo_Closure_Parameter_Access is access function
     (This     : System.Address;
      Argument : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Echo_Closure_Parameter_Access);

   function To_Closure_Gcallback is new Ada.Unchecked_Conversion
     (Echo_Closure_Parameter_Access, Gtk.Cclosure_Expression.Gcallback);

   --  GtkClosureExpression takes ownership of the GClosure pointer it receives.
   function G_Cclosure_New
     (Callback   : Gtk.Cclosure_Expression.Gcallback;
      User_Data  : System.Address;
      User_Destroy : Gtk.Cclosure_Expression.GClosureNotify)
      return System.Address;
   pragma Import (C, G_Cclosure_New, "g_cclosure_new");

   -----------------------
   -- Print_Filter_Info --
   -----------------------

   function Print_Filter_Info
     (Filter      : System.Address;
      Search      : Gtkada.Types.Chars_Ptr;
      Ignore_Case : Glib.Gboolean;
      Match_Mode  : Glib.Gint) return Gtkada.Types.Chars_Ptr
   is
   begin
      --  Return the configured result while preserving the callback arguments
      --  for explicit checks after expression evaluation.
      Callback_Got_Object := Filter = Expected_Callback_Object;
      Callback_Got_Search := Gtkada.Types.Value (Search) = "needle";
      Callback_Got_Ignore_Case := Ignore_Case = 0;
      Callback_Got_Match_Mode := Match_Mode = Glib.Gint
        (Gtk.String_Filter.Gtk_String_Filter_Match_Mode'Pos
           (Gtk.String_Filter.String_Filter_Match_Mode_Exact));
      --  The GValue marshaller owns the returned string, matching C's
      --  g_strdup result in test_cclosure.
      return Gtkada.Types.New_String ("OK");
   end Print_Filter_Info;

   ------------------------------
   -- Echo_Closure_Parameter --
   ------------------------------

   function Echo_Closure_Parameter
     (This     : System.Address;
      Argument : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr
   is
      pragma Unreferenced (This);
   begin
      --  Gtk passes `this` first, even when it is null, before expression values.
      Callback_Got_Closure_Parameter :=
        Gtkada.Types.Value (Argument) = "closure argument";
      return Gtkada.Types.New_String ("closure result");
   end Echo_Closure_Parameter;

   --  The plain watch test uses a no-argument callback, matching the C API.
   procedure On_Changed is
   begin
      Direct_Notifications := Direct_Notifications + Glib.Guint'(1);
   end On_Changed;

   --  A separate callback records the payload passed through Watch_User_Data.
   procedure On_Changed_With_Data (Data : Integer) is
   begin
      Data_Notifications := Data_Notifications + Glib.Guint'(1);
      Last_Data := Data;
   end On_Changed_With_Data;

   --  Destroy is checked when GTK releases the watch's callback data.
   procedure Destroy_Data (Data : in out Integer) is
   begin
      Destroyed_Data := Destroyed_Data + Glib.Guint'(1);
      Data := 0;
   end Destroy_Data;

   package Watch_With_Data is new Gtk.Expression.Watch_User_Data
     (User_Data_Type => Integer,
      Destroy        => Destroy_Data);

   procedure Test_Constant with Convention => C;
   procedure Test_Property_And_Watch with Convention => C;
   procedure Test_Property_Constructors with Convention => C;
   procedure Test_Object_And_Try with Convention => C;
   procedure Test_Try_Bind with Convention => C;
   procedure Test_Bind with Convention => C;
   procedure Test_Watch_User_Data with Convention => C;
   procedure Test_Dummy_Wrapper with Convention => C;
   procedure Test_Cclosure with Convention => C;
   procedure Test_Closure with Convention => C;

   -------------------
   -- Test_Constant --
   -------------------

   procedure Test_Constant is
      Input  : Glib.Values.GValue;
      Result : Glib.Values.GValue;
      Expr   : Gtk.Constant_Expression.Gtk_Constant_Expression;
      Procedure_Expr : Gtk.Constant_Expression.Gtk_Constant_Expression;
      Base   : Gtk.Expression.Gtk_Expression;
   begin
      --  The constant expression copies the supplied value and is always
      --  evaluable without a `this` object.
      Glib.Values.Init_Set_Int (Input, 22);
      Expr := Gtk.Constant_Expression.Gtk_Constant_Expression_New_For_Value
        (Input);
      Base := Gtk.Expression.Gtk_Expression (Expr);
      Glib.Test.Assert_True (Gtk.Expression.Is_Static (Base));
      Glib.Test.Assert_True
        (Gtk.Expression.Get_Value_Type (Base) = Glib.GType_Int);
      Glib.Test.Assert_True (Gtk.Expression.Evaluate (Base, System.Null_Address, Result));
      Glib.Test.Assert_Cmpint_Eq (Glib.Values.Get_Int (Result), 22);
      Glib.Values.Unset (Result);

      --  The typed getter returns the same value as generic evaluation.
      Glib.Test.Assert_Cmpint_Eq
        (Glib.Values.Get_Int
           (Gtk.Constant_Expression.Get_Value (Expr).all),
         22);

      --  The out-parameter constructor creates the same constant value.
      Gtk.Constant_Expression.Gtk_New_For_Value (Procedure_Expr, Input);
      Glib.Test.Assert_Cmpint_Eq
        (Glib.Values.Get_Int
           (Gtk.Constant_Expression.Get_Value (Procedure_Expr).all),
         22);
      Glib.Values.Unset (Input);
   end Test_Constant;

   --------------------------------
   -- Test_Property_And_Watch --
   --------------------------------

   procedure Test_Property_And_Watch is
      --  From_Object_Free consumes a heap-allocated Ada wrapper while
      --  preserving the GtkExpressionWatch reference stored in it.
      type Watch_Wrapper_Access is access all
        Gtk.Expression_Watch.Gtk_Expression_Watch;
      Filter  : constant Gtk.String_Filter.Gtk_String_Filter :=
        Gtk.String_Filter.Gtk_String_Filter_New (null);
      Expr    : constant Gtk.Property_Expression.Gtk_Property_Expression :=
        Gtk.Property_Expression.Gtk_Property_Expression_New
          (Gtk.String_Filter.Get_Type, null, "search");
      Base    : constant Gtk.Expression.Gtk_Expression :=
        Gtk.Expression.Gtk_Expression (Expr);
      Watch   : Gtk.Expression_Watch.Gtk_Expression_Watch;
      Owned_Watch : Gtk.Expression_Watch.Gtk_Expression_Watch;
      Watch_Wrapper : Watch_Wrapper_Access;
      Value   : Glib.Values.GValue;
      No_Parent : Gtk.Expression.Gtk_Expression;
   begin
      --  The property expression reads the filter's search string from `this`.
      Glib.Test.Assert_False (Gtk.Expression.Is_Static (Base));
      Glib.Test.Assert_True
        (Gtk.Expression.Get_Value_Type (Base) = Glib.GType_String);
      Glib.Test.Assert_True
        (Gtk.Expression.Evaluate
           (Base, Glib.Object.Get_Object (Filter), Value));
      Glib.Test.Assert_Cmpstr_Eq (Glib.Values.Get_String (Value), "");
      Glib.Values.Unset (Value);

      --  A NULL `this` cannot supply the property, so evaluation reports
      --  failure; the output GValue remains unset.
      Glib.Test.Assert_False
        (Gtk.Expression.Evaluate (Base, System.Null_Address, Value));
      Glib.Test.Assert_True (Glib.Values.Type_Of (Value) = Glib.GType_Invalid);

      --  Get_Expression returns NULL for a direct property lookup. This also
      --  covers the null-object path in Gtk.Expression's ownership converter.
      No_Parent := Gtk.Property_Expression.Get_Expression (Expr);
      Glib.Test.Assert_True (No_Parent = null);

      --  GTK notifies after a watched property changes, then stops after the
      --  watch is unwatched.
      Direct_Notifications := 0;
      Watch := Gtk.Expression.Watch
        (Base, Glib.Object.Get_Object (Filter), On_Changed'Unrestricted_Access);
      --  Watch is borrowed from the expression. Ref gives the wrapper its own
      --  reference; From_Object_Free also exercises the generated ownership
      --  conversion by consuming the temporary wrapper allocation.
      Watch_Wrapper := new Gtk.Expression_Watch.Gtk_Expression_Watch'
        (Gtk.Expression_Watch.Ref (Watch));
      Owned_Watch := Gtk.Expression_Watch.From_Object_Free (Watch_Wrapper);
      Gtk.String_Filter.Set_Search (Filter, "Hello World");
      Glib.Test.Assert_Cmpuint_Eq (Direct_Notifications, 1);
      Glib.Test.Assert_True
        (Gtk.Expression_Watch.Evaluate (Owned_Watch, Value));
      Glib.Test.Assert_Cmpstr_Eq (Glib.Values.Get_String (Value), "Hello World");
      Glib.Values.Unset (Value);
      Gtk.Expression_Watch.Unwatch (Watch);
      Gtk.String_Filter.Set_Search (Filter, "after unwatch");
      Glib.Test.Assert_Cmpuint_Eq (Direct_Notifications, 1);
      --  Release the reference acquired above after stopping notifications.
      Gtk.Expression_Watch.Unref (Owned_Watch);

   end Test_Property_And_Watch;

   ---------------------------------
   -- Test_Property_Constructors --
   ---------------------------------

   procedure Test_Property_Constructors is
      Filter : constant Gtk.String_Filter.Gtk_String_Filter :=
        Gtk.String_Filter.Gtk_String_Filter_New (null);
      Class : constant Glib.Object.GObject_Class :=
        Glib.Types.Class_Ref (Gtk.String_Filter.Get_Type);
      Properties : constant Glib.Param_Spec_Array :=
        Glib.Object.Class_List_Properties (Class);
      Search_Pspec : Glib.Param_Spec := null;
      Parent : constant Gtk.Object_Expression.Gtk_Object_Expression :=
        Gtk.Object_Expression.Gtk_Object_Expression_New
          (Glib.Object.GObject (Filter));
      Parent_Base : constant Gtk.Expression.Gtk_Expression :=
        Gtk.Expression.Gtk_Expression (Parent);
      By_Name : Gtk.Property_Expression.Gtk_Property_Expression;
      By_Pspec : Gtk.Property_Expression.Gtk_Property_Expression;
      By_Pspec_Function : Gtk.Property_Expression.Gtk_Property_Expression;
      Interface_Lookup : Gtk.Property_Expression.Gtk_Property_Expression;

      --  Returned expressions are new Ada wrappers, so check the evaluated
      --  object instead of comparing the wrapper access values themselves.
      procedure Assert_Parent (Property : Gtk.Property_Expression.Gtk_Property_Expression) is
         Returned : constant Gtk.Expression.Gtk_Expression :=
           Gtk.Property_Expression.Get_Expression (Property);
         Value : Glib.Values.GValue;
      begin
         Glib.Test.Assert_True (Returned /= null);
         Glib.Test.Assert_True
           (Gtk.Expression.Evaluate (Returned, System.Null_Address, Value));
         Glib.Test.Assert_True
           (Glib.Values.Get_Object (Value) = Glib.Object.GObject (Filter));
         Glib.Values.Unset (Value);
      end Assert_Parent;
   begin
      --  Find the real search property spec needed by the _for_pspec API.
      for Index in Properties'Range loop
         if Glib.Properties.Creation.Pspec_Name (Properties (Index)) = "search" then
            Search_Pspec := Properties (Index);
         end if;
      end loop;
      Glib.Test.Assert_True (Search_Pspec /= null);

      --  The procedure constructor retains a reference to its parent expression.
      Gtk.Property_Expression.Gtk_New
        (By_Name, Gtk.String_Filter.Get_Type, Parent_Base, "search");
      Assert_Parent (By_Name);
      Glib.Test.Assert_True
        (Glib.Properties.Creation.Pspec_Name
           (Gtk.Property_Expression.Get_Pspec (By_Name)) = "search");

      --  Both pspec constructors accept the same parent and expose its spec.
      Gtk.Property_Expression.Gtk_New_For_Pspec
        (By_Pspec, Parent_Base, Search_Pspec);
      By_Pspec_Function :=
        Gtk.Property_Expression.Gtk_Property_Expression_New_For_Pspec
          (Parent_Base, Search_Pspec);
      Assert_Parent (By_Pspec);
      Glib.Test.Assert_True
        (Glib.Properties.Creation.Pspec_Name
           (Gtk.Property_Expression.Get_Pspec (By_Pspec)) = "search");
      Assert_Parent (By_Pspec_Function);
      Glib.Test.Assert_True
        (Glib.Properties.Creation.Pspec_Name
           (Gtk.Property_Expression.Get_Pspec (By_Pspec_Function)) = "search");

      --  Port C's interface-property case: GtkOrientable exposes orientation.
      Interface_Lookup := Gtk.Property_Expression.Gtk_Property_Expression_New
        (Gtk.Orientable.Get_Type, null, "orientation");
      Glib.Test.Assert_True
        (Glib.Properties.Creation.Pspec_Name
           (Gtk.Property_Expression.Get_Pspec (Interface_Lookup)) = "orientation");
      Glib.Types.Class_Unref (Gtk.String_Filter.Get_Type);
   end Test_Property_Constructors;

   -----------------------------
   -- Test_Object_And_Try --
   -----------------------------

   procedure Test_Object_And_Try is
      Filter : Gtk.String_Filter.Gtk_String_Filter :=
        Gtk.String_Filter.Gtk_String_Filter_New (null);
      Object_Expr : Gtk.Object_Expression.Gtk_Object_Expression :=
        Gtk.Object_Expression.Gtk_Object_Expression_New (Filter);
      Procedure_Expr : Gtk.Object_Expression.Gtk_Object_Expression;
      Value  : Glib.Values.GValue;
      Fallback : Glib.Values.GValue;
      Constant_Expr : Gtk.Constant_Expression.Gtk_Constant_Expression;
      Lookup : Gtk.Property_Expression.Gtk_Property_Expression;
      Try_Expr : Gtk.Try_Expression.Gtk_Try_Expression;
      Parts : Gtk.Expression.Gtk_Expression_Array (0 .. 1);
      Parent : Gtk.Expression.Gtk_Expression;
      Object_Filter : Gtk.String_Filter.Gtk_String_Filter;
      Factory_Object_Expr : Gtk.Object_Expression.Gtk_Object_Expression;
      Object_Try : Gtk.Try_Expression.Gtk_Try_Expression;
      Object_Parts : Gtk.Expression.Gtk_Expression_Array (0 .. 0);
      Nested_Lookup : Gtk.Property_Expression.Gtk_Property_Expression;
      Wrapped_Parent : Gtk.Expression.Gtk_Expression;
   begin
      --  Object expressions hold weak references: they evaluate while the
      --  filter exists and fail after its last strong reference is released.
      --  Exercise the procedure constructor as well as the function form;
      --  both wrappers must expose the same watched GObject.
      Gtk.Object_Expression.Gtk_New (Procedure_Expr, Filter);
      Glib.Test.Assert_True
        (Gtk.Object_Expression.Get_Object (Procedure_Expr) =
         Glib.Object.GObject (Filter));
      Glib.Test.Assert_True
        (Gtk.Object_Expression.Get_Object (Object_Expr) =
         Glib.Object.GObject (Filter));
      Glib.Test.Assert_True
        (Gtk.Expression.Evaluate
           (Gtk.Expression.Gtk_Expression (Object_Expr),
            System.Null_Address, Value));
      Glib.Test.Assert_True
        (Glib.Values.Get_Object (Value) = Glib.Object.GObject (Filter));
      Glib.Values.Unset (Value);
      Glib.Object.Unref (Glib.Object.GObject (Filter));
      Filter := null;
      Glib.Test.Assert_False
        (Gtk.Expression.Evaluate
           (Gtk.Expression.Gtk_Expression (Object_Expr),
            System.Null_Address, Value));

      --  A property expression using a missing `this` fails. GtkTryExpression
      --  then evaluates the constant fallback, exercising the Ada-to-C array
      --  conversion for both expressions.
      Glib.Values.Init_Set_String (Fallback, "fallback");
      Constant_Expr := Gtk.Constant_Expression.Gtk_Constant_Expression_New_For_Value
        (Fallback);
      Lookup := Gtk.Property_Expression.Gtk_Property_Expression_New
        (Gtk.String_Filter.Get_Type, null, "search");
      Parts (0) := Gtk.Expression.Gtk_Expression (Lookup);
      Parts (1) := Gtk.Expression.Gtk_Expression (Constant_Expr);
      Gtk.Try_Expression.Gtk_New (Try_Expr, Parts);
      Parent := Gtk.Property_Expression.Get_Expression (Lookup);
      Glib.Test.Assert_True (Parent = null);
      Glib.Test.Assert_True
        (Gtk.Expression.Evaluate
           (Gtk.Expression.Gtk_Expression (Try_Expr),
            System.Null_Address, Value));
      Glib.Test.Assert_Cmpstr_Eq (Glib.Values.Get_String (Value), "fallback");
      Glib.Values.Unset (Value);

      --  A nested property lookup returns its parent through Gtk.Expression's
      --  runtime type factory. This makes GTK recreate the try-expression as
      --  its specific Ada wrapper instead of the fallback expression wrapper.
      Object_Filter := Gtk.String_Filter.Gtk_String_Filter_New (null);
      Factory_Object_Expr := Gtk.Object_Expression.Gtk_Object_Expression_New
        (Glib.Object.GObject (Object_Filter));
      Object_Parts (0) := Gtk.Expression.Gtk_Expression (Factory_Object_Expr);
      Gtk.Try_Expression.Gtk_New (Object_Try, Object_Parts);
      Nested_Lookup := Gtk.Property_Expression.Gtk_Property_Expression_New
        (Gtk.String_Filter.Get_Type,
         Gtk.Expression.Gtk_Expression (Object_Try), "search");

      --  The nested expression still evaluates to the filter's property, and
      --  the returned parent keeps both the native pointer and Ada subtype.
      Glib.Test.Assert_True
        (Gtk.Expression.Evaluate
           (Gtk.Expression.Gtk_Expression (Nested_Lookup),
            System.Null_Address, Value));
      Glib.Test.Assert_Cmpstr_Eq (Glib.Values.Get_String (Value), "");
      Glib.Values.Unset (Value);
      Wrapped_Parent := Gtk.Property_Expression.Get_Expression (Nested_Lookup);
      Glib.Test.Assert_True (Wrapped_Parent /= null);
      Glib.Test.Assert_True
        (Wrapped_Parent.all in Gtk.Try_Expression.Gtk_Try_Expression_Record'Class);
      Glib.Test.Assert_True
        (Gtk.Expression.Get_Object (Wrapped_Parent.all'Access) =
         Gtk.Expression.Get_Object (Object_Try.all'Access));
      Glib.Values.Unset (Fallback);
   end Test_Object_And_Try;

   --------------------
   -- Test_Try_Bind --
   --------------------

   procedure Test_Try_Bind is
      --  C test_try follows GtkLabel.root to a window title, then uses a
      --  constant when the label is not rooted in a window.
      Root_Expr : constant Gtk.Property_Expression.Gtk_Property_Expression :=
        Gtk.Property_Expression.Gtk_Property_Expression_New
          (Gtk.Label.Get_Type, null, "root");
      Title_Expr : constant Gtk.Property_Expression.Gtk_Property_Expression :=
        Gtk.Property_Expression.Gtk_Property_Expression_New
          (Gtk.Window.Get_Type,
           Gtk.Expression.Gtk_Expression (Root_Expr), "title");
      Fallback : Glib.Values.GValue;
      Constant_Expr : Gtk.Constant_Expression.Gtk_Constant_Expression;
      Parts : Gtk.Expression.Gtk_Expression_Array (0 .. 1);
      Try_Expr : Gtk.Try_Expression.Gtk_Try_Expression;
      Label : constant Gtk.Label.Gtk_Label := Gtk.Label.Gtk_Label_New ("");
      Window : Gtk.Window.Gtk_Window;
      Watch : Gtk.Expression_Watch.Gtk_Expression_Watch;
      pragma Unreferenced (Watch);
   begin
      --  GtkTryExpression tries the nested window lookup first and then the
      --  constant expression. Bind reevaluates it whenever the label changes root.
      Glib.Values.Init_Set_String (Fallback, "fallback");
      Constant_Expr := Gtk.Constant_Expression.Gtk_Constant_Expression_New_For_Value
        (Fallback);
      Parts (0) := Gtk.Expression.Gtk_Expression (Title_Expr);
      Parts (1) := Gtk.Expression.Gtk_Expression (Constant_Expr);
      Gtk.Try_Expression.Gtk_New (Try_Expr, Parts);
      --  Keep an independent reference so detaching the label from its window
      --  does not destroy it while GtkExpressionBind is still watching it.
      Label.Ref_Sink;
      Watch := Gtk.Expression.Bind
        (Gtk.Expression.Gtk_Expression (Try_Expr),
         Glib.Object.Get_Object (Label), "label",
         Glib.Object.Get_Object (Label));

      --  Without a root window the title lookup fails, so the label uses fallback.
      Glib.Test.Assert_Cmpstr_Eq (Gtk.Label.Get_Label (Label), "fallback");
      Window := Gtk.Window.Gtk_Window_New;
      Gtk.Window.Set_Title (Window, "window");
      Window.Set_Child (Label);
      Glib.Test.Assert_Cmpstr_Eq (Gtk.Label.Get_Label (Label), "window");

      --  Updating the title refreshes the binding; detaching the label restores fallback.
      Gtk.Window.Set_Title (Window, "still window");
      Glib.Test.Assert_Cmpstr_Eq (Gtk.Label.Get_Label (Label), "still window");
      Window.Set_Child (null);
      Glib.Test.Assert_Cmpstr_Eq (Gtk.Label.Get_Label (Label), "fallback");
      --  Bind's returned watch is borrowed; like the C test, leave it attached
      --  to the label so GTK releases it with the target object.
      Window.Destroy;
      Glib.Object.Unref (Glib.Object.GObject (Label));
      Glib.Values.Unset (Fallback);
   end Test_Try_Bind;

   ---------------
   -- Test_Bind --
   ---------------

   procedure Test_Bind is
      Source : constant Gtk.String_Filter.Gtk_String_Filter :=
        Gtk.String_Filter.Gtk_String_Filter_New (null);
      Target : constant Gtk.String_Filter.Gtk_String_Filter :=
        Gtk.String_Filter.Gtk_String_Filter_New (null);
      Expr : constant Gtk.Property_Expression.Gtk_Property_Expression :=
        Gtk.Property_Expression.Gtk_Property_Expression_New
          (Gtk.String_Filter.Get_Type, null, "search");
      Watch : Gtk.Expression_Watch.Gtk_Expression_Watch;
   begin
      --  Bind duplicates the expression reference internally; it tracks the
      --  source filter and keeps the target's search property synchronized.
      Watch := Gtk.Expression.Bind
        (Gtk.Expression.Gtk_Expression (Expr),
         Glib.Object.Get_Object (Target), "search",
         Glib.Object.Get_Object (Source));
      Glib.Test.Assert_Cmpstr_Eq (Gtk.String_Filter.Get_Search (Target), "");
      Gtk.String_Filter.Set_Search (Source, "bound value");
      Glib.Test.Assert_Cmpstr_Eq
        (Gtk.String_Filter.Get_Search (Target), "bound value");
      Gtk.Expression_Watch.Unwatch (Watch);
      Gtk.String_Filter.Set_Search (Source, "not propagated");
      Glib.Test.Assert_Cmpstr_Eq
        (Gtk.String_Filter.Get_Search (Target), "bound value");
   end Test_Bind;

   ------------------------------
   -- Test_Watch_User_Data --
   ------------------------------

   procedure Test_Watch_User_Data is
      Filter : constant Gtk.String_Filter.Gtk_String_Filter :=
        Gtk.String_Filter.Gtk_String_Filter_New (null);
      Expr : constant Gtk.Property_Expression.Gtk_Property_Expression :=
        Gtk.Property_Expression.Gtk_Property_Expression_New
          (Gtk.String_Filter.Get_Type, null, "search");
      Watch : Gtk.Expression_Watch.Gtk_Expression_Watch;
   begin
      --  The generic wrapper forwards the supplied integer to the callback
      --  and invokes Destroy when the watch releases that callback data.
      Data_Notifications := 0;
      Destroyed_Data := 0;
      Last_Data := 0;
      Watch := Watch_With_Data.Watch
        (Gtk.Expression.Gtk_Expression (Expr),
         Glib.Object.Get_Object (Filter),
         On_Changed_With_Data'Unrestricted_Access,
         42);
      Gtk.String_Filter.Set_Search (Filter, "notify");
      Glib.Test.Assert_Cmpuint_Eq (Data_Notifications, 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Last_Data), 42);
      Gtk.Expression_Watch.Unwatch (Watch);
      Glib.Test.Assert_Cmpuint_Eq (Destroyed_Data, 1);

   end Test_Watch_User_Data;

   -------------------
   -- Test_Cclosure --
   -------------------

   procedure Test_Cclosure is
      Filter : constant Gtk.String_Filter.Gtk_String_Filter :=
        Gtk.String_Filter.Gtk_String_Filter_New (null);
      Params : Gtk.Expression.Gtk_Expression_Array (0 .. 2);
      Closure : Gtk.Cclosure_Expression.Gtk_Cclosure_Expression;
      Base : Gtk.Expression.Gtk_Expression;
      Wrapped : Gtk.Expression.Gtk_Expression;
      Value : Glib.Values.GValue;
      Object : System.Address;
   begin
      --  Supply recognizable property values to a callback like the one in
      --  testsuite/c_tests/expression.c:test_cclosure.
      Gtk.String_Filter.Set_Search (Filter, "needle");
      Gtk.String_Filter.Set_Ignore_Case (Filter, False);
      Gtk.String_Filter.Set_Match_Mode
        (Filter, Gtk.String_Filter.String_Filter_Match_Mode_Exact);
      Expected_Callback_Object := Glib.Object.Get_Object (Filter);
      Callback_Got_Object := False;
      Callback_Got_Search := False;
      Callback_Got_Ignore_Case := False;
      Callback_Got_Match_Mode := False;

      --  These expressions produce the three C callback arguments after Gtk
      --  supplies the filter itself as the implicit `this` argument.
      Params (0) := Gtk.Expression.Gtk_Expression
        (Gtk.Property_Expression.Gtk_Property_Expression_New
           (Gtk.String_Filter.Get_Type, null, "search"));
      Params (1) := Gtk.Expression.Gtk_Expression
        (Gtk.Property_Expression.Gtk_Property_Expression_New
           (Gtk.String_Filter.Get_Type, null, "ignore-case"));
      Params (2) := Gtk.Expression.Gtk_Expression
        (Gtk.Property_Expression.Gtk_Property_Expression_New
           (Gtk.String_Filter.Get_Type, null, "match-mode"));

      --  Gtk_New initializes the wrapper and transfers references to Gtk for
      --  every expression in Params; the wrapper retains its own references.
      Gtk.Cclosure_Expression.Gtk_New
        (Closure, Glib.GType_String, null, Params,
         To_Gcallback (Print_Filter_Info'Unrestricted_Access),
         System.Null_Address, null);
      Base := Gtk.Expression.Gtk_Expression (Closure);
      Object := Gtk.Expression.Get_Object (Closure.all'Access);
      Glib.Test.Assert_True (Object /= System.Null_Address);
      Glib.Test.Assert_True
        (Gtk.Expression.Get_Value_Type (Base) = Glib.GType_String);

      --  Initialize is documented as a no-op for an already created wrapper.
      --  Calling it again covers that guard without replacing the live object.
      Gtk.Cclosure_Expression.Initialize
        (Closure, Glib.GType_String, null, Params,
         To_Gcallback (Print_Filter_Info'Unrestricted_Access),
         System.Null_Address, null);
      Glib.Test.Assert_True
        (Gtk.Expression.Get_Object (Closure.all'Access) = Object);

      --  Evaluate checks both the callback result and each native argument.
      Glib.Test.Assert_True
        (Gtk.Expression.Evaluate (Base, Expected_Callback_Object, Value));
      Glib.Test.Assert_Cmpstr_Eq (Glib.Values.Get_String (Value), "OK");
      Glib.Values.Unset (Value);
      Glib.Test.Assert_True (Callback_Got_Object);
      Glib.Test.Assert_True (Callback_Got_Search);
      Glib.Test.Assert_True (Callback_Got_Ignore_Case);
      Glib.Test.Assert_True (Callback_Got_Match_Mode);

      --  Re-wrap a borrowed raw pointer. Gtk.Expression's runtime tag
      --  dispatch selects Gtk.Cclosure_Expression.Create for this object type.
      Wrapped := Gtk.Expression.From_Object_None_Ownership (Object);
      Glib.Test.Assert_True (Wrapped /= null);
      Glib.Test.Assert_True
        (Gtk.Expression.Get_Object (Wrapped.all'Access) = Object);

   end Test_Cclosure;

   ---------------------
   -- Test_Closure --
   ---------------------

   procedure Test_Closure is
      Input : Glib.Values.GValue;
      Parameter : Gtk.Constant_Expression.Gtk_Constant_Expression;
      Params : Gtk.Expression.Gtk_Expression_Array (0 .. 0);
      Native_Closure : System.Address;
      Closure : Gtk.Closure_Expression.Gtk_Closure_Expression;
      Base : Gtk.Expression.Gtk_Expression;
      Wrapped : Gtk.Expression.Gtk_Expression;
      Value : Glib.Values.GValue;
      Object : System.Address;
   begin
      --  The source C case uses g_cclosure_new with an empty parameter array;
      --  this callback also verifies that GtkAda forwards an expression value.
      Glib.Values.Init_Set_String (Input, "closure argument");
      Parameter := Gtk.Constant_Expression.Gtk_Constant_Expression_New_For_Value
        (Input);
      Params (0) := Gtk.Expression.Gtk_Expression (Parameter);
      Glib.Values.Unset (Input);
      Native_Closure := G_Cclosure_New
        (To_Closure_Gcallback (Echo_Closure_Parameter'Unrestricted_Access),
         System.Null_Address, null);

      --  Gtk_New calls Initialize and transfers an additional reference to
      --  each parameter expression while preserving the Ada wrapper's own ref.
      Gtk.Closure_Expression.Gtk_New
        (Closure, Glib.GType_String, Native_Closure, Params);
      Base := Gtk.Expression.Gtk_Expression (Closure);
      Object := Gtk.Expression.Get_Object (Closure.all'Access);
      Glib.Test.Assert_True (Object /= System.Null_Address);
      Glib.Test.Assert_True
        (Gtk.Expression.Get_Value_Type (Base) = Glib.GType_String);

      --  A second Initialize call must leave the existing expression intact.
      Gtk.Closure_Expression.Initialize
        (Closure, Glib.GType_String, Native_Closure, Params);
      Glib.Test.Assert_True
        (Gtk.Expression.Get_Object (Closure.all'Access) = Object);

      --  Evaluation invokes the closure with the parameter's string value.
      Callback_Got_Closure_Parameter := False;
      Glib.Test.Assert_True
        (Gtk.Expression.Evaluate (Base, System.Null_Address, Value));
      Glib.Test.Assert_Cmpstr_Eq
        (Glib.Values.Get_String (Value), "closure result");
      Glib.Values.Unset (Value);
      Glib.Test.Assert_True (Callback_Got_Closure_Parameter);

      --  Runtime tag dispatch reconstructs the specific Ada wrapper record.
      Wrapped := Gtk.Expression.From_Object_None_Ownership (Object);
      Glib.Test.Assert_True
        (Wrapped.all in Gtk.Closure_Expression.Gtk_Closure_Expression_Record'Class);
      Glib.Test.Assert_True
        (Gtk.Expression.Get_Object (Wrapped.all'Access) = Object);
   end Test_Closure;

   -------------------------
   -- Test_Dummy_Wrapper --
   -------------------------

   procedure Test_Dummy_Wrapper is
      Raw_Pointer : aliased System.Address := System.Null_Address;
      Dummy : aliased Gtk.Expression.Dummy_Gtk_Expression_Record :=
        Gtk.Expression.Create (Raw_Pointer'Access);
   begin
      --  Gtk.Expression.Create provides the fallback Ada wrapper record.
      --  Constructing it directly verifies that it starts without a C object.
      Glib.Test.Assert_False (Gtk.Expression.Is_Created (Dummy'Access));
      Glib.Test.Assert_True
        (Gtk.Expression.Get_Object (Dummy'Access) = System.Null_Address);
   end Test_Dummy_Wrapper;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func ("/expression/constant", Test_Constant'Unrestricted_Access);
   Glib.Test.Add_Func ("/expression/property-watch", Test_Property_And_Watch'Unrestricted_Access);
   Glib.Test.Add_Func ("/expression/property-constructors", Test_Property_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func ("/expression/object-try", Test_Object_And_Try'Unrestricted_Access);
   Glib.Test.Add_Func ("/expression/try-bind", Test_Try_Bind'Unrestricted_Access);
   Glib.Test.Add_Func ("/expression/bind", Test_Bind'Unrestricted_Access);
   Glib.Test.Add_Func ("/expression/watch-user-data", Test_Watch_User_Data'Unrestricted_Access);
   Glib.Test.Add_Func ("/expression/cclosure", Test_Cclosure'Unrestricted_Access);
   Glib.Test.Add_Func ("/expression/closure", Test_Closure'Unrestricted_Access);
   Glib.Test.Add_Func ("/expression/dummy-wrapper", Test_Dummy_Wrapper'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
