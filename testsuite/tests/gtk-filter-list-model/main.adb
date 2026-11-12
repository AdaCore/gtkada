--  Port of the creation, empty-model, and section checks in
--  testsuite/c_tests/filterlistmodel.c (test_create, test_empty,
--  test_sections), plus test_string_properties and test_any_simple from
--  testsuite/c_tests/filter.c.
--  The notification case also checks GtkAda's inherited list-model and
--  section-model signal wrappers.

with Ada.Command_Line;
with GNAT.Strings;
with Interfaces.C.Strings;
with System;

with Glib;                    use Glib;
with Glib.List_Model;         use Glib.List_Model;
with Glib.Object;             use Glib.Object;
with Glib.Properties;
with Glib.Test;               use Glib.Test;
with Glib.Values;
with Gtk.Filter;              use Gtk.Filter;
with Gtk.Filter_List_Model;   use Gtk.Filter_List_Model;
with Gtk.Any_Filter;          use Gtk.Any_Filter;
with Gtk.Bool_Filter;
with Gtk.Buildable;
with Gtk.Constant_Expression;
with Gtk.Expression;
with Gtk.Every_Filter;
with Gtk.Main;
with Gtk.Multi_Filter;
with Gtk.Property_Expression; use Gtk.Property_Expression;
with Gtk.Section_Model;       use Gtk.Section_Model;
with Gtk.String_Filter;       use Gtk.String_Filter;
with Gtk.String_List;         use Gtk.String_List;
with Gtk.String_Object;       use Gtk.String_Object;

procedure Main is

   use type Gtk_Filter_List_Model;
   use type Gtk_String_List;
   use type Gtk_String_Filter;
   use type Gtk_Filter;
   use type Gtk_Any_Filter;
   use type Gtk.Every_Filter.Gtk_Every_Filter;
   use type Gtk.Multi_Filter.Gtk_Multi_Filter;
   use type Glist_Model;
   use type Gtk_Section_Model;
   use type GType;
   use type Gtk.Expression.Gtk_Expression;
   use type System.Address;

   Items_Position : Guint := Guint'Last;
   Items_Removed  : Guint := Guint'Last;
   Items_Added    : Guint := Guint'Last;
   Sections_Position : Guint := Guint'Last;
   Sections_Count : Guint := Guint'Last;

   procedure Test_Create with Convention => C;
   procedure Test_String_Filter_Properties with Convention => C;
   procedure Test_Bool_Filter with Convention => C;
   procedure Test_Empty_And_Setters with Convention => C;
   procedure Test_Sections_And_Notifications with Convention => C;
   procedure Test_Multi_Filter with Convention => C;
   procedure Test_Every_Filter with Convention => C;

   --  These callback declarations match g_signal_connect_data's C ABI.
   procedure On_Items_Changed
     (Instance : System.Address;
      Position, Removed, Added : Guint;
      User_Data : System.Address)
   with Convention => C;

   procedure On_Sections_Changed
     (Instance : System.Address;
      Position, N_Items : Guint;
      User_Data : System.Address)
   with Convention => C;

   type Items_Handler is access procedure
     (Instance : System.Address;
      Position, Removed, Added : Guint;
      User_Data : System.Address)
   with Convention => C;

   type Sections_Handler is access procedure
     (Instance : System.Address;
      Position, N_Items : Guint;
      User_Data : System.Address)
   with Convention => C;

   function Signal_Connect_Items
     (Instance : System.Address;
      Name : Interfaces.C.Strings.chars_ptr;
      Handler : Items_Handler;
      User_Data, Destroy_Data : System.Address;
      Flags : Gint) return Gulong
   with Import, Convention => C, External_Name => "g_signal_connect_data";

   function Signal_Connect_Sections
     (Instance : System.Address;
      Name : Interfaces.C.Strings.chars_ptr;
      Handler : Sections_Handler;
      User_Data, Destroy_Data : System.Address;
      Flags : Gint) return Gulong
   with Import, Convention => C, External_Name => "g_signal_connect_data";

   --  Keep a small source list deterministic so assertions can inspect the
   --  filtered items by position.
   function New_Store return Gtk_String_List is
      Empty : constant GNAT.Strings.String_List (1 .. 0) := (others => null);
      Store : constant Gtk_String_List := Gtk_String_List_New (Empty);
   begin
      Append (Store, "maple");
      Append (Store, "oak");
      Append (Store, "ash");
      Append (Store, "pine");
      return Store;
   end New_Store;

   --  The property expression reads GtkStringObject.string, like the C
   --  filterlistmodel test's string-expression cases.
   function New_Filter (Search : String) return Gtk_String_Filter is
      Expression : constant Gtk_Property_Expression :=
        Gtk_Property_Expression_New
          (Gtk.String_Object.Get_Type, null, "string");
      Result : constant Gtk_String_Filter :=
        Gtk_String_Filter_New (Gtk.Expression.Gtk_Expression (Expression));
   begin
      Set_Search (Result, Search);
      return Result;
   end New_Filter;

   -----------------
   -- Test_Create --
   -----------------

   procedure Test_String_Filter_Properties is
      Expression : constant Gtk_Property_Expression :=
        Gtk_Property_Expression_New
          (Gtk.String_Object.Get_Type, null, "string");
      Replacement : constant Gtk_Property_Expression :=
        Gtk_Property_Expression_New
          (Gtk.String_Object.Get_Type, null, "string");
      Filter : Gtk_String_Filter;

      --  GtkExpression is boxed, so each GtkAda conversion can make a new Ada
      --  wrapper; compare the underlying C handles rather than Ada access values.
      function Expression_Handle
        (Value : Gtk.Expression.Gtk_Expression) return System.Address is
      begin
         return Gtk.Expression.Get_Object (Value.all'Access);
      end Expression_Handle;
   begin
      --  Gtk_New transfers a reference to GTK; compare the borrowed expression
      --  while the filter and original expression are both still live.
      Gtk_New (Filter, Gtk.Expression.Gtk_Expression (Expression));
      Assert_True
        (Expression_Handle (Get_Expression (Filter)) =
         Expression_Handle (Gtk.Expression.Gtk_Expression (Expression)));

      --  Replacing the expression checks both sides of the accessor pair.
      Set_Expression (Filter, Gtk.Expression.Gtk_Expression (Replacement));
      Assert_True
        (Expression_Handle (Get_Expression (Filter)) =
         Expression_Handle (Gtk.Expression.Gtk_Expression (Replacement)));

      --  Check both Boolean values through the setter and getter.
      Set_Ignore_Case (Filter, True);
      Assert_True (Get_Ignore_Case (Filter));
      Set_Ignore_Case (Filter, False);
      Assert_False (Get_Ignore_Case (Filter));

      --  Exercise each public match mode to verify the Ada-to-C enum mapping.
      Set_Match_Mode (Filter, String_Filter_Match_Mode_Exact);
      Assert_True (Get_Match_Mode (Filter) = String_Filter_Match_Mode_Exact);
      Set_Match_Mode (Filter, String_Filter_Match_Mode_Prefix);
      Assert_True (Get_Match_Mode (Filter) = String_Filter_Match_Mode_Prefix);
      Set_Match_Mode (Filter, String_Filter_Match_Mode_Substring);
      Assert_True
        (Get_Match_Mode (Filter) = String_Filter_Match_Mode_Substring);
   end Test_String_Filter_Properties;

   procedure Test_Create is
      Store : constant Gtk_String_List := New_Store;
      Filter : constant Gtk_String_Filter := New_Filter ("maple");
      Model : Gtk_Filter_List_Model;
      Via_Procedure : Gtk_Filter_List_Model;
      Item : GObject;
   begin
      --  The C constructor takes full references. Retain the Ada-owned source
      --  and filter references while the model keeps its own references.
      Ref (Store);
      Ref (Filter);
      Model := Gtk_Filter_List_Model_New (+Store, Filter);

      Assert_True (Get_Model (Model) = +Store);
      Assert_True (Get_Filter (Model) = Gtk_Filter (Filter));
      Assert_False (Get_Incremental (Model));
      Assert_Cmpuint_Eq (Get_Pending (Model), 0);
      Assert_False (Get_Watch_Items (Model));
      Assert_Cmpuint_Eq (Get_N_Items (Model), 1);
      Assert_True (Get_Item_Type (Model) = Get_Item_Type (Store));

      --  Get_Item returns a referenced base GObject; release it after checking
      --  the typed GtkStringObject value.
      Item := Get_Item (Model, 0);
      Assert_True (Item /= null);
      Assert_Cmpstr_Eq (Get_String (Gtk_String_Object (Item)), "maple");
      Unref (Item);
      Assert_True (Get_Item (Model, 9) = null);

      --  Gtk_New exercises the procedure constructor as well as Initialize.
      Gtk_New (Via_Procedure, Null_Glist_Model, null);
      Assert_True (Get_Model (Via_Procedure) = Null_Glist_Model);
      Assert_Cmpuint_Eq (Get_N_Items (Via_Procedure), 0);

      Unref (Via_Procedure);
      Unref (Model);
      Unref (Filter);
      Unref (Store);
   end Test_Create;

   -----------------------------
   -- Test_Empty_And_Setters --
   -----------------------------

   procedure Test_Empty_And_Setters is
      Empty : constant GNAT.Strings.String_List (1 .. 0) := (others => null);
      Store : constant Gtk_String_List := Gtk_String_List_New (Empty);
      Filter : constant Gtk_String_Filter := New_Filter ("a");
      Model : constant Gtk_Filter_List_Model :=
        Gtk_Filter_List_Model_New (Null_Glist_Model, null);
   begin
      --  A model with no source and no filter is a valid empty model.
      Assert_True (Get_Filter (Model) = null);
      Assert_True (Get_Model (Model) = Null_Glist_Model);
      Assert_Cmpuint_Eq (Get_N_Items (Model), 0);
      Assert_True (Get_Item (Model, 11) = null);
      Assert_True (Get_Item_Type (Model) = GType_Object);
      Assert_Cmpuint_Eq (Get_Pending (Model), 0);

      --  Replacing the model and filter checks their getters and the filtered
      --  item count; the source contains three strings matching "a".
      Set_Model (Model, +Store);
      Assert_True (Get_Model (Model) = +Store);
      Assert_Cmpuint_Eq (Get_N_Items (Model), 0);
      Set_Filter (Model, Filter);
      Assert_True (Get_Filter (Model) = Gtk_Filter (Filter));
      Assert_Cmpuint_Eq (Get_N_Items (Model), 0);

      --  Add matching data and verify the list updates through its model API.
      Append (Store, "maple");
      Append (Store, "oak");
      Append (Store, "ash");
      Append (Store, "pine");
      Assert_Cmpuint_Eq (Get_N_Items (Model), 3);

      Set_Incremental (Model, True);
      Assert_True (Get_Incremental (Model));
      Set_Incremental (Model, False);
      Assert_False (Get_Incremental (Model));
      Assert_Cmpuint_Eq (Get_Pending (Model), 0);

      Set_Watch_Items (Model, True);
      Assert_True (Get_Watch_Items (Model));
      Set_Watch_Items (Model, False);
      Assert_False (Get_Watch_Items (Model));

      Set_Filter (Model, null);
      Assert_True (Get_Filter (Model) = null);
      Assert_Cmpuint_Eq (Get_N_Items (Model), 4);
      Set_Model (Model, Null_Glist_Model);
      Assert_True (Get_Model (Model) = Null_Glist_Model);
      Assert_Cmpuint_Eq (Get_N_Items (Model), 0);

      Unref (Model);
      Unref (Filter);
      Unref (Store);
   end Test_Empty_And_Setters;

   ---------------------------------------
   -- Test_Sections_And_Notifications --
   ---------------------------------------

   procedure Test_Sections_And_Notifications is
      Store : constant Gtk_String_List := New_Store;
      Filter : constant Gtk_String_Filter := New_Filter ("a");
      Model : Gtk_Filter_List_Model;
      List_Interface : Glist_Model;
      Section_Interface : Gtk_Section_Model;
      Start, Finish : Guint;
      Items_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("items-changed");
      Sections_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("sections-changed");
      Items_Id, Sections_Id : Gulong;
   begin
      Ref (Store);
      Ref (Filter);
      Model := Gtk_Filter_List_Model_New (+Store, Filter);

      --  StringList has one section; the filter model reports its current end.
      Get_Section (Model, 0, Start, Finish);
      Assert_Cmpuint_Eq (Start, 0);
      Assert_Cmpuint_Eq (Finish, Get_N_Items (Model));

      --  Round-trip both interfaces implemented by GtkFilterListModel.
      List_Interface := +Model;
      Section_Interface := +Model;
      Assert_True ((-List_Interface) = Model);
      Assert_True ((-Section_Interface) = Model);
      Assert_Cmpuint_Eq (Get_N_Items (Model), 3);

      --  Connect signal observers so both inherited notification helpers are
      --  checked for the exact arguments delivered by GTK.
      Items_Position := Guint'Last;
      Items_Removed := Guint'Last;
      Items_Added := Guint'Last;
      Sections_Position := Guint'Last;
      Sections_Count := Guint'Last;
      Items_Id := Signal_Connect_Items
        (Get_Object (Model), Items_Name,
         On_Items_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
      Sections_Id := Signal_Connect_Sections
        (Get_Object (Model), Sections_Name,
         On_Sections_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
      Interfaces.C.Strings.Free (Items_Name);
      Interfaces.C.Strings.Free (Sections_Name);

      Gtk.Filter_List_Model.Items_Changed (Model, 1, 1, 1);
      Assert_Cmpuint_Eq (Items_Position, 1);
      Assert_Cmpuint_Eq (Items_Removed, 1);
      Assert_Cmpuint_Eq (Items_Added, 1);
      Gtk.Filter_List_Model.Sections_Changed (Model, 1, 2);
      Assert_Cmpuint_Eq (Sections_Position, 1);
      Assert_Cmpuint_Eq (Sections_Count, 2);
      Assert_True (Items_Id /= 0 and Sections_Id /= 0);

      Unref (Model);
      Unref (Filter);
      Unref (Store);
   end Test_Sections_And_Notifications;

   -----------------------
   -- Test_Multi_Filter --
   -----------------------

   procedure Test_Multi_Filter is
      Store : constant Gtk_String_List := New_Store;
      Any : constant Gtk_Any_Filter := Gtk_Any_Filter_New;
      --  MultiFilter's generated methods take an access to its parent record;
      --  this access view preserves the derived object's actual GObject.
      Base : not null access Gtk.Multi_Filter.Gtk_Multi_Filter_Record :=
        Gtk.Multi_Filter.Gtk_Multi_Filter_Record (Any.all)'Access;
      Filter_A : constant Gtk_String_Filter := New_Filter ("a");
      Filter_P : constant Gtk_String_Filter := New_Filter ("p");
      Via_Procedure : Gtk_Any_Filter;
      Model : Gtk_Filter_List_Model;
      List_Interface : Glist_Model;
      Buildable : Gtk.Buildable.Gtk_Buildable;
      Item : GObject;
      Items_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("items-changed");
      Items_Id : Gulong;

      --  Checking each returned string verifies the filter's OR semantics and
      --  ensures its model keeps the original source ordering.
      procedure Assert_Model_Item (Position : Guint; Expected : String) is
         Value : constant GObject := Get_Item (Model, Position);
      begin
         Assert_True (Value /= null);
         Assert_Cmpstr_Eq
           (Get_String (Gtk_String_Object (Value)), Expected);
         Unref (Value);
      end Assert_Model_Item;
   begin
      Assert_True (Gtk.Multi_Filter.Get_Type /= GType_Invalid);

      --  Exercise GtkAnyFilter's Gtk_New constructor as well as the function
      --  constructor used below; both create an empty GtkFilter list model.
      Gtk_New (Via_Procedure);
      Assert_Cmpuint_Eq (Gtk.Any_Filter.Get_N_Items (Via_Procedure), 0);
      Assert_True
        (Gtk.Any_Filter.Get_Item_Type (Via_Procedure) = Gtk.Filter.Get_Type);
      Unref (Via_Procedure);

      --  The model and the Ada variables each retain a reference because the
      --  C constructor takes full references to both arguments.
      Ref (Store);
      Ref (Any);
      Model := Gtk_Filter_List_Model_New (+Store, Gtk_Filter (Any));
      Assert_Cmpuint_Eq (Get_N_Items (Model), 0);

      --  GtkAnyFilter matches nothing while empty, then unions the results of
      --  its child filters as test_any_simple in testsuite/c_tests/filter.c.
      Gtk.Multi_Filter.Append (Base, Filter_A);
      Assert_Cmpuint_Eq (Get_N_Items (Model), 3);
      Assert_Model_Item (0, "maple");
      Assert_Model_Item (1, "oak");
      Assert_Model_Item (2, "ash");

      Gtk.Multi_Filter.Append (Base, Filter_P);
      Assert_Cmpuint_Eq (Gtk.Multi_Filter.Get_N_Items (Base), 2);
      Assert_Cmpuint_Eq
        (Glib.Properties.Get_Property
           (Any, Gtk.Multi_Filter.N_Items_Property), 2);
      --  Call GtkAnyFilter's own ListModel wrappers; the parent package has
      --  equivalent methods, but those do not cover this generated unit.
      Assert_True
        (Gtk.Any_Filter.Get_Item_Type (Any) = Gtk.Filter.Get_Type);
      Assert_Cmpuint_Eq (Gtk.Any_Filter.Get_N_Items (Any), 2);
      --  Build stores the C terminator as part of the property name string.
      Assert_True
        (Glib.Property_Name
           (Glib.Property (Gtk.Multi_Filter.Item_Type_Property)) =
         "item-type" & ASCII.NUL);
      Assert_Cmpuint_Eq (Get_N_Items (Model), 4);
      Assert_Model_Item (0, "maple");
      Assert_Model_Item (1, "oak");
      Assert_Model_Item (2, "ash");
      Assert_Model_Item (3, "pine");

      --  The fetched child is a new reference, released after comparing its
      --  underlying GObject with the second appended filter.
      Item := Gtk.Any_Filter.Get_Item (Any, 1);
      Assert_True (Item /= null);
      Assert_True (Get_Object (Item) = Get_Object (Filter_P));
      Unref (Item);
      Assert_True (Gtk.Any_Filter.Get_Item (Any, 10) = null);
      Assert_True (Gtk.Multi_Filter.Get_Item_Type (Base) = Gtk.Filter.Get_Type);

      --  Round-trip both interfaces inherited from GtkMultiFilter.
      List_Interface := Gtk.Multi_Filter."+" (Any);
      Assert_True
        (Gtk.Multi_Filter."-" (List_Interface) =
         Gtk.Multi_Filter.Gtk_Multi_Filter (Any));
      Buildable := Gtk.Multi_Filter."+" (Any);
      Assert_True
        (Gtk.Multi_Filter."-" (Buildable) =
         Gtk.Multi_Filter.Gtk_Multi_Filter (Any));

      --  An out-of-range removal is a no-op; removing each valid child updates
      --  the model immediately and preserves the matching-item order.
      Gtk.Multi_Filter.Remove (Base, 10);
      Assert_Cmpuint_Eq (Gtk.Multi_Filter.Get_N_Items (Base), 2);
      Assert_Cmpuint_Eq (Get_N_Items (Model), 4);
      Gtk.Multi_Filter.Remove (Base, 0);
      Assert_Cmpuint_Eq (Get_N_Items (Model), 2);
      Assert_Model_Item (0, "maple");
      Assert_Model_Item (1, "pine");
      Gtk.Multi_Filter.Remove (Base, 0);
      Assert_Cmpuint_Eq (Gtk.Multi_Filter.Get_N_Items (Base), 0);
      Assert_Cmpuint_Eq (Get_N_Items (Model), 0);

      --  Items_Changed is the inherited GListModel notification helper. The
      --  callback checks the exact signal arguments sent through its wrapper.
      Items_Position := Guint'Last;
      Items_Removed := Guint'Last;
      Items_Added := Guint'Last;
      Items_Id := Signal_Connect_Items
        (Get_Object (Any), Items_Name,
         On_Items_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
      Interfaces.C.Strings.Free (Items_Name);
      --  Exercise AnyFilter's generated notification helper and check that
      --  its signal carries the requested position and change counts.
      Gtk.Any_Filter.Items_Changed (Any, 2, 1, 3);
      Assert_True (Items_Id /= 0);
      Assert_Cmpuint_Eq (Items_Position, 2);
      Assert_Cmpuint_Eq (Items_Removed, 1);
      Assert_Cmpuint_Eq (Items_Added, 3);

      Unref (Model);
      Unref (Any);
      Unref (Store);
   end Test_Multi_Filter;

   ------------------------
   -- Test_Every_Filter --
   ------------------------

   procedure Test_Every_Filter is
      Store : constant Gtk_String_List := New_Store;
      Every : constant Gtk.Every_Filter.Gtk_Every_Filter :=
        Gtk.Every_Filter.Gtk_Every_Filter_New;
      First : constant Gtk_String_Filter := New_Filter ("a");
      Second : constant Gtk_String_Filter := New_Filter ("p");
      Via_Procedure : Gtk.Every_Filter.Gtk_Every_Filter;
      Base : not null access Gtk.Multi_Filter.Gtk_Multi_Filter_Record :=
        Gtk.Multi_Filter.Gtk_Multi_Filter_Record (Every.all)'Access;
      Model : Gtk_Filter_List_Model;
      List_Interface : Glist_Model;
      Buildable : Gtk.Buildable.Gtk_Buildable;
      Item : GObject;
      Items_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("items-changed");
      Items_Id : Gulong;
   begin
      Assert_True (Gtk.Every_Filter.Get_Type /= GType_Invalid);

      --  Both constructors make an empty filter; Initialize on the
      --  constructor-created object exercises its documented no-op branch.
      Gtk.Every_Filter.Gtk_New (Via_Procedure);
      Assert_Cmpuint_Eq (Gtk.Every_Filter.Get_N_Items (Via_Procedure), 0);
      Assert_True
        (Gtk.Every_Filter.Get_Item_Type (Via_Procedure) = Gtk.Filter.Get_Type);
      Gtk.Every_Filter.Initialize (Via_Procedure);
      Assert_Cmpuint_Eq (Gtk.Every_Filter.Get_N_Items (Via_Procedure), 0);
      Unref (Gtk_Filter (Via_Procedure));

      --  The empty EveryFilter matches all source items, as documented.
      Ref (Store);
      Ref (Gtk_Filter (Every));
      Model := Gtk_Filter_List_Model_New (+Store, Gtk_Filter (Every));
      Assert_Cmpuint_Eq (Gtk.Every_Filter.Get_N_Items (Every), 0);
      Assert_Cmpuint_Eq (Get_N_Items (Model), 4);
      Gtk.Every_Filter.Initialize (Every);
      Assert_Cmpuint_Eq (Get_N_Items (Model), 4);

      --  Round-trip both interfaces that GtkEveryFilter inherits from
      --  GtkMultiFilter; these operators are aliases generated on this type.
      List_Interface := Gtk.Every_Filter."+" (Every);
      Assert_True (Gtk.Every_Filter."-" (List_Interface) = Every);
      Buildable := Gtk.Every_Filter."+" (Every);
      Assert_True (Gtk.Every_Filter."-" (Buildable) = Every);

      --  Append transfers a reference to each child, so retain our own
      --  references while also checking the child list through Get_Item.
      Ref (First);
      Gtk.Multi_Filter.Append (Base, First);
      Assert_Cmpuint_Eq (Gtk.Every_Filter.Get_N_Items (Every), 1);
      Item := Gtk.Every_Filter.Get_Item (Every, 0);
      Assert_True (Item /= null);
      Assert_True (Get_Object (Item) = Get_Object (First));
      Unref (Item);
      Assert_True (Gtk.Every_Filter.Get_Item (Every, 9) = null);
      Assert_Cmpuint_Eq (Get_N_Items (Model), 3);

      --  The conjunction keeps only strings matching both searches, then
      --  removing a child shows that the remaining filter controls the model.
      Ref (Second);
      Gtk.Multi_Filter.Append (Base, Second);
      Assert_Cmpuint_Eq (Gtk.Every_Filter.Get_N_Items (Every), 2);
      Assert_Cmpuint_Eq (Get_N_Items (Model), 1);
      Item := Get_Item (Model, 0);
      Assert_Cmpstr_Eq (Get_String (Gtk_String_Object (Item)), "maple");
      Unref (Item);

      --  The inherited ListModel notification wrapper forwards its arguments
      --  through the standard GListModel signal.
      Items_Position := Guint'Last;
      Items_Removed := Guint'Last;
      Items_Added := Guint'Last;
      Items_Id := Signal_Connect_Items
        (Get_Object (Every), Items_Name,
         On_Items_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
      Interfaces.C.Strings.Free (Items_Name);
      Gtk.Every_Filter.Items_Changed (Every, 2, 1, 3);
      Assert_True (Items_Id /= 0);
      Assert_Cmpuint_Eq (Items_Position, 2);
      Assert_Cmpuint_Eq (Items_Removed, 1);
      Assert_Cmpuint_Eq (Items_Added, 3);

      --  Removing both children restores the empty filter's match-all rule.
      Gtk.Multi_Filter.Remove (Base, 0);
      Assert_Cmpuint_Eq (Get_N_Items (Model), 2);
      Gtk.Multi_Filter.Remove (Base, 0);
      Assert_Cmpuint_Eq (Gtk.Every_Filter.Get_N_Items (Every), 0);
      Assert_Cmpuint_Eq (Get_N_Items (Model), 4);

      Unref (Model);
      Unref (Gtk_Filter (Every));
      Unref (Store);
      Unref (First);
      Unref (Second);
   end Test_Every_Filter;

   --  GtkBoolFilter evaluates a boolean GtkExpression. Constant expressions
   --  let this test check its behavior without introducing a callback fixture.
   function New_Boolean_Expression
     (Value : Boolean) return Gtk.Expression.Gtk_Expression
   is
      Input : Glib.Values.GValue;
      Result : Gtk.Constant_Expression.Gtk_Constant_Expression;
   begin
      Glib.Values.Init_Set_Boolean (Input, Value);
      Result := Gtk.Constant_Expression.Gtk_Constant_Expression_New_For_Value
        (Input);
      Glib.Values.Unset (Input);
      return Gtk.Expression.Gtk_Expression (Result);
   end New_Boolean_Expression;

   --  GtkExpression is boxed and access conversions can return a fresh Ada
   --  wrapper, so expression identity is checked by comparing its C handle.
   function Expression_Handle
     (Value : Gtk.Expression.Gtk_Expression) return System.Address
   is
   begin
      return Gtk.Expression.Get_Object (Value.all'Access);
   end Expression_Handle;

   ----------------------
   -- Test_Bool_Filter --
   ----------------------

   procedure Test_Bool_Filter is
      True_Expression : constant Gtk.Expression.Gtk_Expression :=
        New_Boolean_Expression (True);
      False_Expression : constant Gtk.Expression.Gtk_Expression :=
        New_Boolean_Expression (False);
      Item : constant Gtk_String_Object := Gtk_String_Object_New ("unused");
      Filter : constant Gtk.Bool_Filter.Gtk_Bool_Filter :=
        Gtk.Bool_Filter.Gtk_Bool_Filter_New (True_Expression);
      Via_Procedure : Gtk.Bool_Filter.Gtk_Bool_Filter;
      Empty_Filter : Gtk.Bool_Filter.Gtk_Bool_Filter;
   begin
      --  The function constructor owns an expression reference, and the
      --  borrowed getter must identify that same underlying expression.
      Assert_True (Gtk.Bool_Filter.Get_Type /= GType_Invalid);
      Assert_True
        (Expression_Handle (Gtk.Bool_Filter.Get_Expression (Filter)) =
         Expression_Handle (True_Expression));
      Assert_False (Gtk.Bool_Filter.Get_Invert (Filter));
      Assert_True (Match (Gtk_Filter (Filter), Item));

      --  Inversion changes the match result and is also exposed by the
      --  generated boolean property.
      Gtk.Bool_Filter.Set_Invert (Filter, True);
      Assert_True (Gtk.Bool_Filter.Get_Invert (Filter));
      Assert_True
        (Glib.Properties.Get_Property
           (Filter, Gtk.Bool_Filter.Invert_Property));
      Assert_False (Match (Gtk_Filter (Filter), Item));
      Glib.Properties.Set_Property
        (Filter, Gtk.Bool_Filter.Invert_Property, False);
      Assert_False (Gtk.Bool_Filter.Get_Invert (Filter));
      Assert_True (Match (Gtk_Filter (Filter), Item));

      --  Replacing the expression updates both the accessor and filtering
      --  result, matching the checks in test_bool_simple from filter.c.
      Gtk.Bool_Filter.Set_Expression (Filter, False_Expression);
      Assert_True
        (Expression_Handle (Gtk.Bool_Filter.Get_Expression (Filter)) =
         Expression_Handle (False_Expression));
      Assert_False (Match (Gtk_Filter (Filter), Item));
      Gtk.Bool_Filter.Set_Expression (Filter, True_Expression);
      Assert_True (Match (Gtk_Filter (Filter), Item));

      --  The procedure constructor covers the same non-null initialization
      --  path; a second Initialize call must leave the original expression.
      Gtk.Bool_Filter.Gtk_New (Via_Procedure, True_Expression);
      Gtk.Bool_Filter.Initialize (Via_Procedure, False_Expression);
      Assert_True
        (Expression_Handle
           (Gtk.Bool_Filter.Get_Expression (Via_Procedure)) =
         Expression_Handle (True_Expression));

      --  A null initial expression follows the other initialization branch.
      --  Setting NULL later matches no item, as in the C test.
      Gtk.Bool_Filter.Gtk_New (Empty_Filter, null);
      Assert_True (Gtk.Bool_Filter.Get_Expression (Empty_Filter) = null);
      Assert_False (Match (Gtk_Filter (Empty_Filter), Item));
      Gtk.Bool_Filter.Set_Expression (Filter, null);
      Assert_True (Gtk.Bool_Filter.Get_Expression (Filter) = null);
      Assert_False (Match (Gtk_Filter (Filter), Item));

      Unref (Gtk_Filter (Empty_Filter));
      Unref (Gtk_Filter (Via_Procedure));
      Unref (Gtk_Filter (Filter));
      Unref (Item);
   end Test_Bool_Filter;

   -----------------------
   -- Signal callbacks --
   -----------------------

   procedure On_Items_Changed
     (Instance : System.Address;
      Position, Removed, Added : Guint;
      User_Data : System.Address)
   is
      pragma Unreferenced (Instance, User_Data);
   begin
      Items_Position := Position;
      Items_Removed := Removed;
      Items_Added := Added;
   end On_Items_Changed;

   procedure On_Sections_Changed
     (Instance : System.Address;
      Position, N_Items : Guint;
      User_Data : System.Address)
   is
      pragma Unreferenced (Instance, User_Data);
   begin
      Sections_Position := Position;
      Sections_Count := N_Items;
   end On_Sections_Changed;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/filter-list-model/string-filter-properties",
      Test_String_Filter_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/filter-list-model/bool-filter",
      Test_Bool_Filter'Unrestricted_Access);
   Glib.Test.Add_Func ("/filter-list-model/create", Test_Create'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/filter-list-model/empty-and-setters",
      Test_Empty_And_Setters'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/filter-list-model/sections-and-notifications",
      Test_Sections_And_Notifications'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/filter-list-model/multi-filter",
      Test_Multi_Filter'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/filter-list-model/every-filter",
      Test_Every_Filter'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
