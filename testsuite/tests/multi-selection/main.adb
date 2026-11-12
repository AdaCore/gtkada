--  Port of the constructor, selection, and model checks from
--  testsuite/c_tests/multiselection.c.  The notification case also checks
--  GtkAda's wrappers for the ListModel, SectionModel, and SelectionModel
--  notification helpers.

with Ada.Command_Line;
with GNAT.Strings;
with Interfaces.C.Strings;
with System;

with Glib;                    use Glib;
with Glib.List_Model;         use Glib.List_Model;
with Glib.Object;             use Glib.Object;
with Glib.Properties;
with Glib.Test;               use Glib.Test;
with Gtk.Bitset;              use Gtk.Bitset;
with Gtk.Multi_Selection;     use Gtk.Multi_Selection;
with Gtk.Section_Model;       use Gtk.Section_Model;
with Gtk.Selection_Filter_Model;
use Gtk.Selection_Filter_Model;
with Gtk.Selection_Model;     use Gtk.Selection_Model;
with Gtk.String_List;         use Gtk.String_List;
with Gtk.String_Object;       use Gtk.String_Object;

procedure Main is
   use type System.Address;

   Items_Position  : Guint := Guint'Last;
   Items_Removed   : Guint := Guint'Last;
   Items_Added     : Guint := Guint'Last;
   Sections_Position : Guint := Guint'Last;
   Sections_Count    : Guint := Guint'Last;
   Selection_Position : Guint := Guint'Last;
   Selection_Count    : Guint := Guint'Last;
   Interface_Callback_Position : Guint := Guint'Last;
   Interface_Callback_Count    : Guint := Guint'Last;
   Slot_Callback_Position      : Guint := Guint'Last;
   Slot_Callback_Count         : Guint := Guint'Last;
   Expected_Interface          : Gtk_Selection_Model;
   Callback_Model_Matched      : Boolean := False;
   Slot_Object_Matched         : Boolean := False;
   Expected_Slot_Address       : System.Address := System.Null_Address;

   procedure Test_Create_And_Interfaces with Convention => C;
   procedure Test_Selection_Filter with Convention => C;
   procedure Test_Selection with Convention => C;
   procedure Test_Interface_Wrappers with Convention => C;
   procedure Test_Set_Model with Convention => C;
   procedure Test_Notifications with Convention => C;

   --  These callbacks exercise the two Selection_Changed signal marshallers.
   procedure On_Interface_Selection_Changed
     (Self : Gtk_Selection_Model; Position, N_Items : Guint);

   procedure On_Slot_Selection_Changed
     (Self : access GObject_Record'Class; Position, N_Items : Guint);

   --  C-convention callbacks match the three GTK signal signatures.  Global
   --  slots keep the test independent of callback user-data marshalling.
   procedure On_Items_Changed
     (Instance : System.Address;
      Position, Removed, Added : Guint;
      User_Data : System.Address)
   with Convention => C;

   procedure On_Range_Changed
     (Instance : System.Address;
      Position, N_Items : Guint;
      User_Data : System.Address)
   with Convention => C;

   type Items_Handler is access procedure
     (Instance : System.Address;
      Position, Removed, Added : Guint;
      User_Data : System.Address)
   with Convention => C;

   type Range_Handler is access procedure
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

   function Signal_Connect_Range
     (Instance : System.Address;
      Name : Interfaces.C.Strings.chars_ptr;
      Handler : Range_Handler;
      User_Data, Destroy_Data : System.Address;
      Flags : Gint) return Gulong
   with Import, Convention => C, External_Name => "g_signal_connect_data";

   --  Create a string list with three predictable items.  The multi-selection
   --  constructor takes its own full reference, so callers retain theirs.
   function New_Store return Gtk_String_List is
      Empty : constant GNAT.Strings.String_List (1 .. 0) := (others => null);
      Store : constant Gtk_String_List := Gtk_String_List_New (Empty);
   begin
      Append (Store, "first");
      Append (Store, "second");
      Append (Store, "third");
      return Store;
   end New_Store;

   --  Constructor and interface checks port /multiselection/create and
   --  /multiselection/empty from the C test, including the section bounds
   --  GTK reports for a model without explicit sections.
   procedure Test_Create_And_Interfaces is
      Store : constant Gtk_String_List := New_Store;
      Source : constant Glist_Model := +Store;
      Selection : Gtk_Multi_Selection;
      List_View : Glist_Model;
      Section_View : Gtk_Section_Model;
      Selection_View : Gtk_Selection_Model;
      Item : GObject;
      Start, Finish : Guint;
   begin
      Ref (Store);
      Selection := Gtk_Multi_Selection_New (Source);

      Assert_True (Get_Model (Selection) = Source);
      --  The selection exposes exactly the same item type and count as its
      --  wrapped list model.
      Assert_True (Get_Item_Type (Selection) = Get_Item_Type (Store));
      Assert_Cmpuint_Eq (Get_N_Items (Selection), 3);

      --  Get_Item returns a full reference, which this test releases after
      --  checking the typed GtkStringObject view.
      Item := Get_Item (Selection, 1);
      Assert_True (Item /= null);
      Assert_True (Get_String (Gtk_String_Object (Item)) = "second");
      Unref (Item);
      --  Like any GListModel, the selection returns null outside its bounds.
      Assert_True (Get_Item (Selection, 3) = null);

      Get_Section (Selection, 0, Start, Finish);
      Assert_Cmpuint_Eq (Start, 0);
      Assert_Cmpuint_Eq (Finish, Guint'Last);

      --  Round-trip all implemented interfaces to cover their generated
      --  Ada conversions as well as the list-model, section, and selection
      --  interface views used by GTK clients.
      List_View := +Selection;
      Section_View := +Selection;
      Selection_View := +Selection;
      Assert_True ((-List_View) = Selection);
      Assert_True ((-Section_View) = Selection);
      Assert_True ((-Selection_View) = Selection);

      Unref (Selection);
      Unref (Store);
   end Test_Create_And_Interfaces;

   --  Port of /multiselection/selection-filter and /selection-filter/empty
   --  from the C test.  It also checks both Ada constructors, the model
   --  property, and the inherited ListModel item-change helper.
   procedure Test_Selection_Filter is
      Store : constant Gtk_String_List := New_Store;
      Source : constant Glist_Model := +Store;
      Selection : Gtk_Multi_Selection;
      Other_Selection : Gtk_Multi_Selection;
      Selection_List : Glist_Model;
      Other_Store : constant Gtk_String_List := Gtk_String_List_New
        ((1 => new String'("replacement")));
      Other_Source : constant Glist_Model := +Other_Store;
      Filter : Gtk_Selection_Filter_Model;
      Function_Filter : Gtk_Selection_Filter_Model;
      Filter_View : Glist_Model;
      Property_Model : Gtk.Selection_Model.Gtk_Selection_Model;
      Item : GObject;
      Items_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("items-changed");
      Items_Id : Gulong;
   begin
      --  GtkMultiSelection takes a reference to its model; the original list
      --  reference remains owned here and is released at the end of the test.
      Ref (Store);
      Ref (Other_Store);
      Selection := Gtk_Multi_Selection_New (Source);
      Other_Selection := Gtk_Multi_Selection_New (Other_Source);

      --  Gtk_New exercises the procedure constructor.  Reinitializing an
      --  existing object must leave its first model in place.
      Gtk_New (Filter, +Selection);
      Initialize (Filter, +Other_Selection);
      Assert_True (Get_Model (Filter) = +Selection);
      Assert_Cmpuint_Eq (Get_N_Items (Filter), 0);
      Assert_Cmpuint_Eq
        (Glib.Properties.Get_Property
           (Filter, Gtk.Selection_Filter_Model.N_Items_Property), 0);
      Property_Model := Gtk.Selection_Model.Gtk_Selection_Model
        (Glib.Properties.Get_Property
           (Filter, Gtk.Selection_Filter_Model.Model_Property));
      Assert_True (Property_Model = +Selection);
      Selection_List := +Selection;
      Assert_True (Get_Item_Type (Filter) = Get_Item_Type (Selection_List));
      Assert_True (Get_Item (Filter, 0) = null);

      --  Selection changes are reflected as the filtered list, preserving the
      --  source item's type and order.
      Assert_True (Select_Item (Selection, 1, False));
      Assert_Cmpuint_Eq (Get_N_Items (Filter), 1);
      Item := Get_Item (Filter, 0);
      Assert_True (Item /= null);
      Assert_Cmpstr_Eq (Get_String (Gtk_String_Object (Item)), "second");
      Unref (Item);
      Assert_True (Get_Item (Filter, 1) = null);
      Assert_True (Unselect_Item (Selection, 1));
      Assert_Cmpuint_Eq (Get_N_Items (Filter), 0);

      --  The function constructor follows the same initial-model behavior.
      Function_Filter := Gtk_Selection_Filter_Model_New (+Selection);
      Assert_True (Get_Model (Function_Filter) = +Selection);
      Assert_Cmpuint_Eq (Get_N_Items (Function_Filter), 0);

      --  Replacing the model changes the filtered contents; null clears it.
      Assert_True (Select_Item (Other_Selection, 0, False));
      Set_Model (Filter, +Other_Selection);
      Assert_True (Get_Model (Filter) = +Other_Selection);
      Assert_Cmpuint_Eq (Get_N_Items (Filter), 1);
      Item := Get_Item (Filter, 0);
      Assert_True (Item /= null);
      Assert_Cmpstr_Eq (Get_String (Gtk_String_Object (Item)), "replacement");
      Unref (Item);
      Set_Model (Filter, Gtk.Selection_Model.Null_Gtk_Selection_Model);
      Assert_True (Get_Model (Filter) = Gtk.Selection_Model.Null_Gtk_Selection_Model);
      Assert_Cmpuint_Eq (Get_N_Items (Filter), 0);
      Set_Model (Filter, +Selection);

      --  Round-trip the ListModel interface conversion supplied by the
      --  generated binding, then verify its item count through that view.
      Filter_View := +Filter;
      Assert_True ((-Filter_View) = Filter);
      Assert_Cmpuint_Eq (Glib.List_Model.Get_N_Items (Filter_View), 0);

      --  The inherited notification helper forwards its exact range through
      --  the standard GListModel signal.
      Items_Position := Guint'Last;
      Items_Removed := Guint'Last;
      Items_Added := Guint'Last;
      Items_Id := Signal_Connect_Items
        (Get_Object (Filter), Items_Name,
         On_Items_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
      Interfaces.C.Strings.Free (Items_Name);
      Items_Changed (Filter, 1, 2, 3);
      Assert_True (Items_Id /= 0);
      Assert_Cmpuint_Eq (Items_Position, 1);
      Assert_Cmpuint_Eq (Items_Removed, 2);
      Assert_Cmpuint_Eq (Items_Added, 3);

      Unref (Function_Filter);
      Unref (Filter);
      Unref (Other_Selection);
      Unref (Selection);
      Unref (Other_Store);
      Unref (Store);
   end Test_Selection_Filter;

   --  Port of /multiselection/selection and /multiselection/set_selection.
   --  Each operation is checked through the returned bitset, rather than
   --  relying only on the method's success flag.
   procedure Test_Selection is
      Store : constant Gtk_String_List := New_Store;
      Source : constant Glist_Model := +Store;
      Selection : Gtk_Multi_Selection;
      Current : Gtk_Bitset;
      Selected : Gtk_Bitset;
      Mask : Gtk_Bitset;
      In_Range : Gtk_Bitset;
   begin
      Append (Store, "fourth");
      Append (Store, "fifth");
      Ref (Store);
      Selection := Gtk_Multi_Selection_New (Source);

      Assert_True (Select_Item (Selection, 1, False));
      --  Selection positions are zero-based; selecting one item leaves the
      --  neighboring positions clear.
      Assert_True (Is_Selected (Selection, 1));
      Assert_False (Is_Selected (Selection, 0));
      Current := Get_Selection (Selection);
      Assert_True (Contains (Current, 1));
      Assert_Cmpuint_Eq (Guint (Get_Size (Current)), 1);
      Unref (Current);

      Assert_True (Select_Range (Selection, 3, 2, False));
      --  The range query returns selected model positions in that interval.
      In_Range := Get_Selection_In_Range (Selection, 2, 3);
      Assert_True (Contains (In_Range, 3));
      Assert_True (Contains (In_Range, 4));
      Assert_False (Contains (In_Range, 2));
      Unref (In_Range);

      Assert_True (Unselect_Range (Selection, 3, 1));
      --  Removing one selected position preserves the other selected item.
      Assert_False (Is_Selected (Selection, 3));
      Assert_True (Is_Selected (Selection, 4));

      --  Unselect_Rest clears prior selections before adding the requested
      --  position or range.
      Assert_True (Select_Item (Selection, 2, True));
      Assert_True (Is_Selected (Selection, 2));
      Assert_False (Is_Selected (Selection, 1));
      Assert_True (Select_Range (Selection, 0, 2, True));
      Assert_True (Is_Selected (Selection, 0));
      Assert_True (Is_Selected (Selection, 1));
      Assert_False (Is_Selected (Selection, 2));

      Assert_True (Select_All (Selection));
      --  The full bitset size equals the number of items in the model.
      Current := Get_Selection (Selection);
      Assert_Cmpuint_Eq (Guint (Get_Size (Current)), 5);
      Unref (Current);
      Assert_True (Unselect_Item (Selection, 1));
      Assert_False (Is_Selected (Selection, 1));
      Assert_True (Unselect_All (Selection));
      Current := Get_Selection (Selection);
      Assert_True (Is_Empty (Current));
      Unref (Current);

      --  The mask updates only its positions; selected values outside the
      --  mask are deliberately irrelevant to the result.
      Selected := Gtk_Bitset_New_Empty;
      Assert_True (Add (Selected, 0));
      Assert_True (Add (Selected, 2));
      Mask := Gtk_Bitset_New_Range (0, 3);
      Assert_True (Set_Selection (Selection, Selected, Mask));
      --  Only the mask is applied; the requested selected positions remain.
      Current := Get_Selection (Selection);
      Assert_True (Contains (Current, 0));
      Assert_False (Contains (Current, 1));
      Assert_True (Contains (Current, 2));
      Unref (Current);
      Unref (Selected);
      Unref (Mask);

      Unref (Selection);
      Unref (Store);
   end Test_Selection;

   --  Call each selection operation through the interface view.  The
   --  concrete GtkMultiSelection package has its own wrappers, so using this
   --  view specifically tests Gtk.Selection_Model's binding.
   procedure Test_Interface_Wrappers is
      Store : constant Gtk_String_List := New_Store;
      Selection : Gtk_Multi_Selection;
      Model : Gtk_Selection_Model;
      Current, In_Range, Selected, Mask : Gtk_Bitset;
   begin
      Ref (Store);
      Selection := Gtk_Multi_Selection_New (+Store);
      Model := +Selection;

      --  The base interface's identity operator returns the same interface
      --  reference and all selection queries start empty.
      Assert_True (Gtk.Selection_Model."+" (Model) = Model);
      Current := Gtk.Selection_Model.Get_Selection (Model);
      Assert_True (Is_Empty (Current));
      Unref (Current);
      In_Range := Gtk.Selection_Model.Get_Selection_In_Range (Model, 0, 3);
      Assert_True (Is_Empty (In_Range));
      Unref (In_Range);
      Assert_False (Gtk.Selection_Model.Is_Selected (Model, 1));

      --  Selection queries reflect each successful interface-level update.
      Assert_True (Gtk.Selection_Model.Select_Item (Model, 1, False));
      Assert_True (Gtk.Selection_Model.Is_Selected (Model, 1));
      Assert_True (Gtk.Selection_Model.Select_Range (Model, 0, 2, False));
      In_Range := Gtk.Selection_Model.Get_Selection_In_Range (Model, 0, 3);
      Assert_True (Contains (In_Range, 0));
      Assert_True (Contains (In_Range, 1));
      Assert_False (Contains (In_Range, 2));
      Unref (In_Range);
      Assert_True (Gtk.Selection_Model.Unselect_Range (Model, 0, 1));
      Assert_False (Gtk.Selection_Model.Is_Selected (Model, 0));

      --  Unselect_Rest replaces the existing state; the all and single-item
      --  operations then prove the corresponding interface entry points.
      Assert_True (Gtk.Selection_Model.Select_Item (Model, 2, True));
      Assert_True (Gtk.Selection_Model.Is_Selected (Model, 2));
      Assert_False (Gtk.Selection_Model.Is_Selected (Model, 1));
      Assert_True (Gtk.Selection_Model.Select_All (Model));
      Current := Gtk.Selection_Model.Get_Selection (Model);
      Assert_Cmpuint_Eq (Guint (Get_Size (Current)), 3);
      Unref (Current);
      Assert_True (Gtk.Selection_Model.Unselect_Item (Model, 1));
      Assert_False (Gtk.Selection_Model.Is_Selected (Model, 1));
      Assert_True (Gtk.Selection_Model.Unselect_All (Model));

      --  Set_Selection applies selected values only where the mask has bits.
      Selected := Gtk_Bitset_New_Empty;
      Assert_True (Add (Selected, 0));
      Assert_True (Add (Selected, 2));
      Mask := Gtk_Bitset_New_Range (0, 3);
      Assert_True (Gtk.Selection_Model.Set_Selection (Model, Selected, Mask));
      Current := Gtk.Selection_Model.Get_Selection (Model);
      Assert_True (Contains (Current, 0));
      Assert_False (Contains (Current, 1));
      Assert_True (Contains (Current, 2));
      Unref (Current);
      Unref (Selected);
      Unref (Mask);

      --  Connect both overloads through the interface.  The second overload
      --  uses Selection as its GObject slot, which the marshaller passes to
      --  the callback in place of the emitting model.
      Interface_Callback_Position := Guint'Last;
      Interface_Callback_Count := Guint'Last;
      Slot_Callback_Position := Guint'Last;
      Slot_Callback_Count := Guint'Last;
      Expected_Interface := Model;
      Callback_Model_Matched := False;
      Slot_Object_Matched := False;
      Expected_Slot_Address := Selection.all'Address;
      Gtk.Selection_Model.On_Selection_Changed
        (Model, On_Interface_Selection_Changed'Unrestricted_Access);
      Gtk.Selection_Model.On_Selection_Changed
        (Model, On_Slot_Selection_Changed'Unrestricted_Access,
         Slot => Selection, After => True);
      Gtk.Selection_Model.Selection_Changed (Model, 1, 2);
      Assert_True (Callback_Model_Matched);
      Assert_Cmpuint_Eq (Interface_Callback_Position, 1);
      Assert_Cmpuint_Eq (Interface_Callback_Count, 2);
      Assert_True (Slot_Object_Matched);
      Assert_Cmpuint_Eq (Slot_Callback_Position, 1);
      Assert_Cmpuint_Eq (Slot_Callback_Count, 2);

      Unref (Selection);
      Unref (Store);
   end Test_Interface_Wrappers;

   --  The interface callback records its model and range for assertions after
   --  Gtk emits selection-changed.
   procedure On_Interface_Selection_Changed
     (Self : Gtk_Selection_Model; Position, N_Items : Guint)
   is
   begin
      Callback_Model_Matched := Self = Expected_Interface;
      Interface_Callback_Position := Position;
      Interface_Callback_Count := N_Items;
   end On_Interface_Selection_Changed;

   --  The slotted callback receives the supplied GObject as Self; compare its
   --  address with the selected object to verify the closure's slot data.
   procedure On_Slot_Selection_Changed
     (Self : access GObject_Record'Class; Position, N_Items : Guint)
   is
   begin
      Slot_Object_Matched := Self /= null
        and then Self.all'Address = Expected_Slot_Address;
      Slot_Callback_Position := Position;
      Slot_Callback_Count := N_Items;
   end On_Slot_Selection_Changed;

   --  Port of /multiselection/set-model.  Repeated Initialize is a no-op;
   --  Set_Model replaces the model and accepts null to empty the selection.
   procedure Test_Set_Model is
      First : constant Gtk_String_List := New_Store;
      Empty : constant GNAT.Strings.String_List (1 .. 0) := (others => null);
      Second : constant Gtk_String_List := Gtk_String_List_New (Empty);
      First_View : constant Glist_Model := +First;
      Second_View : constant Glist_Model := +Second;
      Selection : Gtk_Multi_Selection;
   begin
      Append (Second, "replacement");
      Ref (First);
      --  Gtk_New is the procedure constructor; a second initialization must
      --  leave the model supplied by the first call untouched.
      Gtk_New (Selection, First_View);
      Initialize (Selection, Second_View);
      Assert_True (Get_Model (Selection) = First_View);
      Assert_Cmpuint_Eq (Get_N_Items (Selection), 3);

      Set_Model (Selection, Second_View);
      Assert_True (Get_Model (Selection) = Second_View);
      Assert_Cmpuint_Eq (Get_N_Items (Selection), 1);
      Set_Model (Selection, Null_Glist_Model);
      Assert_True (Get_Model (Selection) = Null_Glist_Model);
      Assert_Cmpuint_Eq (Get_N_Items (Selection), 0);

      Unref (Selection);
      Unref (First);
      Unref (Second);
   end Test_Set_Model;

   --  The public helper wrappers emit the corresponding interface signals.
   --  Capture the arguments to check that each wrapper forwards its range.
   procedure Test_Notifications is
      Store : constant Gtk_String_List := New_Store;
      Source : constant Glist_Model := +Store;
      Selection : Gtk_Multi_Selection;
      Items_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("items-changed");
      Sections_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("sections-changed");
      Selection_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("selection-changed");
      Items_Id, Sections_Id, Selection_Id : Gulong;
   begin
      Ref (Store);
      Selection := Gtk_Multi_Selection_New (Source);

      Items_Position := Guint'Last;
      Items_Removed := Guint'Last;
      Items_Added := Guint'Last;
      Sections_Position := Guint'Last;
      Sections_Count := Guint'Last;
      Selection_Position := Guint'Last;
      Selection_Count := Guint'Last;

      Items_Id := Signal_Connect_Items
        (Get_Object (Selection), Items_Name,
         On_Items_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
      --  The notification helpers are low-level interface APIs, so observers
      --  verify the exact ranges delivered through their GTK signals.
      Sections_Id := Signal_Connect_Range
        (Get_Object (Selection), Sections_Name,
         On_Range_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
      Selection_Id := Signal_Connect_Range
        (Get_Object (Selection), Selection_Name,
         On_Range_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
      Interfaces.C.Strings.Free (Items_Name);
      Interfaces.C.Strings.Free (Sections_Name);
      Interfaces.C.Strings.Free (Selection_Name);

      Items_Changed (Selection, 1, 1, 2);
      Assert_Cmpuint_Eq (Items_Position, 1);
      Assert_Cmpuint_Eq (Items_Removed, 1);
      Assert_Cmpuint_Eq (Items_Added, 2);
      Sections_Changed (Selection, 0, 2);
      Assert_Cmpuint_Eq (Sections_Position, 0);
      Assert_Cmpuint_Eq (Sections_Count, 2);
      Selection_Changed (Selection, 2, 1);
      Assert_Cmpuint_Eq (Selection_Position, 2);
      Assert_Cmpuint_Eq (Selection_Count, 1);

      --  Successful connections are also confirmed by all three callbacks
      --  receiving their specific arguments above.
      Assert_True (Items_Id /= 0 and Sections_Id /= 0 and Selection_Id /= 0);
      Unref (Selection);
      Unref (Store);
   end Test_Notifications;

   --  Signal callbacks record only the arguments relevant to each assertion.
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

   procedure On_Range_Changed
     (Instance : System.Address;
      Position, N_Items : Guint;
      User_Data : System.Address)
   is
      pragma Unreferenced (Instance, User_Data);
   begin
      if Position = 0 then
         Sections_Position := Position;
         Sections_Count := N_Items;
      else
         Selection_Position := Position;
         Selection_Count := N_Items;
      end if;
   end On_Range_Changed;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/multi-selection/create-and-interfaces",
      Test_Create_And_Interfaces'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/multi-selection/selection-filter",
      Test_Selection_Filter'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/multi-selection/selection", Test_Selection'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/multi-selection/interface-wrappers",
      Test_Interface_Wrappers'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/multi-selection/set-model", Test_Set_Model'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/multi-selection/notifications",
      Test_Notifications'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
