--  Port of the creation, selection, model, and section cases in
--  testsuite/c_tests/noselection.c. The sort-list-model case ports
--  test_create, test_set_model, test_set_sorter, and test_sections from
--  testsuite/c_tests/sortlistmodel.c. Notification cases also check inherited
--  list and section notification wrappers.

with Ada.Command_Line;
with GNAT.Strings;
with Interfaces.C.Strings;
with System;

with Glib;                    use Glib;
with Glib.List_Model;         use Glib.List_Model;
with Glib.Object;             use Glib.Object;
with Glib.Test;               use Glib.Test;
with Gtk.Bitset;              use Gtk.Bitset;
with Gtk.Custom_Sorter;       use Gtk.Custom_Sorter;
with Gtk.No_Selection;        use Gtk.No_Selection;
with Gtk.Section_Model;       use Gtk.Section_Model;
with Gtk.Selection_Model;     use Gtk.Selection_Model;
with Gtk.Sort_List_Model;     use Gtk.Sort_List_Model;
with Gtk.Sorter;               use Gtk.Sorter;
with Gtk.String_List;         use Gtk.String_List;
with Gtk.String_Object;       use Gtk.String_Object;

procedure Main is

   Items_Position : Guint := Guint'Last;
   Items_Removed  : Guint := Guint'Last;
   Items_Added    : Guint := Guint'Last;
   Sections_Position : Guint := Guint'Last;
   Sections_Count : Guint := Guint'Last;
   Selection_Position : Guint := Guint'Last;
   Selection_Count : Guint := Guint'Last;

   procedure Test_Create with Convention => C;
   procedure Test_Empty with Convention => C;
   procedure Test_Selection with Convention => C;
   procedure Test_Changes with Convention => C;
   procedure Test_Set_Model with Convention => C;
   procedure Test_Sections with Convention => C;
   procedure Test_Notifications with Convention => C;
   procedure Test_Sort_List_Model with Convention => C;

   --  The section test sorts strings lexically inside groups keyed by their
   --  first character, making the expected section boundaries deterministic.
   function Compare_Lexical
     (A, B : not null access GObject_Record'Class) return Gint
   with Convention => C;

   function Compare_Group
     (A, B : not null access GObject_Record'Class) return Gint
   with Convention => C;

   --  These callbacks match the GTK signal signatures. Global slots make the
   --  delivered position and count available for Glib.Test assertions.
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

   --  A small deterministic source model for each independent case.
   function New_Store return Gtk_String_List is
      Empty : constant GNAT.Strings.String_List (1 .. 0) := (others => null);
      Store : constant Gtk_String_List := Gtk_String_List_New (Empty);
   begin
      Append (Store, "first");
      Append (Store, "second");
      Append (Store, "third");
      return Store;
   end New_Store;

   --------------------
   -- Compare_Lexical --
   --------------------

   function Compare_Lexical
     (A, B : not null access GObject_Record'Class) return Gint
   is
      Left  : constant String := Get_String (Gtk_String_Object (A));
      Right : constant String := Get_String (Gtk_String_Object (B));
   begin
      if Left < Right then
         return -1;
      elsif Left > Right then
         return 1;
      else
         return 0;
      end if;
   end Compare_Lexical;

   ------------------
   -- Compare_Group --
   ------------------

   function Compare_Group
     (A, B : not null access GObject_Record'Class) return Gint
   is
      Left  : constant String := Get_String (Gtk_String_Object (A));
      Right : constant String := Get_String (Gtk_String_Object (B));
   begin
      if Left (Left'First) < Right (Right'First) then
         return -1;
      elsif Left (Left'First) > Right (Right'First) then
         return 1;
      else
         return 0;
      end if;
   end Compare_Group;

   -----------------
   -- Test_Create --
   -----------------

   procedure Test_Create is
      Store : constant Gtk_String_List := New_Store;
      Source : constant Glist_Model := +Store;
      Selection : Gtk_No_Selection;
      List_View : Glist_Model;
      Section_View : Gtk_Section_Model;
      Selection_View : Gtk_Selection_Model;
      Item : GObject;
      Start, Finish : Guint;
   begin
      --  The constructor consumes a model reference, so retain one for this
      --  test to release after the selection wrapper is destroyed.
      Ref (Store);
      Selection := Gtk_No_Selection_New (Source);

      Assert_True (Get_Model (Selection) = Source);
      Assert_True (Get_Item_Type (Selection) = Get_Item_Type (Store));
      Assert_Cmpuint_Eq (Get_N_Items (Selection), 3);

      --  List-model access returns a referenced object; release it after the
      --  typed GtkStringObject view has checked the item at position one.
      Item := Get_Item (Selection, 1);
      Assert_True (Item /= null);
      Assert_Cmpstr_Eq (Get_String (Gtk_String_Object (Item)), "second");
      Unref (Item);
      Assert_True (Get_Item (Selection, 3) = null);

      --  A plain string list has one implicit section spanning its items.
      Get_Section (Selection, 0, Start, Finish);
      Assert_Cmpuint_Eq (Start, 0);
      Assert_Cmpuint_Eq (Finish, Guint'Last);

      --  Round-trip all three interfaces exposed by GtkNoSelection.
      List_View := +Selection;
      Section_View := +Selection;
      Selection_View := +Selection;
      Assert_True ((-List_View) = Selection);
      Assert_True ((-Section_View) = Selection);
      Assert_True ((-Selection_View) = Selection);

      Unref (Selection);
      Unref (Store);
   end Test_Create;

   ----------------
   -- Test_Empty --
   ----------------

   procedure Test_Empty is
      Selection : constant Gtk_No_Selection :=
        Gtk_No_Selection_New (Null_Glist_Model);
      Empty : constant GNAT.Strings.String_List (1 .. 0) := (others => null);
      Store : constant Gtk_String_List := Gtk_String_List_New (Empty);
   begin
      --  A null source produces a valid empty model and returns no item even
      --  when asked for a position outside its range.
      Assert_Cmpuint_Eq (Get_N_Items (Selection), 0);
      Assert_True (Get_Item (Selection, 11) = null);
      Assert_True (Get_Model (Selection) = Null_Glist_Model);
      Assert_True (Get_Item_Type (Selection) = GType_Object);

      --  Replacing the null source with a real empty model remains empty and
      --  preserves its item type.
      Set_Model (Selection, +Store);
      Assert_True (Get_Model (Selection) = +Store);
      Assert_Cmpuint_Eq (Get_N_Items (Selection), 0);
      Assert_True (Get_Item (Selection, 11) = null);
      Assert_True (Get_Item_Type (Selection) = Get_Item_Type (Store));

      Unref (Selection);
      Unref (Store);
   end Test_Empty;

   --------------------
   -- Test_Selection --
   --------------------

   procedure Test_Selection is
      Store : constant Gtk_String_List := New_Store;
      Selection : Gtk_No_Selection;
      Current, In_Range, Selected, Mask : Gtk_Bitset;
   begin
      Ref (Store);
      Selection := Gtk_No_Selection_New (+Store);

      --  GtkNoSelection always returns an empty bitset, including for a range,
      --  and every selection-changing operation reports that it did nothing.
      Current := Get_Selection (Selection);
      Assert_True (Is_Empty (Current));
      Unref (Current);
      In_Range := Get_Selection_In_Range (Selection, 1, 2);
      Assert_True (Is_Empty (In_Range));
      Unref (In_Range);
      Assert_False (Is_Selected (Selection, 1));

      Assert_False (Select_Item (Selection, 1, False));
      Assert_False (Unselect_Item (Selection, 1));
      Assert_False (Select_Range (Selection, 1, 2, False));
      Assert_False (Unselect_Range (Selection, 1, 2));
      Assert_False (Select_All (Selection));
      Assert_False (Unselect_All (Selection));

      Selected := Gtk_Bitset_New_Empty;
      Mask := Gtk_Bitset_New_Empty;
      Assert_False (Set_Selection (Selection, Selected, Mask));
      Unref (Selected);
      Unref (Mask);

      Current := Get_Selection (Selection);
      Assert_True (Is_Empty (Current));
      Unref (Current);
      Unref (Selection);
      Unref (Store);
   end Test_Selection;

   ------------------
   -- Test_Changes --
   ------------------

   procedure Test_Changes is
      Store : constant Gtk_String_List := New_Store;
      Selection : Gtk_No_Selection;
      Item : GObject;
   begin
      Ref (Store);
      Selection := Gtk_No_Selection_New (+Store);
      Assert_Cmpuint_Eq (Get_N_Items (Selection), 3);

      --  Mutations to the wrapped GListModel are reflected through the
      --  selection wrapper, as in the C test's insert/remove/splice checks.
      Append (Store, "fourth");
      Assert_Cmpuint_Eq (Get_N_Items (Selection), 4);
      Item := Get_Item (Selection, 3);
      Assert_Cmpstr_Eq (Get_String (Gtk_String_Object (Item)), "fourth");
      Unref (Item);
      Remove (Store, 1);
      Assert_Cmpuint_Eq (Get_N_Items (Selection), 3);
      Item := Get_Item (Selection, 1);
      Assert_Cmpstr_Eq (Get_String (Gtk_String_Object (Item)), "third");
      Unref (Item);

      Unref (Selection);
      Unref (Store);
   end Test_Changes;

   ---------------------
   -- Test_Set_Model --
   ---------------------

   procedure Test_Set_Model is
      First : constant Gtk_String_List := New_Store;
      Empty : constant GNAT.Strings.String_List (1 .. 0) := (others => null);
      Second : constant Gtk_String_List := Gtk_String_List_New (Empty);
      Selection : Gtk_No_Selection;
   begin
      Ref (First);
      --  Gtk_New reaches the procedure constructor. Initialize on an already
      --  created object must leave the original model in place.
      Gtk_New (Selection, +First);
      Initialize (Selection, +Second);
      Assert_True (Get_Model (Selection) = +First);
      Assert_Cmpuint_Eq (Get_N_Items (Selection), 3);

      Set_Model (Selection, +Second);
      Assert_True (Get_Model (Selection) = +Second);
      Assert_Cmpuint_Eq (Get_N_Items (Selection), 0);
      Set_Model (Selection, Null_Glist_Model);
      Assert_True (Get_Model (Selection) = Null_Glist_Model);
      Assert_Cmpuint_Eq (Get_N_Items (Selection), 0);

      Unref (Selection);
      Unref (First);
      Unref (Second);
   end Test_Set_Model;

   --------------------
   -- Test_Sections --
   --------------------

   procedure Test_Sections is
      Empty : constant GNAT.Strings.String_List (1 .. 0) := (others => null);
      Store : constant Gtk_String_List := Gtk_String_List_New (Empty);
      Inner_Sorter : constant Gtk_Custom_Sorter :=
        Gtk_Custom_Sorter_New (Compare_Lexical'Unrestricted_Access);
      Group_Sorter : constant Gtk_Custom_Sorter :=
        Gtk_Custom_Sorter_New (Compare_Group'Unrestricted_Access);
      Sorted : Gtk_Sort_List_Model;
      Selection : Gtk_No_Selection;
      Start, Finish : Guint;
      Item : GObject;
      Sections_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("sections-changed");
      Sections_Id : Gulong;
   begin
      Append (Store, "ba");
      Append (Store, "aa");
      Append (Store, "ab");
      Append (Store, "bb");
      Append (Store, "ca");

      Ref (Store);
      Sorted := Gtk_Sort_List_Model_New (+Store, Inner_Sorter);
      --  The constructor takes the sorter's reference; the section-sorting
      --  setter retains its own reference, so release that local one.
      Sorted.Set_Section_Sorter (Group_Sorter);
      Unref (Group_Sorter);
      Ref (Sorted);
      Selection := Gtk_No_Selection_New (+Sorted);

      --  GtkNoSelection preserves the section boundaries of a section-aware
      --  source model, including its order of items within each section.
      Assert_Cmpuint_Eq (Get_N_Items (Selection), 5);
      Get_Section (Selection, 0, Start, Finish);
      Assert_Cmpuint_Eq (Start, 0);
      Assert_Cmpuint_Eq (Finish, 2);
      Get_Section (Selection, 2, Start, Finish);
      Assert_Cmpuint_Eq (Start, 2);
      Assert_Cmpuint_Eq (Finish, 4);
      Get_Section (Selection, 4, Start, Finish);
      Assert_Cmpuint_Eq (Start, 4);
      Assert_Cmpuint_Eq (Finish, 5);
      Item := Get_Item (Selection, 0);
      Assert_Cmpstr_Eq (Get_String (Gtk_String_Object (Item)), "aa");
      Unref (Item);
      Item := Get_Item (Selection, 4);
      Assert_Cmpstr_Eq (Get_String (Gtk_String_Object (Item)), "ca");
      Unref (Item);

      --  Section notifications from the wrapped model are forwarded too.
      Sections_Position := Guint'Last;
      Sections_Count := Guint'Last;
      Sections_Id := Signal_Connect_Range
        (Get_Object (Selection), Sections_Name,
         On_Range_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
      Interfaces.C.Strings.Free (Sections_Name);
      Sections_Changed (Sorted, 0, 2);
      Assert_True (Sections_Id /= 0);
      Assert_Cmpuint_Eq (Sections_Position, 0);
      Assert_Cmpuint_Eq (Sections_Count, 2);

      Unref (Selection);
      Unref (Sorted);
      Unref (Store);
   end Test_Sections;

   -------------------------
   -- Test_Notifications --
   -------------------------

   procedure Test_Notifications is
      Store : constant Gtk_String_List := New_Store;
      Selection : Gtk_No_Selection;
      Items_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("items-changed");
      Sections_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("sections-changed");
      Selection_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("selection-changed");
      Items_Id, Sections_Id, Selection_Id : Gulong;
   begin
      Ref (Store);
      Selection := Gtk_No_Selection_New (+Store);
      Items_Position := Guint'Last;
      Items_Removed := Guint'Last;
      Items_Added := Guint'Last;
      Sections_Position := Guint'Last;
      Sections_Count := Guint'Last;
      Selection_Position := Guint'Last;
      Selection_Count := Guint'Last;

      --  Observe the GTK signals so each Ada notification helper is checked
      --  for forwarding its exact position and count.
      Items_Id := Signal_Connect_Items
        (Get_Object (Selection), Items_Name,
         On_Items_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
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
      Assert_True (Items_Id /= 0 and Sections_Id /= 0 and Selection_Id /= 0);

      Unref (Selection);
      Unref (Store);
   end Test_Notifications;

   --------------------------
   -- Test_Sort_List_Model --
   --------------------------

   procedure Test_Sort_List_Model is
      Empty : constant GNAT.Strings.String_List (1 .. 0) := (others => null);
      Store : constant Gtk_String_List := Gtk_String_List_New (Empty);
      Other_Store : constant Gtk_String_List := Gtk_String_List_New (Empty);
      Inner_Sorter : constant Gtk_Custom_Sorter :=
        Gtk_Custom_Sorter_New (Compare_Lexical'Unrestricted_Access);
      Group_Sorter : constant Gtk_Custom_Sorter :=
        Gtk_Custom_Sorter_New (Compare_Group'Unrestricted_Access);
      Sorted : Gtk_Sort_List_Model;
      Item : GObject;
      Start, Finish : Guint;
      Items_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("items-changed");
      Sections_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("sections-changed");
      Items_Id, Sections_Id : Gulong;
   begin
      --  Deliberately insert items out of order so both the sorter and the
      --  section boundaries can be checked against meaningful results.
      Append (Store, "ba");
      Append (Store, "aa");
      Append (Store, "ab");
      Append (Store, "bb");
      Append (Store, "ca");

      --  Gtk_New transfers a model reference to the new sort model. Keep a
      --  caller reference as well so both objects can be released below.
      Ref (Store);
      Ref (Inner_Sorter);
      Gtk_New (Sorted, +Store, Inner_Sorter);

      --  Initialize is a no-op for an already-created object. The original
      --  model and sorter remain installed after this second initialization.
      Initialize (Sorted, +Other_Store, null);
      Assert_True (Get_Model (Sorted) = +Store);
      Assert_True (Get_Sorter (Sorted) = Gtk.Sorter.Gtk_Sorter (Inner_Sorter));
      Assert_False (Get_Incremental (Sorted));
      Assert_Cmpuint_Eq (Get_Pending (Sorted), 0);
      Assert_True (Get_Item_Type (Sorted) = Get_Item_Type (Store));
      Assert_Cmpuint_Eq (Get_N_Items (Sorted), 5);

      --  Direct list-model access must return the sorted item with a new
      --  reference, which this test releases after inspecting its string.
      Item := Get_Item (Sorted, 0);
      Assert_True (Item /= null);
      Assert_Cmpstr_Eq (Get_String (Gtk_String_Object (Item)), "aa");
      Unref (Item);
      Assert_True (Get_Item (Sorted, 9) = null);

      --  The section sorter groups by first character while the ordinary
      --  sorter orders strings within each group.
      Assert_True (Get_Section_Sorter (Sorted) = null);
      Set_Section_Sorter (Sorted, Group_Sorter);
      Assert_True
        (Get_Section_Sorter (Sorted) = Gtk.Sorter.Gtk_Sorter (Group_Sorter));
      Get_Section (Sorted, 0, Start, Finish);
      Assert_Cmpuint_Eq (Start, 0);
      Assert_Cmpuint_Eq (Finish, 2);
      Get_Section (Sorted, 4, Start, Finish);
      Assert_Cmpuint_Eq (Start, 4);
      Assert_Cmpuint_Eq (Finish, 5);

      --  Clearing the section sorter and ordinary sorter is observable via
      --  their getters; restoring the ordinary sorter restores lexical order.
      Set_Section_Sorter (Sorted, null);
      Assert_True (Get_Section_Sorter (Sorted) = null);
      Set_Sorter (Sorted, null);
      Assert_True (Get_Sorter (Sorted) = null);
      Item := Get_Item (Sorted, 0);
      Assert_Cmpstr_Eq (Get_String (Gtk_String_Object (Item)), "ba");
      Unref (Item);
      Set_Sorter (Sorted, Inner_Sorter);
      Assert_True (Get_Sorter (Sorted) = Gtk.Sorter.Gtk_Sorter (Inner_Sorter));
      Item := Get_Item (Sorted, 0);
      Assert_Cmpstr_Eq (Get_String (Gtk_String_Object (Item)), "aa");
      Unref (Item);

      --  Replacing the source with an empty list and restoring it exercises
      --  both model transitions and confirms the resulting item counts.
      Set_Model (Sorted, +Other_Store);
      Assert_True (Get_Model (Sorted) = +Other_Store);
      Assert_Cmpuint_Eq (Get_N_Items (Sorted), 0);
      Set_Model (Sorted, +Store);
      Assert_True (Get_Model (Sorted) = +Store);
      Assert_Cmpuint_Eq (Get_N_Items (Sorted), 5);

      --  Incremental mode is a mutable property; disabling it leaves no
      --  pending work for this small, synchronously sorted model.
      Set_Incremental (Sorted, True);
      Assert_True (Get_Incremental (Sorted));
      Set_Incremental (Sorted, False);
      Assert_False (Get_Incremental (Sorted));
      Assert_Cmpuint_Eq (Get_Pending (Sorted), 0);

      --  Connect to the model signals and check that the public notification
      --  wrappers forward their arguments without changing list contents.
      Items_Position := Guint'Last;
      Items_Removed := Guint'Last;
      Items_Added := Guint'Last;
      Sections_Position := Guint'Last;
      Sections_Count := Guint'Last;
      Items_Id := Signal_Connect_Items
        (Get_Object (Sorted), Items_Name,
         On_Items_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
      Sections_Id := Signal_Connect_Range
        (Get_Object (Sorted), Sections_Name,
         On_Range_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
      Interfaces.C.Strings.Free (Items_Name);
      Interfaces.C.Strings.Free (Sections_Name);

      Items_Changed (Sorted, 1, 0, 0);
      Assert_Cmpuint_Eq (Items_Position, 1);
      Assert_Cmpuint_Eq (Items_Removed, 0);
      Assert_Cmpuint_Eq (Items_Added, 0);
      Sections_Changed (Sorted, 0, 2);
      Assert_Cmpuint_Eq (Sections_Position, 0);
      Assert_Cmpuint_Eq (Sections_Count, 2);
      Assert_True (Items_Id /= 0 and Sections_Id /= 0);

      Unref (Sorted);
      Unref (Store);
      Unref (Other_Store);
      Unref (Inner_Sorter);
      Unref (Group_Sorter);
   end Test_Sort_List_Model;

   --  Signal handlers store only values that their matching assertion reads.
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

   Glib.Test.Add_Func ("/noselection/create", Test_Create'Unrestricted_Access);
   Glib.Test.Add_Func ("/noselection/empty", Test_Empty'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/noselection/selection", Test_Selection'Unrestricted_Access);
   Glib.Test.Add_Func ("/noselection/changes", Test_Changes'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/noselection/set-model", Test_Set_Model'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/noselection/sections", Test_Sections'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/noselection/notifications", Test_Notifications'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/noselection/sort-list-model",
      Test_Sort_List_Model'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
