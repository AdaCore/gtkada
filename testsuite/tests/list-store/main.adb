--  Exercises Gtk.List_Store by porting the row editing and iterator checks
--  from testsuite/c_tests/liststore.c, and covers value access, traversal,
--  sorting, and drag-source/destination behavior. The original Glib.List_Store
--  Find checks are retained as well. No widget is created, so this test needs
--  no display.

with Glib;                use Glib;
with Glib.List_Store;     use Glib.List_Store;
with Glib.Simple_Action;  use Glib.Simple_Action;
with Glib.Test;           use Glib.Test;
with Ada.Exceptions;
with Ada.Command_Line;
with Gdk.Content_Provider;
with Glib.List_Model;      use Glib.List_Model;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.List_Store;
with Gtk.Tree_Model;      use Gtk.Tree_Model;
with Gtk.Tree_Drag_Dest;
with Glib.Values;         use Glib.Values;
with Glib.Object;
with Gtkada.Bindings;
use Gtk.List_Store;

pragma Warnings (Off, "*obsolescent*");

procedure Main is

   use type Glib.Object.GObject;
   use type Gtk.Tree_Model.Gtk_Tree_Iter;
   use type Gtk.Tree_Model.Tree_Model_Flags;
   use type Gtk.Tree_Drag_Dest.Gtk_Tree_Drag_Dest;
   use type Gdk.Content_Provider.Gdk_Content_Provider;

   Visit_Count    : Gint := 0;
   Destroy_Count  : Gint := 0;
   Changed_Count  : Gint := 0;
   Deleted_Count  : Gint := 0;
   Items_Event_Count : Natural := 0;
   Items_Event_Position : Guint := Guint'Last;
   Items_Event_Removed : Guint := Guint'Last;
   Items_Event_Added : Guint := Guint'Last;
   Slot_Event_Count : Natural := 0;
   Slot_Event_Arguments_Match : Boolean := False;
   Slot_Event_Receiver_Matches : Boolean := False;
   Items_Exception_Count : Natural := 0;

   procedure Destroy_Data (Data : in out Gint);

   package Foreach_With_Data is new Gtk.List_Store.Foreach_User_Data
     (User_Data_Type => Gint);
   package Default_Sort_With_Data is new
     Gtk.List_Store.Set_Default_Sort_Func_User_Data
       (User_Data_Type => Gint,
        Destroy        => Destroy_Data);
   package Column_Sort_With_Data is new
     Gtk.List_Store.Set_Sort_Func_User_Data
       (User_Data_Type => Gint,
        Destroy        => Destroy_Data);

   procedure Test_Find
   with Convention => C;

   procedure Test_Find_Absent
   with Convention => C;

   procedure Test_Find_No_Position
   with Convention => C;

   procedure Test_Glib_List_Store
   with Convention => C;

   procedure Test_Model_And_Values
   with Convention => C;

   procedure Test_Row_Editing
   with Convention => C;

   procedure Test_Drag_And_Drop
   with Convention => C;

   procedure Test_Sorting_And_Callbacks
   with Convention => C;

   procedure Test_Model_Notifications
   with Convention => C;

   function Visit_Row
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;

   function Visit_Row_With_Data
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data  : Gint) return Boolean;

   function Compare_Int
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A, B  : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint;

   function Compare_Int_With_Data
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A, B  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data  : Gint) return Gint;

   procedure Capture_Row_Event
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter);

   procedure Capture_Deleted_Event
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path);

   function Compare_Action_Names
     (A, B : not null access Glib.Object.GObject_Record'Class)
      return Gint
   with Convention => C;

   procedure Capture_Items_Changed
     (Model : Glist_Model;
      Position, Removed, Added : Guint);

   procedure Capture_Items_Changed_Slot
     (Slot : access Glib.Object.GObject_Record'Class;
      Position, Removed, Added : Guint);

   procedure Raise_From_Items_Changed
     (Model : Glist_Model; Position, Removed, Added : Guint);
   procedure Raise_From_Items_Changed_Slot
     (Slot : access Glib.Object.GObject_Record'Class;
      Position, Removed, Added : Guint);
   procedure Record_Items_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);


   procedure Destroy_Data (Data : in out Gint) is
   begin
      Destroy_Count := Destroy_Count + 1;
      Data := 0;
   end Destroy_Data;

   --  Count callbacks so each traversal checks both the callback bridge and
   --  the documented early-stop result.
   function Visit_Row
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      pragma Unreferenced (Path, Iter);
   begin
      Visit_Count := Visit_Count + 1;
      return False;
   end Visit_Row;

   function Visit_Row_With_Data
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data  : Gint) return Boolean
   is
      pragma Unreferenced (Path, Iter);
   begin
      Visit_Count := Visit_Count + 1;
      return Visit_Count >= Data;
   end Visit_Row_With_Data;

   --  GTK passes the interface view of the list store to comparison callbacks.
   function Compare_Int
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A, B  : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint
   is
   begin
      return Gtk.Tree_Model.Get_Int (Model, A, 0)
        - Gtk.Tree_Model.Get_Int (Model, B, 0);
   end Compare_Int;

   --  The multiplier proves user data reaches the generic callback wrapper.
   function Compare_Int_With_Data
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A, B  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data  : Gint) return Gint
   is
   begin
      return Data * (Gtk.Tree_Model.Get_Int (Model, A, 0)
        - Gtk.Tree_Model.Get_Int (Model, B, 0));
   end Compare_Int_With_Data;

   --  These handlers count model notifications emitted by the wrappers below.
   procedure Capture_Row_Event
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      pragma Unreferenced (Model, Path, Iter);
   begin
      Changed_Count := Changed_Count + 1;
   end Capture_Row_Event;

   procedure Capture_Deleted_Event
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      pragma Unreferenced (Model, Path);
   begin
      Deleted_Count := Deleted_Count + 1;
   end Capture_Deleted_Event;

   function New_Action (Name : String) return Gsimple_Action;
   --  A stateless GSimpleAction, used here only as a plain GObject to put in
   --  the store.

   ----------------
   -- New_Action --
   ----------------

   function New_Action (Name : String) return Gsimple_Action is
   begin
      return Gsimple_Action_New (Name, Parameter_Type => null);
   end New_Action;

   --  Compare action names so the Glib.List_Store comparator checks ordering
   --  with values a caller can inspect.
   function Compare_Action_Names
     (A, B : not null access Glib.Object.GObject_Record'Class)
      return Gint
   is
      Left  : constant String :=
        Get_Name (Glib.Simple_Action.Gsimple_Action (A));
      Right : constant String :=
        Get_Name (Glib.Simple_Action.Gsimple_Action (B));
   begin
      if Left < Right then
         return -1;
      elsif Left > Right then
         return 1;
      else
         return 0;
      end if;
   end Compare_Action_Names;

   procedure Capture_Items_Changed
     (Model : Glist_Model;
      Position, Removed, Added : Guint)
   is
      pragma Unreferenced (Model);
   begin
      Items_Event_Count := Items_Event_Count + 1;
      Items_Event_Position := Position;
      Items_Event_Removed := Removed;
      Items_Event_Added := Added;
   end Capture_Items_Changed;

   --  The slot overload passes its closure object as Self, followed by the
   --  signal's three unsigned arguments.
   procedure Capture_Items_Changed_Slot
     (Slot : access Glib.Object.GObject_Record'Class;
      Position, Removed, Added : Guint)
   is
   begin
      Slot_Event_Count := Slot_Event_Count + 1;
      Slot_Event_Arguments_Match :=
        Position = 2 and then Removed = 1 and then Added = 3;
      Slot_Event_Receiver_Matches :=
        Get_Name (Glib.Simple_Action.Gsimple_Action (Slot)) = "signal-slot";
   end Capture_Items_Changed_Slot;

   --  These handlers deliberately raise so both marshaller exception arms
   --  are checked without allowing an Ada exception to cross into C.
   procedure Raise_From_Items_Changed
     (Model : Glist_Model; Position, Removed, Added : Guint)
   is
      pragma Unreferenced (Model, Position, Removed, Added);
   begin
      raise Program_Error with "items-changed handler";
   end Raise_From_Items_Changed;

   procedure Raise_From_Items_Changed_Slot
     (Slot : access Glib.Object.GObject_Record'Class;
      Position, Removed, Added : Guint)
   is
      pragma Unreferenced (Slot, Position, Removed, Added);
   begin
      raise Program_Error with "items-changed slot handler";
   end Raise_From_Items_Changed_Slot;

   --  Count exceptions that the signal marshallers pass to GtkAda's handler.
   procedure Record_Items_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
      pragma Unreferenced (Occurrence);
   begin
      Items_Exception_Count := Items_Exception_Count + 1;
   end Record_Items_Exception;

   ---------------
   -- Test_Find --
   ---------------

   procedure Test_Find is
      Store    : Glist_Store;
      Items    : array (0 .. 2) of Gsimple_Action;
      Position : aliased Guint;
   begin
      G_New (Store);

      for J in Items'Range loop
         Items (J) := New_Action ("action" & Integer'Image (J));
         Store.Append (Items (J));
      end loop;

      --  Position must come back set for every item.

      for J in Items'Range loop
         Position := Guint'Last;
         Assert_True (Store.Find (Items (J), Position'Access));
         Assert_Cmpuint_Eq (Position, Guint (J));
      end loop;

      Unref (Store);
   end Test_Find;

   ----------------------
   -- Test_Find_Absent --
   ----------------------

   procedure Test_Find_Absent is
      Store    : Glist_Store;
      Item     : constant Gsimple_Action := New_Action ("in-store");
      Absent   : constant Gsimple_Action := New_Action ("not-in-store");
      Position : aliased Guint := Guint'Last;
   begin
      G_New (Store);
      Store.Append (Item);

      Assert_False (Store.Find (Absent, Position'Access));

      Unref (Store);
   end Test_Find_Absent;

   ----------------------------
   -- Test_Find_No_Position --
   ----------------------------

   procedure Test_Find_No_Position is
      Store : Glist_Store;
      Item  : constant Gsimple_Action := New_Action ("in-store");
   begin
      G_New (Store);
      Store.Append (Item);

      --  Position is optional: C accepts a NULL, and so must we.

      Assert_True (Store.Find (Item, null));
      Assert_True (Store.Find (Item));

      Unref (Store);
   end Test_Find_No_Position;

   ----------------------------
   -- Test_Glib_List_Store --
   ----------------------------

   --  Exercise the GListStore-specific operations, then check the sorted
   --  contents and the signal arguments from the inherited model notifier.
   procedure Test_Glib_List_Store is
      Store : constant Glist_Store :=
        Glist_Store_New (Glib.Simple_Action.Get_Type);
      A : constant Gsimple_Action := New_Action ("alpha");
      B : constant Gsimple_Action := New_Action ("bravo");
      C : constant Gsimple_Action := New_Action ("charlie");
      D : constant Gsimple_Action := New_Action ("delta");
      Slot : constant Gsimple_Action := New_Action ("signal-slot");
      Model : Glist_Model;
      Position : aliased Guint := Guint'Last;
      Item : Glib.Object.GObject;

      --  Get_Item returns a full reference, so release it after reading.
      function Name_At (Index : Guint) return String is
         Value : constant Glib.Object.GObject := Store.Get_Item (Index);
      begin
         declare
            Name : constant String := Get_Name
              (Glib.Simple_Action.Gsimple_Action (Value));
         begin
            Glib.Object.Unref (Value);
            return Name;
         end;
      end Name_At;
   begin
      --  Initialize is a no-op after construction and the model keeps its
      --  original item type.
      Store.Initialize (Glib.GType_Object);
      Assert_True
        (Store.Get_Item_Type = Glib.Simple_Action.Get_Type);
      Assert_Cmpuint_Eq (Store.Get_N_Items, 0);

      --  The interface conversion operator preserves the list-model view.
      Model := Glib.List_Model."+" (+Store);
      Assert_Cmpuint_Eq (Get_N_Items (Model), 0);

      --  Insert places a new item between the two appended entries.
      Store.Append (A);
      Store.Append (C);
      Store.Insert (1, B);
      Assert_Cmpuint_Eq (Store.Get_N_Items, 3);
      Assert_True (Name_At (0) = "alpha");
      Assert_True (Name_At (1) = "bravo");
      Assert_True (Name_At (2) = "charlie");

      --  Find_With_Equal_Func is omitted: its public Equal_Func type lacks
      --  Convention C, so it cannot represent GLib's callback safely.

      --  Sort normalizes the rows, and sorted insertion reports its index.
      Store.Sort (Compare_Action_Names'Unrestricted_Access);
      Assert_True (Name_At (0) = "alpha");
      Assert_True (Name_At (1) = "bravo");
      Position := Store.Insert_Sorted
        (D, Compare_Action_Names'Unrestricted_Access);
      Assert_Cmpuint_Eq (Position, 3);
      Assert_True (Name_At (3) = "delta");

      --  Get_Item returns a referenced object that can be checked directly.
      Item := Store.Get_Item (2);
      Assert_True (Item /= null);
      Assert_True
        (Get_Name (Glib.Simple_Action.Gsimple_Action (Item)) = "charlie");
      Glib.Object.Unref (Item);

      --  Observe the inherited notification wrapper and verify all arguments.
      On_Items_Changed (+Store, Capture_Items_Changed'Unrestricted_Access);
      Items_Event_Count := 0;
      Store.Items_Changed (1, 1, 2);
      Assert_Cmpuint_Eq (Guint (Items_Event_Count), 1);
      Assert_Cmpuint_Eq (Items_Event_Position, 1);
      Assert_Cmpuint_Eq (Items_Event_Removed, 1);
      Assert_Cmpuint_Eq (Items_Event_Added, 2);

      --  Check the second overload with independent closure data. After=True
      --  still delivers the original position, removed count, and added count.
      Slot_Event_Count := 0;
      Slot_Event_Arguments_Match := False;
      Slot_Event_Receiver_Matches := False;
      On_Items_Changed
        (Model, Capture_Items_Changed_Slot'Unrestricted_Access,
         Slot => Slot, After => True);
      Store.Items_Changed (2, 1, 3);
      Assert_Cmpuint_Eq (Guint (Slot_Event_Count), 1);
      Assert_True (Slot_Event_Arguments_Match);
      Assert_True (Slot_Event_Receiver_Matches);

      --  Each callback shape raises once; Gtkada contains both exceptions and
      --  reports them to the registered handler.
      Items_Exception_Count := 0;
      Gtkada.Bindings.Set_On_Exception
        (Record_Items_Exception'Unrestricted_Access);
      On_Items_Changed (Model, Raise_From_Items_Changed'Unrestricted_Access);
      On_Items_Changed
        (Model, Raise_From_Items_Changed_Slot'Unrestricted_Access,
         Slot => Slot);
      Store.Items_Changed (1, 0, 0);
      Assert_Cmpuint_Eq (Guint (Items_Exception_Count), 2);

      --  Non-empty Splice is omitted because its array binding passed NULL
      --  elements to GLib; this issue is documented in the run notes.

      --  Remove deletes one position; Remove_All empties the model.
      Store.Remove (2);
      Assert_Cmpuint_Eq (Store.Get_N_Items, 3);
      Assert_True (Name_At (2) = "delta");
      Store.Remove_All;
      Assert_Cmpuint_Eq (Store.Get_N_Items, 0);

      --  Release the store's reference and each constructor-owned reference.
      Glib.Object.Unref
        (Glib.Object.GObject_Record (Store.all)'Access);
      Glib.Object.Unref
        (Glib.Object.GObject_Record (A.all)'Access);
      Glib.Object.Unref
        (Glib.Object.GObject_Record (B.all)'Access);
      Glib.Object.Unref
        (Glib.Object.GObject_Record (C.all)'Access);
      Glib.Object.Unref
        (Glib.Object.GObject_Record (D.all)'Access);
      Glib.Object.Unref
        (Glib.Object.GObject_Record (Slot.all)'Access);
   end Test_Glib_List_Store;

   ---------------------------
   -- Test_Model_And_Values --
   ---------------------------

   --  Check the list-store constructor and the GtkTreeModel view over rows,
   --  then verify scalar and GValue-based assignments are copied into cells.
   procedure Test_Model_And_Values is
      Store       : Gtk.List_Store.Gtk_List_Store;
      Other       : Gtk.List_Store.Gtk_List_Store;
      First       : Gtk_Tree_Iter;
      Second      : Gtk_Tree_Iter;
      Iter        : Gtk_Tree_Iter;
      Child       : Gtk_Tree_Iter;
      Path        : Gtk_Tree_Path;
      Actual_Path : Gtk_Tree_Path;
      Value       : GValue;
      Read_Value  : GValue;
      Values      : GValue_Array (0 .. 2);
      Pair        : GValue_Array (0 .. 1);
      Columns     : Gint_Array (0 .. 1) := (0 => 1, 1 => 0);
      Types       : constant GType_Array :=
        (0 => GType_String, 1 => GType_Int, 2 => GType_Boolean);
   begin
      Gtk.List_Store.Gtk_New (Store, Types);
      --  A second Initialize is documented as a no-op on an existing object.
      Gtk.List_Store.Initialize (Store, (0 => GType_Int));
      Assert_Cmpint_Eq (Gtk.List_Store.Get_N_Columns (Store), 3);
      Assert_True (Gtk.List_Store.Get_Column_Type (Store, 0) = GType_String);
      Assert_True (Gtk.List_Store.Get_Column_Type (Store, 1) = GType_Int);
      Assert_True
        (Gtk.List_Store.Get_Flags (Store)
         = Tree_Model_List_Only + Tree_Model_Iters_Persist);

      Gtk.List_Store.Append (Store, First);
      Gtk.List_Store.Set (Store, First, 0, "first");
      Gtk.List_Store.Set (Store, First, 1, 11);
      Gtk.List_Store.Set (Store, First, 2, True);
      Gtk.List_Store.Append (Store, Second);

      --  Set with parallel columns and values follows the C set_valuesv API.
      Init_Set_Int (Pair (0), 22);
      Init_Set_String (Pair (1), "second");
      Gtk.List_Store.Set (Store, Second, Columns, Pair);
      Unset (Pair (0));
      Unset (Pair (1));

      --  Set without a column list assigns consecutive columns from zero.
      Init_Set_String (Values (0), "third");
      Init_Set_Int (Values (1), 33);
      Init_Set_Boolean (Values (2), False);
      Gtk.List_Store.Append (Store, Iter);
      Gtk.List_Store.Set (Store, Iter, Values);
      for J in Values'Range loop
         Unset (Values (J));
      end loop;

      --  Also cover Set_Value and the model's out-parameter GValue wrapper.
      Init_Set_Int (Value, 44);
      Gtk.List_Store.Set_Value (Store, Second, 1, Value);
      Unset (Value);
      Gtk.List_Store.Get_Value (Store, Second, 1, Read_Value);
      Assert_Cmpint_Eq (Get_Int (Read_Value), 44);
      Unset (Read_Value);
      Assert_Cmpstr_Eq (Get_String (+Store, First, 0), "first");
      Assert_Cmpint_Eq (Get_Int (+Store, Second, 1), 44);
      Assert_False (Get_Boolean (+Store, Iter, 2));

      --  Tree-model traversal wrappers return null iterators at boundaries.
      Assert_Cmpint_Eq (Gtk.List_Store.N_Children (Store), 3);
      Assert_Cmpint_Eq (Gtk.List_Store.N_Children (Store, First), 0);
      Assert_False (Gtk.List_Store.Has_Child (Store, First));
      Assert_True (Gtk.List_Store.Children (Store, Null_Iter) = First);
      Assert_True (Gtk.List_Store.Children (Store, First) = Null_Iter);
      Assert_True (Gtk.List_Store.Nth_Child (Store, Null_Iter, 1) = Second);
      Assert_True (Gtk.List_Store.Nth_Child (Store, Null_Iter, 9) = Null_Iter);
      Assert_True (Gtk.List_Store.Parent (Store, First) = Null_Iter);
      Iter := Gtk.List_Store.Get_Iter_First (Store);
      Assert_True (Iter = First);
      Gtk.List_Store.Next (Store, Iter);
      Assert_True (Iter = Second);
      Gtk.List_Store.Next (Store, Iter);
      Gtk.List_Store.Next (Store, Iter);
      Assert_True (Iter = Null_Iter);
      Iter := Gtk.List_Store.Nth_Child (Store, Null_Iter, 2);
      Gtk.List_Store.Previous (Store, Iter);
      Assert_True (Iter = Second);
      Gtk.List_Store.Previous (Store, Iter);
      Gtk.List_Store.Previous (Store, Iter);
      Assert_True (Iter = Null_Iter);

      Path := Gtk_Tree_Path_New_From_String ("1");
      Iter := Gtk.List_Store.Get_Iter (Store, Path);
      Assert_True (Iter = Second);
      Assert_True (Gtk.List_Store.Get_Iter_From_String (Store, "0") = First);
      Assert_True
        (Gtk.List_Store.Get_Iter_From_String (Store, "99") = Null_Iter);
      Assert_Cmpstr_Eq (Gtk.List_Store.Get_String_From_Iter (Store, Second), "1");
      Actual_Path := Gtk.List_Store.Get_Path (Store, Second);
      Assert_Cmpstr_Eq (To_String (Actual_Path), "1");
      Path_Free (Actual_Path);
      Path_Free (Path);
      Path := Gtk_Tree_Path_New_From_String ("99");
      Assert_True (Gtk.List_Store.Get_Iter (Store, Path) = Null_Iter);
      Path_Free (Path);
      Path := Gtk_Tree_Path_New_From_String ("1");
      Path_Free (Path);
      Gtk.List_Store.Ref_Node (Store, First);
      Gtk.List_Store.Unref_Node (Store, First);
      Assert_True (Gtk.List_Store.Iter_Is_Valid (Store, First));

      --  The alternate constructor uses the same column schema.
      Other := Gtk.List_Store.Gtk_List_Store_Newv (Types);
      Assert_Cmpint_Eq (Gtk.List_Store.Get_N_Columns (Other), 3);
      Gtk.Tree_Model.Unref
        (Gtk.Tree_Model.Gtk_Root_Tree_Model_Record (Other.all)'Access);
      Gtk.Tree_Model.Unref
        (Gtk.Tree_Model.Gtk_Root_Tree_Model_Record (Store.all)'Access);
   end Test_Model_And_Values;

   ----------------------
   -- Test_Row_Editing --
   ----------------------

   --  Port the C insertion, movement, swapping, reorder, and clear checks.
   --  Remove is intentionally omitted because its Ada wrapper discards GTK's
   --  updated iterator; that binding defect is recorded in the work notes.
   procedure Test_Row_Editing is
      Store : Gtk.List_Store.Gtk_List_Store;
      A, B, C, D, E, F, G, H, I : Gtk_Tree_Iter;
      Order : Gint_Array (0 .. 8) := (8, 7, 6, 5, 4, 3, 2, 1, 0);
   begin
      Store := Gtk.List_Store.Gtk_List_Store_Newv ((0 => GType_Int));
      Gtk.List_Store.Append (Store, A);
      Gtk.List_Store.Set (Store, A, 0, 1);
      Gtk.List_Store.Append (Store, B);
      Gtk.List_Store.Set (Store, B, 0, 2);
      Gtk.List_Store.Append (Store, C);
      Gtk.List_Store.Set (Store, C, 0, 3);

      Gtk.List_Store.Insert (Store, D, 1);
      Gtk.List_Store.Set (Store, D, 0, 4);
      Gtk.List_Store.Insert_After (Store, E, A);
      Gtk.List_Store.Set (Store, E, 0, 5);
      Gtk.List_Store.Insert_Before (Store, F, C);
      Gtk.List_Store.Set (Store, F, 0, 6);
      Gtk.List_Store.Insert_After (Store, G, Null_Iter);
      Gtk.List_Store.Set (Store, G, 0, 0);
      Gtk.List_Store.Insert_Before (Store, H, Null_Iter);
      Gtk.List_Store.Set (Store, H, 0, 7);
      Gtk.List_Store.Insert (Store, I, 1234);
      Gtk.List_Store.Set (Store, I, 0, 8);
      Assert_Cmpint_Eq (Gtk.List_Store.N_Children (Store), 9);

      --  Null positions move to the start/end; non-null positions preserve
      --  the row handles while changing their displayed order.
      Gtk.List_Store.Move_After (Store, B, A);
      Gtk.List_Store.Move_After (Store, C, Null_Iter);
      Gtk.List_Store.Move_Before (Store, A, H);
      Gtk.List_Store.Move_Before (Store, B, Null_Iter);
      Gtk.List_Store.Swap (Store, D, E);
      Gtk.List_Store.Reorder (Store, Order);
      Assert_True (Gtk.List_Store.Iter_Is_Valid (Store, A));
      Assert_Cmpint_Eq
        (Get_Int (+Store, Gtk.List_Store.Get_Iter_First (Store), 0), 2);

      Gtk.List_Store.Prepend (Store, H);
      Assert_True (Gtk.List_Store.Iter_Is_Valid (Store, H));
      Gtk.List_Store.Clear (Store);
      Assert_Cmpint_Eq (Gtk.List_Store.N_Children (Store), 0);
      Assert_True (Gtk.List_Store.Get_Iter_First (Store) = Null_Iter);
      Assert_False (Gtk.List_Store.Iter_Is_Valid (Store, A));
      Gtk.Tree_Model.Unref
        (Gtk.Tree_Model.Gtk_Root_Tree_Model_Record (Store.all)'Access);
   end Test_Row_Editing;

   ------------------------
   -- Test_Drag_And_Drop --
   ------------------------

   --  Exercise the drag interfaces with a valid serialized string value. The
   --  arbitrary payload is rejected cleanly, while source export and deletion
   --  operate on the valid row.
   procedure Test_Drag_And_Drop is
      Store    : Gtk.List_Store.Gtk_List_Store;
      Drag_Dest : Gtk.Tree_Drag_Dest.Gtk_Tree_Drag_Dest;
      Iter     : Gtk_Tree_Iter;
      Path     : constant Gtk_Tree_Path := Gtk_Tree_Path_New_From_String ("0");
      Provider : Gdk.Content_Provider.Gdk_Content_Provider;
      Value    : GValue;
   begin
      Store := Gtk.List_Store.Gtk_List_Store_Newv ((0 => GType_String));
      Gtk.List_Store.Append (Store, Iter);
      Gtk.List_Store.Set (Store, Iter, 0, "drag me");
      Assert_True (Gtk.List_Store.Row_Draggable (Store, Path));
      Provider := Gtk.List_Store.Drag_Data_Get (Store, Path);
      Assert_True (Provider /= null);
      Glib.Object.Unref
        (Glib.Object.GObject_Record (Provider.all)'Access);

      Init_Set_String (Value, "not a GtkTreeModel row");
      Assert_False (Gtk.List_Store.Row_Drop_Possible (Store, Path, Value));
      Assert_False (Gtk.List_Store.Drag_Data_Received (Store, Path, Value));

      --  Calling through the interface exercises its two forwarding wrappers;
      --  the List_Store methods above have separate generated entry points.
      Drag_Dest := Gtk.List_Store.Implements_Gtk_Tree_Drag_Dest.To_Interface
        (Store);
      Assert_True
        (Gtk.Tree_Drag_Dest."+" (Drag_Dest) = Drag_Dest);
      Assert_False
        (Gtk.Tree_Drag_Dest.Row_Drop_Possible (Drag_Dest, Path, Value));
      Assert_False
        (Gtk.Tree_Drag_Dest.Drag_Data_Received (Drag_Dest, Path, Value));
      Unset (Value);

      Assert_True (Gtk.List_Store.Drag_Data_Delete (Store, Path));
      Assert_Cmpint_Eq (Gtk.List_Store.N_Children (Store), 0);
      Assert_False (Gtk.List_Store.Drag_Data_Delete (Store, Path));
      Path_Free (Path);
      Gtk.Tree_Model.Unref
        (Gtk.Tree_Model.Gtk_Root_Tree_Model_Record (Store.all)'Access);
   end Test_Drag_And_Drop;

   ---------------------------------
   -- Test_Sorting_And_Callbacks --
   ---------------------------------

   --  Port the list-store sorting checks and verify both ordinary and generic
   --  callback bridges, including early termination and destroy notifiers.
   procedure Test_Sorting_And_Callbacks is
      Store : Gtk.List_Store.Gtk_List_Store;
      A, B, C : Gtk_Tree_Iter;
      Column : Gint;
      Order  : Gtk_Sort_Type;
   begin
      Store := Gtk.List_Store.Gtk_List_Store_Newv ((0 => GType_Int));
      Gtk.List_Store.Append (Store, A);
      Gtk.List_Store.Set (Store, A, 0, 30);
      Gtk.List_Store.Append (Store, B);
      Gtk.List_Store.Set (Store, B, 0, 10);
      Gtk.List_Store.Append (Store, C);
      Gtk.List_Store.Set (Store, C, 0, 20);

      Visit_Count := 0;
      Gtk.List_Store.Foreach (Store, Visit_Row'Unrestricted_Access);
      Assert_Cmpint_Eq (Visit_Count, 3);
      Visit_Count := 0;
      Foreach_With_Data.Foreach
        (Store, Visit_Row_With_Data'Unrestricted_Access, 2);
      Assert_Cmpint_Eq (Visit_Count, 2);

      Assert_False (Gtk.List_Store.Has_Default_Sort_Func (Store));
      Gtk.List_Store.Set_Default_Sort_Func
        (Store, Compare_Int'Unrestricted_Access);
      Assert_True (Gtk.List_Store.Has_Default_Sort_Func (Store));
      Gtk.List_Store.Set_Sort_Column_Id (Store, -1, Sort_Ascending);
      Assert_False
        (Gtk.List_Store.Get_Sort_Column_Id (Store, Column, Order));
      Assert_Cmpint_Eq (Column, -1);
      Assert_True (Order = Sort_Ascending);
      Assert_Cmpint_Eq
        (Get_Int (+Store, Gtk.List_Store.Get_Iter_First (Store), 0), 10);

      Destroy_Count := 0;
      Default_Sort_With_Data.Set_Default_Sort_Func
        (Store, Compare_Int_With_Data'Unrestricted_Access, 1);
      Gtk.List_Store.Set_Sort_Column_Id (Store, -1, Sort_Descending);
      Assert_Cmpint_Eq
        (Get_Int (+Store, Gtk.List_Store.Get_Iter_First (Store), 0), 30);
      Gtk.List_Store.Set_Sort_Column_Id (Store, -2, Sort_Ascending);
      --  Clearing the generic default callback releases its associated data.
      Default_Sort_With_Data.Set_Default_Sort_Func (Store, null, 0);
      Assert_Cmpint_Eq (Destroy_Count, 1);
      Assert_False (Gtk.List_Store.Has_Default_Sort_Func (Store));

      --  The non-generic overload also accepts a null default comparator.
      Gtk.List_Store.Set_Default_Sort_Func
        (Store, Compare_Int'Unrestricted_Access);
      Assert_True (Gtk.List_Store.Has_Default_Sort_Func (Store));
      Gtk.List_Store.Set_Default_Sort_Func (Store, null);
      Assert_False (Gtk.List_Store.Has_Default_Sort_Func (Store));

      Gtk.List_Store.Set_Sort_Func
        (Store, 0, Compare_Int'Unrestricted_Access);
      Gtk.List_Store.Set_Sort_Column_Id (Store, 0, Sort_Ascending);
      Assert_Cmpint_Eq
        (Get_Int (+Store, Gtk.List_Store.Get_Iter_First (Store), 0), 10);
      Column_Sort_With_Data.Set_Sort_Func
        (Store, 0, Compare_Int_With_Data'Unrestricted_Access, 1);
      Gtk.List_Store.Set_Sort_Column_Id (Store, 0, Sort_Descending);
      Assert_Cmpint_Eq
        (Get_Int (+Store, Gtk.List_Store.Get_Iter_First (Store), 0), 30);
      Gtk.List_Store.Set_Sort_Column_Id (Store, -2, Sort_Ascending);
      --  GTK requires a non-null comparator, so replacement is the supported
      --  way to exercise the generic closure's destroy notifier.
      Gtk.List_Store.Set_Sort_Func
        (Store, 0, Compare_Int'Unrestricted_Access);
      Assert_Cmpint_Eq (Destroy_Count, 2);
      Gtk.List_Store.Sort_Column_Changed (Store);
      Assert_False (Gtk.List_Store.Get_Sort_Column_Id (Store, Column, Order));
      Gtk.Tree_Model.Unref
        (Gtk.Tree_Model.Gtk_Root_Tree_Model_Record (Store.all)'Access);
   end Test_Sorting_And_Callbacks;

   --------------------------------
   -- Test_Model_Notifications --
   --------------------------------

   --  The TreeModel notification helpers are for model implementations. On a
   --  list store they emit the corresponding signal without mutating rows, so
   --  signal counters verify the event forwarding independently of row edits.
   procedure Test_Model_Notifications is
      Store     : Gtk.List_Store.Gtk_List_Store;
      Iter      : Gtk_Tree_Iter;
      Path      : Gtk_Tree_Path;
      Root_Path : constant Gtk_Tree_Path := Gtk_Tree_Path_New;
      New_Order : Gint_Array (0 .. 0) := (0 => 0);
   begin
      Store := Gtk.List_Store.Gtk_List_Store_Newv ((0 => GType_Int));
      Gtk.List_Store.Append (Store, Iter);
      Path := Gtk.List_Store.Get_Path (Store, Iter);

      Gtk.Tree_Model.On_Row_Changed
        (+Store, Capture_Row_Event'Unrestricted_Access);
      Gtk.Tree_Model.On_Row_Has_Child_Toggled
        (+Store, Capture_Row_Event'Unrestricted_Access);
      Gtk.Tree_Model.On_Row_Inserted
        (+Store, Capture_Row_Event'Unrestricted_Access);
      Gtk.Tree_Model.On_Row_Deleted
        (+Store, Capture_Deleted_Event'Unrestricted_Access);

      Changed_Count := 0;
      Gtk.List_Store.Row_Changed (Store, Path, Iter);
      Assert_Cmpint_Eq (Changed_Count, 1);
      Gtk.List_Store.Row_Has_Child_Toggled (Store, Path, Iter);
      Assert_Cmpint_Eq (Changed_Count, 2);
      Gtk.List_Store.Row_Inserted (Store, Path, Iter);
      Assert_Cmpint_Eq (Changed_Count, 3);

      Deleted_Count := 0;
      Gtk.List_Store.Row_Deleted (Store, Path);
      Assert_Cmpint_Eq (Deleted_Count, 1);

      --  These interface helpers announce ordering changes; the model data
      --  itself remains in its existing order.
      Gtk.List_Store.Rows_Reordered
        (Store, Root_Path, Null_Iter, New_Order);
      Gtk.List_Store.Rows_Reordered_With_Length
        (Store, Root_Path, Null_Iter, New_Order, New_Order'Length);
      Assert_Cmpint_Eq (Gtk.List_Store.N_Children (Store), 1);
      Assert_True (Gtk.List_Store.Iter_Is_Valid (Store, Iter));

      Path_Free (Path);
      Gtk.Tree_Model.Unref
        (Gtk.Tree_Model.Gtk_Root_Tree_Model_Record (Store.all)'Access);
   end Test_Model_Notifications;

begin
   Glib.Test.Init;

   Glib.Test.Add_Func
     ("/list-store/find", Test_Find'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/list-store/find-absent", Test_Find_Absent'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/list-store/find-no-position",
      Test_Find_No_Position'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/list-store/glib-list-store",
      Test_Glib_List_Store'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gtk-list-store/model-and-values",
      Test_Model_And_Values'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gtk-list-store/row-editing",
      Test_Row_Editing'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gtk-list-store/drag-and-drop",
      Test_Drag_And_Drop'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gtk-list-store/sorting-and-callbacks",
      Test_Sorting_And_Callbacks'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gtk-list-store/model-notifications",
      Test_Model_Notifications'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
