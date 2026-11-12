--  Ada port of GTK's testsuite/gtk/treestore.c (GTK 4.22.2).
--
--  Gtk.Tree_Store is a pure data model, so the whole suite runs without a
--  realized window: it exercises insertion, value setting, removal, reorder,
--  swap, move and iter-invalidation on the model, plus the child-level
--  bug-77977 regression. This is a representative subset of the C suite, as
--  agreed on the work item.
--
--  The C subprocess test bug-698396 (which traps a g_critical from reordering
--  an empty store) is intentionally omitted: reproducing subprocess/critical
--  trapping in Ada is fragile and out of step with the other ported tests,
--  mirroring the deliberate omission of the realized-window case in the
--  tree-view port.
--
--  The whole Gtk.Tree_Store API is marked obsolescent (deprecated since
--  GTK 4.10), so the obsolescent warnings are suppressed locally for this
--  unit, as requested by the work item.

pragma Warnings (Off, "*obsolescent*");

with Ada.Command_Line;
with System;

with Glib;           use Glib;
with Glib.Object;
with Glib.Simple_Action;
with Glib.Test;      use Glib.Test;
with Glib.Values;    use Glib.Values;

with Gdk.Content_Provider;
with Gtk.Enums;      use Gtk.Enums;
with Gtk.Main;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Store; use Gtk.Tree_Store;

procedure Main is

   use type Gtk.Tree_Model.Gtk_Tree_Iter;
   use type Gtk.Tree_Model.Tree_Model_Flags;
   use type Glib.C_Proxy;
   use type Gdk.Content_Provider.Gdk_Content_Provider;
   use type Glib.Object.GObject;
   use type System.Address;

   Visit_Count   : Gint := 0;
   Destroy_Count : Gint := 0;
   Changed_Count : Gint := 0;
   Deleted_Count : Gint := 0;
   Reordered_Count : Gint := 0;
   Slot_Event_Count : Gint := 0;
   Slot_Reordered_Count : Gint := 0;

   type Pointer_Data is record
      Value : Gint;
   end record;

   package Pointer_Column is new Gtk.Tree_Store.Generic_Set
     (Data_Type => Pointer_Data);
   use type Pointer_Column.Data_Type_Access;

   procedure Destroy_Data (Data : in out Gint);

   package Foreach_With_Data is new Gtk.Tree_Store.Foreach_User_Data
     (User_Data_Type => Gint);
   package Model_Foreach_With_Data is new Gtk.Tree_Model.Foreach_User_Data
     (User_Data_Type => Gint);
   package Default_Sort_With_Data is new
     Gtk.Tree_Store.Set_Default_Sort_Func_User_Data
       (User_Data_Type => Gint,
        Destroy        => Destroy_Data);
   package Column_Sort_With_Data is new
     Gtk.Tree_Store.Set_Sort_Func_User_Data
       (User_Data_Type => Gint,
        Destroy        => Destroy_Data);

   --  The fixture: five toplevel rows whose iters are saved so reorder/move
   --  tests can assert iter persistence.
   type Iter_Array is array (0 .. 4) of Gtk_Tree_Iter;

   --  A new_order mapping new position -> original position, as in C
   --  check_model. -1 marks a position to skip (a removed row).
   type Order_Array is array (0 .. 4) of Integer;

   function Iter_Position
     (Store : Gtk_Tree_Store;
      Iter  : Gtk_Tree_Iter;
      N     : Gint) return Boolean;

   procedure Setup (Store : out Gtk_Tree_Store; Iters : out Iter_Array);

   procedure Check_Model
     (Store     : Gtk_Tree_Store;
      Iters     : Iter_Array;
      New_Order : Order_Array;
      Skip      : Integer);

   -------------------
   -- Iter_Position --
   -------------------

   --  Analogue of C iter_position: the path of Iter must start with index N.

   function Iter_Position
     (Store : Gtk_Tree_Store;
      Iter  : Gtk_Tree_Iter;
      N     : Gint) return Boolean
   is
      Path   : Gtk_Tree_Path := Get_Path (+Store, Iter);
      Result : Boolean;
   begin
      if Path = Null_Gtk_Tree_Path then
         return False;
      end if;

      declare
         Indices : constant Gint_Array := Get_Indices (Path);
      begin
         Result := Indices (Indices'First) = N;
      end;

      Path_Free (Path);
      return Result;
   end Iter_Position;

   -----------
   -- Setup --
   -----------

   --  Analogue of C tree_store_setup: a 1-column G_TYPE_INT store with five
   --  toplevel rows, each holding its own index, with iters saved in Iters.

   procedure Setup (Store : out Gtk_Tree_Store; Iters : out Iter_Array) is
   begin
      Gtk_New (Store, (0 => GType_Int));

      for I in Iters'Range loop
         Store.Insert (Iters (I), Null_Iter, Gint (I));
         Store.Set (Iters (I), 0, Gint (I));
      end loop;
   end Setup;

   -----------------
   -- Check_Model --
   -----------------

   --  Analogue of C check_model: walking the toplevel, the iter at position i
   --  must equal the saved iter New_Order (i), skipping position Skip (the
   --  skipped position does not consume a toplevel slot, exactly as in C).

   procedure Check_Model
     (Store     : Gtk_Tree_Store;
      Iters     : Iter_Array;
      New_Order : Order_Array;
      Skip      : Integer)
   is
      Pos  : Gint := 0;
      Iter : Gtk_Tree_Iter;
   begin
      for I in New_Order'Range loop
         if I /= Skip then
            Iter := Nth_Child (+Store, Null_Iter, Pos);
            Assert_True (Store.Iter_Is_Valid (Iter));
            Assert_True (Iter = Iters (New_Order (I)));
            Pos := Pos + 1;
         end if;
      end loop;
   end Check_Model;

   ----------------------------------------------------------------------
   --  Insertion                                                       --
   ----------------------------------------------------------------------

   procedure Test_Insert_High_Values
   with Convention => C;

   procedure Test_Append
   with Convention => C;

   procedure Test_Prepend
   with Convention => C;

   procedure Test_Insert_After
   with Convention => C;

   procedure Test_Insert_After_Null
   with Convention => C;

   procedure Test_Insert_Before
   with Convention => C;

   procedure Test_Insert_Before_Null
   with Convention => C;

   procedure Test_Set_Value
   with Convention => C;

   procedure Test_Remove_Begin
   with Convention => C;

   procedure Test_Remove_Middle
   with Convention => C;

   procedure Test_Remove_End
   with Convention => C;

   procedure Test_Clear
   with Convention => C;

   procedure Test_Reorder
   with Convention => C;

   procedure Test_Swap_Begin
   with Convention => C;

   procedure Test_Swap_Single
   with Convention => C;

   procedure Test_Move_After_From_Start
   with Convention => C;

   procedure Test_Move_After_Null
   with Convention => C;

   procedure Test_Move_Before_To_Start
   with Convention => C;

   procedure Test_Move_Before_Null
   with Convention => C;

   procedure Test_Iter_Prev_Invalid
   with Convention => C;

   procedure Test_Iter_Next_Invalid
   with Convention => C;

   procedure Test_Iter_Children_Invalid
   with Convention => C;

   procedure Test_Iter_Nth_Child_Invalid
   with Convention => C;

   procedure Test_Iter_Parent_Invalid
   with Convention => C;

   procedure Test_Bug_77977
   with Convention => C;

   procedure Test_Model_And_Values
   with Convention => C;

   procedure Test_Pointer_Columns
   with Convention => C;

   procedure Test_Foreach_And_Sorting
   with Convention => C;

   procedure Test_Drag_And_Drop
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

   function Visit_Model_Row_With_Data
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

   procedure Capture_Reordered_Event
     (Model     : Gtk.Tree_Model.Gtk_Tree_Model;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
      New_Order : System.Address);

   procedure Capture_Slot_Row_Event
     (Self : access Glib.Object.GObject_Record'Class;
      Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter);

   procedure Capture_Slot_Deleted_Event
     (Self : access Glib.Object.GObject_Record'Class;
      Path : Gtk.Tree_Model.Gtk_Tree_Path);

   procedure Capture_Slot_Reordered_Event
     (Self      : access Glib.Object.GObject_Record'Class;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
      New_Order : System.Address);

   ------------------
   -- Destroy_Data --
   ------------------

   --  The generic sortable closures own their data until replaced or cleared.
   procedure Destroy_Data (Data : in out Gint) is
   begin
      Destroy_Count := Destroy_Count + 1;
      Data := 0;
   end Destroy_Data;

   ---------------
   -- Visit_Row --
   ---------------

   --  Count all rows visited by the non-generic tree-model callback bridge.
   function Visit_Row
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      pragma Unreferenced (Model, Path, Iter);
   begin
      Visit_Count := Visit_Count + 1;
      return False;
   end Visit_Row;

   ------------------------
   -- Visit_Row_With_Data --
   ------------------------

   --  The requested visit count also checks that generic callback data arrives.
   function Visit_Row_With_Data
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data  : Gint) return Boolean
   is
      pragma Unreferenced (Model, Path, Iter);
   begin
      Visit_Count := Visit_Count + 1;
      return Visit_Count >= Data;
   end Visit_Row_With_Data;

   ----------------------------------
   -- Visit_Model_Row_With_Data --
   ----------------------------------

   --  This callback has the Tree_Model generic's own access type, so it tests
   --  that package's user-data closure independently of the store wrappers.
   function Visit_Model_Row_With_Data
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data  : Gint) return Boolean
   is
      pragma Unreferenced (Model, Path, Iter);
   begin
      Visit_Count := Visit_Count + 1;
      return Visit_Count >= Data;
   end Visit_Model_Row_With_Data;

   -----------------
   -- Compare_Int --
   -----------------

   --  Read model values through the interface passed by GTK to the callback.
   function Compare_Int
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A, B  : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint
   is
   begin
      return Gtk.Tree_Model.Get_Int (Model, A, 1)
        - Gtk.Tree_Model.Get_Int (Model, B, 1);
   end Compare_Int;

   --------------------------
   -- Compare_Int_With_Data --
   --------------------------

   --  A negative multiplier reverses order and proves generic user data flows.
   function Compare_Int_With_Data
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A, B  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data  : Gint) return Gint
   is
   begin
      return Data * (Gtk.Tree_Model.Get_Int (Model, A, 1)
        - Gtk.Tree_Model.Get_Int (Model, B, 1));
   end Compare_Int_With_Data;

   -----------------------
   -- Capture_Row_Event --
   -----------------------

   --  Count row signals raised by the TreeModel convenience procedures.
   procedure Capture_Row_Event
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      pragma Unreferenced (Model, Path, Iter);
   begin
      Changed_Count := Changed_Count + 1;
   end Capture_Row_Event;

   --------------------------
   -- Capture_Deleted_Event --
   --------------------------

   procedure Capture_Deleted_Event
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      pragma Unreferenced (Model, Path);
   begin
      Deleted_Count := Deleted_Count + 1;
   end Capture_Deleted_Event;

   ----------------------------
   -- Capture_Reordered_Event --
   ----------------------------

   procedure Capture_Reordered_Event
     (Model     : Gtk.Tree_Model.Gtk_Tree_Model;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
      New_Order : System.Address)
   is
      pragma Unreferenced (Model, Path, Iter, New_Order);
   begin
      Reordered_Count := Reordered_Count + 1;
   end Capture_Reordered_Event;

   ---------------------------
   -- Capture_Slot_Row_Event --
   ---------------------------

   --  Slot callbacks receive the connected GObject as their first parameter.
   procedure Capture_Slot_Row_Event
     (Self : access Glib.Object.GObject_Record'Class;
      Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      pragma Unreferenced (Self, Path, Iter);
   begin
      Slot_Event_Count := Slot_Event_Count + 1;
   end Capture_Slot_Row_Event;

   -------------------------------
   -- Capture_Slot_Deleted_Event --
   -------------------------------

   procedure Capture_Slot_Deleted_Event
     (Self : access Glib.Object.GObject_Record'Class;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      pragma Unreferenced (Self, Path);
   begin
      Slot_Event_Count := Slot_Event_Count + 1;
   end Capture_Slot_Deleted_Event;

   ---------------------------------
   -- Capture_Slot_Reordered_Event --
   ---------------------------------

   procedure Capture_Slot_Reordered_Event
     (Self      : access Glib.Object.GObject_Record'Class;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
      New_Order : System.Address)
   is
      pragma Unreferenced (Self, Path, Iter, New_Order);
   begin
      Slot_Reordered_Count := Slot_Reordered_Count + 1;
   end Capture_Slot_Reordered_Event;

   -----------------------------
   -- Test_Insert_High_Values --
   -----------------------------

   procedure Test_Insert_High_Values is
      Store     : Gtk_Tree_Store;
      Iter      : Gtk_Tree_Iter;
      Iter2     : Gtk_Tree_Iter;
      Iter_Copy : Gtk_Tree_Iter;
   begin
      Gtk_New (Store, (0 => GType_Int));

      Store.Insert (Iter, Null_Iter, 1234);
      Assert_True (Store.Iter_Is_Valid (Iter));
      Assert_Cmpint_Eq (N_Children (+Store), 1);
      Iter_Copy := Get_Iter_First (+Store);
      Assert_True (Iter_Copy /= Null_Iter);
      Assert_True (Iter = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter, 0));

      Store.Insert (Iter2, Null_Iter, 765);
      Assert_True (Store.Iter_Is_Valid (Iter2));
      Assert_Cmpint_Eq (N_Children (+Store), 2);

      --  Walk over the model.
      Iter_Copy := Get_Iter_First (+Store);
      Assert_True (Iter = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter, 0));

      Next (+Store, Iter_Copy);
      Assert_True (Iter_Copy /= Null_Iter);
      Assert_True (Iter2 = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter2, 1));

      Next (+Store, Iter_Copy);
      Assert_True (Iter_Copy = Null_Iter);

      Iter_Copy := Nth_Child (+Store, Null_Iter, 1);
      Assert_True (Iter2 = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter2, 1));

      Previous (+Store, Iter_Copy);
      Assert_True (Iter_Copy /= Null_Iter);
      Assert_True (Iter = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter, 0));

      Previous (+Store, Iter_Copy);
      Assert_True (Iter_Copy = Null_Iter);
   end Test_Insert_High_Values;

   -----------------
   -- Test_Append --
   -----------------

   procedure Test_Append is
      Store     : Gtk_Tree_Store;
      Iter      : Gtk_Tree_Iter;
      Iter2     : Gtk_Tree_Iter;
      Iter_Copy : Gtk_Tree_Iter;
   begin
      Gtk_New (Store, (0 => GType_Int));

      Store.Append (Iter, Null_Iter);
      Assert_True (Store.Iter_Is_Valid (Iter));
      Assert_Cmpint_Eq (N_Children (+Store), 1);
      Iter_Copy := Get_Iter_First (+Store);
      Assert_True (Iter = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter, 0));

      Store.Append (Iter2, Null_Iter);
      Assert_True (Store.Iter_Is_Valid (Iter2));
      Assert_Cmpint_Eq (N_Children (+Store), 2);

      --  Walk over the model.
      Iter_Copy := Get_Iter_First (+Store);
      Assert_True (Iter = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter, 0));

      Next (+Store, Iter_Copy);
      Assert_True (Iter_Copy /= Null_Iter);
      Assert_True (Iter2 = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter2, 1));

      Next (+Store, Iter_Copy);
      Assert_True (Iter_Copy = Null_Iter);

      Iter_Copy := Nth_Child (+Store, Null_Iter, 1);
      Assert_True (Iter2 = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter2, 1));

      Previous (+Store, Iter_Copy);
      Assert_True (Iter_Copy /= Null_Iter);
      Assert_True (Iter = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter, 0));

      Previous (+Store, Iter_Copy);
      Assert_True (Iter_Copy = Null_Iter);
   end Test_Append;

   ------------------
   -- Test_Prepend --
   ------------------

   procedure Test_Prepend is
      Store     : Gtk_Tree_Store;
      Iter      : Gtk_Tree_Iter;
      Iter2     : Gtk_Tree_Iter;
      Iter_Copy : Gtk_Tree_Iter;
   begin
      Gtk_New (Store, (0 => GType_Int));

      Store.Prepend (Iter, Null_Iter);
      Assert_True (Store.Iter_Is_Valid (Iter));
      Assert_Cmpint_Eq (N_Children (+Store), 1);
      Iter_Copy := Get_Iter_First (+Store);
      Assert_True (Iter = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter, 0));

      Store.Prepend (Iter2, Null_Iter);
      Assert_True (Store.Iter_Is_Valid (Iter2));
      Assert_Cmpint_Eq (N_Children (+Store), 2);

      --  Walk over the model: the prepended Iter2 now comes first.
      Iter_Copy := Get_Iter_First (+Store);
      Assert_True (Iter2 = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter2, 0));

      Next (+Store, Iter_Copy);
      Assert_True (Iter_Copy /= Null_Iter);
      Assert_True (Iter = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter, 1));

      Next (+Store, Iter_Copy);
      Assert_True (Iter_Copy = Null_Iter);

      Iter_Copy := Nth_Child (+Store, Null_Iter, 1);
      Assert_True (Iter = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter, 1));

      Previous (+Store, Iter_Copy);
      Assert_True (Iter_Copy /= Null_Iter);
      Assert_True (Iter2 = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter2, 0));

      Previous (+Store, Iter_Copy);
      Assert_True (Iter_Copy = Null_Iter);
   end Test_Prepend;

   -----------------------
   -- Test_Insert_After --
   -----------------------

   procedure Test_Insert_After is
      Store     : Gtk_Tree_Store;
      Iter      : Gtk_Tree_Iter;
      Iter2     : Gtk_Tree_Iter;
      Iter3     : Gtk_Tree_Iter;
      Iter_Copy : Gtk_Tree_Iter;
   begin
      Gtk_New (Store, (0 => GType_Int));

      Store.Append (Iter, Null_Iter);
      Store.Append (Iter2, Null_Iter);

      Store.Insert_After (Iter3, Null_Iter, Iter);
      Assert_True (Store.Iter_Is_Valid (Iter3));
      Assert_Cmpint_Eq (N_Children (+Store), 3);
      Iter_Copy := Nth_Child (+Store, Null_Iter, 1);
      Assert_True (Iter3 = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter3, 1));

      --  Walk over the model: order is Iter, Iter3, Iter2.
      Iter_Copy := Get_Iter_First (+Store);
      Assert_True (Iter = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter_Copy, 0));

      Next (+Store, Iter_Copy);
      Assert_True (Iter_Copy /= Null_Iter);
      Assert_True (Iter3 = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter_Copy, 1));

      Next (+Store, Iter_Copy);
      Assert_True (Iter_Copy /= Null_Iter);
      Assert_True (Iter2 = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter_Copy, 2));

      Next (+Store, Iter_Copy);
      Assert_True (Iter_Copy = Null_Iter);
   end Test_Insert_After;

   ----------------------------
   -- Test_Insert_After_Null --
   ----------------------------

   procedure Test_Insert_After_Null is
      Store     : Gtk_Tree_Store;
      Iter      : Gtk_Tree_Iter;
      Iter2     : Gtk_Tree_Iter;
      Iter_Copy : Gtk_Tree_Iter;
   begin
      Gtk_New (Store, (0 => GType_Int));

      Store.Append (Iter, Null_Iter);

      --  insert_after with a null sibling is basically a prepend.
      Store.Insert_After (Iter2, Null_Iter, Null_Iter);
      Assert_True (Store.Iter_Is_Valid (Iter2));
      Assert_Cmpint_Eq (N_Children (+Store), 2);

      Iter_Copy := Get_Iter_First (+Store);
      Assert_True (Iter2 = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter2, 0));

      Next (+Store, Iter_Copy);
      Assert_True (Iter_Copy /= Null_Iter);
      Assert_True (Iter = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter, 1));

      Next (+Store, Iter_Copy);
      Assert_True (Iter_Copy = Null_Iter);
   end Test_Insert_After_Null;

   ------------------------
   -- Test_Insert_Before --
   ------------------------

   procedure Test_Insert_Before is
      Store     : Gtk_Tree_Store;
      Iter      : Gtk_Tree_Iter;
      Iter2     : Gtk_Tree_Iter;
      Iter3     : Gtk_Tree_Iter;
      Iter_Copy : Gtk_Tree_Iter;
   begin
      Gtk_New (Store, (0 => GType_Int));

      Store.Append (Iter, Null_Iter);
      Store.Append (Iter2, Null_Iter);

      Store.Insert_Before (Iter3, Null_Iter, Iter2);
      Assert_True (Store.Iter_Is_Valid (Iter3));
      Assert_Cmpint_Eq (N_Children (+Store), 3);
      Iter_Copy := Nth_Child (+Store, Null_Iter, 1);
      Assert_True (Iter3 = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter3, 1));

      --  Walk over the model: order is Iter, Iter3, Iter2.
      Iter_Copy := Get_Iter_First (+Store);
      Assert_True (Iter = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter_Copy, 0));

      Next (+Store, Iter_Copy);
      Assert_True (Iter_Copy /= Null_Iter);
      Assert_True (Iter3 = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter_Copy, 1));

      Next (+Store, Iter_Copy);
      Assert_True (Iter_Copy /= Null_Iter);
      Assert_True (Iter2 = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter_Copy, 2));

      Next (+Store, Iter_Copy);
      Assert_True (Iter_Copy = Null_Iter);
   end Test_Insert_Before;

   -----------------------------
   -- Test_Insert_Before_Null --
   -----------------------------

   procedure Test_Insert_Before_Null is
      Store     : Gtk_Tree_Store;
      Iter      : Gtk_Tree_Iter;
      Iter2     : Gtk_Tree_Iter;
      Iter_Copy : Gtk_Tree_Iter;
   begin
      Gtk_New (Store, (0 => GType_Int));

      Store.Append (Iter, Null_Iter);

      --  insert_before with a null sibling is basically an append.
      Store.Insert_Before (Iter2, Null_Iter, Null_Iter);
      Assert_True (Store.Iter_Is_Valid (Iter2));
      Assert_Cmpint_Eq (N_Children (+Store), 2);

      Iter_Copy := Get_Iter_First (+Store);
      Assert_True (Iter = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter, 0));

      Next (+Store, Iter_Copy);
      Assert_True (Iter_Copy /= Null_Iter);
      Assert_True (Iter2 = Iter_Copy);
      Assert_True (Iter_Position (Store, Iter2, 1));

      Next (+Store, Iter_Copy);
      Assert_True (Iter_Copy = Null_Iter);
   end Test_Insert_Before_Null;

   --------------------
   -- Test_Set_Value --
   --------------------

   --  Clean Ada analogue of C set-gvalue-to-transform: set an Gint and read
   --  it back through the Gtk.Tree_Model Get_Int accessor.

   procedure Test_Set_Value is
      Store : Gtk_Tree_Store;
      Iter  : Gtk_Tree_Iter;
   begin
      Gtk_New (Store, (0 => GType_Int));
      Store.Append (Iter, Null_Iter);
      Store.Set (Iter, 0, Gint (42));
      Assert_Cmpint_Eq (Get_Int (+Store, Iter, 0), 42);
   end Test_Set_Value;

   -----------------------
   -- Test_Remove_Begin --
   -----------------------

   procedure Test_Remove_Begin is
      Store : Gtk_Tree_Store;
      Iters : Iter_Array;
      Iter  : Gtk_Tree_Iter;
   begin
      Setup (Store, Iters);

      --  Remove node at 0; Iter is set to the next valid row.
      Iter := Nth_Child (+Store, Null_Iter, 0);
      Store.Remove (Iter);
      Assert_False (Store.Iter_Is_Valid (Iters (0)));
      Assert_True (Iter = Iters (1));

      Check_Model (Store, Iters, (-1, 1, 2, 3, 4), 0);
   end Test_Remove_Begin;

   ------------------------
   -- Test_Remove_Middle --
   ------------------------

   procedure Test_Remove_Middle is
      Store : Gtk_Tree_Store;
      Iters : Iter_Array;
      Iter  : Gtk_Tree_Iter;
   begin
      Setup (Store, Iters);

      --  Remove node at 2.
      Iter := Nth_Child (+Store, Null_Iter, 2);
      Store.Remove (Iter);
      Assert_False (Store.Iter_Is_Valid (Iters (2)));
      Assert_True (Iter = Iters (3));

      Check_Model (Store, Iters, (0, 1, -1, 3, 4), 2);
   end Test_Remove_Middle;

   ---------------------
   -- Test_Remove_End --
   ---------------------

   procedure Test_Remove_End is
      Store : Gtk_Tree_Store;
      Iters : Iter_Array;
      Iter  : Gtk_Tree_Iter;
   begin
      Setup (Store, Iters);

      --  Remove node at 4; there is no next row, so Iter is invalidated.
      Iter := Nth_Child (+Store, Null_Iter, 4);
      Store.Remove (Iter);
      Assert_True (Iter = Null_Iter);
      Assert_False (Store.Iter_Is_Valid (Iters (4)));

      Check_Model (Store, Iters, (0, 1, 2, 3, -1), 4);
   end Test_Remove_End;

   ----------------
   -- Test_Clear --
   ----------------

   procedure Test_Clear is
      Store : Gtk_Tree_Store;
      Iters : Iter_Array;
   begin
      Setup (Store, Iters);

      Store.Clear;
      Assert_Cmpint_Eq (N_Children (+Store), 0);

      for I in Iters'Range loop
         Assert_False (Store.Iter_Is_Valid (Iters (I)));
      end loop;
   end Test_Clear;

   ------------------
   -- Test_Reorder --
   ------------------

   procedure Test_Reorder is
      Store : Gtk_Tree_Store;
      Iters : Iter_Array;
   begin
      Setup (Store, Iters);

      Store.Reorder (Null_Iter, (4, 1, 0, 2, 3));
      Check_Model (Store, Iters, (4, 1, 0, 2, 3), -1);
   end Test_Reorder;

   ---------------------
   -- Test_Swap_Begin --
   ---------------------

   procedure Test_Swap_Begin is
      Store  : Gtk_Tree_Store;
      Iters  : Iter_Array;
      Iter_A : Gtk_Tree_Iter;
      Iter_B : Gtk_Tree_Iter;
   begin
      Setup (Store, Iters);

      --  Swap nodes 0 and 1 at the beginning.
      Iter_A := Get_Iter_From_String (+Store, "0");
      Iter_B := Get_Iter_From_String (+Store, "1");
      Assert_True (Iter_A /= Null_Iter);
      Assert_True (Iter_B /= Null_Iter);

      Store.Swap (Iter_A, Iter_B);
      Check_Model (Store, Iters, (1, 0, 2, 3, 4), -1);
   end Test_Swap_Begin;

   ----------------------
   -- Test_Swap_Single --
   ----------------------

   --  Swapping a single node with itself must not corrupt the store.

   procedure Test_Swap_Single is
      Store     : Gtk_Tree_Store;
      Iter      : Gtk_Tree_Iter;
      Iter_Copy : Gtk_Tree_Iter;
   begin
      Gtk_New (Store, (0 => GType_Int));

      Store.Append (Iter, Null_Iter);
      Iter_Copy := Iter;

      Store.Swap (Iter, Iter);
      Assert_True (Iter = Iter_Copy);
      Iter := Get_Iter_First (+Store);
      Assert_True (Iter = Iter_Copy);
   end Test_Swap_Single;

   ---------------------------------
   -- Test_Move_After_From_Start --
   ---------------------------------

   procedure Test_Move_After_From_Start is
      Store    : Gtk_Tree_Store;
      Iters    : Iter_Array;
      Iter     : Gtk_Tree_Iter;
      Position : Gtk_Tree_Iter;
   begin
      Setup (Store, Iters);

      --  Move node 0 after 2.
      Iter := Get_Iter_From_String (+Store, "0");
      Position := Get_Iter_From_String (+Store, "2");
      Store.Move_After (Iter, Position);

      Check_Model (Store, Iters, (1, 2, 0, 3, 4), -1);
   end Test_Move_After_From_Start;

   --------------------------
   -- Test_Move_After_Null --
   --------------------------

   procedure Test_Move_After_Null is
      Store : Gtk_Tree_Store;
      Iters : Iter_Array;
      Iter  : Gtk_Tree_Iter;
   begin
      Setup (Store, Iters);

      --  Move node 2; a null position prepends.
      Iter := Get_Iter_From_String (+Store, "2");
      Store.Move_After (Iter, Null_Iter);

      Check_Model (Store, Iters, (2, 0, 1, 3, 4), -1);
   end Test_Move_After_Null;

   --------------------------------
   -- Test_Move_Before_To_Start --
   --------------------------------

   procedure Test_Move_Before_To_Start is
      Store    : Gtk_Tree_Store;
      Iters    : Iter_Array;
      Iter     : Gtk_Tree_Iter;
      Position : Gtk_Tree_Iter;
   begin
      Setup (Store, Iters);

      --  Move node 2 before 0.
      Iter := Get_Iter_From_String (+Store, "2");
      Position := Get_Iter_From_String (+Store, "0");
      Store.Move_Before (Iter, Position);

      Check_Model (Store, Iters, (2, 0, 1, 3, 4), -1);
   end Test_Move_Before_To_Start;

   ---------------------------
   -- Test_Move_Before_Null --
   ---------------------------

   procedure Test_Move_Before_Null is
      Store : Gtk_Tree_Store;
      Iters : Iter_Array;
      Iter  : Gtk_Tree_Iter;
   begin
      Setup (Store, Iters);

      --  Move node 2; a null position appends.
      Iter := Get_Iter_From_String (+Store, "2");
      Store.Move_Before (Iter, Null_Iter);

      Check_Model (Store, Iters, (0, 1, 3, 4, 2), -1);
   end Test_Move_Before_Null;

   -----------------------------
   -- Test_Iter_Prev_Invalid --
   -----------------------------

   procedure Test_Iter_Prev_Invalid is
      Store : Gtk_Tree_Store;
      Iters : Iter_Array;
      Iter  : Gtk_Tree_Iter;
   begin
      Setup (Store, Iters);

      Iter := Get_Iter_First (+Store);
      Previous (+Store, Iter);
      Assert_True (Iter = Null_Iter);
      Assert_False (Store.Iter_Is_Valid (Iter));
   end Test_Iter_Prev_Invalid;

   -----------------------------
   -- Test_Iter_Next_Invalid --
   -----------------------------

   procedure Test_Iter_Next_Invalid is
      Store : Gtk_Tree_Store;
      Iters : Iter_Array;
      Iter  : Gtk_Tree_Iter;
   begin
      Setup (Store, Iters);

      Iter := Nth_Child (+Store, Null_Iter, 4);
      Next (+Store, Iter);
      Assert_True (Iter = Null_Iter);
      Assert_False (Store.Iter_Is_Valid (Iter));
   end Test_Iter_Next_Invalid;

   ---------------------------------
   -- Test_Iter_Children_Invalid --
   ---------------------------------

   procedure Test_Iter_Children_Invalid is
      Store : Gtk_Tree_Store;
      Iters : Iter_Array;
      Iter  : Gtk_Tree_Iter;
      Child : Gtk_Tree_Iter;
   begin
      Setup (Store, Iters);

      Iter := Get_Iter_First (+Store);
      Assert_True (Store.Iter_Is_Valid (Iter));

      Child := Children (+Store, Iter);
      Assert_True (Child = Null_Iter);
      Assert_False (Store.Iter_Is_Valid (Child));
   end Test_Iter_Children_Invalid;

   ----------------------------------
   -- Test_Iter_Nth_Child_Invalid --
   ----------------------------------

   procedure Test_Iter_Nth_Child_Invalid is
      Store : Gtk_Tree_Store;
      Iters : Iter_Array;
      Iter  : Gtk_Tree_Iter;
      Child : Gtk_Tree_Iter;
   begin
      Setup (Store, Iters);

      Iter := Get_Iter_First (+Store);
      Assert_True (Store.Iter_Is_Valid (Iter));

      Child := Nth_Child (+Store, Iter, 0);
      Assert_True (Child = Null_Iter);
      Assert_False (Store.Iter_Is_Valid (Child));
   end Test_Iter_Nth_Child_Invalid;

   -------------------------------
   -- Test_Iter_Parent_Invalid --
   -------------------------------

   procedure Test_Iter_Parent_Invalid is
      Store : Gtk_Tree_Store;
      Iters : Iter_Array;
      Iter  : Gtk_Tree_Iter;
      Child : Gtk_Tree_Iter;
   begin
      Setup (Store, Iters);

      Child := Get_Iter_First (+Store);
      Assert_True (Store.Iter_Is_Valid (Child));

      Iter := Parent (+Store, Child);
      Assert_True (Iter = Null_Iter);
      Assert_False (Store.Iter_Is_Valid (Iter));
   end Test_Iter_Parent_Invalid;

   --------------------
   -- Test_Bug_77977 --
   --------------------

   --  http://bugzilla.gnome.org/show_bug.cgi?id=77977
   --  Removing the root of a 3-deep tree must not crash. This is the suite's
   --  child-level coverage.

   procedure Test_Bug_77977 is
      Store : Gtk_Tree_Store;
      Iter1 : Gtk_Tree_Iter;
      Iter2 : Gtk_Tree_Iter;
      Iter3 : Gtk_Tree_Iter;
   begin
      Gtk_New (Store, (0 => GType_String));

      Store.Append (Iter1, Null_Iter);
      Store.Set (Iter1, 0, "Window1");

      Store.Append (Iter2, Iter1);
      Store.Set (Iter2, 0, "Table1");

      Store.Append (Iter3, Iter2);
      Store.Set (Iter3, 0, "Button1");

      Store.Remove (Iter1);
      Assert_Cmpint_Eq (N_Children (+Store), 0);
   end Test_Bug_77977;

   ---------------------------
   -- Test_Model_And_Values --
   ---------------------------

   --  Check model metadata, tree traversal, paths, every scalar setter, and
   --  the GValue forms of Set and Get_Value on a small three-level tree.
   procedure Test_Model_And_Values is
      Store       : Gtk_Tree_Store;
      Empty       : Gtk_Tree_Store;
      Root        : Gtk_Tree_Iter;
      Child       : Gtk_Tree_Iter;
      Grandchild  : Gtk_Tree_Iter;
      Second_Root : Gtk_Tree_Iter;
      Iter        : Gtk_Tree_Iter;
      Path        : Gtk_Tree_Path;
      Bad_Path    : Gtk_Tree_Path;
      Value       : GValue;
      Read_Value  : GValue;
      Iter_Value  : GValue;
      Pair        : GValue_Array (0 .. 1);
      All_Values  : GValue_Array (0 .. 2);
      Columns     : Gint_Array (0 .. 1) := (0 => 1, 1 => 0);
      Types       : constant GType_Array :=
        (0 => GType_String, 1 => GType_Int, 2 => GType_Boolean,
         3 => GType_Ulong, 4 => GType_Pointer, 5 => GType_Object);
      Action      : Glib.Simple_Action.Gsimple_Action;
      Object_Value : Glib.Object.GObject;
      Read_Path    : Gtk_Tree_Path;
      Iter_Copy    : Gtk_Tree_Iter;
      Caught       : Boolean;
      Sort_Id     : Gint;
      Order       : Gtk.Enums.Gtk_Sort_Type;
   begin
      Gtk_New (Store, Types);
      Gtk.Tree_Store.Initialize (Store, (0 => GType_Int));
      Assert_Cmpint_Eq (Get_N_Columns (Store), 6);
      Assert_True (Get_Column_Type (Store, 0) = GType_String);
      Assert_True (Get_Column_Type (Store, 5) = GType_Object);
      Assert_True
        (Get_Flags (Store) = Tree_Model_Iters_Persist);

      --  A second construction path and the empty-tree iterator result are
      --  both part of the public API.
      Empty := Gtk_Tree_Store_Newv ((0 => GType_Int));
      Assert_True (Get_Iter_First (Empty) = Null_Iter);
      Assert_True (Gtk.Tree_Model.Get_Iter_First (+Empty) = Null_Iter);
      Assert_Cmpint_Eq (Get_N_Columns (Empty), 1);

      Store.Append (Root, Null_Iter);
      Store.Set (Root, 0, "root");
      Store.Set (Root, 1, Gint (10));
      Store.Set (Root, 2, True);
      Store.Set_Ulong (Root, 3, Gulong (99));
      Store.Append (Child, Root);
      Store.Set (Child, 0, "child");
      Store.Set (Child, 1, Gint (20));
      Store.Append (Grandchild, Child);
      Store.Set (Grandchild, 0, "grandchild");
      Store.Set (Grandchild, 1, Gint (30));
      Store.Append (Second_Root, Null_Iter);
      --  A newly added string cell is null in GTK and the Ada getter maps it
      --  to the documented empty string for both overloads.
      Assert_Cmpstr_Eq (Gtk.Tree_Model.Get_String (+Store, Second_Root, 0), "");
      Assert_Cmpstr_Eq
        (Gtk.Tree_Model.Get_String
           (Gtk.Tree_Model.Gtk_Root_Tree_Model_Record (Store.all)'Access,
            Second_Root, 0), "");
      Store.Set (Second_Root, 0, "second root");
      Store.Set (Second_Root, 1, Gint (40));

      --  Check the hierarchical iterator wrappers and their null boundaries.
      Assert_Cmpint_Eq (N_Children (Store), 2);
      Assert_Cmpint_Eq (N_Children (Store, Root), 1);
      Assert_True (Has_Child (Store, Root));
      Assert_False (Has_Child (Store, Grandchild));
      Assert_True (Children (Store, Null_Iter) = Root);
      Assert_True (Children (Store, Root) = Child);
      Assert_True (Children (Store, Grandchild) = Null_Iter);
      Assert_True (Nth_Child (Store, Null_Iter, 1) = Second_Root);
      Assert_True (Nth_Child (Store, Null_Iter, 8) = Null_Iter);
      Assert_True (Parent (Store, Grandchild) = Child);
      Assert_True (Parent (Store, Root) = Null_Iter);
      Assert_Cmpint_Eq (Store.Iter_Depth (Root), 0);
      Assert_Cmpint_Eq (Store.Iter_Depth (Grandchild), 2);
      Assert_True (Store.Is_Ancestor (Root, Grandchild));
      Assert_False (Store.Is_Ancestor (Child, Root));
      --  Reach the interface bodies directly as well as the concrete-store
      --  methods above, including their traversal and string-path wrappers.
      Assert_True (Gtk.Tree_Model.Has_Child (+Store, Root));
      Assert_True (Gtk.Tree_Model.Children (+Store, Null_Iter) = Root);
      Assert_True (Gtk.Tree_Model.Nth_Child (+Store, Null_Iter, 1) = Second_Root);
      Assert_True (Gtk.Tree_Model.Parent (+Store, Grandchild) = Child);
      Assert_Cmpstr_Eq
        (Gtk.Tree_Model.Get_String_From_Iter (+Store, Grandchild), "0:0:0");
      Iter := Root;
      Gtk.Tree_Model.Next (+Store, Iter);
      Assert_True (Iter = Second_Root);

      Iter := Get_Iter_First (Store);
      Assert_True (Iter = Root);
      Next (Store, Iter);
      Assert_True (Iter = Second_Root);
      Next (Store, Iter);
      Assert_True (Iter = Null_Iter);
      Iter := Second_Root;
      Previous (Store, Iter);
      Assert_True (Iter = Root);
      Previous (Store, Iter);
      Assert_True (Iter = Null_Iter);

      --  Paths can be used in either direction, and invalid paths return a
      --  null iterator instead of an uninitialized one.
      Path := Get_Path (Store, Grandchild);
      Assert_Cmpstr_Eq (Get_String_From_Iter (Store, Grandchild), "0:0:0");
      Assert_True (Get_Iter (Store, Path) = Grandchild);
      --  Convert is a borrowed address round trip; both wrappers must retain
      --  the same C path without taking or releasing another reference.
      Read_Path := Gtk.Tree_Model.Convert
        (Gtk.Tree_Model.Convert (Path));
      Assert_True (Compare (Read_Path, Path) = 0);
      Assert_True (Get_Iter_From_String (Store, "0:0") = Child);
      Bad_Path := Gtk_Tree_Path_New_From_String ("9");
      Assert_True (Get_Iter (Store, Bad_Path) = Null_Iter);
      Assert_True (Gtk.Tree_Model.Get_Iter (+Store, Bad_Path) = Null_Iter);
      Assert_True (Get_Iter_From_String (Store, "9") = Null_Iter);
      Assert_True
        (Gtk.Tree_Model.Get_Iter_From_String (+Store, "9") = Null_Iter);
      Path_Free (Path);
      Path_Free (Bad_Path);

      --  Exercise both GValue array overloads and the explicit GValue setter.
      Init_Set_Int (Pair (0), 22);
      Init_Set_String (Pair (1), "updated root");
      Store.Set (Root, Columns, Pair);
      Unset (Pair (0));
      Unset (Pair (1));
      Assert_Cmpint_Eq (Get_Int (+Store, Root, 1), 22);
      Assert_Cmpstr_Eq (Get_String (+Store, Root, 0), "updated root");
      --  Exercise the Gtk_Root_Tree_Model access overloads explicitly. These
      --  are distinct from the Gtk_Tree_Model interface overloads above.
      Assert_Cmpint_Eq
        (Gtk.Tree_Model.Get_Int
           (Gtk.Tree_Model.Gtk_Root_Tree_Model_Record (Store.all)'Access,
            Root, 1), 22);
      Assert_Cmpstr_Eq
        (Gtk.Tree_Model.Get_String
           (Gtk.Tree_Model.Gtk_Root_Tree_Model_Record (Store.all)'Access,
            Root, 0), "updated root");
      Assert_True
        (Gtk.Tree_Model.Get_Boolean
           (Gtk.Tree_Model.Gtk_Root_Tree_Model_Record (Store.all)'Access,
            Root, 2));
      Assert_True
        (Gtk.Tree_Model.Get_Ulong
           (Gtk.Tree_Model.Gtk_Root_Tree_Model_Record (Store.all)'Access,
            Root, 3) = Gulong (99));

      Init_Set_String (All_Values (0), "second root");
      Init_Set_Int (All_Values (1), 44);
      Init_Set_Boolean (All_Values (2), False);
      Store.Set (Second_Root, All_Values);
      for J in All_Values'Range loop
         Unset (All_Values (J));
      end loop;
      Init_Set_Int (Value, 45);
      Gtk.Tree_Store.Set_Value (Store, Second_Root, 1, Value);
      Unset (Value);
      Gtk.Tree_Store.Get_Value (Store, Second_Root, 1, Read_Value);
      Assert_Cmpint_Eq (Get_Int (Read_Value), 45);
      Unset (Read_Value);
      Assert_False (Get_Boolean (+Store, Second_Root, 2));
      Assert_True (Get_Ulong (+Store, Root, 3) = Gulong (99));
      Assert_False
        (Gtk.Tree_Model.Get_Boolean
           (Gtk.Tree_Model.Gtk_Root_Tree_Model_Record (Store.all)'Access,
            Second_Root, 2));
      Assert_True
        (Gtk.Tree_Model.Get_Ulong
           (Gtk.Tree_Model.Gtk_Root_Tree_Model_Record (Store.all)'Access,
            Root, 3) = Gulong (99));

      --  Store and clear a regular GObject to cover both object setter paths.
      Action := Glib.Simple_Action.Gsimple_Action_New ("tree-store-value", null);
      Store.Set (Root, 5, Glib.Object.GObject (Action));
      Object_Value := Get_Object (+Store, Root, 5);
      Assert_True (Object_Value = Glib.Object.GObject (Action));
      Object_Value := Gtk.Tree_Model.Get_Object
        (Gtk.Tree_Model.Gtk_Root_Tree_Model_Record (Store.all)'Access,
         Root, 5);
      Assert_True (Object_Value = Glib.Object.GObject (Action));
      Store.Set (Root, 5, Glib.Object.GObject'(null));
      Assert_True (Get_Object (+Store, Root, 5) = null);
      Assert_True
        (Gtk.Tree_Model.Get_Object
           (Gtk.Tree_Model.Gtk_Root_Tree_Model_Record (Store.all)'Access,
            Root, 5) = null);
      Glib.Object.Unref (Glib.Object.GObject (Action));

      --  Gtk_Tree_Iter values are pointers by contract, so retain the source
      --  iterator until both GValue extraction overloads have copied it.
      Init (Iter_Value, GType_Pointer);
      Gtk.Tree_Model.Set_Tree_Iter (Iter_Value, Root);
      Gtk.Tree_Model.Get_Tree_Iter (Iter_Value, Iter_Copy);
      Assert_True (Iter_Copy = Root);
      Assert_True (Gtk.Tree_Model.Get_Tree_Iter (Iter_Value) = Root);
      Assert_True (Gtk.Tree_Model.To_Address (Root) /= System.Null_Address);
      Unset (Iter_Value);

      --  The path GValue helper is a borrowed pointer conversion; freeing the
      --  original path remains the caller's responsibility.
      Path := Gtk_Tree_Path_New_From_String ("0:0:0");
      Init (Value, GType_Pointer);
      Glib.Values.Set_Address (Value, Gtk.Tree_Model.Convert (Path));
      Read_Path := Gtk.Tree_Model.Get_Tree_Path (Value);
      Assert_Cmpstr_Eq (To_String (Read_Path), "0:0:0");
      Unset (Value);
      Path_Free (Path);

      --  Null iterators are rejected by the scalar and pointer accessors before
      --  entering GTK. Catch each documented Program_Error and assert it ran.
      Caught := False;
      begin
         declare
            Ignored : constant Gint := Gtk.Tree_Model.Get_Int (+Store, Null_Iter, 1);
            pragma Unreferenced (Ignored);
         begin
            null;
         end;
      exception
         when Program_Error => Caught := True;
      end;
      Assert_True (Caught);
      Caught := False;
      begin
         declare
            Ignored : constant Gulong := Gtk.Tree_Model.Get_Ulong (+Store, Null_Iter, 3);
            pragma Unreferenced (Ignored);
         begin
            null;
         end;
      exception
         when Program_Error => Caught := True;
      end;
      Assert_True (Caught);
      Caught := False;
      begin
         declare
            Ignored : constant Glib.C_Proxy := Gtk.Tree_Model.Get_C_Proxy (+Store, Null_Iter, 4);
            pragma Unreferenced (Ignored);
         begin
            null;
         end;
      exception
         when Program_Error => Caught := True;
      end;
      Assert_True (Caught);
      Caught := False;
      begin
         declare
            Ignored : constant String := Gtk.Tree_Model.Get_String (+Store, Null_Iter, 0);
            pragma Unreferenced (Ignored);
         begin
            null;
         end;
      exception
         when Program_Error => Caught := True;
      end;
      Assert_True (Caught);
      Caught := False;
      begin
         declare
            Ignored : constant System.Address := Gtk.Tree_Model.Get_Address (+Store, Null_Iter, 4);
            pragma Unreferenced (Ignored);
         begin
            null;
         end;
      exception
         when Program_Error => Caught := True;
      end;
      Assert_True (Caught);

      --  The sortable interface reports no active sort on a fresh store.
      Assert_False (Store.Get_Sort_Column_Id (Sort_Id, Order));
      Glib.Object.Unref (Glib.Object.GObject (Empty));
      Glib.Object.Unref (Glib.Object.GObject (Store));
   end Test_Model_And_Values;

   --------------------------
   -- Test_Pointer_Columns --
   --------------------------

   --  Verify the generic access-value helper and both lower-level pointer
   --  overloads using a live aliased record as the stored address.
   procedure Test_Pointer_Columns is
      Store : Gtk_Tree_Store;
      Iter  : Gtk_Tree_Iter;
      Data  : aliased Pointer_Data := (Value => 73);
      Types : constant GType_Array := (0 => GType_Pointer);
      Proxy : Glib.C_Proxy;
   begin
      Gtk_New (Store, Types);
      Store.Append (Iter, Null_Iter);
      Proxy := Glib.To_Proxy (Data'Address);

      Pointer_Column.Set (Store, Iter, 0, Data'Unchecked_Access);
      Assert_True
        (Pointer_Column.Get (Store, Iter, 0) = Data'Unchecked_Access);
      Assert_Cmpint_Eq (Pointer_Column.Get (Store, Iter, 0).Value, 73);

      Store.Set (Iter, 0, Data'Address);
      Assert_True (Get_Address (+Store, Iter, 0) = Data'Address);
      Assert_True
        (Gtk.Tree_Model.Get_Address
           (Gtk.Tree_Model.Gtk_Root_Tree_Model_Record (Store.all)'Access,
            Iter, 0) = Data'Address);
      Assert_True (Gtk.Tree_Model.Get_C_Proxy (+Store, Iter, 0) = Proxy);
      Assert_True
        (Gtk.Tree_Model.Get_C_Proxy
           (Gtk.Tree_Model.Gtk_Root_Tree_Model_Record (Store.all)'Access,
            Iter, 0) = Proxy);
      Store.Set (Iter, 0, Glib.To_Proxy (Data'Address));
      Assert_True (Get_Address (+Store, Iter, 0) = Data'Address);
      Glib.Object.Unref (Glib.Object.GObject (Store));
   end Test_Pointer_Columns;

   -------------------------------
   -- Test_Foreach_And_Sorting --
   -------------------------------

   --  Run plain and generic depth-first callbacks, custom sort functions,
   --  sort state accessors, and the tree-view freeze/thaw helpers.
   procedure Test_Foreach_And_Sorting is
      Store : Gtk_Tree_Store;
      A, B, C : Gtk_Tree_Iter;
      Column : Gint;
      Order  : Gtk.Enums.Gtk_Sort_Type;
      Frozen : Gint;
      Types  : constant GType_Array :=
        (0 => GType_String, 1 => GType_Int);
   begin
      Store := Gtk_Tree_Store_Newv (Types);
      Store.Append (A, Null_Iter);
      Store.Set (A, 1, Gint (30));
      Store.Append (B, Null_Iter);
      Store.Set (B, 1, Gint (10));
      Store.Append (C, Null_Iter);
      Store.Set (C, 1, Gint (20));

      Visit_Count := 0;
      Store.Foreach (Visit_Row'Unrestricted_Access);
      Assert_Cmpint_Eq (Visit_Count, 3);
      --  Call the interface callback bridge itself, then its separate generic
      --  user-data closure, instead of only the Tree_Store forwarding API.
      Visit_Count := 0;
      Gtk.Tree_Model.Foreach (+Store, Visit_Row'Unrestricted_Access);
      Assert_Cmpint_Eq (Visit_Count, 3);
      Visit_Count := 0;
      Model_Foreach_With_Data.Foreach
        (+Store, Visit_Model_Row_With_Data'Unrestricted_Access, 2);
      Assert_Cmpint_Eq (Visit_Count, 2);
      Visit_Count := 0;
      Foreach_With_Data.Foreach
        (Store, Visit_Row_With_Data'Unrestricted_Access, 2);
      Assert_Cmpint_Eq (Visit_Count, 2);

      Assert_False (Store.Has_Default_Sort_Func);
      Store.Set_Default_Sort_Func (Compare_Int'Unrestricted_Access);
      Assert_True (Store.Has_Default_Sort_Func);
      Store.Set_Sort_Column_Id (-1, Sort_Ascending);
      Assert_Cmpint_Eq (Get_Int (+Store, Get_Iter_First (Store), 1), 10);

      --  User data reverses the comparison and its destroy hook runs when the
      --  generic default callback is cleared.
      Destroy_Count := 0;
      Default_Sort_With_Data.Set_Default_Sort_Func
        (Store, Compare_Int_With_Data'Unrestricted_Access, -1);
      Store.Set_Sort_Column_Id (-1, Sort_Ascending);
      Assert_Cmpint_Eq (Get_Int (+Store, Get_Iter_First (Store), 1), 30);
      Store.Set_Sort_Column_Id (-2, Sort_Ascending);
      Default_Sort_With_Data.Set_Default_Sort_Func (Store, null, 0);
      Assert_Cmpint_Eq (Destroy_Count, 1);
      Assert_False (Store.Has_Default_Sort_Func);

      --  The direct overload accepts null to remove its default comparator.
      Store.Set_Default_Sort_Func (Compare_Int'Unrestricted_Access);
      Assert_True (Store.Has_Default_Sort_Func);
      Store.Set_Default_Sort_Func (null);
      Assert_False (Store.Has_Default_Sort_Func);

      Store.Set_Sort_Func (1, Compare_Int'Unrestricted_Access);
      Store.Set_Sort_Column_Id (1, Sort_Ascending);
      Assert_True (Store.Get_Sort_Column_Id (Column, Order));
      Assert_Cmpint_Eq (Column, 1);
      Assert_True (Order = Sort_Ascending);
      Assert_Cmpint_Eq (Get_Int (+Store, Get_Iter_First (Store), 1), 10);

      --  Replacing a generic column comparator invokes its data destructor.
      Column_Sort_With_Data.Set_Sort_Func
        (Store, 1, Compare_Int_With_Data'Unrestricted_Access, -1);
      Store.Set_Sort_Column_Id (1, Sort_Ascending);
      Assert_Cmpint_Eq (Get_Int (+Store, Get_Iter_First (Store), 1), 30);
      Store.Set_Sort_Func (1, Compare_Int'Unrestricted_Access);
      Assert_Cmpint_Eq (Destroy_Count, 2);
      Store.Sort_Column_Changed;

      Frozen := Store.Freeze_Sort;
      Assert_Cmpint_Eq (Frozen, 1);
      Store.Thaw_Sort (Frozen);
      Assert_True (Store.Get_Sort_Column_Id (Column, Order));
      Assert_Cmpint_Eq (Column, 1);
      Assert_True (Order = Sort_Ascending);
      Store.Set_Sort_Column_Id (-2, Sort_Ascending);
      Assert_False (Store.Get_Sort_Column_Id (Column, Order));
      Glib.Object.Unref (Glib.Object.GObject (Store));
   end Test_Foreach_And_Sorting;

   ------------------------
   -- Test_Drag_And_Drop --
   ------------------------

   --  A normal row can be exported and deleted; an unrelated string payload
   --  is rejected by both destination checks.
   procedure Test_Drag_And_Drop is
      Store    : Gtk_Tree_Store;
      Iter     : Gtk_Tree_Iter;
      Path     : Gtk_Tree_Path;
      Provider : Gdk.Content_Provider.Gdk_Content_Provider;
      Value    : GValue;
   begin
      Store := Gtk_Tree_Store_Newv ((0 => GType_String));
      Store.Append (Iter, Null_Iter);
      Store.Set (Iter, 0, "drag me");
      Path := Gtk_Tree_Path_New_From_String ("0");
      Assert_True (Store.Row_Draggable (Path));
      Provider := Store.Drag_Data_Get (Path);
      Assert_True (Provider /= null);
      Glib.Object.Unref (Glib.Object.GObject (Provider));

      Init_Set_String (Value, "not a serialized tree row");
      Assert_False (Store.Row_Drop_Possible (Path, Value));
      Assert_False (Store.Drag_Data_Received (Path, Value));
      Unset (Value);
      Assert_True (Store.Drag_Data_Delete (Path));
      Assert_Cmpint_Eq (Store.N_Children, 0);
      Assert_False (Store.Drag_Data_Delete (Path));
      Path_Free (Path);
      Glib.Object.Unref (Glib.Object.GObject (Store));
   end Test_Drag_And_Drop;

   --------------------------------
   -- Test_Model_Notifications --
   --------------------------------

   --  Check the row signal forwarding helpers and reference hooks. These
   --  notifications describe model events; they do not edit the stored rows.
   procedure Test_Model_Notifications is
      Store     : Gtk_Tree_Store;
      Root      : Gtk_Tree_Iter;
      Child     : Gtk_Tree_Iter;
      Other_Child : Gtk_Tree_Iter;
      Other_Root  : Gtk_Tree_Iter;
      Path      : Gtk_Tree_Path;
      Root_Path : Gtk_Tree_Path;
      New_Order : Gint_Array (0 .. 1) := (0 => 1, 1 => 0);
   begin
      Gtk_New (Store, (0 => GType_Int));
      Store.Append (Root, Null_Iter);
      Store.Append (Other_Root, Null_Iter);
      Store.Append (Child, Root);
      Store.Append (Other_Child, Root);
      Path := Store.Get_Path (Root);
      Root_Path := Store.Get_Path (Root);

      Gtk.Tree_Model.On_Row_Changed
        (+Store, Capture_Row_Event'Unrestricted_Access);
      Gtk.Tree_Model.On_Row_Has_Child_Toggled
        (+Store, Capture_Row_Event'Unrestricted_Access);
      Gtk.Tree_Model.On_Row_Inserted
        (+Store, Capture_Row_Event'Unrestricted_Access);
      Gtk.Tree_Model.On_Row_Deleted
        (+Store, Capture_Deleted_Event'Unrestricted_Access);
      Gtk.Tree_Model.On_Rows_Reordered
        (+Store, Capture_Reordered_Event'Unrestricted_Access);
      --  These overloads bind callbacks with a GObject slot, exercising the
      --  interface marshaller variants used by connect_object handlers.
      Gtk.Tree_Model.On_Row_Changed
        (+Store, Capture_Slot_Row_Event'Unrestricted_Access, Store);
      Gtk.Tree_Model.On_Row_Has_Child_Toggled
        (+Store, Capture_Slot_Row_Event'Unrestricted_Access, Store);
      Gtk.Tree_Model.On_Row_Inserted
        (+Store, Capture_Slot_Row_Event'Unrestricted_Access, Store);
      Gtk.Tree_Model.On_Row_Deleted
        (+Store, Capture_Slot_Deleted_Event'Unrestricted_Access, Store);
      Gtk.Tree_Model.On_Rows_Reordered
        (+Store, Capture_Slot_Reordered_Event'Unrestricted_Access, Store);

      Changed_Count := 0;
      Slot_Event_Count := 0;
      Store.Row_Changed (Path, Root);
      Store.Row_Has_Child_Toggled (Path, Root);
      Store.Row_Inserted (Path, Root);
      Assert_Cmpint_Eq (Changed_Count, 3);
      --  The Gtk.Tree_Model notification helpers are callable independently
      --  of the model's concrete convenience methods.
      Gtk.Tree_Model.Row_Changed (+Store, Path, Root);
      Gtk.Tree_Model.Row_Has_Child_Toggled (+Store, Path, Root);
      Gtk.Tree_Model.Row_Inserted (+Store, Path, Root);
      Assert_Cmpint_Eq (Changed_Count, 6);
      Assert_Cmpint_Eq (Slot_Event_Count, 6);

      Deleted_Count := 0;
      Store.Row_Deleted (Path);
      Gtk.Tree_Model.Row_Deleted (+Store, Path);
      Assert_Cmpint_Eq (Deleted_Count, 2);
      Assert_Cmpint_Eq (Slot_Event_Count, 8);

      Reordered_Count := 0;
      Slot_Reordered_Count := 0;
      --  Use a non-root parent because nullable root iterators expose a
      --  marshaller defect documented in the binding notes.
      Store.Rows_Reordered (Root_Path, Root, New_Order);
      Gtk.Tree_Model.Rows_Reordered (+Store, Root_Path, Root, New_Order);
      Gtk.Tree_Model.Rows_Reordered_With_Length
        (+Store, Root_Path, Root, New_Order, New_Order'Length);
      Assert_Cmpint_Eq (Reordered_Count, 3);
      Assert_Cmpint_Eq (Slot_Reordered_Count, 3);

      Store.Ref_Node (Root);
      Store.Unref_Node (Root);
      Assert_True (Store.Iter_Is_Valid (Root));
      Assert_Cmpint_Eq (Store.N_Children, 2);
      Path_Free (Path);
      Path_Free (Root_Path);
      Glib.Object.Unref (Glib.Object.GObject (Store));
   end Test_Model_Notifications;

begin
   Glib.Test.Init;

   --  The C test uses gtk_test_init, which also initializes GTK; models
   --  cannot be created until GTK is initialized.
   Gtk.Main.Init;

   --  Insertion.
   Glib.Test.Add_Func
     ("/tree-store/insert-high-values",
      Test_Insert_High_Values'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/append", Test_Append'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/prepend", Test_Prepend'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/insert-after", Test_Insert_After'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/insert-after-null",
      Test_Insert_After_Null'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/insert-before", Test_Insert_Before'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/insert-before-null",
      Test_Insert_Before_Null'Unrestricted_Access);

   --  Value setting.
   Glib.Test.Add_Func
     ("/tree-store/set-value", Test_Set_Value'Unrestricted_Access);

   --  Removal & clear.
   Glib.Test.Add_Func
     ("/tree-store/remove-begin", Test_Remove_Begin'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/remove-middle", Test_Remove_Middle'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/remove-end", Test_Remove_End'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/clear", Test_Clear'Unrestricted_Access);

   --  Reorder & swap.
   Glib.Test.Add_Func
     ("/tree-store/reorder", Test_Reorder'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/swap-begin", Test_Swap_Begin'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/swap-single", Test_Swap_Single'Unrestricted_Access);

   --  Move.
   Glib.Test.Add_Func
     ("/tree-store/move-after-from-start",
      Test_Move_After_From_Start'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/move-after-null",
      Test_Move_After_Null'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/move-before-to-start",
      Test_Move_Before_To_Start'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/move-before-null",
      Test_Move_Before_Null'Unrestricted_Access);

   --  Iter invalidation.
   Glib.Test.Add_Func
     ("/tree-store/iter-prev-invalid",
      Test_Iter_Prev_Invalid'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/iter-next-invalid",
      Test_Iter_Next_Invalid'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/iter-children-invalid",
      Test_Iter_Children_Invalid'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/iter-nth-child-invalid",
      Test_Iter_Nth_Child_Invalid'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/iter-parent-invalid",
      Test_Iter_Parent_Invalid'Unrestricted_Access);

   --  Specific bug (child-level coverage).
   Glib.Test.Add_Func
     ("/tree-store/bug-77977", Test_Bug_77977'Unrestricted_Access);

   --  TreeModel values, generic access columns, and callback bridges.
   Glib.Test.Add_Func
     ("/tree-store/model-and-values",
      Test_Model_And_Values'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/pointer-columns",
      Test_Pointer_Columns'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/foreach-and-sorting",
      Test_Foreach_And_Sorting'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/drag-and-drop",
      Test_Drag_And_Drop'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-store/model-notifications",
      Test_Model_Notifications'Unrestricted_Access);

   --  Return with the exit code.
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);

   pragma Warnings (On, "*obsolescent*");
end Main;
