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

with Glib;           use Glib;
with Glib.Test;      use Glib.Test;

with Gtk.Main;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Store; use Gtk.Tree_Store;

procedure Main is

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

   --  Return with the exit code.
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);

   pragma Warnings (On, "*obsolescent*");
end Main;
