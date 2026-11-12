--  Port of the core GtkBitset cases in testsuite/c_tests/bitset.c, including
--  its iterator tests /bitset/iter/basic and /bitset/iter/forward-reverse.
--  These cases check constructors, ordered queries, mutations, set algebra,
--  shifts, splice behavior, and iterator traversal through the Ada bindings.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib;      use Glib;
with Glib.Test; use Glib.Test;
with Gtk.Bitset; use Gtk.Bitset;
with Gtk.Bitset_Iter; use Gtk.Bitset_Iter;

procedure Main is

   procedure Test_Constructors_And_Queries with Convention => C;
   procedure Test_Copy_And_Reference with Convention => C;
   procedure Test_Mutations with Convention => C;
   procedure Test_Set_Operations with Convention => C;
   procedure Test_Shifts_And_Splice with Convention => C;
   procedure Test_Iterator_Basics with Convention => C;
   procedure Test_Iterator_Forward_Reverse with Convention => C;

   -------------------------------
   -- Test_Constructors_And_Queries --
   -------------------------------

   procedure Test_Constructors_And_Queries is
      Empty, By_Procedure, By_Function : Gtk_Bitset;
      Set : Gtk_Bitset := Gtk_Bitset_New_Empty;
   begin
      --  Both out-parameter constructors produce the expected initial state.
      Gtk_New_Empty (Empty);
      Assert_True (Is_Empty (Empty));
      Assert_Cmpuint_Eq (Guint (Get_Size (Empty)), 0);
      Assert_Cmpuint_Eq (Get_Minimum (Empty), Guint'Last);
      Assert_Cmpuint_Eq (Get_Maximum (Empty), 0);

      Gtk_New_Range (By_Procedure, 30, 4);
      By_Function := Gtk_Bitset_New_Range (30, 4);
      Assert_Cmpuint_Eq (Guint (Get_Size (By_Procedure)), 4);
      Assert_True (Equals (By_Procedure, By_Function));
      Assert_True (Contains (By_Procedure, 30));
      Assert_True (Contains (By_Procedure, 33));
      Assert_True (not Contains (By_Procedure, 34));

      --  Add reports whether membership changed, including a duplicate add.
      Assert_True (Add (Set, 10));
      Assert_True (not Add (Set, 10));
      Assert_True (Add (Set, 12));
      Add_Range (Set, 15, 3);
      Add_Range_Closed (Set, 20, 22);

      --  Metadata and indexed access use ascending order and inclusive bounds.
      Assert_Cmpuint_Eq (Guint (Get_Size (Set)), 8);
      Assert_Cmpuint_Eq (Get_Minimum (Set), 10);
      Assert_Cmpuint_Eq (Get_Maximum (Set), 22);
      Assert_Cmpuint_Eq (Guint (Get_Size_In_Range (Set, 11, 20)), 5);
      Assert_Cmpuint_Eq (Get_Nth (Set, 0), 10);
      Assert_Cmpuint_Eq (Get_Nth (Set, 2), 15);
      Assert_Cmpuint_Eq (Get_Nth (Set, 7), 22);
      Assert_Cmpuint_Eq (Get_Nth (Set, 8), 0);

      --  Each constructor transfers one owned reference to the caller.
      Unref (Set);
      Unref (By_Function);
      Unref (By_Procedure);
      Unref (Empty);
   end Test_Constructors_And_Queries;

   ---------------------------
   -- Test_Copy_And_Reference --
   ---------------------------

   procedure Test_Copy_And_Reference is
      Original : Gtk_Bitset := Gtk_Bitset_New_Range (4, 5);
      Duplicate : Gtk_Bitset;
      Shared : Gtk_Bitset;
   begin
      --  Copy owns independent contents; changing it leaves the source intact.
      Duplicate := Copy (Original);
      Assert_True (Equals (Original, Duplicate));
      Assert_True (Add (Duplicate, 20));
      Assert_True (not Equals (Original, Duplicate));
      Assert_True (not Contains (Original, 20));

      --  Ref shares the same set and keeps it alive after the first unref.
      Shared := Ref (Original);
      Unref (Original);
      Assert_True (Contains (Shared, 4));

      Unref (Shared);
      Unref (Duplicate);
   end Test_Copy_And_Reference;

   -------------------
   -- Test_Mutations --
   -------------------

   procedure Test_Mutations is
      Set : Gtk_Bitset := Gtk_Bitset_New_Empty;
   begin
      --  Value and range removals report the expected remaining membership.
      Add_Range_Closed (Set, 1, 8);
      Assert_True (Remove (Set, 3));
      Assert_True (not Remove (Set, 3));
      Remove_Range (Set, 5, 2);
      Remove_Range_Closed (Set, 7, 8);
      Assert_Cmpuint_Eq (Guint (Get_Size (Set)), 3);
      Assert_True (Contains (Set, 1));
      Assert_True (Contains (Set, 2));
      Assert_True (Contains (Set, 4));
      Assert_True (not Contains (Set, 5));

      --  Rectangles select rows by stride; removing a narrow column leaves
      --  the other cells in those rows untouched.
      Add_Rectangle (Set, 20, 2, 2, 5);
      Assert_True (Contains (Set, 20));
      Assert_True (Contains (Set, 21));
      Assert_True (Contains (Set, 25));
      Assert_True (Contains (Set, 26));
      Remove_Rectangle (Set, 20, 1, 2, 5);
      Assert_True (not Contains (Set, 20));
      Assert_True (Contains (Set, 21));
      Assert_True (not Contains (Set, 25));
      Assert_True (Contains (Set, 26));

      --  Remove_All resets metadata as well as clearing membership.
      Remove_All (Set);
      Assert_True (Is_Empty (Set));
      Assert_Cmpuint_Eq (Guint (Get_Size (Set)), 0);
      Unref (Set);
   end Test_Mutations;

   ------------------------
   -- Test_Set_Operations --
   ------------------------

   procedure Test_Set_Operations is
      Left : Gtk_Bitset := Gtk_Bitset_New_Range (1, 6);
      Right : Gtk_Bitset := Gtk_Bitset_New_Range (4, 5);
      Result : Gtk_Bitset;
   begin
      --  Symmetric difference keeps values present in exactly one operand.
      Result := Copy (Left);
      Difference (Result, Right);
      Assert_Cmpuint_Eq (Guint (Get_Size (Result)), 5);
      Assert_True (Contains (Result, 1));
      Assert_True (not Contains (Result, 4));
      Assert_True (Contains (Result, 7));
      Unref (Result);

      --  Intersection retains only the shared closed interval 4 through 6.
      Result := Copy (Left);
      Intersect (Result, Right);
      Assert_Cmpuint_Eq (Guint (Get_Size (Result)), 3);
      Assert_True (Contains (Result, 4));
      Assert_True (Contains (Result, 6));
      Assert_True (not Contains (Result, 7));
      Unref (Result);

      --  Subtraction removes the right-hand members from the left-hand set.
      Result := Copy (Left);
      Subtract (Result, Right);
      Assert_Cmpuint_Eq (Guint (Get_Size (Result)), 3);
      Assert_True (Contains (Result, 1));
      Assert_True (not Contains (Result, 4));
      Unref (Result);

      --  Union includes every member from both inputs without duplicates.
      Result := Copy (Left);
      Union (Result, Right);
      Assert_Cmpuint_Eq (Guint (Get_Size (Result)), 8);
      Assert_True (Contains (Result, 1));
      Assert_True (Contains (Result, 8));
      Assert_True (not Contains (Result, 9));
      Unref (Result);

      Unref (Right);
      Unref (Left);
   end Test_Set_Operations;

   ----------------------------
   -- Test_Shifts_And_Splice --
   ----------------------------

   procedure Test_Shifts_And_Splice is
      Shifted : Gtk_Bitset := Gtk_Bitset_New_Range (10, 4);
      Spliced : Gtk_Bitset := Gtk_Bitset_New_Empty;
   begin
      --  Left and right shifts move all members by the requested offsets.
      Shift_Left (Shifted, 3);
      Assert_Cmpuint_Eq (Get_Minimum (Shifted), 7);
      Assert_Cmpuint_Eq (Get_Maximum (Shifted), 10);
      Shift_Right (Shifted, 2);
      Assert_Cmpuint_Eq (Get_Minimum (Shifted), 9);
      Assert_Cmpuint_Eq (Get_Maximum (Shifted), 12);
      Assert_Cmpuint_Eq (Guint (Get_Size (Shifted)), 4);

      --  Splice first removes ten positions and inserts twenty empty ones.
      Add_Range (Spliced, 10, 30);
      Splice (Spliced, 20, 10, 20);
      Assert_Cmpuint_Eq (Guint (Get_Size (Spliced)), 20);
      Assert_True (Contains (Spliced, 10));
      Assert_True (Contains (Spliced, 19));
      Assert_True (not Contains (Spliced, 20));
      Assert_True (not Contains (Spliced, 39));
      Assert_True (Contains (Spliced, 40));
      Assert_True (Contains (Spliced, 49));

      --  A second splice with no added positions closes the resulting gap.
      Splice (Spliced, 25, 10, 0);
      Assert_Cmpuint_Eq (Guint (Get_Size (Spliced)), 20);
      Assert_True (not Contains (Spliced, 29));
      Assert_True (Contains (Spliced, 30));
      Assert_True (Contains (Spliced, 39));

      Unref (Spliced);
      Unref (Shifted);
   end Test_Shifts_And_Splice;

   ---------------------------
   -- Test_Iterator_Basics --
   ---------------------------

   procedure Test_Iterator_Basics is
      Empty_Set : Gtk_Bitset := Gtk_Bitset_New_Empty;
      Set       : Gtk_Bitset := Gtk_Bitset_New_Empty;
      Iter      : aliased Gtk_Bitset_Iter;
      Value     : aliased Guint := 0;
   begin
      --  Empty sets fail all three initializers and leave the iterator invalid.
      Assert_True (not Init_First (Iter, Empty_Set, Value'Access));
      Assert_True (not Is_Valid (Iter'Access));
      Assert_Cmpuint_Eq (Get_Value (Iter'Access), 0);
      Assert_True (not Init_Last (Iter, Empty_Set, Value'Access));
      Assert_True (not Is_Valid (Iter'Access));
      Assert_True (not Init_At (Iter, Empty_Set, 0, Value'Access));
      Assert_True (not Is_Valid (Iter'Access));

      --  A closed range makes the first and last elements easy to check.
      Add_Range_Closed (Set, 10, 20);
      Assert_True (Init_First (Iter, Set, Value'Access));
      Assert_True (Is_Valid (Iter'Access));
      Assert_Cmpuint_Eq (Value, 10);
      Assert_Cmpuint_Eq (Get_Value (Iter'Access), 10);

      Assert_True (Init_Last (Iter, Set, Value'Access));
      Assert_True (Is_Valid (Iter'Access));
      Assert_Cmpuint_Eq (Value, 20);
      Assert_Cmpuint_Eq (Get_Value (Iter'Access), 20);

      --  Init_At chooses the first member at or after its target, including
      --  when the requested value is absent or precedes the set.
      Assert_True (Init_At (Iter, Set, 15, Value'Access));
      Assert_Cmpuint_Eq (Value, 15);
      Assert_Cmpuint_Eq (Get_Value (Iter'Access), 15);
      Assert_True (Init_At (Iter, Set, 5));
      Assert_Cmpuint_Eq (Get_Value (Iter'Access), 10);

      --  This GtkAda-only helper consumes a wrapper allocated with GLib's
      --  allocator; preserve the iterator value before it invalidates Iter.
      declare
         type Iter_Access is access all Gtk_Bitset_Iter;
         function To_Iter_Access is new Ada.Unchecked_Conversion
           (System.Address, Iter_Access);
         function G_Malloc (Size : Gsize) return System.Address;
         pragma Import (C, G_Malloc, "g_malloc");

         Wrapper_Bytes : constant Gsize :=
           Gsize ((Gtk_Bitset_Iter'Size + System.Storage_Unit - 1)
                  / System.Storage_Unit);
         Wrapper : constant Iter_Access :=
           To_Iter_Access (G_Malloc (Wrapper_Bytes));
         Converted : aliased Gtk_Bitset_Iter;
      begin
         Wrapper.all := Iter;
         Converted := From_Object_Free (Wrapper);
         Assert_True (Is_Valid (Converted'Access));
         Assert_Cmpuint_Eq (Get_Value (Converted'Access), 10);
      end;

      --  No member at or after 21 exists, so the returned iterator is invalid.
      Assert_True (not Init_At (Iter, Set, 21));
      Assert_True (not Is_Valid (Iter'Access));
      Assert_Cmpuint_Eq (Get_Value (Iter'Access), 0);

      Unref (Set);
      Unref (Empty_Set);
   end Test_Iterator_Basics;

   ----------------------------------
   -- Test_Iterator_Forward_Reverse --
   ----------------------------------

   procedure Test_Iterator_Forward_Reverse is
      Set   : Gtk_Bitset := Gtk_Bitset_New_Range (10, 3);
      Iter  : aliased Gtk_Bitset_Iter;
      Value : aliased Guint := 0;
   begin
      --  Next returns each value in ascending order and supports a null output.
      Assert_True (Init_First (Iter, Set, Value'Access));
      Assert_Cmpuint_Eq (Value, 10);
      Assert_True (Next (Iter'Access, Value'Access));
      Assert_Cmpuint_Eq (Value, 11);
      Assert_True (Next (Iter'Access));
      Assert_Cmpuint_Eq (Get_Value (Iter'Access), 12);

      --  Previous walks back; moving before the first member invalidates it.
      Assert_True (Previous (Iter'Access, Value'Access));
      Assert_Cmpuint_Eq (Value, 11);
      Assert_True (Init_Last (Iter, Set, Value'Access));
      Assert_Cmpuint_Eq (Value, 12);
      Assert_True (Previous (Iter'Access, Value'Access));
      Assert_Cmpuint_Eq (Value, 11);
      Assert_True (Previous (Iter'Access, Value'Access));
      Assert_Cmpuint_Eq (Value, 10);
      Assert_True (not Previous (Iter'Access, Value'Access));
      Assert_True (not Is_Valid (Iter'Access));

      Unref (Set);
   end Test_Iterator_Forward_Reverse;

begin
   --  GtkBitset is a boxed data structure and needs no display initialization.
   Glib.Test.Init;
   Glib.Test.Add_Func ("/bitset/constructors-queries",
                       Test_Constructors_And_Queries'Unrestricted_Access);
   Glib.Test.Add_Func ("/bitset/copy-reference",
                       Test_Copy_And_Reference'Unrestricted_Access);
   Glib.Test.Add_Func ("/bitset/mutations",
                       Test_Mutations'Unrestricted_Access);
   Glib.Test.Add_Func ("/bitset/set-operations",
                       Test_Set_Operations'Unrestricted_Access);
   Glib.Test.Add_Func ("/bitset/shifts-splice",
                       Test_Shifts_And_Splice'Unrestricted_Access);
   --  These cases port the iterator checks from testsuite/c_tests/bitset.c.
   Glib.Test.Add_Func ("/bitset/iter/basic",
                       Test_Iterator_Basics'Unrestricted_Access);
   Glib.Test.Add_Func ("/bitset/iter/forward-reverse",
                       Test_Iterator_Forward_Reverse'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
