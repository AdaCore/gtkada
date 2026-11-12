--  Tests Gtk.Tree_Model_Sort by porting the sorting and conversion checks
--  from testsuite/c_tests/sortmodel.c, including its sorted-insert cases.
--  The small tree also exercises the model interface and callback wrappers.

pragma Warnings (Off, "*obsolescent*");

with Ada.Command_Line;
with Ada.Exceptions;

with Glib;                 use Glib;
with Glib.Object;
with Glib.Test;            use Glib.Test;
with Glib.Values;          use Glib.Values;
with Gdk.Content_Provider;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Main;
with Gtk.Tree_Drag_Source;
with Gtk.Tree_Model;       use Gtk.Tree_Model;
with Gtk.Tree_Model_Sort;  use Gtk.Tree_Model_Sort;
with Gtk.Tree_Sortable;
with Gtk.Tree_Store;       use Gtk.Tree_Store;
with Gtkada.Bindings;

procedure Main is
   use type Gdk.Content_Provider.Gdk_Content_Provider;
   use type Gtk.Tree_Model.Gtk_Tree_Model;
   use type Gtk.Tree_Model_Sort.Gtk_Tree_Model_Sort;
   use type Gtk.Tree_Drag_Source.Gtk_Tree_Drag_Source;
   --  User data passed through the generic callback wrappers must be released
   --  when GTK replaces or drops the associated comparison function.
   Destroyed_Data : Gint := 0;
   Visited        : Gint := 0;
   Sort_Calls     : Gint := 0;
   Sort_Changes   : Gint := 0;
   Sort_Slot_Changes : Gint := 0;
   Caught_Sort_Exceptions : Gint := 0;

   procedure Destroy_Data (Data : in out Gint);

   package Foreach_With_Data is new Gtk.Tree_Model_Sort.Foreach_User_Data
     (User_Data_Type => Gint);
   package Default_Sort_With_Data is new
     Gtk.Tree_Model_Sort.Set_Default_Sort_Func_User_Data
       (User_Data_Type => Gint,
        Destroy        => Destroy_Data);
   package Column_Sort_With_Data is new
     Gtk.Tree_Model_Sort.Set_Sort_Func_User_Data
       (User_Data_Type => Gint,
        Destroy        => Destroy_Data);
   --  Instantiate the interface generics separately so these checks exercise
   --  Gtk.Tree_Sortable's generated wrappers, not the sort-model wrappers.
   package Sortable_Default_Sort_With_Data is new
     Gtk.Tree_Sortable.Set_Default_Sort_Func_User_Data
       (User_Data_Type => Gint,
        Destroy        => Destroy_Data);
   package Sortable_Column_Sort_With_Data is new
     Gtk.Tree_Sortable.Set_Sort_Func_User_Data
       (User_Data_Type => Gint,
        Destroy        => Destroy_Data);

   --  Compare the integer column, matching the C sortmodel fixture's callback.
   function Compare_Int
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A, B  : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint;

   function Compare_Int_With_Data
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A, B  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data  : Gint) return Gint;

   function Compare_Int_With_Column_Data
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A, B  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data  : Gint) return Gint;

   function Visit_Row
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;

   function Visit_Row_With_Data
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data  : Gint) return Boolean;

   procedure On_Sort_Changed
     (Self : Gtk.Tree_Sortable.Gtk_Tree_Sortable);
   procedure On_Sort_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class);
   procedure Raise_Sort_Changed
     (Self : Gtk.Tree_Sortable.Gtk_Tree_Sortable);
   procedure Raise_Sort_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class);
   procedure Record_Sort_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Test_Model_API
   with Convention => C;
   procedure Test_Callbacks_And_Sorting
   with Convention => C;

   -----------------
   -- Destroy_Data --
   -----------------

   --  Keep a count so the tests check the generic closure's destroy path.
   procedure Destroy_Data (Data : in out Gint) is
   begin
      Destroyed_Data := Destroyed_Data + 1;
      Data := 0;
   end Destroy_Data;

   -----------------
   -- Compare_Int --
   -----------------

   function Compare_Int
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A, B  : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint
   is
   begin
      Sort_Calls := Sort_Calls + 1;
      return Get_Int (Model, A, 0) - Get_Int (Model, B, 0);
   end Compare_Int;

   --------------------------
   -- Compare_Int_With_Data --
   --------------------------

   --  The sign of Data reverses the ordering, proving that user data reaches
   --  the callback rather than only the comparison function pointer.
   function Compare_Int_With_Data
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A, B  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data  : Gint) return Gint
   is
   begin
      Sort_Calls := Sort_Calls + 1;
      return Data * (Get_Int (Model, A, 0) - Get_Int (Model, B, 0));
   end Compare_Int_With_Data;

   ---------------------------------
   -- Compare_Int_With_Column_Data --
   ---------------------------------

   --  This second generic callback drives the per-column sort registration.
   function Compare_Int_With_Column_Data
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A, B  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data  : Gint) return Gint
   is
   begin
      Sort_Calls := Sort_Calls + 1;
      return Data * (Get_Int (Model, A, 0) - Get_Int (Model, B, 0));
   end Compare_Int_With_Column_Data;

   ---------------
   -- Visit_Row --
   ---------------

   --  The callback checks the iterator/path pairing and continues traversal.
   function Visit_Row
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
   begin
      Assert_True (Get_Iter (Model, Path) = Iter);
      Visited := Visited + 1;
      return False;
   end Visit_Row;

   -------------------------
   -- Visit_Row_With_Data --
   -------------------------

   --  Verify user data on every row and stop only after the first leaf.
   function Visit_Row_With_Data
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data  : Gint) return Boolean
   is
      pragma Unreferenced (Iter);
   begin
      Assert_Cmpint_Eq (Data, 37);
      Assert_True (Path /= Null_Gtk_Tree_Path);
      Visited := Visited + 1;
      return True;
   end Visit_Row_With_Data;

   -------------------------
   -- On_Sort_Changed --
   -------------------------

   --  The signal count mirrors the C regression's sort-column notification
   --  checks, including the no-op when the selected column is unchanged.
   procedure On_Sort_Changed
     (Self : Gtk.Tree_Sortable.Gtk_Tree_Sortable)
   is
      pragma Unreferenced (Self);
   begin
      Sort_Changes := Sort_Changes + 1;
   end On_Sort_Changed;

   --------------------------
   -- On_Sort_Changed_Slot --
   --------------------------

   --  The slot overload receives its explicit GObject slot as Self.
   procedure On_Sort_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Sort_Slot_Changes := Sort_Slot_Changes + 1;
   end On_Sort_Changed_Slot;

   -------------------------
   -- Raise_Sort_Changed --
   -------------------------

   --  The signal marshaller must contain Ada exceptions before they reach C.
   procedure Raise_Sort_Changed
     (Self : Gtk.Tree_Sortable.Gtk_Tree_Sortable)
   is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "intentional sortable signal test exception";
   end Raise_Sort_Changed;

   -----------------------------
   -- Raise_Sort_Changed_Slot --
   -----------------------------

   procedure Raise_Sort_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "intentional sortable slot test exception";
   end Raise_Sort_Changed_Slot;

   ---------------------------
   -- Record_Sort_Exception --
   ---------------------------

   --  Count exceptions reported by the two GTK signal marshaller overloads.
   procedure Record_Sort_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
      pragma Unreferenced (Occurrence);
   begin
      Caught_Sort_Exceptions := Caught_Sort_Exceptions + 1;
   end Record_Sort_Exception;

   ---------------------
   -- Test_Model_API --
   ---------------------

   --  Port the sortmodel conversion and model checks: sorting changes paths,
   --  while converted iters and paths still identify the same child rows.
   procedure Test_Model_API is
      Store       : Gtk_Tree_Store;
      Sort        : Gtk_Tree_Model_Sort;
      Parent_Iter : Gtk_Tree_Iter;
      Child       : Gtk_Tree_Iter;
      Child_2     : Gtk_Tree_Iter;
      Root_30     : Gtk_Tree_Iter;
      Root_10     : Gtk_Tree_Iter;
      Root_20     : Gtk_Tree_Iter;
      New_Root    : Gtk_Tree_Iter;
      Sorted_Parent : Gtk_Tree_Iter;
      Sorted_Child  : Gtk_Tree_Iter;
      Sorted_Child_2 : Gtk_Tree_Iter;
      Sorted_Root_10 : Gtk_Tree_Iter;
      Sorted      : Gtk_Tree_Iter;
      Cache_Iter  : Gtk_Tree_Iter;
      Round_Trip  : Gtk_Tree_Iter;
      Root_Path   : Gtk_Tree_Path;
      Sorted_Path : Gtk_Tree_Path;
      Child_Path  : Gtk_Tree_Path;
      Value       : GValue;
      Sort_Id     : Gint;
      Order       : Gtk_Sort_Type;
      Model_View  : Gtk.Tree_Model.Gtk_Tree_Model;
      Drag_View   : Gtk.Tree_Drag_Source.Gtk_Tree_Drag_Source;
      Interface_Source : Gdk.Content_Provider.Gdk_Content_Provider;
      Source      : Gdk.Content_Provider.Gdk_Content_Provider;
   begin
      Gtk_New (Store, (0 => GType_Int));
      Store.Append (Parent_Iter, Null_Iter);
      Store.Set (Parent_Iter, 0, 99);
      Store.Append (Child, Parent_Iter);
      Store.Set (Child, 0, 70);
      Store.Append (Child_2, Parent_Iter);
      Store.Set (Child_2, 0, 80);
      Store.Append (Root_30, Null_Iter);
      Store.Set (Root_30, 0, 30);
      Store.Append (Root_10, Null_Iter);
      Store.Set (Root_10, 0, 10);
      Store.Append (Root_20, Null_Iter);
      Store.Set (Root_20, 0, 20);

      --  Exercise both constructor forms and the already-initialized guard.
      Gtk_New_With_Model (Sort, +Store);
      Initialize_With_Model (Sort, +Store);
      Assert_True (Get_Model (Sort) = +Store);
      Model_View := +Sort;
      Drag_View := +Sort;
      Assert_True ((-Model_View) = Sort);
      Assert_True ((-Drag_View) = Sort);

      --  Convert child iters before passing them to sorted-model methods.
      Assert_True
        (Convert_Child_Iter_To_Iter (Sort, Sorted_Parent, Parent_Iter));
      Assert_True (Convert_Child_Iter_To_Iter (Sort, Sorted_Child, Child));
      Assert_True (Convert_Child_Iter_To_Iter (Sort, Sorted_Child_2, Child_2));
      Assert_True
        (Convert_Child_Iter_To_Iter (Sort, Sorted_Root_10, Root_10));

      --  Check the basic model shape before and after sorting.
      Assert_Cmpint_Eq (Get_N_Columns (Sort), 1);
      Assert_True (Get_Column_Type (Sort, 0) = GType_Int);
      Assert_Cmpint_Eq (N_Children (Sort), 4);
      Assert_True (Has_Child (Sort, Get_Iter_First (Sort)));
      Assert_Cmpint_Eq (N_Children (Sort, Sorted_Parent), 2);
      Assert_True (Children (Sort, Sorted_Parent) /= Null_Iter);
      Assert_True (Children (Sort, Sorted_Child) = Null_Iter);
      Assert_True (Parent (Sort, Sorted_Child) = Sorted_Parent);
      Assert_True (Parent (Sort, Sorted_Root_10) = Null_Iter);
      Assert_True (Nth_Child (Sort, Sorted_Parent, 1) = Sorted_Child_2);

      Gtk.Tree_Model_Sort.Set_Sort_Column_Id
        (Sort, 0, Gtk.Enums.Sort_Ascending);
      Assert_True (Get_Sort_Column_Id (Sort, Sort_Id, Order));
      Assert_Cmpint_Eq (Sort_Id, 0);
      Assert_True (Order = Gtk.Enums.Sort_Ascending);
      Sorted := Get_Iter_First (Sort);
      Assert_Cmpint_Eq (Get_Int (Model_View, Sorted, 0), 10);
      Assert_True (Iter_Is_Valid (Sort, Sorted));
      Assert_True (Get_Iter (Sort, Gtk_Tree_Path_New_From_String ("0")) = Sorted);
      Assert_True (Get_Iter_From_String (Sort, "1") /= Null_Iter);
      Assert_True (Get_Iter (Sort, Gtk_Tree_Path_New_From_String ("99")) = Null_Iter);
      Assert_True (Get_Iter_From_String (Sort, "99") = Null_Iter);
      Assert_Cmpstr_Eq (Get_String_From_Iter (Sort, Sorted), "0");
      Sorted_Path := Get_Path (Sort, Sorted);
      Assert_Cmpstr_Eq (To_String (Sorted_Path), "0");
      Path_Free (Sorted_Path);

      --  A GValue read checks the wrapper's output parameter and typed value.
      Get_Value (Sort, Sorted, 0, Value);
      Assert_Cmpint_Eq (Get_Int (Value), 10);
      Unset (Value);

      --  Child/sorted iterator and path conversions must be mutual inverses.
      Assert_True (Convert_Child_Iter_To_Iter (Sort, Sorted, Root_10));
      Convert_Iter_To_Child_Iter (Sort, Round_Trip, Sorted);
      Assert_True (Round_Trip = Root_10);
      Root_Path := Get_Path (+Store, Root_10);
      Sorted_Path := Convert_Child_Path_To_Path (Sort, Root_Path);
      Child_Path := Convert_Path_To_Child_Path (Sort, Sorted_Path);
      Assert_Cmpstr_Eq (To_String (Sorted_Path), "0");
      Assert_Cmpstr_Eq (To_String (Child_Path), To_String (Root_Path));
      Path_Free (Root_Path);
      Path_Free (Sorted_Path);
      Path_Free (Child_Path);

      --  Exercise next/previous success and end-of-list failure paths.
      Sorted := Get_Iter_From_String (Sort, "1");
      Next (Sort, Sorted);
      Assert_Cmpint_Eq (Get_Int (Model_View, Sorted, 0), 30);
      Sorted := Get_Iter_From_String (Sort, "3");
      Next (Sort, Sorted);
      Assert_True (Sorted = Null_Iter);
      Sorted := Get_Iter_From_String (Sort, "1");
      Previous (Sort, Sorted);
      Assert_Cmpint_Eq (Get_Int (Model_View, Sorted, 0), 10);
      Sorted := Get_Iter_From_String (Sort, "0");
      Previous (Sort, Sorted);
      Assert_True (Sorted = Null_Iter);
      Assert_True (Nth_Child (Sort, Null_Iter, 40) = Null_Iter);
      Assert_True (Children (Sort, Null_Iter) /= Null_Iter);
      --  Sorted iters are not persistent even when the child store's are.
      Assert_Cmpuint_Eq (Guint (Get_Flags (Sort)), 0);

      --  The sortable interface shares the same sort state as the class API.
      Assert_True (Gtk.Tree_Sortable.Get_Sort_Column_Id
        (+Sort, Sort_Id, Order));
      Assert_Cmpint_Eq (Sort_Id, 0);

      --  The sort proxy implements GtkTreeDragSource through its child model.
      --  Insert into the child after sorting to port sortmodel.c sorted_insert.
      Store.Append (New_Root, Null_Iter);
      Store.Set (New_Root, 0, 5);
      Assert_Cmpint_Eq (N_Children (Sort), 5);
      Assert_Cmpint_Eq (Get_Int (Model_View, Get_Iter_First (Sort), 0), 5);
      Root_Path := Gtk_Tree_Path_New_From_String ("0");
      --  The interface wrappers return an owned provider and delete through
      --  the interface dispatch path; exercise its identity operator too.
      Assert_True
        (Gtk.Tree_Drag_Source."+" (Drag_View) = Drag_View);
      Assert_True (Gtk.Tree_Drag_Source.Row_Draggable (Drag_View, Root_Path));
      Interface_Source :=
        Gtk.Tree_Drag_Source.Drag_Data_Get (Drag_View, Root_Path);
      Assert_True (Interface_Source /= null);
      Glib.Object.Unref
        (Glib.Object.GObject (Interface_Source));
      Assert_True
        (Gtk.Tree_Drag_Source.Drag_Data_Delete (Drag_View, Root_Path));
      Assert_Cmpint_Eq (N_Children (Sort), 4);

      --  Keep the sort-model wrappers covered on the next row after dispatch
      --  above removed the first sorted child.
      Assert_True (Gtk.Tree_Model_Sort.Row_Draggable (Sort, Root_Path));
      Source := Gtk.Tree_Model_Sort.Drag_Data_Get (Sort, Root_Path);
      Assert_True (Source /= null);
      Assert_True (Gtk.Tree_Model_Sort.Drag_Data_Delete (Sort, Root_Path));
      Assert_Cmpint_Eq (N_Children (Sort), 3);
      Glib.Object.Unref (Glib.Object.GObject (Source));
      Path_Free (Root_Path);
      Cache_Iter := Get_Iter_First (Sort);
      Clear_Cache (Sort);
      --  The selected iter may remain valid because the proxy retains it.
      Assert_True (Iter_Is_Valid (Sort, Cache_Iter));
      Store.Clear;
      Assert_Cmpint_Eq (N_Children (Sort), 0);
      Assert_True (Get_Iter_First (Sort) = Null_Iter);
   end Test_Model_API;

   --------------------------------------
   -- Test_Callbacks_And_Sorting --
   --------------------------------------

   --  Exercise direct and generic callbacks, including closure cleanup paths,
   --  the default sort function, user data and both foreach entry points.
   procedure Test_Callbacks_And_Sorting is
      Store      : Gtk_Tree_Store;
      Sort       : Gtk_Tree_Model_Sort;
      A, B, C    : Gtk_Tree_Iter;
      Iter       : Gtk_Tree_Iter;
      Sort_Id    : Gint;
      Order      : Gtk_Sort_Type;
      Model_View : Gtk.Tree_Model.Gtk_Tree_Model;
   begin
      Gtk_New (Store, (0 => GType_Int));
      Store.Append (A, Null_Iter);
      Store.Set (A, 0, 30);
      Store.Append (B, Null_Iter);
      Store.Set (B, 0, 10);
      Store.Append (C, Null_Iter);
      Store.Set (C, 0, 20);
      Sort := Gtk_Tree_Model_Sort_New_With_Model (+Store);
      Model_View := +Sort;

      Sort_Changes := 0;
      Gtk.Tree_Sortable.On_Sort_Column_Changed
        (+Sort, On_Sort_Changed'Unrestricted_Access);
      Assert_False (Has_Default_Sort_Func (Sort));

      --  The plain foreach callback covers full traversal and early stop.
      Visited := 0;
      Foreach (Sort, Visit_Row'Unrestricted_Access);
      Assert_Cmpint_Eq (Visited, 3);
      Visited := 0;
      Foreach_With_Data.Foreach
        (Sort, Visit_Row_With_Data'Unrestricted_Access, 37);
      Assert_Cmpint_Eq (Visited, 1);

      --  Install a direct default comparator before replacing it with the
      --  generic callback below.
      Set_Default_Sort_Func (Sort, Compare_Int'Unrestricted_Access);
      Assert_True (Has_Default_Sort_Func (Sort));
      Gtk.Tree_Sortable.Set_Sort_Column_Id
        (+Sort, -1, Gtk.Enums.Sort_Ascending);
      Assert_Cmpint_Eq (Sort_Changes, 0);
      Assert_Cmpint_Ge (Sort_Calls, 1);
      Assert_Cmpint_Eq (Get_Int (Model_View, Get_Iter_First (Sort), 0), 10);
      Gtk.Tree_Model_Sort.Set_Sort_Column_Id
        (Sort, 0, Gtk.Enums.Sort_Ascending);
      Assert_Cmpint_Eq (Sort_Changes, 1);

      --  Exercise the generic default callback and its destroy notifier.
      Destroyed_Data := 0;
      Default_Sort_With_Data.Set_Default_Sort_Func
        (Sort, Compare_Int_With_Data'Unrestricted_Access, 1);
      Gtk.Tree_Sortable.Set_Sort_Column_Id
        (+Sort, -1, Gtk.Enums.Sort_Descending);
      Assert_Cmpint_Eq (Sort_Changes, 2);
      Assert_Cmpint_Eq (Get_Int (Model_View, Get_Iter_First (Sort), 0), 30);
      Gtk.Tree_Model_Sort.Set_Sort_Column_Id
        (Sort, 0, Gtk.Enums.Sort_Ascending);
      Assert_Cmpint_Eq (Sort_Changes, 3);
      Default_Sort_With_Data.Set_Default_Sort_Func (Sort, null, 1);
      Assert_Cmpint_Eq (Destroyed_Data, 1);

      --  Set and clear both direct and user-data per-column comparators.
      Set_Sort_Func (Sort, 0, Compare_Int'Unrestricted_Access);
      Gtk.Tree_Model_Sort.Set_Sort_Column_Id
        (Sort, 0, Gtk.Enums.Sort_Ascending);
      Assert_Cmpint_Eq (Get_Int (Model_View, Get_Iter_First (Sort), 0), 10);
      Column_Sort_With_Data.Set_Sort_Func
        (Sort, 0, Compare_Int_With_Column_Data'Unrestricted_Access, -1);
      --  Replacing the same column function frees the previous closure.
      Column_Sort_With_Data.Set_Sort_Func
        (Sort, 0, Compare_Int_With_Column_Data'Unrestricted_Access, 1);
      Gtk.Tree_Model_Sort.Set_Sort_Column_Id
        (Sort, 0, Gtk.Enums.Sort_Descending);
      Assert_Cmpint_Eq (Sort_Changes, 4);
      Assert_Cmpint_Eq (Get_Int (Model_View, Get_Iter_First (Sort), 0), 30);
      Assert_Cmpint_Eq (Destroyed_Data, 2);

      --  Sort-column change and reset calls are observable through the API.
      Gtk.Tree_Model_Sort.Set_Sort_Column_Id (Sort, -2, Gtk.Enums.Sort_Ascending);
      Assert_Cmpint_Eq (Sort_Changes, 5);
      Assert_False (Get_Sort_Column_Id (Sort, Sort_Id, Order));
      Assert_Cmpint_Eq (Sort_Id, -2);
      Gtk.Tree_Model_Sort.Sort_Column_Changed (Sort);
      Assert_Cmpint_Eq (Sort_Changes, 6);

      --  Reset the default callback while unsorted; rows return to child order.
      Reset_Default_Sort_Func (Sort);
      Assert_Cmpint_Eq (Get_Int (Model_View, Get_Iter_First (Sort), 0), 30);

      --  Exercise Gtk.Tree_Sortable's own non-generic setter branches and
      --  comparison trampoline on the child store. The sort-model class has
      --  parallel wrappers with different internal sorting behavior.
      declare
         Store_View : constant Gtk.Tree_Model.Gtk_Tree_Model := +Store;
         Sortable : constant Gtk.Tree_Sortable.Gtk_Tree_Sortable :=
           Gtk.Tree_Sortable."+" (Gtk.Tree_Store."+" (Store));
      begin
         Assert_False (Gtk.Tree_Sortable.Has_Default_Sort_Func (Sortable));
         Gtk.Tree_Sortable.Set_Default_Sort_Func
           (Sortable, Compare_Int'Unrestricted_Access);
         Assert_True (Gtk.Tree_Sortable.Has_Default_Sort_Func (Sortable));
         Gtk.Tree_Sortable.Set_Sort_Column_Id
           (Sortable, -1, Gtk.Enums.Sort_Ascending);
         Assert_True
           (Gtk.Tree_Sortable.Get_Sort_Column_Id
              (Sortable, Sort_Id, Order) = False);
         Assert_Cmpint_Eq (Sort_Id, -1);
         Assert_Cmpint_Eq
           (Get_Int (Store_View, Get_Iter_First (Store_View), 0), 10);
         Gtk.Tree_Sortable.Set_Sort_Column_Id
           (Sortable, 0, Gtk.Enums.Sort_Ascending);
         Gtk.Tree_Sortable.Set_Default_Sort_Func (Sortable, null);
         Assert_False (Gtk.Tree_Sortable.Has_Default_Sort_Func (Sortable));

         --  The user-data generic forwards its payload and releases the
         --  closure when the default comparator is cleared.
         Destroyed_Data := 0;
         Sortable_Default_Sort_With_Data.Set_Default_Sort_Func
           (Sortable, Compare_Int_With_Data'Unrestricted_Access, -1);
         Gtk.Tree_Sortable.Set_Sort_Column_Id
           (Sortable, -1, Gtk.Enums.Sort_Ascending);
         Assert_Cmpint_Eq
           (Get_Int (Store_View, Get_Iter_First (Store_View), 0), 30);
         Gtk.Tree_Sortable.Set_Sort_Column_Id
           (Sortable, 0, Gtk.Enums.Sort_Ascending);
         Sortable_Default_Sort_With_Data.Set_Default_Sort_Func
           (Sortable, null, 1);
         Assert_Cmpint_Eq (Destroyed_Data, 1);

         --  Cover both branches of the direct and user-data column setters.
         Gtk.Tree_Sortable.Set_Sort_Func
           (Sortable, 0, Compare_Int'Unrestricted_Access);
         Gtk.Tree_Sortable.Set_Sort_Column_Id
           (Sortable, 0, Gtk.Enums.Sort_Ascending);
         Assert_Cmpint_Eq
           (Get_Int (Store_View, Get_Iter_First (Store_View), 0), 10);
         --  GTK requires a non-null per-column comparator; leave the callback
         --  installed after checking it rather than trigger a fatal critical.
         Gtk.Tree_Sortable.Set_Sort_Column_Id
           (Sortable, -2, Gtk.Enums.Sort_Ascending);

         Sortable_Column_Sort_With_Data.Set_Sort_Func
           (Sortable, 0, Compare_Int_With_Column_Data'Unrestricted_Access, -1);
         Gtk.Tree_Sortable.Set_Sort_Column_Id
           (Sortable, 0, Gtk.Enums.Sort_Ascending);
         Assert_Cmpint_Eq
           (Get_Int (Store_View, Get_Iter_First (Store_View), 0), 30);
         Sortable_Column_Sort_With_Data.Set_Sort_Func
           (Sortable, 0, Compare_Int_With_Column_Data'Unrestricted_Access, 1);
         Assert_Cmpint_Eq (Destroyed_Data, 2);
         Gtk.Tree_Sortable.Set_Sort_Column_Id
           (Sortable, 0, Gtk.Enums.Sort_Descending);
         Assert_Cmpint_Eq
           (Get_Int (Store_View, Get_Iter_First (Store_View), 0), 30);
         Gtk.Tree_Sortable.Set_Sort_Column_Id
           (Sortable, -2, Gtk.Enums.Sort_Ascending);

         --  The slot overload uses a GObject marshaller and runs after the
         --  ordinary typed handler when the sort-column-changed signal fires.
         Sort_Slot_Changes := 0;
         Gtk.Tree_Sortable.On_Sort_Column_Changed
           (Sortable, On_Sort_Changed_Slot'Unrestricted_Access,
            Sort, After => True);
         Gtk.Tree_Sortable.Sort_Column_Changed (Sortable);
         Assert_Cmpint_Ge (Sort_Slot_Changes, 1);

         --  Both generated marshallers catch callback exceptions and report
         --  them through Gtkada.Bindings without unwinding through GLib.
         Caught_Sort_Exceptions := 0;
         Gtkada.Bindings.Set_On_Exception
           (Record_Sort_Exception'Unrestricted_Access);
         Gtk.Tree_Sortable.On_Sort_Column_Changed
           (Sortable, Raise_Sort_Changed'Unrestricted_Access);
         Gtk.Tree_Sortable.On_Sort_Column_Changed
           (Sortable, Raise_Sort_Changed_Slot'Unrestricted_Access,
            Store, After => True);
         Gtk.Tree_Sortable.Sort_Column_Changed (Sortable);
         Assert_Cmpint_Eq (Caught_Sort_Exceptions, 2);
      end;

      --  Ref/unref the row through the model interface and verify iter access.
      Iter := Get_Iter_First (Sort);
      Ref_Node (Sort, Iter);
      Assert_True (Iter_Is_Valid (Sort, Iter));
      Unref_Node (Sort, Iter);
      Assert_True (Get_Model (Sort) = +Store);
   end Test_Callbacks_And_Sorting;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/tree-model-sort/model-api", Test_Model_API'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-model-sort/callbacks-and-sorting",
      Test_Callbacks_And_Sorting'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
