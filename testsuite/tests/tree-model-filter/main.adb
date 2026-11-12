--  Tests Gtk.Tree_Model_Filter, porting specific_bug_679910 and
--  test_row_changed from testsuite/c_tests/filtermodel.c. The remaining
--  checks cover the filter callbacks and its GtkTreeModel interfaces.

pragma Warnings (Off, "*obsolescent*");

with Ada.Command_Line;
with Gdk.Content_Provider;
with Glib;                use Glib;
with Glib.Object;         use Glib.Object;
with Glib.Test;           use Glib.Test;
with Glib.Values;         use Glib.Values;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Main;
with Gtk.Tree_Drag_Source;
with Gtk.Tree_Model;      use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.Tree_Store;      use Gtk.Tree_Store;
with System;

procedure Main is
   use type Gtk.Tree_Model.Gtk_Tree_Model;
   use type Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter;
   use type Gtk.Tree_Drag_Source.Gtk_Tree_Drag_Source;
   use type Gdk.Content_Provider.Gdk_Content_Provider;
   use type System.Address;

   Destroyed_Data       : Gint := 0;
   Direct_Visible_Calls : Gint := 0;
   Data_Visible_Calls   : Gint := 0;
   Modify_Calls         : Gint := 0;
   Visited              : Gint := 0;
   Store_Changes        : Gint := 0;
   Filter_Changes       : Gint := 0;
   Changed_Events       : Gint := 0;
   Deleted_Events       : Gint := 0;
   Toggled_Events       : Gint := 0;
   Inserted_Events      : Gint := 0;
   Reordered_Events     : Gint := 0;
   Callback_Store       : Gtk_Tree_Store;
   Callback_Filter      : Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter;

   procedure Destroy_Int (Data : in out Gint);

   package Visible_With_Data is new
     Gtk.Tree_Model_Filter.Set_Visible_Func_User_Data
       (User_Data_Type => Gint,
        Destroy        => Destroy_Int);
   package Modify_With_Data is new
     Gtk.Tree_Model_Filter.Set_Modify_Func_User_Data
       (User_Data_Type => Gint,
        Destroy        => Destroy_Int);
   package Foreach_With_Data is new
     Gtk.Tree_Model_Filter.Foreach_User_Data
       (User_Data_Type => Gint,
        Destroy        => Destroy_Int);

   procedure Test_Model_API with Convention => C;
   procedure Test_Callbacks with Convention => C;
   procedure Test_Row_Changed with Convention => C;
   procedure Test_Signal_Emitters with Convention => C;

   function Visible_Twenty
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   function Visible_With_Divisor
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data  : Gint) return Boolean;
   procedure Modify_Direct
     (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Value  : out Glib.Values.GValue;
      Column : Glib.Gint);
   procedure Modify_With_Prefix_Data
     (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Value  : out Glib.Values.GValue;
      Column : Glib.Gint;
      Data   : Gint);
   function Visit_Row
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   function Visit_Row_With_Data
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data  : Gint) return Boolean;

   procedure On_Store_Changed
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter);
   procedure On_Filter_Changed
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter);
   procedure On_Changed_Event
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter);
   procedure On_Deleted_Event
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path);
   procedure On_Toggled_Event
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter);
   procedure On_Inserted_Event
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter);
   procedure On_Reordered_Event
     (Model     : Gtk.Tree_Model.Gtk_Tree_Model;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
      New_Order : System.Address);

   procedure Make_Tree
     (Store       : out Gtk_Tree_Store;
      Root        : out Gtk_Tree_Iter;
      Child_11    : out Gtk_Tree_Iter;
      Child_12    : out Gtk_Tree_Iter;
      Child_13    : out Gtk_Tree_Iter;
      Other_Root  : out Gtk_Tree_Iter);

   -----------------
   -- Destroy_Int --
   -----------------

   --  The generic wrappers own their copied data and must call this notifier.
   procedure Destroy_Int (Data : in out Gint) is
   begin
      Destroyed_Data := Destroyed_Data + 1;
      Data := 0;
   end Destroy_Int;

   ----------------
   -- Make_Tree --
   ----------------

   --  Two roots and three children give the model API both parent and leaf
   --  rows; column 1 drives the visible-column tests.
   procedure Make_Tree
     (Store       : out Gtk_Tree_Store;
      Root        : out Gtk_Tree_Iter;
      Child_11    : out Gtk_Tree_Iter;
      Child_12    : out Gtk_Tree_Iter;
      Child_13    : out Gtk_Tree_Iter;
      Other_Root  : out Gtk_Tree_Iter)
   is
   begin
      Gtk_New (Store, (0 => GType_Int, 1 => GType_Boolean));
      Store.Append (Root, Null_Iter);
      Store.Set (Root, 0, 10);
      Store.Set (Root, 1, True);
      Store.Append (Child_11, Root);
      Store.Set (Child_11, 0, 11);
      Store.Set (Child_11, 1, True);
      Store.Append (Child_12, Root);
      Store.Set (Child_12, 0, 12);
      Store.Set (Child_12, 1, False);
      Store.Append (Child_13, Root);
      Store.Set (Child_13, 0, 13);
      Store.Set (Child_13, 1, True);
      Store.Append (Other_Root, Null_Iter);
      Store.Set (Other_Root, 0, 20);
      Store.Set (Other_Root, 1, True);
   end Make_Tree;

   ------------------
   -- Visible_Twenty --
   ------------------

   --  Keep only value 20 so the test can distinguish callback filtering.
   function Visible_Twenty
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
   begin
      Direct_Visible_Calls := Direct_Visible_Calls + 1;
      return Get_Int (Model, Iter, 0) = 20;
   end Visible_Twenty;

   ---------------------------
   -- Visible_With_Divisor --
   ---------------------------

   --  A nontrivial user-data argument proves it reaches the C callback thunk.
   function Visible_With_Divisor
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data  : Gint) return Boolean
   is
   begin
      Data_Visible_Calls := Data_Visible_Calls + 1;
      Assert_Cmpint_Eq (Data, 10);
      return Get_Int (Model, Iter, 0) mod Data = 0;
   end Visible_With_Divisor;

   --------------------
   -- Modify_Direct --
   --------------------

   --  This callback changes an integer child value into a string display value.
   procedure Modify_Direct
     (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Value  : out Glib.Values.GValue;
      Column : Glib.Gint)
   is
      pragma Unreferenced (Model);
      Child_Iter : Gtk_Tree_Iter;
   begin
      Assert_Cmpint_Eq (Column, 0);
      Modify_Calls := Modify_Calls + 1;
      --  This callback receives a filter iter, so translate it before reading
      --  the raw value from the child store.
      Gtk.Tree_Model_Filter.Convert_Iter_To_Child_Iter
        (Callback_Filter, Child_Iter, Iter);
      Set_String
        (Value, "direct" & Gint'Image
           (Get_Int (+Callback_Store, Child_Iter, 0)));
   end Modify_Direct;

   -----------------------------
   -- Modify_With_Prefix_Data --
   -----------------------------

   --  The supplied data shifts the child value, making the closure observable.
   procedure Modify_With_Prefix_Data
     (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Value  : out Glib.Values.GValue;
      Column : Glib.Gint;
      Data   : Gint)
   is
      pragma Unreferenced (Model);
      Child_Iter : Gtk_Tree_Iter;
   begin
      Assert_Cmpint_Eq (Column, 0);
      Assert_Cmpint_Eq (Data, 5);
      Modify_Calls := Modify_Calls + 1;
      Gtk.Tree_Model_Filter.Convert_Iter_To_Child_Iter
        (Callback_Filter, Child_Iter, Iter);
      Set_String
        (Value, Gint'Image (Get_Int (+Callback_Store, Child_Iter, 0) + Data));
   end Modify_With_Prefix_Data;

   ----------------
   -- Visit_Row --
   ----------------

   --  Check that foreach supplies a usable model, path and iterator.
   function Visit_Row
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
   begin
      Assert_True (Model /= Null_Gtk_Tree_Model);
      Assert_True (Path /= Null_Gtk_Tree_Path);
      Assert_True (Iter /= Null_Iter);
      Visited := Visited + 1;
      return False;
   end Visit_Row;

   -------------------------
   -- Visit_Row_With_Data --
   -------------------------

   --  Stop after the first row to check both closure data and early termination.
   function Visit_Row_With_Data
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data  : Gint) return Boolean
   is
   begin
      Assert_True (Model /= Null_Gtk_Tree_Model);
      Assert_True (Path /= Null_Gtk_Tree_Path);
      Assert_True (Iter /= Null_Iter);
      Assert_Cmpint_Eq (Data, 37);
      Visited := Visited + 1;
      return True;
   end Visit_Row_With_Data;

   ----------------------
   -- On_Store_Changed --
   ----------------------

   --  Count child-model row changes, matching filtermodel.c's test_row_changed.
   procedure On_Store_Changed
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      pragma Unreferenced (Model);
   begin
      Assert_True (Path /= Null_Gtk_Tree_Path);
      Assert_True (Iter /= Null_Iter);
      Store_Changes := Store_Changes + 1;
   end On_Store_Changed;

   -----------------------
   -- On_Filter_Changed --
   -----------------------

   --  Filter notifications should start only after its rows have been reffed.
   procedure On_Filter_Changed
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      pragma Unreferenced (Model);
   begin
      Assert_True (Path /= Null_Gtk_Tree_Path);
      Assert_True (Iter /= Null_Iter);
      Filter_Changes := Filter_Changes + 1;
   end On_Filter_Changed;

   ----------------------
   -- On_Changed_Event --
   ----------------------

   --  The inherited row-notification wrapper must emit its matching signal.
   procedure On_Changed_Event
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      pragma Unreferenced (Model);
   begin
      Assert_True (Path /= Null_Gtk_Tree_Path);
      Assert_True (Iter /= Null_Iter);
      Changed_Events := Changed_Events + 1;
   end On_Changed_Event;

   ----------------------
   -- On_Deleted_Event --
   ----------------------

   --  Deletion carries the prior row path and no iterator.
   procedure On_Deleted_Event
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      pragma Unreferenced (Model);
   begin
      Assert_True (Path /= Null_Gtk_Tree_Path);
      Deleted_Events := Deleted_Events + 1;
   end On_Deleted_Event;

   ----------------------
   -- On_Toggled_Event --
   ----------------------

   procedure On_Toggled_Event
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      pragma Unreferenced (Model);
   begin
      Assert_True (Path /= Null_Gtk_Tree_Path);
      Assert_True (Iter /= Null_Iter);
      Toggled_Events := Toggled_Events + 1;
   end On_Toggled_Event;

   -----------------------
   -- On_Inserted_Event --
   -----------------------

   procedure On_Inserted_Event
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      pragma Unreferenced (Model);
   begin
      Assert_True (Path /= Null_Gtk_Tree_Path);
      Assert_True (Iter /= Null_Iter);
      Inserted_Events := Inserted_Events + 1;
   end On_Inserted_Event;

   ------------------------
   -- On_Reordered_Event --
   ------------------------

   --  The reorder signal includes a non-null mapping array for the two roots.
   procedure On_Reordered_Event
     (Model     : Gtk.Tree_Model.Gtk_Tree_Model;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
      New_Order : System.Address)
   is
      pragma Unreferenced (Model);
   begin
      Assert_True (Path = Null_Gtk_Tree_Path);
      Assert_True (Iter = Null_Iter);
      Assert_True (New_Order /= System.Null_Address);
      Reordered_Events := Reordered_Events + 1;
   end On_Reordered_Event;

   ---------------------
   -- Test_Model_API --
   ---------------------

   --  Port the C iterator conversion regression and check the model interface.
   procedure Test_Model_API is
      Store          : Gtk_Tree_Store;
      Root           : Gtk_Tree_Iter;
      Child_11       : Gtk_Tree_Iter;
      Child_12       : Gtk_Tree_Iter;
      Child_13       : Gtk_Tree_Iter;
      Other_Root     : Gtk_Tree_Iter;
      Filter         : Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter;
      Root_Filter    : Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter;
      Model_View     : Gtk.Tree_Model.Gtk_Tree_Model;
      Drag_View      : Gtk.Tree_Drag_Source.Gtk_Tree_Drag_Source;
      Iter           : Gtk_Tree_Iter;
      Filter_Root    : Gtk_Tree_Iter;
      Filter_Other_Root : Gtk_Tree_Iter;
      Child_Iter     : Gtk_Tree_Iter;
      Filter_Iter    : Gtk_Tree_Iter;
      Path           : Gtk_Tree_Path;
      Child_Path     : Gtk_Tree_Path;
      Filter_Path    : Gtk_Tree_Path;
      Value          : GValue;
      Source         : Gdk.Content_Provider.Gdk_Content_Provider;
   begin
      Make_Tree (Store, Root, Child_11, Child_12, Child_13, Other_Root);
      Assert_True (Gtk.Tree_Model_Filter.Get_Type /= GType_None);

      --  Exercise Gtk_New and Initialize's guard on an already-created object.
      Gtk.Tree_Model_Filter.Gtk_New (Filter, +Store);
      Gtk.Tree_Model_Filter.Initialize (Filter, +Store);
      Assert_True (Gtk.Tree_Model_Filter.Get_Model (Filter) = +Store);
      Model_View := +Filter;
      Drag_View := +Filter;
      Assert_True ((-Model_View) = Filter);
      Assert_True ((-Drag_View) = Filter);

      --  Visible-column filtering hides one child until its flag changes.
      Gtk.Tree_Model_Filter.Set_Visible_Column (Filter, 1);
      Gtk.Tree_Model_Filter.Refilter (Filter);
      Gtk.Tree_Model_Filter.Convert_Child_Iter_To_Iter
        (Filter, Filter_Root, Root);
      Gtk.Tree_Model_Filter.Convert_Child_Iter_To_Iter
        (Filter, Filter_Other_Root, Other_Root);
      Assert_True (Filter_Root /= Null_Iter);
      Assert_True (Filter_Other_Root /= Null_Iter);
      Assert_Cmpint_Eq (Gtk.Tree_Model_Filter.Get_N_Columns (Filter), 2);
      Assert_True
        (Gtk.Tree_Model_Filter.Get_Column_Type (Filter, 0) = GType_Int);
      Assert_True
        (Gtk.Tree_Model_Filter.Get_Column_Type (Filter, 1) = GType_Boolean);
      Assert_Cmpint_Eq (Gtk.Tree_Model_Filter.N_Children (Filter), 2);
      Assert_Cmpint_Eq
        (Gtk.Tree_Model_Filter.N_Children (Filter, Filter_Root), 2);
      Assert_True (Gtk.Tree_Model_Filter.Has_Child (Filter, Filter_Root));
      Assert_False
        (Gtk.Tree_Model_Filter.Has_Child (Filter, Filter_Other_Root));

      --  Round-trip paths and iters across the child and filtered models.
      Path := Gtk_Tree_Path_New_From_String ("0:0");
      Filter_Path := Gtk.Tree_Model_Filter.Convert_Child_Path_To_Path
        (Filter, Path);
      Assert_Cmpstr_Eq (To_String (Filter_Path), "0:0");
      Child_Path := Gtk.Tree_Model_Filter.Convert_Path_To_Child_Path
        (Filter, Filter_Path);
      Assert_Cmpstr_Eq (To_String (Child_Path), "0:0");
      Path_Free (Path);
      Path_Free (Filter_Path);
      Path_Free (Child_Path);

      Path := Gtk_Tree_Path_New_From_String ("0:1");
      Filter_Path := Gtk.Tree_Model_Filter.Convert_Child_Path_To_Path
        (Filter, Path);
      Assert_True (Filter_Path = Null_Gtk_Tree_Path);
      Path_Free (Path);

      Gtk.Tree_Model_Filter.Convert_Child_Iter_To_Iter
        (Filter, Filter_Iter, Child_11);
      Assert_True (Filter_Iter /= Null_Iter);
      Gtk.Tree_Model_Filter.Convert_Iter_To_Child_Iter
        (Filter, Child_Iter, Filter_Iter);
      Assert_True (Child_Iter = Child_11);
      Gtk.Tree_Model_Filter.Convert_Child_Iter_To_Iter
        (Filter, Filter_Iter, Child_12);
      Assert_True (Filter_Iter = Null_Iter);

      --  Exercise successful and failed model lookups and value access.
      Iter := Gtk.Tree_Model_Filter.Get_Iter_First (Filter);
      Assert_True (Iter /= Null_Iter);
      Assert_Cmpint_Eq (Get_Int (Model_View, Iter, 0), 10);
      Path := Gtk_Tree_Path_New_From_String ("1");
      Assert_True
        (Gtk.Tree_Model_Filter.Get_Iter (Filter, Path) /= Null_Iter);
      Path_Free (Path);
      Assert_True
        (Gtk.Tree_Model_Filter.Get_Iter_From_String (Filter, "1") /= Null_Iter);
      Assert_True
        (Gtk.Tree_Model_Filter.Get_Iter_From_String (Filter, "99") = Null_Iter);
      Path := Gtk_Tree_Path_New_From_String ("99");
      Assert_True (Gtk.Tree_Model_Filter.Get_Iter (Filter, Path) = Null_Iter);
      Path_Free (Path);
      Assert_Cmpstr_Eq
        (Gtk.Tree_Model_Filter.Get_String_From_Iter (Filter, Iter), "0");
      Path := Gtk.Tree_Model_Filter.Get_Path (Filter, Iter);
      Assert_Cmpstr_Eq (To_String (Path), "0");
      Path_Free (Path);
      Gtk.Tree_Model_Filter.Get_Value (Filter, Iter, 0, Value);
      Assert_Cmpint_Eq (Get_Int (Value), 10);
      Unset (Value);

      --  Check children, parent, next/previous and their end-of-list results.
      Iter := Gtk.Tree_Model_Filter.Children (Filter, Filter_Root);
      Assert_True (Gtk.Tree_Model_Filter.Children (Filter, Iter) = Null_Iter);
      Assert_True
        (Iter = Gtk.Tree_Model_Filter.Nth_Child (Filter, Filter_Root, 0));
      Assert_True (Gtk.Tree_Model_Filter.Parent (Filter, Iter) = Filter_Root);
      Assert_True
        (Gtk.Tree_Model_Filter.Parent
           (Filter, Gtk.Tree_Model_Filter.Get_Iter_First (Filter)) = Null_Iter);
      Gtk.Tree_Model_Filter.Next (Filter, Iter);
      Assert_Cmpint_Eq (Get_Int (Model_View, Iter, 0), 13);
      Gtk.Tree_Model_Filter.Next (Filter, Iter);
      Assert_True (Iter = Null_Iter);
      Iter := Gtk.Tree_Model_Filter.Nth_Child (Filter, Filter_Root, 1);
      Gtk.Tree_Model_Filter.Previous (Filter, Iter);
      Assert_Cmpint_Eq (Get_Int (Model_View, Iter, 0), 11);
      Gtk.Tree_Model_Filter.Previous (Filter, Iter);
      Assert_True (Iter = Null_Iter);
      Assert_True
        (Gtk.Tree_Model_Filter.Nth_Child (Filter, Filter_Root, 99) = Null_Iter);
      Assert_True (Gtk.Tree_Model_Filter.Get_Flags (Filter) = 0);

      --  Ref/unref and clear the filter's cache as the C filter tests do.
      Filter_Iter := Gtk.Tree_Model_Filter.Get_Iter_First (Filter);
      Gtk.Tree_Model_Filter.Ref_Node (Filter, Filter_Iter);
      Gtk.Tree_Model_Filter.Unref_Node (Filter, Filter_Iter);
      Gtk.Tree_Model_Filter.Clear_Cache (Filter);
      --  Exercise the length-aware reorder emitter with its two top-level rows.
      Gtk.Tree_Model_Filter.Rows_Reordered_With_Length
        (Filter, Null_Gtk_Tree_Path, Null_Iter, (0 => 1, 1 => 0), 2);
      Assert_Cmpint_Eq (Gtk.Tree_Model_Filter.N_Children (Filter), 2);

      --  A virtual root makes its child rows the filter's top level.
      Path := Gtk_Tree_Path_New_From_String ("0");
      Root_Filter := Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter_Filter_New
        (+Store, Path);
      Assert_Cmpint_Eq (Gtk.Tree_Model_Filter.N_Children (Root_Filter), 3);
      Iter := Gtk.Tree_Model_Filter.Get_Iter_First (Root_Filter);
      Assert_Cmpint_Eq (Get_Int (+Root_Filter, Iter, 0), 11);
      Path := Gtk_Tree_Path_New_From_String ("0:1");
      Filter_Path := Gtk.Tree_Model_Filter.Convert_Child_Path_To_Path
        (Root_Filter, Path);
      Assert_Cmpstr_Eq (To_String (Filter_Path), "1");
      Child_Path := Gtk.Tree_Model_Filter.Convert_Path_To_Child_Path
        (Root_Filter, Filter_Path);
      Assert_Cmpstr_Eq (To_String (Child_Path), "0:1");
      Path_Free (Path);
      Path_Free (Filter_Path);
      Path_Free (Child_Path);

      --  Tree stores provide drag source data through the filter interface.
      Path := Gtk_Tree_Path_New_From_String ("0");
      Assert_True (Gtk.Tree_Model_Filter.Row_Draggable (Filter, Path));
      Source := Gtk.Tree_Model_Filter.Drag_Data_Get (Filter, Path);
      Assert_True (Source /= null);
      Assert_True (Gtk.Tree_Model_Filter.Drag_Data_Delete (Filter, Path));
      Assert_Cmpint_Eq (Gtk.Tree_Model.N_Children (+Store), 1);
      Path_Free (Path);
      Store.Clear;
      Assert_Cmpint_Eq (Gtk.Tree_Model_Filter.N_Children (Filter), 0);
      Assert_True
        (Gtk.Tree_Model_Filter.Get_Iter_First (Filter) = Null_Iter);

      Unref (Glib.Object.GObject (Root_Filter));
      Unref (Glib.Object.GObject (Filter));
      Unref (Store);
   end Test_Model_API;

   --------------------
   -- Test_Callbacks --
   --------------------

   --  Exercise direct and generic visible/modify callbacks and both foreach
   --  entry points, including closure cleanup and early traversal stop.
   procedure Test_Callbacks is
      Store        : Gtk_Tree_Store;
      Root         : Gtk_Tree_Iter;
      Child_11     : Gtk_Tree_Iter;
      Child_12     : Gtk_Tree_Iter;
      Child_13     : Gtk_Tree_Iter;
      Other_Root   : Gtk_Tree_Iter;
      Direct       : Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter;
      With_Data    : Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter;
      Modified     : Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter;
      Data_Modified : Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter;
      Unfiltered   : Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter;
      Model_View   : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter         : Gtk_Tree_Iter;
      Value        : GValue;
      String_Types : constant GType_Array := (0 => GType_String);
   begin
      Make_Tree (Store, Root, Child_11, Child_12, Child_13, Other_Root);
      Callback_Store := Store;
      Destroyed_Data := 0;

      --  Direct and user-data visibility callbacks make different row sets.
      Direct := Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter_Filter_New (+Store);
      Direct_Visible_Calls := 0;
      Gtk.Tree_Model_Filter.Set_Visible_Func
        (Direct, Visible_Twenty'Unrestricted_Access);
      Gtk.Tree_Model_Filter.Refilter (Direct);
      Assert_Cmpint_Eq (Gtk.Tree_Model_Filter.N_Children (Direct), 1);
      Assert_Cmpint_Gt (Direct_Visible_Calls, 0);

      Data_Visible_Calls := 0;
      With_Data := Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter_Filter_New (+Store);
      Visible_With_Data.Set_Visible_Func
        (With_Data, Visible_With_Divisor'Unrestricted_Access, 10);
      Gtk.Tree_Model_Filter.Refilter (With_Data);
      Assert_Cmpint_Eq (Gtk.Tree_Model_Filter.N_Children (With_Data), 2);
      Assert_Cmpint_Gt (Data_Visible_Calls, 0);
      Unref (Glib.Object.GObject (With_Data));
      Assert_Cmpint_Eq (Destroyed_Data, 1);

      --  Without a callback every row is visible; use this model for foreach.
      Unfiltered := Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter_Filter_New
        (+Store);
      Assert_Cmpint_Eq (Gtk.Tree_Model_Filter.N_Children (Unfiltered), 2);
      --  Both modify callback forms return a value of the declared display type.
      Modified := Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter_Filter_New
        (+Store);
      Gtk.Tree_Model_Filter.Set_Modify_Func
        (Modified, String_Types, Modify_Direct'Unrestricted_Access);
      Model_View := +Modified;
      Assert_True ((-Model_View) = Modified);
      Callback_Filter := Modified;
      Iter := Gtk.Tree_Model_Filter.Get_Iter_First (Modified);
      Gtk.Tree_Model_Filter.Get_Value (Modified, Iter, 0, Value);
      Assert_Cmpstr_Eq (Get_String (Value), "direct 10");
      Unset (Value);
      Assert_Cmpint_Gt (Modify_Calls, 0);
      Assert_True
        (Gtk.Tree_Model_Filter.Get_Column_Type (Modified, 0) = GType_String);

      Data_Modified := Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter_Filter_New
        (+Store);
      Modify_With_Data.Set_Modify_Func
        (Data_Modified, String_Types,
         Modify_With_Prefix_Data'Unrestricted_Access, 5);
      Callback_Filter := Data_Modified;
      Iter := Gtk.Tree_Model_Filter.Get_Iter_First (Data_Modified);
      Gtk.Tree_Model_Filter.Get_Value (Data_Modified, Iter, 0, Value);
      Assert_Cmpstr_Eq (Get_String (Value), " 15");
      Unset (Value);
      Unref (Glib.Object.GObject (Data_Modified));
      Assert_Cmpint_Eq (Destroyed_Data, 2);

      --  Direct and generic foreach verify path/iter access and stop behavior.
      Visited := 0;
      Gtk.Tree_Model_Filter.Foreach
        (Unfiltered, Visit_Row'Unrestricted_Access);
      Assert_Cmpint_Eq (Visited, 5);
      Visited := 0;
      Foreach_With_Data.Foreach
        (Unfiltered, Visit_Row_With_Data'Unrestricted_Access, 37);
      Assert_Cmpint_Eq (Visited, 1);
      Assert_Cmpint_Eq (Destroyed_Data, 3);

      --  Release all objects to run the generic destroy callbacks.
      Unref (Glib.Object.GObject (Direct));
      Unref (Glib.Object.GObject (Modified));
      Unref (Glib.Object.GObject (Unfiltered));
      Callback_Store := null;
      Callback_Filter := null;
      Unref (Store);
   end Test_Callbacks;

   -----------------------
   -- Test_Row_Changed --
   -----------------------

   --  Port filtermodel.c's row-changed test: cached rows notify the filter,
   --  while unreffed rows notify only the child model.
   procedure Test_Row_Changed is
      Store      : Gtk_Tree_Store;
      Filter     : Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter;
      Child      : array (0 .. 2) of Gtk_Tree_Iter;
      Filter_Row : array (0 .. 2) of Gtk_Tree_Iter;
   begin
      Gtk_New (Store, (0 => GType_Int));
      Gtk.Tree_Model_Filter.Gtk_New (Filter, +Store);
      for J in Child'Range loop
         Store.Append (Child (J), Null_Iter);
         Store.Set (Child (J), 0, Gint (J));
      end loop;
      Gtk.Tree_Model.On_Row_Changed
        (+Store, On_Store_Changed'Unrestricted_Access);
      Gtk.Tree_Model.On_Row_Changed
        (+Filter, On_Filter_Changed'Unrestricted_Access);
      for J in Child'Range loop
         Gtk.Tree_Model_Filter.Convert_Child_Iter_To_Iter
           (Filter, Filter_Row (J), Child (J));
         Assert_True (Filter_Row (J) /= Null_Iter);
      end loop;

      Store_Changes := 0;
      Filter_Changes := 0;
      for J in Child'Range loop
         Store.Set (Child (J), 0, Gint (J + 10));
      end loop;
      Assert_Cmpint_Eq (Store_Changes, 3);
      Assert_Cmpint_Eq (Filter_Changes, 0);

      for J in Filter_Row'Range loop
         Gtk.Tree_Model_Filter.Ref_Node (Filter, Filter_Row (J));
      end loop;
      for J in Child'Range loop
         Store.Set (Child (J), 0, Gint (J + 20));
      end loop;
      Assert_Cmpint_Eq (Store_Changes, 6);
      Assert_Cmpint_Eq (Filter_Changes, 3);
      for J in Filter_Row'Range loop
         Gtk.Tree_Model_Filter.Unref_Node (Filter, Filter_Row (J));
      end loop;
      Unref (Glib.Object.GObject (Filter));
      Unref (Store);
   end Test_Row_Changed;

   ---------------------------
   -- Test_Signal_Emitters --
   ---------------------------

   --  Each inherited GtkTreeModel notification helper is checked through its
   --  public signal, while the backing rows remain unchanged.
   procedure Test_Signal_Emitters is
      Store      : Gtk_Tree_Store;
      Root       : Gtk_Tree_Iter;
      Child_11   : Gtk_Tree_Iter;
      Child_12   : Gtk_Tree_Iter;
      Child_13   : Gtk_Tree_Iter;
      Other_Root : Gtk_Tree_Iter;
      Filter     : Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter;
      Model_View : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter;
      Path       : Gtk_Tree_Path;
      Order      : constant Gint_Array := (1, 0);
      Reordered_Callback : constant Gtk.Tree_Model.Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Or_Null_Void :=
        Gtk.Tree_Model.Cb_Gtk_Tree_Model_Gtk_Tree_Path_Gtk_Tree_Iter_Address_Or_Null_Void'
          (On_Reordered_Event'Unrestricted_Access);
   begin
      Make_Tree (Store, Root, Child_11, Child_12, Child_13, Other_Root);
      Gtk.Tree_Model_Filter.Gtk_New (Filter, +Store);
      Model_View := +Filter;
      Gtk.Tree_Model.On_Row_Changed
        (Model_View, On_Changed_Event'Unrestricted_Access);
      Gtk.Tree_Model.On_Row_Deleted
        (Model_View, On_Deleted_Event'Unrestricted_Access);
      Gtk.Tree_Model.On_Row_Has_Child_Toggled
        (Model_View, On_Toggled_Event'Unrestricted_Access);
      Gtk.Tree_Model.On_Row_Inserted
        (Model_View, On_Inserted_Event'Unrestricted_Access);
      Gtk.Tree_Model.On_Rows_Reordered
        (Model_View, Reordered_Callback);

      Changed_Events := 0;
      Deleted_Events := 0;
      Toggled_Events := 0;
      Inserted_Events := 0;
      Reordered_Events := 0;
      Path := Gtk_Tree_Path_New_From_String ("0");
      Iter := Gtk.Tree_Model_Filter.Get_Iter_First (Filter);
      Gtk.Tree_Model_Filter.Row_Changed (Filter, Path, Iter);
      Gtk.Tree_Model_Filter.Row_Deleted (Filter, Path);
      Gtk.Tree_Model_Filter.Row_Has_Child_Toggled (Filter, Path, Iter);
      Gtk.Tree_Model_Filter.Row_Inserted (Filter, Path, Iter);
      Gtk.Tree_Model_Filter.Rows_Reordered
        (Filter, Null_Gtk_Tree_Path, Null_Iter, Order);

      Assert_Cmpint_Eq (Changed_Events, 1);
      Assert_Cmpint_Eq (Deleted_Events, 1);
      Assert_Cmpint_Eq (Toggled_Events, 1);
      Assert_Cmpint_Eq (Inserted_Events, 1);
      Assert_Cmpint_Eq (Reordered_Events, 1);
      Assert_Cmpint_Eq (Gtk.Tree_Model_Filter.N_Children (Filter), 2);
      Path_Free (Path);
      Unref (Glib.Object.GObject (Filter));
      Unref (Store);
   end Test_Signal_Emitters;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/tree-model-filter/model-api", Test_Model_API'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-model-filter/callbacks", Test_Callbacks'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-model-filter/row-changed", Test_Row_Changed'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-model-filter/signal-emitters",
      Test_Signal_Emitters'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
