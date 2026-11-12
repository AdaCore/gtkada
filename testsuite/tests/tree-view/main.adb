--  Ada port of GTK's testsuite/gtk/treeview.c (GTK 4.22.2).
--
--  Exercises the model/cursor/selection corners of Gtk.Tree_View that do not
--  require a realized, presented window: setting the cursor from valid and
--  invalid paths, querying paths/drop rows on a non-realized view, and the
--  Gtk.Tree_Selection counting/selection API over Gtk.List_Store and
--  Gtk.Tree_Store models.
--
--  The whole Gtk_Tree_View / Tree_Selection / List_Store / Tree_Store API is
--  marked obsolescent (deprecated since GTK 4.10), so the obsolescent warnings
--  are suppressed locally for this unit, as requested by the work item.

pragma Warnings (Off, "*obsolescent*");

with Ada.Command_Line;

with Glib;              use Glib;
with Glib.Object;
with Glib.Test;         use Glib.Test;

with Gtk.Enums;         use Gtk.Enums;
with Gtk.List_Store;    use Gtk.List_Store;
with Gtk.Main;
with Gtk.Tree_Model;    use Gtk.Tree_Model;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_Store;    use Gtk.Tree_Store;
with Gtk.Tree_View;     use Gtk.Tree_View;
with Gtk.Tree_View_Column;
with Gtk.Widget;
with System;

procedure Main is

   procedure Test_Bug_546005
   with Convention => C;

   procedure Test_Bug_539377
   with Convention => C;

   procedure Test_Select_Collapsed_Row
   with Convention => C;

   procedure Test_Selection_Count
   with Convention => C;

   procedure Test_Selection_Empty
   with Convention => C;

   procedure Test_Selection_Api
   with Convention => C;

   use type Gtk.Tree_Model.Gtk_Tree_Model;
   use type Gtk.Tree_Model.Gtk_Tree_Iter;
   use type Gtk.Widget.Gtk_Widget;
   use type Gtk.Tree_Model.Gtk_Tree_Path_List.Glist;
   use type System.Address;

   Foreach_Calls          : Natural := 0;
   Foreach_Index_Sum      : Gint := 0;
   Foreach_Data_Calls     : Natural := 0;
   Foreach_Data_Sum       : Gint := 0;
   Plain_Select_Calls     : Natural := 0;
   Plain_Selecting_Calls  : Natural := 0;
   Plain_Unselecting_Calls : Natural := 0;
   Data_Select_Calls      : Natural := 0;
   Data_Select_Sum        : Gint := 0;
   Destroyed_Data_Calls   : Natural := 0;
   Destroyed_Data_Value   : Gint := 0;
   Typed_Changed_Calls    : Natural := 0;
   Slot_Changed_Calls     : Natural := 0;

   procedure On_Selection_Changed
     (Self : access Gtk_Tree_Selection_Record'Class);
   procedure On_Selection_Slot
     (Self : access Glib.Object.GObject_Record'Class);
   procedure Destroy_Selection_Data (Data : in out Gint);

   --  Use both generic adapters to cover the callback forms with user data.
   package Foreach_With_Data is new Selected_Foreach_User_Data
     (User_Data_Type => Gint);
   package Select_With_Data is new Set_Select_Function_User_Data
     (User_Data_Type => Gint,
      Destroy        => Destroy_Selection_Data);

   procedure On_Selection_Changed
     (Self : access Gtk_Tree_Selection_Record'Class)
   is
      --  The test checks dispatch by comparing this count after selection.
      pragma Unreferenced (Self);
   begin
      Typed_Changed_Calls := Typed_Changed_Calls + 1;
   end On_Selection_Changed;

   procedure On_Selection_Slot
     (Self : access Glib.Object.GObject_Record'Class)
   is
      --  This is the GObject slot form of the changed-signal callback.
      pragma Unreferenced (Self);
   begin
      Slot_Changed_Calls := Slot_Changed_Calls + 1;
   end On_Selection_Slot;

   procedure Destroy_Selection_Data (Data : in out Gint) is
   begin
      --  Save the value so the test can check the generic destroy hook.
      Destroyed_Data_Calls := Destroyed_Data_Calls + 1;
      Destroyed_Data_Value := Data;
   end Destroy_Selection_Data;

   procedure Count_Selected
     (Model : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path;
      Iter  : Gtk_Tree_Iter);

   procedure Count_Selected_With_Data
     (Model : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path;
      Iter  : Gtk_Tree_Iter;
      Data  : Gint);

   function Allow_Selected_Path
     (Selection               : not null access Gtk_Tree_Selection_Record'Class;
      Model                   : Gtk_Tree_Model;
      Path                    : Gtk_Tree_Path;
      Path_Currently_Selected : Boolean) return Boolean;

   function Allow_Selected_Path_With_Data
     (Selection               : not null access Gtk_Tree_Selection_Record'Class;
      Model                   : Gtk_Tree_Model;
      Path                    : Gtk_Tree_Path;
      Path_Currently_Selected : Boolean;
      Data                    : Gint) return Boolean;

   procedure Count_Selected
     (Model : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path;
      Iter  : Gtk_Tree_Iter)
   is
      pragma Unreferenced (Model);
      --  Use the path index to confirm the callback sees each selected row.
      Indices : constant Gint_Array := Get_Indices (Path);
   begin
      Assert_True (Iter /= Null_Iter);
      Foreach_Calls := Foreach_Calls + 1;
      Foreach_Index_Sum := Foreach_Index_Sum + Indices (Indices'First);
   end Count_Selected;

   procedure Count_Selected_With_Data
     (Model : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path;
      Iter  : Gtk_Tree_Iter;
      Data  : Gint)
   is
      pragma Unreferenced (Model);
      --  Include Data in the sum so both callback data and paths are checked.
      Indices : constant Gint_Array := Get_Indices (Path);
   begin
      Assert_True (Iter /= Null_Iter);
      Foreach_Data_Calls := Foreach_Data_Calls + 1;
      Foreach_Data_Sum := Foreach_Data_Sum + Data + Indices (Indices'First);
   end Count_Selected_With_Data;

   function Allow_Selected_Path
     (Selection               : not null access Gtk_Tree_Selection_Record'Class;
      Model                   : Gtk_Tree_Model;
      Path                    : Gtk_Tree_Path;
      Path_Currently_Selected : Boolean) return Boolean
   is
      pragma Unreferenced (Selection, Model);
      --  Reject row 1 and count the selected/unselected callback states.
      Indices : constant Gint_Array := Get_Indices (Path);
   begin
      Plain_Select_Calls := Plain_Select_Calls + 1;
      if Path_Currently_Selected then
         Plain_Unselecting_Calls := Plain_Unselecting_Calls + 1;
      else
         Plain_Selecting_Calls := Plain_Selecting_Calls + 1;
      end if;
      return Indices (Indices'First) /= 1;
   end Allow_Selected_Path;

   function Allow_Selected_Path_With_Data
     (Selection               : not null access Gtk_Tree_Selection_Record'Class;
      Model                   : Gtk_Tree_Model;
      Path                    : Gtk_Tree_Path;
      Path_Currently_Selected : Boolean;
      Data                    : Gint) return Boolean
   is
      pragma Unreferenced (Selection, Model, Path_Currently_Selected);
      --  The test supplies 7 and rejects row 2 to verify the generic bridge.
      Indices : constant Gint_Array := Get_Indices (Path);
   begin
      Data_Select_Calls := Data_Select_Calls + 1;
      Data_Select_Sum := Data_Select_Sum + Data;
      return Data = 7 and then Indices (Indices'First) /= 2;
   end Allow_Selected_Path_With_Data;

   ----------------------
   -- Test_Bug_546005 --
   ----------------------

   --  http://bugzilla.gnome.org/show_bug.cgi?id=546005
   --  Setting the cursor from an invalid path must never crash, whether the
   --  view has no model, an empty model, or a populated model.

   procedure Test_Bug_546005 is
      View        : constant Gtk_Tree_View := Gtk_Tree_View_New;
      Store       : Gtk_List_Store;
      Iter        : Gtk_Tree_Iter;
      Path        : Gtk_Tree_Path;
      Cursor_Path : Gtk_Tree_Path;
      Focus       : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
   begin
      --  Invalid path on a tree view without model.
      Path := Gtk_Tree_Path_New_From_Indicesv ((0 => 1), 1);
      View.Set_Cursor (Path, null, False);
      Path_Free (Path);

      Gtk_New (Store, (0 => GType_String));
      View.Set_Model (+Store);

      --  Invalid path on a tree view with an empty model.
      Path := Gtk_Tree_Path_New_From_Indicesv ((0 => 1), 1);
      View.Set_Cursor (Path, null, False);
      Path_Free (Path);

      --  Valid path: insert one row, set and read back the cursor.
      Store.Append (Iter);
      Store.Set (Iter, 0, "hi");

      Path := Gtk_Tree_Path_New_From_Indicesv ((0 => 0), 1);
      View.Set_Cursor (Path, null, False);

      View.Get_Cursor (Cursor_Path, Focus);
      Assert_False (Cursor_Path.Is_Null);

      Path_Free (Path);
      Path_Free (Cursor_Path);

      --  Invalid path on a tree view with a populated model.
      Path := Gtk_Tree_Path_New_From_Indicesv ((0 => 1), 1);
      View.Set_Cursor (Path, null, False);
      Path_Free (Path);
   end Test_Bug_546005;

   ----------------------
   -- Test_Bug_539377 --
   ----------------------

   --  http://bugzilla.gnome.org/show_bug.cgi?id=539377
   --  On a non-realized view, Get_Path_At_Pos and Get_Dest_Row_At_Pos must
   --  report that there is no row at the given position.

   procedure Test_Bug_539377 is
      View      : constant Gtk_Tree_View := Gtk_Tree_View_New;
      Store     : Gtk_List_Store;
      Path      : aliased Gtk_Tree_Path;
      Pos       : aliased Gtk_Tree_View_Drop_Position;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Cell_X    : Gint;
      Cell_Y    : Gint;
      Row_Found : Boolean;
   begin
      --  Non-realized view, no model.
      View.Get_Path_At_Pos
        (10, 10, Path, Column, Cell_X, Cell_Y, Row_Found);
      Assert_False (Row_Found);
      Assert_False
        (View.Get_Dest_Row_At_Pos (10, 10, Path'Access, Pos'Access));

      --  Both outputs are optional; declining them must work too.
      Assert_False (View.Get_Dest_Row_At_Pos (10, 10));

      --  Non-realized view, with an empty model.
      Gtk_New (Store, (0 => GType_String));
      View.Set_Model (+Store);

      View.Get_Path_At_Pos
        (10, 10, Path, Column, Cell_X, Cell_Y, Row_Found);
      Assert_False (Row_Found);
      Assert_False
        (View.Get_Dest_Row_At_Pos (10, 10, Path'Access, Pos'Access));
   end Test_Bug_539377;

   --------------------------------
   -- Test_Select_Collapsed_Row --
   --------------------------------

   --  Trying to put the cursor on a collapsed child must not select anything;
   --  selecting the (visible) parent must work, and once expanded the child
   --  can be selected too.

   procedure Test_Select_Collapsed_Row is
      Store     : Gtk_Tree_Store;
      Parent    : Gtk_Tree_Iter;
      Child     : Gtk_Tree_Iter;
      View      : Gtk_Tree_View;
      Selection : Gtk_Tree_Selection;
      Path      : Gtk_Tree_Path;
   begin
      Gtk_New (Store, (0 => GType_String));
      View := Gtk_Tree_View_New_With_Model (+Store);

      Store.Append (Parent, Null_Iter);
      Store.Set (Parent, 0, "Parent");

      Store.Append (Child, Parent);
      Store.Set (Child, 0, "Child");
      Store.Append (Child, Parent);
      Store.Set (Child, 0, "Child");

      --  Try to select a collapsed child path.
      Path := Gtk_Tree_Path_New_From_Indicesv ((0, 1), 2);
      View.Set_Cursor (Path, null, False);

      Selection := View.Get_Selection;

      --  The parent must not have become selected.
      declare
         Ignore : constant Boolean := Up (Path);
      begin
         null;
      end;
      Assert_False (Selection.Path_Is_Selected (Path));
      Assert_Cmpint_Eq (Selection.Count_Selected_Rows, 0);

      --  Selecting the parent itself works.
      View.Set_Cursor (Path, null, False);
      Assert_True (Selection.Path_Is_Selected (Path));
      Assert_Cmpint_Eq (Selection.Count_Selected_Rows, 1);

      --  Expand the tree and select the child node.
      Append_Index (Path, 1);
      View.Expand_All;

      View.Set_Cursor (Path, null, False);
      Assert_True (Selection.Path_Is_Selected (Path));
      Assert_Cmpint_Eq (Selection.Count_Selected_Rows, 1);

      Path_Free (Path);
   end Test_Select_Collapsed_Row;

   --------------------------
   -- Test_Selection_Count --
   --------------------------

   --  http://bugzilla.gnome.org/show_bug.cgi?id=702957
   --  Count_Selected_Rows tracks Select_Path / Unselect_All in multiple mode,
   --  and selecting an already-selected path does not double-count it.

   procedure Test_Selection_Count is
      Store     : Gtk_List_Store;
      Iter      : Gtk_Tree_Iter;
      View      : Gtk_Tree_View;
      Selection : Gtk_Tree_Selection;
      Path      : Gtk_Tree_Path;
   begin
      Gtk_New (Store, (0 => GType_String));
      View := Gtk_Tree_View_New_With_Model (+Store);

      Store.Append (Iter);
      Store.Set (Iter, 0, "One");
      Store.Append (Iter);
      Store.Set (Iter, 0, "Two");
      Store.Append (Iter);
      Store.Set (Iter, 0, "Tree");

      Selection := View.Get_Selection;
      Selection.Set_Mode (Selection_Multiple);

      Assert_Cmpint_Eq (Selection.Count_Selected_Rows, 0);

      Path := Gtk_Tree_Path_New_From_Indicesv ((0 => 0), 1);
      Selection.Select_Path (Path);
      Path_Free (Path);
      Assert_Cmpint_Eq (Selection.Count_Selected_Rows, 1);

      Path := Gtk_Tree_Path_New_From_Indicesv ((0 => 2), 1);
      Selection.Select_Path (Path);
      Path_Free (Path);
      Assert_Cmpint_Eq (Selection.Count_Selected_Rows, 2);

      --  Re-selecting the same path must not change the count.
      Path := Gtk_Tree_Path_New_From_Indicesv ((0 => 2), 1);
      Selection.Select_Path (Path);
      Path_Free (Path);
      Assert_Cmpint_Eq (Selection.Count_Selected_Rows, 2);

      Path := Gtk_Tree_Path_New_From_Indicesv ((0 => 1), 1);
      Selection.Select_Path (Path);
      Path_Free (Path);
      Assert_Cmpint_Eq (Selection.Count_Selected_Rows, 3);

      Selection.Unselect_All;
      Assert_Cmpint_Eq (Selection.Count_Selected_Rows, 0);
   end Test_Selection_Count;

   --------------------------
   -- Test_Selection_Empty --
   --------------------------

   --  http://bugzilla.gnome.org/show_bug.cgi?id=712760
   --  Every selection operation on an empty model must leave the selection
   --  empty.

   procedure Test_Selection_Empty is
      Store     : Gtk_List_Store;
      View      : Gtk_Tree_View;
      Selection : Gtk_Tree_Selection;
      Model     : Gtk_Tree_Model;
      Iter      : Gtk_Tree_Iter;
      Path      : Gtk_Tree_Path;
   begin
      Gtk_New (Store, (0 => GType_String));
      View := Gtk_Tree_View_New_With_Model (+Store);
      Selection := View.Get_Selection;

      Selection.Get_Selected (Model, Iter);
      Assert_True (Iter = Null_Iter);
      Assert_Cmpint_Eq (Selection.Count_Selected_Rows, 0);

      Path := Gtk_Tree_Path_New_From_Indicesv ((0 => 0), 1);

      Selection.Select_Path (Path);
      Selection.Unselect_Path (Path);
      Assert_False (Selection.Path_Is_Selected (Path));

      Selection.Set_Mode (Selection_Multiple);

      Selection.Select_All;
      Assert_Cmpint_Eq (Selection.Count_Selected_Rows, 0);

      Selection.Unselect_All;
      Assert_Cmpint_Eq (Selection.Count_Selected_Rows, 0);

      Selection.Select_Range (Path, Path);
      Assert_Cmpint_Eq (Selection.Count_Selected_Rows, 0);

      Selection.Unselect_Range (Path, Path);
      Assert_Cmpint_Eq (Selection.Count_Selected_Rows, 0);

      Path_Free (Path);
   end Test_Selection_Empty;

   -------------------------
   -- Test_Selection_Api --
   -------------------------

   --  Completes the treeview.c selection coverage with selected-row lists,
   --  iterator operations, callback adapters, and the changed signal.
   --  Selection is view-side state, so all operations share one tree view.

   procedure Test_Selection_Api is
      Store     : Gtk_List_Store;
      View      : Gtk_Tree_View;
      Selection : Gtk_Tree_Selection;
      Model     : Gtk_Tree_Model;
      Iter      : Gtk_Tree_Iter;
      Path_0    : Gtk_Tree_Path;
      Path_1    : Gtk_Tree_Path;
      Path_2    : Gtk_Tree_Path;
      Paths     : Gtk.Tree_Model.Gtk_Tree_Path_List.Glist;
   begin
      --  Set up three rows so every operation has valid paths and iters.
      Gtk_New (Store, (0 => GType_String));
      View := Gtk_Tree_View_New_With_Model (+Store);
      Store.Append (Iter);
      Store.Set (Iter, 0, "zero");
      Store.Append (Iter);
      Store.Set (Iter, 0, "one");
      Store.Append (Iter);
      Store.Set (Iter, 0, "two");
      Selection := View.Get_Selection;

      --  The selection belongs to this view and starts empty in single mode.
      Assert_True (Selection.Get_Tree_View = Gtk.Widget.Gtk_Widget (View));
      Assert_True (Selection.Get_Mode = Selection_Single);
      Selection.Get_Selected_Rows (Model, Paths);
      Assert_True (Model = +Store);
      Assert_True (Paths = Gtk.Tree_Model.Gtk_Tree_Path_List.Null_List);
      Selection.Get_Selected (Model, Iter);
      Assert_True (Model = +Store);
      Assert_True (Iter = Null_Iter);

      --  Paths 0 and 2 become independent selected rows in multiple mode.
      Path_0 := Gtk_Tree_Path_New_From_Indicesv ((0 => 0), 1);
      Path_1 := Gtk_Tree_Path_New_From_Indicesv ((0 => 1), 1);
      Path_2 := Gtk_Tree_Path_New_From_Indicesv ((0 => 2), 1);

      --  Single mode exposes its selected iterator through Get_Selected.
      Selection.Select_Path (Path_1);
      Assert_True (Selection.Path_Is_Selected (Path_1));
      Selection.Get_Selected (Model, Iter);
      Assert_True (Model = +Store);
      Assert_True (Iter /= Null_Iter);
      Assert_True (Selection.Iter_Is_Selected (Iter));
      Selection.Unselect_Path (Path_1);

      --  Multiple mode exposes an independent set of selected paths.
      Selection.Set_Mode (Selection_Multiple);
      Assert_True (Selection.Get_Mode = Selection_Multiple);
      Selection.Select_Path (Path_0);
      Selection.Select_Path (Path_2);
      Assert_Cmpint_Eq (Selection.Count_Selected_Rows, 2);
      Assert_True (Selection.Path_Is_Selected (Path_0));
      Assert_False (Selection.Path_Is_Selected (Path_1));

      --  Both selected-foreach forms report the selected row paths and data.
      Foreach_Calls := 0;
      Foreach_Index_Sum := 0;
      Selection.Selected_Foreach (Count_Selected'Unrestricted_Access);
      Assert_Cmpuint_Eq (Guint (Foreach_Calls), 2);
      Assert_Cmpint_Eq (Foreach_Index_Sum, 2);
      Foreach_Data_Calls := 0;
      Foreach_Data_Sum := 0;
      Foreach_With_Data.Selected_Foreach
        (Selection, Count_Selected_With_Data'Unrestricted_Access, 5);
      Assert_Cmpuint_Eq (Guint (Foreach_Data_Calls), 2);
      Assert_Cmpint_Eq (Foreach_Data_Sum, 12);

      --  Get_Selected_Rows returns owned paths; release each path and list.
      Selection.Get_Selected_Rows (Model, Paths);
      Assert_True (Model = +Store);
      Assert_Cmpuint_Eq
        (Gtk.Tree_Model.Gtk_Tree_Path_List.Length (Paths), 2);
      Assert_Cmpint_Eq
        (Get_Indices (Gtk.Tree_Model.Gtk_Tree_Path_List.Get_Data (Paths)) (0),
         0);
      declare
         --  Path_Free owns each path; Free then releases only the GList nodes.
         First  : Gtk.Tree_Model.Gtk_Tree_Path_List.Glist := Paths;
         Second : Gtk.Tree_Model.Gtk_Tree_Path_List.Glist :=
           Gtk.Tree_Model.Gtk_Tree_Path_List.Next (Paths);
      begin
         Path_Free (Gtk.Tree_Model.Gtk_Tree_Path_List.Get_Data (First));
         Path_Free (Gtk.Tree_Model.Gtk_Tree_Path_List.Get_Data (Second));
         Gtk.Tree_Model.Gtk_Tree_Path_List.Free (Paths);
      end;

      --  Iterator selection and range selection mirror the path operations.
      Iter := Get_Iter (+Store, Path_1);
      Selection.Select_Iter (Iter);
      Assert_True (Selection.Iter_Is_Selected (Iter));
      Selection.Unselect_Iter (Iter);
      Assert_False (Selection.Iter_Is_Selected (Iter));
      Selection.Select_Range (Path_0, Path_2);
      Assert_Cmpint_Eq (Selection.Count_Selected_Rows, 3);
      Selection.Unselect_Range (Path_0, Path_2);
      Assert_Cmpint_Eq (Selection.Count_Selected_Rows, 0);

      --  Select_All selects every row in multiple mode, then clears them.
      Selection.Select_All;
      Assert_Cmpint_Eq (Selection.Count_Selected_Rows, 3);
      Selection.Unselect_All;
      Assert_Cmpint_Eq (Selection.Count_Selected_Rows, 0);

      --  The raw callback rejects row 1 and observes both selection states.
      Plain_Select_Calls := 0;
      Plain_Selecting_Calls := 0;
      Plain_Unselecting_Calls := 0;
      Selection.Set_Select_Function
        (Allow_Selected_Path'Unrestricted_Access);
      Assert_True (Selection.Get_User_Data /= System.Null_Address);
      Selection.Select_Path (Path_1);
      Assert_False (Selection.Path_Is_Selected (Path_1));
      Selection.Select_Path (Path_0);
      Assert_True (Selection.Path_Is_Selected (Path_0));
      Selection.Unselect_Path (Path_0);
      Assert_True (Plain_Select_Calls >= 3);
      Assert_True (Plain_Selecting_Calls >= 2);
      Assert_True (Plain_Unselecting_Calls >= 1);
      Selection.Set_Select_Function (null);

      --  The generic callback receives its data and destroys it on removal.
      Destroyed_Data_Calls := 0;
      Data_Select_Calls := 0;
      Data_Select_Sum := 0;
      Select_With_Data.Set_Select_Function
        (Selection, Allow_Selected_Path_With_Data'Unrestricted_Access, 7);
      Assert_True (Selection.Get_User_Data /= System.Null_Address);
      Selection.Select_Path (Path_2);
      Assert_False (Selection.Path_Is_Selected (Path_2));
      Selection.Select_Path (Path_0);
      Assert_True (Selection.Path_Is_Selected (Path_0));
      Assert_Cmpuint_Ge (Guint (Data_Select_Calls), 2);
      Assert_Cmpint_Eq (Data_Select_Sum, Gint (Data_Select_Calls) * 7);
      Select_With_Data.Set_Select_Function (Selection, null, 7);
      Assert_Cmpuint_Eq (Guint (Destroyed_Data_Calls), 1);
      Assert_Cmpint_Eq (Destroyed_Data_Value, 7);

      --  Typed and slot handlers exercise both GtkAda signal marshallers.
      Typed_Changed_Calls := 0;
      Slot_Changed_Calls := 0;
      Selection.On_Changed (On_Selection_Changed'Unrestricted_Access);
      Selection.On_Changed
        (Gtk.Tree_Selection.Cb_GObject_Void'
           (On_Selection_Slot'Unrestricted_Access),
         Slot => View, After => True);
      Selection.Select_Path (Path_1);
      Assert_True (Typed_Changed_Calls > 0);
      Assert_True (Slot_Changed_Calls > 0);

      --  Clear selected paths before releasing the test-owned path objects.
      --  The view owns its selection, while the paths were allocated here.
      Selection.Set_Select_Function (null);
      Selection.Unselect_All;
      Path_Free (Path_0);
      Path_Free (Path_1);
      Path_Free (Path_2);
   end Test_Selection_Api;

begin
   Glib.Test.Init;

   --  The C test uses gtk_test_init, which also initializes GTK; widgets
   --  cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/tree-view/cursor/bug-546005", Test_Bug_546005'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-view/cursor/bug-539377", Test_Bug_539377'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-view/selection/collapsed-row",
      Test_Select_Collapsed_Row'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-view/selection/count", Test_Selection_Count'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-view/selection/empty", Test_Selection_Empty'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-view/selection/api", Test_Selection_Api'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);

   pragma Warnings (On, "*obsolescent*");
end Main;
