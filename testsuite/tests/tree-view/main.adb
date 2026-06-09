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
with Glib.Test;         use Glib.Test;

with Gtk.Enums;         use Gtk.Enums;
with Gtk.List_Store;    use Gtk.List_Store;
with Gtk.Main;
with Gtk.Tree_Model;    use Gtk.Tree_Model;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_Store;    use Gtk.Tree_Store;
with Gtk.Tree_View;     use Gtk.Tree_View;
with Gtk.Tree_View_Column;

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

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);

   pragma Warnings (On, "*obsolescent*");
end Main;
