--  Ada port of GTK's testsuite/gtk/grid.c (GTK 4.22.2).
--
--  Exercises Gtk.Grid.Attach, Attach_Next_To and Query_Child, checking that
--  Attach_Next_To picks the cells we expect when there is any choice.

with Glib;             use Glib;
with Glib.Test;        use Glib.Test;
with Ada.Command_Line;
with Glib.Object;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Constraint_Target;
with Gtk.Grid;         use Gtk.Grid;
with Gtk.Label;        use Gtk.Label;
with Gtk.Main;
with Gtk.Orientable;
with Gtk.Widget;

use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Accessible.Gtk_Accessible_Role;
use type Gtk.Atcontext.Gtk_Atcontext;
use type Gtk.Grid.Gtk_Grid;
use type Gtk.Widget.Gtk_Widget;

procedure Grid is

   procedure Test_Attach
   with Convention => C;

   procedure Test_Layout_Properties
   with Convention => C;

   procedure Test_Children_And_Tracks
   with Convention => C;

   procedure Test_Accessibility_And_Interfaces
   with Convention => C;

   ----------------
   -- Test_Attach --
   ----------------

   procedure Test_Attach is
      G                        : constant Gtk_Grid := Gtk_Grid_New;
      Child                    : Gtk_Label;
      Sibling                  : Gtk_Label;
      Z                        : Gtk_Label;
      A, B                     : Gtk_Label;
      Left, Top, Width, Height : Gint;
   begin
      Child := Gtk_Label_New ("a");
      G.Attach_Next_To (Child, null, Pos_Left, 1, 1);
      G.Query_Child (Child, Left, Top, Width, Height);
      Assert_Cmpint_Eq (Left, -1);
      Assert_Cmpint_Eq (Top, 0);
      Assert_Cmpint_Eq (Width, 1);
      Assert_Cmpint_Eq (Height, 1);

      Sibling := Child;
      Child := Gtk_Label_New ("b");
      G.Attach_Next_To (Child, Sibling, Pos_Right, 2, 2);
      G.Query_Child (Child, Left, Top, Width, Height);
      Assert_Cmpint_Eq (Left, 0);
      Assert_Cmpint_Eq (Top, 0);
      Assert_Cmpint_Eq (Width, 2);
      Assert_Cmpint_Eq (Height, 2);

      --  this one should just be ignored
      Z := Gtk_Label_New ("z");
      G.Attach (Z, 4, 4, 1, 1);

      Child := Gtk_Label_New ("c");
      G.Attach_Next_To (Child, Sibling, Pos_Bottom, 3, 1);
      G.Query_Child (Child, Left, Top, Width, Height);
      Assert_Cmpint_Eq (Left, -1);
      Assert_Cmpint_Eq (Top, 1);
      Assert_Cmpint_Eq (Width, 3);
      Assert_Cmpint_Eq (Height, 1);

      Child := Gtk_Label_New ("u");
      G.Attach_Next_To (Child, Z, Pos_Left, 2, 1);
      G.Query_Child (Child, Left, Top, Width, Height);
      Assert_Cmpint_Eq (Left, 2);
      Assert_Cmpint_Eq (Top, 4);
      Assert_Cmpint_Eq (Width, 2);
      Assert_Cmpint_Eq (Height, 1);

      Child := Gtk_Label_New ("v");
      G.Attach_Next_To (Child, Z, Pos_Right, 2, 1);
      G.Query_Child (Child, Left, Top, Width, Height);
      Assert_Cmpint_Eq (Left, 5);
      Assert_Cmpint_Eq (Top, 4);
      Assert_Cmpint_Eq (Width, 2);
      Assert_Cmpint_Eq (Height, 1);

      Child := Gtk_Label_New ("x");
      G.Attach_Next_To (Child, Z, Pos_Top, 1, 2);
      G.Query_Child (Child, Left, Top, Width, Height);
      Assert_Cmpint_Eq (Left, 4);
      Assert_Cmpint_Eq (Top, 2);
      Assert_Cmpint_Eq (Width, 1);
      Assert_Cmpint_Eq (Height, 2);

      Child := Gtk_Label_New ("x");
      G.Attach_Next_To (Child, Z, Pos_Top, 1, 2);
      G.Query_Child (Child, Left, Top, Width, Height);
      Assert_Cmpint_Eq (Left, 4);
      Assert_Cmpint_Eq (Top, 2);
      Assert_Cmpint_Eq (Width, 1);
      Assert_Cmpint_Eq (Height, 2);

      Child := Gtk_Label_New ("y");
      G.Attach_Next_To (Child, Z, Pos_Bottom, 1, 2);
      G.Query_Child (Child, Left, Top, Width, Height);
      Assert_Cmpint_Eq (Left, 4);
      Assert_Cmpint_Eq (Top, 5);
      Assert_Cmpint_Eq (Width, 1);
      Assert_Cmpint_Eq (Height, 2);

      A := Gtk_Label_New ("A");
      G.Attach (A, 10, 10, 1, 1);
      B := Gtk_Label_New ("B");
      G.Attach (B, 10, 12, 1, 1);

      Child := Gtk_Label_New ("D");
      G.Attach_Next_To (Child, A, Pos_Right, 1, 3);
      G.Query_Child (Child, Left, Top, Width, Height);
      Assert_Cmpint_Eq (Left, 11);
      Assert_Cmpint_Eq (Top, 10);
      Assert_Cmpint_Eq (Width, 1);
      Assert_Cmpint_Eq (Height, 3);
   end Test_Attach;

   --  Check GtkGrid's scalar layout properties through each setter/getter
   --  pair, including the global and per-row baseline settings.
   procedure Test_Layout_Properties is
      G        : constant Gtk_Grid := Gtk_Grid_New;
      By_Proc  : Gtk_Grid;
   begin
      --  Exercise the out-parameter constructor as well as Gtk_Grid_New.
      Gtk_New (By_Proc);
      Assert_True (By_Proc /= null);
      Assert_True (By_Proc.Get_Accessible_Role =
                     Gtk.Accessible.Accessible_Role_Generic);

      --  A new grid starts with its global baseline assigned to row zero.
      Assert_Cmpint_Eq (G.Get_Baseline_Row, 0);
      G.Set_Baseline_Row (2);
      Assert_Cmpint_Eq (G.Get_Baseline_Row, 2);

      Assert_False (G.Get_Column_Homogeneous);
      G.Set_Column_Homogeneous (True);
      Assert_True (G.Get_Column_Homogeneous);
      G.Set_Column_Homogeneous (False);
      Assert_False (G.Get_Column_Homogeneous);

      Assert_Cmpuint_Eq (G.Get_Column_Spacing, 0);
      --  Nonzero spacing values are returned unchanged by the getter.
      G.Set_Column_Spacing (7);
      Assert_Cmpuint_Eq (G.Get_Column_Spacing, 7);

      Assert_True
        (G.Get_Row_Baseline_Position (1) = Baseline_Position_Center);
      G.Set_Row_Baseline_Position (1, Baseline_Position_Top);
      Assert_True
        (G.Get_Row_Baseline_Position (1) = Baseline_Position_Top);

      Assert_False (G.Get_Row_Homogeneous);
      G.Set_Row_Homogeneous (True);
      Assert_True (G.Get_Row_Homogeneous);
      G.Set_Row_Homogeneous (False);
      Assert_False (G.Get_Row_Homogeneous);

      Assert_Cmpuint_Eq (G.Get_Row_Spacing, 0);
      G.Set_Row_Spacing (9);
      Assert_Cmpuint_Eq (G.Get_Row_Spacing, 9);

      --  GtkGrid implements GtkOrientable; both directions of the pair are
      --  observable on the unrooted object without creating a display window.
      G.Set_Orientation (Orientation_Horizontal);
      Assert_True (G.Get_Orientation = Orientation_Horizontal);
      G.Set_Orientation (Orientation_Vertical);
      Assert_True (G.Get_Orientation = Orientation_Vertical);
   end Test_Layout_Properties;

   --  Verify child lookup/query and that explicit row/column edits move
   --  widgets in the documented way. The queried positions are the checks.
   procedure Test_Children_And_Tracks is
      G                        : constant Gtk_Grid := Gtk_Grid_New;
      A, B, C                  : Gtk_Label;
      Column, Row, Width, Height : Gint;
   begin
      A := Gtk_Label_New ("a");
      B := Gtk_Label_New ("b");
      C := Gtk_Label_New ("c");
      G.Attach (A, 0, 0, 2, 2);
      G.Attach (B, 2, 0);
      G.Attach (C, 0, 2);

      --  Child_At returns the same widget for each cell covered by its span.
      Assert_True (G.Get_Child_At (0, 0) = Gtk.Widget.Gtk_Widget (A));
      Assert_True (G.Get_Child_At (1, 1) = Gtk.Widget.Gtk_Widget (A));
      Assert_True (G.Get_Child_At (2, 0) = Gtk.Widget.Gtk_Widget (B));
      Assert_True (G.Get_Child_At (3, 3) = null);
      G.Query_Child (A, Column, Row, Width, Height);
      Assert_Cmpint_Eq (Column, 0);
      Assert_Cmpint_Eq (Row, 0);
      Assert_Cmpint_Eq (Width, 2);
      Assert_Cmpint_Eq (Height, 2);

      --  Inserting through a spanning child grows its span; removing that
      --  track restores the original dimensions.
      G.Insert_Column (1);
      G.Query_Child (A, Column, Row, Width, Height);
      Assert_Cmpint_Eq (Width, 3);
      G.Insert_Row (1);
      G.Query_Child (A, Column, Row, Width, Height);
      Assert_Cmpint_Eq (Height, 3);
      G.Remove_Column (1);
      G.Query_Child (A, Column, Row, Width, Height);
      Assert_Cmpint_Eq (Width, 2);
      G.Remove_Row (1);
      G.Query_Child (A, Column, Row, Width, Height);
      Assert_Cmpint_Eq (Height, 2);

      --  A neighboring insertion shifts the adjacent child by one column.
      G.Insert_Next_To (A, Pos_Right);
      G.Query_Child (B, Column, Row, Width, Height);
      Assert_Cmpint_Eq (Column, 3);

      G.Remove (B);
      Assert_True (G.Get_Child_At (3, 0) = null);
   end Test_Children_And_Tracks;

   --  Exercise GtkAccessible and all four interfaces declared by Gtk.Grid.
   --  An unrooted grid has no AT context or screen bounds in the test setup.
   procedure Test_Accessibility_And_Interfaces is
      G       : constant Gtk_Grid := Gtk_Grid_New;
      Child   : constant Gtk_Label := Gtk_Label_New ("accessible child");
      X, Y, Width, Height : Gint;
      Has_Bounds : Boolean;
   begin
      G.Attach (Child, 0, 0);
      Assert_True (G.Get_Accessible_Role =
                     Gtk.Accessible.Accessible_Role_Generic);
      Assert_Cmpstr_Eq (G.Get_Accessible_Id, "");
      Assert_True
        (G.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (G.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (G.Get_First_Accessible_Child /= Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (G.Get_At_Context = null);
      Has_Bounds := G.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);
      Assert_False
        (G.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focused));

      --  Reset calls accept valid enum values; parent/sibling updates are
      --  checked through the corresponding accessible-tree getters.
      G.Announce
        ("grid test", Gtk.Accessible.Accessible_Announcement_Priority_Low);
      G.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      G.Reset_Relation (Gtk.Accessible.Accessible_Relation_Labelled_By);
      G.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      G.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focusable);
      G.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (G.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      G.Update_Next_Accessible_Sibling
        (Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (G.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);

      --  Each interface conversion round-trips to the same Ada grid handle.
      declare
         Accessible : constant Gtk.Accessible.Gtk_Accessible := +G;
         Buildable  : constant Gtk.Buildable.Gtk_Buildable := +G;
         Constraint : constant Gtk.Constraint_Target.Gtk_Constraint_Target := +G;
         Orientable : constant Gtk.Orientable.Gtk_Orientable := +G;
      begin
         Assert_True (-Accessible = G);
         Assert_True (-Buildable = G);
         Assert_True (-Constraint = G);
         Assert_True (-Orientable = G);
      end;
   end Test_Accessibility_And_Interfaces;

begin
   Glib.Test.Init;

   --  The C test uses gtk_test_init, which also initializes GTK; widgets
   --  cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/grid/attach", Test_Attach'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/grid/layout-properties", Test_Layout_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/grid/children-and-tracks", Test_Children_And_Tracks'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/grid/accessibility-and-interfaces",
      Test_Accessibility_And_Interfaces'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Grid;
