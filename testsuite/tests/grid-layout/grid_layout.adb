--  Ada port of GTK's testsuite/gtk/grid-layout.c (GTK 4.22.2).
--
--  Drives a GtkGridLayout directly (Measure / Allocate / Get_Layout_Child)
--  over a tree of custom "gizmo" widgets whose minimum and natural sizes are
--  known, and checks the layout's reported sizes and the children's
--  allocations against the C test's expectations.

with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Glib.Test;          use Glib.Test;
with Ada.Command_Line;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Grid_Layout;    use Gtk.Grid_Layout;
with Gtk.Layout_Manager; use Gtk.Layout_Manager;
with Gtk.Main;
with Gtk.Widget;         use Gtk.Widget;
with Gtk.Window;         use Gtk.Window;

with Gizmo;              use Gizmo;

procedure Grid_Layout is

   function Grid_Child
     (Layout : not null access Gtk_Grid_Layout_Record'Class;
      Child  : not null access Gizmo_Record'Class)
      return Gtk_Grid_Layout_Child;
   --  The grid-layout child of Child within Layout.
   --
   --  Get_Layout_Child is typed as Gtk_Layout_Child, but the object it returns
   --  is really a GtkGridLayoutChild (registered as Gtk_Grid_Layout_Child in
   --  the type-conversion hooks), which is where the Set_Column / Set_Row /
   --  Set_Column_Span setters live. A plain tag-checked view conversion is
   --  therefore all that is needed; no new binding is required.

   procedure Test_Simple_Row with Convention => C;
   procedure Test_Simple_Column with Convention => C;
   procedure Test_Spans with Convention => C;
   procedure Test_Homogeneous with Convention => C;
   procedure Test_Simple_Layout with Convention => C;

   ----------------
   -- Grid_Child --
   ----------------

   function Grid_Child
     (Layout : not null access Gtk_Grid_Layout_Record'Class;
      Child  : not null access Gizmo_Record'Class)
      return Gtk_Grid_Layout_Child is
   begin
      return Gtk_Grid_Layout_Child
        (Layout.Get_Layout_Child (GObject (Child)));
   end Grid_Child;

   --------------------
   -- Test_Simple_Row --
   --------------------

   --  Create a grid with three children in a row, and verify the layout's
   --  min/nat sizes, that the children get their natural width and a shared
   --  height.

   procedure Test_Simple_Row is
      Window                 : Gtk_Window;
      Parent                 : Gizmo_Widget;
      Layout                 : Gtk_Grid_Layout;
      Child1, Child2, Child3 : Gizmo_Widget;
      Min_Size, Nat_Size     : Gint;
      Min_Base, Nat_Base     : Gint;
   begin
      Window := Gtk_Window_New;
      Gtk_New (Parent);
      Window.Set_Child (Parent);

      Layout := Gtk_Grid_Layout_New;
      Parent.Set_Layout_Manager (Layout);

      Gtk_New (Child1);
      Gtk_New (Child2);
      Gtk_New (Child3);

      Child1.Set_Sizes (10, 10, 20, 20);
      Child2.Set_Sizes (20, 20, 30, 30);
      Child3.Set_Sizes (30, 30, 40, 40);

      Child1.Set_Parent (Parent);
      Child2.Set_Parent (Parent);
      Child3.Set_Parent (Parent);

      Grid_Child (Layout, Child1).Set_Column (0);
      Grid_Child (Layout, Child2).Set_Column (1);
      Grid_Child (Layout, Child3).Set_Column (2);

      Layout.Measure
        (GObject (Parent), Orientation_Horizontal, -1,
         Min_Size, Nat_Size, Min_Base, Nat_Base);
      Assert_Cmpint_Eq (Min_Size, 10 + 20 + 30);
      Assert_Cmpint_Eq (Nat_Size, 20 + 30 + 40);

      Layout.Measure
        (GObject (Parent), Orientation_Vertical, -1,
         Min_Size, Nat_Size, Min_Base, Nat_Base);
      Assert_Cmpint_Eq (Min_Size, 30);
      Assert_Cmpint_Eq (Nat_Size, 40);

      Layout.Allocate (GObject (Parent), 90, 40, 0);

      Assert_Cmpint_Eq (Child1.Allocated_Width, 20);
      Assert_Cmpint_Eq (Child2.Allocated_Width, 30);
      Assert_Cmpint_Eq (Child3.Allocated_Width, 40);

      Assert_Cmpint_Eq (Child1.Allocated_Height, 40);
      Assert_Cmpint_Eq (Child2.Allocated_Height, 40);
      Assert_Cmpint_Eq (Child3.Allocated_Height, 40);

      Child1.Unparent;
      Child2.Unparent;
      Child3.Unparent;

      Window.Destroy;
   end Test_Simple_Row;

   -----------------------
   -- Test_Simple_Column --
   -----------------------

   --  Same as the previous test, with a column.

   procedure Test_Simple_Column is
      Window                 : Gtk_Window;
      Parent                 : Gizmo_Widget;
      Layout                 : Gtk_Grid_Layout;
      Child1, Child2, Child3 : Gizmo_Widget;
      Min_Size, Nat_Size     : Gint;
      Min_Base, Nat_Base     : Gint;
   begin
      Window := Gtk_Window_New;
      Gtk_New (Parent);
      Window.Set_Child (Parent);

      Layout := Gtk_Grid_Layout_New;
      Parent.Set_Layout_Manager (Layout);

      Gtk_New (Child1);
      Gtk_New (Child2);
      Gtk_New (Child3);

      Child1.Set_Sizes (10, 10, 20, 20);
      Child2.Set_Sizes (20, 20, 30, 30);
      Child3.Set_Sizes (30, 30, 40, 40);

      Child1.Set_Parent (Parent);
      Child2.Set_Parent (Parent);
      Child3.Set_Parent (Parent);

      Grid_Child (Layout, Child1).Set_Row (0);
      Grid_Child (Layout, Child2).Set_Row (1);
      Grid_Child (Layout, Child3).Set_Row (2);

      Layout.Measure
        (GObject (Parent), Orientation_Horizontal, -1,
         Min_Size, Nat_Size, Min_Base, Nat_Base);
      Assert_Cmpint_Eq (Min_Size, 30);
      Assert_Cmpint_Eq (Nat_Size, 40);

      Layout.Measure
        (GObject (Parent), Orientation_Vertical, -1,
         Min_Size, Nat_Size, Min_Base, Nat_Base);
      Assert_Cmpint_Eq (Min_Size, 10 + 20 + 30);
      Assert_Cmpint_Eq (Nat_Size, 20 + 30 + 40);

      Layout.Allocate (GObject (Parent), 40, 90, 0);

      Assert_Cmpint_Eq (Child1.Allocated_Width, 40);
      Assert_Cmpint_Eq (Child2.Allocated_Width, 40);
      Assert_Cmpint_Eq (Child3.Allocated_Width, 40);

      Assert_Cmpint_Eq (Child1.Allocated_Height, 20);
      Assert_Cmpint_Eq (Child2.Allocated_Height, 30);
      Assert_Cmpint_Eq (Child3.Allocated_Height, 40);

      Child1.Unparent;
      Child2.Unparent;
      Child3.Unparent;

      Window.Destroy;
   end Test_Simple_Column;

   -----------------
   -- Test_Spans --
   -----------------

   --  Create a grid with spanning children and verify min/nat sizes plus the
   --  children's natural widths.
   --
   --  +--------+-----------------+
   --  | child1 |      child2     |
   --  +--------+--------+--------+
   --  |      child3     | child4 |
   --  +-----------------+--------+

   procedure Test_Spans is
      Window                         : Gtk_Window;
      Parent                         : Gizmo_Widget;
      Layout                         : Gtk_Grid_Layout;
      Child1, Child2, Child3, Child4 : Gizmo_Widget;
      LC                             : Gtk_Grid_Layout_Child;
      Min_Size, Nat_Size             : Gint;
      Min_Base, Nat_Base             : Gint;
   begin
      Window := Gtk_Window_New;
      Gtk_New (Parent);
      Window.Set_Child (Parent);

      Layout := Gtk_Grid_Layout_New;
      Parent.Set_Layout_Manager (Layout);

      Gtk_New (Child1);
      Gtk_New (Child2);
      Gtk_New (Child3);
      Gtk_New (Child4);

      Child1.Set_Sizes (10, 10, 20, 20);
      Child2.Set_Sizes (20, 20, 30, 30);
      Child3.Set_Sizes (30, 30, 40, 40);
      Child4.Set_Sizes (30, 30, 40, 40);

      Child1.Set_Parent (Parent);
      Child2.Set_Parent (Parent);
      Child3.Set_Parent (Parent);
      Child4.Set_Parent (Parent);

      LC := Grid_Child (Layout, Child1);
      LC.Set_Row (0);
      LC.Set_Column (0);

      LC := Grid_Child (Layout, Child2);
      LC.Set_Row (0);
      LC.Set_Column (1);
      LC.Set_Column_Span (2);

      LC := Grid_Child (Layout, Child3);
      LC.Set_Row (1);
      LC.Set_Column (0);
      LC.Set_Column_Span (2);

      LC := Grid_Child (Layout, Child4);
      LC.Set_Row (1);
      LC.Set_Column (2);

      Layout.Measure
        (GObject (Parent), Orientation_Horizontal, -1,
         Min_Size, Nat_Size, Min_Base, Nat_Base);
      Assert_Cmpint_Eq (Min_Size, 60);
      Assert_Cmpint_Eq (Nat_Size, 80);

      Layout.Measure
        (GObject (Parent), Orientation_Vertical, -1,
         Min_Size, Nat_Size, Min_Base, Nat_Base);
      Assert_Cmpint_Eq (Min_Size, 50);
      Assert_Cmpint_Eq (Nat_Size, 70);

      Layout.Allocate (GObject (Parent), 80, 70, 0);

      Assert_Cmpint_Eq (Child1.Allocated_Width, 30);
      Assert_Cmpint_Eq (Child2.Allocated_Width, 50);
      Assert_Cmpint_Eq (Child3.Allocated_Width, 40);
      Assert_Cmpint_Eq (Child4.Allocated_Width, 40);

      Assert_Cmpint_Eq (Child1.Allocated_Height, 30);
      Assert_Cmpint_Eq (Child2.Allocated_Height, 30);
      Assert_Cmpint_Eq (Child3.Allocated_Height, 40);
      Assert_Cmpint_Eq (Child4.Allocated_Height, 40);

      Child1.Unparent;
      Child2.Unparent;
      Child3.Unparent;
      Child4.Unparent;

      Window.Destroy;
   end Test_Spans;

   ----------------------
   -- Test_Homogeneous --
   ----------------------

   --  Create a 2x2 homogeneous grid and verify all children get the same size.

   procedure Test_Homogeneous is
      Window                         : Gtk_Window;
      Parent                         : Gizmo_Widget;
      Layout                         : Gtk_Grid_Layout;
      Child1, Child2, Child3, Child4 : Gizmo_Widget;
      LC                             : Gtk_Grid_Layout_Child;
      Min_Size, Nat_Size             : Gint;
      Min_Base, Nat_Base             : Gint;
   begin
      Window := Gtk_Window_New;
      Gtk_New (Parent);
      Window.Set_Child (Parent);

      Layout := Gtk_Grid_Layout_New;
      Layout.Set_Row_Homogeneous (True);
      Layout.Set_Column_Homogeneous (True);
      Parent.Set_Layout_Manager (Layout);

      Gtk_New (Child1);
      Gtk_New (Child2);
      Gtk_New (Child3);
      Gtk_New (Child4);

      Child1.Set_Sizes (10, 10, 20, 20);
      Child2.Set_Sizes (20, 20, 30, 30);
      Child3.Set_Sizes (30, 30, 40, 40);
      Child4.Set_Sizes (30, 30, 40, 40);

      Child1.Set_Parent (Parent);
      Child2.Set_Parent (Parent);
      Child3.Set_Parent (Parent);
      Child4.Set_Parent (Parent);

      LC := Grid_Child (Layout, Child1);
      LC.Set_Row (0);
      LC.Set_Column (0);

      LC := Grid_Child (Layout, Child2);
      LC.Set_Row (0);
      LC.Set_Column (1);

      LC := Grid_Child (Layout, Child3);
      LC.Set_Row (1);
      LC.Set_Column (0);

      LC := Grid_Child (Layout, Child4);
      LC.Set_Row (1);
      LC.Set_Column (1);

      Layout.Measure
        (GObject (Parent), Orientation_Horizontal, -1,
         Min_Size, Nat_Size, Min_Base, Nat_Base);
      Assert_Cmpint_Eq (Min_Size, 60);
      Assert_Cmpint_Eq (Nat_Size, 80);

      Layout.Measure
        (GObject (Parent), Orientation_Vertical, -1,
         Min_Size, Nat_Size, Min_Base, Nat_Base);
      Assert_Cmpint_Eq (Min_Size, 60);
      Assert_Cmpint_Eq (Nat_Size, 80);

      Layout.Allocate (GObject (Parent), 80, 80, 0);

      Assert_Cmpint_Eq (Child1.Allocated_Width, 40);
      Assert_Cmpint_Eq (Child2.Allocated_Width, 40);
      Assert_Cmpint_Eq (Child3.Allocated_Width, 40);
      Assert_Cmpint_Eq (Child4.Allocated_Width, 40);

      Assert_Cmpint_Eq (Child1.Allocated_Height, 40);
      Assert_Cmpint_Eq (Child2.Allocated_Height, 40);
      Assert_Cmpint_Eq (Child3.Allocated_Height, 40);
      Assert_Cmpint_Eq (Child4.Allocated_Height, 40);

      Child1.Unparent;
      Child2.Unparent;
      Child3.Unparent;
      Child4.Unparent;

      Window.Destroy;
   end Test_Homogeneous;

   -----------------------
   -- Test_Simple_Layout --
   -----------------------

   --  Create a layout with three children that we also reproduce with
   --  constraints elsewhere, for comparison.
   --
   --  +--------+--------+
   --  | child1 | child2 |
   --  +--------+--------+
   --  |      child3     |
   --  +-----------------+

   procedure Test_Simple_Layout is
      Window                 : Gtk_Window;
      Parent                 : Gizmo_Widget;
      Layout                 : Gtk_Grid_Layout;
      Child1, Child2, Child3 : Gizmo_Widget;
      LC                     : Gtk_Grid_Layout_Child;
      Min_Size, Nat_Size     : Gint;
      Min_Base, Nat_Base     : Gint;
   begin
      Window := Gtk_Window_New;
      Gtk_New (Parent);
      Window.Set_Child (Parent);

      Layout := Gtk_Grid_Layout_New;
      Layout.Set_Row_Homogeneous (True);
      Layout.Set_Column_Homogeneous (True);
      Parent.Set_Layout_Manager (Layout);

      Gtk_New (Child1);
      Gtk_New (Child2);
      Gtk_New (Child3);

      Child1.Set_Sizes (10, 10, 50, 50);
      Child2.Set_Sizes (20, 20, 50, 50);
      Child3.Set_Sizes (50, 10, 50, 50);

      Child1.Set_Parent (Parent);
      Child2.Set_Parent (Parent);
      Child3.Set_Parent (Parent);

      LC := Grid_Child (Layout, Child1);
      LC.Set_Row (0);
      LC.Set_Column (0);

      LC := Grid_Child (Layout, Child2);
      LC.Set_Row (0);
      LC.Set_Column (1);

      LC := Grid_Child (Layout, Child3);
      LC.Set_Row (1);
      LC.Set_Column (0);
      LC.Set_Column_Span (2);

      Layout.Measure
        (GObject (Parent), Orientation_Horizontal, -1,
         Min_Size, Nat_Size, Min_Base, Nat_Base);
      Assert_Cmpint_Eq (Min_Size, 50);
      Assert_Cmpint_Eq (Nat_Size, 100);

      Layout.Measure
        (GObject (Parent), Orientation_Vertical, -1,
         Min_Size, Nat_Size, Min_Base, Nat_Base);
      Assert_Cmpint_Eq (Min_Size, 40);
      Assert_Cmpint_Eq (Nat_Size, 100);

      Layout.Allocate (GObject (Parent), 100, 100, 0);

      Assert_Cmpint_Eq (Child1.Allocated_Width, 50);
      Assert_Cmpint_Eq (Child2.Allocated_Width, 50);
      Assert_Cmpint_Eq (Child3.Allocated_Width, 100);

      Assert_Cmpint_Eq (Child1.Allocated_Height, 50);
      Assert_Cmpint_Eq (Child2.Allocated_Height, 50);
      Assert_Cmpint_Eq (Child3.Allocated_Height, 50);

      Child1.Unparent;
      Child2.Unparent;
      Child3.Unparent;

      Window.Destroy;
   end Test_Simple_Layout;

begin
   Glib.Test.Init;

   --  The C test uses gtk_test_init, which also initializes GTK; widgets
   --  cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/grid-layout/row", Test_Simple_Row'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/grid-layout/column", Test_Simple_Column'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/grid-layout/span", Test_Spans'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/grid-layout/homogeneous", Test_Homogeneous'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/grid-layout/simple", Test_Simple_Layout'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Grid_Layout;
