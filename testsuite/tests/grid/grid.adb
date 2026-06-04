--  Ada port of GTK's testsuite/gtk/grid.c (GTK 4.22.2).
--
--  Exercises Gtk.Grid.Attach, Attach_Next_To and Query_Child, checking that
--  Attach_Next_To picks the cells we expect when there is any choice.

with Glib;             use Glib;
with Glib.Test;        use Glib.Test;
with Ada.Command_Line;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Grid;         use Gtk.Grid;
with Gtk.Label;        use Gtk.Label;
with Gtk.Main;

procedure Grid is

   procedure Test_Attach
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

begin
   Glib.Test.Init;

   --  The C test uses gtk_test_init, which also initializes GTK; widgets
   --  cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/grid/attach", Test_Attach'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Grid;
