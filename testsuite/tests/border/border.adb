--  Port the basic border copy and free test from testsuite/c_tests/border.c,
--  including a check of Gtk.Border's Ada ownership conversion helper.

with Ada.Command_Line;

with Glib;      use Glib;
with Glib.Test; use Glib.Test;
with Gtk.Border;

procedure Border is
   --  GTK exposes constructors and copying in C, but GtkAda only binds the
   --  border type and its free operation. These imports create C-owned data
   --  so the test can exercise those existing bindings with valid ownership.
   type Gtk_Border_Access is access all Gtk.Border.Gtk_Border;
   pragma Convention (C, Gtk_Border_Access);

   function New_Border return Gtk_Border_Access;
   pragma Import (C, New_Border, "gtk_border_new");

   function Copy_Border (Border : Gtk_Border_Access) return Gtk_Border_Access;
   pragma Import (C, Copy_Border, "gtk_border_copy");

   procedure Test_Basic
   with Convention => C;

   procedure Test_Basic is
      --  Distinct values on every side catch both a bad copy and field-order
      --  mistakes in the C-compatible record representation.
      Original : constant Gtk_Border_Access := New_Border;
      Copied   : Gtk.Border.Gtk_Border;
   begin
      Original.all := (Left => 5, Right => 6, Top => 666, Bottom => 777);
      Copied := Gtk.Border.From_Object_Free (Copy_Border (Original));

      --  Check the values after From_Object_Free has copied and freed the
      --  C-allocated struct, matching the bytewise comparison in the C test.
      Assert_Cmpint_Eq (Gint (Copied.Left), 5);
      Assert_Cmpint_Eq (Gint (Copied.Right), 6);
      Assert_Cmpint_Eq (Gint (Copied.Top), 666);
      Assert_Cmpint_Eq (Gint (Copied.Bottom), 777);

      --  Free the original with the public GtkAda binding, as the C test does.
      Gtk.Border.Free (Original);
      Assert_True (Gtk.Border.Get_Type /= 0);
   end Test_Basic;
begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/border/basic", Test_Basic'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Border;
