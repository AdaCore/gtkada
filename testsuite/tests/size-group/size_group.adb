--  Exercises Gtk.Size_Group. GTK's own testsuite has no sizegroup.c, so this
--  is not a port: it checks the mode accessors (both through the subprograms
--  and through Mode_Property), the widget list returned by Get_Widgets, and
--  that grouping widgets makes them request the same width.

with Glib;             use Glib;
with Glib.Test;        use Glib.Test;
with Ada.Command_Line;
with Gtk.Label;        use Gtk.Label;
with Gtk.Main;
with Gtk.Size_Group;   use Gtk.Size_Group;
with Gtk.Widget;       use Gtk.Widget;
use type Gtk.Widget.Widget_SList.GSlist;

procedure Size_Group is

   procedure Test_Mode
   with Convention => C;

   procedure Test_Widgets
   with Convention => C;

   procedure Test_Requested_Size
   with Convention => C;

   ---------------
   -- Test_Mode --
   ---------------

   procedure Test_Mode is
      Group : constant Gtk_Size_Group := Gtk_Size_Group_New (Horizontal);
   begin
      Assert_True (Group.Get_Mode = Horizontal);

      Group.Set_Mode (Both);
      Assert_True (Group.Get_Mode = Both);
      Assert_True (Get_Property (Group, Mode_Property) = Both);

      Set_Property (Group, Mode_Property, Vertical);
      Assert_True (Group.Get_Mode = Vertical);

      Set_Property (Group, Mode_Property, None);
      Assert_True (Get_Property (Group, Mode_Property) = None);

      Group.Unref;
   end Test_Mode;

   ------------------
   -- Test_Widgets --
   ------------------

   procedure Test_Widgets is
      Group : constant Gtk_Size_Group := Gtk_Size_Group_New (Horizontal);
      A     : constant Gtk_Label := Gtk_Label_New ("a");
      B     : constant Gtk_Label := Gtk_Label_New ("b");
      List  : Widget_SList.GSlist;
      Found : Boolean := False;
   begin
      Assert_Cmpuint_Eq (Widget_SList.Length (Group.Get_Widgets), 0);

      Group.Add_Widget (A);
      Group.Add_Widget (B);
      Assert_Cmpuint_Eq (Widget_SList.Length (Group.Get_Widgets), 2);

      Group.Remove_Widget (A);

      --  The list is owned by GTK: walk it, but do not free it
      List := Group.Get_Widgets;
      Assert_Cmpuint_Eq (Widget_SList.Length (List), 1);
      while List /= Widget_SList.Null_List loop
         Found := Found or else Widget_SList.Get_Data (List) = Gtk_Widget (B);
         Assert_True (Widget_SList.Get_Data (List) /= Gtk_Widget (A));
         List := Widget_SList.Next (List);
      end loop;
      Assert_True (Found);

      Group.Unref;
   end Test_Widgets;

   -------------------------
   -- Test_Requested_Size --
   -------------------------

   procedure Test_Requested_Size is
      Group        : constant Gtk_Size_Group :=
        Gtk_Size_Group_New (Horizontal);
      Short        : constant Gtk_Label := Gtk_Label_New ("a");
      Long         : constant Gtk_Label :=
        Gtk_Label_New ("a much, much longer label");
      Short_Before : Gtk_Requisition;
      Long_Before  : Gtk_Requisition;
      Short_After  : Gtk_Requisition;
      Long_After   : Gtk_Requisition;
      Unused       : Gtk_Requisition;
   begin
      Short.Get_Preferred_Size (Short_Before, Unused);
      Long.Get_Preferred_Size (Long_Before, Unused);
      Assert_Cmpint_Lt (Short_Before.Width, Long_Before.Width);

      Group.Add_Widget (Short);
      Group.Add_Widget (Long);

      Short.Get_Preferred_Size (Short_After, Unused);
      Long.Get_Preferred_Size (Long_After, Unused);
      Assert_Cmpint_Eq (Short_After.Width, Long_After.Width);
      Assert_Cmpint_Eq (Long_After.Width, Long_Before.Width);

      --  A horizontal group leaves the heights alone
      Assert_Cmpint_Eq (Short_After.Height, Short_Before.Height);

      --  Without grouping, each widget requests its own width again
      Group.Set_Mode (None);
      Short.Get_Preferred_Size (Short_After, Unused);
      Assert_Cmpint_Eq (Short_After.Width, Short_Before.Width);

      Group.Unref;
   end Test_Requested_Size;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/size-group/mode", Test_Mode'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/size-group/widgets", Test_Widgets'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/size-group/requested-size", Test_Requested_Size'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Size_Group;
