--  Tests of Gtk.Flow_Box and Gtk.Flow_Box_Child. GTK's own
--  testsuite/gtk/flowbox.c has a single case, a regression test for a crash in
--  gtk_flow_box_measure(); it is not ported, so every case here is ours. The
--  label of each child carries the integer the sort and filter functions look
--  at.

with Ada.Command_Line;
with GNAT.Strings;          use GNAT.Strings;
with Glib;                  use Glib;
with Glib.List_Model;       use Glib.List_Model;
with Glib.Object;           use Glib.Object;
with Glib.Test;             use Glib.Test;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Flow_Box;          use Gtk.Flow_Box;
with Gtk.Flow_Box_Child;    use Gtk.Flow_Box_Child;
with Gtk.Label;             use Gtk.Label;
with Gtk.Main;
with Gtk.String_List;       use Gtk.String_List;
with Gtk.Widget;            use Gtk.Widget;
with System;

procedure Flow_Box is

   Count : Natural := 0;
   --  Bumped by the callbacks below

   Destroyed      : Natural := 0;
   Last_Destroyed : Integer := 0;
   Created_With   : Integer := 0;
   --  What Note_Destroyed and Make_Child saw

   procedure Note_Destroyed (Data : in out Integer);
   function Make_Child
     (Item : System.Address; User_Data : Integer) return Gtk_Widget;
   function Make_Plain_Child (Item : System.Address) return Gtk_Widget;
   function Sort_Func
     (A, B : not null access Gtk_Flow_Box_Child_Record'Class) return Gint;
   function Filter_Func
     (Child : not null access Gtk_Flow_Box_Child_Record'Class) return Boolean;
   procedure On_Selected_Changed (Self : access Gtk_Flow_Box_Record'Class);

   package Model_Binding is new Gtk.Flow_Box.Bind_Model_User_Data
     (Integer, Note_Destroyed);

   procedure Test_Order with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Selection with Convention => C;
   procedure Test_Sort_Filter with Convention => C;
   procedure Test_Bind_Model with Convention => C;

   function Value (Child : not null access Gtk_Flow_Box_Child_Record'Class)
      return Integer
   is (Integer'Value (Gtk_Label (Child.Get_Child).Get_Text));
   --  The integer the label of Child carries

   function Fill (Box : Gtk_Flow_Box; Values : Boolean) return Gtk_Flow_Box;
   --  Fill Box with 20 labelled children: I, or a scrambled value in
   --  0 .. 99 when Values is set.

   function Count_Children
     (Box : not null access Gtk_Flow_Box_Record'Class) return Natural;

   --------------------
   -- Note_Destroyed --
   --------------------

   procedure Note_Destroyed (Data : in out Integer) is
   begin
      Destroyed := Destroyed + 1;
      Last_Destroyed := Data;
   end Note_Destroyed;

   ----------------
   -- Make_Child --
   ----------------

   function Make_Child
     (Item : System.Address; User_Data : Integer) return Gtk_Widget
   is
      pragma Unreferenced (Item);
   begin
      Created_With := User_Data;
      return Gtk_Widget (Gtk_Label_New ("item"));
   end Make_Child;

   ----------------------
   -- Make_Plain_Child --
   ----------------------

   function Make_Plain_Child (Item : System.Address) return Gtk_Widget is
      pragma Unreferenced (Item);
   begin
      return Gtk_Widget (Gtk_Label_New ("item"));
   end Make_Plain_Child;

   ---------------
   -- Sort_Func --
   ---------------

   function Sort_Func
     (A, B : not null access Gtk_Flow_Box_Child_Record'Class) return Gint is
   begin
      Count := Count + 1;
      return Gint (Value (A) - Value (B));
   end Sort_Func;

   -----------------
   -- Filter_Func --
   -----------------

   function Filter_Func
     (Child : not null access Gtk_Flow_Box_Child_Record'Class) return Boolean
   is
   begin
      Count := Count + 1;
      return Value (Child) mod 2 = 0;
   end Filter_Func;

   -------------------------
   -- On_Selected_Changed --
   -------------------------

   procedure On_Selected_Changed (Self : access Gtk_Flow_Box_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Count := Count + 1;
   end On_Selected_Changed;

   ----------
   -- Fill --
   ----------

   function Fill (Box : Gtk_Flow_Box; Values : Boolean) return Gtk_Flow_Box is
      Seed : Integer := 7;
   begin
      for I in 0 .. 19 loop
         Seed := (Seed * 421 + 97) mod 1009;
         declare
            N : constant Integer := (if Values then Seed mod 100 else I);
            S : constant String := Integer'Image (N);
         begin
            Box.Insert (Gtk_Label_New (S (S'First + 1 .. S'Last)), -1);
         end;
      end loop;
      return Box;
   end Fill;

   --------------------
   -- Count_Children --
   --------------------

   function Count_Children
     (Box : not null access Gtk_Flow_Box_Record'Class) return Natural
   is
      Child : Gtk_Widget := Box.Get_First_Child;
      N     : Natural := 0;
   begin
      while Child /= null loop
         if Child.all in Gtk_Flow_Box_Child_Record'Class then
            N := N + 1;
         end if;
         Child := Child.Get_Next_Sibling;
      end loop;
      return N;
   end Count_Children;

   ----------------
   -- Test_Order --
   ----------------

   procedure Test_Order is
      Box : constant Gtk_Flow_Box := Gtk_Flow_Box_New;
   begin
      Ref_Sink (Box);

      Box.Append (Gtk_Label_New ("1"));
      Box.Append (Gtk_Label_New ("2"));
      Box.Prepend (Gtk_Label_New ("0"));
      Box.Insert (Gtk_Label_New ("9"), 1);
      Assert_Cmpint_Eq (Gint (Count_Children (Box)), 4);

      --  The children come out in the order 0, 9, 1, 2.
      Assert_Cmpint_Eq (Gint (Value (Box.Get_Child_At_Index (0))), 0);
      Assert_Cmpint_Eq (Gint (Value (Box.Get_Child_At_Index (1))), 9);
      Assert_Cmpint_Eq (Gint (Value (Box.Get_Child_At_Index (3))), 2);
      Assert_True (Box.Get_Child_At_Index (4) = null);
      Assert_Cmpint_Eq (Box.Get_Child_At_Index (1).Get_Index, 1);

      Box.Remove (Box.Get_Child_At_Index (1));
      Assert_Cmpint_Eq (Gint (Count_Children (Box)), 3);
      Assert_Cmpint_Eq (Gint (Value (Box.Get_Child_At_Index (1))), 1);

      Box.Remove_All;
      Assert_Cmpint_Eq (Gint (Count_Children (Box)), 0);

      --  A child that is in no box has no index.
      declare
         Loose : constant Gtk_Flow_Box_Child := Gtk_Flow_Box_Child_New;
      begin
         Ref_Sink (Loose);
         Assert_Cmpint_Eq (Loose.Get_Index, -1);
         Unref (Loose);
      end;

      Unref (Box);
   end Test_Order;

   ---------------------
   -- Test_Properties --
   ---------------------

   procedure Test_Properties is
      Box : constant Gtk_Flow_Box := Gtk_Flow_Box_New;
   begin
      Ref_Sink (Box);

      Assert_False (Box.Get_Homogeneous);
      Box.Set_Homogeneous (True);
      Assert_True (Box.Get_Homogeneous);

      Box.Set_Row_Spacing (7);
      Assert_Cmpuint_Eq (Box.Get_Row_Spacing, 7);
      Box.Set_Column_Spacing (9);
      Assert_Cmpuint_Eq (Box.Get_Column_Spacing, 9);

      Box.Set_Max_Children_Per_Line (6);
      Assert_Cmpuint_Eq (Box.Get_Max_Children_Per_Line, 6);
      Box.Set_Min_Children_Per_Line (2);
      Assert_Cmpuint_Eq (Box.Get_Min_Children_Per_Line, 2);

      Assert_True (Box.Get_Activate_On_Single_Click);
      Box.Set_Activate_On_Single_Click (False);
      Assert_False (Box.Get_Activate_On_Single_Click);

      Unref (Box);
   end Test_Properties;

   --------------------
   -- Test_Selection --
   --------------------

   procedure Test_Selection is
      use Gtk.Flow_Box_Child.Flow_Box_Child_List;

      Box : constant Gtk_Flow_Box := Fill (Gtk_Flow_Box_New, False);
      A   : Gtk_Flow_Box_Child;
      B   : Gtk_Flow_Box_Child;
      L   : Glist;
   begin
      Ref_Sink (Box);

      Assert_True (Box.Get_Selection_Mode = Selection_Single);
      Assert_True (Box.Get_Selected_Children = Null_List);

      Count := 0;
      Box.On_Selected_Children_Changed (On_Selected_Changed'Unrestricted_Access);

      A := Box.Get_Child_At_Index (3);
      B := Box.Get_Child_At_Index (5);
      Assert_False (A.Is_Selected);

      Box.Select_Child (A);
      Assert_True (A.Is_Selected);
      Assert_Cmpint_Eq (Gint (Count), 1);
      L := Box.Get_Selected_Children;
      Assert_Cmpint_Eq (Gint (Length (L)), 1);
      Assert_True (Get_Data (L) = A);
      Free (L);

      --  In single mode selecting another child replaces the selection.
      Box.Select_Child (B);
      Assert_False (A.Is_Selected);
      Assert_True (B.Is_Selected);

      Box.Unselect_All;
      Assert_False (B.Is_Selected);
      Assert_True (Box.Get_Selected_Children = Null_List);

      Box.Set_Selection_Mode (Selection_Multiple);
      Box.Select_All;
      L := Box.Get_Selected_Children;
      Assert_Cmpint_Eq (Gint (Length (L)), 20);
      Free (L);

      Box.Unselect_Child (A);
      Assert_False (A.Is_Selected);
      L := Box.Get_Selected_Children;
      Assert_Cmpint_Eq (Gint (Length (L)), 19);
      Free (L);

      --  None clears the selection.
      Box.Set_Selection_Mode (Selection_None);
      Assert_True (Box.Get_Selected_Children = Null_List);

      Unref (Box);
   end Test_Selection;

   ----------------------
   -- Test_Sort_Filter --
   ----------------------

   procedure Test_Sort_Filter is
      Box : constant Gtk_Flow_Box := Fill (Gtk_Flow_Box_New, True);

      procedure Check_Sorted;
      procedure Check_Sorted is
         Previous : Integer := Integer'First;
      begin
         for I in 0 .. 19 loop
            declare
               V : constant Integer := Value (Box.Get_Child_At_Index (Gint (I)));
            begin
               Assert_True (Previous <= V);
               Previous := V;
            end;
         end loop;
      end Check_Sorted;

      procedure Check_Filtered;
      procedure Check_Filtered is
         Child   : Gtk_Widget := Box.Get_First_Child;
         Visible : Natural := 0;
         Even    : Natural := 0;
      begin
         for I in 0 .. 19 loop
            if Value (Box.Get_Child_At_Index (Gint (I))) mod 2 = 0 then
               Even := Even + 1;
            end if;
         end loop;
         while Child /= null loop
            if Child.all in Gtk_Flow_Box_Child_Record'Class
              and then Child.Get_Child_Visible
            then
               Visible := Visible + 1;
            end if;
            Child := Child.Get_Next_Sibling;
         end loop;
         Assert_Cmpint_Eq (Gint (Visible), Gint (Even));
      end Check_Filtered;
   begin
      Ref_Sink (Box);

      Count := 0;
      Box.Set_Sort_Func (Sort_Func'Unrestricted_Access);
      Assert_True (Count > 0);
      Check_Sorted;

      Count := 0;
      Box.Invalidate_Sort;
      Assert_True (Count > 0);

      Count := 0;
      Box.Set_Filter_Func (Filter_Func'Unrestricted_Access);
      Assert_True (Count > 0);
      Check_Filtered;

      Count := 0;
      Box.Invalidate_Filter;
      Assert_True (Count > 0);

      Count := 0;
      Box.Get_Child_At_Index (0).Changed;
      Assert_True (Count > 0);

      Unref (Box);
   end Test_Sort_Filter;

   ----------------------
   -- Test_Bind_Model --
   ----------------------

   procedure Test_Bind_Model is
      Strings : constant Gtk_String_List :=
        Gtk_String_List_New
          ((new String'("a"), new String'("b"), new String'("c")));
      Other   : constant Gtk_String_List :=
        Gtk_String_List_New ((1 => new String'("only")));
      Box     : constant Gtk_Flow_Box := Gtk_Flow_Box_New;
   begin
      Ref_Sink (Box);

      --  The plain form keeps no data of its own, so has nothing to destroy.
      Box.Bind_Model (+Strings, Make_Plain_Child'Unrestricted_Access);
      Assert_Cmpint_Eq (Gint (Count_Children (Box)), 3);
      Box.Bind_Model (+Other, Make_Plain_Child'Unrestricted_Access);
      Assert_Cmpint_Eq (Gint (Count_Children (Box)), 1);
      Box.Bind_Model (Null_Glist_Model, null);
      Assert_Cmpint_Eq (Gint (Count_Children (Box)), 0);

      --  The generic form wraps the caller's data, and must release it,
      --  handing Destroy the caller's own value.
      Destroyed := 0;
      Model_Binding.Bind_Model
        (Box, +Strings, Make_Child'Unrestricted_Access, 42);
      Assert_Cmpint_Eq (Gint (Count_Children (Box)), 3);
      Assert_Cmpint_Eq (Gint (Created_With), 42);
      Assert_Cmpint_Eq (Gint (Destroyed), 0);

      Model_Binding.Bind_Model
        (Box, +Other, Make_Child'Unrestricted_Access, 43);
      Assert_Cmpint_Eq (Gint (Destroyed), 1);
      Assert_Cmpint_Eq (Gint (Last_Destroyed), 42);

      Model_Binding.Bind_Model (Box, Null_Glist_Model, null, 0);
      Assert_Cmpint_Eq (Gint (Count_Children (Box)), 0);
      Assert_Cmpint_Eq (Gint (Destroyed), 2);
      Assert_Cmpint_Eq (Gint (Last_Destroyed), 43);

      --  A binding still in place when the box goes away is released too.
      Model_Binding.Bind_Model
        (Box, +Strings, Make_Child'Unrestricted_Access, 44);
      Unref (Box);
      Assert_Cmpint_Eq (Gint (Destroyed), 3);
      Assert_Cmpint_Eq (Gint (Last_Destroyed), 44);

      Unref (Strings);
      Unref (Other);
   end Test_Bind_Model;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/flowbox/order", Test_Order'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/flowbox/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/flowbox/selection", Test_Selection'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/flowbox/sort-filter", Test_Sort_Filter'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/flowbox/bind-model", Test_Bind_Model'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Flow_Box;
