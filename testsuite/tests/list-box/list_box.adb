--  Port of testsuite/c_tests/listbox.c, plus the two things the C tests
--  cannot say: that the selection signals arrive with a null row when the
--  selection is cleared, and that the typed List_Box_Row_List round-trips.
--  Bind_Model has no C counterpart here either: its test checks that the
--  generic form hands its destructor the caller's own data, exactly once.
--
--  The C tests hang an integer on each label with g_object_set_data; here
--  the label's text is the integer.

with Ada.Command_Line;
with GNAT.Strings;        use GNAT.Strings;
with Glib;                use Glib;
with Glib.List_Model;     use Glib.List_Model;
with Glib.Test;           use Glib.Test;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Label;           use Gtk.Label;
with Gtk.List_Box;        use Gtk.List_Box;
with Gtk.List_Box_Row;    use Gtk.List_Box_Row;
with Gtk.Main;
with Gtk.String_List;     use Gtk.String_List;
with Gtk.Widget;          use Gtk.Widget;
with System;

procedure List_Box is

   Count : Natural := 0;
   --  Bumped by every callback below; each test resets it before use.

   Callback_Row : Gtk_List_Box_Row;
   --  The row last reported by "row-selected", null after a clear.

   procedure Test_Sort with Convention => C;
   procedure Test_Selection with Convention => C;
   procedure Test_Multi_Selection with Convention => C;
   procedure Test_Filter with Convention => C;
   procedure Test_Header with Convention => C;
   procedure Test_Activatable with Convention => C;
   procedure Test_Bind_Model with Convention => C;

   function Value (Row : not null access Gtk_List_Box_Row_Record'Class)
      return Integer
   is (Integer'Value (Gtk_Label (Row.Get_Child).Get_Text));
   --  The integer the row's label carries

   Destroyed : Natural := 0;
   --  How many times Note_Destroyed ran

   Last_Destroyed : Integer := 0;
   --  The value Note_Destroyed saw last

   Created_With : Integer := 0;
   --  The user data Make_Row was last handed

   procedure Note_Destroyed (Data : in out Integer);
   --  Destroy for the Bind_Model instance below

   function Make_Row
     (Item : System.Address; User_Data : Integer) return Gtk_Widget;
   function Make_Plain_Row (Item : System.Address) return Gtk_Widget;
   --  Create_Widget_Func for the generic and the plain Bind_Model

   function Count_Rows
     (Box : not null access Gtk_List_Box_Record'Class) return Natural;
   --  The number of rows Box holds

   package Model_Binding is new Gtk.List_Box.Bind_Model_User_Data
     (Integer, Note_Destroyed);

   function Fill (Box : Gtk_List_Box; Values : Boolean) return Gtk_List_Box;
   --  Fill Box with 100 labels. The label of row I reads I, or, when Values
   --  is set, a scrambled value in 0 .. 999 that is not I.

   procedure Check_Sorted (Box : not null access Gtk_List_Box_Record'Class);

   function Sort_Func
     (Row1, Row2 : not null access Gtk_List_Box_Row_Record'Class) return Gint;
   function Filter_Func
     (Row : not null access Gtk_List_Box_Row_Record'Class) return Boolean;
   procedure Header_Func
     (Row    : not null access Gtk_List_Box_Row_Record'Class;
      Before : access Gtk_List_Box_Row_Record'Class);
   procedure On_Row_Selected
     (Self : access Gtk_List_Box_Record'Class;
      Row  : access Gtk_List_Box_Row_Record'Class);
   procedure On_Selected_Rows_Changed
     (Self : access Gtk_List_Box_Record'Class);

   --------------------
   -- Note_Destroyed --
   --------------------

   procedure Note_Destroyed (Data : in out Integer) is
   begin
      Destroyed := Destroyed + 1;
      Last_Destroyed := Data;
   end Note_Destroyed;

   --------------
   -- Make_Row --
   --------------

   function Make_Row
     (Item : System.Address; User_Data : Integer) return Gtk_Widget
   is
      pragma Unreferenced (Item);
   begin
      Created_With := User_Data;
      return Gtk_Widget (Gtk_Label_New ("item"));
   end Make_Row;

   --------------------
   -- Make_Plain_Row --
   --------------------

   function Make_Plain_Row (Item : System.Address) return Gtk_Widget is
      pragma Unreferenced (Item);
   begin
      return Gtk_Widget (Gtk_Label_New ("item"));
   end Make_Plain_Row;

   ----------------
   -- Count_Rows --
   ----------------

   function Count_Rows
     (Box : not null access Gtk_List_Box_Record'Class) return Natural
   is
      Row   : Gtk_Widget := Box.Get_First_Child;
      Count : Natural := 0;
   begin
      while Row /= null loop
         if Row.all in Gtk_List_Box_Row_Record'Class then
            Count := Count + 1;
         end if;
         Row := Row.Get_Next_Sibling;
      end loop;
      return Count;
   end Count_Rows;

   ----------
   -- Fill --
   ----------

   function Fill (Box : Gtk_List_Box; Values : Boolean) return Gtk_List_Box is
      Seed : Integer := 7;
   begin
      for I in 0 .. 99 loop
         Seed := (Seed * 421 + 97) mod 1009;
         declare
            N : constant Integer := (if Values then Seed mod 1000 else I);
         begin
            Box.Insert
              (Gtk_Label_New (Integer'Image (N) (2 .. Integer'Image (N)'Last)),
               -1);
         end;
      end loop;
      return Box;
   end Fill;

   ---------------
   -- Sort_Func --
   ---------------

   function Sort_Func
     (Row1, Row2 : not null access Gtk_List_Box_Row_Record'Class) return Gint
   is
   begin
      Count := Count + 1;
      return Gint (Value (Row1) - Value (Row2));
   end Sort_Func;

   -----------------
   -- Filter_Func --
   -----------------

   function Filter_Func
     (Row : not null access Gtk_List_Box_Row_Record'Class) return Boolean is
   begin
      Count := Count + 1;
      return Value (Row) mod 2 = 0;
   end Filter_Func;

   -----------------
   -- Header_Func --
   -----------------

   procedure Header_Func
     (Row    : not null access Gtk_List_Box_Row_Record'Class;
      Before : access Gtk_List_Box_Row_Record'Class)
   is
      pragma Unreferenced (Before);
      N : constant Integer := Value (Row);
   begin
      Count := Count + 1;
      if N mod 2 = 0 then
         Row.Set_Header (Gtk_Label_New ("Header" & Integer'Image (N)));
      else
         Row.Set_Header (null);
      end if;
   end Header_Func;

   ---------------------
   -- On_Row_Selected --
   ---------------------

   procedure On_Row_Selected
     (Self : access Gtk_List_Box_Record'Class;
      Row  : access Gtk_List_Box_Row_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Count := Count + 1;
      Callback_Row := Gtk_List_Box_Row (Row);
   end On_Row_Selected;

   ------------------------------
   -- On_Selected_Rows_Changed --
   ------------------------------

   procedure On_Selected_Rows_Changed
     (Self : access Gtk_List_Box_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Count := Count + 1;
   end On_Selected_Rows_Changed;

   ------------------
   -- Check_Sorted --
   ------------------

   procedure Check_Sorted (Box : not null access Gtk_List_Box_Record'Class) is
      Row      : Gtk_Widget := Box.Get_First_Child;
      Previous : Integer := Integer'First;
      Seen     : Natural := 0;
   begin
      while Row /= null loop
         if Row.all in Gtk_List_Box_Row_Record'Class then
            declare
               Item : constant Gtk_List_Box_Row := Gtk_List_Box_Row (Row);
               V    : constant Integer := Value (Item);
            begin
               --  Position in the widget tree is the position in the list
               Assert_Cmpint_Eq (Item.Get_Index, Gint (Seen));
               Assert_True (Previous <= V);
               Previous := V;
               Seen := Seen + 1;
            end;
         end if;
         Row := Row.Get_Next_Sibling;
      end loop;
      Assert_Cmpint_Eq (Gint (Seen), 100);
   end Check_Sorted;

   ---------------
   -- Test_Sort --
   ---------------

   procedure Test_Sort is
      Box : constant Gtk_List_Box := Fill (Gtk_List_Box_New, True);
   begin
      Box.Ref_Sink;

      Count := 0;
      Box.Set_Sort_Func (Sort_Func'Unrestricted_Access);
      Assert_True (Count > 0);
      Check_Sorted (Box);

      Count := 0;
      Box.Invalidate_Sort;
      Assert_True (Count > 0);

      Count := 0;
      Box.Get_Row_At_Index (0).Changed;
      Assert_True (Count > 0);

      Box.Unref;
   end Test_Sort;

   --------------------
   -- Test_Selection --
   --------------------

   procedure Test_Selection is
      Box  : constant Gtk_List_Box := Fill (Gtk_List_Box_New, False);
      Row  : Gtk_List_Box_Row;
      Row2 : Gtk_List_Box_Row;
   begin
      Box.Ref_Sink;

      Assert_True (Box.Get_Selection_Mode = Selection_Single);
      Assert_True (Box.Get_Selected_Row = null);

      Box.On_Row_Selected (On_Row_Selected'Unrestricted_Access);
      Count := 0;
      Callback_Row := null;

      Row := Box.Get_Row_At_Index (20);
      Assert_False (Row.Is_Selected);
      Box.Select_Row (Row);
      Assert_True (Row.Is_Selected);
      Assert_True (Callback_Row = Row);
      Assert_Cmpint_Eq (Gint (Count), 1);
      Assert_True (Box.Get_Selected_Row = Row);

      --  Clearing the selection emits "row-selected" with a null row.
      Box.Unselect_All;
      Assert_True (Box.Get_Selected_Row = null);
      Assert_True (Callback_Row = null);
      Assert_Cmpint_Eq (Gint (Count), 2);

      Box.Select_Row (Row);
      Assert_True (Box.Get_Selected_Row = Row);
      Assert_Cmpint_Eq (Gint (Count), 3);

      --  Removing the selected row clears the selection, in browse mode too.
      Box.Set_Selection_Mode (Selection_Browse);
      Box.Remove (Row);
      Assert_True (Callback_Row = null);
      Assert_Cmpint_Eq (Gint (Count), 4);
      Assert_True (Box.Get_Selected_Row = null);

      Row := Box.Get_Row_At_Index (20);
      Box.Select_Row (Row);
      Assert_True (Row.Is_Selected);
      Assert_True (Callback_Row = Row);
      Assert_Cmpint_Eq (Gint (Count), 5);

      Box.Set_Selection_Mode (Selection_None);
      Assert_False (Row.Is_Selected);
      Assert_True (Callback_Row = null);
      Assert_Cmpint_Eq (Gint (Count), 6);
      Assert_True (Box.Get_Selected_Row = null);

      Row2 := Box.Get_Row_At_Index (20);
      Assert_Cmpint_Eq (Row2.Get_Index, 20);

      --  A row that is in no list has no index.
      declare
         Loose : constant Gtk_List_Box_Row := Gtk_List_Box_Row_New;
      begin
         Loose.Ref_Sink;
         Assert_Cmpint_Eq (Loose.Get_Index, -1);
         Loose.Unref;
      end;

      Box.Unref;
   end Test_Selection;

   ----------------------------
   -- Test_Multi_Selection --
   ----------------------------

   procedure Test_Multi_Selection is
      use Gtk.List_Box_Row.List_Box_Row_List;

      Box  : constant Gtk_List_Box := Fill (Gtk_List_Box_New, False);
      Row  : Gtk_List_Box_Row;
      Row2 : Gtk_List_Box_Row;
      L    : Glist;
   begin
      Box.Ref_Sink;

      Assert_True (Box.Get_Selection_Mode = Selection_Single);
      Assert_True (Box.Get_Selected_Rows = Null_List);

      Box.Set_Selection_Mode (Selection_Multiple);

      Box.On_Selected_Rows_Changed (On_Selected_Rows_Changed'Unrestricted_Access);
      Count := 0;

      Row := Box.Get_Row_At_Index (20);

      Box.Select_All;
      Assert_Cmpint_Eq (Gint (Count), 1);
      L := Box.Get_Selected_Rows;
      Assert_Cmpint_Eq (Gint (Length (L)), 100);
      Free (L);
      Assert_True (Row.Is_Selected);

      Box.Unselect_All;
      Assert_Cmpint_Eq (Gint (Count), 2);
      L := Box.Get_Selected_Rows;
      Assert_True (L = Null_List);
      Assert_False (Row.Is_Selected);

      Box.Select_Row (Row);
      Assert_True (Row.Is_Selected);
      Assert_Cmpint_Eq (Gint (Count), 3);
      L := Box.Get_Selected_Rows;
      Assert_Cmpint_Eq (Gint (Length (L)), 1);
      Assert_True (Get_Data (L) = Row);
      Free (L);

      Row2 := Box.Get_Row_At_Index (40);
      Assert_False (Row2.Is_Selected);
      Box.Select_Row (Row2);
      Assert_True (Row2.Is_Selected);
      Assert_Cmpint_Eq (Gint (Count), 4);
      L := Box.Get_Selected_Rows;
      Assert_Cmpint_Eq (Gint (Length (L)), 2);
      Assert_True (Get_Data (L) = Row);
      Assert_True (Get_Data (Next (L)) = Row2);
      Free (L);

      Box.Unselect_Row (Row);
      Assert_False (Row.Is_Selected);
      Assert_Cmpint_Eq (Gint (Count), 5);
      L := Box.Get_Selected_Rows;
      Assert_Cmpint_Eq (Gint (Length (L)), 1);
      Assert_True (Get_Data (L) = Row2);
      Free (L);

      Box.Unref;
   end Test_Multi_Selection;

   -----------------
   -- Test_Filter --
   -----------------

   procedure Test_Filter is
      Box : constant Gtk_List_Box := Fill (Gtk_List_Box_New, False);

      procedure Check_Filtered;
      procedure Check_Filtered is
         Row     : Gtk_Widget := Box.Get_First_Child;
         Visible : Natural := 0;
      begin
         while Row /= null loop
            if Row.all in Gtk_List_Box_Row_Record'Class
              and then Row.Get_Child_Visible
            then
               Visible := Visible + 1;
            end if;
            Row := Row.Get_Next_Sibling;
         end loop;
         Assert_Cmpint_Eq (Gint (Visible), 50);
      end Check_Filtered;
   begin
      Box.Ref_Sink;

      Assert_True (Box.Get_Selection_Mode = Selection_Single);
      Assert_True (Box.Get_Selected_Row = null);

      Count := 0;
      Box.Set_Filter_Func (Filter_Func'Unrestricted_Access);
      Assert_True (Count > 0);
      Check_Filtered;

      Count := 0;
      Box.Invalidate_Filter;
      Assert_True (Count > 0);

      Count := 0;
      Box.Get_Row_At_Index (0).Changed;
      Assert_True (Count > 0);

      Box.Unref;
   end Test_Filter;

   -----------------
   -- Test_Header --
   -----------------

   procedure Test_Header is
      Box : constant Gtk_List_Box := Fill (Gtk_List_Box_New, False);

      procedure Check_Headers;
      procedure Check_Headers is
         Row     : Gtk_Widget := Box.Get_First_Child;
         Headers : Natural := 0;
      begin
         while Row /= null loop
            if Row.all in Gtk_List_Box_Row_Record'Class
              and then Gtk_List_Box_Row (Row).Get_Header /= null
            then
               Headers := Headers + 1;
            end if;
            Row := Row.Get_Next_Sibling;
         end loop;
         Assert_Cmpint_Eq (Gint (Headers), 50);
      end Check_Headers;
   begin
      Box.Ref_Sink;

      Assert_True (Box.Get_Selection_Mode = Selection_Single);
      Assert_True (Box.Get_Selected_Row = null);

      Count := 0;
      Box.Set_Header_Func (Header_Func'Unrestricted_Access);
      Assert_True (Count > 0);
      Check_Headers;

      Count := 0;
      Box.Invalidate_Headers;
      Assert_True (Count > 0);

      Count := 0;
      Box.Get_Row_At_Index (0).Changed;
      Assert_True (Count > 0);

      Box.Unref;
   end Test_Header;

   -----------------------
   -- Test_Activatable --
   -----------------------

   procedure Test_Activatable is
      --  Not in the C tests: the per-row and per-box flags Gtk.List_Box_Row
      --  and Gtk.List_Box add.
      Box : constant Gtk_List_Box := Gtk_List_Box_New;
      Row : constant Gtk_List_Box_Row := Gtk_List_Box_Row_New;
   begin
      Box.Ref_Sink;

      Assert_True (Row.Get_Activatable);
      Row.Set_Activatable (False);
      Assert_False (Row.Get_Activatable);

      Assert_True (Row.Get_Selectable);
      Row.Set_Selectable (False);
      Assert_False (Row.Get_Selectable);

      Assert_True (Row.Get_Child = null);
      Row.Set_Child (Gtk_Label_New ("child"));
      Assert_Cmpstr_Eq (Gtk_Label (Row.Get_Child).Get_Text, "child");

      Assert_True (Box.Get_Activate_On_Single_Click);
      Box.Set_Activate_On_Single_Click (False);
      Assert_False (Box.Get_Activate_On_Single_Click);

      Assert_False (Box.Get_Show_Separators);
      Box.Set_Show_Separators (True);
      Assert_True (Box.Get_Show_Separators);

      Box.Append (Row);
      Assert_True (Box.Get_Row_At_Index (0) = Row);
      Assert_True (Box.Get_Row_At_Index (1) = null);

      Box.Unref;
   end Test_Activatable;

   ---------------------
   -- Test_Bind_Model --
   ---------------------

   procedure Test_Bind_Model is
      Strings : constant Gtk_String_List :=
        Gtk_String_List_New
          ((new String'("a"), new String'("b"), new String'("c")));
      Other   : constant Gtk_String_List :=
        Gtk_String_List_New ((1 => new String'("only")));
      Box     : constant Gtk_List_Box := Gtk_List_Box_New;
   begin
      Box.Ref_Sink;

      --  The plain form keeps no data of its own, so it has nothing to
      --  destroy, whatever the box does with the binding.
      Box.Bind_Model (+Strings, Make_Plain_Row'Unrestricted_Access);
      Assert_Cmpint_Eq (Gint (Count_Rows (Box)), 3);
      Box.Bind_Model (+Other, Make_Plain_Row'Unrestricted_Access);
      Assert_Cmpint_Eq (Gint (Count_Rows (Box)), 1);
      Box.Bind_Model (Null_Glist_Model, null);
      Assert_Cmpint_Eq (Gint (Count_Rows (Box)), 0);

      --  The generic form wraps the caller's data, and must be the one to
      --  release the wrapper, handing Destroy the caller's value.
      Destroyed := 0;
      Model_Binding.Bind_Model
        (Box, +Strings, Make_Row'Unrestricted_Access, 42);
      Assert_Cmpint_Eq (Gint (Count_Rows (Box)), 3);
      Assert_Cmpint_Eq (Gint (Created_With), 42);
      Assert_Cmpint_Eq (Gint (Destroyed), 0);

      --  Replacing the binding destroys the previous one's data.
      Model_Binding.Bind_Model
        (Box, +Other, Make_Row'Unrestricted_Access, 43);
      Assert_Cmpint_Eq (Gint (Count_Rows (Box)), 1);
      Assert_Cmpint_Eq (Gint (Created_With), 43);
      Assert_Cmpint_Eq (Gint (Destroyed), 1);
      Assert_Cmpint_Eq (Gint (Last_Destroyed), 42);

      --  So does unbinding. Binding nothing has no data to destroy.
      Model_Binding.Bind_Model (Box, Null_Glist_Model, null, 0);
      Assert_Cmpint_Eq (Gint (Count_Rows (Box)), 0);
      Assert_Cmpint_Eq (Gint (Destroyed), 2);
      Assert_Cmpint_Eq (Gint (Last_Destroyed), 43);

      --  A binding still in place when the box goes away is released too.
      Model_Binding.Bind_Model
        (Box, +Strings, Make_Row'Unrestricted_Access, 44);
      Assert_Cmpint_Eq (Gint (Destroyed), 2);
      Box.Unref;
      Assert_Cmpint_Eq (Gint (Destroyed), 3);
      Assert_Cmpint_Eq (Gint (Last_Destroyed), 44);

      Strings.Unref;
      Other.Unref;
   end Test_Bind_Model;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/listbox/sort", Test_Sort'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/listbox/selection", Test_Selection'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/listbox/multi-selection", Test_Multi_Selection'Unrestricted_Access);
   Glib.Test.Add_Func ("/listbox/filter", Test_Filter'Unrestricted_Access);
   Glib.Test.Add_Func ("/listbox/header", Test_Header'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/listbox/activatable", Test_Activatable'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/listbox/bind-model", Test_Bind_Model'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end List_Box;
