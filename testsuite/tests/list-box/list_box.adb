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
with Interfaces.C;
with Interfaces.C.Strings;
with Glib;                use Glib;
with Glib.List_Model;     use Glib.List_Model;
with Glib.Object;         use Glib.Object;
with Glib.Test;           use Glib.Test;
with Glib.Variant;        use Glib.Variant;
with Gtk.Accessible;
with Gtk.Adjustment;      use Gtk.Adjustment;
with Gtk.Atcontext;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Label;           use Gtk.Label;
with Gtk.List_Box;        use Gtk.List_Box;
with Gtk.List_Box_Row;    use Gtk.List_Box_Row;
with Gtk.Main;
with Gtk.String_List;     use Gtk.String_List;
with Gtk.Widget;          use Gtk.Widget;
with System;

use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Accessible.Gtk_Accessible_Role;
use type Gtk.Atcontext.Gtk_Atcontext;
use type Glib.Variant.Gvariant;
use type System.Address;

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
   procedure Test_User_Data_Callbacks with Convention => C;
   procedure Test_Additional_Api with Convention => C;
   procedure Test_Row_Api with Convention => C;
   procedure Test_Signals with Convention => C;

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
   --  Instantiate all user-data callback families to test their Ada/C bridge.
   package Foreach_Data is new Gtk.List_Box.Selected_Foreach_User_Data
     (Integer);
   package Filter_Data is new Gtk.List_Box.Set_Filter_Func_User_Data
     (Integer);
   package Header_Data is new Gtk.List_Box.Set_Header_Func_User_Data
     (Integer);
   package Sort_Data is new Gtk.List_Box.Set_Sort_Func_User_Data
     (Integer);

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
   procedure Count_Selected
     (Box : not null access Gtk_List_Box_Record'Class;
      Row : not null access Gtk_List_Box_Row_Record'Class;
      User_Data : Integer);
   function Filter_With_Data
     (Row : not null access Gtk_List_Box_Row_Record'Class;
      User_Data : Integer) return Boolean;
   procedure Header_With_Data
     (Row : not null access Gtk_List_Box_Row_Record'Class;
      Before : access Gtk_List_Box_Row_Record'Class;
      User_Data : Integer);
   function Sort_With_Data
     (Row1, Row2 : not null access Gtk_List_Box_Row_Record'Class;
      User_Data : Integer) return Gint;
   procedure Count_Selected_Plain
     (Box : not null access Gtk_List_Box_Record'Class;
      Row : not null access Gtk_List_Box_Row_Record'Class);

   Data_Sum : Natural := 0;
   --  Accumulates callback data so the tests verify it reaches Ada intact.

   Header_First_Was_Null : Boolean := False;
   --  The header callback must receive a null predecessor for the first row.

   Typed_Signal_Calls : Natural := 0;
   --  Calls received by handlers whose first argument is GtkListBox.

   Slot_Signal_Calls : Natural := 0;
   --  Calls received by handlers that use a separate GObject slot.

   procedure Emit_By_Name
     (Instance : System.Address; Name : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";
   --  Emit action signals directly so their generated Ada marshallers run
   --  deterministically without synthesizing keyboard input.

   procedure Emit_Row_By_Name
     (Instance : System.Address; Name : Interfaces.C.Strings.chars_ptr;
      Row : System.Address)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Move_By_Name
     (Instance : System.Address; Name : Interfaces.C.Strings.chars_ptr;
      Step, Count, Extend, Modify : Interfaces.C.int)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure On_List_Box_Void (Self : access Gtk_List_Box_Record'Class);
   procedure On_GObject_Void (Self : access GObject_Record'Class);
   procedure On_List_Box_Row
     (Self : access Gtk_List_Box_Record'Class;
      Row : not null access Gtk_List_Box_Row_Record'Class);
   procedure On_GObject_Row
     (Self : access GObject_Record'Class;
      Row : not null access Gtk_List_Box_Row_Record'Class);
   procedure On_List_Box_Row_Or_Null
     (Self : access Gtk_List_Box_Record'Class;
      Row : access Gtk_List_Box_Row_Record'Class);
   procedure On_GObject_Row_Or_Null
     (Self : access GObject_Record'Class;
      Row : access Gtk_List_Box_Row_Record'Class);
   procedure On_List_Box_Move
     (Self : access Gtk_List_Box_Record'Class;
      Step : Gtk.Enums.Gtk_Movement_Step; Count : Gint;
      Extend, Modify : Boolean);
   procedure On_GObject_Move
     (Self : access GObject_Record'Class;
      Step : Gtk.Enums.Gtk_Movement_Step; Count : Gint;
      Extend, Modify : Boolean);
   procedure On_Row_Activate
     (Self : access Gtk_List_Box_Row_Record'Class);
   procedure On_Row_Activate_Slot
     (Self : access GObject_Record'Class);

   Row_Signal_Calls : Natural := 0;
   --  Count row activate emissions received by either callback overload.

   Row_Signal_Object : System.Address := System.Null_Address;
   --  The last row object passed to a callback using the GObject slot form.

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

   --------------------
   -- Count_Selected --
   --------------------

   procedure Count_Selected
     (Box : not null access Gtk_List_Box_Record'Class;
      Row : not null access Gtk_List_Box_Row_Record'Class;
      User_Data : Integer)
   is
   begin
      --  Both callback objects must be converted back from their C pointers.
      Assert_True (Box /= null);
      Assert_True (Row.Get_Index >= 0);
      Count := Count + 1;
      Data_Sum := Data_Sum + Natural (User_Data);
   end Count_Selected;

   ----------------------
   -- Filter_With_Data --
   ----------------------

   function Filter_With_Data
     (Row : not null access Gtk_List_Box_Row_Record'Class;
      User_Data : Integer) return Boolean
   is
   begin
      Count := Count + 1;
      Data_Sum := Data_Sum + Natural (User_Data);
      return Value (Row) mod User_Data = 0;
   end Filter_With_Data;

   ----------------------
   -- Header_With_Data --
   ----------------------

   procedure Header_With_Data
     (Row : not null access Gtk_List_Box_Row_Record'Class;
      Before : access Gtk_List_Box_Row_Record'Class;
      User_Data : Integer)
   is
   begin
      --  Before is nullable; GTK passes null for the first row in the list.
      if Before = null then
         Header_First_Was_Null := True;
      end if;
      Count := Count + 1;
      Data_Sum := Data_Sum + Natural (User_Data);
      if Value (Row) mod User_Data = 0 then
         Row.Set_Header (Gtk_Label_New ("group"));
      else
         Row.Set_Header (null);
      end if;
   end Header_With_Data;

   --------------------
   -- Sort_With_Data --
   --------------------

   function Sort_With_Data
     (Row1, Row2 : not null access Gtk_List_Box_Row_Record'Class;
      User_Data : Integer) return Gint
   is
   begin
      Count := Count + 1;
      Data_Sum := Data_Sum + Natural (User_Data);
      return Gint (Value (Row1) - Value (Row2));
   end Sort_With_Data;

   --------------------------
   -- Count_Selected_Plain --
   --------------------------

   procedure Count_Selected_Plain
     (Box : not null access Gtk_List_Box_Record'Class;
      Row : not null access Gtk_List_Box_Row_Record'Class)
   is
   begin
      Assert_True (Box /= null and then Row /= null);
      Count := Count + 1;
   end Count_Selected_Plain;

   --------------------
   -- On_List_Box_Void --
   --------------------

   procedure On_List_Box_Void (Self : access Gtk_List_Box_Record'Class) is
   begin
      Assert_True (Self /= null);
      Typed_Signal_Calls := Typed_Signal_Calls + 1;
   end On_List_Box_Void;

   -------------------
   -- On_GObject_Void --
   -------------------

   procedure On_GObject_Void (Self : access GObject_Record'Class) is
   begin
      Assert_True (Self /= null);
      Slot_Signal_Calls := Slot_Signal_Calls + 1;
   end On_GObject_Void;

   --------------------
   -- On_List_Box_Row --
   --------------------

   procedure On_List_Box_Row
     (Self : access Gtk_List_Box_Record'Class;
      Row : not null access Gtk_List_Box_Row_Record'Class)
   is
   begin
      Assert_True (Self /= null and then Row.Get_Index >= 0);
      Typed_Signal_Calls := Typed_Signal_Calls + 1;
   end On_List_Box_Row;

   -------------------
   -- On_GObject_Row --
   -------------------

   procedure On_GObject_Row
     (Self : access GObject_Record'Class;
      Row : not null access Gtk_List_Box_Row_Record'Class)
   is
   begin
      Assert_True (Self /= null and then Row.Get_Index >= 0);
      Slot_Signal_Calls := Slot_Signal_Calls + 1;
   end On_GObject_Row;

   -----------------------------
   -- On_List_Box_Row_Or_Null --
   -----------------------------

   procedure On_List_Box_Row_Or_Null
     (Self : access Gtk_List_Box_Record'Class;
      Row : access Gtk_List_Box_Row_Record'Class)
   is
      pragma Unreferenced (Row);
   begin
      Assert_True (Self /= null);
      Typed_Signal_Calls := Typed_Signal_Calls + 1;
   end On_List_Box_Row_Or_Null;

   ----------------------------
   -- On_GObject_Row_Or_Null --
   ----------------------------

   procedure On_GObject_Row_Or_Null
     (Self : access GObject_Record'Class;
      Row : access Gtk_List_Box_Row_Record'Class)
   is
      pragma Unreferenced (Row);
   begin
      Assert_True (Self /= null);
      Slot_Signal_Calls := Slot_Signal_Calls + 1;
   end On_GObject_Row_Or_Null;

   --------------------
   -- On_List_Box_Move --
   --------------------

   procedure On_List_Box_Move
     (Self : access Gtk_List_Box_Record'Class;
      Step : Gtk.Enums.Gtk_Movement_Step; Count : Gint;
      Extend, Modify : Boolean)
   is
      pragma Unreferenced (Step, Extend, Modify);
   begin
      Assert_True (Self /= null and then Count = 2);
      Typed_Signal_Calls := Typed_Signal_Calls + 1;
   end On_List_Box_Move;

   -------------------
   -- On_GObject_Move --
   -------------------

   procedure On_GObject_Move
     (Self : access GObject_Record'Class;
      Step : Gtk.Enums.Gtk_Movement_Step; Count : Gint;
      Extend, Modify : Boolean)
   is
      pragma Unreferenced (Step, Extend, Modify);
   begin
      Assert_True (Self /= null and then Count = 2);
      Slot_Signal_Calls := Slot_Signal_Calls + 1;
   end On_GObject_Move;

   --------------------
   -- On_Row_Activate --
   --------------------

   procedure On_Row_Activate
     (Self : access Gtk_List_Box_Row_Record'Class)
   is
   begin
      Assert_True (Self /= null);
      Row_Signal_Calls := Row_Signal_Calls + 1;
   end On_Row_Activate;

   -------------------------
   -- On_Row_Activate_Slot --
   -------------------------

   procedure On_Row_Activate_Slot
     (Self : access GObject_Record'Class)
   is
   begin
      Assert_True (Self /= null);
      Row_Signal_Calls := Row_Signal_Calls + 1;
      Row_Signal_Object := Self.Get_Object;
   end On_Row_Activate_Slot;

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

   ------------------------------
   -- Test_User_Data_Callbacks --
   ------------------------------

   procedure Test_User_Data_Callbacks is
      Box : constant Gtk_List_Box := Fill (Gtk_List_Box_New, False);
      Visible : Natural := 0;
      Child : Gtk_Widget;
   begin
      Box.Ref_Sink;

      --  Each callback sees both converted widget arguments and its data.
      Box.Set_Selection_Mode (Selection_Multiple);
      Box.Select_Row (Box.Get_Row_At_Index (2));
      Box.Select_Row (Box.Get_Row_At_Index (4));
      Count := 0;
      Data_Sum := 0;
      Foreach_Data.Selected_Foreach
        (Box, Count_Selected'Unrestricted_Access, 7);
      Assert_Cmpint_Eq (Gint (Count), 2);
      Assert_Cmpint_Eq (Gint (Data_Sum), 14);

      --  The plain callback overload reports the same selected rows.
      Count := 0;
      Box.Selected_Foreach (Count_Selected_Plain'Unrestricted_Access);
      Assert_Cmpint_Eq (Gint (Count), 2);

      --  The generic filter receives its data for every row and controls
      --  child visibility; invalidation and row-changed both re-run it.
      Count := 0;
      Data_Sum := 0;
      Filter_Data.Set_Filter_Func
        (Box, Filter_With_Data'Unrestricted_Access, 3);
      Assert_True (Count > 0);
      Assert_True (Data_Sum > 0);
      Child := Box.Get_First_Child;
      while Child /= null loop
         if Child.all in Gtk_List_Box_Row_Record'Class
           and then Child.Get_Child_Visible
         then
            Visible := Visible + 1;
         end if;
         Child := Child.Get_Next_Sibling;
      end loop;
      Assert_Cmpint_Eq (Gint (Visible), 34);
      Count := 0;
      Box.Invalidate_Filter;
      Assert_True (Count > 0);
      Count := 0;
      Box.Get_Row_At_Index (0).Changed;
      Assert_True (Count > 0);
      Filter_Data.Set_Filter_Func (Box, null, 3);
      Box.Set_Filter_Func (Filter_Func'Unrestricted_Access);
      Count := 0;
      Box.Set_Filter_Func (null);
      Box.Invalidate_Filter;
      Assert_Cmpint_Eq (Gint (Count), 0);

      --  The generic header callback accepts a nullable predecessor and
      --  replaces headers on rows divisible by the supplied group size.
      Count := 0;
      Data_Sum := 0;
      Header_First_Was_Null := False;
      Header_Data.Set_Header_Func
        (Box, Header_With_Data'Unrestricted_Access, 2);
      Assert_True (Count > 0);
      Assert_True (Data_Sum > 0);
      Assert_True (Header_First_Was_Null);
      Count := 0;
      Box.Invalidate_Headers;
      Assert_True (Count > 0);
      Header_Data.Set_Header_Func (Box, null, 2);
      Box.Set_Header_Func (Header_Func'Unrestricted_Access);
      Count := 0;
      Box.Set_Header_Func (null);
      Box.Invalidate_Headers;
      Assert_Cmpint_Eq (Gint (Count), 0);

      --  The generic comparator is called during installation and when
      --  sorting is invalidated; clearing it exercises the null overload.
      Count := 0;
      Data_Sum := 0;
      Sort_Data.Set_Sort_Func (Box, Sort_With_Data'Unrestricted_Access, 5);
      Assert_True (Count > 0);
      Assert_True (Data_Sum > 0);
      Count := 0;
      Box.Invalidate_Sort;
      Assert_True (Count > 0);
      Sort_Data.Set_Sort_Func (Box, null, 5);
      Box.Set_Sort_Func (Sort_Func'Unrestricted_Access);
      Count := 0;
      Box.Set_Sort_Func (null);
      Box.Invalidate_Sort;
      Assert_Cmpint_Eq (Gint (Count), 0);

      Box.Unref;
   end Test_User_Data_Callbacks;

   -----------------------
   -- Test_Additional_Api --
   -----------------------

   procedure Test_Additional_Api is
      Box : Gtk_List_Box;
      Adjustment : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (1.0, 0.0, 10.0, 1.0, 2.0, 0.0);
      First : constant Gtk_Label := Gtk_Label_New ("first");
      Middle : constant Gtk_Label := Gtk_Label_New ("middle");
      Last : constant Gtk_Label := Gtk_Label_New ("last");
      Placeholder : constant Gtk_Label := Gtk_Label_New ("empty");
      Row : Gtk_List_Box_Row;
      X, Y, Width, Height : Gint;
   begin
      --  Exercise the procedure constructor and its already-created guard.
      Gtk_New (Box);
      Box.Ref_Sink;
      Gtk.List_Box.Initialize (Box);
      Assert_True (Gtk.List_Box.Get_Type /= 0);
      Assert_True (Box.Get_Selection_Mode = Selection_Single);

      --  The adjustment setter/getter round-trip the same object and accept
      --  null when scrolling is managed elsewhere.
      Assert_True (Box.Get_Adjustment = null);
      --  GtkAdjustment is initially unowned; retain a caller reference while
      --  the list box temporarily holds its own reference.
      Adjustment.Ref_Sink;
      Box.Set_Adjustment (Adjustment);
      Assert_True (Box.Get_Adjustment = Adjustment);
      Box.Set_Adjustment (null);
      Assert_True (Box.Get_Adjustment = null);

      --  Tab behavior is an enum-valued property with all three API values.
      Box.Set_Tab_Behavior (List_Tab_Cell);
      Assert_True (Box.Get_Tab_Behavior = List_Tab_Cell);
      Box.Set_Tab_Behavior (List_Tab_Item);
      Assert_True (Box.Get_Tab_Behavior = List_Tab_Item);
      Box.Set_Tab_Behavior (List_Tab_All);
      Assert_True (Box.Get_Tab_Behavior = List_Tab_All);

      --  Prepend, append, positional insertion, index lookup and invalid
      --  positions together verify the list's documented ordering.
      Box.Prepend (First);
      Box.Append (Last);
      Box.Insert (Middle, 1);
      Assert_True (Gtk_Label (Box.Get_Row_At_Index (0).Get_Child).Get_Text = "first");
      Assert_True (Gtk_Label (Box.Get_Row_At_Index (1).Get_Child).Get_Text = "middle");
      Assert_True (Gtk_Label (Box.Get_Row_At_Index (2).Get_Child).Get_Text = "last");
      Assert_True (Box.Get_Row_At_Index (-1) = null);
      Assert_True (Box.Get_Row_At_Index (3) = null);

      --  Y lookup is valid before allocation and returns no row for this
      --  unallocated list; drag highlight can be applied and removed by API.
      Row := Box.Get_Row_At_Index (1);
      Assert_True (Box.Get_Row_At_Y (0) = null);
      Box.Drag_Highlight_Row (Row);
      Box.Drag_Unhighlight_Row;

      --  GtkListBox displays a placeholder in place of its empty contents.
      Box.Remove_All;
      Assert_Cmpint_Eq (Gint (Count_Rows (Box)), 0);
      Box.Set_Placeholder (Placeholder);
      Assert_True (Box.Get_First_Child = Gtk_Widget (Placeholder));
      Box.Set_Placeholder (null);

      --  Accessible getters report the list role and its empty, unrooted
      --  relationships; the remaining calls cover supported reset/update APIs.
      Assert_True
        (Box.Get_Accessible_Role = Gtk.Accessible.Accessible_Role_List);
      Assert_True
        (Box.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (Box.Get_Accessible_Id = "");
      Assert_True
        (Box.Get_First_Accessible_Child = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Box.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_False (Box.Get_Bounds (X, Y, Width, Height));
      --  The test driver disables accessibility, so no AT context is made.
      Assert_True (Box.Get_At_Context = null);
      Assert_True
        (Gtk.List_Box.Get_Platform_State
           (Box, Gtk.Accessible.Accessible_Platform_State_Focusable)
         = Gtk.List_Box.Get_Platform_State
             (Box, Gtk.Accessible.Accessible_Platform_State_Focusable));
      Box.Reset_Property (Gtk.Accessible.Accessible_Property_Description);
      Box.Reset_Relation (Gtk.Accessible.Accessible_Relation_Controls);
      Box.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      Box.Announce
        ("ListBox test announcement",
         Gtk.Accessible.Accessible_Announcement_Priority_Low);

      Box.Unref;
      Adjustment.Unref;
   end Test_Additional_Api;

   ------------------
   -- Test_Row_Api --
   ------------------

   procedure Test_Row_Api is
      Box : constant Gtk_List_Box := Gtk_List_Box_New;
      Row : Gtk_List_Box_Row;
      Next_Row : constant Gtk_List_Box_Row := Gtk_List_Box_Row_New;
      Child : constant Gtk_Label := Gtk_Label_New ("row child");
      Slot : constant Gtk_Label := Gtk_Label_New ("signal slot");
      Target : constant Gvariant := Gvariant_New_String ("direct target");
      Current_Target : Gvariant;
      X, Y, Width, Height : Gint;
      Name : Interfaces.C.Strings.chars_ptr;
   begin
      Box.Ref_Sink;
      Next_Row.Set_Child (Gtk_Label_New ("next row"));

      --  The procedure constructor and public Initialize are both supported;
      --  Initialize is idempotent after Gtk_New has created the object.
      Gtk_New (Row);
      Row.Ref_Sink;
      Gtk.List_Box_Row.Initialize (Row);
      Assert_True (Gtk.List_Box_Row.Get_Type /= 0);

      --  The generated conversion hooks preserve the row wrapper around its
      --  underlying GObject address in both directions.
      Assert_True (Gtk.List_Box_Row.Convert (Gtk.List_Box_Row.Convert (Row)) = Row);

      --  The row owns its child and reports the list-item accessible role.
      Assert_True (Row.Get_Child = null);
      Row.Set_Child (Child);
      Assert_Cmpstr_Eq (Gtk_Label (Row.Get_Child).Get_Text, "row child");
      Assert_True
        (Row.Get_Accessible_Role = Gtk.Accessible.Accessible_Role_List_Item);
      Assert_Cmpstr_Eq (Row.Get_Accessible_Id, "");

      --  GTK_A11Y=none leaves the unrooted row without an AT context or
      --  screen coordinates; its ordinary widget child remains accessible.
      Assert_True (Row.Get_At_Context = null);
      Assert_False (Row.Get_Bounds (X, Y, Width, Height));
      Assert_True (Row.Get_First_Accessible_Child /= Gtk.Accessible.Null_Gtk_Accessible);

      --  Accessible parent and sibling lookups follow the row's place in the
      --  list.  Setting null values clears any explicit accessibility links.
      Assert_True
        (Row.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Row.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      Row.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Row.Update_Next_Accessible_Sibling (Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Row.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Row.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);

      --  Focusable platform state mirrors the GtkWidget focusable setting.
      Row.Set_Focusable (False);
      Assert_False
        (Row.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focusable));
      Row.Set_Focusable (True);
      Assert_True
        (Row.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focusable));

      --  Reset operations are safe for values that are already at their
      --  defaults, and Announce accepts a UTF-8 message with low priority.
      Row.Reset_Property (Gtk.Accessible.Accessible_Property_Description);
      Row.Reset_Relation (Gtk.Accessible.Accessible_Relation_Controls);
      Row.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      Row.Announce
        ("Row API test announcement",
         Gtk.Accessible.Accessible_Announcement_Priority_Low);

      --  GtkActionable stores a named action and its target GVariant.  The
      --  getter returns the same string target set through the row wrapper.
      Assert_Cmpstr_Eq (Row.Get_Action_Name, "");
      Assert_True (Row.Get_Action_Target_Value = Null_Gvariant);
      Row.Set_Action_Name ("app.test");
      Assert_Cmpstr_Eq (Row.Get_Action_Name, "app.test");
      Row.Set_Action_Target_Value (Target);
      Current_Target := Row.Get_Action_Target_Value;
      Assert_Cmpstr_Eq (Current_Target.Get_String, "direct target");

      --  Detailed action syntax sets both the action name and the quoted
      --  string target.  An empty action name removes the action association.
      Row.Set_Detailed_Action_Name ("app.test('detailed target')");
      Assert_Cmpstr_Eq (Row.Get_Action_Name, "app.test");
      Current_Target := Row.Get_Action_Target_Value;
      Assert_Cmpstr_Eq (Current_Target.Get_String, "detailed target");
      Row.Set_Action_Name;
      Assert_Cmpstr_Eq (Row.Get_Action_Name, "");

      --  GtkListBoxRow's activate signal exercises both generated marshallers.
      --  Emitting synchronously makes the callback data observable here.
      Slot.Ref_Sink;
      Row_Signal_Calls := 0;
      Row_Signal_Object := System.Null_Address;
      Row.On_Activate (On_Row_Activate'Unrestricted_Access);
      Row.On_Activate (On_Row_Activate_Slot'Unrestricted_Access, Slot);
      Name := Interfaces.C.Strings.New_String ("activate");
      Emit_By_Name (Row.Get_Object, Name);
      Interfaces.C.Strings.Free (Name);
      Assert_Cmpint_Eq (Gint (Row_Signal_Calls), 2);
      --  The slot overload receives the separately supplied slot object.
      Assert_True (Row_Signal_Object = Slot.Get_Object);

      --  Once parented, the list supplies the row's accessible relationships.
      Box.Append (Row);
      Box.Append (Next_Row);
      Assert_True (Row.Get_Accessible_Parent /= Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (Row.Get_Next_Accessible_Sibling /= Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (Row.Get_First_Accessible_Child /= Gtk.Accessible.Null_Gtk_Accessible);

      Slot.Unref;
      Row.Unref;
      Box.Unref;
   end Test_Row_Api;

   ------------------
   -- Test_Signals --
   ------------------

   procedure Test_Signals is
      Box : constant Gtk_List_Box := Fill (Gtk_List_Box_New, False);
      Slot : constant Gtk_Label := Gtk_Label_New ("signal slot");
      Row : Gtk_List_Box_Row;
      Before_Typed : Natural;
      Before_Slot : Natural;

      procedure Emit_No_Args (Name : String);
      procedure Emit_Row (Name : String; Item : Gtk_List_Box_Row);
      procedure Emit_Move;
      procedure Check_No_Args (Name : String);

      procedure Emit_No_Args (Name : String) is
         C_Name : Interfaces.C.Strings.chars_ptr :=
           Interfaces.C.Strings.New_String (Name);
      begin
         Emit_By_Name (Box.Get_Object, C_Name);
         Interfaces.C.Strings.Free (C_Name);
      end Emit_No_Args;

      procedure Emit_Row (Name : String; Item : Gtk_List_Box_Row) is
         C_Name : Interfaces.C.Strings.chars_ptr :=
           Interfaces.C.Strings.New_String (Name);
      begin
         --  GtkListBox signal parameters are passed as object pointers.
         Emit_Row_By_Name (Box.Get_Object, C_Name, Item.Get_Object);
         Interfaces.C.Strings.Free (C_Name);
      end Emit_Row;

      procedure Emit_Move is
         C_Name : Interfaces.C.Strings.chars_ptr :=
           Interfaces.C.Strings.New_String ("move-cursor");
      begin
         --  C varargs promote the enum, count and booleans to int values.
         Emit_Move_By_Name
           (Box.Get_Object, C_Name,
            Interfaces.C.int
              (Gtk.Enums.Gtk_Movement_Step'Pos
                 (Gtk.Enums.Movement_Words)),
            2, 0, 0);
         Interfaces.C.Strings.Free (C_Name);
      end Emit_Move;

      procedure Check_No_Args (Name : String) is
      begin
         Before_Typed := Typed_Signal_Calls;
         Before_Slot := Slot_Signal_Calls;
         Emit_No_Args (Name);
         Assert_True (Typed_Signal_Calls > Before_Typed);
         Assert_True (Slot_Signal_Calls > Before_Slot);
      end Check_No_Args;

   begin
      Box.Ref_Sink;
      Slot.Ref_Sink;
      Row := Box.Get_Row_At_Index (1);
      Box.Set_Selection_Mode (Selection_Multiple);

      --  Register each signal overload, including the variants whose
      --  callback receives a separate GObject slot.
      Box.On_Activate_Cursor_Row (On_List_Box_Void'Unrestricted_Access);
      Box.On_Activate_Cursor_Row
        (On_GObject_Void'Unrestricted_Access, Slot);
      Box.On_Move_Cursor (On_List_Box_Move'Unrestricted_Access);
      Box.On_Move_Cursor (On_GObject_Move'Unrestricted_Access, Slot);
      Box.On_Row_Activated (On_List_Box_Row'Unrestricted_Access);
      Box.On_Row_Activated (On_GObject_Row'Unrestricted_Access, Slot);
      Box.On_Row_Selected (On_List_Box_Row_Or_Null'Unrestricted_Access);
      Box.On_Row_Selected
        (On_GObject_Row_Or_Null'Unrestricted_Access, Slot);
      Box.On_Select_All (On_List_Box_Void'Unrestricted_Access);
      Box.On_Select_All (On_GObject_Void'Unrestricted_Access, Slot);
      Box.On_Selected_Rows_Changed (On_List_Box_Void'Unrestricted_Access);
      Box.On_Selected_Rows_Changed
        (On_GObject_Void'Unrestricted_Access, Slot);
      Box.On_Toggle_Cursor_Row (On_List_Box_Void'Unrestricted_Access);
      Box.On_Toggle_Cursor_Row
        (On_GObject_Void'Unrestricted_Access, Slot);
      Box.On_Unselect_All (On_List_Box_Void'Unrestricted_Access);
      Box.On_Unselect_All (On_GObject_Void'Unrestricted_Access, Slot);

      --  Emit signals with their documented argument shapes and check that
      --  both typed and slot callbacks receive each emission.
      Before_Typed := Typed_Signal_Calls;
      Before_Slot := Slot_Signal_Calls;
      Emit_No_Args ("activate-cursor-row");
      Assert_True (Typed_Signal_Calls > Before_Typed);
      Assert_True (Slot_Signal_Calls > Before_Slot);

      Before_Typed := Typed_Signal_Calls;
      Before_Slot := Slot_Signal_Calls;
      Emit_Move;
      Assert_True (Typed_Signal_Calls > Before_Typed);
      Assert_True (Slot_Signal_Calls > Before_Slot);

      Before_Typed := Typed_Signal_Calls;
      Before_Slot := Slot_Signal_Calls;
      Emit_Row ("row-activated", Row);
      Assert_True (Typed_Signal_Calls > Before_Typed);
      Assert_True (Slot_Signal_Calls > Before_Slot);

      Before_Typed := Typed_Signal_Calls;
      Before_Slot := Slot_Signal_Calls;
      Emit_Row ("row-selected", Row);
      Assert_True (Typed_Signal_Calls > Before_Typed);
      Assert_True (Slot_Signal_Calls > Before_Slot);

      Check_No_Args ("select-all");
      Check_No_Args ("selected-rows-changed");
      Check_No_Args ("toggle-cursor-row");
      Check_No_Args ("unselect-all");

      Slot.Unref;
      Box.Unref;
   end Test_Signals;

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
   Glib.Test.Add_Func
     ("/listbox/user-data-callbacks",
      Test_User_Data_Callbacks'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/listbox/additional-api", Test_Additional_Api'Unrestricted_Access);
   Glib.Test.Add_Func ("/listbox/row-api", Test_Row_Api'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/listbox/signals", Test_Signals'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end List_Box;
