--  Tests of Gtk.Flow_Box and Gtk.Flow_Box_Child. The measure regression case
--  is ported from testsuite/c_tests/flowbox.c:test_measure_crash. The other
--  cases exercise ordering, selection, callback adapters, model binding,
--  accessibility and signal marshallers. Each labelled child carries the
--  integer used by the sort and filter functions.

with Ada.Command_Line;
with Ada.Exceptions;
with GNAT.Strings;          use GNAT.Strings;
with Glib;                  use Glib;
with Glib.List_Model;       use Glib.List_Model;
with Glib.Object;           use Glib.Object;
with Glib.Properties;        use Glib.Properties;
with Glib.Test;             use Glib.Test;
with Glib.Values;
with Gtk.Accessible;      use Gtk.Accessible;
with Gtk.Adjustment;      use Gtk.Adjustment;
with Gtk.Atcontext;       use Gtk.Atcontext;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Flow_Box;          use Gtk.Flow_Box;
with Gtk.Flow_Box_Child;    use Gtk.Flow_Box_Child;
with Gtk.Label;             use Gtk.Label;
with Gtk.Main;
with Gtk.String_List;       use Gtk.String_List;
with Gtk.Window;            use Gtk.Window;
with Gtk.Widget;            use Gtk.Widget;
with Gtkada.Bindings;
with Gtkada.Types;
with Interfaces.C;
with Interfaces.C.Strings;
with System;

use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Atcontext.Gtk_Atcontext;
use type Interfaces.C.int;
use type System.Address;

procedure Flow_Box is

   Count : Natural := 0;
   --  Bumped by the callbacks below

   Data_Sum : Integer := 0;
   --  Accumulates callback data so every generic adapter is checked.

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

   --  These callbacks make the user-data generic adapters observable.
   procedure Count_Selected
     (Box : not null access Gtk_Flow_Box_Record'Class;
      Child : not null access Gtk_Flow_Box_Child_Record'Class);
   procedure Count_Selected_With_Data
     (Box : not null access Gtk_Flow_Box_Record'Class;
      Child : not null access Gtk_Flow_Box_Child_Record'Class;
      User_Data : Integer);
   function Filter_With_Data
     (Child : not null access Gtk_Flow_Box_Child_Record'Class;
      User_Data : Integer) return Boolean;
   function Sort_With_Data
     (A, B : not null access Gtk_Flow_Box_Child_Record'Class;
      User_Data : Integer) return Gint;

   --  Signal handlers count calls and retain the arguments for assertions.
   procedure On_Typed_Void (Self : access Gtk_Flow_Box_Record'Class);
   procedure On_Slot_Void (Self : access GObject_Record'Class);
   procedure On_Typed_Child
     (Self : access Gtk_Flow_Box_Record'Class;
      Child : not null access Gtk_Flow_Box_Child_Record'Class);
   procedure On_Slot_Child
     (Self : access GObject_Record'Class;
      Child : not null access Gtk_Flow_Box_Child_Record'Class);
   function On_Typed_Move
     (Self : access Gtk_Flow_Box_Record'Class;
      Step : Gtk_Movement_Step; Move_Count : Gint;
      Extend, Modify : Boolean) return Boolean;
   function On_Slot_Move
     (Self : access GObject_Record'Class;
      Step : Gtk_Movement_Step; Move_Count : Gint;
      Extend, Modify : Boolean) return Boolean;

   --  The child signal test records object identity for both callback styles.
   Child_Signal_Calls : Natural := 0;
   Typed_Child_Signal_Object : System.Address := System.Null_Address;
   Slot_Child_Signal_Object : System.Address := System.Null_Address;
   procedure On_Typed_Child_Activate
     (Self : access Gtk_Flow_Box_Child_Record'Class);
   procedure On_Slot_Child_Activate
     (Self : access GObject_Record'Class);
   procedure Raise_Typed_Child_Activate
     (Self : access Gtk_Flow_Box_Child_Record'Class);
   procedure Raise_Slot_Child_Activate
     (Self : access GObject_Record'Class);
   procedure Note_Child_Signal_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);
   Child_Signal_Exceptions : Natural := 0;

   --  Emit the keybinding signal directly so the callback test needs no input.
   procedure Emit_Child_Activate
     (Instance : System.Address; Name : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   --  These imports emit GTK signals directly, preserving their C argument
   --  conventions while exercising the generated Ada marshallers.
   procedure Emit_No_Args
     (Instance : System.Address; Name : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";
   procedure Emit_Child
     (Instance : System.Address; Name : Interfaces.C.Strings.chars_ptr;
      Child : System.Address)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";
   procedure Emit_Move
     (Instance : System.Address; Name : Interfaces.C.Strings.chars_ptr;
      Step, Move_Count, Extend, Modify : Interfaces.C.int;
      Return_Value : System.Address)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   package Model_Binding is new Gtk.Flow_Box.Bind_Model_User_Data
     (Integer, Note_Destroyed);
   package Foreach_Data is new Gtk.Flow_Box.Selected_Foreach_User_Data
     (Integer);
   package Filter_Data is new Gtk.Flow_Box.Set_Filter_Func_User_Data
     (Integer, Note_Destroyed);
   package Sort_Data is new Gtk.Flow_Box.Set_Sort_Func_User_Data
     (Integer, Note_Destroyed);

   procedure Test_Order with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Selection with Convention => C;
   procedure Test_Sort_Filter with Convention => C;
   procedure Test_Bind_Model with Convention => C;
   procedure Test_User_Data_Callbacks with Convention => C;
   procedure Test_Additional_Api with Convention => C;
   procedure Test_Child_Api with Convention => C;
   procedure Test_Child_Signals with Convention => C;
   procedure Test_Measure_Crash with Convention => C;
   procedure Test_Signals with Convention => C;

   Typed_Signal_Calls : Natural := 0;
   Slot_Signal_Calls : Natural := 0;
   Signal_Child : Gtk_Flow_Box_Child;
   Signal_Arguments_Ok : Boolean := False;

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

   --------------------
   -- Count_Selected --
   --------------------

   procedure Count_Selected
     (Box : not null access Gtk_Flow_Box_Record'Class;
      Child : not null access Gtk_Flow_Box_Child_Record'Class)
   is
   begin
      --  Confirm the callback receives a selected child belonging to Box.
      Assert_True (Child.Is_Selected);
      Assert_True
        (Get_Object (Box.Get_Child_At_Index (Child.Get_Index)) =
           Get_Object (Child));
      Count := Count + 1;
   end Count_Selected;

   -----------------------------
   -- Count_Selected_With_Data --
   -----------------------------

   procedure Count_Selected_With_Data
     (Box : not null access Gtk_Flow_Box_Record'Class;
      Child : not null access Gtk_Flow_Box_Child_Record'Class;
      User_Data : Integer)
   is
   begin
      --  The generic wrapper forwards the original caller data to each visit.
      Count_Selected (Box, Child);
      Data_Sum := Data_Sum + User_Data;
   end Count_Selected_With_Data;

   ----------------------
   -- Filter_With_Data --
   ----------------------

   function Filter_With_Data
     (Child : not null access Gtk_Flow_Box_Child_Record'Class;
      User_Data : Integer) return Boolean
   is
   begin
      --  Use the data in the predicate and record every callback invocation.
      Count := Count + 1;
      Data_Sum := Data_Sum + User_Data;
      return Value (Child) mod 2 = User_Data;
   end Filter_With_Data;

   --------------------
   -- Sort_With_Data --
   --------------------

   function Sort_With_Data
     (A, B : not null access Gtk_Flow_Box_Child_Record'Class;
      User_Data : Integer) return Gint
   is
   begin
      --  Sorting uses label values while the sum confirms callback data arrived.
      Count := Count + 1;
      Data_Sum := Data_Sum + User_Data;
      return Gint (Value (A) - Value (B));
   end Sort_With_Data;

   --------------------
   -- On_Typed_Void --
   --------------------

   procedure On_Typed_Void (Self : access Gtk_Flow_Box_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Typed_Signal_Calls := Typed_Signal_Calls + 1;
   end On_Typed_Void;

   -------------------
   -- On_Slot_Void --
   -------------------

   procedure On_Slot_Void (Self : access GObject_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Slot_Signal_Calls := Slot_Signal_Calls + 1;
   end On_Slot_Void;

   ---------------------
   -- On_Typed_Child --
   ---------------------

   procedure On_Typed_Child
     (Self : access Gtk_Flow_Box_Record'Class;
      Child : not null access Gtk_Flow_Box_Child_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Typed_Signal_Calls := Typed_Signal_Calls + 1;
      Signal_Arguments_Ok :=
        Signal_Arguments_Ok and
          Get_Object (Child) = Get_Object (Signal_Child);
   end On_Typed_Child;

   --------------------
   -- On_Slot_Child --
   --------------------

   procedure On_Slot_Child
     (Self : access GObject_Record'Class;
      Child : not null access Gtk_Flow_Box_Child_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Slot_Signal_Calls := Slot_Signal_Calls + 1;
      Signal_Arguments_Ok :=
        Signal_Arguments_Ok and
          Get_Object (Child) = Get_Object (Signal_Child);
   end On_Slot_Child;

   --------------------
   -- On_Typed_Move --
   --------------------

   function On_Typed_Move
     (Self : access Gtk_Flow_Box_Record'Class;
      Step : Gtk_Movement_Step; Move_Count : Gint;
      Extend, Modify : Boolean) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      Typed_Signal_Calls := Typed_Signal_Calls + 1;
      Signal_Arguments_Ok :=
        Signal_Arguments_Ok and Step = Movement_Display_Lines and
          Move_Count = -2 and Extend and not Modify;
      return True;
   end On_Typed_Move;

   -------------------
   -- On_Slot_Move --
   -------------------

   function On_Slot_Move
     (Self : access GObject_Record'Class;
      Step : Gtk_Movement_Step; Move_Count : Gint;
      Extend, Modify : Boolean) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      Slot_Signal_Calls := Slot_Signal_Calls + 1;
      Signal_Arguments_Ok :=
        Signal_Arguments_Ok and Step = Movement_Display_Lines and
          Move_Count = -2 and Extend and not Modify;
      return True;
   end On_Slot_Move;

   -----------------------------
   -- On_Typed_Child_Activate --
   -----------------------------

   procedure On_Typed_Child_Activate
     (Self : access Gtk_Flow_Box_Child_Record'Class) is
   begin
      Child_Signal_Calls := Child_Signal_Calls + 1;
      Typed_Child_Signal_Object := Get_Object (Self);
   end On_Typed_Child_Activate;

   ---------------------------
   -- On_Slot_Child_Activate --
   ---------------------------

   procedure On_Slot_Child_Activate
     (Self : access GObject_Record'Class) is
   begin
      Child_Signal_Calls := Child_Signal_Calls + 1;
      Slot_Child_Signal_Object := Get_Object (Self);
   end On_Slot_Child_Activate;

   ---------------------------------
   -- Raise_Typed_Child_Activate --
   ---------------------------------

   procedure Raise_Typed_Child_Activate
     (Self : access Gtk_Flow_Box_Child_Record'Class) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error;
   end Raise_Typed_Child_Activate;

   -------------------------------
   -- Raise_Slot_Child_Activate --
   -------------------------------

   procedure Raise_Slot_Child_Activate
     (Self : access GObject_Record'Class) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error;
   end Raise_Slot_Child_Activate;

   --------------------------------
   -- Note_Child_Signal_Exception --
   --------------------------------

   procedure Note_Child_Signal_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Child_Signal_Exceptions := Child_Signal_Exceptions + 1;
   end Note_Child_Signal_Exception;

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

      --  The typed property descriptors expose the same property values.
      Set_Property (Box, Row_Spacing_Property, 5);
      Assert_Cmpuint_Eq (Box.Get_Row_Spacing, 5);

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

   -------------------------------
   -- Test_User_Data_Callbacks --
   -------------------------------

   procedure Test_User_Data_Callbacks is
      Box : constant Gtk_Flow_Box := Fill (Gtk_Flow_Box_New, False);
      Visible : Natural := 0;
      Child : Gtk_Widget;
   begin
      Ref_Sink (Box);
      Box.Set_Selection_Mode (Selection_Multiple);
      Box.Select_Child (Box.Get_Child_At_Index (1));
      Box.Select_Child (Box.Get_Child_At_Index (3));
      Box.Select_Child (Box.Get_Child_At_Index (5));

      --  Both selected-foreach interfaces visit the same three selected rows.
      Count := 0;
      Box.Selected_Foreach (Count_Selected'Unrestricted_Access);
      Assert_Cmpint_Eq (Gint (Count), 3);
      Count := 0;
      Data_Sum := 0;
      Foreach_Data.Selected_Foreach
        (Box, Count_Selected_With_Data'Unrestricted_Access, 7);
      Assert_Cmpint_Eq (Gint (Count), 3);
      Assert_Cmpint_Eq (Gint (Data_Sum), 21);

      --  The generic sort callback sees its data and can be removed cleanly.
      Destroyed := 0;
      Count := 0;
      Data_Sum := 0;
      Sort_Data.Set_Sort_Func
        (Box, Sort_With_Data'Unrestricted_Access, 9);
      Assert_True (Count > 0);
      Assert_Cmpint_Eq (Gint (Data_Sum), 9 * Gint (Count));
      Sort_Data.Set_Sort_Func (Box, null, 10);
      Assert_Cmpint_Eq (Gint (Destroyed), 1);
      Assert_Cmpint_Eq (Gint (Last_Destroyed), 9);

      --  Even values remain visible; setting a null generic predicate clears
      --  the filter and runs the generic data destructor exactly once.
      Count := 0;
      Data_Sum := 0;
      Filter_Data.Set_Filter_Func
        (Box, Filter_With_Data'Unrestricted_Access, 0);
      Assert_True (Count > 0);
      Assert_Cmpint_Eq (Gint (Data_Sum), 0);
      Child := Box.Get_First_Child;
      while Child /= null loop
         if Child.all in Gtk_Flow_Box_Child_Record'Class
           and then Child.Get_Child_Visible
         then
            Visible := Visible + 1;
            Assert_Cmpint_Eq
              (Gint (Value (Gtk_Flow_Box_Child (Child)) mod 2), 0);
         end if;
         Child := Child.Get_Next_Sibling;
      end loop;
      Assert_True (Visible > 0 and Visible < 20);
      Filter_Data.Set_Filter_Func (Box, null, 0);
      Assert_Cmpint_Eq (Gint (Destroyed), 2);
      Assert_Cmpint_Eq (Gint (Last_Destroyed), 0);

      --  The non-generic setters also accept null to remove callbacks.
      Box.Set_Filter_Func (null);
      Box.Set_Sort_Func (null);
      Assert_Cmpint_Eq (Gint (Count_Children (Box)), 20);
      Unref (Box);
   end Test_User_Data_Callbacks;

   ------------------------
   -- Test_Additional_Api --
   ------------------------

   procedure Test_Additional_Api is
      Box : Gtk_Flow_Box;
      Context : Gtk_Atcontext;
      First : Gtk_Accessible;
      X, Y, Width, Height : Gint;
      H_Adjustment, V_Adjustment : Gtk_Adjustment;
      Has_Bounds : Boolean;
   begin
      --  Gtk_New and an extra Initialize call exercise both constructor paths.
      Gtk_New (Box);
      Ref_Sink (Box);
      Gtk.Flow_Box.Initialize (Box);
      Assert_Nonnull (Get_Object (Box));
      Box.Append (Gtk_Label_New ("accessible child"));
      Assert_Cmpint_Eq (Box.Get_Child_At_Index (0).Get_Index, 0);
      Assert_True (Box.Get_Child_At_Pos (0, 0) = null);

      --  The unrooted accessible tree has no parent, context, sibling or bounds.
      Assert_True (Box.Get_Accessible_Role = Accessible_Role_Grid);
      Assert_Cmpstr_Eq (Box.Get_Accessible_Id, "");
      Assert_True (Box.Get_Accessible_Parent = Null_Gtk_Accessible);
      Assert_True (Box.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      First := Box.Get_First_Accessible_Child;
      Assert_True (First /= Null_Gtk_Accessible);
      Context := Box.Get_At_Context;
      Assert_True (Context = null);
      Has_Bounds := Box.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);

      --  Accessible updates are supported without a window or assistive tech.
      Box.Set_Focusable (True);
      Box.Update_Platform_State (Accessible_Platform_State_Focusable);
      Assert_True
        (Box.Get_Platform_State (Accessible_Platform_State_Focusable));
      Box.Reset_Property (Accessible_Property_Label);
      Box.Reset_Relation (Accessible_Relation_Labelled_By);
      Box.Reset_State (Accessible_State_Busy);
      Box.Announce ("flow box test", Accessible_Announcement_Priority_Low);
      Box.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Box.Get_Accessible_Parent = Null_Gtk_Accessible);
      Box.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (Box.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  Flow boxes implement GtkOrientable; changing orientation updates it.
      Assert_True (Box.Get_Orientation = Orientation_Horizontal);
      Box.Set_Orientation (Orientation_Vertical);
      Assert_True (Box.Get_Orientation = Orientation_Vertical);

      --  The adjustments stay valid after being handed to the focus handlers.
      H_Adjustment := Gtk_Adjustment_New (12.0, 0.0, 100.0, 1.0, 10.0);
      V_Adjustment := Gtk_Adjustment_New (7.0, 0.0, 100.0, 1.0, 10.0);
      Box.Set_Hadjustment (H_Adjustment);
      Box.Set_Vadjustment (V_Adjustment);
      Assert_True (H_Adjustment.Get_Value = 12.0);
      Assert_True (V_Adjustment.Get_Value = 7.0);

      Unref (Box);
      Unref (H_Adjustment);
      Unref (V_Adjustment);
   end Test_Additional_Api;

   --------------------
   -- Test_Child_Api --
   --------------------

   procedure Test_Child_Api is
      Box : constant Gtk_Flow_Box := Gtk_Flow_Box_New;
      By_Procedure : Gtk_Flow_Box_Child;
      By_Function : constant Gtk_Flow_Box_Child := Gtk_Flow_Box_Child_New;
      Text : constant Gtk_Label := Gtk_Label_New ("child label");
      Child : Gtk_Flow_Box_Child;
      X, Y, Width, Height : Gint;
   begin
      Ref_Sink (Box);

      --  Exercise both constructors and the public idempotent Initialize.
      Gtk_New (By_Procedure);
      Ref_Sink (By_Procedure);
      Gtk.Flow_Box_Child.Initialize (By_Procedure);
      Ref_Sink (By_Function);
      Assert_Nonnull (Get_Object (By_Procedure));
      Assert_Nonnull (Get_Object (By_Function));
      Assert_True (Gtk.Flow_Box_Child.Get_Type /= 0);

      --  Conversion hooks preserve the Ada wrapper and its GObject address.
      Assert_True
        (Gtk.Flow_Box_Child.Convert
           (Gtk.Flow_Box_Child.Convert (By_Procedure)) = By_Procedure);

      --  A standalone child has no index or selection until inserted in a box.
      By_Procedure.Set_Child (Text);
      Assert_Cmpstr_Eq (Gtk_Label (By_Procedure.Get_Child).Get_Text,
                        "child label");
      Assert_Cmpint_Eq (By_Procedure.Get_Index, -1);
      Assert_False (By_Procedure.Is_Selected);
      By_Procedure.Changed;

      --  Accessibility getters expose the child role, label child, and empty
      --  relationships of an unrooted widget under the headless test driver.
      Assert_True
        (By_Procedure.Get_Accessible_Role = Accessible_Role_Grid_Cell);
      Assert_Cmpstr_Eq (By_Procedure.Get_Accessible_Id, "");
      Assert_True (By_Procedure.Get_At_Context = null);
      Assert_False (By_Procedure.Get_Bounds (X, Y, Width, Height));
      Assert_True
        (By_Procedure.Get_First_Accessible_Child /= Null_Gtk_Accessible);
      Assert_True
        (By_Procedure.Get_Accessible_Parent = Null_Gtk_Accessible);
      Assert_True
        (By_Procedure.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  Exercise accessible relationship updates and state operations with
      --  valid values; null links leave this standalone widget unparented.
      By_Procedure.Set_Accessible_Parent
        (Null_Gtk_Accessible, Null_Gtk_Accessible);
      By_Procedure.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True
        (By_Procedure.Get_Accessible_Parent = Null_Gtk_Accessible);
      Assert_True
        (By_Procedure.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      By_Procedure.Set_Focusable (False);
      Assert_False
        (By_Procedure.Get_Platform_State
           (Accessible_Platform_State_Focusable));
      By_Procedure.Set_Focusable (True);
      By_Procedure.Update_Platform_State (Accessible_Platform_State_Focusable);
      Assert_True
        (By_Procedure.Get_Platform_State
           (Accessible_Platform_State_Focusable));
      By_Procedure.Reset_Property (Accessible_Property_Label);
      By_Procedure.Reset_Relation (Accessible_Relation_Labelled_By);
      By_Procedure.Reset_State (Accessible_State_Busy);
      By_Procedure.Announce
        ("flow box child test", Accessible_Announcement_Priority_Low);

      --  Insertion wraps an ordinary widget in a FlowBoxChild, whose index
      --  and selected state reflect the containing flow box.
      Box.Append (Gtk_Label_New ("inserted child"));
      Child := Box.Get_Child_At_Index (0);
      Assert_Cmpint_Eq (Child.Get_Index, 0);
      Assert_False (Child.Is_Selected);
      Box.Select_Child (Child);
      Assert_True (Child.Is_Selected);
      Child.Changed;

      Unref (By_Function);
      Unref (By_Procedure);
      Unref (Box);
   end Test_Child_Api;

   ------------------------
   -- Test_Child_Signals --
   ------------------------

   procedure Test_Child_Signals is
      Child : constant Gtk_Flow_Box_Child := Gtk_Flow_Box_Child_New;
      Exceptional : constant Gtk_Flow_Box_Child := Gtk_Flow_Box_Child_New;
      Slot : constant Gtk_Label := Gtk_Label_New ("child signal slot");
      Name : Interfaces.C.Strings.chars_ptr;
   begin
      Ref_Sink (Child);
      Ref_Sink (Exceptional);
      Ref_Sink (Slot);

      --  Register both generated marshallers for GtkFlowBoxChild::activate.
      Child_Signal_Calls := 0;
      Typed_Child_Signal_Object := System.Null_Address;
      Slot_Child_Signal_Object := System.Null_Address;
      Child.On_Activate (On_Typed_Child_Activate'Unrestricted_Access);
      Child.On_Activate
        (On_Slot_Child_Activate'Unrestricted_Access, Slot);

      --  Typed callbacks receive the signal instance; slot callbacks receive
      --  the separately supplied user-data object.
      Name := Interfaces.C.Strings.New_String ("activate");
      Emit_Child_Activate (Child.Get_Object, Name);
      Interfaces.C.Strings.Free (Name);
      Assert_Cmpint_Eq (Gint (Child_Signal_Calls), 2);
      Assert_True (Typed_Child_Signal_Object = Child.Get_Object);
      Assert_True (Slot_Child_Signal_Object = Slot.Get_Object);

      --  Callback exceptions must be reported to GtkAda's handler rather than
      --  escaping through GLib's C signal emitter.
      Child_Signal_Exceptions := 0;
      Gtkada.Bindings.Set_On_Exception
        (Note_Child_Signal_Exception'Unrestricted_Access);
      Exceptional.On_Activate
        (Raise_Typed_Child_Activate'Unrestricted_Access);
      Exceptional.On_Activate
        (Raise_Slot_Child_Activate'Unrestricted_Access, Slot);
      Name := Interfaces.C.Strings.New_String ("activate");
      Emit_Child_Activate (Exceptional.Get_Object, Name);
      Interfaces.C.Strings.Free (Name);
      Assert_Cmpint_Eq (Gint (Child_Signal_Exceptions), 2);

      Unref (Slot);
      Unref (Exceptional);
      Unref (Child);
   end Test_Child_Signals;

   -----------------------
   -- Test_Measure_Crash --
   -----------------------

   procedure Test_Measure_Crash is
      Box : constant Gtk_Flow_Box := Gtk_Flow_Box_New;
      function New_Css_Child return Gtk_Flow_Box_Child;
      function New_Css_Child return Gtk_Flow_Box_Child is
         Child : constant Gtk_Flow_Box_Child :=
           new Gtk_Flow_Box_Child_Record;
         Params : Glib.Values.GParameter_Array (1 .. 1);
      begin
         --  css-name is construct-only, so provide it through GObject creation.
         Params (1).Name := Gtkada.Types.New_String ("css-name");
         Glib.Values.Init (Params (1).Value, GType_String);
         Glib.Values.Set_String (Params (1).Value, "nopadding");
         Glib.Values.G_New (Child, Gtk.Flow_Box_Child.Get_Type, Params);
         Glib.Values.Free (Params);
         return Child;
      end New_Css_Child;
      Child : constant Gtk_Flow_Box_Child := New_Css_Child;
      Minimum, Natural, Minimum_Baseline, Natural_Baseline : Gint;
   begin
      Ref_Sink (Box);
      Ref_Sink (Child);

      --  Port of flowbox.c:test_measure_crash: an empty FlowBoxChild with a
      --  custom CSS node used to crash when a vertical row has zero spacing.
      Box.Set_Valign (Align_Start);
      Box.Append (Child);
      Box.Set_Orientation (Orientation_Vertical);
      Box.Set_Row_Spacing (0);
      Gtk.Widget.Measure
        (Gtk_Widget (Box), Orientation_Vertical, -1, Minimum, Natural,
         Minimum_Baseline, Natural_Baseline);
      Assert_True (Minimum >= 0 and Natural >= Minimum);
      Unref (Box);
   end Test_Measure_Crash;

   ------------------
   -- Test_Signals --
   ------------------

   procedure Test_Signals is
      Box : constant Gtk_Flow_Box := Fill (Gtk_Flow_Box_New, False);
      Slot : constant Gtk_Label := Gtk_Label_New ("signal slot");
      Move_Return : aliased Glib.Gboolean := 0;

      procedure Emit_No_Args (Name : String);
      procedure Emit_Child_Activated;
      procedure Check_No_Args (Name : String);
      procedure Emit_Move_Cursor;

      procedure Emit_No_Args (Name : String) is
         C_Name : Interfaces.C.Strings.chars_ptr :=
           Interfaces.C.Strings.New_String (Name);
      begin
         --  Action signals have no arguments beyond the instance and name.
         Emit_No_Args (Box.Get_Object, C_Name);
         Interfaces.C.Strings.Free (C_Name);
      end Emit_No_Args;

      procedure Emit_Child_Activated is
         C_Name : Interfaces.C.Strings.chars_ptr :=
           Interfaces.C.Strings.New_String ("child-activated");
      begin
         --  The child pointer is the sole signal argument after instance/name.
         Emit_Child
           (Box.Get_Object, C_Name, Signal_Child.Get_Object);
         Interfaces.C.Strings.Free (C_Name);
      end Emit_Child_Activated;

      procedure Check_No_Args (Name : String) is
         Before_Typed : constant Natural := Typed_Signal_Calls;
         Before_Slot : constant Natural := Slot_Signal_Calls;
      begin
         Emit_No_Args (Name);
         Assert_True (Typed_Signal_Calls > Before_Typed);
         Assert_True (Slot_Signal_Calls > Before_Slot);
      end Check_No_Args;

      procedure Emit_Move_Cursor is
         C_Name : Interfaces.C.Strings.chars_ptr :=
           Interfaces.C.Strings.New_String ("move-cursor");
      begin
         --  C varargs promote the enum, count and booleans to int values.
         Emit_Move
           (Box.Get_Object, C_Name,
            Interfaces.C.int (Gtk_Movement_Step'Pos (Movement_Display_Lines)),
            -2, 1, 0, Move_Return'Address);
         Interfaces.C.Strings.Free (C_Name);
      end Emit_Move_Cursor;
   begin
      Ref_Sink (Box);
      Ref_Sink (Slot);
      Box.Set_Selection_Mode (Selection_Multiple);
      Signal_Child := Box.Get_Child_At_Index (2);

      --  Register both callback forms for each FlowBox signal signature.
      Box.On_Activate_Cursor_Child (On_Typed_Void'Unrestricted_Access);
      Box.On_Activate_Cursor_Child
        (On_Slot_Void'Unrestricted_Access, Slot);
      Box.On_Child_Activated (On_Typed_Child'Unrestricted_Access);
      Box.On_Child_Activated
        (On_Slot_Child'Unrestricted_Access, Slot);
      Box.On_Move_Cursor (On_Typed_Move'Unrestricted_Access);
      Box.On_Move_Cursor (On_Slot_Move'Unrestricted_Access, Slot);
      Box.On_Select_All (On_Typed_Void'Unrestricted_Access);
      Box.On_Select_All (On_Slot_Void'Unrestricted_Access, Slot);
      Box.On_Selected_Children_Changed
        (On_Typed_Void'Unrestricted_Access);
      Box.On_Selected_Children_Changed
        (On_Slot_Void'Unrestricted_Access, Slot);
      Box.On_Toggle_Cursor_Child (On_Typed_Void'Unrestricted_Access);
      Box.On_Toggle_Cursor_Child
        (On_Slot_Void'Unrestricted_Access, Slot);
      Box.On_Unselect_All (On_Typed_Void'Unrestricted_Access);
      Box.On_Unselect_All (On_Slot_Void'Unrestricted_Access, Slot);

      --  Emission invokes each generated typed and slot marshaller directly.
      Typed_Signal_Calls := 0;
      Slot_Signal_Calls := 0;
      Check_No_Args ("activate-cursor-child");
      Signal_Arguments_Ok := True;
      Typed_Signal_Calls := 0;
      Slot_Signal_Calls := 0;
      Emit_Child_Activated;
      Assert_True (Typed_Signal_Calls > 0 and Slot_Signal_Calls > 0);
      Assert_True (Signal_Arguments_Ok);

      Signal_Arguments_Ok := True;
      Emit_Move_Cursor;
      Assert_True (Typed_Signal_Calls > 1 and Slot_Signal_Calls > 1);
      Assert_True (Signal_Arguments_Ok);
      Assert_True (Move_Return /= 0);

      Check_No_Args ("select-all");
      Check_No_Args ("selected-children-changed");
      Check_No_Args ("toggle-cursor-child");
      Check_No_Args ("unselect-all");

      Unref (Slot);
      Unref (Box);
   end Test_Signals;

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
   Glib.Test.Add_Func
     ("/flowbox/user-data-callbacks",
      Test_User_Data_Callbacks'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/flowbox/additional-api", Test_Additional_Api'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/flowbox/child-api", Test_Child_Api'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/flowbox/child-signals", Test_Child_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/flowbox/measure-crash", Test_Measure_Crash'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/flowbox/signals", Test_Signals'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Flow_Box;
