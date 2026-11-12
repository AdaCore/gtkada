--  Exercise Gtkada.Types string allocation, bounded conversions, and arrays.
--  There is no corresponding C test: these are GtkAda's Ada-side helpers.

with Ada.Command_Line;
with Interfaces.C;
with Interfaces.C.Strings;
with Glib.Test;
with Gtkada.Types;

procedure Main is
   use Glib.Test;
   use Gtkada.Types;
   use type Gtkada.Types.Chars_Ptr;
   use type Interfaces.C.size_t;
   use type Interfaces.C.Strings.chars_ptr;

   procedure Test_String_Values
   with Convention => C;

   procedure Test_String_Values is
      Text   : Chars_Ptr := New_String ("hello");
      Prefix : constant Interfaces.C.char_array :=
        Value (Text, Interfaces.C.size_t (3));
   begin
      --  Check both the nul-terminated and explicitly bounded conversions.
      Assert_Cmpstr_Eq (Value (Text), "hello");
      Assert_Cmpstr_Eq (Value (Text, Interfaces.C.size_t (3)), "hel");

      --  The char_array overload preserves exactly the requested prefix.
      Assert_True (Prefix'Length = 3);
      Assert_True (Interfaces.C.To_Ada (Prefix (Prefix'First)) = 'h');
      Assert_True
        (Interfaces.C.To_Ada
           (Prefix (Prefix'First + Interfaces.C.size_t (2))) = 'l');

      --  New_String storage belongs to Gtk and must be released with g_free.
      Free (Text);
   end Test_String_Values;

   procedure Test_String_Array_Operators
   with Convention => C;

   procedure Test_String_Array_Operators is
      Empty : constant Chars_Ptr_Array := Null_Array;
   begin
      --  Null_Array is an empty array, useful when building optional lists.
      Assert_True (Empty'Length = 0);

      declare
         First : Chars_Ptr_Array := "one" + "two";
      begin
         --  Appending a String allocates a new entry in the returned array.
         declare
            Second : Chars_Ptr_Array := First + "three";
         begin
            --  Appending an existing pointer copies it without reallocating it.
            declare
               Third : Chars_Ptr_Array := Second + New_String ("four");
            begin
               Assert_True (Third'First = 0 and Third'Last = 3);
               Assert_Cmpstr_Eq (Value (Third (0)), "one");
               Assert_Cmpstr_Eq (Value (Third (1)), "two");
               Assert_Cmpstr_Eq (Value (Third (2)), "three");
               Assert_Cmpstr_Eq (Value (Third (3)), "four");

               --  These arrays share pointers; only the final array owns them.
               Free (Third);
               Assert_True (Third (0) = Null_Ptr);
               Assert_True (Third (3) = Null_Ptr);
            end;
         end;
      end;

      declare
         First : Chars_Ptr := New_String ("left");
         Pair  : Chars_Ptr_Array := First + "right";
      begin
         --  The pointer/String overload keeps its pointer and allocates the tail.
         Assert_Cmpstr_Eq (Value (Pair (0)), "left");
         Assert_Cmpstr_Eq (Value (Pair (1)), "right");
         Free (Pair);
      end;
   end Test_String_Array_Operators;

   procedure Test_C_String_Array_Free
   with Convention => C;

   procedure Test_C_String_Array_Free is
      --  This overload owns strings allocated by Interfaces.C.Strings.
      Items : Interfaces.C.Strings.chars_ptr_array (0 .. 1) :=
        (0 => Interfaces.C.Strings.New_String ("first"),
         1 => Interfaces.C.Strings.New_String ("second"));
   begin
      Assert_Cmpstr_Eq (Interfaces.C.Strings.Value (Items (0)), "first");
      Assert_Cmpstr_Eq (Interfaces.C.Strings.Value (Items (1)), "second");

      --  Free releases each C string and clears its array slot.
      Gtkada.Types.Free (Items);
      Assert_True (Items (0) = Interfaces.C.Strings.Null_Ptr);
      Assert_True (Items (1) = Interfaces.C.Strings.Null_Ptr);
   end Test_C_String_Array_Free;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gtkada-types/string-values", Test_String_Values'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gtkada-types/string-array-operators",
      Test_String_Array_Operators'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gtkada-types/c-string-array-free",
      Test_C_String_Array_Free'Unrestricted_Access);

   --  Return the GLib test runner's status to the testsuite driver.
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
