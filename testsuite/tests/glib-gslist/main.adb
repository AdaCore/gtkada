--  Unit test for Glib.GSlist.Generic_SList, written from scratch because
--  the C testsuite has no dedicated test for this GtkAda generic wrapper.
--  Integer values are opaque pointer tokens: GLib stores and compares them,
--  but never dereferences them.

with Ada.Command_Line;
with System;
with System.Storage_Elements;

with Glib;
with Glib.GSlist;
with Glib.Test; use Glib.Test;

procedure Main is

   type Datum is record
      Value : System.Storage_Elements.Integer_Address;
   end record;

   --  Convert integer tokens to opaque addresses for the generic instance.
   function To_Address (Value : Datum) return System.Address is
   begin
      return System.Storage_Elements.To_Address (Value.Value);
   end To_Address;

   function To_Datum (Value : System.Address) return Datum is
   begin
      return (Value => System.Storage_Elements.To_Integer (Value));
   end To_Datum;

   package Lists is new Glib.GSlist.Generic_SList
     (Datum, To_Address, To_Datum);

   use type Lists.GSlist;
   use type Glib.Gint;
   use type System.Address;
   use type System.Storage_Elements.Integer_Address;

   --  Check allocation, conversion, traversal, lookup, edits, and freeing.
   procedure Test_List with Convention => C;

   procedure Test_List is
      List     : Lists.GSlist := Lists.Null_List;
      Link     : Lists.GSlist;
      Detached : Lists.GSlist;
      Outside  : Lists.GSlist;
   begin
      --  Alloc creates a standalone cell. Free it without reading its
      --  uninitialized data field. Keep it alive until the membership check.
      Lists.Alloc (Outside);
      Assert_True (Lists.Get_Object (Outside) /= System.Null_Address);

      --  Append and Prepend accept an empty list and keep the expected head.
      Lists.Append (List, (Value => 20));
      Lists.Prepend (List, (Value => 10));
      Lists.Append (List, (Value => 30));
      Assert_Cmpuint_Eq (Lists.Length (List), 3);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Nth_Data (List, 0).Value), 10);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Nth_Data (List, 1).Value), 20);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Nth_Data (List, 2).Value), 30);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Get_Data (List).Value), 10);
      Assert_True
        (Lists.Get_Data_Address (List) = To_Address ((Value => 10)));

      --  GSlist values are links. Next advances to a link, while Last and
      --  Nth return links that can be compared by their list-cell address.
      Link := Lists.Nth (List, 1);
      Assert_True (Link /= Lists.Null_List);
      Assert_True (Lists.Next (List) = Link);
      Assert_True (Lists.Last (Link) = Lists.Nth (List, 2));
      Assert_Cmpint_Eq (Lists.Position (List, Link), 1);
      Assert_Cmpint_Eq (Lists.Position (List, Outside), -1);
      Assert_True (Lists.Nth (List, 3) = Lists.Null_List);
      Lists.Free (Outside);

      --  Find and Index compare converted values; a miss returns null/-1.
      Assert_True (Lists.Find (List, (Value => 20)) = Link);
      Assert_True (Lists.Find (List, (Value => 99)) = Lists.Null_List);
      Assert_Cmpint_Eq (Lists.Index (List, (Value => 20)), 1);
      Assert_Cmpint_Eq (Lists.Index (List, (Value => 99)), -1);

      --  Insert covers the beginning, middle, and append positions.
      Lists.Insert (List, (Value => 5), 0);
      Lists.Insert (List, (Value => 15), 2);
      Lists.Insert (List, (Value => 40), -1);
      Assert_Cmpuint_Eq (Lists.Length (List), 6);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Nth_Data (List, 0).Value), 5);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Nth_Data (List, 2).Value), 15);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Nth_Data (List, 5).Value), 40);

      --  Reverse changes traversal order and can be reversed again.
      Lists.List_Reverse (List);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Nth_Data (List, 0).Value), 40);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Nth_Data (List, 5).Value), 5);
      Lists.List_Reverse (List);

      --  Remove deletes the first equal value; removing a miss is harmless.
      Lists.Remove (List, (Value => 20));
      Lists.Remove (List, (Value => 99));
      Assert_Cmpuint_Eq (Lists.Length (List), 5);
      Assert_Cmpint_Eq (Lists.Index (List, (Value => 20)), -1);

      --  Remove_Link detaches one cell. It remains valid until freed alone.
      Detached := Lists.Nth (List, 2);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Get_Data (Detached).Value), 15);
      Lists.Remove_Link (List, Detached);
      Assert_Cmpuint_Eq (Lists.Length (List), 4);
      Assert_True (Lists.Next (Detached) = Lists.Null_List);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Get_Data (Detached).Value), 15);
      Lists.Free (Detached);

      --  Free releases the cells. The wrapper does not clear its handle, so
      --  avoid inspecting the stale value after this call.
      Lists.Free (List);
   end Test_List;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/glib-gslist/operations", Test_List'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
