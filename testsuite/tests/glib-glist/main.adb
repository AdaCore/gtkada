--  Unit test for Glib.Glist.Generic_List, written from scratch because the
--  C testsuite has no test for this GtkAda generic list wrapper.
--  The integer values below are only opaque pointer tokens: GLib stores and
--  compares them but never dereferences them.

with Ada.Command_Line;
with System;
with System.Storage_Elements;

with Glib;
with Glib.Glist;
with Glib.Test; use Glib.Test;

procedure Main is

   type Datum is record
      Value : System.Storage_Elements.Integer_Address;
   end record;

   --  Convert integer tokens to opaque addresses and back for the generic.
   function To_Address (Value : Datum) return System.Address is
   begin
      return System.Storage_Elements.To_Address
        (Value.Value);
   end To_Address;

   function To_Datum (Value : System.Address) return Datum is
   begin
      return (Value => System.Storage_Elements.To_Integer (Value));
   end To_Datum;

   package Lists is new Glib.Glist.Generic_List
     (Datum, To_Address, To_Datum);

   use type Lists.Glist;
   use type Glib.Gint;
   use type System.Address;
   use type System.Storage_Elements.Integer_Address;

   --  Check allocation, data conversion, traversal, lookup, edits, and free.
   procedure Test_List with Convention => C;

   procedure Test_List is
      List     : Lists.Glist := Lists.Null_List;
      Link     : Lists.Glist;
      Detached : Lists.Glist;
      Outside  : Lists.Glist;
   begin
      --  Alloc creates a standalone link; its address is available through
      --  Get_Gpointer, while its data field is deliberately left unread.
      Lists.Alloc (Outside);
      Assert_True (Lists.Is_Created (Outside));
      Assert_True (Lists.Get_Gpointer (Outside).Value /= 0);
      Lists.Free (Outside);
      Assert_True (Outside = Lists.Null_List);
      Assert_False (Lists.Is_Created (Outside));

      --  Prepend and Append both accept an empty list and return its head.
      Assert_False (Lists.Is_Created (List));
      Lists.Append (List, (Value => 20));
      Lists.Prepend (List, (Value => 10));
      Lists.Append (List, (Value => 30));
      Assert_True (Lists.Is_Created (List));
      Assert_Cmpuint_Eq (Lists.Length (List), 3);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Nth_Data (List, 0).Value), 10);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Nth_Data (List, 1).Value), 20);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Nth_Data (List, 2).Value), 30);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Get_Data (List).Value), 10);
      Assert_True
        (Lists.Get_Data_Address (List) = To_Address ((Value => 10)));

      --  First and Last work from either the head or an interior link.
      Link := Lists.Nth (List, 1);
      Assert_True (Link /= Lists.Null_List);
      Assert_True (Lists.First (Link) = List);
      Assert_True (Lists.Last (Link) = Lists.Nth (List, 2));
      Assert_Cmpint_Eq (Lists.Position (List, Link), 1);
      Assert_True (Lists.Next (List) = Link);
      Assert_True (Lists.Prev (Link) = List);
      Assert_True (Lists.Prev (List) = Lists.Null_List);
      Assert_True (Lists.Next (Lists.Last (List)) = Lists.Null_List);
      Assert_True (Lists.Nth (List, 3) = Lists.Null_List);
      Assert_Cmpint_Eq (Lists.Position (List, Outside), -1);

      --  Find and Index compare the converted data values; a miss is null/-1.
      Assert_True (Lists.Find (List, (Value => 20)) = Link);
      Assert_True
        (Lists.Find (List, (Value => 99)) = Lists.Null_List);
      Assert_Cmpint_Eq (Lists.Index (List, (Value => 20)), 1);
      Assert_Cmpint_Eq (Lists.Index (List, (Value => 99)), -1);

      --  Insert accepts the documented beginning, middle, and end positions.
      Lists.Insert (List, (Value => 5), 0);
      Lists.Insert (List, (Value => 15), 2);
      Lists.Insert (List, (Value => 40), -1);
      Assert_Cmpuint_Eq (Lists.Length (List), 6);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Nth_Data (List, 0).Value), 5);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Nth_Data (List, 2).Value), 15);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Nth_Data (List, 5).Value), 40);

      --  Reverse changes the traversal order and can be reversed again.
      Lists.List_Reverse (List);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Nth_Data (List, 0).Value), 40);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Nth_Data (List, 5).Value), 5);
      Lists.List_Reverse (List);

      --  Remove deletes the first matching value; a missing value is harmless.
      Lists.Remove (List, (Value => 20));
      Lists.Remove (List, (Value => 99));
      Assert_Cmpuint_Eq (Lists.Length (List), 5);
      Assert_Cmpint_Eq (Lists.Index (List, (Value => 20)), -1);

      --  Remove_Link detaches the selected cell, which can then be freed alone.
      Detached := Lists.Nth (List, 2);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Get_Data (Detached).Value), 15);
      Lists.Remove_Link (List, Detached);
      Assert_Cmpuint_Eq (Lists.Length (List), 4);
      Assert_True (Lists.Prev (Detached) = Lists.Null_List);
      Assert_True (Lists.Next (Detached) = Lists.Null_List);
      Assert_Cmpint_Eq (Glib.Gint (Lists.Get_Data (Detached).Value), 15);
      Lists.Free (Detached);

      --  Free releases only the link cells and resets the Ada list handle.
      Lists.Free (List);
      Assert_True (List = Lists.Null_List);
      Assert_False (Lists.Is_Created (List));
   end Test_List;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/glib-glist/operations", Test_List'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
