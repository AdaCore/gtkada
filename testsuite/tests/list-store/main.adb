--  Exercises Glib.List_Store.Find, whose body is hand-written in
--  contrib/binding/packages/GListStore.toml. Position is an Ada 2012 "out"
--  parameter of a function, so the point of interest is that the value C
--  wrote into the wrapper's temporary really is copied back to the caller.
--
--  No widget is created, so this test needs no display.

with Glib;                use Glib;
with Glib.List_Store;     use Glib.List_Store;
with Glib.Simple_Action;  use Glib.Simple_Action;
with Glib.Test;           use Glib.Test;
with Ada.Command_Line;

procedure Main is

   procedure Test_Find
   with Convention => C;

   procedure Test_Find_Absent
   with Convention => C;

   function New_Action (Name : String) return Gsimple_Action;
   --  A stateless GSimpleAction, used here only as a plain GObject to put in
   --  the store.

   ----------------
   -- New_Action --
   ----------------

   function New_Action (Name : String) return Gsimple_Action is
   begin
      return Gsimple_Action_New (Name, Parameter_Type => null);
   end New_Action;

   ---------------
   -- Test_Find --
   ---------------

   procedure Test_Find is
      Store    : Glist_Store;
      Items    : array (0 .. 2) of Gsimple_Action;
      Position : Guint;
   begin
      G_New (Store);

      for J in Items'Range loop
         Items (J) := New_Action ("action" & Integer'Image (J));
         Store.Append (Items (J));
      end loop;

      --  Position must come back set for every item.

      for J in Items'Range loop
         Position := Guint'Last;
         Assert_True (Store.Find (Items (J), Position));
         Assert_Cmpuint_Eq (Position, Guint (J));
      end loop;

      Unref (Store);
   end Test_Find;

   ----------------------
   -- Test_Find_Absent --
   ----------------------

   procedure Test_Find_Absent is
      Store    : Glist_Store;
      Item     : constant Gsimple_Action := New_Action ("in-store");
      Absent   : constant Gsimple_Action := New_Action ("not-in-store");
      Position : Guint := Guint'Last;
   begin
      G_New (Store);
      Store.Append (Item);

      Assert_False (Store.Find (Absent, Position));

      Unref (Store);
   end Test_Find_Absent;

begin
   Glib.Test.Init;

   Glib.Test.Add_Func
     ("/list-store/find", Test_Find'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/list-store/find-absent", Test_Find_Absent'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
