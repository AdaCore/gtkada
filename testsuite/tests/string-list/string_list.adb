--  Unit test for Gtk.String_List and Gtk.String_Object.
--
--  GTK has no stringlist.c to port, so this is written from scratch.
--
--  The Take case is a regression test: gtk_string_list_take is
--  transfer-ownership='full' on its string, so the binding must hand its
--  allocation over to GTK rather than free it. Freeing it left the model
--  holding a dangling pointer, and the Unref at the end of the case --
--  which makes GTK free the strings it believes it owns -- turned that
--  into a double free.

with Ada.Command_Line;

with Glib;              use Glib;
with Glib.List_Model;   use Glib.List_Model;
with Glib.Object;       use Glib.Object;
with Glib.Test;         use Glib.Test;

with GNAT.Strings;

with Gtk.Main;
with Gtk.String_List;   use Gtk.String_List;
with Gtk.String_Object; use Gtk.String_Object;

procedure String_List is

   function New_Strings return Gtk_String_List;

   procedure Test_Basics with Convention => C;
   procedure Test_Splice with Convention => C;
   procedure Test_Take with Convention => C;
   procedure Test_Items with Convention => C;

   -----------------
   -- New_Strings --
   -----------------

   function New_Strings return Gtk_String_List is
      Items : GNAT.Strings.String_List :=
        (new String'("cherry"), new String'("apple"), new String'("banana"));
   begin
      return Gtk_String_List_New (Items);
   end New_Strings;

   -----------------
   -- Test_Basics --
   -----------------

   procedure Test_Basics is
      List : constant Gtk_String_List := New_Strings;
   begin
      Assert_Cmpuint_Eq (List.Get_N_Items, 3);
      Assert_Cmpstr_Eq (List.Get_String (0), "cherry");
      Assert_Cmpstr_Eq (List.Get_String (2), "banana");

      List.Append ("damson");
      Assert_Cmpuint_Eq (List.Get_N_Items, 4);
      Assert_Cmpstr_Eq (List.Get_String (3), "damson");

      List.Remove (0);
      Assert_Cmpuint_Eq (List.Get_N_Items, 3);
      Assert_Cmpstr_Eq (List.Get_String (0), "apple");

      Unref (List);
   end Test_Basics;

   -----------------
   -- Test_Splice --
   -----------------

   procedure Test_Splice is
      List      : constant Gtk_String_List := New_Strings;
      Additions : GNAT.Strings.String_List :=
        (new String'("elderberry"), new String'("fig"));
   begin
      --  Replace the two last items with the two additions.
      List.Splice (1, 2, Additions);
      Assert_Cmpuint_Eq (List.Get_N_Items, 3);
      Assert_Cmpstr_Eq (List.Get_String (0), "cherry");
      Assert_Cmpstr_Eq (List.Get_String (1), "elderberry");
      Assert_Cmpstr_Eq (List.Get_String (2), "fig");

      --  Splice copies its additions, so they are ours to free.
      for Addition of Additions loop
         GNAT.Strings.Free (Addition);
      end loop;
      Assert_Cmpstr_Eq (List.Get_String (1), "elderberry");

      Unref (List);
   end Test_Splice;

   ---------------
   -- Test_Take --
   ---------------

   procedure Test_Take is
      Items : GNAT.Strings.String_List (1 .. 0);
      List  : constant Gtk_String_List := Gtk_String_List_New (Items);
   begin
      Assert_Cmpuint_Eq (List.Get_N_Items, 0);

      List.Take ("cherry");
      List.Take ("apple");
      Assert_Cmpuint_Eq (List.Get_N_Items, 2);

      --  Reading the taken strings back: a string freed on this side would
      --  have left the model pointing at released memory.
      Assert_Cmpstr_Eq (List.Get_String (0), "cherry");
      Assert_Cmpstr_Eq (List.Get_String (1), "apple");
      Assert_Cmpstr_Eq
        (Gtk_String_Object (List.Get_Item (1)).Get_String, "apple");

      --  Removing one makes GTK free the string it took ownership of.
      List.Remove (0);
      Assert_Cmpuint_Eq (List.Get_N_Items, 1);
      Assert_Cmpstr_Eq (List.Get_String (0), "apple");

      --  And this frees the other one.
      Unref (List);
   end Test_Take;

   ----------------
   -- Test_Items --
   ----------------

   procedure Test_Items is
      List : constant Gtk_String_List := New_Strings;
      Obj  : constant GObject := List.Get_Item (1);
   begin
      --  Not compared against Gtk.String_Object.Get_Type: GtkStringList
      --  answers g_list_model_get_item_type with the generic G_TYPE_OBJECT,
      --  so only the fact that a type comes back at all is checked here.
      Assert_True (List.Get_Item_Type /= GType_Invalid);
      Assert_Cmpuint_Eq (Get_N_Items (+List), 3);

      --  Get_Item returns a GObject rather than a bare address, so no
      --  Unchecked_Conversion is needed to reach the string object.
      Assert_True (Obj /= null);
      Assert_Cmpstr_Eq (Gtk_String_Object (Obj).Get_String, "apple");

      Assert_Cmpuint_Eq (List.Find ("banana"), 2);

      Unref (List);
   end Test_Items;

begin
   Glib.Test.Init;

   --  The list model itself needs no display, but Gtk.Main.Init keeps this
   --  in line with the other tests and costs nothing.
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/stringlist/basics", Test_Basics'Unrestricted_Access);
   Glib.Test.Add_Func ("/stringlist/splice", Test_Splice'Unrestricted_Access);
   Glib.Test.Add_Func ("/stringlist/take", Test_Take'Unrestricted_Access);
   Glib.Test.Add_Func ("/stringlist/items", Test_Items'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end String_List;
