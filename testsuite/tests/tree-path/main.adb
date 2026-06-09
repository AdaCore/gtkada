--  Ada port of GTK's testsuite/gtk/treepath.c (GTK 4.22.2).
--
--  Exercises the Gtk_Tree_Path API from Gtk.Tree_Model: building paths by
--  appending and prepending indices, parsing from and rendering to strings,
--  constructing from an index array, and navigating (next/prev/up/down) with
--  the associated ancestor/descendant relationships.

with Glib;             use Glib;
with Glib.Test;        use Glib.Test;
with Gtk.Tree_Model;   use Gtk.Tree_Model;
with Ada.Command_Line;

procedure Main is

   procedure Test_Append
   with Convention => C;

   procedure Test_Prepend
   with Convention => C;

   procedure Test_To_String
   with Convention => C;

   procedure Test_From_Indices
   with Convention => C;

   procedure Test_First
   with Convention => C;

   procedure Test_Navigation
   with Convention => C;

   -----------------
   -- Test_Append --
   -----------------

   procedure Test_Append is
      P : constant Gtk_Tree_Path := Gtk_Tree_Path_New;
   begin
      for I in 0 .. 99 loop
         Assert_Cmpint_Eq (Get_Depth (P), Gint (I));
         Append_Index (P, Gint (I));
      end loop;

      declare
         Indices : constant Gint_Array := Get_Indices (P);
      begin
         for I in 0 .. 99 loop
            Assert_Cmpint_Eq (Indices (I), Gint (I));
         end loop;
      end;

      Path_Free (P);
   end Test_Append;

   ------------------
   -- Test_Prepend --
   ------------------

   procedure Test_Prepend is
      P : constant Gtk_Tree_Path := Gtk_Tree_Path_New;
   begin
      for I in 0 .. 99 loop
         Assert_Cmpint_Eq (Get_Depth (P), Gint (I));
         Prepend_Index (P, Gint (I));
      end loop;

      declare
         Indices : constant Gint_Array := Get_Indices (P);
      begin
         for I in 0 .. 99 loop
            Assert_Cmpint_Eq (Indices (I), Gint (99 - I));
         end loop;
      end;

      Path_Free (P);
   end Test_Prepend;

   --------------------
   -- Test_To_String --
   --------------------

   procedure Test_To_String is
      Str : constant String := "0:1:2:3:4:5:6:7:8:9:10";
      P   : constant Gtk_Tree_Path := Gtk_Tree_Path_New_From_String (Str);
   begin
      declare
         Indices : constant Gint_Array := Get_Indices (P);
      begin
         for I in 0 .. 9 loop
            Assert_Cmpint_Eq (Indices (I), Gint (I));
         end loop;
      end;

      Assert_Cmpstr_Eq (To_String (P), Str);

      Path_Free (P);
   end Test_To_String;

   -----------------------
   -- Test_From_Indices --
   -----------------------

   procedure Test_From_Indices is
      Idx : constant Gint_Array := (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
      P   : constant Gtk_Tree_Path :=
        Gtk_Tree_Path_New_From_Indicesv (Idx, Idx'Length);
   begin
      Assert_Cmpint_Eq (Get_Depth (P), 10);

      declare
         Indices : constant Gint_Array := Get_Indices (P);
      begin
         for I in 0 .. 9 loop
            Assert_Cmpint_Eq (Indices (I), Gint (I));
         end loop;
      end;

      Path_Free (P);
   end Test_From_Indices;

   ----------------
   -- Test_First --
   ----------------

   procedure Test_First is
      P : constant Gtk_Tree_Path := Gtk_Tree_Path_New_First;
   begin
      Assert_Cmpint_Eq (Get_Depth (P), 1);
      Assert_Cmpint_Eq (Get_Indices (P) (0), 0);
      Path_Free (P);
   end Test_First;

   ---------------------
   -- Test_Navigation --
   ---------------------

   procedure Test_Navigation is
      Idx : constant Gint_Array := (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
      P   : constant Gtk_Tree_Path :=
        Gtk_Tree_Path_New_From_Indicesv (Idx, Idx'Length);
      Q   : constant Gtk_Tree_Path := Copy (P);
      Res : Boolean;
   begin
      Assert_True (Compare (P, Q) = 0);

      Next (Q);
      declare
         Pi : constant Gint_Array := Get_Indices (P);
         Qi : constant Gint_Array := Get_Indices (Q);
      begin
         for I in 0 .. 8 loop
            Assert_Cmpint_Eq (Pi (I), Qi (I));
         end loop;
         Assert_Cmpint_Eq (Qi (9), Pi (9) + 1);
      end;

      Assert_False (Is_Ancestor (P, Q));
      Assert_False (Is_Ancestor (Q, P));
      Assert_False (Is_Descendant (P, Q));
      Assert_False (Is_Descendant (Q, P));

      Res := Prev (Q);
      Assert_True (Res);
      Assert_True (Compare (P, Q) = 0);

      Assert_False (Is_Ancestor (P, Q));
      Assert_False (Is_Ancestor (Q, P));
      Assert_False (Is_Descendant (P, Q));
      Assert_False (Is_Descendant (Q, P));

      Down (Q);

      Assert_True (Compare (P, Q) < 0);

      Assert_True (Is_Ancestor (P, Q));
      Assert_False (Is_Ancestor (Q, P));
      Assert_False (Is_Descendant (P, Q));
      Assert_True (Is_Descendant (Q, P));

      Res := Prev (Q);
      Assert_False (Res);

      Res := Up (Q);
      Assert_True (Res);
      Assert_True (Compare (P, Q) = 0);

      Assert_Cmpint_Eq (Get_Depth (Q), 10);
      Res := Up (Q);
      Assert_True (Res);
      Assert_Cmpint_Eq (Get_Depth (Q), 9);

      Path_Free (P);
      Path_Free (Q);
   end Test_Navigation;

begin
   Glib.Test.Init;

   Glib.Test.Add_Func
     ("/tree-path/append", Test_Append'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-path/prepend", Test_Prepend'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-path/to-string", Test_To_String'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-path/from-indices", Test_From_Indices'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-path/first", Test_First'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-path/navigation", Test_Navigation'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
