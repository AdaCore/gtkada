--  Tests Gtk.Tree_List_Row accessors with a small, in-memory tree model.
--  GTK's C tests for tree-list models cover expansion, but do not check this
--  binding's child-row and parent-row wrappers directly.

with Ada.Command_Line;

with Glib;                          use Glib;
with Glib.List_Model;               use Glib.List_Model;
with Glib.Object;                   use Glib.Object;
with Glib.Test;                     use Glib.Test;

with GNAT.Strings;

with Gtk.Main;
with Gtk.String_List;                use Gtk.String_List;
with Gtk.String_Object;              use Gtk.String_Object;
with Gtk.Tree_List_Model;            use Gtk.Tree_List_Model;
with Gtk.Tree_List_Row;              use Gtk.Tree_List_Row;

procedure Tree_List_Row is
   use type Gtk.Tree_List_Row.Gtk_Tree_List_Row;

   function New_Root return Gtk_String_List;
   function Create_Children (Item : GObject) return Glist_Model;
   procedure Test_Row_Accessors with Convention => C;

   -----------------
   -- New_Root --
   -----------------

   function New_Root return Gtk_String_List is
      Items : GNAT.Strings.String_List :=
        (new String'("parent"), new String'("leaf"));
   begin
      return Gtk_String_List_New (Items);
   end New_Root;

   ----------------------
   -- Create_Children --
   ----------------------

   function Create_Children (Item : GObject) return Glist_Model is
      Items : GNAT.Strings.String_List :=
        (1 => new String'("child"));
   begin
      --  Only the first root item has a child; this gives the test both an
      --  expandable row and a leaf without any external model setup.
      if Gtk_String_Object (Item).Get_String = "parent" then
         return +Gtk_String_List_New (Items);
      else
         return Null_Glist_Model;
      end if;
   end Create_Children;

   -------------------------
   -- Test_Row_Accessors --
   -------------------------

   procedure Test_Row_Accessors is
      Root : constant Gtk_String_List := New_Root;
      Tree : Gtk_Tree_List_Model;
      Parent : Gtk_Tree_List_Row;
      Child : Gtk_Tree_List_Row;
      Parent_Of_Child : Gtk_Tree_List_Row;
      Children : Glist_Model;
      Item : GObject;
   begin
      --  Retain Root's reference while the tree model takes its own reference.
      Ref (Root);
      Tree := Gtk_Tree_List_Model_New
        (+Root, Passthrough => False, Autoexpand => False,
         Create_Func => Create_Children'Unrestricted_Access);

      Parent := Get_Row (Tree, 0);
      Assert_True (Parent /= null);
      Assert_Cmpuint_Eq (Parent.Get_Depth, 0);
      Assert_Cmpuint_Eq (Parent.Get_Position, 0);
      Assert_True (Parent.Is_Expandable);
      Assert_False (Parent.Get_Expanded);
      Assert_True (Parent.Get_Parent = null);

      --  Children becomes available after expansion and contains the original
      --  child item, while Get_Child_Row returns its GtkTreeListRow wrapper.
      Parent.Set_Expanded (True);
      Assert_True (Parent.Get_Expanded);
      Children := Parent.Get_Children;
      Assert_Cmpuint_Eq (Get_N_Items (Children), 1);
      Child := Parent.Get_Child_Row (0);
      Assert_True (Child /= null);
      Assert_Cmpuint_Eq (Child.Get_Depth, 1);
      Assert_Cmpuint_Eq (Child.Get_Position, 1);
      Assert_False (Child.Is_Expandable);
      Assert_True (Parent.Get_Child_Row (1) = null);

      --  Parent and item accessors return the expected objects for the child.
      Parent_Of_Child := Child.Get_Parent;
      Assert_True (Parent_Of_Child /= null);
      Assert_True (Parent_Of_Child = Parent);
      Item := Child.Get_Item;
      Assert_Cmpstr_Eq (Gtk_String_Object (Item).Get_String, "child");

      --  Collapsing removes the child from the flattened model and updates
      --  the row state reported by the binding.
      Parent.Set_Expanded (False);
      Assert_False (Parent.Get_Expanded);
      Assert_Cmpuint_Eq (Get_N_Items (+Tree), 2);

      Unref (Item);
      Unref (Parent_Of_Child);
      Unref (Child);
      Unref (Parent);
      Unref (Tree);
      Unref (Root);
   end Test_Row_Accessors;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/tree-list-row/accessors", Test_Row_Accessors'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Tree_List_Row;
