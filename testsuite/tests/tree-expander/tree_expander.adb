--  Unit test for Gtk.Tree_Expander.
--
--  GTK's own testsuite has no treeexpander.c to port, so this is written
--  from scratch, in the manner of testsuite/tests/column-view.
--
--  The last case is the one that earns its keep: an expander that is
--  constructed and configured correctly but never actually manages a row
--  inside a list view would otherwise pass unnoticed.

with Ada.Command_Line;

with Glib;                          use Glib;
with Glib.List_Model;               use Glib.List_Model;
with Glib.Main;                     use Glib.Main;
with Glib.Object;                   use Glib.Object;
with Glib.Test;                     use Glib.Test;

with GNAT.Strings;

with Gtk.Label;                     use Gtk.Label;
with Gtk.List_Item;                 use Gtk.List_Item;
with Gtk.List_View;                 use Gtk.List_View;
with Gtk.Main;
with Gtk.Signal_List_Item_Factory;  use Gtk.Signal_List_Item_Factory;
with Gtk.Single_Selection;          use Gtk.Single_Selection;
with Gtk.String_List;               use Gtk.String_List;
with Gtk.String_Object;             use Gtk.String_Object;
with Gtk.Tree_Expander;             use Gtk.Tree_Expander;
with Gtk.Tree_List_Model;           use Gtk.Tree_List_Model;
with Gtk.Tree_List_Row;             use Gtk.Tree_List_Row;
with Gtk.Widget;                    use Gtk.Widget;
with Gtk.Window;                    use Gtk.Window;

procedure Tree_Expander is

   Setup_Count : Natural := 0;
   pragma Volatile (Setup_Count);
   Bind_Count  : Natural := 0;
   pragma Volatile (Bind_Count);
   --  Incremented by the factory handlers, which are dispatched from
   --  Main_Context_Iteration below.

   First_Cell : access String := null;
   pragma Volatile (First_Cell);
   Second_Cell : access String := null;
   pragma Volatile (Second_Cell);
   --  Text the bind handler wrote into the cells of items 0 and 1.

   First_Depth : Guint := Guint'Last;
   pragma Volatile (First_Depth);
   Second_Depth : Guint := Guint'Last;
   pragma Volatile (Second_Depth);
   --  Depth the expander of those same two cells reported.

   Items_Agree : Boolean := False;
   pragma Volatile (Items_Agree);
   --  Whether Get_Item on the expander of item 0 agreed with the item of
   --  the row it was given.

   Done : Boolean := False;
   pragma Volatile (Done);
   --  Set from a timeout callback dispatched by Main_Context_Iteration.

   function Stop return Boolean;

   function New_Strings return Gtk_String_List;
   function Create_Model (Item : GObject) return Glist_Model;
   function New_Tree (Autoexpand : Boolean) return Gtk_Tree_List_Model;

   function Row_String (Row : not null access Gtk_Tree_List_Row_Record'Class)
      return String;

   procedure Setup_Cell
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);
   procedure Bind_Cell
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);

   procedure Test_Properties with Convention => C;
   procedure Test_List_Row with Convention => C;
   procedure Test_In_List_View with Convention => C;

   ----------
   -- Stop --
   ----------

   function Stop return Boolean is
   begin
      Done := True;
      Wakeup (null);
      return False;
   end Stop;

   -----------------
   -- New_Strings --
   -----------------

   function New_Strings return Gtk_String_List is
      Items : GNAT.Strings.String_List :=
        (new String'("fruit"), new String'("rock"));
   begin
      return Gtk_String_List_New (Items);
   end New_Strings;

   ------------------
   -- Create_Model --
   ------------------

   function Create_Model (Item : GObject) return Glist_Model is
      Items : GNAT.Strings.String_List :=
        (new String'("apple"), new String'("banana"));
   begin
      --  "fruit" is the only row with children; everything else, the
      --  children included, is a leaf and gets no model at all.
      if Gtk_String_Object (Item).Get_String = "fruit" then
         return +Gtk_String_List_New (Items);
      else
         return Null_Glist_Model;
      end if;
   end Create_Model;

   --------------
   -- New_Tree --
   --------------

   function New_Tree (Autoexpand : Boolean) return Gtk_Tree_List_Model is
   begin
      --  Passthrough => False is what makes the model hand out
      --  Gtk_Tree_List_Rows, which is what a Gtk_Tree_Expander manages.
      return
        Gtk_Tree_List_Model_New
          (Root        => +New_Strings,
           Passthrough => False,
           Autoexpand  => Autoexpand,
           Create_Func => Create_Model'Unrestricted_Access);
   end New_Tree;

   ----------------
   -- Row_String --
   ----------------

   function Row_String (Row : not null access Gtk_Tree_List_Row_Record'Class)
      return String
   is (Gtk_String_Object (Row.Get_Item).Get_String);

   ----------------
   -- Setup_Cell --
   ----------------

   procedure Setup_Cell
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class)
   is
      pragma Unreferenced (Self);
      Label    : Gtk_Label;
      Expander : Gtk_Tree_Expander;
   begin
      Gtk.Label.Gtk_New (Label, "");
      Gtk.Tree_Expander.Gtk_New (Expander);
      Expander.Set_Child (Label);
      Item.Set_Child (Expander);
      Item.Set_Focusable (False);
      Setup_Count := Setup_Count + 1;
   end Setup_Cell;

   ---------------
   -- Bind_Cell --
   ---------------

   procedure Bind_Cell
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class)
   is
      pragma Unreferenced (Self);
      Expander : constant Gtk_Tree_Expander :=
        Gtk_Tree_Expander (Item.Get_Child);
      --  Get_Item yields the row rather than the item, the model not being
      --  a passthrough one.
      Row      : constant Gtk_Tree_List_Row :=
        Gtk_Tree_List_Row (Item.Get_Item);
      Label    : constant Gtk_Label := Gtk_Label (Expander.Get_Child);
   begin
      if Row /= null then
         Expander.Set_List_Row (Row);
         Label.Set_Text (Row_String (Row));

         case Item.Get_Position is
            when 0 =>
               First_Cell := new String'(Label.Get_Text);
               First_Depth := Expander.Get_List_Row.Get_Depth;
               Items_Agree := Expander.Get_Item = Row.Get_Item;
            when 1 =>
               Second_Cell := new String'(Label.Get_Text);
               Second_Depth := Expander.Get_List_Row.Get_Depth;
            when others =>
               null;
         end case;
      end if;
      Bind_Count := Bind_Count + 1;
   end Bind_Cell;

   ---------------------
   -- Test_Properties --
   ---------------------

   procedure Test_Properties is
      Expander : constant Gtk_Tree_Expander := Gtk_Tree_Expander_New;
      Label    : Gtk_Label;
   begin
      --  Everything an expander holds is nullable, and null is where it
      --  starts.
      Assert_True (Expander.Get_Child = null);
      Assert_True (Expander.Get_List_Row = null);
      Assert_True (Expander.Get_Item = null);

      Gtk.Label.Gtk_New (Label, "cherry");
      Expander.Set_Child (Label);
      Assert_True (Gtk_Label (Expander.Get_Child) = Label);

      Expander.Set_Child (null);
      Assert_True (Expander.Get_Child = null);

      --  Both indentations are on by default; hiding the expander is not.
      Assert_True (Expander.Get_Indent_For_Depth);
      Expander.Set_Indent_For_Depth (False);
      Assert_False (Expander.Get_Indent_For_Depth);
      Expander.Set_Indent_For_Depth (True);
      Assert_True (Expander.Get_Indent_For_Depth);

      Assert_True (Expander.Get_Indent_For_Icon);
      Expander.Set_Indent_For_Icon (False);
      Assert_False (Expander.Get_Indent_For_Icon);

      Assert_False (Expander.Get_Hide_Expander);
      Expander.Set_Hide_Expander (True);
      Assert_True (Expander.Get_Hide_Expander);
   end Test_Properties;

   -------------------
   -- Test_List_Row --
   -------------------

   procedure Test_List_Row is
      Tree     : constant Gtk_Tree_List_Model := New_Tree (Autoexpand => False);
      Expander : constant Gtk_Tree_Expander := Gtk_Tree_Expander_New;
      Row      : constant Gtk_Tree_List_Row := Tree.Get_Child_Row (0);
   begin
      Assert_True (Row /= null);
      Assert_Cmpstr_Eq (Row_String (Row), "fruit");

      Expander.Set_List_Row (Row);
      Assert_True (Expander.Get_List_Row = Row);

      --  Get_Item forwards to the item of the row being managed.
      Assert_True (Expander.Get_Item = Row.Get_Item);
      Assert_Cmpstr_Eq
        (Gtk_String_Object (Expander.Get_Item).Get_String, "fruit");

      Assert_Cmpuint_Eq (Row.Get_Depth, 0);
      Assert_True (Row.Is_Expandable);
      Assert_False (Row.Get_Expanded);

      --  Expanding inserts the children right after their parent, one level
      --  deeper -- the depth the expander turns into an indentation.
      Row.Set_Expanded (True);
      Assert_True (Row.Get_Expanded);
      Assert_Cmpuint_Eq (Get_N_Items (+Tree), 4);
      Assert_Cmpstr_Eq (Row_String (Tree.Get_Row (1)), "apple");
      Assert_Cmpuint_Eq (Tree.Get_Row (1).Get_Depth, 1);

      --  A leaf has no children to show, hence no arrow.
      Assert_False (Tree.Get_Row (1).Is_Expandable);

      --  The row is nullable on the way out as well as in.
      Expander.Set_List_Row (null);
      Assert_True (Expander.Get_List_Row = null);
      Assert_True (Expander.Get_Item = null);
   end Test_List_Row;

   -----------------------
   -- Test_In_List_View --
   -----------------------

   procedure Test_In_List_View is
      Tree    : constant Gtk_Tree_List_Model := New_Tree (Autoexpand => True);
      Factory : constant Gtk_Signal_List_Item_Factory :=
        Gtk_Signal_List_Item_Factory_New;
      View    : Gtk_List_View;
      Window  : Gtk_Window;
      Id      : G_Source_Id;
      pragma Unreferenced (Id);
   begin
      Factory.On_Setup (Setup_Cell'Unrestricted_Access);
      Factory.On_Bind (Bind_Cell'Unrestricted_Access);

      View := Gtk_List_View_New (+Gtk_Single_Selection_New (+Tree), Factory);

      Setup_Count := 0;
      Bind_Count := 0;
      Done := False;

      --  The factory only runs for cells the view actually lays out, so the
      --  view has to be on screen. Present maps and allocates the window,
      --  which is enough to create the cells here -- hence the counters are
      --  reset above it rather than below -- but the loop below stands ready
      --  in case a future GTK defers the layout to the frame clock.
      Gtk.Window.Gtk_New (Window);
      Window.Set_Default_Size (200, 200);
      Window.Set_Child (View);
      Window.Present;

      --  The guard matters: were the binding broken, the loop would
      --  otherwise hang until the driver kills the test, which is a far less
      --  legible failure than an assertion.
      Id := Timeout_Add (5_000, Stop'Unrestricted_Access);

      while not Done and then Bind_Count = 0 loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => True);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;

      Assert_Cmpuint_Gt (Guint (Setup_Count), 0);
      Assert_Cmpuint_Gt (Guint (Bind_Count), 0);

      --  The tree is autoexpanded, so the second row is the first child of
      --  the first row: an expander that indents nothing would still report
      --  a depth of zero here.
      Assert_True (First_Cell /= null);
      Assert_Cmpstr_Eq (First_Cell.all, "fruit");
      Assert_Cmpuint_Eq (First_Depth, 0);
      Assert_True (Items_Agree);

      Assert_True (Second_Cell /= null);
      Assert_Cmpstr_Eq (Second_Cell.all, "apple");
      Assert_Cmpuint_Eq (Second_Depth, 1);

      Window.Destroy;
   end Test_In_List_View;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/treeexpander/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/treeexpander/listrow", Test_List_Row'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/treeexpander/listview", Test_In_List_View'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Tree_Expander;
