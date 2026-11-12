--  Unit test for Gtk.Tree_Expander.
--
--  GTK's own testsuite has no treeexpander.c to port, so this is written
--  from scratch, in the manner of testsuite/tests/column-view.
--  The Gtk.Tree_List_Model cases also port `test_collapse_change` from
--  testsuite/c_tests/treelistmodel.c and cover the Ada callback overloads.
--
--  The last case is the one that earns its keep: an expander that is
--  constructed and configured correctly but never actually manages a row
--  inside a list view would otherwise pass unnoticed.

with Ada.Command_Line;
with Interfaces.C.Strings;
with System;

with Glib;                          use Glib;
with Glib.List_Model;               use Glib.List_Model;
with Glib.Main;                     use Glib.Main;
with Glib.Object;                   use Glib.Object;
with Glib.Test;                     use Glib.Test;

with GNAT.Strings;

with Gtk.Label;                     use Gtk.Label;
with Gtk.List_Item;                 use Gtk.List_Item;
with Gtk.List_View;                 use Gtk.List_View;
with Gtk.Accessible;                use Gtk.Accessible;
with Gtk.Atcontext;                 use Gtk.Atcontext;
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

   Last_User_Data : Guint := 0;
   Destroy_Count  : Natural := 0;
   --  The generic tree-model callback records its supplied data, and its
   --  destroy procedure counts closure cleanup.

   Changed_Position : Guint := Guint'Last;
   Changed_Removed  : Guint := Guint'Last;
   Changed_Added    : Guint := Guint'Last;
   --  Arguments received by the items-changed signal observer.

   Shared_Children : Gtk_String_List := null;
   --  The C-style child factory below returns a retained reference to this
   --  model so the test can mutate its original reference after collapsing.

   function Stop return Boolean;

   function New_Strings return Gtk_String_List;
   function Create_Model (Item : GObject) return Glist_Model;
   function Create_No_Children (Item : GObject) return Glist_Model;
   function Create_Shared_Model (Item : GObject) return Glist_Model;
   function New_Tree (Autoexpand : Boolean) return Gtk_Tree_List_Model;
   function Create_Model_Error (Item : GObject) return Glist_Model;
   function Create_Model_User
     (Item : GObject; User_Data : Guint) return Glist_Model;
   function Create_Model_User_Error
     (Item : GObject; User_Data : Guint) return Glist_Model;
   procedure Destroy_User_Data (Data : in out Guint);

   package Model_User_Data is new Gtk_Tree_List_Model_User_Data
     (User_Data_Type => Guint,
      Destroy => Destroy_User_Data);

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
   procedure Test_Accessibility with Convention => C;
   procedure Test_In_List_View with Convention => C;
   procedure Test_Model_API with Convention => C;
   procedure Test_Callback_Exceptions with Convention => C;
   procedure Test_User_Data_Callbacks with Convention => C;
   procedure Test_Collapse_Change with Convention => C;
   procedure Test_Items_Changed with Convention => C;

   --  Use the C signal ABI so the notification helper can be checked for the
   --  exact position and counts it emits.
   procedure On_Items_Changed
     (Instance : System.Address;
      Position, Removed, Added : Guint;
      User_Data : System.Address)
   with Convention => C;

   type Items_Changed_Handler is access procedure
     (Instance : System.Address;
      Position, Removed, Added : Guint;
      User_Data : System.Address)
   with Convention => C;

   function Signal_Connect_Items_Changed
     (Instance : System.Address;
      Name : Interfaces.C.Strings.chars_ptr;
      Handler : Items_Changed_Handler;
      User_Data, Destroy_Data : System.Address;
      Flags : Gint) return Gulong
   with Import, Convention => C, External_Name => "g_signal_connect_data";

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

   ------------------------
   -- Create_No_Children --
   ------------------------

   function Create_No_Children (Item : GObject) return Glist_Model is
      pragma Unreferenced (Item);
   begin
      --  GTK requires a non-null factory even for a flat tree; returning a
      --  null child model marks every row as a leaf.
      return Null_Glist_Model;
   end Create_No_Children;

   -------------------------
   -- Create_Shared_Model --
   -------------------------

   function Create_Shared_Model (Item : GObject) return Glist_Model is
      pragma Unreferenced (Item);
   begin
      --  The tree model takes ownership of the returned child-model
      --  reference; keep the test's own reference alive for later mutation.
      Ref (Shared_Children);
      return +Shared_Children;
   end Create_Shared_Model;

   -----------------------
   -- Create_Model_Error --
   -----------------------

   function Create_Model_Error (Item : GObject) return Glist_Model is
      pragma Unreferenced (Item);
   begin
      --  The binding catches exceptions before they can cross the C callback
      --  boundary and treats a failed child factory as a leaf.
      raise Program_Error;
      return Null_Glist_Model;
   end Create_Model_Error;

   ----------------------
   -- Create_Model_User --
   ----------------------

   function Create_Model_User
     (Item : GObject; User_Data : Guint) return Glist_Model
   is
      pragma Unreferenced (Item);
      Children : GNAT.Strings.String_List :=
        (1 => new String'("user child"));
   begin
      Last_User_Data := User_Data;
      return +Gtk_String_List_New (Children);
   end Create_Model_User;

   ----------------------------
   -- Create_Model_User_Error --
   ----------------------------

   function Create_Model_User_Error
     (Item : GObject; User_Data : Guint) return Glist_Model
   is
      pragma Unreferenced (Item, User_Data);
   begin
      --  Exercise the same exception guard for the generic callback adapter.
      raise Program_Error;
      return Null_Glist_Model;
   end Create_Model_User_Error;

   -----------------------
   -- Destroy_User_Data --
   -----------------------

   procedure Destroy_User_Data (Data : in out Guint) is
   begin
      --  The closure owns a copy of this value until its model is released.
      Destroy_Count := Destroy_Count + 1;
      Data := 0;
   end Destroy_User_Data;

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

   ------------------------
   -- Test_Accessibility --
   ------------------------

   --  Exercise the accessibility methods inherited by Gtk.Tree_Expander.
   --  The detached widget has no screen bounds or accessibility context.
   procedure Test_Accessibility is
      Expander : constant Gtk_Tree_Expander := Gtk_Tree_Expander_New;
      Child    : constant Gtk_Label := Gtk_Label_New ("accessible child");
      X, Y, Width, Height : Gint;
   begin
      Ref_Sink (Expander);
      Expander.Set_Child (Child);

      --  A tree expander is exposed as a button, with its label in the
      --  accessible child tree and no parent or sibling while unrooted.
      Assert_True (Expander.Get_Accessible_Role = Accessible_Role_Button);
      Assert_Cmpstr_Eq (Expander.Get_Accessible_Id, "");
      Assert_True
        (Expander.Get_Accessible_Parent = Null_Gtk_Accessible);
      Assert_True
        (Expander.Get_First_Accessible_Child /= Null_Gtk_Accessible);
      Assert_True
        (Expander.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      Assert_True (Expander.Get_At_Context = null);
      Assert_False (Expander.Get_Bounds (X, Y, Width, Height));
      Assert_False
        (Expander.Get_Platform_State (Accessible_Platform_State_Focused));

      --  Accessibility notifications can be issued without a screen reader;
      --  null links leave this detached widget outside a synthetic tree.
      Expander.Announce ("tree expander test",
                         Accessible_Announcement_Priority_Low);
      Expander.Reset_Property (Accessible_Property_Label);
      Expander.Reset_Relation (Accessible_Relation_Labelled_By);
      Expander.Reset_State (Accessible_State_Busy);
      Expander.Update_Platform_State (Accessible_Platform_State_Focusable);
      Expander.Set_Accessible_Parent
        (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True
        (Expander.Get_Accessible_Parent = Null_Gtk_Accessible);
      Expander.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True
        (Expander.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      Unref (Expander);
   end Test_Accessibility;

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

   --------------------
   -- Test_Model_API --
   --------------------

   procedure Test_Model_API is
      Root : constant Gtk_String_List := New_Strings;
      Tree : Gtk_Tree_List_Model;
      Row  : Gtk_Tree_List_Row;
      Item : GObject;
   begin
      --  Keep the caller's model reference while GtkTreeListModel consumes a
      --  separate reference to its root model.
      Ref (Root);
      Gtk_New
        (Tree, +Root, Passthrough => True, Autoexpand => False,
         Create_Func => Create_No_Children'Unrestricted_Access);

      Assert_True (Get_Model (Tree) = +Root);
      Assert_True (Get_Passthrough (Tree));
      Assert_False (Get_Autoexpand (Tree));
      Assert_True (Get_Item_Type (Tree) = Get_Item_Type (+Root));
      Assert_Cmpuint_Eq (Get_N_Items (Tree), 2);

      --  The interface returns source objects in passthrough mode while the
      --  row accessors still provide GtkTreeListRow wrappers.
      Item := Gtk.Tree_List_Model.Get_Item (Tree, 0);
      Assert_Cmpstr_Eq (Gtk_String_Object (Item).Get_String, "fruit");
      Unref (Item);
      Row := Get_Child_Row (Tree, 0);
      Assert_True (Row /= null);
      Assert_True (Get_Row (Tree, 0) = Row);
      Assert_True (Get_Child_Row (Tree, 3) = null);
      Assert_True (Get_Row (Tree, 3) = null);
      Unref (Row);

      --  Setting autoexpand changes the reported property; a redundant
      --  Initialize call must leave the already-created object alone.
      Set_Autoexpand (Tree, True);
      Assert_True (Get_Autoexpand (Tree));
      Initialize
        (Tree, Null_Glist_Model, Passthrough => False,
         Autoexpand => False, Create_Func => null);
      Assert_True (Get_Model (Tree) = +Root);
      Assert_True (Get_Passthrough (Tree));
      Assert_True (Get_Autoexpand (Tree));

      Unref (Tree);
      Unref (Root);
   end Test_Model_API;

   -----------------------------
   -- Test_Callback_Exceptions --
   -----------------------------

   procedure Test_Callback_Exceptions is
      Root : constant Gtk_String_List := New_Strings;
      Tree : Gtk_Tree_List_Model;
      Row  : Gtk_Tree_List_Row;
   begin
      --  The standard callback adapter catches Ada exceptions and leaves the
      --  corresponding item as a leaf instead of propagating through GTK.
      Ref (Root);
      Tree := Gtk_Tree_List_Model_New
        (+Root, Passthrough => False, Autoexpand => False,
         Create_Func => Create_Model_Error'Unrestricted_Access);
      Row := Get_Row (Tree, 0);
      Row.Set_Expanded (True);
      Assert_False (Row.Is_Expandable);
      Assert_Cmpuint_Eq (Get_N_Items (Tree), 2);
      Unref (Row);
      Unref (Tree);
      Unref (Root);
   end Test_Callback_Exceptions;

   -----------------------------
   -- Test_User_Data_Callbacks --
   -----------------------------

   procedure Test_User_Data_Callbacks is
      Root : constant Gtk_String_List := New_Strings;
      Tree : Gtk_Tree_List_Model;
      Other : Gtk_Tree_List_Model;
      Initialized : Gtk_Tree_List_Model :=
        new Gtk_Tree_List_Model_Record;
      Null_Tree : Gtk_Tree_List_Model;
      Row : Gtk_Tree_List_Row;
      Before : Natural;
   begin
      Last_User_Data := 0;
      Destroy_Count := 0;

      --  The function constructor stores the supplied value and calls the
      --  user-data callback when a root row expands.
      Ref (Root);
      Tree := Model_User_Data.Gtk_Tree_List_Model_New_User
        (+Root, Passthrough => False, Autoexpand => False,
         Create_Func => Create_Model_User'Unrestricted_Access,
         User_Data => 17);
      Row := Get_Row (Tree, 0);
      Row.Set_Expanded (True);
      Assert_Cmpuint_Eq (Last_User_Data, 17);
      Assert_Cmpuint_Eq (Get_N_Items (Tree), 3);
      Unref (Row);
      Unref (Tree);
      Assert_Cmpuint_Eq (Guint (Destroy_Count), 1);
      Unref (Root);

      --  The procedure constructor uses the same closure, and Initialize_User
      --  on its created object is a no-op that preserves the original root.
      declare
         Procedure_Root : constant Gtk_String_List := New_Strings;
      begin
         Ref (Procedure_Root);
         Model_User_Data.Gtk_New_User
           (Other, +Procedure_Root, Passthrough => False,
            Autoexpand => False,
            Create_Func => Create_Model_User'Unrestricted_Access,
            User_Data => 23);
         Before := Destroy_Count;
         Model_User_Data.Initialize_User
           (Other, Null_Glist_Model, Passthrough => True,
            Autoexpand => True,
            Create_Func => null, User_Data => 0);
         Assert_True (Get_Model (Other) = +Procedure_Root);
         Assert_False (Get_Autoexpand (Other));
         Row := Get_Row (Other, 0);
         Row.Set_Expanded (True);
         Assert_Cmpuint_Eq (Last_User_Data, 23);
         Unref (Row);
         Unref (Other);
         Assert_Cmpuint_Eq (Guint (Destroy_Count), Guint (Before + 1));
         Unref (Procedure_Root);
      end;

      --  Initialize_User also constructs a previously uninitialized record.
      declare
         Init_Root : constant Gtk_String_List := New_Strings;
      begin
         Ref (Init_Root);
         Model_User_Data.Initialize_User
           (Initialized, +Init_Root, Passthrough => False,
            Autoexpand => False,
            Create_Func => Create_Model_User'Unrestricted_Access,
            User_Data => 31);
         Row := Get_Row (Initialized, 0);
         Row.Set_Expanded (True);
         Assert_Cmpuint_Eq (Last_User_Data, 31);
         Unref (Row);
         Unref (Initialized);
         Assert_Cmpuint_Eq (Guint (Destroy_Count), 3);
         Unref (Init_Root);
      end;

      --  Exceptions raised by the user-data callback are caught at the C
      --  boundary and likewise leave the row without children.
      declare
         Error_Root : constant Gtk_String_List := New_Strings;
      begin
         Ref (Error_Root);
         Null_Tree := Model_User_Data.Gtk_Tree_List_Model_New_User
           (+Error_Root, Passthrough => False, Autoexpand => False,
            Create_Func => Create_Model_User_Error'Unrestricted_Access,
            User_Data => 0);
         Row := Get_Row (Null_Tree, 0);
         Row.Set_Expanded (True);
         Assert_False (Row.Is_Expandable);
         Assert_Cmpuint_Eq (Get_N_Items (Null_Tree), 2);
         Unref (Row);
         Unref (Null_Tree);
         Assert_Cmpuint_Eq (Guint (Destroy_Count), 4);
         Unref (Error_Root);
      end;
   end Test_User_Data_Callbacks;

   -------------------------
   -- Test_Collapse_Change --
   -------------------------

   procedure Test_Collapse_Change is
      Root : constant Gtk_String_List := New_Strings;
      Children : constant Gtk_String_List :=
        Gtk_String_List_New ((1 => new String'("first child")));
      Tree : Gtk_Tree_List_Model;
      Row : Gtk_Tree_List_Row;
      Added_Row : Gtk_Tree_List_Row;
   begin
      Shared_Children := Children;
      Ref (Root);
      Tree := Gtk_Tree_List_Model_New
        (+Root, Passthrough => False, Autoexpand => False,
         Create_Func => Create_Shared_Model'Unrestricted_Access);
      Row := Get_Row (Tree, 0);
      Row.Set_Expanded (True);
      Assert_Cmpuint_Eq (Get_N_Items (Tree), 3);
      Row.Set_Expanded (False);
      Assert_Cmpuint_Eq (Get_N_Items (Tree), 2);

      --  Mutating a child model after its row collapses must not make hidden
      --  child items visible in the flattened tree.
      Append (Children, "second child");
      Assert_Cmpuint_Eq (Get_N_Items (Tree), 2);
      Row.Set_Expanded (True);
      Assert_Cmpuint_Eq (Get_N_Items (Tree), 4);
      Added_Row := Get_Row (Tree, 2);
      Assert_Cmpstr_Eq (Row_String (Added_Row), "second child");

      Unref (Added_Row);
      Unref (Row);
      Unref (Tree);
      Unref (Children);
      Shared_Children := null;
      Unref (Root);
   end Test_Collapse_Change;

   ------------------------
   -- Test_Items_Changed --
   ------------------------

   procedure Test_Items_Changed is
      Root : constant Gtk_String_List := New_Strings;
      Tree : Gtk_Tree_List_Model;
      Signal_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("items-changed");
      Signal_Id : Gulong;
   begin
      Ref (Root);
      Tree := Gtk_Tree_List_Model_New
        (+Root, Passthrough => False, Autoexpand => False,
         Create_Func => Create_No_Children'Unrestricted_Access);

      --  GtkAda exposes the list-model notification helper on this
      --  implementation; verify the emitted signal arguments through C ABI.
      Changed_Position := Guint'Last;
      Changed_Removed := Guint'Last;
      Changed_Added := Guint'Last;
      Signal_Id := Signal_Connect_Items_Changed
        (Get_Object (Tree), Signal_Name,
         On_Items_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
      Interfaces.C.Strings.Free (Signal_Name);
      Assert_Cmpuint_Gt (Guint (Signal_Id), 0);

      Items_Changed (Tree, 1, 2, 3);
      Assert_Cmpuint_Eq (Changed_Position, 1);
      Assert_Cmpuint_Eq (Changed_Removed, 2);
      Assert_Cmpuint_Eq (Changed_Added, 3);

      Unref (Tree);
      Unref (Root);
   end Test_Items_Changed;

   ---------------------
   -- On_Items_Changed --
   ---------------------

   procedure On_Items_Changed
     (Instance : System.Address;
      Position, Removed, Added : Guint;
      User_Data : System.Address)
   is
      pragma Unreferenced (Instance, User_Data);
   begin
      Changed_Position := Position;
      Changed_Removed := Removed;
      Changed_Added := Added;
   end On_Items_Changed;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/treeexpander/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/treeexpander/listrow", Test_List_Row'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/treeexpander/accessibility", Test_Accessibility'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/treeexpander/listview", Test_In_List_View'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/treelistmodel/api", Test_Model_API'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/treelistmodel/callback-exceptions",
      Test_Callback_Exceptions'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/treelistmodel/user-data-callbacks",
      Test_User_Data_Callbacks'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/treelistmodel/collapse-change",
      Test_Collapse_Change'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/treelistmodel/items-changed",
      Test_Items_Changed'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Tree_Expander;
