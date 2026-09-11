--  Unit test for Gtk.Column_View and Gtk.Signal_List_Item_Factory.
--
--  GTK's own testsuite has no columnview.c to port -- its only neighbour,
--  listitemmanager.c, reaches into private headers -- so this is written
--  from scratch.
--
--  Besides the column view's own surface, the last case exercises
--  Gtk.Signal_List_Item_Factory end to end: a factory that compiles but
--  never fires its handlers would otherwise pass unnoticed.

with Ada.Command_Line;

with Glib;                          use Glib;
with Glib.List_Model;               use Glib.List_Model;
with Glib.Main;                     use Glib.Main;
with Glib.Object;                   use Glib.Object;
with Glib.Test;                     use Glib.Test;

with GNAT.Strings;

with Gtk.Column_View;               use Gtk.Column_View;
with Gtk.Column_View_Column;        use Gtk.Column_View_Column;
with Gtk.Enums;                     use Gtk.Enums;
with Gtk.Expression;
with Gtk.Label;                     use Gtk.Label;
with Gtk.List_Item;                 use Gtk.List_Item;
with Gtk.Main;
with Gtk.Property_Expression;       use Gtk.Property_Expression;
with Gtk.Selection_Model;
with Gtk.Signal_List_Item_Factory;  use Gtk.Signal_List_Item_Factory;
with Gtk.Single_Selection;          use Gtk.Single_Selection;
with Gtk.Sort_List_Model;           use Gtk.Sort_List_Model;
with Gtk.Sorter;                    use Gtk.Sorter;
with Gtk.String_List;               use Gtk.String_List;
with Gtk.String_Object;             use Gtk.String_Object;
with Gtk.String_Sorter;             use Gtk.String_Sorter;
with Gtk.Window;                    use Gtk.Window;

procedure Column_View is

   Setup_Count : Natural := 0;
   pragma Volatile (Setup_Count);
   Bind_Count  : Natural := 0;
   pragma Volatile (Bind_Count);
   --  Incremented by the factory handlers, which are dispatched from
   --  Main_Context_Iteration below.

   First_Cell : access String := null;
   pragma Volatile (First_Cell);
   --  Text the bind handler wrote into the cell of item 0.

   Done : Boolean := False;
   pragma Volatile (Done);
   --  Set from a timeout callback dispatched by Main_Context_Iteration.

   function Stop return Boolean;

   function New_Strings return Gtk_String_List;
   function New_Sorter return Gtk_String_Sorter;

   procedure Setup_Cell
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);
   procedure Bind_Cell
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);

   procedure Test_Model with Convention => C;
   procedure Test_Columns with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Sorting with Convention => C;
   procedure Test_Factory with Convention => C;

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
        (new String'("cherry"), new String'("apple"), new String'("banana"));
   begin
      return Gtk_String_List_New (Items);
   end New_Strings;

   ----------------
   -- New_Sorter --
   ----------------

   function New_Sorter return Gtk_String_Sorter is
   begin
      --  Sorts Gtk_String_Objects on their "string" property.
      return
        Gtk_String_Sorter_New
          (Gtk.Expression.Gtk_Expression
             (Gtk_Property_Expression_New
                (Gtk.String_Object.Get_Type, null, "string")));
   end New_Sorter;

   ----------------
   -- Setup_Cell --
   ----------------

   procedure Setup_Cell
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class)
   is
      pragma Unreferenced (Self);
      Label : Gtk_Label;
   begin
      Gtk.Label.Gtk_New (Label, "");
      Item.Set_Child (Label);
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
      --  Get_Item returns a GObject rather than a bare address, so the
      --  conversion below needs no Unchecked_Conversion.
      Obj   : constant GObject := Item.Get_Item;
      Label : constant Gtk_Label := Gtk_Label (Item.Get_Child);
   begin
      if Obj /= null then
         Label.Set_Text (Gtk_String_Object (Obj).Get_String);
         if Item.Get_Position = 0 then
            First_Cell := new String'(Label.Get_Text);
         end if;
      end if;
      Bind_Count := Bind_Count + 1;
   end Bind_Cell;

   ----------------
   -- Test_Model --
   ----------------

   procedure Test_Model is
      View      : constant Gtk_Column_View :=
        Gtk_Column_View_New (Gtk.Selection_Model.Null_Gtk_Selection_Model);
      Selection : constant Gtk_Single_Selection :=
        Gtk_Single_Selection_New (+New_Strings);
      use type Gtk.Selection_Model.Gtk_Selection_Model;
   begin
      Assert_True
        (View.Get_Model = Gtk.Selection_Model.Null_Gtk_Selection_Model);

      View.Set_Model (+Selection);

      --  Gtk_Selection_Model is an interface reference rather than an
      --  object pointer, so the round trip is checked on the object behind
      --  it rather than on the reference itself.
      Assert_True (Gtk_Single_Selection (-View.Get_Model) = Selection);
      Assert_Cmpuint_Eq (Get_N_Items (+Selection), 3);
   end Test_Model;

   ------------------
   -- Test_Columns --
   ------------------

   procedure Test_Columns is
      View : constant Gtk_Column_View :=
        Gtk_Column_View_New (Gtk.Selection_Model.Null_Gtk_Selection_Model);
      A    : constant Gtk_Column_View_Column :=
        Gtk_Column_View_Column_New ("A", null);
      B    : constant Gtk_Column_View_Column :=
        Gtk_Column_View_Column_New ("B", null);
      C    : constant Gtk_Column_View_Column :=
        Gtk_Column_View_Column_New ("C", null);
      D    : constant Gtk_Column_View_Column :=
        Gtk_Column_View_Column_New ("D", null);

      function Title_At (Position : Guint) return String;

      --------------
      -- Title_At --
      --------------

      function Title_At (Position : Guint) return String is
        (Gtk_Column_View_Column (Get_Item (View.Get_Columns, Position))
           .Get_Title);

   begin
      Assert_Cmpuint_Eq (Get_N_Items (View.Get_Columns), 0);

      View.Append_Column (A);
      View.Append_Column (B);
      View.Append_Column (C);
      Assert_Cmpuint_Eq (Get_N_Items (View.Get_Columns), 3);
      Assert_Cmpstr_Eq (Title_At (0), "A");
      Assert_Cmpstr_Eq (Title_At (1), "B");
      Assert_Cmpstr_Eq (Title_At (2), "C");

      View.Insert_Column (0, D);
      Assert_Cmpuint_Eq (Get_N_Items (View.Get_Columns), 4);
      Assert_Cmpstr_Eq (Title_At (0), "D");
      Assert_Cmpstr_Eq (Title_At (1), "A");

      --  Inserting a column that is already there repositions it.
      View.Insert_Column (3, D);
      Assert_Cmpuint_Eq (Get_N_Items (View.Get_Columns), 4);
      Assert_Cmpstr_Eq (Title_At (0), "A");
      Assert_Cmpstr_Eq (Title_At (3), "D");

      View.Remove_Column (B);
      Assert_Cmpuint_Eq (Get_N_Items (View.Get_Columns), 3);
      Assert_Cmpstr_Eq (Title_At (0), "A");
      Assert_Cmpstr_Eq (Title_At (1), "C");
      Assert_Cmpstr_Eq (Title_At (2), "D");
   end Test_Columns;

   ---------------------
   -- Test_Properties --
   ---------------------

   procedure Test_Properties is
      View : constant Gtk_Column_View :=
        Gtk_Column_View_New (Gtk.Selection_Model.Null_Gtk_Selection_Model);
   begin
      Assert_False (View.Get_Enable_Rubberband);
      View.Set_Enable_Rubberband (True);
      Assert_True (View.Get_Enable_Rubberband);

      --  reorderable is the one of these that starts out enabled.
      Assert_True (View.Get_Reorderable);
      View.Set_Reorderable (False);
      Assert_False (View.Get_Reorderable);
      View.Set_Reorderable (True);
      Assert_True (View.Get_Reorderable);

      Assert_False (View.Get_Show_Column_Separators);
      View.Set_Show_Column_Separators (True);
      Assert_True (View.Get_Show_Column_Separators);

      Assert_False (View.Get_Show_Row_Separators);
      View.Set_Show_Row_Separators (True);
      Assert_True (View.Get_Show_Row_Separators);

      Assert_False (View.Get_Single_Click_Activate);
      View.Set_Single_Click_Activate (True);
      Assert_True (View.Get_Single_Click_Activate);

      Assert_True (View.Get_Tab_Behavior = List_Tab_All);
      View.Set_Tab_Behavior (List_Tab_Cell);
      Assert_True (View.Get_Tab_Behavior = List_Tab_Cell);
   end Test_Properties;

   ------------------
   -- Test_Sorting --
   ------------------

   procedure Test_Sorting is
      Strings     : constant Gtk_String_List := New_Strings;
      View        : constant Gtk_Column_View :=
        Gtk_Column_View_New (Gtk.Selection_Model.Null_Gtk_Selection_Model);
      Column      : constant Gtk_Column_View_Column :=
        Gtk_Column_View_Column_New ("Fruit", null);
      Sorter      : constant Gtk_String_Sorter := New_Sorter;
      View_Sorter : Gtk_Sorter;
      Sorted      : Gtk_Sort_List_Model;

      function Item_At (Position : Guint) return String;

      -------------
      -- Item_At --
      -------------

      function Item_At (Position : Guint) return String is
        (Gtk_String_Object (Get_Item (+Sorted, Position)).Get_String);

   begin
      --  There is no Set_Sorter on a column view: the sorter reported by
      --  Get_Sorter is the view's own, reflecting the header the user last
      --  clicked, and is read-only.
      Assert_True (View.Get_Sorter /= null);

      Column.Set_Sorter (Sorter);
      Assert_True (Gtk_String_Sorter (Column.Get_Sorter) = Sorter);
      View.Append_Column (Column);

      --  Get_Sorter is transfer-none whereas Gtk_Sort_List_Model_New is
      --  transfer-full on its sorter, hence the explicit reference.
      View_Sorter := View.Get_Sorter;
      Ref (View_Sorter);
      Sorted := Gtk_Sort_List_Model_New (+Strings, View_Sorter);
      View.Set_Model (+Gtk_Single_Selection_New (+Sorted));

      --  Unsorted, the sort model preserves the order of the source.
      Assert_Cmpstr_Eq (Item_At (0), "cherry");

      View.Sort_By_Column (Column, Sort_Ascending);
      Assert_Cmpstr_Eq (Item_At (0), "apple");
      Assert_Cmpstr_Eq (Item_At (1), "banana");
      Assert_Cmpstr_Eq (Item_At (2), "cherry");

      View.Sort_By_Column (Column, Sort_Descending);
      Assert_Cmpstr_Eq (Item_At (0), "cherry");
      Assert_Cmpstr_Eq (Item_At (2), "apple");

      --  A null column reverts to the source order.
      View.Sort_By_Column (null, Sort_Ascending);
      Assert_Cmpstr_Eq (Item_At (0), "cherry");
      Assert_Cmpstr_Eq (Item_At (1), "apple");
      Assert_Cmpstr_Eq (Item_At (2), "banana");
   end Test_Sorting;

   ------------------
   -- Test_Factory --
   ------------------

   procedure Test_Factory is
      Strings : constant Gtk_String_List := New_Strings;
      Factory : constant Gtk_Signal_List_Item_Factory :=
        Gtk_Signal_List_Item_Factory_New;
      View    : constant Gtk_Column_View :=
        Gtk_Column_View_New (+Gtk_Single_Selection_New (+Strings));
      Column  : constant Gtk_Column_View_Column :=
        Gtk_Column_View_Column_New ("Fruit", Factory);
      Window  : Gtk_Window;
      Id      : G_Source_Id;
      pragma Unreferenced (Id);
   begin
      Factory.On_Setup (Setup_Cell'Unrestricted_Access);
      Factory.On_Bind (Bind_Cell'Unrestricted_Access);
      View.Append_Column (Column);

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

      --  The guard matters: were the factory binding broken, the loop would
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

      --  And the handlers saw the real item, not a null one.
      Assert_True (First_Cell /= null);
      Assert_Cmpstr_Eq (First_Cell.all, "cherry");

      Window.Destroy;
   end Test_Factory;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/columnview/model", Test_Model'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/columnview/columns", Test_Columns'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/columnview/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/columnview/sorting", Test_Sorting'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/columnview/factory", Test_Factory'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Column_View;
