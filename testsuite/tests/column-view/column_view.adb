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
with Ada.Exceptions;

with Glib;                          use Glib;
with Glib.List_Model;               use Glib.List_Model;
with Glib.Menu;                     use Glib.Menu;
with Glib.Menu_Model;
with Glib.Main;                     use Glib.Main;
with Glib.Object;                   use Glib.Object;
with Glib.Properties;
with Glib.Test;                     use Glib.Test;

with GNAT.Strings;
with GNAT.IO;

with Gtk.Column_View;               use Gtk.Column_View;
with Gtk.Column_View_Column;        use Gtk.Column_View_Column;
with Gtk.Column_View_Cell;
with Gtk.Column_View_Row;           use Gtk.Column_View_Row;
with Gtk.Column_View_Sorter;
with Gtk.Accessible;                use Gtk.Accessible;
with Gtk.Adjustment;                 use Gtk.Adjustment;
with Gtk.Atcontext;
with Gtk.Border;
with Gtk.Enums;                     use Gtk.Enums;
with Gtk.Expression;
with Gtk.Label;                     use Gtk.Label;
with Gtk.List_Item_Factory;
with Gtk.List_Item;                 use Gtk.List_Item;
with Gtk.Main;
with Gtk.Property_Expression;       use Gtk.Property_Expression;
with Gtk.Scroll_Info;               use Gtk.Scroll_Info;
with Gtk.Selection_Model;
with Gtk.Signal_List_Item_Factory;  use Gtk.Signal_List_Item_Factory;
with Gtk.Single_Selection;          use Gtk.Single_Selection;
with Gtk.Sort_List_Model;           use Gtk.Sort_List_Model;
with Gtk.Sorter;                    use Gtk.Sorter;
with Gtk.String_List;               use Gtk.String_List;
with Gtk.String_Object;             use Gtk.String_Object;
with Gtk.String_Sorter;             use Gtk.String_Sorter;
with Gtk.Window;                    use Gtk.Window;
with Gtk.Widget;
with Gtkada.Bindings;
with Gtkada.Types;
with System;

use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Adjustment.Gtk_Adjustment;
use type Gtk.Column_View_Row.Gtk_Column_View_Row;
use type Gtk.Column_View.Gtk_Column_View;
use type Gtk.List_Item_Factory.Gtk_List_Item_Factory;
use type Glib.Menu_Model.Gmenu_Model;
use type Gtk.Atcontext.Gtk_Atcontext;
use type Gtk.Selection_Model.Gtk_Selection_Model;
use type Gtk.Widget.Gtk_Widget;
use type System.Address;

procedure Column_View is

   Setup_Count : Natural := 0;
   pragma Volatile (Setup_Count);
   Bind_Count  : Natural := 0;
   pragma Volatile (Bind_Count);
   Unbind_Count : Natural := 0;
   pragma Volatile (Unbind_Count);
   Teardown_Count : Natural := 0;
   pragma Volatile (Teardown_Count);
   --  Incremented by the factory handlers, which are dispatched from
   --  Main_Context_Iteration below.

   Factory_Slot_Object : Gtk_Label := null;
   Slot_Setup_Count    : Natural := 0;
   Slot_Bind_Count     : Natural := 0;
   Slot_Unbind_Count   : Natural := 0;
   Slot_Teardown_Count : Natural := 0;
   --  The GObject overloads receive this explicit slot as their Self value.

   Callback_Exception_Count : Natural := 0;

   First_Cell : access String := null;
   pragma Volatile (First_Cell);
   --  Text the bind handler wrote into the cell of item 0.

   Done : Boolean := False;
   pragma Volatile (Done);
   --  Set from a timeout callback dispatched by Main_Context_Iteration.

   function Stop return Boolean;

   Typed_Activate_Count : Natural := 0;
   Slot_Activate_Count  : Natural := 0;

   --  GtkColumnView's activate signal carries a row position. This test
   --  helper emits it directly without synthesizing pointer or key input.
   procedure Emit_Activate
     (Object : System.Address;
      Name   : Gtkada.Types.Chars_Ptr;
      Value  : Gint;
      Extra  : System.Address);
   pragma Import (C, Emit_Activate, "ada_g_signal_emit_by_name_int_ptr");

   procedure On_Activate_Typed
     (Self : access Gtk_Column_View_Record'Class; Position : Guint);
   procedure On_Activate_Slot
     (Self : access GObject_Record'Class; Position : Guint);

   function New_Strings return Gtk_String_List;
   function New_Sorter return Gtk_String_Sorter;

   procedure Setup_Cell
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);
   procedure Bind_Cell
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);
   procedure On_Factory_Setup_Slot
     (Self : access GObject_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);
   procedure On_Factory_Bind_Slot
     (Self : access GObject_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);
   procedure On_Factory_Unbind_Typed
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);
   procedure On_Factory_Unbind_Slot
     (Self : access GObject_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);
   procedure On_Factory_Teardown_Typed
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);
   procedure On_Factory_Teardown_Slot
     (Self : access GObject_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);
   procedure Raise_Factory_Typed
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);
   procedure Raise_Factory_Slot
     (Self : access GObject_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);
   procedure Remember_Factory_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);
   procedure Restore_Default_Exception_Output
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Test_Model with Convention => C;
   procedure Test_Columns with Convention => C;
   procedure Test_Column_Api with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Sorting with Convention => C;
   procedure Test_Factory with Convention => C;
   procedure Test_Row_Api with Convention => C;
   procedure Test_Construction_And_Factories with Convention => C;
   procedure Test_Scrollable with Convention => C;
   procedure Test_Accessibility with Convention => C;
   procedure Test_Signals with Convention => C;

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
      --  A ColumnView factory receives its specialized cell as a list item.
      Cell  : constant Gtk.Column_View_Cell.Gtk_Column_View_Cell :=
        Gtk.Column_View_Cell.Gtk_Column_View_Cell (Item);
      Label : Gtk_Label;
   begin
      --  Setup runs before the cell is attached to a model item.
      Assert_True (Cell.Get_Child = null);
      Assert_True (Cell.Get_Item = null);
      Assert_Cmpuint_Eq (Cell.Get_Position, Guint'Last);
      Assert_False (Cell.Get_Selected);

      --  Accessible strings default to empty; setting them copies the Ada
      --  strings into GTK and makes the values available through the getters.
      Assert_Cmpstr_Eq (Cell.Get_Accessible_Description, "");
      Cell.Set_Accessible_Description ("fruit row description");
      Assert_Cmpstr_Eq
        (Cell.Get_Accessible_Description, "fruit row description");
      Assert_Cmpstr_Eq (Cell.Get_Accessible_Label, "");
      Cell.Set_Accessible_Label ("fruit row label");
      Assert_Cmpstr_Eq (Cell.Get_Accessible_Label, "fruit row label");

      --  List-item activation and selection flags can be changed by the
      --  factory while the cell is unbound, then restored to their defaults.
      Assert_True (Cell.Get_Activatable);
      Cell.Set_Activatable (False);
      Assert_False (Cell.Get_Activatable);
      Cell.Set_Activatable (True);
      Assert_True (Cell.Get_Activatable);
      Assert_True (Cell.Get_Selectable);
      Cell.Set_Selectable (False);
      Assert_False (Cell.Get_Selectable);
      Cell.Set_Selectable (True);
      Assert_True (Cell.Get_Selectable);

      --  Check both explicitly set values through the getter.
      Cell.Set_Focusable (False);
      Assert_False (Cell.Get_Focusable);
      Cell.Set_Focusable (True);
      Assert_True (Cell.Get_Focusable);

      --  An explicit null is accepted before setup populates the empty cell.
      Cell.Set_Child (null);
      Assert_True (Cell.Get_Child = null);
      Gtk.Label.Gtk_New (Label, "");
      Cell.Set_Child (Label);
      Assert_True (Cell.Get_Child = Gtk.Widget.Gtk_Widget (Label));
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
      --  The same specialized object is bound after setup with a model item.
      Cell  : constant Gtk.Column_View_Cell.Gtk_Column_View_Cell :=
        Gtk.Column_View_Cell.Gtk_Column_View_Cell (Item);
      --  Get_Item returns a GObject rather than a bare address, so the
      --  conversion below needs no Unchecked_Conversion.
      Obj   : constant GObject := Cell.Get_Item;
      Label : constant Gtk_Label := Gtk_Label (Cell.Get_Child);
   begin
      Assert_True (Obj /= null);
      Assert_Cmpuint_Eq (Cell.Get_Position, Item.Get_Position);
      Assert_True (Label /= null);
      --  The single-selection model selects its first row automatically.
      Assert_True (Cell.Get_Selected = (Cell.Get_Position = 0));
      if Obj /= null then
         --  ColumnView cells expose a non-null row item while bound.
         Label.Set_Text ("bound");
         if Cell.Get_Position = 0 then
            First_Cell := new String'(Label.Get_Text);
         end if;
      end if;
      Bind_Count := Bind_Count + 1;
   end Bind_Cell;

   -----------------------------
   -- Factory signal callbacks --
   -----------------------------

   procedure On_Factory_Setup_Slot
     (Self : access GObject_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class) is
   begin
      --  The slot overload passes the supplied GObject as Self.
      Assert_True (Gtk_Label (Self) = Factory_Slot_Object);
      Assert_True (Item /= null);
      Slot_Setup_Count := Slot_Setup_Count + 1;
   end On_Factory_Setup_Slot;

   procedure On_Factory_Bind_Slot
     (Self : access GObject_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class) is
   begin
      Assert_True (Gtk_Label (Self) = Factory_Slot_Object);
      Assert_True (Item /= null);
      Slot_Bind_Count := Slot_Bind_Count + 1;
   end On_Factory_Bind_Slot;

   procedure On_Factory_Unbind_Typed
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class) is
   begin
      --  The typed overload receives the factory itself as Self.
      Assert_True (Self /= null);
      Assert_True (Item /= null);
      Unbind_Count := Unbind_Count + 1;
   end On_Factory_Unbind_Typed;

   procedure On_Factory_Unbind_Slot
     (Self : access GObject_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class) is
   begin
      Assert_True (Gtk_Label (Self) = Factory_Slot_Object);
      Assert_True (Item /= null);
      Slot_Unbind_Count := Slot_Unbind_Count + 1;
   end On_Factory_Unbind_Slot;

   procedure On_Factory_Teardown_Typed
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class) is
   begin
      Assert_True (Self /= null);
      Assert_True (Item /= null);
      Teardown_Count := Teardown_Count + 1;
   end On_Factory_Teardown_Typed;

   procedure On_Factory_Teardown_Slot
     (Self : access GObject_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class) is
   begin
      Assert_True (Gtk_Label (Self) = Factory_Slot_Object);
      Assert_True (Item /= null);
      Slot_Teardown_Count := Slot_Teardown_Count + 1;
   end On_Factory_Teardown_Slot;

   procedure Raise_Factory_Typed
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class) is
      pragma Unreferenced (Self, Item);
   begin
      --  An exception raised by an Ada callback must be contained by the
      --  generated marshaller instead of unwinding through GTK's C code.
      raise Program_Error with "intentional typed factory callback failure";
   end Raise_Factory_Typed;

   procedure Raise_Factory_Slot
     (Self : access GObject_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class) is
      pragma Unreferenced (Self, Item);
   begin
      raise Program_Error with "intentional slot factory callback failure";
   end Raise_Factory_Slot;

   procedure Remember_Factory_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      --  Count exceptions delivered by each generated marshaller.
      Callback_Exception_Count := Callback_Exception_Count + 1;
   end Remember_Factory_Exception;

   procedure Restore_Default_Exception_Output
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
   begin
      --  Restore the normal GtkAda diagnostic behavior after observing the
      --  two deliberate exceptions in this test.
      GNAT.IO.Put_Line
        (GNAT.IO.Standard_Error,
         Ada.Exceptions.Exception_Information (Occurrence));
   end Restore_Default_Exception_Output;

   ----------------------
   -- Signal callbacks --
   ----------------------

   procedure On_Activate_Typed
     (Self : access Gtk_Column_View_Record'Class; Position : Guint) is
   begin
      Assert_True (Self /= null);
      Assert_Cmpuint_Eq (Position, 2);
      Typed_Activate_Count := Typed_Activate_Count + 1;
   end On_Activate_Typed;

   procedure On_Activate_Slot
     (Self : access GObject_Record'Class; Position : Guint) is
   begin
      Assert_True (Self /= null);
      Assert_Cmpuint_Eq (Position, 2);
      Slot_Activate_Count := Slot_Activate_Count + 1;
   end On_Activate_Slot;

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

   ----------------------
   -- Test_Column_Api --
   ----------------------

   procedure Test_Column_Api is
      View    : constant Gtk_Column_View :=
        Gtk_Column_View_New (Gtk.Selection_Model.Null_Gtk_Selection_Model);
      Column  : Gtk_Column_View_Column;
      Factory : constant Gtk_Signal_List_Item_Factory :=
        Gtk_Signal_List_Item_Factory_New;
      Menu    : constant Gmenu := Gmenu_New;
   begin
      --  Gtk_New exercises the procedure constructor with GTK's null-title
      --  convention. Before insertion, the column has no owning view.
      Gtk_New (Column, "", null);
      Assert_Cmpstr_Eq (Column.Get_Title, "");
      Assert_True (Column.Get_Column_View = null);
      Assert_True (Column.Get_Factory = null);
      Assert_True (Column.Get_Header_Menu = null);

      --  Check the documented initial state and then both sides of each
      --  mutable Boolean and integer property.
      Assert_False (Column.Get_Expand);
      Column.Set_Expand (True);
      Assert_True (Column.Get_Expand);
      Column.Set_Expand (False);
      Assert_False (Column.Get_Expand);

      Assert_Cmpint_Eq (Column.Get_Fixed_Width, -1);
      Column.Set_Fixed_Width (137);
      Assert_Cmpint_Eq (Column.Get_Fixed_Width, 137);
      Column.Set_Fixed_Width (-1);
      Assert_Cmpint_Eq (Column.Get_Fixed_Width, -1);

      Assert_False (Column.Get_Resizable);
      Column.Set_Resizable (True);
      Assert_True (Column.Get_Resizable);
      Column.Set_Resizable (False);
      Assert_False (Column.Get_Resizable);

      Assert_True (Column.Get_Visible);
      Column.Set_Visible (False);
      Assert_False (Column.Get_Visible);
      Column.Set_Visible (True);
      Assert_True (Column.Get_Visible);

      --  Empty strings map to C NULL pointers in these setters; reading them
      --  back checks that GtkAda converts those nulls to empty Ada strings.
      Column.Set_Title ("Updated title");
      Assert_Cmpstr_Eq (Column.Get_Title, "Updated title");
      Column.Set_Title ("");
      Assert_Cmpstr_Eq (Column.Get_Title, "");
      Column.Set_Id ("customer-name");
      Assert_Cmpstr_Eq (Column.Get_Id, "customer-name");
      Column.Set_Id ("");
      Assert_Cmpstr_Eq (Column.Get_Id, "");

      --  The returned factory and menu are borrowed references to the same
      --  objects supplied to the setters; null is also a supported value.
      Column.Set_Factory (Factory);
      Assert_True
        (Column.Get_Factory =
           Gtk.List_Item_Factory.Gtk_List_Item_Factory (Factory));
      Column.Set_Factory (null);
      Assert_True (Column.Get_Factory = null);
      Column.Set_Header_Menu (Menu);
      Assert_True
        (Column.Get_Header_Menu = Glib.Menu_Model.Gmenu_Model (Menu));
      Column.Set_Header_Menu (null);
      Assert_True (Column.Get_Header_Menu = null);

      --  Appending associates the column with its view, which the column API
      --  then reports through Get_Column_View.
      View.Append_Column (Column);
      Assert_True (Column.Get_Column_View = View);
   end Test_Column_Api;

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
      Column_Sorter : Gtk.Column_View_Sorter.Gtk_Column_View_Sorter;
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
      --  The view's GtkSorter is its GtkColumnViewSorter. Keep the typed
      --  interface to check the sort configuration exposed by that object.
      Column_Sorter :=
        Gtk.Column_View_Sorter.Gtk_Column_View_Sorter (View_Sorter);
      --  Before a header is selected, there is no primary column and GTK
      --  reports ascending as the default direction.
      Assert_Cmpuint_Eq
        (Gtk.Column_View_Sorter.Get_N_Sort_Columns (Column_Sorter), 0);
      Assert_True
        (Gtk.Column_View_Sorter.Get_Primary_Sort_Column (Column_Sorter)
         = null);
      Assert_True
        (Gtk.Column_View_Sorter.Get_Primary_Sort_Order (Column_Sorter)
         = Sort_Ascending);
      Assert_True
        (Glib.Properties.Get_Property
           (Column_Sorter,
            Gtk.Column_View_Sorter.Primary_Sort_Column_Property) = null);
      Assert_True
        (Gtk.Enums.Get_Property
           (Column_Sorter,
            Gtk.Column_View_Sorter.Primary_Sort_Order_Property)
         = Sort_Ascending);
      Sorted := Gtk_Sort_List_Model_New (+Strings, View_Sorter);
      View.Set_Model (+Gtk_Single_Selection_New (+Sorted));

      --  Unsorted, the sort model preserves the order of the source.
      Assert_Cmpstr_Eq (Item_At (0), "cherry");

      View.Sort_By_Column (Column, Sort_Ascending);
      --  Position zero is the primary column; the getter also returns its
      --  current direction through the out parameter.
      declare
         Order : Gtk_Sort_Type;
      begin
         --  The GObject properties mirror the typed accessors, including
         --  the object identity of the selected column.
         Assert_Cmpuint_Eq
           (Gtk.Column_View_Sorter.Get_N_Sort_Columns (Column_Sorter), 1);
         Assert_True
           (Get_Object
              (Gtk.Column_View_Sorter.Get_Nth_Sort_Column
                 (Column_Sorter, 0, Order)) = Get_Object (Column));
         Assert_True (Order = Sort_Ascending);
         Assert_True
           (Get_Object
              (Gtk.Column_View_Sorter.Get_Primary_Sort_Column
                 (Column_Sorter)) = Get_Object (Column));
         Assert_True
           (Gtk.Column_View_Sorter.Get_Primary_Sort_Order (Column_Sorter)
            = Sort_Ascending);
         Assert_True
           (Get_Object (Glib.Properties.Get_Property
              (Column_Sorter,
               Gtk.Column_View_Sorter.Primary_Sort_Column_Property))
            = Get_Object (Column));
         Assert_True
           (Gtk.Enums.Get_Property
              (Column_Sorter,
               Gtk.Column_View_Sorter.Primary_Sort_Order_Property)
            = Sort_Ascending);
      end;
      Assert_Cmpstr_Eq (Item_At (0), "apple");
      Assert_Cmpstr_Eq (Item_At (1), "banana");
      Assert_Cmpstr_Eq (Item_At (2), "cherry");

      View.Sort_By_Column (Column, Sort_Descending);
      declare
         Order : Gtk_Sort_Type;
      begin
         --  Reversing direction keeps the same primary column and updates
         --  both the out parameter and the primary-order property.
         Assert_True
           (Get_Object
              (Gtk.Column_View_Sorter.Get_Nth_Sort_Column
                 (Column_Sorter, 0, Order)) = Get_Object (Column));
         Assert_True (Order = Sort_Descending);
         Assert_True
           (Gtk.Column_View_Sorter.Get_Primary_Sort_Order (Column_Sorter)
            = Sort_Descending);
         Assert_True
           (Gtk.Enums.Get_Property
              (Column_Sorter,
               Gtk.Column_View_Sorter.Primary_Sort_Order_Property)
            = Sort_Descending);
      end;
      Assert_Cmpstr_Eq (Item_At (0), "cherry");
      Assert_Cmpstr_Eq (Item_At (2), "apple");

      --  A null column reverts to the source order.
      View.Sort_By_Column (null, Sort_Ascending);
      --  Passing a null column clears the configuration and restores GTK's
      --  default direction.
      Assert_Cmpuint_Eq
        (Gtk.Column_View_Sorter.Get_N_Sort_Columns (Column_Sorter), 0);
      Assert_True
        (Gtk.Column_View_Sorter.Get_Primary_Sort_Column (Column_Sorter)
         = null);
      Assert_True
        (Gtk.Column_View_Sorter.Get_Primary_Sort_Order (Column_Sorter)
         = Sort_Ascending);
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
      Slot    : constant Gtk_Label := Gtk_Label_New ("factory callback slot");
      Id      : G_Source_Id;
      pragma Unreferenced (Id);
   begin
      --  Register both typed and slot-based overloads for setup and bind,
      --  then both overloads for unbind and teardown. Destroying the view
      --  exercises the full lifecycle while GTK owns its list items.
      Factory.On_Setup (Setup_Cell'Unrestricted_Access);
      Factory.On_Bind (Bind_Cell'Unrestricted_Access);
      Factory_Slot_Object := Slot;
      Slot_Setup_Count := 0;
      Slot_Bind_Count := 0;
      Slot_Unbind_Count := 0;
      Slot_Teardown_Count := 0;
      Unbind_Count := 0;
      Teardown_Count := 0;
      Factory.On_Setup
        (On_Factory_Setup_Slot'Unrestricted_Access, Slot, After => True);
      Factory.On_Bind (On_Factory_Bind_Slot'Unrestricted_Access, Slot);
      Factory.On_Unbind (On_Factory_Unbind_Typed'Unrestricted_Access);
      Factory.On_Unbind (On_Factory_Unbind_Slot'Unrestricted_Access, Slot);
      Factory.On_Teardown (On_Factory_Teardown_Typed'Unrestricted_Access);
      Factory.On_Teardown
        (On_Factory_Teardown_Slot'Unrestricted_Access, Slot);
      Callback_Exception_Count := 0;
      Gtkada.Bindings.Set_On_Exception
        (Remember_Factory_Exception'Unrestricted_Access);
      Factory.On_Setup (Raise_Factory_Typed'Unrestricted_Access);
      Factory.On_Bind (Raise_Factory_Slot'Unrestricted_Access, Slot);
      View.Append_Column (Column);

      Setup_Count := 0;
      Bind_Count := 0;
      Done := False;
      Assert_Cmpstr_Eq
        (Glib.Type_Name (Gtk.Column_View_Cell.Get_Type), "GtkColumnViewCell");

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
      --  Verify each marshaller catches an Ada exception and reports it to
      --  GtkAda's exception hook while signal dispatch continues.
      Assert_Cmpuint_Ge (Guint (Callback_Exception_Count), 2);

      --  The first bound cell received its label update.
      Assert_True (First_Cell /= null);
      Assert_Cmpstr_Eq (First_Cell.all, "bound");

      Window.Destroy;
      Assert_Cmpuint_Gt (Guint (Unbind_Count), 0);
      Assert_Cmpuint_Gt (Guint (Teardown_Count), 0);
      Assert_Cmpuint_Gt (Guint (Slot_Setup_Count), 0);
      Assert_Cmpuint_Gt (Guint (Slot_Bind_Count), 0);
      Assert_Cmpuint_Gt (Guint (Slot_Unbind_Count), 0);
      Assert_Cmpuint_Gt (Guint (Slot_Teardown_Count), 0);
      Gtkada.Bindings.Set_On_Exception
        (Restore_Default_Exception_Output'Unrestricted_Access);
   end Test_Factory;

   -------------------
   -- Test_Row_Api --
   -------------------

   procedure Test_Row_Api is
      Row : constant Gtk_Column_View_Row :=
        new Gtk_Column_View_Row_Record;
   begin
      --  GTK exposes no standalone constructor for a row. GObject's generic
      --  constructor creates the same valid, unbound object used before a row
      --  is attached to a ColumnView item.
      Assert_Cmpstr_Eq
        (Glib.Type_Name (Gtk.Column_View_Row.Get_Type), "GtkColumnViewRow");
      Glib.Object.G_New (Row, Gtk.Column_View_Row.Get_Type);

      --  An unbound row has no model item or position and is not selected.
      Assert_True (Row.Get_Item = null);
      Assert_Cmpuint_Eq (Row.Get_Position, Guint'Last);
      Assert_False (Row.Get_Selected);

      --  New rows start with empty accessible text and enabled interaction.
      Assert_Cmpstr_Eq (Row.Get_Accessible_Description, "");
      Assert_Cmpstr_Eq (Row.Get_Accessible_Label, "");
      Assert_True (Row.Get_Activatable);
      Assert_True (Row.Get_Focusable);
      Assert_True (Row.Get_Selectable);

      --  Check each mutable property through its getter after changing it.
      Row.Set_Accessible_Description ("row description");
      Assert_Cmpstr_Eq (Row.Get_Accessible_Description, "row description");
      Row.Set_Accessible_Label ("row label");
      Assert_Cmpstr_Eq (Row.Get_Accessible_Label, "row label");

      Row.Set_Activatable (False);
      Assert_False (Row.Get_Activatable);
      Row.Set_Activatable (True);
      Assert_True (Row.Get_Activatable);

      Row.Set_Focusable (False);
      Assert_False (Row.Get_Focusable);
      Row.Set_Focusable (True);
      Assert_True (Row.Get_Focusable);

      Row.Set_Selectable (False);
      Assert_False (Row.Get_Selectable);
      Row.Set_Selectable (True);
      Assert_True (Row.Get_Selectable);
   end Test_Row_Api;

   -----------------------------------
   -- Test_Construction_And_Factories --
   -----------------------------------

   procedure Test_Construction_And_Factories is
      View    : Gtk_Column_View;
      Factory : constant Gtk_Signal_List_Item_Factory :=
        Gtk_Signal_List_Item_Factory_New;
      Via_Gtk_New : Gtk_Signal_List_Item_Factory;
   begin
      --  Exercise the procedure constructor separately from the function
      --  constructor used throughout the other factory tests.
      Gtk.Signal_List_Item_Factory.Gtk_New (Via_Gtk_New);
      Assert_True (Via_Gtk_New /= null);

      --  Cover the procedure constructor and its Initialize path. A second
      --  Initialize call must leave the already-created model alone.
      Gtk_New (View, Gtk.Selection_Model.Null_Gtk_Selection_Model);
      Assert_True
        (View.Get_Model = Gtk.Selection_Model.Null_Gtk_Selection_Model);
      View.Initialize (Gtk.Selection_Model.Null_Gtk_Selection_Model);
      Assert_True
        (View.Get_Model = Gtk.Selection_Model.Null_Gtk_Selection_Model);

      --  SignalListItemFactory is a concrete GtkListItemFactory, so it can
      --  exercise both the section-header and row-factory accessors.
      Assert_True (View.Get_Header_Factory = null);
      View.Set_Header_Factory (Factory);
      Assert_True
        (View.Get_Header_Factory =
           Gtk.List_Item_Factory.Gtk_List_Item_Factory (Factory));

      Assert_True (View.Get_Row_Factory = null);
      View.Set_Row_Factory (Factory);
      Assert_True
        (View.Get_Row_Factory =
           Gtk.List_Item_Factory.Gtk_List_Item_Factory (Factory));

      View.Set_Header_Factory (null);
      View.Set_Row_Factory (null);
      Assert_True (View.Get_Header_Factory = null);
      Assert_True (View.Get_Row_Factory = null);
   end Test_Construction_And_Factories;

   ---------------------
   -- Test_Scrollable --
   ---------------------

   procedure Test_Scrollable is
      Items : constant Gtk_String_List := New_Strings;
      View  : constant Gtk_Column_View :=
        Gtk_Column_View_New (+Gtk_Single_Selection_New (+Items));
      Horizontal : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (0.0, 0.0, 100.0, 1.0, 10.0);
      Vertical : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (0.0, 0.0, 100.0, 1.0, 10.0);
      Border : Gtk.Border.Gtk_Border;
      Has_Border : Boolean;
   begin
      --  Assign explicit adjustments, then read them through GtkScrollable's
      --  interface methods to check both interface conversions.
      View.Set_Hadjustment (Horizontal);
      View.Set_Vadjustment (Vertical);
      Assert_True (View.Get_Hadjustment = Horizontal);
      Assert_True (View.Get_Vadjustment = Vertical);

      View.Set_Hscroll_Policy (Scroll_Natural);
      View.Set_Vscroll_Policy (Scroll_Natural);
      Assert_True (View.Get_Hscroll_Policy = Scroll_Natural);
      Assert_True (View.Get_Vscroll_Policy = Scroll_Natural);

      --  Border availability can depend on layout and the GTK theme, but a
      --  repeated query on this unchanged widget must report the same result.
      Has_Border := View.Get_Border (Border);
      Assert_True (Has_Border = View.Get_Border (Border));

      --  Scrolling can be requested before the widget is mapped; GTK retains
      --  the request until the list view becomes visible.
      View.Scroll_To
        (Pos    => 1,
         Column => null,
         Flags  => List_Scroll_None,
         Scroll => Null_Gtk_Scroll_Info);
      Assert_Cmpuint_Eq (Get_N_Items (Items), 3);
   end Test_Scrollable;

   -------------------------
   -- Test_Accessibility --
   -------------------------

   procedure Test_Accessibility is
      View    : constant Gtk_Column_View :=
        Gtk_Column_View_New (Gtk.Selection_Model.Null_Gtk_Selection_Model);
      Parent  : constant Gtk_Column_View :=
        Gtk_Column_View_New (Gtk.Selection_Model.Null_Gtk_Selection_Model);
      Sibling : constant Gtk_Column_View :=
        Gtk_Column_View_New (Gtk.Selection_Model.Null_Gtk_Selection_Model);
      X, Y, Width, Height : Gint;
      Id : constant UTF8_String := View.Get_Accessible_Id;
      Context : constant Gtk.Atcontext.Gtk_Atcontext := View.Get_At_Context;
   begin
      --  The role is fixed by GtkColumnView, and an unparented view is a root
      --  in the accessible tree. Its identifier remains stable across reads.
      Assert_True (View.Get_Accessible_Role = Accessible_Role_Tree_Grid);
      Assert_True (View.Get_Accessible_Parent = Null_Gtk_Accessible);
      Assert_Cmpstr_Eq (View.Get_Accessible_Id, Id);
      Assert_True (Context = null);
      Assert_True
        (View.Get_First_Accessible_Child /= Null_Gtk_Accessible);
      Assert_True
        (View.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  Before realization the widget has no screen bounds or focused state.
      Assert_False (View.Get_Bounds (X, Y, Width, Height));
      Assert_False
        (View.Get_Platform_State (Accessible_Platform_State_Focused));

      --  The accessible-tree setters are intended for non-widget accessible
      --  objects. On a detached GtkWidget, its physical parent remains null.
      View.Set_Accessible_Parent
        (Gtk.Column_View.Implements_Gtk_Accessible.To_Interface (Parent),
         Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (View.Get_Accessible_Parent = Null_Gtk_Accessible);
      View.Update_Next_Accessible_Sibling
        (Gtk.Column_View.Implements_Gtk_Accessible.To_Interface (Sibling));
      Assert_True (View.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      View.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      View.Update_Next_Accessible_Sibling (Gtk.Accessible.Null_Gtk_Accessible);

      --  The reset and announcement methods accept valid values on a
      --  headless widget; the fixed role remains stable across these updates.
      View.Announce ("Column view test", Accessible_Announcement_Priority_Low);
      View.Reset_Property (Accessible_Property_Label);
      View.Reset_Relation (Accessible_Relation_Labelled_By);
      View.Reset_State (Accessible_State_Selected);
      View.Update_Platform_State (Accessible_Platform_State_Focusable);
      Assert_True (View.Get_Accessible_Role = Accessible_Role_Tree_Grid);
      Assert_False
        (View.Get_Platform_State (Accessible_Platform_State_Focused));
   end Test_Accessibility;

   ------------------
   -- Test_Signals --
   ------------------

   procedure Test_Signals is
      Typed : constant Gtk_Column_View :=
        Gtk_Column_View_New (Gtk.Selection_Model.Null_Gtk_Selection_Model);
      Slot : constant Gtk_Column_View :=
        Gtk_Column_View_New (Gtk.Selection_Model.Null_Gtk_Selection_Model);
      Slot_Object : constant Gtk_Label := Gtk_Label_New ("signal slot");
      Name : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("activate");
   begin
      --  The signal has a row-position argument. Emitting it directly covers
      --  both generated marshallers without requiring a click or a window.
      Typed.On_Activate (On_Activate_Typed'Unrestricted_Access);
      Slot.On_Activate
        (On_Activate_Slot'Unrestricted_Access, Slot_Object);
      Emit_Activate (Get_Object (Typed), Name, 2, System.Null_Address);
      Emit_Activate (Get_Object (Slot), Name, 2, System.Null_Address);
      Gtkada.Types.Free (Name);
      Assert_Cmpuint_Eq (Guint (Typed_Activate_Count), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Activate_Count), 1);
   end Test_Signals;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/columnview/model", Test_Model'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/columnview/columns", Test_Columns'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/columnview/column-api", Test_Column_Api'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/columnview/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/columnview/sorting", Test_Sorting'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/columnview/factory", Test_Factory'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/columnview/row-api", Test_Row_Api'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/columnview/construction-factories",
      Test_Construction_And_Factories'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/columnview/scrollable", Test_Scrollable'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/columnview/accessibility", Test_Accessibility'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/columnview/signals", Test_Signals'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Column_View;
