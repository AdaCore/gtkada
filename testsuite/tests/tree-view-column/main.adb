--  Tests the Gtk.Tree_View_Column constructors, properties, cell-layout
--  methods, data callbacks, and clicked signal. The constructor cases port
--  test_column_new and test_column_new_with_area from testsuite/c_tests/cellarea.c.

pragma Warnings (Off, "*obsolescent*");

with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces.C;
with Glib;                  use Glib;
with Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Test;             use Glib.Test;
with Gtk.Cell_Area;         use Gtk.Cell_Area;
with Gtk.Cell_Area_Box;     use Gtk.Cell_Area_Box;
with Gtk.Cell_Layout;
with Gtk.Cell_Renderer;     use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Label;             use Gtk.Label;
with Gtk.List_Store;        use Gtk.List_Store;
with Gtk.Main;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;  use Gtk.Tree_View_Column;
with Gtk.Widget;
with Gtkada.Bindings;
with System;

procedure Main is
   use type Interfaces.C.C_float;
   use type Gtk.Cell_Area.Gtk_Cell_Area;
   use type Gtk.Cell_Renderer.Gtk_Cell_Renderer;
   use type Gtk.Tree_Model.Gtk_Tree_Model;
   use type Gtk.Tree_View_Column.Gtk_Tree_View_Column;
   use type Gtk.Widget.Gtk_Widget;
   use type System.Address;

   Typed_Clicks : Natural := 0;
   Slot_Clicks  : Natural := 0;
   Data_Calls   : Natural := 0;
   Destroy_Calls : Natural := 0;
   Callback_Exception_Calls : Natural := 0;
   Callback_Error : exception;

   --  The data callback replaces the model-mapped text with an observable
   --  value, proving that Gtk's C callback reaches the Ada procedure.
   procedure Set_Cell_Data
     (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter);

   procedure Set_Cell_Data_With_User_Data
     (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data        : Gint);

   --  Count closure destruction when GTK removes the callback's user data.
   procedure Destroy_User_Data (Data : in out Gint);

   package Data_Callbacks is new Gtk.Tree_View_Column.Set_Cell_Data_Func_User_Data
     (User_Data_Type => Gint,
      Destroy        => Destroy_User_Data);

   procedure On_Typed_Clicked
     (Self : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class);
   procedure On_Slot_Clicked
     (Self : access Glib.Object.GObject_Record'Class);
   procedure Raise_Typed_Clicked
     (Self : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class);
   procedure Raise_Slot_Clicked
     (Self : access Glib.Object.GObject_Record'Class);
   procedure Record_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Test_Constructors
   with Convention => C;
   procedure Test_Properties_And_Signals
   with Convention => C;
   procedure Test_Cell_Layout_And_Data
   with Convention => C;

   --  Set the callback's renderer property and count the invocation.
   procedure Set_Cell_Data
     (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter) is
   begin
      Data_Calls := Data_Calls + 1;
      Glib.Properties.Set_Property
        (Cell, Gtk.Cell_Renderer_Text.Text_Property, "plain callback");
   end Set_Cell_Data;

   --  The generic callback additionally confirms that its user data survives
   --  the GtkAda closure and is passed to the Ada callback intact.
   procedure Set_Cell_Data_With_User_Data
     (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data        : Gint) is
   begin
      Data_Calls := Data_Calls + 1;
      Glib.Properties.Set_Property
        (Cell, Gtk.Cell_Renderer_Text.Text_Property, Gint'Image (Data));
   end Set_Cell_Data_With_User_Data;

   --  GTK calls this destroy notify when the user-data callback is cleared.
   procedure Destroy_User_Data (Data : in out Gint) is
   begin
      Destroy_Calls := Destroy_Calls + 1;
   end Destroy_User_Data;

   --  Typed and slot handlers are both exercised through the clicked signal.
   procedure On_Typed_Clicked
     (Self : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class) is
   begin
      Typed_Clicks := Typed_Clicks + 1;
   end On_Typed_Clicked;

   procedure On_Slot_Clicked
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Slot_Clicks := Slot_Clicks + 1;
   end On_Slot_Clicked;

   --  These callbacks deliberately raise so each signal marshaler's
   --  exception boundary can be verified without propagating into GTK.
   procedure Raise_Typed_Clicked
     (Self : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class) is
   begin
      raise Callback_Error;
   end Raise_Typed_Clicked;

   procedure Raise_Slot_Clicked
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      raise Callback_Error;
   end Raise_Slot_Clicked;

   --  GtkAda routes callback exceptions here and keeps them from crossing C.
   procedure Record_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
   begin
      Callback_Exception_Calls := Callback_Exception_Calls + 1;
   end Record_Callback_Exception;

   ----------------------
   -- Test_Constructors --
   ----------------------

   --  Port the default-area and supplied-area constructor checks from C.
   --  Also call each Initialize form twice to cover its created-object guard.
   procedure Test_Constructors is
      Default_Column : constant Gtk_Tree_View_Column :=
        Gtk_Tree_View_Column_New;
      Supplied_Area : constant Gtk_Cell_Area_Box := Gtk_Cell_Area_Box_New;
      Area_Column : constant Gtk_Tree_View_Column :=
        Gtk_Tree_View_Column_New_With_Area (Supplied_Area);
      Via_Procedure : Gtk_Tree_View_Column;
      Via_Area_Procedure : Gtk_Tree_View_Column;
      Existing : constant Gtk_Tree_View_Column := Gtk_Tree_View_Column_New;
   begin
      Assert_True (Default_Column.Is_Created);
      Assert_True
        (Gtk.Cell_Area.Get_Area (+Default_Column) /= null);
      Assert_True
        (Gtk.Cell_Area.Get_Area (+Area_Column) = Gtk_Cell_Area (Supplied_Area));

      Gtk_New (Via_Procedure);
      Gtk_New_With_Area (Via_Area_Procedure, Supplied_Area);
      Assert_True (Via_Procedure.Is_Created and Via_Area_Procedure.Is_Created);
      Assert_True
        (Gtk.Cell_Area.Get_Area (+Via_Area_Procedure)
         = Gtk_Cell_Area (Supplied_Area));

      Gtk.Tree_View_Column.Initialize (Existing);
      Gtk.Tree_View_Column.Initialize_With_Area (Existing, Supplied_Area);
      Assert_True
        (Gtk.Cell_Area.Get_Area (+Existing) /= Gtk_Cell_Area (Supplied_Area));

      --  Both Convert overloads round-trip GtkAda's registered object handle.
      Assert_True (Convert (Default_Column) = Glib.Object.Get_Object (Default_Column));
      Assert_True
        (Convert (Glib.Object.Get_Object (Default_Column)) = Default_Column);
   end Test_Constructors;

   ----------------------------------
   -- Test_Properties_And_Signals --
   ----------------------------------

   --  Round-trip scalar settings, header widgets and signal adapters. A
   --  tree view supplies a real header button without showing a window.
   procedure Test_Properties_And_Signals is
      Column : constant Gtk_Tree_View_Column := Gtk_Tree_View_Column_New;
      View   : constant Gtk.Tree_View.Gtk_Tree_View := Gtk.Tree_View.Gtk_Tree_View_New;
      Store  : constant Gtk_List_Store :=
        Gtk_List_Store_Newv ((0 => GType_String));
      Header : constant Gtk_Label := Gtk_Label_New ("column header");
   begin
      --  A sortable model is required when a column's sort ID is set.
      Gtk.Tree_View.Set_Model (View, +Store);
      Assert_Cmpfloat_Eq (Gdouble (Get_Alignment (Column)), 0.0);
      Set_Alignment (Column, 0.25);
      Assert_Cmpfloat_Eq (Gdouble (Get_Alignment (Column)), 0.25);

      Assert_False (Get_Clickable (Column));
      Set_Clickable (Column, True);
      Assert_True (Get_Clickable (Column));
      Set_Expand (Column, True);
      Assert_True (Get_Expand (Column));
      Set_Expand (Column, False);
      Assert_False (Get_Expand (Column));

      Set_Fixed_Width (Column, 126);
      Assert_Cmpint_Eq (Get_Fixed_Width (Column), 126);
      Set_Min_Width (Column, 41);
      Set_Max_Width (Column, 260);
      Assert_Cmpint_Eq (Get_Min_Width (Column), 41);
      Assert_Cmpint_Eq (Get_Max_Width (Column), 260);
      Set_Min_Width (Column, -1);
      Set_Max_Width (Column, -1);
      Assert_Cmpint_Eq (Get_Min_Width (Column), -1);
      Assert_Cmpint_Eq (Get_Max_Width (Column), -1);

      Set_Sizing (Column, Autosize);
      Assert_True (Get_Sizing (Column) = Autosize);
      Set_Resizable (Column, True);
      Assert_True (Get_Resizable (Column));
      Assert_True (Get_Sizing (Column) = Grow_Only);
      Set_Sizing (Column, Fixed);
      Assert_True (Get_Sizing (Column) = Fixed);
      Set_Resizable (Column, False);
      Assert_False (Get_Resizable (Column));

      Set_Reorderable (Column, True);
      Assert_True (Get_Reorderable (Column));
      Set_Reorderable (Column, False);
      Assert_False (Get_Reorderable (Column));
      Set_Sort_Column_Id (Column, 0);
      Assert_Cmpint_Eq (Get_Sort_Column_Id (Column), 0);
      Set_Sort_Indicator (Column, True);
      Assert_True (Get_Sort_Indicator (Column));
      Set_Sort_Order (Column, Sort_Descending);
      Assert_True (Get_Sort_Order (Column) = Sort_Descending);
      Set_Sort_Indicator (Column, False);
      Assert_False (Get_Sort_Indicator (Column));

      Set_Spacing (Column, 7);
      Assert_Cmpint_Eq (Get_Spacing (Column), 7);
      Set_Title (Column, "Ada column");
      Assert_Cmpstr_Eq (Get_Title (Column), "Ada column");
      Assert_True (Get_Visible (Column));
      Set_Visible (Column, False);
      Assert_False (Get_Visible (Column));
      Set_Visible (Column, True);

      Assert_True (Gtk.Tree_View.Append_Column (View, Column) = 1);
      Assert_True (Get_Tree_View (Column) = Gtk.Widget.Gtk_Widget (View));
      Assert_Nonnull (Glib.Object.Get_Object (Get_Button (Column)));
      Set_Widget (Column, Header);
      Assert_True (Get_Widget (Column) = Gtk.Widget.Gtk_Widget (Header));
      Set_Widget (Column, null);
      Assert_True (Get_Widget (Column) = null);

      --  Width and offset are zero or positive before the view is allocated.
      Assert_True (Get_Width (Column) >= 0);
      Assert_True (Get_X_Offset (Column) >= 0);
      Queue_Resize (Column);

      On_Clicked (Column, Call => On_Typed_Clicked'Unrestricted_Access);
      On_Clicked
        (Column, On_Slot_Clicked'Unrestricted_Access, Slot => Header);
      Clicked (Column);
      Assert_Cmpuint_Eq (Guint (Typed_Clicks), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Clicks), 1);

      --  Check both exception handlers while GTK invokes each callback.
      Gtkada.Bindings.Set_On_Exception
        (Record_Callback_Exception'Unrestricted_Access);
      On_Clicked (Column, Call => Raise_Typed_Clicked'Unrestricted_Access);
      On_Clicked
        (Column, Raise_Slot_Clicked'Unrestricted_Access, Slot => Header);
      Clicked (Column);
      Assert_Cmpuint_Eq (Guint (Callback_Exception_Calls), 2);
   end Test_Properties_And_Signals;

   --------------------------------
   -- Test_Cell_Layout_And_Data --
   --------------------------------

   --  Exercise renderer packing/order/clearing, attribute mapping, cell
   --  sizing, and both callback forms against a deterministic one-row model.
   procedure Test_Cell_Layout_And_Data is
      Column   : constant Gtk_Tree_View_Column := Gtk_Tree_View_Column_New;
      View     : constant Gtk.Tree_View.Gtk_Tree_View := Gtk.Tree_View.Gtk_Tree_View_New;
      First    : constant Gtk_Cell_Renderer_Text := Gtk_Cell_Renderer_Text_New;
      Last     : constant Gtk_Cell_Renderer_Text := Gtk_Cell_Renderer_Text_New;
      Outside  : constant Gtk_Cell_Renderer_Text := Gtk_Cell_Renderer_Text_New;
      Store    : Gtk_List_Store;
      Iter     : Gtk_Tree_Iter;
      X_Offset : Gint;
      Y_Offset : Gint;
      Width    : Gint;
      Height   : Gint;
      Cell_X   : Gint;
      Cell_W   : Gint;
      Found    : Boolean;
      Cells    : Gtk.Cell_Renderer.Cell_Renderer_List.Glist;
   begin
      Gtk_New (Store, (0 => GType_String));
      Store.Append (Iter);
      Store.Set (Iter, 0, "model value");
      Gtk.Tree_View.Set_Model (View, +Store);
      Assert_True (Gtk.Tree_View.Append_Column (View, Column) = 1);
      Pack_Start (Column, First, False);
      Pack_End (Column, Last, True);
      Assert_Cmpuint_Eq (Gtk.Cell_Renderer.Cell_Renderer_List.Length (Get_Cells (Column)), 2);
      Reorder (Column, Last, 0);
      Cells := Get_Cells (Column);
      Assert_True
        (Gtk_Cell_Renderer
           (Gtk.Cell_Renderer.Cell_Renderer_List.Get_Data
              (Gtk.Cell_Renderer.Cell_Renderer_List.Nth (Cells, 0))) =
         Gtk_Cell_Renderer (Last));

      Add_Attribute (Column, First, "text", 0);
      Cell_Set_Cell_Data (Column, +Store, Iter, False, False);
      Assert_Cmpstr_Eq (Get_Property (First, Text_Property), "model value");
      Clear_Attributes (Column, First);

      --  Cell_Get_Position reports False for a renderer not packed in the
      --  column; the packed renderer can still be focused and measured.
      Cell_Get_Position (Column, Outside, Cell_X, Cell_W, Found);
      Assert_False (Found);
      Focus_Cell (Column, First);
      Cell_Get_Size (Column, X_Offset, Y_Offset, Width, Height);
      Assert_True (Width >= 0 and Height >= 0);
      Assert_True (Cell_Is_Visible (Column));

      Set_Cell_Data_Func
        (Column, First, Set_Cell_Data'Unrestricted_Access);
      Cell_Set_Cell_Data (Column, +Store, Iter, True, False);
      Assert_Cmpuint_Eq (Guint (Data_Calls), 1);
      Assert_Cmpstr_Eq (Get_Property (First, Text_Property), "plain callback");
      Set_Cell_Data_Func (Column, First, null);

      Data_Callbacks.Set_Cell_Data_Func
        (Column, First, Set_Cell_Data_With_User_Data'Unrestricted_Access, 37);
      Cell_Set_Cell_Data (Column, +Store, Iter, False, True);
      Assert_Cmpuint_Eq (Guint (Data_Calls), 2);
      Assert_Cmpstr_Eq (Get_Property (First, Text_Property), " 37");
      Data_Callbacks.Set_Cell_Data_Func (Column, First, null, 0);
      Assert_Cmpuint_Eq (Guint (Destroy_Calls), 1);

      --  Clearing the layout removes both renderer entries.
      Clear (Column);
      Assert_Cmpuint_Eq (Gtk.Cell_Renderer.Cell_Renderer_List.Length (Get_Cells (Column)), 0);
   end Test_Cell_Layout_And_Data;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/tree-view-column/constructors", Test_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-view-column/properties-signals",
      Test_Properties_And_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-view-column/cell-layout-data",
      Test_Cell_Layout_And_Data'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
