--  Headless tests for Gtk.Grid_View. GTK has no matching GridView C test
--  in this checkout, so these cases cover its constructors, properties,
--  interface methods, accessibility hooks, and activate signal adapters.

with Ada.Command_Line;
with GNAT.Strings;
with System;

with Glib;                  use Glib;
with Glib.Test;             use Glib.Test;
with Glib.Object;           use Glib.Object;
with Gtk.Accessible;
with Gtk.Adjustment;        use Gtk.Adjustment;
with Gtk.Atcontext;
with Gtk.Border;
with Gtk.Box;               use Gtk.Box;
with Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Grid_View;         use Gtk.Grid_View;
with Gtk.Label;             use Gtk.Label;
with Gtk.List_Item_Factory;
with Gtk.Main;
with Gtk.Orientable;
with Gtk.Scrollable;
with Gtk.Scroll_Info;
with Gtk.Selection_Model;
with Gtk.Signal_List_Item_Factory; use Gtk.Signal_List_Item_Factory;
with Gtk.Single_Selection;  use Gtk.Single_Selection;
with Gtk.String_List;       use Gtk.String_List;
with Gtk.Buildable;
with Gtkada.Types;

procedure Grid_View is

   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Accessible.Gtk_Accessible_Role;
   use type Gtk.Adjustment.Gtk_Adjustment;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gtk.Grid_View.Gtk_Grid_View;
   use type Gtk.List_Item_Factory.Gtk_List_Item_Factory;
   use type Gtk.Selection_Model.Gtk_Selection_Model;
   use type Gtk.Enums.Gtk_List_Tab_Behavior;
   use type Gtk.Enums.Gtk_Orientation;
   use type Gtk.Enums.Gtk_Scrollable_Policy;
   use type Glib.Signal_Name;
   use type System.Address;

   Signal_Count : Natural := 0;
   Slot_Count   : Natural := 0;
   Last_Position : Guint := 0;

   procedure Emit_Int_Signal
     (Object : System.Address;
      Name   : Gtkada.Types.Chars_Ptr;
      Value  : Gint;
      Extra  : System.Address);
   pragma Import (C, Emit_Int_Signal, "ada_g_signal_emit_by_name_int_ptr");

   procedure Test_Constructors_And_Properties with Convention => C;
   procedure Test_Interfaces_And_Accessibility with Convention => C;
   procedure Test_Activate_Signal with Convention => C;

   function New_Selection return Gtk_Single_Selection;

   -------------------
   -- New_Selection --
   -------------------

   function New_Selection return Gtk_Single_Selection is
      Items : GNAT.Strings.String_List :=
        (new String'("one"), new String'("two"), new String'("three"));
   begin
      --  The helper uses Gtk.String_List's documented transfer-full input.
      return Gtk_Single_Selection_New (+Gtk_String_List_New (Items));
   end New_Selection;

   ---------------------
   -- On_Grid_Activate --
   ---------------------

   procedure On_Grid_Activate
     (Self : access Gtk_Grid_View_Record'Class;
      Position : Guint)
   is
   begin
      --  Record both callback arguments so the marshaller can be checked.
      if Self /= null then
         Signal_Count := Signal_Count + 1;
         Last_Position := Position;
      end if;
   end On_Grid_Activate;

   --------------------
   -- On_Slot_Activate --
   --------------------

   procedure On_Slot_Activate
     (Self : access GObject_Record'Class;
      Position : Guint)
   is
   begin
      --  The slot overload receives the connected slot object as Self.
      if Self /= null then
         Slot_Count := Slot_Count + 1;
         Last_Position := Position;
      end if;
   end On_Slot_Activate;

   --------------------------------
   -- Test_Constructors_And_Properties --
   --------------------------------

   procedure Test_Constructors_And_Properties is
      Selection : constant Gtk_Single_Selection := New_Selection;
      Factory   : constant Gtk_Signal_List_Item_Factory :=
        Gtk_Signal_List_Item_Factory_New;
      View      : constant Gtk_Grid_View :=
        Gtk_Grid_View_New (+Selection, Factory);
      Second_Model : constant Gtk_Single_Selection := New_Selection;
      Second_Factory : constant Gtk_Signal_List_Item_Factory :=
        Gtk_Signal_List_Item_Factory_New;
      Via_Gtk_New : Gtk_Grid_View;
   begin
      --  Verify the constructor retains both supplied objects.
      Assert_True (Gtk_Single_Selection (-View.Get_Model) = Selection);
      Assert_True (Gtk_Signal_List_Item_Factory (View.Get_Factory) = Factory);

      --  A second Initialize call must leave an already-created widget intact.
      View.Initialize (Gtk.Selection_Model.Null_Gtk_Selection_Model, null);
      Assert_True (Gtk_Single_Selection (-View.Get_Model) = Selection);
      Assert_True (Gtk_Signal_List_Item_Factory (View.Get_Factory) = Factory);

      --  Exercise the procedure constructor and its model/factory setters.
      Gtk_New (Via_Gtk_New, Gtk.Selection_Model.Null_Gtk_Selection_Model, null);
      Assert_True (Via_Gtk_New.Get_Model = Gtk.Selection_Model.Null_Gtk_Selection_Model);
      Via_Gtk_New.Set_Model (+Second_Model);
      Assert_True (Gtk_Single_Selection (-Via_Gtk_New.Get_Model) = Second_Model);
      Via_Gtk_New.Set_Factory (Second_Factory);
      Assert_True
        (Gtk_Signal_List_Item_Factory (Via_Gtk_New.Get_Factory) = Second_Factory);

      --  Check property round trips, including column bounds and tab behavior.
      Assert_False (View.Get_Enable_Rubberband);
      View.Set_Enable_Rubberband (True);
      Assert_True (View.Get_Enable_Rubberband);
      View.Set_Max_Columns (5);
      View.Set_Min_Columns (3);
      Assert_Cmpuint_Eq (View.Get_Max_Columns, 5);
      Assert_Cmpuint_Eq (View.Get_Min_Columns, 3);
      View.Set_Max_Columns (2);
      --  GtkGridView stores the requested maximum independently of the minimum.
      Assert_Cmpuint_Eq (View.Get_Max_Columns, 2);
      Assert_Cmpuint_Eq (View.Get_Min_Columns, 3);
      View.Set_Single_Click_Activate (True);
      Assert_True (View.Get_Single_Click_Activate);
      View.Set_Single_Click_Activate (False);
      Assert_False (View.Get_Single_Click_Activate);
      View.Set_Tab_Behavior (List_Tab_Cell);
      Assert_True (View.Get_Tab_Behavior = List_Tab_Cell);
      View.Set_Tab_Behavior (List_Tab_Item);
      Assert_True (View.Get_Tab_Behavior = List_Tab_Item);

      --  Scrolling is valid before the view is shown; the model has position 1.
      View.Scroll_To
        (1, List_Scroll_None, Gtk.Scroll_Info.Null_Gtk_Scroll_Info);
      Assert_Cmpuint_Eq (View.Get_Max_Columns, 2);
   end Test_Constructors_And_Properties;

   -------------------------------------
   -- Test_Interfaces_And_Accessibility --
   -------------------------------------

   procedure Test_Interfaces_And_Accessibility is
      View    : constant Gtk_Grid_View :=
        Gtk_Grid_View_New (Gtk.Selection_Model.Null_Gtk_Selection_Model, null);
      Parent  : Gtk_Box;
      Sibling : Gtk_Label;
      H_Adjustment : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (0.0, 0.0, 100.0, 1.0, 10.0);
      V_Adjustment : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (0.0, 0.0, 100.0, 1.0, 10.0);
      Border : Gtk.Border.Gtk_Border;
      X, Y, Width, Height : Gint;
      Accessible_Id : constant UTF8_String := View.Get_Accessible_Id;
      Accessible : constant Gtk.Accessible.Gtk_Accessible :=
        Gtk.Grid_View.Implements_Gtk_Accessible.To_Interface (View);
      Buildable : constant Gtk.Buildable.Gtk_Buildable :=
        Gtk.Grid_View.Implements_Gtk_Buildable.To_Interface (View);
      Constraint_Target : constant Gtk.Constraint_Target.Gtk_Constraint_Target :=
        Gtk.Grid_View.Implements_Gtk_Constraint_Target.To_Interface (View);
      Orientable : constant Gtk.Orientable.Gtk_Orientable :=
        Gtk.Grid_View.Implements_Gtk_Orientable.To_Interface (View);
      Scrollable : constant Gtk.Scrollable.Gtk_Scrollable :=
        Gtk.Grid_View.Implements_Gtk_Scrollable.To_Interface (View);
   begin
      --  Round-trip all interfaces implemented by GtkGridView, including the
      --  two interfaces whose inherited methods are intentionally not exposed.
      Assert_True
        (Gtk.Grid_View.Implements_Gtk_Accessible.To_Object (Accessible) = View);
      Assert_True
        (Gtk.Grid_View.Implements_Gtk_Buildable.To_Object (Buildable) = View);
      Assert_True
        (Gtk.Grid_View.Implements_Gtk_Constraint_Target.To_Object
           (Constraint_Target) = View);
      Assert_True
        (Gtk.Grid_View.Implements_Gtk_Orientable.To_Object (Orientable) = View);
      Assert_True
        (Gtk.Grid_View.Implements_Gtk_Scrollable.To_Object (Scrollable) = View);

      --  GtkGridView advertises the grid role. Before realization, GTK has not
      --  created the widget's accessibility context yet.
      Assert_True (View.Get_Accessible_Role = Gtk.Accessible.Accessible_Role_Grid);
      Assert_True (View.Get_At_Context = null);
      Assert_Cmpuint_Ge (Guint (Accessible_Id'Length), 0);
      Assert_True (View.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (View.Get_First_Accessible_Child = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (View.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);

      --  An unparented, unrealized widget has no accessible screen bounds.
      Assert_False (View.Get_Bounds (X, Y, Width, Height));
      Assert_False (View.Get_Border (Border));
      Assert_False
        (View.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focused));

      --  These Accessible setters are intended for non-widget accessibility
      --  objects. GtkWidget's physical hierarchy still reports no parent here.
      Gtk_New (Parent, Orientation_Vertical, 0);
      Gtk_New (Sibling, "sibling");
      View.Set_Accessible_Parent
        (Gtk.Box.Implements_Gtk_Accessible.To_Interface (Parent),
                                  Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (View.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      View.Update_Next_Accessible_Sibling
        (Gtk.Label.Implements_Gtk_Accessible.To_Interface (Sibling));
      Assert_True
        (View.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      View.Set_Accessible_Parent (Gtk.Accessible.Null_Gtk_Accessible,
                                  Gtk.Accessible.Null_Gtk_Accessible);
      View.Update_Next_Accessible_Sibling
        (Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (View.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (View.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);

      --  The accessible update/reset calls accept valid enum values on a
      --  headless widget; the role remains stable across these state changes.
      View.Announce ("Grid view test", Gtk.Accessible.Accessible_Announcement_Priority_Low);
      View.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focusable);
      View.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      View.Reset_Relation (Gtk.Accessible.Accessible_Relation_Described_By);
      View.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      Assert_True (View.Get_Accessible_Role = Gtk.Accessible.Accessible_Role_Grid);

      --  GridView implements GtkOrientable and GtkScrollable. Round-trip both
      --  orientations, both policies, and caller-provided adjustments.
      Assert_True (View.Get_Orientation = Orientation_Vertical);
      View.Set_Orientation (Orientation_Horizontal);
      Assert_True (View.Get_Orientation = Orientation_Horizontal);
      View.Set_Orientation (Orientation_Vertical);
      View.Set_Hscroll_Policy (Scroll_Natural);
      View.Set_Vscroll_Policy (Scroll_Natural);
      Assert_True (View.Get_Hscroll_Policy = Scroll_Natural);
      Assert_True (View.Get_Vscroll_Policy = Scroll_Natural);
      View.Set_Hadjustment (H_Adjustment);
      View.Set_Vadjustment (V_Adjustment);
      Assert_True (View.Get_Hadjustment = H_Adjustment);
      Assert_True (View.Get_Vadjustment = V_Adjustment);
      View.Set_Hadjustment (null);
      View.Set_Vadjustment (null);
      --  GtkScrollable supplies default adjustments when these are reset.
      Assert_True (View.Get_Hadjustment /= null);
      Assert_True (View.Get_Vadjustment /= null);
   end Test_Interfaces_And_Accessibility;

   ------------------------
   -- Test_Activate_Signal --
   ------------------------

   procedure Test_Activate_Signal is
      View : constant Gtk_Grid_View :=
        Gtk_Grid_View_New (Gtk.Selection_Model.Null_Gtk_Selection_Model, null);
      Slot : Gtk_Label;
      Name : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("activate");
   begin
      --  Both overloads install a different closure marshaller for the same
      --  position-valued signal. Emit it directly to avoid physical input.
      Assert_True (Gtk.Grid_View.Signal_Activate = "activate");
      Gtk_New (Slot, "signal slot");
      View.On_Activate (On_Grid_Activate'Unrestricted_Access);
      View.On_Activate (On_Slot_Activate'Unrestricted_Access, Slot);
      Emit_Int_Signal (View.Get_Object, Name, 17, System.Null_Address);
      Gtkada.Types.Free (Name);

      Assert_Cmpuint_Eq (Guint (Signal_Count), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Count), 1);
      Assert_Cmpuint_Eq (Last_Position, 17);
   end Test_Activate_Signal;

begin
   --  GTK widgets need GTK initialization in addition to the GLib test setup.
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/grid-view/constructors-properties",
      Test_Constructors_And_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/grid-view/interfaces-accessibility",
      Test_Interfaces_And_Accessibility'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/grid-view/activate-signal",
      Test_Activate_Signal'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Grid_View;
