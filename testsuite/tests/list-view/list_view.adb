--  Headless tests for Gtk.List_View. There is no matching GTK C test in this
--  checkout, so these cases exercise the documented widget API directly.
--  The signal case emits the action signal without physical user input.

with Ada.Command_Line;
with Ada.Exceptions;
with GNAT.Strings;
with System;

with Glib;                    use Glib;
with Glib.Main;               use Glib.Main;
with Glib.Object;             use Glib.Object;
with Glib.Test;               use Glib.Test;
with Gtk.Accessible;
with Gtk.Adjustment;           use Gtk.Adjustment;
with Gtk.Atcontext;
with Gtk.Border;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Label;
with Gtk.List_Base;
with Gtk.List_Item_Factory;
with Gtk.List_View;            use Gtk.List_View;
with Gtk.Main;
with Gtk.List_Item;            use Gtk.List_Item;
with Gtk.Scroll_Info;
with Gtk.Selection_Model;
with Gtk.Signal_List_Item_Factory; use Gtk.Signal_List_Item_Factory;
with Gtk.Single_Selection;     use Gtk.Single_Selection;
with Gtk.String_List;          use Gtk.String_List;
with Gtk.Window;               use Gtk.Window;
with Gtk.Widget;
with Gtkada.Bindings;
with Gtkada.Types;

procedure List_View is

   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Accessible.Gtk_Accessible_Role;
   use type Gtk.Adjustment.Gtk_Adjustment;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Glib.Object.GObject;
   use type Gtk.List_Item_Factory.Gtk_List_Item_Factory;
   use type Gtk.List_View.Gtk_List_View;
   use type Gtk.List_Item.Gtk_List_Item;
   use type Gtk.Selection_Model.Gtk_Selection_Model;
   use type Gtk.Widget.Gtk_Widget;
   use type Gtk.Enums.Gtk_List_Tab_Behavior;
   use type Gtk.Enums.Gtk_Orientation;
   use type Gtk.Enums.Gtk_Scrollable_Policy;
   use type Glib.Signal_Name;
   use type System.Address;

   Signal_Count  : Natural := 0;
   Slot_Count    : Natural := 0;
   Last_Position : Guint := 0;
   Raise_Callbacks : Boolean := False;
   Exception_Count : Natural := 0;
   Unexpected_Exception_Count : Natural := 0;

   Factory_Setup_Count : Natural := 0;
   Factory_Bind_Count  : Natural := 0;
   Factory_Done        : Boolean := False;
   pragma Volatile (Factory_Setup_Count);
   pragma Volatile (Factory_Bind_Count);
   pragma Volatile (Factory_Done);

   --  This helper emits the position-valued activate signal through GObject.
   procedure Emit_Int_Signal
     (Object : System.Address;
      Name   : Gtkada.Types.Chars_Ptr;
      Value  : Gint;
      Extra  : System.Address);
   pragma Import (C, Emit_Int_Signal, "ada_g_signal_emit_by_name_int_ptr");

   procedure Test_Constructors_And_Properties with Convention => C;
   procedure Test_Interfaces_And_Accessibility with Convention => C;
   procedure Test_List_Base_Bindings with Convention => C;
   procedure Test_Activate_Signal with Convention => C;
   procedure Test_List_Item_Api with Convention => C;
   procedure Inspect_List_Item_Setup
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);
   procedure Inspect_List_Item_Bind
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);
   function Stop_List_Item_Test return Boolean;
   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   function New_Selection return Gtk_Single_Selection;

   -------------------
   -- New_Selection --
   -------------------

   function New_Selection return Gtk_Single_Selection is
      Items : GNAT.Strings.String_List :=
        (new String'("one"), new String'("two"), new String'("three"));
   begin
      --  String_List_New consumes the array's strings while constructing the
      --  GListModel, and the selection model owns that list model afterward.
      return Gtk_Single_Selection_New (+Gtk_String_List_New (Items));
   end New_Selection;

   -------------------
   -- On_Activate --
   -------------------

   procedure On_Activate
     (Self : access Gtk_List_View_Record'Class;
      Position : Guint)
   is
   begin
      --  Keep the typed callback's object and position observable to the test.
      if Self /= null then
         Signal_Count := Signal_Count + 1;
         Last_Position := Position;
      end if;
      if Raise_Callbacks then
         raise Program_Error with "expected typed activate callback exception";
      end if;
   end On_Activate;

   ----------------------
   -- On_Slot_Activate --
   ----------------------

   procedure On_Slot_Activate
     (Self : access GObject_Record'Class;
      Position : Guint)
   is
   begin
      --  The slot overload receives the connected GObject as its first input.
      if Self /= null then
         Slot_Count := Slot_Count + 1;
         Last_Position := Position;
      end if;
      if Raise_Callbacks then
         raise Program_Error with "expected slot activate callback exception";
      end if;
   end On_Slot_Activate;

   --------------------
   -- Record_Exception --
   --------------------

   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
   begin
      --  Verify that both marshallers contain the deliberate callback errors.
      if Ada.Exceptions.Exception_Name (Occurrence) = "PROGRAM_ERROR" then
         Exception_Count := Exception_Count + 1;
      else
         Unexpected_Exception_Count := Unexpected_Exception_Count + 1;
      end if;
   end Record_Exception;

   ---------------------------
   -- List-item factory test --
   ---------------------------

   function Stop_List_Item_Test return Boolean is
   begin
      --  Bound the event loop so a broken factory fails by assertion instead
      --  of hanging the testsuite process.
      Factory_Done := True;
      Wakeup (null);
      return False;
   end Stop_List_Item_Test;

   procedure Inspect_List_Item_Setup
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class)
   is
      pragma Unreferenced (Self);
      Label : Gtk.Label.Gtk_Label;
   begin
      --  Setup receives a genuine, unbound GtkListItem from GtkListView.
      Assert_True (Item.Get_Child = null);
      Assert_True (Item.Get_Item = null);
      Assert_Cmpuint_Eq (Item.Get_Position, Guint'Last);
      Assert_False (Item.Get_Selected);

      --  Check the accessible text defaults, then round-trip each setter.
      Assert_Cmpstr_Eq (Item.Get_Accessible_Description, "");
      Item.Set_Accessible_Description ("list item description");
      Assert_Cmpstr_Eq
        (Item.Get_Accessible_Description, "list item description");
      Assert_Cmpstr_Eq (Item.Get_Accessible_Label, "");
      Item.Set_Accessible_Label ("list item label");
      Assert_Cmpstr_Eq (Item.Get_Accessible_Label, "list item label");

      --  These interaction flags start enabled; verify both values can be
      --  set and read while the item is unbound.
      Assert_True (Item.Get_Activatable);
      Item.Set_Activatable (False);
      Assert_False (Item.Get_Activatable);
      Item.Set_Activatable (True);
      Assert_True (Item.Get_Activatable);
      Item.Set_Focusable (False);
      Assert_False (Item.Get_Focusable);
      Item.Set_Focusable (True);
      Assert_True (Item.Get_Focusable);
      Assert_True (Item.Get_Selectable);
      Item.Set_Selectable (False);
      Assert_False (Item.Get_Selectable);
      Item.Set_Selectable (True);
      Assert_True (Item.Get_Selectable);

      --  The factory populates the child before binding the item to a model.
      Gtk.Label.Gtk_New (Label, "list item child");
      Item.Set_Child (Label);
      Assert_True (Item.Get_Child = Gtk.Widget.Gtk_Widget (Label));
      Factory_Setup_Count := Factory_Setup_Count + 1;
   end Inspect_List_Item_Setup;

   procedure Inspect_List_Item_Bind
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      --  Once bound, a list item exposes its model object and current row.
      Assert_True (Item.Get_Item /= null);
      Assert_True (Item.Get_Position /= Guint'Last);
      Assert_True (Item.Get_Selected = (Item.Get_Position = 0));
      Factory_Bind_Count := Factory_Bind_Count + 1;
   end Inspect_List_Item_Bind;

   procedure Test_List_Item_Api is
      Selection : constant Gtk_Single_Selection := New_Selection;
      Factory : constant Gtk_Signal_List_Item_Factory :=
        Gtk_Signal_List_Item_Factory_New;
      View : constant Gtk_List_View := Gtk_List_View_New (+Selection, Factory);
      Window : Gtk_Window;
      Timeout : G_Source_Id;
   begin
      --  List items belong to the view, so observe one through the factory
      --  callbacks instead of constructing it in application code.
      Factory.On_Setup (Inspect_List_Item_Setup'Unrestricted_Access);
      Factory.On_Bind (Inspect_List_Item_Bind'Unrestricted_Access);
      Factory_Setup_Count := 0;
      Factory_Bind_Count := 0;
      Factory_Done := False;

      Gtk.Window.Gtk_New (Window);
      Window.Set_Default_Size (160, 120);
      Window.Set_Child (View);
      Window.Present;
      Timeout := Timeout_Add
        (5_000, Stop_List_Item_Test'Unrestricted_Access);
      pragma Unreferenced (Timeout);

      while not Factory_Done and then Factory_Bind_Count = 0 loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => True);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;

      Assert_Cmpuint_Gt (Guint (Factory_Setup_Count), 0);
      Assert_Cmpuint_Gt (Guint (Factory_Bind_Count), 0);
      Window.Destroy;
   end Test_List_Item_Api;

   --------------------------------------
   -- Test_Constructors_And_Properties --
   --------------------------------------

   procedure Test_Constructors_And_Properties is
      Selection : constant Gtk_Single_Selection := New_Selection;
      Factory : constant Gtk_Signal_List_Item_Factory :=
        Gtk_Signal_List_Item_Factory_New;
      Header_Factory : constant Gtk_Signal_List_Item_Factory :=
        Gtk_Signal_List_Item_Factory_New;
      View : constant Gtk_List_View := Gtk_List_View_New (+Selection, Factory);
      Second_Model : constant Gtk_Single_Selection := New_Selection;
      Second_Factory : constant Gtk_Signal_List_Item_Factory :=
        Gtk_Signal_List_Item_Factory_New;
      Second_Header_Factory : constant Gtk_Signal_List_Item_Factory :=
        Gtk_Signal_List_Item_Factory_New;
      Via_Gtk_New : Gtk_List_View;
   begin
      --  The function constructor retains its supplied model and factory.
      Assert_True (Gtk_Single_Selection (-View.Get_Model) = Selection);
      Assert_True (Gtk_Signal_List_Item_Factory (View.Get_Factory) = Factory);
      Assert_True (View.Get_Header_Factory = null);
      View.Set_Header_Factory (Header_Factory);
      Assert_True
        (Gtk_Signal_List_Item_Factory (View.Get_Header_Factory) = Header_Factory);

      --  Initialize is a no-op for a widget that is already created.
      View.Initialize (Gtk.Selection_Model.Null_Gtk_Selection_Model, null);
      Assert_True (Gtk_Single_Selection (-View.Get_Model) = Selection);
      Assert_True (Gtk_Signal_List_Item_Factory (View.Get_Factory) = Factory);

      --  Exercise the procedure constructor and settable model/factory APIs.
      Gtk_New (Via_Gtk_New, Gtk.Selection_Model.Null_Gtk_Selection_Model, null);
      Assert_True
        (Via_Gtk_New.Get_Model = Gtk.Selection_Model.Null_Gtk_Selection_Model);
      Assert_True (Via_Gtk_New.Get_Factory = null);
      Via_Gtk_New.Set_Model (+Second_Model);
      Assert_True (Gtk_Single_Selection (-Via_Gtk_New.Get_Model) = Second_Model);
      Via_Gtk_New.Set_Factory (Second_Factory);
      Via_Gtk_New.Set_Header_Factory (Second_Header_Factory);
      Assert_True
        (Gtk_Signal_List_Item_Factory (Via_Gtk_New.Get_Factory) = Second_Factory);
      Assert_True
        (Gtk_Signal_List_Item_Factory (Via_Gtk_New.Get_Header_Factory) =
           Second_Header_Factory);
      Via_Gtk_New.Set_Factory (null);
      Via_Gtk_New.Set_Header_Factory (null);
      Assert_True (Via_Gtk_New.Get_Factory = null);
      Assert_True (Via_Gtk_New.Get_Header_Factory = null);

      --  Check all ListView-specific property getters after changing values.
      Assert_False (View.Get_Enable_Rubberband);
      View.Set_Enable_Rubberband (True);
      Assert_True (View.Get_Enable_Rubberband);
      View.Set_Enable_Rubberband (False);
      Assert_False (View.Get_Enable_Rubberband);

      Assert_False (View.Get_Show_Separators);
      View.Set_Show_Separators (True);
      Assert_True (View.Get_Show_Separators);
      View.Set_Show_Separators (False);
      Assert_False (View.Get_Show_Separators);

      Assert_False (View.Get_Single_Click_Activate);
      View.Set_Single_Click_Activate (True);
      Assert_True (View.Get_Single_Click_Activate);
      View.Set_Single_Click_Activate (False);
      Assert_False (View.Get_Single_Click_Activate);

      View.Set_Tab_Behavior (List_Tab_Cell);
      Assert_True (View.Get_Tab_Behavior = List_Tab_Cell);
      View.Set_Tab_Behavior (List_Tab_Item);
      Assert_True (View.Get_Tab_Behavior = List_Tab_Item);
      View.Set_Tab_Behavior (List_Tab_All);
      Assert_True (View.Get_Tab_Behavior = List_Tab_All);

      --  Scrolling to an existing position is safe before the view is shown.
      View.Scroll_To
        (1, List_Scroll_None, Gtk.Scroll_Info.Null_Gtk_Scroll_Info);
      Assert_True (Gtk_Single_Selection (-View.Get_Model) = Selection);
   end Test_Constructors_And_Properties;

   -------------------------------------------
   -- Test_Interfaces_And_Accessibility --
   -------------------------------------------

   procedure Test_Interfaces_And_Accessibility is
      View : constant Gtk_List_View :=
        Gtk_List_View_New (Gtk.Selection_Model.Null_Gtk_Selection_Model, null);
      H_Adjustment : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (0.0, 0.0, 100.0, 1.0, 10.0);
      V_Adjustment : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (0.0, 0.0, 100.0, 1.0, 10.0);
      Border : Gtk.Border.Gtk_Border;
      X, Y, Width, Height : Gint;
      Accessible_Id : constant UTF8_String := View.Get_Accessible_Id;
   begin
      --  A list view identifies itself as a list, but has no realized context
      --  or accessible children while it is still an unparented widget.
      Assert_True (View.Get_Accessible_Role = Gtk.Accessible.Accessible_Role_List);
      Assert_True (View.Get_At_Context = null);
      Assert_Cmpuint_Ge (Guint (Accessible_Id'Length), 0);
      Assert_True
        (View.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (View.Get_First_Accessible_Child = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (View.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);

      --  Unallocated widgets have no screen bounds or scrollable border.
      Assert_False (View.Get_Bounds (X, Y, Width, Height));
      Assert_False (View.Get_Border (Border));
      Assert_False
        (View.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focused));

      --  Exercise Accessible update/reset methods with valid values. GtkWidget
      --  keeps its physical hierarchy unchanged when passed null references.
      View.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      View.Update_Next_Accessible_Sibling
        (Gtk.Accessible.Null_Gtk_Accessible);
      View.Announce
        ("List view test",
         Gtk.Accessible.Accessible_Announcement_Priority_Low);
      View.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focusable);
      View.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      View.Reset_Relation (Gtk.Accessible.Accessible_Relation_Described_By);
      View.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      Assert_True
        (View.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (View.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);

      --  GtkListView implements GtkOrientable and GtkScrollable directly.
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

   ----------------------------
   -- Test_List_Base_Bindings --
   ----------------------------

   procedure Test_List_Base_Bindings is
      View : constant Gtk_List_View :=
        Gtk_List_View_New (Gtk.Selection_Model.Null_Gtk_Selection_Model, null);
      --  ListBase is abstract in GTK. Copy the parent Ada record view to keep
      --  calls statically bound to Gtk.List_Base while retaining its C object.
      Base : aliased Gtk.List_Base.Gtk_List_Base_Record :=
        Gtk.List_Base.Gtk_List_Base_Record (View.all);
      H_Adjustment : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (0.0, 0.0, 100.0, 1.0, 10.0);
      V_Adjustment : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (0.0, 0.0, 100.0, 1.0, 10.0);
      Border : Gtk.Border.Gtk_Border;
      X, Y, Width, Height : Gint;
      Accessible_Id : constant UTF8_String :=
        Gtk.List_Base.Get_Accessible_Id (Base'Access);
   begin
      --  A newly constructed list has a list role but no realized accessible
      --  context, parent, children, sibling, or screen bounds.
      Assert_Cmpuint_Ge (Guint (Accessible_Id'Length), 0);
      Assert_True
        (Gtk.List_Base.Get_Accessible_Role (Base'Access) =
           Gtk.Accessible.Accessible_Role_List);
      Assert_True (Gtk.List_Base.Get_At_Context (Base'Access) = null);
      Assert_True
        (Gtk.List_Base.Get_Accessible_Parent (Base'Access) =
           Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Gtk.List_Base.Get_First_Accessible_Child (Base'Access) =
           Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Gtk.List_Base.Get_Next_Accessible_Sibling (Base'Access) =
           Gtk.Accessible.Null_Gtk_Accessible);

      --  Unallocated list widgets have neither accessible bounds nor a border.
      Assert_False
        (Gtk.List_Base.Get_Bounds
           (Base'Access, X, Y, Width, Height));
      Assert_False (Gtk.List_Base.Get_Border (Base'Access, Border));
      Assert_False
        (Gtk.List_Base.Get_Platform_State
           (Base'Access, Gtk.Accessible.Accessible_Platform_State_Focused));

      --  These Accessible operations accept null hierarchy references and
      --  reset the selected accessibility metadata on the list widget.
      Gtk.List_Base.Set_Accessible_Parent
        (Base'Access, Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Gtk.List_Base.Update_Next_Accessible_Sibling
        (Base'Access, Gtk.Accessible.Null_Gtk_Accessible);
      Gtk.List_Base.Announce
        (Base'Access, "List base test",
         Gtk.Accessible.Accessible_Announcement_Priority_Low);
      Gtk.List_Base.Update_Platform_State
        (Base'Access, Gtk.Accessible.Accessible_Platform_State_Focusable);
      Gtk.List_Base.Reset_Property
        (Base'Access, Gtk.Accessible.Accessible_Property_Label);
      Gtk.List_Base.Reset_Relation
        (Base'Access, Gtk.Accessible.Accessible_Relation_Described_By);
      Gtk.List_Base.Reset_State
        (Base'Access, Gtk.Accessible.Accessible_State_Busy);
      Assert_True
        (Gtk.List_Base.Get_Accessible_Parent (Base'Access) =
           Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Gtk.List_Base.Get_Next_Accessible_Sibling (Base'Access) =
           Gtk.Accessible.Null_Gtk_Accessible);

      --  GtkListBase implements GtkOrientable; changing orientation through
      --  its inherited binding must be reflected by the getter.
      Assert_True
        (Gtk.List_Base.Get_Orientation (Base'Access) = Gtk.Enums.Orientation_Vertical);
      Gtk.List_Base.Set_Orientation (Base'Access, Gtk.Enums.Orientation_Horizontal);
      Assert_True
        (Gtk.List_Base.Get_Orientation (Base'Access) = Gtk.Enums.Orientation_Horizontal);
      Gtk.List_Base.Set_Orientation (Base'Access, Gtk.Enums.Orientation_Vertical);

      --  GtkScrollable keeps the supplied adjustments and policies; null
      --  adjustments restore GTK's default adjustment objects.
      Gtk.List_Base.Set_Hscroll_Policy (Base'Access, Gtk.Enums.Scroll_Natural);
      Gtk.List_Base.Set_Vscroll_Policy (Base'Access, Gtk.Enums.Scroll_Natural);
      Assert_True
        (Gtk.List_Base.Get_Hscroll_Policy (Base'Access) = Gtk.Enums.Scroll_Natural);
      Assert_True
        (Gtk.List_Base.Get_Vscroll_Policy (Base'Access) = Gtk.Enums.Scroll_Natural);
      Gtk.List_Base.Set_Hadjustment (Base'Access, H_Adjustment);
      Gtk.List_Base.Set_Vadjustment (Base'Access, V_Adjustment);
      Assert_True (Gtk.List_Base.Get_Hadjustment (Base'Access) = H_Adjustment);
      Assert_True (Gtk.List_Base.Get_Vadjustment (Base'Access) = V_Adjustment);
      Gtk.List_Base.Set_Hadjustment (Base'Access, null);
      Gtk.List_Base.Set_Vadjustment (Base'Access, null);
      Assert_True (Gtk.List_Base.Get_Hadjustment (Base'Access) /= null);
      Assert_True (Gtk.List_Base.Get_Vadjustment (Base'Access) /= null);
   end Test_List_Base_Bindings;

   ---------------------------
   -- Test_Activate_Signal --
   ---------------------------

   procedure Test_Activate_Signal is
      View : constant Gtk_List_View :=
        Gtk_List_View_New (Gtk.Selection_Model.Null_Gtk_Selection_Model, null);
      Slot : Gtk.Label.Gtk_Label;
      Name : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("activate");
   begin
      --  Test both generated overloads by emitting one signal and inspecting
      --  the callback arguments, rather than requiring keyboard or pointer IO.
      Assert_True (Gtk.List_View.Signal_Activate = "activate");
      Gtk.Label.Gtk_New (Slot, "signal slot");
      View.On_Activate (On_Activate'Unrestricted_Access);
      View.On_Activate (On_Slot_Activate'Unrestricted_Access, Slot);
      Emit_Int_Signal (View.Get_Object, Name, 17, System.Null_Address);

      Assert_Cmpuint_Eq (Guint (Signal_Count), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Count), 1);
      Assert_Cmpuint_Eq (Last_Position, 17);

      --  Generated signal marshallers catch Ada exceptions before they cross
      --  the C boundary, and report them through Gtkada's exception hook.
      Gtkada.Bindings.Set_On_Exception
        (Record_Exception'Unrestricted_Access);
      Raise_Callbacks := True;
      Emit_Int_Signal (View.Get_Object, Name, 23, System.Null_Address);
      Raise_Callbacks := False;
      Assert_Cmpuint_Eq (Guint (Signal_Count), 2);
      Assert_Cmpuint_Eq (Guint (Slot_Count), 2);
      Assert_Cmpuint_Eq (Guint (Exception_Count), 2);
      Assert_Cmpuint_Eq (Guint (Unexpected_Exception_Count), 0);
      Assert_Cmpuint_Eq (Last_Position, 23);
      Gtkada.Types.Free (Name);
   end Test_Activate_Signal;

begin
   --  Widget construction needs GTK initialization after the GLib test setup.
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/list-view/constructors-properties",
      Test_Constructors_And_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/list-view/interfaces-accessibility",
      Test_Interfaces_And_Accessibility'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/list-view/list-base-bindings",
      Test_List_Base_Bindings'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/list-view/activate-signal",
      Test_Activate_Signal'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/list-view/list-item-api", Test_List_Item_Api'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end List_View;
