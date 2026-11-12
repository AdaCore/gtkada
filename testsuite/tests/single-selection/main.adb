--  Port of the single-selection cases in testsuite/c_tests/singleselection.c.
--  It also checks the concrete Gtk.Single_Selection wrappers for its three
--  implemented interfaces and the Ada procedure constructor.

with Ada.Command_Line;
with GNAT.Strings;

with Glib;                  use Glib;
with Glib.List_Model;       use Glib.List_Model;
with Glib.Object;           use Glib.Object;
with Glib.Test;             use Glib.Test;
with Gtk.Bitset;            use Gtk.Bitset;
with Gtk.Section_Model;
with Gtk.Selection_Model;
with Gtk.Single_Selection;  use Gtk.Single_Selection;
with Gtk.String_List;       use Gtk.String_List;
with Gtk.String_Object;     use Gtk.String_Object;

procedure Main is

   Items_Position    : Guint := Guint'Last;
   Items_Removed     : Guint := Guint'Last;
   Items_Added       : Guint := Guint'Last;
   Sections_Position : Guint := Guint'Last;
   Sections_Count    : Guint := Guint'Last;
   Selection_Position : Guint := Guint'Last;
   Selection_Count    : Guint := Guint'Last;

   procedure Test_Create_And_Model with Convention => C;
   procedure Test_Selection_Operations with Convention => C;
   procedure Test_Set_Model with Convention => C;
   procedure Test_Interface_Notifications with Convention => C;

   --  Typed signal callbacks store arguments for checks after each helper
   --  emits its notification.  The model interfaces are the types accepted
   --  by GTK's generated signal connection wrappers.
   procedure On_Items_Changed
     (Self : Glist_Model; Position, Removed, Added : Guint);
   procedure On_Sections_Changed
     (Self : Gtk.Section_Model.Gtk_Section_Model;
      Position, N_Items : Guint);
   procedure On_Selection_Changed
     (Self : Gtk.Selection_Model.Gtk_Selection_Model;
      Position, N_Items : Guint);

   --  Build a small model whose items are easy to identify in assertions.
   function New_Store return Gtk_String_List is
      Empty : constant GNAT.Strings.String_List (1 .. 0) := (others => null);
      Store : constant Gtk_String_List := Gtk_String_List_New (Empty);
   begin
      Append (Store, "first");
      Append (Store, "second");
      Append (Store, "third");
      Append (Store, "fourth");
      return Store;
   end New_Store;

   --  Port of /singleselection/create, with the Ada-only procedure
   --  constructor and Initialize's documented already-created behavior.
   procedure Test_Create_And_Model is
      Store : constant Gtk_String_List := New_Store;
      Empty : constant GNAT.Strings.String_List (1 .. 0) := (others => null);
      Empty_Store : constant Gtk_String_List := Gtk_String_List_New (Empty);
      Selection : Gtk_Single_Selection;
      Function_Selection : Gtk_Single_Selection;
      List_View : Glist_Model;
      Section_View : Gtk.Section_Model.Gtk_Section_Model;
      Selection_View : Gtk.Selection_Model.Gtk_Selection_Model;
      Item : GObject;
      Start, Finish : Guint;
   begin
      --  Constructor arguments transfer a model reference, so retain the
      --  local list reference while each selection owns its own reference.
      Ref (Store);
      Gtk_New (Selection, +Store);
      Initialize (Selection, +Empty_Store);
      Assert_True (Get_Model (Selection) = +Store);
      Assert_True (Get_Autoselect (Selection));
      Assert_False (Get_Can_Unselect (Selection));

      --  ListModel accessors return the source item type, count and objects.
      Assert_True (Get_Item_Type (Selection) = Get_Item_Type (Store));
      Assert_Cmpuint_Eq (Get_N_Items (Selection), 4);
      Item := Get_Item (Selection, 1);
      Assert_True (Item /= null);
      Assert_Cmpstr_Eq (Get_String (Gtk_String_Object (Item)), "second");
      Unref (Item);
      Assert_True (Get_Item (Selection, 4) = null);

      --  Autoselect initially chooses the first model item. The selected-item
      --  getter returns a borrowed object, so keep it owned by the model.
      Assert_Cmpuint_Eq (Get_Selected (Selection), 0);
      Item := Get_Selected_Item (Selection);
      Assert_True (Item /= null);
      Assert_Cmpstr_Eq (Get_String (Gtk_String_Object (Item)), "first");

      --  A plain list is one section spanning its complete position domain.
      Get_Section (Selection, 0, Start, Finish);
      Assert_Cmpuint_Eq (Start, 0);
      Assert_Cmpuint_Eq (Finish, Guint'Last);

      --  Check conversions through each interface implemented by the object.
      List_View := +Selection;
      Section_View := +Selection;
      Selection_View := +Selection;
      Assert_True ((-List_View) = Selection);
      Assert_True ((-Section_View) = Selection);
      Assert_True ((-Selection_View) = Selection);

      --  The function constructor also handles an empty model correctly.
      Ref (Empty_Store);
      Function_Selection := Gtk_Single_Selection_New (+Empty_Store);
      Assert_True (Get_Model (Function_Selection) = +Empty_Store);
      Assert_Cmpuint_Eq (Get_N_Items (Function_Selection), 0);
      Assert_Cmpuint_Eq (Get_Selected (Function_Selection), Guint'Last);
      Assert_True (Get_Selected_Item (Function_Selection) = null);

      Unref (Function_Selection);
      Unref (Selection);
      Unref (Empty_Store);
      Unref (Store);
   end Test_Create_And_Model;

   --  Port of /singleselection/selection, /can-unselect, /query-range and
   --  /autoselect-toggle, with assertions over every returned selection view.
   procedure Test_Selection_Operations is
      Store : constant Gtk_String_List := New_Store;
      Selection : Gtk_Single_Selection;
      Current, In_Range, Selected, Mask : Gtk_Bitset;
      Item : GObject;
   begin
      Ref (Store);
      Selection := Gtk_Single_Selection_New (+Store);

      --  Defaults select the first item; turn off autoselect before checking
      --  explicit changes and allow the selected item to be cleared.
      Assert_Cmpuint_Eq (Get_Selected (Selection), 0);
      Set_Autoselect (Selection, False);
      Set_Can_Unselect (Selection, True);
      Assert_False (Get_Autoselect (Selection));
      Assert_True (Get_Can_Unselect (Selection));

      --  Selecting a valid item succeeds and the borrowed selected object has
      --  the expected value.
      Assert_True (Select_Item (Selection, 3, False));
      Assert_Cmpuint_Eq (Get_Selected (Selection), 3);
      Assert_True (Is_Selected (Selection, 3));
      Assert_False (Is_Selected (Selection, 0));
      Item := Get_Selected_Item (Selection);
      Assert_Cmpstr_Eq (Get_String (Gtk_String_Object (Item)), "fourth");

      --  Get_Selection reports exactly one bit; the range query clips that
      --  bit to the requested positions.
      Current := Get_Selection (Selection);
      Assert_Cmpuint_Eq (Guint (Get_Size (Current)), 1);
      Assert_True (Contains (Current, 3));
      Unref (Current);
      In_Range := Get_Selection_In_Range (Selection, 2, 2);
      Assert_True (Contains (In_Range, 3));
      Assert_Cmpuint_Eq (Guint (Get_Size (In_Range)), 1);
      Unref (In_Range);

      --  A multi-item range and Select_All cannot be represented by a single
      --  selection, so they report failure and preserve the current item.
      Assert_False (Select_Range (Selection, 1, 2, False));
      Assert_False (Select_All (Selection));
      Assert_Cmpuint_Eq (Get_Selected (Selection), 3);

      --  GtkSingleSelection does not support the advanced mask operation;
      --  check its rejection and verify that the current bit remains intact.
      Selected := Gtk_Bitset_New_Empty;
      Assert_True (Add (Selected, 1));
      Mask := Gtk_Bitset_New_Range (1, 3);
      Assert_False (Set_Selection (Selection, Selected, Mask));
      Assert_Cmpuint_Eq (Get_Selected (Selection), 3);
      Assert_False (Is_Selected (Selection, 1));
      Assert_True (Is_Selected (Selection, 3));
      Unref (Selected);
      Unref (Mask);

      --  GtkSingleSelection rejects range operations, even for one item.
      Assert_False (Select_Range (Selection, 2, 1, True));
      Assert_Cmpuint_Eq (Get_Selected (Selection), 3);
      Assert_False (Unselect_Range (Selection, 3, 1));
      Assert_Cmpuint_Eq (Get_Selected (Selection), 3);
      Assert_True (Get_Selected_Item (Selection) /= null);

      --  Explicitly selecting then clearing the current position checks both
      --  unselection entry points and the empty-selection bitset.
      Set_Selected (Selection, 0);
      Assert_True (Unselect_Item (Selection, 0));
      Assert_True (Unselect_All (Selection));
      Assert_Cmpuint_Eq (Get_Selected (Selection), Guint'Last);
      Current := Get_Selection (Selection);
      Assert_True (Is_Empty (Current));
      Unref (Current);

      --  Autoselect can be restored and then selects the first item again.
      Set_Autoselect (Selection, True);
      Assert_True (Get_Autoselect (Selection));
      Assert_Cmpuint_Eq (Get_Selected (Selection), 0);
      Set_Selected (Selection, Guint'Last);
      Assert_Cmpuint_Eq (Get_Selected (Selection), 0);

      Unref (Selection);
      Unref (Store);
   end Test_Selection_Operations;

   --  Port of /singleselection/set-model and /empty, including null models and
   --  changing models while autoselect is disabled.
   procedure Test_Set_Model is
      Store : constant Gtk_String_List := New_Store;
      Empty : constant GNAT.Strings.String_List (1 .. 0) := (others => null);
      Other : constant Gtk_String_List := Gtk_String_List_New (Empty);
      Selection : Gtk_Single_Selection;
   begin
      --  Keep the caller's model reference alongside the constructor's.
      Ref (Store);
      Selection := Gtk_Single_Selection_New (+Store);
      --  Set_Model retains its input and updates the visible list properties.
      Set_Model (Selection, +Other);
      Assert_True (Get_Model (Selection) = +Other);
      Assert_Cmpuint_Eq (Get_N_Items (Selection), 0);
      Assert_Cmpuint_Eq (Get_Selected (Selection), Guint'Last);
      Assert_True (Get_Selected_Item (Selection) = null);

      --  A null model represents an empty list, and a later model can replace
      --  it without enabling autoselect implicitly.
      Set_Model (Selection, Null_Glist_Model);
      Assert_True (Get_Model (Selection) = Null_Glist_Model);
      Assert_Cmpuint_Eq (Get_N_Items (Selection), 0);
      Set_Autoselect (Selection, False);
      Set_Model (Selection, +Store);
      Assert_True (Get_Model (Selection) = +Store);
      Assert_Cmpuint_Eq (Get_N_Items (Selection), 4);
      Assert_Cmpuint_Eq (Get_Selected (Selection), Guint'Last);

      --  With autoselect off, an out-of-range selected position clears the
      --  selection; a valid position continues to return its model object.
      Set_Can_Unselect (Selection, True);
      Set_Selected (Selection, 2);
      Assert_Cmpuint_Eq (Get_Selected (Selection), 2);
      Set_Selected (Selection, Guint'Last);
      Assert_Cmpuint_Eq (Get_Selected (Selection), Guint'Last);

      Unref (Selection);
      Unref (Other);
      Unref (Store);
   end Test_Set_Model;

   --  The generated concrete wrappers emit their interface notification
   --  signals.  Typed callbacks let each forwarded argument be asserted.
   procedure Test_Interface_Notifications is
      Store : constant Gtk_String_List := New_Store;
      Selection : Gtk_Single_Selection;
      List_View : Glist_Model;
      Section_View : Gtk.Section_Model.Gtk_Section_Model;
      Selection_View : Gtk.Selection_Model.Gtk_Selection_Model;
   begin
      --  The selection constructor consumes the extra reference supplied here.
      Ref (Store);
      Selection := Gtk_Single_Selection_New (+Store);
      List_View := +Selection;
      Section_View := +Selection;
      Selection_View := +Selection;
      Items_Position := Guint'Last;
      Items_Removed := Guint'Last;
      Items_Added := Guint'Last;
      Sections_Position := Guint'Last;
      Sections_Count := Guint'Last;
      Selection_Position := Guint'Last;
      Selection_Count := Guint'Last;

      --  Connect to the views, then call the concrete Gtk.Single_Selection
      --  helpers whose bodies are the target of this coverage task.
      Glib.List_Model.On_Items_Changed
        (List_View, On_Items_Changed'Unrestricted_Access);
      Gtk.Section_Model.On_Sections_Changed
        (Section_View, On_Sections_Changed'Unrestricted_Access);
      Gtk.Selection_Model.On_Selection_Changed
        (Selection_View, On_Selection_Changed'Unrestricted_Access);

      Items_Changed (Selection, 1, 2, 3);
      Assert_Cmpuint_Eq (Items_Position, 1);
      Assert_Cmpuint_Eq (Items_Removed, 2);
      Assert_Cmpuint_Eq (Items_Added, 3);
      Sections_Changed (Selection, 1, 2);
      Assert_Cmpuint_Eq (Sections_Position, 1);
      Assert_Cmpuint_Eq (Sections_Count, 2);
      Selection_Changed (Selection, 3, 1);
      Assert_Cmpuint_Eq (Selection_Position, 3);
      Assert_Cmpuint_Eq (Selection_Count, 1);

      Unref (Selection);
      Unref (Store);
   end Test_Interface_Notifications;

   --  Save each emitted signal's range for assertions in its test case.
   procedure On_Items_Changed
     (Self : Glist_Model; Position, Removed, Added : Guint)
   is
      pragma Unreferenced (Self);
   begin
      Items_Position := Position;
      Items_Removed := Removed;
      Items_Added := Added;
   end On_Items_Changed;

   procedure On_Sections_Changed
     (Self : Gtk.Section_Model.Gtk_Section_Model;
      Position, N_Items : Guint)
   is
      pragma Unreferenced (Self);
   begin
      Sections_Position := Position;
      Sections_Count := N_Items;
   end On_Sections_Changed;

   procedure On_Selection_Changed
     (Self : Gtk.Selection_Model.Gtk_Selection_Model;
      Position, N_Items : Guint)
   is
      pragma Unreferenced (Self);
   begin
      Selection_Position := Position;
      Selection_Count := N_Items;
   end On_Selection_Changed;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/singleselection/create-model",
      Test_Create_And_Model'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/singleselection/selection-operations",
      Test_Selection_Operations'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/singleselection/set-model", Test_Set_Model'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/singleselection/interface-notifications",
      Test_Interface_Notifications'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
