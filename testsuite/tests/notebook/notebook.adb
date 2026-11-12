--  Exercises Gtk.Notebook's page, label, property, accessibility and signal
--  bindings. The page and tab-label checks follow testsuite/c_tests/builder.c
--  (test_notebook), but create the widget directly because GtkBuilder's
--  GtkNotebookPage construction is not available through the current bindings.

with Ada.Command_Line;
with Interfaces.C.Strings;
with System;
with Glib;                use Glib;
with Glib.List_Model;     use Glib.List_Model;
with Glib.Object;         use Glib.Object;
with Glib.Test;           use Glib.Test;
with Gtk.Accessible;      use Gtk.Accessible;
with Gtk.Atcontext;       use Gtk.Atcontext;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Label;           use Gtk.Label;
with Gtk.Main;
with Gtk.Notebook;        use Gtk.Notebook;
with Gtk.Notebook_Page;   use Gtk.Notebook_Page;
with Gtk.Widget;          use Gtk.Widget;

procedure Notebook is
   use Interfaces.C.Strings;
   use type System.Address;

   --  Counters and arguments make the marshaller checks observable.
   Added_Typed, Added_Slot       : Natural := 0;
   Removed_Typed, Removed_Slot   : Natural := 0;
   Reordered_Typed, Reordered_Slot : Natural := 0;
   Switched_Typed, Switched_Slot : Natural := 0;
   Action_Typed, Action_Slot     : Natural := 0;
   Last_Child : Gtk_Widget := null;
   Last_Page  : Guint := 0;

   --  g_signal_emit_by_name is variadic; these declarations expose just the
   --  signatures used below to drive action signals without pointer input.
   --  Each result argument below is a C pointer supplied after the signal's
   --  ordinary varargs, following GLib's g_signal_emit_by_name convention.
   procedure Emit_Int_Result
     (Instance : System.Address; Name : chars_ptr; Arg : Gint;
      Result : System.Address)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Tab_Result
     (Instance : System.Address; Name : chars_ptr; Arg : Gtk_Notebook_Tab;
      Result : System.Address)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Direction
     (Instance : System.Address; Name : chars_ptr; Arg : Gtk_Direction_Type)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Direction_Bool_Result
     (Instance : System.Address; Name : chars_ptr; Arg : Gtk_Direction_Type;
      Flag : Gint; Result : System.Address)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Bool_Result
     (Instance : System.Address; Name : chars_ptr; Flag : Gint;
      Result : System.Address)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Widget_Result
     (Instance : System.Address; Name : chars_ptr; Child : System.Address;
      Result : System.Address)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Child_Page
     (Instance : System.Address; Name : chars_ptr; Child : System.Address;
      Page : Guint)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Action
     (Object : Gtk_Notebook; Name : String; Result : out Gint);

   --  The lifecycle callbacks check the shared child-and-page-number
   --  signature used by page-added, page-removed, page-reordered and switch-page.
   procedure On_Added
     (Self : access Gtk_Notebook_Record'Class;
      Child : not null access Gtk_Widget_Record'Class; Page_Num : Guint);
   procedure On_Added_Slot
     (Self : access GObject_Record'Class;
      Child : not null access Gtk_Widget_Record'Class; Page_Num : Guint);
   procedure On_Removed
     (Self : access Gtk_Notebook_Record'Class;
      Child : not null access Gtk_Widget_Record'Class; Page_Num : Guint);
   procedure On_Removed_Slot
     (Self : access GObject_Record'Class;
      Child : not null access Gtk_Widget_Record'Class; Page_Num : Guint);
   procedure On_Reordered
     (Self : access Gtk_Notebook_Record'Class;
      Child : not null access Gtk_Widget_Record'Class; Page_Num : Guint);
   procedure On_Reordered_Slot
     (Self : access GObject_Record'Class;
      Child : not null access Gtk_Widget_Record'Class; Page_Num : Guint);
   procedure On_Switched
     (Self : access Gtk_Notebook_Record'Class;
      Child : not null access Gtk_Widget_Record'Class; Page_Num : Guint);
   procedure On_Switched_Slot
     (Self : access GObject_Record'Class;
      Child : not null access Gtk_Widget_Record'Class; Page_Num : Guint);

   --  These action callbacks check each distinct signal argument type and
   --  exercise both Gtk.Notebook-specific and GObject slot overloads.
   function On_Change
     (Self : access Gtk_Notebook_Record'Class; Page : Gint) return Boolean;
   function On_Change_Slot
     (Self : access GObject_Record'Class; Page : Gint) return Boolean;
   function On_Create_Window
     (Self : access Gtk_Notebook_Record'Class;
      Page : not null access Gtk_Widget_Record'Class) return Gtk_Notebook;
   function On_Create_Window_Slot
     (Self : access GObject_Record'Class;
      Page : not null access Gtk_Widget_Record'Class) return Gtk_Notebook;
   function On_Focus_Tab
     (Self : access Gtk_Notebook_Record'Class; Tab : Gtk_Notebook_Tab)
      return Boolean;
   function On_Focus_Tab_Slot
     (Self : access GObject_Record'Class; Tab : Gtk_Notebook_Tab)
      return Boolean;
   procedure On_Move_Focus
     (Self : access Gtk_Notebook_Record'Class; Direction : Gtk_Direction_Type);
   procedure On_Move_Focus_Slot
     (Self : access GObject_Record'Class; Direction : Gtk_Direction_Type);
   function On_Reorder_Tab
     (Self : access Gtk_Notebook_Record'Class; Direction : Gtk_Direction_Type;
      Move_To_Last : Boolean) return Boolean;
   function On_Reorder_Tab_Slot
     (Self : access GObject_Record'Class; Direction : Gtk_Direction_Type;
      Move_To_Last : Boolean) return Boolean;
   function On_Select_Page
     (Self : access Gtk_Notebook_Record'Class; Move_Focus : Boolean)
      return Boolean;
   function On_Select_Page_Slot
     (Self : access GObject_Record'Class; Move_Focus : Boolean)
      return Boolean;

   procedure Test_Pages_And_Properties with Convention => C;
   procedure Test_Accessibility with Convention => C;
   procedure Test_Signals with Convention => C;

   --  The wrapper owns child widgets after insertion; these callback aliases
   --  are borrowed and are compared without taking an extra reference.
   --  Page numbers are copied to globals so the test can inspect the callback
   --  arguments after synchronous signal emission returns.
   procedure On_Added
     (Self : access Gtk_Notebook_Record'Class;
      Child : not null access Gtk_Widget_Record'Class; Page_Num : Guint) is
      pragma Unreferenced (Self);
   begin
      Added_Typed := Added_Typed + 1;
      Last_Child := Gtk_Widget (Child);
      Last_Page := Page_Num;
   end On_Added;

   procedure On_Added_Slot
     (Self : access GObject_Record'Class;
      Child : not null access Gtk_Widget_Record'Class; Page_Num : Guint) is
   begin
      --  A slot handler receives the explicit slot object as Self.
      Assert_True (Self /= null);
      Added_Slot := Added_Slot + 1;
      Last_Child := Gtk_Widget (Child);
      Last_Page := Page_Num;
   end On_Added_Slot;

   procedure On_Removed
     (Self : access Gtk_Notebook_Record'Class;
      Child : not null access Gtk_Widget_Record'Class; Page_Num : Guint) is
      pragma Unreferenced (Self);
   begin
      Removed_Typed := Removed_Typed + 1;
      Last_Child := Gtk_Widget (Child);
      Last_Page := Page_Num;
   end On_Removed;

   procedure On_Removed_Slot
     (Self : access GObject_Record'Class;
      Child : not null access Gtk_Widget_Record'Class; Page_Num : Guint) is
   begin
      --  The removed widget is still a valid callback argument during emit.
      Assert_True (Self /= null);
      Removed_Slot := Removed_Slot + 1;
      Last_Child := Gtk_Widget (Child);
      Last_Page := Page_Num;
   end On_Removed_Slot;

   procedure On_Reordered
     (Self : access Gtk_Notebook_Record'Class;
      Child : not null access Gtk_Widget_Record'Class; Page_Num : Guint) is
      pragma Unreferenced (Self);
   begin
      Reordered_Typed := Reordered_Typed + 1;
      Last_Child := Gtk_Widget (Child);
      Last_Page := Page_Num;
   end On_Reordered;

   procedure On_Reordered_Slot
     (Self : access GObject_Record'Class;
      Child : not null access Gtk_Widget_Record'Class; Page_Num : Guint) is
   begin
      --  The page number is the widget's position after the reorder.
      Assert_True (Self /= null);
      Reordered_Slot := Reordered_Slot + 1;
      Last_Child := Gtk_Widget (Child);
      Last_Page := Page_Num;
   end On_Reordered_Slot;

   procedure On_Switched
     (Self : access Gtk_Notebook_Record'Class;
      Child : not null access Gtk_Widget_Record'Class; Page_Num : Guint) is
      pragma Unreferenced (Self);
   begin
      Switched_Typed := Switched_Typed + 1;
      Last_Child := Gtk_Widget (Child);
      Last_Page := Page_Num;
   end On_Switched;

   procedure On_Switched_Slot
     (Self : access GObject_Record'Class;
      Child : not null access Gtk_Widget_Record'Class; Page_Num : Guint) is
   begin
      --  switch-page reports the newly selected page and its current index.
      Assert_True (Self /= null);
      Switched_Slot := Switched_Slot + 1;
      Last_Child := Gtk_Widget (Child);
      Last_Page := Page_Num;
   end On_Switched_Slot;

   --  These action handlers record their arguments and return values so the
   --  generated Boolean and object-return marshallers are exercised.
   function On_Change
     (Self : access Gtk_Notebook_Record'Class; Page : Gint) return Boolean is
      pragma Unreferenced (Self);
   begin
      --  A non-slot callback sees the original GtkNotebook instance.
      Assert_True (Page = 1);
      Action_Typed := Action_Typed + 1;
      return True;
   end On_Change;

   function On_Change_Slot
     (Self : access GObject_Record'Class; Page : Gint) return Boolean is
   begin
      --  The slot signature differs only in the declared Self type.
      Assert_True (Self /= null and then Page = 1);
      Action_Slot := Action_Slot + 1;
      return True;
   end On_Change_Slot;

   function On_Create_Window
     (Self : access Gtk_Notebook_Record'Class;
      Page : not null access Gtk_Widget_Record'Class) return Gtk_Notebook is
      pragma Unreferenced (Self);
   begin
      --  Returning null declines to create a destination notebook window.
      Assert_True (Page /= null);
      Action_Typed := Action_Typed + 1;
      return null;
   end On_Create_Window;

   function On_Create_Window_Slot
     (Self : access GObject_Record'Class;
      Page : not null access Gtk_Widget_Record'Class) return Gtk_Notebook is
   begin
      Assert_True (Self /= null and then Page /= null);
      Action_Slot := Action_Slot + 1;
      return null;
   end On_Create_Window_Slot;

   function On_Focus_Tab
     (Self : access Gtk_Notebook_Record'Class; Tab : Gtk_Notebook_Tab)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      --  The test requests the first tab, one of GtkNotebook's two enum values.
      Assert_True (Tab = Notebook_Tab_First);
      Action_Typed := Action_Typed + 1;
      return False;
   end On_Focus_Tab;

   function On_Focus_Tab_Slot
     (Self : access GObject_Record'Class; Tab : Gtk_Notebook_Tab)
      return Boolean is
   begin
      Assert_True (Self /= null and then Tab = Notebook_Tab_First);
      Action_Slot := Action_Slot + 1;
      return True;
   end On_Focus_Tab_Slot;

   procedure On_Move_Focus
     (Self : access Gtk_Notebook_Record'Class; Direction : Gtk_Direction_Type) is
      pragma Unreferenced (Self);
   begin
      --  Direction values cross the marshaller as GtkDirectionType enums.
      Assert_True (Direction = Dir_Tab_Forward);
      Action_Typed := Action_Typed + 1;
   end On_Move_Focus;

   procedure On_Move_Focus_Slot
     (Self : access GObject_Record'Class; Direction : Gtk_Direction_Type) is
   begin
      Assert_True (Self /= null and then Direction = Dir_Tab_Forward);
      Action_Slot := Action_Slot + 1;
   end On_Move_Focus_Slot;

   function On_Reorder_Tab
     (Self : access Gtk_Notebook_Record'Class; Direction : Gtk_Direction_Type;
      Move_To_Last : Boolean) return Boolean is
      pragma Unreferenced (Self);
   begin
      --  Move_To_Last exercises the Boolean argument after the direction enum.
      Assert_True (Direction = Dir_Left and then Move_To_Last);
      Action_Typed := Action_Typed + 1;
      return False;
   end On_Reorder_Tab;

   function On_Reorder_Tab_Slot
     (Self : access GObject_Record'Class; Direction : Gtk_Direction_Type;
      Move_To_Last : Boolean) return Boolean is
   begin
      Assert_True
        (Self /= null and then Direction = Dir_Left and then Move_To_Last);
      Action_Slot := Action_Slot + 1;
      return True;
   end On_Reorder_Tab_Slot;

   function On_Select_Page
     (Self : access Gtk_Notebook_Record'Class; Move_Focus : Boolean)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      --  This action receives a single gboolean and returns a gboolean.
      Assert_True (Move_Focus);
      Action_Typed := Action_Typed + 1;
      return False;
   end On_Select_Page;

   function On_Select_Page_Slot
     (Self : access GObject_Record'Class; Move_Focus : Boolean)
      return Boolean is
   begin
      Assert_True (Self /= null and then Move_Focus);
      Action_Slot := Action_Slot + 1;
      return True;
   end On_Select_Page_Slot;

   procedure Emit_Action
     (Object : Gtk_Notebook; Name : String; Result : out Gint) is
      C_Name : chars_ptr := New_String (Name);
      Value  : aliased Gint := 0;
   begin
      --  The output pointer is the final vararg in g_signal_emit_by_name.
      if Name = "change-current-page" then
         Emit_Int_Result (Get_Object (Object), C_Name, 1, Value'Address);
      elsif Name = "focus-tab" then
         Emit_Tab_Result
           (Get_Object (Object), C_Name, Notebook_Tab_First, Value'Address);
      elsif Name = "reorder-tab" then
         Emit_Direction_Bool_Result
           (Get_Object (Object), C_Name, Dir_Left, 1, Value'Address);
      else
         Emit_Bool_Result (Get_Object (Object), C_Name, 1, Value'Address);
      end if;
      Free (C_Name);
      Result := Value;
   end Emit_Action;

   procedure Test_Pages_And_Properties is
      N : Gtk_Notebook;
      A, B, C, D : Gtk_Label;
      Tab_A, Tab_B, Tab_C, Tab_D, Tab_A2 : Gtk_Label;
      Menu_B, Menu_C, Menu_A : Gtk_Label;
      Action_Start, Action_End : Gtk_Label;
      Model : Glist_Model;
      Page : Gtk_Notebook_Page;
   begin
      --  Both constructors and Initialize's already-created branch are useful
      --  because Gtk_New and Gtk_Notebook_New share this initialization path.
      --  The empty notebook also documents the no-page sentinel values.
      Gtk_New (N);
      N.Ref_Sink;
      Gtk.Notebook.Initialize (N);
      Assert_True (N.Get_N_Pages = 0 and then N.Get_Current_Page = -1);
      Assert_True (N.Get_Nth_Page (-1) = null);
      Assert_True (N.Get_Nth_Page (0) = null);
      Assert_True (Gtk.Notebook.Get_Type /= 0);

      --  Labels are visible before insertion so GtkNotebook accepts page
      --  selection, preserving GtkNotebook's historical visibility rule.
      A := Gtk_Label_New ("label1");
      B := Gtk_Label_New ("label2");
      C := Gtk_Label_New ("label3");
      D := Gtk_Label_New ("label4");
      Tab_A := Gtk_Label_New ("tab_label1");
      Tab_B := Gtk_Label_New ("tab_label2");
      Tab_C := Gtk_Label_New ("tab_label3");
      Tab_D := Gtk_Label_New ("tab_label4");
      Tab_A2 := Gtk_Label_New ("tab_label1b");
      Menu_B := Gtk_Label_New ("menu_label2");
      Menu_C := Gtk_Label_New ("menu_label3");
      Menu_A := Gtk_Label_New ("menu_label1");
      A.Set_Visible (True);
      B.Set_Visible (True);
      C.Set_Visible (True);
      D.Set_Visible (True);

      --  This reproduces builder.c's two-child order and label assertions,
      --  adding all four page-insertion entry points and a live list model.
      --  The model reference remains valid while page insertions and removals
      --  change its item count.
      Assert_True (N.Append_Page (A, Tab_A) = 0);
      Assert_True (N.Append_Page_Menu (B, Tab_B, Menu_B) = 1);
      Assert_True (N.Prepend_Page_Menu (C, Tab_C, Menu_C) = 0);
      Assert_True (N.Insert_Page (D, Tab_D, 2) = 2);
      Model := N.Get_Pages;
      Assert_True (Get_N_Items (Model) = 4);
      Assert_True (N.Get_N_Pages = 4);
      Assert_True (N.Get_Nth_Page (0) = Gtk_Widget (C));
      Assert_True (N.Get_Nth_Page (1) = Gtk_Widget (A));
      Assert_True (N.Get_Nth_Page (2) = Gtk_Widget (D));
      Assert_True (N.Get_Nth_Page (3) = Gtk_Widget (B));
      Assert_True (N.Get_Nth_Page (-1) = Gtk_Widget (B));
      Assert_True (N.Get_Nth_Page (4) = null);
      Assert_True (N.Page_Num (A) = 1 and then N.Page_Num (B) = 3);
      Assert_True (N.Page_Num (Tab_A2) = -1);
      Assert_True (N.Get_Tab_Label (A) = Gtk_Widget (Tab_A));
      Assert_True (N.Get_Tab_Label_Text (A) = "tab_label1");
      Assert_True (N.Get_Tab_Label_Text (B) = "tab_label2");
      Assert_True (N.Get_Menu_Label (B) = Gtk_Widget (Menu_B));
      Assert_True (N.Get_Menu_Label_Text (B) = "menu_label2");
      Page := N.Get_Page (A);
      Assert_True (Page /= null and then Page.Get_Child = Gtk_Widget (A));

      --  Exercise replacement and string convenience setters independently.
      --  Label text helpers create label widgets internally when called.
      N.Set_Tab_Label_Text (A, "changed tab");
      Assert_True (N.Get_Tab_Label_Text (A) = "changed tab");
      N.Set_Tab_Label (A, Tab_A2);
      Assert_True (N.Get_Tab_Label (A) = Gtk_Widget (Tab_A2));
      N.Set_Menu_Label (A, Menu_A);
      Assert_True (N.Get_Menu_Label (A) = Gtk_Widget (Menu_A));
      N.Set_Menu_Label_Text (A, "changed menu");
      Assert_True (N.Get_Menu_Label_Text (A) = "changed menu");
      N.Set_Tab_Label (A, null);
      Assert_True (N.Get_Tab_Label (A) = null);
      N.Set_Tab_Label (A, Tab_A);

      --  Reordering, selection, page-removal and detach operations update the
      --  same page model, so each result is checked immediately.
      --  The negative page number moves B to the end, as documented.
      N.Reorder_Child (D, 0);
      Assert_True (N.Get_Nth_Page (0) = Gtk_Widget (D));
      N.Reorder_Child (B, -1);
      Assert_True (N.Page_Num (B) = 3);
      N.Set_Current_Page (2);
      Assert_True (N.Get_Current_Page = 2);
      N.Next_Page;
      Assert_True (N.Get_Current_Page = 3);
      N.Next_Page;
      Assert_True (N.Get_Current_Page = 3);
      N.Prev_Page;
      Assert_True (N.Get_Current_Page = 2);
      N.Set_Current_Page (0);
      N.Prev_Page;
      Assert_True (N.Get_Current_Page = 0);
      N.Set_Current_Page (-1);
      Assert_True (N.Get_Current_Page = 3);
      N.Set_Current_Page (99);
      Assert_True (N.Get_Current_Page = 3);
      N.Remove_Page (-1);
      Assert_True (N.Get_N_Pages = 3 and then Get_N_Items (Model) = 3);
      N.Detach_Tab (C);
      Assert_True (N.Page_Num (C) = -1 and then N.Get_N_Pages = 2);

      --  Per-page flags and notebook-wide properties have inverse checks.
      --  These settings are read back before toggling to the opposite value.
      N.Set_Tab_Detachable (A, True);
      Assert_True (N.Get_Tab_Detachable (A));
      N.Set_Tab_Detachable (A, False);
      Assert_True (not N.Get_Tab_Detachable (A));
      N.Set_Tab_Reorderable (A, True);
      Assert_True (N.Get_Tab_Reorderable (A));
      N.Set_Tab_Reorderable (A, False);
      Assert_True (not N.Get_Tab_Reorderable (A));
      N.Set_Scrollable (True);
      Assert_True (N.Get_Scrollable);
      N.Set_Scrollable (False);
      Assert_True (not N.Get_Scrollable);
      N.Set_Show_Border (False);
      Assert_True (not N.Get_Show_Border);
      N.Set_Show_Border (True);
      Assert_True (N.Get_Show_Border);
      N.Set_Show_Tabs (False);
      Assert_True (not N.Get_Show_Tabs);
      N.Set_Show_Tabs (True);
      Assert_True (N.Get_Show_Tabs);
      N.Set_Tab_Pos (Pos_Bottom);
      Assert_True (N.Get_Tab_Pos = Pos_Bottom);
      N.Set_Tab_Pos (Pos_Left);
      Assert_True (N.Get_Tab_Pos = Pos_Left);

      --  Empty and nonempty group names cover both C string conversion paths.
      --  An empty Ada string maps to a null C pointer in the binding.
      N.Set_Group_Name ("shared-tabs");
      Assert_True (N.Get_Group_Name = "shared-tabs");
      N.Set_Group_Name ("");
      Assert_True (N.Get_Group_Name = "");

      --  Action widgets are borrowed from the notebook after assignment.
      --  Pack_Start and Pack_End are separate slots and can hold one widget
      --  each without changing the page order.
      Action_Start := Gtk_Label_New ("start");
      Action_End := Gtk_Label_New ("end");
      Assert_True (N.Get_Action_Widget (Pack_Start) = null);
      Assert_True (N.Get_Action_Widget (Pack_End) = null);
      N.Set_Action_Widget (Action_Start, Pack_Start);
      N.Set_Action_Widget (Action_End, Pack_End);
      Assert_True (N.Get_Action_Widget (Pack_Start) = Gtk_Widget (Action_Start));
      Assert_True (N.Get_Action_Widget (Pack_End) = Gtk_Widget (Action_End));
      N.Popup_Enable;
      N.Popup_Disable;

      Unref (N);
   end Test_Pages_And_Properties;

   procedure Test_Accessibility is
      N : constant Gtk_Notebook := Gtk_Notebook_New;
      X, Y, Width, Height : Gint;
      Bounds : Boolean;
      Context : Gtk_Atcontext;
   begin
      --  Standalone widgets start with a floating reference; sink it before
      --  this test releases the reference at the end.
      N.Ref_Sink;
      --  The notebook itself supplies the group role. Unrooted bounds are
      --  expected to be unavailable in this headless, unmapped test.
      --  Other accessible objects, such as the notebook's internal tab list,
      --  can still appear as children even before a window is presented.
      Assert_True (N.Get_Accessible_Role = Accessible_Role_Group);
      Assert_True (N.Get_Accessible_Parent = Null_Gtk_Accessible);
      Assert_True (N.Get_Accessible_Id = "");
      Bounds := N.Get_Bounds (X, Y, Width, Height);
      Assert_True (not Bounds);
      Assert_True (N.Get_First_Accessible_Child /= Null_Gtk_Accessible);
      Assert_True (N.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  These updates are safe without a platform accessibility backend and
      --  cover the inherited interface forwarding methods on Gtk.Notebook.
      --  The explicit focusable state can then be read through the matching
      --  query, while reset methods accept valid enum values as no-ops.
      N.Reset_Property (Accessible_Property_Label);
      N.Reset_Relation (Accessible_Relation_Labelled_By);
      N.Reset_State (Accessible_State_Selected);
      N.Update_Platform_State (Accessible_Platform_State_Focusable);
      Assert_True
        (N.Get_Platform_State (Accessible_Platform_State_Focusable));
      N.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      N.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      N.Announce ("Notebook ready", Accessible_Announcement_Priority_Low);
      Context := N.Get_At_Context;
      if Context /= null then
         Unref (Context);
      end if;
      Unref (N);
   end Test_Accessibility;

   procedure Test_Signals is
      N : Gtk_Notebook;
      A, B : Gtk_Label;
      Value : Gint;
      Window_Return : aliased System.Address := System.Null_Address;
      Name : chars_ptr;
   begin
      --  The page lifecycle signals use the same child/page-number signature;
      --  both typed and GObject-slot overloads must deliver the arguments.
      --  Register handlers before insertion so page-added and switch-page
      --  events are observed as GtkNotebook updates its internal model.
      N := Gtk_Notebook_New;
      N.Ref_Sink;
      N.On_Page_Added (On_Added'Unrestricted_Access);
      N.On_Page_Added (On_Added_Slot'Unrestricted_Access, Slot => N);
      N.On_Page_Removed (On_Removed'Unrestricted_Access);
      N.On_Page_Removed (On_Removed_Slot'Unrestricted_Access, Slot => N);
      N.On_Page_Reordered (On_Reordered'Unrestricted_Access);
      N.On_Page_Reordered (On_Reordered_Slot'Unrestricted_Access, Slot => N);
      N.On_Switch_Page (On_Switched'Unrestricted_Access);
      N.On_Switch_Page (On_Switched_Slot'Unrestricted_Access, Slot => N);
      A := Gtk_Label_New ("first");
      B := Gtk_Label_New ("second");
      A.Set_Visible (True);
      B.Set_Visible (True);
      Assert_True (N.Append_Page (A, null) = 0);
      Assert_True (Added_Typed = 1 and then Added_Slot = 1);
      Assert_True (Last_Child = Gtk_Widget (A) and then Last_Page = 0);
      Assert_True (N.Append_Page (B, null) = 1);
      N.Set_Current_Page (1);
      Assert_True (Switched_Typed > 0 and then Switched_Slot > 0);
      Assert_True (Last_Child = Gtk_Widget (B) and then Last_Page = 1);
      N.Reorder_Child (B, 0);
      Assert_True (Reordered_Typed = 1 and then Reordered_Slot = 1);
      Assert_True (Last_Child = Gtk_Widget (B) and then Last_Page = 0);
      N.Remove_Page (0);
      Assert_True (Removed_Typed = 1 and then Removed_Slot = 1);
      Assert_True (Last_Child = Gtk_Widget (B) and then Last_Page = 0);

      --  The action signals are not normally emitted by applications. Emitting
      --  them by name tests all generated callback conversion paths directly.
      --  Signals with return values use their native GLib run-last behavior;
      --  counters verify that both Ada handlers received the arguments.
      N.On_Change_Current_Page (On_Change'Unrestricted_Access);
      N.On_Change_Current_Page
        (On_Change_Slot'Unrestricted_Access, Slot => N);
      N.On_Create_Window (On_Create_Window'Unrestricted_Access);
      N.On_Create_Window
        (On_Create_Window_Slot'Unrestricted_Access, Slot => N);
      N.On_Focus_Tab (On_Focus_Tab'Unrestricted_Access);
      N.On_Focus_Tab (On_Focus_Tab_Slot'Unrestricted_Access, Slot => N);
      N.On_Move_Focus_Out (On_Move_Focus'Unrestricted_Access);
      N.On_Move_Focus_Out
        (On_Move_Focus_Slot'Unrestricted_Access, Slot => N);
      N.On_Reorder_Tab (On_Reorder_Tab'Unrestricted_Access);
      N.On_Reorder_Tab (On_Reorder_Tab_Slot'Unrestricted_Access, Slot => N);
      N.On_Select_Page (On_Select_Page'Unrestricted_Access);
      N.On_Select_Page (On_Select_Page_Slot'Unrestricted_Access, Slot => N);

      Action_Typed := 0;
      Action_Slot := 0;
      Emit_Action (N, "change-current-page", Value);
      Assert_True
        (Value = 1 and then Action_Typed = 1 and then Action_Slot = 1);
      --  GtkNotebook's run-last defaults return False here because this
      --  standalone notebook has no focus target or reorderable tab.
      Emit_Action (N, "focus-tab", Value);
      Assert_True
        (Value = 0 and then Action_Typed = 2 and then Action_Slot = 2);

      Name := New_String ("move-focus-out");
      Emit_Direction (Get_Object (N), Name, Dir_Tab_Forward);
      Free (Name);
      Assert_True (Action_Typed = 3 and then Action_Slot = 3);
      Emit_Action (N, "reorder-tab", Value);
      Assert_True
        (Value = 0 and then Action_Typed = 4 and then Action_Slot = 4);
      Emit_Action (N, "select-page", Value);
      Assert_True
        (Value = 0 and then Action_Typed = 5 and then Action_Slot = 5);

      Name := New_String ("create-window");
      Emit_Widget_Result
        (Get_Object (N), Name, Get_Object (A), Window_Return'Address);
      Free (Name);
      Assert_True
        (Window_Return = System.Null_Address
         and then Action_Typed = 6 and then Action_Slot = 6);

      --  These four signals take a child and page number, and exercise the
      --  generic GObject marshaller through the same C emission mechanism.
      --  They are emitted directly after the real lifecycle events so the
      --  generic GObject marshaller is covered for every signal name.
      Name := New_String ("page-added");
      Emit_Child_Page (Get_Object (N), Name, Get_Object (A), 0);
      Free (Name);
      Name := New_String ("page-removed");
      Emit_Child_Page (Get_Object (N), Name, Get_Object (A), 0);
      Free (Name);
      Name := New_String ("page-reordered");
      Emit_Child_Page (Get_Object (N), Name, Get_Object (A), 0);
      Free (Name);
      Name := New_String ("switch-page");
      Emit_Child_Page (Get_Object (N), Name, Get_Object (A), 0);
      Free (Name);
      Assert_True (Added_Typed = 3 and then Added_Slot = 3);
      Assert_True (Removed_Typed = 2 and then Removed_Slot = 2);
      Assert_True (Reordered_Typed = 2 and then Reordered_Slot = 2);
      Assert_True (Switched_Typed > 1 and then Switched_Slot > 1);
      Unref (N);
   end Test_Signals;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/notebook/pages-and-properties",
      Test_Pages_And_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/notebook/accessibility", Test_Accessibility'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/notebook/signals", Test_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Notebook;
