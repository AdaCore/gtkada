--  GTK's own testsuite has no stack.c, so this test is written from scratch
--  over the whole of Gtk.Stack, Gtk.Stack_Page, Gtk.Stack_Switcher and
--  Gtk.Stack_Sidebar.

with Ada.Command_Line;
with Glib;              use Glib;
with Glib.List_Model;   use Glib.List_Model;
with Glib.Object;       use Glib.Object;
with Glib.Test;         use Glib.Test;
with Glib.Types;
with Gtk.Button;        use Gtk.Button;
with Gtk.Accessible;    use Gtk.Accessible;
with Gtk.Atcontext;
use type Gtk.Atcontext.Gtk_Atcontext;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Label;         use Gtk.Label;
with Gtk.Main;
with Gtk.Selection_Model;
with Gtk.Stack;         use Gtk.Stack;
with Gtk.Stack_Page;    use Gtk.Stack_Page;
with Gtk.Stack_Sidebar; use Gtk.Stack_Sidebar;
with Gtk.Stack_Switcher; use Gtk.Stack_Switcher;
with Gtk.Widget;        use Gtk.Widget;

procedure Stack is

   procedure Test_Add with Convention => C;
   procedure Test_Constructor with Convention => C;
   procedure Test_Page_Properties with Convention => C;
   procedure Test_Visible_Child with Convention => C;
   procedure Test_Transitions with Convention => C;
   procedure Test_Pages_Model with Convention => C;
   procedure Test_Remove with Convention => C;
   procedure Test_Switcher_Stack with Convention => C;
   procedure Test_Sidebar_Stack with Convention => C;
   procedure Test_Accessibility with Convention => C;

   function N_Pages (Self : not null access Gtk_Stack_Record'Class)
      return Guint;
   --  The number of pages of Self, counted through the transfer-full model
   --  returned by Get_Pages, which is dropped again before returning.

   -------------
   -- N_Pages --
   -------------

   function N_Pages (Self : not null access Gtk_Stack_Record'Class)
      return Guint
   is
      Model : constant Gtk.Selection_Model.Gtk_Selection_Model :=
        Self.Get_Pages;
      Count : constant Guint :=
        Get_N_Items (Glist_Model (Glib.Types.GType_Interface (Model)));
   begin
      Unref (Glib.Types.To_Object (Glib.Types.GType_Interface (Model)));
      return Count;
   end N_Pages;

   ---------------------
   -- Test_Constructor --
   ---------------------

   --  Check the procedure constructor as well as the function form used by
   --  the rest of this test file.
   procedure Test_Constructor is
      Stack : Gtk_Stack;
      Sidebar : Gtk_Stack_Sidebar;
      Switcher : Gtk_Stack_Switcher;
   begin
      Gtk_New (Stack);
      Assert_True (Stack /= null);
      Assert_True (Stack.Get_Visible_Child = null);

      --  Exercise the procedure form of GtkStackSidebar's constructor too.
      Gtk_New (Sidebar);
      Assert_True (Sidebar /= null);
      Assert_True (Sidebar.Get_Stack = null);

      --  Exercise GtkStackSwitcher's procedure constructor as well as the
      --  function constructor used in the stack-controller test.
      Gtk_New (Switcher);
      Assert_True (Switcher /= null);
      Assert_True (Switcher.Get_Stack = null);

      --  Gtk_New returns a floating widget reference, like Gtk_Stack_New.
      Ref_Sink (Stack);
      Ref_Sink (Sidebar);
      Ref_Sink (Switcher);
      Unref (Stack);
      Unref (Sidebar);
      Unref (Switcher);
   end Test_Constructor;

   --------------
   -- Test_Add --
   --------------

   procedure Test_Add is
      Stack  : constant Gtk_Stack := Gtk_Stack_New;
      Plain  : constant Gtk_Label := Gtk_Label_New ("plain");
      Named  : constant Gtk_Label := Gtk_Label_New ("named");
      Titled : constant Gtk_Label := Gtk_Label_New ("titled");

      P1, P2, P3 : Gtk_Stack_Page;
   begin
      Ref_Sink (Stack);

      P1 := Stack.Add_Child (Plain);
      P2 := Stack.Add_Named (Named, "named");
      P3 := Stack.Add_Titled (Titled, "titled", "Titled");

      Assert_True (P1 /= null and then P2 /= null and then P3 /= null);

      --  Get_Page hands back the very page object the adder returned.
      Assert_True (Stack.Get_Page (Plain) = P1);
      Assert_True (Stack.Get_Page (Named) = P2);
      Assert_True (Stack.Get_Page (Titled) = P3);

      Assert_True (Stack.Get_Child_By_Name ("named") = Gtk_Widget (Named));
      Assert_True (Stack.Get_Child_By_Name ("titled") = Gtk_Widget (Titled));
      Assert_True (Stack.Get_Child_By_Name ("no-such-child") = null);

      Unref (Stack);
   end Test_Add;

   ---------------------------
   -- Test_Page_Properties --
   ---------------------------

   procedure Test_Page_Properties is
      Stack : constant Gtk_Stack := Gtk_Stack_New;
      Child : constant Gtk_Button := Gtk_Button_New_With_Label ("child");
      Page  : Gtk_Stack_Page;
   begin
      Ref_Sink (Stack);

      Page := Stack.Add_Titled (Child, "one", "One");

      Assert_True (Page.Get_Child = Gtk_Widget (Child));
      Assert_True (Page.Get_Name = "one");
      Assert_True (Page.Get_Title = "One");

      Page.Set_Name ("first");
      Assert_True (Page.Get_Name = "first");
      Assert_True (Stack.Get_Child_By_Name ("first") = Gtk_Widget (Child));

      Page.Set_Title ("First");
      Assert_True (Page.Get_Title = "First");

      Page.Set_Icon_Name ("edit-find");
      Assert_True (Page.Get_Icon_Name = "edit-find");

      Assert_False (Page.Get_Needs_Attention);
      Page.Set_Needs_Attention (True);
      Assert_True (Page.Get_Needs_Attention);

      Assert_False (Page.Get_Use_Underline);
      Page.Set_Use_Underline (True);
      Assert_True (Page.Get_Use_Underline);

      Assert_True (Page.Get_Visible);
      Page.Set_Visible (False);
      Assert_False (Page.Get_Visible);
      Page.Set_Visible (True);

      Unref (Stack);
   end Test_Page_Properties;

   -------------------------
   -- Test_Visible_Child --
   -------------------------

   procedure Test_Visible_Child is
      Stack : constant Gtk_Stack := Gtk_Stack_New;
      One   : constant Gtk_Label := Gtk_Label_New ("one");
      Two   : constant Gtk_Label := Gtk_Label_New ("two");
      Three : constant Gtk_Label := Gtk_Label_New ("three");
      Page  : Gtk_Stack_Page with Unreferenced;
   begin
      Ref_Sink (Stack);

      Page := Stack.Add_Titled (One, "one", "One");
      Page := Stack.Add_Titled (Two, "two", "Two");
      Page := Stack.Add_Titled (Three, "three", "Three");

      --  The first child added becomes visible on its own.
      Assert_True (Stack.Get_Visible_Child = Gtk_Widget (One));
      Assert_True (Stack.Get_Visible_Child_Name = "one");

      Stack.Set_Visible_Child (Two);
      Assert_True (Stack.Get_Visible_Child = Gtk_Widget (Two));
      Assert_True (Stack.Get_Visible_Child_Name = "two");

      Stack.Set_Visible_Child_Name ("three");
      Assert_True (Stack.Get_Visible_Child = Gtk_Widget (Three));
      Assert_True (Stack.Get_Visible_Child_Name = "three");

      Stack.Set_Visible_Child_Full
        ("one", Stack_Transition_Type_Slide_Left);
      Assert_True (Stack.Get_Visible_Child = Gtk_Widget (One));
      Assert_True (Stack.Get_Visible_Child_Name = "one");

      Unref (Stack);
   end Test_Visible_Child;

   ----------------------
   -- Test_Transitions --
   ----------------------

   procedure Test_Transitions is
      Stack : constant Gtk_Stack := Gtk_Stack_New;
   begin
      Ref_Sink (Stack);

      --  An empty stack cannot be partway through a page transition.
      Assert_False (Stack.Get_Transition_Running);

      Assert_True (Stack.Get_Transition_Type = Stack_Transition_Type_None);
      Stack.Set_Transition_Type (Stack_Transition_Type_Crossfade);
      Assert_True
        (Stack.Get_Transition_Type = Stack_Transition_Type_Crossfade);
      Stack.Set_Transition_Type (Stack_Transition_Type_Rotate_Left_Right);
      Assert_True
        (Stack.Get_Transition_Type
         = Stack_Transition_Type_Rotate_Left_Right);

      Stack.Set_Transition_Duration (1_500);
      Assert_Cmpuint_Eq (Stack.Get_Transition_Duration, 1_500);

      --  Hhomogeneous and Vhomogeneous both default to True, the rest to
      --  False.
      Assert_True (Stack.Get_Hhomogeneous);
      Stack.Set_Hhomogeneous (False);
      Assert_False (Stack.Get_Hhomogeneous);

      Assert_True (Stack.Get_Vhomogeneous);
      Stack.Set_Vhomogeneous (False);
      Assert_False (Stack.Get_Vhomogeneous);

      Assert_False (Stack.Get_Interpolate_Size);
      Stack.Set_Interpolate_Size (True);
      Assert_True (Stack.Get_Interpolate_Size);

      Unref (Stack);
   end Test_Transitions;

   ----------------------
   -- Test_Pages_Model --
   ----------------------

   procedure Test_Pages_Model is
      Stack : constant Gtk_Stack := Gtk_Stack_New;
      One   : constant Gtk_Label := Gtk_Label_New ("one");
      Two   : constant Gtk_Label := Gtk_Label_New ("two");
      Model : Gtk.Selection_Model.Gtk_Selection_Model;
      Page  : Gtk_Stack_Page with Unreferenced;
   begin
      Ref_Sink (Stack);

      Assert_Cmpuint_Eq (N_Pages (Stack), 0);

      --  The model is live: it is taken before the children are added and
      --  still tracks them.
      Model := Stack.Get_Pages;

      Page := Stack.Add_Titled (One, "one", "One");
      Page := Stack.Add_Titled (Two, "two", "Two");

      Assert_Cmpuint_Eq
        (Get_N_Items (Glist_Model (Glib.Types.GType_Interface (Model))), 2);
      Assert_Cmpuint_Eq (N_Pages (Stack), 2);

      Stack.Remove (One);
      Assert_Cmpuint_Eq
        (Get_N_Items (Glist_Model (Glib.Types.GType_Interface (Model))), 1);

      --  Get_Pages is transfer-full, so the reference taken above is ours.
      Unref (Glib.Types.To_Object (Glib.Types.GType_Interface (Model)));

      Unref (Stack);
   end Test_Pages_Model;

   -----------------
   -- Test_Remove --
   -----------------

   procedure Test_Remove is
      Stack : constant Gtk_Stack := Gtk_Stack_New;
      One   : constant Gtk_Label := Gtk_Label_New ("one");
      Two   : constant Gtk_Label := Gtk_Label_New ("two");
      Page  : Gtk_Stack_Page with Unreferenced;
   begin
      Ref_Sink (Stack);

      Page := Stack.Add_Titled (One, "one", "One");
      Page := Stack.Add_Titled (Two, "two", "Two");
      Assert_Cmpuint_Eq (N_Pages (Stack), 2);

      Stack.Remove (One);

      Assert_True (Stack.Get_Child_By_Name ("one") = null);
      Assert_True (Stack.Get_Child_By_Name ("two") = Gtk_Widget (Two));
      Assert_Cmpuint_Eq (N_Pages (Stack), 1);

      --  Removing the visible child leaves the stack with none rather than
      --  promoting the survivor, so it has to be named again explicitly.
      Assert_True (Stack.Get_Visible_Child = null);
      Assert_True (Stack.Get_Visible_Child_Name = "");

      Stack.Set_Visible_Child_Name ("two");
      Assert_True (Stack.Get_Visible_Child = Gtk_Widget (Two));

      Stack.Remove (Two);
      Assert_Cmpuint_Eq (N_Pages (Stack), 0);
      Assert_True (Stack.Get_Visible_Child = null);

      Unref (Stack);
   end Test_Remove;

   --------------------------
   -- Test_Switcher_Stack --
   --------------------------

   procedure Test_Switcher_Stack is
      Switcher : constant Gtk_Stack_Switcher := Gtk_Stack_Switcher_New;
      Stack    : constant Gtk_Stack := Gtk_Stack_New;
   begin
      Ref_Sink (Switcher);
      Ref_Sink (Stack);

      Assert_True (Switcher.Get_Stack = null);

      Switcher.Set_Stack (Stack);
      Assert_True (Switcher.Get_Stack = Stack);

      Switcher.Set_Stack (null);
      Assert_True (Switcher.Get_Stack = null);

      --  The switcher is the only one of the three that is Gtk.Orientable.
      Assert_True (Switcher.Get_Orientation = Orientation_Horizontal);
      Switcher.Set_Orientation (Orientation_Vertical);
      Assert_True (Switcher.Get_Orientation = Orientation_Vertical);

      Unref (Switcher);
      Unref (Stack);
   end Test_Switcher_Stack;

   -------------------------
   -- Test_Sidebar_Stack --
   -------------------------

   procedure Test_Sidebar_Stack is
      Sidebar : constant Gtk_Stack_Sidebar := Gtk_Stack_Sidebar_New;
      Stack   : constant Gtk_Stack := Gtk_Stack_New;
   begin
      Ref_Sink (Sidebar);
      Ref_Sink (Stack);

      Assert_True (Sidebar.Get_Stack = null);

      --  Unlike the switcher's, the sidebar's Set_Stack is not nullable, so
      --  there is no clearing half to this round trip.
      Sidebar.Set_Stack (Stack);
      Assert_True (Sidebar.Get_Stack = Stack);

      Unref (Sidebar);
      Unref (Stack);
   end Test_Sidebar_Stack;

   -------------------------
   -- Test_Accessibility --
   -------------------------

   --  A detached stack has deterministic accessible metadata and no screen
   --  geometry or AT context, so these assertions do not need a real window.
   procedure Test_Accessibility is
      Stack  : constant Gtk_Stack := Gtk_Stack_New;
      Child  : constant Gtk_Label := Gtk_Label_New ("accessible page");
      Sidebar : constant Gtk_Stack_Sidebar := Gtk_Stack_Sidebar_New;
      Switcher : constant Gtk_Stack_Switcher := Gtk_Stack_Switcher_New;
      Page   : Gtk_Stack_Page;
      Parent : Gtk_Accessible;
      Next   : Gtk_Accessible;
      First  : Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
   begin
      Ref_Sink (Stack);
      Ref_Sink (Child);
      Ref_Sink (Sidebar);
      Ref_Sink (Switcher);
      Page := Stack.Add_Child (Child);
      Assert_True (Page /= null);

      --  GtkStack is exposed as an accessible group and has an unnamed ID
      --  until an application assigns one.
      Assert_True (Stack.Get_Accessible_Role = Accessible_Role_Group);
      Assert_Cmpstr_Eq (Stack.Get_Accessible_Id, "");
      Parent := Stack.Get_Accessible_Parent;
      Assert_True (Parent = Null_Gtk_Accessible);
      Next := Stack.Get_Next_Accessible_Sibling;
      Assert_True (Next = Null_Gtk_Accessible);
      First := Stack.Get_First_Accessible_Child;
      Assert_True (First /= Null_Gtk_Accessible);

      --  Before the widget is rooted, it has no accessibility backend data.
      Context := Stack.Get_At_Context;
      Assert_True (Context = null);
      Assert_False (Stack.Get_Bounds (X, Y, Width, Height));
      Assert_False
        (Stack.Get_Platform_State (Accessible_Platform_State_Focused));

      --  These inherited updates accept valid enum values. Null tree updates
      --  keep this detached stack without an accessible parent or sibling.
      Stack.Announce ("stack test", Accessible_Announcement_Priority_Low);
      Stack.Reset_Property (Accessible_Property_Label);
      Stack.Reset_Relation (Accessible_Relation_Labelled_By);
      Stack.Reset_State (Accessible_State_Busy);
      Stack.Update_Platform_State (Accessible_Platform_State_Focusable);
      Stack.Set_Accessible_Parent
        (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Stack.Get_Accessible_Parent = Null_Gtk_Accessible);
      Stack.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (Stack.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  GtkStackPage also implements GtkAccessible. With one page, its
      --  accessible parent and child are the stack and its page widget.
      Assert_Cmpstr_Eq (Page.Get_Accessible_Id, "");
      Assert_True (Page.Get_Accessible_Role /= Accessible_Role_None);
      Parent := Page.Get_Accessible_Parent;
      Assert_True (Parent = +Stack);
      Next := Page.Get_Next_Accessible_Sibling;
      Assert_True (Next = Null_Gtk_Accessible);
      First := Page.Get_First_Accessible_Child;
      Assert_True (First = +Child);

      --  The page is not rooted in a window, so it has no accessibility
      --  backend context, screen bounds, or focused platform state.
      Context := Page.Get_At_Context;
      Assert_True (Context = null);
      Assert_False (Page.Get_Bounds (X, Y, Width, Height));
      Assert_False
        (Page.Get_Platform_State (Accessible_Platform_State_Focused));

      --  Exercise the inherited accessibility operations on the page itself.
      Page.Announce ("page test", Accessible_Announcement_Priority_Low);
      Page.Reset_Property (Accessible_Property_Label);
      Page.Reset_Relation (Accessible_Relation_Labelled_By);
      Page.Reset_State (Accessible_State_Busy);
      Page.Update_Platform_State (Accessible_Platform_State_Focusable);
      Page.Set_Accessible_Parent
        (Null_Gtk_Accessible, Null_Gtk_Accessible);
      --  The owning stack continues to manage the page's accessible parent.
      Assert_True (Page.Get_Accessible_Parent = +Stack);
      Page.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (Page.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  GtkStackSidebar exposes the same GtkAccessible interface. Connecting
      --  it to a one-page stack creates a stable accessible child without
      --  rooting either widget in a window.
      Sidebar.Set_Stack (Stack);
      Assert_True (Sidebar.Get_Stack = Stack);
      Assert_True (Sidebar.Get_Accessible_Role /= Accessible_Role_None);
      Assert_Cmpstr_Eq (Sidebar.Get_Accessible_Id, "");
      Parent := Sidebar.Get_Accessible_Parent;
      Assert_True (Parent = Null_Gtk_Accessible);
      Next := Sidebar.Get_Next_Accessible_Sibling;
      Assert_True (Next = Null_Gtk_Accessible);
      First := Sidebar.Get_First_Accessible_Child;
      Assert_True (First /= Null_Gtk_Accessible);

      --  An unrooted sidebar has neither an AT context nor screen geometry.
      Context := Sidebar.Get_At_Context;
      Assert_True (Context = null);
      Assert_False (Sidebar.Get_Bounds (X, Y, Width, Height));
      Assert_False
        (Sidebar.Get_Platform_State (Accessible_Platform_State_Focused));

      --  Exercise inherited accessibility operations and verify that null
      --  tree updates leave the detached sidebar without parent or sibling.
      Sidebar.Announce
        ("sidebar test", Accessible_Announcement_Priority_Low);
      Sidebar.Reset_Property (Accessible_Property_Label);
      Sidebar.Reset_Relation (Accessible_Relation_Labelled_By);
      Sidebar.Reset_State (Accessible_State_Busy);
      Sidebar.Update_Platform_State (Accessible_Platform_State_Focusable);
      Sidebar.Set_Accessible_Parent
        (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Sidebar.Get_Accessible_Parent = Null_Gtk_Accessible);
      Sidebar.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True
        (Sidebar.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  StackSwitcher also implements GtkAccessible. Its tab-list role and
      --  generated accessible child are available with this detached stack.
      Switcher.Set_Stack (Stack);
      Assert_True (Switcher.Get_Stack = Stack);
      Assert_True
        (Switcher.Get_Accessible_Role = Accessible_Role_Tab_List);
      Assert_Cmpstr_Eq (Switcher.Get_Accessible_Id, "");
      Parent := Switcher.Get_Accessible_Parent;
      Assert_True (Parent = Null_Gtk_Accessible);
      Next := Switcher.Get_Next_Accessible_Sibling;
      Assert_True (Next = Null_Gtk_Accessible);
      First := Switcher.Get_First_Accessible_Child;
      Assert_True (First /= Null_Gtk_Accessible);

      --  Without a root widget there is no AT context, screen bounds, or
      --  focused platform state for the switcher.
      Context := Switcher.Get_At_Context;
      Assert_True (Context = null);
      Assert_False (Switcher.Get_Bounds (X, Y, Width, Height));
      Assert_False
        (Switcher.Get_Platform_State (Accessible_Platform_State_Focused));

      --  Exercise inherited accessible mutations and verify the null tree
      --  updates leave the switcher without an explicit parent or sibling.
      Switcher.Announce
        ("switcher test", Accessible_Announcement_Priority_Low);
      Switcher.Reset_Property (Accessible_Property_Label);
      Switcher.Reset_Relation (Accessible_Relation_Labelled_By);
      Switcher.Reset_State (Accessible_State_Busy);
      Switcher.Update_Platform_State (Accessible_Platform_State_Focusable);
      Switcher.Set_Accessible_Parent
        (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True
        (Switcher.Get_Accessible_Parent = Null_Gtk_Accessible);
      Switcher.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True
        (Switcher.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      Stack.Remove (Child);
      Unref (Child);
      Unref (Switcher);
      Unref (Sidebar);
      Unref (Stack);
   end Test_Accessibility;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/stack/constructor", Test_Constructor'Unrestricted_Access);
   Glib.Test.Add_Func ("/stack/add", Test_Add'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/stack/page-properties", Test_Page_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/stack/visible-child", Test_Visible_Child'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/stack/transitions", Test_Transitions'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/stack/pages-model", Test_Pages_Model'Unrestricted_Access);
   Glib.Test.Add_Func ("/stack/remove", Test_Remove'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/stackswitcher/stack", Test_Switcher_Stack'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/stacksidebar/stack", Test_Sidebar_Stack'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/stack/accessibility", Test_Accessibility'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Stack;
