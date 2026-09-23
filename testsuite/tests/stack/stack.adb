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
   procedure Test_Page_Properties with Convention => C;
   procedure Test_Visible_Child with Convention => C;
   procedure Test_Transitions with Convention => C;
   procedure Test_Pages_Model with Convention => C;
   procedure Test_Remove with Convention => C;
   procedure Test_Switcher_Stack with Convention => C;
   procedure Test_Sidebar_Stack with Convention => C;

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

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

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

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Stack;
