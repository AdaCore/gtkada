--  GTK has no testsuite/gtk/shortcutcontroller.c to port, so these cases are
--  ours: they cover the surface the Gtk.Shortcut_Controller binding adds,
--  together with the Gtk.Shortcut, trigger and action packages it consumes.
--
--  Gtk_Shortcut_New and Add_Shortcut take ownership of what they are given,
--  so none of it is unreferenced here.

with Ada.Command_Line;
with Gdk.Enums;              use Gdk.Enums;
with Glib;                   use Glib;
with Glib.List_Model;        use Glib.List_Model;
with Glib.List_Store;        use Glib.List_Store;
with Glib.Object;            use Glib.Object;
with Glib.Test;              use Glib.Test;
with Glib.Variant;           use Glib.Variant;
with Gtk.Callback_Action;    use Gtk.Callback_Action;
with Gtk.Event_Controller;   use Gtk.Event_Controller;
with Gtk.Main;
with Gtk.Search_Entry;       use Gtk.Search_Entry;
with Gtk.Shortcut;           use Gtk.Shortcut;
with Gtk.Shortcut_Action;    use Gtk.Shortcut_Action;
with Gtk.Shortcut_Controller; use Gtk.Shortcut_Controller;
with Gtk.Shortcut_Trigger;   use Gtk.Shortcut_Trigger;
with Gtk.Widget;             use Gtk.Widget;

procedure Shortcut_Controller is

   Stopped : Natural := 0;
   --  Incremented by the "stop-search" handler

   procedure On_Stop_Search (Self : access Gtk_Search_Entry_Record'Class);

   procedure Test_Defaults with Convention => C;
   procedure Test_Scope with Convention => C;
   procedure Test_Shortcuts with Convention => C;
   procedure Test_Model with Convention => C;
   procedure Test_Trigger_Action with Convention => C;
   procedure Test_Widget with Convention => C;
   procedure Test_Callback_Action with Convention => C;

   Seen_Data : Integer := 0;
   Seen_Widget : Gtk_Widget;
   --  What the callback action's function was last handed

   function Callback
     (Widget : Gtk_Widget; Args : Gvariant; Data : Integer) return Gboolean;

   package Action_With_Data is new Callback_Action_With_Data (Integer);

   function New_Shortcut (Trigger, Action : String) return Gtk_Shortcut;
   --  A shortcut for the given trigger and action strings

   --------------------
   -- On_Stop_Search --
   --------------------

   procedure On_Stop_Search (Self : access Gtk_Search_Entry_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Stopped := Stopped + 1;
   end On_Stop_Search;

   ------------------
   -- New_Shortcut --
   ------------------

   function New_Shortcut (Trigger, Action : String) return Gtk_Shortcut is
      T : Gtk_Shortcut_Trigger;
      A : Gtk_Shortcut_Action;
   begin
      Gtk_New (T, Trigger);
      Gtk_New (A, Action);
      return Gtk_Shortcut_New (T, A);
   end New_Shortcut;

   -------------------
   -- Test_Defaults --
   -------------------

   procedure Test_Defaults is
      C : constant Gtk_Shortcut_Controller := Gtk_Shortcut_Controller_New;
   begin
      Assert_True (C.Get_Scope = Local);
      Assert_True (C.Get_Mnemonics_Modifiers = Gdk_Alt_Mask);
      Assert_Cmpuint_Eq (C.Get_N_Items, 0);
      --  GTK types the model as plain objects, not as shortcuts.
      Assert_True (C.Get_Item_Type = Glib.GType_Object);

      Unref (C);
   end Test_Defaults;

   ----------------
   -- Test_Scope --
   ----------------

   procedure Test_Scope is
      C : constant Gtk_Shortcut_Controller := Gtk_Shortcut_Controller_New;
   begin
      --  Every value must survive the trip through the C enum.
      for S in Gtk_Shortcut_Scope loop
         C.Set_Scope (S);
         Assert_True (C.Get_Scope = S);
      end loop;

      C.Set_Mnemonics_Modifiers (Gdk_Control_Mask);
      Assert_True (C.Get_Mnemonics_Modifiers = Gdk_Control_Mask);

      Unref (C);
   end Test_Scope;

   --------------------
   -- Test_Shortcuts --
   --------------------

   procedure Test_Shortcuts is
      C  : constant Gtk_Shortcut_Controller := Gtk_Shortcut_Controller_New;
      S1 : constant Gtk_Shortcut := New_Shortcut ("<Control>q", "nothing");
      S2 : constant Gtk_Shortcut := New_Shortcut ("<Control>w", "nothing");
   begin
      --  Add_Shortcut consumes a reference and Remove_Shortcut drops the
      --  controller's; hold one more on S1 so that it outlives the removals.
      Ref (S1);

      C.Add_Shortcut (S1);
      Assert_Cmpuint_Eq (C.Get_N_Items, 1);
      C.Add_Shortcut (S2);
      Assert_Cmpuint_Eq (C.Get_N_Items, 2);

      C.Remove_Shortcut (S1);
      Assert_Cmpuint_Eq (C.Get_N_Items, 1);

      --  Removing what is not there does nothing.
      C.Remove_Shortcut (S1);
      Assert_Cmpuint_Eq (C.Get_N_Items, 1);

      Unref (S1);
      Unref (C);
   end Test_Shortcuts;

   ----------------
   -- Test_Model --
   ----------------

   procedure Test_Model is
      Store : constant Glist_Store := Glist_Store_New (Gtk.Shortcut.Get_Type);
      C     : Gtk_Shortcut_Controller;
   begin
      Store.Append (New_Shortcut ("<Control>a", "nothing"));
      Store.Append (New_Shortcut ("<Control>b", "nothing"));

      C := Gtk_Shortcut_Controller_New_For_Model (+Store);
      Assert_Cmpuint_Eq (C.Get_N_Items, 2);

      --  A controller over an external list neither adds nor removes.
      C.Add_Shortcut (New_Shortcut ("<Control>c", "nothing"));
      Assert_Cmpuint_Eq (C.Get_N_Items, 2);

      Unref (C);
      Unref (Store);
   end Test_Model;

   -------------------------
   -- Test_Trigger_Action --
   -------------------------

   procedure Test_Trigger_Action is
      Entry_1 : constant Gtk_Search_Entry := Gtk_Search_Entry_New;
      Action  : Gtk_Shortcut_Action;
      Trigger : Gtk_Shortcut_Trigger;
   begin
      Ref_Sink (Entry_1);

      Gtk_New (Trigger, "<Control>q");
      Assert_Cmpstr_Eq (Trigger.To_String, "<Control>q");

      --  "stop-search" is an action signal of the search entry, so this
      --  activates synchronously, without a key event or a main loop.
      Entry_1.On_Stop_Search (On_Stop_Search'Unrestricted_Access);
      Gtk_New (Action, "signal(stop-search)");
      Assert_Cmpstr_Eq (Action.To_String, "signal(stop-search)");

      Stopped := 0;
      Assert_True (Action.Activate (0, Entry_1, Null_Gvariant));
      Assert_Cmpint_Eq (Gint (Stopped), 1);

      --  "nothing" does nothing. Whether GTK reports that as success is
      --  its own business, so the result is not looked at.
      Gtk_New (Action, "nothing");
      declare
         Ignored : constant Boolean :=
           Action.Activate (0, Entry_1, Null_Gvariant);
         pragma Unreferenced (Ignored);
      begin
         Assert_Cmpint_Eq (Gint (Stopped), 1);
      end;

      Unref (Entry_1);
   end Test_Trigger_Action;

   --------------
   -- Callback --
   --------------

   function Callback
     (Widget : Gtk_Widget; Args : Gvariant; Data : Integer) return Gboolean
   is
      pragma Unreferenced (Args);
   begin
      Seen_Widget := Widget;
      Seen_Data := Data;
      return 1;
   end Callback;

   -------------------------
   -- Test_Callback_Action --
   -------------------------

   procedure Test_Callback_Action is
      W : constant Gtk_Search_Entry := Gtk_Search_Entry_New;
      A : Gtk_Callback_Action;
   begin
      Ref_Sink (W);

      Action_With_Data.Gtk_New (A, Callback'Unrestricted_Access, 42);

      Seen_Data := 0;
      Seen_Widget := null;
      Assert_True (A.Activate (0, W, Null_Gvariant));
      Assert_Cmpint_Eq (Gint (Seen_Data), 42);
      Assert_True (Seen_Widget = Gtk_Widget (W));

      Unref (A);
      Unref (W);
   end Test_Callback_Action;

   -----------------
   -- Test_Widget --
   -----------------

   procedure Test_Widget is
      W : constant Gtk_Search_Entry := Gtk_Search_Entry_New;
      C : constant Gtk_Shortcut_Controller := Gtk_Shortcut_Controller_New;
   begin
      Ref_Sink (W);

      C.Add_Shortcut (New_Shortcut ("<Control>q", "signal(stop-search)"));
      C.Set_Scope (Global);

      --  The widget owns the controller from here on.
      W.Add_Controller (C);
      Assert_True
        (Gtk_Event_Controller (C).Get_Widget = Gtk_Widget (W));
      Assert_True (C.Get_Scope = Global);

      Unref (W);
   end Test_Widget;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/shortcutcontroller/defaults", Test_Defaults'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/shortcutcontroller/scope", Test_Scope'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/shortcutcontroller/shortcuts", Test_Shortcuts'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/shortcutcontroller/model", Test_Model'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/shortcutcontroller/trigger-action",
      Test_Trigger_Action'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/shortcutcontroller/widget", Test_Widget'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/shortcutcontroller/callback-action",
      Test_Callback_Action'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Shortcut_Controller;
