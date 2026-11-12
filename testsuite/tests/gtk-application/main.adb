--  Gtk.Application binding tests for accelerators, action interfaces,
--  menus, windows, inhibitors and the Gtk.Application-specific signals.
--  There is no focused GTK C test for these methods in testsuite/c_tests/.

with Ada.Command_Line;
with GNAT.Strings;
with System;

with Glib;                    use Glib;
with Glib.Action;
with Glib.Action_Group;
with Glib.Action_Map;
with Glib.Application;        use Glib.Application;
with Glib.Menu;
with Glib.Menu_Model;
with Glib.Object;
with Glib.Simple_Action;
with Glib.Test;               use Glib.Test;
with Glib.Variant;
with Gdk.Display;
with Gdk.Surface;
with Gsk.Renderer;
with Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Application;         use Gtk.Application;
with Gtk.Application_Window;
with Gtk.Button;
with Gtk.Main;
with Gtk.Widget;
with Gtk.Window;
with Gtkada.Types;

procedure Main is
   use type Gtk.Application.Gtk_Application;
   use type Gtk.Window.Gtk_Window;
   use type Glib.Action.Gaction;
   use type Glib.Menu.Gmenu;
   use type Glib.Menu_Model.Gmenu_Model;
   use type System.Address;
   use type Glib.Variant.Gvariant_Type;
   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Accessible.Gtk_Accessible_Role;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gtk.Widget.Gtk_Widget;
   use type Gdk.Gdk_Display;
   use type Gdk.Gdk_Surface;
   use type Gsk.Renderer.Gsk_Renderer;

   Added_Calls       : Natural := 0;
   Added_Slot_Calls  : Natural := 0;
   Removed_Calls     : Natural := 0;
   Removed_Slot_Calls : Natural := 0;
   Query_End_Calls   : Natural := 0;
   Query_End_Slot_Calls : Natural := 0;
   Action_Added_Calls : Natural := 0;
   Action_Enabled_Calls : Natural := 0;
   Action_Removed_Calls : Natural := 0;
   Action_State_Calls : Natural := 0;
   Expected_Window  : Gtk.Window.Gtk_Window;

   --  Emit the zero-argument query-end signal to exercise both callback
   --  marshallers without depending on a desktop session manager.
   procedure Emit_Query_End
     (Instance : System.Address;
      Name     : Gtkada.Types.Chars_Ptr);
   pragma Import (C, Emit_Query_End, "g_signal_emit_by_name");

   procedure On_Added
     (Self   : access Gtk_Application_Record'Class;
      Window : not null access Gtk.Window.Gtk_Window_Record'Class);
   procedure On_Added_Slot
     (Self   : access Glib.Object.GObject_Record'Class;
      Window : not null access Gtk.Window.Gtk_Window_Record'Class);
   procedure On_Removed
     (Self   : access Gtk_Application_Record'Class;
      Window : not null access Gtk.Window.Gtk_Window_Record'Class);
   procedure On_Removed_Slot
     (Self   : access Glib.Object.GObject_Record'Class;
      Window : not null access Gtk.Window.Gtk_Window_Record'Class);
   procedure On_Query_End
     (Self : access Gtk_Application_Record'Class);
   procedure On_Query_End_Slot
     (Self : access Glib.Object.GObject_Record'Class);
   procedure On_Action_Added
     (Self : Glib.Action_Group.Gaction_Group; Name : UTF8_String);
   procedure On_Action_Enabled
     (Self : Glib.Action_Group.Gaction_Group;
      Name : UTF8_String; Enabled : Boolean);
   procedure On_Action_Removed
     (Self : Glib.Action_Group.Gaction_Group; Name : UTF8_String);
   procedure On_Action_State
     (Self : Glib.Action_Group.Gaction_Group;
      Name : UTF8_String; State : Glib.Variant.Gvariant);

   ----------------
   -- On_Added --
   ----------------

   --  The direct signal callback receives the GtkApplication and the same
   --  window that was just attached.
   procedure On_Added
     (Self   : access Gtk_Application_Record'Class;
      Window : not null access Gtk.Window.Gtk_Window_Record'Class) is
   begin
      Assert_True (Self /= null);
      Assert_True (Window.Get_Object = Expected_Window.Get_Object);
      Added_Calls := Added_Calls + 1;
   end On_Added;

   ---------------------
   -- On_Added_Slot --
   ---------------------

   --  The slot overload passes the explicit GObject slot before the window.
   procedure On_Added_Slot
     (Self   : access Glib.Object.GObject_Record'Class;
      Window : not null access Gtk.Window.Gtk_Window_Record'Class) is
   begin
      Assert_True (Self /= null);
      Assert_True (Window.Get_Object = Expected_Window.Get_Object);
      Added_Slot_Calls := Added_Slot_Calls + 1;
   end On_Added_Slot;

   ------------------
   -- On_Removed --
   ------------------

   --  Removing the window emits the matching direct signal with that window.
   procedure On_Removed
     (Self   : access Gtk_Application_Record'Class;
      Window : not null access Gtk.Window.Gtk_Window_Record'Class) is
   begin
      Assert_True (Self /= null);
      Assert_True (Window.Get_Object = Expected_Window.Get_Object);
      Removed_Calls := Removed_Calls + 1;
   end On_Removed;

   -------------------------
   -- On_Removed_Slot --
   -------------------------

   --  The slot overload covers the GObject-plus-window marshaller as well.
   procedure On_Removed_Slot
     (Self   : access Glib.Object.GObject_Record'Class;
      Window : not null access Gtk.Window.Gtk_Window_Record'Class) is
   begin
      Assert_True (Self /= null);
      Assert_True (Window.Get_Object = Expected_Window.Get_Object);
      Removed_Slot_Calls := Removed_Slot_Calls + 1;
   end On_Removed_Slot;

   --------------------
   -- On_Query_End --
   --------------------

   --  This callback checks the Gtk.Application-specific void signal path.
   procedure On_Query_End
     (Self : access Gtk_Application_Record'Class) is
   begin
      Assert_True (Self /= null);
      Query_End_Calls := Query_End_Calls + 1;
   end On_Query_End;

   -------------------------
   -- On_Query_End_Slot --
   -------------------------

   --  The slot form exercises the second marshaller for the same signal.
   procedure On_Query_End_Slot
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Assert_True (Self /= null);
      Query_End_Slot_Calls := Query_End_Slot_Calls + 1;
   end On_Query_End_Slot;

   ---------------------
   -- Action Signals --
   ---------------------

   --  Check each payload from the GActionGroup interface signal emitters.
   procedure On_Action_Added
     (Self : Glib.Action_Group.Gaction_Group; Name : UTF8_String) is
   begin
      Assert_Cmpstr_Eq (Name, "signal-probe");
      Action_Added_Calls := Action_Added_Calls + 1;
   end On_Action_Added;

   procedure On_Action_Enabled
     (Self : Glib.Action_Group.Gaction_Group;
      Name : UTF8_String; Enabled : Boolean) is
   begin
      Assert_Cmpstr_Eq (Name, "signal-probe");
      Assert_True (Enabled);
      Action_Enabled_Calls := Action_Enabled_Calls + 1;
   end On_Action_Enabled;

   procedure On_Action_Removed
     (Self : Glib.Action_Group.Gaction_Group; Name : UTF8_String) is
   begin
      Assert_Cmpstr_Eq (Name, "signal-probe");
      Action_Removed_Calls := Action_Removed_Calls + 1;
   end On_Action_Removed;

   procedure On_Action_State
     (Self : Glib.Action_Group.Gaction_Group;
      Name : UTF8_String; State : Glib.Variant.Gvariant) is
   begin
      Assert_Cmpstr_Eq (Name, "signal-probe");
      Assert_Cmpstr_Eq (State.Get_String, "busy");
      Action_State_Calls := Action_State_Calls + 1;
   end On_Action_State;

   --  Check action accessors, mutations and accelerator conversion together
   --  so each returned GNAT.Strings list can be validated and freed.
   procedure Test_Actions_And_Accelerators
   with Convention => C;

   procedure Test_Actions_And_Accelerators is
      App : constant Gtk_Application := Gtk_Application_New
        ("org.gtkada.Coverage.GtkActions", G_Application_Non_Unique);
      Initial_State : constant Glib.Variant.Gvariant :=
        Glib.Variant.Gvariant_New_String ("ready");
      State_Hint : constant Glib.Variant.Gvariant :=
        Glib.Variant.Gvariant_New_String ("ready or busy");
      New_State : constant Glib.Variant.Gvariant :=
        Glib.Variant.Ref_Sink
          (Glib.Variant.Gvariant_New_String ("busy"));
      Action : constant Glib.Simple_Action.Gsimple_Action :=
        Glib.Simple_Action.Gsimple_Action_New_Stateful
          ("mode", Glib.Variant.Gvariant_Type_String, Initial_State);
      Entries : constant Glib.Action_Map.GAction_Entry_Array :=
        (0 => Glib.Action_Map.Build ("entry-one"),
         1 => Glib.Action_Map.Build ("entry-two"));
      Empty_Accels : constant GNAT.Strings.String_List (1 .. 0) :=
        (others => null);
      Enabled      : Boolean;
      Parameter_Ty : aliased Glib.Variant.Gvariant_Type;
      State_Ty     : aliased Glib.Variant.Gvariant_Type;
      Hint_Value   : aliased Glib.Variant.Gvariant;
      State_Value  : aliased Glib.Variant.Gvariant;
      Value        : Glib.Variant.Gvariant;
   begin
      --  Registering a non-unique application avoids a D-Bus service while
      --  still allowing Gtk.Application's action descriptions to be queried.
      Assert_True (App.Register (null));
      App.Add_Action (Glib.Simple_Action."+" (Action));
      Action.Set_State_Hint (State_Hint);
      Assert_True (App.Has_Action ("mode"));
      Assert_True (App.Get_Action_Enabled ("mode"));
      Assert_True
        (App.Lookup_Action ("mode") = Glib.Simple_Action."+" (Action));

      --  Each action metadata accessor reports the declared state and hint.
      Assert_True
        (Glib.Variant.Peek_String
           (App.Get_Action_Parameter_Type ("mode")) = "s");
      Assert_True
        (Glib.Variant.Peek_String (App.Get_Action_State_Type ("mode")) = "s");
      Value := App.Get_Action_State ("mode");
      Assert_Cmpstr_Eq (Value.Get_String, "ready");
      Glib.Variant.Unref (Value);
      Value := App.Get_Action_State_Hint ("mode");
      Assert_Cmpstr_Eq (Value.Get_String, "ready or busy");
      Glib.Variant.Unref (Value);

      --  Query_Action returns both optional variant values when requested.
      Assert_True
        (App.Query_Action
           ("mode", Enabled, Parameter_Ty'Access, State_Ty'Access,
            Hint_Value'Access, State_Value'Access));
      Assert_True (Enabled);
      Assert_True (Glib.Variant.Peek_String (Parameter_Ty) = "s");
      Assert_True (Glib.Variant.Peek_String (State_Ty) = "s");
      Assert_Cmpstr_Eq (Hint_Value.Get_String, "ready or busy");
      Assert_Cmpstr_Eq (State_Value.Get_String, "ready");
      Glib.Variant.Unref (Hint_Value);
      Glib.Variant.Unref (State_Value);

      --  A missing action is reported as absent by both query APIs.
      Assert_False (App.Query_Action ("missing", Enabled));
      Assert_False (App.Has_Action ("missing"));
      Assert_True
        (App.Lookup_Action ("missing") = Glib.Action.Null_Gaction);

      --  Add and remove a pair of convenience entries, checking both names.
      App.Add_Action_Entries (Entries);
      Assert_True (App.Has_Action ("entry-one"));
      Assert_True (App.Has_Action ("entry-two"));
      declare
         Actions : GNAT.Strings.String_List := App.List_Actions;
      begin
         Assert_True (Actions'Length >= 3);
         for Name of Actions loop
            GNAT.Strings.Free (Name);
         end loop;
      end;
      App.Remove_Action_Entries (Entries, Entries'Length);
      Assert_False (App.Has_Action ("entry-one"));
      Assert_False (App.Has_Action ("entry-two"));

      --  Gtk accelerators use zero-terminated arrays of strings at the C
      --  boundary; validate both directions and then clear the association.
      App.Set_Accels_For_Action
        ("app.mode", (1 => new String'("<Control>q")));
      declare
         Accels : GNAT.Strings.String_List :=
           App.Get_Accels_For_Action ("app.mode");
         Actions : GNAT.Strings.String_List :=
           App.Get_Actions_For_Accel ("<Control>q");
      begin
         Assert_True (Accels'Length = 1);
         Assert_Cmpstr_Eq (Accels (Accels'First).all, "<Control>q");
         Assert_True (Actions'Length = 1);
         Assert_Cmpstr_Eq (Actions (Actions'First).all, "app.mode");
         for Accel of Accels loop
            GNAT.Strings.Free (Accel);
         end loop;
         for Name of Actions loop
            GNAT.Strings.Free (Name);
         end loop;
      end;
      --  List_Action_Descriptions exposes the application-qualified names.
      declare
         Descriptions : GNAT.Strings.String_List :=
           App.List_Action_Descriptions;
         Found_Mode : Boolean := False;
      begin
         for Description of Descriptions loop
            if Description.all = "app.mode" then
               Found_Mode := True;
            end if;
            GNAT.Strings.Free (Description);
         end loop;
         Assert_True (Found_Mode);
      end;

      --  An empty, zero-terminated list removes all accelerators for an action.
      App.Set_Accels_For_Action ("app.mode", Empty_Accels);
      declare
         Accels : GNAT.Strings.String_List :=
           App.Get_Accels_For_Action ("app.mode");
      begin
         Assert_True (Accels'Length = 0);
      end;

      --  State and enabled changes, activation, and removal all round-trip.
      App.Change_Action_State ("mode", New_State);
      Value := App.Get_Action_State ("mode");
      Assert_Cmpstr_Eq (Value.Get_String, "busy");
      Glib.Variant.Unref (Value);
      App.Activate_Action ("mode", New_State);
      Action.Set_Enabled (False);
      Assert_False (App.Get_Action_Enabled ("mode"));
      Action.Set_Enabled (True);
      App.Remove_Action ("mode");
      Assert_False (App.Has_Action ("mode"));

      --  These interface methods emit their action-group signals to exercise
      --  the action-name, enabled-flag and variant argument conversions.
      declare
         Probe : constant Glib.Simple_Action.Gsimple_Action :=
           Glib.Simple_Action.Gsimple_Action_New_Stateful
             ("signal-probe", Glib.Variant.Gvariant_Type_String,
              Glib.Variant.Gvariant_New_String ("ready"));
         Group : constant Glib.Action_Group.Gaction_Group :=
           Glib.Application."+" (Glib.Application.Gapplication (App));
      begin
         App.Add_Action (Glib.Simple_Action."+" (Probe));
         Glib.Action_Group.On_Action_Added
           (Group, On_Action_Added'Unrestricted_Access);
         Glib.Action_Group.On_Action_Enabled_Changed
           (Group, On_Action_Enabled'Unrestricted_Access);
         Glib.Action_Group.On_Action_Removed
           (Group, On_Action_Removed'Unrestricted_Access);
         Glib.Action_Group.On_Action_State_Changed
           (Group, On_Action_State'Unrestricted_Access);
         App.Action_Added ("signal-probe");
         App.Action_Enabled_Changed ("signal-probe", True);
         App.Action_State_Changed ("signal-probe", New_State);
         App.Action_Removed ("signal-probe");
         Assert_True (Action_Added_Calls = 1);
         Assert_True (Action_Enabled_Calls = 1);
         Assert_True (Action_State_Calls = 1);
         Assert_True (Action_Removed_Calls = 1);
         Glib.Object.Unref (Glib.Object.GObject (Probe));
      end;

      --  Release the references created by the variant and GObject factories.
      Glib.Variant.Unref (New_State);
      Glib.Object.Unref (Glib.Object.GObject (Action));
      Glib.Object.Unref (Glib.Object.GObject (App));
   end Test_Actions_And_Accelerators;

   --  Exercise Gtk.Application's windows, menus, session inhibition wrappers
   --  and every Gtk.Application signal callback form.
   procedure Test_Windows_Menus_And_Signals
   with Convention => C;

   procedure Test_Application_Window_Interfaces
   with Convention => C;

   procedure Test_Windows_Menus_And_Signals is
      App : constant Gtk_Application := Gtk_Application_New
        ("org.gtkada.Coverage.GtkWindows", G_Application_Non_Unique);
      Second_App : Gtk_Application;
      App_Window : Gtk.Application_Window.Gtk_Application_Window;
      Window : Gtk.Window.Gtk_Window;
      Menu : constant Glib.Menu.Gmenu := Glib.Menu.Gmenu_New;
      Windows : Gtk.Widget.Widget_List.Glist;
      Menu_Model : Glib.Menu_Model.Gmenu_Model;
      Cookie : Guint;
      App_Window_Id : Guint;
   begin
      --  The out-parameter constructor is equivalent to Gtk_Application_New.
      Gtk_New
        (Second_App, "org.gtkada.Coverage.GtkSecond",
         G_Application_Non_Unique);
      Assert_Cmpstr_Eq (Second_App.Get_Application_Id,
                        "org.gtkada.Coverage.GtkSecond");
      Glib.Object.Unref (Glib.Object.GObject (Second_App));

      --  Gtk.Application menu and window methods are only valid after the
      --  application has registered; non-unique registration needs no bus.
      Assert_True (App.Register (null));

      --  Empty queries cover the null-object conversions before any menu or
      --  window has been associated with the application.
      Assert_True (App.Get_Active_Window = null);
      Assert_True (App.Get_Window_By_Id (0) = null);
      Assert_Cmpuint_Eq (Gtk.Widget.Widget_List.Length (App.Get_Windows), 0);
      Assert_True (App.Get_Menubar = null);
      Assert_True (App.Get_Menu_By_Id ("missing-resource-menu") = null);

      --  Set and retrieve a caller-owned menu model; GTK keeps its own ref.
      Glib.Menu.Append (Menu, "Quit", "app.quit");
      App.Set_Menubar (Menu);
      Menu_Model := App.Get_Menubar;
      Assert_True (Menu_Model /= null);
      Assert_Cmpint_Eq (Menu_Model.Get_N_Items, 1);
      App.Set_Menubar (null);
      Assert_True (App.Get_Menubar = null);

      --  GtkApplicationWindow exposes the ID accepted by Get_Window_By_Id;
      --  a plain GtkWindow below separately exercises explicit Add_Window.
      App_Window := Gtk.Application_Window.Gtk_Application_Window_New (App);
      App_Window_Id := App_Window.Get_Id;
      Assert_True (App_Window_Id /= 0);
      Assert_True
        (App.Get_Window_By_Id (App_Window_Id) =
           Gtk.Window.Gtk_Window (App_Window));
      App_Window.Destroy;
      Assert_True (App.Get_Window_By_Id (App_Window_Id) = null);

      --  Adding a window emits both signal callback forms.
      App.On_Window_Added (On_Added'Unrestricted_Access);
      App.On_Window_Added (On_Added_Slot'Unrestricted_Access, App);
      App.On_Window_Removed (On_Removed'Unrestricted_Access);
      App.On_Window_Removed (On_Removed_Slot'Unrestricted_Access, App);
      App.On_Query_End (On_Query_End'Unrestricted_Access);
      App.On_Query_End (On_Query_End_Slot'Unrestricted_Access, App);

      Window := Gtk.Window.Gtk_Window_New;
      Expected_Window := Window;
      App.Add_Window (Window);
      Assert_True (Added_Calls = 1);
      Assert_True (Added_Slot_Calls = 1);
      Windows := App.Get_Windows;
      Assert_Cmpuint_Eq (Gtk.Widget.Widget_List.Length (Windows), 1);

      --  Inhibiting is unavailable without a session manager and returns 0.
      --  Only a positive cookie is valid to pass to Uninhibit.
      Cookie := App.Inhibit
        (Window, Application_Inhibit_Logout or Application_Inhibit_Idle,
         "coverage test");
      if Cookie > 0 then
         App.Uninhibit (Cookie);
      end if;

      --  Emitting query-end directly makes the signal test independent of a
      --  desktop session manager initiating shutdown.
      declare
         Name : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("query-end");
      begin
         Emit_Query_End (App.Get_Object, Name);
         Gtkada.Types.Free (Name);
      end;
      Assert_True (Query_End_Calls = 1);
      Assert_True (Query_End_Slot_Calls = 1);

      --  Explicit removal emits both removal signals and empties the window
      --  list; destroy the detached toplevel to close it cleanly.
      App.Remove_Window (Window);
      Assert_True (Removed_Calls = 1);
      Assert_True (Removed_Slot_Calls = 1);
      Assert_Cmpuint_Eq (Gtk.Widget.Widget_List.Length (App.Get_Windows), 0);
      Assert_True (App.Get_Window_By_Id (1) = null);
      Window.Destroy;

      --  Ensure the callback's borrowed expected reference is not used again.
      Expected_Window := null;
      Glib.Object.Unref (Glib.Object.GObject (Menu));
      Glib.Object.Unref (Glib.Object.GObject (App));
   end Test_Windows_Menus_And_Signals;

   -----------------------------------------
   -- Test_Application_Window_Interfaces --
   -----------------------------------------

   --  Exercise GtkApplicationWindow's own action, accessibility, root and
   --  native wrappers.  The related Gtk.Window checks cannot cover this
   --  generated unit because each binding calls its C API through a new body.
   procedure Test_Application_Window_Interfaces is
      App : constant Gtk_Application := Gtk_Application_New
        ("org.gtkada.Coverage.ApplicationWindow", G_Application_Non_Unique);
      Window : Gtk.Application_Window.Gtk_Application_Window;
      Button : constant Gtk.Button.Gtk_Button :=
        Gtk.Button.Gtk_Button_New_With_Label ("focus target");
      Initial_State : constant Glib.Variant.Gvariant :=
        Glib.Variant.Ref_Sink (Glib.Variant.Gvariant_New_Boolean (True));
      State_Hint : constant Glib.Variant.Gvariant :=
        Glib.Variant.Ref_Sink (Glib.Variant.Gvariant_New_Boolean (False));
      New_State : constant Glib.Variant.Gvariant :=
        Glib.Variant.Ref_Sink (Glib.Variant.Gvariant_New_Boolean (False));
      Signal_State : constant Glib.Variant.Gvariant :=
        Glib.Variant.Ref_Sink
          (Glib.Variant.Gvariant_New_String ("busy"));
      Toggle : constant Glib.Simple_Action.Gsimple_Action :=
        Glib.Simple_Action.Gsimple_Action_New_Stateful
          ("toggle", null, Initial_State);
      Probe : constant Glib.Simple_Action.Gsimple_Action :=
        Glib.Simple_Action.Gsimple_Action_New_Stateful
          ("signal-probe", Glib.Variant.Gvariant_Type_String,
           Glib.Variant.Gvariant_New_String ("ready"));
      Entries : constant Glib.Action_Map.GAction_Entry_Array :=
        (0 => Glib.Action_Map.Build ("entry-one"),
         1 => Glib.Action_Map.Build ("entry-two"));
      Group : Glib.Action_Group.Gaction_Group;
      X, Y, Width, Height : Glib.Gint;
      Has_Bounds : Boolean;
      Enabled : Boolean;
      State_Type : aliased Glib.Variant.Gvariant_Type;
      Hint_Value : aliased Glib.Variant.Gvariant;
      State_Value : aliased Glib.Variant.Gvariant;
      Current_State : Glib.Variant.Gvariant;
      Saw_Entry_One : Boolean := False;
      Saw_Entry_Two : Boolean := False;
   begin
      --  A registered non-unique application needs no session bus and assigns
      --  window IDs synchronously, making the constructor checks deterministic.
      Assert_True (App.Register (null));
      Gtk.Application_Window.Gtk_New (Window, App);
      Gtk.Application_Window.Initialize (Window, App);
      Assert_Cmpuint_Gt (Window.Get_Id, 0);

      --  The property getter reflects both values written through its setter.
      Assert_False (Window.Get_Show_Menubar);
      Window.Set_Show_Menubar (True);
      Assert_True (Window.Get_Show_Menubar);
      Window.Set_Show_Menubar (False);
      Assert_False (Window.Get_Show_Menubar);

      --  Add a stateful action and check metadata, borrowed lookup identity,
      --  and owned variants returned by the action-group getters.
      Toggle.Set_State_Hint (State_Hint);
      Window.Add_Action (Glib.Simple_Action."+" (Toggle));
      Assert_True (Window.Has_Action ("toggle"));
      Assert_True (Window.Get_Action_Enabled ("toggle"));
      Assert_True
        (Window.Lookup_Action ("toggle") =
           Glib.Simple_Action."+" (Toggle));
      Assert_True (Window.Get_Action_Parameter_Type ("toggle") = null);
      Assert_True (Window.Get_Action_State_Type ("toggle") /= null);
      Current_State := Window.Get_Action_State ("toggle");
      Assert_True (Current_State.Get_Boolean);
      Glib.Variant.Unref (Current_State);
      Hint_Value := Window.Get_Action_State_Hint ("toggle");
      Assert_False (Hint_Value.Get_Boolean);
      Glib.Variant.Unref (Hint_Value);

      --  Query_Action fills the requested values and safely handles a missing
      --  name when its optional result pointers are omitted.
      --  Returned hint and state variants are transfer-full and are released.
      Assert_True
        (Window.Query_Action
           ("toggle", Enabled, State_Type => State_Type'Access,
            State_Hint => Hint_Value'Access, State => State_Value'Access));
      Assert_True (Enabled);
      Assert_True (State_Type /= null);
      Assert_False (Hint_Value.Get_Boolean);
      Assert_True (State_Value.Get_Boolean);
      Glib.Variant.Unref (Hint_Value);
      Glib.Variant.Unref (State_Value);
      Assert_False (Window.Query_Action ("missing", Enabled));
      --  GTK permits every output field to be modified on a failed query.
      Assert_False (Window.Has_Action ("missing"));
      Assert_True
        (Window.Lookup_Action ("missing") = Glib.Action.Null_Gaction);

      --  State changes and activation are observed through fresh variants;
      --  the latter uses the default boolean toggle behavior of GSimpleAction.
      Window.Change_Action_State ("toggle", New_State);
      Current_State := Window.Get_Action_State ("toggle");
      Assert_False (Current_State.Get_Boolean);
      Glib.Variant.Unref (Current_State);
      Window.Activate_Action ("toggle", Glib.Variant.Null_Gvariant);
      Current_State := Window.Get_Action_State ("toggle");
      Assert_True (Current_State.Get_Boolean);
      Glib.Variant.Unref (Current_State);
      Toggle.Set_Enabled (False);
      Assert_False (Window.Get_Action_Enabled ("toggle"));
      Toggle.Set_Enabled (True);
      Assert_True (Window.Get_Action_Enabled ("toggle"));
      Window.Remove_Action ("toggle");
      Assert_False (Window.Has_Action ("toggle"));

      --  The entry helpers add and remove a batch, and List_Actions returns
      --  separately allocated names that this test releases after checking.
      Window.Add_Action_Entries (Entries);
      Assert_True (Window.Has_Action ("entry-one"));
      Assert_True (Window.Has_Action ("entry-two"));
      declare
         Names : GNAT.Strings.String_List := Window.List_Actions;
      begin
         for Name of Names loop
            Saw_Entry_One := Saw_Entry_One or else Name.all = "entry-one";
            Saw_Entry_Two := Saw_Entry_Two or else Name.all = "entry-two";
            GNAT.Strings.Free (Name);
         end loop;
      end;
      Assert_True (Saw_Entry_One and Saw_Entry_Two);
      Window.Remove_Action_Entries (Entries, Entries'Length);
      Assert_False (Window.Has_Action ("entry-one"));
      Assert_False (Window.Has_Action ("entry-two"));

      --  Connect to the interface signals before invoking its explicit
      --  emitters, so every name, enabled flag and variant reaches an assertion.
      --  These notifications are separate from adding and removing map entries.
      Window.Add_Action (Glib.Simple_Action."+" (Probe));
      Group := Gtk.Application_Window."+" (Window);
      Action_Added_Calls := 0;
      Action_Enabled_Calls := 0;
      Action_Removed_Calls := 0;
      Action_State_Calls := 0;
      Glib.Action_Group.On_Action_Added
        (Group, On_Action_Added'Unrestricted_Access);
      Glib.Action_Group.On_Action_Enabled_Changed
        (Group, On_Action_Enabled'Unrestricted_Access);
      Glib.Action_Group.On_Action_Removed
        (Group, On_Action_Removed'Unrestricted_Access);
      Glib.Action_Group.On_Action_State_Changed
        (Group, On_Action_State'Unrestricted_Access);
      Window.Action_Added ("signal-probe");
      Window.Action_Enabled_Changed ("signal-probe", True);
      Window.Action_State_Changed
        ("signal-probe", Signal_State);
      Window.Action_Removed ("signal-probe");
      Assert_True (Action_Added_Calls = 1);
      Assert_True (Action_Enabled_Calls = 1);
      Assert_True (Action_State_Calls = 1);
      Assert_True (Action_Removed_Calls = 1);
      Glib.Variant.Unref (Signal_State);
      Window.Remove_Action ("signal-probe");
      Assert_False (Window.Has_Action ("signal-probe"));
      Assert_False (Window.Has_Action ("toggle"));

      --  A window is itself the root accessible object; accessibility updates
      --  remain safe in the driver's GTK_A11Y=none configuration.
      Assert_Cmpstr_Eq (Window.Get_Accessible_Id, "");
      Assert_True
        (Window.Get_Accessible_Role =
           Gtk.Accessible.Accessible_Role_Window);
      Assert_True
        (Window.Get_Accessible_Parent =
           Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Window.Get_First_Accessible_Child =
           Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Window.Get_Next_Accessible_Sibling =
           Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (Window.Get_At_Context = null);
      Window.Announce
        ("GtkAda application-window announcement",
         Gtk.Accessible.Accessible_Announcement_Priority_Low);
      Window.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      Window.Reset_Relation
        (Gtk.Accessible.Accessible_Relation_Labelled_By);
      Window.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      Window.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Window.Get_Accessible_Parent =
           Gtk.Accessible.Null_Gtk_Accessible);
      Window.Update_Next_Accessible_Sibling
        (Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Window.Get_Next_Accessible_Sibling =
           Gtk.Accessible.Null_Gtk_Accessible);
      Window.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focused);
      Assert_False
        (Window.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focused));
      Has_Bounds := Window.Get_Bounds (X, Y, Width, Height);
      if Has_Bounds then
         Assert_True (Width >= 0 and Height >= 0);
      else
         Assert_False (Has_Bounds);
      end if;

      --  Root focus and display are available before presentation; the native
      --  surface and renderer are null until GTK realizes the window.
      Assert_True (Window.Get_Focus = null);
      Assert_True (Window.Get_Display /= null);
      Window.Set_Child (Button);
      Window.Set_Focus (Button);
      Assert_True (Window.Get_Focus = Gtk.Widget.Gtk_Widget (Button));
      Assert_True (Window.Get_Surface = null);
      Assert_True (Window.Get_Renderer = null);
      declare
         Surface_X, Surface_Y : Glib.Gdouble;
      begin
         Window.Get_Surface_Transform (Surface_X, Surface_Y);
         Assert_True (Surface_X = 0.0 and Surface_Y = 0.0);
      end;

      --  Drop the objects and variants created by this test after GTK releases
      --  the action-map's references during window destruction.
      Window.Destroy;
      Glib.Variant.Unref (Initial_State);
      Glib.Variant.Unref (State_Hint);
      Glib.Variant.Unref (New_State);
      Glib.Object.Unref (Glib.Object.GObject (Toggle));
      Glib.Object.Unref (Glib.Object.GObject (Probe));
      Glib.Object.Unref (Glib.Object.GObject (App));
   end Test_Application_Window_Interfaces;

begin
   --  Initialize GTK after GLib.Test as required for unattended widget tests.
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/gtk-application/actions-accelerators",
             Test_Actions_And_Accelerators'Unrestricted_Access);
   Add_Func ("/gtk-application/windows-menus-signals",
             Test_Windows_Menus_And_Signals'Unrestricted_Access);
   Add_Func ("/gtk-application/application-window-interfaces",
             Test_Application_Window_Interfaces'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Run);
end Main;
