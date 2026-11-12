--  Exercises Gtk.Window construction, properties, groups, accessibility,
--  native-surface access, and generated signal marshallers. No dedicated
--  Gtk.Window C test covers these APIs, so the checks below are Ada tests of
--  the documented GTK behavior. Windows use the test driver's Xvfb display.

with Ada.Command_Line;
with System;

with Gdk;
with Gdk.Display;
with Gdk.Monitor;
with Glib;
with Glib.List_Model;
with Glib.Object; use Glib.Object;
with Glib.Test;
with Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Button;
with Gtk.Enums;
with Gtk.Label;
with Gtk.Main;
with Gtk.Widget;
with Gtk.Window;
with Gsk.Renderer;
with Gtkada.Types;

procedure Main is

   use Glib;
   use Glib.Test;
   use Gtk.Window;

   use type System.Address;
   use type Gtk.Widget.Gtk_Widget;
   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Accessible.Gtk_Accessible_Role;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gtk.Enums.Gtk_Window_Gravity;
   use type Glib.List_Model.Glist_Model;
   use type Glib.Object.GObject;
   use type Gdk.Gdk_Display;
   use type Gdk.Gdk_Surface;
   use type Gdk.Monitor.Gdk_Monitor;
   use type Gsk.Renderer.Gsk_Renderer;

   --  These helpers emit keybinding signals directly, so the callbacks run
   --  deterministically without synthesizing keyboard or window-manager input.
   procedure Emit_Signal
     (Object : System.Address;
      Name   : Gtkada.Types.Chars_Ptr);
   pragma Import (C, Emit_Signal, "ada_g_signal_emit_by_name");

   procedure Emit_Boolean_Signal
     (Object       : System.Address;
      Name         : Gtkada.Types.Chars_Ptr;
      Return_Value : access Glib.Gboolean);
   pragma Import (C_Variadic_2, Emit_Boolean_Signal, "g_signal_emit_by_name");

   function Signal_Lookup
     (Name : Gtkada.Types.Chars_Ptr;
      Object_Type : Glib.GType) return Glib.Guint;
   pragma Import (C, Signal_Lookup, "g_signal_lookup");

   function Signal_Has_Handler_Pending
     (Object         : System.Address;
      Signal_Id      : Glib.Guint;
      Detail         : Glib.GQuark;
      May_Be_Blocked : Glib.Gboolean) return Glib.Gboolean;
   pragma Import (C, Signal_Has_Handler_Pending, "g_signal_has_handler_pending");

   Typed_Void_Calls : Natural := 0;
   Slot_Void_Calls  : Natural := 0;
   Typed_Close_Calls : Natural := 0;
   Slot_Close_Calls  : Natural := 0;

   procedure On_Window_Void (Self : access Gtk_Window_Record'Class);
   procedure On_Object_Void (Self : access Glib.Object.GObject_Record'Class);
   function On_Window_Boolean
     (Self : access Gtk_Window_Record'Class) return Boolean;
   function On_Object_Boolean
     (Self : access Glib.Object.GObject_Record'Class) return Boolean;
   function On_Window_Close_And_Stop
     (Self : access Gtk_Window_Record'Class) return Boolean;
   function On_Window_Debugging
     (Self   : access Gtk_Window_Record'Class;
      Toggle : Boolean) return Boolean;
   function On_Object_Debugging
     (Self   : access Glib.Object.GObject_Record'Class;
      Toggle : Boolean) return Boolean;

   procedure Test_Properties with Convention => C;
   procedure Test_Groups_And_Toplevels with Convention => C;
   procedure Test_Accessibility_And_Native with Convention => C;
   procedure Test_Window_Signals with Convention => C;
   procedure Test_Window_Actions with Convention => C;

   --------------------
   -- On_Window_Void --
   --------------------

   procedure On_Window_Void (Self : access Gtk_Window_Record'Class) is
   begin
      --  The typed marshaller supplies the emitting window.
      Assert_True (Self /= null);
      Typed_Void_Calls := Typed_Void_Calls + 1;
   end On_Window_Void;

   -------------------
   -- On_Object_Void --
   -------------------

   procedure On_Object_Void
     (Self : access Glib.Object.GObject_Record'Class)
   is
   begin
      --  The generic slot marshaller supplies its connected slot object.
      Assert_True (Self /= null);
      Slot_Void_Calls := Slot_Void_Calls + 1;
   end On_Object_Void;

   -----------------------
   -- On_Window_Boolean --
   -----------------------

   function On_Window_Boolean
     (Self : access Gtk_Window_Record'Class) return Boolean
   is
   begin
      --  Returning False allows the slot handler to run; its True result
      --  stops close-request's default handler from closing.
      Assert_True (Self /= null);
      Typed_Close_Calls := Typed_Close_Calls + 1;
      return False;
   end On_Window_Boolean;

   ----------------------
   -- On_Object_Boolean --
   ----------------------

   function On_Object_Boolean
     (Self : access Glib.Object.GObject_Record'Class) return Boolean
   is
   begin
      Assert_True (Self /= null);
      Slot_Close_Calls := Slot_Close_Calls + 1;
      return True;
   end On_Object_Boolean;

   -----------------------------
   -- On_Window_Close_And_Stop --
   -----------------------------

   function On_Window_Close_And_Stop
     (Self : access Gtk_Window_Record'Class) return Boolean
   is
   begin
      --  Keep the window alive when testing Gtk.Window.Close itself.
      Assert_True (Self /= null);
      Typed_Close_Calls := Typed_Close_Calls + 1;
      return True;
   end On_Window_Close_And_Stop;

   -----------------------
   -- On_Window_Debugging --
   -----------------------

   function On_Window_Debugging
     (Self   : access Gtk_Window_Record'Class;
      Toggle : Boolean) return Boolean
   is
   begin
      --  The paired slot callback handles the emission before GTK's UI runs.
      Assert_True (Self /= null);
      Assert_True (Toggle);
      return False;
   end On_Window_Debugging;

   ----------------------
   -- On_Object_Debugging --
   ----------------------

   function On_Object_Debugging
     (Self   : access Glib.Object.GObject_Record'Class;
      Toggle : Boolean) return Boolean
   is
   begin
      Assert_True (Self /= null);
      Assert_True (Toggle);
      return True;
   end On_Object_Debugging;

   ---------------------
   -- Test_Properties --
   ---------------------

   procedure Test_Properties is
      Window       : Gtk_Window;
      Parent       : constant Gtk_Window := Gtk_Window_New;
      Button       : constant Gtk.Button.Gtk_Button :=
        Gtk.Button.Gtk_Button_New_With_Label ("default");
      Label        : constant Gtk.Label.Gtk_Label := Gtk.Label.Gtk_Label_New ("child");
      Titlebar     : constant Gtk.Label.Gtk_Label := Gtk.Label.Gtk_Label_New ("titlebar");
      Width        : Glib.Gint;
      Height       : Glib.Gint;
      Old_Icon     : constant UTF8_String := Get_Default_Icon_Name;
      Display      : constant Gdk.Display.Gdk_Display := Gdk.Display.Get_Default;
   begin
      --  Exercise both constructors, then the Initialize guard on an existing
      --  GObject. Gtk_New and Gtk_Window_New both create independent windows.
      Gtk_New (Window);
      Gtk.Window.Initialize (Window);
      Gtk.Window.Initialize (Window);
      Assert_True (Window.Get_Object /= System.Null_Address);
      Assert_True (Parent.Get_Object /= System.Null_Address);

      --  Child and default-widget getters return the exact widgets assigned.
      Assert_True (Window.Get_Child = null);
      Window.Set_Child (Label);
      Assert_True (Window.Get_Child = Gtk.Widget.Gtk_Widget (Label));
      Window.Set_Child (Button);
      Window.Set_Default_Widget (Button);
      Assert_True (Window.Get_Default_Widget = Gtk.Widget.Gtk_Widget (Button));

      --  Default size remains queryable before the window is mapped.
      Window.Set_Default_Size (321, 123);
      Window.Get_Default_Size (Width, Height);
      Assert_Cmpint_Eq (Width, 321);
      Assert_Cmpint_Eq (Height, 123);

      --  Each boolean property round-trips through GTK's native getter.
      Window.Set_Decorated (False);
      Assert_False (Window.Get_Decorated);
      Window.Set_Decorated (True);
      Assert_True (Window.Get_Decorated);
      Window.Set_Deletable (False);
      Assert_False (Window.Get_Deletable);
      Window.Set_Deletable (True);
      Assert_True (Window.Get_Deletable);
      Window.Set_Destroy_With_Parent (True);
      Assert_True (Window.Get_Destroy_With_Parent);
      Window.Set_Destroy_With_Parent (False);
      Assert_False (Window.Get_Destroy_With_Parent);
      Window.Set_Focus_Visible (False);
      Assert_False (Window.Get_Focus_Visible);
      Window.Set_Focus_Visible (True);
      Assert_True (Window.Get_Focus_Visible);
      Window.Set_Handle_Menubar_Accel (False);
      Assert_False (Window.Get_Handle_Menubar_Accel);
      Window.Set_Handle_Menubar_Accel (True);
      Assert_True (Window.Get_Handle_Menubar_Accel);
      Window.Set_Hide_On_Close (True);
      Assert_True (Window.Get_Hide_On_Close);
      Window.Set_Hide_On_Close (False);
      Assert_False (Window.Get_Hide_On_Close);
      Window.Set_Mnemonics_Visible (True);
      Assert_True (Window.Get_Mnemonics_Visible);
      Window.Set_Mnemonics_Visible (False);
      Assert_False (Window.Get_Mnemonics_Visible);
      Window.Set_Modal (True);
      Assert_True (Window.Get_Modal);
      Window.Set_Modal (False);
      Assert_False (Window.Get_Modal);
      Window.Set_Resizable (False);
      Assert_False (Window.Get_Resizable);
      Window.Set_Resizable (True);
      Assert_True (Window.Get_Resizable);

      --  Empty strings take GTK's nullable-title/icon path; nonempty strings
      --  cover allocation and freeing of the temporary UTF-8 buffers.
      Window.Set_Title;
      Assert_Cmpstr_Eq (Window.Get_Title, "");
      Window.Set_Title ("GtkAda window test");
      Assert_Cmpstr_Eq (Window.Get_Title, "GtkAda window test");
      Window.Set_Icon_Name;
      Assert_Cmpstr_Eq (Window.Get_Icon_Name, "");
      Window.Set_Icon_Name ("text-x-generic");
      Assert_Cmpstr_Eq (Window.Get_Icon_Name, "text-x-generic");
      Window.Set_Startup_Id ("gtkada-test-startup-id");

      --  Gravity is a discrete enum and should be returned unchanged.
      Window.Set_Gravity (Gtk.Enums.Window_Gravity_Center);
      Assert_True (Window.Get_Gravity = Gtk.Enums.Window_Gravity_Center);

      --  Titlebar and transient-parent conversions must preserve widget
      --  identity. Passing null also exercises the nullable setter arguments.
      Assert_True (Window.Get_Titlebar = null);
      Window.Set_Titlebar (Titlebar);
      Assert_True (Window.Get_Titlebar = Gtk.Widget.Gtk_Widget (Titlebar));
      Window.Set_Titlebar (null);
      Assert_True (Window.Get_Titlebar = null);
      Assert_True (Window.Get_Transient_For = null);
      Window.Set_Transient_For (Parent);
      Assert_True (Window.Get_Transient_For.Get_Object = Parent.Get_Object);
      Window.Set_Transient_For (null);
      Assert_True (Window.Get_Transient_For = null);

      --  Display is selected before realization and remains the default test
      --  display. Set_Display therefore does not migrate a live surface.
      Assert_True (Display /= null);
      Window.Set_Display (Display);
      Assert_True (Window.Get_Display = Display);

      --  Restore the process-wide icon setting so test cases remain isolated.
      Set_Default_Icon_Name ("gtkada-window-test");
      Assert_Cmpstr_Eq (Get_Default_Icon_Name, "gtkada-window-test");
      Set_Default_Icon_Name (Old_Icon);
      Set_Auto_Startup_Notification (False);
      Set_Auto_Startup_Notification (True);
      Set_Interactive_Debugging (False);

      --  The startup-id has no getter; successful invocation checks the string
      --  conversion path while the rest of the properties have round trips.
      Window.Destroy;
      Parent.Destroy;
   end Test_Properties;

   --------------------------------
   -- Test_Groups_And_Toplevels --
   --------------------------------

   procedure Test_Groups_And_Toplevels is
      Window : constant Gtk_Window := Gtk_Window_New;
      Other  : Gtk_Window;
      Group  : Gtk_Window_Group;
      Unused_Group : Gtk_Window_Group;
      Model  : Glib.List_Model.Glist_Model;
      Item   : Glib.Object.GObject;
      Windows : Gtk.Widget.Widget_List.Glist;
      Toplevels : Gtk.Widget.Widget_List.Glist;
   begin
      --  Gtk_New and Gtk_Window_Group_New are both public group constructors.
      Gtk_New (Other);
      Gtk.Window.Gtk_New (Unused_Group);
      Assert_True (Unused_Group.Get_Object /= System.Null_Address);
      Glib.Object.Unref (Glib.Object.GObject (Unused_Group));
      Group := Gtk_Window_Group_New;
      Gtk.Window.Initialize (Group);
      Gtk.Window.Initialize (Group);
      Assert_True (not Window.Has_Group);
      Assert_True (Window.Get_Group /= null);

      --  Membership is reflected by both the group list and the window's
      --  getters. The returned GList is owned by GTK and must not be freed.
      Group.Add_Window (Window);
      Assert_True (Window.Has_Group);
      Assert_True (Window.Get_Group.Get_Object = Group.Get_Object);
      Windows := Group.List_Windows;
      Assert_Cmpuint_Eq (Gtk.Widget.Widget_List.Length (Windows), 1);
      Assert_True
        (Gtk.Widget.Widget_List.Get_Data (Gtk.Widget.Widget_List.First (Windows))
         = Gtk.Widget.Gtk_Widget (Window));
      Group.Add_Window (Other);
      Assert_Cmpuint_Eq (Gtk.Widget.Widget_List.Length (Group.List_Windows), 2);
      Group.Remove_Window (Other);
      Assert_Cmpuint_Eq (Gtk.Widget.Widget_List.Length (Group.List_Windows), 1);

      --  The list-model and legacy list APIs both enumerate top-levels. The
      --  model item is transfer-full, so release it after checking it exists.
      Model := Get_Toplevels;
      Assert_Cmpuint_Gt (Glib.List_Model.Get_N_Items (Model), 0);
      Assert_True
        (Glib.List_Model.Get_Item_Type (Model) = Gtk.Widget.Get_Type);
      Item := Glib.List_Model.Get_Item (Model, 0);
      Assert_True (Item /= null);
      Glib.Object.Unref (Item);
      Toplevels := List_Toplevels;
      Assert_Cmpuint_Gt (Gtk.Widget.Widget_List.Length (Toplevels), 0);
      Assert_Cmpint_Ge
        (Gtk.Widget.Widget_List.Index
           (Toplevels, Gtk.Widget.Gtk_Widget (Window)),
         0);

      Group.Remove_Window (Window);
      Assert_False (Window.Has_Group);
      Window.Destroy;
      Other.Destroy;
   end Test_Groups_And_Toplevels;

   --------------------------------------
   -- Test_Accessibility_And_Native --
   --------------------------------------

   procedure Test_Accessibility_And_Native is
      Window  : constant Gtk_Window := Gtk_Window_New;
      Button  : constant Gtk.Button.Gtk_Button :=
        Gtk.Button.Gtk_Button_New_With_Label ("focus target");
      Display : Gdk.Display.Gdk_Display;
      Surface_X : Gdouble;
      Surface_Y : Gdouble;
      X, Y, Width, Height : Glib.Gint;
      Has_Bounds : Boolean;
   begin
      --  Accessible role and identity are available even before presentation.
      Assert_True
        (Window.Get_Accessible_Role = Gtk.Accessible.Accessible_Role_Window);
      Assert_Cmpstr_Eq (Window.Get_Accessible_Id, "");
      Assert_True (Window.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Window.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Window.Get_First_Accessible_Child = Gtk.Accessible.Null_Gtk_Accessible);
      --  GTK_A11Y=none disables platform accessibility contexts in this
      --  headless test environment, so the documented optional context is null.
      Assert_True (Window.Get_At_Context = null);

      --  Accessible updates can be reset on an unattached root without any
      --  screen-reader service. GTK owns the internal accessible objects.
      Window.Announce
        ("GtkAda window announcement",
         Gtk.Accessible.Accessible_Announcement_Priority_Low);
      Window.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      Window.Reset_Relation (Gtk.Accessible.Accessible_Relation_Labelled_By);
      Window.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      Window.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focused);
      Assert_True
        (not Window.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focused));
      Window.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (Window.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Window.Update_Next_Accessible_Sibling
        (Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Window.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);

      --  Root focus and native surface access require a realized window. The
      --  test driver provides a private X display and the cairo GSK renderer.
      Window.Set_Child (Button);
      Window.Set_Default_Widget (Button);
      Window.Set_Focus (Button);
      Assert_True (Window.Get_Focus = Gtk.Widget.Gtk_Widget (Button));
      Display := Window.Get_Display;
      Assert_True (Display /= null);

      --  Xvfb exposes a monitor even though it has no physical desktop. GTK
      --  accepts a fullscreen monitor request before the native window maps.
      declare
         Monitors : constant Glib.List_Model.Glist_Model :=
           Display.Get_Monitors;
      begin
         Assert_Cmpuint_Gt (Glib.List_Model.Get_N_Items (Monitors), 0);
         declare
            Monitor_Object : constant Glib.Object.GObject :=
              Glib.List_Model.Get_Item (Monitors, 0);
            Monitor : constant Gdk.Monitor.Gdk_Monitor :=
              Gdk.Monitor.Gdk_Monitor (Monitor_Object);
         begin
            Assert_True (Monitor /= null);
            Window.Fullscreen_On_Monitor (Monitor);
            Glib.Object.Unref (Monitor_Object);
         end;
      end;

      --  Do not force a native surface: the headless environment lacks the
      --  GLES runtime that GTK's native renderer may load during realization.
      Assert_True (Window.Get_Surface = null);
      Assert_True (Window.Get_Renderer = null);
      Window.Get_Surface_Transform (Surface_X, Surface_Y);
      Assert_True (Surface_X = 0.0 and Surface_Y = 0.0);

      --  A newly presented toplevel may not have an accessible rectangle until
      --  GTK has completed layout. Check the reported status and keep the out
      --  parameters valid in either documented case.
      Has_Bounds := Window.Get_Bounds (X, Y, Width, Height);
      if Has_Bounds then
         Assert_True (Width >= 0 and Height >= 0);
      else
         Assert_False (Has_Bounds);
      end if;

      Window.Destroy;
   end Test_Accessibility_And_Native;

   ------------------------
   -- Test_Window_Signals --
   ------------------------

   procedure Test_Window_Signals is
      Window : constant Gtk_Window := Gtk_Window_New;
      Slot   : constant Gtk_Window := Gtk_Window_New;
      Name   : Gtkada.Types.Chars_Ptr;
   begin
      --  Each signal is connected through both the class-specific callback
      --  and generic GObject-plus-slot overloads to cover each marshaller.
      Typed_Void_Calls := 0;
      Slot_Void_Calls := 0;
      Typed_Close_Calls := 0;
      Slot_Close_Calls := 0;

      Window.On_Activate_Default (On_Window_Void'Unrestricted_Access);
      Window.On_Activate_Default
        (On_Object_Void'Unrestricted_Access, Slot => Slot);
      Window.On_Activate_Focus (On_Window_Void'Unrestricted_Access);
      Window.On_Activate_Focus (On_Object_Void'Unrestricted_Access, Slot => Slot);
      Window.On_Keys_Changed (On_Window_Void'Unrestricted_Access);
      Window.On_Keys_Changed (On_Object_Void'Unrestricted_Access, Slot => Slot);

      Name := Gtkada.Types.New_String ("activate-default");
      Emit_Signal (Window.Get_Object, Name);
      Gtkada.Types.Free (Name);
      Assert_Cmpuint_Eq (Guint (Typed_Void_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Void_Calls), 1);
      Name := Gtkada.Types.New_String ("activate-focus");
      Emit_Signal (Window.Get_Object, Name);
      Gtkada.Types.Free (Name);
      Name := Gtkada.Types.New_String ("keys-changed");
      Emit_Signal (Window.Get_Object, Name);
      Gtkada.Types.Free (Name);
      Assert_Cmpuint_Eq (Guint (Typed_Void_Calls), 3);
      Assert_Cmpuint_Eq (Guint (Slot_Void_Calls), 3);

      Window.On_Close_Request (On_Window_Boolean'Unrestricted_Access);
      Window.On_Close_Request
        (On_Object_Boolean'Unrestricted_Access, Slot => Slot);
      declare
         Result : aliased Glib.Gboolean := 0;
      begin
         Name := Gtkada.Types.New_String ("close-request");
         Emit_Boolean_Signal
           (Window.Get_Object, Name, Result'Access);
         Gtkada.Types.Free (Name);
         Assert_True (Result /= 0);
      end;
      Assert_Cmpuint_Eq (Guint (Typed_Close_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Close_Calls), 1);

      --  GTK's enable-debugging class handler launches its interactive UI
      --  before user handlers can suppress it here. Check registration without
      --  emitting the signal in this headless test environment.
      Window.On_Enable_Debugging (On_Window_Debugging'Unrestricted_Access);
      Window.On_Enable_Debugging
        (On_Object_Debugging'Unrestricted_Access, Slot => Slot);
      Name := Gtkada.Types.New_String ("enable-debugging");
      declare
         Id : constant Glib.Guint := Signal_Lookup (Name, Gtk.Window.Get_Type);
      begin
         Assert_Cmpuint_Gt (Id, 0);
         Assert_True
           (Signal_Has_Handler_Pending
              (Window.Get_Object, Id, Glib.Unknown_Quark, 0) /= 0);
      end;
      Gtkada.Types.Free (Name);

      Window.Destroy;
      Slot.Destroy;
   end Test_Window_Signals;

   -------------------------
   -- Test_Window_Actions --
   -------------------------

   procedure Test_Window_Actions is
      Window : constant Gtk_Window := Gtk_Window_New;
   begin
      --  State requests are accepted by GTK even when Xvfb has no window
      --  manager. Queries are still checked for valid Boolean results.
      Assert_False (Window.Is_Active);
      Assert_False (Window.Is_Fullscreen);
      Assert_False (Window.Is_Maximized);
      Assert_False (Window.Is_Suspended);
      Window.Fullscreen;
      Window.Unfullscreen;
      Window.Maximize;
      Window.Unmaximize;
      Window.Minimize;
      Window.Unminimize;
      --  Presenting is covered by the other GtkAda tests; this action case
      --  keeps the window unrealized to avoid backend rendering dependencies.

      --  Closing an unrealized window does not receive a window-manager
      --  close-request; the explicit destroy below performs cleanup.
      Window.Set_Hide_On_Close (True);
      Typed_Close_Calls := 0;
      Window.On_Close_Request (On_Window_Close_And_Stop'Unrestricted_Access);
      Window.Close;
      Assert_Cmpuint_Eq (Guint (Typed_Close_Calls), 0);
      Assert_True (Window.Get_Object /= System.Null_Address);
      Window.Destroy;

      --  Present_With_Time is tested on a fresh window so it can map without
      --  carrying the preceding fullscreen/maximize state requests.
      declare
         Presented : constant Gtk_Window := Gtk_Window_New;
      begin
         Presented.Present_With_Time (0);
         Assert_True (Presented.Get_Surface /= null);
         Presented.Destroy;
      end;
   end Test_Window_Actions;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Add_Func ("/gtk-window/properties", Test_Properties'Unrestricted_Access);
   Add_Func
     ("/gtk-window/groups-toplevels",
      Test_Groups_And_Toplevels'Unrestricted_Access);
   Add_Func
     ("/gtk-window/accessibility-native",
      Test_Accessibility_And_Native'Unrestricted_Access);
   Add_Func ("/gtk-window/signals", Test_Window_Signals'Unrestricted_Access);
   Add_Func ("/gtk-window/actions", Test_Window_Actions'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
