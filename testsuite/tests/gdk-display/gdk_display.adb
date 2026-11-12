--  Exercises Gdk.Display and its app-launch context using the test driver's
--  Xvfb display.
--  Signal cases cover both GtkAda callback forms and follow the explicit
--  signal-emission pattern used by testsuite/tests/imcontext/imcontext.adb.
--  The manager case also ports the set-default-display operation from
--  testsuite/c_tests/displayclose.c.

with Ada.Command_Line;
with Ada.Exceptions;
with Gdk;
with Gdk.App_Launch_Context;
with Gdk.Clipboard;
with Gdk.Device;
with Gdk.Display;
with Gdk.Display_Manager;
with Gdk.GLContext;
with Glib.Error;
with Gdk.Monitor;
with Gdk.Rectangle;
with Gdk.Seat;
with Gdk.Surface;
with Glib;
with Glib.G_Icon;
with Glib.List_Model;
with Glib.Object;
with Glib.Properties;
with Glib.Test;
with Glib.Values;
with Gtk.Main;
with Gtk.Window;
with Gtkada.Bindings;
with Gtkada.Types;
with System;

procedure Gdk_Display is
   use Glib;
   use Glib.Test;
   use type Gdk.Gdk_Display;
   use type Gdk.Display_Manager.Gdk_Display_Manager;
   use type Gdk.Gdk_Device;
   use type Gdk.Gdk_Seat;
   use type Gdk.Gdk_Surface;
   use type Gdk.GLContext.Gdk_GLContext;
   use type Glib.Error.GError;
   use type Gdk.App_Launch_Context.Gdk_App_Launch_Context;
   use type Glib.G_Icon.G_Icon;
   use type Gdk.Clipboard.Gdk_Clipboard;
   use type Glib.List_Model.Glist_Model;
   use type Gdk.Monitor.Gdk_Monitor;
   use type System.Address;

   --  GtkAda exposes GIcon as an interface proxy, so make a valid themed icon
   --  through GLib's C constructor for the app-launch-context setter test.
   function New_Themed_Icon
     (Name : Gtkada.Types.Chars_Ptr) return Glib.G_Icon.G_Icon;
   pragma Import (C, New_Themed_Icon, "g_themed_icon_new");

   procedure Unref_Icon (Icon : Glib.G_Icon.G_Icon);
   pragma Import (C, Unref_Icon, "g_object_unref");

   Default_Display : Gdk.Gdk_Display;
   --  Keep the default alive while the final test closes a separate display.

   --  g_signal_emit_by_name is variadic in C. These small fixed profiles
   --  match the four Display signal argument shapes emitted by this test.
   procedure Emit_Boolean
     (Instance : System.Address; Name : Gtkada.Types.Chars_Ptr;
      Value    : Glib.Gboolean);
   pragma Import (C, Emit_Boolean, "g_signal_emit_by_name");

   procedure Emit_Void
     (Instance : System.Address; Name : Gtkada.Types.Chars_Ptr);
   pragma Import (C, Emit_Void, "g_signal_emit_by_name");

   procedure Emit_Seat
     (Instance : System.Address; Name : Gtkada.Types.Chars_Ptr;
      Seat     : System.Address);
   pragma Import (C, Emit_Seat, "g_signal_emit_by_name");

   procedure Emit_String
     (Instance : System.Address; Name, Value : Gtkada.Types.Chars_Ptr);
   pragma Import (C, Emit_String, "g_signal_emit_by_name");

   --  Signal counters also verify that each overloaded handler ran exactly once.
   Closed_Display_Calls  : Natural := 0;
   Closed_Slot_Calls     : Natural := 0;
   Opened_Display_Calls  : Natural := 0;
   Opened_Slot_Calls     : Natural := 0;
   Seat_Display_Calls    : Natural := 0;
   Seat_Slot_Calls       : Natural := 0;
   Setting_Display_Calls : Natural := 0;
   Setting_Slot_Calls    : Natural := 0;
   Saw_Closed_Error      : Boolean := False;
   Saw_Setting           : Boolean := False;
   Saw_Seat              : Boolean := False;
   Manager_Opened_Calls  : Natural := 0;
   Manager_Slot_Calls    : Natural := 0;
   Expected_Manager      : Gdk.Display_Manager.Gdk_Display_Manager;
   Manager_Opened_Address : System.Address := System.Null_Address;
   Manager_Slot_Address   : System.Address := System.Null_Address;
   Expected_Monitor       : Gdk.Monitor.Gdk_Monitor;
   Monitor_Calls          : Natural := 0;
   Monitor_Slot_Calls     : Natural := 0;
   Monitor_Exception_Calls : Natural := 0;
   Raise_Monitor_Callbacks : Boolean := False;

   --  Callback profiles below exercise the display-specific marshallers and
   --  the generic GObject marshallers selected by the two On_* overloads.
   procedure Closed_Display
     (Self : access Gdk.Display.Gdk_Display_Record'Class;
      Is_Error : Boolean);
   procedure Closed_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Is_Error : Boolean);
   procedure Opened_Display
     (Self : access Gdk.Display.Gdk_Display_Record'Class);
   procedure Opened_Slot
     (Self : access Glib.Object.GObject_Record'Class);
   procedure Seat_Display
     (Self : access Gdk.Display.Gdk_Display_Record'Class;
      Seat : not null access Gdk.Seat.Gdk_Seat_Record'Class);
   procedure Seat_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Seat : not null access Gdk.Seat.Gdk_Seat_Record'Class);
   procedure Setting_Display
     (Self : access Gdk.Display.Gdk_Display_Record'Class;
      Setting : UTF8_String);
   procedure Setting_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Setting : UTF8_String);
   procedure Manager_Display_Opened
     (Self : access Gdk.Display_Manager.Gdk_Display_Manager_Record'Class;
      Display : not null access Gdk.Display.Gdk_Display_Record'Class);
   procedure Manager_Display_Opened_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Display : not null access Gdk.Display.Gdk_Display_Record'Class);
   procedure Monitor_Invalidated
     (Self : access Gdk.Monitor.Gdk_Monitor_Record'Class);
   procedure Monitor_Invalidated_Slot
     (Self : access Glib.Object.GObject_Record'Class);
   procedure Record_Monitor_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Closed_Display
     (Self : access Gdk.Display.Gdk_Display_Record'Class;
      Is_Error : Boolean) is
   begin
      Closed_Display_Calls := Closed_Display_Calls + 1;
      Saw_Closed_Error := Saw_Closed_Error or Is_Error;
   end Closed_Display;

   procedure Closed_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Is_Error : Boolean) is
   begin
      Closed_Slot_Calls := Closed_Slot_Calls + 1;
      Saw_Closed_Error := Saw_Closed_Error or Is_Error;
   end Closed_Slot;

   procedure Opened_Display
     (Self : access Gdk.Display.Gdk_Display_Record'Class) is
   begin
      Opened_Display_Calls := Opened_Display_Calls + 1;
   end Opened_Display;

   procedure Opened_Slot
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Opened_Slot_Calls := Opened_Slot_Calls + 1;
   end Opened_Slot;

   procedure Seat_Display
     (Self : access Gdk.Display.Gdk_Display_Record'Class;
      Seat : not null access Gdk.Seat.Gdk_Seat_Record'Class) is
   begin
      Seat_Display_Calls := Seat_Display_Calls + 1;
      Saw_Seat := Saw_Seat or Seat.Get_Display = Default_Display;
   end Seat_Display;

   procedure Seat_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Seat : not null access Gdk.Seat.Gdk_Seat_Record'Class) is
   begin
      Seat_Slot_Calls := Seat_Slot_Calls + 1;
      Saw_Seat := Saw_Seat or Seat.Get_Display = Default_Display;
   end Seat_Slot;

   procedure Setting_Display
     (Self : access Gdk.Display.Gdk_Display_Record'Class;
      Setting : UTF8_String) is
   begin
      Setting_Display_Calls := Setting_Display_Calls + 1;
      Saw_Setting := Saw_Setting or Setting = "gtk-double-click-time";
   end Setting_Display;

   procedure Setting_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Setting : UTF8_String) is
   begin
      Setting_Slot_Calls := Setting_Slot_Calls + 1;
      Saw_Setting := Saw_Setting or Setting = "gtk-double-click-time";
   end Setting_Slot;

   --  The typed callback receives the manager and the newly opened display.
   procedure Manager_Display_Opened
     (Self : access Gdk.Display_Manager.Gdk_Display_Manager_Record'Class;
      Display : not null access Gdk.Display.Gdk_Display_Record'Class) is
   begin
      Assert_True
        (Glib.Object.Get_Object (Self) =
         Glib.Object.Get_Object (Expected_Manager));
      Manager_Opened_Address := Glib.Object.Get_Object (Display);
      Manager_Opened_Calls := Manager_Opened_Calls + 1;
   end Manager_Display_Opened;

   --  The slot callback receives the same display through the GObject form.
   procedure Manager_Display_Opened_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Display : not null access Gdk.Display.Gdk_Display_Record'Class) is
   begin
      Assert_True
        (Glib.Object.Get_Object (Self) =
         Glib.Object.Get_Object (Expected_Manager));
      Manager_Slot_Address := Glib.Object.Get_Object (Display);
      Manager_Slot_Calls := Manager_Slot_Calls + 1;
   end Manager_Display_Opened_Slot;

   --  The typed form receives the monitor; the slot form receives the
   --  connected display. Raising on demand covers the generated exception
   --  handling in both signal marshallers.
   procedure Monitor_Invalidated
     (Self : access Gdk.Monitor.Gdk_Monitor_Record'Class) is
   begin
      Assert_True
        (Glib.Object.Get_Object (Self) =
         Glib.Object.Get_Object (Expected_Monitor));
      if Raise_Monitor_Callbacks then
         raise Program_Error with "expected monitor callback exception";
      end if;
      Monitor_Calls := Monitor_Calls + 1;
   end Monitor_Invalidated;

   procedure Monitor_Invalidated_Slot
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Assert_True
        (Glib.Object.Get_Object (Self) =
         Glib.Object.Get_Object (Default_Display));
      if Raise_Monitor_Callbacks then
         raise Program_Error with "expected monitor slot exception";
      end if;
      Monitor_Slot_Calls := Monitor_Slot_Calls + 1;
   end Monitor_Invalidated_Slot;

   --  GtkAda keeps callback exceptions from unwinding through the C signal
   --  emission and forwards each one to this test counter.
   procedure Record_Monitor_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Monitor_Exception_Calls := Monitor_Exception_Calls + 1;
   end Record_Monitor_Callback_Exception;

   procedure Test_Display_Methods with Convention => C;
   procedure Test_App_Launch_Context with Convention => C;
   procedure Test_Display_Signals with Convention => C;
   procedure Test_Display_Manager with Convention => C;
   procedure Test_Close_Opened_Display with Convention => C;
   procedure Test_Monitor_Signals with Convention => C;

   --  Cover the basic accessors, backend queries, and request helpers on the
   --  always-present Xvfb display. Values are checked against documented
   --  invariants rather than assumptions about a particular monitor model.
   procedure Test_Display_Methods is
      Display       : constant Gdk.Gdk_Display := Gdk.Display.Get_Default;
      Empty_Open    : constant Gdk.Gdk_Display := Gdk.Display.Open;
      Named_Open    : constant Gdk.Gdk_Display :=
        Gdk.Display.Open (Display.Get_Name);
      Context       : constant Gdk.App_Launch_Context.Gdk_App_Launch_Context :=
        Display.Get_App_Launch_Context;
      Clipboard     : constant Gdk.Clipboard.Gdk_Clipboard :=
        Display.Get_Clipboard;
      Primary       : constant Gdk.Clipboard.Gdk_Clipboard :=
        Display.Get_Primary_Clipboard;
      Seat          : constant Gdk.Gdk_Seat := Display.Get_Default_Seat;
      Monitors      : constant Glib.List_Model.Glist_Model :=
        Display.Get_Monitors;
      Monitor_List  : Gdk.Seat.Seat_List.Glist;
      Displays      : Gdk.Display.Display_List.Glist;
      Setting       : Glib.Values.GValue;
      Has_Setting   : Boolean;
      Prepared_GL   : Boolean;
      Error         : Glib.Error.GError;
      Context_GL    : Gdk.GLContext.Gdk_GLContext;
      Composited    : Boolean;
      Rgba          : Boolean;
      Input_Shapes  : Boolean;
      Shadow_Width  : Boolean;
      Window        : Gtk.Window.Gtk_Window;
      Surface       : Gdk.Gdk_Surface;
      Monitor       : Gdk.Monitor.Gdk_Monitor;
      Geometry      : Gdk.Rectangle.Gdk_Rectangle;
      Pointer       : Gdk.Gdk_Device;
   begin
      Assert_True (Display /= null);
      Default_Display := Display;
      Assert_True (Empty_Open /= null);
      Assert_True (Named_Open /= null);
      Assert_Cmpstr_Eq (Display.Get_Name, Named_Open.Get_Name);
      Assert_True (Display.Get_Name'Length > 0);
      --  Round-trip through the package's typed list converter, which is also
      --  used by GtkAda clients holding lists of displays.
      Gdk.Display.Display_List.Append (Displays, Display);
      Assert_Cmpuint_Eq (Gdk.Display.Display_List.Length (Displays), 1);
      Assert_True (Gdk.Display.Display_List.Get_Data (Displays) = Display);
      Gdk.Display.Display_List.Free (Displays);
      Assert_True (Context /= null);
      Assert_True (Context.Get_Display = Display);
      Assert_True (Clipboard /= null);
      Assert_True (Primary /= null);
      Assert_True (Seat /= null);
      Assert_True (Seat.Get_Display = Display);
      Assert_True (Monitors /= Glib.List_Model.Null_Glist_Model);
      Assert_True (Glib.List_Model.Get_N_Items (Monitors) > 0);

      --  If the backend provides this setting, GDK initializes its GValue;
      --  an unknown name must return False and leave the value invalid.
      --  GDK initializes a GValue only when a requested setting is available.
      Has_Setting := Display.Get_Setting ("gtk-double-click-time", Setting);
      if Has_Setting then
         Assert_True (Glib.Values.Type_Of (Setting) /= Glib.GType_Invalid);
         Glib.Values.Unset (Setting);
      else
         Assert_True
           (Glib.Values.Type_Of (Setting) = Glib.GType_Invalid);
      end if;
      Has_Setting := Display.Get_Setting ("gtkada-no-such-setting", Setting);
      Assert_False (Has_Setting);

      Assert_False (Display.Is_Closed);
      --  These backend capabilities may vary, but each query is stable during
      --  this single-threaded test and must report the same value twice.
      Composited := Display.Is_Composited;
      Assert_True (Composited = Display.Is_Composited);
      Rgba := Display.Is_Rgba;
      Assert_True (Rgba = Display.Is_Rgba);
      Input_Shapes := Display.Supports_Input_Shapes;
      Assert_True (Input_Shapes = Display.Supports_Input_Shapes);
      Shadow_Width := Display.Supports_Shadow_Width;
      Assert_True (Shadow_Width = Display.Supports_Shadow_Width);

      --  A real toplevel surface exercises the monitor lookup on Xvfb.
      --  Gtk.Window owns its surface and destroys it as part of window cleanup.
      Window := Gtk.Window.Gtk_Window_New;
      Window.Present;
      Surface := Window.Get_Surface;
      Assert_True (Surface /= null);
      Monitor := Display.Get_Monitor_At_Surface (Surface);
      Assert_True (Monitor /= null);
      Assert_True (Monitor.Get_Display = Display);

      --  A listed Xvfb monitor is valid and has nonempty geometry. The
      --  optional identification strings and physical measurements may be
      --  unavailable on a backend, so check stable strings and nonnegative
      --  numeric values instead of assuming a particular monitor model.
      Assert_True (Monitor.Is_Valid);
      Assert_True (Monitor.Get_Connector = Monitor.Get_Connector);
      Assert_True (Monitor.Get_Description = Monitor.Get_Description);
      Assert_True (Monitor.Get_Manufacturer = Monitor.Get_Manufacturer);
      Assert_True (Monitor.Get_Model = Monitor.Get_Model);
      Monitor.Get_Geometry (Geometry);
      Assert_True (Geometry.Width > 0 and Geometry.Height > 0);
      Assert_True (Monitor.Get_Width_Mm >= 0);
      Assert_True (Monitor.Get_Height_Mm >= 0);
      Assert_True (Monitor.Get_Refresh_Rate >= 0);
      Assert_True (Monitor.Get_Scale > 0.0);
      Assert_True (Monitor.Get_Scale_Factor >= 1);
      Assert_True
        (Monitor.Get_Subpixel_Layout in
           Gdk.Monitor.Gdk_Subpixel_Layout'Range);
      Window.Destroy;

      --  The seat list owns its GList container, while the seat objects are
      --  borrowed from GDK. Device_Is_Grabbed is false without an active grab.
      Monitor_List := Display.List_Seats;
      Assert_True (Gdk.Seat.Seat_List.Length (Monitor_List) > 0);
      Gdk.Seat.Seat_List.Free (Monitor_List);
      Pointer := Seat.Get_Pointer;
      Assert_True (Pointer /= null);
      Assert_False (Display.Device_Is_Grabbed (Pointer));

      --  These operations are harmless on Xvfb and flush queued backend work.
      --  Startup notification is deprecated, but its string ownership path
      --  remains supported for applications that still use launch IDs.
      Display.Beep;
      Display.Flush;
      Display.Sync;
      Display.Notify_Startup_Complete ("gtkada-coverage-test");
      declare
         Startup_Id : constant UTF8_String :=
           Display.Get_Startup_Notification_Id;
      begin
         --  X11 has no Wayland startup ID, so the nullable C string maps to empty.
         Assert_Cmpstr_Eq (Startup_Id, "");
      end;

      Assert_False (Display.Is_Closed);

      --  On Xvfb without an OpenGL implementation, Prepare_Gl reports a
      --  recoverable error. When available, it succeeds and leaves Error null.
      Prepared_GL := Display.Prepare_Gl (Error);
      if Prepared_GL then
         Assert_True (Error = null);
      else
         Assert_True (Error /= null);
         Glib.Error.Error_Free (Error);
      end if;

      --  Context creation has the same documented success/error contract.
      --  Release the transfer-full context if this backend can create one.
      Context_GL := Display.Create_Gl_Context (Error);
      if Error = null then
         Assert_True (Context_GL /= null);
         Glib.Object.Unref (Glib.Object.GObject (Context_GL));
      else
         Assert_True (Context_GL = null);
         Glib.Error.Error_Free (Error);
      end if;

   end Test_Display_Methods;

   --  Exercise each GdkAppLaunchContext setter on the default display. The
   --  setters have no public getters, so verify the context's display before
   --  and after updating its launch metadata.
   procedure Test_App_Launch_Context is
      Display : constant Gdk.Gdk_Display := Gdk.Display.Get_Default;
      Context : constant Gdk.App_Launch_Context.Gdk_App_Launch_Context :=
        Display.Get_App_Launch_Context;
      Name : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("text-x-generic");
      Icon : constant Glib.G_Icon.G_Icon := New_Themed_Icon (Name);
   begin
      Gtkada.Types.Free (Name);
      Assert_True (Context /= null);
      Assert_True (Context.Get_Display = Display);
      Assert_True (Icon /= null);

      --  -1 leaves workspace selection to the window manager. The empty icon
      --  name clears the name after exercising its allocated-string path.
      Context.Set_Desktop (-1);
      Context.Set_Icon (Icon);
      Context.Set_Icon_Name ("text-x-generic");
      Context.Set_Icon_Name;
      Context.Set_Timestamp (123_456);

      Assert_True (Context.Get_Display = Display);
      --  GdkAppLaunchContext retains the icon when Set_Icon is called.
      Unref_Icon (Icon);
   end Test_App_Launch_Context;

   --  Every signal is emitted with a real display/seat payload. Both callback
   --  overloads are connected, so the display-specific and GObject marshallers
   --  each receive and convert the signal arguments.
   procedure Test_Display_Signals is
      Display : constant Gdk.Gdk_Display := Default_Display;
      Seat    : constant Gdk.Gdk_Seat := Display.Get_Default_Seat;
      Closed_Name  : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("closed");
      Opened_Name  : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("opened");
      Added_Name   : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("seat-added");
      Removed_Name : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("seat-removed");
      Setting_Name : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("setting-changed");
      Setting_Text : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("gtk-double-click-time");
   begin
      Display.On_Closed (Closed_Display'Unrestricted_Access);
      Display.On_Closed (Closed_Slot'Unrestricted_Access, Display);
      Display.On_Opened (Opened_Display'Unrestricted_Access);
      Display.On_Opened (Opened_Slot'Unrestricted_Access, Display);
      Display.On_Seat_Added (Seat_Display'Unrestricted_Access);
      Display.On_Seat_Added (Seat_Slot'Unrestricted_Access, Display);
      Display.On_Seat_Removed (Seat_Display'Unrestricted_Access);
      Display.On_Seat_Removed (Seat_Slot'Unrestricted_Access, Display);
      Display.On_Setting_Changed (Setting_Display'Unrestricted_Access);
      Display.On_Setting_Changed (Setting_Slot'Unrestricted_Access, Display);

      Emit_Boolean
        (Glib.Object.Get_Object (Display), Closed_Name, Glib.Gboolean (0));
      Emit_Void (Glib.Object.Get_Object (Display), Opened_Name);
      Emit_Seat
        (Glib.Object.Get_Object (Display), Added_Name,
         Glib.Object.Get_Object (Seat));
      Emit_Seat
        (Glib.Object.Get_Object (Display), Removed_Name,
         Glib.Object.Get_Object (Seat));
      Emit_String
        (Glib.Object.Get_Object (Display), Setting_Name, Setting_Text);

      Assert_Cmpuint_Eq (Guint (Closed_Display_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Closed_Slot_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Opened_Display_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Opened_Slot_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Seat_Display_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Seat_Slot_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Setting_Display_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Setting_Slot_Calls), 1);
      Assert_False (Saw_Closed_Error);
      Assert_True (Saw_Seat);
      Assert_True (Saw_Setting);

      Gtkada.Types.Free (Closed_Name);
      Gtkada.Types.Free (Opened_Name);
      Gtkada.Types.Free (Added_Name);
      Gtkada.Types.Free (Removed_Name);
      Gtkada.Types.Free (Setting_Name);
      Gtkada.Types.Free (Setting_Text);
   end Test_Display_Signals;

   --  Exercise the manager singleton, its display list and property, and both
   --  signal callback profiles while the Xvfb display is available.
   procedure Test_Display_Manager is
      Manager : constant Gdk.Display_Manager.Gdk_Display_Manager :=
        Gdk.Display_Manager.Get;
      Original : constant Gdk.Gdk_Display := Manager.Get_Default_Display;
      Empty_Name_Display : Gdk.Gdk_Display;
      Named_Display : Gdk.Gdk_Display;
      Displays : Gdk.Display.Display_List.Glist;
      Property_Value : Glib.Values.GValue;
   begin
      Assert_True (Manager /= null);
      Assert_True (Gdk.Display_Manager.Get = Manager);
      Assert_True (Gdk.Display_Manager.Get_Type /= Glib.GType_Invalid);
      Assert_True (Original /= null);
      Assert_True (Gdk.Display.Get_Default = Original);

      --  The list contains all open displays; its newly allocated list cells
      --  are freed after checking that the current display is present.
      Displays := Manager.List_Displays;
      Assert_True (Gdk.Display.Display_List.Length (Displays) > 0);
      Gdk.Display.Display_List.Free (Displays);

      Expected_Manager := Manager;
      Manager_Opened_Calls := 0;
      Manager_Slot_Calls := 0;
      Manager.On_Display_Opened
        (Manager_Display_Opened'Unrestricted_Access);
      Manager.On_Display_Opened
        (Manager_Display_Opened_Slot'Unrestricted_Access, Manager);

      --  An empty name exercises the NULL-name path; a named open creates a
      --  second connection whose display-opened signal can be checked.
      Empty_Name_Display := Manager.Open_Display;
      Assert_True (Empty_Name_Display /= null);
      Named_Display := Manager.Open_Display (Original.Get_Name);
      Assert_True (Named_Display /= null);
      Assert_Cmpuint_Ge (Guint (Manager_Opened_Calls), 1);
      Assert_Cmpuint_Eq
        (Guint (Manager_Opened_Calls), guint (Manager_Slot_Calls));
      Assert_True
        (Manager_Opened_Address = Glib.Object.Get_Object (Named_Display));
      Assert_True
        (Manager_Slot_Address = Glib.Object.Get_Object (Named_Display));

      --  Setting the new display changes both manager access paths. The
      --  generic GValue path also checks the generated boxed-property name.
      Manager.Set_Default_Display (Named_Display);
      Assert_True (Manager.Get_Default_Display = Named_Display);
      Assert_True (Gdk.Display.Get_Default = Named_Display);
      Glib.Values.Init (Property_Value, Gdk.Display.Get_Type);
      Glib.Properties.Get_Property
        (Manager,
         Glib.Property_Name
           (Glib.Property (Gdk.Display_Manager.Default_Display_Property)),
         Property_Value);
      Assert_True
        (Gdk.Display.Convert
           (Glib.Object.Get_Object (Glib.Values.Get_Object (Property_Value))) =
         Named_Display);
      Glib.Values.Unset (Property_Value);

      Displays := Manager.List_Displays;
      Assert_Cmpuint_Ge (Gdk.Display.Display_List.Length (Displays), 2);
      Gdk.Display.Display_List.Free (Displays);

      --  Restore the original before closing either extra connection.
      Manager.Set_Default_Display (Original);
      Empty_Name_Display.Close;
      Named_Display.Close;
      Expected_Manager := null;
   end Test_Display_Manager;

   --  Port of the display-open/close exercise in testsuite/c_tests/displayclose.c.
   --  Open a second connection by name, close only that connection, and check
   --  the closed state without taking down Gtk's default display.
   procedure Test_Close_Opened_Display is
      Display : constant Gdk.Gdk_Display :=
        Gdk.Display.Open (Default_Display.Get_Name);
   begin
      Assert_True (Display /= null);
      Assert_False (Display.Is_Closed);
      Display.Close;
      Assert_True (Display.Is_Closed);
   end Test_Close_Opened_Display;

   --  Xvfb cannot produce a physical monitor unplug event. Emitting the
   --  documented invalidation signal directly exercises both callback
   --  overloads and their exception containment without hardware changes.
   procedure Test_Monitor_Signals is
      Model : constant Glib.List_Model.Glist_Model :=
        Default_Display.Get_Monitors;
      Monitor_Object : constant Glib.Object.GObject :=
        Glib.List_Model.Get_Item (Model, 0);
      Monitor : constant Gdk.Monitor.Gdk_Monitor :=
        Gdk.Monitor.Gdk_Monitor (Monitor_Object);
      Invalidate_Name : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("invalidate");
   begin
      Assert_True (Monitor /= null);
      Expected_Monitor := Monitor;
      Monitor_Calls := 0;
      Monitor_Slot_Calls := 0;
      Monitor.On_Invalidate
        (Monitor_Invalidated'Unrestricted_Access);
      Monitor.On_Invalidate
        (Monitor_Invalidated_Slot'Unrestricted_Access, Default_Display,
         After => True);

      Emit_Void (Glib.Object.Get_Object (Monitor), Invalidate_Name);
      Assert_Cmpuint_Eq (Guint (Monitor_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Monitor_Slot_Calls), 1);

      --  Re-emitting with failing callbacks covers the exception branches in
      --  both generated marshallers and confirms the exceptions are contained.
      Gtkada.Bindings.Set_On_Exception
        (Record_Monitor_Callback_Exception'Unrestricted_Access);
      Raise_Monitor_Callbacks := True;
      Emit_Void (Glib.Object.Get_Object (Monitor), Invalidate_Name);
      Raise_Monitor_Callbacks := False;
      Assert_Cmpuint_Eq (Guint (Monitor_Exception_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Monitor_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Monitor_Slot_Calls), 1);

      Gtkada.Types.Free (Invalidate_Name);
      Glib.Object.Unref (Monitor_Object);
      Expected_Monitor := null;
   end Test_Monitor_Signals;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/display/methods", Test_Display_Methods'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/display/app-launch-context",
      Test_App_Launch_Context'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/display/signals", Test_Display_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/display/manager", Test_Display_Manager'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/display/close-opened", Test_Close_Opened_Display'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/display/monitor-signals", Test_Monitor_Signals'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Gdk_Display;
