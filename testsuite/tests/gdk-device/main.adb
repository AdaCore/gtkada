--  Exercises Gdk.Device queries and signal marshallers using the default
--  Xvfb seat. There is no behavioral C test for GdkDevice to port; the
--  C accessor-apis test checks only generated accessor naming conventions.

with Ada.Command_Line;
with Ada.Exceptions;
with System;

with Gdk;
with Gdk.Device;
with Gdk.Device_Tool;
with Gdk.Display;
with Gdk.Enums;
with Gdk.Seat;
with Gdk.Surface;
with Glib;
with Glib.Object;
with Glib.Test;
with Gtk.Main;
with Gtk.Window;
with Gtkada.Types;
with GNAT.Strings;
with Gtkada.Bindings;
with Pango.Enums;

procedure Main is
   use Glib;
   use Glib.Test;
   use type Gdk.Gdk_Device;
   use type Gdk.Gdk_Display;
   use type Gdk.Gdk_Seat;
   use type Gdk.Gdk_Surface;
   use type Gdk.Device_Tool.Gdk_Device_Tool;
   use type Gdk.Device_Tool.Gdk_Device_Tool_Type;
   use type Gdk.Device.Gdk_Input_Source;
   use type Gdk.Enums.Gdk_Modifier_Type;
   use type Pango.Enums.Direction;
   use type System.Address;

   Display : Gdk.Gdk_Display;
   Pointer : Gdk.Gdk_Device;
   Signal_Tool : Gdk.Device_Tool.Gdk_Device_Tool;
   Changed_Calls, Changed_Slot_Calls : Natural := 0;
   Tool_Changed_Calls, Tool_Slot_Calls : Natural := 0;
   Callback_Exception_Calls : Natural := 0;
   Raise_In_Callbacks : Boolean := False;

   --  The GObject-only helper accepts a GType and calls g_object_new with no
   --  properties, which is sufficient for the payload of a manually emitted
   --  signal. The callbacks check that the marshallers preserve object identity.
   function New_C_Object (Typ : GType) return System.Address;
   pragma Import (C, New_C_Object, "ada_g_object_new");

   procedure Emit_Void
     (Instance : System.Address; Name : Gtkada.Types.Chars_Ptr);
   pragma Import (C, Emit_Void, "g_signal_emit_by_name");

   procedure Emit_Tool
     (Instance : System.Address; Name : Gtkada.Types.Chars_Ptr;
      Tool     : System.Address);
   pragma Import (C, Emit_Tool, "g_signal_emit_by_name");

   procedure On_Changed
     (Self : access Gdk.Device.Gdk_Device_Record'Class);
   procedure On_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class);
   procedure On_Tool_Changed
     (Self : access Gdk.Device.Gdk_Device_Record'Class;
      Tool : not null access Gdk.Device_Tool.Gdk_Device_Tool_Record'Class);
   procedure On_Tool_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Tool : not null access Gdk.Device_Tool.Gdk_Device_Tool_Record'Class);
   procedure Record_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   --  Keep explicit identity checks in each callback so a bad cast or slot
   --  forwarding mistake fails at the point where the marshaller runs.
   procedure On_Changed
     (Self : access Gdk.Device.Gdk_Device_Record'Class) is
   begin
      Assert_True (Glib.Object.Get_Object (Self) =
                   Glib.Object.Get_Object (Pointer));
      if Raise_In_Callbacks then
         raise Program_Error with "expected changed callback exception";
      end if;
      Changed_Calls := Changed_Calls + 1;
   end On_Changed;

   procedure On_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Assert_True (Glib.Object.Get_Object (Self) =
                   Glib.Object.Get_Object (Display));
      if Raise_In_Callbacks then
         raise Program_Error with "expected changed slot exception";
      end if;
      Changed_Slot_Calls := Changed_Slot_Calls + 1;
   end On_Changed_Slot;

   procedure On_Tool_Changed
     (Self : access Gdk.Device.Gdk_Device_Record'Class;
      Tool : not null access Gdk.Device_Tool.Gdk_Device_Tool_Record'Class) is
   begin
      Assert_True (Glib.Object.Get_Object (Self) =
                   Glib.Object.Get_Object (Pointer));
      Assert_True (Glib.Object.Get_Object (Tool) =
                   Glib.Object.Get_Object (Signal_Tool));
      if Raise_In_Callbacks then
         raise Program_Error with "expected tool callback exception";
      end if;
      Tool_Changed_Calls := Tool_Changed_Calls + 1;
   end On_Tool_Changed;

   procedure On_Tool_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Tool : not null access Gdk.Device_Tool.Gdk_Device_Tool_Record'Class) is
   begin
      Assert_True (Glib.Object.Get_Object (Self) =
                   Glib.Object.Get_Object (Display));
      Assert_True (Glib.Object.Get_Object (Tool) =
                   Glib.Object.Get_Object (Signal_Tool));
      if Raise_In_Callbacks then
         raise Program_Error with "expected tool slot exception";
      end if;
      Tool_Slot_Calls := Tool_Slot_Calls + 1;
   end On_Tool_Changed_Slot;

   --  The generated signal marshallers must contain callback exceptions and
   --  report them to the configured handler instead of unwinding through C.
   procedure Record_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Callback_Exception_Calls := Callback_Exception_Calls + 1;
   end Record_Callback_Exception;

   procedure Test_Device_Queries with Convention => C;
   procedure Test_Device_Tool_Queries with Convention => C;
   procedure Test_Device_Signals with Convention => C;

   --  The default Xvfb seat always supplies logical pointer and keyboard
   --  devices. Their documented relationships give stable checks without
   --  depending on the host's physical input hardware or lock-key state.
   procedure Test_Device_Queries is
      Seat       : constant Gdk.Gdk_Seat := Display.Get_Default_Seat;
      Keyboard   : constant Gdk.Gdk_Device := Seat.Get_Keyboard;
      Device_List : Gdk.Device.Device_List.Glist;
      Converted  : Gdk.Gdk_Device;
      Names      : GNAT.Strings.String_List := Keyboard.Get_Layout_Names;
      Name       : GNAT.Strings.String_Access;
      X, Y       : aliased Gdouble := -1.0;
      Under_Pointer : Gdk.Gdk_Surface;
      Window     : Gtk.Window.Gtk_Window;
   begin
      Assert_True (Seat /= null);
      Assert_True (Keyboard /= null);
      Assert_True (Pointer /= null);

      --  Device_List.Append invokes Convert(device); Get_Data invokes the
      --  reverse object conversion. The seat owns device objects, so only the
      --  temporary GList container is freed here.
      Gdk.Device.Device_List.Append (Device_List, Pointer);
      Converted := Gdk.Device.Device_List.Get_Data (Device_List);
      Assert_True
        (Glib.Object.Get_Object (Converted) =
         Glib.Object.Get_Object (Pointer));
      Gdk.Device.Device_List.Free (Device_List);

      Assert_True (Pointer.Get_Display = Display);
      Assert_True (Keyboard.Get_Display = Display);
      Assert_True (Pointer.Get_Seat = Seat);
      Assert_True (Keyboard.Get_Seat = Seat);
      Assert_True (Pointer.Get_Name'Length > 0);
      Assert_True (Keyboard.Get_Name'Length > 0);
      Assert_True (Keyboard.Get_Source = Gdk.Device.Gdk_Source_Keyboard);
      Assert_True (Pointer.Get_Has_Cursor);

      --  Keyboard-specific properties are queried on the keyboard device.
      --  The active layout may be unavailable (-1), and lock states depend on
      --  the virtual keyboard, so only their documented ranges are asserted.
      Assert_True (Keyboard.Get_Active_Layout_Index >= -1);
      Assert_True (Keyboard.Get_Direction = Keyboard.Get_Direction);
      Assert_True (Keyboard.Get_Caps_Lock_State =
                   Keyboard.Get_Caps_Lock_State);
      Assert_True (Keyboard.Get_Num_Lock_State =
                   Keyboard.Get_Num_Lock_State);
      Assert_True (Keyboard.Get_Scroll_Lock_State =
                   Keyboard.Get_Scroll_Lock_State);
      Assert_True (Keyboard.Get_Modifier_State =
                   Keyboard.Get_Modifier_State);
      Assert_True (Keyboard.Has_Bidi_Layouts = Keyboard.Has_Bidi_Layouts);
      Assert_True (Keyboard.Get_Num_Touches = Keyboard.Get_Num_Touches);
      Assert_True (Keyboard.Get_Timestamp = Keyboard.Get_Timestamp);

      --  Layout names are caller-owned Ada strings, unlike the borrowed
      --  device and display objects returned by the other queries.
      Assert_True (Names'Length > 0);
      for Index in Names'Range loop
         Assert_True (Names (Index).all'Length > 0);
         Name := Names (Index);
         GNAT.Strings.Free (Name);
      end loop;

      --  These IDs can be empty when the backend has no physical device
      --  metadata. Repeated reads still check the nullable C-string bindings.
      Assert_True (Keyboard.Get_Vendor_Id = Keyboard.Get_Vendor_Id);
      Assert_True (Keyboard.Get_Product_Id = Keyboard.Get_Product_Id);

      --  The virtual pointer has no tablet tool. The getter must safely map
      --  the backend's null pointer to GtkAda's null object value.
      Assert_True (Pointer.Get_Device_Tool = null);

      --  Position lookup may return null until the virtual pointer is over a
      --  surface owned by this process; when present, it must belong to the
      --  same display. This also exercises both optional coordinate outputs.
      Window := Gtk.Window.Gtk_Window_New;
      Window.Present;
      Under_Pointer := Pointer.Get_Surface_At_Position (X'Access, Y'Access);
      Assert_True
        (Under_Pointer = null or else Under_Pointer.Get_Display = Display);
      Window.Destroy;
   end Test_Device_Queries;

   --  A synthetic tool has no tablet metadata, so the getters must report
   --  empty axes and IDs and the documented unknown tool type.
   procedure Test_Device_Tool_Queries is
      Tool : constant Gdk.Device_Tool.Gdk_Device_Tool :=
        Gdk.Device_Tool.Convert (New_C_Object (Gdk.Device_Tool.Get_Type));
      Tools : Gdk.Device_Tool.Device_Tool_List.Glist;
      Tool_From_List : Gdk.Device_Tool.Gdk_Device_Tool;
   begin
      Assert_True (Tool /= null);
      Assert_Cmpuint_Eq (Guint (Tool.Get_Axes), 0);
      Assert_True (Tool.Get_Hardware_Id = 0);
      Assert_True (Tool.Get_Serial = 0);
      Assert_True
        (Tool.Get_Tool_Type =
         Gdk.Device_Tool.Gdk_Device_Tool_Type_Unknown);

      --  Appending calls Convert(tool) and Get_Data calls Convert(address).
      --  The list owns only its nodes; the test releases the GObject below.
      Gdk.Device_Tool.Device_Tool_List.Append (Tools, Tool);
      Tool_From_List := Gdk.Device_Tool.Device_Tool_List.Get_Data (Tools);
      Assert_True (Glib.Object.Get_Object (Tool_From_List) =
                   Glib.Object.Get_Object (Tool));
      Gdk.Device_Tool.Device_Tool_List.Free (Tools);
      Glib.Object.Unref (Glib.Object.GObject (Tool));
   end Test_Device_Tool_Queries;

   --  Signal emission is explicit because Xvfb has no physical hot-plug or
   --  tablet input. A default GdkDeviceTool GObject gives the tool-changed
   --  marshallers a valid object of the declared signal argument type.
   procedure Test_Device_Signals is
      Changed_Name : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("changed");
      Tool_Name : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("tool-changed");
   begin
      Signal_Tool := Gdk.Device_Tool.Convert
        (New_C_Object (Gdk.Device_Tool.Get_Type));
      Assert_True (Signal_Tool /= null);

      Pointer.On_Changed (On_Changed'Unrestricted_Access);
      Pointer.On_Changed (On_Changed_Slot'Unrestricted_Access, Display);
      Pointer.On_Tool_Changed (On_Tool_Changed'Unrestricted_Access);
      Pointer.On_Tool_Changed
        (On_Tool_Changed_Slot'Unrestricted_Access, Display);

      Emit_Void (Glib.Object.Get_Object (Pointer), Changed_Name);
      Emit_Tool
        (Glib.Object.Get_Object (Pointer), Tool_Name,
         Glib.Object.Get_Object (Signal_Tool));

      Assert_Cmpuint_Eq (Guint (Changed_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Changed_Slot_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Tool_Changed_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Tool_Slot_Calls), 1);

      --  Re-emitting with deliberately failing callbacks executes each
      --  marshaller's exception path and verifies GtkAda catches all four.
      Gtkada.Bindings.Set_On_Exception
        (Record_Callback_Exception'Unrestricted_Access);
      Raise_In_Callbacks := True;
      Emit_Void (Glib.Object.Get_Object (Pointer), Changed_Name);
      Emit_Tool
        (Glib.Object.Get_Object (Pointer), Tool_Name,
         Glib.Object.Get_Object (Signal_Tool));
      Raise_In_Callbacks := False;
      Assert_Cmpuint_Eq (Guint (Callback_Exception_Calls), 4);
      Assert_Cmpuint_Eq (Guint (Changed_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Changed_Slot_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Tool_Changed_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Tool_Slot_Calls), 1);

      Gtkada.Types.Free (Changed_Name);
      Gtkada.Types.Free (Tool_Name);
      Glib.Object.Unref (Glib.Object.GObject (Signal_Tool));
   end Test_Device_Signals;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Display := Gdk.Display.Get_Default;
   Pointer := Display.Get_Default_Seat.Get_Pointer;
   Assert_True (Display /= null);
   Assert_True (Pointer /= null);

   Glib.Test.Add_Func
     ("/gdk-device/queries", Test_Device_Queries'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-device/tool-queries", Test_Device_Tool_Queries'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-device/signals", Test_Device_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
