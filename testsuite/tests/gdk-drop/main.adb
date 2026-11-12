--  Exercise Gdk.Drop's accessors and asynchronous value-read callback wrapper. No C
--  test covers gdk_drop_*; the abstract base gets a test subclass carrying
--  valid local drag properties so no physical drag or platform backend is
--  needed.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with Gdk.Content_Formats;
with Gdk.Content_Provider;
with Gdk.Device;
with Gdk.Display;
with Gdk.Drag;
with Gdk.Drop;
with Gdk.Seat;
with Gdk.Surface;
with Glib;
with Glib.Main;
with Glib.Object;
with Glib.Test;
with Glib.Values;
with Gtk.Main;
with Gtk.Window;
with Interfaces.C;
with Interfaces.C.Strings;
with System;

procedure Main is
   use Glib.Test;
   use type Glib.GType;
   use type Glib.Gboolean;
   use type Gdk.Gdk_Device;
   use type Gdk.Gdk_Display;
   use type Gdk.Gdk_Seat;
   use type Gdk.Gdk_Surface;
   use type Gdk.Drag.Drag_Action;
   use type Gdk.Drag.Gdk_Drag;
   use type System.Address;

   --  This concrete Ada subtype makes GdkDrop's inherited, data-only methods
   --  callable while its backend-specific read/status implementation remains
   --  owned by GDK.
   type Test_Drop_Record is new Gdk.Drop.Gdk_Drop_Record with null record;
   type Test_Drop is access all Test_Drop_Record'Class;
   Drop_Class : aliased Glib.Object.Ada_GObject_Class :=
     Glib.Object.Uninitialized_Class;
   --  This unchecked conversion only widens the access type; the designated
   --  object is the Test_Drop_Record created above and derives from GdkDrop.
   function To_Gdk_Drop is new Ada.Unchecked_Conversion
     (Test_Drop, Gdk.Drop.Gdk_Drop);
   function To_Address is new Ada.Unchecked_Conversion
     (Glib.G_Async_Result, System.Address);

   type GValue_Array is array (Interfaces.C.size_t range <>) of aliased
     Glib.Values.GValue;
   pragma Convention (C, GValue_Array);

   procedure Test_Accessors_And_Read with Convention => C;

   function Test_Drop_Get_Type return Glib.GType is
   begin
      --  GdkDrop has private class data, so GtkAda sizes the subtype from
      --  the registered parent rather than duplicating any C structure.
      Glib.Object.Initialize_Class_Record
        (Ancestor     => Gdk.Drop.Get_Type,
         Class_Record => Drop_Class,
         Type_Name    => "GtkAdaCoverageDrop");
      return Drop_Class.The_Type;
   end Test_Drop_Get_Type;

   function New_Object_With_Properties
     (Object_Type  : Glib.GType;
      N_Properties : Glib.Guint;
      Names        : System.Address;
      Values       : System.Address) return System.Address;
   pragma Import
     (C, New_Object_With_Properties, "g_object_new_with_properties");

   function G_Task_Is_Valid
     (Result : System.Address;
      Source : System.Address) return Glib.Gboolean;
   pragma Import (C, G_Task_Is_Valid, "g_task_is_valid");

   --  GTest captures the runtime's critical diagnostic for the null callback
   --  case, which the Gdk GIR marks nullable but the linked library rejects.
   procedure G_Test_Expect_Message
     (Domain : Interfaces.C.Strings.chars_ptr;
      Level  : Interfaces.C.int;
      Pattern : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, G_Test_Expect_Message, "g_test_expect_message");
   procedure G_Test_Assert_Expected_Messages
     (Domain : Interfaces.C.Strings.chars_ptr;
      File   : Interfaces.C.Strings.chars_ptr;
      Line   : Interfaces.C.int;
      Func   : Interfaces.C.Strings.chars_ptr);
   pragma Import
     (C, G_Test_Assert_Expected_Messages,
      "g_test_assert_expected_messages_internal");

   --  The callback records its result here; assertions stay in the test
   --  thread instead of raising through GDK's C callback stack.
   Current_Drop       : Gdk.Drop.Gdk_Drop := null;
   Callback_Source   : System.Address := System.Null_Address;
   Callback_Completed : Boolean := False;
   Callback_Task_Valid : Boolean := False;

   procedure On_Value_Read
     (Source : access Glib.Object.GObject_Record'Class;
      Result : Glib.G_Async_Result);
   procedure On_Value_Read
     (Source : access Glib.Object.GObject_Record'Class;
      Result : Glib.G_Async_Result) is
   begin
      Callback_Source := Glib.Object.Get_Object (Source);
      Callback_Task_Valid := G_Task_Is_Valid
        (To_Address (Result), Glib.Object.Get_Object (Current_Drop)) /= 0;
      Callback_Completed := True;
   end On_Value_Read;

   --  The local GdkDrag provider lets GdkDrop satisfy this value read without
   --  entering any display-server-specific drop-transfer code.
   procedure Test_Accessors_And_Read is
      Window       : Gtk.Window.Gtk_Window := Gtk.Window.Gtk_Window_New;
      Display      : Gdk.Gdk_Display := Gdk.Display.Get_Default;
      Seat         : Gdk.Gdk_Seat;
      Pointer      : Gdk.Gdk_Device;
      Surface      : Gdk.Gdk_Surface;
      Drag         : Gdk.Drag.Gdk_Drag;
      Provider     : Gdk.Content_Provider.Gdk_Content_Provider;
      Formats      : Gdk.Content_Formats.Gdk_Content_Formats;
      Drop         : Test_Drop := new Test_Drop_Record;
      Stored_Drop  : Gdk.Drop.Gdk_Drop;
      Input_Value  : Glib.Values.GValue;
      Values       : aliased GValue_Array (1 .. 5);
      Names        : Interfaces.C.Strings.chars_ptr_array (1 .. 5);
      Actions_Type : Glib.GType;
      Raw_Drop     : System.Address;
   begin
      --  A presented window supplies a genuine surface; its default seat
      --  supplies a device tied to the same display for the test subclass.
      Window.Present;
      Surface := Window.Get_Surface;
      Seat := Display.Get_Default_Seat;
      Pointer := Seat.Get_Pointer;
      Assert_True (Display /= null and then Seat /= null);
      Assert_True (Pointer /= null and then Surface /= null);

      --  The provider owns a copy of this GValue and exposes it to local drag
      --  reads after the temporary value is unset.
      Glib.Values.Init_Set_String (Input_Value, "gdk-drop test payload");
      Provider := Gdk.Content_Provider.Gdk_Content_Provider_New_For_Value
        (Input_Value);
      Glib.Values.Unset (Input_Value);
      Drag := Gdk.Drag.Begin_Drag
        (Surface, Pointer, Provider, Gdk.Drag.Gdk_Action_Copy, 0.0, 0.0);
      Formats := Gdk.Content_Formats.Gdk_Content_Formats_New_For_Gtype
        (Glib.GType_String);

      --  Construct-only GdkDrop properties are supplied through GValue just
      --  as a backend subclass would supply them during object construction.
      Actions_Type := Glib.Type_From_Name ("GdkDragAction");
      Assert_True (Actions_Type /= Glib.GType_Invalid);
      Names :=
        (1 => Interfaces.C.Strings.New_String ("actions"),
         2 => Interfaces.C.Strings.New_String ("device"),
         3 => Interfaces.C.Strings.New_String ("drag"),
         4 => Interfaces.C.Strings.New_String ("formats"),
         5 => Interfaces.C.Strings.New_String ("surface"));
      Glib.Values.Init (Values (1), Actions_Type);
      Glib.Values.Set_Flags
        (Values (1), Glib.Guint (Gdk.Drag.Gdk_Action_Copy));
      Glib.Values.Init (Values (2), Gdk.Device.Get_Type);
      Glib.Values.Set_Object (Values (2), Pointer);
      Glib.Values.Init (Values (3), Gdk.Drag.Get_Type);
      Glib.Values.Set_Object (Values (3), Drag);
      Glib.Values.Init (Values (4), Gdk.Content_Formats.Get_Type);
      Glib.Values.Set_Boxed
        (Values (4), Glib.Get_Object (Formats));
      Glib.Values.Init (Values (5), Gdk.Surface.Get_Type);
      Glib.Values.Set_Object (Values (5), Surface);

      Raw_Drop := New_Object_With_Properties
        (Test_Drop_Get_Type, 5, Names'Address, Values'Address);
      --  Set_Object associates GtkAda's wrapper with the full C reference, so
      --  releasing Stored_Drop below also releases this heap-allocated Ada
      --  wrapper through GtkAda's GObject qdata hook.
      Glib.Object.Set_Object (Drop, Raw_Drop);
      Stored_Drop := To_Gdk_Drop (Drop);
      Current_Drop := Stored_Drop;

      --  Each getter must preserve the object or content format assigned at
      --  construction, including GtkAda's conversion back to Ada handles.
      Assert_True (Stored_Drop.Get_Actions = Gdk.Drag.Gdk_Action_Copy);
      Assert_True (Stored_Drop.Get_Device = Pointer);
      Assert_True (Stored_Drop.Get_Display = Display);
      Assert_True (Stored_Drop.Get_Drag = Drag);
      Assert_True
        (Gdk.Content_Formats.Contain_Gtype
           (Stored_Drop.Get_Formats, Glib.GType_String));
      Assert_True (Stored_Drop.Get_Surface = Surface);

      --  Status accepts an action and its preferred member while the base
      --  class keeps the backend-independent action property readable.
      Stored_Drop.Status
        (Gdk.Drag.Gdk_Action_Copy or Gdk.Drag.Gdk_Action_Move,
         Gdk.Drag.Gdk_Action_Copy);
      Assert_True (Stored_Drop.Get_Actions = Gdk.Drag.Gdk_Action_Copy);

      --  A successful async local read reaches Ada's callback conversion. The
      --  result is also a GTask for this exact drop, as required by the finish
      --  API; Read_Value_Finish itself is excluded due to the binding issue
      --  recorded in the unit notes.
      Stored_Drop.Read_Value_Async
        (Glib.GType_String, 0, null,
         On_Value_Read'Unrestricted_Access);
      while not Callback_Completed loop
         Assert_True (Glib.Main.Main_Context_Iteration (May_Block => True));
      end loop;
      Assert_True (Callback_Source = Glib.Object.Get_Object (Stored_Drop));
      Assert_True (Callback_Task_Valid);

      --  The GIR allows a null callback, and GtkAda forwards it as null for
      --  both the C callback and user data. The runtime currently rejects it.
      Callback_Completed := False;
      declare
         Domain  : Interfaces.C.Strings.chars_ptr :=
           Interfaces.C.Strings.New_String ("Gdk");
         Pattern : Interfaces.C.Strings.chars_ptr :=
           Interfaces.C.Strings.New_String
             ("*assertion 'callback != NULL' failed*");
         File    : Interfaces.C.Strings.chars_ptr :=
           Interfaces.C.Strings.New_String ("main.adb");
         Func    : Interfaces.C.Strings.chars_ptr :=
           Interfaces.C.Strings.New_String ("Test_Accessors_And_Read");
      begin
         G_Test_Expect_Message (Domain, 8, Pattern);
         Stored_Drop.Read_Value_Async (Glib.GType_String, 0, null, null);
         G_Test_Assert_Expected_Messages
           (Interfaces.C.Strings.Null_Ptr, File, 1, Func);
         Interfaces.C.Strings.Free (Domain);
         Interfaces.C.Strings.Free (Pattern);
         Interfaces.C.Strings.Free (File);
         Interfaces.C.Strings.Free (Func);
      end;
      Assert_False (Callback_Completed);

      --  Unset every locally initialized GValue and free C strings after
      --  g_object_new_with_properties has consumed the constructor arguments.
      for Index in Values'Range loop
         Glib.Values.Unset (Values (Index));
         Interfaces.C.Strings.Free (Names (Index));
      end loop;
      --  The constructed drop holds its own refs to the supplied properties;
      --  finish the test drag before releasing the caller-owned references.
      Gdk.Content_Formats.Unref (Formats);
      Gdk.Drag.Drop_Done (Drag, False);
      Glib.Object.Unref (Glib.Object.GObject (Stored_Drop));
      Glib.Object.Unref (Glib.Object.GObject (Drag));
      Glib.Object.Unref (Glib.Object.GObject (Provider));
      Window.Destroy;
   end Test_Accessors_And_Read;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/gdk/drop/accessors-and-read", Test_Accessors_And_Read'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
