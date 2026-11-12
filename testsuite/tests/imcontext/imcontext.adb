--  Tests Gtk.IM_Context methods and signal marshallers.
--  The surrounding-text scenarios are adapted from testsuite/c_tests/imcontext.c
--  (test_text_surrounding and test_textview_surrounding); the Gtk.Text private
--  key-controller accessor used by C is not exposed in the Ada bindings.
--  The Gtk_New and Add_Compose_File checks below exercise APIs with no direct
--  counterpart in testsuite/c_tests/imcontext.c.

with Ada.Directories;
with Ada.Command_Line;
with Interfaces.C;
with Gdk;
with Gdk.Device;
with Gdk.Display;
with Gdk.Enums;
with Gdk.Event;
with Gdk.Event.Key_Event;
with Gdk.Rectangle;
with Gdk.Surface;
with Glib;
with Glib.Object;
with Glib.Test;
with Gtk.Enums;
with Gtk.IM_Context;
with Gtk.IM_Context_Simple;
with Gtk.Main;
with Gtk.Text_Buffer;
with Gtk.Text_View;
with Gtkada.Types;
with Imcontext_Handlers;
with Pango.Attributes;
with System;
with System.Address_To_Access_Conversions;

procedure Imcontext is
   use Glib;
   use type Gdk.Gdk_Device;
   use type Gdk.Gdk_Surface;
   use type Gtk.Enums.Gtk_Input_Hints;
   use type Pango.Attributes.Pango_Attr_List;
   use type Gtk.Enums.Gtk_Input_Purpose;
   use type Interfaces.C.unsigned;
   use type Interfaces.C.unsigned_short;
   use type System.Address;
   use Glib.Test;
   use Gtk.IM_Context;
   use Gtk.IM_Context_Simple;
   use Gtkada.Types;
   use Imcontext_Handlers;

   --  These fields mirror the common GdkEvent prefix and GdkKeyEvent layout
   --  used by the neighboring gdk-event test. GDK 4 has no public event
   --  constructor, so this lets the compose test provide deterministic input.
   type Event_Base_Record is record
      Class      : System.Address;
      Ref_Count  : Interfaces.C.int;
      Event_Type : Gdk.Event.Gdk_Event_Type;
      Surface    : System.Address;
      Device     : System.Address;
      Time       : Interfaces.C.unsigned;
      Flags      : Interfaces.C.unsigned_short;
   end record
     with Convention => C;

   type Translated_Key_Record is record
      Keyval   : Interfaces.C.unsigned;
      Consumed : Gdk.Enums.Gdk_Modifier_Type;
      Layout   : Interfaces.C.unsigned;
      Level    : Interfaces.C.unsigned;
   end record
     with Convention => C;

   type Translated_Key_Array is array (0 .. 1) of Translated_Key_Record
     with Convention => C;

   --  GDK stores the keyval translations after the keycode and modifier data.
   --  Matching this layout lets GTK consume the event through its normal path.
   type Key_Event_Record is record
      Parent          : Event_Base_Record;
      State           : Gdk.Enums.Gdk_Modifier_Type;
      Keycode         : Interfaces.C.unsigned;
      Key_Is_Modifier : Interfaces.C.int;
      Translated      : Translated_Key_Array;
   end record
     with Convention => C;

   package Key_Pointers is new
     System.Address_To_Access_Conversions (Key_Event_Record);

   function Create_Event_Instance (Type_Id : Glib.GType) return System.Address
     with Import, Convention => C,
       External_Name => "g_type_create_instance";
   procedure Unref_Event (Event : System.Address)
     with Import, Convention => C,
       External_Name => "gdk_event_unref";
   function Ref_Object (Object : System.Address) return System.Address
     with Import, Convention => C,
       External_Name => "g_object_ref";

   Display : Gdk.Display.Gdk_Display;
   Seat    : Gdk.Gdk_Seat;
   Device  : Gdk.Gdk_Device;
   Surface : Gdk.Gdk_Surface;

   --  A copied GdkEvent owns references to its surface and device. Keep those
   --  references in the synthetic event so GTK can inspect normal metadata.
   --  The keyval occupies both translation slots as in a simple unshifted key.
   function New_Key_Event (Keyval : Interfaces.C.unsigned)
      return Gdk.Event.Gdk_Event
   is
      Raw  : constant System.Address :=
        Create_Event_Instance (Gdk.Event.Key_Event.Get_Type);
      Data : constant Key_Pointers.Object_Pointer :=
        Key_Pointers.To_Pointer (Raw);
      Result : Gdk.Event.Gdk_Event;
   begin
      Data.all.Parent.Event_Type := Gdk.Event.Key_Press;
      Data.all.Parent.Surface := Ref_Object (Glib.Object.Get_Object (Surface));
      Data.all.Parent.Device := Ref_Object (Glib.Object.Get_Object (Device));
      Data.all.Parent.Time := 1;
      Data.all.State := Gdk.Enums.Gdk_No_Modifier_Mask;
      Data.all.Keycode := 0;
      Data.all.Key_Is_Modifier := 0;
      Data.all.Translated (0) :=
        (Keyval   => Keyval,
         Consumed => Gdk.Enums.Gdk_No_Modifier_Mask,
         Layout   => 0,
         Level    => 0);
      Data.all.Translated (1) := Data.all.Translated (0);
      Result := Gdk.Event.From_Object (Raw);
      Unref_Event (Raw);
      return Result;
   end New_Key_Event;

   Last_Commit : String (1 .. 16) := (others => ' ');
   Last_Commit_Length : Natural := 0;

   --  Save the commit emitted when the custom compose sequence completes.
   procedure Record_Commit
     (Self : access Gtk_IM_Context_Record'Class;
      Text : UTF8_String)
   is
      pragma Unreferenced (Self);
   begin
      Last_Commit_Length := Text'Length;
      Last_Commit (1 .. Text'Length) := Text;
   end Record_Commit;

   --  Gtk's signal emitter is variadic. These fixed Ada profiles match the
   --  argument shapes for the individual signals emitted below.
   procedure Emit_Void
     (Instance : System.Address; Name : Chars_Ptr);
   pragma Import (C, Emit_Void, "g_signal_emit_by_name");

   procedure Emit_String
     (Instance : System.Address; Name, Text : Chars_Ptr);
   pragma Import (C, Emit_String, "g_signal_emit_by_name");

   procedure Test_Methods with Convention => C;
   procedure Test_Signals with Convention => C;
   procedure Test_Text_View_Surrounding with Convention => C;
   procedure Test_Constructors_And_Compose_File with Convention => C;

   procedure Test_Constructors_And_Compose_File is
      --  The static Compose table contains a custom Multi_key x y sequence.
      --  This verifies that Add_Compose_File affects the context's key input.
      By_Function : constant Gtk_IM_Context_Simple :=
        Gtk_IM_Context_Simple_New;
      By_Procedure : Gtk_IM_Context_Simple;
      Compose_File : constant String :=
        Ada.Directories.Compose
          (Ada.Directories.Current_Directory, "compose");
      Multi_Key : constant Interfaces.C.unsigned := 16#FF20#;
      Key_X     : constant Interfaces.C.unsigned := Character'Pos ('x');
      Key_Y     : constant Interfaces.C.unsigned := Character'Pos ('y');
   begin
      --  Exercise both public constructors and verify each yields an object.
      Gtk_New (By_Procedure);
      Assert_True (By_Function /= null);
      Assert_True (By_Procedure /= null);

      --  Add the deterministic table, then feed its sequence as key events.
      By_Procedure.Add_Compose_File (Compose_File);
      Display := Gdk.Display.Get_Default;
      Seat := Display.Get_Default_Seat;
      Device := Seat.Get_Keyboard;
      Surface := Gdk.Surface.Gdk_Surface_New_Toplevel (Display);
      Assert_True (Device /= null);
      Assert_True (Surface /= null);

      Last_Commit_Length := 0;
      By_Procedure.On_Commit (Record_Commit'Unrestricted_Access);
      declare
         --  The keysyms match the fixture's Multi_key, x, y entry in order.
         First  : constant Gdk.Event.Gdk_Event := New_Key_Event (Multi_Key);
         Second : constant Gdk.Event.Gdk_Event := New_Key_Event (Key_X);
         Third  : constant Gdk.Event.Gdk_Event := New_Key_Event (Key_Y);
      begin
         Assert_True (By_Procedure.Filter_Keypress (First));
         Assert_True (By_Procedure.Filter_Keypress (Second));
         Assert_True (By_Procedure.Filter_Keypress (Third));
      end;
      Assert_Cmpstr_Eq
        (Last_Commit (1 .. Last_Commit_Length), "Z");
      Surface.Destroy;
   end Test_Constructors_And_Compose_File;

   procedure Test_Methods is
      Context : constant Gtk_IM_Context :=
        Gtk_IM_Context (Gtk_IM_Context_Simple_New);
      View    : Gtk.Text_View.Gtk_Text_View;
      Display : Gdk.Display.Gdk_Display;
      Seat    : Gdk.Gdk_Seat;
      Device  : Gdk.Gdk_Device;
      Surface : Gdk.Gdk_Surface;
      Text    : Chars_Ptr;
      Attrs   : Pango.Attributes.Pango_Attr_List;
      Cursor  : Glib.Gint;
   begin
      --  Focus, reset, cursor geometry, and client-widget setters accept an
      --  unattached context and are safe to exercise without user input.
      Context.Focus_In;
      Context.Focus_Out;
      Context.Reset;
      --  GDK explicitly permits a null event here; Xvfb has no on-screen
      --  keyboard backend, so the request is expected to be declined.
      declare
         No_Event : Gdk.Event.Gdk_Event;
      begin
         Assert_False (Context.Activate_Osk (No_Event));
      end;
      Context.Set_Use_Preedit (True);
      Context.Set_Use_Preedit (False);
      Context.Set_Cursor_Location ((X => 2, Y => 3, Width => 4, Height => 5));
      Context.Set_Client_Widget (null);

      --  Read the initial preedit state and release both transfer-full values.
      Context.Get_Preedit_String (Text, Attrs, Cursor);
      Assert_Cmpstr_Eq (Value (Text), "");
      Assert_Cmpint_Eq (Cursor, 0);
      Free (Text);
      if Attrs /= Pango.Attributes.Null_Pango_Attr_List then
         Pango.Attributes.Unref (Attrs);
      end if;

      --  Check both input-method properties through GtkAda's typed helpers.
      Gtk.Enums.Gtk_Input_Hints_Properties.Set_Property
        (Context,
         Gtk.Enums.Gtk_Input_Hints_Properties.Property
           (Gtk.IM_Context.Input_Hints_Property),
         Gtk.Enums.Input_Hint_Emoji);
      Assert_True
        (Gtk.Enums.Gtk_Input_Hints_Properties.Get_Property
           (Context,
            Gtk.Enums.Gtk_Input_Hints_Properties.Property
              (Gtk.IM_Context.Input_Hints_Property)) = Gtk.Enums.Input_Hint_Emoji);
      Gtk.Enums.Gtk_Input_Purpose_Properties.Set_Property
        (Context,
         Gtk.Enums.Gtk_Input_Purpose_Properties.Property
           (Gtk.IM_Context.Input_Purpose_Property),
         Gtk.Enums.Input_Purpose_Email);
      Assert_True
        (Gtk.Enums.Gtk_Input_Purpose_Properties.Get_Property
           (Context,
            Gtk.Enums.Gtk_Input_Purpose_Properties.Property
              (Gtk.IM_Context.Input_Purpose_Property)) = Gtk.Enums.Input_Purpose_Email);

      --  A GDK toplevel surface and the Xvfb keyboard supply the objects
      --  needed by Filter_Key without synthesizing physical input.
      Display := Gdk.Display.Get_Default;
      Surface := Gdk.Surface.Gdk_Surface_New_Toplevel (Display);
      Seat := Display.Get_Default_Seat;
      Device := Seat.Get_Keyboard;
      Assert_True (Surface /= null);
      Assert_True (Device /= null);
      Assert_False
        (Context.Filter_Key
           (Press => True,
            Surface => Surface,
            Device => Device,
            Time => 0,
            Keycode => 0,
            State => 0,
            Group => 0));
      Surface.Destroy;

      --  Associate a normal widget, then clear it as clients do on teardown.
      View := Gtk.Text_View.Gtk_Text_View_New;
      Context.Set_Client_Widget (View);
      Context.Set_Client_Widget (null);
   end Test_Methods;

   procedure Test_Signals is
      Context : constant Gtk_IM_Context :=
        Gtk_IM_Context (Gtk_IM_Context_Simple_New);
      Name    : Chars_Ptr;
      Text    : Chars_Ptr;
      Got_Text : Chars_Ptr;
      Cursor   : Glib.Gint;
      Anchor   : Glib.Gint;
   begin
      --  Register both callback forms for every signal family. The slot form
      --  receives the context as its user-data object, matching GtkAda's API.
      Current_Context := Gtk_IM_Context (Context);
      Context_Void_Calls := 0;
      Slot_Void_Calls := 0;
      Context_String_Length := 0;
      Slot_String_Length := 0;
      Context.On_Commit (Context_Commit'Access);
      Context.On_Commit (Slot_Commit'Access, Context);
      Context.On_Delete_Surrounding (Context_Delete'Access);
      Context.On_Delete_Surrounding (Slot_Delete'Access, Context);
      Context.On_Invalid_Composition (Context_Invalid'Access);
      Context.On_Invalid_Composition (Slot_Invalid'Access, Context);
      Context.On_Preedit_Changed (Context_Void'Access);
      Context.On_Preedit_Changed (Slot_Void'Access, Context);
      Context.On_Preedit_End (Context_Void'Access);
      Context.On_Preedit_End (Slot_Void'Access, Context);
      Context.On_Preedit_Start (Context_Void'Access);
      Context.On_Preedit_Start (Slot_Void'Access, Context);
      Context.On_Retrieve_Surrounding (Context_Retrieve'Access);
      Context.On_Retrieve_Surrounding (Slot_Retrieve'Access, Context);

      --  Emit the string signal and check each overload received its payload.
      Name := New_String ("commit");
      Text := New_String ("composed");
      Emit_String (Glib.Object.Get_Object (Context), Name, Text);
      Free (Name);
      Free (Text);
      Assert_Cmpstr_Eq
        (Context_String (1 .. Context_String_Length), "composed");
      Assert_Cmpstr_Eq
        (Slot_String (1 .. Slot_String_Length), "composed");
      Context_Void_Calls := 0;
      Slot_Void_Calls := 0;

      --  The void signal trio exercises the context and GObject marshallers.
      Name := New_String ("preedit-changed");
      Emit_Void (Glib.Object.Get_Object (Context), Name);
      Free (Name);
      Name := New_String ("preedit-start");
      Emit_Void (Glib.Object.Get_Object (Context), Name);
      Free (Name);
      Name := New_String ("preedit-end");
      Emit_Void (Glib.Object.Get_Object (Context), Name);
      Free (Name);
      Assert_Cmpuint_Eq (Guint (Context_Void_Calls), 3);
      Assert_Cmpuint_Eq (Guint (Slot_Void_Calls), 3);

      --  Delete_Surrounding routes through both handlers; the first declines
      --  and the slot handler accepts, so the method returns True.
      Assert_True (Context.Delete_Surrounding (-1, 1));
      Assert_Cmpint_Eq (Last_Offset, -1);
      Assert_Cmpint_Eq (Last_Count, 1);

      --  Retrieving context invokes both handlers. The slot supplies the
      --  final selected range; the returned allocation belongs to the caller.
      Assert_True
        (Context.Get_Surrounding_With_Selection
           (Got_Text, Cursor, Anchor));
      Assert_Cmpstr_Eq (Value (Got_Text), "abcd");
      Assert_Cmpint_Eq (Cursor, 2);
      Assert_Cmpint_Eq (Anchor, 4);
      Free (Got_Text);

      --  GtkIMContext exposes no public operation to emit invalid-composition;
      --  the handlers above still test both generated connection overloads.
   end Test_Signals;

   procedure Test_Text_View_Surrounding is
      --  C's GTK test obtains the text view's private key controller. Ada
      --  creates a public context and supplies equivalent surrounding data.
      Context : constant Gtk_IM_Context :=
        Gtk_IM_Context (Gtk_IM_Context_Simple_New);
      Buffer  : constant Gtk.Text_Buffer.Gtk_Text_Buffer :=
        Gtk.Text_Buffer.Gtk_Text_Buffer_New;
      View    : constant Gtk.Text_View.Gtk_Text_View :=
        Gtk.Text_View.Gtk_Text_View_New_With_Buffer (Buffer);
      Text    : Chars_Ptr;
      Cursor  : Glib.Gint;
      Anchor  : Glib.Gint;
   begin
      Buffer.Set_Text ("abcd");
      Current_Context := Context;
      Retrieve_Anchor := 2;
      Context.On_Retrieve_Surrounding (Context_Retrieve'Access);
      Context.On_Retrieve_Surrounding (Slot_Retrieve'Access, Context);
      Context.Set_Client_Widget (View);
      Context.Set_Surrounding_With_Selection ("abcd", -1, 2, 2);
      Assert_True
        (Context.Get_Surrounding_With_Selection (Text, Cursor, Anchor));
      Assert_Cmpstr_Eq (Value (Text), "abcd");
      Assert_Cmpint_Eq (Cursor, 2);
      Assert_Cmpint_Eq (Anchor, 2);
      Free (Text);

      --  Exercise a selected range and the UTF-8 byte offsets used by GTK.
      Retrieve_Anchor := 4;
      Context.Set_Surrounding_With_Selection ("abcd", 4, 2, 4);
      Assert_True
        (Context.Get_Surrounding_With_Selection (Text, Cursor, Anchor));
      Assert_Cmpstr_Eq (Value (Text), "abcd");
      Assert_Cmpint_Eq (Cursor, 2);
      Assert_Cmpint_Eq (Anchor, 4);
      Free (Text);
      Context.Set_Client_Widget (null);
   end Test_Text_View_Surrounding;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func ("/imcontext/methods", Test_Methods'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/imcontext/constructors-compose-file",
      Test_Constructors_And_Compose_File'Unrestricted_Access);
   Glib.Test.Add_Func ("/imcontext/signals", Test_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/imcontext/textview-surrounding",
      Test_Text_View_Surrounding'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Imcontext;
