--  Glib.GMountOperations tests for its properties, accessors, and signals.
--  Gtk/GIO has no C tests for GMountOperation to port, so the signal cases
--  emit each documented signal directly and verify both callback overloads.

with Ada.Command_Line;
with System;

with Glib;                     use Glib;
with Glib.Arrays;
with Glib.GEnums;              use Glib.GEnums;
with Glib.GMountOperations;    use Glib.GMountOperations;
with Glib.Object;
with Glib.Test;                use Glib.Test;
with Gtkada.Bindings;
with Gtkada.Types;             use Gtkada.Types;

procedure Main is

   use type Glib.GMountOperations.Gmount_Operation;
   use type Glib.GEnums.GAsk_Password_Flags;
   use type Glib.GEnums.GPassword_Save;
   use type Glib.GEnums.GMount_Operation_Result;
   use type Gtkada.Bindings.chars_ptr_array_access;
   use type Glib.Arrays.Garray_Access;
   use type System.Address;

   Current_Object : System.Address := System.Null_Address;
   Aborted_Self, Aborted_Slot : Natural := 0;
   Ask_Password_Self, Ask_Password_Slot : Natural := 0;
   Ask_Question_Self, Ask_Question_Slot : Natural := 0;
   Reply_Self, Reply_Slot : Natural := 0;
   Show_Processes_Self, Show_Processes_Slot : Natural := 0;
   Progress_Self, Progress_Slot : Natural := 0;

   --  These declarations give g_signal_emit_by_name the argument types for
   --  each signal's C varargs. The first two arguments are always fixed.
   procedure Emit_No_Arguments
     (Object : System.Address; Name : Chars_Ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Ask_Password
     (Object : System.Address;
      Name, Message, Username, Domain : Chars_Ptr;
      Flags : GAsk_Password_Flags)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Ask_Question
     (Object : System.Address;
      Name, Message : Chars_Ptr;
      Choices : System.Address)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Show_Processes
     (Object : System.Address;
      Name, Message : Chars_Ptr;
      Processes, Choices : System.Address)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Progress
     (Object : System.Address;
      Name, Message : Chars_Ptr;
      Time_Left, Bytes_Left : Gint64)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   --  The callback checks share the emitter address so both the typed and
   --  slot callbacks can confirm which GObject was supplied by GObject.
   procedure On_Aborted (Self : access Gmount_Operation_Record'Class);
   procedure On_Aborted_Slot (Self : access Glib.Object.GObject_Record'Class);
   procedure On_Ask_Password
     (Self : access Gmount_Operation_Record'Class;
      Message, Username, Domain : UTF8_String;
      Flags : GAsk_Password_Flags);
   procedure On_Ask_Password_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Message, Username, Domain : UTF8_String;
      Flags : GAsk_Password_Flags);
   procedure On_Ask_Question
     (Self : access Gmount_Operation_Record'Class;
      Message : UTF8_String;
      Choices : Gtkada.Bindings.chars_ptr_array_access);
   procedure On_Ask_Question_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Message : UTF8_String;
      Choices : Gtkada.Bindings.chars_ptr_array_access);
   procedure On_Reply
     (Self : access Gmount_Operation_Record'Class;
      Result : GMount_Operation_Result);
   procedure On_Reply_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Result : GMount_Operation_Result);
   procedure On_Show_Processes
     (Self : access Gmount_Operation_Record'Class;
      Message : UTF8_String;
      Processes : Glib.Arrays.Garray_Access;
      Choices : Gtkada.Bindings.chars_ptr_array_access);
   procedure On_Show_Processes_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Message : UTF8_String;
      Processes : Glib.Arrays.Garray_Access;
      Choices : Gtkada.Bindings.chars_ptr_array_access);
   procedure On_Progress
     (Self : access Gmount_Operation_Record'Class;
      Message : UTF8_String;
      Time_Left, Bytes_Left : Gint64);
   procedure On_Progress_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Message : UTF8_String;
      Time_Left, Bytes_Left : Gint64);

   procedure Test_Accessors with Convention => C;
   procedure Test_Signals with Convention => C;

   ----------------
   -- Callbacks --
   ----------------

   --  The no-argument handlers verify the operation reaches both overloads,
   --  with slot dispatch preserving the explicitly supplied object.
   procedure On_Aborted (Self : access Gmount_Operation_Record'Class) is
   begin
      Assert_True (Self.Get_Object = Current_Object);
      Aborted_Self := Aborted_Self + 1;
   end On_Aborted;

   procedure On_Aborted_Slot
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Assert_True (Glib.Object.Get_Object (Self) = Current_Object);
      Aborted_Slot := Aborted_Slot + 1;
   end On_Aborted_Slot;

   --  Password callbacks check all three strings and the combined flags
   --  passed by a backend when it requests authentication details.
   procedure On_Ask_Password
     (Self : access Gmount_Operation_Record'Class;
      Message, Username, Domain : UTF8_String;
      Flags : GAsk_Password_Flags) is
   begin
      Assert_True (Self.Get_Object = Current_Object);
      Assert_Cmpstr_Eq (Message, "Authentication required");
      Assert_Cmpstr_Eq (Username, "ada-user");
      Assert_Cmpstr_Eq (Domain, "example.test");
      Assert_Cmpuint_Eq
        (Guint (Flags),
         Guint (G_Ask_Password_Need_Password or G_Ask_Password_Need_Username));
      Ask_Password_Self := Ask_Password_Self + 1;
   end On_Ask_Password;

   procedure On_Ask_Password_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Message, Username, Domain : UTF8_String;
      Flags : GAsk_Password_Flags) is
   begin
      Assert_True (Glib.Object.Get_Object (Self) = Current_Object);
      Assert_Cmpstr_Eq (Message, "Authentication required");
      Assert_Cmpstr_Eq (Username, "ada-user");
      Assert_Cmpstr_Eq (Domain, "example.test");
      Assert_Cmpuint_Eq
        (Guint (Flags),
         Guint (G_Ask_Password_Need_Password or G_Ask_Password_Need_Username));
      Ask_Password_Slot := Ask_Password_Slot + 1;
   end On_Ask_Password_Slot;

   --  A null strv represents an empty choices list, and the adapter must
   --  preserve that nullable pointer for both callback signatures.
   procedure On_Ask_Question
     (Self : access Gmount_Operation_Record'Class;
      Message : UTF8_String;
      Choices : Gtkada.Bindings.chars_ptr_array_access) is
   begin
      Assert_True (Self.Get_Object = Current_Object);
      Assert_Cmpstr_Eq (Message, "Replace the existing file?");
      Assert_True (Choices = null);
      Ask_Question_Self := Ask_Question_Self + 1;
   end On_Ask_Question;

   procedure On_Ask_Question_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Message : UTF8_String;
      Choices : Gtkada.Bindings.chars_ptr_array_access) is
   begin
      Assert_True (Glib.Object.Get_Object (Self) = Current_Object);
      Assert_Cmpstr_Eq (Message, "Replace the existing file?");
      Assert_True (Choices = null);
      Ask_Question_Slot := Ask_Question_Slot + 1;
   end On_Ask_Question_Slot;

   --  Reply callbacks check the enum value emitted by the public Reply
   --  method through both generated callback signatures.
   procedure On_Reply
     (Self : access Gmount_Operation_Record'Class;
      Result : GMount_Operation_Result) is
   begin
      Assert_True (Self.Get_Object = Current_Object);
      Assert_True (Result = Handled);
      Reply_Self := Reply_Self + 1;
   end On_Reply;

   procedure On_Reply_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Result : GMount_Operation_Result) is
   begin
      Assert_True (Glib.Object.Get_Object (Self) = Current_Object);
      Assert_True (Result = Handled);
      Reply_Slot := Reply_Slot + 1;
   end On_Reply_Slot;

   --  These callbacks validate nullable process/choice arrays and the
   --  message passed by the show-processes signal.
   procedure On_Show_Processes
     (Self : access Gmount_Operation_Record'Class;
      Message : UTF8_String;
      Processes : Glib.Arrays.Garray_Access;
      Choices : Gtkada.Bindings.chars_ptr_array_access) is
   begin
      Assert_True (Self.Get_Object = Current_Object);
      Assert_Cmpstr_Eq (Message, "Applications are using this volume");
      Assert_True (Processes = null);
      Assert_True (Choices = null);
      Show_Processes_Self := Show_Processes_Self + 1;
   end On_Show_Processes;

   procedure On_Show_Processes_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Message : UTF8_String;
      Processes : Glib.Arrays.Garray_Access;
      Choices : Gtkada.Bindings.chars_ptr_array_access) is
   begin
      Assert_True (Glib.Object.Get_Object (Self) = Current_Object);
      Assert_Cmpstr_Eq (Message, "Applications are using this volume");
      Assert_True (Processes = null);
      Assert_True (Choices = null);
      Show_Processes_Slot := Show_Processes_Slot + 1;
   end On_Show_Processes_Slot;

   --  The progress handlers check each 64-bit value after marshalling, in
   --  addition to checking the operation object.
   procedure On_Progress
     (Self : access Gmount_Operation_Record'Class;
      Message : UTF8_String;
      Time_Left, Bytes_Left : Gint64) is
   begin
      Assert_True (Self.Get_Object = Current_Object);
      Assert_Cmpstr_Eq (Message, "Waiting for applications to close");
      Assert_True (Time_Left = 17);
      Assert_True (Bytes_Left = 2048);
      Progress_Self := Progress_Self + 1;
   end On_Progress;

   procedure On_Progress_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Message : UTF8_String;
      Time_Left, Bytes_Left : Gint64) is
   begin
      Assert_True (Glib.Object.Get_Object (Self) = Current_Object);
      Assert_Cmpstr_Eq (Message, "Waiting for applications to close");
      Assert_True (Time_Left = 17);
      Assert_True (Bytes_Left = 2048);
      Progress_Slot := Progress_Slot + 1;
   end On_Progress_Slot;

   --------------------
   -- Test_Accessors --
   --------------------

   procedure Test_Accessors is
      Operation : Gmount_Operation := Gmount_Operation_New;
      Via_G_New : Gmount_Operation;
   begin
      --  Check both constructor entry points and Initialize's already-created
      --  branch before exercising the shared object's fields.
      --  Both public constructors create initialized objects; a second
      --  Initialize call is safe and leaves the existing object intact.
      Assert_True (Operation /= null);
      G_New (Via_G_New);
      Assert_True (Via_G_New /= null);
      Glib.GMountOperations.Initialize (Via_G_New);

      --  Exercise default values and each getter/setter pair through its
      --  public API, including the empty-string-to-NULL setter paths.
      Assert_True (not Operation.Get_Anonymous);
      Operation.Set_Anonymous (True);
      Assert_True (Operation.Get_Anonymous);
      Operation.Set_Anonymous (False);
      Assert_True (not Operation.Get_Anonymous);

      Assert_Cmpint_Eq (Operation.Get_Choice, 0);
      Operation.Set_Choice (3);
      Assert_Cmpint_Eq (Operation.Get_Choice, 3);

      Assert_Cmpstr_Eq (Operation.Get_Domain, "");
      Operation.Set_Domain ("example.test");
      Assert_Cmpstr_Eq (Operation.Get_Domain, "example.test");
      Operation.Set_Domain ("");
      Assert_Cmpstr_Eq (Operation.Get_Domain, "");

      Assert_True (not Operation.Get_Is_Tcrypt_Hidden_Volume);
      Operation.Set_Is_Tcrypt_Hidden_Volume (True);
      Assert_True (Operation.Get_Is_Tcrypt_Hidden_Volume);
      Assert_True (not Operation.Get_Is_Tcrypt_System_Volume);
      Operation.Set_Is_Tcrypt_System_Volume (True);
      Assert_True (Operation.Get_Is_Tcrypt_System_Volume);

      Assert_Cmpstr_Eq (Operation.Get_Password, "");
      Operation.Set_Password ("correct horse battery staple");
      Assert_Cmpstr_Eq (Operation.Get_Password, "correct horse battery staple");

      Assert_True (Operation.Get_Password_Save = Never);
      Operation.Set_Password_Save (For_Session);
      Assert_True (Operation.Get_Password_Save = For_Session);
      Operation.Set_Password_Save (Permanently);
      Assert_True (Operation.Get_Password_Save = Permanently);

      Assert_Cmpuint_Eq (Operation.Get_Pim, 0);
      Operation.Set_Pim (42);
      Assert_Cmpuint_Eq (Operation.Get_Pim, 42);

      Assert_Cmpstr_Eq (Operation.Get_Username, "");
      Operation.Set_Username ("ada-user");
      Assert_Cmpstr_Eq (Operation.Get_Username, "ada-user");
      Operation.Set_Username ("");
      Assert_Cmpstr_Eq (Operation.Get_Username, "");

      --  G_New transfers a GObject reference to each Ada owner; release both
      --  references explicitly so this case does not leak GObjects.
      Glib.Object.Unref (Glib.Object.GObject (Operation));
      Glib.Object.Unref (Glib.Object.GObject (Via_G_New));
   end Test_Accessors;

   ------------------
   -- Test_Signals --
   ------------------

   procedure Test_Signals is
      Operation : constant Gmount_Operation := Gmount_Operation_New;
      Name      : Chars_Ptr;
      Message   : Chars_Ptr;
      Username  : Chars_Ptr;
      Domain    : Chars_Ptr;
   begin
      --  Register both callback signatures before emitting signals so every
      --  emission exercises both generated marshaller families.
      --  The same operation is used as the explicit GObject slot. Its object
      --  address lets callbacks verify the adapter's self/slot conversion.
      Current_Object := Operation.Get_Object;
      Operation.On_Aborted (On_Aborted'Unrestricted_Access);
      Operation.On_Aborted
        (On_Aborted_Slot'Unrestricted_Access, Operation);
      Operation.On_Ask_Password (On_Ask_Password'Unrestricted_Access);
      Operation.On_Ask_Password
        (On_Ask_Password_Slot'Unrestricted_Access, Operation);
      Operation.On_Ask_Question (On_Ask_Question'Unrestricted_Access);
      Operation.On_Ask_Question
        (On_Ask_Question_Slot'Unrestricted_Access, Operation);
      Operation.On_Reply (On_Reply'Unrestricted_Access);
      Operation.On_Reply (On_Reply_Slot'Unrestricted_Access, Operation);
      Operation.On_Show_Processes (On_Show_Processes'Unrestricted_Access);
      Operation.On_Show_Processes
        (On_Show_Processes_Slot'Unrestricted_Access, Operation);
      Operation.On_Show_Unmount_Progress (On_Progress'Unrestricted_Access);
      Operation.On_Show_Unmount_Progress
        (On_Progress_Slot'Unrestricted_Access, Operation);

      --  A no-argument signal exercises the generic aborted marshallers.
      Name := New_String ("aborted");
      Emit_No_Arguments (Current_Object, Name);
      Free (Name);
      Assert_True (Aborted_Self = 1 and Aborted_Slot = 1);

      --  This signal carries three strings and a flags value, matching the
      --  fields a password prompt receives from a mount backend.
      Name := New_String ("ask-password");
      Message := New_String ("Authentication required");
      Username := New_String ("ada-user");
      Domain := New_String ("example.test");
      Emit_Ask_Password
        (Current_Object, Name, Message, Username, Domain,
         G_Ask_Password_Need_Password or G_Ask_Password_Need_Username);
      Free (Name);
      Free (Message);
      Free (Username);
      Free (Domain);
      Assert_True (Ask_Password_Self = 1 and Ask_Password_Slot = 1);

      --  A null string vector is a valid empty choice list for this signal.
      Name := New_String ("ask-question");
      Message := New_String ("Replace the existing file?");
      Emit_Ask_Question (Current_Object, Name, Message, System.Null_Address);
      Free (Name);
      Free (Message);
      Assert_True (Ask_Question_Self = 1 and Ask_Question_Slot = 1);

      --  Reply is the public API that emits this signal and passes its enum.
      Operation.Reply (Handled);
      Assert_True (Reply_Self = 1 and Reply_Slot = 1);

      --  No applications are represented by null process and choice arrays;
      --  the adapters still receive and convert each documented argument.
      Name := New_String ("show-processes");
      Message := New_String ("Applications are using this volume");
      Emit_Show_Processes
        (Current_Object, Name, Message,
         System.Null_Address, System.Null_Address);
      Free (Name);
      Free (Message);
      Assert_True (Show_Processes_Self = 1 and Show_Processes_Slot = 1);

      --  Progress arguments are 64-bit values; check both survive the C
      --  varargs boundary and both generated marshallers.
      Name := New_String ("show-unmount-progress");
      Message := New_String ("Waiting for applications to close");
      Emit_Progress (Current_Object, Name, Message, 17, 2048);
      Free (Name);
      Free (Message);
      Assert_True (Progress_Self = 1 and Progress_Slot = 1);

      Glib.Object.Unref (Glib.Object.GObject (Operation));
      Current_Object := System.Null_Address;
   end Test_Signals;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/glib/gmountoperations/accessors", Test_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib/gmountoperations/signals", Test_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
