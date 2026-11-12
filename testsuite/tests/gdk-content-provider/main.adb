--  Gdk.Content_Provider.Get_Value: the only GError-throwing method on a
--  content provider built directly from a GValue (Gdk_New_For_Value), so no
--  display/clipboard/DnD machinery is needed.
--
--  No widget is created, so this test needs no display.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib;                use Glib;
with Glib.Bytes;
with Glib.Error;          use Glib.Error;
with Glib.Main;
with Glib.Object;
with Glib.Output_Stream;
with Glib.Values;         use Glib.Values;
with Glib.Test;           use Glib.Test;
with Gdk.Content_Formats;  use Gdk.Content_Formats;
with Gdk.Content_Provider; use Gdk.Content_Provider;

procedure Main is
   use type Glib.Bytes.Gbytes;
   use type Glib.Error.GError;
   use type Glib.Object.GObject;
   use type System.Address;

   Byte_Text : constant String := "provider bytes";

   --  The callback stores its result because the test resumes after dispatch.
   Async_Provider       : Gdk_Content_Provider;
   Async_Stream         : Glib.Output_Stream.Goutput_Stream;
   Async_Done           : Boolean := False;
   Async_Source_Matches : Boolean := False;
   Async_Success        : Boolean := False;
   Async_Error          : Glib.Error.GError;

   Typed_Changed_Count : Natural := 0;
   Slot_Changed_Count  : Natural := 0;
   Typed_Exception_Count : Natural := 0;
   Slot_Exception_Count  : Natural := 0;

   procedure On_Write_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);

   procedure On_Content_Changed
     (Self : access Gdk_Content_Provider_Record'Class);

   procedure On_Content_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class);

   procedure Raise_From_Typed_Callback
     (Self : access Gdk_Content_Provider_Record'Class);

   procedure Raise_From_Slot_Callback
     (Self : access Glib.Object.GObject_Record'Class);

   procedure Test_Byte_Formats_And_Signals
   with Convention => C;

   procedure Test_Async_Write_With_Callback
   with Convention => C;

   procedure Test_Async_Write_Without_Callback
   with Convention => C;

   --  GMemoryOutputStream is not wrapped by GtkAda, so the test imports its
   --  constructor and read-only inspection functions just as goutputstream does.
   --  The returned wrapper is associated with GLib's new object via its stub.
   function New_Memory_Stream return Glib.Output_Stream.Goutput_Stream;

   function Contents (Stream : Glib.Output_Stream.Goutput_Stream) return String;

   function To_Bytes (Text : String) return Glib.Guint8_Array;

   --  Recover the Ada-owned output-stream wrapper from GLib's new GObject.
   function New_Memory_Stream return Glib.Output_Stream.Goutput_Stream is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_memory_output_stream_new_resizable");
      Stub : Glib.Output_Stream.Goutput_Stream_Record;
   begin
      return Glib.Output_Stream.Goutput_Stream
        (Glib.Object.Get_User_Data (Internal, Stub));
   end New_Memory_Stream;

   --  Read the bytes written to the in-memory stream without changing it.
   --  A constrained Ada view is needed because C returns a pointer without
   --  carrying the array bounds.
   function Contents (Stream : Glib.Output_Stream.Goutput_Stream) return String is
      function Get_Data (Self : System.Address) return System.Address;
      pragma Import (C, Get_Data, "g_memory_output_stream_get_data");

      function Get_Data_Size (Self : System.Address) return Glib.Gsize;
      pragma Import (C, Get_Data_Size, "g_memory_output_stream_get_data_size");

      Object : constant System.Address :=
        Glib.Object.Get_Object (Glib.Object.GObject (Stream));
      Size  : constant Natural := Natural (Get_Data_Size (Object));
      subtype Data is Glib.Guchar_Array (1 .. Size);
      type Data_Access is access all Data;
      function To_Data is new Ada.Unchecked_Conversion
        (System.Address, Data_Access);
      Bytes : constant Data_Access := To_Data (Get_Data (Object));
      Result : String (1 .. Size);
   begin
      for Index in Result'Range loop
         Result (Index) := Character'Val (Integer (Bytes (Index)));
      end loop;
      return Result;
   end Contents;

   --  GBytes copies the string data, so this local array can go out of scope.
   function To_Bytes (Text : String) return Glib.Guint8_Array is
      Result : Glib.Guint8_Array (1 .. Text'Length);
      Index  : Natural := Result'First;
   begin
      for Char of Text loop
         Result (Index) := Glib.Guint8 (Character'Pos (Char));
         Index := Index + 1;
      end loop;
      return Result;
   end To_Bytes;

   --  The bridge invokes this callback from the thread-default main context.
   --  Finish must be called here while GLib's async result is still valid.
   procedure On_Write_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result)
   is
   begin
      Async_Source_Matches :=
        Source_Object.Get_Object = Async_Provider.Get_Object;
      Async_Success := Async_Provider.Write_Mime_Type_Finish (Res, Async_Error);
      Async_Done := True;
   end On_Write_Ready;

   --  The typed signal form receives the provider that emitted the signal.
   procedure On_Content_Changed
     (Self : access Gdk_Content_Provider_Record'Class)
   is
   begin
      if Self.Get_Object = Async_Provider.Get_Object then
         Typed_Changed_Count := Typed_Changed_Count + 1;
      end if;
   end On_Content_Changed;

   --  The slot form receives the explicitly supplied GObject slot.
   procedure On_Content_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class)
   is
   begin
      if Self.Get_Object = Async_Provider.Get_Object then
         Slot_Changed_Count := Slot_Changed_Count + 1;
      end if;
   end On_Content_Changed_Slot;

   --  The marshaller contains exceptions so they cannot unwind through C.
   procedure Raise_From_Typed_Callback
     (Self : access Gdk_Content_Provider_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Typed_Exception_Count := Typed_Exception_Count + 1;
      raise Program_Error with "content provider typed callback containment";
   end Raise_From_Typed_Callback;

   procedure Raise_From_Slot_Callback
     (Self : access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Slot_Exception_Count := Slot_Exception_Count + 1;
      raise Program_Error with "content provider slot callback containment";
   end Raise_From_Slot_Callback;

   --  A byte provider advertises both its formats and storable formats.
   --  Emitting content-changed exercises the typed and slot signal adapters.
   --  A custom MIME type makes the expected format unambiguous.
   procedure Test_Byte_Formats_And_Signals is
      Buffer           : constant Glib.Guint8_Array := To_Bytes (Byte_Text);
      Bytes            : Glib.Bytes.Gbytes := Glib.Bytes.Gbytes_New
        (Buffer, Buffer'Length);
      Provider         : Gdk_Content_Provider;
      Procedure_Built  : Gdk_Content_Provider;
      Formats          : Gdk_Content_Formats;
      Storable_Formats : Gdk_Content_Formats;
   begin
      Provider := Gdk_Content_Provider_New_For_Bytes
        ("application/x-gtkada-content-provider", Bytes);
      --  The provider retains its own reference to the byte data.
      Glib.Bytes.Unref (Bytes);
      Assert_True (Provider /= null);

      Formats := Provider.Ref_Formats;
      Storable_Formats := Provider.Ref_Storable_Formats;
      --  Each result transfers a boxed reference that this test releases.
      Assert_True
        (Formats.Contain_Mime_Type ("application/x-gtkada-content-provider"));
      Assert_True
        (Storable_Formats.Contain_Mime_Type
           ("application/x-gtkada-content-provider"));
      Gdk.Content_Formats.Unref (Formats);
      Gdk.Content_Formats.Unref (Storable_Formats);

      --  Initializing an already-created provider is documented as a no-op.
      --  A different MIME type reveals an accidental reinitialization.
      declare
         Other_Buffer : constant Glib.Guint8_Array := To_Bytes ("other");
         Other_Bytes  : Glib.Bytes.Gbytes := Glib.Bytes.Gbytes_New
           (Other_Buffer, Other_Buffer'Length);
      begin
         Provider.Initialize_For_Bytes ("text/plain", Other_Bytes);
         Glib.Bytes.Unref (Other_Bytes);
      end;
      Formats := Provider.Ref_Formats;
      Assert_True
        (Formats.Contain_Mime_Type ("application/x-gtkada-content-provider"));
      Assert_False (Formats.Contain_Mime_Type ("text/plain"));
      Gdk.Content_Formats.Unref (Formats);

      --  Cover the out-parameter constructor as well as the function form.
      --  Verify its format before releasing the independently built provider.
      declare
         Procedure_Buffer : constant Glib.Guint8_Array := To_Bytes ("procedure");
         Procedure_Bytes  : Glib.Bytes.Gbytes := Glib.Bytes.Gbytes_New
           (Procedure_Buffer, Procedure_Buffer'Length);
      begin
         Gdk_New_For_Bytes
           (Procedure_Built, "application/x-gtkada-procedure", Procedure_Bytes);
         Glib.Bytes.Unref (Procedure_Bytes);
      end;
      Formats := Procedure_Built.Ref_Formats;
      Assert_True
        (Formats.Contain_Mime_Type ("application/x-gtkada-procedure"));
      Gdk.Content_Formats.Unref (Formats);
      Glib.Object.Unref (Glib.Object.GObject (Procedure_Built));

      Async_Provider := Provider;
      Typed_Changed_Count := 0;
      Slot_Changed_Count := 0;
      Typed_Exception_Count := 0;
      Slot_Exception_Count := 0;
      Provider.On_Content_Changed
        (On_Content_Changed'Unrestricted_Access, After => True);
      Provider.On_Content_Changed
        (On_Content_Changed_Slot'Unrestricted_Access, Slot => Provider);
      --  Exceptions must be contained at the C signal callback boundary.
      Provider.On_Content_Changed
        (Raise_From_Typed_Callback'Unrestricted_Access);
      Provider.On_Content_Changed
        (Raise_From_Slot_Callback'Unrestricted_Access, Slot => Provider);
      Provider.Content_Changed;
      Assert_Cmpint_Eq (Glib.Gint (Typed_Changed_Count), 1);
      Assert_Cmpint_Eq (Glib.Gint (Slot_Changed_Count), 1);
      Assert_Cmpint_Eq (Glib.Gint (Typed_Exception_Count), 1);
      Assert_Cmpint_Eq (Glib.Gint (Slot_Exception_Count), 1);

      Glib.Object.Unref (Glib.Object.GObject (Provider));
   end Test_Byte_Formats_And_Signals;

   --  Wait for an async provider write and assert its finish callback ran.
   --  Poll without blocking indefinitely so a broken callback fails promptly.
   procedure Wait_For_Async_Write is
   begin
      for Attempt in 1 .. 300 loop
         exit when Async_Done;
         declare
            Dispatched : constant Boolean :=
              Glib.Main.Main_Context_Iteration (May_Block => False);
         begin
            if not Dispatched then
               delay 0.01;
            end if;
         end;
      end loop;
      Assert_True (Async_Done);
      Assert_True (Async_Source_Matches);
      Assert_True (Async_Success);
      Assert_True (Async_Error = null);
   end Wait_For_Async_Write;

   --  The non-null callback path runs the Ada callback bridge and Finish.
   --  The bytes read back prove that the requested serialization succeeded.
   procedure Test_Async_Write_With_Callback is
      Buffer   : constant Glib.Guint8_Array := To_Bytes (Byte_Text);
      Bytes    : Glib.Bytes.Gbytes := Glib.Bytes.Gbytes_New
        (Buffer, Buffer'Length);
      Provider : Gdk_Content_Provider;
   begin
      Provider := Gdk_Content_Provider_New_For_Bytes ("text/plain", Bytes);
      Glib.Bytes.Unref (Bytes);
      Async_Provider := Provider;
      Async_Stream := New_Memory_Stream;
      Async_Done := False;
      Async_Source_Matches := False;
      Async_Success := False;
      Async_Error := null;
      Provider.Write_Mime_Type_Async
        ("text/plain", Async_Stream, 0, null,
         On_Write_Ready'Unrestricted_Access);
      Wait_For_Async_Write;
      Assert_Cmpstr_Eq (Contents (Async_Stream), Byte_Text);
      Glib.Object.Unref (Glib.Object.GObject (Async_Stream));
      Glib.Object.Unref (Glib.Object.GObject (Provider));
   end Test_Async_Write_With_Callback;

   --  A null callback is supported; wait for stream pending state to clear.
   --  Reading the bytes confirms completion even without a Finish callback.
   procedure Test_Async_Write_Without_Callback is
      Buffer   : constant Glib.Guint8_Array := To_Bytes (Byte_Text);
      Bytes    : Glib.Bytes.Gbytes := Glib.Bytes.Gbytes_New
        (Buffer, Buffer'Length);
      Provider : Gdk_Content_Provider;
      Stream   : Glib.Output_Stream.Goutput_Stream;
   begin
      Provider := Gdk_Content_Provider_New_For_Bytes ("text/plain", Bytes);
      Glib.Bytes.Unref (Bytes);
      Stream := New_Memory_Stream;
      Provider.Write_Mime_Type_Async ("text/plain", Stream, 0, null, null);

      for Attempt in 1 .. 300 loop
         exit when not Stream.Has_Pending;
         declare
            Dispatched : constant Boolean :=
              Glib.Main.Main_Context_Iteration (May_Block => False);
         begin
            if not Dispatched then
               delay 0.01;
            end if;
         end;
      end loop;
      Assert_False (Stream.Has_Pending);
      Assert_Cmpstr_Eq (Contents (Stream), Byte_Text);
      Glib.Object.Unref (Glib.Object.GObject (Stream));
      Glib.Object.Unref (Glib.Object.GObject (Provider));
   end Test_Async_Write_Without_Callback;

   procedure Test_Get_Value_Success
   with Convention => C;

   procedure Test_Get_Value_Invalid_Target
   with Convention => C;

   -----------------------------
   -- Test_Get_Value_Success --
   -----------------------------

   procedure Test_Get_Value_Success is
      Source   : GValue;
      Target   : GValue;
      Provider : Gdk_Content_Provider;
      Error    : Glib.Error.GError;
      Success  : Boolean;
   begin
      Init_Set_Int (Source, 42);
      Gdk_New_For_Value (Provider, Source);

      Init (Target, GType_Int);
      Success := Get_Value (Provider, Target, Error);

      Assert_True (Error = null);
      Assert_True (Success);
      Assert_Cmpint_Eq (Get_Int (Target), 42);

      Unref (Provider);
   end Test_Get_Value_Success;

   --------------------------------------
   -- Test_Get_Value_Invalid_Target --
   --------------------------------------

   procedure Test_Get_Value_Invalid_Target is
      Source   : GValue;
      Target   : GValue;
      Provider : Gdk_Content_Provider;
      Error    : Glib.Error.GError;
      Success  : Boolean;
   begin
      Init_Set_Int (Source, 42);
      Gdk_New_For_Value (Provider, Source);

      --  A validly-initialised (G_IS_VALUE-passing) but incompatible
      --  target: an Int can never transform into a GObject reference.
      Init (Target, GType_Object);
      Success := Get_Value (Provider, Target, Error);

      Assert_False (Success);
      Assert_True (Error /= null);

      Unref (Provider);
   end Test_Get_Value_Invalid_Target;

begin
   Glib.Test.Init;

   Glib.Test.Add_Func
     ("/gdk-content-provider/get-value-success",
      Test_Get_Value_Success'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-content-provider/get-value-invalid-target",
      Test_Get_Value_Invalid_Target'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-content-provider/byte-formats-signals",
      Test_Byte_Formats_And_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-content-provider/async-write-callback",
      Test_Async_Write_With_Callback'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-content-provider/async-write-null-callback",
      Test_Async_Write_Without_Callback'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
