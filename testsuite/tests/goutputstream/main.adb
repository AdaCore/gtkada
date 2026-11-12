--  Goutput_Stream tests: writing to a GMemoryOutputStream.
--
--  GMemoryOutputStream is not bound, so the concrete stream and the
--  inspection of what was written to it are imported here directly.
--
--  The binary fixture sample.gresource is generated from sample.gresource.xml,
--  alpha.txt and beta.txt. Regenerate it from testsuite/tests/goutputstream
--  with:
--    glib-compile-resources sample.gresource.xml --target=sample.gresource
--      --sourcedir=.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib;               use Glib;
with Glib.Bytes;
with Glib.Error;
with Glib.Input_Stream;  use Glib.Input_Stream;
with Glib.Main;
with Glib.Object;
with Glib.Output_Stream; use Glib.Output_Stream;
with Glib.Resource;      use Glib.Resource;
with Glib.Test;          use Glib.Test;

procedure Main is

   Alpha_Text : constant UTF8_String := "alpha resource" & ASCII.LF;

   use type Glib.Error.GError;
   use type Glib.Bytes.Gbytes;

   type Async_Kind is
     (Async_None, Async_Close, Async_Flush, Async_Splice, Async_Write,
      Async_Write_All, Async_Write_Bytes);

   Current_Async        : Async_Kind := Async_None;
   Async_Done           : Boolean := False;
   Async_Target         : Goutput_Stream;
   Async_Error          : Glib.Error.GError;
   Async_Boolean_Result : Boolean := False;
   Async_Size_Result    : Gssize := 0;
   Async_Bytes_Written  : aliased Gsize := 0;

   procedure On_Async_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);

   procedure Iterate_Once;
   procedure Run_Async;
   procedure Wait_While_Pending (Stream : Goutput_Stream);

   --  The memory stream uses GLib's boxed GBytes type directly.
   procedure Unref_Bytes (Bytes : System.Address);
   pragma Import (C, Unref_Bytes, "g_bytes_unref");

   function New_Memory_Stream return Goutput_Stream;
   --  A resizable GMemoryOutputStream, which grows as it is written to.

   function Contents (Stream : Goutput_Stream) return String;
   --  Whatever has been written to the memory stream so far.

   function Open_Alpha return Ginput_Stream;
   --  alpha.txt, out of sample.gresource.

   function To_Buffer (Text : String) return Guint8_Array;

   -----------------------
   -- New_Memory_Stream --
   -----------------------

   function New_Memory_Stream return Goutput_Stream is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_memory_output_stream_new_resizable");

      Stub : Goutput_Stream_Record;
   begin
      return Goutput_Stream
        (Glib.Object.Get_User_Data (Internal, Stub));
   end New_Memory_Stream;

   --------------
   -- Contents --
   --------------

   function Contents (Stream : Goutput_Stream) return String is
      function Get_Data (Self : System.Address) return System.Address;
      pragma Import (C, Get_Data, "g_memory_output_stream_get_data");

      function Get_Data_Size (Self : System.Address) return Gsize;
      pragma Import (C, Get_Data_Size, "g_memory_output_stream_get_data_size");

      Object : constant System.Address :=
        Glib.Object.Get_Object (Glib.Object.GObject (Stream));
      Size   : constant Natural := Natural (Get_Data_Size (Object));

      subtype Data is Guchar_Array (1 .. Size);
      type Data_Access is access all Data;
      function To_Data is new Ada.Unchecked_Conversion
        (System.Address, Data_Access);

      Bytes : constant Data_Access := To_Data (Get_Data (Object));

      Result : String (1 .. Size);
   begin
      for I in Result'Range loop
         Result (I) := Character'Val (Integer (Bytes (I)));
      end loop;

      return Result;
   end Contents;

   ----------------
   -- Open_Alpha --
   ----------------

   function Open_Alpha return Ginput_Stream is
      Load_Error   : Glib.Error.GError;
      Stream_Error : Glib.Error.GError;
      Resource     : Gresource := Load ("sample.gresource", Load_Error);
      Stream       : constant Ginput_Stream := Open_Stream
        (Self         => Resource,
         Path         => "/org/gtkada/test/alpha.txt",
         Lookup_Flags => G_Resource_Lookup_Flags_None,
         Error        => Stream_Error);
   begin
      Assert_True (Load_Error = null);
      Assert_True (Stream_Error = null);
      Assert_True (Resource /= Null_Gresource);
      Unref (Resource);

      return Stream;
   end Open_Alpha;

   ---------------
   -- To_Buffer --
   ---------------

   function To_Buffer (Text : String) return Guint8_Array is
      Result : Guint8_Array (1 .. Text'Length);
      Index  : Natural := Result'First;
   begin
      for C of Text loop
         Result (Index) := Character'Pos (C);
         Index := Index + 1;
      end loop;

      return Result;
   end To_Buffer;

   procedure Test_Write
   with Convention => C;

   procedure Test_Write_Slice
   with Convention => C;

   procedure Test_Write_All
   with Convention => C;

   procedure Test_Splice
   with Convention => C;

   procedure Test_State_And_Flush
   with Convention => C;

   procedure Test_Write_Bytes
   with Convention => C;

   procedure Test_Async
   with Convention => C;

   ----------------
   -- Test_Write --
   ----------------

   procedure Test_Write is
      Stream  : constant Goutput_Stream := New_Memory_Stream;
      Buffer  : constant Guint8_Array := To_Buffer ("hello");
      Written : Gssize;
      Error   : Glib.Error.GError;
   begin
      Assert_True (Stream /= null);

      Written := Write
        (Self        => Stream,
         Buffer      => Buffer,
         Cancellable => null,
         Error       => Error);

      Assert_True (Error = null);
      Assert_Cmpint_Eq (Gint (Written), 5);
      Assert_Cmpstr_Eq (Contents (Stream), "hello");

      Assert_True (Close (Self => Stream, Cancellable => null, Error => Error));
      Assert_True (Error = null);
      Assert_True (Is_Closed (Stream));

      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_Write;

   ----------------------
   -- Test_Write_Slice --
   ----------------------

   procedure Test_Write_Slice is
      Stream  : constant Goutput_Stream := New_Memory_Stream;
      Buffer  : constant Guint8_Array := To_Buffer ("hello");
      Written : Gssize;
      Error   : Glib.Error.GError;
   begin
      Assert_True (Stream /= null);

      --  "Count" is not part of the Ada profile: a caller who wants to write
      --  less passes a slice, whose 'Length is handed to C.

      Written := Write
        (Self        => Stream,
         Buffer      => Buffer (1 .. 3),
         Cancellable => null,
         Error       => Error);

      Assert_True (Error = null);
      Assert_Cmpint_Eq (Gint (Written), 3);
      Assert_Cmpstr_Eq (Contents (Stream), "hel");

      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_Write_Slice;

   --------------------
   -- Test_Write_All --
   --------------------

   procedure Test_Write_All is
      Stream  : constant Goutput_Stream := New_Memory_Stream;
      Buffer  : constant Guint8_Array := To_Buffer (Alpha_Text);
      Written : aliased Gsize := 0;
      Error   : Glib.Error.GError;
   begin
      Assert_True (Stream /= null);

      Assert_True
        (Write_All
           (Self          => Stream,
            Buffer        => Buffer,
            Bytes_Written => Written'Access,
            Cancellable   => null,
            Error         => Error));

      Assert_True (Error = null);
      Assert_Cmpint_Eq (Gint (Written), Gint (Alpha_Text'Length));
      Assert_Cmpstr_Eq (Contents (Stream), Alpha_Text);

      --  Bytes_Written is optional: C accepts a NULL there.

      Assert_True
        (Write_All
           (Self          => Stream,
            Buffer        => Buffer,
            Bytes_Written => null,
            Cancellable   => null,
            Error         => Error));

      Assert_True (Error = null);
      Assert_Cmpstr_Eq (Contents (Stream), Alpha_Text & Alpha_Text);

      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_Write_All;

   -----------------
   -- Test_Splice --
   -----------------

   procedure Test_Splice is
      Source  : constant Ginput_Stream := Open_Alpha;
      Target  : constant Goutput_Stream := New_Memory_Stream;
      Spliced : Gssize;
      Error   : Glib.Error.GError;
   begin
      Assert_True (Source /= null);
      Assert_True (Target /= null);

      Spliced := Splice
        (Self        => Target,
         Source      => Source,
         Flags       => G_Output_Stream_Splice_Close_Source,
         Cancellable => null,
         Error       => Error);

      Assert_True (Error = null);
      Assert_Cmpint_Eq (Gint (Spliced), Gint (Alpha_Text'Length));
      Assert_Cmpstr_Eq (Contents (Target), Alpha_Text);
      Assert_True (Is_Closed (Source));

      Glib.Object.Unref (Glib.Object.GObject (Target));
      Glib.Object.Unref (Glib.Object.GObject (Source));
   end Test_Splice;

   ----------------------------
   -- On_Async_Ready / Run_Async --
   ----------------------------

   --  Each callback uses the matching finish routine while the GAsyncResult
   --  is valid, then records the checked result for its calling test case.
   procedure On_Async_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result)
   is
      pragma Unreferenced (Source_Object);
   begin
      case Current_Async is
         when Async_Close =>
            Async_Boolean_Result := Close_Finish
              (Async_Target, Res, Async_Error);
         when Async_Flush =>
            Async_Boolean_Result := Flush_Finish
              (Async_Target, Res, Async_Error);
         when Async_Splice =>
            Async_Size_Result := Splice_Finish
              (Async_Target, Res, Async_Error);
         when Async_Write =>
            Async_Size_Result := Write_Finish
              (Async_Target, Res, Async_Error);
         when Async_Write_All =>
            Async_Boolean_Result := Write_All_Finish
              (Async_Target, Res, Async_Bytes_Written'Access, Async_Error);
         when Async_Write_Bytes =>
            Async_Size_Result := Write_Bytes_Finish
              (Async_Target, Res, Async_Error);
         when Async_None =>
            Assert_True (False);
      end case;
      Async_Done := True;
   end On_Async_Ready;

   procedure Iterate_Once is
      Dispatched : constant Boolean :=
        Glib.Main.Main_Context_Iteration (May_Block => True);
      pragma Unreferenced (Dispatched);
   begin
      null;
   end Iterate_Once;

   procedure Run_Async is
   begin
      --  Async GIO callbacks are dispatched by the thread-default GLib
      --  context, which this loop iterates until the operation's callback.
      while not Async_Done loop
         Iterate_Once;
      end loop;
      Assert_True (Async_Error = null);
   end Run_Async;

   procedure Wait_While_Pending (Stream : Goutput_Stream) is
   begin
      --  A null callback is legal; pending state tells us when it completed.
      while Has_Pending (Stream) loop
         Iterate_Once;
      end loop;
   end Wait_While_Pending;

   --------------------------
   -- Test_State_And_Flush --
   --------------------------

   --  Verify the pending flag transitions and the synchronous flush/close
   --  state queries on a stream with real buffered output.
   procedure Test_State_And_Flush is
      Stream : constant Goutput_Stream := New_Memory_Stream;
      Buffer : constant Guint8_Array := To_Buffer ("flush me");
      Error  : Glib.Error.GError;
   begin
      Assert_True (Stream /= null);
      Assert_True (not Has_Pending (Stream));
      Assert_True (not Is_Closed (Stream));
      Assert_True (not Is_Closing (Stream));

      Assert_True (Set_Pending (Stream, Error));
      Assert_True (Error = null);
      Assert_True (Has_Pending (Stream));

      --  A second pending request is rejected with a GError.
      Assert_True (not Set_Pending (Stream, Error));
      Assert_True (Error /= null);
      Glib.Error.Error_Free (Error);
      Error := null;

      Clear_Pending (Stream);
      Assert_True (not Has_Pending (Stream));

      Assert_Cmpint_Eq
        (Gint (Write (Stream, Buffer, null, Error)), Gint (Buffer'Length));
      Assert_True (Error = null);
      Assert_True (Flush (Stream, null, Error));
      Assert_True (Error = null);
      Assert_Cmpstr_Eq (Contents (Stream), "flush me");

      Assert_True (Close (Stream, null, Error));
      Assert_True (Error = null);
      Assert_True (Is_Closed (Stream));
      Assert_True (not Is_Closing (Stream));

      --  GLib documents repeated close as successful and harmless.
      Assert_True (Close (Stream, null, Error));
      Assert_True (Error = null);

      --  A closed stream cannot be marked pending.
      Assert_True (not Set_Pending (Stream, Error));
      Assert_True (Error /= null);
      Glib.Error.Error_Free (Error);
      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_State_And_Flush;

   ---------------------
   -- Test_Write_Bytes --
   ---------------------

   --  Write_Bytes accepts a refcounted byte sequence and preserves its data.
   procedure Test_Write_Bytes is
      Stream  : constant Goutput_Stream := New_Memory_Stream;
      Buffer  : constant Guint8_Array := To_Buffer ("boxed bytes");
      Bytes   : Glib.Bytes.Gbytes := Glib.Bytes.Gbytes_New
        (Buffer, Buffer'Length);
      Written : Gssize;
      Error   : Glib.Error.GError;
   begin
      Written := Write_Bytes (Stream, Bytes, null, Error);
      Assert_True (Error = null);
      Assert_Cmpint_Eq (Gint (Written), Gint (Buffer'Length));
      Assert_Cmpstr_Eq (Contents (Stream), "boxed bytes");
      Unref_Bytes (Glib.Get_Object (Bytes));
      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_Write_Bytes;

   -----------------
   -- Test_Async --
   -----------------

   --  Exercise each bound async start/finish pair and each nullable callback
   --  branch. Buffers stay in scope until their callbacks have completed.
   procedure Test_Async is
      Stream       : Goutput_Stream;
      Source       : Ginput_Stream;
      Buffer       : constant Guint8_Array := To_Buffer ("async data" & ASCII.LF);
      Bytes        : Glib.Bytes.Gbytes := Glib.Bytes.Gbytes_New
        (Buffer, Buffer'Length);
      Error        : Glib.Error.GError;
      Expected_Size : constant Gssize := Gssize (Buffer'Length);
   begin
      --  Write_Async and its finish method report the number of bytes.
      Stream := New_Memory_Stream;
      Async_Target := Stream;
      Current_Async := Async_Write;
      Async_Done := False;
      Async_Error := null;
      Write_Async (Stream, Buffer, 0, null, On_Async_Ready'Unrestricted_Access);
      Run_Async;
      Assert_Cmpint_Eq (Gint (Async_Size_Result), Gint (Expected_Size));
      Assert_Cmpstr_Eq (Contents (Stream), "async data" & ASCII.LF);

      --  The all-write pair also returns its optional byte count.
      Current_Async := Async_Write_All;
      Async_Done := False;
      Async_Bytes_Written := 0;
      Async_Error := null;
      Write_All_Async
        (Stream, Buffer, 0, null, On_Async_Ready'Unrestricted_Access);
      Run_Async;
      Assert_True (Async_Boolean_Result);
      Assert_Cmpint_Eq (Gint (Async_Bytes_Written), Gint (Expected_Size));

      --  The boxed-byte async pair holds the GBytes data through completion.
      Current_Async := Async_Write_Bytes;
      Async_Done := False;
      Async_Error := null;
      Write_Bytes_Async
        (Stream, Bytes, 0, null, On_Async_Ready'Unrestricted_Access);
      Run_Async;
      Assert_Cmpint_Eq (Gint (Async_Size_Result), Gint (Expected_Size));

      --  Flush_Async reports its result through Flush_Finish.
      Current_Async := Async_Flush;
      Async_Done := False;
      Async_Error := null;
      Flush_Async (Stream, 0, null, On_Async_Ready'Unrestricted_Access);
      Run_Async;
      Assert_True (Async_Boolean_Result);

      --  Splice_Async uses the same resource fixture as the sync splice case.
      Source := Open_Alpha;
      Current_Async := Async_Splice;
      Async_Done := False;
      Async_Error := null;
      Splice_Async
        (Stream, Source, G_Output_Stream_Splice_Close_Source,
         0, null, On_Async_Ready'Unrestricted_Access);
      Run_Async;
      Assert_Cmpint_Eq (Gint (Async_Size_Result), Gint (Alpha_Text'Length));
      Assert_True (Is_Closed (Source));
      Glib.Object.Unref (Glib.Object.GObject (Source));

      --  Close_Async's callback path is paired with Close_Finish.
      Current_Async := Async_Close;
      Async_Done := False;
      Async_Error := null;
      Close_Async (Stream, 0, null, On_Async_Ready'Unrestricted_Access);
      Run_Async;
      Assert_True (Async_Boolean_Result);
      Assert_True (Is_Closed (Stream));
      Glib.Object.Unref (Glib.Object.GObject (Stream));

      --  The same async entry points accept a null callback. Their pending
      --  state is cleared on completion, after which their effects are checked.
      Stream := New_Memory_Stream;
      Write_Async (Stream, Buffer, 0, null, null);
      Wait_While_Pending (Stream);
      Assert_Cmpstr_Eq (Contents (Stream), "async data" & ASCII.LF);

      Write_All_Async (Stream, Buffer, 0, null, null);
      Wait_While_Pending (Stream);
      Assert_Cmpstr_Eq
        (Contents (Stream), ("async data" & ASCII.LF) & ("async data" & ASCII.LF));

      Write_Bytes_Async (Stream, Bytes, 0, null, null);
      Wait_While_Pending (Stream);
      Assert_Cmpstr_Eq
        (Contents (Stream),
         ("async data" & ASCII.LF) & ("async data" & ASCII.LF)
         & ("async data" & ASCII.LF));

      Flush_Async (Stream, 0, null, null);
      Wait_While_Pending (Stream);
      Assert_True (Flush (Stream, null, Error));
      Assert_True (Error = null);

      Source := Open_Alpha;
      Splice_Async (Stream, Source, G_Output_Stream_Splice_None, 0, null, null);
      Wait_While_Pending (Stream);
      Assert_Cmpstr_Eq
        (Contents (Stream),
         ("async data" & ASCII.LF) & ("async data" & ASCII.LF)
         & ("async data" & ASCII.LF) & Alpha_Text);
      Glib.Object.Unref (Glib.Object.GObject (Source));

      Close_Async (Stream, 0, null, null);
      while not Is_Closed (Stream) loop
         Iterate_Once;
      end loop;
      Assert_True (Is_Closed (Stream));

      Unref_Bytes (Glib.Get_Object (Bytes));
      Glib.Object.Unref (Glib.Object.GObject (Stream));
      Current_Async := Async_None;
   end Test_Async;

begin
   Glib.Test.Init;

   Glib.Test.Add_Func ("/goutputstream/write", Test_Write'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/goutputstream/write-slice", Test_Write_Slice'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/goutputstream/write-all", Test_Write_All'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/goutputstream/splice", Test_Splice'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/goutputstream/state-and-flush",
      Test_State_And_Flush'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/goutputstream/write-bytes", Test_Write_Bytes'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/goutputstream/async", Test_Async'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
