--  Ginput_Stream tests: reading from the stream returned by
--  Glib.Resource.Open_Stream.
--
--  The binary fixture sample.gresource is generated from sample.gresource.xml,
--  alpha.txt and beta.txt. Regenerate it from testsuite/tests/ginputstream
--  with:
--    glib-compile-resources sample.gresource.xml --target=sample.gresource
--      --sourcedir=.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib;              use Glib;
with Glib.Bytes;        use Glib.Bytes;
with Glib.Error;
with Glib.Input_Stream; use Glib.Input_Stream;
with Glib.Main;
with Glib.Object;
with Glib.Resource;     use Glib.Resource;
with Glib.Test;         use Glib.Test;

procedure Main is

   Alpha_Text : constant UTF8_String := "alpha resource" & ASCII.LF;
   Beta_Text  : constant UTF8_String := "beta value" & ASCII.LF;

   use type Glib.Error.GError;
   use type Glib.Bytes.Gbytes;

   type Async_Kind is
     (Async_None, Async_Close, Async_Read_Bytes, Async_Skip,
      Async_Read, Async_Read_All);

   Current_Async       : Async_Kind := Async_None;
   Async_Done          : Boolean := False;
   Async_Target        : Ginput_Stream;
   Async_Error         : Glib.Error.GError;
   Async_Boolean       : Boolean := False;
   Async_Size          : Gssize := 0;
   Async_Bytes_Read    : Gsize := 0;
   Async_Bytes         : Gbytes;
   Async_Buffer        : aliased Guint8_Array (1 .. 64) := (others => 0);

   --  Two C-only starters cover the matching finish methods that GtkAda
   --  exposes without an Ada declaration for their async starters.
   procedure C_Read_Async
     (Self        : System.Address;
      Buffer      : System.Address;
      Count       : Gsize;
      Io_Priority : Gint;
      Cancellable : System.Address;
      Callback    : System.Address;
      User_Data   : System.Address);
   pragma Import (C, C_Read_Async, "g_input_stream_read_async");

   procedure C_Read_All_Async
     (Self        : System.Address;
      Buffer      : System.Address;
      Count       : Gsize;
      Io_Priority : Gint;
      Cancellable : System.Address;
      Callback    : System.Address;
      User_Data   : System.Address);
   pragma Import (C, C_Read_All_Async, "g_input_stream_read_all_async");

   type C_Ready_Callback is access procedure
     (Source_Object : System.Address;
      Res           : Glib.G_Async_Result;
      Data          : System.Address)
     with Convention => C;

   function To_Address is new Ada.Unchecked_Conversion
     (C_Ready_Callback, System.Address);

   procedure On_Async_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);

   procedure On_C_Async_Ready
     (Source_Object : System.Address;
      Res           : Glib.G_Async_Result;
      Data          : System.Address)
     with Convention => C;

   procedure Iterate_Once;
   procedure Run_Async;
   procedure Wait_While_Pending (Stream : Ginput_Stream);

   function Open (Path : UTF8_String) return Ginput_Stream;
   --  Open Path in sample.gresource. The resource itself is released at once:
   --  the stream keeps it alive for as long as it needs it.

   function To_String (Buffer : Guint8_Array) return String;

   function To_String (Bytes : Gbytes) return String;

   ----------
   -- Open --
   ----------

   function Open (Path : UTF8_String) return Ginput_Stream is
      Load_Error   : Glib.Error.GError;
      Stream_Error : Glib.Error.GError;
      Resource     : Gresource := Load ("sample.gresource", Load_Error);
      Stream       : constant Ginput_Stream := Open_Stream
        (Self         => Resource,
         Path         => Path,
         Lookup_Flags => G_Resource_Lookup_Flags_None,
         Error        => Stream_Error);
   begin
      Assert_True (Load_Error = null);
      Assert_True (Stream_Error = null);
      Assert_True (Resource /= Null_Gresource);
      Unref (Resource);

      return Stream;
   end Open;

   ---------------
   -- To_String --
   ---------------

   function To_String (Buffer : Guint8_Array) return String is
      Result : String (1 .. Buffer'Length);
      Index  : Natural := Result'First;
   begin
      for B of Buffer loop
         Result (Index) := Character'Val (Integer (B));
         Index := Index + 1;
      end loop;

      return Result;
   end To_String;

   ----------------
   -- To_String  --
   ----------------

   function To_String (Bytes : Gbytes) return String is
      Size : constant Natural := Natural (Get_Size (Bytes));
      subtype Byte_Array is Guint8_Array (1 .. Size);
      Data : Byte_Array;
      for Data'Address use Get_Region
        (Bytes, Element_Size => 1, Offset => 0, N_Elements => Gsize (Size));
      Result : String (1 .. Size);
   begin
      --  GBytes owns the immutable storage for the lifetime of this view.
      for Index in Result'Range loop
         Result (Index) := Character'Val (Integer (Data (Index)));
      end loop;
      return Result;
   end To_String;

   ----------------------------
   -- On_Async_Ready / waits --
   ----------------------------

   --  Each callback calls the corresponding finish function before the
   --  asynchronous result becomes invalid.
   procedure On_Async_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result)
   is
      pragma Unreferenced (Source_Object);
   begin
      case Current_Async is
         when Async_Close =>
            Async_Boolean := Close_Finish (Async_Target, Res, Async_Error);
         when Async_Read_Bytes =>
            Async_Bytes := Read_Bytes_Finish
              (Async_Target, Res, Async_Error);
         when Async_Skip =>
            Async_Size := Skip_Finish (Async_Target, Res, Async_Error);
         when others =>
            Assert_True (False);
      end case;
      Async_Done := True;
   end On_Async_Ready;

   --  The C-only read starters pass a three-argument GLib callback directly.
   procedure On_C_Async_Ready
     (Source_Object : System.Address;
      Res           : Glib.G_Async_Result;
      Data          : System.Address)
   is
      pragma Unreferenced (Source_Object, Data);
   begin
      case Current_Async is
         when Async_Read =>
            Async_Size := Read_Finish (Async_Target, Res, Async_Error);
         when Async_Read_All =>
            Async_Boolean := Read_All_Finish
              (Async_Target, Res, Async_Bytes_Read, Async_Error);
         when others =>
            Assert_True (False);
      end case;
      Async_Done := True;
   end On_C_Async_Ready;

   procedure Iterate_Once is
      Dispatched : constant Boolean :=
        Glib.Main.Main_Context_Iteration (May_Block => True);
      pragma Unreferenced (Dispatched);
   begin
      null;
   end Iterate_Once;

   procedure Run_Async is
   begin
      --  GIO callbacks run through the thread-default main context.
      while not Async_Done loop
         Iterate_Once;
      end loop;
      Assert_True (Async_Error = null);
   end Run_Async;

   procedure Wait_While_Pending (Stream : Ginput_Stream) is
   begin
      --  A null callback is valid; the pending flag reports completion.
      while Has_Pending (Stream) loop
         Iterate_Once;
      end loop;
   end Wait_While_Pending;

   procedure Test_Read
   with Convention => C;

   procedure Test_Read_Slice
   with Convention => C;

   procedure Test_Read_All
   with Convention => C;

   procedure Test_Close
   with Convention => C;

   procedure Test_Read_Bytes
   with Convention => C;

   procedure Test_Skip_And_Eof
   with Convention => C;

   procedure Test_Pending
   with Convention => C;

   procedure Test_Closed_Errors
   with Convention => C;

   procedure Test_Async
   with Convention => C;

   ---------------
   -- Test_Read --
   ---------------

   procedure Test_Read is
      Stream : constant Ginput_Stream := Open ("/org/gtkada/test/alpha.txt");
      Buffer : Guint8_Array (1 .. 64) := (others => 0);
      Read_Count : Gssize;
      Error      : Glib.Error.GError;
   begin
      Assert_True (Stream /= null);

      Read_Count := Read
        (Self        => Stream,
         Buffer      => Buffer,
         Cancellable => null,
         Error       => Error);

      Assert_True (Error = null);
      Assert_Cmpint_Eq (Gint (Read_Count), Gint (Alpha_Text'Length));
      Assert_Cmpstr_Eq
        (To_String (Buffer (1 .. Natural (Read_Count))), Alpha_Text);

      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_Read;

   ---------------------
   -- Test_Read_Slice --
   ---------------------

   procedure Test_Read_Slice is
      Stream : constant Ginput_Stream := Open ("/org/gtkada/test/alpha.txt");
      Buffer : Guint8_Array (1 .. 64) := (others => 0);
      Read_Count : Gssize;
      Error      : Glib.Error.GError;
   begin
      Assert_True (Stream /= null);

      --  "Count" is not part of the Ada profile: a caller who wants to read
      --  less passes a slice, whose 'Length is handed to C.

      Read_Count := Read
        (Self        => Stream,
         Buffer      => Buffer (1 .. 5),
         Cancellable => null,
         Error       => Error);

      Assert_True (Error = null);
      Assert_Cmpint_Eq (Gint (Read_Count), 5);
      Assert_Cmpstr_Eq (To_String (Buffer (1 .. 5)), Alpha_Text (1 .. 5));

      --  Nothing was written past the slice.

      Assert_True (Buffer (6) = 0);

      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_Read_Slice;

   -------------------
   -- Test_Read_All --
   -------------------

   procedure Test_Read_All is
      Stream     : constant Ginput_Stream := Open ("/org/gtkada/test/beta.txt");
      Buffer     : Guint8_Array (1 .. 64) := (others => 0);
      Bytes_Read : Gsize;
      Error      : Glib.Error.GError;
   begin
      Assert_True (Stream /= null);

      Assert_True
        (Read_All
           (Self        => Stream,
            Buffer      => Buffer,
            Bytes_Read  => Bytes_Read,
            Cancellable => null,
            Error       => Error));

      Assert_True (Error = null);
      Assert_Cmpint_Eq (Gint (Bytes_Read), Gint (Beta_Text'Length));
      Assert_Cmpstr_Eq
        (To_String (Buffer (1 .. Natural (Bytes_Read))), Beta_Text);

      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_Read_All;

   ----------------
   -- Test_Close --
   ----------------

   procedure Test_Close is
      Stream : constant Ginput_Stream := Open ("/org/gtkada/test/alpha.txt");
      Error  : Glib.Error.GError;
   begin
      Assert_True (Stream /= null);
      Assert_False (Is_Closed (Stream));

      Assert_True (Close (Self => Stream, Cancellable => null, Error => Error));
      Assert_True (Error = null);
      Assert_True (Is_Closed (Stream));

      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_Close;

   ---------------------
   -- Test_Read_Bytes --
   ---------------------

   --  Check both the length and bytes returned by the boxed GBytes result.
   procedure Test_Read_Bytes is
      Stream : constant Ginput_Stream := Open ("/org/gtkada/test/alpha.txt");
      Bytes  : Gbytes;
      Error  : Glib.Error.GError;
   begin
      Bytes := Read_Bytes
        (Self => Stream, Count => 5, Cancellable => null, Error => Error);
      Assert_True (Error = null);
      Assert_True (Bytes /= Null_Gbytes);
      Assert_Cmpint_Eq (Gint (Get_Size (Bytes)), 5);
      Assert_Cmpstr_Eq (To_String (Bytes), "alpha");
      Unref (Bytes);
      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_Read_Bytes;

   ----------------------
   -- Test_Skip_And_Eof --
   ----------------------

   --  Skipping advances the stream, and a read after the final byte reports
   --  end of file as zero bytes without setting an error.
   procedure Test_Skip_And_Eof is
      Stream : constant Ginput_Stream := Open ("/org/gtkada/test/alpha.txt");
      Buffer : Guint8_Array (1 .. 64) := (others => 0);
      Count  : Gssize;
      Error  : Glib.Error.GError;
   begin
      Count := Skip (Stream, 6, null, Error);
      Assert_True (Error = null);
      Assert_Cmpint_Eq (Gint (Count), 6);
      Count := Read (Stream, Buffer, null, Error);
      Assert_True (Error = null);
      Assert_Cmpint_Eq (Gint (Count), 9);
      Assert_Cmpstr_Eq (To_String (Buffer (1 .. Natural (Count))),
                        "resource" & ASCII.LF);
      Count := Read (Stream, Buffer, null, Error);
      Assert_True (Error = null);
      Assert_Cmpint_Eq (Gint (Count), 0);
      Assert_True (Close (Stream, null, Error));
      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_Skip_And_Eof;

   -------------------
   -- Test_Pending --
   -------------------

   --  The pending flag is exclusive: setting it twice reports an error, and
   --  Clear_Pending makes the stream available again.
   procedure Test_Pending is
      Stream : constant Ginput_Stream := Open ("/org/gtkada/test/alpha.txt");
      Error  : Glib.Error.GError;
   begin
      Assert_False (Has_Pending (Stream));
      Assert_True (Set_Pending (Stream, Error));
      Assert_True (Error = null);
      Assert_True (Has_Pending (Stream));
      Assert_False (Set_Pending (Stream, Error));
      Assert_True (Error /= null);
      Glib.Error.Error_Free (Error);
      Clear_Pending (Stream);
      Assert_False (Has_Pending (Stream));
      Assert_True (Set_Pending (Stream, Error));
      Clear_Pending (Stream);
      Assert_True (Close (Stream, null, Error));
      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_Pending;

   -------------------------
   -- Test_Closed_Errors --
   -------------------------

   --  These result types have defined error returns, so verify their closed
   --  stream behavior without relying on a numeric read/skip error result.
   procedure Test_Closed_Errors is
      Stream     : constant Ginput_Stream := Open ("/org/gtkada/test/alpha.txt");
      Buffer     : Guint8_Array (1 .. 8) := (others => 0);
      Bytes      : Gbytes;
      Bytes_Read : Gsize;
      Error      : Glib.Error.GError;
   begin
      Assert_True (Close (Stream, null, Error));
      Assert_True (Is_Closed (Stream));
      Assert_False
        (Read_All (Stream, Buffer, Bytes_Read, null, Error));
      Assert_True (Error /= null);
      Assert_Cmpint_Eq (Gint (Bytes_Read), 0);
      Glib.Error.Error_Free (Error);
      Bytes := Read_Bytes (Stream, 4, null, Error);
      Assert_True (Bytes = Null_Gbytes);
      Assert_True (Error /= null);
      Glib.Error.Error_Free (Error);
      Assert_True (Close (Stream, null, Error));
      Assert_True (Error = null);
      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_Closed_Errors;

   ----------------
   -- Test_Async --
   ----------------

   --  Exercise callback and null-callback paths, plus the finish routines for
   --  read and read-all through their C starters.
   procedure Test_Async is
      Stream : Ginput_Stream;
   begin
      Stream := Open ("/org/gtkada/test/alpha.txt");
      Async_Target := Stream;
      Current_Async := Async_Read_Bytes;
      Async_Done := False;
      Async_Error := null;
      Read_Bytes_Async
        (Stream, 5, 0, null, On_Async_Ready'Unrestricted_Access);
      Run_Async;
      Assert_Cmpstr_Eq (To_String (Async_Bytes), "alpha");
      Unref (Async_Bytes);
      Glib.Object.Unref (Glib.Object.GObject (Stream));

      --  Skip_Async's completion reports the number of bytes skipped.
      Stream := Open ("/org/gtkada/test/alpha.txt");
      Async_Target := Stream;
      Current_Async := Async_Skip;
      Async_Done := False;
      Async_Error := null;
      Skip_Async (Stream, 6, 0, null, On_Async_Ready'Unrestricted_Access);
      Run_Async;
      Assert_Cmpint_Eq (Gint (Async_Size), 6);
      Glib.Object.Unref (Glib.Object.GObject (Stream));

      --  Close_Async uses the same callback bridge and its matching finish.
      Stream := Open ("/org/gtkada/test/alpha.txt");
      Async_Target := Stream;
      Current_Async := Async_Close;
      Async_Done := False;
      Async_Error := null;
      Close_Async (Stream, 0, null, On_Async_Ready'Unrestricted_Access);
      Run_Async;
      Assert_True (Async_Boolean);
      Assert_True (Is_Closed (Stream));
      Glib.Object.Unref (Glib.Object.GObject (Stream));

      --  The raw starters here let the exposed finish wrappers receive real
      --  GAsyncResult values for operations without an Ada async declaration.
      Stream := Open ("/org/gtkada/test/alpha.txt");
      Async_Target := Stream;
      Current_Async := Async_Read;
      Async_Buffer := (others => 0);
      Async_Done := False;
      Async_Error := null;
      C_Read_Async
        (Glib.Object.Get_Object (Glib.Object.GObject (Stream)),
         Async_Buffer'Address, 5, 0, System.Null_Address,
         To_Address (On_C_Async_Ready'Unrestricted_Access),
         System.Null_Address);
      Run_Async;
      Assert_Cmpint_Eq (Gint (Async_Size), 5);
      Assert_Cmpstr_Eq (To_String (Async_Buffer (1 .. 5)), "alpha");
      Glib.Object.Unref (Glib.Object.GObject (Stream));

      Stream := Open ("/org/gtkada/test/beta.txt");
      Async_Target := Stream;
      Current_Async := Async_Read_All;
      Async_Buffer := (others => 0);
      Async_Bytes_Read := 0;
      Async_Done := False;
      Async_Error := null;
      C_Read_All_Async
        (Glib.Object.Get_Object (Glib.Object.GObject (Stream)),
         Async_Buffer'Address, Async_Buffer'Length, 0,
         System.Null_Address,
         To_Address (On_C_Async_Ready'Unrestricted_Access),
         System.Null_Address);
      Run_Async;
      Assert_True (Async_Boolean);
      Assert_Cmpint_Eq (Gint (Async_Bytes_Read), Gint (Beta_Text'Length));
      Assert_Cmpstr_Eq
        (To_String (Async_Buffer (1 .. Natural (Async_Bytes_Read))), Beta_Text);
      Glib.Object.Unref (Glib.Object.GObject (Stream));

      --  Null callbacks are allowed; wait on pending state before reusing the
      --  stream or inspecting its final state.
      Stream := Open ("/org/gtkada/test/alpha.txt");
      Read_Bytes_Async (Stream, 4, 0, null, null);
      Wait_While_Pending (Stream);
      Skip_Async (Stream, 1, 0, null, null);
      Wait_While_Pending (Stream);
      Close_Async (Stream, 0, null, null);
      while not Is_Closed (Stream) loop
         Iterate_Once;
      end loop;
      Assert_True (Is_Closed (Stream));
      Glib.Object.Unref (Glib.Object.GObject (Stream));
      Current_Async := Async_None;
   end Test_Async;

begin
   Glib.Test.Init;

   Glib.Test.Add_Func ("/ginputstream/read", Test_Read'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/ginputstream/read-slice", Test_Read_Slice'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/ginputstream/read-all", Test_Read_All'Unrestricted_Access);
   Glib.Test.Add_Func ("/ginputstream/close", Test_Close'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/ginputstream/read-bytes", Test_Read_Bytes'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/ginputstream/skip-and-eof", Test_Skip_And_Eof'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/ginputstream/pending", Test_Pending'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/ginputstream/closed-errors", Test_Closed_Errors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/ginputstream/async", Test_Async'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
