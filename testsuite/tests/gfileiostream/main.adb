--  Glib.File_IO_Stream's synchronous and asynchronous query methods and
--  Get_Etag, and (via inheritance from Glib.IO_Stream, which has no other
--  concrete implementation to test through) Close/Is_Closed.
--
--  No widget is created, so this test needs no display.

with Ada.Command_Line;
with Ada.Directories;

with Glib;                use Glib;
with Glib.Error;
with Glib.File_Info;      use Glib.File_Info;
with Glib.File_IO_Stream; use Glib.File_IO_Stream;
with Glib.GFile;          use Glib.GFile;
with Glib.IO_Stream;      use Glib.IO_Stream;
with Glib.Main;
with Glib.Object;
with Glib.Output_Stream;
with Glib.Test;           use Glib.Test;
with Glib.Types;

procedure Main is

   subtype Gfile is Glib.GFile.Gfile;

   use type Glib.Error.GError;

   Base : constant String := Ada.Directories.Compose
     (Ada.Directories.Current_Directory, "scratch");
   Data_Path_1 : constant String :=
     Ada.Directories.Compose (Base, "iostream1.dat");
   Data_Path_2 : constant String :=
     Ada.Directories.Compose (Base, "iostream2.dat");
   Data_Path_3 : constant String :=
     Ada.Directories.Compose (Base, "iostream3.dat");
   Data_Path_4 : constant String :=
     Ada.Directories.Compose (Base, "iostream4.dat");

   --  The callback state stays in this procedure until the main context has
   --  dispatched the async completion and Query_Info_Finish has run.
   Async_Stream : Gfile_Iostream;
   Async_Info   : Gfile_Info;
   Async_Error  : Glib.Error.GError;
   Async_Done   : Boolean := False;

   procedure On_Query_Info_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);

   function To_Buffer (Text : String) return Guint8_Array;

   ----------------
   -- To_Buffer --
   ----------------

   function To_Buffer (Text : String) return Guint8_Array is
      Result : Guint8_Array (1 .. Text'Length);
      Index  : Natural := Result'First;
   begin
      --  Convert the Ada payload into the byte array expected by GIO.
      for C of Text loop
         Result (Index) := Guint8 (Character'Pos (C));
         Index := Index + 1;
      end loop;

      return Result;
   end To_Buffer;

   -------------------------
   -- On_Query_Info_Ready --
   -------------------------

   procedure On_Query_Info_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result)
   is
      pragma Unreferenced (Source_Object);
   begin
      --  A GIO async result must be completed by its matching finish call.
      Async_Info := Async_Stream.Query_Info_Finish (Res, Async_Error);
      Assert_True (Async_Error = null);
      Assert_True (Async_Info /= null);
      Async_Done := True;
   end On_Query_Info_Ready;

   procedure Release (File : Gfile);

   -------------
   -- Release --
   -------------

   procedure Release (File : Gfile) is
   begin
      Glib.Object.Unref
        (Glib.Types.To_Object (Glib.Types.GType_Interface (File)));
   end Release;

   procedure Test_Query_Info_Success
   with Convention => C;

   procedure Test_Query_Info_After_Close
   with Convention => C;

   procedure Test_Query_Info_Async
   with Convention => C;

   procedure Test_Get_Etag_After_Write
   with Convention => C;

   ------------------------------
   -- Test_Query_Info_Success --
   ------------------------------

   procedure Test_Query_Info_Success is
      File   : constant Gfile := New_For_Path (Data_Path_1);
      Error  : Glib.Error.GError;
      Stream : constant Gfile_Iostream :=
        Create_Readwrite (File, G_File_Create_None, null, Error);
      Info   : Gfile_Info;
   begin
      Assert_True (Error = null);
      Assert_True (Stream /= null);

      Info := Stream.Query_Info
        (Attributes => "standard::*", Cancellable => null, Error => Error);

      Assert_True (Error = null);
      Assert_True (Info /= null);

      Assert_True (Stream.Close (null, Error));
      Assert_True (Error = null);

      Glib.Object.Unref (Glib.Object.GObject (Info));
      Glib.Object.Unref (Glib.Object.GObject (Stream));
      Release (File);
   end Test_Query_Info_Success;

   ---------------------------------
   -- Test_Query_Info_After_Close --
   ---------------------------------

   procedure Test_Query_Info_After_Close is
      File   : constant Gfile := New_For_Path (Data_Path_2);
      Error  : Glib.Error.GError;
      Stream : constant Gfile_Iostream :=
        Create_Readwrite (File, G_File_Create_None, null, Error);
      Info   : Gfile_Info;
   begin
      Assert_True (Error = null);
      Assert_True (Stream /= null);
      Assert_True (Stream.Close (null, Error));
      Assert_True (Error = null);

      --  The stream is now closed: querying it should fail cleanly rather
      --  than return a usable Gfile_Info.

      Info := Stream.Query_Info
        (Attributes => "standard::*", Cancellable => null, Error => Error);

      Assert_True (Info = null);
      Assert_True (Error /= null);

      Glib.Error.Error_Free (Error);
      Glib.Object.Unref (Glib.Object.GObject (Stream));
      Release (File);
   end Test_Query_Info_After_Close;

   -----------------------------
   -- Test_Query_Info_Async --
   -----------------------------

   procedure Test_Query_Info_Async is
      File  : constant Gfile := New_For_Path (Data_Path_3);
      Error : Glib.Error.GError;
      Dispatched : Boolean;
   begin
      Async_Stream := Create_Readwrite (File, G_File_Create_None, null, Error);
      Assert_True (Error = null);
      Assert_True (Async_Stream /= null);

      --  Querying standard attributes dispatches the Ada callback later on
      --  the main context; the callback verifies the returned file info.
      Async_Done := False;
      Async_Stream.Query_Info_Async
        (Attributes  => "standard::*",
         Io_Priority => 0,
         Cancellable => null,
         Callback    => On_Query_Info_Ready'Unrestricted_Access);

      for I in 1 .. 5_000 loop
         exit when Async_Done;
         Dispatched := Glib.Main.Main_Context_Iteration (May_Block => False);
         delay 0.001;
      end loop;
      Assert_True (Async_Done);

      --  Query_Info_Finish returns an owned object; the stream and file
      --  references are released after the operation has completed.
      Glib.Object.Unref (Glib.Object.GObject (Async_Info));
      Async_Info := null;
      Assert_True (Async_Stream.Close (null, Error));
      Assert_True (Error = null);
      Glib.Object.Unref (Glib.Object.GObject (Async_Stream));
      Async_Stream := null;
      Release (File);
   end Test_Query_Info_Async;

   --------------------------------
   -- Test_Get_Etag_After_Write --
   --------------------------------

   procedure Test_Get_Etag_After_Write is
      File   : constant Gfile := New_For_Path (Data_Path_4);
      Error  : Glib.Error.GError;
      Stream : constant Gfile_Iostream :=
        Create_Readwrite (File, G_File_Create_None, null, Error);
      Payload : constant String := "etag payload";
   begin
      Assert_True (Error = null);
      Assert_True (Stream /= null);

      --  GFileIOStream reports its final entity tag after content is written
      --  and the stream is closed, as the tag may change during writing.
      Assert_True
        (Glib.Output_Stream.Write_All
           (Get_Output_Stream (Giostream (Stream)),
            To_Buffer (Payload),
            Cancellable => null,
            Error       => Error));
      Assert_True (Error = null);
      Assert_True (Stream.Close (null, Error));
      Assert_True (Error = null);
      Assert_True (Get_Etag (Stream) /= "");

      Glib.Object.Unref (Glib.Object.GObject (Stream));
      Release (File);
   end Test_Get_Etag_After_Write;

begin
   Glib.Test.Init;

   if not Ada.Directories.Exists (Base) then
      Ada.Directories.Create_Directory (Base);
   end if;

   if Ada.Directories.Exists (Data_Path_1) then
      Ada.Directories.Delete_File (Data_Path_1);
   end if;

   if Ada.Directories.Exists (Data_Path_2) then
      Ada.Directories.Delete_File (Data_Path_2);
   end if;

   if Ada.Directories.Exists (Data_Path_3) then
      Ada.Directories.Delete_File (Data_Path_3);
   end if;

   if Ada.Directories.Exists (Data_Path_4) then
      Ada.Directories.Delete_File (Data_Path_4);
   end if;

   Glib.Test.Add_Func
     ("/glib-file-io-stream/query-info-success",
      Test_Query_Info_Success'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-file-io-stream/query-info-after-close",
      Test_Query_Info_After_Close'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-file-io-stream/query-info-async",
      Test_Query_Info_Async'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-file-io-stream/get-etag-after-write",
      Test_Get_Etag_After_Write'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
