--  Glib.File_Output_Stream.Query_Info.
--
--  No widget is created, so this test needs no display.

with Ada.Command_Line;
with Ada.Directories;
with System;

with Glib;                    use Glib;
with Glib.Error;
with Glib.File_Info;          use Glib.File_Info;
with Glib.File_Output_Stream; use Glib.File_Output_Stream;
with Glib.GFile;              use Glib.GFile;
with Glib.Main;
with Glib.Object;
with Glib.Output_Stream;
with Glib.Test;                use Glib.Test;
with Glib.Types;

procedure Main is

   subtype Gfile is Glib.GFile.Gfile;

   use type Glib.Error.GError;
   use type System.Address;

   Base : constant String := Ada.Directories.Compose
     (Ada.Directories.Current_Directory, "scratch");
   Data_Path_1 : constant String :=
     Ada.Directories.Compose (Base, "outstream1.dat");
   Data_Path_2 : constant String :=
     Ada.Directories.Compose (Base, "outstream2.dat");
   Data_Path_3 : constant String :=
     Ada.Directories.Compose (Base, "outstream3.dat");
   Data_Path_4 : constant String :=
     Ada.Directories.Compose (Base, "outstream4.dat");
   Data_Path_5 : constant String :=
     Ada.Directories.Compose (Base, "outstream5.dat");
   Data_Path_6 : constant String :=
     Ada.Directories.Compose (Base, "outstream6.dat");

   --  The async callback completes the request before the test releases the
   --  stream, so these globals remain valid for the callback's full lifetime.
   Async_Stream : Gfile_Output_Stream;
   Async_Info   : Gfile_Info;
   Async_Error  : Glib.Error.GError;
   Async_Done   : Boolean := False;
   Async_Source_Matches : Boolean := False;

   procedure Release (File : Gfile);
   procedure On_Query_Info_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);
   function To_Buffer (Text : String) return Guint8_Array;

   -------------
   -- Release --
   -------------

   procedure Release (File : Gfile) is
   begin
      Glib.Object.Unref
        (Glib.Types.To_Object (Glib.Types.GType_Interface (File)));
   end Release;

   ----------------
   -- To_Buffer --
   ----------------

   --  File output streams accept bytes, so convert the ETag test's text into
   --  the byte array used by Output_Stream.Write_All.
   function To_Buffer (Text : String) return Guint8_Array is
      Result : Guint8_Array (1 .. Text'Length);
      Index  : Natural := Result'First;
   begin
      for C of Text loop
         Result (Index) := Guint8 (Character'Pos (C));
         Index := Index + 1;
      end loop;
      return Result;
   end To_Buffer;

   -------------------------
   -- On_Query_Info_Ready --
   -------------------------

   --  Finish inside the callback while the async result and source stream are
   --  both alive; the test body checks the saved outcome after dispatch.
   procedure On_Query_Info_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result) is
   begin
      Async_Source_Matches :=
        Glib.Object.Get_Object (Source_Object) =
          Glib.Object.Get_Object
            (Glib.Object.GObject (Async_Stream));
      Async_Info := Async_Stream.Query_Info_Finish (Res, Async_Error);
      Async_Done := True;
   end On_Query_Info_Ready;

   -----------------------
   -- Wait_For_Async --
   -----------------------

   --  Drive only the default main context until GIO reports completion.
   procedure Wait_For_Async is
      Dispatched : Boolean;
   begin
      for I in 1 .. 5_000 loop
         exit when Async_Done;
         Dispatched := Glib.Main.Main_Context_Iteration (May_Block => False);
         delay 0.001;
      end loop;
      Assert_True (Async_Done);
   end Wait_For_Async;

   procedure Test_Query_Info_Async
   with Convention => C;

   procedure Test_Query_Info_Async_After_Close
   with Convention => C;

   procedure Test_Query_Info_Async_Without_Callback
   with Convention => C;

   procedure Test_Get_Etag_After_Write
   with Convention => C;

   procedure Test_Query_Info_Success
   with Convention => C;

   procedure Test_Query_Info_After_Close
   with Convention => C;

   ------------------------------
   -- Test_Query_Info_Success --
   ------------------------------

   procedure Test_Query_Info_Success is
      File   : constant Gfile := New_For_Path (Data_Path_1);
      Error  : Glib.Error.GError;
      Stream : constant Gfile_Output_Stream :=
        Create (File, G_File_Create_None, null, Error);
      Info   : Gfile_Info;
   begin
      Assert_True (Error = null);
      Assert_True (Stream /= null);

      Info := Stream.Query_Info
        (Attributes => "standard::*", Cancellable => null, Error => Error);

      Assert_True (Error = null);
      Assert_True (Info /= null);

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
      Stream : constant Gfile_Output_Stream :=
        Create (File, G_File_Create_None, null, Error);
      Info   : Gfile_Info;
   begin
      Assert_True (Error = null);
      Assert_True (Stream /= null);
      Assert_True
        (Glib.Output_Stream.Close
           (Glib.Output_Stream.Goutput_Stream (Stream), null, Error));
      Assert_True (Error = null);

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

   --  Check the non-null callback path and its matching finish operation.
   procedure Test_Query_Info_Async is
      File   : constant Gfile := New_For_Path (Data_Path_3);
      Error  : Glib.Error.GError;
      Stream : constant Gfile_Output_Stream :=
        Create (File, G_File_Create_None, null, Error);
   begin
      Assert_True (Error = null);
      Assert_True (Stream /= null);

      Async_Stream := Stream;
      Async_Info := null;
      Async_Error := null;
      Async_Done := False;
      Async_Source_Matches := False;
      Stream.Query_Info_Async
        (Attributes  => "standard::*",
         Io_Priority => 0,
         Cancellable => null,
         Callback    => On_Query_Info_Ready'Unrestricted_Access);
      Wait_For_Async;

      Assert_True (Async_Source_Matches);
      Assert_True (Async_Error = null);
      Assert_True (Async_Info /= null);
      Glib.Object.Unref (Glib.Object.GObject (Async_Info));
      Async_Info := null;

      Assert_True
        (Glib.Output_Stream.Close
           (Glib.Output_Stream.Goutput_Stream (Stream), null, Error));
      Assert_True (Error = null);
      Glib.Object.Unref (Glib.Object.GObject (Stream));
      Release (File);
      Async_Stream := null;
   end Test_Query_Info_Async;

   --------------------------------------
   -- Test_Query_Info_Async_After_Close --
   --------------------------------------

   --  A finished async request on a closed stream must return null and set
   --  GError, just as the synchronous operation does.
   procedure Test_Query_Info_Async_After_Close is
      File   : constant Gfile := New_For_Path (Data_Path_4);
      Error  : Glib.Error.GError;
      Stream : constant Gfile_Output_Stream :=
        Create (File, G_File_Create_None, null, Error);
   begin
      Assert_True (Error = null);
      Assert_True (Stream /= null);
      Assert_True
        (Glib.Output_Stream.Close
           (Glib.Output_Stream.Goutput_Stream (Stream), null, Error));
      Assert_True (Error = null);

      Async_Stream := Stream;
      Async_Info := null;
      Async_Error := null;
      Async_Done := False;
      Async_Source_Matches := False;
      Stream.Query_Info_Async
        (Attributes  => "standard::*",
         Io_Priority => 0,
         Cancellable => null,
         Callback    => On_Query_Info_Ready'Unrestricted_Access);
      Wait_For_Async;

      Assert_True (Async_Source_Matches);
      Assert_True (Async_Info = null);
      Assert_True (Async_Error /= null);
      Glib.Error.Error_Free (Async_Error);
      Async_Error := null;
      Glib.Object.Unref (Glib.Object.GObject (Stream));
      Release (File);
      Async_Stream := null;
   end Test_Query_Info_Async_After_Close;

   --------------------------------------------
   -- Test_Query_Info_Async_Without_Callback --
   --------------------------------------------

   --  GLib permits a null callback for this fire-and-forget request. A later
   --  synchronous query proves the stream is usable after it completes.
   procedure Test_Query_Info_Async_Without_Callback is
      File   : constant Gfile := New_For_Path (Data_Path_6);
      Error  : Glib.Error.GError;
      Stream : constant Gfile_Output_Stream :=
        Create (File, G_File_Create_None, null, Error);
      Info : Gfile_Info;
      Dispatched : Boolean;
   begin
      Assert_True (Error = null);
      Assert_True (Stream /= null);

      Stream.Query_Info_Async
        (Attributes  => "standard::*",
         Io_Priority => 0,
         Cancellable => null,
         Callback    => null);

      --  Query_Info can report G_IO_ERROR_PENDING while the async request is
      --  active. Retry after processing the main context until it completes.
      for I in 1 .. 5_000 loop
         Info := Stream.Query_Info
           (Attributes => "standard::*", Cancellable => null, Error => Error);
         exit when Error = null;
         Glib.Error.Error_Free (Error);
         Error := null;
         Dispatched := Glib.Main.Main_Context_Iteration (May_Block => False);
         delay 0.001;
      end loop;
      Assert_True (Error = null);
      Assert_True (Info /= null);
      Glib.Object.Unref (Glib.Object.GObject (Info));

      Assert_True
        (Glib.Output_Stream.Close
           (Glib.Output_Stream.Goutput_Stream (Stream), null, Error));
      Assert_True (Error = null);
      Glib.Object.Unref (Glib.Object.GObject (Stream));
      Release (File);
   end Test_Query_Info_Async_Without_Callback;

   -----------------------------
   -- Test_Get_Etag_After_Write --
   -----------------------------

   --  The entity tag is available after the newly created file has been
   --  written and the stream is closed.
   procedure Test_Get_Etag_After_Write is
      File   : constant Gfile := New_For_Path (Data_Path_5);
      Error  : Glib.Error.GError;
      Stream : constant Gfile_Output_Stream :=
        Create (File, G_File_Create_None, null, Error);
   begin
      Assert_True (Error = null);
      Assert_True (Stream /= null);
      Assert_True
        (Glib.Output_Stream.Write_All
           (Glib.Output_Stream.Goutput_Stream (Stream),
            To_Buffer ("etag"),
            Cancellable => null,
            Error       => Error));
      Assert_True (Error = null);
      Assert_True
        (Glib.Output_Stream.Close
           (Glib.Output_Stream.Goutput_Stream (Stream), null, Error));
      Assert_True (Error = null);
      Assert_True (Stream.Get_Etag /= "");

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

   if Ada.Directories.Exists (Data_Path_5) then
      Ada.Directories.Delete_File (Data_Path_5);
   end if;

   if Ada.Directories.Exists (Data_Path_6) then
      Ada.Directories.Delete_File (Data_Path_6);
   end if;

   Glib.Test.Add_Func
     ("/glib-file-output-stream/query-info-success",
      Test_Query_Info_Success'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-file-output-stream/query-info-after-close",
      Test_Query_Info_After_Close'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-file-output-stream/query-info-async",
      Test_Query_Info_Async'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-file-output-stream/query-info-async-after-close",
      Test_Query_Info_Async_After_Close'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-file-output-stream/query-info-async-without-callback",
      Test_Query_Info_Async_Without_Callback'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-file-output-stream/get-etag-after-write",
      Test_Get_Etag_After_Write'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
