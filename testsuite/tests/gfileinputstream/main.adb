--  Glib.File_Input_Stream.Query_Info.
--
--  No widget is created, so this test needs no display.

with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;
with System;

with Glib;                   use Glib;
with Glib.Error;
with Glib.File_Info;         use Glib.File_Info;
with Glib.File_Input_Stream; use Glib.File_Input_Stream;
with Glib.GFile;             use Glib.GFile;
with Glib.Input_Stream;
with Glib.Main;
with Glib.Object;
with Glib.Test;              use Glib.Test;
with Glib.Types;

procedure Main is

   subtype Gfile is Glib.GFile.Gfile;

   use type Glib.Error.GError;
   use type System.Address;

   Base : constant String := Ada.Directories.Compose
     (Ada.Directories.Current_Directory, "scratch");
   Data_Path : constant String := Ada.Directories.Compose (Base, "data.txt");

   --  The async callback finishes each request while the result and stream
   --  are alive; the test body checks the outcome after main-context dispatch.
   Async_Stream : Gfile_Input_Stream;
   Async_Info   : Gfile_Info;
   Async_Error  : Glib.Error.GError;
   Async_Done   : Boolean := False;
   Async_Source_Matches : Boolean := False;

   procedure Release (File : Gfile);
   procedure On_Query_Info_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);

   -------------
   -- Release --
   -------------

   procedure Release (File : Gfile) is
   begin
      Glib.Object.Unref
        (Glib.Types.To_Object (Glib.Types.GType_Interface (File)));
   end Release;

   -------------------------
   -- On_Query_Info_Ready --
   -------------------------

   --  Complete the query in the callback, which is the only safe point to
   --  pass its GAsyncResult to Query_Info_Finish.
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

   -------------------
   -- Wait_For_Async --
   -------------------

   --  Iterate the default context until GIO invokes the completion callback.
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

   procedure Test_Query_Info_Success
   with Convention => C;

   procedure Test_Query_Info_After_Close
   with Convention => C;

   procedure Test_Query_Info_Async
   with Convention => C;

   procedure Test_Query_Info_Async_After_Close
   with Convention => C;

   procedure Test_Query_Info_Async_Without_Callback
   with Convention => C;

   ------------------------------
   -- Test_Query_Info_Success --
   ------------------------------

   procedure Test_Query_Info_Success is
      File   : constant Gfile := New_For_Path (Data_Path);
      Error  : Glib.Error.GError;
      Stream : constant Gfile_Input_Stream := Read (File, null, Error);
      Info   : Gfile_Info;
   begin
      Assert_True (Error = null);
      Assert_True (Stream /= null);

      Info := Stream.Query_Info
        (Attributes => "standard::*", Cancellable => null, Error => Error);

      Assert_True (Error = null);
      Assert_True (Info /= null);
      --  Not asserting an exact size: Ada.Text_IO may append a trailing
      --  line terminator when the file is closed.
      Assert_True (Glib.File_Info.Get_Size (Info) > 0);

      Glib.Object.Unref (Glib.Object.GObject (Info));
      Glib.Object.Unref (Glib.Object.GObject (Stream));
      Release (File);
   end Test_Query_Info_Success;

   ---------------------------------
   -- Test_Query_Info_After_Close --
   ---------------------------------

   procedure Test_Query_Info_After_Close is
      File   : constant Gfile := New_For_Path (Data_Path);
      Error  : Glib.Error.GError;
      Stream : constant Gfile_Input_Stream := Read (File, null, Error);
      Info   : Gfile_Info;
   begin
      Assert_True (Error = null);
      Assert_True (Stream /= null);
      Assert_True
        (Glib.Input_Stream.Close
           (Glib.Input_Stream.Ginput_Stream (Stream), null, Error));
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

   --  Check both the async callback bridge and the successful finish result.
   procedure Test_Query_Info_Async is
      File   : constant Gfile := New_For_Path (Data_Path);
      Error  : Glib.Error.GError;
      Stream : constant Gfile_Input_Stream := Read (File, null, Error);
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
      Glib.Object.Unref (Glib.Object.GObject (Stream));
      Release (File);
      Async_Stream := null;
   end Test_Query_Info_Async;

   --------------------------------------
   -- Test_Query_Info_Async_After_Close --
   --------------------------------------

   --  A query on a closed stream completes with a null info and a GError.
   procedure Test_Query_Info_Async_After_Close is
      File   : constant Gfile := New_For_Path (Data_Path);
      Error  : Glib.Error.GError;
      Stream : constant Gfile_Input_Stream := Read (File, null, Error);
   begin
      Assert_True (Error = null);
      Assert_True (Stream /= null);
      Assert_True
        (Glib.Input_Stream.Close
           (Glib.Input_Stream.Ginput_Stream (Stream), null, Error));
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

   --  The callback is optional. Retry the synchronous query after dispatching
   --  the main context until the async operation has released its pending flag.
   procedure Test_Query_Info_Async_Without_Callback is
      File      : constant Gfile := New_For_Path (Data_Path);
      Error     : Glib.Error.GError;
      Stream    : constant Gfile_Input_Stream := Read (File, null, Error);
      Info      : Gfile_Info;
      Dispatched : Boolean;
   begin
      Assert_True (Error = null);
      Assert_True (Stream /= null);

      Stream.Query_Info_Async
        (Attributes  => "standard::*",
         Io_Priority => 0,
         Cancellable => null,
         Callback    => null);

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
      Glib.Object.Unref (Glib.Object.GObject (Stream));
      Release (File);
   end Test_Query_Info_Async_Without_Callback;

begin
   Glib.Test.Init;

   if not Ada.Directories.Exists (Base) then
      Ada.Directories.Create_Directory (Base);
   end if;

   declare
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, Data_Path);
      Ada.Text_IO.Put (F, "hello");
      Ada.Text_IO.Close (F);
   end;

   Glib.Test.Add_Func
     ("/glib-file-input-stream/query-info-success",
      Test_Query_Info_Success'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-file-input-stream/query-info-after-close",
      Test_Query_Info_After_Close'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-file-input-stream/query-info-async",
      Test_Query_Info_Async'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-file-input-stream/query-info-async-after-close",
      Test_Query_Info_Async_After_Close'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-file-input-stream/query-info-async-without-callback",
      Test_Query_Info_Async_Without_Callback'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
