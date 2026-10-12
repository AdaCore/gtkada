--  Glib.File_Input_Stream.Query_Info.
--
--  No widget is created, so this test needs no display.

with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;

with Glib;                   use Glib;
with Glib.Error;
with Glib.File_Info;         use Glib.File_Info;
with Glib.File_Input_Stream; use Glib.File_Input_Stream;
with Glib.GFile;             use Glib.GFile;
with Glib.Input_Stream;
with Glib.Object;
with Glib.Test;              use Glib.Test;
with Glib.Types;

procedure Main is

   subtype Gfile is Glib.GFile.Gfile;

   use type Glib.Error.GError;

   Base : constant String := Ada.Directories.Compose
     (Ada.Directories.Current_Directory, "scratch");
   Data_Path : constant String := Ada.Directories.Compose (Base, "data.txt");

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

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
