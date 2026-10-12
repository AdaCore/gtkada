--  Glib.File_IO_Stream.Query_Info, and (via inheritance from
--  Glib.IO_Stream, which has no other concrete implementation to test
--  through) Close/Is_Closed. Gfile_Iostream_Record is new Giostream_Record
--  with null record, so Close dispatches straight to the base type.
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
with Glib.Object;
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

   Glib.Test.Add_Func
     ("/glib-file-io-stream/query-info-success",
      Test_Query_Info_Success'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-file-io-stream/query-info-after-close",
      Test_Query_Info_After_Close'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
