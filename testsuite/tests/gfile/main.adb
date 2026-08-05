--  Glib.GFile tests: the minimal GFile binding, exercised against a
--  scratch directory that the test creates in its own working directory.
--
--  A Gfile is an interface value, so every case releases it with
--  Glib.Object.Unref (Glib.Types.To_Object (...)) -- the idiom documented in
--  Glib.GFile -- rather than with a direct Unref.

with Ada.Command_Line;
with Ada.Directories;
with Ada.Unchecked_Conversion;
with System;

with Glib;                    use Glib;
with Glib.Bytes;
with Glib.File_Info;          use Glib.File_Info;
with Glib.File_Input_Stream;  use Glib.File_Input_Stream;
with Glib.File_Output_Stream; use Glib.File_Output_Stream;
with Glib.GFile;              use Glib.GFile;
with Glib.Input_Stream;
with Glib.Object;
with Glib.Output_Stream;      use Glib.Output_Stream;
with Glib.Test;               use Glib.Test;
with Glib.Types;

procedure Main is

   subtype Gfile is Glib.GFile.Gfile;
   --  "GFile" and "Gfile" are the same Ada identifier, so with both "use Glib"
   --  and "use Glib.GFile" in scope the type has to be named through the
   --  package. A local subtype spares the rest of the file the prefix.

   use type Glib.Bytes.Gbytes;

   Payload : constant UTF8_String := "gfile payload" & ASCII.LF;

   Base : constant String := Ada.Directories.Compose
     (Ada.Directories.Current_Directory, "scratch");
   --  A directory of our own, so nothing depends on the source tree layout.

   Data_Path : constant String := Ada.Directories.Compose (Base, "data.txt");

   procedure Release (File : Gfile);
   --  Drop the reference that a Gfile constructor transferred to us.

   function To_Buffer (Text : String) return Guint8_Array;
   function To_String (Buffer : Guint8_Array) return String;

   procedure Write_Payload (File : Gfile);
   --  (Re)create File with Payload as its whole contents.

   -------------
   -- Release --
   -------------

   procedure Release (File : Gfile) is
   begin
      if File /= Null_Gfile then
         Glib.Object.Unref
           (Glib.Types.To_Object (Glib.Types.GType_Interface (File)));
      end if;
   end Release;

   ---------------
   -- To_Buffer --
   ---------------

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

   -------------------
   -- Write_Payload --
   -------------------

   procedure Write_Payload (File : Gfile) is
      Stream : constant Gfile_Output_Stream := Replace
        (Self        => File,
         Etag        => "",
         Make_Backup => False,
         Flags       => G_File_Create_None,
         Cancellable => null);
   begin
      Assert_True (Stream /= null);
      Assert_True
        (Glib.Output_Stream.Write_All
           (Self          => Goutput_Stream (Stream),
            Buffer        => To_Buffer (Payload),
            Cancellable   => null));
      Assert_True
        (Glib.Output_Stream.Close (Goutput_Stream (Stream), null));

      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Write_Payload;

   procedure Test_Paths
   with Convention => C;

   procedure Test_Write_Read
   with Convention => C;

   procedure Test_Load_Bytes
   with Convention => C;

   procedure Test_Query_Info
   with Convention => C;

   procedure Test_Mutate
   with Convention => C;

   ----------------
   -- Test_Paths --
   ----------------

   procedure Test_Paths is
      Dir    : constant Gfile := New_For_Path (Base);
      File   : constant Gfile := Get_Child (Dir, "data.txt");
      Same   : constant Gfile := New_For_Path (Data_Path);
      Parent : constant Gfile := Get_Parent (File);
      Copy   : constant Gfile := Dup (File);
      Solved : constant Gfile := Resolve_Relative_Path (Dir, "data.txt");
      From_Uri   : constant Gfile := New_For_Uri (Get_Uri (File));
      From_Parse : constant Gfile := Parse_Name (Get_Parse_Name (File));
   begin
      --  Names

      Assert_True (File /= Null_Gfile);
      Assert_Cmpstr_Eq (Get_Basename (File), "data.txt");
      Assert_Cmpstr_Eq (Get_Path (File), Data_Path);
      Assert_Cmpstr_Eq (Peek_Path (File), Data_Path);
      Assert_Cmpstr_Eq (Get_Uri_Scheme (File), "file");
      Assert_True (Has_Uri_Scheme (File, "file"));
      Assert_True (Is_Native (File));

      --  Two Gfile values for the same path are Equal, and Hash agrees

      Assert_True (Equal (File, Same));
      Assert_Cmpuint_Eq (Hash (File), Hash (Same));
      Assert_True (Equal (File, Copy));

      --  A URI and a parse name both round-trip back to the same file

      Assert_True (Equal (File, From_Uri));
      Assert_True (Equal (File, From_Parse));

      --  Hierarchy

      Assert_True (Equal (Parent, Dir));
      Assert_True (Has_Parent (File, Dir));
      Assert_True (Has_Prefix (File, Dir));
      Assert_Cmpstr_Eq (Get_Relative_Path (Dir, File), "data.txt");
      Assert_True (Equal (Solved, File));

      Assert_True (Supports_Thread_Contexts (File));

      Release (From_Parse);
      Release (From_Uri);
      Release (Solved);
      Release (Copy);
      Release (Parent);
      Release (Same);
      Release (File);
      Release (Dir);
   end Test_Paths;

   ---------------------
   -- Test_Write_Read --
   ---------------------

   procedure Test_Write_Read is
      File   : constant Gfile := New_For_Path (Data_Path);
      Stream : Gfile_Input_Stream;
      Buffer : Guint8_Array (1 .. 64) := (others => 0);
      Count  : Gssize;
   begin
      --  Glib.GFile.Replace hands out a Gfile_Output_Stream, and
      --  Glib.GFile.Read a Gfile_Input_Stream: this is where GFile meets the
      --  stream packages.

      Write_Payload (File);

      Stream := Read (Self => File, Cancellable => null);
      Assert_True (Stream /= null);

      Count := Glib.Input_Stream.Read
        (Self        => Glib.Input_Stream.Ginput_Stream (Stream),
         Buffer      => Buffer,
         Cancellable => null);

      Assert_Cmpint_Eq (Gint (Count), Gint (Payload'Length));
      Assert_Cmpstr_Eq (To_String (Buffer (1 .. Natural (Count))), Payload);

      Assert_True
        (Glib.Input_Stream.Close
           (Glib.Input_Stream.Ginput_Stream (Stream), null));
      Glib.Object.Unref (Glib.Object.GObject (Stream));

      Release (File);
   end Test_Write_Read;

   ---------------------
   -- Test_Load_Bytes --
   ---------------------

   procedure Test_Load_Bytes is
      subtype Contents is Guchar_Array (1 .. Payload'Length);
      type Contents_Access is access all Contents;
      function To_Contents is new Ada.Unchecked_Conversion
        (System.Address, Contents_Access);

      File  : constant Gfile := New_For_Path (Data_Path);
      Bytes : Glib.Bytes.Gbytes;
      Size  : Gsize := 0;
      Data  : System.Address;
   begin
      Write_Payload (File);

      --  Etag_Out is declined: the C side then computes no entity tag.

      Bytes := Load_Bytes (Self => File, Cancellable => null);
      Assert_True (Bytes /= Glib.Bytes.Null_Gbytes);
      Assert_Cmpint_Eq
        (Gint (Glib.Bytes.Get_Size (Bytes)), Gint (Payload'Length));

      --  Unref_To_Data consumes the last reference and yields the buffer.

      Data := Glib.Bytes.Unref_To_Data (Bytes, Size);
      Assert_Cmpint_Eq (Gint (Size), Gint (Payload'Length));
      for I in Contents'Range loop
         Assert_True
           (Character'Val (Integer (To_Contents (Data) (I)))
            = Payload (Payload'First + I - 1));
      end loop;
      Glib.g_free (Data);

      Release (File);
   end Test_Load_Bytes;

   ---------------------
   -- Test_Query_Info --
   ---------------------

   procedure Test_Query_Info is
      Dir     : constant Gfile := New_For_Path (Base);
      File    : constant Gfile := New_For_Path (Data_Path);
      Missing : constant Gfile :=
        New_For_Path (Ada.Directories.Compose (Base, "no-such-file"));
      Info    : Gfile_Info;
   begin
      Write_Payload (File);

      Assert_True (Query_Exists (File, null));
      Assert_False (Query_Exists (Missing, null));

      Info := Query_Info
        (Self        => File,
         Attributes  => "standard::*",
         Flags       => G_File_Query_Info_None,
         Cancellable => null);
      Assert_True (Info /= null);
      Assert_Cmpstr_Eq (Get_Name (Info), "data.txt");
      Assert_Cmpint_Eq
        (Gint (Glib.File_Info.Get_Size (Info)), Gint (Payload'Length));

      --  GFile_Type is declared in Glib.File_Info, and Query_File_Type is the
      --  shorthand for the same query.

      Assert_True (Get_File_Type (Info) = G_File_Type_Regular);
      Glib.Object.Unref (Glib.Object.GObject (Info));

      Assert_True
        (Query_File_Type (File, G_File_Query_Info_None, null)
         = G_File_Type_Regular);
      Assert_True
        (Query_File_Type (Dir, G_File_Query_Info_None, null)
         = G_File_Type_Directory);
      Assert_True
        (Query_File_Type (Missing, G_File_Query_Info_None, null)
         = G_File_Type_Unknown);

      Release (Missing);
      Release (File);
      Release (Dir);
   end Test_Query_Info;

   -----------------
   -- Test_Mutate --
   -----------------

   procedure Test_Mutate is
      Dir    : constant Gfile :=
        New_For_Path (Ada.Directories.Compose (Base, "sub"));
      File   : constant Gfile := Get_Child (Dir, "created.txt");
      Stream : Gfile_Output_Stream;
   begin
      Assert_False (Query_Exists (Dir, null));
      Assert_True (Make_Directory (Dir, null));
      Assert_True (Query_Exists (Dir, null));

      Assert_False (Query_Exists (File, null));
      Stream := Create
        (Self        => File,
         Flags       => G_File_Create_None,
         Cancellable => null);
      Assert_True (Stream /= null);
      Assert_True
        (Glib.Output_Stream.Close (Goutput_Stream (Stream), null));
      Glib.Object.Unref (Glib.Object.GObject (Stream));
      Assert_True (Query_Exists (File, null));

      Assert_True (Delete (File, null));
      Assert_False (Query_Exists (File, null));
      Assert_True (Delete (Dir, null));
      Assert_False (Query_Exists (Dir, null));

      Release (File);
      Release (Dir);
   end Test_Mutate;

begin
   Glib.Test.Init;

   if not Ada.Directories.Exists (Base) then
      Ada.Directories.Create_Directory (Base);
   end if;

   Glib.Test.Add_Func ("/gfile/paths", Test_Paths'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gfile/write-read", Test_Write_Read'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gfile/load-bytes", Test_Load_Bytes'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gfile/query-info", Test_Query_Info'Unrestricted_Access);
   Glib.Test.Add_Func ("/gfile/mutate", Test_Mutate'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
