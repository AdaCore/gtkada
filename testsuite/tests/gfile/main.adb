--  Glib.GFile tests: the minimal GFile binding, exercised against a
--  scratch directory that the test creates in its own working directory.
--
--  A Gfile is an interface value, so every case releases it with
--  Glib.Object.Unref (Glib.Types.To_Object (...)) -- the idiom documented in
--  Glib.GFile -- rather than with a direct Unref.

with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with GNAT.Strings;
with System;

with Glib;                    use Glib;
with Glib.Bytes;
with Glib.Error;
with Glib.File_IO_Stream;
with Glib.Date_Time;
with Glib.File_Info;          use Glib.File_Info;
with Glib.File_Input_Stream;  use Glib.File_Input_Stream;
with Glib.File_Monitors;
with Glib.File_Output_Stream; use Glib.File_Output_Stream;
with Glib.GFile;              use Glib.GFile;
with Glib.G_Icon;
with Glib.GEnums;
with Glib.GMounts;
with Glib.Input_Stream;
with Glib.IO_Stream;
with Glib.Main;
with Glib.Object;
with Glib.Properties;
with Glib.Output_Stream;      use Glib.Output_Stream;
with Glib.Test;               use Glib.Test;
with Glib.Types;
with Glib.Time_Zone;
with Gtkada.Bindings;
with Gtkada.Types;

procedure Main is

   subtype Gfile is Glib.GFile.Gfile;
   --  "GFile" and "Gfile" are the same Ada identifier, so with both "use Glib"
   --  and "use Glib.GFile" in scope the type has to be named through the
   --  package. A local subtype spares the rest of the file the prefix.

   use type Glib.Bytes.Gbytes;
   use type Glib.Error.GError;
   use type Glib.File_Monitors.Gfile_Monitor;
   use type Glib.GMounts.Gmount;
   use type Glib.File_Info.Gfile_Info;
   use type Glib.File_IO_Stream.Gfile_Iostream;
   use type Glib.Input_Stream.Ginput_Stream;
   use type Glib.G_Icon.G_Icon;
   use type Glib.IO_Stream.GIOStream_Splice_Flags;
   use type Glib.Object.GObject;
   use type Glib.Output_Stream.Goutput_Stream;
   use type Glib.Gint32;
   use type Glib.Gint64;
   use type Glib.Guint32;
   use type Glib.Guint64;
   use type GNAT.Strings.String_Access;
   use type Glib.GFile.Gfile;
   use type Glib.GEnums.GFile_Monitor_Event;
   use type System.Address;

   --  GtkAda does not expose a GIcon constructor, so use GLib's themed-icon
   --  constructor only to make a valid object for the File_Info accessors.
   function New_Themed_Icon
     (Name : Gtkada.Types.Chars_Ptr) return Glib.G_Icon.G_Icon;
   pragma Import (C, New_Themed_Icon, "g_themed_icon_new");

   procedure Unref_Icon (Icon : Glib.G_Icon.G_Icon);
   pragma Import (C, Unref_Icon, "g_object_unref");

   type Async_Operation is
     (Async_Append,
      Async_Create,
      Async_Create_Readwrite,
      Async_Delete,
      Async_Load_Bytes,
      Async_Make_Directory,
      Async_Make_Symbolic_Link,
      Async_New_Tmp,
      Async_New_Tmp_Dir,
      Async_Open_Readwrite,
      Async_Query_Filesystem_Info,
      Async_Query_Info,
      Async_Read,
      Async_Replace,
      Async_Replace_Contents,
      Async_Replace_Contents_Bytes,
      Async_Replace_Readwrite,
      Async_Set_Attributes,
      Async_Set_Display_Name,
      Async_Find_Mount,
      Async_Eject,
      Async_Mount_Volume,
      Async_Mount_Mountable,
      Async_Poll_Mountable,
      Async_Start_Mountable,
      Async_Stop_Mountable,
      Async_Unmount_Mountable);

   Current_Async_Operation : Async_Operation := Async_Create;
   Async_File              : Gfile := Null_Gfile;
   Async_Done              : Boolean := False;
   Async_Tmp_File          : Gfile := Null_Gfile;
   Async_Tmp_Dir           : Gfile := Null_Gfile;
   Async_Info              : Gfile_Info;
   Async_Attribute_Info    : Gfile_Info;

   --  State for the GIOStream callbacks. The source object remains owned by
   --  the stream variable until the matching finish call has returned.
   type IO_Stream_Async_Operation is (IO_Async_None, IO_Async_Close,
                                      IO_Async_Splice);
   Current_IO_Stream_Async : IO_Stream_Async_Operation := IO_Async_None;
   IO_Stream_Async_Target  : Glib.IO_Stream.Giostream := null;
   IO_Stream_Async_Done    : Boolean := False;
   IO_Stream_Async_Success : Boolean := False;
   IO_Stream_Async_Error   : Glib.Error.GError;

   procedure On_IO_Stream_Async_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);

   procedure Wait_For_IO_Stream_Async;

   procedure On_Async_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result)
   with Convention => C;

   procedure Wait_For_Async;

   Payload : constant UTF8_String := "gfile payload" & ASCII.LF;

   Base : constant String := Ada.Directories.Compose
     (Ada.Directories.Current_Directory, "scratch");
   --  A directory of our own, so nothing depends on the source tree layout.

   Data_Path : constant String := Ada.Directories.Compose (Base, "data.txt");

   IO_Stream_State_Path : constant String :=
     Ada.Directories.Compose (Base, "io-stream-state.txt");
   IO_Stream_First_Path : constant String :=
     Ada.Directories.Compose (Base, "io-stream-first.txt");
   IO_Stream_Second_Path : constant String :=
     Ada.Directories.Compose (Base, "io-stream-second.txt");

   Expected_Monitor : Glib.File_Monitors.Gfile_Monitor;
   Expected_Monitor_File : Gfile;
   Monitor_Typed_Calls : Natural := 0;
   Monitor_Slot_Calls : Natural := 0;
   Monitor_Typed_Args_Ok : Boolean := False;
   Monitor_Slot_Args_Ok : Boolean := False;
   Raise_Monitor_Callbacks : Boolean := False;
   Monitor_Exception_Calls : Natural := 0;
   Unexpected_Monitor_Exception_Calls : Natural := 0;

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

   --  This initialized object follows the function body so elaboration does
   --  not call To_Buffer before its body is available.
   Async_Data : constant Guint8_Array := To_Buffer (Payload);

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
      Error  : Glib.Error.GError;
      Stream : constant Gfile_Output_Stream := Replace
        (Self        => File,
         Etag        => "",
         Make_Backup => False,
         Flags       => G_File_Create_None,
         Cancellable => null,
         Error       => Error);
   begin
      Assert_True (Error = null);
      Assert_True (Stream /= null);
      Assert_True
        (Glib.Output_Stream.Write_All
           (Self          => Goutput_Stream (Stream),
            Buffer        => To_Buffer (Payload),
            Cancellable   => null,
            Error         => Error));
      Assert_True (Error = null);
      Assert_True
        (Glib.Output_Stream.Close (Goutput_Stream (Stream), null, Error));
      Assert_True (Error = null);

      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Write_Payload;

   --------------------
   -- On_Async_Ready --
   --------------------

   procedure On_Async_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result)
   is
      Error       : Glib.Error.GError;
      Out_Stream  : Gfile_Output_Stream;
      IO_Stream   : Glib.File_IO_Stream.Gfile_Iostream;
      In_Stream   : Gfile_Input_Stream;
      Info        : Gfile_Info;
      Result_File : Gfile;
      Bytes       : Glib.Bytes.Gbytes;
      Success     : Boolean;
   begin
      --  The operation tag selects the matching finish function. GIO requires
      --  each asynchronous request to be completed through that function.
      case Current_Async_Operation is
         when Async_Append =>
            Out_Stream := Append_To_Finish (Async_File, Res, Error);
            Assert_True (Error = null and then Out_Stream /= null);
            Assert_True
              (Glib.Output_Stream.Close
                 (Goutput_Stream (Out_Stream), null, Error));
            Assert_True (Error = null);
            Glib.Object.Unref (Glib.Object.GObject (Out_Stream));

         when Async_Create =>
            Out_Stream := Create_Finish (Async_File, Res, Error);
            Assert_True (Error = null and then Out_Stream /= null);
            Assert_True
              (Glib.Output_Stream.Close
                 (Goutput_Stream (Out_Stream), null, Error));
            Assert_True (Error = null);
            Glib.Object.Unref (Glib.Object.GObject (Out_Stream));

         when Async_Create_Readwrite =>
            IO_Stream := Create_Readwrite_Finish (Async_File, Res, Error);
            Assert_True (Error = null and then IO_Stream /= null);
            Assert_True
              (Glib.IO_Stream.Close
                 (Glib.IO_Stream.Giostream (IO_Stream), null, Error));
            Assert_True (Error = null);
            Glib.Object.Unref (Glib.Object.GObject (IO_Stream));

         when Async_Delete =>
            Success := Delete_Finish (Async_File, Res, Error);
            Assert_True (Success);
            Assert_True (Error = null);

         when Async_Load_Bytes =>
            Bytes := Load_Bytes_Finish (Async_File, Res, Error => Error);
            Assert_True (Error = null and then Bytes /= Glib.Bytes.Null_Gbytes);
            Assert_Cmpint_Eq
              (Gint (Glib.Bytes.Get_Size (Bytes)), Gint (Payload'Length));
            Glib.Bytes.Unref (Bytes);

         when Async_Make_Directory =>
            Success := Make_Directory_Finish (Async_File, Res, Error);
            Assert_True (Success);
            Assert_True (Error = null);

         when Async_Make_Symbolic_Link =>
            Success := Make_Symbolic_Link_Finish (Async_File, Res, Error);
            Assert_True (Success);
            Assert_True (Error = null);

         when Async_New_Tmp =>
            Async_Tmp_File := New_Tmp_Finish (Res, IO_Stream, Error);
            Assert_True (Error = null and then Async_Tmp_File /= Null_Gfile);
            Assert_True (IO_Stream /= null);
            Assert_True
              (Glib.IO_Stream.Close
                 (Glib.IO_Stream.Giostream (IO_Stream), null, Error));
            Assert_True (Error = null);
            Glib.Object.Unref (Glib.Object.GObject (IO_Stream));

         when Async_New_Tmp_Dir =>
            Async_Tmp_Dir := New_Tmp_Dir_Finish (Res, Error);
            Assert_True (Error = null and then Async_Tmp_Dir /= Null_Gfile);

         when Async_Open_Readwrite =>
            IO_Stream := Open_Readwrite_Finish (Async_File, Res, Error);
            Assert_True (Error = null and then IO_Stream /= null);
            Assert_True
              (Glib.IO_Stream.Close
                 (Glib.IO_Stream.Giostream (IO_Stream), null, Error));
            Assert_True (Error = null);
            Glib.Object.Unref (Glib.Object.GObject (IO_Stream));

         when Async_Query_Filesystem_Info =>
            Info := Query_Filesystem_Info_Finish (Async_File, Res, Error);
            Assert_True (Error = null and then Info /= null);
            Glib.Object.Unref (Glib.Object.GObject (Info));

         when Async_Query_Info =>
            Info := Query_Info_Finish (Async_File, Res, Error);
            Assert_True (Error = null and then Info /= null);
            Assert_Cmpstr_Eq (Get_Name (Info), "async.txt");
            Glib.Object.Unref (Glib.Object.GObject (Info));

         when Async_Read =>
            In_Stream := Read_Finish (Async_File, Res, Error);
            Assert_True (Error = null and then In_Stream /= null);
            Assert_True
              (Glib.Input_Stream.Close
                 (Glib.Input_Stream.Ginput_Stream (In_Stream), null, Error));
            Assert_True (Error = null);
            Glib.Object.Unref (Glib.Object.GObject (In_Stream));

         when Async_Replace =>
            Out_Stream := Replace_Finish (Async_File, Res, Error);
            Assert_True (Error = null and then Out_Stream /= null);
            Assert_True
              (Glib.Output_Stream.Close
                 (Goutput_Stream (Out_Stream), null, Error));
            Assert_True (Error = null);
            Glib.Object.Unref (Glib.Object.GObject (Out_Stream));

         when Async_Replace_Contents | Async_Replace_Contents_Bytes =>
            Success := Replace_Contents_Finish
              (Async_File, Res, Error => Error);
            Assert_True (Success);
            Assert_True (Error = null);

         when Async_Replace_Readwrite =>
            IO_Stream := Replace_Readwrite_Finish (Async_File, Res, Error);
            Assert_True (Error = null and then IO_Stream /= null);
            Assert_True
              (Glib.IO_Stream.Close
                 (Glib.IO_Stream.Giostream (IO_Stream), null, Error));
            Assert_True (Error = null);
            Glib.Object.Unref (Glib.Object.GObject (IO_Stream));

         when Async_Set_Attributes =>
            Success := Set_Attributes_Finish (Async_File, Res, Info, Error);
            Assert_True (Success or else Error /= null);
            if Info /= null then
               Glib.Object.Unref (Glib.Object.GObject (Info));
            end if;

         when Async_Set_Display_Name =>
            Result_File := Set_Display_Name_Finish (Async_File, Res, Error);
            Assert_True (Error = null and then Result_File /= Null_Gfile);
            Assert_Cmpstr_Eq (Get_Basename (Result_File), "async-renamed.txt");
            Release (Result_File);

         when Async_Find_Mount =>
            declare
               Mount : constant Glib.GMounts.Gmount :=
                 Find_Enclosing_Mount_Finish (Async_File, Res, Error);
            begin
               --  GMount is an interface proxy, so compare with its null
               --  interface value and unref through the represented object.
               Assert_True
                 (Mount /= Glib.GMounts.Null_Gmount or else Error /= null);
               if Mount /= Glib.GMounts.Null_Gmount then
                  Glib.Object.Unref
                    (Glib.Types.To_Object (Glib.Types.GType_Interface (Mount)));
               end if;
            end;

         when Async_Eject =>
            Success := Eject_Mountable_With_Operation_Finish
              (Async_File, Res, Error);
            Assert_True (Success or else Error /= null);

         when Async_Mount_Volume =>
            Success := Mount_Enclosing_Volume_Finish
              (Async_File, Res, Error);
            Assert_True (Success or else Error /= null);

         when Async_Mount_Mountable =>
            Result_File := Mount_Mountable_Finish (Async_File, Res, Error);
            Assert_True (Result_File /= Null_Gfile or else Error /= null);
            Release (Result_File);

         when Async_Poll_Mountable =>
            Success := Poll_Mountable_Finish (Async_File, Res, Error);
            Assert_True (Success or else Error /= null);

         when Async_Start_Mountable =>
            Success := Start_Mountable_Finish (Async_File, Res, Error);
            Assert_True (Success or else Error /= null);

         when Async_Stop_Mountable =>
            Success := Stop_Mountable_Finish (Async_File, Res, Error);
            Assert_True (Success or else Error /= null);

         when Async_Unmount_Mountable =>
            Success := Unmount_Mountable_With_Operation_Finish
              (Async_File, Res, Error);
            Assert_True (Success or else Error /= null);
      end case;

      --  Errors returned by a finish function are owned by this callback.
      if Error /= null then
         Glib.Error.Error_Free (Error);
      end if;
      Async_Done := True;
   end On_Async_Ready;

   ---------------------
   -- Wait_For_Async --
   ---------------------

   procedure Wait_For_Async is
      Dispatched : Boolean;
   begin
      --  Pump the default main context until GIO dispatches the ready callback.
      for I in 1 .. 5_000 loop
         exit when Async_Done;
         Dispatched := Glib.Main.Main_Context_Iteration (May_Block => False);
         delay 0.001;
      end loop;
      Assert_True (Async_Done);
   end Wait_For_Async;

   -----------------------------
   -- On_IO_Stream_Async_Ready --
   -----------------------------

   --  GIOStream has its own callback type; its generated bridge converts the
   --  source GObject back to a GtkAda proxy before invoking this procedure.
   procedure On_IO_Stream_Async_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result)
   is
      pragma Unreferenced (Source_Object);
   begin
      case Current_IO_Stream_Async is
         when IO_Async_Close =>
            IO_Stream_Async_Success := Glib.IO_Stream.Close_Finish
              (IO_Stream_Async_Target, Res, IO_Stream_Async_Error);
         when IO_Async_Splice =>
            IO_Stream_Async_Success := Glib.IO_Stream.Splice_Finish
              (Res, IO_Stream_Async_Error);
         when IO_Async_None =>
            Assert_True (False);
      end case;
      IO_Stream_Async_Done := True;
   end On_IO_Stream_Async_Ready;

   -------------------------------
   -- Wait_For_IO_Stream_Async --
   -------------------------------

   procedure Wait_For_IO_Stream_Async is
      Dispatched : Boolean;
   begin
      --  GIO dispatches completion through the thread's default main context.
      for I in 1 .. 5_000 loop
         exit when IO_Stream_Async_Done;
         Dispatched := Glib.Main.Main_Context_Iteration (May_Block => False);
         delay 0.001;
      end loop;
      Assert_True (IO_Stream_Async_Done);
      Assert_True (IO_Stream_Async_Error = null);
   end Wait_For_IO_Stream_Async;

   procedure Test_Paths
   with Convention => C;

   procedure Test_Write_Read
   with Convention => C;

   procedure Test_IO_Stream
   with Convention => C;

   procedure Test_Load_Bytes_Contents
   with Convention => C;

   --  procedure Test_Load_Bytes
   --  with Convention => C; --  TODO FIXME

   procedure Test_Query_Info
   with Convention => C;

   procedure Test_Mutate
   with Convention => C;

   procedure Test_List
   with Convention => C;

   procedure Test_SList
   with Convention => C;

   procedure Test_File_Monitor
   with Convention => C;

   --  Keep the callback exception checks at the C signal boundary, matching
   --  the generated marshaller's promise to contain Ada exceptions.
   procedure On_Monitor_Changed
     (Self       : access Glib.File_Monitors.Gfile_Monitor_Record'Class;
      File       : Gfile;
      Other_File : Gfile;
      Event_Type : Glib.GEnums.GFile_Monitor_Event);

   --  The second callback overload receives the connected slot object rather
   --  than the emitting monitor, so test that conversion separately.
   procedure On_Monitor_Changed_Slot
     (Self       : access Glib.Object.GObject_Record'Class;
      File       : Gfile;
      Other_File : Gfile;
      Event_Type : Glib.GEnums.GFile_Monitor_Event);

   procedure Record_Monitor_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Test_File_Info_Attributes
   with Convention => C;
   procedure Test_File_Info_Properties
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
      Error  : Glib.Error.GError;
   begin
      --  Glib.GFile.Replace hands out a Gfile_Output_Stream, and
      --  Glib.GFile.Read a Gfile_Input_Stream: this is where GFile meets the
      --  stream packages.

      Write_Payload (File);

      Stream := Read (Self => File, Cancellable => null, Error => Error);
      Assert_True (Error = null);
      Assert_True (Stream /= null);

      Count := Glib.Input_Stream.Read
        (Self        => Glib.Input_Stream.Ginput_Stream (Stream),
         Buffer      => Buffer,
         Cancellable => null,
         Error       => Error);

      Assert_True (Error = null);
      Assert_Cmpint_Eq (Gint (Count), Gint (Payload'Length));
      Assert_Cmpstr_Eq (To_String (Buffer (1 .. Natural (Count))), Payload);

      Assert_True
        (Glib.Input_Stream.Close
           (Glib.Input_Stream.Ginput_Stream (Stream), null, Error));
      Assert_True (Error = null);
      Glib.Object.Unref (Glib.Object.GObject (Stream));

      Release (File);
   end Test_Write_Read;

   --------------------
   -- Test_IO_Stream --
   --------------------

   --  GFileIOStream is a concrete GIOStream, so it exercises the generic
   --  input/output accessors and state methods without a test-only subclass.
   procedure Test_IO_Stream is
      State_File  : constant Gfile := New_For_Path (IO_Stream_State_Path);
      First_File  : constant Gfile := New_For_Path (IO_Stream_First_Path);
      Second_File : constant Gfile := New_For_Path (IO_Stream_Second_Path);
      State_Stream  : Glib.File_IO_Stream.Gfile_Iostream;
      First_Stream  : Glib.File_IO_Stream.Gfile_Iostream;
      Second_Stream : Glib.File_IO_Stream.Gfile_Iostream;
      Input          : Glib.Input_Stream.Ginput_Stream;
      Output         : Glib.Output_Stream.Goutput_Stream;
      Error          : Glib.Error.GError;
      Dispatched     : Boolean;
   begin
      --  A previous interrupted run may have left these private fixtures.
      if Ada.Directories.Exists (IO_Stream_State_Path) then
         Ada.Directories.Delete_File (IO_Stream_State_Path);
      end if;
      if Ada.Directories.Exists (IO_Stream_First_Path) then
         Ada.Directories.Delete_File (IO_Stream_First_Path);
      end if;
      if Ada.Directories.Exists (IO_Stream_Second_Path) then
         Ada.Directories.Delete_File (IO_Stream_Second_Path);
      end if;

      --  The stream owns both substreams; getters return usable borrowed
      --  proxies, while the parent remains responsible for their lifetime.
      Write_Payload (State_File);
      State_Stream := Open_Readwrite (State_File, null, Error);
      Assert_True (Error = null and then State_Stream /= null);
      Input := Glib.IO_Stream.Get_Input_Stream
        (Glib.IO_Stream.Giostream (State_Stream));
      Output := Glib.IO_Stream.Get_Output_Stream
        (Glib.IO_Stream.Giostream (State_Stream));
      Assert_True (Input /= null);
      Assert_True (Output /= null);

      --  Pending state can be claimed once, cleared, and claimed again.
      Assert_False
        (Glib.IO_Stream.Has_Pending (Glib.IO_Stream.Giostream (State_Stream)));
      Assert_False
        (Glib.IO_Stream.Is_Closed (Glib.IO_Stream.Giostream (State_Stream)));
      Assert_True
        (Glib.IO_Stream.Set_Pending
           (Glib.IO_Stream.Giostream (State_Stream), Error));
      Assert_True (Error = null);
      Assert_True
        (Glib.IO_Stream.Has_Pending (Glib.IO_Stream.Giostream (State_Stream)));

      --  A competing operation gets an error until Clear_Pending releases it.
      Assert_False
        (Glib.IO_Stream.Set_Pending
           (Glib.IO_Stream.Giostream (State_Stream), Error));
      Assert_True (Error /= null);
      Glib.Error.Error_Free (Error);
      Error := null;
      Glib.IO_Stream.Clear_Pending
        (Glib.IO_Stream.Giostream (State_Stream));
      Assert_False
        (Glib.IO_Stream.Has_Pending (Glib.IO_Stream.Giostream (State_Stream)));

      --  Synchronous close is successful and remains harmless when repeated.
      Assert_True
        (Glib.IO_Stream.Close
           (Glib.IO_Stream.Giostream (State_Stream), null, Error));
      Assert_True (Error = null);
      Assert_True
        (Glib.IO_Stream.Is_Closed (Glib.IO_Stream.Giostream (State_Stream)));
      Assert_True
        (Glib.IO_Stream.Close
           (Glib.IO_Stream.Giostream (State_Stream), null, Error));
      Assert_True (Error = null);

      --  Closed streams reject a new pending operation with a GError.
      Assert_False
        (Glib.IO_Stream.Set_Pending
           (Glib.IO_Stream.Giostream (State_Stream), Error));
      Assert_True (Error /= null);
      Glib.Error.Error_Free (Error);
      Error := null;
      Glib.Object.Unref (Glib.Object.GObject (State_Stream));
      Release (State_File);
      Ada.Directories.Delete_File (IO_Stream_State_Path);

      --  Two populated files make a deterministic pair of readable and
      --  writable streams for the bidirectional asynchronous splice API.
      Write_Payload (First_File);
      Write_Payload (Second_File);
      First_Stream := Open_Readwrite (First_File, null, Error);
      Assert_True (Error = null and then First_Stream /= null);
      Second_Stream := Open_Readwrite (Second_File, null, Error);
      Assert_True (Error = null and then Second_Stream /= null);

      --  The callback path must pass its GAsyncResult to Splice_Finish.
      Current_IO_Stream_Async := IO_Async_Splice;
      IO_Stream_Async_Target := Glib.IO_Stream.Giostream (First_Stream);
      IO_Stream_Async_Done := False;
      IO_Stream_Async_Error := null;
      Glib.IO_Stream.Splice_Async
        (Glib.IO_Stream.Giostream (First_Stream),
         Glib.IO_Stream.Giostream (Second_Stream),
         Glib.IO_Stream.G_Io_Stream_Splice_None, 0, null,
         On_IO_Stream_Async_Ready'Unrestricted_Access);
      Wait_For_IO_Stream_Async;
      Assert_True (IO_Stream_Async_Success);
      Assert_False
        (Glib.IO_Stream.Is_Closed (Glib.IO_Stream.Giostream (First_Stream)));
      Assert_False
        (Glib.IO_Stream.Is_Closed (Glib.IO_Stream.Giostream (Second_Stream)));

      --  The null-callback branch is complete when both objects close; these
      --  flags also check that the splice request honours its close options.
      Glib.IO_Stream.Splice_Async
        (Glib.IO_Stream.Giostream (First_Stream),
         Glib.IO_Stream.Giostream (Second_Stream),
         Glib.IO_Stream.G_Io_Stream_Splice_Close_Stream1 or
           Glib.IO_Stream.G_Io_Stream_Splice_Close_Stream2,
         0, null, null);
      for I in 1 .. 5_000 loop
         exit when
           Glib.IO_Stream.Is_Closed
             (Glib.IO_Stream.Giostream (First_Stream))
           and then Glib.IO_Stream.Is_Closed
             (Glib.IO_Stream.Giostream (Second_Stream));
         Dispatched := Glib.Main.Main_Context_Iteration (May_Block => False);
         delay 0.001;
      end loop;
      Assert_True
        (Glib.IO_Stream.Is_Closed (Glib.IO_Stream.Giostream (First_Stream)));
      Assert_True
        (Glib.IO_Stream.Is_Closed (Glib.IO_Stream.Giostream (Second_Stream)));
      Glib.Object.Unref (Glib.Object.GObject (First_Stream));
      Glib.Object.Unref (Glib.Object.GObject (Second_Stream));

      --  A fresh pair lets the close callback and its nullable form be checked
      --  independently after the splice has closed the first pair.
      First_Stream := Open_Readwrite (First_File, null, Error);
      Assert_True (Error = null and then First_Stream /= null);
      Second_Stream := Open_Readwrite (Second_File, null, Error);
      Assert_True (Error = null and then Second_Stream /= null);

      Current_IO_Stream_Async := IO_Async_Close;
      IO_Stream_Async_Target := Glib.IO_Stream.Giostream (First_Stream);
      IO_Stream_Async_Done := False;
      IO_Stream_Async_Error := null;
      Glib.IO_Stream.Close_Async
        (Glib.IO_Stream.Giostream (First_Stream), 0, null,
         On_IO_Stream_Async_Ready'Unrestricted_Access);
      Wait_For_IO_Stream_Async;
      Assert_True (IO_Stream_Async_Success);
      Assert_True
        (Glib.IO_Stream.Is_Closed (Glib.IO_Stream.Giostream (First_Stream)));

      --  A null callback is valid too; wait for the stream's closed state.
      Glib.IO_Stream.Close_Async
        (Glib.IO_Stream.Giostream (Second_Stream), 0, null, null);
      for I in 1 .. 5_000 loop
         exit when Glib.IO_Stream.Is_Closed
           (Glib.IO_Stream.Giostream (Second_Stream));
         Dispatched := Glib.Main.Main_Context_Iteration (May_Block => False);
         delay 0.001;
      end loop;
      Assert_True
        (Glib.IO_Stream.Is_Closed (Glib.IO_Stream.Giostream (Second_Stream)));

      Glib.Object.Unref (Glib.Object.GObject (First_Stream));
      Glib.Object.Unref (Glib.Object.GObject (Second_Stream));
      Release (First_File);
      Release (Second_File);
      Ada.Directories.Delete_File (IO_Stream_First_Path);
      Ada.Directories.Delete_File (IO_Stream_Second_Path);
      Current_IO_Stream_Async := IO_Async_None;
   end Test_IO_Stream;

   -----------------------------
   -- Test_Load_Bytes_Contents --
   -----------------------------

   procedure Test_Load_Bytes_Contents is
      File  : constant Gfile := New_For_Path (Data_Path);
      Bytes : Glib.Bytes.Gbytes;
      Error : Glib.Error.GError;
   begin
      --  Reading into GBytes exercises Load_Bytes without relying on the
      --  separate Unref_To_Data ownership conversion.
      Write_Payload (File);
      Bytes := Load_Bytes (File, null, Error => Error);
      Assert_True (Error = null and then Bytes /= Glib.Bytes.Null_Gbytes);
      Assert_Cmpint_Eq
        (Gint (Glib.Bytes.Get_Size (Bytes)), Gint (Payload'Length));
      Glib.Bytes.Unref (Bytes);
      Release (File);
   end Test_Load_Bytes_Contents;

   ---------------------
   -- Test_Load_Bytes --
   ---------------------

   --  TODO FIXME : g_bytes_unref_to_data
   --  Test commented out pending g_bytes_unref_to_data binding update

   --  procedure Test_Load_Bytes is
   --     subtype Contents is Guchar_Array (1 .. Payload'Length);
   --     type Contents_Access is access all Contents;
   --     function To_Contents is new Ada.Unchecked_Conversion
   --       (System.Address, Contents_Access);
   --     File  : constant Gfile := New_For_Path (Data_Path);
   --     Bytes : Glib.Bytes.Gbytes;
   --     Size  : Gsize := 0;
   --     Data  : System.Address;
   --     Error : Glib.Error.GError;
   --  begin
   --     Write_Payload (File);

   --     --  Etag_Out is declined: the C side then computes no entity tag.
   --     Bytes := Load_Bytes (Self => File, Cancellable => null, Error => Error);
   --     Assert_True (Error = null);
   --     Assert_True (Bytes /= Glib.Bytes.Null_Gbytes);
   --     Assert_Cmpint_Eq
   --        (Gint (Glib.Bytes.Get_Size (Bytes)), Gint (Payload'Length));

   --     --  Unref_To_Data consumes the last reference and yields the buffer.
   --     Data := Glib.Bytes.Unref_To_Data (Bytes, Size);
   --     Assert_Cmpint_Eq (Gint (Size), Gint (Payload'Length));
   --     for I in Contents'Range loop
   --        Assert_True
   --          (Character'Val (Integer (To_Contents (Data) (I)))
   --           = Payload (Payload'First + I - 1));
   --     end loop;
   --     Glib.g_free (Data);
   --     Release (File);
   --  end Test_Load_Bytes;

   ---------------------
   -- Test_Query_Info --
   ---------------------

   procedure Test_Query_Info is
      Dir     : constant Gfile := New_For_Path (Base);
      File    : constant Gfile := New_For_Path (Data_Path);
      Missing : constant Gfile :=
        New_For_Path (Ada.Directories.Compose (Base, "no-such-file"));
      Info    : Gfile_Info;
      Error   : Glib.Error.GError;
   begin
      Write_Payload (File);

      Assert_True (Query_Exists (File, null));
      Assert_False (Query_Exists (Missing, null));

      Info := Query_Info
        (Self        => File,
         Attributes  => "standard::*",
         Flags       => G_File_Query_Info_None,
         Cancellable => null,
         Error       => Error);
      Assert_True (Error = null);
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

   -------------------------------
   -- Test_File_Info_Attributes --
   -------------------------------

   procedure Test_File_Info_Attributes is
      Info      : constant Gfile_Info := Gfile_Info_New;
      Other     : constant Gfile_Info := Gfile_Info_New;
      From_G_New : Gfile_Info;
      Destination : constant Gfile_Info := Gfile_Info_New;
      Duplicate : Gfile_Info;

      procedure Free_List (Items : in out GNAT.Strings.String_List);
      function Contains
        (Items : GNAT.Strings.String_List; Value : String) return Boolean;

      ----------------
      -- Free_List --
      ----------------

      procedure Free_List (Items : in out GNAT.Strings.String_List) is
      begin
         --  The binding copies C strings into Ada allocations for these lists.
         for Index in Items'Range loop
            GNAT.Strings.Free (Items (Index));
         end loop;
      end Free_List;

      --------------
      -- Contains --
      --------------

      function Contains
        (Items : GNAT.Strings.String_List; Value : String) return Boolean
      is
      begin
         for Item of Items loop
            if Item /= null and then Item.all = Value then
               return True;
            end if;
         end loop;
         return False;
      end Contains;

      Strings : GNAT.Strings.String_List (1 .. 2) :=
        (1 => new String'("first"), 2 => new String'("second"));
   begin
      --  Both generated constructors create usable GObjects; Initialize is
      --  idempotent when the object already has its underlying C instance.
      Assert_True (Info /= null);
      G_New (From_G_New);
      Assert_True (From_G_New /= null);
      Initialize (From_G_New);
      Set_Name (From_G_New, "made-with-g-new");
      Assert_Cmpstr_Eq (Get_Name (From_G_New), "made-with-g-new");
      Set_Name (Other, "object-holder");

      --  Exercise each typed attribute setter/getter pair with a private
      --  namespace so the test does not depend on filesystem metadata.
      Set_Attribute_Boolean (Info, "test::boolean", True);
      Assert_True (Get_Attribute_Boolean (Info, "test::boolean"));
      Set_Attribute_Byte_String (Info, "test::bytes", "raw-bytes");
      Assert_Cmpstr_Eq
        (Get_Attribute_Byte_String (Info, "test::bytes"), "raw-bytes");
      Set_Attribute_File_Path (Info, "test::path", "/virtual/file");
      Assert_Cmpstr_Eq
        (Get_Attribute_File_Path (Info, "test::path"), "/virtual/file");
      Set_Attribute_Int32 (Info, "test::int32", -32);
      Assert_True (Get_Attribute_Int32 (Info, "test::int32") = -32);
      Set_Attribute_Int64 (Info, "test::int64", -64000);
      Assert_True (Get_Attribute_Int64 (Info, "test::int64") = -64000);
      Set_Attribute_Uint32 (Info, "test::uint32", 32000);
      Assert_True (Get_Attribute_Uint32 (Info, "test::uint32") = 32000);
      Set_Attribute_Uint64 (Info, "test::uint64", 64000);
      Assert_True (Get_Attribute_Uint64 (Info, "test::uint64") = 64000);

      --  The generic object accessor returns a borrowed GObject reference.
      Set_Attribute_Object (Info, "test::object", Other);
      declare
         Object : constant Glib.Object.GObject :=
           Get_Attribute_Object (Info, "test::object");
      begin
         Assert_True (Object /= null);
         Assert_Cmpstr_Eq (Gfile_Info (Object).Get_Name, "object-holder");
      end;

      Set_Attribute_String (Info, "test::text", "hello");
      Assert_Cmpstr_Eq (Get_Attribute_String (Info, "test::text"), "hello");
      Assert_Cmpstr_Eq
        (Get_Attribute_As_String (Info, "test::text"), "hello");

      --  Set_Attribute_Stringv copies its input. The returned Ada list is also
      --  a copy, so both lists are freed here after verifying the round trip.
      Set_Attribute_Stringv (Info, "test::vector", Strings);
      Free_List (Strings);
      declare
         Result : GNAT.Strings.String_List :=
           Get_Attribute_Stringv (Info, "test::vector");
      begin
         Assert_Cmpuint_Eq (Guint (Result'Length), 2);
         Assert_Cmpstr_Eq (Result (1).all, "first");
         Assert_Cmpstr_Eq (Result (2).all, "second");
         Free_List (Result);
      end;

      --  Namespace and list queries report the attributes set above; the
      --  names in List_Attributes are namespace-relative when filtered.
      Assert_True (Has_Attribute (Info, "test::text"));
      Assert_False (Has_Attribute (Info, "test::missing"));
      Assert_True (Has_Namespace (Info, "test"));
      Assert_False (Has_Namespace (Info, "absent"));
      declare
         All_Attributes : GNAT.Strings.String_List := List_Attributes (Info);
         Test_Attributes : GNAT.Strings.String_List :=
           List_Attributes (Info, "test");
      begin
         Assert_True (All_Attributes'Length >= 1);
         Assert_True (Test_Attributes'Length >= 1);
         Free_List (All_Attributes);
         Free_List (Test_Attributes);
      end;

      --  Copy_Into clears the destination before copying; Dup creates an
      --  independent object with the same attributes.
      Set_Attribute_String (Destination, "test::old", "discard me");
      Copy_Into (Info, Destination);
      Assert_True (Has_Attribute (Destination, "test::text"));
      Assert_False (Has_Attribute (Destination, "test::old"));
      Duplicate := Dup (Info);
      Assert_True (Duplicate /= null);
      Assert_Cmpstr_Eq (Get_Attribute_String (Duplicate, "test::text"), "hello");

      --  These status controls have no public getter in this binding, so
      --  verify their calls leave ordinary metadata available.
      Clear_Status (Info);
      Assert_Cmpstr_Eq (Get_Attribute_String (Info, "test::text"), "hello");
      Unset_Attribute_Mask (Info);
      Assert_True (Has_Attribute (Info, "test::text"));
      Remove_Attribute (Info, "test::text");
      Assert_False (Has_Attribute (Info, "test::text"));

      Glib.Object.Unref (Glib.Object.GObject (Duplicate));
      Glib.Object.Unref (Glib.Object.GObject (Destination));
      Glib.Object.Unref (Glib.Object.GObject (Other));
      Glib.Object.Unref (Glib.Object.GObject (From_G_New));
      Glib.Object.Unref (Glib.Object.GObject (Info));
   end Test_File_Info_Attributes;

   --------------------------------
   -- Test_File_Info_Properties --
   --------------------------------

   procedure Test_File_Info_Properties is
      Info : constant Gfile_Info := Gfile_Info_New;
      Zone : Glib.Time_Zone.Gtime_Zone :=
        Glib.Time_Zone.Gtime_Zone_New_Utc;
      Stamp : Glib.Date_Time.Gdate_Time := Glib.Date_Time.Gdate_Time_New
        (Tz      => Zone,
         Year    => 2020,
         Month   => 1,
         Day     => 2,
         Hour    => 3,
         Minute  => 4,
         Seconds => 5.0);
      Icon_Name : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("text-x-generic");
      Icon : constant Glib.G_Icon.G_Icon := New_Themed_Icon (Icon_Name);
      use type Glib.Date_Time.Gdate_Time;
      use type Glib.Time_Zone.Gtime_Zone;
   begin
      --  The themed icon constructor takes a C string; release that temporary
      --  immediately because the returned icon owns its own name.
      Gtkada.Types.Free (Icon_Name);
      Assert_True (Icon /= null);

      --  The direct metadata methods map to standard GFile attributes.
      Set_Content_Type (Info, "text/plain");
      Assert_Cmpstr_Eq (Get_Content_Type (Info), "text/plain");
      Set_Display_Name (Info, "A display name");
      Assert_Cmpstr_Eq (Get_Display_Name (Info), "A display name");
      Set_Edit_Name (Info, "editable-name");
      Assert_Cmpstr_Eq (Get_Edit_Name (Info), "editable-name");
      Set_Attribute_String (Info, "etag::value", "etag-123");
      Assert_Cmpstr_Eq (Get_Etag (Info), "etag-123");
      Set_File_Type (Info, G_File_Type_Regular);
      Assert_True (Get_File_Type (Info) = G_File_Type_Regular);
      Set_Icon (Info, Icon);
      Assert_True (Get_Icon (Info) = Icon);
      Set_Symbolic_Icon (Info, Icon);
      Assert_True (Get_Symbolic_Icon (Info) = Icon);

      --  Backup has no direct setter in this binding, so specify that standard
      --  boolean attribute through the generic typed accessor.
      Set_Attribute_Boolean (Info, "standard::is-backup", True);
      Assert_True (Get_Is_Backup (Info));
      Set_Is_Hidden (Info, True);
      Assert_True (Get_Is_Hidden (Info));
      Set_Is_Symlink (Info, True);
      Assert_True (Get_Is_Symlink (Info));

      Set_Name (Info, "metadata.txt");
      Assert_Cmpstr_Eq (Get_Name (Info), "metadata.txt");
      Set_Size (Info, 1234);
      Assert_True (Get_Size (Info) = 1234);
      Set_Sort_Order (Info, -7);
      Assert_True (Get_Sort_Order (Info) = -7);
      Set_Symlink_Target (Info, "target.txt");
      Assert_Cmpstr_Eq (Get_Symlink_Target (Info), "target.txt");

      --  Each date setter stores the same known instant; the matching getter
      --  returns an owned GDateTime, which is released after its value check.
      Set_Access_Date_Time (Info, Stamp);
      declare
         Value : Glib.Date_Time.Gdate_Time := Get_Access_Date_Time (Info);
      begin
         Assert_True
           (Glib.Date_Time.To_Unix (Value) = Glib.Date_Time.To_Unix (Stamp));
         Glib.Date_Time.Unref (Value);
      end;
      Set_Creation_Date_Time (Info, Stamp);
      declare
         Value : Glib.Date_Time.Gdate_Time := Get_Creation_Date_Time (Info);
      begin
         Assert_True
           (Glib.Date_Time.To_Unix (Value) = Glib.Date_Time.To_Unix (Stamp));
         Glib.Date_Time.Unref (Value);
      end;
      Set_Modification_Date_Time (Info, Stamp);
      declare
         Value : Glib.Date_Time.Gdate_Time :=
           Get_Modification_Date_Time (Info);
      begin
         Assert_True
           (Glib.Date_Time.To_Unix (Value) = Glib.Date_Time.To_Unix (Stamp));
         Glib.Date_Time.Unref (Value);
      end;

      --  Trash metadata stores its deletion date as ISO 8601 text.
      Set_Attribute_String
        (Info, "trash::deletion-date", "2020-01-02T03:04:05Z");
      declare
         Value : Glib.Date_Time.Gdate_Time := Get_Deletion_Date (Info);
      begin
         Assert_True
           (Glib.Date_Time.To_Unix (Value) = Glib.Date_Time.To_Unix (Stamp));
         Glib.Date_Time.Unref (Value);
      end;

      Glib.Object.Unref (Glib.Object.GObject (Info));
      Unref_Icon (Icon);
      Glib.Date_Time.Unref (Stamp);
      Glib.Time_Zone.Unref (Zone);
   end Test_File_Info_Properties;

   -----------------
   -- Test_Mutate --
   -----------------

   procedure Test_Mutate is
      Dir    : constant Gfile :=
        New_For_Path (Ada.Directories.Compose (Base, "sub"));
      File   : constant Gfile := Get_Child (Dir, "created.txt");
      Stream : Gfile_Output_Stream;
      Error  : Glib.Error.GError;
   begin
      Assert_False (Query_Exists (Dir, null));
      Assert_True (Make_Directory (Dir, null, Error));
      Assert_True (Error = null);
      Assert_True (Query_Exists (Dir, null));

      Assert_False (Query_Exists (File, null));
      Stream := Create
        (Self        => File,
         Flags       => G_File_Create_None,
         Cancellable => null,
         Error       => Error);
      Assert_True (Error = null);
      Assert_True (Stream /= null);
      Assert_True
        (Glib.Output_Stream.Close (Goutput_Stream (Stream), null, Error));
      Assert_True (Error = null);
      Glib.Object.Unref (Glib.Object.GObject (Stream));
      Assert_True (Query_Exists (File, null));

      Assert_True (Delete (File, null, Error));
      Assert_True (Error = null);
      Assert_False (Query_Exists (File, null));
      Assert_True (Delete (Dir, null, Error));
      Assert_True (Error = null);
      Assert_False (Query_Exists (Dir, null));

      Release (File);
      Release (Dir);
   end Test_Mutate;

   ---------------
   -- Test_List --
   ---------------

   procedure Test_List is
      One  : constant Gfile := New_For_Path (Data_Path);
      Two  : constant Gfile :=
        New_For_Path (Ada.Directories.Compose (Base, "other.txt"));

      List  : Gfile_List.Glist := Gfile_List.Null_List;
      Item  : Gfile_List.Glist;
      Count : Guint := 0;

      use type Gfile_List.Glist;
   begin
      --  Append then Prepend, so the list reads Two, One.

      Gfile_List.Append (List, One);
      Gfile_List.Prepend (List, Two);
      Assert_Cmpuint_Eq (Gfile_List.Length (List), 2);

      --  Every element goes through the generated Convert pair on its way in
      --  and out of the C list, so it must come back unchanged.

      Assert_True (Gfile_List.Nth_Data (List, 0) = Two);
      Assert_True (Gfile_List.Nth_Data (List, 1) = One);
      Assert_True (Gfile_List.Nth_Data (List, 0) /= Null_Gfile);
      Assert_True (Equal (Gfile_List.Nth_Data (List, 1), One));
      Assert_Cmpstr_Eq (Get_Basename (Gfile_List.Nth_Data (List, 1)),
                        "data.txt");

      Assert_Cmpint_Eq (Gfile_List.Index (List, Two), 0);
      Assert_Cmpint_Eq (Gfile_List.Index (List, One), 1);

      --  Walk the whole list with First / Next.

      Item := Gfile_List.First (List);
      while Item /= Gfile_List.Null_List loop
         Assert_True (Gfile_List.Get_Data (Item) /= Null_Gfile);
         Count := Count + 1;
         Item := Gfile_List.Next (Item);
      end loop;
      Assert_Cmpuint_Eq (Count, 2);

      --  Free only releases the link cells, so the elements are still ours.

      Gfile_List.Free (List);
      Assert_True (List = Gfile_List.Null_List);

      Release (Two);
      Release (One);
   end Test_List;

   ----------------
   -- Test_SList --
   ----------------

   procedure Test_SList is
      One  : constant Gfile := New_For_Path (Data_Path);
      Two  : constant Gfile :=
        New_For_Path (Ada.Directories.Compose (Base, "other.txt"));

      List  : Gfile_SList.GSlist := Gfile_SList.Null_List;
      Item  : Gfile_SList.GSlist;
      Count : Guint := 0;

      use type Gfile_SList.GSlist;
   begin
      Gfile_SList.Append (List, One);
      Gfile_SList.Prepend (List, Two);
      Assert_Cmpuint_Eq (Gfile_SList.Length (List), 2);

      Assert_True (Gfile_SList.Nth_Data (List, 0) = Two);
      Assert_True (Gfile_SList.Nth_Data (List, 1) = One);
      Assert_True (Equal (Gfile_SList.Nth_Data (List, 1), One));
      Assert_Cmpint_Eq (Gfile_SList.Index (List, One), 1);

      --  A single-linked list has no First, so start from the head itself.

      Item := List;
      while Item /= Gfile_SList.Null_List loop
         Assert_True (Gfile_SList.Get_Data (Item) /= Null_Gfile);
         Count := Count + 1;
         Item := Gfile_SList.Next (Item);
      end loop;
      Assert_Cmpuint_Eq (Count, 2);

      --  Unlike Glib.Glist.Free, Glib.GSlist.Free leaves List pointing at the
      --  freed cells, so there is nothing to assert about it afterwards.

      Gfile_SList.Free (List);

      Release (Two);
      Release (One);
   end Test_SList;

   ------------------------
   -- On_Monitor_Changed --
   ------------------------

   --  Save the callback values for assertions in the test body. Assertions
   --  stay outside the callback so a failure cannot be swallowed by GLib.
   procedure On_Monitor_Changed
     (Self       : access Glib.File_Monitors.Gfile_Monitor_Record'Class;
      File       : Gfile;
      Other_File : Gfile;
      Event_Type : Glib.GEnums.GFile_Monitor_Event) is
   begin
      if Raise_Monitor_Callbacks then
         raise Program_Error with "expected typed monitor callback exception";
      end if;

      Monitor_Typed_Calls := Monitor_Typed_Calls + 1;
      Monitor_Typed_Args_Ok :=
        Glib.Object.Get_Object (Self) =
          Glib.Object.Get_Object (Expected_Monitor)
        and then File = Expected_Monitor_File
        and then Other_File = Glib.GFile.Null_Gfile
        and then Event_Type = Glib.GEnums.Created;
   end On_Monitor_Changed;

   -----------------------------
   -- On_Monitor_Changed_Slot --
   -----------------------------

   --  The slot is the monitor itself here, which keeps its GObject lifetime
   --  tied to the local monitor reference while still exercising slot wiring.
   procedure On_Monitor_Changed_Slot
     (Self       : access Glib.Object.GObject_Record'Class;
      File       : Gfile;
      Other_File : Gfile;
      Event_Type : Glib.GEnums.GFile_Monitor_Event) is
   begin
      if Raise_Monitor_Callbacks then
         raise Program_Error with "expected slot monitor callback exception";
      end if;

      Monitor_Slot_Calls := Monitor_Slot_Calls + 1;
      Monitor_Slot_Args_Ok :=
        Glib.Object.Get_Object (Self) =
          Glib.Object.Get_Object (Expected_Monitor)
        and then File = Expected_Monitor_File
        and then Other_File = Glib.GFile.Null_Gfile
        and then Event_Type = Glib.GEnums.Created;
   end On_Monitor_Changed_Slot;

   -----------------------------
   -- Record_Monitor_Exception --
   -----------------------------

   --  Distinguish the two deliberate callback exceptions from any unexpected
   --  exception reported by a signal marshaller.
   procedure Record_Monitor_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
   begin
      if Ada.Exceptions.Exception_Name (Occurrence) = "PROGRAM_ERROR" then
         Monitor_Exception_Calls := Monitor_Exception_Calls + 1;
      else
         Unexpected_Monitor_Exception_Calls :=
           Unexpected_Monitor_Exception_Calls + 1;
      end if;
   end Record_Monitor_Exception;

   ------------------------
   -- Test_File_Monitor --
   ------------------------

   --  Use Emit_Event to trigger a controlled changed signal instead of relying
   --  on backend-specific filesystem notification timing.
   procedure Test_File_Monitor is
      File : constant Gfile := New_For_Path (Data_Path);
      Error : Glib.Error.GError;
      Monitor : Glib.File_Monitors.Gfile_Monitor;
      Dispatched : Boolean;
   begin
      Write_Payload (File);
      Monitor := Monitor_File
        (File, Glib.GEnums.G_File_Monitor_None, null, Error);
      Assert_True (Error = null);
      Assert_True (Monitor /= null);

      --  The generated wrappers expose cancellation state and the rate-limit
      --  property, so check both the initial and updated values.
      Expected_Monitor := Monitor;
      Expected_Monitor_File := File;
      Assert_False (Glib.File_Monitors.Is_Cancelled (Monitor));
      Assert_False
        (Glib.Properties.Get_Property
           (Monitor,
            Glib.Properties.Property_Boolean (Glib.Build ("cancelled"))));
      Glib.File_Monitors.Set_Rate_Limit (Monitor, 0);
      Assert_Cmpint_Eq
        (Glib.Properties.Int_Properties.Get_Property
           (Monitor,
            Glib.Properties.Int_Properties.Property
              (Glib.Build ("rate-limit"))), 0);

      --  The typed and slot forms share the same signal but use separate
      --  generated marshallers and distinct callback object conversions.
      Monitor_Typed_Calls := 0;
      Monitor_Slot_Calls := 0;
      Monitor_Typed_Args_Ok := False;
      Monitor_Slot_Args_Ok := False;
      Monitor.On_Changed (On_Monitor_Changed'Unrestricted_Access);
      Monitor.On_Changed
        (On_Monitor_Changed_Slot'Unrestricted_Access, Monitor,
         After => True);

      Glib.File_Monitors.Emit_Event
        (Monitor, File, Glib.GFile.Null_Gfile, Glib.GEnums.Created);
      for I in 1 .. 5_000 loop
         exit when Monitor_Typed_Calls = 1 and Monitor_Slot_Calls = 1;
         Dispatched := Glib.Main.Main_Context_Iteration (May_Block => False);
         delay 0.001;
      end loop;
      Assert_Cmpuint_Eq (Guint (Monitor_Typed_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Monitor_Slot_Calls), 1);
      Assert_True (Monitor_Typed_Args_Ok);
      Assert_True (Monitor_Slot_Args_Ok);

      --  Both marshallers must catch a callback exception and forward it to
      --  the configured Ada exception handler instead of unwinding through C.
      Gtkada.Bindings.Set_On_Exception
        (Record_Monitor_Exception'Unrestricted_Access);
      Raise_Monitor_Callbacks := True;
      Glib.File_Monitors.Emit_Event
        (Monitor, File, Glib.GFile.Null_Gfile, Glib.GEnums.Created);
      Raise_Monitor_Callbacks := False;
      Assert_Cmpuint_Eq (Guint (Monitor_Exception_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Unexpected_Monitor_Exception_Calls), 0);

      Assert_True (Glib.File_Monitors.Cancel (Monitor));
      Assert_True (Glib.File_Monitors.Is_Cancelled (Monitor));
      Assert_True
        (Glib.Properties.Get_Property
           (Monitor,
            Glib.Properties.Property_Boolean (Glib.Build ("cancelled"))));

      Glib.Object.Unref (Glib.Object.GObject (Monitor));
      Expected_Monitor := null;
      Expected_Monitor_File := Glib.GFile.Null_Gfile;
      Release (File);
   end Test_File_Monitor;

begin
   Glib.Test.Init;

   if not Ada.Directories.Exists (Base) then
      Ada.Directories.Create_Directory (Base);
   end if;

   Glib.Test.Add_Func ("/gfile/paths", Test_Paths'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gfile/write-read", Test_Write_Read'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gfile/io-stream", Test_IO_Stream'Unrestricted_Access);
   --  TODO FIXME g_bytes_unref_to_data
   --  Glib.Test.Add_Func
   --    ("/gfile/load-bytes", Test_Load_Bytes'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gfile/query-info", Test_Query_Info'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gfile/file-info-attributes",
      Test_File_Info_Attributes'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gfile/file-info-properties",
      Test_File_Info_Properties'Unrestricted_Access);
   Glib.Test.Add_Func ("/gfile/mutate", Test_Mutate'Unrestricted_Access);
   Glib.Test.Add_Func ("/gfile/list", Test_List'Unrestricted_Access);
   Glib.Test.Add_Func ("/gfile/slist", Test_SList'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gfile/file-monitor", Test_File_Monitor'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
