--  Glib.IOChannel tests for a private temporary file descriptor. There is no
--  matching C test in testsuite/c_tests/ to port.
--  The tests exercise channel configuration, byte and Unicode I/O, seeking,
--  and the low-level ownership conversion helpers.

with Ada.Command_Line;
with Ada.Streams;
with System;
with System.Address_To_Access_Conversions;

with Glib;                 use Glib;
with Glib.Error;
with Glib.IOChannel;       use Glib.IOChannel;
with Glib.String;
with Glib.Test;            use Glib.Test;
with Gtkada.Bindings;
with Gtkada.Types;

procedure Main is

   use type Glib.Error.GError;
   use type Glib.Gint;
   use type Glib.Gsize;
   use type Glib.Gunichar;
   use type Glib.IOChannel.GIOFlags;
   use type Glib.IOChannel.GIOCondition;
   use type Glib.IOChannel.GIOStatus;
   use type Glib.IOChannel.GIOChannel_Error;
   use type Ada.Streams.Stream_Element_Array;
   use type Ada.Streams.Stream_Element_Offset;
   use type Gtkada.Types.Chars_Ptr;
   use type System.Address;

   --  mkstemp gives this test an isolated, already-open read/write file.
   function Make_Temp (Template : Gtkada.Types.Chars_Ptr) return Glib.Gint;
   pragma Import (C, Make_Temp, "mkstemp");

   procedure Remove_File (Path : Gtkada.Types.Chars_Ptr);
   pragma Import (C, Remove_File, "unlink");

   --  These buffers are allocated with GLib's allocator because the binding
   --  helpers below take ownership and release them with g_free.
   function Malloc_Zeroed (Size : Glib.Gsize) return System.Address;
   pragma Import (C, Malloc_Zeroed, "g_malloc0");

   package Funcs_Access is new
     System.Address_To_Access_Conversions (GIOFuncs);
   package Channel_Access is new
     System.Address_To_Access_Conversions (GIO_Channel_Record);

   function To_Bytes (Text : String) return Ada.Streams.Stream_Element_Array;
   procedure Test_Channel_Operations with Convention => C;
   procedure Test_Ownership_Conversions with Convention => C;

   ----------------
   -- To_Bytes --
   ----------------

   --  Keep the stream buffers byte-for-byte comparable with the C API.
   function To_Bytes (Text : String) return Ada.Streams.Stream_Element_Array is
      Result : Ada.Streams.Stream_Element_Array (1 .. Text'Length);
      Index  : Ada.Streams.Stream_Element_Offset := Result'First;
   begin
      for C of Text loop
         Result (Index) := Ada.Streams.Stream_Element (Character'Pos (C));
         Index := Index + Ada.Streams.Stream_Element_Offset'(1);
      end loop;
      return Result;
   end To_Bytes;

   ---------------------------
   -- Test_Channel_Operations --
   ---------------------------

   --  Check configured state through the getters, then round-trip text and
   --  raw bytes through a seekable descriptor.
   procedure Test_Channel_Operations is
      Template : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String
        ("/tmp/gtkada-iochannel-XXXXXX");
      Fd       : Glib.Gint;
      Channel  : Giochannel;
      Other    : Giochannel;
      Error    : aliased Glib.Error.GError := null;
      Status   : GIOStatus;
      Length   : aliased Glib.Gint := -1;
      Written  : aliased Gsize := 0;
      Read     : aliased Gsize := 0;
      Character_Read : aliased Gunichar := 0;
      Terminator_Pos : Gsize := 0;
      Bytes    : constant Ada.Streams.Stream_Element_Array :=
        To_Bytes ("hello" & ASCII.LF);
      Readback : Ada.Streams.Stream_Element_Array (1 .. Bytes'Length);
      Line     : Glib.String.Gstring;
   begin
      Fd := Make_Temp (Template);
      Assert_True (Fd >= 0);
      Remove_File (Template);
      Gtkada.Types.Free (Template);

      --  Both public constructors wrap the same descriptor; release the
      --  second channel without closing the descriptor it does not own.
      Channel := Giochannel_Unix_New (Fd);
      G_Unix_New (Other, Fd);
      Assert_True (Unix_Get_Fd (Channel) = Fd);
      Assert_True (Unix_Get_Fd (Other) = Fd);
      Unref (Other);

      Assert_True (Get_Buffered (Channel));
      Assert_True (not Get_Close_On_Unref (Channel));
      Assert_True (Get_Buffer_Size (Channel) > 0);
      Set_Buffer_Size (Channel, 128);
      Assert_True (Get_Buffer_Size (Channel) = 128);
      Assert_True ((Get_Buffer_Condition (Channel)
                    and not (G_Io_In or G_Io_Out)) = 0);

      --  The writable nonblocking flag can be set and read back on this
      --  regular file, while the read/write/seekable flags are discovered.
      Assert_True ((Get_Flags (Channel) and G_Io_Flag_Is_Readable) /= 0);
      Assert_True ((Get_Flags (Channel) and G_Io_Flag_Is_Writable) /= 0);
      Assert_True ((Get_Flags (Channel) and G_Io_Flag_Is_Seekable) /= 0);
      Status := Set_Flags (Channel, G_Io_Flag_Nonblock, Error'Access);
      Assert_True (Status = G_Io_Status_Normal and then Error = null);
      Assert_True ((Get_Flags (Channel) and G_Io_Flag_Nonblock) /= 0);
      Assert_True ((Get_Flags (Channel) and G_Io_Flag_Mask)
                   = (Get_Flags (Channel) and G_Io_Flag_Get_Mask));

      --  Test both string and null encodings; unbuffering is only valid for
      --  the latter. The no-op close-on-unref value is restored before unref.
      Status := Set_Encoding (Channel, "UTF-8", Error'Access);
      Assert_True (Status = G_Io_Status_Normal and then Error = null);
      Assert_True (Get_Encoding (Channel) = "UTF-8");
      Status := Set_Encoding (Channel, "", Error'Access);
      Assert_True (Status = G_Io_Status_Normal and then Error = null);
      Assert_True (Get_Encoding (Channel) = "");
      Set_Buffered (Channel, False);
      Assert_True (not Get_Buffered (Channel));
      Set_Buffered (Channel, True);
      Assert_True (Get_Buffered (Channel));

      Set_Close_On_Unref (Channel, True);
      Assert_True (Get_Close_On_Unref (Channel));
      Set_Close_On_Unref (Channel, False);
      Assert_True (not Get_Close_On_Unref (Channel));

      --  A custom terminator exercises the allocated-string branch; an empty
      --  terminator asks GLib to restore automatic detection.
      Set_Line_Term (Channel, "|", 1);
      Assert_True (Get_Line_Term (Channel, Length'Access) = "|");
      Assert_True (Length = 1);
      Set_Line_Term (Channel, "", 0);
      Assert_True (Get_Line_Term (Channel, Length'Access) = "");
      Assert_True (Length = 0);

      --  Write and flush a complete line, then read it through both the
      --  GString line API and the stream-element byte API.
      Status := Write_Chars
        (Channel, Bytes, Written'Access, Error'Access);
      Assert_True (Status = G_Io_Status_Normal and then Error = null);
      Assert_True (Written = Gsize (Bytes'Length));
      Assert_True (Flush (Channel, Error'Access) = G_Io_Status_Normal);
      Assert_True (Error = null);

      Assert_True
        (Seek_Position
           (Channel, 0, G_Seek_Set, Error) = G_Io_Status_Normal);
      Assert_True (Error = null);
      Line := Glib.String.Gstring_New;
      Status := Read_Line_String
        (Channel, Line, Terminator_Pos, Error'Access);
      Assert_True (Status = G_Io_Status_Normal and then Error = null);
      Assert_True (Line.Len = Gsize (Bytes'Length));
      Assert_True
        (UTF8_String'(Gtkada.Bindings.Value_Allowing_Null (Line.Str))
         = "hello" & ASCII.LF);
      declare
         Discarded : constant UTF8_String := Glib.String.Free (Line, True);
      begin
         Assert_True (Discarded = "");
      end;

      Assert_True
        (Seek_Position
           (Channel, 0, G_Seek_Set, Error) = G_Io_Status_Normal);
      Status := Read_Chars
        (Channel, Readback, Read'Access, Error'Access);
      Assert_True (Status = G_Io_Status_Normal and then Error = null);
      Assert_True (Read = Gsize (Bytes'Length));
      Assert_True (Readback = Bytes);

      --  Passing null for Bytes_Read is supported; the data still arrives.
      Assert_True
        (Seek_Position
           (Channel, 0, G_Seek_Set, Error) = G_Io_Status_Normal);
      Status := Read_Chars (Channel, Readback, null, Error'Access);
      Assert_True (Status = G_Io_Status_Normal and then Error = null);
      Assert_True (Readback = Bytes);

      --  Unicode helpers require a non-null encoding and round-trip one
      --  character through the same seekable file.
      Status := Set_Encoding (Channel, "UTF-8", Error'Access);
      Assert_True (Status = G_Io_Status_Normal and then Error = null);
      Assert_True
        (Seek_Position
           (Channel, 0, G_Seek_Set, Error) = G_Io_Status_Normal);
      Assert_True
        (Write_Unichar
           (Channel, Gunichar (Character'Pos ('Z')), Error'Access)
         = G_Io_Status_Normal);
      Assert_True (Flush (Channel, Error'Access) = G_Io_Status_Normal);
      Assert_True
        (Seek_Position
           (Channel, 0, G_Seek_Set, Error) = G_Io_Status_Normal);
      Assert_True
        (Read_Unichar (Channel, Character_Read'Access, Error'Access)
         = G_Io_Status_Normal);
      Assert_True (Character_Read = Gunichar (Character'Pos ('Z')));

      --  A negative absolute offset is rejected with the documented
      --  IOChannel invalid-argument error.
      Status := Seek_Position (Channel, -1, G_Seek_Set, Error);
      Assert_True (Status = G_Io_Status_Error and then Error /= null);
      Assert_True
        (GIOChannel_Error_Matches (Error, G_Io_Channel_Error_Inval));
      Glib.Error.Error_Free (Error);
      Error := null;
      Assert_True (Error_Quark = GIOChannel_Error_Domain);
      Assert_True
        (Error_From_Errno (22) = G_Io_Channel_Error_Inval);

      Assert_True
        (Shutdown (Channel, 1, Error'Access) = G_Io_Status_Normal);
      Assert_True (Error = null);
      Unref (Channel);
   end Test_Channel_Operations;

   -------------------------------
   -- Test_Ownership_Conversions --
   -------------------------------

   --  These GtkAda-internal helpers copy a C record and release its
   --  g_malloc0 allocation. Zeroed records are valid inputs for both layouts.
   procedure Test_Ownership_Conversions is
      Funcs_Ptr : constant Funcs_Access.Object_Pointer :=
        Funcs_Access.To_Pointer
          (Malloc_Zeroed (Gsize (GIOFuncs'Size / System.Storage_Unit)));
      Record_Ptr : constant Channel_Access.Object_Pointer :=
        Channel_Access.To_Pointer
          (Malloc_Zeroed
             (Gsize (GIO_Channel_Record'Size / System.Storage_Unit)));
      Funcs_Result : GIOFuncs;
      Record_Result : GIO_Channel_Record;
   begin
      Funcs_Result := From_Object_Free (Funcs_Ptr.all'Access);
      Assert_True (Funcs_Result.Io_Read = System.Null_Address);
      Assert_True (Funcs_Result.Io_Get_Flags = System.Null_Address);

      Record_Result := From_Object_Free (Record_Ptr.all'Access);
      Assert_True (Record_Result.Ref_Count = 0);
      Assert_True (Record_Result.Encoding = Gtkada.Types.Null_Ptr);
      Assert_True (Record_Result.Reserved1 = System.Null_Address);
   end Test_Ownership_Conversions;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/glib-iochannel/channel-operations",
      Test_Channel_Operations'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-iochannel/ownership-conversions",
      Test_Ownership_Conversions'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
