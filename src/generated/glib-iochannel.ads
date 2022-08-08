------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  <description>
--  A data structure representing an IO Channel. The fields should be
--  considered private and should only be accessed with the following
--  functions.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Ada.Streams;             use Ada.Streams;
with Glib.Error;              use Glib.Error;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Main;               use Glib.Main;
with Glib.String;             use Glib.String;
with Gtkada.Types;            use Gtkada.Types;

package Glib.IOChannel is

   type GIOCondition is mod 2 ** Integer'Size;
   pragma Convention (C, GIOCondition);
   --  A bitwise combination representing a condition to watch for on an event
   --  source.

   G_Io_In : constant GIOCondition := 1;
   G_Io_Out : constant GIOCondition := 4;
   G_Io_Pri : constant GIOCondition := 2;
   G_Io_Err : constant GIOCondition := 8;
   G_Io_Hup : constant GIOCondition := 16;
   G_Io_Nval : constant GIOCondition := 32;

   type GIOFlags is mod 2 ** Integer'Size;
   pragma Convention (C, GIOFlags);
   --  Specifies properties of a Glib.IOChannel.Giochannel. Some of the flags
   --  can only be read with Glib.IOChannel.Get_Flags, but not changed with
   --  g_io_channel_set_flags.

   G_Io_Flag_Append : constant GIOFlags := 1;
   G_Io_Flag_Nonblock : constant GIOFlags := 2;
   G_Io_Flag_Is_Readable : constant GIOFlags := 4;
   G_Io_Flag_Is_Writable : constant GIOFlags := 8;
   G_Io_Flag_Is_Writeable : constant GIOFlags := 8;
   G_Io_Flag_Is_Seekable : constant GIOFlags := 16;
   G_Io_Flag_Mask : constant GIOFlags := 31;
   G_Io_Flag_Get_Mask : constant GIOFlags := 31;
   G_Io_Flag_Set_Mask : constant GIOFlags := 3;

   type GIOStatus is (
      G_Io_Status_Error,
      G_Io_Status_Normal,
      G_Io_Status_Eof,
      G_Io_Status_Again);
   pragma Convention (C, GIOStatus);
   --  Stati returned by most of the Glib.IOChannel.GIOFuncs functions.

   type GIOError is (
      G_Io_Error_None,
      G_Io_Error_Again,
      G_Io_Error_Inval,
      G_Io_Error_Unknown);
   pragma Convention (C, GIOError);
   --  Glib.IOChannel.GIOError is only used by the deprecated functions
   --  g_io_channel_read, g_io_channel_write, and g_io_channel_seek.

   type GIOChannel_Error is (
      G_Io_Channel_Error_Fbig,
      G_Io_Channel_Error_Inval,
      G_Io_Channel_Error_Io,
      G_Io_Channel_Error_Isdir,
      G_Io_Channel_Error_Nospc,
      G_Io_Channel_Error_Nxio,
      G_Io_Channel_Error_Overflow,
      G_Io_Channel_Error_Pipe,
      G_Io_Channel_Error_Failed);
   pragma Convention (C, GIOChannel_Error);
   --  Error codes returned by Glib.IOChannel.Giochannel operations.

   type GSeek_Type is (
      G_Seek_Cur,
      G_Seek_Set,
      G_Seek_End);
   pragma Convention (C, GSeek_Type);
   --  An enumeration specifying the base position for a
   --  Glib.IOChannel.Seek_Position operation.

   type GIOFuncs is record
      Io_Read : System.Address;
      Io_Write : System.Address;
      Io_Seek : System.Address;
      Io_Close : System.Address;
      Io_Create_Watch : System.Address;
      Io_Free : System.Address;
      Io_Set_Flags : System.Address;
      Io_Get_Flags : System.Address;
   end record;
   pragma Convention (C, GIOFuncs);

   function From_Object_Free (B : access GIOFuncs) return GIOFuncs;
   pragma Inline (From_Object_Free);
   --  A table of functions used to handle different types of
   --  Glib.IOChannel.Giochannel in a generic way.

   type GIO_Channel_Record is record
      Ref_Count : Glib.Gint := 0;
      Funcs : GIOFuncs;
      Encoding : Gtkada.Types.Chars_Ptr;
      Read_Cd : System.Address;
      Write_Cd : System.Address;
      Line_Term : Gtkada.Types.Chars_Ptr;
      Line_Term_Len : Guint;
      Buf_Size : Gsize;
      Read_Buf : Glib.String.Gstring;
      Encoded_Read_Buf : Glib.String.Gstring;
      Write_Buf : Glib.String.Gstring;
      Partial_Write_Buf : Gchar_Array (1 .. 6);
      Use_Buffer : Guint;
      Do_Encode : Guint;
      Close_On_Unref : Guint;
      Is_Readable : Guint;
      Is_Writeable : Guint;
      Is_Seekable : Guint;
      Reserved1 : System.Address := System.Null_Address;
      Reserved2 : System.Address := System.Null_Address;
   end record;
   pragma Convention (C, GIO_Channel_Record);

   function From_Object_Free (B : access GIO_Channel_Record) return GIO_Channel_Record;
   pragma Inline (From_Object_Free);
   --  A data structure representing an IO Channel. The fields should be
   --  considered private and should only be accessed with the following
   --  functions.

   type Giochannel is access all GIO_Channel_Record;
   pragma No_Strict_Aliasing (Giochannel);

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package GIOCondition_Properties is
      new Generic_Internal_Discrete_Property (GIOCondition);
   type Property_GIOCondition is new GIOCondition_Properties.Property;

   package GIOFlags_Properties is
      new Generic_Internal_Discrete_Property (GIOFlags);
   type Property_GIOFlags is new GIOFlags_Properties.Property;

   package GIOStatus_Properties is
      new Generic_Internal_Discrete_Property (GIOStatus);
   type Property_GIOStatus is new GIOStatus_Properties.Property;

   package GIOError_Properties is
      new Generic_Internal_Discrete_Property (GIOError);
   type Property_GIOError is new GIOError_Properties.Property;

   package GIOChannel_Error_Properties is
      new Generic_Internal_Discrete_Property (GIOChannel_Error);
   type Property_GIOChannel_Error is new GIOChannel_Error_Properties.Property;

   package GSeek_Type_Properties is
      new Generic_Internal_Discrete_Property (GSeek_Type);
   type Property_GSeek_Type is new GSeek_Type_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure G_Unix_New (Self : out Giochannel; Fd : Glib.Gint);
   --  Creates a new Glib.IOChannel.Giochannel given a file descriptor. On
   --  UNIX systems this works for plain files, pipes, and sockets.
   --  The returned Glib.IOChannel.Giochannel has a reference count of 1.
   --  The default encoding for Glib.IOChannel.Giochannel is UTF-8. If your
   --  application is reading output from a command using via pipe, you may
   --  need to set the encoding to the encoding of the current locale (see
   --  g_get_charset) with the g_io_channel_set_encoding function.
   --  If you want to read raw binary data without interpretation, then call
   --  the g_io_channel_set_encoding function with null for the encoding
   --  argument.
   --  This function is available in GLib on Windows, too, but you should
   --  avoid using it on Windows. The domain of file descriptors and sockets
   --  overlap. There is no way for GLib to know which one you mean in case the
   --  argument you pass to this function happens to be both a valid file
   --  descriptor and socket. If that happens a warning is issued, and GLib
   --  assumes that it is the file descriptor you mean.
   --  "fd": a file descriptor.

   function Giochannel_Unix_New (Fd : Glib.Gint) return Giochannel;
   --  Creates a new Glib.IOChannel.Giochannel given a file descriptor. On
   --  UNIX systems this works for plain files, pipes, and sockets.
   --  The returned Glib.IOChannel.Giochannel has a reference count of 1.
   --  The default encoding for Glib.IOChannel.Giochannel is UTF-8. If your
   --  application is reading output from a command using via pipe, you may
   --  need to set the encoding to the encoding of the current locale (see
   --  g_get_charset) with the g_io_channel_set_encoding function.
   --  If you want to read raw binary data without interpretation, then call
   --  the g_io_channel_set_encoding function with null for the encoding
   --  argument.
   --  This function is available in GLib on Windows, too, but you should
   --  avoid using it on Windows. The domain of file descriptors and sockets
   --  overlap. There is no way for GLib to know which one you mean in case the
   --  argument you pass to this function happens to be both a valid file
   --  descriptor and socket. If that happens a warning is issued, and GLib
   --  assumes that it is the file descriptor you mean.
   --  "fd": a file descriptor.

   -------------
   -- Methods --
   -------------

   function Get_Buffer_Condition (Self : Giochannel) return GIOCondition;
   pragma Import (C, Get_Buffer_Condition, "g_io_channel_get_buffer_condition");
   --  This function returns a Glib.IOChannel.GIOCondition depending on
   --  whether there is data to be read/space to write data in the internal
   --  buffers in the Glib.IOChannel.Giochannel. Only the flags
   --  Glib.IOChannel.G_Io_In and Glib.IOChannel.G_Io_Out may be set.

   function Get_Buffer_Size (Self : Giochannel) return Gsize;
   pragma Import (C, Get_Buffer_Size, "g_io_channel_get_buffer_size");
   --  Gets the buffer size.

   procedure Set_Buffer_Size (Self : Giochannel; Size : Gsize);
   pragma Import (C, Set_Buffer_Size, "g_io_channel_set_buffer_size");
   --  Sets the buffer size.
   --  "size": the size of the buffer, or 0 to let GLib pick a good size

   function Get_Buffered (Self : Giochannel) return Boolean;
   --  Returns whether Channel is buffered.

   procedure Set_Buffered (Self : Giochannel; Buffered : Boolean);
   --  The buffering state can only be set if the channel's encoding is null.
   --  For any other encoding, the channel must be buffered.
   --  A buffered channel can only be set unbuffered if the channel's internal
   --  buffers have been flushed. Newly created channels or channels which have
   --  returned Glib.IOChannel.G_Io_Status_Eof not require such a flush. For
   --  write-only channels, a call to g_io_channel_flush () is sufficient. For
   --  all other channels, the buffers may be flushed by a call to
   --  g_io_channel_seek_position (). This includes the possibility of seeking
   --  with seek type Glib.IOChannel.G_Seek_Cur and an offset of zero. Note
   --  that this means that socket-based channels cannot be set unbuffered once
   --  they have had data read from them.
   --  On unbuffered channels, it is safe to mix read and write calls from the
   --  new and old APIs, if this is necessary for maintaining old code.
   --  The default state of the channel is buffered.
   --  "buffered": whether to set the channel buffered or unbuffered

   function Get_Close_On_Unref (Self : Giochannel) return Boolean;
   --  Returns whether the file/socket/whatever associated with Channel will
   --  be closed when Channel receives its final unref and is destroyed. The
   --  default value of this is True for channels created by
   --  g_io_channel_new_file (), and False for all other channels.

   procedure Set_Close_On_Unref (Self : Giochannel; Do_Close : Boolean);
   --  Setting this flag to True for a channel you have already closed can
   --  cause problems.
   --  "do_close": Whether to close the channel on the final unref of the
   --  GIOChannel data structure. The default value of this is True for
   --  channels created by g_io_channel_new_file (), and False for all other
   --  channels.

   function Get_Encoding (Self : Giochannel) return UTF8_String;
   --  Gets the encoding for the input/output of the channel. The internal
   --  encoding is always UTF-8. The encoding null makes the channel safe for
   --  binary data.

   function Get_Flags (Self : Giochannel) return GIOFlags;
   pragma Import (C, Get_Flags, "g_io_channel_get_flags");
   --  Gets the current flags for a Glib.IOChannel.Giochannel, including
   --  read-only flags such as Glib.IOChannel.G_Io_Flag_Is_Readable.
   --  The values of the flags Glib.IOChannel.G_Io_Flag_Is_Readable and
   --  Glib.IOChannel.G_Io_Flag_Is_Writable are cached for internal use by the
   --  channel when it is created. If they should change at some later point
   --  (e.g. partial shutdown of a socket with the UNIX shutdown function), the
   --  user should immediately call Glib.IOChannel.Get_Flags to update the
   --  internal values of these flags.

   function Get_Line_Term
      (Self   : Giochannel;
       Length : in out Glib.Gint) return UTF8_String;
   --  This returns the string that Glib.IOChannel.Giochannel uses to
   --  determine where in the file a line break occurs. A value of null
   --  indicates autodetection.
   --  "length": a location to return the length of the line terminator

   procedure Set_Line_Term
      (Self      : Giochannel;
       Line_Term : UTF8_String := "";
       Length    : Glib.Gint);
   --  This sets the string that Glib.IOChannel.Giochannel uses to determine
   --  where in the file a line break occurs.
   --  "line_term": The line termination string. Use null for autodetect.
   --  Autodetection breaks on "\n", "\r\n", "\r", "\0", and the Unicode
   --  paragraph separator. Autodetection should not be used for anything other
   --  than file-based channels.
   --  "length": The length of the termination string. If -1 is passed, the
   --  string is assumed to be nul-terminated. This option allows termination
   --  strings with embedded nuls.

   procedure Init (Self : Giochannel);
   pragma Import (C, Init, "g_io_channel_init");
   --  Initializes a Glib.IOChannel.Giochannel struct.
   --  This is called by each of the above functions when creating a
   --  Glib.IOChannel.Giochannel, and so is not often needed by the application
   --  programmer (unless you are creating a new type of
   --  Glib.IOChannel.Giochannel).

   function Ref (Self : Giochannel) return Giochannel;
   pragma Import (C, Ref, "g_io_channel_ref");
   --  Increments the reference count of a Glib.IOChannel.Giochannel.

   function Seek_Position
      (Self     : Giochannel;
       Offset   : Gint64;
       The_Type : GSeek_Type) return GIOStatus;
   pragma Import (C, Seek_Position, "g_io_channel_seek_position");
   --  Replacement for g_io_channel_seek with the new API.
   --  "offset": The offset in bytes from the position specified by Type
   --  "type": a Glib.IOChannel.GSeek_Type. The type Glib.IOChannel.G_Seek_Cur
   --  is only allowed in those cases where a call to g_io_channel_set_encoding
   --  () is allowed. See the documentation for g_io_channel_set_encoding ()
   --  for details.

   function Unix_Get_Fd (Self : Giochannel) return Glib.Gint;
   pragma Import (C, Unix_Get_Fd, "g_io_channel_unix_get_fd");
   --  Returns the file descriptor of the Glib.IOChannel.Giochannel.
   --  On Windows this function returns the file descriptor or socket of the
   --  Glib.IOChannel.Giochannel.

   procedure Unref (Self : Giochannel);
   pragma Import (C, Unref, "g_io_channel_unref");
   --  Decrements the reference count of a Glib.IOChannel.Giochannel.

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Shutdown
     (Self     : Giochannel;
      Flush    : Glib.Gboolean;
      Error    : access Glib.Error.GError) return GIOStatus;
   pragma Import (C, Shutdown, "g_io_channel_shutdown");
   --  Close an IO channel. Any pending data to be written will be flushed if
   --  Flush is True. The channel will not be freed until the last reference is
   --  dropped using Glib.IOChannel.Unref.
   --  "flush": if True, flush pending

   function Flush
     (Self  : Giochannel;
      Error : access Glib.Error.GError) return GIOStatus;
   pragma Import (C, Flush, "g_io_channel_flush");
   --  Flushes the write buffer for the GIOChannel.

   function Set_Encoding
     (Self     : Giochannel;
      Encoding : UTF8_String := "";
      Error    : access Glib.Error.GError) return GIOStatus;
   --  Sets the encoding for the input/output of the channel. The internal
   --  encoding is always UTF-8. The default encoding for the external file is
   --  UTF-8.
   --  The encoding null is safe to use with binary data.
   --  The encoding can only be set if one of the following conditions is
   --  true:
   --  - The channel was just created, and has not been written to or read
   --  from yet.
   --  - The channel is write-only.
   --  - The channel is a file, and the file pointer was just repositioned by
   --  a call to Glib.IOChannel.Seek_Position. (This flushes all the internal
   --  buffers.)
   --  - The current encoding is null or UTF-8.
   --  - One of the (new API) read functions has just returned
   --  Glib.IOChannel.G_Io_Status_Eof (or, in the case of
   --  g_io_channel_read_to_end, Glib.IOChannel.G_Io_Status_Normal).
   --  - One of the functions Glib.IOChannel.Read_Chars or
   --  Glib.IOChannel.Read_Unichar has returned
   --  Glib.IOChannel.G_Io_Status_Again or Glib.IOChannel.G_Io_Status_Error.
   --  This may be useful in the case of G_CONVERT_ERROR_ILLEGAL_SEQUENCE.
   --  Returning one of these statuses from g_io_channel_read_line,
   --  Glib.IOChannel.Read_Line_String, or g_io_channel_read_to_end does not
   --  guarantee that the encoding can be changed.
   --  Channels which do not meet one of the above conditions cannot call
   --  Glib.IOChannel.Seek_Position with an offset of
   --  Glib.IOChannel.G_Seek_Cur, and, if they are "seekable", cannot call
   --  Glib.IOChannel.Write_Chars after calling one of the API "read"
   --  functions.
   --  "encoding": the encoding type

   function Set_Flags
     (Self : Giochannel;
      Flags : GIOFlags;
      Error : access Glib.Error.GError) return GIOStatus;
   pragma Import (C, Set_Flags, "g_io_channel_set_flags");
   --  Sets the (writeable) flags in Channel to (Flags and
   --  Glib.IOChannel.G_Io_Flag_Set_Mask).
   --  "flags": the flags to set on the IO channel

   function Read_Chars
     (Self       : Giochannel;
      Buf        : out Ada.Streams.Stream_Element_Array;
      Bytes_Read : access Gsize;
      Error      : access Glib.Error.GError) return GIOStatus;
   --  Replacement for g_io_channel_read with the new API.
   --  "buf": a buffer to read data into
   --  "count": is sent as Buf'Length.
   --  "bytes_read": The number of bytes read. This may be zero even on
   --  success if count less than 6 and the channel's encoding is non-null.
   --  This indicates that the next UTF-8 character is too wide for
   --  the buffer.

   function Write_Chars
     (Self          : Giochannel;
      Buf           : Ada.Streams.Stream_Element_Array;
      Bytes_Written : access Gsize;
      Error         : access Glib.Error.GError) return GIOStatus;
   --  Replacement for g_io_channel_write with the new API.
   --  On seekable channels with encodings other than null or UTF-8, generic
   --  mixing of reading and writing is not allowed. A call to
   --  g_io_channel_write_chars () may only be made on a channel from which
   --  data has been read in the cases described in the documentation for
   --  g_io_channel_set_encoding ().
   --  "buf": a buffer to write data from
   --  "count": is sent as Buf'Length.
   --  "bytes_written": The number of bytes written. This can be nonzero even
   --  if the return value is not Glib.IOChannel.G_Io_Status_Normal. If the
   --  return value is Glib.IOChannel.G_Io_Status_Normal and the channel is
   --  blocking, this will always be equal to Count if Count >= 0.

   function Read_Line_String
     (Self           : Giochannel;
      Buffer         : Glib.String.Gstring;
      Terminator_Pos : in out Gsize;
      Error          : access Glib.Error.GError) return GIOStatus;
   pragma Import (C, Read_Line_String, "g_io_channel_read_line_string");
   --  Reads a line from a Glib.IOChannel.Giochannel, using a
   --  Glib.String.Gstring as a buffer.
   --  "buffer": a Glib.String.Gstring into which the line will be written. If
   --  Buffer already contains data, the old data will be overwritten.
   --  "terminator_pos": location to store position of line terminator, or
   --  null

   function Read_Unichar
     (Self    : Giochannel;
      Thechar : access Gunichar;
      Error   : access Glib.Error.GError) return GIOStatus;
   pragma Import (C, Read_Unichar, "g_io_channel_read_unichar");
   --  Reads a Unicode character from Channel. This function cannot be called
   --  on a channel with null encoding.
   --  "thechar": a location to return a character

   function Write_Unichar
     (Self    : Giochannel;
      Thechar : Gunichar;
      Error   : access Glib.Error.GError) return GIOStatus;
   pragma Import (C, Write_Unichar, "g_io_channel_write_unichar");
   --  Writes a Unicode character to Channel. This function cannot be called
   --  on a channel with null encoding.
   --  "thechar": a character

   generic
   type User_Data is limited private;
   function Generic_Add_Watch
     (Channel    : Giochannel;
      Condition  : GIOCondition;
      Callback   : access function
        (Source    : Giochannel;
         Condition : GIOCondition;
         Data      : access User_Data) return Glib.Gboolean;
      Data       : access User_Data) return Glib.Main.G_Source_Id;
   pragma Import (C, Generic_Add_Watch, "g_io_add_watch");
   --  Adds the Glib.IOChannel.Giochannel into the default main loop context
   --  with the default priority.
   --  "channel": a Glib.IOChannel.Giochannel
   --  "condition": the condition to watch for
   --  "callback": the function to call when the condition is satisfied

   ---------------
   -- Functions --
   ---------------

   function Error_From_Errno (En : Glib.Gint) return GIOChannel_Error;
   pragma Import (C, Error_From_Errno, "g_io_channel_error_from_errno");
   --  Converts an `errno` error number to a Glib.IOChannel.GIOChannel_Error.
   --  "en": an `errno` error number, e.g. `EINVAL`

   function Error_Quark return Glib.GQuark;
   pragma Import (C, Error_Quark, "g_io_channel_error_quark");

   function Io_Create_Watch
      (Channel   : Giochannel;
       Condition : GIOCondition) return Glib.Main.G_Source;
   pragma Import (C, Io_Create_Watch, "g_io_create_watch");
   --  Creates a Glib.Main.G_Source that's dispatched when Condition is met
   --  for the given Channel. For example, if condition is G_IO_IN, the source
   --  will be dispatched when there's data available for reading.
   --  g_io_add_watch is a simpler interface to this same functionality, for
   --  the case where you want to add the source to the default main loop
   --  context at the default priority.
   --  On Windows, polling a Glib.Main.G_Source created to watch a channel for
   --  a socket puts the socket in non-blocking mode. This is a side-effect of
   --  the implementation and unavoidable.
   --  "channel": a Glib.IOChannel.Giochannel to watch
   --  "condition": conditions to watch for

end Glib.IOChannel;
