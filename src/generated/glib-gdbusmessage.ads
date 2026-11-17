------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  A type for representing D-Bus messages that can be sent or received on a
--  Gdbus.Connection.Gdbus_Connection.

pragma Warnings (Off, "*is already use-visible*");
with Glib.Error;       use Glib.Error;
with Glib.GEnums;      use Glib.GEnums;
with Glib.GUnixFDList; use Glib.GUnixFDList;
with Glib.Object;      use Glib.Object;
with Glib.Properties;  use Glib.Properties;
with Glib.Variant;     use Glib.Variant;

package Glib.GDBusMessage is

   type Gdbus_Message_Record is new GObject_Record with null record;
   type Gdbus_Message is access all Gdbus_Message_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure G_New (Self : out Gdbus_Message);
   --  Creates a new empty Glib.GDBusMessage.Gdbus_Message.
   --  Since: gtk+ 2.26

   procedure Initialize (Self : not null access Gdbus_Message_Record'Class);
   --  Creates a new empty Glib.GDBusMessage.Gdbus_Message.
   --  Since: gtk+ 2.26
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gdbus_Message_New return Gdbus_Message;
   --  Creates a new empty Glib.GDBusMessage.Gdbus_Message.
   --  Since: gtk+ 2.26

   procedure G_New_From_Blob
      (Self         : out Gdbus_Message;
       Blob         : Guint8_Array;
       Blob_Len     : Gsize;
       Capabilities : Glib.GEnums.GDBus_Capability_Flags;
       Error        : out Glib.Error.GError);
   --  Creates a new Glib.GDBusMessage.Gdbus_Message from the data stored at
   --  Blob. The byte order that the message was in can be retrieved using
   --  Glib.GDBusMessage.Get_Byte_Order.
   --  If the Blob cannot be parsed, contains invalid fields, or contains
   --  invalid headers, G_IO_ERROR_INVALID_ARGUMENT will be returned.
   --  Since: gtk+ 2.26
   --  @param Blob A blob representing a binary D-Bus message.
   --  @param Blob_Len The length of Blob.
   --  @param Capabilities A Glib.GEnums.GDBus_Capability_Flags describing
   --  what protocol features are supported.
   --  @param Error the return location for a recoverable error

   procedure Initialize_From_Blob
      (Self         : not null access Gdbus_Message_Record'Class;
       Blob         : Guint8_Array;
       Blob_Len     : Gsize;
       Capabilities : Glib.GEnums.GDBus_Capability_Flags;
       Error        : out Glib.Error.GError);
   --  Creates a new Glib.GDBusMessage.Gdbus_Message from the data stored at
   --  Blob. The byte order that the message was in can be retrieved using
   --  Glib.GDBusMessage.Get_Byte_Order.
   --  If the Blob cannot be parsed, contains invalid fields, or contains
   --  invalid headers, G_IO_ERROR_INVALID_ARGUMENT will be returned.
   --  Since: gtk+ 2.26
   --  Initialize_From_Blob does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Blob A blob representing a binary D-Bus message.
   --  @param Blob_Len The length of Blob.
   --  @param Capabilities A Glib.GEnums.GDBus_Capability_Flags describing
   --  what protocol features are supported.
   --  @param Error the return location for a recoverable error

   function Gdbus_Message_New_From_Blob
      (Blob         : Guint8_Array;
       Blob_Len     : Gsize;
       Capabilities : Glib.GEnums.GDBus_Capability_Flags;
       Error        : out Glib.Error.GError) return Gdbus_Message;
   --  Creates a new Glib.GDBusMessage.Gdbus_Message from the data stored at
   --  Blob. The byte order that the message was in can be retrieved using
   --  Glib.GDBusMessage.Get_Byte_Order.
   --  If the Blob cannot be parsed, contains invalid fields, or contains
   --  invalid headers, G_IO_ERROR_INVALID_ARGUMENT will be returned.
   --  Since: gtk+ 2.26
   --  @param Blob A blob representing a binary D-Bus message.
   --  @param Blob_Len The length of Blob.
   --  @param Capabilities A Glib.GEnums.GDBus_Capability_Flags describing
   --  what protocol features are supported.
   --  @param Error the return location for a recoverable error

   procedure G_New_Method_Call
      (Self   : out Gdbus_Message;
       Name   : UTF8_String := "";
       Path   : UTF8_String;
       Iface  : UTF8_String := "";
       Method : UTF8_String);
   --  Creates a new Glib.GDBusMessage.Gdbus_Message for a method call.
   --  Since: gtk+ 2.26
   --  @param Name A valid D-Bus name or null.
   --  @param Path A valid object path.
   --  @param Iface A valid D-Bus interface name or null.
   --  @param Method A valid method name.

   procedure Initialize_Method_Call
      (Self   : not null access Gdbus_Message_Record'Class;
       Name   : UTF8_String := "";
       Path   : UTF8_String;
       Iface  : UTF8_String := "";
       Method : UTF8_String);
   --  Creates a new Glib.GDBusMessage.Gdbus_Message for a method call.
   --  Since: gtk+ 2.26
   --  Initialize_Method_Call does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Name A valid D-Bus name or null.
   --  @param Path A valid object path.
   --  @param Iface A valid D-Bus interface name or null.
   --  @param Method A valid method name.

   function Gdbus_Message_New_Method_Call
      (Name   : UTF8_String := "";
       Path   : UTF8_String;
       Iface  : UTF8_String := "";
       Method : UTF8_String) return Gdbus_Message;
   --  Creates a new Glib.GDBusMessage.Gdbus_Message for a method call.
   --  Since: gtk+ 2.26
   --  @param Name A valid D-Bus name or null.
   --  @param Path A valid object path.
   --  @param Iface A valid D-Bus interface name or null.
   --  @param Method A valid method name.

   procedure G_New_Signal
      (Self   : out Gdbus_Message;
       Path   : UTF8_String;
       Iface  : UTF8_String;
       Signal : UTF8_String);
   --  Creates a new Glib.GDBusMessage.Gdbus_Message for a signal emission.
   --  Since: gtk+ 2.26
   --  @param Path A valid object path.
   --  @param Iface A valid D-Bus interface name.
   --  @param Signal A valid signal name.

   procedure Initialize_Signal
      (Self   : not null access Gdbus_Message_Record'Class;
       Path   : UTF8_String;
       Iface  : UTF8_String;
       Signal : UTF8_String);
   --  Creates a new Glib.GDBusMessage.Gdbus_Message for a signal emission.
   --  Since: gtk+ 2.26
   --  Initialize_Signal does nothing if the object was already created with
   --  another call to Initialize* or G_New.
   --  @param Path A valid object path.
   --  @param Iface A valid D-Bus interface name.
   --  @param Signal A valid signal name.

   function Gdbus_Message_New_Signal
      (Path   : UTF8_String;
       Iface  : UTF8_String;
       Signal : UTF8_String) return Gdbus_Message;
   --  Creates a new Glib.GDBusMessage.Gdbus_Message for a signal emission.
   --  Since: gtk+ 2.26
   --  @param Path A valid object path.
   --  @param Iface A valid D-Bus interface name.
   --  @param Signal A valid signal name.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_dbus_message_get_type");

   -------------
   -- Methods --
   -------------

   function Copy
      (Self  : not null access Gdbus_Message_Record;
       Error : out Glib.Error.GError) return Gdbus_Message;
   --  Copies Message. The copy is a deep copy and the returned
   --  Glib.GDBusMessage.Gdbus_Message is completely identical except that it
   --  is guaranteed to not be locked.
   --  This operation can fail if e.g. Message contains file descriptors and
   --  the per-process or system-wide open files limit is reached.
   --  Since: gtk+ 2.26
   --  @param Error the return location for a recoverable error
   --  @return A new Glib.GDBusMessage.Gdbus_Message or null if Error is set.
   --  Free with g_object_unref. Has transfer-ownership='full'.

   function Get_Arg0
      (Self : not null access Gdbus_Message_Record) return UTF8_String;
   --  Convenience to get the first item in the body of Message.
   --  Since: gtk+ 2.26
   --  @return The string item or null if the first item in the body of
   --  Message is not a string.

   function Get_Arg0_Path
      (Self : not null access Gdbus_Message_Record) return UTF8_String;

   function Get_Body
      (Self : not null access Gdbus_Message_Record)
       return Glib.Variant.Gvariant;
   --  Gets the body of a message.
   --  Since: gtk+ 2.26
   --  @return A Glib.Variant.Gvariant or null if the body is empty. Do not
   --  free, it is owned by Message. Has transfer-ownership='none'.

   procedure Set_Body
      (Self     : not null access Gdbus_Message_Record;
       The_Body : Glib.Variant.Gvariant);
   --  Sets the body Message. As a side-effect the Glib.GEnums.Field_Signature
   --  header field is set to the type string of Body (or cleared if Body is
   --  null).
   --  If Body is floating, Message assumes ownership of Body.
   --  Since: gtk+ 2.26
   --  @param The_Body Either null or a Glib.Variant.Gvariant that is a tuple.

   function Get_Byte_Order
      (Self : not null access Gdbus_Message_Record)
       return Glib.GEnums.GDBus_Message_Byte_Order;
   --  Gets the byte order of Message.
   --  @return The byte order.

   procedure Set_Byte_Order
      (Self       : not null access Gdbus_Message_Record;
       Byte_Order : Glib.GEnums.GDBus_Message_Byte_Order);
   --  Sets the byte order of Message.
   --  @param Byte_Order The byte order.

   function Get_Destination
      (Self : not null access Gdbus_Message_Record) return UTF8_String;
   --  Convenience getter for the Glib.GEnums.Field_Destination header field.
   --  Since: gtk+ 2.26
   --  @return The value.

   procedure Set_Destination
      (Self  : not null access Gdbus_Message_Record;
       Value : UTF8_String := "");
   --  Convenience setter for the Glib.GEnums.Field_Destination header field.
   --  Since: gtk+ 2.26
   --  @param Value The value to set.

   function Get_Error_Name
      (Self : not null access Gdbus_Message_Record) return UTF8_String;
   --  Convenience getter for the Glib.GEnums.Field_Error_Name header field.
   --  Since: gtk+ 2.26
   --  @return The value.

   procedure Set_Error_Name
      (Self  : access Gdbus_Message_Record'Class;
       Value : UTF8_String);
   --  Convenience setter for the Glib.GEnums.Field_Error_Name header field.
   --  Since: gtk+ 2.26
   --  @param Value The value to set.

   function Get_Flags
      (Self : not null access Gdbus_Message_Record)
       return Glib.GEnums.GDBus_Message_Flags;
   --  Gets the flags for Message.
   --  Since: gtk+ 2.26
   --  @return Flags that are set (typically values from the
   --  Glib.GEnums.GDBus_Message_Flags enumeration bitwise ORed together).

   procedure Set_Flags
      (Self  : not null access Gdbus_Message_Record;
       Flags : Glib.GEnums.GDBus_Message_Flags);
   --  Sets the flags to set on Message.
   --  Since: gtk+ 2.26
   --  @param Flags Flags for Message that are set (typically values from the
   --  Glib.GEnums.GDBus_Message_Flags enumeration bitwise ORed together).

   function Get_Header
      (Self         : not null access Gdbus_Message_Record;
       Header_Field : Glib.GEnums.GDBus_Message_Header_Field)
       return Glib.Variant.Gvariant;
   --  Gets a header field on Message.
   --  The caller is responsible for checking the type of the returned
   --  Glib.Variant.Gvariant matches what is expected.
   --  Since: gtk+ 2.26
   --  @param Header_Field A 8-bit unsigned integer (typically a value from
   --  the Glib.GEnums.GDBus_Message_Header_Field enumeration)
   --  @return A Glib.Variant.Gvariant with the value if the header was found,
   --  null otherwise. Do not free, it is owned by Message. Has
   --  transfer-ownership='none'.

   procedure Set_Header
      (Self         : not null access Gdbus_Message_Record;
       Header_Field : Glib.GEnums.GDBus_Message_Header_Field;
       Value        : Glib.Variant.Gvariant);
   --  Sets a header field on Message.
   --  If Value is floating, Message assumes ownership of Value.
   --  Since: gtk+ 2.26
   --  @param Header_Field A 8-bit unsigned integer (typically a value from
   --  the Glib.GEnums.GDBus_Message_Header_Field enumeration)
   --  @param Value A Glib.Variant.Gvariant to set the header field or null to
   --  clear the header field.

   function Get_Header_Fields
      (Self : not null access Gdbus_Message_Record) return Guint8_Array;
   --  Gets an array of all header fields on Message that are set.
   --  Since: gtk+ 2.26
   --  @return An array of header fields terminated by
   --  Glib.GEnums.Field_Invalid. Each element is a Guchar. Free with g_free.

   function Get_Interface
      (Self : not null access Gdbus_Message_Record) return UTF8_String;
   --  Convenience getter for the Glib.GEnums.Field_Interface header field.
   --  Since: gtk+ 2.26
   --  @return The value.

   procedure Set_Interface
      (Self  : not null access Gdbus_Message_Record;
       Value : UTF8_String := "");
   --  Convenience setter for the Glib.GEnums.Field_Interface header field.
   --  Since: gtk+ 2.26
   --  @param Value The value to set.

   function Get_Locked
      (Self : not null access Gdbus_Message_Record) return Boolean;
   --  Checks whether Message is locked. To monitor changes to this value,
   --  conncet to the Glib.Object.GObject::notify signal to listen for changes
   --  on the Glib.GDBusMessage.Gdbus_Message:locked property.
   --  Since: gtk+ 2.26
   --  @return True if Message is locked, False otherwise.

   function Get_Member
      (Self : not null access Gdbus_Message_Record) return UTF8_String;
   --  Convenience getter for the Glib.GEnums.Field_Member header field.
   --  Since: gtk+ 2.26
   --  @return The value.

   procedure Set_Member
      (Self  : not null access Gdbus_Message_Record;
       Value : UTF8_String := "");
   --  Convenience setter for the Glib.GEnums.Field_Member header field.
   --  Since: gtk+ 2.26
   --  @param Value The value to set.

   function Get_Message_Type
      (Self : not null access Gdbus_Message_Record)
       return Glib.GEnums.GDBus_Message_Type;
   --  Gets the type of Message.
   --  Since: gtk+ 2.26
   --  @return A 8-bit unsigned integer (typically a value from the
   --  Glib.GEnums.GDBus_Message_Type enumeration).

   procedure Set_Message_Type
      (Self     : not null access Gdbus_Message_Record;
       The_Type : Glib.GEnums.GDBus_Message_Type);
   --  Sets Message to be of Type.
   --  Since: gtk+ 2.26
   --  @param The_Type A 8-bit unsigned integer (typically a value from the
   --  Glib.GEnums.GDBus_Message_Type enumeration).

   function Get_Num_Unix_Fds
      (Self : not null access Gdbus_Message_Record) return Guint32;
   --  Convenience getter for the Glib.GEnums.Field_Num_Unix_Fds header field.
   --  Since: gtk+ 2.26
   --  @return The value.

   procedure Set_Num_Unix_Fds
      (Self  : not null access Gdbus_Message_Record;
       Value : Guint32);
   --  Convenience setter for the Glib.GEnums.Field_Num_Unix_Fds header field.
   --  Since: gtk+ 2.26
   --  @param Value The value to set.

   function Get_Path
      (Self : not null access Gdbus_Message_Record) return UTF8_String;
   --  Convenience getter for the Glib.GEnums.Field_Path header field.
   --  Since: gtk+ 2.26
   --  @return The value.

   procedure Set_Path
      (Self  : not null access Gdbus_Message_Record;
       Value : UTF8_String := "");
   --  Convenience setter for the Glib.GEnums.Field_Path header field.
   --  Since: gtk+ 2.26
   --  @param Value The value to set.

   function Get_Reply_Serial
      (Self : not null access Gdbus_Message_Record) return Guint32;
   --  Convenience getter for the Glib.GEnums.Field_Reply_Serial header field.
   --  Since: gtk+ 2.26
   --  @return The value.

   procedure Set_Reply_Serial
      (Self  : not null access Gdbus_Message_Record;
       Value : Guint32);
   --  Convenience setter for the Glib.GEnums.Field_Reply_Serial header field.
   --  Since: gtk+ 2.26
   --  @param Value The value to set.

   function Get_Sender
      (Self : not null access Gdbus_Message_Record) return UTF8_String;
   --  Convenience getter for the Glib.GEnums.Field_Sender header field.
   --  Since: gtk+ 2.26
   --  @return The value.

   procedure Set_Sender
      (Self  : not null access Gdbus_Message_Record;
       Value : UTF8_String := "");
   --  Convenience setter for the Glib.GEnums.Field_Sender header field.
   --  Since: gtk+ 2.26
   --  @param Value The value to set.

   function Get_Serial
      (Self : not null access Gdbus_Message_Record) return Guint32;
   --  Gets the serial for Message.
   --  Since: gtk+ 2.26
   --  @return A Guint32.

   procedure Set_Serial
      (Self   : not null access Gdbus_Message_Record;
       Serial : Guint32);
   --  Sets the serial for Message.
   --  Since: gtk+ 2.26
   --  @param Serial A Guint32.

   function Get_Signature
      (Self : not null access Gdbus_Message_Record) return UTF8_String;
   --  Convenience getter for the Glib.GEnums.Field_Signature header field.
   --  This will always be non-null, but may be an empty string.
   --  Since: gtk+ 2.26
   --  @return The value.

   procedure Set_Signature
      (Self  : not null access Gdbus_Message_Record;
       Value : UTF8_String := "");
   --  Convenience setter for the Glib.GEnums.Field_Signature header field.
   --  Since: gtk+ 2.26
   --  @param Value The value to set.

   function Get_Unix_Fd_List
      (Self : not null access Gdbus_Message_Record)
       return Glib.GUnixFDList.Gunix_Fdlist;
   --  Gets the UNIX file descriptors associated with Message, if any.
   --  This method is only available on UNIX.
   --  The file descriptors normally correspond to G_VARIANT_TYPE_HANDLE
   --  values in the body of the message. For example, if
   --  Glib.Variant.Get_Handle returns 5, that is intended to be a reference to
   --  the file descriptor that can be accessed by `g_unix_fd_list_get (list,
   --  5, ...)`.
   --  Since: gtk+ 2.26
   --  @return A Glib.GUnixFDList.Gunix_Fdlist or null if no file descriptors
   --  are associated. Do not free, this object is owned by Message. Has
   --  transfer-ownership='none'.

   procedure Set_Unix_Fd_List
      (Self    : not null access Gdbus_Message_Record;
       Fd_List : access Glib.GUnixFDList.Gunix_Fdlist_Record'Class);
   --  Sets the UNIX file descriptors associated with Message. As a
   --  side-effect the Glib.GEnums.Field_Num_Unix_Fds header field is set to
   --  the number of fds in Fd_List (or cleared if Fd_List is null).
   --  This method is only available on UNIX.
   --  When designing D-Bus APIs that are intended to be interoperable, please
   --  note that non-GDBus implementations of D-Bus can usually only access
   --  file descriptors if they are referenced by a value of type
   --  G_VARIANT_TYPE_HANDLE in the body of the message.
   --  Since: gtk+ 2.26
   --  @param Fd_List A Glib.GUnixFDList.Gunix_Fdlist or null.

   procedure Lock (Self : not null access Gdbus_Message_Record);
   --  If Message is locked, does nothing. Otherwise locks the message.
   --  Since: gtk+ 2.26

   function New_Method_Error_Literal
      (Self          : not null access Gdbus_Message_Record;
       Error_Name    : UTF8_String;
       Error_Message : UTF8_String) return Gdbus_Message;
   --  Creates a new Glib.GDBusMessage.Gdbus_Message that is an error reply to
   --  Method_Call_Message.
   --  Since: gtk+ 2.26
   --  @param Error_Name A valid D-Bus error name.
   --  @param Error_Message The D-Bus error message.
   --  @return A Glib.GDBusMessage.Gdbus_Message. Free with g_object_unref.
   --  Has transfer-ownership='full'.

   function New_Method_Reply
      (Self : not null access Gdbus_Message_Record) return Gdbus_Message;
   --  Creates a new Glib.GDBusMessage.Gdbus_Message that is a reply to
   --  Method_Call_Message.
   --  Since: gtk+ 2.26
   --  @return Glib.GDBusMessage.Gdbus_Message. Free with g_object_unref. Has
   --  transfer-ownership='full'.

   function Print
      (Self   : not null access Gdbus_Message_Record;
       Indent : Guint) return UTF8_String;
   --  Produces a human-readable multi-line description of Message.
   --  The contents of the description has no ABI guarantees, the contents and
   --  formatting is subject to change at any time. Typical output looks
   --  something like this:
   --
   --     Flags:   none
   --     Version: 0
   --     Serial:  4
   --     Headers:
   --       path -> objectpath '/org/gtk/GDBus/TestObject'
   --       interface -> 'org.gtk.GDBus.TestInterface'
   --       member -> 'GimmeStdout'
   --       destination -> ':1.146'
   --     Body: ()
   --     UNIX File Descriptors:
   --       (none)
   --
   --  or
   --
   --     Flags:   no-reply-expected
   --     Version: 0
   --     Serial:  477
   --     Headers:
   --       reply-serial -> uint32 4
   --       destination -> ':1.159'
   --       sender -> ':1.146'
   --       num-unix-fds -> uint32 1
   --     Body: ()
   --     UNIX File Descriptors:
   --       fd 12: dev=0:10,mode=020620,ino=5,uid=500,gid=5,rdev=136:2,size=0,atime=1273085037,mtime=1273085851,ctime=1272982635
   --
   --  Since: gtk+ 2.26
   --  @param Indent Indentation level.
   --  @return A string that should be freed with g_free.

   function To_Blob
      (Self         : not null access Gdbus_Message_Record;
       Capabilities : Glib.GEnums.GDBus_Capability_Flags;
       Error        : out Glib.Error.GError) return Guint8_Array;
   --  Serializes Message to a blob. The byte order returned by
   --  Glib.GDBusMessage.Get_Byte_Order will be used.
   --  Since: gtk+ 2.26
   --  @param Capabilities A Glib.GEnums.GDBus_Capability_Flags describing
   --  what protocol features are supported.
   --  @param Error the return location for a recoverable error
   --  @return A pointer to a valid binary D-Bus message of Out_Size bytes
   --  generated by Message or null if Error is set.

   function To_Gerror
      (Self  : not null access Gdbus_Message_Record;
       Error : out Glib.Error.GError) return Boolean;
   --  If Message is not of type Glib.GEnums.Error does nothing and returns
   --  False.
   --  Otherwise this method encodes the error in Message as a Gerror.Gerror
   --  using g_dbus_error_set_dbus_error using the information in the
   --  Glib.GEnums.Field_Error_Name header field of Message as well as the
   --  first string item in Message's body.
   --  Since: gtk+ 2.26
   --  @param Error the return location for a recoverable error
   --  @return True if Error was set, False otherwise.

   ---------------
   -- Functions --
   ---------------

   function Bytes_Needed
      (Blob     : Guint8_Array;
       Blob_Len : Gsize;
       Error    : out Glib.Error.GError) return Gssize;
   --  Utility function to calculate how many bytes are needed to completely
   --  deserialize the D-Bus message stored at Blob.
   --  Since: gtk+ 2.26
   --  @param Blob A blob representing a binary D-Bus message.
   --  @param Blob_Len The length of Blob (must be at least 16).
   --  @param Error the return location for a recoverable error
   --  @return Number of bytes needed or -1 if Error is set (e.g. if Blob
   --  contains invalid data or not enough data is available to determine the
   --  size).

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Locked_Property : constant Glib.Properties.Property_Boolean;

private
   Locked_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("locked");
end Glib.GDBusMessage;
