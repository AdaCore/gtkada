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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Error;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Pointers;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

use type Glib.Error.GError;

package body Glib.GDBusMessage is

   use Interfaces.C;
   package P is new Interfaces.C.Pointers
     (Natural, Guint8, Guint8_Array, 0);

   package Type_Conversion_Gdbus_Message is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdbus_Message_Record);
   pragma Unreferenced (Type_Conversion_Gdbus_Message);

   -----------
   -- G_New --
   -----------

   procedure G_New (Self : out Gdbus_Message) is
   begin
      Self := new Gdbus_Message_Record;
      Glib.GDBusMessage.Initialize (Self);
   end G_New;

   ---------------------
   -- G_New_From_Blob --
   ---------------------

   procedure G_New_From_Blob
      (Self         : out Gdbus_Message;
       Blob         : Guint8_Array;
       Blob_Len     : Gsize;
       Capabilities : Glib.GEnums.GDBus_Capability_Flags;
       Error        : out Glib.Error.GError)
   is
   begin
      Self := new Gdbus_Message_Record;
      Glib.GDBusMessage.Initialize_From_Blob (Self, Blob, Blob_Len, Capabilities, Error);
      if Error /= null then
         Self.Deallocate;
         Self := null;
      end if;
   end G_New_From_Blob;

   -----------------------
   -- G_New_Method_Call --
   -----------------------

   procedure G_New_Method_Call
      (Self   : out Gdbus_Message;
       Name   : UTF8_String := "";
       Path   : UTF8_String;
       Iface  : UTF8_String := "";
       Method : UTF8_String)
   is
   begin
      Self := new Gdbus_Message_Record;
      Glib.GDBusMessage.Initialize_Method_Call (Self, Name, Path, Iface, Method);
   end G_New_Method_Call;

   ------------------
   -- G_New_Signal --
   ------------------

   procedure G_New_Signal
      (Self   : out Gdbus_Message;
       Path   : UTF8_String;
       Iface  : UTF8_String;
       Signal : UTF8_String)
   is
   begin
      Self := new Gdbus_Message_Record;
      Glib.GDBusMessage.Initialize_Signal (Self, Path, Iface, Signal);
   end G_New_Signal;

   -----------------------
   -- Gdbus_Message_New --
   -----------------------

   function Gdbus_Message_New return Gdbus_Message is
      Self : constant Gdbus_Message := new Gdbus_Message_Record;
   begin
      Glib.GDBusMessage.Initialize (Self);
      return Self;
   end Gdbus_Message_New;

   ---------------------------------
   -- Gdbus_Message_New_From_Blob --
   ---------------------------------

   function Gdbus_Message_New_From_Blob
      (Blob         : Guint8_Array;
       Blob_Len     : Gsize;
       Capabilities : Glib.GEnums.GDBus_Capability_Flags;
       Error        : out Glib.Error.GError) return Gdbus_Message
   is
      Self : Gdbus_Message := new Gdbus_Message_Record;
   begin
      Glib.GDBusMessage.Initialize_From_Blob (Self, Blob, Blob_Len, Capabilities, Error);
      if Error /= null then
         Self.Deallocate;
         Self := null;
      end if;
      return Self;
   end Gdbus_Message_New_From_Blob;

   -----------------------------------
   -- Gdbus_Message_New_Method_Call --
   -----------------------------------

   function Gdbus_Message_New_Method_Call
      (Name   : UTF8_String := "";
       Path   : UTF8_String;
       Iface  : UTF8_String := "";
       Method : UTF8_String) return Gdbus_Message
   is
      Self : constant Gdbus_Message := new Gdbus_Message_Record;
   begin
      Glib.GDBusMessage.Initialize_Method_Call (Self, Name, Path, Iface, Method);
      return Self;
   end Gdbus_Message_New_Method_Call;

   ------------------------------
   -- Gdbus_Message_New_Signal --
   ------------------------------

   function Gdbus_Message_New_Signal
      (Path   : UTF8_String;
       Iface  : UTF8_String;
       Signal : UTF8_String) return Gdbus_Message
   is
      Self : constant Gdbus_Message := new Gdbus_Message_Record;
   begin
      Glib.GDBusMessage.Initialize_Signal (Self, Path, Iface, Signal);
      return Self;
   end Gdbus_Message_New_Signal;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gdbus_Message_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_dbus_message_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   --------------------------
   -- Initialize_From_Blob --
   --------------------------

   procedure Initialize_From_Blob
      (Self         : not null access Gdbus_Message_Record'Class;
       Blob         : Guint8_Array;
       Blob_Len     : Gsize;
       Capabilities : Glib.GEnums.GDBus_Capability_Flags;
       Error        : out Glib.Error.GError)
   is
      function Internal
         (Blob         : System.Address;
          Blob_Len     : Gsize;
          Capabilities : Glib.GEnums.GDBus_Capability_Flags;
          Acc_Error    : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "g_dbus_message_new_from_blob");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : System.Address;
   begin
      Error := null;
      if not Self.Is_Created then
         Tmp_Return := Internal (Blob'Address, Blob_Len, Capabilities, Acc_Error'Access);
         Error := Acc_Error;
         if Error = null then
            Set_Object (Self, Tmp_Return);
         end if;
      end if;
   end Initialize_From_Blob;

   ----------------------------
   -- Initialize_Method_Call --
   ----------------------------

   procedure Initialize_Method_Call
      (Self   : not null access Gdbus_Message_Record'Class;
       Name   : UTF8_String := "";
       Path   : UTF8_String;
       Iface  : UTF8_String := "";
       Method : UTF8_String)
   is
      function Internal
         (Name   : Gtkada.Types.Chars_Ptr;
          Path   : Gtkada.Types.Chars_Ptr;
          Iface  : Gtkada.Types.Chars_Ptr;
          Method : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_dbus_message_new_method_call");
      Tmp_Name   : Gtkada.Types.Chars_Ptr;
      Tmp_Path   : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Iface  : Gtkada.Types.Chars_Ptr;
      Tmp_Method : Gtkada.Types.Chars_Ptr := New_String (Method);
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Name :=
           (if Name = ""
            then Gtkada.Types.Null_Ptr
            else New_String (Name));
         Tmp_Iface :=
           (if Iface = ""
            then Gtkada.Types.Null_Ptr
            else New_String (Iface));
         Tmp_Return := Internal (Tmp_Name, Tmp_Path, Tmp_Iface, Tmp_Method);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Method);
      Free (Tmp_Iface);
      Free (Tmp_Path);
      Free (Tmp_Name);
   end Initialize_Method_Call;

   -----------------------
   -- Initialize_Signal --
   -----------------------

   procedure Initialize_Signal
      (Self   : not null access Gdbus_Message_Record'Class;
       Path   : UTF8_String;
       Iface  : UTF8_String;
       Signal : UTF8_String)
   is
      function Internal
         (Path   : Gtkada.Types.Chars_Ptr;
          Iface  : Gtkada.Types.Chars_Ptr;
          Signal : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_dbus_message_new_signal");
      Tmp_Path   : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Iface  : Gtkada.Types.Chars_Ptr := New_String (Iface);
      Tmp_Signal : Gtkada.Types.Chars_Ptr := New_String (Signal);
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Path, Tmp_Iface, Tmp_Signal);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Signal);
      Free (Tmp_Iface);
      Free (Tmp_Path);
   end Initialize_Signal;

   ----------
   -- Copy --
   ----------

   function Copy
      (Self  : not null access Gdbus_Message_Record;
       Error : out Glib.Error.GError) return Gdbus_Message
   is
      function Internal
         (Self      : System.Address;
          Acc_Error : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "g_dbus_message_copy");
      Acc_Error          : aliased Glib.Error.GError;
      Return_Obj         : Gdbus_Message;
      Stub_Gdbus_Message : Gdbus_Message_Record;
      Tmp_Return         : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Error'Access);
      Error := Acc_Error;
      if Error = null then
         Return_Obj := Glib.GDBusMessage.Gdbus_Message (Get_User_Data (Tmp_Return, Stub_Gdbus_Message));
      end if;
      return Return_Obj;
   end Copy;

   --------------
   -- Get_Arg0 --
   --------------

   function Get_Arg0
      (Self : not null access Gdbus_Message_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_dbus_message_get_arg0");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Arg0;

   -------------------
   -- Get_Arg0_Path --
   -------------------

   function Get_Arg0_Path
      (Self : not null access Gdbus_Message_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_dbus_message_get_arg0_path");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Arg0_Path;

   --------------
   -- Get_Body --
   --------------

   function Get_Body
      (Self : not null access Gdbus_Message_Record)
       return Glib.Variant.Gvariant
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_dbus_message_get_body");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Body;

   --------------------
   -- Get_Byte_Order --
   --------------------

   function Get_Byte_Order
      (Self : not null access Gdbus_Message_Record)
       return Glib.GEnums.GDBus_Message_Byte_Order
   is
      function Internal
         (Self : System.Address) return Glib.GEnums.GDBus_Message_Byte_Order;
      pragma Import (C, Internal, "g_dbus_message_get_byte_order");
   begin
      return Internal (Get_Object (Self));
   end Get_Byte_Order;

   ---------------------
   -- Get_Destination --
   ---------------------

   function Get_Destination
      (Self : not null access Gdbus_Message_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_dbus_message_get_destination");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Destination;

   --------------------
   -- Get_Error_Name --
   --------------------

   function Get_Error_Name
      (Self : not null access Gdbus_Message_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_dbus_message_get_error_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Error_Name;

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags
      (Self : not null access Gdbus_Message_Record)
       return Glib.GEnums.GDBus_Message_Flags
   is
      function Internal
         (Self : System.Address) return Glib.GEnums.GDBus_Message_Flags;
      pragma Import (C, Internal, "g_dbus_message_get_flags");
   begin
      return Internal (Get_Object (Self));
   end Get_Flags;

   ----------------
   -- Get_Header --
   ----------------

   function Get_Header
      (Self         : not null access Gdbus_Message_Record;
       Header_Field : Glib.GEnums.GDBus_Message_Header_Field)
       return Glib.Variant.Gvariant
   is
      function Internal
         (Self         : System.Address;
          Header_Field : Glib.GEnums.GDBus_Message_Header_Field)
          return System.Address;
      pragma Import (C, Internal, "g_dbus_message_get_header");
   begin
      return From_Object (Internal (Get_Object (Self), Header_Field));
   end Get_Header;

   -----------------------
   -- Get_Header_Fields --
   -----------------------

   function Get_Header_Fields
      (Self : not null access Gdbus_Message_Record) return Guint8_Array
   is
      function Internal (Self : System.Address) return P.Pointer;
      pragma Import (C, Internal, "g_dbus_message_get_header_fields");
      Acc_Result : P.Pointer := null;
   begin
      Acc_Result := Internal (Get_Object (Self));
      declare
         Result : constant GLib.Guint8_Array :=
           P.Value
             (Acc_Result,
              Terminator =>
                Guint8 (GDBus_Message_Header_Field'Pos (Field_Invalid)));
      begin
         Glib.g_free (Acc_Result.all'Address); --  transfer-ownership="full"
         return Result;
      end;
   end Get_Header_Fields;

   -------------------
   -- Get_Interface --
   -------------------

   function Get_Interface
      (Self : not null access Gdbus_Message_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_dbus_message_get_interface");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Interface;

   ----------------
   -- Get_Locked --
   ----------------

   function Get_Locked
      (Self : not null access Gdbus_Message_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_dbus_message_get_locked");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Locked;

   ----------------
   -- Get_Member --
   ----------------

   function Get_Member
      (Self : not null access Gdbus_Message_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_dbus_message_get_member");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Member;

   ----------------------
   -- Get_Message_Type --
   ----------------------

   function Get_Message_Type
      (Self : not null access Gdbus_Message_Record)
       return Glib.GEnums.GDBus_Message_Type
   is
      function Internal
         (Self : System.Address) return Glib.GEnums.GDBus_Message_Type;
      pragma Import (C, Internal, "g_dbus_message_get_message_type");
   begin
      return Internal (Get_Object (Self));
   end Get_Message_Type;

   ----------------------
   -- Get_Num_Unix_Fds --
   ----------------------

   function Get_Num_Unix_Fds
      (Self : not null access Gdbus_Message_Record) return Guint32
   is
      function Internal (Self : System.Address) return Guint32;
      pragma Import (C, Internal, "g_dbus_message_get_num_unix_fds");
   begin
      return Internal (Get_Object (Self));
   end Get_Num_Unix_Fds;

   --------------
   -- Get_Path --
   --------------

   function Get_Path
      (Self : not null access Gdbus_Message_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_dbus_message_get_path");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Path;

   ----------------------
   -- Get_Reply_Serial --
   ----------------------

   function Get_Reply_Serial
      (Self : not null access Gdbus_Message_Record) return Guint32
   is
      function Internal (Self : System.Address) return Guint32;
      pragma Import (C, Internal, "g_dbus_message_get_reply_serial");
   begin
      return Internal (Get_Object (Self));
   end Get_Reply_Serial;

   ----------------
   -- Get_Sender --
   ----------------

   function Get_Sender
      (Self : not null access Gdbus_Message_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_dbus_message_get_sender");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Sender;

   ----------------
   -- Get_Serial --
   ----------------

   function Get_Serial
      (Self : not null access Gdbus_Message_Record) return Guint32
   is
      function Internal (Self : System.Address) return Guint32;
      pragma Import (C, Internal, "g_dbus_message_get_serial");
   begin
      return Internal (Get_Object (Self));
   end Get_Serial;

   -------------------
   -- Get_Signature --
   -------------------

   function Get_Signature
      (Self : not null access Gdbus_Message_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_dbus_message_get_signature");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Signature;

   ----------------------
   -- Get_Unix_Fd_List --
   ----------------------

   function Get_Unix_Fd_List
      (Self : not null access Gdbus_Message_Record)
       return Glib.GUnixFDList.Gunix_Fdlist
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_dbus_message_get_unix_fd_list");
      Stub_Gunix_Fdlist : Glib.GUnixFDList.Gunix_Fdlist_Record;
   begin
      return Glib.GUnixFDList.Gunix_Fdlist (Get_User_Data (Internal (Get_Object (Self)), Stub_Gunix_Fdlist));
   end Get_Unix_Fd_List;

   ----------
   -- Lock --
   ----------

   procedure Lock (Self : not null access Gdbus_Message_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_dbus_message_lock");
   begin
      Internal (Get_Object (Self));
   end Lock;

   ------------------------------
   -- New_Method_Error_Literal --
   ------------------------------

   function New_Method_Error_Literal
      (Self          : not null access Gdbus_Message_Record;
       Error_Name    : UTF8_String;
       Error_Message : UTF8_String) return Gdbus_Message
   is
      function Internal
         (Self          : System.Address;
          Error_Name    : Gtkada.Types.Chars_Ptr;
          Error_Message : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_dbus_message_new_method_error_literal");
      Tmp_Error_Name     : Gtkada.Types.Chars_Ptr := New_String (Error_Name);
      Tmp_Error_Message  : Gtkada.Types.Chars_Ptr := New_String (Error_Message);
      Stub_Gdbus_Message : Gdbus_Message_Record;
      Tmp_Return         : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Error_Name, Tmp_Error_Message);
      Free (Tmp_Error_Message);
      Free (Tmp_Error_Name);
      return Glib.GDBusMessage.Gdbus_Message (Get_User_Data (Tmp_Return, Stub_Gdbus_Message));
   end New_Method_Error_Literal;

   ----------------------
   -- New_Method_Reply --
   ----------------------

   function New_Method_Reply
      (Self : not null access Gdbus_Message_Record) return Gdbus_Message
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_dbus_message_new_method_reply");
      Stub_Gdbus_Message : Gdbus_Message_Record;
   begin
      return Glib.GDBusMessage.Gdbus_Message (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdbus_Message));
   end New_Method_Reply;

   -----------
   -- Print --
   -----------

   function Print
      (Self   : not null access Gdbus_Message_Record;
       Indent : Guint) return UTF8_String
   is
      function Internal
         (Self   : System.Address;
          Indent : Guint) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_dbus_message_print");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self), Indent));
   end Print;

   --------------
   -- Set_Body --
   --------------

   procedure Set_Body
      (Self     : not null access Gdbus_Message_Record;
       The_Body : Glib.Variant.Gvariant)
   is
      procedure Internal (Self : System.Address; The_Body : System.Address);
      pragma Import (C, Internal, "g_dbus_message_set_body");
   begin
      Internal (Get_Object (Self), Get_Object (The_Body));
   end Set_Body;

   --------------------
   -- Set_Byte_Order --
   --------------------

   procedure Set_Byte_Order
      (Self       : not null access Gdbus_Message_Record;
       Byte_Order : Glib.GEnums.GDBus_Message_Byte_Order)
   is
      procedure Internal
         (Self       : System.Address;
          Byte_Order : Glib.GEnums.GDBus_Message_Byte_Order);
      pragma Import (C, Internal, "g_dbus_message_set_byte_order");
   begin
      Internal (Get_Object (Self), Byte_Order);
   end Set_Byte_Order;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
      (Self  : not null access Gdbus_Message_Record;
       Value : UTF8_String := "")
   is
      procedure Internal
         (Self  : System.Address;
          Value : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_dbus_message_set_destination");
      Tmp_Value : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Value :=
        (if Value = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Value));
      Internal (Get_Object (Self), Tmp_Value);
      Free (Tmp_Value);
   end Set_Destination;

   --------------------
   -- Set_Error_Name --
   --------------------

   procedure Set_Error_Name
      (Self  : access Gdbus_Message_Record'Class;
       Value : UTF8_String)
   is
      procedure Internal
         (Self  : System.Address;
          Value : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_dbus_message_set_error_name");
      Tmp_Value : Gtkada.Types.Chars_Ptr := New_String (Value);
   begin
      Internal (Get_Object_Or_Null (GObject (Self)), Tmp_Value);
      Free (Tmp_Value);
   end Set_Error_Name;

   ---------------
   -- Set_Flags --
   ---------------

   procedure Set_Flags
      (Self  : not null access Gdbus_Message_Record;
       Flags : Glib.GEnums.GDBus_Message_Flags)
   is
      procedure Internal
         (Self  : System.Address;
          Flags : Glib.GEnums.GDBus_Message_Flags);
      pragma Import (C, Internal, "g_dbus_message_set_flags");
   begin
      Internal (Get_Object (Self), Flags);
   end Set_Flags;

   ----------------
   -- Set_Header --
   ----------------

   procedure Set_Header
      (Self         : not null access Gdbus_Message_Record;
       Header_Field : Glib.GEnums.GDBus_Message_Header_Field;
       Value        : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self         : System.Address;
          Header_Field : Glib.GEnums.GDBus_Message_Header_Field;
          Value        : System.Address);
      pragma Import (C, Internal, "g_dbus_message_set_header");
   begin
      Internal (Get_Object (Self), Header_Field, Get_Object (Value));
   end Set_Header;

   -------------------
   -- Set_Interface --
   -------------------

   procedure Set_Interface
      (Self  : not null access Gdbus_Message_Record;
       Value : UTF8_String := "")
   is
      procedure Internal
         (Self  : System.Address;
          Value : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_dbus_message_set_interface");
      Tmp_Value : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Value :=
        (if Value = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Value));
      Internal (Get_Object (Self), Tmp_Value);
      Free (Tmp_Value);
   end Set_Interface;

   ----------------
   -- Set_Member --
   ----------------

   procedure Set_Member
      (Self  : not null access Gdbus_Message_Record;
       Value : UTF8_String := "")
   is
      procedure Internal
         (Self  : System.Address;
          Value : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_dbus_message_set_member");
      Tmp_Value : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Value :=
        (if Value = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Value));
      Internal (Get_Object (Self), Tmp_Value);
      Free (Tmp_Value);
   end Set_Member;

   ----------------------
   -- Set_Message_Type --
   ----------------------

   procedure Set_Message_Type
      (Self     : not null access Gdbus_Message_Record;
       The_Type : Glib.GEnums.GDBus_Message_Type)
   is
      procedure Internal
         (Self     : System.Address;
          The_Type : Glib.GEnums.GDBus_Message_Type);
      pragma Import (C, Internal, "g_dbus_message_set_message_type");
   begin
      Internal (Get_Object (Self), The_Type);
   end Set_Message_Type;

   ----------------------
   -- Set_Num_Unix_Fds --
   ----------------------

   procedure Set_Num_Unix_Fds
      (Self  : not null access Gdbus_Message_Record;
       Value : Guint32)
   is
      procedure Internal (Self : System.Address; Value : Guint32);
      pragma Import (C, Internal, "g_dbus_message_set_num_unix_fds");
   begin
      Internal (Get_Object (Self), Value);
   end Set_Num_Unix_Fds;

   --------------
   -- Set_Path --
   --------------

   procedure Set_Path
      (Self  : not null access Gdbus_Message_Record;
       Value : UTF8_String := "")
   is
      procedure Internal
         (Self  : System.Address;
          Value : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_dbus_message_set_path");
      Tmp_Value : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Value :=
        (if Value = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Value));
      Internal (Get_Object (Self), Tmp_Value);
      Free (Tmp_Value);
   end Set_Path;

   ----------------------
   -- Set_Reply_Serial --
   ----------------------

   procedure Set_Reply_Serial
      (Self  : not null access Gdbus_Message_Record;
       Value : Guint32)
   is
      procedure Internal (Self : System.Address; Value : Guint32);
      pragma Import (C, Internal, "g_dbus_message_set_reply_serial");
   begin
      Internal (Get_Object (Self), Value);
   end Set_Reply_Serial;

   ----------------
   -- Set_Sender --
   ----------------

   procedure Set_Sender
      (Self  : not null access Gdbus_Message_Record;
       Value : UTF8_String := "")
   is
      procedure Internal
         (Self  : System.Address;
          Value : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_dbus_message_set_sender");
      Tmp_Value : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Value :=
        (if Value = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Value));
      Internal (Get_Object (Self), Tmp_Value);
      Free (Tmp_Value);
   end Set_Sender;

   ----------------
   -- Set_Serial --
   ----------------

   procedure Set_Serial
      (Self   : not null access Gdbus_Message_Record;
       Serial : Guint32)
   is
      procedure Internal (Self : System.Address; Serial : Guint32);
      pragma Import (C, Internal, "g_dbus_message_set_serial");
   begin
      Internal (Get_Object (Self), Serial);
   end Set_Serial;

   -------------------
   -- Set_Signature --
   -------------------

   procedure Set_Signature
      (Self  : not null access Gdbus_Message_Record;
       Value : UTF8_String := "")
   is
      procedure Internal
         (Self  : System.Address;
          Value : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_dbus_message_set_signature");
      Tmp_Value : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Value :=
        (if Value = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Value));
      Internal (Get_Object (Self), Tmp_Value);
      Free (Tmp_Value);
   end Set_Signature;

   ----------------------
   -- Set_Unix_Fd_List --
   ----------------------

   procedure Set_Unix_Fd_List
      (Self    : not null access Gdbus_Message_Record;
       Fd_List : access Glib.GUnixFDList.Gunix_Fdlist_Record'Class)
   is
      procedure Internal (Self : System.Address; Fd_List : System.Address);
      pragma Import (C, Internal, "g_dbus_message_set_unix_fd_list");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Fd_List)));
   end Set_Unix_Fd_List;

   -------------
   -- To_Blob --
   -------------

   function To_Blob
      (Self         : not null access Gdbus_Message_Record;
       Capabilities : Glib.GEnums.GDBus_Capability_Flags;
       Error        : out Glib.Error.GError) return Guint8_Array
   is
      use P;

      function Internal
         (Self         : System.Address;
          Acc_Out_Size : access Gsize;
          Capabilities : Glib.GEnums.GDBus_Capability_Flags;
          Acc_Error    : access Glib.Error.GError) return P.Pointer;
      pragma Import (C, Internal, "g_dbus_message_to_blob");
      Acc_Out_Size : aliased Gsize;
      Acc_Error    : aliased Glib.Error.GError;
      Tmp_Return   : P.Pointer := null;
   begin
      Tmp_Return := Internal
        (Get_Object (Self), Acc_Out_Size'Access,
         Capabilities, Acc_Error'Access);

      Error := Acc_Error;
      if Tmp_Return /= null then
         declare
            Result : constant GLib.Guint8_Array :=
              P.Value (Tmp_Return, Interfaces.C.ptrdiff_t (Acc_Out_Size));
         begin
            --  transfer-ownership="full"
            Glib.g_free (Tmp_Return.all'Address);

            return Result;
         end;
      else
         return (1 .. 0 => <>);
      end if;
   end To_Blob;

   ---------------
   -- To_Gerror --
   ---------------

   function To_Gerror
      (Self  : not null access Gdbus_Message_Record;
       Error : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_dbus_message_to_gerror");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end To_Gerror;

   ------------------
   -- Bytes_Needed --
   ------------------

   function Bytes_Needed
      (Blob     : Guint8_Array;
       Blob_Len : Gsize;
       Error    : out Glib.Error.GError) return Gssize
   is
      function Internal
         (Blob      : System.Address;
          Blob_Len  : Gsize;
          Acc_Error : access Glib.Error.GError) return Gssize;
      pragma Import (C, Internal, "g_dbus_message_bytes_needed");
      Acc_Error  : aliased Glib.Error.GError;
      Return_Obj : Gssize := -1;
      Tmp_Return : Gssize;
   begin
      Tmp_Return := Internal (Blob'Address, Blob_Len, Acc_Error'Access);
      Error := Acc_Error;
      if Error = null then
         Return_Obj := Tmp_Return;
      end if;
      return Return_Obj;
   end Bytes_Needed;

end Glib.GDBusMessage;
