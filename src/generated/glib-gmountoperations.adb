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
with Ada.Unchecked_Conversion;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Glib.GMountOperations is

   ------------------
   -- Set_Password --
   ------------------

   procedure Set_Password
     (Self     : not null access Gmount_Operation_Record;
      Password : UTF8_String := "")
   is
      procedure Internal
        (Self     : System.Address;
         Password : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_mount_operation_set_password");
      Tmp_Password : Gtkada.Types.Chars_Ptr := New_String (Password);
   begin
      Internal (Get_Object (Self), Tmp_Password);
      Free (Tmp_Password);
   end Set_Password;

   package Type_Conversion_Gmount_Operation is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gmount_Operation_Record);
   pragma Unreferenced (Type_Conversion_Gmount_Operation);

   -----------
   -- G_New --
   -----------

   procedure G_New (Self : out Gmount_Operation) is
   begin
      Self := new Gmount_Operation_Record;
      Glib.GMountOperations.Initialize (Self);
   end G_New;

   --------------------------
   -- Gmount_Operation_New --
   --------------------------

   function Gmount_Operation_New return Gmount_Operation is
      Self : constant Gmount_Operation := new Gmount_Operation_Record;
   begin
      Glib.GMountOperations.Initialize (Self);
      return Self;
   end Gmount_Operation_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gmount_Operation_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_mount_operation_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -------------------
   -- Get_Anonymous --
   -------------------

   function Get_Anonymous
      (Self : not null access Gmount_Operation_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_mount_operation_get_anonymous");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Anonymous;

   ----------------
   -- Get_Choice --
   ----------------

   function Get_Choice
      (Self : not null access Gmount_Operation_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "g_mount_operation_get_choice");
   begin
      return Internal (Get_Object (Self));
   end Get_Choice;

   ----------------
   -- Get_Domain --
   ----------------

   function Get_Domain
      (Self : not null access Gmount_Operation_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_mount_operation_get_domain");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Domain;

   ---------------------------------
   -- Get_Is_Tcrypt_Hidden_Volume --
   ---------------------------------

   function Get_Is_Tcrypt_Hidden_Volume
      (Self : not null access Gmount_Operation_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_mount_operation_get_is_tcrypt_hidden_volume");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Is_Tcrypt_Hidden_Volume;

   ---------------------------------
   -- Get_Is_Tcrypt_System_Volume --
   ---------------------------------

   function Get_Is_Tcrypt_System_Volume
      (Self : not null access Gmount_Operation_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_mount_operation_get_is_tcrypt_system_volume");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Is_Tcrypt_System_Volume;

   ------------------
   -- Get_Password --
   ------------------

   function Get_Password
      (Self : not null access Gmount_Operation_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_mount_operation_get_password");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Password;

   -----------------------
   -- Get_Password_Save --
   -----------------------

   function Get_Password_Save
      (Self : not null access Gmount_Operation_Record)
       return Glib.GEnums.GPassword_Save
   is
      function Internal
         (Self : System.Address) return Glib.GEnums.GPassword_Save;
      pragma Import (C, Internal, "g_mount_operation_get_password_save");
   begin
      return Internal (Get_Object (Self));
   end Get_Password_Save;

   -------------
   -- Get_Pim --
   -------------

   function Get_Pim
      (Self : not null access Gmount_Operation_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "g_mount_operation_get_pim");
   begin
      return Internal (Get_Object (Self));
   end Get_Pim;

   ------------------
   -- Get_Username --
   ------------------

   function Get_Username
      (Self : not null access Gmount_Operation_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_mount_operation_get_username");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Username;

   -----------
   -- Reply --
   -----------

   procedure Reply
      (Self   : not null access Gmount_Operation_Record;
       Result : Glib.GEnums.GMount_Operation_Result)
   is
      procedure Internal
         (Self   : System.Address;
          Result : Glib.GEnums.GMount_Operation_Result);
      pragma Import (C, Internal, "g_mount_operation_reply");
   begin
      Internal (Get_Object (Self), Result);
   end Reply;

   -------------------
   -- Set_Anonymous --
   -------------------

   procedure Set_Anonymous
      (Self      : not null access Gmount_Operation_Record;
       Anonymous : Boolean)
   is
      procedure Internal (Self : System.Address; Anonymous : Glib.Gboolean);
      pragma Import (C, Internal, "g_mount_operation_set_anonymous");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Anonymous));
   end Set_Anonymous;

   ----------------
   -- Set_Choice --
   ----------------

   procedure Set_Choice
      (Self   : not null access Gmount_Operation_Record;
       Choice : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Choice : Glib.Gint);
      pragma Import (C, Internal, "g_mount_operation_set_choice");
   begin
      Internal (Get_Object (Self), Choice);
   end Set_Choice;

   ----------------
   -- Set_Domain --
   ----------------

   procedure Set_Domain
      (Self   : not null access Gmount_Operation_Record;
       Domain : UTF8_String := "")
   is
      procedure Internal
         (Self   : System.Address;
          Domain : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_mount_operation_set_domain");
      Tmp_Domain : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Domain :=
        (if Domain = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Domain));
      Internal (Get_Object (Self), Tmp_Domain);
      Free (Tmp_Domain);
   end Set_Domain;

   ---------------------------------
   -- Set_Is_Tcrypt_Hidden_Volume --
   ---------------------------------

   procedure Set_Is_Tcrypt_Hidden_Volume
      (Self          : not null access Gmount_Operation_Record;
       Hidden_Volume : Boolean)
   is
      procedure Internal
         (Self          : System.Address;
          Hidden_Volume : Glib.Gboolean);
      pragma Import (C, Internal, "g_mount_operation_set_is_tcrypt_hidden_volume");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Hidden_Volume));
   end Set_Is_Tcrypt_Hidden_Volume;

   ---------------------------------
   -- Set_Is_Tcrypt_System_Volume --
   ---------------------------------

   procedure Set_Is_Tcrypt_System_Volume
      (Self          : not null access Gmount_Operation_Record;
       System_Volume : Boolean)
   is
      procedure Internal
         (Self          : System.Address;
          System_Volume : Glib.Gboolean);
      pragma Import (C, Internal, "g_mount_operation_set_is_tcrypt_system_volume");
   begin
      Internal (Get_Object (Self), Boolean'Pos (System_Volume));
   end Set_Is_Tcrypt_System_Volume;

   -----------------------
   -- Set_Password_Save --
   -----------------------

   procedure Set_Password_Save
      (Self : not null access Gmount_Operation_Record;
       Save : Glib.GEnums.GPassword_Save)
   is
      procedure Internal
         (Self : System.Address;
          Save : Glib.GEnums.GPassword_Save);
      pragma Import (C, Internal, "g_mount_operation_set_password_save");
   begin
      Internal (Get_Object (Self), Save);
   end Set_Password_Save;

   -------------
   -- Set_Pim --
   -------------

   procedure Set_Pim
      (Self : not null access Gmount_Operation_Record;
       Pim  : Guint)
   is
      procedure Internal (Self : System.Address; Pim : Guint);
      pragma Import (C, Internal, "g_mount_operation_set_pim");
   begin
      Internal (Get_Object (Self), Pim);
   end Set_Pim;

   ------------------
   -- Set_Username --
   ------------------

   procedure Set_Username
      (Self     : not null access Gmount_Operation_Record;
       Username : UTF8_String := "")
   is
      procedure Internal
         (Self     : System.Address;
          Username : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_mount_operation_set_username");
      Tmp_Username : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Username :=
        (if Username = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Username));
      Internal (Get_Object (Self), Tmp_Username);
      Free (Tmp_Username);
   end Set_Username;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gmount_Operation_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gmount_Operation_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gmount_Operation_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gmount_Operation_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gmount_Operation_UTF8_String_chars_ptr_array_access_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gmount_Operation_UTF8_String_chars_ptr_array_access_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_chars_ptr_array_access_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_chars_ptr_array_access_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gmount_Operation_GMount_Operation_Result_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gmount_Operation_GMount_Operation_Result_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_GMount_Operation_Result_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_GMount_Operation_Result_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gmount_Operation_UTF8_String_Garray_Access_chars_ptr_array_access_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gmount_Operation_UTF8_String_Garray_Access_chars_ptr_array_access_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Garray_Access_chars_ptr_array_access_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Garray_Access_chars_ptr_array_access_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gmount_Operation_UTF8_String_Gint64_Gint64_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gmount_Operation_UTF8_String_Gint64_Gint64_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Gint64_Gint64_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Gint64_Gint64_Void);

   procedure Connect
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gmount_Operation_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gmount_Operation_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gmount_Operation_UTF8_String_chars_ptr_array_access_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gmount_Operation_GMount_Operation_Result_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gmount_Operation_UTF8_String_Garray_Access_chars_ptr_array_access_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gmount_Operation_UTF8_String_Gint64_Gint64_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_chars_ptr_array_access_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_GMount_Operation_Result_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Garray_Access_chars_ptr_array_access_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Gint64_Gint64_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_GMount_Operation_Result_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_GMount_Operation_Result_Void);

   procedure Marsh_GObject_UTF8_String_Garray_Access_chars_ptr_array_access_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Garray_Access_chars_ptr_array_access_Void);

   procedure Marsh_GObject_UTF8_String_Gint64_Gint64_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Gint64_Gint64_Void);

   procedure Marsh_GObject_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void);

   procedure Marsh_GObject_UTF8_String_chars_ptr_array_access_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_chars_ptr_array_access_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gmount_Operation_GMount_Operation_Result_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gmount_Operation_GMount_Operation_Result_Void);

   procedure Marsh_Gmount_Operation_UTF8_String_Garray_Access_chars_ptr_array_access_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gmount_Operation_UTF8_String_Garray_Access_chars_ptr_array_access_Void);

   procedure Marsh_Gmount_Operation_UTF8_String_Gint64_Gint64_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gmount_Operation_UTF8_String_Gint64_Gint64_Void);

   procedure Marsh_Gmount_Operation_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gmount_Operation_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void);

   procedure Marsh_Gmount_Operation_UTF8_String_chars_ptr_array_access_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gmount_Operation_UTF8_String_chars_ptr_array_access_Void);

   procedure Marsh_Gmount_Operation_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gmount_Operation_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gmount_Operation_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gmount_Operation_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gmount_Operation_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gmount_Operation_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gmount_Operation_UTF8_String_chars_ptr_array_access_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gmount_Operation_UTF8_String_chars_ptr_array_access_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gmount_Operation_GMount_Operation_Result_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gmount_Operation_GMount_Operation_Result_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gmount_Operation_UTF8_String_Garray_Access_chars_ptr_array_access_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gmount_Operation_UTF8_String_Garray_Access_chars_ptr_array_access_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gmount_Operation_UTF8_String_Gint64_Gint64_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gmount_Operation_UTF8_String_Gint64_Gint64_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_chars_ptr_array_access_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_chars_ptr_array_access_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_GMount_Operation_Result_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_GMount_Operation_Result_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Garray_Access_chars_ptr_array_access_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_Garray_Access_chars_ptr_array_access_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gmount_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Gint64_Gint64_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_Gint64_Gint64_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------------------------------
   -- Marsh_GObject_GMount_Operation_Result_Void --
   ------------------------------------------------

   procedure Marsh_GObject_GMount_Operation_Result_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_GMount_Operation_Result_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_GMount_Operation_Result (Params, 1));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_GMount_Operation_Result_Void;

   -------------------------------------------------------------------------
   -- Marsh_GObject_UTF8_String_Garray_Access_chars_ptr_array_access_Void --
   -------------------------------------------------------------------------

   procedure Marsh_GObject_UTF8_String_Garray_Access_chars_ptr_array_access_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_Garray_Access_chars_ptr_array_access_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1), Unchecked_To_Garray_Access (Params, 2), Unchecked_To_chars_ptr_array_access (Params, 3));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_Garray_Access_chars_ptr_array_access_Void;

   --------------------------------------------------
   -- Marsh_GObject_UTF8_String_Gint64_Gint64_Void --
   --------------------------------------------------

   procedure Marsh_GObject_UTF8_String_Gint64_Gint64_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_Gint64_Gint64_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1), Unchecked_To_Gint64 (Params, 2), Unchecked_To_Gint64 (Params, 3));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_Gint64_Gint64_Void;

   --------------------------------------------------------------------------------
   -- Marsh_GObject_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void --
   --------------------------------------------------------------------------------

   procedure Marsh_GObject_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1), Unchecked_To_UTF8_String (Params, 2), Unchecked_To_UTF8_String (Params, 3), Unchecked_To_GAsk_Password_Flags (Params, 4));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void;

   -----------------------------------------------------------
   -- Marsh_GObject_UTF8_String_chars_ptr_array_access_Void --
   -----------------------------------------------------------

   procedure Marsh_GObject_UTF8_String_chars_ptr_array_access_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_chars_ptr_array_access_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1), Unchecked_To_chars_ptr_array_access (Params, 2));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_chars_ptr_array_access_Void;

   ------------------------
   -- Marsh_GObject_Void --
   ------------------------

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj);
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   ---------------------------------------------------------
   -- Marsh_Gmount_Operation_GMount_Operation_Result_Void --
   ---------------------------------------------------------

   procedure Marsh_Gmount_Operation_GMount_Operation_Result_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gmount_Operation_GMount_Operation_Result_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gmount_Operation := Gmount_Operation (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_GMount_Operation_Result (Params, 1));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gmount_Operation_GMount_Operation_Result_Void;

   ----------------------------------------------------------------------------------
   -- Marsh_Gmount_Operation_UTF8_String_Garray_Access_chars_ptr_array_access_Void --
   ----------------------------------------------------------------------------------

   procedure Marsh_Gmount_Operation_UTF8_String_Garray_Access_chars_ptr_array_access_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gmount_Operation_UTF8_String_Garray_Access_chars_ptr_array_access_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gmount_Operation := Gmount_Operation (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1), Unchecked_To_Garray_Access (Params, 2), Unchecked_To_chars_ptr_array_access (Params, 3));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gmount_Operation_UTF8_String_Garray_Access_chars_ptr_array_access_Void;

   -----------------------------------------------------------
   -- Marsh_Gmount_Operation_UTF8_String_Gint64_Gint64_Void --
   -----------------------------------------------------------

   procedure Marsh_Gmount_Operation_UTF8_String_Gint64_Gint64_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gmount_Operation_UTF8_String_Gint64_Gint64_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gmount_Operation := Gmount_Operation (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1), Unchecked_To_Gint64 (Params, 2), Unchecked_To_Gint64 (Params, 3));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gmount_Operation_UTF8_String_Gint64_Gint64_Void;

   -----------------------------------------------------------------------------------------
   -- Marsh_Gmount_Operation_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void --
   -----------------------------------------------------------------------------------------

   procedure Marsh_Gmount_Operation_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gmount_Operation_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gmount_Operation := Gmount_Operation (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1), Unchecked_To_UTF8_String (Params, 2), Unchecked_To_UTF8_String (Params, 3), Unchecked_To_GAsk_Password_Flags (Params, 4));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gmount_Operation_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void;

   --------------------------------------------------------------------
   -- Marsh_Gmount_Operation_UTF8_String_chars_ptr_array_access_Void --
   --------------------------------------------------------------------

   procedure Marsh_Gmount_Operation_UTF8_String_chars_ptr_array_access_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gmount_Operation_UTF8_String_chars_ptr_array_access_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gmount_Operation := Gmount_Operation (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1), Unchecked_To_chars_ptr_array_access (Params, 2));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gmount_Operation_UTF8_String_chars_ptr_array_access_Void;

   ---------------------------------
   -- Marsh_Gmount_Operation_Void --
   ---------------------------------

   procedure Marsh_Gmount_Operation_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gmount_Operation_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gmount_Operation := Gmount_Operation (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gmount_Operation_Void;

   ----------------
   -- On_Aborted --
   ----------------

   procedure On_Aborted
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_Gmount_Operation_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "aborted" & ASCII.NUL, Call, After);
   end On_Aborted;

   ----------------
   -- On_Aborted --
   ----------------

   procedure On_Aborted
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "aborted" & ASCII.NUL, Call, After, Slot);
   end On_Aborted;

   ---------------------
   -- On_Ask_Password --
   ---------------------

   procedure On_Ask_Password
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_Gmount_Operation_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "ask-password" & ASCII.NUL, Call, After);
   end On_Ask_Password;

   ---------------------
   -- On_Ask_Password --
   ---------------------

   procedure On_Ask_Password
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_GObject_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "ask-password" & ASCII.NUL, Call, After, Slot);
   end On_Ask_Password;

   ---------------------
   -- On_Ask_Question --
   ---------------------

   procedure On_Ask_Question
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_Gmount_Operation_UTF8_String_chars_ptr_array_access_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "ask-question" & ASCII.NUL, Call, After);
   end On_Ask_Question;

   ---------------------
   -- On_Ask_Question --
   ---------------------

   procedure On_Ask_Question
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_GObject_UTF8_String_chars_ptr_array_access_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "ask-question" & ASCII.NUL, Call, After, Slot);
   end On_Ask_Question;

   --------------
   -- On_Reply --
   --------------

   procedure On_Reply
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_Gmount_Operation_GMount_Operation_Result_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "reply" & ASCII.NUL, Call, After);
   end On_Reply;

   --------------
   -- On_Reply --
   --------------

   procedure On_Reply
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_GObject_GMount_Operation_Result_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "reply" & ASCII.NUL, Call, After, Slot);
   end On_Reply;

   -----------------------
   -- On_Show_Processes --
   -----------------------

   procedure On_Show_Processes
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_Gmount_Operation_UTF8_String_Garray_Access_chars_ptr_array_access_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "show-processes" & ASCII.NUL, Call, After);
   end On_Show_Processes;

   -----------------------
   -- On_Show_Processes --
   -----------------------

   procedure On_Show_Processes
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_GObject_UTF8_String_Garray_Access_chars_ptr_array_access_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "show-processes" & ASCII.NUL, Call, After, Slot);
   end On_Show_Processes;

   ------------------------------
   -- On_Show_Unmount_Progress --
   ------------------------------

   procedure On_Show_Unmount_Progress
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_Gmount_Operation_UTF8_String_Gint64_Gint64_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "show-unmount-progress" & ASCII.NUL, Call, After);
   end On_Show_Unmount_Progress;

   ------------------------------
   -- On_Show_Unmount_Progress --
   ------------------------------

   procedure On_Show_Unmount_Progress
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_GObject_UTF8_String_Gint64_Gint64_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "show-unmount-progress" & ASCII.NUL, Call, After, Slot);
   end On_Show_Unmount_Progress;

end Glib.GMountOperations;
