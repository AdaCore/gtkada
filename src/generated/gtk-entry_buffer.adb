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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Entry_Buffer is

   package Type_Conversion_Gtk_Entry_Buffer is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Entry_Buffer_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Entry_Buffer);

   --------------------------
   -- Gtk_Entry_Buffer_New --
   --------------------------

   function Gtk_Entry_Buffer_New
      (Initial_Chars   : UTF8_String := "";
       N_Initial_Chars : Glib.Gint) return Gtk_Entry_Buffer
   is
      Self : constant Gtk_Entry_Buffer := new Gtk_Entry_Buffer_Record;
   begin
      Gtk.Entry_Buffer.Initialize (Self, Initial_Chars, N_Initial_Chars);
      return Self;
   end Gtk_Entry_Buffer_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self            : out Gtk_Entry_Buffer;
       Initial_Chars   : UTF8_String := "";
       N_Initial_Chars : Glib.Gint)
   is
   begin
      Self := new Gtk_Entry_Buffer_Record;
      Gtk.Entry_Buffer.Initialize (Self, Initial_Chars, N_Initial_Chars);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self            : not null access Gtk_Entry_Buffer_Record'Class;
       Initial_Chars   : UTF8_String := "";
       N_Initial_Chars : Glib.Gint)
   is
      function Internal
         (Initial_Chars   : Gtkada.Types.Chars_Ptr;
          N_Initial_Chars : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_entry_buffer_new");
      Tmp_Initial_Chars : Gtkada.Types.Chars_Ptr;
      Tmp_Return        : System.Address;
   begin
      if not Self.Is_Created then
         if Initial_Chars = "" then
            Tmp_Initial_Chars := Gtkada.Types.Null_Ptr;
         else
            Tmp_Initial_Chars := New_String (Initial_Chars);
         end if;
         Tmp_Return := Internal (Tmp_Initial_Chars, N_Initial_Chars);
         Free (Tmp_Initial_Chars);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize;

   -----------------
   -- Delete_Text --
   -----------------

   function Delete_Text
      (Self     : not null access Gtk_Entry_Buffer_Record;
       Position : Guint;
       N_Chars  : Glib.Gint) return Guint
   is
      function Internal
         (Self     : System.Address;
          Position : Guint;
          N_Chars  : Glib.Gint) return Guint;
      pragma Import (C, Internal, "gtk_entry_buffer_delete_text");
   begin
      return Internal (Get_Object (Self), Position, N_Chars);
   end Delete_Text;

   -----------------------
   -- Emit_Deleted_Text --
   -----------------------

   procedure Emit_Deleted_Text
      (Self     : not null access Gtk_Entry_Buffer_Record;
       Position : Guint;
       N_Chars  : Guint)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Guint;
          N_Chars  : Guint);
      pragma Import (C, Internal, "gtk_entry_buffer_emit_deleted_text");
   begin
      Internal (Get_Object (Self), Position, N_Chars);
   end Emit_Deleted_Text;

   ------------------------
   -- Emit_Inserted_Text --
   ------------------------

   procedure Emit_Inserted_Text
      (Self     : not null access Gtk_Entry_Buffer_Record;
       Position : Guint;
       Chars    : UTF8_String;
       N_Chars  : Guint)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Guint;
          Chars    : Gtkada.Types.Chars_Ptr;
          N_Chars  : Guint);
      pragma Import (C, Internal, "gtk_entry_buffer_emit_inserted_text");
      Tmp_Chars : Gtkada.Types.Chars_Ptr := New_String (Chars);
   begin
      Internal (Get_Object (Self), Position, Tmp_Chars, N_Chars);
      Free (Tmp_Chars);
   end Emit_Inserted_Text;

   ---------------
   -- Get_Bytes --
   ---------------

   function Get_Bytes
      (Self : not null access Gtk_Entry_Buffer_Record) return Gsize
   is
      function Internal (Self : System.Address) return Gsize;
      pragma Import (C, Internal, "gtk_entry_buffer_get_bytes");
   begin
      return Internal (Get_Object (Self));
   end Get_Bytes;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length
      (Self : not null access Gtk_Entry_Buffer_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_entry_buffer_get_length");
   begin
      return Internal (Get_Object (Self));
   end Get_Length;

   --------------------
   -- Get_Max_Length --
   --------------------

   function Get_Max_Length
      (Self : not null access Gtk_Entry_Buffer_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_entry_buffer_get_max_length");
   begin
      return Internal (Get_Object (Self));
   end Get_Max_Length;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
      (Self : not null access Gtk_Entry_Buffer_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_entry_buffer_get_text");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Text;

   -----------------
   -- Insert_Text --
   -----------------

   function Insert_Text
      (Self     : not null access Gtk_Entry_Buffer_Record;
       Position : Guint;
       Chars    : UTF8_String;
       N_Chars  : Glib.Gint) return Guint
   is
      function Internal
         (Self     : System.Address;
          Position : Guint;
          Chars    : Gtkada.Types.Chars_Ptr;
          N_Chars  : Glib.Gint) return Guint;
      pragma Import (C, Internal, "gtk_entry_buffer_insert_text");
      Tmp_Chars  : Gtkada.Types.Chars_Ptr := New_String (Chars);
      Tmp_Return : Guint;
   begin
      Tmp_Return := Internal (Get_Object (Self), Position, Tmp_Chars, N_Chars);
      Free (Tmp_Chars);
      return Tmp_Return;
   end Insert_Text;

   --------------------
   -- Set_Max_Length --
   --------------------

   procedure Set_Max_Length
      (Self       : not null access Gtk_Entry_Buffer_Record;
       Max_Length : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Max_Length : Glib.Gint);
      pragma Import (C, Internal, "gtk_entry_buffer_set_max_length");
   begin
      Internal (Get_Object (Self), Max_Length);
   end Set_Max_Length;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (Self    : not null access Gtk_Entry_Buffer_Record;
       Chars   : UTF8_String;
       N_Chars : Glib.Gint)
   is
      procedure Internal
         (Self    : System.Address;
          Chars   : Gtkada.Types.Chars_Ptr;
          N_Chars : Glib.Gint);
      pragma Import (C, Internal, "gtk_entry_buffer_set_text");
      Tmp_Chars : Gtkada.Types.Chars_Ptr := New_String (Chars);
   begin
      Internal (Get_Object (Self), Tmp_Chars, N_Chars);
      Free (Tmp_Chars);
   end Set_Text;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Entry_Buffer_Guint_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Entry_Buffer_Guint_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Guint_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Guint_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Entry_Buffer_Guint_UTF8_String_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Entry_Buffer_Guint_UTF8_String_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Guint_UTF8_String_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Guint_UTF8_String_Guint_Void);

   procedure Connect
      (Object  : access Gtk_Entry_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Buffer_Guint_Guint_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Entry_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Buffer_Guint_UTF8_String_Guint_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_UTF8_String_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Guint_Guint_Void);

   procedure Marsh_GObject_Guint_UTF8_String_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Guint_UTF8_String_Guint_Void);

   procedure Marsh_Gtk_Entry_Buffer_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Entry_Buffer_Guint_Guint_Void);

   procedure Marsh_Gtk_Entry_Buffer_Guint_UTF8_String_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Entry_Buffer_Guint_UTF8_String_Guint_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Entry_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Buffer_Guint_Guint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Entry_Buffer_Guint_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Entry_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Buffer_Guint_UTF8_String_Guint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Entry_Buffer_Guint_UTF8_String_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Guint_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_UTF8_String_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Guint_UTF8_String_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------------------
   -- Marsh_GObject_Guint_Guint_Void --
   ------------------------------------

   procedure Marsh_GObject_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Guint_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Guint (Params, 1), Unchecked_To_Guint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Guint_Guint_Void;

   ------------------------------------------------
   -- Marsh_GObject_Guint_UTF8_String_Guint_Void --
   ------------------------------------------------

   procedure Marsh_GObject_Guint_UTF8_String_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Guint_UTF8_String_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Guint (Params, 1), Unchecked_To_UTF8_String (Params, 2), Unchecked_To_Guint (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Guint_UTF8_String_Guint_Void;

   ---------------------------------------------
   -- Marsh_Gtk_Entry_Buffer_Guint_Guint_Void --
   ---------------------------------------------

   procedure Marsh_Gtk_Entry_Buffer_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Entry_Buffer_Guint_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Entry_Buffer := Gtk_Entry_Buffer (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Guint (Params, 1), Unchecked_To_Guint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Entry_Buffer_Guint_Guint_Void;

   ---------------------------------------------------------
   -- Marsh_Gtk_Entry_Buffer_Guint_UTF8_String_Guint_Void --
   ---------------------------------------------------------

   procedure Marsh_Gtk_Entry_Buffer_Guint_UTF8_String_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Entry_Buffer_Guint_UTF8_String_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Entry_Buffer := Gtk_Entry_Buffer (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Guint (Params, 1), Unchecked_To_UTF8_String (Params, 2), Unchecked_To_Guint (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Entry_Buffer_Guint_UTF8_String_Guint_Void;

   ---------------------
   -- On_Deleted_Text --
   ---------------------

   procedure On_Deleted_Text
      (Self  : not null access Gtk_Entry_Buffer_Record;
       Call  : Cb_Gtk_Entry_Buffer_Guint_Guint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "deleted-text" & ASCII.NUL, Call, After);
   end On_Deleted_Text;

   ---------------------
   -- On_Deleted_Text --
   ---------------------

   procedure On_Deleted_Text
      (Self  : not null access Gtk_Entry_Buffer_Record;
       Call  : Cb_GObject_Guint_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "deleted-text" & ASCII.NUL, Call, After, Slot);
   end On_Deleted_Text;

   ----------------------
   -- On_Inserted_Text --
   ----------------------

   procedure On_Inserted_Text
      (Self  : not null access Gtk_Entry_Buffer_Record;
       Call  : Cb_Gtk_Entry_Buffer_Guint_UTF8_String_Guint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "inserted-text" & ASCII.NUL, Call, After);
   end On_Inserted_Text;

   ----------------------
   -- On_Inserted_Text --
   ----------------------

   procedure On_Inserted_Text
      (Self  : not null access Gtk_Entry_Buffer_Record;
       Call  : Cb_GObject_Guint_UTF8_String_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "inserted-text" & ASCII.NUL, Call, After, Slot);
   end On_Inserted_Text;

end Gtk.Entry_Buffer;
