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
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
with System;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gdk.Content_Provider is

   package Type_Conversion_Gdk_Content_Provider is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Content_Provider_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Content_Provider);

   ----------------------------------------
   -- Gdk_Content_Provider_New_For_Bytes --
   ----------------------------------------

   function Gdk_Content_Provider_New_For_Bytes
      (Mime_Type : UTF8_String;
       Bytes     : Glib.Bytes.Gbytes) return Gdk_Content_Provider
   is
      Self : constant Gdk_Content_Provider := new Gdk_Content_Provider_Record;
   begin
      Gdk.Content_Provider.Initialize_For_Bytes (Self, Mime_Type, Bytes);
      return Self;
   end Gdk_Content_Provider_New_For_Bytes;

   ----------------------------------------
   -- Gdk_Content_Provider_New_For_Value --
   ----------------------------------------

   function Gdk_Content_Provider_New_For_Value
      (Value : in out Glib.Values.GValue) return Gdk_Content_Provider
   is
      Self : constant Gdk_Content_Provider := new Gdk_Content_Provider_Record;
   begin
      Gdk.Content_Provider.Initialize_For_Value (Self, Value);
      return Self;
   end Gdk_Content_Provider_New_For_Value;

   -----------------------
   -- Gdk_New_For_Bytes --
   -----------------------

   procedure Gdk_New_For_Bytes
      (Self      : out Gdk_Content_Provider;
       Mime_Type : UTF8_String;
       Bytes     : Glib.Bytes.Gbytes)
   is
   begin
      Self := new Gdk_Content_Provider_Record;
      Gdk.Content_Provider.Initialize_For_Bytes (Self, Mime_Type, Bytes);
   end Gdk_New_For_Bytes;

   -----------------------
   -- Gdk_New_For_Value --
   -----------------------

   procedure Gdk_New_For_Value
      (Self  : out Gdk_Content_Provider;
       Value : in out Glib.Values.GValue)
   is
   begin
      Self := new Gdk_Content_Provider_Record;
      Gdk.Content_Provider.Initialize_For_Value (Self, Value);
   end Gdk_New_For_Value;

   --------------------------
   -- Initialize_For_Bytes --
   --------------------------

   procedure Initialize_For_Bytes
      (Self      : not null access Gdk_Content_Provider_Record'Class;
       Mime_Type : UTF8_String;
       Bytes     : Glib.Bytes.Gbytes)
   is
      function Internal
         (Mime_Type : Gtkada.Types.Chars_Ptr;
          Bytes     : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_content_provider_new_for_bytes");
      Tmp_Mime_Type : Gtkada.Types.Chars_Ptr := New_String (Mime_Type);
      Tmp_Return    : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Mime_Type, Get_Object (Bytes));
         Free (Tmp_Mime_Type);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_For_Bytes;

   --------------------------
   -- Initialize_For_Value --
   --------------------------

   procedure Initialize_For_Value
      (Self  : not null access Gdk_Content_Provider_Record'Class;
       Value : in out Glib.Values.GValue)
   is
      function Internal
         (Acc_Value : access Glib.Values.GValue) return System.Address;
      pragma Import (C, Internal, "gdk_content_provider_new_for_value");
      Acc_Value : aliased Glib.Values.GValue := Value;
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Acc_Value'Access));
      end if;
   end Initialize_For_Value;

   ---------------------
   -- Content_Changed --
   ---------------------

   procedure Content_Changed
      (Self : not null access Gdk_Content_Provider_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_content_provider_content_changed");
   begin
      Internal (Get_Object (Self));
   end Content_Changed;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
      (Self  : not null access Gdk_Content_Provider_Record;
       Value : access Glib.Values.GValue) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Acc_Value : access Glib.Values.GValue) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_content_provider_get_value");
      Acc_Value  : aliased Glib.Values.GValue;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Value'Access);
      Value.all := Acc_Value;
      return Tmp_Return /= 0;
   end Get_Value;

   -----------------
   -- Ref_Formats --
   -----------------

   function Ref_Formats
      (Self : not null access Gdk_Content_Provider_Record)
       return Gdk.Content_Formats.Gdk_Content_Formats
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_content_provider_ref_formats");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref_Formats;

   --------------------------
   -- Ref_Storable_Formats --
   --------------------------

   function Ref_Storable_Formats
      (Self : not null access Gdk_Content_Provider_Record)
       return Gdk.Content_Formats.Gdk_Content_Formats
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_content_provider_ref_storable_formats");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref_Storable_Formats;

   ----------------------------
   -- Write_Mime_Type_Finish --
   ----------------------------

   function Write_Mime_Type_Finish
      (Self   : not null access Gdk_Content_Provider_Record;
       Result : Glib.G_Async_Result) return Boolean
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_content_provider_write_mime_type_finish");
   begin
      return Internal (Get_Object (Self), Result) /= 0;
   end Write_Mime_Type_Finish;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Content_Provider_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Content_Provider_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gdk_Content_Provider_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Content_Provider_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gdk_Content_Provider_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gdk_Content_Provider_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Content_Provider_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Content_Provider_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Content_Provider_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Content_Provider_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gdk_Content_Provider_Record'Class;
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
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   -------------------------------------
   -- Marsh_Gdk_Content_Provider_Void --
   -------------------------------------

   procedure Marsh_Gdk_Content_Provider_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Content_Provider_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Content_Provider := Gdk_Content_Provider (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gdk_Content_Provider_Void;

   ------------------------
   -- On_Content_Changed --
   ------------------------

   procedure On_Content_Changed
      (Self  : not null access Gdk_Content_Provider_Record;
       Call  : Cb_Gdk_Content_Provider_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "content-changed" & ASCII.NUL, Call, After);
   end On_Content_Changed;

   ------------------------
   -- On_Content_Changed --
   ------------------------

   procedure On_Content_Changed
      (Self  : not null access Gdk_Content_Provider_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "content-changed" & ASCII.NUL, Call, After, Slot);
   end On_Content_Changed;

end Gdk.Content_Provider;
