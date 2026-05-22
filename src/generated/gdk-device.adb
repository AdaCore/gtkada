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
with Gdk.Display;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
with System;

package body Gdk.Device is

   package Type_Conversion_Gdk_Device is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Device_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Device);

   -----------------------------
   -- Get_Active_Layout_Index --
   -----------------------------

   function Get_Active_Layout_Index
      (Self : not null access Gdk_Device_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_device_get_active_layout_index");
   begin
      return Internal (Get_Object (Self));
   end Get_Active_Layout_Index;

   -------------------------
   -- Get_Caps_Lock_State --
   -------------------------

   function Get_Caps_Lock_State
      (Self : not null access Gdk_Device_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_device_get_caps_lock_state");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Caps_Lock_State;

   ---------------------
   -- Get_Device_Tool --
   ---------------------

   function Get_Device_Tool
      (Self : not null access Gdk_Device_Record)
       return Gdk.Device_Tool.Gdk_Device_Tool
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_device_get_device_tool");
      Stub_Gdk_Device_Tool : Gdk.Device_Tool.Gdk_Device_Tool_Record;
   begin
      return Gdk.Device_Tool.Gdk_Device_Tool (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Device_Tool));
   end Get_Device_Tool;

   -------------------
   -- Get_Direction --
   -------------------

   function Get_Direction
      (Self : not null access Gdk_Device_Record)
       return Pango.Enums.Direction
   is
      function Internal (Self : System.Address) return Pango.Enums.Direction;
      pragma Import (C, Internal, "gdk_device_get_direction");
   begin
      return Internal (Get_Object (Self));
   end Get_Direction;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gdk_Device_Record) return Gdk.Gdk_Display
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_device_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Gdk_Display (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Display;

   --------------------
   -- Get_Has_Cursor --
   --------------------

   function Get_Has_Cursor
      (Self : not null access Gdk_Device_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_device_get_has_cursor");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Has_Cursor;

   ----------------------
   -- Get_Layout_Names --
   ----------------------

   function Get_Layout_Names
      (Self : not null access Gdk_Device_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gdk_device_get_layout_names");
   begin
      return To_String_List_And_Free (Internal (Get_Object (Self)));
   end Get_Layout_Names;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Self : not null access Gdk_Device_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gdk_device_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Name;

   ------------------------
   -- Get_Num_Lock_State --
   ------------------------

   function Get_Num_Lock_State
      (Self : not null access Gdk_Device_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_device_get_num_lock_state");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Num_Lock_State;

   ---------------------
   -- Get_Num_Touches --
   ---------------------

   function Get_Num_Touches
      (Self : not null access Gdk_Device_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gdk_device_get_num_touches");
   begin
      return Internal (Get_Object (Self));
   end Get_Num_Touches;

   --------------------
   -- Get_Product_Id --
   --------------------

   function Get_Product_Id
      (Self : not null access Gdk_Device_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gdk_device_get_product_id");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Product_Id;

   ---------------------------
   -- Get_Scroll_Lock_State --
   ---------------------------

   function Get_Scroll_Lock_State
      (Self : not null access Gdk_Device_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_device_get_scroll_lock_state");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Scroll_Lock_State;

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source
      (Self : not null access Gdk_Device_Record) return Gdk_Input_Source
   is
      function Internal (Self : System.Address) return Gdk_Input_Source;
      pragma Import (C, Internal, "gdk_device_get_source");
   begin
      return Internal (Get_Object (Self));
   end Get_Source;

   -------------------
   -- Get_Timestamp --
   -------------------

   function Get_Timestamp
      (Self : not null access Gdk_Device_Record) return Guint32
   is
      function Internal (Self : System.Address) return Guint32;
      pragma Import (C, Internal, "gdk_device_get_timestamp");
   begin
      return Internal (Get_Object (Self));
   end Get_Timestamp;

   -------------------
   -- Get_Vendor_Id --
   -------------------

   function Get_Vendor_Id
      (Self : not null access Gdk_Device_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gdk_device_get_vendor_id");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Vendor_Id;

   ----------------------
   -- Has_Bidi_Layouts --
   ----------------------

   function Has_Bidi_Layouts
      (Self : not null access Gdk_Device_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_device_has_bidi_layouts");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Has_Bidi_Layouts;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Device_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Device_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Device_Gdk_Device_Tool_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Device_Gdk_Device_Tool_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Device_Tool_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Device_Tool_Void);

   procedure Connect
      (Object  : access Gdk_Device_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Device_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gdk_Device_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Device_Gdk_Device_Tool_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gdk_Device_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gdk_Device_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Device_Tool_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gdk_Device_Tool_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Device_Tool_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gdk_Device_Gdk_Device_Tool_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Device_Gdk_Device_Tool_Void);

   procedure Marsh_Gdk_Device_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Device_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Device_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Device_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Device_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Device_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Device_Gdk_Device_Tool_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Device_Gdk_Device_Tool_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gdk_Device_Record'Class;
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
      (Object  : access Gdk_Device_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Device_Tool_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Device_Tool_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ----------------------------------------
   -- Marsh_GObject_Gdk_Device_Tool_Void --
   ----------------------------------------

   procedure Marsh_GObject_Gdk_Device_Tool_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Device_Tool_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gdk.Device_Tool.Gdk_Device_Tool (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Device_Tool_Void;

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

   -------------------------------------------
   -- Marsh_Gdk_Device_Gdk_Device_Tool_Void --
   -------------------------------------------

   procedure Marsh_Gdk_Device_Gdk_Device_Tool_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Device_Gdk_Device_Tool_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Device := Gdk_Device (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gdk.Device_Tool.Gdk_Device_Tool (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gdk_Device_Gdk_Device_Tool_Void;

   ---------------------------
   -- Marsh_Gdk_Device_Void --
   ---------------------------

   procedure Marsh_Gdk_Device_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Device_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Device := Gdk_Device (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gdk_Device_Void;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gdk_Device_Record;
       Call  : Cb_Gdk_Device_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "changed" & ASCII.NUL, Call, After);
   end On_Changed;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gdk_Device_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "changed" & ASCII.NUL, Call, After, Slot);
   end On_Changed;

   ---------------------
   -- On_Tool_Changed --
   ---------------------

   procedure On_Tool_Changed
      (Self  : not null access Gdk_Device_Record;
       Call  : Cb_Gdk_Device_Gdk_Device_Tool_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "tool-changed" & ASCII.NUL, Call, After);
   end On_Tool_Changed;

   ---------------------
   -- On_Tool_Changed --
   ---------------------

   procedure On_Tool_Changed
      (Self  : not null access Gdk_Device_Record;
       Call  : Cb_GObject_Gdk_Device_Tool_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "tool-changed" & ASCII.NUL, Call, After, Slot);
   end On_Tool_Changed;

end Gdk.Device;
