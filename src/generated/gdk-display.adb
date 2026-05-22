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

package body Gdk.Display is

   package Type_Conversion_Gdk_Display is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Display_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Display);

   ----------
   -- Beep --
   ----------

   procedure Beep (Self : not null access Gdk_Display_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_display_beep");
   begin
      Internal (Get_Object (Self));
   end Beep;

   -----------
   -- Close --
   -----------

   procedure Close (Self : not null access Gdk_Display_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_display_close");
   begin
      Internal (Get_Object (Self));
   end Close;

   -----------------------
   -- Create_Gl_Context --
   -----------------------

   function Create_Gl_Context
      (Self : not null access Gdk_Display_Record)
       return Gdk.GLContext.Gdk_GLContext
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_display_create_gl_context");
      Stub_Gdk_GLContext : Gdk.GLContext.Gdk_GLContext_Record;
   begin
      return Gdk.GLContext.Gdk_GLContext (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_GLContext));
   end Create_Gl_Context;

   -----------------------
   -- Device_Is_Grabbed --
   -----------------------

   function Device_Is_Grabbed
      (Self   : not null access Gdk_Display_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class)
       return Boolean
   is
      function Internal
         (Self   : System.Address;
          Device : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_display_device_is_grabbed");
   begin
      return Internal (Get_Object (Self), Get_Object (Device)) /= 0;
   end Device_Is_Grabbed;

   -----------
   -- Flush --
   -----------

   procedure Flush (Self : not null access Gdk_Display_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_display_flush");
   begin
      Internal (Get_Object (Self));
   end Flush;

   -------------------
   -- Get_Clipboard --
   -------------------

   function Get_Clipboard
      (Self : not null access Gdk_Display_Record)
       return Gdk.Clipboard.Gdk_Clipboard
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_display_get_clipboard");
      Stub_Gdk_Clipboard : Gdk.Clipboard.Gdk_Clipboard_Record;
   begin
      return Gdk.Clipboard.Gdk_Clipboard (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Clipboard));
   end Get_Clipboard;

   ------------------------
   -- Get_Dmabuf_Formats --
   ------------------------

   function Get_Dmabuf_Formats
      (Self : not null access Gdk_Display_Record)
       return Gdk.Dmabuf_Formats.Gdk_Dmabuf_Formats
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_display_get_dmabuf_formats");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Dmabuf_Formats;

   ----------------------------
   -- Get_Monitor_At_Surface --
   ----------------------------

   function Get_Monitor_At_Surface
      (Self    : not null access Gdk_Display_Record;
       Surface : not null access Gdk.Surface.Gdk_Surface_Record'Class)
       return Gdk.Monitor.Gdk_Monitor
   is
      function Internal
         (Self    : System.Address;
          Surface : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_display_get_monitor_at_surface");
      Stub_Gdk_Monitor : Gdk.Monitor.Gdk_Monitor_Record;
   begin
      return Gdk.Monitor.Gdk_Monitor (Get_User_Data (Internal (Get_Object (Self), Get_Object (Surface)), Stub_Gdk_Monitor));
   end Get_Monitor_At_Surface;

   ------------------
   -- Get_Monitors --
   ------------------

   function Get_Monitors
      (Self : not null access Gdk_Display_Record)
       return Glib.List_Model.Glist_Model
   is
      function Internal
         (Self : System.Address) return Glib.List_Model.Glist_Model;
      pragma Import (C, Internal, "gdk_display_get_monitors");
   begin
      return Internal (Get_Object (Self));
   end Get_Monitors;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Self : not null access Gdk_Display_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gdk_display_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Name;

   ---------------------------
   -- Get_Primary_Clipboard --
   ---------------------------

   function Get_Primary_Clipboard
      (Self : not null access Gdk_Display_Record)
       return Gdk.Clipboard.Gdk_Clipboard
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_display_get_primary_clipboard");
      Stub_Gdk_Clipboard : Gdk.Clipboard.Gdk_Clipboard_Record;
   begin
      return Gdk.Clipboard.Gdk_Clipboard (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Clipboard));
   end Get_Primary_Clipboard;

   -----------------
   -- Get_Setting --
   -----------------

   function Get_Setting
      (Self  : not null access Gdk_Display_Record;
       Name  : UTF8_String;
       Value : in out Glib.Values.GValue) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Name      : Gtkada.Types.Chars_Ptr;
          Acc_Value : access Glib.Values.GValue) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_display_get_setting");
      Acc_Value  : aliased Glib.Values.GValue := Value;
      Tmp_Name   : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Name, Acc_Value'Access);
      Free (Tmp_Name);
      Value := Acc_Value;
      return Tmp_Return /= 0;
   end Get_Setting;

   ---------------------------------
   -- Get_Startup_Notification_Id --
   ---------------------------------

   function Get_Startup_Notification_Id
      (Self : not null access Gdk_Display_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gdk_display_get_startup_notification_id");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Startup_Notification_Id;

   ---------------
   -- Is_Closed --
   ---------------

   function Is_Closed
      (Self : not null access Gdk_Display_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_display_is_closed");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Closed;

   -------------------
   -- Is_Composited --
   -------------------

   function Is_Composited
      (Self : not null access Gdk_Display_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_display_is_composited");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Composited;

   -------------
   -- Is_Rgba --
   -------------

   function Is_Rgba
      (Self : not null access Gdk_Display_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_display_is_rgba");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Rgba;

   -----------------------------
   -- Notify_Startup_Complete --
   -----------------------------

   procedure Notify_Startup_Complete
      (Self       : not null access Gdk_Display_Record;
       Startup_Id : UTF8_String)
   is
      procedure Internal
         (Self       : System.Address;
          Startup_Id : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gdk_display_notify_startup_complete");
      Tmp_Startup_Id : Gtkada.Types.Chars_Ptr := New_String (Startup_Id);
   begin
      Internal (Get_Object (Self), Tmp_Startup_Id);
      Free (Tmp_Startup_Id);
   end Notify_Startup_Complete;

   ----------------
   -- Prepare_Gl --
   ----------------

   function Prepare_Gl
      (Self : not null access Gdk_Display_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_display_prepare_gl");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Prepare_Gl;

   ---------------------------
   -- Supports_Input_Shapes --
   ---------------------------

   function Supports_Input_Shapes
      (Self : not null access Gdk_Display_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_display_supports_input_shapes");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Supports_Input_Shapes;

   ---------------------------
   -- Supports_Shadow_Width --
   ---------------------------

   function Supports_Shadow_Width
      (Self : not null access Gdk_Display_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_display_supports_shadow_width");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Supports_Shadow_Width;

   ----------
   -- Sync --
   ----------

   procedure Sync (Self : not null access Gdk_Display_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_display_sync");
   begin
      Internal (Get_Object (Self));
   end Sync;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default return Gdk.Gdk_Display is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_display_get_default");
      Stub_Gdk_Display : Gdk_Display_Record;
   begin
      return Gdk.Gdk_Display (Get_User_Data (Internal, Stub_Gdk_Display));
   end Get_Default;

   ----------
   -- Open --
   ----------

   function Open (Display_Name : UTF8_String := "") return Gdk.Gdk_Display is
      function Internal
         (Display_Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gdk_display_open");
      Tmp_Display_Name : Gtkada.Types.Chars_Ptr;
      Stub_Gdk_Display : Gdk_Display_Record;
      Tmp_Return       : System.Address;
   begin
      if Display_Name = "" then
         Tmp_Display_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Display_Name := New_String (Display_Name);
      end if;
      Tmp_Return := Internal (Tmp_Display_Name);
      Free (Tmp_Display_Name);
      return Gdk.Gdk_Display (Get_User_Data (Tmp_Return, Stub_Gdk_Display));
   end Open;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Display_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Display_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Display_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Display_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Display_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Display_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Void);

   procedure Connect
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Display_Boolean_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Display_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Display_UTF8_String_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean_Void);

   procedure Marsh_GObject_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gdk_Display_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Display_Boolean_Void);

   procedure Marsh_Gdk_Display_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Display_UTF8_String_Void);

   procedure Marsh_Gdk_Display_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Display_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Display_Boolean_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Display_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Display_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Display_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Display_UTF8_String_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Display_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gdk_Display_Record'Class;
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
      (Object  : access Gdk_Display_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   --------------------------------
   -- Marsh_GObject_Boolean_Void --
   --------------------------------

   procedure Marsh_GObject_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Boolean (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean_Void;

   ------------------------------------
   -- Marsh_GObject_UTF8_String_Void --
   ------------------------------------

   procedure Marsh_GObject_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_Void;

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

   ------------------------------------
   -- Marsh_Gdk_Display_Boolean_Void --
   ------------------------------------

   procedure Marsh_Gdk_Display_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Display_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Display := Gdk_Display (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Boolean (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gdk_Display_Boolean_Void;

   ----------------------------------------
   -- Marsh_Gdk_Display_UTF8_String_Void --
   ----------------------------------------

   procedure Marsh_Gdk_Display_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Display_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Display := Gdk_Display (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gdk_Display_UTF8_String_Void;

   ----------------------------
   -- Marsh_Gdk_Display_Void --
   ----------------------------

   procedure Marsh_Gdk_Display_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Display_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Display := Gdk_Display (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gdk_Display_Void;

   ---------------
   -- On_Closed --
   ---------------

   procedure On_Closed
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_Gdk_Display_Boolean_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "closed" & ASCII.NUL, Call, After);
   end On_Closed;

   ---------------
   -- On_Closed --
   ---------------

   procedure On_Closed
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_GObject_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "closed" & ASCII.NUL, Call, After, Slot);
   end On_Closed;

   ---------------
   -- On_Opened --
   ---------------

   procedure On_Opened
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_Gdk_Display_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "opened" & ASCII.NUL, Call, After);
   end On_Opened;

   ---------------
   -- On_Opened --
   ---------------

   procedure On_Opened
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "opened" & ASCII.NUL, Call, After, Slot);
   end On_Opened;

   ------------------------
   -- On_Setting_Changed --
   ------------------------

   procedure On_Setting_Changed
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_Gdk_Display_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "setting-changed" & ASCII.NUL, Call, After);
   end On_Setting_Changed;

   ------------------------
   -- On_Setting_Changed --
   ------------------------

   procedure On_Setting_Changed
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "setting-changed" & ASCII.NUL, Call, After, Slot);
   end On_Setting_Changed;

end Gdk.Display;
