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

package body Gdk.Drag_Contexts is

   package Type_Conversion_Drag_Context is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Drag_Context_Record);
   pragma Unreferenced (Type_Conversion_Drag_Context);

   -----------------
   -- Get_Actions --
   -----------------

   function Get_Actions
      (Self : not null access Drag_Context_Record) return Gdk_Drag_Action
   is
      function Internal (Self : System.Address) return Gdk_Drag_Action;
      pragma Import (C, Internal, "gdk_drag_context_get_actions");
   begin
      return Internal (Get_Object (Self));
   end Get_Actions;

   ---------------------
   -- Get_Dest_Window --
   ---------------------

   function Get_Dest_Window
      (Self : not null access Drag_Context_Record) return Gdk.Gdk_Window
   is
      function Internal (Self : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gdk_drag_context_get_dest_window");
   begin
      return Internal (Get_Object (Self));
   end Get_Dest_Window;

   ----------------
   -- Get_Device --
   ----------------

   function Get_Device
      (Self : not null access Drag_Context_Record)
       return Gdk.Device.Gdk_Device
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_drag_context_get_device");
      Stub_Gdk_Device : Gdk.Device.Gdk_Device_Record;
   begin
      return Gdk.Device.Gdk_Device (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Device));
   end Get_Device;

   ---------------------
   -- Get_Drag_Window --
   ---------------------

   function Get_Drag_Window
      (Self : not null access Drag_Context_Record) return Gdk.Gdk_Window
   is
      function Internal (Self : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gdk_drag_context_get_drag_window");
   begin
      return Internal (Get_Object (Self));
   end Get_Drag_Window;

   ------------------
   -- Get_Protocol --
   ------------------

   function Get_Protocol
      (Self : not null access Drag_Context_Record) return Gdk_Drag_Protocol
   is
      function Internal (Self : System.Address) return Gdk_Drag_Protocol;
      pragma Import (C, Internal, "gdk_drag_context_get_protocol");
   begin
      return Internal (Get_Object (Self));
   end Get_Protocol;

   -------------------------
   -- Get_Selected_Action --
   -------------------------

   function Get_Selected_Action
      (Self : not null access Drag_Context_Record) return Gdk_Drag_Action
   is
      function Internal (Self : System.Address) return Gdk_Drag_Action;
      pragma Import (C, Internal, "gdk_drag_context_get_selected_action");
   begin
      return Internal (Get_Object (Self));
   end Get_Selected_Action;

   -----------------------
   -- Get_Source_Window --
   -----------------------

   function Get_Source_Window
      (Self : not null access Drag_Context_Record) return Gdk.Gdk_Window
   is
      function Internal (Self : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gdk_drag_context_get_source_window");
   begin
      return Internal (Get_Object (Self));
   end Get_Source_Window;

   --------------------------
   -- Get_Suggested_Action --
   --------------------------

   function Get_Suggested_Action
      (Self : not null access Drag_Context_Record) return Gdk_Drag_Action
   is
      function Internal (Self : System.Address) return Gdk_Drag_Action;
      pragma Import (C, Internal, "gdk_drag_context_get_suggested_action");
   begin
      return Internal (Get_Object (Self));
   end Get_Suggested_Action;

   ----------------
   -- Manage_Dnd --
   ----------------

   function Manage_Dnd
      (Self       : not null access Drag_Context_Record;
       Ipc_Window : Gdk.Gdk_Window;
       Actions    : Gdk_Drag_Action) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Ipc_Window : Gdk.Gdk_Window;
          Actions    : Gdk_Drag_Action) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_drag_context_manage_dnd");
   begin
      return Internal (Get_Object (Self), Ipc_Window, Actions) /= 0;
   end Manage_Dnd;

   ----------------
   -- Set_Device --
   ----------------

   procedure Set_Device
      (Self   : not null access Drag_Context_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class)
   is
      procedure Internal (Self : System.Address; Device : System.Address);
      pragma Import (C, Internal, "gdk_drag_context_set_device");
   begin
      Internal (Get_Object (Self), Get_Object (Device));
   end Set_Device;

   -----------------
   -- Set_Hotspot --
   -----------------

   procedure Set_Hotspot
      (Self  : not null access Drag_Context_Record;
       Hot_X : Glib.Gint;
       Hot_Y : Glib.Gint)
   is
      procedure Internal
         (Self  : System.Address;
          Hot_X : Glib.Gint;
          Hot_Y : Glib.Gint);
      pragma Import (C, Internal, "gdk_drag_context_set_hotspot");
   begin
      Internal (Get_Object (Self), Hot_X, Hot_Y);
   end Set_Hotspot;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Drag_Context_Gdk_Drag_Action_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Drag_Context_Gdk_Drag_Action_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Drag_Action_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Drag_Action_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Drag_Context_Gdk_Drag_Cancel_Reason_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Drag_Context_Gdk_Drag_Cancel_Reason_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Drag_Cancel_Reason_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Drag_Cancel_Reason_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Drag_Context_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Drag_Context_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Drag_Context_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Drag_Context_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gint_Void);

   procedure Connect
      (Object  : access Drag_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Drag_Context_Gdk_Drag_Action_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Drag_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Drag_Context_Gdk_Drag_Cancel_Reason_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Drag_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Drag_Context_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Drag_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Drag_Context_Gint_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Drag_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Drag_Action_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Drag_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Drag_Cancel_Reason_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Drag_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Drag_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_Drag_Context_Gdk_Drag_Action_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Drag_Context_Gdk_Drag_Action_Void);

   procedure Marsh_Drag_Context_Gdk_Drag_Cancel_Reason_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Drag_Context_Gdk_Drag_Cancel_Reason_Void);

   procedure Marsh_Drag_Context_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Drag_Context_Gint_Void);

   procedure Marsh_Drag_Context_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Drag_Context_Void);

   procedure Marsh_GObject_Gdk_Drag_Action_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Drag_Action_Void);

   procedure Marsh_GObject_Gdk_Drag_Cancel_Reason_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Drag_Cancel_Reason_Void);

   procedure Marsh_GObject_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gint_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Drag_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Drag_Context_Gdk_Drag_Action_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Drag_Context_Gdk_Drag_Action_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Drag_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Drag_Context_Gdk_Drag_Cancel_Reason_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Drag_Context_Gdk_Drag_Cancel_Reason_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Drag_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Drag_Context_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Drag_Context_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Drag_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Drag_Context_Gint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Drag_Context_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Drag_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Drag_Action_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Drag_Action_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Drag_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Drag_Cancel_Reason_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Drag_Cancel_Reason_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Drag_Context_Record'Class;
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
      (Object  : access Drag_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ---------------------------------------------
   -- Marsh_Drag_Context_Gdk_Drag_Action_Void --
   ---------------------------------------------

   procedure Marsh_Drag_Context_Gdk_Drag_Action_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Drag_Context_Gdk_Drag_Action_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Drag_Context := Drag_Context (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gdk_Drag_Action (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Drag_Context_Gdk_Drag_Action_Void;

   ----------------------------------------------------
   -- Marsh_Drag_Context_Gdk_Drag_Cancel_Reason_Void --
   ----------------------------------------------------

   procedure Marsh_Drag_Context_Gdk_Drag_Cancel_Reason_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Drag_Context_Gdk_Drag_Cancel_Reason_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Drag_Context := Drag_Context (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gdk_Drag_Cancel_Reason (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Drag_Context_Gdk_Drag_Cancel_Reason_Void;

   ----------------------------------
   -- Marsh_Drag_Context_Gint_Void --
   ----------------------------------

   procedure Marsh_Drag_Context_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Drag_Context_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Drag_Context := Drag_Context (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gint (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Drag_Context_Gint_Void;

   -----------------------------
   -- Marsh_Drag_Context_Void --
   -----------------------------

   procedure Marsh_Drag_Context_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Drag_Context_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Drag_Context := Drag_Context (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Drag_Context_Void;

   ----------------------------------------
   -- Marsh_GObject_Gdk_Drag_Action_Void --
   ----------------------------------------

   procedure Marsh_GObject_Gdk_Drag_Action_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Drag_Action_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gdk_Drag_Action (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Drag_Action_Void;

   -----------------------------------------------
   -- Marsh_GObject_Gdk_Drag_Cancel_Reason_Void --
   -----------------------------------------------

   procedure Marsh_GObject_Gdk_Drag_Cancel_Reason_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Drag_Cancel_Reason_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gdk_Drag_Cancel_Reason (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Drag_Cancel_Reason_Void;

   -----------------------------
   -- Marsh_GObject_Gint_Void --
   -----------------------------

   procedure Marsh_GObject_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gint (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gint_Void;

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

   -----------------------
   -- On_Action_Changed --
   -----------------------

   procedure On_Action_Changed
      (Self  : not null access Drag_Context_Record;
       Call  : Cb_Drag_Context_Gdk_Drag_Action_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "action-changed" & ASCII.NUL, Call, After);
   end On_Action_Changed;

   -----------------------
   -- On_Action_Changed --
   -----------------------

   procedure On_Action_Changed
      (Self  : not null access Drag_Context_Record;
       Call  : Cb_GObject_Gdk_Drag_Action_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "action-changed" & ASCII.NUL, Call, After, Slot);
   end On_Action_Changed;

   ---------------
   -- On_Cancel --
   ---------------

   procedure On_Cancel
      (Self  : not null access Drag_Context_Record;
       Call  : Cb_Drag_Context_Gdk_Drag_Cancel_Reason_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "cancel" & ASCII.NUL, Call, After);
   end On_Cancel;

   ---------------
   -- On_Cancel --
   ---------------

   procedure On_Cancel
      (Self  : not null access Drag_Context_Record;
       Call  : Cb_GObject_Gdk_Drag_Cancel_Reason_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "cancel" & ASCII.NUL, Call, After, Slot);
   end On_Cancel;

   ---------------------
   -- On_Dnd_Finished --
   ---------------------

   procedure On_Dnd_Finished
      (Self  : not null access Drag_Context_Record;
       Call  : Cb_Drag_Context_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "dnd-finished" & ASCII.NUL, Call, After);
   end On_Dnd_Finished;

   ---------------------
   -- On_Dnd_Finished --
   ---------------------

   procedure On_Dnd_Finished
      (Self  : not null access Drag_Context_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "dnd-finished" & ASCII.NUL, Call, After, Slot);
   end On_Dnd_Finished;

   -----------------------
   -- On_Drop_Performed --
   -----------------------

   procedure On_Drop_Performed
      (Self  : not null access Drag_Context_Record;
       Call  : Cb_Drag_Context_Gint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "drop-performed" & ASCII.NUL, Call, After);
   end On_Drop_Performed;

   -----------------------
   -- On_Drop_Performed --
   -----------------------

   procedure On_Drop_Performed
      (Self  : not null access Drag_Context_Record;
       Call  : Cb_GObject_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "drop-performed" & ASCII.NUL, Call, After, Slot);
   end On_Drop_Performed;

end Gdk.Drag_Contexts;
