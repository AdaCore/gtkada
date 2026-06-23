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
with System;

package body Gdk.Drag is

   function Unchecked_To_Drag_Cancel_Reason is new
   Glib.Values.Unsafe_Enum_Nth (Drag_Cancel_Reason);

   package Type_Conversion_Gdk_Drag is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Drag_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Drag);

   ---------------
   -- Drop_Done --
   ---------------

   procedure Drop_Done
      (Self    : not null access Gdk_Drag_Record;
       Success : Boolean)
   is
      procedure Internal (Self : System.Address; Success : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_drag_drop_done");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Success));
   end Drop_Done;

   -----------------
   -- Get_Actions --
   -----------------

   function Get_Actions
      (Self : not null access Gdk_Drag_Record) return Drag_Action
   is
      function Internal (Self : System.Address) return Drag_Action;
      pragma Import (C, Internal, "gdk_drag_get_actions");
   begin
      return Internal (Get_Object (Self));
   end Get_Actions;

   -----------------
   -- Get_Content --
   -----------------

   function Get_Content
      (Self : not null access Gdk_Drag_Record)
       return Gdk.Content_Provider.Gdk_Content_Provider
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_drag_get_content");
      Stub_Gdk_Content_Provider : Gdk.Content_Provider.Gdk_Content_Provider_Record;
   begin
      return Gdk.Content_Provider.Gdk_Content_Provider (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Content_Provider));
   end Get_Content;

   ----------------
   -- Get_Device --
   ----------------

   function Get_Device
      (Self : not null access Gdk_Drag_Record) return Gdk.Gdk_Device
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_drag_get_device");
      Stub_Gdk_Device : Gdk.Device.Gdk_Device_Record;
   begin
      return Gdk.Gdk_Device (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Device));
   end Get_Device;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gdk_Drag_Record) return Gdk.Gdk_Display
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_drag_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Gdk_Display (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Display;

   ----------------------
   -- Get_Drag_Surface --
   ----------------------

   function Get_Drag_Surface
      (Self : not null access Gdk_Drag_Record) return Gdk.Gdk_Surface
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_drag_get_drag_surface");
      Stub_Gdk_Surface : Gdk.Surface.Gdk_Surface_Record;
   begin
      return Gdk.Gdk_Surface (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Surface));
   end Get_Drag_Surface;

   -----------------
   -- Get_Formats --
   -----------------

   function Get_Formats
      (Self : not null access Gdk_Drag_Record)
       return Gdk.Content_Formats.Gdk_Content_Formats
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_drag_get_formats");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Formats;

   -------------------------
   -- Get_Selected_Action --
   -------------------------

   function Get_Selected_Action
      (Self : not null access Gdk_Drag_Record) return Drag_Action
   is
      function Internal (Self : System.Address) return Drag_Action;
      pragma Import (C, Internal, "gdk_drag_get_selected_action");
   begin
      return Internal (Get_Object (Self));
   end Get_Selected_Action;

   -----------------
   -- Get_Surface --
   -----------------

   function Get_Surface
      (Self : not null access Gdk_Drag_Record) return Gdk.Gdk_Surface
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_drag_get_surface");
      Stub_Gdk_Surface : Gdk.Surface.Gdk_Surface_Record;
   begin
      return Gdk.Gdk_Surface (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Surface));
   end Get_Surface;

   -----------------
   -- Set_Hotspot --
   -----------------

   procedure Set_Hotspot
      (Self  : not null access Gdk_Drag_Record;
       Hot_X : Glib.Gint;
       Hot_Y : Glib.Gint)
   is
      procedure Internal
         (Self  : System.Address;
          Hot_X : Glib.Gint;
          Hot_Y : Glib.Gint);
      pragma Import (C, Internal, "gdk_drag_set_hotspot");
   begin
      Internal (Get_Object (Self), Hot_X, Hot_Y);
   end Set_Hotspot;

   ----------------
   -- Begin_Drag --
   ----------------

   function Begin_Drag
      (Surface : not null access Gdk.Surface.Gdk_Surface_Record'Class;
       Device  : not null access Gdk.Device.Gdk_Device_Record'Class;
       Content : not null access Gdk.Content_Provider.Gdk_Content_Provider_Record'Class;
       Actions : Drag_Action;
       Dx      : Gdouble;
       Dy      : Gdouble) return Gdk_Drag
   is
      function Internal
         (Surface : System.Address;
          Device  : System.Address;
          Content : System.Address;
          Actions : Drag_Action;
          Dx      : Gdouble;
          Dy      : Gdouble) return System.Address;
      pragma Import (C, Internal, "gdk_drag_begin");
      Stub_Gdk_Drag : Gdk_Drag_Record;
   begin
      return Gdk.Drag.Gdk_Drag (Get_User_Data (Internal (Get_Object (Surface), Get_Object (Device), Get_Object (Content), Actions, Dx, Dy), Stub_Gdk_Drag));
   end Begin_Drag;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Drag_Drag_Cancel_Reason_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Drag_Drag_Cancel_Reason_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Drag_Cancel_Reason_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Drag_Cancel_Reason_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Drag_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Drag_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gdk_Drag_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Drag_Drag_Cancel_Reason_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gdk_Drag_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Drag_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gdk_Drag_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Drag_Cancel_Reason_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gdk_Drag_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Drag_Cancel_Reason_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Drag_Cancel_Reason_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gdk_Drag_Drag_Cancel_Reason_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Drag_Drag_Cancel_Reason_Void);

   procedure Marsh_Gdk_Drag_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Drag_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Drag_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Drag_Drag_Cancel_Reason_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Drag_Drag_Cancel_Reason_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Drag_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Drag_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Drag_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gdk_Drag_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Drag_Cancel_Reason_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Drag_Cancel_Reason_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gdk_Drag_Record'Class;
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

   -------------------------------------------
   -- Marsh_GObject_Drag_Cancel_Reason_Void --
   -------------------------------------------

   procedure Marsh_GObject_Drag_Cancel_Reason_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Drag_Cancel_Reason_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Drag_Cancel_Reason (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Drag_Cancel_Reason_Void;

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

   --------------------------------------------
   -- Marsh_Gdk_Drag_Drag_Cancel_Reason_Void --
   --------------------------------------------

   procedure Marsh_Gdk_Drag_Drag_Cancel_Reason_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Drag_Drag_Cancel_Reason_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Drag := Gdk_Drag (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Drag_Cancel_Reason (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gdk_Drag_Drag_Cancel_Reason_Void;

   -------------------------
   -- Marsh_Gdk_Drag_Void --
   -------------------------

   procedure Marsh_Gdk_Drag_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Drag_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Drag := Gdk_Drag (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gdk_Drag_Void;

   ---------------
   -- On_Cancel --
   ---------------

   procedure On_Cancel
      (Self  : not null access Gdk_Drag_Record;
       Call  : Cb_Gdk_Drag_Drag_Cancel_Reason_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "cancel" & ASCII.NUL, Call, After);
   end On_Cancel;

   ---------------
   -- On_Cancel --
   ---------------

   procedure On_Cancel
      (Self  : not null access Gdk_Drag_Record;
       Call  : Cb_GObject_Drag_Cancel_Reason_Void;
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
      (Self  : not null access Gdk_Drag_Record;
       Call  : Cb_Gdk_Drag_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "dnd-finished" & ASCII.NUL, Call, After);
   end On_Dnd_Finished;

   ---------------------
   -- On_Dnd_Finished --
   ---------------------

   procedure On_Dnd_Finished
      (Self  : not null access Gdk_Drag_Record;
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
      (Self  : not null access Gdk_Drag_Record;
       Call  : Cb_Gdk_Drag_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "drop-performed" & ASCII.NUL, Call, After);
   end On_Drop_Performed;

   -----------------------
   -- On_Drop_Performed --
   -----------------------

   procedure On_Drop_Performed
      (Self  : not null access Gdk_Drag_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "drop-performed" & ASCII.NUL, Call, After, Slot);
   end On_Drop_Performed;

end Gdk.Drag;
