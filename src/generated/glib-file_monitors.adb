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
with Gtkada.Bindings;            use Gtkada.Bindings;

package body Glib.File_Monitors is

   package Type_Conversion_Gfile_Monitor is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gfile_Monitor_Record);
   pragma Unreferenced (Type_Conversion_Gfile_Monitor);

   ------------
   -- Cancel --
   ------------

   function Cancel
      (Self : not null access Gfile_Monitor_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_monitor_cancel");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Cancel;

   ----------------
   -- Emit_Event --
   ----------------

   procedure Emit_Event
      (Self       : not null access Gfile_Monitor_Record;
       Child      : Glib.GFile.Gfile;
       Other_File : Glib.GFile.Gfile;
       Event_Type : Glib.GEnums.GFile_Monitor_Event)
   is
      procedure Internal
         (Self       : System.Address;
          Child      : Glib.GFile.Gfile;
          Other_File : Glib.GFile.Gfile;
          Event_Type : Glib.GEnums.GFile_Monitor_Event);
      pragma Import (C, Internal, "g_file_monitor_emit_event");
   begin
      Internal (Get_Object (Self), Child, Other_File, Event_Type);
   end Emit_Event;

   ------------------
   -- Is_Cancelled --
   ------------------

   function Is_Cancelled
      (Self : not null access Gfile_Monitor_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_monitor_is_cancelled");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Cancelled;

   --------------------
   -- Set_Rate_Limit --
   --------------------

   procedure Set_Rate_Limit
      (Self        : not null access Gfile_Monitor_Record;
       Limit_Msecs : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Limit_Msecs : Glib.Gint);
      pragma Import (C, Internal, "g_file_monitor_set_rate_limit");
   begin
      Internal (Get_Object (Self), Limit_Msecs);
   end Set_Rate_Limit;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gfile_Monitor_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gfile_Monitor_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void);

   procedure Connect
      (Object  : access Gfile_Monitor_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gfile_Monitor_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gfile_Monitor_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void);

   procedure Marsh_Gfile_Monitor_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gfile_Monitor_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gfile_Monitor_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gfile_Monitor_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gfile_Monitor_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gfile_Monitor_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ----------------------------------------------------------------
   -- Marsh_GObject_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void --
   ----------------------------------------------------------------

   procedure Marsh_GObject_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Glib.GFile.Gfile (Unchecked_To_Interface (Params, 1)), Glib.GFile.Gfile (Unchecked_To_Interface (Params, 2)), Unchecked_To_GFile_Monitor_Event (Params, 3));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void;

   ----------------------------------------------------------------------
   -- Marsh_Gfile_Monitor_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void --
   ----------------------------------------------------------------------

   procedure Marsh_Gfile_Monitor_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gfile_Monitor_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gfile_Monitor := Gfile_Monitor (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Glib.GFile.Gfile (Unchecked_To_Interface (Params, 1)), Glib.GFile.Gfile (Unchecked_To_Interface (Params, 2)), Unchecked_To_GFile_Monitor_Event (Params, 3));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gfile_Monitor_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gfile_Monitor_Record;
       Call  : Cb_Gfile_Monitor_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "changed" & ASCII.NUL, Call, After);
   end On_Changed;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gfile_Monitor_Record;
       Call  : Cb_GObject_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "changed" & ASCII.NUL, Call, After, Slot);
   end On_Changed;

end Glib.File_Monitors;
