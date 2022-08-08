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

package body Glib.Cancellable is

   function C_G_Cancellable_Connect
      (Self              : System.Address;
       Callback          : System.Address;
       Data              : System.Address;
       Data_Destroy_Func : Glib.G_Destroy_Notify_Address) return Gulong;
   pragma Import (C, C_G_Cancellable_Connect, "g_cancellable_connect");
   --  Convenience function to connect to the
   --  Glib.Cancellable.Gcancellable::cancelled signal. Also handles the race
   --  condition that may happen if the cancellable is cancelled right before
   --  connecting.
   --  Callback is called at most once, either directly at the time of the
   --  connect if Cancellable is already cancelled, or when Cancellable is
   --  cancelled in some thread.
   --  Data_Destroy_Func will be called when the handler is disconnected, or
   --  immediately if the cancellable is already cancelled.
   --  See Glib.Cancellable.Gcancellable::cancelled for details on how to use
   --  this.
   --  Since GLib 2.40, the lock protecting Cancellable is not held when
   --  Callback is invoked. This lifts a restriction in place for earlier GLib
   --  versions which now makes it easier to write cleanup code that
   --  unconditionally invokes e.g. Glib.Cancellable.Cancel.
   --  Since: gtk+ 2.22
   --  "callback": The Gcallback to connect.
   --  "data": Data to pass to Callback.
   --  "data_destroy_func": Free function for Data or null.

   function To_Gcallback is new Ada.Unchecked_Conversion
     (System.Address, Gcallback);

   function To_Address is new Ada.Unchecked_Conversion
     (Gcallback, System.Address);

   procedure Internal_Gcallback (Data : System.Address);
   pragma Convention (C, Internal_Gcallback);

   ------------------------
   -- Internal_Gcallback --
   ------------------------

   procedure Internal_Gcallback (Data : System.Address) is
      Func : constant Gcallback := To_Gcallback (Data);
   begin
      Func.all;
   end Internal_Gcallback;

   package Type_Conversion_Gcancellable is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gcancellable_Record);
   pragma Unreferenced (Type_Conversion_Gcancellable);

   -----------
   -- G_New --
   -----------

   procedure G_New (Self : out Gcancellable) is
   begin
      Self := new Gcancellable_Record;
      Glib.Cancellable.Initialize (Self);
   end G_New;

   ----------------------
   -- Gcancellable_New --
   ----------------------

   function Gcancellable_New return Gcancellable is
      Self : constant Gcancellable := new Gcancellable_Record;
   begin
      Glib.Cancellable.Initialize (Self);
      return Self;
   end Gcancellable_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gcancellable_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_cancellable_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ------------
   -- Cancel --
   ------------

   procedure Cancel (Self : not null access Gcancellable_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_cancellable_cancel");
   begin
      Internal (Get_Object (Self));
   end Cancel;

   -------------
   -- Connect --
   -------------

   function Connect
      (Self              : not null access Gcancellable_Record;
       Callback          : Gcallback;
       Data_Destroy_Func : Glib.G_Destroy_Notify_Address) return Gulong
   is
   begin
      if Callback = null then
         return C_G_Cancellable_Connect (Get_Object (Self), System.Null_Address, System.Null_Address, Data_Destroy_Func);
      else
         return C_G_Cancellable_Connect (Get_Object (Self), Internal_Gcallback'Address, To_Address (Callback), Data_Destroy_Func);
      end if;
   end Connect;

   package body Connect_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gcallback is new Ada.Unchecked_Conversion
        (System.Address, Gcallback);

      function To_Address is new Ada.Unchecked_Conversion
        (Gcallback, System.Address);

      procedure Internal_Cb (Data : System.Address);
      pragma Convention (C, Internal_Cb);
      --  The type used for callback functions in structure definitions and
      --  function signatures. This doesn't mean that all callback functions
      --  must take no parameters and return void. The required signature of a
      --  callback function is determined by the context in which is used (e.g.
      --  the signal to which it is connected). Use G_CALLBACK to cast the
      --  callback function to a Gcallback.

      -------------
      -- Connect --
      -------------

      function Connect
         (Self              : not null access Glib.Cancellable.Gcancellable_Record'Class;
          Callback          : Gcallback;
          Data              : User_Data_Type;
          Data_Destroy_Func : Glib.G_Destroy_Notify_Address) return Gulong
      is
         D : System.Address;
      begin
         if Callback = null then
            return C_G_Cancellable_Connect (Get_Object (Self), System.Null_Address, System.Null_Address, Data_Destroy_Func);
         else
            D := Users.Build (To_Address (Callback), Data);
            return C_G_Cancellable_Connect (Get_Object (Self), Internal_Cb'Address, D, Data_Destroy_Func);
         end if;
      end Connect;

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb (Data : System.Address) is
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         To_Gcallback (D.Func) (D.Data.all);
      end Internal_Cb;

   end Connect_User_Data;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect
      (Self       : not null access Gcancellable_Record;
       Handler_Id : Gulong)
   is
      procedure Internal (Self : System.Address; Handler_Id : Gulong);
      pragma Import (C, Internal, "g_cancellable_disconnect");
   begin
      Internal (Get_Object (Self), Handler_Id);
   end Disconnect;

   ------------
   -- Get_Fd --
   ------------

   function Get_Fd
      (Self : not null access Gcancellable_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "g_cancellable_get_fd");
   begin
      return Internal (Get_Object (Self));
   end Get_Fd;

   ------------------
   -- Is_Cancelled --
   ------------------

   function Is_Cancelled
      (Self : not null access Gcancellable_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_cancellable_is_cancelled");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Cancelled;

   -----------------
   -- Pop_Current --
   -----------------

   procedure Pop_Current (Self : not null access Gcancellable_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_cancellable_pop_current");
   begin
      Internal (Get_Object (Self));
   end Pop_Current;

   ------------------
   -- Push_Current --
   ------------------

   procedure Push_Current (Self : not null access Gcancellable_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_cancellable_push_current");
   begin
      Internal (Get_Object (Self));
   end Push_Current;

   ----------------
   -- Release_Fd --
   ----------------

   procedure Release_Fd (Self : not null access Gcancellable_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_cancellable_release_fd");
   begin
      Internal (Get_Object (Self));
   end Release_Fd;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : not null access Gcancellable_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_cancellable_reset");
   begin
      Internal (Get_Object (Self));
   end Reset;

   ----------------------------
   -- Set_Error_If_Cancelled --
   ----------------------------

   function Set_Error_If_Cancelled
      (Self : not null access Gcancellable_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_cancellable_set_error_if_cancelled");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Set_Error_If_Cancelled;

   ----------------
   -- Source_New --
   ----------------

   function Source_New
      (Self : not null access Gcancellable_Record) return Glib.Main.G_Source
   is
      function Internal (Self : System.Address) return Glib.Main.G_Source;
      pragma Import (C, Internal, "g_cancellable_source_new");
   begin
      return Internal (Get_Object (Self));
   end Source_New;

   -----------------
   -- Get_Current --
   -----------------

   function Get_Current return Gcancellable is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_cancellable_get_current");
      Stub_Gcancellable : Gcancellable_Record;
   begin
      return Glib.Cancellable.Gcancellable (Get_User_Data (Internal, Stub_Gcancellable));
   end Get_Current;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gcancellable_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gcancellable_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gcancellable_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gcancellable_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gcancellable_Record'Class;
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

   procedure Marsh_Gcancellable_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gcancellable_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gcancellable_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gcancellable_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gcancellable_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gcancellable_Record'Class;
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

   -----------------------------
   -- Marsh_Gcancellable_Void --
   -----------------------------

   procedure Marsh_Gcancellable_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gcancellable_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gcancellable := Gcancellable (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gcancellable_Void;

   ------------------
   -- On_Cancelled --
   ------------------

   procedure On_Cancelled
      (Self  : not null access Gcancellable_Record;
       Call  : Cb_Gcancellable_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "cancelled" & ASCII.NUL, Call, After);
   end On_Cancelled;

   ------------------
   -- On_Cancelled --
   ------------------

   procedure On_Cancelled
      (Self  : not null access Gcancellable_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "cancelled" & ASCII.NUL, Call, After, Slot);
   end On_Cancelled;

end Glib.Cancellable;
