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
with Glib.Error;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;

use type Glib.Error.GError;

package body Glib.Cancellable is

   function C_G_Cancellable_Connect
      (Self              : System.Address;
       Callback          : System.Address;
       Data              : System.Address;
       Data_Destroy_Func : System.Address) return Gulong;
   pragma Import (C, C_G_Cancellable_Connect, "g_cancellable_connect");
   --  Convenience function to connect to the cancelled signal, handling the
   --  race with concurrent cancellation.
   --  Callback is called at most once: before Connect returns if Self is
   --  already cancelled, or when Self is cancelled in some thread.
   --  Connect_User_Data copies Data into a private closure. Its generic
   --  Destroy procedure is called exactly once when the handler is
   --  disconnected or Self is finalized, or immediately after Callback returns
   --  if Self was already cancelled. Use that generic procedure to release Ada
   --  data; the C destroy notifier is managed by GtkAda.
   --  Since GLib 2.40, the lock protecting Self is not held when Callback
   --  runs, so Callback may itself invoke Cancel.
   --  Since: gtk+ 2.22
   --  @param Callback The Gcallback to connect.
   --  @param Data Data to pass to Callback.
   --  @param Data_Destroy_Func Free function for Data or null.
   --  @return The id of the signal handler or 0 if Cancellable has already
   --  been cancelled.

   function To_Gcallback is new Ada.Unchecked_Conversion
     (System.Address, Gcallback);

   function To_Address is new Ada.Unchecked_Conversion
     (Gcallback, System.Address);

   procedure Internal_Gcallback
      (Cancellable : System.Address;
       Data        : System.Address);
   pragma Convention (C, Internal_Gcallback);

   ------------------------
   -- Internal_Gcallback --
   ------------------------

   procedure Internal_Gcallback
      (Cancellable : System.Address;
       Data        : System.Address)
   is
      pragma Unreferenced (Cancellable);
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

   procedure Cancel (Self : access Gcancellable_Record'Class) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_cancellable_cancel");
   begin
      Internal (Get_Object_Or_Null (GObject (Self)));
   end Cancel;

   -------------
   -- Connect --
   -------------

   function Connect
      (Self     : access Gcancellable_Record'Class;
       Callback : Gcallback) return Gulong
   is
   begin
      if Callback = null then
         return C_G_Cancellable_Connect (Get_Object_Or_Null (GObject (Self)), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         return C_G_Cancellable_Connect (Get_Object_Or_Null (GObject (Self)), Internal_Gcallback'Address, To_Address (Callback), System.Null_Address);
      end if;
   end Connect;

   package body Connect_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gcallback is new Ada.Unchecked_Conversion
        (System.Address, Gcallback);

      function To_Address is new Ada.Unchecked_Conversion
        (Gcallback, System.Address);

      procedure Internal_Cb
         (Cancellable : System.Address;
          Data        : System.Address);
      pragma Convention (C, Internal_Cb);
      --  Callback invoked when the cancellable is cancelled.

      -------------
      -- Connect --
      -------------

      function Connect
         (Self     : access Glib.Cancellable.Gcancellable_Record'Class;
          Callback : Gcallback;
          Data     : User_Data_Type) return Gulong
      is
         D : System.Address;
      begin
         if Callback = null then
            return C_G_Cancellable_Connect (Get_Object_Or_Null (GObject (Self)), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Callback), Data);
            return C_G_Cancellable_Connect (Get_Object_Or_Null (GObject (Self)), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Connect;

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Cancellable : System.Address;
          Data        : System.Address)
      is
         pragma Unreferenced (Cancellable);
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         To_Gcallback (D.Func) (D.Data.all);
      end Internal_Cb;

   end Connect_User_Data;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect
      (Self       : access Gcancellable_Record'Class;
       Handler_Id : Gulong)
   is
      procedure Internal (Self : System.Address; Handler_Id : Gulong);
      pragma Import (C, Internal, "g_cancellable_disconnect");
   begin
      Internal (Get_Object_Or_Null (GObject (Self)), Handler_Id);
   end Disconnect;

   ------------
   -- Get_Fd --
   ------------

   function Get_Fd
      (Self : access Gcancellable_Record'Class) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "g_cancellable_get_fd");
   begin
      return Internal (Get_Object_Or_Null (GObject (Self)));
   end Get_Fd;

   ------------------
   -- Is_Cancelled --
   ------------------

   function Is_Cancelled
      (Self : access Gcancellable_Record'Class) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_cancellable_is_cancelled");
   begin
      return Internal (Get_Object_Or_Null (GObject (Self))) /= 0;
   end Is_Cancelled;

   -----------------
   -- Pop_Current --
   -----------------

   procedure Pop_Current (Self : access Gcancellable_Record'Class) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_cancellable_pop_current");
   begin
      Internal (Get_Object_Or_Null (GObject (Self)));
   end Pop_Current;

   ------------------
   -- Push_Current --
   ------------------

   procedure Push_Current (Self : access Gcancellable_Record'Class) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_cancellable_push_current");
   begin
      Internal (Get_Object_Or_Null (GObject (Self)));
   end Push_Current;

   ----------------
   -- Release_Fd --
   ----------------

   procedure Release_Fd (Self : access Gcancellable_Record'Class) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_cancellable_release_fd");
   begin
      Internal (Get_Object_Or_Null (GObject (Self)));
   end Release_Fd;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : access Gcancellable_Record'Class) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_cancellable_reset");
   begin
      Internal (Get_Object_Or_Null (GObject (Self)));
   end Reset;

   ----------------------------
   -- Set_Error_If_Cancelled --
   ----------------------------

   function Set_Error_If_Cancelled
      (Self  : access Gcancellable_Record'Class;
       Error : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_cancellable_set_error_if_cancelled");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object_Or_Null (GObject (Self)), Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Set_Error_If_Cancelled;

   ----------------
   -- Source_New --
   ----------------

   function Source_New
      (Self : access Gcancellable_Record'Class) return Glib.Main.G_Source
   is
      function Internal (Self : System.Address) return Glib.Main.G_Source;
      pragma Import (C, Internal, "g_cancellable_source_new");
   begin
      return Internal (Get_Object_Or_Null (GObject (Self)));
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
   exception
      when E : others => Process_Exception (E);
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
   exception
      when E : others => Process_Exception (E);
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
