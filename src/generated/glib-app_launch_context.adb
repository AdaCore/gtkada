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
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Glib.App_Launch_Context is

   package Type_Conversion_Gapp_Launch_Context is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gapp_Launch_Context_Record);
   pragma Unreferenced (Type_Conversion_Gapp_Launch_Context);

   -----------
   -- G_New --
   -----------

   procedure G_New (Self : out Gapp_Launch_Context) is
   begin
      Self := new Gapp_Launch_Context_Record;
      Glib.App_Launch_Context.Initialize (Self);
   end G_New;

   -----------------------------
   -- Gapp_Launch_Context_New --
   -----------------------------

   function Gapp_Launch_Context_New return Gapp_Launch_Context is
      Self : constant Gapp_Launch_Context := new Gapp_Launch_Context_Record;
   begin
      Glib.App_Launch_Context.Initialize (Self);
      return Self;
   end Gapp_Launch_Context_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gapp_Launch_Context_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_app_launch_context_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ---------------------
   -- Get_Environment --
   ---------------------

   function Get_Environment
      (Self : not null access Gapp_Launch_Context_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_app_launch_context_get_environment");
   begin
      return To_String_List_And_Free (Internal (Get_Object (Self)));
   end Get_Environment;

   -------------------
   -- Launch_Failed --
   -------------------

   procedure Launch_Failed
      (Self              : not null access Gapp_Launch_Context_Record;
       Startup_Notify_Id : UTF8_String)
   is
      procedure Internal
         (Self              : System.Address;
          Startup_Notify_Id : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_app_launch_context_launch_failed");
      Tmp_Startup_Notify_Id : Gtkada.Types.Chars_Ptr := New_String (Startup_Notify_Id);
   begin
      Internal (Get_Object (Self), Tmp_Startup_Notify_Id);
      Free (Tmp_Startup_Notify_Id);
   end Launch_Failed;

   ------------
   -- Setenv --
   ------------

   procedure Setenv
      (Self     : not null access Gapp_Launch_Context_Record;
       Variable : UTF8_String;
       Value    : UTF8_String)
   is
      procedure Internal
         (Self     : System.Address;
          Variable : Gtkada.Types.Chars_Ptr;
          Value    : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_app_launch_context_setenv");
      Tmp_Variable : Gtkada.Types.Chars_Ptr := New_String (Variable);
      Tmp_Value    : Gtkada.Types.Chars_Ptr := New_String (Value);
   begin
      Internal (Get_Object (Self), Tmp_Variable, Tmp_Value);
      Free (Tmp_Value);
      Free (Tmp_Variable);
   end Setenv;

   --------------
   -- Unsetenv --
   --------------

   procedure Unsetenv
      (Self     : not null access Gapp_Launch_Context_Record;
       Variable : UTF8_String)
   is
      procedure Internal
         (Self     : System.Address;
          Variable : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_app_launch_context_unsetenv");
      Tmp_Variable : Gtkada.Types.Chars_Ptr := New_String (Variable);
   begin
      Internal (Get_Object (Self), Tmp_Variable);
      Free (Tmp_Variable);
   end Unsetenv;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gapp_Launch_Context_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gapp_Launch_Context_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Void);

   procedure Connect
      (Object  : access Gapp_Launch_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gapp_Launch_Context_UTF8_String_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gapp_Launch_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Void);

   procedure Marsh_Gapp_Launch_Context_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gapp_Launch_Context_UTF8_String_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gapp_Launch_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gapp_Launch_Context_UTF8_String_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gapp_Launch_Context_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gapp_Launch_Context_Record'Class;
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
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_Void;

   ------------------------------------------------
   -- Marsh_Gapp_Launch_Context_UTF8_String_Void --
   ------------------------------------------------

   procedure Marsh_Gapp_Launch_Context_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gapp_Launch_Context_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gapp_Launch_Context := Gapp_Launch_Context (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gapp_Launch_Context_UTF8_String_Void;

   ----------------------
   -- On_Launch_Failed --
   ----------------------

   procedure On_Launch_Failed
      (Self  : not null access Gapp_Launch_Context_Record;
       Call  : Cb_Gapp_Launch_Context_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "launch-failed" & ASCII.NUL, Call, After);
   end On_Launch_Failed;

   ----------------------
   -- On_Launch_Failed --
   ----------------------

   procedure On_Launch_Failed
      (Self  : not null access Gapp_Launch_Context_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "launch-failed" & ASCII.NUL, Call, After, Slot);
   end On_Launch_Failed;

end Glib.App_Launch_Context;
